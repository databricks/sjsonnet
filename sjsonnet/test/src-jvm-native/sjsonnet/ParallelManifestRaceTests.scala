package sjsonnet

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.concurrent.{
  BrokenBarrierException,
  CountDownLatch,
  CyclicBarrier,
  Executors,
  TimeoutException,
  TimeUnit
}
import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable
import sjsonnet.stdlib.ArrayModule
import sjsonnet.stdlib.StdLibModule
import utest._

/**
 * Regression tests for data races that corrupted manifest output when many sjsonnet evaluations run
 * in parallel — e.g. a Bazel bundle worker that fans evaluation across a thread pool while sharing
 * one parse cache (and therefore the StaticOptimizer-folded constants embedded in the cached AST)
 * and the process-wide default `std` module. See issue #1047.
 *
 * Distinct shared-mutable-state bugs are covered:
 *
 *   1. The renderer's integer-formatting scratch buffer ([[BaseCharRenderer]] / [[BaseByteRenderer]])
 *      used to be a process-global array shared by every renderer instance. Concurrent number
 *      rendering interleaved digits in that buffer, producing malformed JSON whose classic symptom
 *      was a stray digit where a separator was expected ("expected , or } got 0").
 *
 *   2. `Val.staticObject` constants are interned and shared across threads via the parse cache. Their
 *      `sortedVisibleKeyNames` cache used a non-volatile field populated lazily at materialization
 *      time; unsafe publication let one thread observe a half-sorted key array, corrupting key
 *      order / count in the output.
 *
 *   3. The default `std` module is a process-wide singleton shared by independent interpreters. Its
 *      field lookups (e.g. resolving `std.map` while optimizing) used to populate a lazy value cache
 *      on first access — concurrent first access raced on the shared inline cache / `valueCache`
 *      HashMap, returning the wrong builtin or corrupting the map. `mkWithConstCache` now pre-fills
 *      it so lookups are read-only.
 *
 *   4. `Expr.ObjBody.MemberList` caches key-name arrays (`_cachedAllKeyNames` / `_cachedVisibleKeyNames`)
 *      shared across all objects from that (parse-cached, cross-interpreter) expression. The fields
 *      were non-volatile; the test below exercises the shared path under contention.
 *
 * Race tests are probabilistic: before the fixes these fail frequently under load; after the fixes
 * they always pass. They do not flake when the code is correct — a correct implementation has no
 * interleaving that can produce a wrong result.
 */
object ParallelManifestRaceTests extends TestSuite {

  private val Threads = math.max(4, Runtime.getRuntime.availableProcessors())
  private val Iterations = 400

  /**
   * Run `body(threadIndex)` on `Threads` threads released simultaneously (a latch maximizes
   * contention), and return the first failure observed by any thread, if any.
   */
  private def stress(body: Int => Unit): Option[Throwable] = {
    val pool = Executors.newFixedThreadPool(Threads)
    val start = new CountDownLatch(1)
    val failure = new AtomicReference[Throwable](null)
    try {
      val futures = (0 until Threads).map { t =>
        pool.submit(new Runnable {
          def run(): Unit = {
            start.await()
            try body(t)
            catch { case th: Throwable => failure.compareAndSet(null, th) }
          }
        })
      }
      start.countDown()
      futures.foreach(_.get(120, TimeUnit.SECONDS))
    } finally pool.shutdownNow()
    Option(failure.get())
  }

  /**
   * Run `rounds` rounds. Each round builds ONE fresh `std` module and hands it to every thread at
   * the same instant (via a barrier), so all `Threads` first-touch the cold singleton concurrently —
   * the worst case for a lazily-populated shared cache. A second barrier keeps threads in lockstep
   * so no thread starts the next round (with a new `std`) while a peer is still using this one.
   */
  private def stressFreshSharedStd(rounds: Int)(body: (Val.Obj, Int) => Unit): Option[Throwable] = {
    val pool = Executors.newFixedThreadPool(Threads)
    val barrier = new CyclicBarrier(Threads)
    val shared = new AtomicReference[Val.Obj](null)
    val failure = new AtomicReference[Throwable](null)
    try {
      val futures = (0 until Threads).map { t =>
        pool.submit(new Runnable {
          def run(): Unit =
            try {
              var n = 0
              while (n < rounds && failure.get() == null) {
                if (t == 0) shared.set(new StdLibModule().module)
                barrier.await(30, TimeUnit.SECONDS)
                body(shared.get(), t)
                barrier.await(30, TimeUnit.SECONDS)
                n += 1
              }
            } catch {
              case _: BrokenBarrierException | _: TimeoutException => // a peer failed; just exit
              case th: Throwable =>
                failure.compareAndSet(null, th)
                barrier.reset() // unblock any peers waiting at the barrier
            }
        })
      }
      futures.foreach(_.get(120, TimeUnit.SECONDS))
    } finally pool.shutdownNow()
    Option(failure.get())
  }

  private def newInterpreter(): Interpreter =
    new Interpreter(Map(), Map(), DummyPath(), Importer.empty, parseCache = new DefaultParseCache)

  private val noEvalScope: EvalScope = new EvalScope {
    def extVars: String => Option[Expr] = _ => None
    def importer: CachedImporter = new CachedImporter(Importer.empty)
    def wd: Path = DummyPath()
    def visitExpr(expr: Expr)(implicit scope: ValScope): Val =
      throw new UnsupportedOperationException("not used")
    def materialize(v: Val): ujson.Value =
      throw new UnsupportedOperationException("not used")
    def equal(x: Val, y: Val): Boolean = x == y
    def compare(x: Val, y: Val): Int = 0
    def settings: Settings = Settings.default
    def debugStats: DebugStats = null
    def trace(msg: String): Unit = ()
    def warn(e: Error): Unit = ()
  }

  private def directOptimizer(): StaticOptimizer =
    new StaticOptimizer(
      noEvalScope,
      _ => None,
      new StdLibModule().module,
      new mutable.HashMap[String, String],
      new mutable.HashMap[
        Val.StaticObjectFieldSet,
        java.util.LinkedHashMap[String, java.lang.Boolean]
      ]
    )

  /**
   * Parse + StaticOptimize `src` on `interp` and return the optimized root Expr WITHOUT evaluating it.
   * Used both as a deterministic guard (inspect the folded AST node) and to warm a shared parse cache
   * with a COLD folded constant (no thunk/array/object cache forced yet).
   */
  private def parseOptimized(interp: Interpreter, src: String): Expr = {
    val path = DummyPath("(memory)")
    interp.resolver.cache((path, false)) = StaticResolvedFile(src)
    interp.resolver.parse(path, StaticResolvedFile(src))(interp.evaluator) match {
      case Right((expr, _)) => expr
      case Left(err)        => throw new Exception(Error.formatError(err))
    }
  }

  /**
   * Each round builds ONE fresh DefaultParseCache and, on thread 0, parses+optimizes `src` into it
   * WITHOUT evaluating — so the StaticOptimizer-folded constants embedded in the cached AST (e.g. a
   * folded `std.objectValues` array, or a static object whose `value0` is built lazily by getValue0)
   * are COLD. A barrier then releases all threads to evaluate via independent interpreters sharing
   * that one cache at the same instant — the worst case for a lazily-populated cache on a shared,
   * parse-cached Val. A second barrier keeps rounds in lockstep (no thread races ahead to the next,
   * freshly-cold round while a peer still uses this one).
   */
  private def stressFreshSharedParseCache(rounds: Int, src: String)(
      check: ujson.Value => Unit): Option[Throwable] = {
    val path = DummyPath("(memory)")
    val pool = Executors.newFixedThreadPool(Threads)
    val barrier = new CyclicBarrier(Threads)
    val shared = new AtomicReference[DefaultParseCache](null)
    val failure = new AtomicReference[Throwable](null)
    try {
      val futures = (0 until Threads).map { t =>
        pool.submit(new Runnable {
          def run(): Unit =
            try {
              var n = 0
              while (n < rounds && failure.get() == null) {
                if (t == 0) {
                  val cache = new DefaultParseCache
                  parseOptimized(new Interpreter(Map(), Map(), DummyPath(), Importer.empty, cache), src)
                  shared.set(cache)
                }
                barrier.await(30, TimeUnit.SECONDS) // cold folded AST is cached and shared
                // Build the (heavy) interpreter BEFORE the next barrier so the cold shared Val is
                // forced by all threads at the same instant, not staggered by interpreter construction.
                val interp = new Interpreter(Map(), Map(), DummyPath(), Importer.empty, shared.get())
                barrier.await(30, TimeUnit.SECONDS) // all interpreters built — now force together
                interp.interpret(src, path) match {
                  case Right(v)  => check(v)
                  case Left(err) => throw new java.lang.AssertionError("eval failed: " + err)
                }
                barrier.await(30, TimeUnit.SECONDS) // end of round
                n += 1
              }
            } catch {
              case _: BrokenBarrierException | _: TimeoutException => // a peer failed; just exit
              case th: Throwable =>
                failure.compareAndSet(null, th)
                barrier.reset() // unblock peers waiting at the barrier
            }
        })
      }
      futures.foreach(_.get(120, TimeUnit.SECONDS))
    } finally pool.shutdownNow()
    Option(failure.get())
  }

  def tests: Tests = Tests {

    // ---- Fix 1: process-global renderer scratch buffer in writeLongDirect ----

    test("charRendererConcurrentLongRendering") {
      // Many distinct 13-digit longs widen the window in writeLongDirect's digit loop, so a shared
      // scratch buffer reliably interleaves under load.
      val arr = ujson.Arr((0 until 256).map(i => ujson.Num((1234567890000L + i).toDouble))*)
      val expected = ujson.transform(arr, new Renderer()).toString
      val failure = stress { _ =>
        var n = 0
        while (n < Iterations) {
          val got = ujson.transform(arr, new Renderer()).toString
          if (got != expected) throw new java.lang.AssertionError("corrupted char render: " + got)
          n += 1
        }
      }
      failure.foreach(throw _)
    }

    test("byteRendererConcurrentLongRendering") {
      val arr = ujson.Arr((0 until 256).map(i => ujson.Num((9876543210000L + i).toDouble))*)
      def renderBytes(): String = {
        val out = new ByteArrayOutputStream()
        ujson.transform(arr, new ByteRenderer(out))
        new String(out.toByteArray, StandardCharsets.UTF_8)
      }
      val expected = renderBytes()
      val failure = stress { _ =>
        var n = 0
        while (n < Iterations) {
          val got = renderBytes()
          if (got != expected) throw new java.lang.AssertionError("corrupted byte render: " + got)
          n += 1
        }
      }
      failure.foreach(throw _)
    }

    // ---- Fix 2: shared Val.staticObject sortedVisibleKeyNames cache ----

    test("staticObjectPrecomputesSortedKeysAtConstruction") {
      // A constant object literal folds to a Val.staticObject. The fix populates its sorted-key-name
      // cache eagerly at construction, so the field is fully built (non-null, complete, strictly
      // sorted) WITHOUT any materialization having forced it.
      val src = "{ " + (0 until 20).map(i => s""""k${19 - i}": $i""").mkString(", ") + " }"
      val obj = newInterpreter().evaluate(src, DummyPath("(memory)")) match {
        case Right(v)  => v.asInstanceOf[Val.Obj]
        case Left(err) => throw new Exception(Error.formatError(err))
      }
      val keys = obj._sortedVisibleKeyNames
      assert(keys != null)
      assert(keys.length == 20)
      var i = 1
      while (i < keys.length) {
        // strictly increasing — a half-sorted (raced) array would violate this
        assert(Util.compareStringsByCodepoint(keys(i - 1), keys(i)) < 0)
        i += 1
      }
    }

    test("staticObjectConcurrentSortedMaterialize") {
      // Materialize the SAME Val.staticObject from many threads with key sorting (the default),
      // mirroring a shared parse-cached constant materialized across a worker thread pool.
      val src = "{ " + (0 until 64).map(i => s""""key${63 - i}": ${1000000 + i}""").mkString(", ") + " }"
      val interp = newInterpreter()
      val value = interp.evaluate(src, DummyPath("(memory)")) match {
        case Right(v)  => v
        case Left(err) => throw new Exception(Error.formatError(err))
      }
      def manifest(): String = interp.materialize(value, new Renderer()) match {
        case Right(w)  => w.toString
        case Left(err) => throw new Exception(Error.formatError(err))
      }
      val expected = manifest()
      val failure = stress { _ =>
        var n = 0
        while (n < Iterations) {
          val got = manifest()
          if (got != expected) throw new java.lang.AssertionError("corrupted materialize: " + got)
          n += 1
        }
      }
      failure.foreach(throw _)
    }

    // ---- Fix 3: shared default std module value cache ----

    test("sharedStdModulePrefillsConstCacheAtConstruction") {
      // The fix builds the std module via mkWithConstCache, which pre-populates the value cache for
      // every constant field at construction. So the cache is fully built (lookups are read-only)
      // WITHOUT any field access having forced it. The lazy path (plain mk) would instead leave the
      // cache empty and write it on first lookup, racing across the interpreters that share this
      // singleton. This is the deterministic guard; the concurrent test below is a stress companion.
      val std = new StdLibModule().module
      for (n <- Seq("map", "filter", "length", "join", "sort", "substr", "reverse", "range")) {
        val cached = std.cachedValueForTest(n)
        assert(cached != null) // null here means the cache was not pre-filled (regression)
        assert(cached.isInstanceOf[Val.Func])
      }
    }

    test("functionModulesPrefillConstCacheAtConstruction") {
      val module = ArrayModule.module
      for ((name, expected) <- ArrayModule.functions.take(8)) {
        val cached = module.cachedValueForTest(name)
        assert(cached != null)
        assert(cached eq expected)
      }
    }

    test("sharedStdModuleConcurrentFirstAccess") {
      // Independent interpreters share one std module (Interpreter's default std is a singleton). The
      // StaticOptimizer folds `std.fn` by calling std.value(fn), so concurrent first access to a cold
      // shared module races on its lazily-populated cache. This reproduces that call directly — no
      // parse preamble, so all threads truly hit the cold module at the same instant. Each thread
      // looks the builtins up in a DIFFERENT order, so they tear the inline cache (ck1/cv1) against
      // DIFFERENT keys: a torn write makes a lookup return the WRONG builtin, which the
      // reference-identity check below catches (stdlib functions are shared instances across
      // StdLibModule instances). Many distinct names also force the overflow valueCache HashMap to
      // grow under concurrent puts. mkWithConstCache pre-fills the cache so every lookup is read-only.
      val names = Array(
        "length",
        "map",
        "filter",
        "join",
        "reverse",
        "range",
        "type",
        "startsWith",
        "strReplace",
        "max",
        "foldl",
        "sort",
        "asciiUpper",
        "substr"
      )
      val ev: EvalScope = newInterpreter().evaluator
      val pos = new Position(new FileScope(DummyPath()), 0)
      // Canonical builtin instance for each name (from a separate, warm std).
      val expected: Map[String, Val] =
        names.map(n => n -> new StdLibModule().module.value(n, pos)(ev)).toMap
      expected.foreach { case (n, v) => assert(v.isInstanceOf[Val.Func]) }
      val failure = stressFreshSharedStd(rounds = 800) { (std, t) =>
        var i = 0
        while (i < names.length) {
          val n = names((i + t) % names.length)
          val v = std.value(n, pos)(ev)
          if (v ne expected(n))
            throw new java.lang.AssertionError(s"std.$n returned the wrong value (torn cache): $v")
          i += 1
        }
      }
      failure.foreach(throw _)
    }

    // ---- Fix 4: shared MemberList key-name caches (parse cache) ----

    test("defaultParseCacheConcurrentColdSharedInterpreters") {
      // End-to-end guard: independent interpreters share a cold DefaultParseCache and all parse the
      // same root at once. This exercises CachedResolver.parse -> ParseCache.getOrElseUpdate under
      // contention, not just direct cache calls.
      val src =
        """local base = { hidden:: 1, visible: 2 };
          |{
          |  fields: std.objectFields(base),
          |  mapped: std.map(function(x) x * 3, [1, 2, 3]),
          |  nested: { c: 3, a: 1, b: 2 },
          |}
          |""".stripMargin
      val path = DummyPath("root", "cold-shared.jsonnet")
      val expected =
        new Interpreter(Map(), Map(), DummyPath("root"), Importer.empty, new DefaultParseCache)
          .interpret(src, path)
      assert(expected.isRight)

      val sharedCache = new DefaultParseCache
      val failure = stress { _ =>
        var n = 0
        while (n < 60) {
          val got =
            new Interpreter(Map(), Map(), DummyPath("root"), Importer.empty, sharedCache)
              .interpret(src, path)
          if (got != expected) {
            throw new java.lang.AssertionError(s"unexpected interpret result: $got")
          }
          n += 1
        }
      }
      failure.foreach(throw _)
      assert(sharedCache.keySet.size == 1)
    }

    test("sharedMemberListConcurrentVisibleKeys") {
      // A non-static object (fields reference a local, so it is NOT folded to a Val.staticObject) with
      // interleaved hidden/visible fields, evaluated by independent interpreters that SHARE one parse
      // cache. They share the folded MemberList, whose _cachedVisibleKeyNames/_cachedAllKeyNames are
      // populated by whichever Val.Obj materializes first and read by the rest. Making those fields
      // @volatile safely publishes the fully-built array. NOTE: on a strong (TSO) memory model like
      // x86 an unsafe publication rarely manifests, so this is primarily a hardening guard for weak
      // memory models (e.g. ARM) and a smoke test that the shared path stays consistent under load.
      val fields = (0 until 40).map { i =>
        val vis = if (i % 3 == 0) "::" else ":" // hide every third field
        s""""m${39 - i}"$vis n + $i"""
      }
      val src = "local n = 1;\n{ " + fields.mkString(", ") + " }"
      val sharedCache = new DefaultParseCache
      def manifest(): String = {
        val interp =
          new Interpreter(Map(), Map(), DummyPath(), Importer.empty, parseCache = sharedCache)
        interp.evaluate(src, DummyPath("(memory)")) match {
          case Right(v) =>
            interp.materialize(v, new Renderer()) match {
              case Right(w)  => w.toString
              case Left(err) => throw new Exception(Error.formatError(err))
            }
          case Left(err) => throw new Exception(Error.formatError(err))
        }
      }
      val expected = manifest()
      val failure = stress { _ =>
        var n = 0
        while (n < Iterations) {
          val got = manifest()
          if (got != expected) throw new java.lang.AssertionError("corrupted visible keys: " + got)
          n += 1
        }
      }
      failure.foreach(throw _)
    }

    // ---- Fix 5: StaticOptimizer folding a lazy array into the shared AST ----

    test("optimizerFoldsObjectValuesToEagerArray") {
      // std.objectValues(constObj) is a staticSafe builtin applied to a constant, so StaticOptimizer
      // folds it via tryStaticApply and embeds the result in the parse-cached AST shared across worker
      // threads. The fix materializes it to a plain eager Val.Arr instead of a LazyIndexedArr
      // (ObjectValuesArr), whose mutable per-index `slots` cache races across threads — the symptom
      // was a spurious "Infinite recursion detected" thrown from std.member iterating the shared array.
      val expr = parseOptimized(newInterpreter(), """std.objectValues({ a: 1, b: 2, c: 3 })""")
      assert(expr.isInstanceOf[Val.Arr])
      assert(!expr.isInstanceOf[Val.LazyIndexedArr]) // regression: a lazy array would still race
      // directBackingArray is non-null only for a plain, fully-materialized array (not lazy/concat/reversed)
      assert(expr.asInstanceOf[Val.Arr].directBackingArray != null)
    }

    test("optimizerFoldsArrayConcatToEagerArray") {
      // Constant array concat folds at optimization time. The fix folds to a plain eager Val.Arr, NOT a
      // lazy concat view, whose materialize() writes `arr` via a non-volatile, unsafely-published store
      // — racy when the folded view is shared across threads via the parse cache.
      val expr = parseOptimized(newInterpreter(), """[1, 2, 3] + [4, 5, 6]""")
      assert(expr.isInstanceOf[Val.Arr])
      val arr = expr.asInstanceOf[Val.Arr]
      assert(arr.directBackingArray != null) // materialized, not a concat view
      assert(arr.length == 6)
    }

    test("optimizerReusesSharedValsWithoutMutatingPositions") {
      val std = new StdLibModule().module
      assert(std.pos == null)
      val stdInterp =
        new Interpreter(Map(), Map(), DummyPath(), Importer.empty, new DefaultParseCache, std = std)
      assert(parseOptimized(stdInterp, "if true then std else {}") eq std)
      assert(std.pos == null)

      val truePos = Val.staticTrue.pos
      val falsePos = Val.staticFalse.pos
      val cachedThree = Val.cachedNum(new Position(null, -1), 3)
      val cachedThreePos = cachedThree.pos
      val sharedAsciiStr = Val.Str.asciiSafe(new Position(null, -1), "abc")
      val sharedAsciiStrPos = sharedAsciiStr.pos
      val staticNullPos = Val.staticNull.pos

      val foldedTrue = parseOptimized(newInterpreter(), "if true then std.all([]) else false")
      assert(foldedTrue.isInstanceOf[Val.True])
      assert(foldedTrue eq Val.staticTrue)
      assert(Val.staticTrue.pos eq truePos)

      val foldedFalse = parseOptimized(newInterpreter(), "if true then std.any([]) else true")
      assert(foldedFalse.isInstanceOf[Val.False])
      assert(foldedFalse eq Val.staticFalse)
      assert(Val.staticFalse.pos eq falsePos)

      val foldedNum = parseOptimized(newInterpreter(), """if true then std.length("abc") else 0""")
      assert(foldedNum.isInstanceOf[Val.Num])
      assert(foldedNum eq cachedThree)
      assert(cachedThree.pos eq cachedThreePos)

      val foldedStr = directOptimizer().optimize(
        Expr.IfElse(new Position(null, 123), Val.True(new Position(null, 124)), sharedAsciiStr, null)
      )
      assert(foldedStr.isInstanceOf[Val.AsciiSafeStr])
      assert(foldedStr.asInstanceOf[Val.Str].str == "abc")
      assert(foldedStr eq sharedAsciiStr)
      assert(sharedAsciiStr.pos eq sharedAsciiStrPos)

      val foldedNull = directOptimizer().optimize(
        Expr.IfElse(new Position(null, 125), Val.True(new Position(null, 126)), Val.staticNull, null)
      )
      assert(foldedNull.isInstanceOf[Val.Null])
      assert(foldedNull eq Val.staticNull)
      assert(Val.staticNull.pos eq staticNullPos)

      val lhsFalse = Val.False(new Position(null, 127))
      val lhsFalsePos = lhsFalse.pos
      val foldedAnd = directOptimizer().optimize(
        Expr.And(new Position(null, 128), lhsFalse, Val.Str(new Position(null, 129), "ignored"))
      )
      assert(foldedAnd eq lhsFalse)
      assert(lhsFalse.pos eq lhsFalsePos)

      val lhsTrue = Val.True(new Position(null, 130))
      val lhsTruePos = lhsTrue.pos
      val foldedOr = directOptimizer().optimize(
        Expr.Or(new Position(null, 131), lhsTrue, Val.Str(new Position(null, 132), "ignored"))
      )
      assert(foldedOr eq lhsTrue)
      assert(lhsTrue.pos eq lhsTruePos)
    }

    test("sharedFoldedObjectValuesConcurrentMember") {
      // Concurrent smoke companion to optimizerFoldsObjectValuesToEagerArray (the authoritative,
      // deterministic regression guard). A constant object's std.objectValues folds to ONE array shared
      // via the parse-cached AST; std.member forces its elements; many interpreters force the COLD
      // folded array together. NOTE: the LazyIndexedArr `slots` Computing-window for a static-object
      // element is tiny, so this seldom fails in-process even pre-fix — the production race needed
      // hundreds of concurrent compilations. It guards that the shared (eager, post-fix) array path
      // stays correct and consistent under load.
      val scopeFields = (0 until 16).map(i => s""""s$i": "v$i"""").mkString(", ")
      // Probe a value NOT present so std.member must force EVERY element (widest race window); the
      // result is always false.
      val src = s"""local scopes = { $scopeFields }; std.member(std.objectValues(scopes), "absent")"""
      val failure = stressFreshSharedParseCache(rounds = 200, src) { v =>
        if (v != ujson.Bool(false))
          throw new java.lang.AssertionError("unexpected std.member result: " + v)
      }
      failure.foreach(throw _)
    }

    // ---- Fix 6: shared static-object value0 unsafe publication (getValue0) ----

    test("sharedFoldedStaticObjectConcurrentAddSuper") {
      // A constant object folds to a shared Val.staticObject. Using it as an operand of `+` (object
      // merge) calls addSuper -> getValue0 on the shared static object, which lazily builds and
      // publishes value0; many interpreters trigger that cold build concurrently and @volatile value0
      // makes the publication safe. Like sharedMemberListConcurrentVisibleKeys this is a weak-memory
      // HARDENING / smoke test: on a strong (TSO) model such as x86 the unsafe publication is masked,
      // so it rarely fails pre-fix here — it guards the shared static-object merge path stays correct
      // and consistent (and matters on weak models, e.g. ARM). Assert every thread gets the same merge.
      val baseFields = (0 until 16).map(i => s""""k$i": $i""").mkString(", ")
      val src = s"""local base = { $baseFields }; { extra: 1 } + base"""
      val expected = newInterpreter().interpret(src, DummyPath("(memory)")) match {
        case Right(v)  => v
        case Left(err) => throw new Exception(err)
      }
      val failure = stressFreshSharedParseCache(rounds = 200, src) { v =>
        if (v != expected) throw new java.lang.AssertionError("unexpected merge result: " + v)
      }
      failure.foreach(throw _)
    }

    // ---- Fix 7: LazyExpr re-entrancy flag poisoning on exception ----

    test("lazyExprResetsEvaluatingFlagOnException") {
      // A LazyExpr whose computation throws must reset its `evaluating` re-entrancy flag (try/finally).
      // Otherwise a thunk reused across evaluations (e.g. a magic-import thunk a Bazel bundle worker
      // re-forces per context on one thread) stays poisoned and reports a spurious "self-referential
      // thunk" on every later force instead of re-raising the real error. Deterministic — no
      // concurrency needed: force the SAME thunk twice and require the real error both times.
      val interp = newInterpreter()
      val errExpr = parseOptimized(interp, """error "boom-marker"""")
      val thunk = new LazyExpr(errExpr, ValScope.empty, interp.evaluator)
      def force(): Throwable =
        try { thunk.value; null }
        catch { case t: Throwable => t }
      val first = force()
      assert(first != null)
      assert(String.valueOf(first.getMessage).contains("boom-marker"))
      val second = force() // re-force the would-be-poisoned thunk
      assert(second != null)
      // Regression: a poisoned thunk reports "self-referential thunk" here instead of the real error.
      assert(!String.valueOf(second.getMessage).contains("self-referential"))
      assert(String.valueOf(second.getMessage).contains("boom-marker")) // re-raises the real error
    }
  }
}
