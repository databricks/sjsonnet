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
  }
}
