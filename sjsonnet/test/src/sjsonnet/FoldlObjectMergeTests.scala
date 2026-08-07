package sjsonnet

import utest._
import TestUtils.{eval, evalErr}

/**
 * Tests for the `std.foldl` object-merge fast path (see `ArrayModule.FoldlObjectMerge`).
 *
 * Two things are checked:
 *   - Correctness: the optimized result must equal the standard object-composition semantics for
 *     the whole range of tricky cases (dedup, key overrides, non-empty init, `self` late-binding,
 *     `if/else` spines, asserts that read the accumulator, an `init` with its own super chain or
 *     assertions, ...).
 *   - Firing: the optimizer must actually emit the specialized builtin for the recognized patterns
 *     and must NOT emit it for patterns where in-place merging would be unsound (a reference to the
 *     accumulator inside a delta — direct or transitive, `super`/`+:`, an assert inside the delta,
 *     or a non-literal callback). Those still evaluate correctly via the generic fallback.
 */
object FoldlObjectMergeTests extends TestSuite {

  private val interp = new Interpreter(
    Map(),
    Map(),
    DummyPath(),
    Importer.empty,
    parseCache = new DefaultParseCache,
    Settings.default,
    std = sjsonnet.stdlib.StdLibModule.Default.module
  )

  /**
   * Records whether the optimized AST contains a call to the specialized object-merge foldl.
   * Intercepts array comprehensions because `rec`'s `Comp` case throws at runtime (it is dead code
   * in the real optimizer, which overrides it).
   */
  private final class FoldlMergeScanner extends ExprTransform {
    var found = false
    def transform(e: Expr): Expr = {
      e match {
        case Expr.ApplyBuiltin3(_, func, _, _, _, _)
            if func.getClass.getName.contains("FoldlObjectMerge") =>
          found = true
        case Expr.Comp(_, value, first, rest) =>
          transform(value)
          transform(first)
          var i = 0
          while (i < rest.length) { transform(rest(i)); i += 1 }
        case _ => rec(e)
      }
      e
    }
  }

  private def fires(code: String): Boolean = {
    val expr = interp.resolver
      .parse(DummyPath("(memory)"), StaticResolvedFile(code))(interp.evaluator)
      .fold(e => throw new Exception(e.toString), _._1)
    val s = new FoldlMergeScanner
    s.transform(expr)
    s.found
  }

  def tests: Tests = Tests {

    // -------------------------------------------------------------------------
    // Correctness of the recognized (fast-path) patterns
    // -------------------------------------------------------------------------
    test("correctness") {
      test("basicMerge") {
        eval("std.foldl(function(acc, t) acc { [t.k]: t.v }, " +
          "[{k:'a',v:1},{k:'b',v:2}], {})") ==> ujson.Obj("a" -> 1, "b" -> 2)
      }
      test("lastWriteWins") {
        eval("std.foldl(function(acc, t) acc { [t.k]: t.v }, " +
          "[{k:'a',v:1},{k:'a',v:9},{k:'b',v:2}], {})") ==> ujson.Obj("a" -> 9, "b" -> 2)
      }
      test("nonEmptyInit") {
        eval("std.foldl(function(acc, t) acc { [t]: true }, ['a','b'], {seed: 0})") ==>
          ujson.Obj("seed" -> 0, "a" -> true, "b" -> true)
      }
      test("initKeyOverridden") {
        eval("std.foldl(function(acc, t) acc { [t.k]: t.v }, [{k:'x',v:5}], {x: 0})") ==>
          ujson.Obj("x" -> 5)
      }
      test("emptyArrayReturnsInit") {
        eval("std.foldl(function(acc, t) acc { [t]: true }, [], {x: 1})") ==> ujson.Obj("x" -> 1)
      }
      test("singleElement") {
        eval("std.foldl(function(acc, t) acc { [t]: t }, ['a'], {})") ==> ujson.Obj("a" -> "a")
      }
      test("plusOperatorForm") {
        eval("std.foldl(function(acc, t) acc + { [t]: t }, ['x','y'], {})") ==>
          ujson.Obj("x" -> "x", "y" -> "y")
      }
      test("localBoundToElement") {
        // A `local` whose rhs depends on the element (not the accumulator) is fine.
        eval("std.foldl(function(acc, t) local k = t.id; acc { [k]: t.val }, " +
          "[{id:'a',val:1},{id:'b',val:2}], {})") ==> ujson.Obj("a" -> 1, "b" -> 2)
      }
      test("nestedObjectValue") {
        eval("std.foldl(function(acc, t) acc { [t.k]: { nested: t.v } }, [{k:'a',v:1}], {})") ==>
          ujson.Obj("a" -> ujson.Obj("nested" -> 1))
      }
      test("deltaWithArrayComprehension") {
        // A delta value containing an array comprehension (exercises the comprehension traversal
        // in the static reference scan).
        eval("std.foldl(function(acc, t) acc { [t.k]: [x * 2 for x in t.vs] }, " +
          "[{k:'a',vs:[1,2]},{k:'b',vs:[3]}], {})") ==>
          ujson.Obj("a" -> ujson.Arr(2, 4), "b" -> ujson.Arr(6))
      }
      test("deltaWithObjectComprehension") {
        eval("std.foldl(function(acc, t) acc { [t.k]: { [f]: true for f in t.fs } }, " +
          "[{k:'a',fs:['x','y']}], {})") ==>
          ujson.Obj("a" -> ujson.Obj("x" -> true, "y" -> true))
      }
      test("ifElseSkipBranch") {
        eval("std.foldl(function(acc, t) if t.skip then acc else acc { [t.k]: t.v }, " +
          "[{k:'a',v:1,skip:false},{k:'b',v:2,skip:true},{k:'c',v:3,skip:false}], {})") ==>
          ujson.Obj("a" -> 1, "c" -> 3)
      }
      test("selfLateBinding") {
        // `self` in a delta must late-bind to the final merged object, exactly as the standard
        // super-chain semantics require. Here `latest` is overwritten each step; the surviving one
        // reads `self['b']` against the fully merged object.
        eval("std.foldl(function(acc, t) acc { [t.k]: t.v, latest: self[t.k] }, " +
          "[{k:'a',v:1},{k:'b',v:2}], {})") ==>
          ujson.Obj("a" -> 1, "b" -> 2, "latest" -> 2)
      }
      test("dedupAssertReadsAccumulator") {
        // The canonical dedup pattern: the assert reads the accumulator, then the delta adds a key.
        eval("std.foldl(" +
          "function(acc, t) assert !std.objectHas(acc, t.k) || acc[t.k] == t.v : 'dup'; " +
          "acc { [t.k]: t.v }, [{k:'a',v:1},{k:'b',v:2},{k:'a',v:1}], {})") ==>
          ujson.Obj("a" -> 1, "b" -> 2)
      }
      test("dedupAssertStillFires") {
        // A genuine conflict must still raise the callback's assertion under the fast path.
        val err = evalErr("std.foldl(" +
          "function(acc, t) assert !std.objectHas(acc, t.k) || acc[t.k] == t.v : 'dup'; " +
          "acc { [t.k]: t.v }, [{k:'a',v:1},{k:'a',v:2}], {})")
        assert(err.contains("dup"))
      }
      test("assertReadsAccKeyCount") {
        // The stripped assert reads the accumulator's *key set* every step (not just objectHas):
        // `std.objectFields(acc)` hits the per-step key-name array. A fresh wrapper per step keeps
        // that array correct as the shared key union grows -- reusing one accumulator would freeze
        // the lazy `visibleKeyNames` at its first mid-fold read and make later steps observe stale
        // keys.
        eval("std.foldl(" +
          "function(acc, t) assert std.length(std.objectFields(acc)) == t - 1 : 'len'; " +
          "acc { [std.toString(t)]: t }, std.range(1, 5), {})") ==>
          ujson.Obj("1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5)
      }
      test("hiddenFieldDelta") {
        // A hidden (`::`) delta field is folded into the shared key union with the right
        // visibility: excluded from objectFields/materialization but present via objectFieldsAll.
        eval("std.foldl(function(acc, t) acc { [t]:: t }, ['a','b'], {c: 1})") ==> ujson.Obj("c" -> 1)
        eval("std.objectFieldsAll(" +
          "std.foldl(function(acc, t) acc { [t]:: t }, ['a','b'], {c: 1}))") ==>
          ujson.Arr("a", "b", "c")
      }
      test("initWithSuperChain") {
        // `init` keeps its own `super` chain: it becomes the accumulator's super rather than being
        // flattened, so all of its keys still show through.
        eval("std.foldl(function(acc, t) acc { [t]: t }, ['x'], {a: 1} + {b: 2})") ==>
          ujson.Obj("a" -> 1, "b" -> 2, "x" -> "x")
      }
      test("plusFormInitWithSuperChain") {
        eval("std.foldl(function(acc, t) acc + { [t]: t }, ['x', 'y'], {base: 0} + {seed: 1})") ==>
          ujson.Obj("base" -> 0, "seed" -> 1, "x" -> "x", "y" -> "y")
      }
      test("initWithPassingAssert") {
        // `init`'s assertions still fire against the merged object and pass when satisfied.
        eval("std.foldl(function(acc, t) acc { [t]: t }, ['x'], {assert self.a == 1, a: 1})") ==>
          ujson.Obj("a" -> 1, "x" -> "x")
      }
      test("initWithFailingAssert") {
        // An `init` assertion that fails must still raise, exactly as in naive evaluation.
        val err = evalErr(
          "std.foldl(function(acc, t) acc { [t]: t }, ['x'], {assert self.a == 2 : 'boom', a: 1})"
        )
        assert(err.contains("boom"))
      }
      test("initAssertObservesMergedField") {
        // `init`'s assert late-binds `self` to the fully merged object: a delta that overrides the
        // asserted key is observed by the assert, exactly as under naive super-chain evaluation.
        val err = evalErr(
          "std.foldl(function(acc, t) acc { a: 2 }, ['x'], {assert self.a == 1 : 'changed', a: 1})"
        )
        assert(err.contains("changed"))
      }
      test("nonObjectInitEmptyArray") {
        // Non-object `init` is left to the generic foldl; an empty array returns `init` unchanged.
        eval("std.foldl(function(acc, t) acc { [t]: t }, [], 5)") ==> ujson.Num(5)
      }
    }

    // -------------------------------------------------------------------------
    // The optimizer emits the specialized builtin for recognized patterns
    // -------------------------------------------------------------------------
    test("fires") {
      test("basicMerge")(assert(fires("std.foldl(function(acc, t) acc { [t.k]: t.v }, [], {})")))
      test("plusForm")(assert(fires("std.foldl(function(acc, t) acc + { [t]: t }, [], {})")))
      test("bareAccAndMerge")(
        assert(fires("std.foldl(function(acc, t) if t then acc else acc { x: 1 }, [], {})"))
      )
      test("localOnElement")(
        assert(fires("std.foldl(function(acc, t) local k = t.id; acc { [k]: t }, [], {})"))
      )
      test("assertReadsAcc")(
        assert(
          fires(
            "std.foldl(function(acc, t) assert !std.objectHas(acc, t) : 'e'; acc { [t]: 1 }, [], {})"
          )
        )
      )
      test("assertReadsAccKeys")(
        assert(
          fires(
            "std.foldl(function(acc, t) assert std.length(std.objectFields(acc)) == 0; " +
              "acc { [t]: 1 }, [], {})"
          )
        )
      )
      test("hiddenFieldDelta")(
        assert(fires("std.foldl(function(acc, t) acc { [t]:: t }, [], {})"))
      )
      test("selfInDelta")(
        assert(fires("std.foldl(function(acc, t) acc { [t]: self.x }, [], {x: 1})"))
      )
      test("comprehensionOnElement")(
        assert(fires("std.foldl(function(acc, t) acc { [t.k]: [x for x in t.vs] }, [], {})"))
      )
      // Firing depends only on the callback shape, so it fires regardless of `init`'s shape.
      test("initWithSuperChain")(
        assert(fires("std.foldl(function(acc, t) acc { [t]: t }, [], {a: 1} + {b: 2})"))
      )
      test("initWithAssert")(
        assert(fires("std.foldl(function(acc, t) acc { [t]: t }, [], {assert self.a == 1, a: 1})"))
      )
    }

    // -------------------------------------------------------------------------
    // The optimizer does NOT fire where in-place merging would be unsound; the
    // generic fallback must still produce the correct result.
    // -------------------------------------------------------------------------
    test("doesNotFire") {
      test("accInDeltaValue") {
        val code = "std.foldl(function(acc, t) " +
          "acc { total: (if std.objectHas(acc,'total') then acc.total else 0) + t }, [1,2,3], {})"
        assert(!fires(code))
        eval(code) ==> ujson.Obj("total" -> 6)
      }
      test("transitiveAccAlias") {
        val code = "std.foldl(function(acc, t) local m = acc; acc { seen: m.start }, ['a'], " +
          "{start: 5})"
        assert(!fires(code))
        eval(code) ==> ujson.Obj("start" -> 5, "seen" -> 5)
      }
      test("plusMergeField") {
        val code = "std.foldl(function(acc, t) acc { vals+: [t] }, [1,2], {vals: []})"
        assert(!fires(code))
        eval(code) ==> ujson.Obj("vals" -> ujson.Arr(1, 2))
      }
      test("explicitSuperRef") {
        val code = "std.foldl(function(acc, t) acc { first: super.a }, ['x'], {a: 1})"
        assert(!fires(code))
        eval(code) ==> ujson.Obj("a" -> 1, "first" -> 1)
      }
      test("assertInsideDelta") {
        val code = "std.foldl(function(acc, t) acc { assert t > 0, [std.toString(t)]: t }, [1,2], {})"
        assert(!fires(code))
        eval(code) ==> ujson.Obj("1" -> 1, "2" -> 2)
      }
      test("nonLiteralCallback") {
        val code = "local f = function(acc, t) acc { [t]: true }; std.foldl(f, ['a'], {})"
        assert(!fires(code))
        eval(code) ==> ujson.Obj("a" -> true)
      }
      test("comprehensionReadsAccumulator") {
        // A comprehension inside a delta that iterates over the accumulator must be rejected.
        val code = "std.foldl(function(acc, t) acc { [t]: [k for k in std.objectFields(acc)] }, " +
          "['a','b'], {})"
        assert(!fires(code))
        eval(code) ==> ujson.Obj("a" -> ujson.Arr(), "b" -> ujson.Arr("a"))
      }
    }
  }
}
