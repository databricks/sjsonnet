package sjsonnet

import utest._
import TestUtils.{eval, evalErr}

object TailCallOptimizationTests extends TestSuite {
  val tests: Tests = Tests {
    test("tailstrictFactorialSmall") {
      eval(
        """
          |local factorial(n, accum=1) =
          |  if n <= 1 then accum
          |  else factorial(n - 1, n * accum) tailstrict;
          |
          |factorial(10)
          |""".stripMargin
      ) ==> ujson.Num(3628800)
    }

    test("tailstrictFactorialOverflow") {
      // factorial(1000) overflows IEEE 754 double, sjsonnet should report overflow
      val err = evalErr(
        """
          |local factorial(n, accum=1) =
          |  if n <= 1 then accum
          |  else factorial(n - 1, n * accum) tailstrict;
          |
          |factorial(1000)
          |""".stripMargin
      )
      assert(err.contains("overflow"))
    }

    test("tailstrictDeepRecursionSum") {
      // Sum 1..10000 via tail-recursive accumulator — verifies TCO prevents stack overflow
      eval(
        """
          |local sum(n, accum=0) =
          |  if n <= 0 then accum
          |  else sum(n - 1, accum + n) tailstrict;
          |
          |local sz = 10000;
          |std.assertEqual(sum(sz), sz * (sz + 1) / 2)
          |""".stripMargin
      ) ==> ujson.True
    }

    test("tailstrictDeepRecursionCountdown") {
      // 100000 recursive calls — would blow the JVM stack without TCO
      eval(
        """
          |local countdown(n) =
          |  if n <= 0 then 0
          |  else countdown(n - 1) tailstrict;
          |
          |countdown(100000)
          |""".stripMargin
      ) ==> ujson.Num(0)
    }

    test("tailstrictWithDefaultParams") {
      // Verify tailstrict works correctly with default parameter values
      eval(
        """
          |local f(n, step=1, accum=0) =
          |  if n <= 0 then accum
          |  else f(n - step, accum=accum + n) tailstrict;
          |
          |f(100)
          |""".stripMargin
      ) ==> ujson.Num(5050)
    }

    test("tailstrictMutuallyIndirect") {
      // Tailstrict through if-else tail position propagation
      eval(
        """
          |local f(n, accum=0) =
          |  if n <= 0 then accum
          |  else if n % 2 == 0 then f(n - 1, accum + n) tailstrict
          |  else f(n - 1, accum + n) tailstrict;
          |
          |f(1000)
          |""".stripMargin
      ) ==> ujson.Num(500500)
    }

    test("tailstrictThroughLocal") {
      // Tailstrict call in tail position after local binding
      eval(
        """
          |local f(n, accum=0) =
          |  if n <= 0 then accum
          |  else
          |    local next = n - 1;
          |    local added = accum + n;
          |    f(next, added) tailstrict;
          |
          |f(10000)
          |""".stripMargin
      ) ==> ujson.Num(50005000)
    }

    test("tailstrictThroughAssert") {
      // Tailstrict call in tail position after assert
      eval(
        """
          |local f(n, accum=0) =
          |  assert n >= 0 : "n must be non-negative";
          |  if n == 0 then accum
          |  else f(n - 1, accum + n) tailstrict;
          |
          |f(1000)
          |""".stripMargin
      ) ==> ujson.Num(500500)
    }

    test("tailstrictBuiltinHigherOrder") {
      // Verify that a builtin higher-order function (std.makeArray) correctly resolves
      // TailCall produced by a user callback that uses tailstrict internally.
      // std.makeArray calls the callback with TailstrictModeDisabled, so the callback's
      // own Val.Func.apply* must resolve any TailCall before returning to the builtin.
      eval(
        """
          |local double(n, accum=0) =
          |  if n <= 0 then accum
          |  else double(n - 1, accum + 2) tailstrict;
          |
          |std.makeArray(5, function(i) double(i))
          |""".stripMargin
      ) ==> ujson.Arr(ujson.Num(0), ujson.Num(2), ujson.Num(4), ujson.Num(6), ujson.Num(8))
    }

    test("tailstrictBuiltinFilterDirectTailstrict") {
      // Regression: the predicate's function body is *directly* a tailstrict call
      // (not wrapped in a non-tailstrict intermediate call). When std.filter's
      // scope-reuse fast path calls evalRhs, visitExprWithTailCallSupport returns
      // a TailCall sentinel because the outermost expression is `tailstrict`.
      // Without evalRhsResolved, .asBoolean would fail on the TailCall sentinel.
      eval(
        """
          |local identity(x) = x;
          |local pred(x) = identity(x > 0) tailstrict;
          |std.filter(pred, [1, -1, 2, -3, 4])
          |""".stripMargin
      ) ==> ujson.Arr(ujson.Num(1), ujson.Num(2), ujson.Num(4))
    }

    test("tailstrictBuiltinFilterDirectTailstrictAllPass") {
      // All elements pass — exercises the first evalRhs call site (line 129)
      // where the predicate body is directly a tailstrict call.
      eval(
        """
          |local identity(x) = x;
          |local pred(x) = identity(x > 0) tailstrict;
          |std.filter(pred, [1, 2, 3])
          |""".stripMargin
      ) ==> ujson.Arr(ujson.Num(1), ujson.Num(2), ujson.Num(3))
    }

    test("tailstrictBuiltinFilterDirectTailstrictAllReject") {
      // All elements rejected — exercises both call sites with a predicate
      // whose body is directly a tailstrict call returning false.
      eval(
        """
          |local identity(x) = x;
          |local pred(x) = identity(x < 0) tailstrict;
          |std.filter(pred, [1, 2, 3])
          |""".stripMargin
      ) ==> ujson.Arr()
    }
    test("tailstrictZeroArgs") {
      // Apply0: zero-argument tailstrict call
      eval(
        """
          |local x() = 42;
          |x() tailstrict
          |""".stripMargin
      ) ==> ujson.Num(42)
    }

    test("tailstrictThreeArgs") {
      // Apply3: three-argument tailstrict call with deep recursion
      eval(
        """
          |local f(n, a, b) =
          |  if n <= 0 then a + b
          |  else f(n - 1, a + 1, b + 1) tailstrict;
          |
          |f(10000, 0, 0)
          |""".stripMargin
      ) ==> ujson.Num(20000)
    }

    test("tailstrictNamedArgs") {
      // Apply with named arguments in tailstrict call
      eval(
        """
          |local f(n, accum=0) =
          |  if n <= 0 then accum
          |  else f(accum=accum + n, n=n - 1) tailstrict;
          |
          |f(100)
          |""".stripMargin
      ) ==> ujson.Num(5050)
    }

    test("tailstrictEagerParamEvaluation") {
      // tailstrict forces eager evaluation of arguments — error in unused param should trigger
      val err = evalErr(
        """
          |local f(x, y) = x;
          |f(42, error "kaboom") tailstrict
          |""".stripMargin
      )
      assert(err.contains("kaboom"))
    }

    test("nonTailstrictLazyParams") {
      // Without tailstrict, unused error param should NOT trigger (lazy evaluation)
      eval(
        """
          |local f(x, y) = x;
          |f(42, error "kaboom")
          |""".stripMargin
      ) ==> ujson.Num(42)
    }

    test("tailstrictErrorStackFrame") {
      // Errors inside tailstrict calls should preserve meaningful stack frames
      val err = evalErr(
        """
          |local f(n) =
          |  if n <= 0 then error "reached zero"
          |  else f(n - 1) tailstrict;
          |
          |f(3)
          |""".stripMargin
      )
      assert(err.contains("reached zero"))
    }

    test("tailstrictChainedCalls") {
      // Mutual recursion via object methods — Jsonnet's local bindings are sequential,
      // so we use an object to allow even/odd to reference each other.
      eval(
        """
          |local fns = {
          |  even(n)::
          |    if n == 0 then true
          |    else fns.odd(n - 1) tailstrict,
          |  odd(n)::
          |    if n == 0 then false
          |    else fns.even(n - 1) tailstrict,
          |};
          |
          |fns.even(1000)
          |""".stripMargin
      ) ==> ujson.True
    }

    // ---- Materializer integration tests ----
    // These verify that TailCall sentinels never leak into the Materializer.
    // If a TailCall escapes, the Materializer would hit "Unknown value type tailcall"
    // instead of producing valid JSON.

    test("materializeObjectFieldFromTailstrict") {
      // Object field value computed via tailstrict recursion — Materializer must see
      // the resolved Val, not a TailCall sentinel.
      eval(
        """
          |local sum(n, accum=0) =
          |  if n <= 0 then accum
          |  else sum(n - 1, accum + n) tailstrict;
          |
          |{ result: sum(100) }
          |""".stripMargin
      ) ==> ujson.Obj("result" -> ujson.Num(5050))
    }

    test("materializeArrayElementFromTailstrict") {
      // Array element computed via tailstrict recursion — each element must be
      // fully resolved before the Materializer iterates over the array.
      eval(
        """
          |local fib(n, a=0, b=1) =
          |  if n <= 0 then a
          |  else fib(n - 1, b, a + b) tailstrict;
          |
          |[fib(0), fib(1), fib(5), fib(10)]
          |""".stripMargin
      ) ==> ujson.Arr(ujson.Num(0), ujson.Num(1), ujson.Num(5), ujson.Num(55))
    }

    test("materializeNestedObjectFromTailstrict") {
      // Deeply nested object where multiple fields are computed via tailstrict.
      // Tests that the iterative Materializer stack correctly handles resolved values
      // at every nesting level.
      eval(
        """
          |local countdown(n) =
          |  if n <= 0 then 0
          |  else countdown(n - 1) tailstrict;
          |
          |{
          |  outer: {
          |    inner: {
          |      value: countdown(1000),
          |    },
          |    sibling: countdown(500),
          |  },
          |}
          |""".stripMargin
      ) ==> ujson.Obj(
        "outer" -> ujson.Obj(
          "inner" -> ujson.Obj("value" -> ujson.Num(0)),
          "sibling" -> ujson.Num(0)
        )
      )
    }

    test("materializeMixedContainerFromTailstrict") {
      // Mixed array-of-objects where both container types contain tailstrict-computed values.
      // Exercises the Materializer's MaterializeObjFrame/MaterializeArrFrame stack interleaving.
      eval(
        """
          |local double(n, accum=0) =
          |  if n <= 0 then accum
          |  else double(n - 1, accum + 2) tailstrict;
          |
          |[
          |  { x: double(3) },
          |  { x: double(5) },
          |]
          |""".stripMargin
      ) ==> ujson.Arr(
        ujson.Obj("x" -> ujson.Num(6)),
        ujson.Obj("x" -> ujson.Num(10))
      )
    }

    test("materializeLazyFieldFromTailstrict") {
      // Object field that is lazily evaluated — the tailstrict call happens inside
      // a Lazy thunk that is only forced when the Materializer accesses the field.
      eval(
        """
          |local sum(n, accum=0) =
          |  if n <= 0 then accum
          |  else sum(n - 1, accum + n) tailstrict;
          |
          |local obj = { a: sum(50), b: sum(100) };
          |[obj.a, obj.b]
          |""".stripMargin
      ) ==> ujson.Arr(ujson.Num(1275), ujson.Num(5050))
    }

    test("materializeStringifyFromTailstrict") {
      // std.toString forces materialization to string — verifies TailCall is resolved
      // before the Renderer visitor processes the value.
      eval(
        """
          |local repeat(n, s="", accum="") =
          |  if n <= 0 then accum
          |  else repeat(n - 1, s, accum + s) tailstrict;
          |
          |std.toString({ msg: repeat(3, "ab") })
          |""".stripMargin
      ) ==> ujson.Str("""{"msg": "ababab"}""")
    }

    test("tailstrictTwoArgs") {
      // Apply2: two-argument tailstrict call with deep recursion — exercises the
      // visitApply2 / Val.Func.apply2 code path specifically.
      eval(
        """
          |local gcd(a, b) =
          |  if b == 0 then a
          |  else gcd(b, a % b) tailstrict;
          |
          |[gcd(48, 18), gcd(100, 75), gcd(17, 13)]
          |""".stripMargin
      ) ==> ujson.Arr(ujson.Num(6), ujson.Num(25), ujson.Num(1))
    }

    test("tailstrictNonTailPosition") {
      // tailstrict call in non-tail position (bound to a local variable).
      // The call is NOT in tail position of the enclosing function, so it goes through
      // visitApply* (not visitExprWithTailCallSupport). TailCall.resolve in visitApply* must
      // still resolve any TailCall chain produced by the callee.
      eval(
        """
          |local sum(n, accum=0) =
          |  if n <= 0 then accum
          |  else sum(n - 1, accum + n) tailstrict;
          |
          |local result = sum(10000);
          |result + 1
          |""".stripMargin
      ) ==> ujson.Num(50005001)
    }

    test("tailstrictBuiltinFoldl") {
      // std.foldl invokes a user callback with TailstrictModeDisabled.
      // The callback itself uses tailstrict recursion internally — verifies that
      // Val.Func.apply* resolves TailCall before returning to the builtin.
      eval(
        """
          |local power(base, exp, accum=1) =
          |  if exp <= 0 then accum
          |  else power(base, exp - 1, accum * base) tailstrict;
          |
          |std.foldl(function(acc, x) acc + power(2, x), [0, 1, 2, 3, 4], 0)
          |""".stripMargin
      ) ==> ujson.Num(31)
    }

    test("tailstrictReturnsContainer") {
      // Tail-recursive function that returns an object/array at the base case.
      // Verifies that TailCall.resolve correctly resolves to a container value
      // that the Materializer can then process without issues.
      eval(
        """
          |local buildList(n, accum=[]) =
          |  if n <= 0 then accum
          |  else buildList(n - 1, accum + [n]) tailstrict;
          |
          |local buildObj(n, accum={}) =
          |  if n <= 0 then accum
          |  else buildObj(n - 1, accum { ["k" + n]: n }) tailstrict;
          |
          |{
          |  list: buildList(5),
          |  obj: buildObj(3),
          |}
          |""".stripMargin
      ) ==> ujson.Obj(
        "list" -> ujson.Arr(
          ujson.Num(5),
          ujson.Num(4),
          ujson.Num(3),
          ujson.Num(2),
          ujson.Num(1)
        ),
        "obj" -> ujson.Obj(
          "k3" -> ujson.Num(3),
          "k2" -> ujson.Num(2),
          "k1" -> ujson.Num(1)
        )
      )
    }
  }
}
