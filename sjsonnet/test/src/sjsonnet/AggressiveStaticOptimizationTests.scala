package sjsonnet

import utest._
import TestUtils.{eval, evalErr}

/**
 * Tests for `aggressiveStaticOptimization = true`.
 *
 * Covers every optimization branch in [[StaticOptimizer.tryAggressiveOptimize]], which runs during
 * the optimization phase (not parse time):
 *   - Constant folding: arithmetic (+, -, *, /, %), comparison (<, >, <=, >=, ==, !=), bitwise (&,
 *     ^, |), shift (<<, >>), unary (!, -, ~, +), string/array concatenation.
 *   - Branch elimination: if-else with constant condition.
 *   - Short-circuit elimination: And/Or with constant lhs.
 *
 * Each case is verified to produce the same result as without the optimization, confirming that the
 * optimization is semantics-preserving.
 */
object AggressiveStaticOptimizationTests extends TestSuite {

  /** Shorthand: evaluate with aggressiveStaticOptimization enabled. */
  def evalOpt(s: String): ujson.Value =
    eval(s, aggressiveStaticOptimization = true)

  /** Shorthand: evaluate and expect an error with aggressiveStaticOptimization enabled. */
  def evalErrOpt(s: String): String =
    evalErr(s, aggressiveStaticOptimization = true)

  def tests: Tests = Tests {

    // -------------------------------------------------------------------------
    // Arithmetic constant folding
    // -------------------------------------------------------------------------
    test("constantFolding") {
      test("addNumbers") {
        evalOpt("1 + 2") ==> ujson.Num(3)
        evalOpt("1.5 + 2.5") ==> ujson.Num(4)
      }
      test("subtractNumbers") {
        evalOpt("10 - 3") ==> ujson.Num(7)
        evalOpt("0 - 5") ==> ujson.Num(-5)
      }
      test("multiplyNumbers") {
        evalOpt("3 * 4") ==> ujson.Num(12)
        evalOpt("2.5 * 2") ==> ujson.Num(5)
      }
      test("divideNumbers") {
        evalOpt("10 / 4") ==> ujson.Num(2.5)
        evalOpt("9 / 3") ==> ujson.Num(3)
      }
      test("moduloNumbers") {
        evalOpt("10 % 3") ==> ujson.Num(1)
        evalOpt("7 % 7") ==> ujson.Num(0)
      }
      test("addStrings") {
        evalOpt(""" "hello" + " world" """) ==> ujson.Str("hello world")
        evalOpt(""" "foo" + "bar" """) ==> ujson.Str("foobar")
      }
      test("addArrays") {
        evalOpt("[1, 2] + [3, 4]") ==> ujson.Arr(1, 2, 3, 4)
        evalOpt("[] + [1]") ==> ujson.Arr(1)
      }
    }

    // -------------------------------------------------------------------------
    // Unary operator constant folding
    // -------------------------------------------------------------------------
    test("unaryConstantFolding") {
      test("logicalNot") {
        evalOpt("!true") ==> ujson.False
        evalOpt("!false") ==> ujson.True
      }
      test("negateNumber") {
        evalOpt("-5") ==> ujson.Num(-5)
        evalOpt("-(-3)") ==> ujson.Num(3)
      }
      test("bitwiseNot") {
        evalOpt("~0") ==> ujson.Num(-1)
        evalOpt("~(-1)") ==> ujson.Num(0)
      }
      test("unaryPlus") {
        evalOpt("+7") ==> ujson.Num(7)
      }
    }

    // -------------------------------------------------------------------------
    // Comparison constant folding
    // -------------------------------------------------------------------------
    test("comparisonConstantFolding") {
      test("lessThan") {
        evalOpt("1 < 2") ==> ujson.True
        evalOpt("2 < 1") ==> ujson.False
        evalOpt("1 < 1") ==> ujson.False
      }
      test("greaterThan") {
        evalOpt("2 > 1") ==> ujson.True
        evalOpt("1 > 2") ==> ujson.False
        evalOpt("1 > 1") ==> ujson.False
      }
      test("lessThanOrEqual") {
        evalOpt("1 <= 1") ==> ujson.True
        evalOpt("1 <= 2") ==> ujson.True
        evalOpt("2 <= 1") ==> ujson.False
      }
      test("greaterThanOrEqual") {
        evalOpt("1 >= 1") ==> ujson.True
        evalOpt("2 >= 1") ==> ujson.True
        evalOpt("1 >= 2") ==> ujson.False
      }
      test("equalNumbers") {
        evalOpt("1 == 1") ==> ujson.True
        evalOpt("1 == 2") ==> ujson.False
      }
      test("notEqualNumbers") {
        evalOpt("1 != 2") ==> ujson.True
        evalOpt("1 != 1") ==> ujson.False
      }
      test("equalStrings") {
        evalOpt(""" "abc" == "abc" """) ==> ujson.True
        evalOpt(""" "abc" == "def" """) ==> ujson.False
      }
      test("notEqualStrings") {
        evalOpt(""" "abc" != "def" """) ==> ujson.True
        evalOpt(""" "abc" != "abc" """) ==> ujson.False
      }
      test("equalBooleans") {
        evalOpt("true == true") ==> ujson.True
        evalOpt("false == false") ==> ujson.True
        evalOpt("true == false") ==> ujson.False
      }
      test("equalNull") {
        evalOpt("null == null") ==> ujson.True
        evalOpt("null != null") ==> ujson.False
      }
      test("stringComparison") {
        evalOpt(""" "abc" < "abd" """) ==> ujson.True
        evalOpt(""" "b" > "a" """) ==> ujson.True
        evalOpt(""" "abc" <= "abc" """) ==> ujson.True
        evalOpt(""" "abc" >= "abc" """) ==> ujson.True
      }
    }

    // -------------------------------------------------------------------------
    // Bitwise and shift constant folding
    // -------------------------------------------------------------------------
    test("bitwiseConstantFolding") {
      test("bitwiseAnd") {
        evalOpt("12 & 10") ==> ujson.Num(8)
        evalOpt("0 & 255") ==> ujson.Num(0)
      }
      test("bitwiseXor") {
        evalOpt("12 ^ 10") ==> ujson.Num(6)
        evalOpt("0 ^ 0") ==> ujson.Num(0)
      }
      test("bitwiseOr") {
        evalOpt("12 | 10") ==> ujson.Num(14)
        evalOpt("0 | 0") ==> ujson.Num(0)
      }
      test("shiftLeft") {
        evalOpt("1 << 3") ==> ujson.Num(8)
        evalOpt("3 << 2") ==> ujson.Num(12)
      }
      test("shiftRight") {
        evalOpt("8 >> 2") ==> ujson.Num(2)
        evalOpt("16 >> 4") ==> ujson.Num(1)
      }
    }

    // -------------------------------------------------------------------------
    // Branch elimination: if-else with constant condition
    // -------------------------------------------------------------------------
    test("branchElimination") {
      test("trueConditionSelectsThenBranch") {
        evalOpt("if true then 42 else 0") ==> ujson.Num(42)
        evalOpt("if true then 'yes' else 'no'") ==> ujson.Str("yes")
      }
      test("falseConditionSelectsElseBranch") {
        evalOpt("if false then 42 else 0") ==> ujson.Num(0)
        evalOpt("if false then 'yes' else 'no'") ==> ujson.Str("no")
      }
      test("falseConditionWithNoElseYieldsNull") {
        // `if false then expr` with no else branch should yield null
        evalOpt("if false then 42") ==> ujson.Null
      }
      test("nestedBranchElimination") {
        evalOpt("if true then (if false then 1 else 2) else 3") ==> ujson.Num(2)
      }
    }

    // -------------------------------------------------------------------------
    // Short-circuit elimination for And / Or
    // -------------------------------------------------------------------------
    test("shortCircuitElimination") {
      test("trueAndTrue") {
        evalOpt("true && true") ==> ujson.True
      }
      test("trueAndFalse") {
        evalOpt("true && false") ==> ujson.False
      }
      test("falseAndAnything") {
        // false && rhs should short-circuit to false regardless of rhs
        evalOpt("false && true") ==> ujson.False
        evalOpt("false && false") ==> ujson.False
      }
      test("trueOrAnything") {
        // true || rhs should short-circuit to true regardless of rhs
        evalOpt("true || false") ==> ujson.True
        evalOpt("true || true") ==> ujson.True
      }
      test("falseOrTrue") {
        evalOpt("false || true") ==> ujson.True
      }
      test("falseOrFalse") {
        evalOpt("false || false") ==> ujson.False
      }
    }

    // -------------------------------------------------------------------------
    // Semantics-preserving: results must match the non-optimized evaluator
    // -------------------------------------------------------------------------
    test("semanticsMatch") {
      val expressions = Seq(
        "1 + 2",
        "10 - 3",
        "3 * 4",
        "10 / 4",
        "10 % 3",
        "!true",
        "!false",
        "-5",
        "~0",
        "+7",
        "1 < 2",
        "2 > 1",
        "1 <= 1",
        "2 >= 1",
        "1 == 1",
        "1 != 2",
        "12 & 10",
        "12 ^ 10",
        "12 | 10",
        "1 << 3",
        "8 >> 2",
        "if true then 1 else 0",
        "if false then 1 else 0",
        "if false then 1",
        "true && true",
        "true && false",
        "false && true",
        "true || false",
        "false || true",
        "false || false"
      )
      for (expr <- expressions) {
        val withOpt = eval(expr, aggressiveStaticOptimization = true)
        val withoutOpt = eval(expr, aggressiveStaticOptimization = false)
        withOpt ==> withoutOpt
      }
    }

    // -------------------------------------------------------------------------
    // Edge cases: optimizations that must NOT fire (non-constant operands)
    // -------------------------------------------------------------------------
    test("nonConstantOperandsNotFolded") {
      // Variables are not statically known values; the optimizer must not fold these.
      evalOpt("local x = 3; local y = 4; x + y") ==> ujson.Num(7)
      evalOpt("local b = true; if b then 1 else 0") ==> ujson.Num(1)
      evalOpt("local b = false; b || true") ==> ujson.True
    }

    // -------------------------------------------------------------------------
    // Error cases: runtime errors must still be raised correctly
    // -------------------------------------------------------------------------
    test("runtimeErrorsPreserved") {
      test("divisionByZeroNotFolded") {
        // Division by zero: the optimizer must NOT fold `1 / 0` into a value;
        // it should fall back to the runtime error path.
        val err = evalErrOpt("1 / 0")
        assert(err.contains("sjsonnet.Error"))
      }
      test("negativeShiftNotFolded") {
        // Negative shift amounts must not be constant-folded; runtime error expected.
        val err = evalErrOpt("1 << -1")
        assert(err.contains("sjsonnet.Error"))
      }
      test("andWithNonBoolRhsStillErrors") {
        // `true && "hello"` must still error: the optimizer only short-circuits when
        // rhs is a Val.Bool. If rhs is not a Bool, the BinaryOp is left intact and
        // the runtime type-check fires.
        val err = evalErrOpt(""" true && "hello" """)
        assert(err.contains("binary operator &&"))
      }
      test("orWithNonBoolRhsStillErrors") {
        val err = evalErrOpt(""" false || "hello" """)
        assert(err.contains("binary operator ||"))
      }
    }

    // -------------------------------------------------------------------------
    // Interaction with other settings: aggressiveStaticOptimization + useNewEvaluator
    // -------------------------------------------------------------------------
    test("withNewEvaluator") {
      def evalBoth(s: String): ujson.Value =
        eval(s, aggressiveStaticOptimization = true, useNewEvaluator = true)

      evalBoth("1 + 2") ==> ujson.Num(3)
      evalBoth("if true then 'yes' else 'no'") ==> ujson.Str("yes")
      evalBoth("true && false") ==> ujson.False
      evalBoth("false || true") ==> ujson.True
      evalBoth("~0") ==> ujson.Num(-1)
      evalBoth("12 & 10") ==> ujson.Num(8)
    }
  }
}
