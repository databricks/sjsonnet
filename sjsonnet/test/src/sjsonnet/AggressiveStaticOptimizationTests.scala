package sjsonnet

import utest._
import TestUtils.{eval, evalErr}

/**
 * Tests for aggressive static optimizations in [[StaticOptimizer.tryAggressiveOptimize]].
 *
 * Covers every optimization branch:
 *   - Constant folding: arithmetic (+, -, *, /, %), comparison (<, >, <=, >=, ==, !=), bitwise (&,
 *     ^, |), shift (<<, >>), unary (!, -, ~, +), string/array concatenation.
 *   - Branch elimination: if-else with constant condition.
 *   - Short-circuit elimination: And/Or with constant lhs.
 */
object AggressiveStaticOptimizationTests extends TestSuite {

  def tests: Tests = Tests {

    // -------------------------------------------------------------------------
    // Arithmetic constant folding
    // -------------------------------------------------------------------------
    test("constantFolding") {
      test("addNumbers") {
        eval("1 + 2") ==> ujson.Num(3)
        eval("1.5 + 2.5") ==> ujson.Num(4)
      }
      test("subtractNumbers") {
        eval("10 - 3") ==> ujson.Num(7)
        eval("0 - 5") ==> ujson.Num(-5)
      }
      test("multiplyNumbers") {
        eval("3 * 4") ==> ujson.Num(12)
        eval("2.5 * 2") ==> ujson.Num(5)
      }
      test("divideNumbers") {
        eval("10 / 4") ==> ujson.Num(2.5)
        eval("9 / 3") ==> ujson.Num(3)
      }
      test("moduloNumbers") {
        eval("10 % 3") ==> ujson.Num(1)
        eval("7 % 7") ==> ujson.Num(0)
      }
      test("addStrings") {
        eval(""" "hello" + " world" """) ==> ujson.Str("hello world")
        eval(""" "foo" + "bar" """) ==> ujson.Str("foobar")
      }
      test("addArrays") {
        eval("[1, 2] + [3, 4]") ==> ujson.Arr(1, 2, 3, 4)
        eval("[] + [1]") ==> ujson.Arr(1)
      }
    }

    // -------------------------------------------------------------------------
    // Unary operator constant folding
    // -------------------------------------------------------------------------
    test("unaryConstantFolding") {
      test("logicalNot") {
        eval("!true") ==> ujson.False
        eval("!false") ==> ujson.True
      }
      test("negateNumber") {
        eval("-5") ==> ujson.Num(-5)
        eval("-(-3)") ==> ujson.Num(3)
      }
      test("bitwiseNot") {
        eval("~0") ==> ujson.Num(-1)
        eval("~(-1)") ==> ujson.Num(0)
      }
      test("unaryPlus") {
        eval("+7") ==> ujson.Num(7)
      }
    }

    // -------------------------------------------------------------------------
    // Comparison constant folding
    // -------------------------------------------------------------------------
    test("comparisonConstantFolding") {
      test("lessThan") {
        eval("1 < 2") ==> ujson.True
        eval("2 < 1") ==> ujson.False
        eval("1 < 1") ==> ujson.False
      }
      test("greaterThan") {
        eval("2 > 1") ==> ujson.True
        eval("1 > 2") ==> ujson.False
        eval("1 > 1") ==> ujson.False
      }
      test("lessThanOrEqual") {
        eval("1 <= 1") ==> ujson.True
        eval("1 <= 2") ==> ujson.True
        eval("2 <= 1") ==> ujson.False
      }
      test("greaterThanOrEqual") {
        eval("1 >= 1") ==> ujson.True
        eval("2 >= 1") ==> ujson.True
        eval("1 >= 2") ==> ujson.False
      }
      test("equalNumbers") {
        eval("1 == 1") ==> ujson.True
        eval("1 == 2") ==> ujson.False
      }
      test("notEqualNumbers") {
        eval("1 != 2") ==> ujson.True
        eval("1 != 1") ==> ujson.False
      }
      test("equalStrings") {
        eval(""" "abc" == "abc" """) ==> ujson.True
        eval(""" "abc" == "def" """) ==> ujson.False
      }
      test("notEqualStrings") {
        eval(""" "abc" != "def" """) ==> ujson.True
        eval(""" "abc" != "abc" """) ==> ujson.False
      }
      test("equalBooleans") {
        eval("true == true") ==> ujson.True
        eval("false == false") ==> ujson.True
        eval("true == false") ==> ujson.False
      }
      test("equalNull") {
        eval("null == null") ==> ujson.True
        eval("null != null") ==> ujson.False
      }
      test("stringComparison") {
        eval(""" "abc" < "abd" """) ==> ujson.True
        eval(""" "b" > "a" """) ==> ujson.True
        eval(""" "abc" <= "abc" """) ==> ujson.True
        eval(""" "abc" >= "abc" """) ==> ujson.True
      }
    }

    // -------------------------------------------------------------------------
    // Bitwise and shift constant folding
    // -------------------------------------------------------------------------
    test("bitwiseConstantFolding") {
      test("bitwiseAnd") {
        eval("12 & 10") ==> ujson.Num(8)
        eval("0 & 255") ==> ujson.Num(0)
      }
      test("bitwiseXor") {
        eval("12 ^ 10") ==> ujson.Num(6)
        eval("0 ^ 0") ==> ujson.Num(0)
      }
      test("bitwiseOr") {
        eval("12 | 10") ==> ujson.Num(14)
        eval("0 | 0") ==> ujson.Num(0)
      }
      test("shiftLeft") {
        eval("1 << 3") ==> ujson.Num(8)
        eval("3 << 2") ==> ujson.Num(12)
      }
      test("shiftRight") {
        eval("8 >> 2") ==> ujson.Num(2)
        eval("16 >> 4") ==> ujson.Num(1)
      }
    }

    // -------------------------------------------------------------------------
    // Branch elimination: if-else with constant condition
    // -------------------------------------------------------------------------
    test("branchElimination") {
      test("trueConditionSelectsThenBranch") {
        eval("if true then 42 else 0") ==> ujson.Num(42)
        eval("if true then 'yes' else 'no'") ==> ujson.Str("yes")
      }
      test("falseConditionSelectsElseBranch") {
        eval("if false then 42 else 0") ==> ujson.Num(0)
        eval("if false then 'yes' else 'no'") ==> ujson.Str("no")
      }
      test("falseConditionWithNoElseYieldsNull") {
        // `if false then expr` with no else branch should yield null
        eval("if false then 42") ==> ujson.Null
      }
      test("nestedBranchElimination") {
        eval("if true then (if false then 1 else 2) else 3") ==> ujson.Num(2)
      }
    }

    // -------------------------------------------------------------------------
    // Short-circuit elimination for And / Or
    // -------------------------------------------------------------------------
    test("shortCircuitElimination") {
      test("trueAndTrue") {
        eval("true && true") ==> ujson.True
      }
      test("trueAndFalse") {
        eval("true && false") ==> ujson.False
      }
      test("falseAndAnything") {
        // false && rhs should short-circuit to false regardless of rhs
        eval("false && true") ==> ujson.False
        eval("false && false") ==> ujson.False
      }
      test("trueOrAnything") {
        // true || rhs should short-circuit to true regardless of rhs
        eval("true || false") ==> ujson.True
        eval("true || true") ==> ujson.True
      }
      test("falseOrTrue") {
        eval("false || true") ==> ujson.True
      }
      test("falseOrFalse") {
        eval("false || false") ==> ujson.False
      }
    }

    // -------------------------------------------------------------------------
    // Edge cases: optimizations that must NOT fire (non-constant operands)
    // -------------------------------------------------------------------------
    test("nonConstantOperandsNotFolded") {
      // Variables are not statically known values; the optimizer must not fold these.
      eval("local x = 3; local y = 4; x + y") ==> ujson.Num(7)
      eval("local b = true; if b then 1 else 0") ==> ujson.Num(1)
      eval("local b = false; b || true") ==> ujson.True
    }

    // -------------------------------------------------------------------------
    // Error cases: runtime errors must still be raised correctly
    // -------------------------------------------------------------------------
    test("runtimeErrorsPreserved") {
      def assertErrContainsBoth(input: String, expected: String): Unit = {
        val err = evalErr(input)
        assert(err.contains(expected))

        val newEvaluatorErr = evalErr(input, useNewEvaluator = true)
        assert(newEvaluatorErr.contains(expected))
      }

      test("divisionByZeroNotFolded") {
        // Division by zero: the optimizer must NOT fold `1 / 0` into a value;
        // it should fall back to the runtime error path.
        assertErrContainsBoth("1 / 0", "sjsonnet.Error")
      }
      test("negativeShiftNotFolded") {
        // Negative shift amounts must not be constant-folded; runtime error expected.
        assertErrContainsBoth("1 << -1", "sjsonnet.Error")
      }
      test("moduloByZeroNotFolded") {
        assertErrContainsBoth("1 % 0", "not a number")
      }
      test("andWithNonBoolRhsStillErrors") {
        // `true && "hello"` must still error: the optimizer only short-circuits when
        // rhs is a Val.Bool. If rhs is not a Bool, the BinaryOp is left intact and
        // the runtime type-check fires.
        assertErrContainsBoth(""" true && "hello" """, "binary operator &&")
      }
      test("orWithNonBoolRhsStillErrors") {
        assertErrContainsBoth(""" false || "hello" """, "binary operator ||")
      }
      test("constantBitwiseNaNStillErrors") {
        assertErrContainsBoth("0 & (0 % 0)", "numeric value is not finite")
      }
      test("dynamicBitwiseNaNStillErrors") {
        assertErrContainsBoth("local x = 0; 0 & (x % x)", "numeric value is not finite")
      }
      test("constantShiftNaNStillErrors") {
        assertErrContainsBoth("1 << (0 % 0)", "numeric value is not finite")
      }
      test("rhsErrorsStillWinOverTypeMismatch") {
        assertErrContainsBoth(""" "a" * error "boom" """, "boom")
      }
      test("binaryTypeErrorsStayOperatorSpecific") {
        assertErrContainsBoth(""" "a" * 1 """, "Unknown binary operation: string * number")
      }
      test("unaryTypeErrorsStayOperatorSpecific") {
        assertErrContainsBoth(""" +{} """, "Unknown unary operation: + object")
      }
    }

    // -------------------------------------------------------------------------
    // Interaction with useNewEvaluator
    // -------------------------------------------------------------------------
    test("withNewEvaluator") {
      def evalBoth(s: String): ujson.Value =
        eval(s, useNewEvaluator = true)

      evalBoth("1 + 2") ==> ujson.Num(3)
      evalBoth("if true then 'yes' else 'no'") ==> ujson.Str("yes")
      evalBoth("true && false") ==> ujson.False
      evalBoth("false || true") ==> ujson.True
      evalBoth("~0") ==> ujson.Num(-1)
      evalBoth("12 & 10") ==> ujson.Num(8)
    }
  }
}
