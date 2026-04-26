package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

object MathModule extends AbstractFunctionModule {
  def name = "math"

  private object Clamp extends Val.Builtin3("clamp", "x", "minVal", "maxVal") {
    private def applyClamp(
        x: Eval,
        minVal: Eval,
        maxVal: Eval,
        ev: EvalScope,
        pos: Position): Val = {
      val xValue = x.value
      val minValue = minVal.value
      if (compareForClamp("<", xValue, minValue, pos)(ev) < 0) {
        minValue
      } else {
        val maxValue = maxVal.value
        if (compareForClamp(">", xValue, maxValue, pos)(ev) > 0) maxValue else xValue
      }
    }

    def evalRhs(x: Eval, minVal: Eval, maxVal: Eval, ev: EvalScope, pos: Position): Val =
      applyClamp(x, minVal, maxVal, ev, pos)

    override def apply3(x: Eval, minVal: Eval, maxVal: Eval, outerPos: Position)(implicit
        ev: EvalScope,
        tailstrictMode: TailstrictMode): Val = {
      if (tailstrictMode == TailstrictModeEnabled) {
        x.value
        minVal.value
        maxVal.value
      }
      applyClamp(x, minVal, maxVal, ev, outerPos)
    }

    override def apply(argsL: Array[? <: Eval], namedNames: Array[String], outerPos: Position)(
        implicit
        ev: EvalScope,
        tailstrictMode: TailstrictMode): Val = {
      val args = new Array[Eval](3)
      val positionalArgCount =
        if (namedNames == null) argsL.length else argsL.length - namedNames.length

      if (argsL.length > 3) Error.fail("Too many args, has 3 parameter(s)", outerPos)

      var i = 0
      while (i < positionalArgCount) {
        args(i) = argsL(i)
        i += 1
      }

      if (namedNames != null) {
        var namedIndex = 0
        var argIndex = positionalArgCount
        while (namedIndex < namedNames.length) {
          val paramIndex = namedNames(namedIndex) match {
            case "x"      => 0
            case "minVal" => 1
            case "maxVal" => 2
            case name     => Error.fail(s"has no parameter $name", outerPos)
          }
          if (args(paramIndex) != null) {
            Error.fail(
              s"binding parameter a second time: ${namedNames(namedIndex)} in function clamp",
              outerPos
            )
          }
          args(paramIndex) = argsL(argIndex)
          namedIndex += 1
          argIndex += 1
        }
      }

      var missing: String = null
      var missingCount = 0
      i = 0
      while (i < args.length) {
        if (args(i) == null) {
          if (missing == null) missing = params.names(i) else missing += ", " + params.names(i)
          missingCount += 1
        }
        i += 1
      }
      if (missingCount > 0) {
        val plural = if (missingCount > 1) "s" else ""
        Error.fail(s"parameter$plural $missing not bound in call", outerPos)
      }

      if (tailstrictMode == TailstrictModeEnabled) args.foreach(_.value)
      applyClamp(args(0), args(1), args(2), ev, outerPos)
    }
  }

  private def compareForClamp(op: String, left: Val, right: Val, pos: Position)(implicit
      ev: EvalScope): Int = {
    (left, right) match {
      case (l: Val.Num, r: Val.Num)   => compareNumbers(l.asDouble, r.asDouble)
      case (l: Val.Str, r: Val.Str)   => Util.compareStringsByCodepoint(l.str, r.str)
      case (l: Val.Arr, r: Val.Arr)   => compareArraysForClamp(l, r, pos)
      case (_: Val.Bool, _: Val.Bool) =>
        Error.fail(s"binary operator $op does not operate on booleans.", pos)
      case (_: Val.Null, _: Val.Null) =>
        Error.fail(s"binary operator $op does not operate on null.", pos)
      case (_: Val.Obj, _: Val.Obj) =>
        Error.fail(s"binary operator $op does not operate on objects.", pos)
      case (_: Val.Func, _: Val.Func) =>
        Error.fail(s"binary operator $op does not operate on functions.", pos)
      case _ =>
        Error.fail(
          s"binary operator $op requires matching types, got ${left.prettyName} and ${right.prettyName}.",
          pos
        )
    }
  }

  private def compareArrayValuesForClamp(left: Val, right: Val, pos: Position)(implicit
      ev: EvalScope): Int = {
    (left, right) match {
      case (l: Val.Num, r: Val.Num)   => compareNumbers(l.asDouble, r.asDouble)
      case (l: Val.Str, r: Val.Str)   => Util.compareStringsByCodepoint(l.str, r.str)
      case (l: Val.Arr, r: Val.Arr)   => compareArraysForClamp(l, r, pos)
      case (_: Val.Null, _: Val.Null) =>
        Error.fail("binary operator < does not operate on null.", pos)
      case (_: Val.Bool, _: Val.Bool) =>
        Error.fail("Values of type boolean are not comparable.", pos)
      case (_: Val.Obj, _: Val.Obj) =>
        Error.fail("Values of type object are not comparable.", pos)
      case (_: Val.Func, _: Val.Func) =>
        Error.fail("Values of type function are not comparable.", pos)
      case _ =>
        Error.fail(
          s"Comparison requires matching types. Got ${left.prettyName} and ${right.prettyName}",
          pos
        )
    }
  }

  private def compareArraysForClamp(left: Val.Arr, right: Val.Arr, pos: Position)(implicit
      ev: EvalScope): Int = {
    val leftLength = left.length
    val rightLength = right.length
    val length = math.min(leftLength, rightLength)
    var i = 0
    while (i < length) {
      val leftEval = left.eval(i)
      val rightEval = right.eval(i)
      val comparison =
        if (
          (leftEval eq rightEval) &&
          leftEval.isInstanceOf[Val] &&
          isPrimitiveComparable(leftEval.asInstanceOf[Val])
        ) {
          0
        } else {
          compareArrayValuesForClamp(leftEval.value, rightEval.value, pos)
        }
      if (comparison != 0) return comparison
      i += 1
    }
    Integer.compare(leftLength, rightLength)
  }

  @inline private def compareNumbers(left: Double, right: Double): Int =
    if (left < right) -1 else if (left > right) 1 else 0

  @inline private def isPrimitiveComparable(value: Val): Boolean =
    value.isInstanceOf[Val.Num] || value.isInstanceOf[Val.Str]

  val functions: Seq[(String, Val.Func)] = Seq(
    builtin("sqrt", "x") { (pos, ev, x: Double) =>
      math.sqrt(x)
    },
    builtin("max", "a", "b") { (pos, ev, a: Double, b: Double) =>
      math.max(a, b)
    },
    builtin("min", "a", "b") { (pos, ev, a: Double, b: Double) =>
      math.min(a, b)
    },
    (
      "mod",
      new Val.Builtin2("mod", "a", "b") {
        def evalRhs(a: Eval, b: Eval, ev: EvalScope, pos: Position): Val = {
          (a.value, b.value) match {
            case (x: Val.Num, y: Val.Num) => Val.cachedNum(pos, x.asDouble % y.asDouble)
            case _ => Val.Str(pos, Format.format(a.value.asString, b.value, pos)(ev))
          }
        }
      }
    ),
    builtin("modulo", "a", "b") { (pos, ev, a: Double, b: Double) =>
      a % b
    },
    builtin(Clamp),
    builtin("pow", "x", "n") { (pos, ev, x: Double, n: Double) =>
      math.pow(x, n)
    },
    builtin("floor", "x") { (pos, ev, x: Double) =>
      math.floor(x)
    },
    builtin("round", "x") { (pos, ev, x: Double) =>
      math.round(x)
    },
    builtin("ceil", "x") { (pos, ev, x: Double) =>
      math.ceil(x)
    },
    builtin("abs", "n") { (pos, ev, x: Double) =>
      math.abs(x)
    },
    builtin("sign", "n") { (_, _, x: Double) =>
      math.signum(x)
    },
    builtin("sin", "x") { (pos, ev, x: Double) =>
      math.sin(x)
    },
    builtin("cos", "x") { (pos, ev, x: Double) =>
      math.cos(x)
    },
    builtin("tan", "x") { (pos, ev, x: Double) =>
      math.tan(x)
    },
    builtin("isEven", "x") { (_, _, x: Double) =>
      math.round(x) % 2 == 0
    },
    builtin("isInteger", "x") { (_, _, x: Double) =>
      math.round(x).toDouble == x
    },
    builtin("isOdd", "x") { (_, _, x: Double) =>
      math.round(x) % 2 != 0
    },
    builtin("isDecimal", "x") { (_, _, x: Double) =>
      math.round(x).toDouble != x
    },
    builtin("asin", "x") { (pos, ev, x: Double) =>
      math.asin(x)
    },
    builtin("acos", "x") { (pos, ev, x: Double) =>
      math.acos(x)
    },
    builtin("atan", "x") { (pos, ev, x: Double) =>
      math.atan(x)
    },
    builtin("atan2", "x", "y") { (pos, ev, x: Double, y: Double) =>
      math.atan2(x, y)
    },
    builtin("hypot", "x", "y") { (pos, ev, x: Double, y: Double) =>
      math.hypot(x, y)
    },
    builtin("deg2rad", "x") { (pos, ev, x: Double) =>
      math.toRadians(x)
    },
    builtin("rad2deg", "x") { (pos, ev, x: Double) =>
      math.toDegrees(x)
    },
    builtin("log", "x") { (pos, ev, x: Double) =>
      math.log(x)
    },
    builtin("log2", "x") { (pos, ev, x: Double) =>
      // no scala log2, do our best without getting fancy with numerics
      math.log(x) / math.log(2.0)
    },
    builtin("log10", "x") { (pos, ev, x: Double) =>
      math.log10(x)
    },
    builtin("exp", "x") { (pos, ev, x: Double) =>
      math.exp(x)
    },
    builtin("mantissa", "x") { (pos, ev, x: Double) =>
      if (x == 0) 0
      else {
        val exponent = Math.floor((Math.log(Math.abs(x)) / Math.log(2)) + 1).toLong
        x * Math.pow(2.0, (-exponent).toDouble)
      }
    },
    builtin("exponent", "x") { (pos, ev, x: Double) =>
      if (x == 0) 0L
      else {
        Math.floor((Math.log(Math.abs(x)) / Math.log(2)) + 1).toLong
      }
    },
    builtin("xor", "bool1", "bool2") { (_, _, bool1: Boolean, bool2: Boolean) =>
      bool1 ^ bool2
    },
    builtin("xnor", "bool1", "bool2") { (_, _, bool1: Boolean, bool2: Boolean) =>
      !(bool1 ^ bool2)
    }
  )
}
