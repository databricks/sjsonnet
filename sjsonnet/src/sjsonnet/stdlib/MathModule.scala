package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

class MathModule extends AbstractFunctionModule {
  def name = "math"

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
        def evalRhs(a: Lazy, b: Lazy, ev: EvalScope, pos: Position): Val = {
          (a.force, b.force) match {
            case (x: Val.Num, y: Val.Num) => Val.Num(pos, x.asDouble % y.asDouble)
            case _ => Val.Str(pos, Format.format(a.force.asString, b.force, pos)(ev))
          }
        }
      }
    ),
    builtin("modulo", "a", "b") { (pos, ev, a: Double, b: Double) =>
      a % b
    },
    builtin("clamp", "x", "minVal", "maxVal") {
      (pos, ev, x: Double, minVal: Double, maxVal: Double) =>
        math.max(minVal, math.min(x, maxVal))
    },
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
