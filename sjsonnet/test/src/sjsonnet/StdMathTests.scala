package sjsonnet

import utest._
import TestUtils.{eval, evalErr}

object StdMathTests extends TestSuite {
  def tests: Tests = Tests {
    test("sign normalizes negative zero") {
      eval("""std.atan2(std.sign(-0), -1)""") ==> ujson.Num(math.Pi)
    }
    test("atan2 named parameters") {
      eval("""std.atan2(y=1, x=0)""") ==> ujson.Num(math.Pi / 2)
      eval("""std.atan2(y=0, x=1)""") ==> ujson.Num(0.0)
    }
    test("mantissa and exponent for normal values") {
      eval("std.mantissa(0)") ==> ujson.Num(0)
      eval("std.mantissa(1.0)") ==> ujson.Num(0.5)
      eval("std.mantissa(-8.0)") ==> ujson.Num(-0.5)
      eval("std.mantissa(std.pow(2, 53))") ==> ujson.Num(0.5)
      eval("std.exponent(0)") ==> ujson.Num(0)
      eval("std.exponent(1.0)") ==> ujson.Num(1)
      eval("std.exponent(-8.0)") ==> ujson.Num(4)
      eval("std.exponent(std.pow(2, 53))") ==> ujson.Num(54)
    }
    test("mantissa and exponent for subnormal values") {
      eval("std.mantissa(std.pow(2, -1074))") ==> ujson.Num(0.5)
      eval("std.exponent(std.pow(2, -1074))") ==> ujson.Num(-1073)
    }
    test("mantissa and exponent for NaN and Infinity") {
      evalErr("std.mantissa(std.sqrt(-1))")
      evalErr("std.mantissa(std.pow(2, 1024))")
      evalErr("std.exponent(std.sqrt(-1))")
      evalErr("std.exponent(std.pow(2, 1024))")
    }
  }
}
