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
      eval("std.mantissa(3 * std.pow(2, -1074))") ==> ujson.Num(0.75)
      eval("std.exponent(3 * std.pow(2, -1074))") ==> ujson.Num(-1072)
      eval("std.mantissa(std.pow(2, -1023))") ==> ujson.Num(0.5)
      eval("std.exponent(std.pow(2, -1023))") ==> ujson.Num(-1022)
      eval("std.mantissa(-std.pow(2, -1074))") ==> ujson.Num(-0.5)
      eval("std.exponent(-std.pow(2, -1074))") ==> ujson.Num(-1073)
    }
    test("mantissa and exponent for NaN and Infinity") {
      evalErr("std.mantissa(std.sqrt(-1))")
      evalErr("std.mantissa(std.pow(2, 1024))")
      evalErr("std.exponent(std.sqrt(-1))")
      evalErr("std.exponent(std.pow(2, 1024))")
    }
    test("sqrt rejects negative input") {
      // go-jsonnet: makeDoubleCheck returns "Not a number" for NaN results
      val err = evalErr("std.sqrt(-1)")
      assert(err.contains("Not a number"))
      val err2 = evalErr("std.sqrt(-0.001)")
      assert(err2.contains("Not a number"))
      // sqrt(0) and sqrt(positive) must still work
      eval("std.sqrt(0)") ==> ujson.Num(0.0)
      eval("std.sqrt(4)") ==> ujson.Num(2.0)
    }
  }
}
