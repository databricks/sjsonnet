package sjsonnet

import utest._
import TestUtils.eval

object StdMathTests extends TestSuite {
  def tests: Tests = Tests {
    test("sign normalizes negative zero") {
      eval("""std.atan2(std.sign(-0), -1)""") ==> ujson.Num(math.Pi)
    }
    test("atan2 named parameters") {
      eval("""std.atan2(y=1, x=0)""") ==> ujson.Num(math.Pi / 2)
      eval("""std.atan2(y=0, x=1)""") ==> ujson.Num(0.0)
    }
  }
}
