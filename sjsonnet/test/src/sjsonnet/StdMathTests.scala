package sjsonnet

import utest._
import TestUtils.eval

object StdMathTests extends TestSuite {
  def tests: Tests = Tests {
    test("sign normalizes negative zero") {
      eval("""std.atan2(std.sign(-0), -1)""") ==> ujson.Num(math.Pi)
    }
  }
}
