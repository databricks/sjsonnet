package sjsonnet

import utest._
import TestUtils.eval

object StdSetMemberTests extends TestSuite {
  def tests: Tests = Tests {
    test("setMember accepts string sets") {
      eval("""std.setMember("b", "abc")""") ==> ujson.True
      eval("""std.setMember("d", "abc")""") ==> ujson.False
    }
  }
}
