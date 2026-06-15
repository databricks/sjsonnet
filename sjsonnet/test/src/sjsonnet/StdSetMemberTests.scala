package sjsonnet

import utest._
import TestUtils.eval

object StdSetMemberTests extends TestSuite {
  def tests: Tests = Tests {
    test("setMember accepts string sets") {
      eval("""std.setMember("b", "abc")""") ==> ujson.True
      eval("""std.setMember("d", "abc")""") ==> ujson.False
    }
    test("setMember treats negative zero and zero as equal") {
      eval("std.setMember(-0.0, [0])") ==> ujson.True
      eval("std.setMember(0, [-0.0])") ==> ujson.True
    }
  }
}
