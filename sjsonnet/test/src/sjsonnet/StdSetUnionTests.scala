package sjsonnet

import sjsonnet.TestUtils.eval
import utest.*

object StdSetUnionTests extends TestSuite {

  def tests: Tests = Tests {
    test {
      eval("""
          |{
          |  local options = ['a', 'b', 'c'],
          |  union: std.setUnion('d', options),
          |}
          |""".stripMargin).toString ==> """{"union":["a","b","c","d"]}"""
    }
    test("setUnion deduplicates negative zero and zero") {
      eval("std.setUnion([0], [-0.0])") ==> ujson.Arr(0)
      eval("std.setInter([0], [-0.0])") ==> ujson.Arr(0)
    }
  }
}
