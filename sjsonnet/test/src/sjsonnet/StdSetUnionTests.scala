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
  }
}
