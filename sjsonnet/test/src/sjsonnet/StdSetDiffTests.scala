package sjsonnet

import sjsonnet.TestUtils.eval
import utest.*

object StdSetDiffTests extends TestSuite {

  def tests: Tests = Tests {
    test {
      eval("""
          |{
          |  local options = ['a', 'b', 'c'],
          |  diff: std.setDiff('d', options),
          |}
          |""".stripMargin).toString ==> """{"diff":["d"]}"""
    }
  }
}
