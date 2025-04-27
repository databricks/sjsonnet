package sjsonnet

import sjsonnet.TestUtils.eval
import utest.{ArrowAssert, TestSuite, Tests, test}

object StaticOptimizerTest extends TestSuite {
  def tests: Tests = Tests {
    test("test optimize if else") {
      eval(
        """
          |local alwaysTrue() = true;
          |local anyString() = "anyString";
          |if alwaysTrue() then anyString() else anyString()
          |""".stripMargin) ==> ujson.Str("anyString")
    }
  }
}
