package sjsonnet

import utest._

object ErrorTestsJvmOnly extends TestSuite with ErrorTestsBase{
  val tests = Tests{
    test("array_recursive_manifest") - check(
      """Stackoverflow while materializing, possibly due to recursive value""".stripMargin
    )
    test("obj_recursive") - check(
      """Stackoverflow while materializing, possibly due to recursive value""".stripMargin
    )
    test("obj_recursive_manifest") - check(
      """Stackoverflow while materializing, possibly due to recursive value""".stripMargin
    )
  }
}
