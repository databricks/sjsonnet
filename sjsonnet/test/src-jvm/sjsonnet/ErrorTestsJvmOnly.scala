package sjsonnet

import utest._

object ErrorTestsJvmOnly extends TestSuite {
  val testSuiteRoot = os.pwd / "sjsonnet" / "test" / "resources" / "test_suite"
  def eval(p: os.Path) = {
    val interp = new Interpreter(
      Map(),
      Map(),
      OsPath(os.pwd),
      importer = sjsonnet.SjsonnetMain.resolveImport(Array.empty[Path]),
    )
    interp.interpret(os.read(p), OsPath(p))
  }
  def check(expected: String)(implicit tp: utest.framework.TestPath) = {
    val res = eval(testSuiteRoot / s"error.${tp.value.mkString(".")}.jsonnet")

    assert(res == Left(expected))
  }

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
