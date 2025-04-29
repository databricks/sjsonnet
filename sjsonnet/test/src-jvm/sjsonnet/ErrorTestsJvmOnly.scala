package sjsonnet

import utest._
import ujson.Value

object ErrorTestsJvmOnly extends TestSuite {
  val testSuiteRoot: os.Path = os.pwd / "sjsonnet" / "test" / "resources" / "test_suite"
  def eval(p: os.Path): Either[String, Value] = {
    val interp = new Interpreter(
      Map(),
      Map(),
      OsPath(os.pwd),
      importer = sjsonnet.SjsonnetMain.resolveImport(Array.empty[Path].toIndexedSeq),
      parseCache = new DefaultParseCache
    )
    interp.interpret(os.read(p), OsPath(p))
  }
  def check(expected: String)(implicit tp: utest.framework.TestPath): Unit = {
    val res = eval(testSuiteRoot / s"error.${tp.value.mkString(".")}.jsonnet")

    assert(res == Left(expected))
  }

  val tests: Tests = Tests {
    test("array_recursive_manifest") - check(
      """sjsonnet.Error: Stackoverflow while materializing, possibly due to recursive value
        |    at .(sjsonnet/test/resources/test_suite/error.array_recursive_manifest.jsonnet:17:12)
        |""".stripMargin
    )
    test("obj_recursive") - check(
      """sjsonnet.Error: Stackoverflow while materializing, possibly due to recursive value
        |    at .(sjsonnet/test/resources/test_suite/error.obj_recursive.jsonnet:17:3)
        |""".stripMargin
    )
    test("obj_recursive_manifest") - check(
      """sjsonnet.Error: Stackoverflow while materializing, possibly due to recursive value
        |    at .(sjsonnet/test/resources/test_suite/error.obj_recursive_manifest.jsonnet:17:3)
        |""".stripMargin
    )
  }
}
