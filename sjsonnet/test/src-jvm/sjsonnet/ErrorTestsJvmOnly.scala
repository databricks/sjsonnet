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
    // Hack: this test suite may flakily fail if this suite runs prior to other error tests:
    // if classloading or linking happens inside the StackOverflowError handling then that
    // may will trigger a secondary StackOverflowError and cause the test to fail.
    // As a temporary solution, we redundantly run one of the other error tests first.
    // A better long term solution would be to change how we handle StackOverflowError
    // to avoid this failure mode, but for now we add this hack to avoid CI flakiness:
    test("02") - check(
      """sjsonnet.Error: Foo.
        |    at [Error].(sjsonnet/test/resources/test_suite/error.02.jsonnet:17:1)
        |""".stripMargin
    )
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
