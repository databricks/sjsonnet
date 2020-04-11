package sjsonnet

trait ErrorTestsBase {
  val testSuiteRoot = os.pwd / "sjsonnet" / "test" / "resources" / "test_suite"
  def eval(p: os.Path) = {
    val interp = new Interpreter(
      sjsonnet.SjsonnetMain.createParseCache(),
      Map(),
      Map(),
      OsPath(os.pwd),
      importer = sjsonnet.SjsonnetMain.resolveImport(Array.empty[Path])
    )
    interp.interpret(os.read(p), OsPath(p))
  }
  def check(expected: String)(implicit tp: utest.framework.TestPath) = {
    val res = eval(testSuiteRoot / s"error.${tp.value.mkString(".")}.jsonnet")

    assert(res == Left(expected))
  }
}
