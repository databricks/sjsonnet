package sjsonnet

import utest._

object PrettyYamlRendererTests extends TestSuite{
  val testSuiteRoot = os.pwd / "sjsonnet" / "test" / "resources" / "test_suite"
  def eval(path: os.Path, comments: Boolean) = {
    var currentPos: Position = null
    val interp = new Interpreter(
      Map(),
      Map(),
      OsPath(testSuiteRoot),
      importer = sjsonnet.SjsonnetMain.resolveImport(Array(OsPath(testSuiteRoot))),
      storePos = if (comments) currentPos = _ else null
    )
    val res = interp.interpret0(
      os.read(path),
      OsPath(path),
      new PrettyYamlRenderer(
        indent = 2,
        getCurrentPosition = () => currentPos
      )
    )
    res.right.get.toString
  }
  val nontrivial = os.pwd / "sjsonnet" / "test" / "resources" / "nontrivial"
  def tests = Tests{
    test("nocomments"){
      eval(nontrivial / "mixins.jsonnet", comments = false) ==>
        os.read(nontrivial / "mixins.golden.yaml")
    }
    test("comments"){
      eval(nontrivial / "mixins.jsonnet", comments = true) ==>
        os.read(nontrivial / "mixins.golden.comments.yaml")
    }
  }
}

