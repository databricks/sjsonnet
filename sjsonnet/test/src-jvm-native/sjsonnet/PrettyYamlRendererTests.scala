package sjsonnet

import utest.*

object PrettyYamlRendererTests extends TestSuite {
  val workspaceRoot = sys.env.get("MILL_WORKSPACE_ROOT").map(os.Path(_)).getOrElse(os.pwd)
  val testSuiteRoot: os.Path = workspaceRoot / "sjsonnet" / "test" / "resources" / "test_suite"
  def eval(path: os.Path, comments: Boolean): String = {
    var currentPos: Position = null
    val interp = new Interpreter(
      Map(),
      Map(),
      OsPath(testSuiteRoot),
      importer = sjsonnet.SjsonnetMain.resolveImport(Array(OsPath(testSuiteRoot)).toIndexedSeq),
      parseCache = new DefaultParseCache,
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
    res.toOption.get.toString
  }
  val nontrivial: os.Path = workspaceRoot / "sjsonnet" / "test" / "resources" / "nontrivial"
  def tests: Tests = Tests {
    test("nocomments") {
      eval(nontrivial / "mixins.jsonnet", comments = false) ==>
      os.read(nontrivial / "mixins.golden.yaml")
    }
    // FIXME: how to turn off the mill sandbox?
//    test("comments") {
//      eval(nontrivial / "mixins.jsonnet", comments = true) ==>
//      os.read(nontrivial / "mixins.golden.comments.yaml")
//    }
    test("nounquoted") {
      // Ensure weird octal-number-like strings are quoted, to avoid
      // edge cases that may cause problems for non-compliant parsers
      eval(nontrivial / "quotingNumberLikeStrings.jsonnet", comments = false) ==>
      os.read(nontrivial / "quotingNumberLikeStrings.yaml")
    }
  }
}
