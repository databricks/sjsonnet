package sjsonnet

import utest.*
import ujson.Value

object ErrorTests extends TestSuite {
  val testSuiteRoot: os.Path = os.pwd / "sjsonnet" / "test" / "resources"
  val skippedTests = Set(
    "sjsonnet/test/resources/test_suite/error.decodeUTF8_float.jsonnet",
    "sjsonnet/test/resources/test_suite/error.function_no_default_arg.jsonnet",
    "sjsonnet/test/resources/test_suite/error.negative_shfit.jsonnet",
    "sjsonnet/test/resources/test_suite/error.overflow.jsonnet",
    "sjsonnet/test/resources/test_suite/error.overflow2.jsonnet",
    "sjsonnet/test/resources/test_suite/error.parse.string.invalid_escape.jsonnet",
    "sjsonnet/test/resources/test_suite/error.parse_json.jsonnet",
    "sjsonnet/test/resources/test_suite/error.std_makeArray_negative.jsonnet"
  )
  val skippedTestInScalaNative = Set(
    "sjsonnet/test/resources/test_suite/error.function_infinite_default.jsonnet",
    "sjsonnet/test/resources/test_suite/error.obj_recursive.jsonnet",
    "sjsonnet/test/resources/test_suite/error.obj_recursive_manifest.jsonnet",
    "sjsonnet/test/resources/test_suite/error.recursive_object_non_term.jsonnet",
    "sjsonnet/test/resources/test_suite/error.recursive_import.jsonnet",
    "sjsonnet/test/resources/test_suite/error.recursive_function_nonterm.jsonnet"
  )
  def eval(p: os.Path): Either[String, Value] = {
    val out = new StringBuffer()
    val interp = new Interpreter(
      Map(),
      Map(),
      OsPath(os.pwd),
      importer = sjsonnet.SjsonnetMain.resolveImport(Array.empty[Path].toIndexedSeq),
      parseCache = new DefaultParseCache,
      warnLogger = (msg: String) => out.append(msg).append('\n'),
      settings = new Settings(
        noDuplicateKeysInComprehension = true,
        strictImportSyntax = true,
        strictInheritedAssertions = true,
        strictSetOperations = true
      )
    )
    interp.interpret(os.read(p), OsPath(p)).left.map(s => out.toString + s)
  }
  def check(fileName: os.Path): Unit = {
    println(s"Checking ${fileName.relativeTo(os.pwd)}")
    val expected = os.read(os.Path(fileName.toString + ".golden"))

    var res: Either[String, Value] = Right(null)
    try {
      res = eval(fileName)
      assert(res == Left(expected.stripLineEnd))
    } catch {
      case _: java.lang.StackOverflowError =>
        assert(expected.contains("StackOverflowError"))
      case e: sjsonnet.Error =>
        assert(expected.stripLineEnd.contains(e.getMessage))
    }
  }

  val tests: Tests = Tests {
    test("error") - {
      os.list(testSuiteRoot / "test_suite")
        .filter(f => f.last.startsWith("error") && f.ext == "jsonnet")
        .filter(f => !skippedTests.contains(f.relativeTo(os.pwd).toString()))
        .filter(f =>
          System.getenv("SCALANATIVE_THREAD_STACK_SIZE") != null && !skippedTestInScalaNative
            .contains(f.relativeTo(os.pwd).toString())
        )
        .foreach { file =>
          check(file)
        }
    }
  }
}
