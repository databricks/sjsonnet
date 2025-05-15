package sjsonnet

import java.nio.charset.StandardCharsets
import scala.scalajs.js
import utest._

object ErrorTests extends TestSuite {
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
  val skippedTestInScalaJs = Set(
    "sjsonnet/test/resources/test_suite/error.array_recursive_manifest.jsonnet",
    "sjsonnet/test/resources/test_suite/error.function_infinite_default.jsonnet",
    "sjsonnet/test/resources/test_suite/error.obj_recursive.jsonnet",
    "sjsonnet/test/resources/test_suite/error.obj_recursive_manifest.jsonnet",
    "sjsonnet/test/resources/test_suite/error.recursive_object_non_term.jsonnet",
    "sjsonnet/test/resources/test_suite/error.recursive_import.jsonnet",
    "sjsonnet/test/resources/test_suite/error.recursive_function_nonterm.jsonnet",
    "sjsonnet/test/resources/test_suite/error.array_large_index.jsonnet"
  )

  def joinPath(a: String, b: String) = {
    val aStripped = if (a.endsWith("/")) a.substring(0, a.length - 1) else a
    val bStripped = if (b.startsWith("/")) b.substring(1) else b
    if (aStripped.isEmpty)
      bStripped
    else if (bStripped.isEmpty)
      aStripped
    else
      aStripped + "/" + bStripped
  }

  def eval(fileName: String, suite: String) = {
    val importResolver = (wd: String, path: String) => {
      val p = joinPath(wd, path)
      if (TestResources.files.contains(p)) {
        p
      } else {
        null
      }
    }
    val importLoader = (path: String, binaryData: Boolean) =>
      if (binaryData) {
        Right(TestResources.files(path))
      } else {
        Left(new String(TestResources.files(path), StandardCharsets.UTF_8))
      }

    val interp = new Interpreter(
      Map(),
      Map(),
      JsVirtualPath(""),
      new Importer {
        def resolve(docBase: Path, importName: String): Option[Path] =
          importResolver(docBase.asInstanceOf[JsVirtualPath].path, importName) match {
            case null => None
            case s    => Some(JsVirtualPath(s))
          }
        def read(path: Path, binaryData: Boolean): Option[ResolvedFile] =
          importLoader(path.asInstanceOf[JsVirtualPath].path, binaryData) match {
            case Left(s)    => Some(StaticResolvedFile(s))
            case Right(arr) => Some(StaticBinaryResolvedFile(arr))
          }
      },
      parseCache = new DefaultParseCache,
      settings = new Settings(
        noDuplicateKeysInComprehension = true,
        strictImportSyntax = true,
        strictInheritedAssertions = true,
        strictSetOperations = true
      )
    )

    interp.interpret0(
      importLoader(fileName, false).left.getOrElse(""),
      JsVirtualPath(fileName),
      ujson.WebJson.Builder
    ) match {
      case Left(msg) => throw new js.JavaScriptException(msg)
      case Right(v)  => v
    }
  }

  def check(fileName: String): Unit = {
    println(s"Checking $fileName")
    val expected =
      new String(TestResources.files(fileName + ".golden"), StandardCharsets.UTF_8).stripLineEnd

    try {
      ujson.WebJson.transform(eval(fileName, "test_suite"), ujson.Value)
      assert(false)
    } catch {
      case e: js.JavaScriptException =>
        val msg = e.getMessage
          .replaceAll("\\(test_suite", "(sjsonnet/test/resources/test_suite")
          .replaceAll("  at", "    at")
        assert(msg == expected)
      case e: sjsonnet.Error =>
        assert(expected.stripLineEnd.contains(e.getMessage))
    }
  }

  val tests: Tests = Tests {
    test("error") - {
      TestResources.files.keys.toSeq.sorted
        .filter(f => f.startsWith("test_suite/") && f.contains("/error") && f.endsWith(".jsonnet"))
        .filter(f => !skippedTests.contains("sjsonnet/test/resources/" + f))
        .filter(f => !skippedTestInScalaJs.contains("sjsonnet/test/resources/" + f))
        .foreach { file =>
          check(file)
        }
    }
  }
}
