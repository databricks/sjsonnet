package sjsonnet

import java.nio.charset.StandardCharsets
import scala.scalajs.js
import utest._

abstract class BaseFileTests extends TestSuite {
  private val stderr = new StringBuffer()

  def importResolver(wd: Path, path: String): Option[Path] = {
    val p = wd / path
    if (TestResources.files.contains(p.toString)) {
      Some(p)
    } else {
      None
    }
  }

  def importLoader(path: Path, binaryData: Boolean): Either[String, Array[Byte]] =
    if (binaryData) {
      Right(TestResources.files(path.toString))
    } else {
      Left(new String(TestResources.files(path.toString), StandardCharsets.UTF_8))
    }

  def eval(fileName: String, suite: String) = {
    stderr.setLength(0)
    val interp = new Interpreter(
      Map("var1" -> "\"test\"", "var2" -> """local f(a, b) = {[a]: b, "y": 2}; f("x", 1)"""),
      Map("var1" -> "\"test\"", "var2" -> """{"x": 1, "y": 2}"""),
      JsVirtualPath(suite),
      new Importer {
        def resolve(docBase: Path, importName: String): Option[Path] =
          importResolver(docBase, importName)
        def read(path: Path, binaryData: Boolean): Option[ResolvedFile] =
          if (!TestResources.files.contains(path.toString)) None
          else
            importLoader(path, binaryData) match {
              case Left(s)    => Some(StaticResolvedFile(s))
              case Right(arr) => Some(StaticBinaryResolvedFile(arr))
            }
      },
      logger = (isTrace: Boolean, msg: String) => {
        if (isTrace) {
          stderr.append(msg).append("\n")
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
      importLoader(JsVirtualPath(fileName), false).left.getOrElse(""),
      JsVirtualPath(fileName),
      ujson.WebJson.Builder
    ) match {
      case Left(msg) => throw new js.JavaScriptException(msg)
      case Right(v)  => v
    }
  }

  def check(fileName: String, testSuite: String): Unit = {
    println(s"Checking $fileName")
    val goldenContent = if (TestResources.files.contains(fileName + ".golden_js")) {
      new String(TestResources.files(fileName + ".golden_js"), StandardCharsets.UTF_8)
    } else {
      new String(TestResources.files(fileName + ".golden"), StandardCharsets.UTF_8)
    }
    if (
      goldenContent.startsWith("sjsonnet.Error") ||
      goldenContent.startsWith("sjsonnet.ParseError") ||
      goldenContent.startsWith("sjsonnet.StaticError") ||
      goldenContent.contains("java.lang.StackOverflowError")
    ) {
      checkError(fileName, goldenContent, testSuite)
    } else {
      val res = ujson.WebJson.transform(eval(fileName, testSuite), ujson.Value)
      try {
        val expected = ujson.read(goldenContent)
        assert(res == expected)
        assert(stderr.toString.isEmpty)
      } catch {
        case e: ujson.ParsingFailedException =>
          if (goldenContent.contains(stderr.toString.stripLineEnd)) {
            assert(true)
          } else {
            assert(e.getMessage == goldenContent.stripLineEnd)
          }
      }
    }
  }

  def checkError(fileName: String, goldenContent: String, testSuite: String): Unit = {
    val expected = goldenContent
      .replaceAll(f"\\(sjsonnet/test/resources/$testSuite/", "(")
      .replaceAll("    at", "  at")
      .strip()

    try {
      ujson.WebJson.transform(eval(fileName, testSuite), ujson.Value)
      assert(false)
    } catch {
      case e: js.JavaScriptException =>
        val msg = e.getMessage.replaceAll("<no stack trace available>", "").strip()
        assert(msg == expected)
      case e: sjsonnet.Error =>
        assert(expected.contains(e.getMessage.strip()))
    }
  }
}
