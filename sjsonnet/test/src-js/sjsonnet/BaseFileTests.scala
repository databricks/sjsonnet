package sjsonnet

import java.nio.charset.StandardCharsets
import scala.scalajs.js
import utest._

abstract class BaseFileTests extends TestSuite {
  private val stderr = new StringBuffer()

  def joinPath(a: String, b: String): String = {
    val aStripped = if (a.endsWith("/")) a.substring(0, a.length - 1) else a
    val bStripped = if (b.startsWith("/")) b.substring(1) else b
    if (aStripped.isEmpty)
      bStripped
    else if (bStripped.isEmpty)
      aStripped
    else
      aStripped + "/" + bStripped
  }

  def importResolver(wd: String, path: String): String = {
    val p = joinPath(wd, path)
    if (TestResources.files.contains(p)) {
      p
    } else {
      null
    }
  }

  def importLoader(path: String, binaryData: Boolean): Either[String, Array[Byte]] =
    if (binaryData) {
      Right(TestResources.files(path))
    } else {
      Left(new String(TestResources.files(path), StandardCharsets.UTF_8))
    }

  def eval(fileName: String, suite: String) = {
    stderr.setLength(0)
    val interp = new Interpreter(
      Map("var1" -> "\"test\"", "var2" -> """local f(a, b) = {[a]: b, "y": 2}; f("x", 1)"""),
      Map("var1" -> "\"test\"", "var2" -> """{"x": 1, "y": 2}"""),
      JsVirtualPath(suite + "/"),
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
      ujson.read(new String(TestResources.files(fileName + ".golden"), StandardCharsets.UTF_8))
    val res = ujson.WebJson.transform(eval(fileName, "test_suite"), ujson.Value)
    assert(res == expected)
    assert(stderr.toString.isEmpty)
  }

  def checkStderr(fileName: String): Unit = {
    println(s"Checking $fileName")
    eval(fileName, "test_suite")
    val expected = new String(TestResources.files(fileName + ".golden"), StandardCharsets.UTF_8)
    assert(expected.startsWith(stderr.toString))
  }

  def checkError(fileName: String): Unit = {
    println(s"Checking $fileName")
    val expected =
      new String(TestResources.files(fileName + ".golden"), StandardCharsets.UTF_8).stripLineEnd
        .replaceAll("\\(sjsonnet/test/resources/test_suite/", "(")
        .replaceAll("    at", "  at")

    try {
      ujson.WebJson.transform(eval(fileName, "test_suite"), ujson.Value)
      assert(false)
    } catch {
      case e: js.JavaScriptException =>
        val msg = e.getMessage
        assert(msg == expected)
      case e: sjsonnet.Error =>
        assert(expected.stripLineEnd.contains(e.getMessage))
      case _: Throwable =>
        assert(false)
    }
  }
}
