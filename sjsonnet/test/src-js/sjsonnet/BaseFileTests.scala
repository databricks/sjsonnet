package sjsonnet

import sjsonnet.stdlib.NativeRegex

import java.nio.charset.StandardCharsets
import scala.scalajs.js
import utest._

abstract class BaseFileTests extends TestSuite {
  private val stderr = new StringBuffer()
  private val std = new Std(
    nativeFunctions = Map(
      "jsonToString" -> new Val.Builtin1("jsonToString", "x") {
        override def evalRhs(arg1: Lazy, ev: EvalScope, pos: Position): Val = {
          Val.Str(
            pos,
            Materializer
              .apply0(
                arg1.force,
                MaterializeJsonRenderer(indent = -1, newline = "", keyValueSeparator = ":")
              )(ev)
              .toString
          )
        }
      },
      "nativeError" -> new Val.Builtin0("nativeError") {
        override def evalRhs(ev: EvalScope, pos: Position): Val =
          Error.fail("native function error")
      },
      "nativePanic" -> new Val.Builtin0("nativePanic") {
        override def evalRhs(ev: EvalScope, pos: Position): Val =
          throw new RuntimeException("native function panic")
      }
    ) ++ new NativeRegex().functions
  )

  def importResolver(
      files: Map[String, () => Array[Byte]],
      wd: Path,
      path: String): Option[Path] = {
    val p = wd / path
    if (files.contains(p.toString)) {
      Some(p)
    } else {
      None
    }
  }

  def importLoader(
      files: Map[String, () => Array[Byte]],
      path: Path,
      binaryData: Boolean): Either[String, Array[Byte]] =
    if (binaryData) {
      Right(files(path.toString)())
    } else {
      Left(new String(files(path.toString)(), StandardCharsets.UTF_8))
    }

  def eval(files: Map[String, () => Array[Byte]], fileName: String, suite: String) = {
    stderr.setLength(0)
    val interp = new Interpreter(
      Map(
        "var1" -> "\"test\"",
        "var2" -> """local f(a, b) = {[a]: b, "y": 2}; f("x", 1)""",
        "stringVar" -> """"2 + 2"""",
        "codeVar" -> "3 + 3",
        "errorVar" -> "error 'xxx'",
        "staticErrorVar" -> ")",
        "UndeclaredX" -> "x",
        "selfRecursiveVar" -> """[42, std.extVar("selfRecursiveVar")[0] + 1]""",
        "mutuallyRecursiveVar1" -> """[42, std.extVar("mutuallyRecursiveVar2")[0] + 1]""",
        "mutuallyRecursiveVar2" -> """[42, std.extVar("mutuallyRecursiveVar1")[0] + 1]"""
      ),
      Map("var1" -> "\"test\"", "var2" -> """{"x": 1, "y": 2}"""),
      JsVirtualPath(suite),
      new Importer {
        def resolve(docBase: Path, importName: String): Option[Path] =
          importResolver(files, docBase, importName)
        def read(path: Path, binaryData: Boolean): Option[ResolvedFile] =
          if (!files.contains(path.toString)) None
          else
            importLoader(files, path, binaryData) match {
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
      settings = new Settings(),
      std = std.Std
    )

    interp.interpret0(
      importLoader(files, JsVirtualPath(fileName), false).left.getOrElse(""),
      JsVirtualPath(fileName),
      ujson.WebJson.Builder
    ) match {
      case Left(msg) => throw new js.JavaScriptException(msg)
      case Right(v)  => v
    }
  }

  def check(files: Map[String, () => Array[Byte]], fileName: String, testSuite: String): Unit = {
    println(s"Checking $testSuite/$fileName")
    val goldenContent = if (files.contains(fileName + ".golden_js")) {
      new String(files(fileName + ".golden_js")(), StandardCharsets.UTF_8)
    } else {
      new String(files(fileName + ".golden")(), StandardCharsets.UTF_8)
    }
    if (
      goldenContent.startsWith("sjsonnet.Error") ||
      goldenContent.startsWith("sjsonnet.ParseError") ||
      goldenContent.startsWith("sjsonnet.StaticError") ||
      goldenContent.contains("java.lang.StackOverflowError") ||
      goldenContent.startsWith("RUNTIME ERROR")
    ) {
      checkError(files, fileName, goldenContent, testSuite)
    } else {
      val res = ujson.WebJson.transform(eval(files, fileName, testSuite), ujson.Value)
      try {
        val expected = ujson.read(goldenContent)
        assert(res == expected)
        assert(stderr.toString.isEmpty)
      } catch {
        case e: ujson.ParsingFailedException =>
          if (
            stderr.toString.stripLineEnd.nonEmpty && goldenContent.contains(
              stderr.toString.stripLineEnd
            )
          ) {
            assert(true)
          } else {
            println(res)
            assert(e.getMessage == goldenContent.stripLineEnd)
          }
      }
    }
  }

  def checkError(
      files: Map[String, () => Array[Byte]],
      fileName: String,
      goldenContent: String,
      testSuite: String): Unit = {
    val expected = goldenContent
      .replaceAll(f"\\(sjsonnet/test/resources/$testSuite/", "(")
      .replaceAll("    at", "  at")
      .strip()

    try {
      ujson.WebJson.transform(eval(files, fileName, testSuite), ujson.Value)
      assert(false)
    } catch {
      case e: js.JavaScriptException =>
        val msg = e.getMessage.replaceAll("<no stack trace available>", "").strip()
        if (fileName.endsWith("native_panic.jsonnet"))
          assert(msg.strip().contains(expected))
        else
          assert(msg == expected)
      case e: sjsonnet.Error =>
        assert(expected.contains(e.getMessage.strip()))
    }
  }
}
