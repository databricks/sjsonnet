package sjsonnet

import sjsonnet.stdlib.NativeRegex
import ujson.Value
import utest.{TestSuite, assert}

abstract class BaseFileTests extends TestSuite {
  val workspaceRoot = sys.env.get("MILL_WORKSPACE_ROOT").map(os.Path(_)).getOrElse(os.pwd)
  val testSuiteRoot: os.Path = workspaceRoot / "sjsonnet" / "test" / "resources"
  private val stderr = new StringBuffer()
  private val std = new sjsonnet.stdlib.StdLibModule(
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
    ) ++ NativeRegex.functions
  )

  def eval(p: os.Path, testSuite: String): Either[String, Value] = {
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
      OsPath(testSuiteRoot / testSuite),
      importer = sjsonnet.SjsonnetMainBase.resolveImport(Array.empty[Path].toIndexedSeq),
      parseCache = new DefaultParseCache,
      logger = (isTrace: Boolean, msg: String) => {
        if (isTrace) {
          stderr.append(msg).append("\n")
        }
      },
      settings = new Settings(),
      std = std.module
    )
    interp.interpret(os.read(p), OsPath(p))
  }

  def check(fileName: os.Path, testSuite: String): Unit = {
    println(s"Checking ${fileName.relativeTo(workspaceRoot)}")
    val goldenContent =
      if (isScalaNative && os.exists(os.Path(fileName.toString + ".golden_native"))) {
        os.read(os.Path(fileName.toString + ".golden_native")).stripLineEnd
      } else {
        os.read(os.Path(fileName.toString + ".golden")).stripLineEnd
      }
    if (
      goldenContent.startsWith("sjsonnet.Error") ||
      goldenContent.startsWith("sjsonnet.ParseError") ||
      goldenContent.startsWith("sjsonnet.StaticError") ||
      goldenContent.contains("java.lang.StackOverflowError") ||
      goldenContent.startsWith("RUNTIME ERROR")
    ) {
      checkError(fileName, goldenContent, testSuite)
    } else {
      val res = eval(fileName, testSuite)
      try {
        val expected = ujson.read(goldenContent)
        assert(res == Right(expected))
        assert(stderr.toString.isEmpty)
      } catch {
        case e: ujson.ParsingFailedException =>
          if (
            res.isRight && stderr.toString.stripLineEnd.nonEmpty && goldenContent.contains(
              stderr.toString.stripLineEnd
            )
          ) {
            assert(true)
          } else {
            println(res.left.getOrElse(""))
            assert(e.getMessage == goldenContent.stripLineEnd)
          }
        case e: Throwable =>
          println(stderr.toString)
          throw e
      }
    }
  }

  private def checkError(fileName: os.Path, goldenContent: String, testSuite: String): Unit = {
    val expected = goldenContent.strip()
    try {
      eval(fileName, testSuite) match {
        case Left(e) =>
          assert(e.strip() startsWith expected)
        case Right(_) =>
          assert(false)
      }
    } catch {
      case _: java.lang.StackOverflowError =>
        assert(expected.contains("StackOverflowError"))
    }
  }

  val isScalaNative: Boolean = System.getenv("SCALANATIVE_THREAD_STACK_SIZE") != null
}
