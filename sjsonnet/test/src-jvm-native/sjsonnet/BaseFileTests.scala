package sjsonnet

import ujson.Value
import utest.{TestSuite, assert}

abstract class BaseFileTests extends TestSuite {
  val workspaceRoot = sys.env.get("MILL_WORKSPACE_ROOT").map(os.Path(_)).getOrElse(os.pwd)
  val testSuiteRoot: os.Path = workspaceRoot / "sjsonnet" / "test" / "resources"
  private val stderr = new StringBuffer()

  def eval(p: os.Path, testSuite: String): Either[String, Value] = {
    stderr.setLength(0)
    val interp = new Interpreter(
      Map("var1" -> "\"test\"", "var2" -> """local f(a, b) = {[a]: b, "y": 2}; f("x", 1)"""),
      Map("var1" -> "\"test\"", "var2" -> """{"x": 1, "y": 2}"""),
      OsPath(testSuiteRoot / testSuite),
      importer = sjsonnet.SjsonnetMain.resolveImport(Array.empty[Path].toIndexedSeq),
      parseCache = new DefaultParseCache,
      logger = (isTrace: Boolean, msg: String) => {
        if (isTrace) {
          stderr.append(msg).append("\n")
        }
      },
      settings = new Settings(
        noDuplicateKeysInComprehension = true,
        strictImportSyntax = true,
        strictInheritedAssertions = true,
        strictSetOperations = true
      )
    )
    interp.interpret(os.read(p), OsPath(p))
  }

  def check(fileName: os.Path, testSuite: String): Unit = {
    println(s"Checking ${fileName.relativeTo(workspaceRoot)}")
    val goldenContent = os.read(os.Path(fileName.toString + ".golden")).stripLineEnd
    if (
      goldenContent.startsWith("sjsonnet.Error") ||
      goldenContent.startsWith("sjsonnet.ParseError") ||
      goldenContent.startsWith("sjsonnet.StaticError") ||
      goldenContent.contains("java.lang.StackOverflowError")
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
          if (goldenContent.contains(stderr.toString.stripLineEnd)) {
            assert(true)
          } else {
            assert(e.getMessage == goldenContent.stripLineEnd)
          }
      }
    }
  }

  private def checkError(fileName: os.Path, goldenContent: String, testSuite: String): Unit = {
    val expected = goldenContent.replaceAll(s"\\(sjsonnet/test/resources/$testSuite/", "(").strip()

    var res: Either[String, Value] = Right(null)
    try {
      res = eval(fileName, testSuite)
      assert(res.isLeft)
      val actual = res.left.getOrElse("")
      assert(actual.strip() == expected)
    } catch {
      case _: java.lang.StackOverflowError =>
        assert(expected.contains("StackOverflowError"))
      case e: sjsonnet.Error =>
        assert(expected.contains(e.getMessage.stripLineEnd))
    }
  }

  val isScalaNative: Boolean = System.getenv("SCALANATIVE_THREAD_STACK_SIZE") != null
}
