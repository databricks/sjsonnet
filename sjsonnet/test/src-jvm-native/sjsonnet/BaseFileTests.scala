package sjsonnet

import ujson.Value
import utest.{TestSuite, assert}

abstract class BaseFileTests extends TestSuite {
  val workspaceRoot = sys.env.get("MILL_WORKSPACE_ROOT").map(os.Path(_)).getOrElse(os.pwd)
  val testSuiteRoot: os.Path = workspaceRoot / "sjsonnet" / "test" / "resources" / "test_suite"
  private val stderr = new StringBuffer()

  def eval(p: os.Path): Either[String, Value] = {
    stderr.setLength(0)
    val interp = new Interpreter(
      Map("var1" -> "\"test\"", "var2" -> """local f(a, b) = {[a]: b, "y": 2}; f("x", 1)"""),
      Map("var1" -> "\"test\"", "var2" -> """{"x": 1, "y": 2}"""),
      OsPath(testSuiteRoot),
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

  def check(fileName: os.Path): Unit = {
    println(s"Checking ${fileName.relativeTo(workspaceRoot)}")
    val res = eval(fileName)
    val expected = ujson.read(os.read(os.Path(fileName.toString + ".golden")))
    assert(res == Right(expected))
    assert(stderr.toString.isEmpty)
  }

  def checkStderr(fileName: os.Path): Unit = {
    println(s"Checking ${fileName.relativeTo(workspaceRoot)}")
    eval(fileName)
    val expected = os.read(os.Path(fileName.toString + ".golden")).stripLineEnd
    assert(expected.startsWith(stderr.toString))
  }

  def checkError(fileName: os.Path): Unit = {
    println(s"Checking ${fileName.relativeTo(workspaceRoot)}")
    val expected = os
      .read(os.Path(fileName.toString + ".golden"))
      .replaceAll("\\(sjsonnet/test/resources/test_suite/", "(")

    var res: Either[String, Value] = Right(null)
    try {
      res = eval(fileName)
      assert(res == Left(expected.stripLineEnd))
    } catch {
      case _: java.lang.StackOverflowError =>
        assert(expected.contains("StackOverflowError"))
      case e: sjsonnet.Error =>
        assert(expected.stripLineEnd.contains(e.getMessage))
      case _: Throwable =>
        assert(false)
    }
  }

  val isScalaNative: Boolean = System.getenv("SCALANATIVE_THREAD_STACK_SIZE") != null
}
