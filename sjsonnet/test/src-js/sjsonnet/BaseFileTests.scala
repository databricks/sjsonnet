package sjsonnet

import sjsonnet.stdlib.NativeRegex

import java.nio.charset.StandardCharsets
import scala.scalajs.js
import utest._

abstract class BaseFileTests extends TestSuite {
  private val stderr = new StringBuffer()

  case class TestFailure(fileName: String, message: String)
  protected val failures = scala.collection.mutable.ArrayBuffer.empty[TestFailure]

  protected def printSummaryAndAssert(): Unit = {
    if (failures.nonEmpty) {
      val count = failures.size
      val failedFiles = failures.map(_.fileName).mkString(", ")
      println()
      println(s"==================== FAILURE SUMMARY ($count failed) ====================")
      failures.foreach { f =>
        println(s"  FAIL: ${f.fileName}")
        f.message.linesIterator.take(3).foreach(line => println(s"        $line"))
      }
      println("=" * 60)
      println()
      failures.clear()
      throw new utest.AssertionError(s"$count test(s) failed: $failedFiles", Seq.empty, null)
    }
  }
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
    ) ++ NativeRegex.functions,
    additionalStdFunctions = Map(
      // Scala.js does not support md5, for now, so we stub it out for the various smoke tests.
      "md5" -> new Val.Builtin1("md5", "s") {
        override def evalRhs(arg1: Lazy, ev: EvalScope, pos: Position): Val = {
          arg1.force match {
            case Val.Str(_, s) =>
              s match {
                case ""      => Val.Str(pos, "d41d8cd98f00b204e9800998ecf8427e")
                case "grape" => Val.Str(pos, "b781cbb29054db12f88f08c6e161c199")
                case "{}[]01234567890\"'+=-_/<>?,.!@#$%^&*|\\:;`~" =>
                  Val.Str(
                    pos,
                    "a680db28332f0c9647376e5b2aeb4b3d"
                  )
                case "md5" => Val.Str(pos, "1bc29b36f623ba82aaf6724fd3b16718")
                case "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer nec odio. Praesent libero. Sed cursus ante dapibus diam. Sed nisi. Nulla quis sem at nibh elementum imperdiet. Duis sagittis ipsum. Praesent mauris. Fusce nec tellus sed augue semper porta. Mauris massa. Vestibulum lacinia arcu eget nulla. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Curabitur sodales ligula in libero. Sed dignissim lacinia nunc. Curabitur tortor. Pellentesque nibh. Aenean quam. In scelerisque sem at dolor. Maecenas mattis. Sed convallis tristique sem. Proin ut ligula vel nunc egestas porttitor. Morbi lectus risus, iaculis vel, suscipit quis, luctus non, massa. Fusce ac turpis quis ligula lacinia aliquet. Mauris ipsum. Nulla metus metus, ullamcorper vel, tincidunt sed, euismod in, nibh. Quisque volutpat condimentum velit. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nam nec ante. Sed lacinia, urna non tincidunt mattis, tortor neque adipiscing diam, a cursus ipsum ante quis turpis. Nulla facilisi. Ut fringilla. Suspendisse potenti. Nunc feugiat mi a tellus consequat imperdiet. Vestibulum sapien. Proin quam. Etiam ultrices. Suspendisse in justo eu magna luctus suscipit. Sed lectus. Integer euismod lacus luctus magna. Quisque cursus, metus vitae pharetra auctor, sem massa mattis sem, at interdum magna augue eget diam. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Morbi lacinia molestie dui. Praesent blandit dolor. Sed non quam. In vel mi sit amet augue congue elementum. Morbi in ipsum sit amet pede facilisis laoreet. Donec lacus nunc, viverra nec, blandit vel, egestas et, augue. Vestibulum tincidunt malesuada tellus. Ut ultrices ultrices enim. Curabitur sit amet mauris. Morbi in dui quis est pulvinar ullamcorper. Nulla facilisi. Integer lacinia sollicitudin massa. Cras metus. Sed aliquet risus a tortor. Integer id quam. Morbi mi. Quisque nisl felis, venenatis tristique, dignissim in, ultrices sit amet, augue. Proin sodales libero eget ante. Nulla quam. Aenean laoreet. Vestibulum nisi lectus, commodo ac, facilisis ac, ultricies eu, pede. Ut orci risus, accumsan porttitor, cursus quis, aliquet eget, justo. Sed pretium blandit orci. Ut eu diam at pede suscipit sodales. Aenean lectus elit, fermentum non, convallis id, sagittis at, neque. Nullam mauris orci, aliquet et, iaculis et, viverra vitae, ligula. Nulla ut felis in purus aliquam imperdiet. Maecenas aliquet mollis lectus. Vivamus consectetuer risus et tortor. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer nec odio. Praesent libero. Sed cursus ante dapibus diam. Sed nisi. Nulla quis sem at nibh elementum imperdiet. Duis sagittis ipsum. Praesent mauris. Fusce nec tellus sed augue semper porta. Mauris massa. Vestibulum lacinia arcu eget nulla. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Curabitur sodales ligula in libero. Sed dignissim lacinia nunc. Curabitur tortor. Pellentesque nibh. Aenean quam. In scelerisque sem at dolor. Maecenas mattis. Sed convallis tristique sem. Proin ut ligula vel nunc egestas porttitor. Morbi lectus risus, iaculis vel, suscipit quis, luctus non, massa. Fusce ac turpis quis ligula lacinia aliquet. Mauris ipsum. Nulla metus metus, ullamcorper vel, tincidunt sed, euismod in, nibh. Quisque volutpat condimentum velit. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nam nec ante. Sed lacinia, urna non tincidunt mattis, tortor neque adipiscing diam, a cursus ipsum ante quis turpis. Nulla facilisi. Ut fringilla. Suspendisse potenti. Nunc feugiat mi a tellus consequat imperdiet. Vestibulum sapien. Proin quam. Etiam ultrices. Suspendisse in justo eu magna luctus suscipit. Sed lectus. Integer euismod lacus luctus magna. Quisque cursus, metus vitae pharetra auctor, sem massa mattis sem, at interdum magna augue eget diam. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Morbi lacinia molestie dui. Praesent blandit dolor. Sed non quam. In vel mi sit amet augue congue elementum. Morbi in ipsum sit amet pede facilisis laoreet. Donec lacus nunc, viverra nec, blandit vel, egestas et, augue. Vestibulum tincidunt malesuada tellus. Ut ultrices ultrices enim. Curabitur sit amet mauris. Morbi in dui quis est pulvinar ullamcorper. Nulla facilisi. Integer lacinia sollicitudin massa. Cras metus. Sed aliquet risus a tortor. Integer id quam. Morbi mi. Quisque nisl felis, venenatis tristique, dignissim in, ultrices sit amet, augue. Proin sodales libero eget ante. Nulla quam. Aenean laoreet. Vestibulum nisi lectus, commodo ac, facilisis ac, ultricies eu, pede. Ut orci risus, accumsan porttitor, cursus quis, aliquet eget, justo. Sed pretium blandit orci. Ut eu diam at pede suscipit sodales. Aenean lectus elit, fermentum non, convallis id, sagittis at, neque. Nullam mauris orci, aliquet et, iaculis et, viverra vitae, ligula. Nulla ut felis in purus aliquam imperdiet. Maecenas aliquet mollis lectus. Vivamus consectetuer risus et tortor. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer nec odio. Praesent libero. Sed cursus ante dapibus diam. Sed nisi. Nulla quis sem at nibh elementum imperdiet. Duis sagittis ipsum. Praesent mauris. Fusce nec tellus sed augue semper porta. Mauris massa. Vestibulum lacinia arcu eget nulla. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Curabitur sodales ligula in libero. Sed dignissim lacinia nunc. Curabitur tortor. Pellentesque nibh. Aenean quam. In scelerisque sem at dolor. Maecenas mattis. Sed convallis tristique sem. Proin ut ligula vel nunc egestas porttitor. Morbi lectus risus, iaculis vel, suscipit quis, luctus non, massa. Fusce ac turpis quis ligula lacinia aliquet. Mauris ipsum. Nulla metus metus, ullamcorper vel, tincidunt sed, euismod in, nibh. Quisque volutpat condimentum velit. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nam nec ante. Sed lacinia, urna non tincidunt mattis, tortor neque adipiscing diam, a cursus ipsum ante quis turpis. Nulla facilisi. Ut fringilla. Suspendisse potenti. Nunc feugiat mi a tellus consequat imperdiet. Vestibulum sapien. Proin quam. Etiam ultrices. Suspendisse in justo eu magna luctus suscipit. Sed lectus. Integer euismod lacus luctus magna. Quisque cursus, metus vitae pharetra auctor, sem massa mattis sem, at interdum magna augue eget diam. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Morbi lacinia molestie dui. Praesent blandit dolor. Sed non quam. In vel mi sit amet augue congue elementum. Morbi in ipsum si." =>
                  Val.Str(pos, "3496bb633e830e7679ce53700d42de1e")
              }
            case _ => Error.fail("unexpected stub value for md5")
          }
        }
      }
    )
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
      std = std.module
    )

    interp.interpret0(
      importLoader(files, JsVirtualPath(fileName), false).left.getOrElse(""),
      JsVirtualPath(fileName),
      ujson.WebJson.Builder
    ) match {
      case Left(msg) => throw js.JavaScriptException(msg)
      case Right(v)  => v
    }
  }

  def check(files: Map[String, () => Array[Byte]], fileName: String, testSuite: String): Unit = {
    println(s"Checking $testSuite/$fileName")
    try {
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
          val expected = ujson.read(goldenContent, false)
          if (res != expected) {
            val actual = res.toString.take(200)
            failures += TestFailure(
              fileName,
              s"Expected: ${goldenContent.take(200)}\nActual:   $actual"
            )
          } else if (stderr.toString.nonEmpty) {
            failures += TestFailure(fileName, s"Unexpected stderr: ${stderr.toString.take(200)}")
          }
        } catch {
          case e: ujson.ParsingFailedException =>
            if (
              stderr.toString.stripLineEnd.nonEmpty && goldenContent.contains(
                stderr.toString.stripLineEnd
              )
            ) {
              // ok - stderr matches golden
            } else {
              failures += TestFailure(fileName, s"JSON parse error: ${e.getMessage.take(200)}")
            }
        }
      }
    } catch {
      case e: Throwable =>
        failures += TestFailure(
          fileName,
          s"Exception: ${e.getClass.getSimpleName}: ${e.getMessage.take(200)}"
        )
    }
  }

  def checkError(
      files: Map[String, () => Array[Byte]],
      fileName: String,
      goldenContent: String,
      testSuite: String): Unit = {
    val expected = goldenContent
      .replaceAll("    at", "  at")
      .strip()

    try {
      val result = ujson.WebJson.transform(eval(files, fileName, testSuite), ujson.Value)
      failures += TestFailure(
        fileName,
        s"Expected error but got success: ${result.toString.take(200)}"
      )
    } catch {
      case e: js.JavaScriptException =>
        val msg = e.getMessage.replaceAll("<no stack trace available>", "").strip()
        if (!(msg startsWith expected)) {
          failures += TestFailure(
            fileName,
            s"Expected: ${expected.linesIterator.next()}\nActual:   ${msg.linesIterator.next()}"
          )
        }
    }
  }
}
