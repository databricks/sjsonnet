package sjsonnet

import java.util.Base64
import java.nio.charset.StandardCharsets
import scala.scalajs.js
import utest._

object FileTests extends TestSuite {
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

  def eval(fileName: String) = {
    SjsonnetMain.interpret(
      new String(TestResources.files(joinPath("test_suite", fileName)), StandardCharsets.UTF_8),
      js.Dictionary("var1" -> """"test"""", "var2" -> """local f(a, b) = {[a]: b, "y": 2}; f("x", 1)"""),
      js.Dictionary("var1" -> """"test"""", "var2" -> """{"x": 1, "y": 2}"""),
      "test_suite",
      (wd: String, path: String) => joinPath(wd, path),
      (path: String, binaryData: Boolean) => if (binaryData) {
        Right(TestResources.files(joinPath("test_suite", path)))
      } else {
        Left(new String(TestResources.files(joinPath("test_suite", path)), StandardCharsets.UTF_8))
      }
    )
  }
  def check(expected: ujson.Value = ujson.True)(implicit tp: utest.framework.TestPath) = {
    val res = ujson.WebJson.transform(eval(s"${tp.value.last}.jsonnet"), ujson.Value)
    assert(res == expected)
    res
  }

  def checkFail(expected: String)(implicit tp: utest.framework.TestPath) = {
    try {
      val res = ujson.WebJson.transform(eval(s"${tp.value.last}.jsonnet"), ujson.Value)
      assert(false)
    } catch {
      case e: js.JavaScriptException =>
        assert(e.getMessage == expected)
    }
  }

  def checkGolden()(implicit tp: utest.framework.TestPath) = {
    check(ujson.read(new String(TestResources.files(joinPath("test_suite", s"${tp.value.last}.jsonnet.golden")), StandardCharsets.UTF_8)))
  }

  def tests = Tests{
    test("arith_bool") - check()
    test("arith_float") - check()
    test("arith_string") - check()
    test("array") - check()
    test("array_comparison") - checkGolden()
    test("array_comparison2") - checkGolden()
    test("assert") - check()
    test("binary") - check()
    test("comments") - check()
    test("condition") - check()
    // test("dos_line_endings") - checkGolden()
    test("format") - check()
    // test("formatter") - checkGolden()
    test("formatting_braces") - checkGolden()
    test("formatting_braces2") - checkGolden()
    test("functions") - check()
    test("import") - check()
    test("invariant") - check()
    test("invariant_manifest") - checkGolden()
    test("local") - check()
    test("lazy") - check(42)
    test("lazy_operator1") - check(ujson.False)
    test("lazy_operator2") - checkFail(
      """sjsonnet.Error: should happen
        |  at [Error].((memory):1:9)
        |  at [And].((memory):1:6)
        |""".stripMargin)
    test("merge") - check()
    test("null") - check()
    test("object") - check()
    test("oop") - check()
    test("oop_extra") - check()
    test("parsing_edge_cases") - check()
    test("precedence") - check()
    test("recursive_function_native") - check()
    test("recursive_import_ok") - check()
    test("recursive_object") - check()
    test("regex_js") - check()
    test("sanity") - checkGolden()
    test("sanity2") - checkGolden()
    test("shebang") - check()
    test("slice.sugar") - check()
    test("std_all_hidden") - check()
    test("stdlib_native") - check()
    test("text_block") - check()
    test("tla.simple")- check()
    test("unicode") - check()
    test("unix_line_endings") - checkGolden()
    test("unparse") - checkGolden()
    test("verbatim_strings") - check()
    test("issue_127") - check()
  }
}

