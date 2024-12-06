package sjsonnet

import utest._

object FileTests extends TestSuite{
  val testSuiteRoot = os.pwd / "sjsonnet" / "test" / "resources" / "test_suite"
  def eval(p: os.Path) = {
    val interp = new Interpreter(
      Map("var1" -> "\"test\"", "var2" -> """local f(a, b) = {[a]: b, "y": 2}; f("x", 1)"""),
      Map("var1" -> "\"test\"", "var2" -> """{"x": 1, "y": 2}"""),
      OsPath(testSuiteRoot),
      importer = sjsonnet.SjsonnetMain.resolveImport(Array(OsPath(testSuiteRoot))),
      parseCache = new DefaultParseCache
    )
    interp.interpret(os.read(p), OsPath(p))
  }
  def check(expected: ujson.Value = ujson.True)(implicit tp: utest.framework.TestPath) = {
    val res = eval(testSuiteRoot / s"${tp.value.last}.jsonnet")
    assert(res == Right(expected))
    res
  }
  def checkFail(expected: String)(implicit tp: utest.framework.TestPath) = {
    val res = eval(testSuiteRoot / s"${tp.value.last}.jsonnet")
    assert(res == Left(expected))
    res
  }
  def checkGolden()(implicit tp: utest.framework.TestPath) = {
    check(ujson.read(os.read(testSuiteRoot / s"${tp.value.last}.jsonnet.golden")))
  }
  def tests = Tests{
    test("arith_bool") - check()
    test("arith_float") - check()
    test("arith_string") - check()
    test("array") - check()
    test("assert") - check()
    test("binary") - check()
    test("comments") - check()
    test("condition") - check()
//    test("dos_line_endings") - checkGolden()
    test("format") - check()
//    test("formatter") - checkGolden()
    test("formatting_braces") - checkGolden()
    test("formatting_braces2") - checkGolden()
    test("functions") - check()
    test("import") - check()
    test("invariant") - check()
    test("invariant_manifest") - checkGolden()
    test("local") - check()
    test("merge") - check()
    test("null") - check()
    test("object") - check()
    test("oop") - check()
    test("oop_extra") - check()
    test("parsing_edge_cases") - check()
    test("precedence") - check()
//    test("recursive_function") - check()
    test("recursive_import_ok") - check()
    test("recursive_object") - check()
    test("sanity") - checkGolden()
    test("sanity2") - checkGolden()
    test("shebang") - check()
    "slice.sugar" - check()
    test("std_all_hidden") - check()
    test("stdlib") - check()
    test("text_block") - check()
    "tla.simple"- check()
    test("unicode") - check()
    test("unix_line_endings") - checkGolden()
    test("unparse") - checkGolden()
    test("verbatim_strings") - check()
    test("issue_127") - check()
  }
}

