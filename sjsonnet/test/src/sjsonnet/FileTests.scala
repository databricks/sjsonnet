package sjsonnet

import utest._

object FileTests extends TestSuite{
  val testSuiteRoot = ammonite.ops.pwd / 'sjsonnet / 'test / 'resources / 'test_suite
  def eval(p: ammonite.ops.Path) = {
    val interp = new Interpreter(new Parser, Scope.standard(p, testSuiteRoot, Nil))
    interp.interpret(p)
  }
  def check(expected: ujson.Js = ujson.Js.True)(implicit tp: utest.framework.TestPath) = {
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
    check(ujson.read(ammonite.ops.read(testSuiteRoot / s"${tp.value.last}.jsonnet.golden")))
  }
  def tests = Tests{
    'arith_bool - check()
    'arith_float - check()
    'arith_string - check()
    'array - check()
    'assert - check()
    'binary - check()
    'comments - check()
    'condition - check()
//    'dos_line_endings - checkGolden()
    'format - check()
//    'formatter - checkGolden()
    'formatting_braces - checkGolden()
    'formatting_braces2 - checkGolden()
    'functions - check()
    'import - check()
    'invariant - check()
    'invariant_manifest - checkGolden()
    'local - check()
    'merge - check()
    'null - check()
    'object - check()
    'oop - check()
    'oop_extra - check()
    'parsing_edge_cases - check()
    'precedence - check()
//    'recursive_function - check()
    'recursive_import_ok - check()
    'recursive_object - check()
    'sanity - checkGolden()
    'sanity2 - checkGolden()
    'shebang - check()
    "slice.sugar" - check()
    'std_all_hidden - check()
    'stdlib - {
      check()
//       Lock in the existing progress fleshing out the stdlib
//      checkFail(
//        """sjsonnet.Error: Field does not exist: thisFile
//          |    at .(sjsonnet/test/resources/test_suite/stdlib.jsonnet:378:9)
//          |""".stripMargin
//      )
    }
    'text_block - check()
    'unicode - check()
    'unix_line_endings - checkGolden()
    'unparse - checkGolden()
    'verbatim_strings - check()
  }
}

