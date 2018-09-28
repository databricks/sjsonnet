package sjsonnet

import utest._

object FileTests extends TestSuite{
  val testSuiteRoot = ammonite.ops.pwd / 'sjsonnet / 'test / 'resources / 'test_suite
  def eval(p: ammonite.ops.Path) = {
    val s = ammonite.ops.read(p)
    val scope = new Scope(None, None, None, Map("std" -> Ref(Scope.Std)), p, Nil, None)
    val parser = new Parser
    new Evaluator(parser, scope).visitExpr(
      parser.expr.parse(s).get.value,
      scope
    )
  }
  def check(expected: ujson.Js = ujson.Js.True)(implicit tp: utest.framework.TestPath) = {
    val res = Materializer(eval(testSuiteRoot / s"${tp.value.last}.jsonnet"))
    assert(res == expected)
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
//      check()
//       Lock in the existing progress fleshing out the stdlib
      intercept[Exception]{check()}.getMessage ==> "Field does not exist: thisFile"
    }
//    'text_block - check()
    'unicode - check()
    'unix_line_endings - checkGolden()
    'unparse - checkGolden()
    'verbatim_strings - check()
  }
}

