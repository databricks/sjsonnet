package sjsonnet

import utest._

object FileTests extends TestSuite{
  def eval(s: String) = {
    val scope = new Scope(None, None, None, Map("std" -> Ref(Scope.Std)),
      ammonite.ops.pwd / 'test_suite
    )
    new Evaluator(scope).visitExpr(
      Parser.expr.parse(s).get.value,
      scope
    )
  }
  def check(expected: ujson.Js = ujson.Js.True)(implicit tp: utest.framework.TestPath) = {
    val res = Materializer(
      eval(ammonite.ops.read(ammonite.ops.pwd / 'test_suite / s"${tp.value.last}.jsonnet"))
    )
    assert(res == expected)
    res
  }
  def checkGolden()(implicit tp: utest.framework.TestPath) = {
    check(ujson.read(ammonite.ops.read(ammonite.ops.pwd / 'test_suite / s"${tp.value.last}.jsonnet.golden")))
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
//    'stdlib - check()
    'text_block - check()
    'unicode - check()
    'unix_line_endings - checkGolden()
    'unparse - checkGolden()
    'verbatim_strings - check()
  }
}

