package sjsonnet

import utest._

object FileTests extends TestSuite{
  def eval(s: String) = {
    Evaluator.visitExpr(
      Parser.expr.parse(s).get.value,
      new Evaluator.Scope(None, None, Map("std" -> Ref(Evaluator.Scope.Std)))
    )
  }
  def check(expected: Value = Value.True)(implicit tp: utest.framework.TestPath) = {
    val res = eval(ammonite.ops.read(ammonite.ops.pwd / 'test_suite / s"${tp.value.last}.jsonnet"))
    assert(res == expected)
    res
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
//    'format - check()
//    'functions - check()
//    'import - check()
//    'invariant - check()
//    'invariant_manifest - check()
    'local - check()
//    'merge - check()
    'null - check()
    'object - check()
//    'oop - check()
//    'oop_extra - check()
    'parsing_edge_cases - check()
    'precedence - check()
//    'recursive_function - check()
    'recursive_import_ok - check()
//    'recursive_object - check()
    'sanity - check(Value.False)
    'sanity2 - check(Value.False)
    'shebang - check()
//    "slice.sugar" - check()
//    'std_all_hidden - check()
//    'stdlib - check()
    'text_block - check()
//    "tla.simple" - check()
//    'unicode - check()
//    'unparse - check()
    'verbatim_strings - check()
  }
}

