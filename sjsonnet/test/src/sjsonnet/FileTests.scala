package sjsonnet

import utest._

object FileTests extends TestSuite{
  def eval(s: String) = {
    Evaluator.visitExpr(
      Parser.expr.parse(s).get.value,
      new Scope(None, None, None, Map("std" -> Ref(Scope.Std)))
    )
  }
  def check(expected: ujson.Js = ujson.Js.True)(implicit tp: utest.framework.TestPath) = {
    val res = Materializer(
      eval(ammonite.ops.read(ammonite.ops.pwd / 'test_suite / s"${tp.value.last}.jsonnet"))
    )
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
    'format - check()
    'functions - check()
//    'import - check()
    'invariant - check()
    'invariant_manifest - check(
      ujson.read(ammonite.ops.read(ammonite.ops.pwd / 'test_suite / "invariant_manifest.jsonnet.golden"))
    )
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
    'sanity - check(ujson.Js.False)
    'sanity2 - check(ujson.Js.False)
    'shebang - check()
    "slice.sugar" - check()
    'std_all_hidden - check()
//    'stdlib - check()
    'text_block - check()
    'unicode - check()
    'unparse - check(
      ujson.read(ammonite.ops.read(ammonite.ops.pwd / 'test_suite / "unparse.jsonnet.golden"))
    )
    'verbatim_strings - check()
  }
}

