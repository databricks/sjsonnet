package sjsonnet

import utest._

object FileTests extends TestSuite{
  def eval(s: String) = {
    Evaluator.visitExpr(
      Parser.expr.parse(s).get.value,
      new Evaluator.Scope(None, None, Map("std" -> Ref(Evaluator.Scope.Std)))
    )
  }
  def check(implicit tp: utest.framework.TestPath) = {
    eval(ammonite.ops.read(ammonite.ops.pwd / 'test_suite / s"${tp.value.last}.jsonnet"))
  }
  def tests = Tests{
    'arith_bool - check
    'arith_float - check
    'arith_string - check
    'array - check
    'assert - check
    'binary - check
    'comments - check
//    'condition - check
  }
}

