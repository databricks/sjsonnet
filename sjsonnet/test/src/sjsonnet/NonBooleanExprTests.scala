package sjsonnet

import utest._
import TestUtils.eval
object NonBooleanExprTests extends TestSuite{
  def tests: Tests = Tests{
    test("nonBooleanExpr") {
      eval("local boo(x) = true; [x for x in [1,2,3] if boo(x)]").toString() ==> "[1,2,3]"
      eval("[x for x in [1,2,3] if x < 0]").toString() ==> "[]"
      val ex = intercept[Exception]{
        eval("[x for x in [1,2,3] if 'a']")
      }
      assert(ex.getMessage.contains("Condition must be boolean, got string"))
    }
  }
}
