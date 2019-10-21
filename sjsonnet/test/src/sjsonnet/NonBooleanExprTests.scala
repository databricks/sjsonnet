package sjsonnet

import utest._

object NonBooleanExprTests extends TestSuite{
  def eval(s: String) = {
    new Interpreter(
      SjsonnetMain.createParseCache(),
      Map(),
      Map(),
      DummyPath(),
      (_, _) => None
    ).interpret(s, DummyPath("(memory)")) match{
      case Right(x) => x
      case Left(e) => throw new Exception(e)
    }
  }
  def tests = Tests{
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
