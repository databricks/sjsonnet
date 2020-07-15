package sjsonnet

import utest._

object Std0150FunctionsTests extends TestSuite {
  def eval(s: String) = {
    new Interpreter(
      SjsonnetMain.createParseCache(),
      Map(),
      Map(),
      DummyPath(),
      (_, _) => None
    ).interpret(s, DummyPath("(memory)")) match {
      case Right(x) => x
      case Left(e) => throw new Exception(e)
    }
  }

  def tests = Tests {
    test("stdClamp") {
      eval("std.clamp(-3, 0, 5)") ==> ujson.Num(0)
      eval("std.clamp(4, 0, 5)") ==> ujson.Num(4)
      eval("std.clamp(7, 0, 5)") ==> ujson.Num(5)
    }
  }
}
