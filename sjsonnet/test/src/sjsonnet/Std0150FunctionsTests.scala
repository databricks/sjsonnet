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
    test("member") {
      eval("std.member('foo', 'o')") ==> ujson.True
      eval("std.member('foo', 'f')") ==> ujson.True
      eval("std.member('foo', 'x')") ==> ujson.False
      eval("std.member([], 'o')") ==> ujson.False
      eval("std.member(['f'], 'o')") ==> ujson.False
      eval("std.member(['f', 'o', 'o'], 'o')") ==> ujson.True
      eval("std.member(['f', 'o', 'o'], 'f')") ==> ujson.True
      eval("std.member(['f', 'o', 'o'], 'g')") ==> ujson.False

      eval("std.member('foo', 123)") ==> ujson.False
      eval("std.member('foo1', 1)") ==> ujson.True
      eval("std.member([1, 2, 3], 1)") ==> ujson.True
      eval("std.member([1, 2, 3], 4)") ==> ujson.False

      eval("std.member([['f', 'o', 'o'], ['b', 'a', 'r']], ['f', 'o', 'o'])") ==> ujson.True
    }

  }
}
