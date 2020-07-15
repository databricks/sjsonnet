package sjsonnet

import utest._

object StdFlatMapTests extends TestSuite {
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
    test("stdFlatMap") {
      eval("std.flatMap(function(x) [x, x], [1, 2, 3])") ==> ujson.Arr(1, 1, 2, 2, 3, 3)
      eval("std.flatMap(function(x) if x == 2 then [] else [x], [1, 2, 3])") ==> ujson.Arr(1, 3)
      eval("std.flatMap(function(x) if x == 2 then [] else [x * 3, x * 2], [1, 2, 3])") ==> ujson.Arr(3, 2, 9, 6)

      eval("std.flatMap(function(x) x + x, 'Hello')") ==> ujson.Str("HHeelllloo")
      eval("std.flatMap(function(x) [x + x], 'Hello')") ==> ujson.Str("[\"HH\"][\"ee\"][\"ll\"][\"ll\"][\"oo\"]")
    }
  }
}
