package sjsonnet

import utest._

object StdRegexTests extends TestSuite{
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
    test("stdRegex") {
      eval("std.regexFullMatch('ab.*c', 'abxyzc')") ==> ujson.True
//      eval("std.pow(x=2, n=3)") ==> ujson.Num(8)
//      eval("std.pow(n=3, x=2)") ==> ujson.Num(8)
//      eval("({a:: 1} + {a+:::2}).a") ==> ujson.Num(3)
//      eval("(std.prune({a:: 1}) + {a+:::2}).a") ==> ujson.Num(2)
//      eval("std.toString(std.mapWithIndex(function(idx, elem) elem, [2,1,0]))") ==> ujson.Str("[2, 1, 0]")
    }
  }
}
