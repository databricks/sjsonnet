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
      eval("std.regexFullMatch('ab.*c', 'abxyz')") ==> ujson.False
      eval("std.regexPartialMatch('.*[aeiou]', 'abcdefgh')") ==> ujson.True
      eval("std.regexPartialMatch('.*[aeiou]', 'xyz')") ==> ujson.False
//      eval("std.regexQuoteMeta('foo\\\\.\\\\$')") ==> ujson.Str("foo\\\\\\\\\\\\.\\\\\\\\\\\\$")
      eval("std.regexReplace('abcdabc', '^[a-c]*', 'x')") ==> ujson.Str("xdabc")
      eval("std.regexGlobalReplace('abcdabc', '[a-c]', 'x')") ==> ujson.Str("xxxdxxx")
    }
  }
}
