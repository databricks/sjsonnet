package sjsonnet

import sjsonnet.TestUtils.eval
import utest._

object StdRegexTests extends TestSuite {
  def tests: Tests = Tests {
    test("std.native - regex") {
      eval("""std.native("regexPartialMatch")("a(b)c", "cabc")""") ==> ujson.Obj(
        "string" -> "abc",
        "captures" -> ujson.Arr("b"),
        "namedCaptures" -> ujson.Obj()
      )
      eval("""std.native("regexPartialMatch")("a(?P<foo>b)c", "cabc")""") ==> ujson.Obj(
        "string" -> "abc",
        "captures" -> ujson.Arr("b"),
        "namedCaptures" -> ujson.Obj(
          "foo" -> ujson.Str("b")
        )
      )
      eval("""std.native("regexPartialMatch")("a(b)c", "def")""") ==> ujson.Null
      eval("""std.native("regexPartialMatch")("a(b)c", "abcabc")""") ==> ujson.Obj(
        "string" -> "abc",
        "captures" -> ujson.Arr("b", "b"),
        "namedCaptures" -> ujson.Obj()
      )
      eval("""std.native("regexFullMatch")("a(b)c", "abc")""") ==> ujson.Obj(
        "string" -> "abc",
        "captures" -> ujson.Arr("b"),
        "namedCaptures" -> ujson.Obj()
      )
      eval("""std.native("regexFullMatch")("a(?P<foo>b)c", "abc")""") ==> ujson.Obj(
        "string" -> "abc",
        "captures" -> ujson.Arr("b"),
        "namedCaptures" -> ujson.Obj(
          "foo" -> ujson.Str("b")
        )
      )
      eval("""std.native("regexFullMatch")("a(b)c", "cabc")""") ==> ujson.Null
      eval("""std.native("regexFullMatch")("a(b)c", "def")""") ==> ujson.Null
      eval("""std.native("regexGlobalReplace")("abcbbb", "b", "d")""") ==> ujson.Str("adcddd")
      eval("""std.native("regexReplace")("abcbbb", "b", "d")""") ==> ujson.Str("adcbbb")
      eval("""std.native("regexQuoteMeta")("a.b")""") ==> ujson.Str(Platform.regexQuote("a.b"))
    }
  }
}
