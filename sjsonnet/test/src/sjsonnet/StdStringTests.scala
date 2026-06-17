package sjsonnet

import utest._
import TestUtils.{eval, evalErr}

object StdStringTests extends TestSuite {
  def tests: Tests = Tests {
    test("ascii case conversion preserves non-ascii characters") {
      eval("""std.asciiUpper("azAZéßı")""") ==> ujson.Str("AZAZéßı")
      eval("""std.asciiLower("azAZÉİ")""") ==> ujson.Str("azazÉİ")
    }

    test("equalsIgnoreCase uses ascii-only case conversion") {
      eval("""std.equalsIgnoreCase("FOo", "foO")""") ==> ujson.True
      eval("""std.equalsIgnoreCase("É", "é")""") ==> ujson.False
      eval("""std.equalsIgnoreCase("İ", "i")""") ==> ujson.False
    }

    test("strReplace rejects empty from string") {
      // go-jsonnet: "'from' string must not be zero length."
      val err = evalErr("""std.strReplace("abc", "", "-")""")
      assert(err.contains("'from' string must not be zero length."))
      // Normal replacement still works
      eval("""std.strReplace("hello world", "world", "jsonnet")""") ==> ujson.Str("hello jsonnet")
      eval("""std.strReplace("aaa", "a", "b")""") ==> ujson.Str("bbb")
    }
  }
}
