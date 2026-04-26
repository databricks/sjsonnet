package sjsonnet

import utest._
import TestUtils.eval

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
  }
}
