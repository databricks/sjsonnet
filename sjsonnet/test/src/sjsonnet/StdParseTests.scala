package sjsonnet

import utest._
import TestUtils.eval

object StdParseTests extends TestSuite {
  def tests: Tests = Tests {
    test("parse numeric strings beyond Long range") {
      eval("""std.parseInt("9223372036854775808")""") ==> ujson.Num(9223372036854777856.0)
      eval("""std.parseHex("10000000000000000")""") ==> ujson.Num(18446744073709551616.0)
      eval("""std.parseOctal("1000000000000000000000")""") ==> ujson.Num(
        9223372036854775808.0
      )
    }

    test("parseHex follows official character digit mapping") {
      eval("""std.parseHex(":")""") ==> ujson.Num(10)
      eval("""std.parseHex("?")""") ==> ujson.Num(15)
    }
  }
}
