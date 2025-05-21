package sjsonnet

import utest._
import TestUtils.eval

object StdMapTests extends TestSuite {
  def tests: Tests = Tests {
    test("stdMap") {
      eval("std.map(function(x) x * x, [])") ==> ujson.Arr()
      eval("std.map(function(x) x * x, [1, 2, 3, 4])") ==> ujson.Arr(1, 4, 9, 16)

      // Map accepts strings as well, interpreting it as an array of one-character strings
      eval("std.map(function(x) x + x, 'Hello')") ==> ujson.Arr("HH", "ee", "ll", "ll", "oo")

      // Test lazy evaluation
      eval("std.map(function(x) assert x != 'A'; x + x, 'AB')[1]") ==> ujson.Str("BB")

      // Test returning arbitrary values from the mapping function
      eval("std.map(function(x) std.codepoint(x), 'AB')") ==> ujson.Arr(65, 66)
    }
  }
}
