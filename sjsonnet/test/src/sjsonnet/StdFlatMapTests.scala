package sjsonnet

import utest._
import TestUtils.{eval, evalErr}
object StdFlatMapTests extends TestSuite {

  def tests = Tests {
    test("stdFlatMap") {
      eval("std.flatMap(function(x) [x, x], [1, 2, 3])") ==> ujson.Arr(1, 1, 2, 2, 3, 3)
      eval("std.flatMap(function(x) if x == 2 then [] else [x], [1, 2, 3])") ==> ujson.Arr(1, 3)
      eval("std.flatMap(function(x) if x == 2 then [] else [x * 3, x * 2], [1, 2, 3])") ==> ujson.Arr(3, 2, 9, 6)

      eval("std.flatMap(function(x) x + x, 'Hello')") ==> ujson.Str("HHeelllloo")

      eval("std.flatMap(function (x) if x == \" \" then null else x, \"a b c d e\")") ==> ujson.Str("abcde")

      assert(
        evalErr("std.flatMap(function(x) 123, 'Hello')")
          .startsWith("sjsonnet.Error: flatMap func must return string, got number")
      )

    }
  }
}
