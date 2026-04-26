package sjsonnet

import utest._
import TestUtils.eval

object StdFilterMapTests extends TestSuite {
  def tests: Tests = Tests {
    test("filterMap preserves lazy element evaluation") {
      eval("""std.filterMap(function(x) false, function(x) error "map", [error "elem"])""") ==>
      ujson.Arr()

      eval("""std.filterMap(function(x) true, function(x) 1, [error "elem"])""") ==>
      ujson.Arr(1)
    }
  }
}
