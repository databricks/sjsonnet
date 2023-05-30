package sjsonnet

import utest._
import TestUtils.eval
object Std0150FunctionsTests extends TestSuite {

  def tests = Tests {
    test("stdClamp") {
      eval("std.clamp(-3, 0, 5)") ==> ujson.Num(0)
      eval("std.clamp(4, 0, 5)") ==> ujson.Num(4)
      eval("std.clamp(7, 0, 5)") ==> ujson.Num(5)
    }
    test("member") {
      eval("std.member('foo', 'o')") ==> ujson.True
      eval("std.member('foo', 'f')") ==> ujson.True
      eval("std.member('foo', 'x')") ==> ujson.False
      eval("std.member([], 'o')") ==> ujson.False
      eval("std.member(['f'], 'o')") ==> ujson.False
      eval("std.member(['f', 'o', 'o'], 'o')") ==> ujson.True
      eval("std.member(['f', 'o', 'o'], 'f')") ==> ujson.True
      eval("std.member(['f', 'o', 'o'], 'g')") ==> ujson.False

      eval("std.member([1, 2, 3], 1)") ==> ujson.True
      eval("std.member([1, 2, 3], 4)") ==> ujson.False

      eval("std.member([['f', 'o', 'o'], ['b', 'a', 'r']], ['f', 'o', 'o'])") ==> ujson.True
    }
    test("repeat") {
      eval("std.repeat([], 0)") ==> ujson.Arr()
      eval("std.repeat([1], 1)") ==> ujson.Arr(1)
      eval("std.repeat([1, 2], 1)") ==> ujson.Arr(1, 2)
      eval("std.repeat([1], 2)") ==> ujson.Arr(1, 1)
      eval("std.repeat([1, 2], 2)") ==> ujson.Arr(1, 2, 1, 2)
      eval("std.repeat('a', 1)") ==> ujson.Str("a")
      eval("std.repeat('a', 4)") ==> ujson.Str("aaaa")
      eval("std.repeat('ab', 4)") ==> ujson.Str("abababab")
      eval("std.repeat('a', 0)") ==> ujson.Str("")
    }

    test("join") {
      eval("std.join(' ', ['', 'foo'])") ==> ujson.Str(" foo")
      eval("std.join(' ', [null, 'foo'])") ==> ujson.Str("foo")
    }

    test("slice"){
      eval("std.slice([1, 2, 3, 4, 5, 6], 0, 4, 1)") ==> ujson.read("[ 1, 2, 3, 4 ]")
      eval("std.slice([1, 2, 3, 4, 5, 6], 1, 6, 2)") ==> ujson.read("[ 2, 4, 6 ]")
      eval("""std.slice("jsonnet", 0, 4, 1)""") ==> ujson.Str("json")
    }

    test("manifestJsonMinified"){
      eval("""std.manifestJsonMinified( { x: [1, 2, 3, true, false, null, "string\nstring"], y: { a: 1, b: 2, c: [1, 2] }, })""") ==>
        ujson.Str("{\"x\":[1,2,3,true,false,null,\"string\\nstring\"],\"y\":{\"a\":1,\"b\":2,\"c\":[1,2]}}")
    }
  }
}
