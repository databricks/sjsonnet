package sjsonnet

import utest._
import TestUtils.{eval, evalErr}
object StdMergePatchTests extends TestSuite {

  def tests = Tests {
    test("mergePatch - basic") {
      eval("std.mergePatch([{a: 1}], [{b: 2}])") ==> ujson.Arr(ujson.Obj("b" -> 2))
    }
    test("mergePatch - null input") {
      eval("std.mergePatch(null, {a: 1})") ==> ujson.Obj("a" -> 1)
    }
    test("mergePatch - null field") {
      eval("std.mergePatch({a: null}, {b: 2})") ==> ujson.Obj("a" -> ujson.Null, "b" -> 2)
    }
    test("mergePatch - hidden fields") {
      eval("{a:: 1} + std.mergePatch({}, {a: 2})") ==> ujson.Obj("a" -> 2)
    }
    test("mergePatch - plus fields") {
      eval("{a: 1} + std.mergePatch({}, {a+: 2})") ==> ujson.Obj("a" -> 2)
    }
    test("mergePatch - nested objects") {
      eval("""std.mergePatch({"a": {b: "B"}}, {a: {c: "C"}})""") ==>
        ujson.Obj("a" -> ujson.Obj("b" -> "B", "c" -> "C"))
    }

    // New mergePatchAll tests
    test("mergePatchAll - empty array") {
      eval("std.mergePatchAll([])") ==> ujson.Obj()
    }

    test("mergePatchAll - single object") {
      eval("std.mergePatchAll([{a: 1}])") ==> ujson.Obj("a" -> 1)
    }

    test("mergePatchAll - basic merge") {
      eval("std.mergePatchAll([{a: 1}, {b: 2}])") ==>
        ujson.Obj("a" -> 1, "b" -> 2)
    }

    test("mergePatchAll - null field handling") {
      eval("std.mergePatchAll([{a: 1}, {a: null}, {b: 2}])") ==>
        ujson.Obj("b" -> 2)
    }

    test("mergePatchAll - nested object merge") {
      eval("""std.mergePatchAll([
        {a: {x: 1}},
        {a: {y: 2}},
        {a: {z: 3}}
      ])""") ==> ujson.Obj(
        "a" -> ujson.Obj(
          "x" -> 1,
          "y" -> 2,
          "z" -> 3
        )
      )
    }

    test("mergePatchAll - overwrite primitives") {
      eval("std.mergePatchAll([{a: 1}, {a: {b: 2}}, {a: 3}])") ==>
        ujson.Obj("a" -> 3)
    }

    test("mergePatchAll - hidden fields") {
      eval("{a:: 1} + std.mergePatchAll([{a:: 1}, {a: 2}])") ==>
        ujson.Obj("a" -> 2)
    }

    test("mergePatchAll - visibility override") {
      eval("std.mergePatchAll([{a:: 1}, {a::: 2}])") ==>
        ujson.Obj("a" -> 2)
    }

    test("mergePatchAll - plus fields") {
      eval("std.mergePatchAll([{a: 1}, {a+: 2}])") ==>
        ujson.Obj("a" -> 2)
    }

    test("mergePatchAll - error on non-object") {
      assert(evalErr("std.mergePatchAll([{a: 1}, 2, {b: 3}])").startsWith("sjsonnet.Error: Expected array of objects"))
    }

    // TODO: need way way way more test cases for equivalence here.
    test("mergePatchAll - equivalent to foldl mergePatch") {
      eval("""
        local arr = [{a: 1}, {b: 2}, {a: 3}];
        std.assertEqual(
          std.mergePatchAll(arr),
          std.foldl(std.mergePatch, arr, {})
        )
      """) ==> ujson.Bool(true)
    }
  }
}