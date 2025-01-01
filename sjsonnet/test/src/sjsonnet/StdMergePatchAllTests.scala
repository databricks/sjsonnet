package sjsonnet

import utest._
import TestUtils.{eval, evalErr}

object StdMergePatchAllTests extends TestSuite {
  def tests = Tests {

    test("empty array handling") {
      eval("std.mergePatchAll([])") ==> ujson.Obj()
      eval("std.mergePatchAll([{}])") ==> ujson.Obj()

      // Verify same as foldl
      eval("""
        local arr = [];
        std.assertEqual(
          std.mergePatchAll(arr),
          std.foldl(std.mergePatch, arr, {})
        )
      """) ==> ujson.True
    }

    test("basic non-nested merging") {
      // Simple two object merge
      eval("std.mergePatchAll([{a: 1}, {b: 2}])") ==> ujson.Obj("a" -> 1, "b" -> 2)

      // Multiple object merge
      eval("std.mergePatchAll([{a: 1}, {b: 2}, {c: 3}])") ==> ujson.Obj("a" -> 1, "b" -> 2, "c" -> 3)

      // Later values should override earlier ones
      eval("std.mergePatchAll([{a: 1}, {a: 2}, {a: 3}])") ==> ujson.Obj("a" -> 3)

      // Mixed overrides and additions
      eval("std.mergePatchAll([{a: 1, b: 1}, {b: 2, c: 2}, {c: 3, d: 3}])") ==>
        ujson.Obj("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 3)

      // Verify against foldl
      eval("""
        local arr = [{a: 1, b: 1}, {b: 2, c: 2}, {c: 3, d: 3}];
        std.assertEqual(
          std.mergePatchAll(arr),
          std.foldl(std.mergePatch, arr, {})
        )
      """) ==> ujson.True
    }

    test("nested object merging") {
      // Basic nested merge
      eval("std.mergePatchAll([{a: {x: 1}}, {a: {y: 2}}])") ==>
        ujson.Obj("a" -> ujson.Obj("x" -> 1, "y" -> 2))

      // Nested override
      eval("std.mergePatchAll([{a: {x: 1, y: 1}}, {a: {y: 2}}])") ==>
        ujson.Obj("a" -> ujson.Obj("x" -> 1, "y" -> 2))

      // Deep nested merge
      eval("std.mergePatchAll([{a: {b: {x: 1}}}, {a: {b: {y: 2}}}])") ==>
        ujson.Obj("a" -> ujson.Obj("b" -> ujson.Obj("x" -> 1, "y" -> 2)))

      // Mix of nested and top-level
      eval("std.mergePatchAll([{a: {x: 1}, b: 1}, {a: {y: 2}, c: 2}])") ==>
        ujson.Obj("a" -> ujson.Obj("x" -> 1, "y" -> 2), "b" -> 1, "c" -> 2)

      // Verify against foldl for complex nested case
      eval("""
        local arr = [{a: {b: {x: 1}}}, {a: {b: {y: 2}}}, {a: {c: 3}}];
        std.assertEqual(
          std.mergePatchAll(arr),
          std.foldl(std.mergePatch, arr, {})
        )
      """) ==> ujson.True
    }

    test("null handling") {
      // Null in nested object should remove field
      eval("std.mergePatchAll([{a: {x: 1, y: 1}}, {a: {y: null}}])") ==>
        ujson.Obj("a" -> ujson.Obj("x" -> 1))

      // Null at top level should remove field
      eval("std.mergePatchAll([{a: 1, b: 1}, {a: null}])") ==>
        ujson.Obj("b" -> 1)

      // A field set to null then set again should have the later value
      eval("std.mergePatchAll([{a: 1}, {a: null}, {a: 2}])") ==>
        ujson.Obj("a" -> 2)

      // Verify against foldl
      eval("""
        local arr = [{a: {x: 1, y: 1}}, {a: {y: null}}, {a: {z: 3}}];
        std.assertEqual(
          std.mergePatchAll(arr),
          std.foldl(std.mergePatch, arr, {})
        )
      """) ==> ujson.True
    }

    test("hidden field handling") {
      // Hidden fields should always be dropped, no matter where they appear in the array
      eval("std.objectFieldsAll(std.mergePatchAll([{a:: 1}, {b: 2}]))") ==>
        ujson.Arr("b")
      eval("std.objectFieldsAll(std.mergePatchAll([{a: 1}, {b:: 2}]))") ==>
        ujson.Arr("a")

      // Nested hidden fields should also be dropped
      eval("""
        local result = std.mergePatchAll([{a: {h:: 1, v: 1}}, {b: 2}]);
        std.objectFieldsAll(result.a)
      """) ==> ujson.Arr("v")

      // Hidden fields in middle of array should be dropped
      eval("""
        local result = std.mergePatchAll([{a: 1}, {b:: 2}, {c: 3}]);
        std.objectFieldsAll(result)
      """) ==> ujson.Arr("a", "c")

      // Complex nested case with hidden fields
      eval("""
        local arr = [
          {a: {x:: 1, y: 2}},
          {a: {y: 3, z:: 4}},
          {a: {w: 5}}
        ];
        std.objectFieldsAll(std.mergePatchAll(arr).a)
      """) ==> ujson.Arr("y", "w")

      // Verify all hidden field cases against foldl
      eval("""
        local arr = [
          {a: {h:: 1, v: 1, w:: 2}},
          {b:: 2, c: {d:: 3, e: 4}},
          {f:: 5, g: 6}
        ];
        std.assertEqual(
          std.mergePatchAll(arr),
          std.foldl(std.mergePatch, arr, {})
        )
      """) ==> ujson.True
    }

    test("ordering preservation") {
      // Should preserve order from first object that defines each field
      eval("std.mergePatchAll([{b: 1, a: 1}, {c: 1, d: 1}])", preserveOrder = true).toString ==>
        """{"b":1,"a":1,"c":1,"d":1}"""

      // Later objects shouldn't change ordering of existing fields
      eval("std.mergePatchAll([{b: 1, a: 1}, {a: 2, b: 2}])", preserveOrder = true).toString ==>
        """{"b":2,"a":2}"""

      // Verify ordering behavior matches foldl
      eval("""
        local arr = [{b: 1, a: 1}, {c: 1, d: 1}];
        std.assertEqual(
          std.mergePatchAll(arr),
          std.foldl(std.mergePatch, arr, {})
        )
      """) ==> ujson.True
    }

    test("plus operator handling") {
      // The +: operator should be ignored during merging
      eval("std.mergePatchAll([{a: 1}, {a+: 2}])") ==> ujson.Obj("a" -> 2)

      // The resulting field should not be treated as +: in future merges
      eval("{a: 1} + std.mergePatchAll([{}, {a+: 2}])") ==> ujson.Obj("a" -> 2)

      // +: in first object should also be ignored
      eval("{a: 1} + std.mergePatchAll([{a+: 2}, {}])") ==> ujson.Obj("a" -> 2)

      // Should work the same for nested fields
      eval("{a: {b: 1}} + std.mergePatchAll([{a: {b+: 2}}, {}])") ==>
        ujson.Obj("a" -> ujson.Obj("b" -> 2))

      // Verify against foldl with +:
      eval("""
        local arr = [{a: 1}, {a+: 2}, {b+: 3}];
        std.assertEqual(
          std.mergePatchAll(arr),
          std.foldl(std.mergePatch, arr, {})
        )
      """) ==> ujson.True
    }

    test("error handling") {
      // Array must contain only objects
      assert(evalErr("std.mergePatchAll([{a: 1}, 2, {b: 3}])").startsWith("sjsonnet.Error: Expected array of objects"))

      // Input must be an array
      assert(evalErr("std.mergePatchAll({a: 1})").startsWith("sjsonnet.Error: Expected array"))
    }
  }
}