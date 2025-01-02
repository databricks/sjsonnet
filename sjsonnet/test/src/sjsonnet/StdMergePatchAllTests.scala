package sjsonnet

import utest._
import TestUtils.{assertSameEvalWithAndWithoutSpecialization, evalErr}

object StdMergePatchAllTests extends TestSuite {
  def tests = Tests {

    test("empty array handling") {
      assertSameEvalWithAndWithoutSpecialization("""std.foldl(std.mergePatch, [], {})""")
      assertSameEvalWithAndWithoutSpecialization("""std.foldl(std.mergePatch, [{}], {})""")
    }

    test("basic non-nested merging") {
      // Simple two object merge
      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{a: 1}, {b: 2}], {})"""
      )

      // Multiple object merge
      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{a: 1}, {b: 2}, {c: 3}], {})"""
      )

      // Later values should override earlier ones
      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{a: 1}, {a: 2}, {a: 3}], {})"""
      )

      // Mixed overrides and additions
      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{a: 1, b: 1}, {b: 2, c: 2}, {c: 3, d: 3}], {})"""
      )
    }

    test("nested object merging") {
      // Basic nested merge
      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{a: {x: 1}}, {a: {y: 2}}], {})"""
      )

      // Nested override
      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{a: {x: 1, y: 1}}, {a: {y: 2}}], {})"""
      )

      // Deep nested merge
      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{a: {b: {x: 1}}}, {a: {b: {y: 2}}}], {})"""
      )

      // Mix of nested and top-level
      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{a: {x: 1}, b: 1}, {a: {y: 2}, c: 2}], {})"""
      )
    }

    test("null handling") {
      // Null in nested object should remove field
      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{a: {x: 1, y: 1}}, {a: {y: null}}], {})"""
      )

      // Null at top level should remove field
      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{a: 1, b: 1}, {a: null}], {})"""
      )

      // A field set to null then set again should have the later value
      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{a: 1}, {a: null}, {a: 2}], {})"""
      )

      // Complex case with nulls at different levels
      assertSameEvalWithAndWithoutSpecialization("""
        local arr = [
          {a: {x: 1, y: 1}, b: 1},
          {a: {y: null}, b: null},
          {a: {z: 3}}
        ];
        std.foldl(std.mergePatch, arr, {})
      """)
    }

    test("hidden field handling") {
      // Hidden fields should always be dropped
      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{a:: 1}, {b: 2}], {})"""
      )

      // Hidden fields in later objects should be dropped
      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{a: 1}, {b:: 2}], {})"""
      )

      // Nested hidden fields should be dropped
      assertSameEvalWithAndWithoutSpecialization("""
        local arr = [{a: {h:: 1, v: 1}}, {b: 2}];
        std.foldl(std.mergePatch, arr, {})
      """)

      // Hidden fields in middle of array should be dropped
      assertSameEvalWithAndWithoutSpecialization("""
        local arr = [{a: 1}, {b:: 2}, {c: 3}];
        std.foldl(std.mergePatch, arr, {})
      """)

      // Complex nested case with hidden fields
      assertSameEvalWithAndWithoutSpecialization("""
        local arr = [
          {a: {x:: 1, y: 2}},
          {a: {y: 3, z:: 4}},
          {a: {w: 5}}
        ];
        std.foldl(std.mergePatch, arr, {})
      """)
    }

    test("ordering preservation") {
      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{b: 1, a: 1}, {c: 1, d: 1}], {})"""
      )

      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{b: 1, a: 1}, {a: 2, b: 2}], {})"""
      )
    }

    test("plus operator handling") {
      // The +: operator should be ignored during merging
      assertSameEvalWithAndWithoutSpecialization(
        """std.foldl(std.mergePatch, [{a: 1}, {a+: 2}], {})"""
      )

      // The resulting field should not be treated as +: in future merges
      assertSameEvalWithAndWithoutSpecialization("""
        local result = std.foldl(std.mergePatch, [{}, {a+: 2}], {});
        {a: 1} + result
      """)

      // +: in first object should also be ignored
      assertSameEvalWithAndWithoutSpecialization("""
        local result = std.foldl(std.mergePatch, [{a+: 2}, {}], {});
        {a: 1} + result
      """)

      // Should work the same for nested fields
      assertSameEvalWithAndWithoutSpecialization("""
        local result = std.foldl(std.mergePatch, [{a: {b+: 2}}, {}], {});
        {a: {b: 1}} + result
      """)

      // Complex case with multiple +: fields
      assertSameEvalWithAndWithoutSpecialization("""
        local arr = [{a: 1}, {a+: 2}, {b+: 3}, {b: 4}];
        std.foldl(std.mergePatch, arr, {})
      """)
    }

    test("error handling") {
      assert(evalErr("""std.foldl(std.mergePatch, null, {})""", disableBuiltinSpecialization = true).startsWith("sjsonnet.Error: Cannot call foldl on null"))
      assert(evalErr("""std.foldl(std.mergePatch, null, {})""", disableBuiltinSpecialization = false).startsWith("sjsonnet.Error: Expected array, got null"))
    }
  }
}