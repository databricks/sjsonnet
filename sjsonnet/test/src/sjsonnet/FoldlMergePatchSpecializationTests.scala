package sjsonnet

import utest._
import TestUtils.{assertSameEvalWithAndWithoutSpecialization, eval, evalErr}

object FoldlMergePatchSpecializationTests extends TestSuite {

  @noinline
  private def check(s: String): Unit = {
    assertSameEvalWithAndWithoutSpecialization(s)
  }

  def tests = Tests {

    test("abc") {
      check("""std.foldl(std.mergePatch, [{a: 1, b: 1}, {a: null}], {})""")
    }

    test("empty array handling") {
      check("""std.foldl(std.mergePatch, [], {})""")
      check("""std.foldl(std.mergePatch, [{}], {})""")
    }

    test("single patch") {
      check("""std.foldl(std.mergePatch, [1], {})""")
      check("""std.foldl(std.mergePatch, [null], {})""")
      check("""std.foldl(std.mergePatch, [{}], {})""")
      check("""std.foldl(std.mergePatch, [{a: 1}], {})""")
      check("""std.foldl(std.mergePatch, [{a: null}], {})""")
      check("""std.foldl(std.mergePatch, [{a: {b: null}}], {})""")
      check("""std.foldl(std.mergePatch, [{a: {b: {c: null}}}], {})""")
      check("""std.objectFieldsAll(std.foldl(std.mergePatch, [{a: 1, b:: 1}], {}))""")
      check("""std.objectFieldsAll(std.foldl(std.mergePatch, [{a: {b: { c:: 1, d: 2}}}], {}).a.b)""")
    }

    test("basic non-nested merging") {
      check("""std.foldl(std.mergePatch, [{a: 1}, {b: 2}], {})""")
      check("""std.foldl(std.mergePatch, [{a: 1}, {b: 2}, {c: 3}], {})""")
      check("""std.foldl(std.mergePatch, [{a: 1}, {a: 2}, {a: 3}], {})""")
      check("""std.foldl(std.mergePatch, [{a: 1, b: 1}, {b: 2, c: 2}, {c: 3, d: 3}], {})""")
    }

    test("merging of non-object patches") {
      check("""std.foldl(std.mergePatch, [{a: 1}, 1, 2, {a: 3}], {})""")
      check("""std.foldl(std.mergePatch, [1, 2, 3, 4], {})""")
      check("""std.foldl(std.mergePatch, [1, {a: 1}], {})""")
    }

    test("nested object merging") {
      check("""std.foldl(std.mergePatch, [{a: {x: 1}}, {a: {y: 2}}], {})""")
      check("""std.foldl(std.mergePatch, [{a: {x: 1, y: 1}}, {a: {y: 2}}], {})""")
      check("""std.foldl(std.mergePatch, [{a: {b: {x: 1}}}, {a: {b: {y: 2}}}], {})""")
      check("""std.foldl(std.mergePatch, [{a: {x: 1}, b: 1}, {a: {y: 2}, c: 2}], {})""")
    }

    test("null handling") {
      check("""std.foldl(std.mergePatch, [{a: {x: 1, y: 1}}, {a: {y: null}}], {})""")
      check("""std.foldl(std.mergePatch, [{a: 1, b: 1}, {a: null}], {})""")
      check("""std.foldl(std.mergePatch, [{a: 1}, {a: null}, {a: 2}], {})""")

      check("""
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
      check("""std.foldl(std.mergePatch, [{a:: 1}, {b: 2}], {})""")
      check("""std.objectFieldsAll(std.foldl(std.mergePatch, [{a:: 1}, {b: 2}], {}))""")
      check("""std.foldl(std.mergePatch, [{a: 1}, {b:: 2}], {})""")
      check("""std.objectFieldsAll(std.foldl(std.mergePatch, [{a: 1}, {b:: 2}], {}))""")

      // Nested hidden fields should also be dropped
      check("""
        local arr = [{a: {h:: 1, v: 1}}, {b: 2}];
        std.foldl(std.mergePatch, arr, {})
      """)
      check(
        """
        local arr = [{a: {h:: 1, v: 1}}, {b: 2}];
        std.objectFieldsAll(std.foldl(std.mergePatch, arr, {}).a)
      """)

      // Hidden fields do not merge with non-hidden fields
      check("""std.foldl(std.mergePatch, [{a: {b: 1}}, {a:: {c: 1}}], {})""")
    }

    test("ordering preservation") {
      check("""std.foldl(std.mergePatch, [{b: 1, a: 1}, {c: 1, d: 1}], {})""")
      check("""std.foldl(std.mergePatch, [{b: 1, a: 1}, {a: 2, b: 2}], {})""")
    }

    test("plus operator handling") {
      // The +: operator should be ignored during merging
      check("""std.foldl(std.mergePatch, [{a: 1}, {a+: 2}], {})""")

      // The resulting field should not be treated as +: in future merges
      check("""
        local result = std.foldl(std.mergePatch, [{}, {a+: 2}], {});
        {a: 1} + result
      """)

      // +: in first object should also be ignored
      check("""
        local result = std.foldl(std.mergePatch, [{a+: 2}, {}], {});
        {a: 1} + result
      """)

      // Should work the same for nested fields
      check("""
        local result = std.foldl(std.mergePatch, [{a: {b+: 2}}, {}], {});
        {a: {b: 1}} + result
      """)
    }

    test("error handling") {
      assert(evalErr(
        """std.foldl(std.mergePatch, null, {})""",
        disableStaticApplyForBuiltInFunctions = true,
        disableBuiltinSpecialization = true
      ).startsWith("sjsonnet.Error: Cannot call foldl on null"))
      assert(evalErr(
        """std.foldl(std.mergePatch, null, {})""",
        disableStaticApplyForBuiltInFunctions = true,
        disableBuiltinSpecialization = false
      ).startsWith("sjsonnet.Error: Expected array, got null"))
    }
  }
}