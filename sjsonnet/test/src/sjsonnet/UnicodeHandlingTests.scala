package sjsonnet

import utest._
import TestUtils.{eval, evalErr}

/**
 * Tests for correct handling of Unicode strings, especially those that require surrogate pairs in
 * UTF-16 (i.e., codepoints above U+FFFF).
 */
object UnicodeHandlingTests extends TestSuite {
  def tests: Tests = Tests {

    test("stringLength") {
      eval("std.length('🌍')") ==> ujson.Num(1)
      eval("std.length('Hello 🌍')") ==> ujson.Num(7)
      eval("std.length('👨‍👩‍👧‍👦')") ==> ujson.Num(7) // Family emoji with ZWJ sequences
    }

    test("stringIndex") {
      eval("'Hello 🌍 World'[6]") ==> ujson.Str("🌍")
      eval("'A🌍B'[1]") ==> ujson.Str("🌍")
      assert(evalErr("'A🌍B'[3]").contains("string bounds error"))
    }

    test("codepoint") {
      eval("std.codepoint('🌍')") ==> ujson.Num(127757)
      assert(evalErr("std.codepoint('')").contains("expected a single character string"))
      assert(evalErr("std.codepoint('🌍!')").contains("expected a single character string"))
      assert(evalErr("std.codepoint('abc')").contains("expected a single character string"))
    }

    test("char") {
      eval("std.char(127757)") ==> ujson.Str("🌍")
    }

    test("stringChars") {
      eval("std.stringChars('🌍')") ==> ujson.Arr("🌍")
      eval("std.stringChars('Hello 🌍')") ==> ujson.Arr("H", "e", "l", "l", "o", " ", "🌍")
    }

    test("map") {
      eval("std.map(function(x) std.codepoint(x), '🌍')") ==> ujson.Arr(127757)
      eval("std.map(function(x) std.codepoint(x), 'A🌍B')") ==> ujson.Arr(65, 127757, 66)
    }

    test("substr") {
      eval("std.substr('A🌍B', 0, 1)") ==> ujson.Str("A")
      eval("std.substr('A🌍B', 1, 1)") ==> ujson.Str("🌍")
      eval("std.substr('A🌍B', 2, 1)") ==> ujson.Str("B")
      eval("std.substr('Hello 🌍 World', 6, 100)") ==> ujson.Str("🌍 World")
      eval("std.substr('🌍', 1, 5)") ==> ujson.Str("") // Beyond string length
    }

    test("stringSlice") {
      eval("'A🌍B'[0:1]") ==> ujson.Str("A")
      eval("'A🌍B'[1:2]") ==> ujson.Str("🌍")
      eval("'A🌍B🚀C'[0:5:2]") ==> ujson.Str("ABC")
      eval("'ABC🚀'[-2:]") ==> ujson.Str("C🚀")
    }

    test("codepointVsUtf16Ordering") {
      // This test demonstrates why sjsonnet uses Unicode codepoint ordering instead of UTF-16 code unit ordering.
      //
      // The problem: UTF-16 encodes characters above U+FFFF as "surrogate pairs" - two 16-bit code units.
      // When comparing strings by UTF-16 code units, we only look at the raw 16-bit values, not the
      // actual Unicode codepoints they represent.
      //
      // Critical test case:
      // - U+FFFF = "\uFFFF" → UTF-16: [0xFFFF] (single code unit)
      // - U+10000 = "\uD800\uDC00" → UTF-16: [0xD800, 0xDC00] (surrogate pair: high + low surrogate)
      //
      // Correct Unicode codepoint order: U+FFFF (65535) < U+10000 (65536) → "\uFFFF" < "\uD800\uDC00"
      //
      // Wrong UTF-16 code unit order: When comparing "\uFFFF" vs "\uD800\uDC00", UTF-16 comparison sees:
      //   0xFFFF (65535) vs 0xD800 (55296) ← only looks at first code unit of surrogate pair!
      //   Since 65535 > 55296, it incorrectly concludes "\uFFFF" > "\uD800\uDC00"
      //
      // This breaks ordering for ALL characters above U+FFFF (emojis, math symbols, etc.)

      val testStrings = Array("\uFFFF", "\uD800\uDC00") // U+FFFF, U+10000

      // Scala's default string ordering uses UTF-16 code units: "\uD800\uDC00" < "\uFFFF"
      val utf16Sorted = testStrings.sorted.toList
      utf16Sorted ==> List("\uD800\uDC00", "\uFFFF")

      // Our Unicode codepoint ordering: "\uFFFF" < "\uD800\uDC00"
      val codepointSorted = testStrings.sorted(sjsonnet.Util.CodepointStringOrdering).toList
      codepointSorted ==> List("\uFFFF", "\uD800\uDC00")

      // Critical: these produce different results! This test would fail with UTF-16 ordering.
      assert(utf16Sorted != codepointSorted)

      // Jsonnet string operations should use Unicode codepoint ordering
      eval("'\\uFFFF' < '\\uD800\\uDC00'") ==> ujson.Bool(true)
      eval("std.sort(['\\uD800\\uDC00', '\\uFFFF'])") ==> ujson.Arr("\uFFFF", "\uD800\uDC00")
    }

    // sjsonnet allows unpaired surrogates in Unicode escape sequences,
    // unlike go-jsonnet and C++ jsonnet which reject them at parse time.

    test("unpairedSurrogatesInEscapes") {
      // These should parse successfully (go-jsonnet/C++ jsonnet would fail)
      eval("\"\\uD800\"") ==> ujson.Str("\uD800") // High surrogate
      eval("\"\\uDC00\"") ==> ujson.Str("\uDC00") // Low surrogate
      eval("\"\\uD83D\\uDE00\"") ==> ujson.Str("😀") // Valid pair
    }

    test("stdCharPreservesRawSurrogates") {
      // std.char() preserves raw surrogate codepoints (go-jsonnet replaces with U+FFFD)
      eval("std.codepoint(std.char(55296))") ==> ujson.Num(55296) // 0xD800 high surrogate
      eval("std.codepoint(std.char(56320))") ==> ujson.Num(56320) // 0xDC00 low surrogate
    }

    test("stringComparisons") {
      // Comprehensive test of ALL comparison operators with the critical UTF-16 vs Unicode boundary case
      // This ensures mutation testing catches bugs in any specific comparison implementation

      val maxBmp = "'\\uFFFF'" // U+FFFF (max Basic Multilingual Plane)
      val minSupplementary = "'\\uD800\\uDC00'" // U+10000 (min Supplementary Plane)

      // All comparison operators must use Unicode codepoint ordering
      // With UTF-16 code unit ordering, these would ALL give wrong results

      // Less than: U+FFFF < U+10000
      eval(s"$maxBmp < $minSupplementary") ==> ujson.Bool(true)
      eval(s"$minSupplementary < $maxBmp") ==> ujson.Bool(false)

      // Less than or equal: U+FFFF <= U+10000
      eval(s"$maxBmp <= $minSupplementary") ==> ujson.Bool(true)
      eval(s"$minSupplementary <= $maxBmp") ==> ujson.Bool(false)
      eval(s"$maxBmp <= $maxBmp") ==> ujson.Bool(true) // Reflexivity

      // Greater than: U+10000 > U+FFFF
      eval(s"$minSupplementary > $maxBmp") ==> ujson.Bool(true)
      eval(s"$maxBmp > $minSupplementary") ==> ujson.Bool(false)

      // Greater than or equal: U+10000 >= U+FFFF
      eval(s"$minSupplementary >= $maxBmp") ==> ujson.Bool(true)
      eval(s"$maxBmp >= $minSupplementary") ==> ujson.Bool(false)
      eval(s"$maxBmp >= $maxBmp") ==> ujson.Bool(true) // Reflexivity

      // Equality and inequality
      eval(s"$maxBmp == $minSupplementary") ==> ujson.Bool(false)
      eval(s"$maxBmp != $minSupplementary") ==> ujson.Bool(true)
      eval(s"$maxBmp == $maxBmp") ==> ujson.Bool(true) // Reflexivity
      eval(s"$maxBmp != $maxBmp") ==> ujson.Bool(false)

      // Array sorting must also use codepoint ordering
      eval(s"std.sort([$minSupplementary, $maxBmp, 'Z', 'A'])") ==> ujson.Arr(
        "A",
        "Z",
        "\uFFFF",
        "\uD800\uDC00"
      )
    }

    test("objectFieldOrdering") {
      // Test object field ordering with the critical UTF-16 vs Unicode boundary case
      // Input deliberately in REVERSE codepoint order to make the test non-trivial
      val testObject = "{\"\uD800\uDC00\": 4, \"\uFFFF\": 3, \"z\": 2, \"a\": 1}"

      // All object field functions should sort by Unicode codepoint order
      eval(s"std.objectFields($testObject)") ==> ujson.Arr("a", "z", "\uFFFF", "\uD800\uDC00")
      eval(s"std.objectFieldsAll($testObject)") ==> ujson.Arr("a", "z", "\uFFFF", "\uD800\uDC00")

      // Default object rendering also uses codepoint ordering
      eval(testObject).toString ==> "{\"a\":1,\"z\":2,\"\uFFFF\":3,\"\uD800\uDC00\":4}"

      // JSON manifest should maintain the same ordering
      eval(s"std.manifestJsonMinified($testObject)") ==>
      ujson.Str("{\"a\":1,\"z\":2,\"\uFFFF\":3,\"\uD800\uDC00\":4}")

      // TOML manifest should also use codepoint ordering (with escaped Unicode)
      eval(s"std.manifestTomlEx($testObject, '  ')") ==>
      ujson.Str("a = 1\nz = 2\n\"\\uffff\" = 3\n\"\\ud800\\udc00\" = 4")
    }

    test("flatMap") {
      // Test that std.flatMap now uses code point semantics like std.map
      // This tests the consistency fix for flatMap on strings

      // Basic ASCII test
      eval("std.flatMap(function(x) x + x, 'ABC')") ==> ujson.Str("AABBCC")

      // Unicode emoji test - should handle each emoji as a single character
      eval("std.flatMap(function(x) x + '!', '🌍🚀')") ==> ujson.Str("🌍!🚀!")

      // Critical test: surrogate pair boundary case
      // U+FFFF followed by U+10000 (which needs surrogate pair)
      eval("std.flatMap(function(x) x + '-', '\\uFFFF\\uD800\\uDC00')") ==> ujson.Str(
        "\uFFFF-\uD800\uDC00-"
      )

      // Test consistency with std.map behavior
      val testStr = "'A🌍B'";
      eval(s"std.length(std.flatMap(function(x) x, $testStr))") ==>
      eval(s"std.length(std.map(function(x) x, $testStr))")

      // Test with function that returns null (should be converted to empty string)
      eval("std.flatMap(function(x) if x == '🌍' then null else x, 'A🌍B')") ==> ujson.Str("AB")

      // Array flatMap should still work (unchanged behavior)
      eval("std.flatMap(function(x) [x, x], [1, 2])") ==> ujson.Arr(1, 1, 2, 2)
    }

    test("findSubstr") {
      // Test that std.findSubstr now returns code point offsets instead of code unit positions
      // This tests the consistency fix for findSubstr indices

      // Basic ASCII test - should be unchanged
      eval("std.findSubstr('l', 'hello')") ==> ujson.Arr(2, 3)
      eval("std.findSubstr('o', 'hello world')") ==> ujson.Arr(4, 7)

      // Test with Unicode emojis - positions should be in code points
      eval("std.findSubstr('🌍', 'Hello 🌍 World')") ==> ujson.Arr(6)
      eval("std.findSubstr('o', 'Hello 🌍 World')") ==> ujson.Arr(4, 9)

      // Critical test: surrogate pair boundary case
      // The string "A\uFFFF\uD800\uDC00B" has:
      // - 'A' at code point 0
      // - U+FFFF at code point 1
      // - U+10000 (\uD800\uDC00) at code point 2
      // - 'B' at code point 3
      eval("std.findSubstr('\\uFFFF', 'A\\uFFFF\\uD800\\uDC00B')") ==> ujson.Arr(1)
      eval("std.findSubstr('\\uD800\\uDC00', 'A\\uFFFF\\uD800\\uDC00B')") ==> ujson.Arr(2)
      eval("std.findSubstr('B', 'A\\uFFFF\\uD800\\uDC00B')") ==> ujson.Arr(3)

      // Test with pattern that spans multiple code points
      eval("std.findSubstr('🌍🚀', '🌍🚀 and more 🌍🚀')") ==> ujson.Arr(0, 12)

      // Empty pattern should return empty array
      eval("std.findSubstr('', 'test')") ==> ujson.Arr()

      // Non-existent pattern should return empty array
      eval("std.findSubstr('xyz', 'hello world')") ==> ujson.Arr()

      // Consistency check: positions returned by findSubstr should work with substr
      val testString = "'Hello 🌍 World'";
      val searchPattern = "'🌍'";
      // This verifies that the index returned by findSubstr works with substr to extract the same pattern
      eval(
        s"std.substr($testString, std.findSubstr($searchPattern, $testString)[0], std.length($searchPattern))"
      ) ==> ujson.Str("🌍")
    }
  }
}
