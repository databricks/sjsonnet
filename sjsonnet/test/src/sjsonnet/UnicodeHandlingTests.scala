package sjsonnet

import utest._
import TestUtils.{eval, evalErr}

/**
 * Tests for correct handling of Unicode strings, especially those that require surrogate pairs in
 * UTF-16 (i.e., characters with codepoints above U+FFFF).
 */
object UnicodeHandlingTests extends TestSuite {
  def tests: Tests = Tests {

    test("stringLength") {
      eval("std.length('🌍')") ==> ujson.Num(1)
      eval("std.length('Hello 🌍')") ==> ujson.Num(7)
      eval("std.length('ASCII only')") ==> ujson.Num(10)
      // Jsonnet strings are defined over codepoints, not grapheme clusters, so the
      // following "family" emoji has a length of 7 (because it has 7 codepoints):
      eval("std.length('👨‍👩‍👧‍👦')") ==> ujson.Num(7)
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

    test("flatMap") {
      eval("std.flatMap(function(x) x + '!', '🌍🚀')") ==> ujson.Str("🌍!🚀!")
    }

    test("substr") {
      eval("std.substr('A🌍B', 0, 1)") ==> ujson.Str("A")
      eval("std.substr('A🌍B', 1, 1)") ==> ujson.Str("🌍")
      eval("std.substr('A🌍B', 2, 1)") ==> ujson.Str("B")
      eval("std.substr('ASCII only', 6, 4)") ==> ujson.Str("only")
      eval("std.substr('Hello 🌍 World', 6, 100)") ==> ujson.Str("🌍 World")
      eval("std.substr('🌍', 1, 5)") ==> ujson.Str("") // Beyond string length
    }

    test("longAsciiLengthAndSubstr") {
      val longAscii = "a" * 1030
      eval(s"std.length('$longAscii')") ==> ujson.Num(1030)
      eval(s"std.substr('$longAscii', 1028, 20)") ==> ujson.Str("aa")
      eval(s"std.substr('$longAscii', 1031, 20)") ==> ujson.Str("")
      eval(s"std.substr('$longAscii' + '$longAscii', 2058, 10)") ==> ujson.Str("aa")
    }

    test("stringSlice") {
      eval("'A🌍B'[0:1]") ==> ujson.Str("A")
      eval("'A🌍B'[1:2]") ==> ujson.Str("🌍")
      eval("'A🌍B🚀C'[0:5:2]") ==> ujson.Str("ABC")
      eval("'ABC🚀'[-2:]") ==> ujson.Str("C🚀")
    }

    test("unicodeEscapeSequences") {
      // Test basic Unicode escapes
      eval("\"\\u0041\"") ==> ujson.Str("A")
      eval("\"\\u0048\\u0065\\u006C\\u006C\\u006F\"") ==> ujson.Str("Hello")

      // Test surrogate pair handling - new parser correctly handles these
      eval("\"\\uD83C\\uDF0D\"") ==> ujson.Str("🌍") // Earth emoji
      eval("\"\\uD83D\\uDE80\"") ==> ujson.Str("🚀") // Rocket emoji

      // Test non-surrogate high Unicode codepoints
      eval("\"\\uFFFF\"") ==> ujson.Str("\uFFFF")
    }

    test("codepointVsUtf16OrderingDemonstration") {
      // This test demonstrates the difference between UTF-16 code unit ordering (wrong)
      // and Unicode codepoint ordering (correct) at the critical boundary between
      // the Basic Multilingual Plane (BMP) and Supplementary Planes.
      //
      // Test case: U+FFFF (last BMP char) vs U+10000 (first supplementary char)
      // - U+FFFF is represented as a single UTF-16 code unit: 0xFFFF
      // - U+10000 requires a surrogate pair: 0xD800 0xDC00
      //
      // UTF-16 comparison incorrectly compares 0xFFFF > 0xD800 (first code unit only)
      // Unicode comparison correctly compares U+FFFF < U+10000 (actual codepoints)

      val testStrings = Array("\uFFFF", "\uD800\uDC00") // U+FFFF, U+10000

      // Scala's default string ordering uses UTF-16 code units: "\uD800\uDC00" < "\uFFFF"
      val utf16Sorted = testStrings.sorted.toList
      utf16Sorted ==> List("\uD800\uDC00", "\uFFFF")

      // Our Unicode codepoint ordering: "\uFFFF" < "\uD800\uDC00"
      val codepointSorted = testStrings.sorted(sjsonnet.Util.CodepointStringOrdering).toList
      codepointSorted ==> List("\uFFFF", "\uD800\uDC00")

      // These produce different results, demonstrating the bug that was fixed
      assert(utf16Sorted != codepointSorted)
    }

    test("codepointOrderingInJsonnet") {
      // Verify that Jsonnet operations use Unicode codepoint ordering
      eval("'\\uFFFF' < '\\uD800\\uDC00'") ==> ujson.Bool(true)
      eval("std.sort(['\\uD800\\uDC00', '\\uFFFF'])") ==> ujson.Arr("\uFFFF", "\uD800\uDC00")
    }

    test("rawSurrogatePrefixOrdering") {
      val rawSurrogatePrefix = "\uD800\uFFFF" // codepoints [0xD800, 0xFFFF]
      val validSurrogatePair = "\uD800\uDC00" // codepoint [0x10000]

      assert(sjsonnet.Util.compareStringsByCodepoint(rawSurrogatePrefix, validSurrogatePair) < 0)
      assert(sjsonnet.Util.compareStringsByCodepoint(validSurrogatePair, rawSurrogatePrefix) > 0)

      eval("(std.char(55296) + std.char(65535)) < (std.char(55296) + std.char(56320))") ==>
      ujson.Bool(true)

      eval(
        "std.sort([std.char(55296) + std.char(56320), std.char(55296) + std.char(65535)])"
      ) ==> ujson.Arr(rawSurrogatePrefix, validSurrogatePair)
    }

    // Unpaired surrogate handling - sjsonnet-specific behavior
    //
    // Note: This is an intentional divergence from go-jsonnet and C++ jsonnet:
    // - go/C++ reject unpaired surrogates in escape sequences at parse time
    // - go-jsonnet's std.char() replaces surrogate codepoints with U+FFFD
    // - sjsonnet was preserving unpaired surrogates throughout
    //
    // sjsonnet now reject these to align with go-jsonet/ c++ jsonnet
    //

    test("unpairedSurrogatesInEscapes") {
      // sjsonnet was parses these successfully (go/C++ would reject)
      // the new behavior will reject thesee too
      evalErr("\"\\uD800\"").contains("Expected") // High surrogate alone
      evalErr("\"\\uDC00\"").contains("Expected") // Low surrogate alone
      eval("\"\\uD83D\\uDE00\"") ==> ujson.Str("😀") // Valid surrogate pair
      eval("\"\\uD83C\\uDF0D\"") ==> ujson.Str("🌍") // Earth emoji
    }

    test("stdCharPreservesRawSurrogates") {
      // sjsonnet preserves raw surrogate codepoints (go-jsonnet would replace with U+FFFD)
      eval("std.codepoint(std.char(55296))") ==> ujson.Num(55296) // 0xD800 high surrogate
      eval("std.codepoint(std.char(56320))") ==> ujson.Num(56320) // 0xDC00 low surrogate
    }

    test("invalidSurrogateHandling") {
      // Test parser's behavior with invalid surrogate sequences
      // High surrogate not followed by low surrogate - should be handled according to parser logic
      evalErr("\"\\uD800\"").contains("Expected") // Unpaired high surrogate
      evalErr("\"\\uD8FF\"").contains("Expected") // Another high surrogate
      evalErr("\"\\uDBFF\"").contains("Expected") // Max high surrogate

      // Low surrogate without preceding high surrogate
      evalErr("\"\\uDC00\"").contains("Expected") // Unpaired low surrogate
      evalErr("\"\\uDFFF\"").contains("Expected") // Max low surrogate

      // High surrogate followed by non-low surrogate
      evalErr("\"\\uD800\\u0041\"").contains("Expected") // High + ASCII
      evalErr("\"\\uD800\\uFFFF\"").contains("Expected") // High + BMP
      evalErr("\"\\uD800\\uD801\"").contains("Expected") // High + High

      // Low surrogate preceded by non-high surrogate
      evalErr("\"\\u0041\\uDC00\"").contains("Expected") // ASCII + Low
      evalErr("\"\\uFFFF\\uDC00\"").contains("Expected") // BMP + Low

      // Multiple invalid surrogates
      evalErr("\"\\uD800\\uD801\\uDC00\"").contains("Expected") // High + High + Low
      evalErr("\"\\uDC00\\uDC01\"").contains("Expected") // Low + Low

      // Mixed valid and invalid in same string
      evalErr("\"\\uD83C\\uDF0D\\uD800\"").contains("Expected") // Valid pair + unpaired high
      evalErr("\"\\uD800\\uD83C\\uDF0D\"").contains("Expected") // Unpaired high + valid pair

      // Note: The new parser's unicode handling is more strict about surrogate pairs
    }

    test("stdCharHandling") {
      // Test std.char with valid codepoints including those requiring surrogate pairs
      eval("std.char(127757)") ==> ujson.Str("🌍") // Earth emoji
      eval("std.char(128640)") ==> ujson.Str("🚀") // Rocket emoji

      // Test with BMP characters
      eval("std.char(65)") ==> ujson.Str("A")
      eval("std.char(65535)") ==> ujson.Str("\uFFFF") // Max BMP character
    }

    test("stringComparisons") {
      val maxBmp = "'\\uFFFF'" // U+FFFF (max Basic Multilingual Plane)
      val minSupplementary = "'\\uD800\\uDC00'" // U+10000 (min Supplementary Plane)

      // Less than: U+FFFF < U+10000
      eval(s"$maxBmp < $minSupplementary") ==> ujson.Bool(true)
      eval(s"$minSupplementary < $maxBmp") ==> ujson.Bool(false)

      // Less than or equal: U+FFFF <= U+10000
      eval(s"$maxBmp <= $minSupplementary") ==> ujson.Bool(true)
      eval(s"$minSupplementary <= $maxBmp") ==> ujson.Bool(false)
      eval(s"$maxBmp <= $maxBmp") ==> ujson.Bool(true)

      // Greater than: U+10000 > U+FFFF
      eval(s"$minSupplementary > $maxBmp") ==> ujson.Bool(true)
      eval(s"$maxBmp > $minSupplementary") ==> ujson.Bool(false)

      // Greater than or equal: U+10000 >= U+FFFF
      eval(s"$minSupplementary >= $maxBmp") ==> ujson.Bool(true)
      eval(s"$maxBmp >= $minSupplementary") ==> ujson.Bool(false)
      eval(s"$maxBmp >= $maxBmp") ==> ujson.Bool(true)

      // Equality and inequality
      eval(s"$maxBmp == $minSupplementary") ==> ujson.Bool(false)
      eval(s"$maxBmp != $minSupplementary") ==> ujson.Bool(true)
      eval(s"$maxBmp == $maxBmp") ==> ujson.Bool(true)
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
      val testObject = "{\"\uD800\uDC00\": 4, \"\uFFFF\": 3, \"z\": 2, \"a\": 1}"

      // Object fields:
      eval(s"std.objectFields($testObject)") ==> ujson.Arr("a", "z", "\uFFFF", "\uD800\uDC00")
      eval(s"std.objectFieldsAll($testObject)") ==> ujson.Arr("a", "z", "\uFFFF", "\uD800\uDC00")

      // Default object rendering:
      eval(testObject).toString ==> "{\"a\":1,\"z\":2,\"\uFFFF\":3,\"\uD800\uDC00\":4}"

      // JSON manifest variants:
      eval(s"std.manifestJsonMinified($testObject)") ==>
      ujson.Str("{\"a\":1,\"z\":2,\"\uFFFF\":3,\"\uD800\uDC00\":4}")

      eval(s"std.manifestJson($testObject)") ==>
      ujson.Str("{\n    \"a\": 1,\n    \"z\": 2,\n    \"\uFFFF\": 3,\n    \"\uD800\uDC00\": 4\n}")

      eval(s"std.manifestJsonEx($testObject, '  ')") ==>
      ujson.Str("{\n  \"a\": 1,\n  \"z\": 2,\n  \"\uFFFF\": 3,\n  \"\uD800\uDC00\": 4\n}")

      // TOML manifest:
      eval(s"std.manifestTomlEx($testObject, '  ')") ==>
      ujson.Str("a = 1\nz = 2\n\"\\uffff\" = 3\n\"\\ud800\\udc00\" = 4")
    }

    test("findSubstr") {
      eval("std.findSubstr('🌍', 'Hello 🌍 World')") ==> ujson.Arr(6)
      eval("std.findSubstr('o', 'Hello 🌍 World')") ==> ujson.Arr(4, 9)
      eval("std.findSubstr('🌍🚀', '🌍🚀 and more 🌍🚀')") ==> ujson.Arr(0, 12)
    }

    test("stripChars") {
      eval("std.stripChars('Hello 🌍 World🌍H', 'Hello 🌍')") ==> ujson.Str("World")
      eval("std.lstripChars('Hello 🌍 World🌍H', 'Hello 🌍')") ==> ujson.Str("World🌍H")
      eval("std.rstripChars('Hello 🌍 World🌍H', 'Hello 🌍')") ==> ujson.Str("Hello 🌍 World")
      // Regression test for rstripChars with emoji (surrogate pairs) at end of string
      eval("""std.rstripChars("hello🎉🎉🎉", "🎉")""") ==> ujson.Str("hello")
      eval("""std.lstripChars("🎉🎉🎉hello", "🎉")""") ==> ujson.Str("hello")
      eval("""std.stripChars("🎉🎉hello🎉🎉", "🎉")""") ==> ujson.Str("hello")
      // Regression test for right-stripping ASCII chars after emoji (must not corrupt emoji)
      eval("""std.rstripChars("🌍 ", " ")""") ==> ujson.Str("🌍")
      eval("""std.trim("🌍   ")""") ==> ujson.Str("🌍")
      eval("""std.trim("   🌍   ")""") ==> ujson.Str("🌍")
    }

    test("foldl") {
      // foldl must iterate by codepoint, not UTF-16 code unit
      eval("""std.foldl(function(acc, c) acc + [c], "a😀b", [])""") ==>
      ujson.Arr("a", "😀", "b")
      eval("""std.foldl(function(acc, c) acc + 1, "a😀b", 0)""") ==> ujson.Num(3)
      eval("""std.foldl(function(acc, c) acc + [c], "🎉🔥", [])""") ==>
      ujson.Arr("🎉", "🔥")
      // Round-trip concatenation
      eval("""std.foldl(function(acc, c) acc + c, "a😀b", "")""") ==> ujson.Str("a😀b")
    }

    test("foldr") {
      // foldr must iterate by codepoint, not UTF-16 code unit
      eval("""std.foldr(function(c, acc) acc + [c], "a😀b", [])""") ==>
      ujson.Arr("b", "😀", "a")
      eval("""std.foldr(function(c, acc) acc + [c], "🎉🔥", [])""") ==>
      ujson.Arr("🔥", "🎉")
      // Round-trip concatenation (right-to-left: 'b' then '😀' then 'a')
      eval("""std.foldr(function(c, acc) acc + c, "a😀b", "")""") ==> ujson.Str("b😀a")
    }

    test("formatPercentC") {
      // %c must handle non-BMP codepoints
      eval("""std.format("%c", [128512])""") ==> ujson.Str("😀") // U+1F600
      eval("""std.format("%c", [128293])""") ==> ujson.Str("🔥") // U+1F525
      eval("""std.format("%c", [127757])""") ==> ujson.Str("🌍") // U+1F30D
      eval("""std.format("%c", [65])""") ==> ujson.Str("A") // BMP char
    }
  }
}
