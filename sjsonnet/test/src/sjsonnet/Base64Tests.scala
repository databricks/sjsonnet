package sjsonnet

import utest._

/**
 * Comprehensive Base64 tests covering:
 * - RFC 4648 Section 10 test vectors (all 7)
 * - Padding variants (0, 1, 2 padding chars)
 * - SIMD boundary sizes (12, 16, 24, 32, 48, 64 bytes)
 * - All 256 byte values coverage
 * - Roundtrip encode/decode for various sizes
 * - Invalid input handling
 * - Byte array encode/decode
 * - Large input stress tests
 *
 * Ported from aklomp/base64 test suite (test_char_table, roundtrip tests)
 * and RFC 4648 Section 10.
 */
object Base64Tests extends TestSuite {

  def eval(s: String) = TestUtils.eval(s)
  def evalErr(s: String) = TestUtils.evalErr(s)

  val tests = Tests {

    // ================================================================
    // RFC 4648 Section 10 — All official test vectors
    // ================================================================
    test("rfc4648") {
      test("empty") {
        val r = eval("""std.base64("")""")
        assert(r.str == "")
      }
      test("f") {
        val r = eval("""std.base64("f")""")
        assert(r.str == "Zg==")
      }
      test("fo") {
        val r = eval("""std.base64("fo")""")
        assert(r.str == "Zm8=")
      }
      test("foo") {
        val r = eval("""std.base64("foo")""")
        assert(r.str == "Zm9v")
      }
      test("foob") {
        val r = eval("""std.base64("foob")""")
        assert(r.str == "Zm9vYg==")
      }
      test("fooba") {
        val r = eval("""std.base64("fooba")""")
        assert(r.str == "Zm9vYmE=")
      }
      test("foobar") {
        val r = eval("""std.base64("foobar")""")
        assert(r.str == "Zm9vYmFy")
      }
    }

    // ================================================================
    // RFC 4648 Section 10 — Decode direction
    // ================================================================
    test("rfc4648Decode") {
      test("empty") {
        val r = eval("""std.base64Decode("")""")
        assert(r.str == "")
      }
      test("Zg") {
        val r = eval("""std.base64Decode("Zg==")""")
        assert(r.str == "f")
      }
      test("Zm8") {
        val r = eval("""std.base64Decode("Zm8=")""")
        assert(r.str == "fo")
      }
      test("Zm9v") {
        val r = eval("""std.base64Decode("Zm9v")""")
        assert(r.str == "foo")
      }
      test("Zm9vYg") {
        val r = eval("""std.base64Decode("Zm9vYg==")""")
        assert(r.str == "foob")
      }
      test("Zm9vYmE") {
        val r = eval("""std.base64Decode("Zm9vYmE=")""")
        assert(r.str == "fooba")
      }
      test("Zm9vYmFy") {
        val r = eval("""std.base64Decode("Zm9vYmFy")""")
        assert(r.str == "foobar")
      }
    }

    // ================================================================
    // Padding variants
    // ================================================================
    test("padding") {
      test("noPadding") {
        // Input length mod 3 == 0 → no padding
        val r = eval("""std.base64("abc")""")
        assert(r.str == "YWJj")
      }
      test("onePad") {
        // Input length mod 3 == 2 → one '='
        val r = eval("""std.base64("ab")""")
        assert(r.str == "YWI=")
      }
      test("twoPads") {
        // Input length mod 3 == 1 → two '='
        val r = eval("""std.base64("a")""")
        assert(r.str == "YQ==")
      }
    }

    // ================================================================
    // String roundtrip tests for various sizes
    // (covers SIMD processing boundaries)
    // ================================================================
    test("roundtripSizes") {
      // Generate deterministic strings of given lengths
      def makeString(len: Int): String = {
        val sb = new StringBuilder(len)
        var i = 0
        while (i < len) {
          sb.append(('A' + (i % 26)).toChar)
          i += 1
        }
        sb.toString()
      }

      val sizes = Seq(
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
        12, // SSSE3 encode unit
        13, 14, 15,
        16, // SSSE3 decode unit
        17, 23,
        24, // AVX2 encode unit
        25, 31,
        32, // AVX2 decode unit
        33, 47,
        48, // NEON encode unit / AVX-512 encode unit
        49, 63,
        64, // NEON decode unit / AVX-512 decode unit
        100, 128, 255, 256, 500, 1000, 4096
      )

      for (size <- sizes) {
        val s = makeString(size)
        val escaped = s.replace("\\", "\\\\").replace("\"", "\\\"")
        val result =
          eval(s"""std.base64Decode(std.base64("$escaped"))""")
        assert(result.str == s)
      }
    }

    // ================================================================
    // Byte array roundtrip (ported from aklomp/base64 test_char_table)
    // All 256 byte values
    // ================================================================
    test("byteValueCoverage") {
      // Encode array [0, 1, 2, ..., 255] and decode back
      val r = eval(
        """local arr = std.makeArray(256, function(i) i);
          |local encoded = std.base64(arr);
          |local decoded = std.base64DecodeBytes(encoded);
          |std.assertEqual(std.length(decoded), 256) &&
          |std.all(std.makeArray(256, function(i) decoded[i] == i))
          |""".stripMargin
      )
      assert(r == ujson.True)
    }

    // ================================================================
    // Sliding window byte coverage
    // (ported from aklomp/base64 test_char_table: loop from offset 0..255)
    // ================================================================
    test("slidingWindowBytes") {
      // For each starting offset, encode remaining bytes and roundtrip
      val offsets = Seq(0, 1, 2, 3, 7, 15, 31, 63, 127, 128, 200, 254, 255)
      for (offset <- offsets) {
        val len = 256 - offset
        val r = eval(
          s"""local arr = std.makeArray($len, function(i) (i + $offset) % 256);
             |local encoded = std.base64(arr);
             |local decoded = std.base64DecodeBytes(encoded);
             |std.assertEqual(std.length(decoded), $len) &&
             |std.all(std.makeArray($len, function(i) decoded[i] == (i + $offset) % 256))
             |""".stripMargin
        )
        assert(r == ujson.True)
      }
    }

    // ================================================================
    // Special byte patterns
    // ================================================================
    test("specialPatterns") {
      test("allZeros") {
        val r = eval(
          """local arr = std.makeArray(64, function(i) 0);
            |local encoded = std.base64(arr);
            |local decoded = std.base64DecodeBytes(encoded);
            |std.assertEqual(std.length(decoded), 64) &&
            |std.all(std.makeArray(64, function(i) decoded[i] == 0))
            |""".stripMargin
        )
        assert(r == ujson.True)
      }
      test("allFF") {
        val r = eval(
          """local arr = std.makeArray(64, function(i) 255);
            |local encoded = std.base64(arr);
            |local decoded = std.base64DecodeBytes(encoded);
            |std.assertEqual(std.length(decoded), 64) &&
            |std.all(std.makeArray(64, function(i) decoded[i] == 255))
            |""".stripMargin
        )
        assert(r == ujson.True)
      }
      test("alternating") {
        val r = eval(
          """local arr = std.makeArray(64, function(i) if i % 2 == 0 then 0 else 255);
            |local encoded = std.base64(arr);
            |local decoded = std.base64DecodeBytes(encoded);
            |std.assertEqual(std.length(decoded), 64) &&
            |std.all(std.makeArray(64, function(i) decoded[i] == (if i % 2 == 0 then 0 else 255)))
            |""".stripMargin
        )
        assert(r == ujson.True)
      }
    }

    // ================================================================
    // Base64 alphabet completeness
    // ================================================================
    test("alphabetCompleteness") {
      // The full base64 alphabet: A-Za-z0-9+/
      // Decode a string that contains every base64 character
      val r = eval(
        """local fullAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
          |local decoded = std.base64DecodeBytes(fullAlphabet);
          |local reEncoded = std.base64(decoded);
          |reEncoded == fullAlphabet
          |""".stripMargin
      )
      assert(r == ujson.True)
    }

    // ================================================================
    // Large roundtrip (ported from aklomp/base64 Moby Dick test)
    // ================================================================
    test("largeTextRoundtrip") {
      val r = eval(
        """local largeStr = std.repeat("Lorem ipsum dolor sit amet, consectetur adipiscing elit. ", 100);
          |local encoded = std.base64(largeStr);
          |local decoded = std.base64Decode(encoded);
          |decoded == largeStr
          |""".stripMargin
      )
      assert(r == ujson.True)
    }

    // ================================================================
    // Large byte array roundtrip
    // ================================================================
    test("largeByteArrayRoundtrip") {
      val r = eval(
        """local arr = std.makeArray(1000, function(i) i % 256);
          |local encoded = std.base64(arr);
          |local decoded = std.base64DecodeBytes(encoded);
          |std.assertEqual(std.length(decoded), 1000) &&
          |std.all(std.makeArray(1000, function(i) decoded[i] == i % 256))
          |""".stripMargin
      )
      assert(r == ujson.True)
    }

    // ================================================================
    // Multiple encode/decode cycles (stability test)
    // ================================================================
    test("multiCycleRoundtrip") {
      val r = eval(
        """local original = "The quick brown fox jumps over the lazy dog";
          |local e1 = std.base64(original);
          |local d1 = std.base64Decode(e1);
          |local e2 = std.base64(d1);
          |local d2 = std.base64Decode(e2);
          |local e3 = std.base64(d2);
          |local d3 = std.base64Decode(e3);
          |d3 == original && e1 == e2 && e2 == e3
          |""".stripMargin
      )
      assert(r == ujson.True)
    }

    // ================================================================
    // Known encode/decode pairs (additional test vectors)
    // All tests verify BOTH encode AND decode directions.
    // ================================================================
    test("knownPairs") {
      test("hello") {
        assert(eval("""std.base64("hello")""").str == "aGVsbG8=")
        assert(eval("""std.base64Decode("aGVsbG8=")""").str == "hello")
      }
      test("helloWorld") {
        assert(eval("""std.base64("Hello, World!")""").str == "SGVsbG8sIFdvcmxkIQ==")
        assert(
          eval("""std.base64Decode("SGVsbG8sIFdvcmxkIQ==")""").str == "Hello, World!"
        )
      }
      test("binaryData") {
        assert(eval("""std.base64([0, 1, 2, 3])""").str == "AAECAw==")
        assert(
          eval("""std.base64DecodeBytes("AAECAw==")""").arr.toSeq
            .map(_.num.toInt) == Seq(0, 1, 2, 3)
        )
      }
      test("singleByte0") {
        assert(eval("""std.base64([0])""").str == "AA==")
        assert(
          eval("""std.base64DecodeBytes("AA==")""").arr.toSeq.map(_.num.toInt) == Seq(0)
        )
      }
      test("singleByte255") {
        assert(eval("""std.base64([255])""").str == "/w==")
        assert(
          eval("""std.base64DecodeBytes("/w==")""").arr.toSeq
            .map(_.num.toInt) == Seq(255)
        )
      }
      test("helloBytes") {
        assert(eval("""std.base64([104, 101, 108, 108, 111])""").str == "aGVsbG8=")
        assert(
          eval("""std.base64DecodeBytes("aGVsbG8=")""").arr.toSeq
            .map(_.num.toInt) == Seq(104, 101, 108, 108, 111)
        )
      }
    }

    // ================================================================
    // Progressive padding tests — "Base64" spelled out incrementally
    // Each prefix exercises a different padding scenario.
    // All tests verify BOTH encode AND decode directions.
    // ================================================================
    test("progressivePadding") {
      val pairs = Seq(
        ("B", "Qg=="),
        ("Ba", "QmE="),
        ("Bas", "QmFz"),
        ("Base", "QmFzZQ=="),
        ("Base6", "QmFzZTY="),
        ("Base64", "QmFzZTY0"),
        ("Base64 is ", "QmFzZTY0IGlzIA==")
      )
      for ((plain, b64) <- pairs) {
        val escaped = plain.replace("\\", "\\\\").replace("\"", "\\\"")
        val encResult = eval(s"""std.base64("$escaped")""")
        assert(encResult.str == b64)
        val decResult = eval(s"""std.base64Decode("$b64")""")
        assert(decResult.str == plain)
      }
    }

    // ================================================================
    // Long text bidirectional test
    // ================================================================
    test("longTextBidirectional") {
      val r = eval(
        """local longText =
          |  "Base64 is a group of similar binary-to-text encoding schemes that " +
          |  "represent binary data in an ASCII string format by translating it " +
          |  "into a radix-64 representation";
          |local expectedB64 =
          |  "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQg" +
          |  "cmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQg" +
          |  "aW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u";
          |std.assertEqual(std.base64(longText), expectedB64) &&
          |std.assertEqual(std.base64Decode(expectedB64), longText)
          |""".stripMargin
      )
      assert(r == ujson.True)
    }

    // ================================================================
    // DecodeBytes verification
    // ================================================================
    test("decodeBytes") {
      test("hello") {
        val r = eval("""std.base64DecodeBytes("aGVsbG8=")""")
        val arr = r.arr.toSeq.map(_.num.toInt)
        assert(arr == Seq(104, 101, 108, 108, 111))
      }
      test("empty") {
        val r = eval("""std.base64DecodeBytes("")""")
        assert(r.arr.toSeq.isEmpty)
      }
      test("singleByte") {
        val r = eval("""std.base64DecodeBytes("AA==")""")
        val arr = r.arr.toSeq.map(_.num.toInt)
        assert(arr == Seq(0))
      }
    }

    // ================================================================
    // Unicode string base64 (UTF-8 encoding)
    // ================================================================
    test("unicode") {
      test("chineseRoundtrip") {
        val r = eval(
          """local s = "你好世界";
            |std.base64Decode(std.base64(s)) == s
            |""".stripMargin
        )
        assert(r == ujson.True)
      }
      test("emojiRoundtrip") {
        val r = eval(
          """local s = "Hello 🌍!";
            |std.base64Decode(std.base64(s)) == s
            |""".stripMargin
        )
        assert(r == ujson.True)
      }
      test("mixedRoundtrip") {
        val r = eval(
          """local s = "café résumé naïve";
            |std.base64Decode(std.base64(s)) == s
            |""".stripMargin
        )
        assert(r == ujson.True)
      }
    }

    // ================================================================
    // SIMD boundary size tests (byte array variant)
    // Tests sizes that hit exact SIMD processing unit boundaries
    // ================================================================
    test("simdBoundaries") {
      val boundaries = Seq(
        11, 12, 13, // SSSE3 encode boundary (12 bytes → 16 chars)
        15, 16, 17, // SSSE3 decode boundary
        23, 24, 25, // AVX2 encode boundary (24 bytes → 32 chars)
        31, 32, 33, // AVX2 decode boundary
        47, 48, 49, // NEON/AVX-512 encode boundary
        63, 64, 65 // NEON/AVX-512 decode boundary
      )
      for (size <- boundaries) {
        val r = eval(
          s"""local arr = std.makeArray($size, function(i) (i * 7 + 13) % 256);
             |local encoded = std.base64(arr);
             |local decoded = std.base64DecodeBytes(encoded);
             |std.assertEqual(std.length(decoded), $size) &&
             |std.all(std.makeArray($size, function(i) decoded[i] == (i * 7 + 13) % 256))
             |""".stripMargin
        )
        assert(r == ujson.True)
      }
    }

    // ================================================================
    // Error handling
    // ================================================================
    test("errors") {
      test("invalidBase64Char") {
        val err = evalErr("""std.base64Decode("!!!!")""")
        assert(err.contains("Invalid base64"))
      }
      test("invalidByteArrayValue") {
        val err = evalErr("""std.base64([256])""")
        assert(err.contains("invalid codepoint"))
      }
      test("negativeByteValue") {
        val err = evalErr("""std.base64([-1])""")
        assert(err.contains("invalid codepoint"))
      }
      test("nonNumberInArray") {
        val err = evalErr("""std.base64(["a"])""")
        assert(err.contains("Expected an array of numbers"))
      }
      test("wrongTypeEncode") {
        val err = evalErr("""std.base64(true)""")
        assert(err.contains("Cannot base64 encode"))
      }
      test("wrongTypeDecode") {
        val err = evalErr("""std.base64Decode(123)""")
        assert(err.toLowerCase.contains("expected"))
      }
    }

    // ================================================================
    // Strict padding enforcement — aligns with go-jsonnet and C++ jsonnet
    //
    // Both official implementations reject unpadded base64 input:
    //   - go-jsonnet: len(str) % 4 != 0 check (UTF-8 byte count)
    //   - C++ jsonnet: std.length(str) % 4 != 0 check (char count)
    //
    // java.util.Base64 on JVM/JS is lenient and accepts unpadded input,
    // so PlatformBase64 adds explicit length validation.
    // ================================================================
    test("strictPadding") {
      // Unpadded variants that java.util.Base64 would accept but Jsonnet spec rejects
      test("missingTwoPads") {
        // "YQ" should be "YQ==" — both go-jsonnet and C++ jsonnet reject this
        val err = evalErr("""std.base64Decode("YQ")""")
        assert(err.contains("Invalid base64"))
      }
      test("missingOnePad") {
        // "YWI" should be "YWI=" — rejected by both official implementations
        val err = evalErr("""std.base64Decode("YWI")""")
        assert(err.contains("Invalid base64"))
      }
      test("singleChar") {
        // Single character is never valid base64
        val err = evalErr("""std.base64Decode("A")""")
        assert(err.contains("Invalid base64"))
      }
      test("fiveChars") {
        // "wrong" (5 chars) — the go_test_suite canonical test case
        val err = evalErr("""std.base64Decode("wrong")""")
        assert(err.contains("Invalid base64"))
      }
      test("validPaddedStillWorks") {
        // Properly padded input must still work
        val r = eval("""std.base64Decode("YQ==")""")
        assert(r.str == "a")
      }
      test("validPaddedOnePad") {
        val r = eval("""std.base64Decode("YWI=")""")
        assert(r.str == "ab")
      }
      test("validNoPadNeeded") {
        // Length is multiple of 4, no padding needed
        val r = eval("""std.base64Decode("YWJj")""")
        assert(r.str == "abc")
      }
      // DecodeBytes also enforces strict padding
      test("decodeBytesUnpadded") {
        val err = evalErr("""std.base64DecodeBytes("YQ")""")
        assert(err.contains("Invalid base64"))
      }
    }
  }
}
