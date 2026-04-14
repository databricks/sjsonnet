// Bidirectional base64 tests — every test vector verifies BOTH directions:
//   encode(input) == expected_base64  AND  decode(expected_base64) == input
//
// This catches bugs where encode and decode are "consistently wrong"
// (i.e., roundtrip passes but both directions produce incorrect output).
// This was the class of bug in PR #749 where hand-written SIMD code
// produced wrong results on x86 that still roundtripped.
//
// All reference values pre-computed via java.util.Base64 reference impl.

// ================================================================
// Helpers: assert both encode and decode match the reference
// ================================================================
local assertBidi(plain, b64) =
  std.assertEqual(std.base64(plain), b64) &&
  std.assertEqual(std.base64Decode(b64), plain);

local assertBidiBytes(arr, b64) =
  std.assertEqual(std.base64(arr), b64) &&
  std.assertEqual(std.base64DecodeBytes(b64), arr);

// ================================================================
// RFC 4648 Section 10 — All 7 official test vectors (bidirectional)
// ================================================================
assertBidi("", "") &&
assertBidi("f", "Zg==") &&
assertBidi("fo", "Zm8=") &&
assertBidi("foo", "Zm9v") &&
assertBidi("foob", "Zm9vYg==") &&
assertBidi("fooba", "Zm9vYmE=") &&
assertBidi("foobar", "Zm9vYmFy") &&

// ================================================================
// Wikipedia "pleasure" progression (bidirectional)
// ================================================================
assertBidi("pleasure.", "cGxlYXN1cmUu") &&
assertBidi("leasure.", "bGVhc3VyZS4=") &&
assertBidi("easure.", "ZWFzdXJlLg==") &&
assertBidi("asure.", "YXN1cmUu") &&
assertBidi("sure.", "c3VyZS4=") &&
assertBidi("ure.", "dXJlLg==") &&
assertBidi("re.", "cmUu") &&
assertBidi("e.", "ZS4=") &&
assertBidi(".", "Lg==") &&

// ================================================================
// Single/multi char strings (bidirectional)
// ================================================================
assertBidi("A", "QQ==") &&
assertBidi("AB", "QUI=") &&
assertBidi("ABC", "QUJD") &&
assertBidi("ABCD", "QUJDRA==") &&
assertBidi("Man", "TWFu") &&
assertBidi("Ma", "TWE=") &&
assertBidi("M", "TQ==") &&
assertBidi("hello", "aGVsbG8=") &&
assertBidi("Hello, World!", "SGVsbG8sIFdvcmxkIQ==") &&
assertBidi("The quick brown fox jumps over the lazy dog",
  "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==") &&

// ================================================================
// Single byte values — boundary values (bidirectional)
// ================================================================
assertBidiBytes([0], "AA==") &&
assertBidiBytes([1], "AQ==") &&
assertBidiBytes([63], "Pw==") &&
assertBidiBytes([64], "QA==") &&
assertBidiBytes([127], "fw==") &&
assertBidiBytes([128], "gA==") &&
assertBidiBytes([191], "vw==") &&
assertBidiBytes([192], "wA==") &&
assertBidiBytes([254], "/g==") &&
assertBidiBytes([255], "/w==") &&

// ================================================================
// Two bytes — padding=1 cases (bidirectional)
// ================================================================
assertBidiBytes([0, 0], "AAA=") &&
assertBidiBytes([0, 1], "AAE=") &&
assertBidiBytes([0, 255], "AP8=") &&
assertBidiBytes([255, 0], "/wA=") &&
assertBidiBytes([255, 255], "//8=") &&
assertBidiBytes([128, 128], "gIA=") &&

// ================================================================
// Three bytes — no padding (bidirectional)
// ================================================================
assertBidiBytes([0, 0, 0], "AAAA") &&
assertBidiBytes([0, 0, 1], "AAAB") &&
assertBidiBytes([0, 0, 63], "AAA/") &&
assertBidiBytes([255, 255, 255], "////") &&
assertBidiBytes([0, 16, 131], "ABCD") &&
assertBidiBytes([77, 97, 110], "TWFu") &&

// ================================================================
// Four, five, six bytes (bidirectional)
// ================================================================
assertBidiBytes([0, 0, 0, 0], "AAAAAA==") &&
assertBidiBytes([255, 255, 255, 255], "/////w==") &&
assertBidiBytes([0, 0, 0, 0, 0], "AAAAAAA=") &&
assertBidiBytes([0, 0, 0, 0, 0, 0], "AAAAAAAA") &&
assertBidiBytes([255, 255, 255, 255, 255, 255], "////////") &&

// ================================================================
// Sequential byte patterns (bidirectional)
// ================================================================
assertBidiBytes([0, 1, 2], "AAEC") &&
assertBidiBytes([0, 1, 2, 3], "AAECAw==") &&
assertBidiBytes([0, 1, 2, 3, 4], "AAECAwQ=") &&
assertBidiBytes([0, 1, 2, 3, 4, 5], "AAECAwQF") &&

// Powers of 2 boundary values
assertBidiBytes([1, 2, 4, 8, 16, 32, 64, 128], "AQIECBAgQIA=") &&

// Vectors that exercise '+' and '/' in output
assertBidiBytes([251, 239, 190], "++++") &&
assertBidiBytes([251, 239], "++8=") &&
assertBidiBytes([104, 101, 108, 108, 111], "aGVsbG8=") &&
assertBidiBytes([0, 1, 128, 254, 255], "AAGA/v8=") &&

// ================================================================
// SIMD boundary sizes — exact pre-computed values (bidirectional)
// ================================================================

// 12 bytes — SSSE3 encode unit boundary
assertBidiBytes(std.makeArray(12, function(i) i),
  "AAECAwQFBgcICQoL") &&

// 24 bytes — AVX2 encode unit boundary
assertBidiBytes(std.makeArray(24, function(i) i),
  "AAECAwQFBgcICQoLDA0ODxAREhMUFRYX") &&

// 48 bytes — NEON / AVX-512 encode unit boundary
assertBidiBytes(std.makeArray(48, function(i) i),
  "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4v") &&

// ================================================================
// Verify standard base64 alphabet (not URL-safe)
// '+' and '/' must appear, not '-' and '_'
// ================================================================
local testPlus = std.base64([251, 239]);
local testSlash = std.base64([255]);
std.assertEqual(testPlus, "++8=") &&
std.assertEqual(testSlash, "/w==") &&
std.assertEqual(std.length(std.findSubstr("+", testPlus)) > 0, true) &&
std.assertEqual(std.length(std.findSubstr("/", testSlash)) > 0, true) &&
std.assertEqual(std.length(std.findSubstr("-", testPlus)), 0) &&
std.assertEqual(std.length(std.findSubstr("_", testSlash)), 0) &&

true
