// Comprehensive base64 tests ported from:
//   - RFC 4648 Section 10 (all 7 test vectors)
//   - aklomp/base64 test suite (char table, Moby Dick roundtrip)
//   - Additional boundary and byte-value coverage vectors
//
// Tests encode (std.base64), decode (std.base64Decode), and
// byte-level decode (std.base64DecodeBytes) for correctness.

// ================================================================
// RFC 4648 Section 10 — All official test vectors (encode)
// ================================================================
std.assertEqual(std.base64(""), "") &&
std.assertEqual(std.base64("f"), "Zg==") &&
std.assertEqual(std.base64("fo"), "Zm8=") &&
std.assertEqual(std.base64("foo"), "Zm9v") &&
std.assertEqual(std.base64("foob"), "Zm9vYg==") &&
std.assertEqual(std.base64("fooba"), "Zm9vYmE=") &&
std.assertEqual(std.base64("foobar"), "Zm9vYmFy") &&

// ================================================================
// RFC 4648 Section 10 — All official test vectors (decode)
// ================================================================
std.assertEqual(std.base64Decode(""), "") &&
std.assertEqual(std.base64Decode("Zg=="), "f") &&
std.assertEqual(std.base64Decode("Zm8="), "fo") &&
std.assertEqual(std.base64Decode("Zm9v"), "foo") &&
std.assertEqual(std.base64Decode("Zm9vYg=="), "foob") &&
std.assertEqual(std.base64Decode("Zm9vYmE="), "fooba") &&
std.assertEqual(std.base64Decode("Zm9vYmFy"), "foobar") &&

// ================================================================
// Padding variants
// ================================================================
// Input length mod 3 == 0 → no padding
std.assertEqual(std.base64("abc"), "YWJj") &&
// Input length mod 3 == 2 → one '='
std.assertEqual(std.base64("ab"), "YWI=") &&
// Input length mod 3 == 1 → two '='
std.assertEqual(std.base64("a"), "YQ==") &&

// ================================================================
// Boundary byte value vectors (byte array encode)
// ================================================================
std.assertEqual(std.base64([0]), "AA==") &&
std.assertEqual(std.base64([1]), "AQ==") &&
std.assertEqual(std.base64([128]), "gA==") &&
std.assertEqual(std.base64([255]), "/w==") &&
std.assertEqual(std.base64([0, 1]), "AAE=") &&
std.assertEqual(std.base64([254, 255]), "/v8=") &&
std.assertEqual(std.base64([0, 1, 128, 254, 255]), "AAGA/v8=") &&

// ================================================================
// Boundary byte value vectors (byte array decode)
// ================================================================
std.assertEqual(std.base64DecodeBytes("AA=="), [0]) &&
std.assertEqual(std.base64DecodeBytes("AQ=="), [1]) &&
std.assertEqual(std.base64DecodeBytes("gA=="), [128]) &&
std.assertEqual(std.base64DecodeBytes("/w=="), [255]) &&
std.assertEqual(std.base64DecodeBytes("AAE="), [0, 1]) &&
std.assertEqual(std.base64DecodeBytes("/v8="), [254, 255]) &&
std.assertEqual(std.base64DecodeBytes("AAGA/v8="), [0, 1, 128, 254, 255]) &&

// ================================================================
// Known string encode/decode pairs
// ================================================================
std.assertEqual(std.base64("hello"), "aGVsbG8=") &&
std.assertEqual(std.base64("Hello, World!"), "SGVsbG8sIFdvcmxkIQ==") &&
std.assertEqual(std.base64("The quick brown fox jumps over the lazy dog"),
  "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==") &&
std.assertEqual(std.base64Decode("aGVsbG8="), "hello") &&
std.assertEqual(std.base64Decode("SGVsbG8sIFdvcmxkIQ=="), "Hello, World!") &&

// ================================================================
// Known byte array encode/decode pairs
// ================================================================
std.assertEqual(std.base64([0, 1, 2, 3]), "AAECAw==") &&
std.assertEqual(std.base64([104, 101, 108, 108, 111]), "aGVsbG8=") &&
std.assertEqual(std.base64DecodeBytes("AAECAw=="), [0, 1, 2, 3]) &&
std.assertEqual(std.base64DecodeBytes("aGVsbG8="), [104, 101, 108, 108, 111]) &&

// ================================================================
// Base64 alphabet completeness — every char in A-Za-z0-9+/ appears
// ================================================================
local fullAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
local decoded = std.base64DecodeBytes(fullAlphabet);
local reEncoded = std.base64(decoded);
std.assertEqual(reEncoded, fullAlphabet) &&

// ================================================================
// Progressive padding tests — "Base64" spelled out incrementally
// Each prefix exercises a different padding scenario
// ================================================================
std.assertEqual(std.base64("B"), "Qg==") &&
std.assertEqual(std.base64Decode("Qg=="), "B") &&
std.assertEqual(std.base64("Ba"), "QmE=") &&
std.assertEqual(std.base64Decode("QmE="), "Ba") &&
std.assertEqual(std.base64("Bas"), "QmFz") &&
std.assertEqual(std.base64Decode("QmFz"), "Bas") &&
std.assertEqual(std.base64("Base"), "QmFzZQ==") &&
std.assertEqual(std.base64Decode("QmFzZQ=="), "Base") &&
std.assertEqual(std.base64("Base6"), "QmFzZTY=") &&
std.assertEqual(std.base64Decode("QmFzZTY="), "Base6") &&
std.assertEqual(std.base64("Base64"), "QmFzZTY0") &&
std.assertEqual(std.base64Decode("QmFzZTY0"), "Base64") &&

// Long text bidirectional (full "Base64 is..." paragraph)
local longText =
  "Base64 is a group of similar binary-to-text encoding schemes that " +
  "represent binary data in an ASCII string format by translating it " +
  "into a radix-64 representation";
local longTextB64 =
  "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQg" +
  "cmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQg" +
  "aW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u";
std.assertEqual(std.base64(longText), longTextB64) &&
std.assertEqual(std.base64Decode(longTextB64), longText) &&

// Progressive substrings of longText — bidirectional
std.assertEqual(std.base64("Base64 is "), "QmFzZTY0IGlzIA==") &&
std.assertEqual(std.base64Decode("QmFzZTY0IGlzIA=="), "Base64 is ") &&

// ================================================================
// Multi-cycle roundtrip stability
// ================================================================
local original = "The quick brown fox jumps over the lazy dog";
local e1 = std.base64(original);
local d1 = std.base64Decode(e1);
local e2 = std.base64(d1);
local d2 = std.base64Decode(e2);
local e3 = std.base64(d2);
local d3 = std.base64Decode(e3);
std.assertEqual(d3, original) &&
std.assertEqual(e1, e2) &&
std.assertEqual(e2, e3) &&

// ================================================================
// Unicode string roundtrip (UTF-8)
// ================================================================
std.assertEqual(std.base64Decode(std.base64("café résumé naïve")), "café résumé naïve") &&
std.assertEqual(std.base64Decode(std.base64("你好世界")), "你好世界") &&
std.assertEqual(std.base64Decode(std.base64("日本語テスト")), "日本語テスト") &&

// ================================================================
// String roundtrip for sizes 0..64 (covers SIMD boundaries)
// ================================================================
local mkStr(len) = std.join("", std.makeArray(len, function(i) std.char(65 + (i % 26))));
std.all(std.makeArray(65, function(n)
  std.base64Decode(std.base64(mkStr(n))) == mkStr(n)
)) &&

// ================================================================
// Moby Dick roundtrip (ported from aklomp/base64 test suite)
// ================================================================
local mobyDickPlain =
  "Call me Ishmael. Some years ago--never mind how long precisely--having\n" +
  "little or no money in my purse, and nothing particular to interest me on\n" +
  "shore, I thought I would sail about a little and see the watery part of\n" +
  "the world. It is a way I have of driving off the spleen and regulating\n" +
  "the circulation. Whenever I find myself growing grim about the mouth;\n" +
  "whenever it is a damp, drizzly November in my soul; whenever I find\n" +
  "myself involuntarily pausing before coffin warehouses, and bringing up\n" +
  "the rear of every funeral I meet; and especially whenever my hypos get\n" +
  "such an upper hand of me, that it requires a strong moral principle to\n" +
  "prevent me from deliberately stepping into the street, and methodically\n" +
  "knocking people's hats off--then, I account it high time to get to sea\n" +
  "as soon as I can. This is my substitute for pistol and ball. With a\n" +
  "philosophical flourish Cato throws himself upon his sword; I quietly\n" +
  "take to the ship. There is nothing surprising in this. If they but knew\n" +
  "it, almost all men in their degree, some time or other, cherish very\n" +
  "nearly the same feelings towards the ocean with me.\n";
local mobyDickBase64 =
  "Q2FsbCBtZSBJc2htYWVsLiBTb21lIHllYXJzIGFnby0tbmV2ZXIgbWluZCBob3cgbG9uZ" +
  "yBwcmVjaXNlbHktLWhhdmluZwpsaXR0bGUgb3Igbm8gbW9uZXkgaW4gbXkgcHVyc2UsIG" +
  "FuZCBub3RoaW5nIHBhcnRpY3VsYXIgdG8gaW50ZXJlc3QgbWUgb24Kc2hvcmUsIEkgdGh" +
  "vdWdodCBJIHdvdWxkIHNhaWwgYWJvdXQgYSBsaXR0bGUgYW5kIHNlZSB0aGUgd2F0ZXJ5" +
  "IHBhcnQgb2YKdGhlIHdvcmxkLiBJdCBpcyBhIHdheSBJIGhhdmUgb2YgZHJpdmluZyBvZ" +
  "mYgdGhlIHNwbGVlbiBhbmQgcmVndWxhdGluZwp0aGUgY2lyY3VsYXRpb24uIFdoZW5ldm" +
  "VyIEkgZmluZCBteXNlbGYgZ3Jvd2luZyBncmltIGFib3V0IHRoZSBtb3V0aDsKd2hlbmV" +
  "2ZXIgaXQgaXMgYSBkYW1wLCBkcml6emx5IE5vdmVtYmVyIGluIG15IHNvdWw7IHdoZW5l" +
  "dmVyIEkgZmluZApteXNlbGYgaW52b2x1bnRhcmlseSBwYXVzaW5nIGJlZm9yZSBjb2Zma" +
  "W4gd2FyZWhvdXNlcywgYW5kIGJyaW5naW5nIHVwCnRoZSByZWFyIG9mIGV2ZXJ5IGZ1bm" +
  "VyYWwgSSBtZWV0OyBhbmQgZXNwZWNpYWxseSB3aGVuZXZlciBteSBoeXBvcyBnZXQKc3V" +
  "jaCBhbiB1cHBlciBoYW5kIG9mIG1lLCB0aGF0IGl0IHJlcXVpcmVzIGEgc3Ryb25nIG1v" +
  "cmFsIHByaW5jaXBsZSB0bwpwcmV2ZW50IG1lIGZyb20gZGVsaWJlcmF0ZWx5IHN0ZXBwa" +
  "W5nIGludG8gdGhlIHN0cmVldCwgYW5kIG1ldGhvZGljYWxseQprbm9ja2luZyBwZW9wbG" +
  "UncyBoYXRzIG9mZi0tdGhlbiwgSSBhY2NvdW50IGl0IGhpZ2ggdGltZSB0byBnZXQgdG8" +
  "gc2VhCmFzIHNvb24gYXMgSSBjYW4uIFRoaXMgaXMgbXkgc3Vic3RpdHV0ZSBmb3IgcGlz" +
  "dG9sIGFuZCBiYWxsLiBXaXRoIGEKcGhpbG9zb3BoaWNhbCBmbG91cmlzaCBDYXRvIHRoc" +
  "m93cyBoaW1zZWxmIHVwb24gaGlzIHN3b3JkOyBJIHF1aWV0bHkKdGFrZSB0byB0aGUgc2" +
  "hpcC4gVGhlcmUgaXMgbm90aGluZyBzdXJwcmlzaW5nIGluIHRoaXMuIElmIHRoZXkgYnV" +
  "0IGtuZXcKaXQsIGFsbW9zdCBhbGwgbWVuIGluIHRoZWlyIGRlZ3JlZSwgc29tZSB0aW1l" +
  "IG9yIG90aGVyLCBjaGVyaXNoIHZlcnkKbmVhcmx5IHRoZSBzYW1lIGZlZWxpbmdzIHRvd" +
  "2FyZHMgdGhlIG9jZWFuIHdpdGggbWUuCg==";
std.assertEqual(std.base64(mobyDickPlain), mobyDickBase64) &&
std.assertEqual(std.base64Decode(mobyDickBase64), mobyDickPlain) &&

true
