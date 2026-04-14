// Byte array base64 encode/decode comprehensive tests.
// Ported from:
//   - aklomp/base64 test_char_table: all 256 byte values, sliding window
//   - Additional boundary byte value coverage vectors

// ================================================================
// All 256 byte values — encode and roundtrip
// (ported from aklomp/base64 test_char_table offset=0)
// ================================================================
local all256 = std.makeArray(256, function(i) i);
local encoded256 = std.base64(all256);
local decoded256 = std.base64DecodeBytes(encoded256);
std.assertEqual(std.length(decoded256), 256) &&
std.all(std.makeArray(256, function(i) decoded256[i] == i)) &&

// ================================================================
// Sliding window byte coverage
// (ported from aklomp/base64 test_char_table: loop from offset 0..255)
// Tests each starting offset to exercise all byte values at
// different alignment positions within SIMD processing units.
// ================================================================

// Offset 0: full 256 bytes [0,1,2,...,255]
local sw0 = std.makeArray(256, function(i) i % 256);
std.assertEqual(std.base64DecodeBytes(std.base64(sw0)), sw0) &&

// Offset 1: [1,2,...,255,0]
local sw1 = std.makeArray(256, function(i) (i + 1) % 256);
std.assertEqual(std.base64DecodeBytes(std.base64(sw1)), sw1) &&

// Offset 3: [3,4,...,255,0,1,2]
local sw3 = std.makeArray(256, function(i) (i + 3) % 256);
std.assertEqual(std.base64DecodeBytes(std.base64(sw3)), sw3) &&

// Offset 7 (non-power-of-2 alignment)
local sw7 = std.makeArray(256, function(i) (i + 7) % 256);
std.assertEqual(std.base64DecodeBytes(std.base64(sw7)), sw7) &&

// Offset 15 (SSSE3-1 boundary)
local sw15 = std.makeArray(256, function(i) (i + 15) % 256);
std.assertEqual(std.base64DecodeBytes(std.base64(sw15)), sw15) &&

// Offset 31 (AVX2-1 boundary)
local sw31 = std.makeArray(256, function(i) (i + 31) % 256);
std.assertEqual(std.base64DecodeBytes(std.base64(sw31)), sw31) &&

// Offset 63 (NEON/AVX512-1 boundary)
local sw63 = std.makeArray(256, function(i) (i + 63) % 256);
std.assertEqual(std.base64DecodeBytes(std.base64(sw63)), sw63) &&

// Offset 127 (half-way)
local sw127 = std.makeArray(256, function(i) (i + 127) % 256);
std.assertEqual(std.base64DecodeBytes(std.base64(sw127)), sw127) &&

// Offset 128
local sw128 = std.makeArray(256, function(i) (i + 128) % 256);
std.assertEqual(std.base64DecodeBytes(std.base64(sw128)), sw128) &&

// Offset 200
local sw200 = std.makeArray(256, function(i) (i + 200) % 256);
std.assertEqual(std.base64DecodeBytes(std.base64(sw200)), sw200) &&

// Offset 255
local sw255 = std.makeArray(256, function(i) (i + 255) % 256);
std.assertEqual(std.base64DecodeBytes(std.base64(sw255)), sw255) &&

// ================================================================
// Shortened arrays (variable length, ported from aklomp/base64)
// Tests that shorter-than-256 arrays at each offset also roundtrip
// ================================================================

// 1 byte at each interesting offset
std.assertEqual(std.base64DecodeBytes(std.base64([0])), [0]) &&
std.assertEqual(std.base64DecodeBytes(std.base64([1])), [1]) &&
std.assertEqual(std.base64DecodeBytes(std.base64([127])), [127]) &&
std.assertEqual(std.base64DecodeBytes(std.base64([128])), [128]) &&
std.assertEqual(std.base64DecodeBytes(std.base64([254])), [254]) &&
std.assertEqual(std.base64DecodeBytes(std.base64([255])), [255]) &&

// 2 bytes (boundary vectors)
std.assertEqual(std.base64DecodeBytes(std.base64([0, 0])), [0, 0]) &&
std.assertEqual(std.base64DecodeBytes(std.base64([0, 1])), [0, 1]) &&
std.assertEqual(std.base64DecodeBytes(std.base64([254, 255])), [254, 255]) &&
std.assertEqual(std.base64DecodeBytes(std.base64([255, 254])), [255, 254]) &&

// 3 bytes (no padding case)
std.assertEqual(std.base64DecodeBytes(std.base64([0, 0, 0])), [0, 0, 0]) &&
std.assertEqual(std.base64DecodeBytes(std.base64([255, 255, 255])), [255, 255, 255]) &&

// 5 bytes (multi-byte boundary vector)
std.assertEqual(std.base64DecodeBytes(std.base64([0, 1, 128, 254, 255])),
  [0, 1, 128, 254, 255]) &&

// ================================================================
// Large byte array roundtrip (1000 elements)
// ================================================================
local large1000 = std.makeArray(1000, function(i) i % 256);
local encodedLarge = std.base64(large1000);
local decodedLarge = std.base64DecodeBytes(encodedLarge);
std.assertEqual(std.length(decodedLarge), 1000) &&
std.all(std.makeArray(1000, function(i) decodedLarge[i] == i % 256)) &&

// ================================================================
// Large byte array roundtrip (4096 elements)
// ================================================================
local large4096 = std.makeArray(4096, function(i) (i * 3 + 17) % 256);
local encoded4096 = std.base64(large4096);
local decoded4096 = std.base64DecodeBytes(encoded4096);
std.assertEqual(std.length(decoded4096), 4096) &&
std.all(std.makeArray(4096, function(i) decoded4096[i] == (i * 3 + 17) % 256)) &&

true
