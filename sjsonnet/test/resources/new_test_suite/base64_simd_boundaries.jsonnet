// SIMD boundary size tests for base64 encode/decode.
// Ported from aklomp/base64 test suite (char table tests at various alignments)
// and additional deterministic byte patterns at SIMD unit sizes.
//
// Tests sizes that hit exact SIMD processing unit boundaries:
//   SSSE3 encode: 12 bytes (→ 16 chars)
//   SSSE3 decode: 16 chars (→ 12 bytes)
//   AVX2 encode:  24 bytes (→ 32 chars)
//   AVX2 decode:  32 chars (→ 24 bytes)
//   NEON/AVX-512 encode: 48 bytes (→ 64 chars)
//   NEON/AVX-512 decode: 64 chars (→ 48 bytes)

// Helper: create a deterministic byte array of given length
local mkArr(len) = std.makeArray(len, function(i) (i * 7 + 13) % 256);

// Helper: verify byte array roundtrip
local verifyRoundtrip(size) =
  local arr = mkArr(size);
  local encoded = std.base64(arr);
  local decoded = std.base64DecodeBytes(encoded);
  std.assertEqual(std.length(decoded), size) &&
  std.all(std.makeArray(size, function(i) decoded[i] == arr[i]));

// ================================================================
// SSSE3 boundaries (12-byte encode unit, 16-byte decode unit)
// ================================================================
verifyRoundtrip(11) &&
verifyRoundtrip(12) &&
verifyRoundtrip(13) &&
verifyRoundtrip(15) &&
verifyRoundtrip(16) &&
verifyRoundtrip(17) &&

// ================================================================
// AVX2 boundaries (24-byte encode unit, 32-byte decode unit)
// ================================================================
verifyRoundtrip(23) &&
verifyRoundtrip(24) &&
verifyRoundtrip(25) &&
verifyRoundtrip(31) &&
verifyRoundtrip(32) &&
verifyRoundtrip(33) &&

// ================================================================
// NEON / AVX-512 boundaries (48/64 bytes)
// ================================================================
verifyRoundtrip(47) &&
verifyRoundtrip(48) &&
verifyRoundtrip(49) &&
verifyRoundtrip(63) &&
verifyRoundtrip(64) &&
verifyRoundtrip(65) &&

// ================================================================
// Multi-block sizes (multiple SIMD iterations)
// ================================================================
verifyRoundtrip(96) &&   // 2x NEON encode
verifyRoundtrip(128) &&  // 2x NEON decode
verifyRoundtrip(192) &&  // 4x NEON encode
verifyRoundtrip(255) &&  // Max byte value as size
verifyRoundtrip(256) &&  // All byte values

// ================================================================
// Special byte patterns at SIMD boundaries
// ================================================================

// All-zeros at SIMD boundary
local zeros48 = std.makeArray(48, function(i) 0);
local zeros48Encoded = std.base64(zeros48);
local zeros48Decoded = std.base64DecodeBytes(zeros48Encoded);
std.assertEqual(std.length(zeros48Decoded), 48) &&
std.all(std.makeArray(48, function(i) zeros48Decoded[i] == 0)) &&

// All-0xFF at SIMD boundary
local ff64 = std.makeArray(64, function(i) 255);
local ff64Encoded = std.base64(ff64);
local ff64Decoded = std.base64DecodeBytes(ff64Encoded);
std.assertEqual(std.length(ff64Decoded), 64) &&
std.all(std.makeArray(64, function(i) ff64Decoded[i] == 255)) &&

// Alternating 0x00/0xFF at AVX2 boundary
local alt32 = std.makeArray(32, function(i) if i % 2 == 0 then 0 else 255);
local alt32Encoded = std.base64(alt32);
local alt32Decoded = std.base64DecodeBytes(alt32Encoded);
std.assertEqual(std.length(alt32Decoded), 32) &&
std.all(std.makeArray(32, function(i)
  alt32Decoded[i] == (if i % 2 == 0 then 0 else 255)
)) &&

// Sequential 0..47 at NEON boundary
local seq48 = std.makeArray(48, function(i) i);
local seq48Encoded = std.base64(seq48);
local seq48Decoded = std.base64DecodeBytes(seq48Encoded);
std.assertEqual(std.length(seq48Decoded), 48) &&
std.all(std.makeArray(48, function(i) seq48Decoded[i] == i)) &&

true
