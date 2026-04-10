// Regression test: base64DecodeBytes must return unsigned byte values (0-255).
// Java's byte type is signed (-128..127), so without masking with 0xff,
// bytes >= 128 would appear as negative numbers.
local bytes = std.base64DecodeBytes("gIA=");  // decodes to [0x80, 0x80] = [128, 128]

std.assertEqual(bytes[0], 128) &&
std.assertEqual(bytes[1], 128) &&

// Test full range: 0xFF = 255
local highByte = std.base64DecodeBytes("/w==");  // decodes to [0xFF] = [255]
std.assertEqual(highByte[0], 255) &&

// Test zero byte
local zeroByte = std.base64DecodeBytes("AA==");  // decodes to [0x00] = [0]
std.assertEqual(zeroByte[0], 0) &&

// Round-trip test: encode then decode should preserve all byte values including high bytes
local mixed = [0, 127, 128, 200, 255];
std.assertEqual(std.base64DecodeBytes(std.base64(mixed)), mixed) &&

true
