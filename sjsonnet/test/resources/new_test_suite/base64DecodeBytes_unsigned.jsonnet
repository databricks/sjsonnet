// Regression test: base64DecodeBytes must return unsigned byte values (0-255)
// Bytes >= 128 must NOT be sign-extended to negative numbers.
local encoded = std.base64([255, 128, 1, 0]);
local decoded = std.base64DecodeBytes(encoded);
std.assertEqual(decoded, [255, 128, 1, 0])
