// Regression tests for ByteArr subclass correctness.
// Covers multi-use, reverse, concat, and round-trip scenarios to ensure
// byte-backed arrays behave identically to normal arrays.
local decoded = std.base64DecodeBytes("AQIDBAUG");  // [1,2,3,4,5,6]
local empty = std.base64DecodeBytes("");             // []

// 1. Multiple consumption of the same ByteArr
local enc1 = std.base64(decoded);
local enc2 = std.base64(decoded);

// 2. Reverse of ByteArr
local rev = std.reverse(decoded);

// 3. Double-reverse of ByteArr (should equal original)
local rev2 = std.reverse(std.reverse(decoded));

// 4. Concat then re-encode
local extended = decoded + [7, 8];
local extEnc = std.base64(extended);

// 5. ByteArr after concat (materialization path) — original still usable
local afterConcat = decoded + [99];
local encAfterConcat = std.base64(decoded);

// 6. Element access on ByteArr
local elem0 = decoded[0];
local elem5 = decoded[5];

// 7. Length
local len = std.length(decoded);

// 8. Slice
local sliced = decoded[1:4];

// 9. std.map over ByteArr
local mapped = std.map(function(x) x * 2, decoded);

// 10. Empty ByteArr
local emptyEnc = std.base64(empty);
local emptyRev = std.reverse(empty);
local emptyLen = std.length(empty);

// 11. Round-trip: decode -> encode -> decode -> encode
local rt = std.base64(std.base64DecodeBytes(std.base64(decoded)));

// 12. Reverse then encode (reversed bytes should encode differently)
local revEnc = std.base64(std.reverse(std.base64DecodeBytes("AQID")));

// Assertions
std.assertEqual(enc1, "AQIDBAUG") &&
std.assertEqual(enc2, "AQIDBAUG") &&
std.assertEqual(rev, [6, 5, 4, 3, 2, 1]) &&
std.assertEqual(rev2, [1, 2, 3, 4, 5, 6]) &&
std.assertEqual(std.base64(rev), std.base64([6, 5, 4, 3, 2, 1])) &&
std.assertEqual(extEnc, std.base64([1, 2, 3, 4, 5, 6, 7, 8])) &&
std.assertEqual(encAfterConcat, "AQIDBAUG") &&
std.assertEqual(elem0, 1) &&
std.assertEqual(elem5, 6) &&
std.assertEqual(len, 6) &&
std.assertEqual(sliced, [2, 3, 4]) &&
std.assertEqual(mapped, [2, 4, 6, 8, 10, 12]) &&
std.assertEqual(emptyEnc, "") &&
std.assertEqual(emptyRev, []) &&
std.assertEqual(emptyLen, 0) &&
std.assertEqual(rt, "AQIDBAUG") &&
std.assertEqual(revEnc, "AwIB") &&
true
