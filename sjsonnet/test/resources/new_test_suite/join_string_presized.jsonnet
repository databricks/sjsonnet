// Directional coverage for std.join string paths:
//  - small array fallback (inline StringBuilder, len < 16)
//  - direct backing array path (joinDirectStringArray)
//  - presized path (len >= 16, joinPresizedStringArray)
//  - asciiSafe propagation (separator and parts both ASCII)
//  - non-ASCII parts that should still join correctly
//  - null skipping at all positions
//  - all-null returns empty string

local small = std.join("-", ["a", "bb", null, "ccc"]);
local nonAscii = std.join("/", ["é", "λ", null, "🚀"]);
local allNull = std.join("ignored", [null, null]);

// 20 ASCII parts to force the presized path on a non-direct array.
local many = std.join(
  ", ",
  std.makeArray(20, function(i) std.toString(i)),
);

// 18 ASCII parts on a direct (literal) array to exercise joinDirectStringArray.
local direct18 = std.join(
  "|",
  ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
   "k", "l", "m", "n", "o", "p", "q", "r"],
);

// Mixed null + ASCII to exercise size pre-walk skipping.
local mixed = std.join(
  ":",
  std.makeArray(20, function(i) if i % 3 == 0 then null else std.toString(i)),
);

std.assertEqual(small, "a-bb-ccc") &&
std.assertEqual(nonAscii, "é/λ/🚀") &&
std.assertEqual(allNull, "") &&
std.assertEqual(many, "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19") &&
std.assertEqual(direct18, "a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r") &&
std.assertEqual(mixed, "1:2:4:5:7:8:10:11:13:14:16:17:19") &&
true
