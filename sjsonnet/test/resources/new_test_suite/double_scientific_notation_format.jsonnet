// Verify double rendering uses a stable, shortest-round-trip decimal policy:
// common JSON-range values stay in fixed decimal form; smaller values use
// normalized scientific notation with lowercase 'e', explicit sign, and minimum
// 2 exponent digits.
// Digit count differences (Java Ryu vs Go %g) are documented in sync_ignore.

local fixedThreshold = "x=" + 0.000001;  // Java: 1.0E-6, JS: 0.000001
local fixedToString = std.toString(0.000001);
local fixedJsonEx = std.manifestJsonEx({a: 0.000001}, "  ");
local fracSci = "x=" + 0.0000001;  // Java: 1.0E-7, JS: 1e-7
local expected1e100 = "1" + std.join("", std.makeArray(100, function(_) "0"));

std.assertEqual(fixedThreshold, "x=0.000001") &&
std.assertEqual(fixedToString, "0.000001") &&
std.assertEqual(fixedJsonEx, '{\n  "a": 0.000001\n}') &&
std.assertEqual("x=" + -0.000001, "x=-0.000001") &&
std.assertEqual("x=" + 0.00000123, "x=0.00000123") &&
std.assertEqual(fracSci, "x=1e-07") &&
std.assertEqual(std.toString(0.0000001), "1e-07") &&
std.assertEqual(std.manifestJsonEx({a: 0.0000001}, "  "), '{\n  "a": 1e-07\n}') &&
std.assertEqual("x=" + -0.0000001, "x=-1e-07") &&
// Large whole numbers still use integer form (matching go-jsonnet)
std.assertEqual("x=" + 1e15, "x=1000000000000000") &&
std.assertEqual("x=" + 1e20, "x=100000000000000000000") &&
std.assertEqual(std.manifestJson(1e100), expected1e100) &&
std.assertEqual(std.manifestToml({a: 1e100}), "a = " + expected1e100) &&
// Non-scientific numbers unchanged
std.assertEqual("x=" + 0.1, "x=0.1") &&
std.assertEqual("x=" + 1.5, "x=1.5") &&
true
