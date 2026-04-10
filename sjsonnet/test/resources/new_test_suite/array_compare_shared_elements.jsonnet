// Regression test for array comparison with shared elements.
// When arrays share element references (e.g., from concatenation of a common
// base), the comparison fast-path must still produce correct results.

local base = std.range(1, 100);
local a = base + [1];
local b = base + [2];
local c = base + [1];

std.assertEqual(a < b, true) &&
std.assertEqual(b < a, false) &&
std.assertEqual(a == c, true) &&
std.assertEqual(a == b, false) &&
std.assertEqual(a <= b, true) &&
std.assertEqual(a >= b, false) &&
// Equality with identical references
std.assertEqual(a == a, true) &&
std.assertEqual(base == base, true) &&
// Comparison with identical references
std.assertEqual(a < a, false) &&
std.assertEqual(a <= a, true) &&
std.assertEqual(a >= a, true) &&
true
