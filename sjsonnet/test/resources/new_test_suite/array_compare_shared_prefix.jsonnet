// Test array comparison when arrays share a common prefix via concatenation.
// Exercises the Eval reference equality optimization in compare() and equal().
local base = std.range(1, 100);
std.assertEqual(base + [0] < base + [1], true) &&
std.assertEqual(base + [2] > base + [1], true) &&
std.assertEqual(base + [1] <= base + [1], true) &&
std.assertEqual(base + [1] >= base + [1], true) &&
std.assertEqual(base + [1] == base + [1], true) &&
std.assertEqual(base + [1] != base + [2], true) &&
// Edge case: different-length shared prefix
std.assertEqual(base < base + [1], true) &&
std.assertEqual(base + [1] > base, true) &&
// Nested array comparison with shared elements
local nested = [base, base + [1]];
std.assertEqual(nested == [base, base + [1]], true) &&
std.assertEqual(nested != [base, base + [2]], true) &&
true
