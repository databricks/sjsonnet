// Regression test: bulk numeric array parser must produce identical results
// to Double.parseDouble for large integers (beyond 2^53 precision).
local arr = [12345678901234567890, -12345678901234567890, 0, -0, 9007199254740993, 1];
std.assertEqual(arr[0], 12345678901234567890) &&
std.assertEqual(arr[1], -12345678901234567890) &&
std.assertEqual(arr[2], 0) &&
std.assertEqual(arr[3], 0) &&
std.assertEqual(arr[4], 9007199254740993) &&
std.assertEqual(arr[5], 1) &&
true
