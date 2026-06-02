// Issue #797: std.slice and the [a:b:c] slice operator must reject fractional
// indices and steps (strict integer semantics) instead of silently truncating.
// Integer arguments (including negatives and nulls) keep working unchanged.
std.assertEqual(std.slice([1, 2, 3, 4, 5, 6], 0, 4, 1), [1, 2, 3, 4]) &&
std.assertEqual(std.slice([1, 2, 3, 4, 5, 6], 1, 6, 2), [2, 4, 6]) &&
std.assertEqual(std.slice([1, 2, 3, 4, 5, 6], -3, 99, 1), [4, 5, 6]) &&
std.assertEqual(std.slice("jsonnet", 0, 4, 1), "json") &&
std.assertEqual(std.slice([1, 2, 3], null, null, null), [1, 2, 3]) &&
std.assertEqual([1, 2, 3, 4, 5][1:4:2], [2, 4]) &&
std.assertEqual([1, 2, 3, 4, 5][-2:], [4, 5]) &&
// Fractional values that happen to be whole (e.g. 2.0) are still accepted.
std.assertEqual(std.slice([1, 2, 3], 0.0, 2.0, 1.0), [1, 2]) &&
true
