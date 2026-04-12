// Test that lazy range arrays produce correct results for all operations.
local range10 = std.range(1, 10);
local range5 = std.range(0, 4);
local emptyRange = std.range(5, 3);  // empty when from > to

std.assertEqual(std.length(range10), 10) &&
std.assertEqual(range10[0], 1) &&
std.assertEqual(range10[9], 10) &&
std.assertEqual(range10[4], 5) &&
// Iteration via array comprehension
std.assertEqual([x * 2 for x in range5], [0, 2, 4, 6, 8]) &&
// Concat with comparison (the key pattern for sharedConcatPrefixLength)
std.assertEqual(range10 + [11] < range10 + [12], true) &&
std.assertEqual(range10 + [11] > range10 + [12], false) &&
std.assertEqual(range10 + [11] == range10 + [11], true) &&
// Slicing
std.assertEqual(range10[2:5], [3, 4, 5]) &&
// std.sort on range (already sorted)
std.assertEqual(std.sort(range5), [0, 1, 2, 3, 4]) &&
// std.reverse on range
std.assertEqual(std.reverse(range5), [4, 3, 2, 1, 0]) &&
// std.member on range
std.assertEqual(std.member(range10, 5), true) &&
std.assertEqual(std.member(range10, 11), false) &&
// std.map on range
std.assertEqual(std.map(function(x) x + 1, range5), [1, 2, 3, 4, 5]) &&
// Empty range
std.assertEqual(std.length(emptyRange), 0) &&
std.assertEqual(emptyRange, []) &&
// Range equality
std.assertEqual(std.range(1, 5), [1, 2, 3, 4, 5]) &&
// Double-reverse preserves original range
std.assertEqual(std.reverse(std.reverse(range5)), [0, 1, 2, 3, 4]) &&
std.assertEqual(std.reverse(std.reverse(range10)), std.range(1, 10)) &&
// Nested range operations
std.assertEqual(std.foldl(function(acc, x) acc + x, range5, 0), 10) &&
// Large range (deferred creation, only access subset)
local bigRange = std.range(1, 100000);
std.assertEqual(bigRange[0], 1) &&
std.assertEqual(bigRange[99999], 100000) &&
std.assertEqual(std.length(bigRange), 100000) &&
// Range concat with shared prefix comparison
local big = std.range(1, 10000);
std.assertEqual(big + [1] < big + [2], true) &&
std.assertEqual(big + [2] < big + [1], false) &&
std.assertEqual(big + [1] == big + [1], true) &&
true
