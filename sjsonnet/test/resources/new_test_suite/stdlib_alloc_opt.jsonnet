// Regression tests for stdlib allocation optimizations:
// flattenArrays, reverse, join (array path), foldl
local test_flattenArrays =
  // Mixed nulls and arrays
  std.assertEqual(std.flattenArrays([null, [1], null, [2, 3]]), [1, 2, 3]) &&
  // All nulls
  std.assertEqual(std.flattenArrays([null, null]), []) &&
  // Empty input
  std.assertEqual(std.flattenArrays([]), []) &&
  // Single element
  std.assertEqual(std.flattenArrays([[42]]), [42]) &&
  // Nested empty arrays
  std.assertEqual(std.flattenArrays([[], [1], [], [2]]), [1, 2]) &&
  true;

local test_reverse =
  std.assertEqual(std.reverse([1, 2, 3]), [3, 2, 1]) &&
  std.assertEqual(std.reverse([]), []) &&
  std.assertEqual(std.reverse([42]), [42]) &&
  true;

local test_join_array_empty_sep =
  // Empty separator (flatten path)
  std.assertEqual(std.join([], [null, [1], [2]]), [1, 2]) &&
  std.assertEqual(std.join([], [[1], [2, 3]]), [1, 2, 3]) &&
  std.assertEqual(std.join([], []), []) &&
  true;

local test_join_array_with_sep =
  // Non-empty separator
  std.assertEqual(std.join([0], [[1], [2]]), [1, 0, 2]) &&
  std.assertEqual(std.join([0], [null, [1], [2]]), [1, 0, 2]) &&
  std.assertEqual(std.join([0], [[1]]), [1]) &&
  true;

local test_foldl =
  std.assertEqual(std.foldl(function(a, b) a + b, [1, 2, 3], 0), 6) &&
  std.assertEqual(std.foldl(function(a, b) a + b, [], 0), 0) &&
  std.assertEqual(std.foldl(function(a, b) a + b, "abc", ""), "abc") &&
  true;

test_flattenArrays && test_reverse && test_join_array_empty_sep &&
test_join_array_with_sep && test_foldl
