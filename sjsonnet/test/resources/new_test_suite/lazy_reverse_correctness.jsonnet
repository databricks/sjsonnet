// Test lazy reverse array correctness across various operations
local arr = [1, 2, 3, 4, 5];
local rev = std.reverse(arr);
local emptyRev = std.reverse([]);
local singleRev = std.reverse([42]);

// Test double reverse cancels out
local doubleRev = std.reverse(std.reverse(arr));

// Test reverse with concatenation
local revConcat = std.reverse([1, 2, 3]) + std.reverse([4, 5, 6]);

// Test reverse inside filter/map/sort
local revFiltered = std.filter(function(x) x > 2, std.reverse(arr));
local revMapped = std.map(function(x) x * 10, std.reverse(arr));
local revSorted = std.sort(std.reverse(arr));

// Test reverse with foldl/foldr
local revSum = std.foldl(function(acc, x) acc + x, std.reverse(arr), 0);

// Test reverse with slice
local revSlice = std.reverse([10, 20, 30, 40, 50])[1:3];

// Test reverse with array comparison
local cmp1 = std.reverse([3, 2, 1]) == [1, 2, 3];
local cmp2 = std.reverse([1, 2, 3]) == std.reverse([1, 2, 3]);

// Test reverse with std.length
local revLen = std.length(std.reverse([1, 2, 3, 4]));

// Test reverse with std.member
local revMember = std.member(std.reverse([10, 20, 30]), 20);

// Test reverse with std.join
local revJoin = std.join(",", std.reverse(["c", "b", "a"]));

// Test reverse with nested arrays
local nested = [[1, 2], [3, 4], [5, 6]];
local revNested = std.reverse(nested);

// Test reverse preserves element identity (lazy thunks)
local lazyArr = [i * i for i in std.range(0, 9)];
local revLazy = std.reverse(lazyArr);

std.assertEqual(rev, [5, 4, 3, 2, 1]) &&
std.assertEqual(emptyRev, []) &&
std.assertEqual(singleRev, [42]) &&
std.assertEqual(doubleRev, arr) &&
std.assertEqual(revConcat, [3, 2, 1, 6, 5, 4]) &&
std.assertEqual(revFiltered, [5, 4, 3]) &&
std.assertEqual(revMapped, [50, 40, 30, 20, 10]) &&
std.assertEqual(revSorted, [1, 2, 3, 4, 5]) &&
std.assertEqual(revSum, 15) &&
std.assertEqual(revSlice, [40, 30]) &&
std.assertEqual(cmp1, true) &&
std.assertEqual(cmp2, true) &&
std.assertEqual(revLen, 4) &&
std.assertEqual(revMember, true) &&
std.assertEqual(revJoin, "a,b,c") &&
std.assertEqual(revNested, [[5, 6], [3, 4], [1, 2]]) &&
std.assertEqual(revLazy, [81, 64, 49, 36, 25, 16, 9, 4, 1, 0]) &&
true
