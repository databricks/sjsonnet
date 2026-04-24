// Regression tests for RangeArr subclass correctness.
// Covers multi-use, reverse, concat, and element access scenarios.
local r = std.range(0, 9);  // [0,1,2,...,9]

// 1. Multiple consumption
local arr1 = [x for x in r];
local arr2 = [x for x in r];

// 2. Reverse
local rev = std.reverse(r);

// 3. Double-reverse
local rev2 = std.reverse(std.reverse(r));

// 4. Concat
local extended = r + [10, 11];

// 5. Original still usable after concat
local afterConcat = r + [99];
local arr3 = [x for x in r];

// 6. Element access
local first = r[0];
local last = r[9];

// 7. Slice
local sliced = r[2:5];

// 8. Length
local len = std.length(r);

// 9. Reversed element access
local revFirst = rev[0];
local revLast = rev[9];

// 10. Large range
local big = std.range(0, 999);
local bigRev = std.reverse(big);
local bigFirst = big[0];
local bigLast = big[999];
local bigRevFirst = bigRev[0];
local bigRevLast = bigRev[999];

std.assertEqual(arr1, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) &&
std.assertEqual(arr2, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) &&
std.assertEqual(arr3, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) &&
std.assertEqual(rev, [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]) &&
std.assertEqual(rev2, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) &&
std.assertEqual(extended, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]) &&
std.assertEqual(first, 0) &&
std.assertEqual(last, 9) &&
std.assertEqual(sliced, [2, 3, 4]) &&
std.assertEqual(len, 10) &&
std.assertEqual(revFirst, 9) &&
std.assertEqual(revLast, 0) &&
std.assertEqual(bigFirst, 0) &&
std.assertEqual(bigLast, 999) &&
std.assertEqual(bigRevFirst, 999) &&
std.assertEqual(bigRevLast, 0) &&
std.assertEqual(std.length(big), 1000) &&
true
