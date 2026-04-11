// Test two-level comprehension with loop-invariant inner array.
// Exercises the visitCompTwoLevel optimization: when the inner ForSpec
// doesn't depend on the outer variable, evaluate it once.

local range = std.range(0, 9);

// BinaryOp(outerVar, innerVar) — multiplication
local mul = [x * y for x in range for y in range];
std.assertEqual(std.length(mul), 100) &&
std.assertEqual(mul[0], 0) &&      // 0*0
std.assertEqual(mul[9], 0) &&      // 0*9
std.assertEqual(mul[11], 1) &&     // 1*1
std.assertEqual(mul[99], 81) &&    // 9*9
std.assertEqual(mul[55], 25) &&    // 5*5

// BinaryOp(innerVar, outerVar) — swapped operands
local sub = [y - x for x in range for y in range];
std.assertEqual(std.length(sub), 100) &&
std.assertEqual(sub[0], 0) &&      // 0-0
std.assertEqual(sub[9], 9) &&      // 9-0
std.assertEqual(sub[10], -1) &&    // 0-1
std.assertEqual(sub[99], 0) &&     // 9-9

// Non-numeric body (string concatenation via nested objects)
local strs = [std.toString(x) + std.toString(y) for x in std.range(0, 2) for y in std.range(0, 2)];
std.assertEqual(strs, ["00", "01", "02", "10", "11", "12", "20", "21", "22"]) &&

// Comparison ops
local cmp = [x < y for x in std.range(0, 2) for y in std.range(0, 2)];
std.assertEqual(cmp, [false, true, true, false, false, true, false, false, false]) &&

// Single-element arrays (should still work)
local single = [x + y for x in [1] for y in [2]];
std.assertEqual(single, [3]) &&

// Empty inner array
local empty = [x + y for x in std.range(0, 2) for y in []];
std.assertEqual(empty, []) &&

// Empty outer array
local emptyOuter = [x + y for x in [] for y in std.range(0, 2)];
std.assertEqual(emptyOuter, []) &&

// Larger cross-product to exercise the optimization at scale
local big = [x * y for x in std.range(0, 49) for y in std.range(0, 49)];
std.assertEqual(std.length(big), 2500) &&
std.assertEqual(big[2499], 2401) &&  // 49*49

true
