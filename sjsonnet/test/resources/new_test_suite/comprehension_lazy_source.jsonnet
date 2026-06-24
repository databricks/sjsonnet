// Regression test: array comprehensions must be lazy — source elements should only be
// evaluated when the corresponding result element is accessed. This matches C++ jsonnet
// and go-jsonnet behavior.

// Basic lazy source: accessing arr[0] should not evaluate the third source element
local basic = local arr = [x * x for x in [1, 2, error "third"]]; arr[0];

// Lazy source with binary op body (previously had an eager fast path)
local binop = local arr = [x + 10 for x in [1, 2, error "third"]]; arr[1];

// Lazy source with ValidId body
local validId = local arr = [x for x in [1, 2, error "third"]]; arr[0];

// Nested comprehension: only accessed elements should be evaluated
local nested = local arr = [x + y for x in [1, 2] for y in [10, error "inner"]]; arr[0];

// std.length should not force element evaluation
local len = std.length([x for x in [1, 2, error "third"]]);

// std.slice should not force evaluation of sliced-out elements
local sliced = local arr = [x * x for x in [1, 2, error "third"]]; std.slice(arr, 0, 1, 1);

// Recursive indexed access must restore the comprehension binding after evaluating another element.
local recursiveSlot = local arr = [if x == 0 then arr[1] + x else x for x in [0, 1]]; arr[0];

// Immediate bodies use a reused scope slot; reentrant source evaluation must restore it.
local mutableSlotReentry = local arr = [x + x for x in [arr[1] + 1, 10]]; arr[0];

// Two-level immediate comprehensions also reuse slots; reentrant source evaluation must restore them.
local twoLevelSlotReentry = local arr = [x + x + y for x in [arr[2] + 1, 10] for y in [1, 2]]; arr[0];

// Empty outer arrays must not force invariant inner sources.
local emptyOuter = std.length([x + y for x in [] for y in error "inner"]);

// Pure range fast paths must still compute by accessed index, not by eager full expansion.
local pureRangePartialError = local arr = [1 / x for x in std.range(0, 1)]; arr[1];
local pureRangeLength = std.length([1 / x for x in std.range(0, 1)]);
local pureTwoRangePartialError =
  local arr = [1 / (x + y) for x in std.range(0, 1) for y in std.range(0, 1)];
  arr[1];
local pureRangeReverseLazy = local arr = [1 / x for x in std.range(0, 1)]; std.reverse(arr)[0];
local pureRangeDoubleReverseLazy =
  local arr = [1 / x for x in std.range(0, 1)];
  std.reverse(std.reverse(arr))[1];
local pureRangeAsSourceLazy = local arr = [1 / x for x in std.range(0, 1)]; [y for y in arr][1];
local pureRangeConcatLazy =
  local arr = [1 / x for x in std.range(0, 1)] + [2];
  arr[1];
local pureRangeSliceLazy =
  local arr = [1 / x for x in std.range(0, 1)];
  std.slice(arr, 1, 2, 1)[0];
local pureRangeLargeSliceViewLazy =
  local arr = [1 / x for x in std.range(0, 5000)];
  std.slice(arr, 1, 5001, 1)[0];

std.assertEqual(basic, 1) &&
std.assertEqual(binop, 12) &&
std.assertEqual(validId, 1) &&
std.assertEqual(nested, 11) &&
std.assertEqual(len, 3) &&
std.assertEqual(sliced, [1]) &&
std.assertEqual(recursiveSlot, 1) &&
std.assertEqual(mutableSlotReentry, 42) &&
std.assertEqual(twoLevelSlotReentry, 45) &&
std.assertEqual(emptyOuter, 0) &&
std.assertEqual(pureRangePartialError, 1) &&
std.assertEqual(pureRangeLength, 2) &&
std.assertEqual(pureTwoRangePartialError, 1) &&
std.assertEqual(pureRangeReverseLazy, 1) &&
std.assertEqual(pureRangeDoubleReverseLazy, 1) &&
std.assertEqual(pureRangeAsSourceLazy, 1) &&
std.assertEqual(pureRangeConcatLazy, 1) &&
std.assertEqual(pureRangeSliceLazy, 1) &&
std.assertEqual(pureRangeLargeSliceViewLazy, 1) &&
true
