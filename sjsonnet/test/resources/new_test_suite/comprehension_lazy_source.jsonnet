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

std.assertEqual(basic, 1) &&
std.assertEqual(binop, 12) &&
std.assertEqual(validId, 1) &&
std.assertEqual(nested, 11) &&
std.assertEqual(len, 3) &&
std.assertEqual(sliced, [1]) &&
std.assertEqual(recursiveSlot, 1) &&
std.assertEqual(mutableSlotReentry, 42) &&
true
