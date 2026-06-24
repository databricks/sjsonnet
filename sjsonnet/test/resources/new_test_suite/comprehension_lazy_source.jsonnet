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

std.assertEqual(basic, 1) &&
std.assertEqual(binop, 12) &&
std.assertEqual(validId, 1) &&
std.assertEqual(nested, 11) &&
std.assertEqual(len, 3) &&
std.assertEqual(sliced, [1]) &&
true
