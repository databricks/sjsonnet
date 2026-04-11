// Test lazy concat array behavior: correctness of element access, comparison,
// and materialization for both large (lazy) and small (eager) concat cases.
local small = [1, 2, 3];
local large = std.range(1, 500);  // above LAZY_CONCAT_THRESHOLD (256)

// Small array concat (eager path)
local small_concat = small + [4, 5];
assert small_concat == [1, 2, 3, 4, 5];
assert std.length(small_concat) == 5;

// Large array concat (lazy path) — element access
local large_a = large + [501];
local large_b = large + [502];
assert std.length(large_a) == 501;
assert std.length(large_b) == 501;
assert large_a[0] == 1;
assert large_a[499] == 500;
assert large_a[500] == 501;
assert large_b[500] == 502;

// Comparison of large concatenated arrays (the benchmark pattern)
assert large_a < large_b;
assert !(large_b < large_a);
assert !(large_a == large_b);
assert large + [500] == large + [500];

// Concat of concat (should flatten/materialize, not nest)
local double_concat = large_a + [503];
assert std.length(double_concat) == 502;
assert double_concat[0] == 1;
assert double_concat[500] == 501;
assert double_concat[501] == 503;

// Bulk operations on lazy concat arrays (triggers materialization)
assert std.length(std.filter(function(x) x > 498, large_a)) == 3;
assert std.map(function(x) x, large_a)[500] == 501;
assert std.reverse(large_a)[0] == 501;
assert std.sort(large_b) == large_b;

// Edge: concat with empty
assert large + [] == large;
assert [] + large == large;

// std.assertEqual chain for golden output
std.assertEqual(large_a[500], 501) &&
std.assertEqual(large_b[500], 502) &&
std.assertEqual(std.length(double_concat), 502) &&
std.assertEqual(large_a < large_b, true) &&
true
