// Regression test: ByteRenderer empty-tracking bitmask overflow at depth >= 64.
local nest(n, leaf) =
  std.foldl(function(acc, _) { inner: acc }, std.range(1, n), leaf);
local deep100 = nest(100, "leaf");
local drillDown(obj, n) =
  std.foldl(function(acc, _) acc.inner, std.range(1, n), obj);
assert std.assertEqual(drillDown(deep100, 100), "leaf") : "depth-100 leaf reachable";
local deep200 = nest(200, 42);
assert std.assertEqual(drillDown(deep200, 200), 42) : "depth-200 leaf reachable";
true
