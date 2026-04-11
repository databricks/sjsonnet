// Regression test: objects WITHOUT self/super/$ references should produce
// correct results even when field caching is skipped during materialization.
// Tests the _skipFieldCache optimization in Materializer.
local simpleObj = { a: 1, b: "hello", c: true };
local nestedObj = {
  x: 42,
  y: "world",
  z: { inner: 1, more: "nested" },
};
// Object built from comprehension variables (no self refs)
local items = [
  { name: "item" + i, val: i * 10 }
  for i in std.range(0, 3)
];
std.assertEqual(simpleObj, { a: 1, b: "hello", c: true }) &&
std.assertEqual(nestedObj.x, 42) &&
std.assertEqual(nestedObj.z, { inner: 1, more: "nested" }) &&
std.assertEqual(std.length(items), 4) &&
std.assertEqual(items[0], { name: "item0", val: 0 }) &&
std.assertEqual(items[3], { name: "item3", val: 30 }) &&
true
