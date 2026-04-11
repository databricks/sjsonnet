// Regression test: objects WITH self-references must still use field caching
// to prevent duplicate evaluation. std.trace prints to stderr and returns
// its second argument; if caching is incorrectly skipped, trace would fire
// multiple times.
local obj = {
  a: 42,
  b: self.a + 1,
  c: self.b * 2,
};
// Nested $ reference: cache must be preserved for the root object
local root = {
  x: 100,
  y: { z: $.x + 1 },
};
// Object local using self
local withLocal = {
  local doubled = self.val,
  val: 7,
  result: doubled,
};
// Assert using self
local withAssert = {
  assert self.x > 0 : "x must be positive",
  x: 42,
  y: self.x + 1,
};
std.assertEqual(obj, { a: 42, b: 43, c: 86 }) &&
std.assertEqual(root, { x: 100, y: { z: 101 } }) &&
std.assertEqual(withLocal, { val: 7, result: 7 }) &&
std.assertEqual(withAssert, { x: 42, y: 43 }) &&
true
