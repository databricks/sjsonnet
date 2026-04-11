// Regression test: inline object materialization must correctly handle
// self-references between sibling fields, verifying that caching is preserved.
local obj = { a: 42, b: self.a + 1, c: self.b * 2 };
std.assertEqual(obj, { a: 42, b: 43, c: 86 })
