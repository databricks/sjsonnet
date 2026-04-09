// Regression test: Verify that object fields referencing lazy bindings
// are not eagerly evaluated. In Jsonnet, field bodies are lazy — they
// should only be evaluated when accessed. This test ensures our
// ConstMember optimization does not break lazy semantics.

// Test 1: Unused field with error should not crash
local obj1 = {
  a: 1,
  b: error "should not be evaluated",
};

// Test 2: Arithmetic in field body should work when accessed
local x = 10;
local y = 20;
local obj2 = {
  sum: x + y,
  product: x * y,
};

// Test 3: Nested objects with computed fields
local obj3 = {
  inner: {
    val: x * 2 + y,
  },
};

// Test 4: Boolean operations in field body
local obj4 = {
  gt: x > 5,
  lt: y < 100,
  eq: x + y == 30,
};

std.assertEqual(obj1.a, 1) &&
std.assertEqual(obj2.sum, 30) &&
std.assertEqual(obj2.product, 200) &&
std.assertEqual(obj3.inner.val, 40) &&
std.assertEqual(obj4.gt, true) &&
std.assertEqual(obj4.lt, true) &&
std.assertEqual(obj4.eq, true) &&
true
