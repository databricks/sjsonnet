// Directional objectRemoveKey tests for inheritance, hidden fields, and +: sugar.
// Each assertion compares sjsonnet behavior to the Jsonnet semantic result directly.

std.assertEqual(
  { a+: 3 } + std.objectRemoveKey({ a+:: 100 }, 'a') + { a+: 2 },
  { a: 5 }
) &&
std.assertEqual(
  std.objectRemoveKey({ a: 100 }, 'a') + { a+: 2 },
  { a: 2 }
) &&
std.assertEqual(
  std.objectRemoveKey({ a:: 100 }, 'a') + { a+: 2 },
  { a: 2 }
) &&
std.assertEqual(
  std.objectRemoveKey({ a: 1 } + { b: super.a }, 'a') + { a+: 2 },
  { a: 2, b: 1 }
) &&
std.assertEqual(
  { a: 1 } + std.objectRemoveKey({ a+:: 100 }, 'a'),
  { a: 1 }
) &&
std.assertEqual(
  std.objectHasAll({ a:: 1 } + std.objectRemoveKey({ a: 2 }, 'a'), 'a'),
  true
) &&
std.assertEqual(
  std.objectHas({ a:: 1 } + std.objectRemoveKey({ a: 2 }, 'a'), 'a'),
  false
) &&
std.assertEqual(
  ({ a:: 1 } + std.objectRemoveKey({ a: 2 }, 'a')).a,
  1
)
