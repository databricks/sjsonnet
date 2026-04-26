// Directional std.mapWithKey tests: match official Jsonnet hidden-field semantics.

std.assertEqual(
  std.mapWithKey(function(k, v) v + 1, { a: 1, b:: 2, c::: 3 }),
  { a: 2, c: 4 }
) &&
std.assertEqual(
  std.objectFields(std.mapWithKey(function(k, v) v + 1, { a: 1, b:: 2, c::: 3 })),
  ['a', 'c']
) &&
std.assertEqual(
  std.objectFieldsAll(std.mapWithKey(function(k, v) v + 1, { a: 1, b:: 2, c::: 3 })),
  ['a', 'c']
) &&
std.assertEqual(
  std.mapWithKey(
    function(k, v) if k == 'hidden' then error 'hidden field mapped' else v,
    { visible: 1, hidden:: error 'hidden field evaluated' }
  ),
  { visible: 1 }
)
