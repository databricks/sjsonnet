// Directional std.filterMap tests: function arguments remain lazy like official Jsonnet.

std.assertEqual(
  std.filterMap(function(x) false, function(x) error 'map', [error 'elem']),
  []
) &&
std.assertEqual(
  std.filterMap(function(x) true, function(x) 1, [error 'elem']),
  [1]
)
