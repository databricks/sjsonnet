std.assertEqual(
  std.sort([std.repeat("b", 1), std.repeat("a", 1)]),
  ["a", "b"]
) &&
std.assertEqual(
  std.sort(["b", "a"], function(x) std.repeat(x, 1)),
  ["a", "b"]
) &&
std.assertEqual(
  std.sort([std.repeat("b", 1), "a"]),
  ["a", "b"]
)
