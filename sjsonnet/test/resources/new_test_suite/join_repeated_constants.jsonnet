// Directional coverage for std.join over repeated/constant arrays.

std.assertEqual(std.join("-", std.repeat(["x"], 4)), "x-x-x-x") &&
std.assertEqual(std.join("/", std.repeat(["é"], 3)), "é/é/é") &&
std.assertEqual(std.join("-", std.repeat([null], 4)), "") &&
std.assertEqual(std.join([0], std.repeat([[1]], 3)), [1, 0, 1, 0, 1]) &&
std.assertEqual(std.join("-", std.repeat([error "unused"], 0)), "") &&
true
