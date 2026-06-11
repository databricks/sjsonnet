// std.primitiveEquals must treat -0.0 and 0.0 as equal (IEEE 754 semantics).
// This matches go-jsonnet and C++ jsonnet behavior.
std.assertEqual(std.primitiveEquals(-0.0, 0.0), true) &&
std.assertEqual(std.primitiveEquals(0.0, -0.0), true) &&
std.assertEqual(std.primitiveEquals(-0.0, -0.0), true) &&
std.assertEqual(std.primitiveEquals(0.0, 0.0), true) &&
std.assertEqual(std.primitiveEquals(-0.0, 1.0), false) &&
std.assertEqual(std.primitiveEquals(0.0, 1.0), false) &&
true
