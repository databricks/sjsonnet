// std.escapeStringJson and std.escapeStringPython should output non-ASCII
// characters literally (not as \uXXXX), matching go-jsonnet and jrsonnet.

std.assertEqual(std.escapeStringJson("é"), "\"é\"") &&
std.assertEqual(std.escapeStringJson("日本語"), "\"日本語\"") &&
std.assertEqual(std.escapeStringJson("hello é world"), "\"hello é world\"") &&
std.assertEqual(std.escapeStringPython("é"), "\"é\"") &&
std.assertEqual(std.escapeStringPython("日本語"), "\"日本語\"") &&
std.assertEqual(std.escapeStringPython("hello é world"), "\"hello é world\"")
