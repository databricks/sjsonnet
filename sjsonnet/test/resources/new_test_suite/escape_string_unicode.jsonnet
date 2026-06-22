// std.escapeStringJson and std.escapeStringPython should output non-ASCII
// characters literally (not as \uXXXX), matching go-jsonnet and jrsonnet.

std.assertEqual(std.escapeStringJson("é"), "\"é\"") &&
std.assertEqual(std.escapeStringJson("日本語"), "\"日本語\"") &&
std.assertEqual(std.escapeStringJson("hello é world"), "\"hello é world\"") &&
std.assertEqual(std.escapeStringPython("é"), "\"é\"") &&
std.assertEqual(std.escapeStringPython("日本語"), "\"日本語\"") &&
std.assertEqual(std.escapeStringPython("hello é world"), "\"hello é world\"") &&
// DEL (0x7F) and C1 control characters (0x80–0x9F) must be escaped as \uXXXX
// to match go-jsonnet and jrsonnet.
std.assertEqual(std.escapeStringJson(std.char(127)), "\"\\u007f\"") &&
std.assertEqual(std.escapeStringJson(std.char(128)), "\"\\u0080\"") &&
std.assertEqual(std.escapeStringJson(std.char(159)), "\"\\u009f\"") &&
std.assertEqual(std.escapeStringPython(std.char(127)), "\"\\u007f\"") &&
std.assertEqual(std.escapeStringPython(std.char(128)), "\"\\u0080\"") &&
std.assertEqual(std.escapeStringPython(std.char(159)), "\"\\u009f\"") &&
// Characters above 0x9F (NBSP, accented letters, etc.) stay literal.
std.assertEqual(std.escapeStringJson(std.char(160)), "\"\u00a0\"") &&
std.assertEqual(std.escapeStringJson(std.char(233)), "\"é\"")
