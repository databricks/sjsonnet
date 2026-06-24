// Regression tests for std.format %c — verified against C++ jsonnet v0.21.0
// Success cases: single-char strings and numeric codepoints
std.assertEqual("%c" % "A", "A") &&
std.assertEqual("%c" % 65, "A") &&
std.assertEqual("%c" % "世", "世") &&
std.assertEqual("%c" % 127757, "🌍") &&
std.assertEqual("%10c" % "A", "         A") &&
true
