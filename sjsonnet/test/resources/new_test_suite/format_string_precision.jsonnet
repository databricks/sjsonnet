// Python-style %s precision limits the formatted string by codepoint.

std.assertEqual("%.3s" % "abcdef", "abc") &&
std.assertEqual("%.0s" % "abcdef", "") &&
std.assertEqual("%5.3s" % "abcdef", "  abc") &&
std.assertEqual("%-5.3s" % "abcdef", "abc  ") &&
std.assertEqual("%.*s" % [3, "abcdef"], "abc") &&
std.assertEqual("%.*s" % [0, "abcdef"], "") &&
std.assertEqual("%.*s" % [-3, "abcdef"], "") &&
std.assertEqual("%*.*s" % [6, 3, "abcdef"], "   abc") &&
std.assertEqual("%-*.*s" % [6, 3, "abcdef"], "abc   ") &&
std.assertEqual("%.1s" % "😀x", "😀") &&
std.assertEqual("%.*s" % [1, "éx"], "é") &&
std.assertEqual("%.2s" % 123.456, "12") &&
std.assertEqual("%.2s" % true, "tr") &&
std.assertEqual("%.2s" % null, "nu") &&
true
