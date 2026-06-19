// Directional tests for std.format %.0g. Jsonnet documents std.format as Python %-formatting,
// where precision 0 for %g behaves as precision 1.

std.assertEqual("%.0g" % 0.0, "0") &&
std.assertEqual("%.0g" % 1.0, "1") &&
std.assertEqual("%+.0g" % 1.0, "+1") &&
std.assertEqual("% .0g" % 1.0, " 1") &&
std.assertEqual("%010.0g" % 1.0, "0000000001") &&
std.assertEqual("%#.0g" % 1.0, "1.") &&
std.assertEqual("%.0g" % 0.1, "0.1") &&
std.assertEqual("%.0g" % 9.9, "1e+01")
