// Directional tests for std.format rounding that carries into the next exponent.
// Jsonnet documents std.format as Python %-formatting.

std.assertEqual("%.0e" % 9.5e10, "1e+11") &&
std.assertEqual("%.1e" % 9.99, "1.0e+01") &&
std.assertEqual("%.1E" % 9.99, "1.0E+01") &&
std.assertEqual("%.1g" % 9.9, "1e+01") &&
std.assertEqual("%.2g" % 99.9, "1e+02") &&
std.assertEqual("%#.1g" % 9.9, "1.e+01") &&
std.assertEqual("%.2G" % 99.9, "1E+02")
