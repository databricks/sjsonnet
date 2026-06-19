// Directional tests for sign flags on zero float conversions.

std.assertEqual("%+f" % 0.0, "+0.000000") &&
std.assertEqual("% f" % 0.0, " 0.000000") &&
std.assertEqual("%+e" % 0.0, "+0.000000e+00") &&
std.assertEqual("%+E" % 0.0, "+0.000000E+00") &&
std.assertEqual("%+g" % 0.0, "+0") &&
std.assertEqual("%+G" % 0.0, "+0") &&
std.assertEqual("%+010f" % 0.0, "+00.000000") &&
std.assertEqual("%+f" % (-0.0), "+0.000000")
