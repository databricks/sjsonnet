// Regression test: %s must still work with booleans after the type-error fix.
// Verified against cpp-jsonnet 0.21.0, go-jsonnet 0.22.0, jrsonnet 0.5.0-pre99.
std.assertEqual("%s" % true, "true") &&
std.assertEqual("%s" % false, "false") &&
std.assertEqual("%10s" % true, "      true") &&
std.assertEqual("%-10s" % false, "false     ") &&
std.assertEqual("val: %s" % true, "val: true") &&
std.assertEqual("%s %s" % [true, false], "true false") &&
true
