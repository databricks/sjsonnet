// Regression test: std.parseJson must handle integers outside Java long range.
// Verified against cpp-jsonnet 0.21.0, go-jsonnet 0.22.0, jrsonnet 0.5.0-pre99.
// These used to crash with NumberFormatException; now they parse as float64.
std.parseJson("9223372036854775808") > 0 &&
std.parseJson("-9223372036854775809") < 0 &&
std.parseJson("99999999999999999999999999999999") > 0 &&
// Normal integers still work
std.parseJson("42") == 42 &&
std.parseJson("-1") == -1 &&
std.parseJson("0") == 0 &&
std.parseJson("-0") == 0 &&
true
