// std.mantissa must preserve the sign of negative zero.
// assertEqual cannot distinguish -0.0 from 0.0 (IEEE equality),
// so we observe the sign through std.toString rendering.
std.toString(std.mantissa(0 * -1)) == "-0" &&
std.toString(std.mantissa(0)) == "0" &&
std.toString(std.mantissa(1.5)) == "0.75" &&
std.toString(std.mantissa(-1.5)) == "-0.75"
