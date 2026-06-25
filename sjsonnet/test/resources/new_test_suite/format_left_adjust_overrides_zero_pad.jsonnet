{
  // When both - (left-justify) and 0 (zero-pad) flags are present,
  // left-justify takes precedence per POSIX/Python convention.
  leftJustifyZeroPadInt: std.format("%-05d", 42),
  leftJustifyZeroPadNeg: std.format("%-05d", -42),
  leftJustifyZeroPadPlus: std.format("%-+05d", 42),
  leftJustifyZeroPadFloat: std.format("%-08.2f", 3.14),
  leftJustifyZeroPadWide: std.format("%-010d", 42),
  leftJustifyZeroPadPercentOperator: "%-05d" % 42,
  // Pure zero-pad still works
  zeroPadInt: std.format("%05d", 42),
  // Pure left-justify still works
  leftJustifyInt: std.format("%-5d", 42),
}
