{
  clampStringMiddle: std.clamp("b", "a", "c"),
  clampStringLow: std.clamp("a", "b", "c"),
  clampStringHigh: std.clamp("d", "b", "c"),
  clampArrayMiddle: std.clamp([2], [1], [3]),
  clampArrayLow: std.clamp([0], [1], [3]),
  clampArrayHigh: std.clamp([4], [1], [3]),
  clampLazyMax: std.clamp(0, 1, error "maxVal forced"),
  splitLimitNegative: std.splitLimit("a,b,c", ",", -2),
  splitLimitFractionalNegative: std.splitLimit("a,b,c", ",", -2.5),
  splitLimitRFractionalNegative: std.splitLimitR("a,b,c", ",", -2.5),
  splitLimitFractionalPositive: std.splitLimit("a,b,c", ",", 1.5),
  splitLimitRFractionalPositive: std.splitLimitR("a,b,c", ",", 1.5),
}
