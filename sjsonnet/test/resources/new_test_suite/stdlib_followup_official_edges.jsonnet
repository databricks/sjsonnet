{
  clampStringMiddle: std.clamp("b", "a", "c"),
  clampStringLow: std.clamp("a", "b", "c"),
  clampStringHigh: std.clamp("d", "b", "c"),
  clampArrayMiddle: std.clamp([2], [1], [3]),
  clampArrayLow: std.clamp([0], [1], [3]),
  clampArrayHigh: std.clamp([4], [1], [3]),
  clampLazyMax: std.clamp(0, 1, error "maxVal forced"),
}
