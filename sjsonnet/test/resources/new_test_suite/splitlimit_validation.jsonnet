std.splitLimit("a-b-c", "-", 1) == ["a", "b-c"] &&
std.splitLimit("a-b-c", "-", -1) == ["a", "b", "c"] &&
std.splitLimit("a-b-c", "-", 0) == ["a-b-c"] &&
std.splitLimitR("a-b-c", "-", 1) == ["a-b", "c"] &&
true
