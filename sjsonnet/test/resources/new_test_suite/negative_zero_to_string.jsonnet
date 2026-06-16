// Verify that -0 is preserved in string conversion paths:
// std.toString, string concatenation, and %s format.
// go-jsonnet produces "-0" for all of these.
local neg0 = -0;
assert std.toString(neg0) == "-0" : "std.toString(-0) should be '-0', got " + std.toString(neg0);
assert ('' + neg0) == "-0" : "'' + (-0) should be '-0', got " + ('' + neg0);
assert (neg0 + '') == "-0" : "(-0) + '' should be '-0', got " + (neg0 + '');
assert ('%s' % neg0) == "-0" : "'%s' % (-0) should be '-0', got " + ('%s' % neg0);
assert ('x' + neg0) == "x-0" : "'x' + (-0) should be 'x-0', got " + ('x' + neg0);
assert (neg0 + 'y') == "-0y" : "(-0) + 'y' should be '-0y', got " + (neg0 + 'y');
true
