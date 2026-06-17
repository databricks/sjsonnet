// Test that arrays without functions compare correctly, and function vs non-function returns false.
local f = function(x) x;
assert [1, 2, 3] == [1, 2, 3];
assert [1, 2] != [1, 3];
assert !(f == 42);
assert f != 42;
true
