// std.asin, std.acos, and std.pow with valid inputs still work
assert std.asin(0) == 0 : "std.asin(0) should be 0";
assert std.acos(1) == 0 : "std.acos(1) should be 0";
assert std.pow(2, 3) == 8 : "std.pow(2, 3) should be 8";
true
