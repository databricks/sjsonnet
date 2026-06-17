// std.isEmpty should work for valid types and error on invalid types
assert std.isEmpty("") == true : "empty string";
assert std.isEmpty("hello") == false : "non-empty string";
assert std.isEmpty([]) == true : "empty array";
assert std.isEmpty({}) == true : "empty object";
true
