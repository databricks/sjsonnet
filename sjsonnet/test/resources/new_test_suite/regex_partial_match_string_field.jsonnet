// std.regexPartialMatch string field should return matched substring, not full input
local r1 = std.regexPartialMatch("foo", "foobar");
local r2 = std.regexPartialMatch("[0-9]+", "abc123def");
local r3 = std.regexFullMatch("foo", "foo");
assert r1.string == "foo" : "partial match substring";
assert r2.string == "123" : "digit match substring";
assert r3.string == "foo" : "full match substring";
true
