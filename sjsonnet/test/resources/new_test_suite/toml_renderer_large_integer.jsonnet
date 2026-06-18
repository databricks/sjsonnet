// TomlRenderer renders large integers as decimal instead of scientific notation
local large = std.manifestToml({a: 1e20});
assert std.length(std.findSubstr("E", large)) == 0 : "Large integer must not contain scientific notation 'E'";
assert std.length(std.findSubstr("100000000000000000000", large)) > 0 : "Large integer must render as decimal";
// Regular integers still work
assert std.manifestToml({a: 42}) == "a = 42" : "Regular integer must render correctly";
// Fractions still render as floats
assert std.manifestToml({a: 3.14}) == "a = 3.14" : "Float must render correctly";
true
