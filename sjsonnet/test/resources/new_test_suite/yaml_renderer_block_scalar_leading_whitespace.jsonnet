// Test YAML block scalar leading whitespace handling.
// Strings with leading whitespace require an indent indicator (e.g., |2).
// Strings without leading whitespace should not have an indent indicator.
local contains(haystack, needle) = std.length(std.findSubstr(needle, haystack)) > 0;
local leading = std.manifestYamlDoc("  foo\n  bar\n");
local noLeading = std.manifestYamlDoc("foo\nbar\n");
assert contains(leading, "|2") : "leading whitespace requires indent indicator |2";
assert !contains(noLeading, "|2") : "no leading whitespace means no indent indicator";
true
