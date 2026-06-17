// Test YAML block scalar trailing newline handling.
// Single trailing newline should use clip mode (|), not keep mode (|+).
// Multiple trailing newlines should use keep mode (|+).
local contains(haystack, needle) = std.length(std.findSubstr(needle, haystack)) > 0;
local singleNl = std.manifestYamlDoc("foo\nbar\n");
local multiNl = std.manifestYamlDoc("foo\nbar\n\n");
assert contains(singleNl, "|") : "single trailing newline should use clip mode |";
assert !contains(singleNl, "|+") : "single trailing newline should not use keep mode |+";
assert contains(multiNl, "|+") : "multiple trailing newlines should use keep mode |+";
true
