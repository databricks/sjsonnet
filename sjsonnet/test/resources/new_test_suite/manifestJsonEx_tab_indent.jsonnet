local tabIndent = std.manifestJsonEx({a: 1, b: [2, 3]}, "\t");
local spaceIndent = std.manifestJsonEx({a: 1, b: [2, 3]}, "  ");
local expected_tab = "{\n\t\"a\": 1,\n\t\"b\": [\n\t\t2,\n\t\t3\n\t]\n}";
local expected_space = "{\n  \"a\": 1,\n  \"b\": [\n    2,\n    3\n  ]\n}";
assert tabIndent == expected_tab : "tab indent mismatch, got length " + std.length(tabIndent) + " expected length " + std.length(expected_tab);
assert spaceIndent == expected_space : "space indent mismatch, got length " + std.length(spaceIndent) + " expected length " + std.length(expected_space);
true
