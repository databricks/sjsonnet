// Test that explicit --- document start markers cause single-doc YAML
// to be wrapped in an array, matching go-jsonnet behavior.
std.assertEqual(std.parseYaml("---"), [null]) &&
std.assertEqual(std.parseYaml("---\n"), [null]) &&
std.assertEqual(std.parseYaml("---\na: 1"), [{a: 1}]) &&
std.assertEqual(std.parseYaml("--- 3\n"), [3]) &&
std.assertEqual(std.parseYaml("---a: 1"), {"---a": 1}) &&
std.assertEqual(std.parseYaml("a: 1"), {a: 1}) &&
true
