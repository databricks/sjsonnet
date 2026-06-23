// Non-finite YAML floats cannot be represented as Jsonnet numbers.
// They are normalized to canonical YAML 1.2 form (lowercase, no + prefix).

std.assertEqual(std.parseYaml(".inf"), ".inf") &&
std.assertEqual(std.parseYaml("+.inf"), ".inf") &&
std.assertEqual(std.parseYaml("-.inf"), "-.inf") &&
std.assertEqual(std.parseYaml(".nan"), ".nan") &&
std.assertEqual(std.parseYaml("+.NaN"), "+.NaN") &&
std.assertEqual(std.parseYaml("-.NaN"), "-.NaN")
