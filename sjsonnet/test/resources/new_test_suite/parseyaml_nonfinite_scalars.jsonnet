// Non-finite YAML floats cannot be represented as Jsonnet numbers.

std.assertEqual(std.parseYaml(".inf"), ".inf") &&
std.assertEqual(std.parseYaml("+.inf"), "+.inf") &&
std.assertEqual(std.parseYaml("-.inf"), "-.inf") &&
std.assertEqual(std.parseYaml(".nan"), ".nan")
