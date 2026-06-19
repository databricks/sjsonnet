// Plain YAML scalars that look like invalid octal should still parse as decimal numbers.

std.assertEqual(std.parseYaml("08"), 8) &&
std.assertEqual(std.parseYaml("09"), 9) &&
std.assertEqual(std.parseYaml("008"), 8) &&
std.assertEqual(std.parseYaml("0009"), 9) &&
std.assertEqual(std.parseYaml("+08"), 8) &&
std.assertEqual(std.parseYaml("-09"), -9) &&
std.assertEqual(std.parseYaml('"08"'), "08") &&
std.assertEqual(std.parseYaml('"09"'), "09")
