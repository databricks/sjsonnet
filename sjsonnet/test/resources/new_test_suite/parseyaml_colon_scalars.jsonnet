// YAML scalars containing colons should not be converted as YAML 1.1 sexagesimal numbers.

std.assertEqual(std.parseYaml("1:20"), "1:20") &&
std.assertEqual(std.parseYaml("1:2:3"), "1:2:3") &&
std.assertEqual(std.parseYaml("1:20.5"), "1:20.5") &&
std.assertEqual(std.parseYaml("10:00"), "10:00")
