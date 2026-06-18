// Test YAML 1.2 modern octal syntax (0o prefix) for unquoted scalars.
// Quoted values must remain strings. Legacy octal (0 prefix) still works.
local yaml = std.parseYaml(|||
  a: 0777
  b: 0o777
  c: 0
  d: 0o10
  e: -0o777
  f: "0o777"
  g: '0o777'
|||);

std.assertEqual(yaml.a, 511) &&
std.assertEqual(yaml.b, 511) &&
std.assertEqual(yaml.c, 0) &&
std.assertEqual(yaml.d, 8) &&
std.assertEqual(yaml.e, -511) &&
std.assertEqual(yaml.f, "0o777") &&
std.assertEqual(yaml.g, "0o777") &&
true
