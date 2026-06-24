// Regression test: YAML output must pass through Unicode natively.
// Golden verified against cpp-jsonnet 0.21.0, go-jsonnet 0.22.0, jrsonnet 0.5.0-pre99.
std.assertEqual(std.manifestYamlDoc("世界"), "\"世界\"") &&
std.assertEqual(std.manifestYamlDoc("café"), "\"café\"") &&
std.assertEqual(std.manifestYamlDoc({name: "世界", drink: "café"}),
  "\"drink\": \"café\"\n\"name\": \"世界\"") &&
std.assertEqual(std.manifestYamlDoc(["🌍", "世界", "café"]),
  "- \"🌍\"\n- \"世界\"\n- \"café\"") &&
true
