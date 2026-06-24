// Regression test: TOML output must pass through Unicode natively.
// Golden verified against cpp-jsonnet 0.21.0, go-jsonnet 0.22.0, jrsonnet 0.5.0-pre99.
std.assertEqual(
  std.manifestToml({name: "世界", drink: "café", emoji: "🌍"}),
  "drink = \"café\"\nemoji = \"🌍\"\nname = \"世界\""
) &&
std.assertEqual(
  std.manifestToml({section: {key: "日本語"}}),
  "\n\n[section]\n  key = \"日本語\""
) &&
true
