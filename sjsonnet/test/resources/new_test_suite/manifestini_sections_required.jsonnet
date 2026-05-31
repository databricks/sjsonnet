// Issue #799: std.manifestIni follows official Jsonnet — `sections` is required (a missing
// `sections` field is an error, see error.manifestini_requires_sections), while `main` is optional.
std.assertEqual(
  std.manifestIni({ main: { a: '1', b: '2' }, sections: { s: { b: '2' } } }),
  'a = 1\nb = 2\n[s]\nb = 2\n'
) &&
std.assertEqual(
  std.manifestIni({ sections: { s: { b: '2' } } }),
  '[s]\nb = 2\n'
) &&
std.assertEqual(
  std.manifestIni({ sections: {} }),
  ''
) &&
true
