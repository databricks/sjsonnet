// Regression test: std.parseYaml block scalar chomping (literal | and folded >).
// Clip chomping (default) must preserve exactly one trailing newline per YAML spec.
// See: https://yaml.org/spec/1.2.2/#8112-block-chomping-indicator

// Literal block scalar — clip (default): one trailing newline
std.assertEqual(std.parseYaml("a: |\n  line1\n  line2").a, "line1\nline2\n") &&
std.assertEqual(std.length(std.parseYaml("a: |\n  line1\n  line2").a), 12) &&

// Literal block scalar — keep (+): preserve all trailing newlines
std.assertEqual(std.parseYaml("a: |+\n  line1\n  line2\n\n").a, "line1\nline2\n\n") &&
std.assertEqual(std.length(std.parseYaml("a: |+\n  line1\n  line2\n\n").a), 13) &&

// Literal block scalar — strip (-): remove all trailing newlines
std.assertEqual(std.parseYaml("a: |-\n  line1\n  line2\n").a, "line1\nline2") &&
std.assertEqual(std.length(std.parseYaml("a: |-\n  line1\n  line2\n").a), 11) &&

// Folded block scalar — clip (default): one trailing newline
std.assertEqual(std.parseYaml("a: >\n  line1\n  line2").a, "line1 line2\n") &&
std.assertEqual(std.length(std.parseYaml("a: >\n  line1\n  line2").a), 12) &&

// Folded block scalar — keep (+): preserve all trailing newlines
std.assertEqual(std.parseYaml("a: >+\n  line1\n  line2\n\n").a, "line1 line2\n\n") &&
std.assertEqual(std.length(std.parseYaml("a: >+\n  line1\n  line2\n\n").a), 13) &&

// Folded block scalar — strip (-): remove all trailing newlines
std.assertEqual(std.parseYaml("a: >-\n  line1\n  line2\n").a, "line1 line2") &&
std.assertEqual(std.length(std.parseYaml("a: >-\n  line1\n  line2\n").a), 11) &&

// Literal with explicit indentation indicator and clip chomping
std.assertEqual(std.parseYaml("a: |2\n  line1\n  line2").a, "line1\nline2\n") &&

// Header comments must not be interpreted as chomping indicators
std.assertEqual(std.parseYaml("a: | # comment +\n  line1\n  line2").a, "line1\nline2\n") &&
std.assertEqual(std.parseYaml("a: > # comment -\n  line1\n  line2").a, "line1 line2\n") &&
std.assertEqual(std.parseYaml("a: |+ # comment -\n  line1\n  line2\n\n").a, "line1\nline2\n\n") &&
std.assertEqual(std.parseYaml("a: |- # comment +\n  line1\n  line2\n").a, "line1\nline2") &&

// Literal with indentation indicator and strip chomping
std.assertEqual(std.parseYaml("a: |-2\n  line1\n  line2\n").a, "line1\nline2") &&

// Literal with indentation indicator and keep chomping
std.assertEqual(std.parseYaml("a: |+2\n  line1\n  line2\n\n").a, "line1\nline2\n\n") &&

// Multi-line literal with blank lines in the middle
std.assertEqual(
  std.parseYaml("a: |\n  line1\n\n  line2").a,
  "line1\n\nline2\n"
) &&

// Folded scalar with blank lines (preserves paragraph breaks)
std.assertEqual(
  std.parseYaml("a: >\n  line1\n\n  line2").a,
  "line1\nline2\n"
) &&

true
