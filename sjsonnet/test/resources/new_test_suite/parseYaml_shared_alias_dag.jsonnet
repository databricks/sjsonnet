// Shared (non-cyclic) aliases must keep working after the cycle guard:
// the same anchor referenced twice is a DAG, not a cycle.
std.assertEqual(
  std.parseYaml("a: &x {v: 1}\nb: *x\nc: *x"),
  {a: {v: 1}, b: {v: 1}, c: {v: 1}}
)
