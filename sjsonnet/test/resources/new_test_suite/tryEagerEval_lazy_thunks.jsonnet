// Test that tryEagerEval preserves lazy semantics: unused local bindings with side effects
// should not be forced. This matches go-jsonnet and jrsonnet behavior.
std.assertEqual(
  (local a = error "should not be evaluated"; local b = a + 1; if false then b else 0),
  0
)
