// Diamond sharing plus nested alias reuse: the same anchor referenced multiple
// times at different positions (including twice within one sequence) is a DAG,
// not a cycle. The cycle guard must not reject it, and each reference converts
// to an equal value.
local dag = std.parseYaml("a: &x {v: 1}\nb: *x\nc: *x\nd: [*x, *x]\ninner: &in {w: 2}\ne: {p: *in, q: *in}");
std.assertEqual(dag, {
  a: {v: 1}, b: {v: 1}, c: {v: 1},
  d: [{v: 1}, {v: 1}],
  inner: {w: 2},
  e: {p: {w: 2}, q: {w: 2}},
})
