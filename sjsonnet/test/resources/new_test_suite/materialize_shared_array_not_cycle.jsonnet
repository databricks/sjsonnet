local shared = [1, [2]];
[
  shared,
  { a: [shared], b: { c: shared } },
  shared,
]
