local o1 = {
  assert self.x : 'x',
  assert super.y : 'y',
  b: super.a,
};
{ a: 'begin' } + std.objectRemoveKey({ y: true } + o1 + { a: 'end', x: true }, 'y')
