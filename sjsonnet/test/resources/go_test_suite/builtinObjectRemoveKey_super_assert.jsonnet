local o1 = {
  assert self.x : 'x',
  assert super.y : 'y',
  b: super.a,
};
local o2 = {
  x: true,
  y: true,
  a: 'one',
};
{ a: 'begin' } + std.objectRemoveKey({ y: std.isString('yyy') } + o1 + o2, 'x')
