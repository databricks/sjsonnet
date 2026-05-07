// Directional coverage for std.manifestTomlEx table/key ordering and escaping.

local value = {
  z: 1,
  a: 'hello',
  'sp ace': 'v',
  'dot.key': true,
  arr: [1, 'two'],
  section: { b: 2, a: 1 },
  tables: [{ name: 'one' }, { name: 'two' }],
};

local expected =
  'a = "hello"\n' +
  'arr = [\n' +
  '  1,\n' +
  '  "two"\n' +
  ']\n' +
  '"dot.key" = true\n' +
  '"sp ace" = "v"\n' +
  'z = 1\n' +
  '\n' +
  '[section]\n' +
  '  a = 1\n' +
  '  b = 2\n' +
  '\n' +
  '[[tables]]\n' +
  '  name = "one"\n' +
  '\n' +
  '[[tables]]\n' +
  '  name = "two"';

std.assertEqual(std.manifestTomlEx(value, '  '), expected) &&
true
