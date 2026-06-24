local inner() = std.map(
  function(y) std.trace('comp-no-hoist ' + std.toString(y), y),
  [1]
);
local arr = [x + y for x in [10, 20] for y in inner()];

std.assertEqual([arr[0], arr[1]], [11, 21]) &&
true
