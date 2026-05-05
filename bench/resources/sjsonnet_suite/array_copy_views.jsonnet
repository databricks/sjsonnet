local block = std.range(0, 4095);
local pair = block + block;
local mixed = [
  std.repeat(block, 64),
  pair,
  block[100:4000:2],
  std.repeat(pair, 16),
];
local flattened = std.flattenArrays(mixed);
local joined = std.join([], mixed);
local len = std.length(flattened);
local probes = std.range(0, 39999);
{
  len: len,
  joinedLen: std.length(joined),
  first: flattened[0],
  middle: flattened[std.floor(len / 2)],
  last: flattened[len - 1],
  checksum: std.foldl(
    function(acc, i) acc + flattened[(i * 7919) % len],
    probes,
    0
  ),
}
