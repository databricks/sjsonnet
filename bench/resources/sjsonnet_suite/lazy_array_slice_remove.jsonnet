local n = 500000;
local source = std.range(0, n - 1);
local sliced = source[123:n - 123:2];
local trimmed = std.removeAt(sliced, 17);
local trimmedLen = std.length(trimmed);
local probes = std.range(0, 19999);
{
  len: trimmedLen,
  first: trimmed[0],
  mid: trimmed[std.floor(trimmedLen / 2)],
  last: trimmed[trimmedLen - 1],
  checksum: std.foldl(
    function(acc, i) acc + trimmed[(i * 7919) % trimmedLen],
    probes,
    0
  ),
}
