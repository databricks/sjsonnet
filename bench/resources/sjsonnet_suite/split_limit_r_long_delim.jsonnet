local s = std.substr(importstr 'split_limit_r_long_delim.txt', 0, 872);
local delim = 'aaaaaaaaab';

std.sum(std.makeArray(5000, function(_) std.length(std.splitLimitR(s, delim, 1)[0])))
