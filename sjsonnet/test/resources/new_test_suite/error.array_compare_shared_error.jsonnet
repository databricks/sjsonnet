// When comparing arrays with a shared lazy element that errors,
// the error must still be raised even when both sides share the reference.
local base = std.makeArray(3, function(i) if i == 1 then error "boom" else i);
base + [0] < base + [1]
