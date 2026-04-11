// Comparing arrays containing a shared nested array with an error element
// must still propagate the error during recursive comparison, even when
// both sides share the exact same array reference.
local a = [error "boom"];
[a] < [a]
