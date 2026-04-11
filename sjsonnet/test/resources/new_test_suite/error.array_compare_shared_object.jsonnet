// Comparing arrays containing shared non-comparable elements (objects)
// must still raise "Cannot compare object with object" even when both
// sides share the exact same object reference.
local o = {};
[o] < [o]
