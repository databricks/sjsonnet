// std.removeAt with valid index works correctly
assert std.removeAt([1, 2, 3], 1) == [1, 3] : "remove middle element";
assert std.removeAt([1, 2, 3], 0) == [2, 3] : "remove first element";
assert std.removeAt([1, 2, 3], 2) == [1, 2] : "remove last element";
true
