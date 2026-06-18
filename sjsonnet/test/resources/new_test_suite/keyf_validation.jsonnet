std.sort([3,1,2]) == [1,2,3] &&
std.sort([3,1,2], keyF=function(x) -x) == [3,2,1] &&
std.set([1,2,1]) == [1,2] &&
std.uniq([1,1,2]) == [1,2] &&
true
