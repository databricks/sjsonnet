std.maxArray([], onEmpty="was" + " empty") == "was empty" &&
std.minArray([], onEmpty="was" + " empty") == "was empty" &&
std.maxArray([1], onEmpty=error "should not be evaluated") == 1 &&
std.minArray([1], onEmpty=error "should not be evaluated") == 1 &&
std.maxArray([1, 2, 3]) == 3 &&
std.minArray([1, 2, 3]) == 1 &&
true
