std.manifestToml({ obj: { a: 1, b: 2 } }) == "\n\n[obj]\n  a = 1\n  b = 2" &&
std.manifestToml({ x: 1, y: 2 }) == "x = 1\ny = 2" &&
true
