// Cyclic alias through a mapping: the anchor value contains itself.
std.parseYaml("a: &x {b: *x}")
