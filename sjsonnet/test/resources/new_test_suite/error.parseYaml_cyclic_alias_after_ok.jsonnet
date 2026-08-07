// A cycle appearing only after several successful conversions (including a
// shared alias) must still be reported as a cycle: the in-progress set must
// neither false-positive on the earlier nodes nor miss the later cycle.
std.parseYaml("ok1: {v: 1}\nok2: [1, 2, 3]\nshared: &s {a: 1}\nreuse: *s\nbad: &x [*x]")
