// Cyclic YAML aliases must fail with a clean error instead of overflowing
// the stack (previously: java.lang.StackOverflowError crashed the process).
std.parseYaml("a: &x [*x]")
