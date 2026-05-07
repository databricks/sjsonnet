// `error value` is a tail position, but it is not automatically a non-recursive
// exit. A function whose only path is `error f()` must not be auto-TCO'd,
// otherwise the trampoline loops forever instead of reporting max stack.
local f() = error f();

f()
