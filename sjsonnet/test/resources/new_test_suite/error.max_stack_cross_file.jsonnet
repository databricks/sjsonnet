local b = import "error.max_stack_cross_file_helper.libsonnet";

local m = function() (
  local foo = { bar: "hi" };
  local bar = b.method(foo);
  std.toString(foo) + " " + bar
);

m()
