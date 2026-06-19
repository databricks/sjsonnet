// Test that std.length on functions counts only required (non-default) parameters
{
  no_params: std.length(function() 42),
  one_required: std.length(function(a) a),
  two_required: std.length(function(a, b) a + b),
  one_req_one_default: std.length(function(a, b=1) a + b),
  one_req_two_defaults: std.length(function(a, b=1, c=2) a + b + c),
  all_defaults: std.length(function(a=1, b=2) a + b),
  builtin_sort: std.length(std.sort),
  builtin_map: std.length(std.map),
  builtin_format: std.length(std.format),
}
