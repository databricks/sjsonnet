local search(s) = search(s);

{
  method: function(arg) (
    assert std.objectHas(arg, search("foo"));
    arg
  )
}
