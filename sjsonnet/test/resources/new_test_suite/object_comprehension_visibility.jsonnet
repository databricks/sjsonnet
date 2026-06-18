// Object comprehension visibility modifiers: :: (hidden), ::: (forced), : (normal)
// :: (hidden) - invisible to objectFields, visible to objectFieldsAll
assert std.objectFields({[k]:: 1 for k in ["a", "b"]}) == [] : "hidden fields invisible to objectFields";
assert std.objectFieldsAll({[k]:: 1 for k in ["a", "b"]}) == ["a", "b"] : "hidden fields visible to objectFieldsAll";
// ::: (forced) - always visible
assert std.objectFields({[k]::: 1 for k in ["a", "b"]}) == ["a", "b"] : "forced visibility";
// : (normal) - standard visibility
assert std.objectFields({[k]: 1 for k in ["a", "b"]}) == ["a", "b"] : "normal visibility";
true
