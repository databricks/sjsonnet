local o = std.objectRemoveKey({foo: 1, bar:: 2, baz: 3}, "foo");
{
    fields: std.objectFields(o),
    all_fields: std.objectFieldsAll(o),
    bar: o.bar,
    object: o,
}
