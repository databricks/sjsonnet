local neg0 = -0.0;
local pos0 = 0;

// std.sort: -0.0 and 0.0 should be treated as equal
assert std.sort([neg0, pos0]) == [pos0, neg0] || std.sort([neg0, pos0]) == [neg0, pos0] : 'sort([-0, 0]) should not error';
assert std.length(std.sort([neg0, pos0, 1, -1])) == 4 : 'sort preserves all elements';

// std.set: -0.0 and 0.0 should be deduplicated
assert std.length(std.set([neg0, pos0])) == 1 : 'set([-0, 0]) should deduplicate to 1 element';
assert std.set([neg0, pos0])[0] == 0 : 'set result should equal 0';

// std.uniq: -0.0 and 0.0 should be deduplicated (requires sorted input)
assert std.length(std.uniq(std.sort([neg0, pos0]))) == 1 : 'uniq(sort([-0, 0])) should deduplicate to 1 element';

// std.setUnion: -0.0 and 0.0 merged
assert std.length(std.setUnion([neg0], [pos0])) == 1 : 'setUnion([-0], [0]) should have 1 element';

// std.setInter: -0.0 and 0.0 intersect
assert std.length(std.setInter([neg0], [pos0])) == 1 : 'setInter([-0], [0]) should have 1 element';

// std.setDiff: -0.0 and 0.0 cancel
assert std.length(std.setDiff([neg0], [pos0])) == 0 : 'setDiff([-0], [0]) should be empty';

// std.setMember: -0.0 is member of [0]
assert std.setMember(neg0, [pos0]) == true : '-0.0 should be member of [0]';
assert std.setMember(pos0, [neg0]) == true : '0 should be member of [-0.0]';

true
