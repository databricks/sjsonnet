// Regression test for https://github.com/databricks/sjsonnet/issues/829
//
// `super[name]` (computed-key super lookup) must bind `$`/`self` of the
// retrieved field to the *outer* self (the merge result), NOT the parent.
// Otherwise fields defined in mixins applied AFTER the parent become
// unreachable from inside fields retrieved through `super[name]`.
//
// This mirrors the semantics of `super.name` (static-key super lookup) and
// matches behavior of google/jsonnet (C++), go-jsonnet, and jrsonnet.
//
// The bug surfaced via grafana/mimir's `overrideSuperIfExists(name, override)`
// helper which uses `super[name] + override` to layer mixins.

// ============================================================
// SECTION 1: Static vs dynamic super key parity for $ binding
// ============================================================

// 1.1 Computed-key super lookup must see fields from later mixins via $
local a1 = {
  _config+:: { a_enabled: false },
  field: { flag: $._config.b_enabled },
};
local b1 = {
  _config+:: { b_enabled: false },
  field: super['field'] + { extra: 1 },
};
local r1 = (a1 + b1).field;
assert r1 == { flag: false, extra: 1 } : 'super[name] must preserve $: ' + std.toString(r1);

// 1.2 Static-key super lookup baseline (was already correct in sjsonnet)
local a2 = {
  _config+:: { a_enabled: false },
  field: { flag: $._config.b_enabled },
};
local b2 = {
  _config+:: { b_enabled: false },
  field: super.field + { extra: 1 },
};
local r2 = (a2 + b2).field;
assert r2 == r1 : 'super.name and super[name] must agree: ' + std.toString(r2);

// ============================================================
// SECTION 2: The grafana/mimir overrideSuperIfExists pattern
// ============================================================

local mixinA = {
  _config+:: { a_only: 'a' },
  // Field references _config field that only exists after merging mixinB.
  alertmanager_statefulset: {
    label: $._config.b_only,
  },
};
local mixinB = {
  _config+:: { b_only: 'b' },
  local overrideSuperIfExists(name, override) =
    if !(name in super) || super[name] == null || super[name] == {} then null
    else super[name] + override,
  alertmanager_statefulset: overrideSuperIfExists(
    'alertmanager_statefulset',
    { extra: 'mixed-in' },
  ),
};
local merged = mixinA + mixinB;
assert merged.alertmanager_statefulset == { label: 'b', extra: 'mixed-in' }
       : 'overrideSuperIfExists must see merged $._config: '
         + std.toString(merged.alertmanager_statefulset);

// ============================================================
// SECTION 3: Edge cases
// ============================================================

// 3.1 super[name] with no override (just rebinding) preserves $
local n1 = { _config+:: { x: 1 }, value: $._config.x };
local n2 = { value: super['value'] };
local r5 = (n1 + n2).value;
assert r5 == 1 : 'bare super[name] must preserve binding';

// 3.2 Chained mixins: 3 levels deep, $ must reach the outermost
local l1 = { _config+:: { l1: 1 }, item: { sum: $._config.l1 + $._config.l2 + $._config.l3 } };
local l2 = { _config+:: { l2: 2 }, item: super['item'] + { mid: true } };
local l3 = { _config+:: { l3: 3 }, item: super['item'] + { top: true } };
local r6 = (l1 + l2 + l3).item;
assert r6 == { sum: 6, mid: true, top: true }
       : 'chained super[name] across 3 mixins: ' + std.toString(r6);

// 3.3 Computed key from a variable (not a literal string)
local k = 'item';
local v1 = { _config+:: { x: 'hello' }, item: { val: $._config.x } };
local v2 = { item: super[k] + { wrapped: true } };
local r7 = (v1 + v2).item;
assert r7 == { val: 'hello', wrapped: true }
       : 'super[var-key] must preserve binding: ' + std.toString(r7);

// 3.4 Object-extension syntax {a + b} also exercises super lookup
local oa = { _config+:: { y: 'yes' }, label: $._config.y };
local ob = oa { label: super['label'] };
assert ob.label == 'yes' : 'super[name] in object-extension must preserve binding';

true
