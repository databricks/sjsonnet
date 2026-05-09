// Regression test for https://github.com/databricks/sjsonnet/issues/829
//
// End-to-end multi-file reproduction of the grafana/mimir failure:
//
//   sjsonnet.Error: Field does not exist: ingest_storage_ingester_autoscaling_enabled
//       at [overrideSuperIfExists].(mimir/rollout-operator.libsonnet:18:64)
//       at [<root>].(mimir/memberlist.libsonnet:106:50)
//
// Faithfully mirrors the structure: three libsonnet mixins merged with `+`,
// where the last mixin's `_config` adds the field that an earlier mixin's
// field references through `super[name] + override`.

local rollout = import 'super_lookup_mimir_rollout.libsonnet';
local memberlist = import 'super_lookup_mimir_memberlist.libsonnet';
local autoscaling = import 'super_lookup_mimir_autoscaling.libsonnet';

local merged = rollout + memberlist + autoscaling;

// Without the fix this throws:
//   Field does not exist: ingest_storage_ingester_autoscaling_enabled
local rt = merged.rollout_operator_replica_template_access_enabled;
assert rt == false : 'rollout enabled flag must evaluate via $._config across mixins';

// memberlist's overrideSuperIfExists must surface the rebuilt statefulset
local sts = merged.alertmanager_statefulset;
assert sts.label == 'autoscaling-disabled' : 'super[name] must rebind $: ' + std.toString(sts);
assert sts.gossip == true : 'override must merge on top of super[name]: ' + std.toString(sts);

true
