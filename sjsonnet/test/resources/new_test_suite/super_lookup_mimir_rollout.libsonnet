// Mimics grafana/mimir's rollout-operator.libsonnet — defines a field that
// references _config keys defined in a later mixin.
{
  _config+:: {
    multi_zone_ingester_enabled: false,
    multi_zone_store_gateway_enabled: false,
    cortex_compactor_concurrent_rollout_enabled: false,
  },

  // Line equivalent to rollout-operator.libsonnet:18 — references
  // ingest_storage_ingester_autoscaling_enabled which is added by the
  // autoscaling mixin appended LATER in the merge chain.
  rollout_operator_replica_template_access_enabled:
    $._config.ingest_storage_ingester_autoscaling_enabled,

  alertmanager_statefulset: {
    label:
      if $._config.ingest_storage_ingester_autoscaling_enabled
      then 'autoscaling-enabled'
      else 'autoscaling-disabled',
  },
}
