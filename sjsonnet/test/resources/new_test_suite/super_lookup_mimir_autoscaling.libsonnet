// Mimics grafana/mimir's ingest-storage-ingester-autoscaling.libsonnet —
// adds the `_config` field that the rollout mixin's fields reference via $.
// The whole point of the bug: this mixin is applied LAST, so super[name]
// inside memberlist must still see this field through $._config.
{
  _config+:: {
    ingest_storage_ingester_autoscaling_enabled: false,
  },
}
