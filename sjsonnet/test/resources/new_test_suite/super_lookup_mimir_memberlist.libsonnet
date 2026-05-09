// Mimics grafana/mimir's memberlist.libsonnet — uses the
// `super[name] + override` pattern through the local helper. This is the
// site that triggered issue #829.
{
  // Verbatim pattern from operations/mimir/memberlist.libsonnet:171.
  local overrideSuperIfExists(name, override) =
    if !(name in super) || super[name] == null || super[name] == {} then null
    else super[name] + override,

  alertmanager_statefulset:
    overrideSuperIfExists('alertmanager_statefulset', { gossip: true }),
}
