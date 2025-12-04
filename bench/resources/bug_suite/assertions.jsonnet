local Obj = {
  assert std.set(std.objectFields(self)) == std.set(["display_name", "path", "relation"]),
  display_name: null,
  path: {
    assert std.set(std.objectFields(self)) == std.set(["cloud_provider", "cloud_provider_region", "environment"]),
    cloud_provider: null,
    cloud_provider_region: null,
    environment: null,
  },
  relation: {
    assert std.set(std.objectFields(self)) == std.set(["cloud_uri", "environment_uri", "kubernetes_cluster_type_uri", "region_uri", "regulatory_domain_uri"]),
    cloud_uri: null,
    environment_uri: null,
    kubernetes_cluster_type_uri: null,
    region_uri: null,
    regulatory_domain_uri: null,
  }
};

[
  Obj {
    assert true,
    display_name: "KubernetesCluster %d" % i,
    path+: {
      assert true,
      cloud_provider: "AWS",
      cloud_provider_region: "AWS_US_EAST_1",
      environment: "DEV",
    },
    relation+: {
      assert true,
      cloud_uri: "https://aws.com/cluster/%d" % i,
      environment_uri: "https://dev.com/environment/%d" % i,
      kubernetes_cluster_type_uri: "https://kubernetes.com/cluster-type/%d" % i,
      region_uri: "https://us-east-1.com/region/%d" % i,
      regulatory_domain_uri: "https://regulatory.com/domain/%d" % i,
    },
  }
  for i in std.range(0, 50)
]