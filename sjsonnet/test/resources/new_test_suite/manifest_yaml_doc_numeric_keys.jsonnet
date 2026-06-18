local doc = std.manifestYamlDoc({ object: { "1.2e3": "value" } }, quote_keys=false);
std.startsWith(doc, "object:\n  \"1.2e3\":") ||
std.startsWith(doc, "\"object\":\n  \"1.2e3\":")
