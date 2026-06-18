std.manifestYamlStream([]) == "---\n\n...\n" &&
std.manifestYamlStream([], c_document_end=false) == "---\n\n" &&
std.manifestYamlStream([null]) == "---\nnull\n...\n" &&
std.manifestYamlStream([1, 2]) == "---\n1\n---\n2\n...\n" &&
true
