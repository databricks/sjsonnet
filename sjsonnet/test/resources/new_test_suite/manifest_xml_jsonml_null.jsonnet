std.manifestXmlJsonml(["tag", { a: null, b: true, c: false, d: 1.5, e: "string" }]) ==
  '<tag a="null" b="true" c="false" d="1.5" e="string"></tag>' &&
std.manifestXmlJsonml(["a", ["b", ["c"]]]) == '<a><b><c></c></b></a>' &&
true
