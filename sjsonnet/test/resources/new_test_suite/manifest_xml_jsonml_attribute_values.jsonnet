std.manifestXmlJsonml(["div", { data: { x: 1 }, items: [1, 2, 3], title: 'a"b<c>d&e' }]) ==
'<div data="{&quot;x&quot;:1}" items="[1,2,3]" title="a&quot;b&lt;c&gt;d&amp;e"></div>' &&
std.manifestXmlJsonml(["ns:tag", { "data-id": "x" }, ["child.node", { "_ok": true }]]) ==
'<ns:tag data-id="x"><child.node _ok="true"></child.node></ns:tag>' &&
std.manifestXmlJsonml(["emoji", { value: std.char(128512) }, std.char(128512)]) ==
'<emoji value="' + std.char(128512) + '">' + std.char(128512) + '</emoji>'
