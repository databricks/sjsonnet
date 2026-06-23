// std.parseYaml should normalize YAML 1.2 non-finite floats to canonical form.
// Plain signed NaN is not a YAML 1.2 Core Schema float and remains a string.
assert std.parseYaml(".inf") == ".inf";
assert std.parseYaml(".Inf") == ".inf";
assert std.parseYaml(".INF") == ".inf";
assert std.parseYaml("+.inf") == ".inf";
assert std.parseYaml("+.Inf") == ".inf";
assert std.parseYaml("+.INF") == ".inf";
assert std.parseYaml("-.inf") == "-.inf";
assert std.parseYaml("-.Inf") == "-.inf";
assert std.parseYaml("-.INF") == "-.inf";
assert std.parseYaml(".nan") == ".nan";
assert std.parseYaml(".NaN") == ".nan";
assert std.parseYaml(".NAN") == ".nan";
assert std.parseYaml("+.nan") == "+.nan";
assert std.parseYaml("+.NaN") == "+.NaN";
assert std.parseYaml("+.NAN") == "+.NAN";
assert std.parseYaml("-.nan") == "-.nan";
assert std.parseYaml("-.NaN") == "-.NaN";
assert std.parseYaml("-.NAN") == "-.NAN";
assert std.parseYaml("'+.NaN'") == "+.NaN";
assert std.parseYaml("'-.NaN'") == "-.NaN";
assert std.parseYaml("|-\n  +.NaN") == "+.NaN";
assert std.parseYaml(">-\n  +.NaN") == "+.NaN";
assert std.parseYaml("!!str .NaN") == ".NaN";
assert std.parseYaml("!!str +.NaN") == "+.NaN";
assert std.parseYaml("!!str -.NaN") == "-.NaN";
assert std.parseYaml("! +.NaN") == "+.NaN";
assert std.parseYaml("%TAG !e! tag:yaml.org,2002:\n--- !e!str +.NaN") == "+.NaN";
assert std.parseYaml("!!str\n+.NaN") == "+.NaN";
assert std.parseYaml("- !!str\n  +.NaN\n- +.NaN") == ["+.NaN", "+.NaN"];
assert std.parseYaml("%TAG !e! tag:yaml.org,2002:\n---\n- !e!str\n  +.NaN\n- +.NaN") == [
  "+.NaN",
  "+.NaN",
];
assert std.parseYaml("!!str &x\n+.NaN") == "+.NaN";
assert std.parseYaml("!!str\n# c\n+.NaN") == "+.NaN";
assert std.parseYaml("a: !!str\n  # c\n  +.NaN").a == "+.NaN";
assert std.parseYaml("!!str a:\n  # c\n  +.NaN").a == "+.NaN";
assert std.parseYaml("# ! not a tag\n+.NaN") == "+.NaN";
assert std.parseYaml("a: # ! not a tag\n  +.NaN").a == "+.NaN";
assert std.parseYaml("# comment: ! not a tag\n+.NaN") == "+.NaN";
assert std.parseYaml("a: # comment: ! not a tag\n  +.NaN").a == "+.NaN";
assert std.parseYaml("!!str # comment: not syntax\n+.NaN") == "+.NaN";
assert std.parseYaml("a: !!str # comment: not syntax\n  +.NaN").a == "+.NaN";
assert std.parseYaml("a:\n  +.NaN").a == "+.NaN";
assert std.parseYaml("a: !!str\n  +.NaN").a == "+.NaN";
assert std.parseYaml("!!str a:\n  +.NaN").a == "+.NaN";
local quotedKeyObj = std.parseYaml('"a: !":\n  +.NaN');
assert quotedKeyObj["a: !"] == "+.NaN";
local quotedTaggedValueObj = std.parseYaml('"a: !": !!str\n  +.NaN');
assert quotedTaggedValueObj["a: !"] == "+.NaN";
assert std.parseYaml('!!str a": +.NaN')['a"'] == "+.NaN";
assert std.parseYaml('!!str a":\n  +.NaN')['a"'] == "+.NaN";
assert std.parseYaml('!!str "a #": +.NaN')["a #"] == "+.NaN";
assert std.parseYaml('!!str "a #":\n  +.NaN')["a #"] == "+.NaN";
assert std.parseYaml("!!float \"+.NaN\"") == ".nan";
assert std.parseYaml("!!float \"-.NaN\"") == ".nan";
assert std.parseYaml("!!float +.NaN") == ".nan";
assert std.parseYaml("%TAG !e! tag:yaml.org,2002:\n--- !e!float +.NaN") == ".nan";
// Non-finite in object context
local obj = std.parseYaml("a: .Inf\nb: .NaN\nc: -.INF");
assert obj.a == ".inf";
assert obj.b == ".nan";
assert obj.c == "-.inf";
local keyObj = std.parseYaml(".Inf: 1\n-.INF: 2\n+.NaN: 3");
assert keyObj[".inf"] == 1;
assert keyObj["-.inf"] == 2;
assert keyObj["+.NaN"] == 3;
assert !std.objectHas(keyObj, ".Inf");
assert !std.objectHas(keyObj, ".nan");
local explicitFloatKeyObj = std.parseYaml("!!float \"+.NaN\": 1\n!!float \"-.Inf\": 2");
assert explicitFloatKeyObj[".nan"] == 1;
assert explicitFloatKeyObj["-.inf"] == 2;
assert std.parseYaml("!!str +.NaN: 1")["+.NaN"] == 1;
assert std.parseYaml("! -.NaN: 1")["-.NaN"] == 1;
assert std.parseYaml("a: |-\n  +.NaN").a == "+.NaN";
// Explicit string tags on nearby flow scalars must not apply to the current scalar.
local flowMap = std.parseYaml("{!!str a: +.NaN, b: !!str +.NaN, c: +.NaN}");
assert flowMap.a == "+.NaN";
assert flowMap.b == "+.NaN";
assert flowMap.c == "+.NaN";
local flowKeyMap = std.parseYaml("{.Inf: 1, !!str +.NaN: 2, !!float \"-.INF\": 3}");
assert flowKeyMap[".inf"] == 1;
assert flowKeyMap["+.NaN"] == 2;
assert flowKeyMap["-.inf"] == 3;
true
