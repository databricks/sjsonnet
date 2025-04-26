package sjsonnet

import utest._
import TestUtils.eval
object StdMergePatchTests extends TestSuite {

  // These test cases' behavior matches v0.20.0 of google/jsonnet and google/go-jsonnet.

  def tests: Tests = Tests {

    test("top-level merging of non-objects") {
      // Both target and patch are non-objects, so patch wins:
      eval("std.mergePatch([{a: 1}], [{b: 2}])") ==> ujson.Arr(ujson.Obj("b" -> 2))
      // Target is an object, patch is an array, so patch wins:
      eval("std.mergePatch({a: 1}, [{b: 2}])") ==> ujson.Arr(ujson.Obj("b" -> 2))
      // Target is an array, patch is an object, so patch wins:
      eval("std.mergePatch([{a: 1}], {b: 2})") ==> ujson.Obj("b" -> 2)
    }

    test("top-level nulls") {
      // Target is null, so patch wins:
      eval("std.mergePatch(null, {a: 1})") ==> ujson.Obj("a" -> 1)
      // A null patch always produces a null result:
      eval("std.mergePatch({a: 1}, null)") ==> ujson.Null
    }

    test("basic merges") {
      // Basic non-conflicting merge of top-level fields:
      eval("std.mergePatch({a: 1}, {b: 2})") ==> ujson.Obj("a" -> 1, "b" -> 2)
      // Basic conflicting merge of top-level fields (patch wins)
      eval("std.mergePatch({a: 1}, {a: 2})") ==> ujson.Obj("a" -> 2)
      // Basic recursive non-conflicting merging:
      eval("std.mergePatch({a: {b: 1}}, {a: {c: 2}})") ==> ujson.Obj("a" -> ujson.Obj("b" -> 1, "c" -> 2))
      // Basic recursive conflicting merging (patch wins):
      eval("std.mergePatch({a: {b: 1, c: 1}}, {a: {b: 2}})") ==> ujson.Obj("a" -> ujson.Obj("b" -> 2, "c" -> 1))
    }

    test("target field order preservation") {
      eval("std.mergePatch({b: 1, a: 1}, {a: 2, b: 2})", preserveOrder = true).toString ==> """{"b":2,"a":2}"""
    }

    test("null fields") {
      // Nested nulls in patch can remove nested fields from target
      eval("std.mergePatch({a: {b: 1, c: 1}}, {a: {b: null}})") ==> ujson.Obj("a" -> ujson.Obj("c" -> 1))
      // Nested nulls in the target are preserved:
      eval("std.mergePatch({a: null}, {b: 2})") ==> ujson.Obj("a" -> ujson.Null, "b" -> 2)
    }

    // Regarding hidden field behavior in other implementations, see also:
    // https://github.com/google/jsonnet/issues/219
    // https://github.com/google/jsonnet/issues/1041

    test("hidden target fields") {
      // Hidden target fields are dropped in the output, even if nothing merges with them:
      eval("std.objectFieldsAll({hidden:: 1, visible: 1})") ==> ujson.Arr("hidden", "visible")
      eval("std.objectFieldsAll(std.mergePatch({hidden:: 1, visible: 1}, {}))") ==> ujson.Arr("visible")
      eval("std.objectFieldsAll(std.mergePatch({hidden:: 1, visible: 1}, {added: 1}))") ==> ujson.Arr("added", "visible")
      // But hidden nested target fields are preserved as hidden if nothing merges with them:
      eval("std.objectFields(std.mergePatch({a: {h:: 1, v: 1}}, {}).a)") ==> ujson.Arr("v")
      eval("std.objectFieldsAll(std.mergePatch({a: {h:: 1, v: 1}}, {}).a)") ==> ujson.Arr("h", "v")
      eval("std.mergePatch({a: {h:: 1, v: 1}}, {}).a.h") ==> ujson.Num(1)
      // Those hidden nested fields are still dropped if something merges with their enclosing object:
      eval("std.objectFieldsAll(std.mergePatch({a: {h:: 1, v: 1}}, {a: {}}).a)") ==> ujson.Arr("v")
      // Hidden target fields are ineligible to merge with visible patch fields;
      // it should be as if the hidden target field doesn't exist:
      eval("std.mergePatch({ a:: { a: 1 } , visible: 1 }, { a: { b: 2 }})") ==> ujson.Obj("visible" -> 1, "a" -> ujson.Obj("b" -> 2))
    }

    test("hidden patch fields") {
      // Hidden patch fields are dropped in the output, even if nothing merges with them:
      eval("std.objectFieldsAll(std.mergePatch({visible: 1}, {hidden:: 2}))") ==> ujson.Arr("visible")
      // Hidden patch fields are ineligible to merge with visible target fields;
      // it should be as if the hidden patch field doesn't exist:
      eval("std.mergePatch({ a: 1 }, { a:: 2})") ==> ujson.Obj("a" -> 1)
      // Nesting:
      eval("std.mergePatch({ a: { b: 1 } }, { a:: { c: 1 }})") ==> ujson.Obj("a" -> ujson.Obj("b" -> 1))
      // Make sure the nested hidden patch field is indeed absent, not just hidden:
      eval("std.objectFieldsAll(std.mergePatch({ a: { b: 1 } }, { a:: { c: 1 }}).a)") ==> ujson.Arr("b")
    }

    test("plus is ignored during merge") {
      // Ordinarily, the :+ operator would cause `+` to performed on fields during object inheritance:
      eval("{a: 1} + {a+: 2}") ==> ujson.Obj("a" -> 3)
      // But mergePatch intentionally does not consider `+:` fields to be special:
      eval("std.mergePatch({a: 1}, {a+: 2})") ==> ujson.Obj("a" -> 2)
      // We also need to check that the resulting output fields aren't treated as `+:` fields for
      // the purposes of subsequent object inheritance:
      eval("{a: 1} + std.mergePatch({}, {a+: 2})") ==> ujson.Obj("a" -> 2)
      // The `+:` behavior is lost even if it is from the target:
      eval("{a: 1} + std.mergePatch({a+: 2}, {})") ==> ujson.Obj("a" -> 2)
      // It's also lost in nested fields:
      eval("{a: {b: 1}} + std.mergePatch({a: {b +: 2}}, {})") ==> ujson.Obj("a" -> ujson.Obj("b" -> 2))
    }

    test("default visibility of resulting fields") {
      // In v0.20.0 of google/jsonnet, the following returned `{a: 1}`, not `{}`:
      eval("{a:: 0} + std.mergePatch({a: 1}, {})") ==> ujson.Obj()
      eval("({a:: 0} + std.mergePatch({a: 1}, {})).a") ==> ujson.Num(1)
      // However, go-jsonnet v0.20.0 returned `{}`. This difference arose because the pure-jsonnet
      // implementation of `std.mergePatch` used object comprehensions in its implementation and
      // the two implementations differed in the default visibility of fields in the reuslting
      // object. See https://github.com/google/jsonnet/issues/1111
      // google/jsonnet merged a change to match go-jsonnet's behavior in a future version:
      // https://github.com/google/jsonnet/pull/1140
      // Interestingly, sjsonnet already matched go-jsonnet's behavior for object comprehensions
      // because the following already returned `{}`; our previous behavior difference was only
      // for mergePatch results:
      eval("""{a:: 0} + {[a]: 1 for a in ["a"]}""") ==> ujson.Obj()
      eval("""({a:: 0} + {[a]: 1 for a in ["a"]}).a""") ==> ujson.Num(1)
    }

    test("lazy evaluation of LHS members") {
      // An erroring target field which doesn't merge with any patch fields.
      // This shouldn't error because we should avoid eager evaluation of unmerged target fields:
      eval("(std.mergePatch({ boom: error \"should not error\" }, {}) + { flag: 42 }).flag") ==> ujson.Num(42)

      // An erroring target field which is removed by the patch:
      // This should not error because we shouldn't evaluate the removed field:
      eval("(std.mergePatch({ boom: error \"should not error\" }, { boom: null }) + { flag: 42 }).flag") ==> ujson.Num(42)
    }
  }
}
