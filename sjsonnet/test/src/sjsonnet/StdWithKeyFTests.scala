package sjsonnet

import utest._
import TestUtils.{eval, evalErr}
object StdWithKeyFTests extends TestSuite {

  def tests: Tests = Tests {
    test("stdSetMemberWithKeyF") {
      eval(
        "std.setMember(\"a\", std.set([\"a\", \"b\", \"c\"], function(x) x), function(x) x)"
      ) ==> ujson.True
      eval("std.setMember(\"a\", std.set([\"a\", \"b\", \"c\"]))") ==> ujson.True
      eval(
        "std.setMember(\"d\", std.set([\"a\", \"b\", \"c\"], function(x) x), function(x) x)"
      ) ==> ujson.False

      eval("""local arr = std.set([
                {
                   "name": "Foo",
                   "language": {
                     "name": "Java",
                     "version": "1.8"
                   }
                 },
                 {
                   "name": "Bar",
                   "language": {
                     "name": "Scala",
                     "version": "1.0"
                   }
                 },
                 {
                   "name": "FooBar",
                   "language": {
                     "name": "C++",
                     "version": "n/a"
                   }
                 }
              ], function(x) x.language.name);
             
              local testObj = {
                   "name": "TestObj",
                   "language": {
                     "name": "Java",
                     "version": "1.7"
                   }
              };
             
              std.setMember(testObj, arr, function(x) x.language.name)
        """) ==> ujson.True
    }
    test("stdSortWithKeyF") {
      eval("""std.sort(["a","b","c"])""").toString() ==> """["a","b","c"]"""
      eval("""std.sort([1, 2, 3])""").toString() ==> """[1,2,3]"""
      eval("""std.sort([1,2,3], keyF=function(x) -x)""").toString() ==> """[3,2,1]"""
      eval("""std.sort([1,2,3], function(x) -x)""").toString() ==> """[3,2,1]"""
      assert(
        evalErr("""std.sort([1,2,3], keyF=function(x) error "foo")""").startsWith(
          "sjsonnet.Error: foo"
        )
      )
      assert(evalErr("""std.sort([1,2, error "foo"])""").startsWith("sjsonnet.Error: foo"))
      assert(
        evalErr("""std.sort([1, [error "foo"]])""").startsWith(
          "sjsonnet.Error: Cannot sort with values that are not all the same type"
        )
      )
      // google/go-jsonnet and google/jsonnet also error on sorting of booleans:
      assert(
        evalErr("""std.sort([false, true])""").startsWith(
          "sjsonnet.Error: Cannot sort with values that are booleans"
        )
      )
      assert(
        evalErr("""std.sort([1, 2], keyF=function(x) x == 1)""").startsWith(
          "sjsonnet.Error: Cannot sort with key values that are booleans"
        )
      )

      eval("""local arr = [
                {
                   "name": "Foo",
                   "language": {
                     "name": "Java",
                     "version": "1.8"
                   }
                 },
                 {
                   "name": "Bar",
                   "language": {
                     "name": "Scala",
                     "version": "1.0"
                   }
                 },
                 {
                   "name": "FooBar",
                   "language": {
                     "name": "C++",
                     "version": "n/a"
                   }
                 }
              ];

              std.sort(arr, function(x) x.language.name)
        """).toString() ==>
      """[{"language":{"name":"C++","version":"n/a"},"name":"FooBar"},{"language":{"name":"Java","version":"1.8"},"name":"Foo"},{"language":{"name":"Scala","version":"1.0"},"name":"Bar"}]"""

      eval("std.sort(\"lskdhdfjblksgh\")")
        .toString() ==> """["b","d","d","f","g","h","h","j","k","k","l","l","s","s"]"""
    }
    test("stdUniqWithKeyF") {
      eval("std.uniq([\"c\", \"c\", \"b\", \"b\", \"b\", \"a\", \"b\", \"a\"])")
        .toString() ==> """["c","b","a","b","a"]"""

      eval("""local arr = [
                {
                   "name": "Foo",
                   "language": {
                     "name": "Java",
                     "version": "1.8"
                   }
                 },
                 {
                   "name": "Bar",
                   "language": {
                     "name": "Java",
                     "version": "1.7"
                   }
                 },
                 {
                   "name": "FooBar",
                   "language": {
                     "name": "C++",
                     "version": "n/a"
                   }
                 }
              ];

              std.uniq(arr, function(x) x.language.name)
        """).toString() ==>
      """[{"language":{"name":"Java","version":"1.8"},"name":"Foo"},{"language":{"name":"C++","version":"n/a"},"name":"FooBar"}]"""
    }
    test("stdSetWithKeyF") {
      eval("std.set([\"c\", \"c\", \"b\", \"b\", \"b\", \"a\", \"b\", \"a\"])")
        .toString() ==> """["a","b","c"]"""

      eval("""local arr = [
                {
                   "name": "Foo",
                   "language": {
                     "name": "Java",
                     "version": "1.8"
                   }
                 },
                 {
                   "name": "Bar",
                   "language": {
                     "name": "Java",
                     "version": "1.7"
                   }
                 },
                 {
                   "name": "FooBar",
                   "language": {
                     "name": "C++",
                     "version": "n/a"
                   }
                 }
              ];

              std.set(arr, function(x) x.language.name)
        """).toString() ==>
      """[{"language":{"name":"C++","version":"n/a"},"name":"FooBar"},{"language":{"name":"Java","version":"1.8"},"name":"Foo"}]"""
    }
    test("stdSetUnionWithKeyF") {
      eval(
        "std.setUnion(std.set([\"c\", \"c\", \"b\"]), std.set([\"b\", \"b\", \"a\", \"b\", \"a\"]))"
      ).toString() ==>
      """["a","b","c"]"""

      eval("std.setUnion(std.set([]), std.set([\"b\", \"b\", \"a\", \"b\", \"a\"]))").toString() ==>
      """["a","b"]"""

      eval("std.setUnion(std.set([\"c\", \"c\", \"b\"]), std.set([]))").toString() ==>
      """["b","c"]"""

      eval("""local arr1 = std.set([
                {
                   "name": "Foo",
                   "language": {
                     "name": "Java",
                     "version": "1.8"
                   }
                 },
                 {
                   "name": "Bar",
                   "language": {
                     "name": "Java",
                     "version": "1.7"
                   }
                 },
                 {
                   "name": "FooBar",
                   "language": {
                     "name": "C++",
                     "version": "n/a"
                   }
                 }
              ], function(x) x.language.name);
             local arr2 = std.set([
               {
                  "name": "Foo",
                  "language": {
                    "name": "Java",
                    "version": "12"
                  }
                },
                {
                  "name": "Bar",
                  "language": {
                    "name": "Scala",
                    "version": "2.13"
                  }
                },
                {
                  "name": "FooBar",
                  "language": {
                    "name": "C++",
                    "version": "n/a"
                  }
                }
             ], function(x) x.language.name);
             
             std.setUnion(arr1, arr2, function(x) x.language.name)""").toString() ==>
      """[{"language":{"name":"C++","version":"n/a"},"name":"FooBar"},{"language":{"name":"Java","version":"1.8"},"name":"Foo"},{"language":{"name":"Scala","version":"2.13"},"name":"Bar"}]"""
    }
    test("stdSetInterWithKeyF") {
      eval("std.setInter([\"b\", \"c\"], [\"a\", \"b\"])").toString() ==> """["b"]"""

      eval("""local arr1 = std.set([
                {
                   "name": "Foo",
                   "language": {
                     "name": "Java",
                     "version": "1.8"
                   }
                 },
                 {
                   "name": "Bar",
                   "language": {
                     "name": "Java",
                     "version": "1.7"
                   }
                 },
                 {
                   "name": "FooBar",
                   "language": {
                     "name": "C++",
                     "version": "n/a"
                   }
                 }
              ], function(x) x.language.name);
             local arr2 = std.set([
               {
                  "name": "Foo",
                  "language": {
                    "name": "Java",
                    "version": "12"
                  }
                },
                {
                  "name": "Bar",
                  "language": {
                    "name": "Scala",
                    "version": "2.13"
                  }
                },
                {
                  "name": "FooBar",
                  "language": {
                    "name": "C++",
                    "version": "n/a"
                  }
                }
             ], function(x) x.language.name);

             std.setInter(arr1, arr2, function(x) x.language.name)""").toString() ==>
      """[{"language":{"name":"C++","version":"n/a"},"name":"FooBar"},{"language":{"name":"Java","version":"1.8"},"name":"Foo"}]"""
    }
    test("stdSetDiffWithKeyF") {
      eval("std.setDiff([\"b\", \"c\"], [\"a\", \"b\"])").toString() ==> """["c"]"""

      eval("""local arr1 = std.set([
                {
                   "name": "Foo",
                   "language": {
                     "name": "Java",
                     "version": "12"
                   }
                 },
                 {
                   "name": "Bar",
                   "language": {
                     "name": "Scala",
                     "version": "2.13"
                   }
                 },
                 {
                   "name": "FooBar",
                   "language": {
                     "name": "C++",
                     "version": "n/a"
                   }
                 }
              ], function(x) x.language.name);
           local arr2 = std.set([
                {
                   "name": "Foo",
                   "language": {
                     "name": "Java",
                     "version": "1.8"
                   }
                 },
                 {
                   "name": "Bar",
                   "language": {
                     "name": "Java",
                     "version": "1.7"
                   }
                 },
                 {
                   "name": "FooBar",
                   "language": {
                     "name": "C++",
                     "version": "n/a"
                   }
                 }
              ], function(x) x.language.name);

             std.setDiff(arr1, arr2, function(x) x.language.name)""").toString() ==>
      """[{"language":{"name":"Scala","version":"2.13"},"name":"Bar"}]"""
    }
  }
}
