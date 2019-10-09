package sjsonnet

import utest._

object StdWithKeyFTests extends TestSuite {
  def eval(s: String) = {
    new Interpreter(
      SjsonnetMain.createParseCache(),
      Map(),
      Map(),
      DummyPath(),
      (_, _) => None
    ).interpret(s, DummyPath("(memory)")) match {
      case Right(x) => x
      case Left(e) => throw new Exception(e)
    }
  }

  def tests = Tests {
    test("stdSetMemberWithKeyF") {
      eval("std.setMember(\"a\", [\"a\", \"b\", \"c\"], function(x) x)") ==> ujson.True
      eval("std.setMember(\"a\", [\"a\", \"b\", \"c\"])") ==> ujson.True
      eval("std.setMember(\"d\", [\"a\", \"b\", \"c\"], function(x) x)") ==> ujson.False

      eval(
        """local arr = [
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
      eval("std.sort([\"c\", \"a\", \"b\"])").toString() ==> """["a","b","c"]"""

      eval(
        """local arr = [
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

      eval("std.sort(\"lskdhdfjblksgh\")").toString() ==> """["b","d","d","f","g","h","h","j","k","k","l","l","s","s"]"""
    }
    test("stdUniqWithKeyF") {
      eval("std.uniq([\"c\", \"c\", \"b\", \"b\", \"b\", \"a\", \"b\", \"a\"])").toString() ==> """["c","b","a","b","a"]"""

      eval(
        """local arr = [
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
      eval("std.set([\"c\", \"c\", \"b\", \"b\", \"b\", \"a\", \"b\", \"a\"])").toString() ==> """["a","b","c"]"""

      eval(
        """local arr = [
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
      eval("std.setUnion([\"c\", \"c\", \"b\"], [\"b\", \"b\", \"a\", \"b\", \"a\"])").toString() ==> """["a","b","c"]"""

      eval(
        """local arr1 = [
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
             local arr2 = [
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
             ];
             
             std.setUnion(arr1, arr2, function(x) x.language.name)""").toString() ==>
        """[{"language":{"name":"C++","version":"n/a"},"name":"FooBar"},{"language":{"name":"Java","version":"1.8"},"name":"Foo"},{"language":{"name":"Scala","version":"2.13"},"name":"Bar"}]"""
    }
    test("stdSetInterWithKeyF") {
      eval("std.setInter([\"c\", \"c\", \"b\"], [\"b\", \"b\", \"a\", \"b\", \"a\"])").toString() ==> """["b"]"""

      eval(
        """local arr1 = [
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
             local arr2 = [
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
             ];

             std.setInter(arr1, arr2, function(x) x.language.name)""").toString() ==>
        """[{"language":{"name":"C++","version":"n/a"},"name":"FooBar"},{"language":{"name":"Java","version":"1.8"},"name":"Foo"}]"""
    }
    test("stdSetDiffWithKeyF") {
      eval("std.setDiff([\"c\", \"c\", \"b\"], [\"b\", \"b\", \"a\", \"b\", \"a\"])").toString() ==> """["c"]"""

      eval(
        """local arr1 = [
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
              ];
           local arr2 = [
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

             std.setDiff(arr1, arr2, function(x) x.language.name)""").toString() ==>
        """[{"language":{"name":"Scala","version":"2.13"},"name":"Bar"}]"""
    }
  }
}
