package sjsonnet

import utest._

object StdSetWithKeyFTests extends TestSuite{
  def eval(s: String) = {
    new Interpreter(
      SjsonnetMain.createParseCache(),
      Map(),
      Map(),
      DummyPath(),
      (_, _) => None
    ).interpret(s, DummyPath("(memory)")) match{
      case Right(x) => x
      case Left(e) => throw new Exception(e)
    }
  }
  def tests = Tests{
    test("stdSetWithKeyF") {
      eval("std.setMember(\"a\", [\"a\", \"b\", \"c\"], function(x) x)") ==> ujson.True
      eval("std.setMember(\"a\", [\"a\", \"b\", \"c\"])") ==> ujson.True
      eval("std.setMember(\"d\", [\"a\", \"b\", \"c\"], function(x) x)") ==> ujson.False

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
        """).toString() ==> """[{"language":{"name":"C++","version":"n/a"},"name":"FooBar"},{"language":{"name":"Java","version":"1.8"},"name":"Foo"},{"language":{"name":"Scala","version":"1.0"},"name":"Bar"}]"""

      eval("std.sort(\"lskdhdfjblksgh\")").toString() ==> """["b","d","d","f","g","h","h","j","k","k","l","l","s","s"]"""
    }
  }
}
