package sjsonnet

import utest._

object YamlRendererTests extends TestSuite {
  def tests: Tests = Tests {
    test("empty") {
      ujson.transform(ujson.Arr(), new YamlRenderer()).toString ==> "[]"
      ujson.transform(ujson.Obj(), new YamlRenderer()).toString ==> "{}"
      ujson
        .transform(ujson.Obj("a" -> ujson.Arr(), "b" -> ujson.Obj()), new YamlRenderer())
        .toString ==>
      """"a": []
        |"b": {}""".stripMargin
    }
    test("nonEmpty") {
      ujson.transform(ujson.Arr(1), new YamlRenderer()).toString ==>
      """- 1""".stripMargin
      ujson.transform(ujson.Arr(1, 2), new YamlRenderer()).toString ==>
      """- 1
        |- 2""".stripMargin
      ujson.transform(ujson.Obj("a" -> 1), new YamlRenderer()).toString ==>
      """"a": 1""".stripMargin
      ujson.transform(ujson.Obj("a" -> 1, "b" -> 2), new YamlRenderer()).toString ==>
      """"a": 1
        |"b": 2""".stripMargin
    }
    test("nested") {
      ujson.transform(ujson.Arr(ujson.Obj("a" -> ujson.Arr(1))), new YamlRenderer()).toString ==>
      """- "a":
        |  - 1""".stripMargin
    }
    test("noQuote") {
      ujson
        .transform(ujson.Obj("k0" -> "v0", "k1" -> "(\\d+)"), new YamlRenderer(quoteKeys = false))
        .toString ==>
      """k0: "v0"
        |k1: "(\\d+)"""".stripMargin
    }
  }

}
