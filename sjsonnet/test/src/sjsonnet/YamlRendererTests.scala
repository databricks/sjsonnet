package sjsonnet

import utest._

object YamlRendererTests extends TestSuite{
  def tests = Tests {
    'empty - {
      ujson.transform(ujson.Js.Arr(), new YamlRenderer()).toString ==> "[]"
      ujson.transform(ujson.Js.Obj(), new YamlRenderer()).toString ==> "{}"
      ujson.transform(ujson.Js.Obj("a" -> ujson.Js.Arr(), "b" -> ujson.Js.Obj()), new YamlRenderer()).toString ==>
        """"a": []
          |"b": {}""".stripMargin
    }
    'nonEmpty - {
      ujson.transform(ujson.Js.Arr(1), new YamlRenderer()).toString ==>
        """- 1""".stripMargin
      ujson.transform(ujson.Js.Arr(1, 2), new YamlRenderer()).toString ==>
        """- 1
          |- 2""".stripMargin
      ujson.transform(ujson.Js.Obj("a" -> 1), new YamlRenderer()).toString ==>
        """"a": 1""".stripMargin
      ujson.transform(ujson.Js.Obj("a" -> 1, "b" -> 2), new YamlRenderer()).toString ==>
        """"a": 1
          |"b": 2""".stripMargin
    }
    'nested - {
      ujson.transform(
        ujson.Js.Arr(ujson.Js.Obj("a" -> ujson.Js.Arr(1))),
        new YamlRenderer()).toString ==>
        """- "a":
          |  - 1""".stripMargin
    }

  }

}