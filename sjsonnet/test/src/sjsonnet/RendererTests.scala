package sjsonnet

import sjsonnet.Expr._
import utest._

object RendererTests extends TestSuite{
  def tests = Tests {
    'hello - {
      ujson.transform(ujson.Js.Arr(ujson.Js.Num(1), ujson.Js.Num(2)), new Renderer()).toString ==>
        "[1, 2]"
    }
    'empty - {
      ujson.transform(ujson.Js.Arr(), new Renderer(indent = 3)).toString ==> "[ ]"
      ujson.transform(ujson.Js.Obj(), new Renderer(indent = 3)).toString ==> "{ }"
    }
    'nonEmpty - {
      ujson.transform(ujson.Js.Arr(1), new Renderer(indent = 3)).toString ==>
        """[
          |   1
          |]""".stripMargin
      ujson.transform(ujson.Js.Arr(1, 2), new Renderer(indent = 3)).toString ==>
        """[
          |   1,
          |   2
          |]""".stripMargin
      ujson.transform(ujson.Js.Obj("a" -> 1), new Renderer(indent = 3)).toString ==>
        """{
          |   "a": 1
          |}""".stripMargin
      ujson.transform(ujson.Js.Obj("a" -> 1, "b" -> 2), new Renderer(indent = 3)).toString ==>
        """{
          |   "a": 1,
          |   "b": 2
          |}""".stripMargin
    }
    'nested - {
      ujson.transform(
        ujson.Js.Arr(ujson.Js.Obj("a" -> ujson.Js.Arr())),
        new Renderer(indent = 3)).toString ==>
          """[
            |   {
            |      "a": [ ]
            |   }
            |]""".stripMargin
    }

  }

}