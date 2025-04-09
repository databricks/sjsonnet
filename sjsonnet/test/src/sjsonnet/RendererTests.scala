package sjsonnet

import sjsonnet.Expr._
import utest._

object RendererTests extends TestSuite{
  def tests: Tests = Tests {
    test("hello") {
      ujson.transform(ujson.Arr(ujson.Num(1), ujson.Num(2)), new Renderer()).toString ==>
        "[1, 2]"
    }
    test("empty") {
      ujson.transform(ujson.Arr(), new Renderer(indent = 3)).toString ==> "[ ]"
      ujson.transform(ujson.Obj(), new Renderer(indent = 3)).toString ==> "{ }"
    }
    test("nonEmpty") {
      ujson.transform(ujson.Arr(1), new Renderer(indent = 3)).toString ==>
        """[
          |   1
          |]""".stripMargin
      ujson.transform(ujson.Arr(1, 2), new Renderer(indent = 3)).toString ==>
        """[
          |   1,
          |   2
          |]""".stripMargin
      ujson.transform(ujson.Obj("a" -> 1), new Renderer(indent = 3)).toString ==>
        """{
          |   "a": 1
          |}""".stripMargin
      ujson.transform(ujson.Obj("a" -> 1, "b" -> 2), new Renderer(indent = 3)).toString ==>
        """{
          |   "a": 1,
          |   "b": 2
          |}""".stripMargin
    }
    test("nested") {
      ujson.transform(
        ujson.Arr(ujson.Obj("a" -> ujson.Arr())),
        new Renderer(indent = 3)).toString ==>
          """[
            |   {
            |      "a": [ ]
            |   }
            |]""".stripMargin
    }

  }

}