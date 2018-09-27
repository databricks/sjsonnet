package sjsonnet

import sjsonnet.Expr._
import utest._

object RendererTests extends TestSuite{
  def tests = Tests {
    'hello - {
      ujson.transform(ujson.Js.Arr(ujson.Js.Num(1), ujson.Js.Num(2)), new Renderer()).toString ==>
        "[1, 2]"
    }
  }

}