package sjsonnet

import utest._

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

object InterpretAsyncTests extends TestSuite {

  /**
   * Wraps a synchronous file map as a JS Promise-returning loader, so the test exercises the real
   * async code path.
   */
  private def makeAsyncLoader(
      files: Map[String, String]): js.Function2[String, Boolean, js.Promise[Any]] = {
    (path: String, _: Boolean) =>
      Future {
        files.get(path) match {
          case Some(s) => s.asInstanceOf[Any]
          case None    => throw js.JavaScriptException(s"missing file: $path")
        }
      }.toJSPromise
  }

  private def makeResolver(known: Set[String]): js.Function2[String, String, String] =
    (_: String, name: String) => if (known.contains(name)) name else null

  private def runAsync(text: String, files: Map[String, String]): Future[ujson.Value] = {
    val loader = makeAsyncLoader(files)
    val resolver = makeResolver(files.keySet)
    SjsonnetMain
      .interpretAsync(
        text,
        js.Dictionary[js.Any](),
        js.Dictionary[js.Any](),
        "/",
        resolver,
        loader
      )
      .toFuture
      .map(v => ujson.WebJson.transform(v, ujson.Value))
  }

  def tests: Tests = Tests {

    test("simple async import returns a Promise of the result") {
      runAsync(
        "(import 'lib.libsonnet').n",
        Map("lib.libsonnet" -> "{ n: 42 }")
      ).map(v => assert(v == ujson.Num(42)))
    }

    test("transitive async imports load and evaluate") {
      runAsync(
        "(import 'a.libsonnet').value",
        Map(
          "a.libsonnet" -> "{ value: (import 'b.libsonnet').y + 1 }",
          "b.libsonnet" -> "{ y: 10 }"
        )
      ).map(v => assert(v == ujson.Num(11)))
    }

    test("importstr loads as text without further parsing") {
      // The data file would be invalid Jsonnet — it must NOT be parsed.
      runAsync(
        "importstr 'data.txt'",
        Map("data.txt" -> "this is :: not :: jsonnet")
      ).map(v => assert(v == ujson.Str("this is :: not :: jsonnet")))
    }

    test("async loader rejection propagates through the returned Promise") {
      val resolver = makeResolver(Set("missing.libsonnet"))
      val loader: js.Function2[String, Boolean, js.Promise[Any]] =
        (path: String, _: Boolean) => js.Promise.reject(s"boom: $path")
      val out = SjsonnetMain.interpretAsync(
        "import 'missing.libsonnet'",
        js.Dictionary[js.Any](),
        js.Dictionary[js.Any](),
        "/",
        resolver,
        loader
      )
      out.toFuture.transform {
        case scala.util.Failure(_) => scala.util.Success(())
        case scala.util.Success(v) =>
          scala.util.Failure(new RuntimeException(s"expected failure, got $v"))
      }
    }
  }
}
