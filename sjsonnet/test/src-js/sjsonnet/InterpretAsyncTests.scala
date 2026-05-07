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

    test("preloads imports referenced from extVars") {
      // extVar value is a Jsonnet snippet that imports a file. interpretAsync must walk
      // ext/tla var snippets too; otherwise the cache-only importer hits a miss at eval time.
      val files = Map("lib.libsonnet" -> "{ n: 5 }")
      val loader = makeAsyncLoader(files)
      val resolver = makeResolver(files.keySet)
      val extVars = js.Dictionary[js.Any]("cfg" -> "(import 'lib.libsonnet').n")
      SjsonnetMain
        .interpretAsync(
          "std.extVar('cfg') + 1",
          extVars,
          js.Dictionary[js.Any](),
          "/",
          resolver,
          loader
        )
        .toFuture
        .map(v => ujson.WebJson.transform(v, ujson.Value))
        .map(v => assert(v == ujson.Num(6)))
    }

    test("parse error in unforced branch does not fail evaluation") {
      // Lazy semantics: `if false then import 'bad' else 1` should evaluate to 1, even though
      // bad.libsonnet has a parse error. The preloader still loads the file (jsonnet imports
      // are statically discoverable), but a parse failure on a discovered file must not abort.
      runAsync(
        "if false then import 'bad.libsonnet' else 1",
        Map("bad.libsonnet" -> "this is :: not :: jsonnet")
      ).map(v => assert(v == ujson.Num(1)))
    }

    test("importstr and importbin for the same path return distinct values") {
      // He-Pin's reproducer: `if true then importstr "x" else importbin "x"`. The cache must
      // keep separate entries for text and bytes; otherwise async returns a binary file to
      // importstr (rejecting with NotImplementedError) or vice versa.
      val text = "the-text"
      val bytes = Array[Byte](1, 2, 3)
      val resolver = makeResolver(Set("same"))
      val loader: js.Function2[String, Boolean, js.Promise[Any]] =
        (_: String, binaryData: Boolean) =>
          Future {
            if (binaryData) bytes.toJSArray.asInstanceOf[Any]
            else text.asInstanceOf[Any]
          }.toJSPromise
      SjsonnetMain
        .interpretAsync(
          "if true then importstr 'same' else importbin 'same'",
          js.Dictionary[js.Any](),
          js.Dictionary[js.Any](),
          "/",
          resolver,
          loader
        )
        .toFuture
        .map(v => ujson.WebJson.transform(v, ujson.Value))
        .map(v => assert(v == ujson.Str(text)))
    }

    test("entry parse error matches synchronous interpret's error formatting") {
      // The async path must route entry parse errors through interpret0 so the message shape
      // and root frame match the synchronous interpret. Verify the error includes the
      // "(memory)" location marker that interpret0 produces.
      val resolver = makeResolver(Set.empty)
      val loader: js.Function2[String, Boolean, js.Promise[Any]] =
        (path: String, _: Boolean) => js.Promise.reject(s"unexpected load: $path")
      val out = SjsonnetMain.interpretAsync(
        "local x =", // syntactically invalid
        js.Dictionary[js.Any](),
        js.Dictionary[js.Any](),
        "/",
        resolver,
        loader
      )
      out.toFuture.transform {
        case scala.util.Success(v) =>
          scala.util.Failure(new RuntimeException(s"expected parse failure, got $v"))
        case scala.util.Failure(e) =>
          val msg = e.getMessage
          if (msg != null && msg.contains("(memory)")) scala.util.Success(())
          else
            scala.util.Failure(
              new RuntimeException(s"expected formatted parse error mentioning (memory), got: $msg")
            )
      }
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
