package sjsonnet

import utest._

import java.util.concurrent.{ConcurrentHashMap, CountDownLatch}
import java.util.concurrent.atomic.AtomicReference

object JsonImportFastPathJvmTests extends TestSuite {
  private class ConcurrentParseCache extends ParseCache {
    private val cache =
      new ConcurrentHashMap[(Path, String), Either[Error, (Expr, FileScope)]]()

    def getOrElseUpdate(
        key: (Path, String),
        defaultValue: => Either[Error, (Expr, FileScope)]): Either[Error, (Expr, FileScope)] = {
      val existing = cache.get(key)
      if (existing != null) existing
      else {
        val computed = defaultValue
        val previous = cache.putIfAbsent(key, computed)
        if (previous == null) computed else previous
      }
    }
  }

  def tests: Tests = Tests {
    test("strict json imports can be shared by concurrent interpreters") {
      val files = Map(
        "data.json" ->
          """{"a":{"b":[1,2,3]},"arr":[{"x":"one"},{"x":"two"}],"z":true}"""
      )
      val importer = new Importer {
        def resolve(docBase: Path, importName: String): Option[Path] =
          if (files.contains(importName)) Some(DummyPath(importName)) else None
        def read(path: Path, binaryData: Boolean): Option[ResolvedFile] =
          path match {
            case DummyPath(name) => files.get(name).map(StaticResolvedFile.apply)
            case _               => None
          }
      }
      val parseCache = new ConcurrentParseCache
      val code =
        """local d = import "data.json";
          |{whole: d, pick: d.arr[1].x, fields: std.objectFields(d), singleFields: std.objectFields(d.a)}
          |""".stripMargin
      val path = DummyPath("root", "main.jsonnet")
      val expected = Right(
        ujson.Obj(
          "whole" -> ujson.Obj(
            "a"   -> ujson.Obj("b" -> ujson.Arr(1, 2, 3)),
            "arr" -> ujson.Arr(ujson.Obj("x" -> "one"), ujson.Obj("x" -> "two")),
            "z"   -> true
          ),
          "pick"         -> "two",
          "fields"       -> ujson.Arr("a", "arr", "z"),
          "singleFields" -> ujson.Arr("b")
        )
      )

      val warm = new Interpreter(
        Map.empty,
        Map.empty,
        DummyPath("root"),
        importer,
        parseCache = parseCache
      ).interpret(code, path)
      assert(warm == expected)

      val threads = new Array[Thread](8)
      val start = new CountDownLatch(1)
      val done = new CountDownLatch(threads.length)
      val failure = new AtomicReference[Throwable]()

      var i = 0
      while (i < threads.length) {
        threads(i) = new Thread(new Runnable {
          def run(): Unit = {
            try {
              start.await()
              var j = 0
              while (j < 50) {
                val result = new Interpreter(
                  Map.empty,
                  Map.empty,
                  DummyPath("root"),
                  importer,
                  parseCache = parseCache
                ).interpret(code, path)
                if (result != expected) {
                  throw new java.lang.AssertionError(s"unexpected result: $result")
                }
                j += 1
              }
            } catch {
              case t: Throwable => failure.compareAndSet(null, t)
            } finally {
              done.countDown()
            }
          }
        })
        threads(i).start()
        i += 1
      }

      start.countDown()
      done.await()
      val thrown = failure.get()
      if (thrown != null) throw thrown
    }
  }
}
