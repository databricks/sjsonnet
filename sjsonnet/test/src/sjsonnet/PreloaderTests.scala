package sjsonnet

import utest._

import scala.collection.mutable

object PreloaderTests extends TestSuite {

  /** A virtual file system used by both the preloader's `resolve` and the test's loading loop. */
  private class FakeFs(files: Map[String, String], binFiles: Map[String, Array[Byte]] = Map.empty) {
    val readPaths: mutable.ArrayBuffer[(String, Boolean)] = mutable.ArrayBuffer.empty

    val importer: Importer = new Importer {
      def resolve(docBase: Path, importName: String): Option[Path] = {
        val candidate = DummyPath(importName)
        if (files.contains(importName) || binFiles.contains(importName)) Some(candidate) else None
      }
      def read(path: Path, binaryData: Boolean): Option[ResolvedFile] =
        throw new RuntimeException(s"read should not be called during preload: $path")
    }

    def load(path: Path, binaryData: Boolean): ResolvedFile = {
      val key = path.asInstanceOf[DummyPath].segments.head
      readPaths += ((key, binaryData))
      if (binaryData) StaticBinaryResolvedFile(binFiles(key))
      else StaticResolvedFile(files(key))
    }
  }

  private def runPreload(fs: FakeFs, entryPath: Path, entry: String): Preloader = {
    val preloader = new Preloader(fs.importer)
    preloader.add(entryPath, StaticResolvedFile(entry), ImportFinder.Kind.Code) match {
      case Left(err) => throw err
      case Right(_)  =>
    }
    while (!preloader.isComplete) {
      val batch = preloader.takePendingImports()
      batch.foreach { p =>
        val content = fs.load(p.path, p.binaryData)
        preloader.add(p.path, content, p.kind) match {
          case Left(err) => throw err
          case Right(_)  =>
        }
      }
    }
    preloader
  }

  def tests: Tests = Tests {

    test("discovers transitive imports") {
      val fs = new FakeFs(
        Map(
          "a.libsonnet" -> "import 'b.libsonnet'",
          "b.libsonnet" -> "{ x: 1 }"
        )
      )
      val entry = "import 'a.libsonnet'"
      val preloader = runPreload(fs, DummyPath("entry"), entry)

      val loaded = fs.readPaths.map(_._1).toSet
      assert(loaded == Set("a.libsonnet", "b.libsonnet"))
      assert(preloader.loaded.size == 3) // entry + a + b
    }

    test("dedupes identical imports") {
      val fs = new FakeFs(
        Map(
          "shared.libsonnet" -> "{ y: 2 }"
        )
      )
      val entry = "[import 'shared.libsonnet', import 'shared.libsonnet']"
      runPreload(fs, DummyPath("entry"), entry)

      assert(fs.readPaths.count(_._1 == "shared.libsonnet") == 1)
    }

    test("handles importstr and importbin") {
      val fs = new FakeFs(
        Map("data.txt" -> "hello"),
        binFiles = Map("blob.bin" -> Array[Byte](1, 2, 3))
      )
      val entry = "{ s: importstr 'data.txt', b: importbin 'blob.bin' }"
      runPreload(fs, DummyPath("entry"), entry)

      assert(fs.readPaths.toSet == Set(("data.txt", false), ("blob.bin", true)))
    }

    test("does not parse importstr/importbin contents for further imports") {
      // The string content here would be invalid Jsonnet if parsed; preloader must not parse it.
      val fs = new FakeFs(Map("data.txt" -> "this is not jsonnet ::: !@#"))
      val entry = "importstr 'data.txt'"
      runPreload(fs, DummyPath("entry"), entry)

      assert(fs.readPaths.toSeq == Seq(("data.txt", false)))
    }

    test("interpreter evaluates against preloaded cache") {
      val fs = new FakeFs(
        Map(
          "lib.libsonnet" -> "{ greet(name): 'hello, ' + name }"
        )
      )
      val entry = "(import 'lib.libsonnet').greet('world')"
      val entryPath = DummyPath("entry")
      val preloader = runPreload(fs, entryPath, entry)

      val interp = new Interpreter(
        Map.empty[String, String],
        Map.empty[String, String],
        DummyPath(),
        preloader.importer,
        parseCache = new DefaultParseCache
      )
      val result = interp.interpret(entry, entryPath)
      assert(result == Right(ujson.Str("hello, world")))
    }

    test("parse error in entry is reported") {
      val fs = new FakeFs(Map.empty)
      val preloader = new Preloader(fs.importer)
      val out =
        preloader.add(DummyPath("entry"), StaticResolvedFile("local x ="), ImportFinder.Kind.Code)
      assert(out.isLeft)
    }
  }
}
