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

    test("preloaded files carry pre-parsed AST so fastparse runs once") {
      // Wrap each ResolvedFile with a counter so we can detect re-parsing. The Preloader parses
      // once during discover; the Interpreter should consume the attached AST without re-parsing.
      val parseCount = mutable.HashMap.empty[Path, Int].withDefaultValue(0)
      class CountingResolvedFile(content: String, path: Path) extends ResolvedFile {
        def getParserInput(): fastparse.ParserInput = {
          parseCount(path) = parseCount(path) + 1
          fastparse.IndexedParserInput(content)
        }
        def readString(): String           = content
        def contentHash(): String          = content
        def readRawBytes(): Array[Byte]    =
          content.getBytes(java.nio.charset.StandardCharsets.UTF_8)
      }
      val files = Map(
        "lib.libsonnet" -> "{ x: 1 }",
        "entry"         -> "(import 'lib.libsonnet').x"
      )
      val importer = new Importer {
        def resolve(docBase: Path, importName: String): Option[Path] =
          if (files.contains(importName)) Some(DummyPath(importName)) else None
        def read(path: Path, binaryData: Boolean): Option[ResolvedFile] =
          throw new RuntimeException(s"unexpected read: $path")
      }
      val preloader = new Preloader(importer)
      val entryPath = DummyPath("entry")
      preloader.add(entryPath, new CountingResolvedFile(files("entry"), entryPath))
      while (!preloader.isComplete) {
        val batch = preloader.takePendingImports()
        batch.foreach { p =>
          val key = p.path.asInstanceOf[DummyPath].segments.head
          preloader.add(p.path, new CountingResolvedFile(files(key), p.path), p.kind)
        }
      }
      // Both files have been parsed exactly once — the Preloader's parse pass.
      assert(parseCount(DummyPath("lib.libsonnet")) == 1)
      assert(parseCount(entryPath) == 1)

      // Run a full interpret. The Interpreter must not re-parse; getParserInput would bump the
      // counter again if it did.
      val interp = new Interpreter(
        Map.empty[String, String],
        Map.empty[String, String],
        DummyPath(),
        preloader.importer,
        parseCache = new DefaultParseCache
      )
      val result = interp.interpret(files("entry"), entryPath)
      assert(result == Right(ujson.Num(1)))
      assert(parseCount(DummyPath("lib.libsonnet")) == 1)
    }

    test("resolves imports relative to the importing file's parent directory") {
      // Resolver records what docBase it was called with, and only resolves names against the
      // expected `dir/` parent — proving the preloader passes parent(), not the file path itself.
      val seenDocBases = mutable.ArrayBuffer.empty[String]
      val files = Map("dir/a.libsonnet" -> "import 'b.libsonnet'", "dir/b.libsonnet" -> "{ ok: true }")
      val importer = new Importer {
        def resolve(docBase: Path, importName: String): Option[Path] = {
          seenDocBases += docBase.asInstanceOf[DummyPath].segments.mkString("/")
          val joined = docBase.asInstanceOf[DummyPath].segments.mkString("/") match {
            case ""   => importName
            case base => s"$base/$importName"
          }
          if (files.contains(joined)) Some(DummyPath(joined.split('/').toIndexedSeq: _*)) else None
        }
        def read(path: Path, binaryData: Boolean): Option[ResolvedFile] =
          throw new RuntimeException(s"unexpected read: $path")
      }
      val preloader = new Preloader(importer)
      val entryPath = DummyPath("dir", "a.libsonnet")
      preloader.add(entryPath, StaticResolvedFile(files("dir/a.libsonnet")), ImportFinder.Kind.Code)
      while (!preloader.isComplete) {
        val batch = preloader.takePendingImports()
        batch.foreach { p =>
          val key = p.path.asInstanceOf[DummyPath].segments.mkString("/")
          preloader.add(p.path, StaticResolvedFile(files(key)), p.kind)
        }
      }
      // Every docBase observed should be the parent dir, never the file path itself.
      assert(seenDocBases.forall(_ == "dir"))
      assert(preloader.loaded.contains(DummyPath("dir", "b.libsonnet")))
    }

    test("does not fail preload on parse errors in discovered files") {
      // A parse error in a discovered file (e.g. behind `if false then import 'bad'`) should
      // not abort preload — Jsonnet evaluation is lazy, the error should only surface if the
      // branch is actually forced.
      val fs = new FakeFs(Map("bad.libsonnet" -> "this is :: not :: jsonnet"))
      val preloader = new Preloader(fs.importer)
      preloader.add(
        DummyPath("entry"),
        StaticResolvedFile("if false then import 'bad.libsonnet' else 1"),
        ImportFinder.Kind.Code
      )
      while (!preloader.isComplete) {
        val batch = preloader.takePendingImports()
        batch.foreach { p =>
          val content = fs.load(p.path, p.binaryData)
          // Parse failure here returns Left, but we deliberately ignore it.
          preloader.add(p.path, content, p.kind)
        }
      }
      // bad.libsonnet was loaded but its parse error did not fail preload.
      assert(fs.readPaths.map(_._1).contains("bad.libsonnet"))
      assert(preloader.loaded.contains(DummyPath("bad.libsonnet")))
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
