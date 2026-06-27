package sjsonnet

import utest._

object ImporterTests extends TestSuite {
  def tests: Tests = Tests {
    test("CachedImporter memoizes missing resolves") {
      var resolveCalls = 0
      val importer = new CachedImporter(new Importer {
        def resolve(docBase: Path, importName: String): Option[Path] = {
          resolveCalls += 1
          None
        }
        def read(path: Path, binaryData: Boolean): Option[ResolvedFile] = None
      })

      importer.resolve(DummyPath("root"), "missing.libsonnet") ==> None
      importer.resolve(DummyPath("root"), "missing.libsonnet") ==> None
      resolveCalls ==> 1
    }

    test("CachedImporter memoizes missing reads") {
      var readCalls = 0
      val importer = new CachedImporter(new Importer {
        def resolve(docBase: Path, importName: String): Option[Path] = None
        def read(path: Path, binaryData: Boolean): Option[ResolvedFile] = {
          readCalls += 1
          None
        }
      })

      importer.read(DummyPath("missing.libsonnet"), binaryData = false) ==> None
      importer.read(DummyPath("missing.libsonnet"), binaryData = false) ==> None
      readCalls ==> 1
    }

    test("StaticBinaryResolvedFile protects cached bytes") {
      val original = Array[Byte](1, 2, 3)
      val file = StaticBinaryResolvedFile(original)
      val expected = Array[Byte](1, 2, 3)
      val expectedHash = Platform.hashBytes(expected)
      val fromConstructor = new StaticBinaryResolvedFile(expected)
      val fromFunction = (StaticBinaryResolvedFile: Array[Byte] => StaticBinaryResolvedFile)(expected)

      // Constructor defensively copies, so mutating the input does not affect the file.
      original(0) = 9
      file.readRawBytes().toSeq ==> expected.toSeq
      file.contentHash() ==> expectedHash
      fromConstructor.readRawBytes().toSeq ==> expected.toSeq
      fromFunction.readRawBytes().toSeq ==> expected.toSeq

      // contentHash is cached (lazy val): same reference on repeated calls.
      assert(file.contentHash() eq file.contentHash())

      file.productArity ==> 1
      file.productElement(0).asInstanceOf[Array[Byte]].toSeq ==> expected.toSeq

      StaticBinaryResolvedFile.unapply(null) ==> None

      val copied = file.copy()
      copied.readRawBytes().toSeq ==> expected.toSeq
      copied ==> file
    }
  }
}
