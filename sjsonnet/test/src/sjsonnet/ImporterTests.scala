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
  }
}
