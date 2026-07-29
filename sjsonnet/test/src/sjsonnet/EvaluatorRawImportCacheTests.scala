package sjsonnet

import utest._

object EvaluatorRawImportCacheTests extends TestSuite {
  def tests: Tests = Tests {
    test("importstr and importbin values are cached independently by path") {
      var stringReads = 0
      var binaryReads = 0
      val binaryContent = Array[Byte](1)

      val importer = new Importer {
        def resolve(docBase: Path, importName: String): Option[Path] =
          Some(DummyPath(importName))

        def read(path: Path, binaryData: Boolean): Option[ResolvedFile] =
          if (binaryData) {
            Some(new ResolvedFile {
              def getParserInput(): fastparse.ParserInput =
                throw new NotImplementedError("not used by importbin")
              def readString(): String =
                throw new NotImplementedError("not used by importbin")
              def contentHash(): String = "binary"
              def readRawBytes(): Array[Byte] = {
                binaryReads += 1
                binaryContent
              }
            })
          } else {
            Some(new ResolvedFile {
              def getParserInput(): fastparse.ParserInput =
                throw new NotImplementedError("not used by importstr")
              def readString(): String = {
                stringReads += 1
                s"text-$stringReads"
              }
              def contentHash(): String = "string"
              def readRawBytes(): Array[Byte] =
                throw new NotImplementedError("not used by importstr")
            })
          }
      }

      val interpreter = new Interpreter(
        Map.empty,
        Map.empty,
        DummyPath("root"),
        importer,
        parseCache = new DefaultParseCache
      )
      val result = interpreter.interpret(
        """[
          |  importstr "same",
          |  importstr "same",
          |  importbin "same",
          |  importbin "same",
          |]""".stripMargin,
        DummyPath("root", "main.jsonnet")
      )

      result ==> Right(ujson.Arr("text-1", "text-1", ujson.Arr(1), ujson.Arr(1)))
      stringReads ==> 1
      binaryReads ==> 1

      binaryContent(0) = 9
      interpreter.interpret(
        """importbin "same"""",
        DummyPath("root", "second.jsonnet")
      ) ==> Right(ujson.Arr(1))
      binaryReads ==> 1
    }
  }
}
