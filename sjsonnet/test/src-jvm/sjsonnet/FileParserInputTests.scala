package sjsonnet

import utest._

import java.io.{File, FileOutputStream}
import java.nio.file.Files
import scala.collection.mutable

object FileParserInputTests extends TestSuite {
  private val tempFiles = mutable.ArrayBuffer[File]()

  def createTestFile(content: String): FileParserInput = {
    val tempFile = Files.createTempFile("fileparser-", ".txt").toFile
    tempFiles += tempFile
    val fos = new FileOutputStream(tempFile)
    try {
      fos.write(content.getBytes("UTF-8"))
    } finally {
      fos.close()
    }
    FileParserInput(tempFile)
  }

  val tests: Tests = Tests {
    test("prettyIndex - empty file") {
      val input = createTestFile("")
      val result = input.prettyIndex(0)
      assert(result == "1:1")
    }

    test("prettyIndex - single line without newline") {
      val input = createTestFile("hello world")

      assert(input.prettyIndex(0) == "1:1")
      assert(input.prettyIndex(1) == "1:2")
      assert(input.prettyIndex(6) == "1:7")
      assert(input.prettyIndex(10) == "1:11")
    }

    test("prettyIndex - single line with newline") {
      val input = createTestFile("hello world\n")

      assert(input.prettyIndex(0) == "1:1")
      assert(input.prettyIndex(5) == "1:6")
      assert(input.prettyIndex(11) == "1:12")
    }

    test("prettyIndex - multiple lines with different lengths") {
      val input = createTestFile("a\nbb\nccc\n")

      assert(input.prettyIndex(0) == "1:1")
      assert(input.prettyIndex(1) == "1:2")

      assert(input.prettyIndex(2) == "2:1")
      assert(input.prettyIndex(3) == "2:2")
      assert(input.prettyIndex(4) == "2:3")

      assert(input.prettyIndex(5) == "3:1")
      assert(input.prettyIndex(6) == "3:2")
      assert(input.prettyIndex(7) == "3:3")
      assert(input.prettyIndex(8) == "3:4")
    }

    test("prettyIndex - no trailing newline") {
      val input = createTestFile("line 1\nline 2\nline 3")

      assert(input.prettyIndex(0) == "1:1")
      assert(input.prettyIndex(7) == "2:1")
      assert(input.prettyIndex(14) == "3:1")
      assert(input.prettyIndex(19) == "3:6")
    }

    test("prettyIndex - large file with many lines") {
      val lines = (1 to 1000).map(i => s"Line $i").mkString("\n")
      val input = createTestFile(lines)

      val line1Bytes = 9 * 7
      val line10Bytes = 90 * 8
      val line100Bytes = 400 * 9
      val offset500 = line1Bytes + line10Bytes + line100Bytes

      val result = input.prettyIndex(offset500)
      assert(result == "500:1")
    }
  }

  override def utestAfterAll(): Unit = {
    tempFiles.foreach(_.delete())
    tempFiles.clear()
  }
}
