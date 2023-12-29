package sjsonnet

import java.io.{File, FileWriter}
import java.nio.file.Files

import scala.util.Random

import utest._

object BufferedRandomAccessFileTests extends TestSuite {

  // Utility function to create a temporary file with known content
  def createTempFile(content: String): File = {
    val tempFile = Files.createTempFile(null, null).toFile
    val writer = new FileWriter(tempFile)
    try {
      writer.write(content)
    } finally {
      writer.close()
    }
    tempFile
  }

  // Test content
  val smallTestContent = "Hello, World! This is a test file."
  val smallTempFile = createTempFile(smallTestContent)
  val testContent = "Hello, World! This is a test file with enough content to test various buffer sizes."
  val tempFile = createTempFile(testContent)

  // Generate a large test content
  val largeTestContent = Random.alphanumeric.take(100000).mkString // 100k characters
  val largeTempFile = createTempFile(largeTestContent)

  val tests = Tests {
    test("readChar") {
      val bufferedFile = new BufferedRandomAccessFile(smallTempFile.getAbsolutePath, 10)

      // Normal operation
      assert(bufferedFile.readChar(0) == 'H')
      assert(bufferedFile.readChar(7) == 'W')

      // Boundary conditions
      assert(bufferedFile.readChar(smallTestContent.length - 1) == '.')
      intercept[AssertionError] {
        println(bufferedFile.readChar(smallTestContent.length))
      }

      bufferedFile.close()
    }

    test("readString") {
      val bufferedFile = new BufferedRandomAccessFile(smallTempFile.getAbsolutePath, 10)

      // Normal operation
      assert(bufferedFile.readString(0, 5) == "Hello")
      assert(bufferedFile.readString(7, 12) == "World")

      // String within buffer
      assert(bufferedFile.readString(0, 10) == "Hello, Wor")

      // String across buffer boundary
      assert(bufferedFile.readString(5, 15) == ", World! Th")

      // Boundary conditions
      assert(bufferedFile.readString(smallTestContent.length - 5, smallTestContent.length) == "file.")
      intercept[AssertionError] {
        bufferedFile.readString(0, smallTestContent.length + 1)
      }

      bufferedFile.close()
    }

    test("close") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 10)

      bufferedFile.close()
      // Testing further operations after close should be done carefully
      // to avoid undefined behavior.
    }

    test("readChar") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 5)

      // Testing buffer reloading
      assert(bufferedFile.readChar(4) == 'o') // Last char in initial buffer
      assert(bufferedFile.readChar(5) == ',') // Triggers buffer reload

      // Testing reading same character, buffer should not reload
      assert(bufferedFile.readChar(5) == ',')

      bufferedFile.close()
    }

    test("readString") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 10)

      // Test reading across multiple buffer reloads
      assert(bufferedFile.readString(5, 25).contains("World! This is a"))

      bufferedFile.close()
    }

    test("bufferManagement") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 4)

      // Test reading with buffer boundary exactly at string end
      assert(bufferedFile.readString(0, 4) == "Hell")

      // Test reading over the buffer boundary
      assert(bufferedFile.readString(3, 8) == "lo, Wo")

      bufferedFile.close()
    }

    test("errorHandling") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 10)

      // Test reading with invalid indices
      intercept[AssertionError] {
        bufferedFile.readChar(-1)
      }
      intercept[AssertionError] {
        bufferedFile.readString(-1, 2)
      }
      intercept[AssertionError] {
        bufferedFile.readString(2, 1) // 'from' is greater than 'until'
      }

      bufferedFile.close()
    }

    test("varyingBufferSizes") {
      val smallBufferFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 2)
      val mediumBufferFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 15)
      val largeBufferFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 50)

      // Small buffer
      assert(smallBufferFile.readChar(1) == 'e')
      assert(smallBufferFile.readString(0, 4) == "Hell")

      // Medium buffer
      assert(mediumBufferFile.readString(7, 22) == "World! This is a")

      // Large buffer
      assert(largeBufferFile.readString(14, 40).contains("is a test file with enough"))

      smallBufferFile.close()
      mediumBufferFile.close()
      largeBufferFile.close()
    }

    test("edgeCases") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 10)

      // Read string at the very end of the file
      assert(bufferedFile.readString(testContent.length - 6, testContent.length) == "sizes.")

      // Read single character at the end of the file
      assert(bufferedFile.readChar(testContent.length - 1) == '.')

      bufferedFile.close()
    }

    test("sequentialReads") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 20)

      // Sequential reads to test buffer usage
      assert(bufferedFile.readChar(0) == 'H')
      assert(bufferedFile.readString(1, 10) == "ello, Worl")
      assert(bufferedFile.readString(10, 20) == "d! This is ")

      bufferedFile.close()
    }

    test("invalidFile") {
      // Attempting to create a BufferedRandomAccessFile with a non-existent file
      intercept[Exception] {
        val invalidFile = new BufferedRandomAccessFile("nonexistent.txt", 10)
        invalidFile.close()
      }
    }

  // Test content
  val testContent = "Hello, World! This is a test file with various content to thoroughly test the BufferedRandomAccessFile."

    test("bufferReloadsAndEdgeReads") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 15)

      // Reading across buffer reloads
      assert(bufferedFile.readString(14, 29).contains("test file with"))

      // Edge case: Reading exactly at the buffer end
      assert(bufferedFile.readChar(14) == 't') // Last char of the first buffer load

      bufferedFile.close()
    }

    test("readFullBuffer") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 10)

      // Read the full buffer length
      assert(bufferedFile.readString(0, 10) == "Hello, Wor")

      bufferedFile.close()
    }

    test("readBeyondBuffer") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 10)

      // Read beyond buffer length, should trigger multiple buffer loads
      assert(bufferedFile.readString(0, 20).contains("Hello, World! This i"))

      bufferedFile.close()
    }

    test("zeroLengthString") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 10)

      // Test for zero-length string
      assert(bufferedFile.readString(10, 10) == "")

      bufferedFile.close()
    }

    test("concurrentAccess") {
      val bufferedFile1 = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 10)
      val bufferedFile2 = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 10)

      // Concurrent access by two different instances
      assert(bufferedFile1.readChar(0) == 'H')
      assert(bufferedFile2.readString(0, 5) == "Hello")

      bufferedFile1.close()
      bufferedFile2.close()
    }

    test("readAfterClose") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 10)
      bufferedFile.close()

      // Attempt to read after closing the file
      intercept[Exception] {
        bufferedFile.readChar(0)
      }
    }

    test("largeFileReads") {
      val bufferedFile = new BufferedRandomAccessFile(largeTempFile.getAbsolutePath, 1024)

      // Read from various positions in a large file
      assert(bufferedFile.readString(50000, 50010).length == 10)
      assert(bufferedFile.readString(99990, 100000).length == 10)

      bufferedFile.close()
    }

    test("characterEncoding") {
      // This test assumes UTF-8 encoding, but the class may need modifications to handle different encodings
      val testString = "こんにちは" // "Hello" in Japanese
      val tempFileWithEncoding = createTempFile(testString)
      val bufferedFile = new BufferedRandomAccessFile(tempFileWithEncoding.getAbsolutePath, 10)

      // Read a string with non-ASCII characters
      assert(bufferedFile.readString(0, testString.getBytes("UTF-8").length) == testString)

      bufferedFile.close()
      tempFileWithEncoding.delete()
    }

    test("randomAccessReads") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 20)

      // Perform random access reads
      val positions = List(10, 30, 15, 5, 25)
      for (pos <- positions) {
        bufferedFile.readChar(pos) // Just to trigger reads at random positions
      }

      bufferedFile.close()
    }

    test("sequentialAndRandomMixedReads") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 15)

      // Perform sequential and random reads mixed
      assert(bufferedFile.readString(0, 5) == "Hello")
      assert(bufferedFile.readChar(20) == 'i')
      assert(bufferedFile.readString(10, 15) == "World")

      bufferedFile.close()
    }
  }

  // Clean up the temporary files after tests
  override def utestAfterAll(): Unit = {
    tempFile.delete()
    largeTempFile.delete()
  }
}
