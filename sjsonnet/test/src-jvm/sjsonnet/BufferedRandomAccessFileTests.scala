package sjsonnet

import utest._

import java.io.{File, FileOutputStream}
import java.nio.file.Files
import scala.util.Random

object BufferedRandomAccessFileTests extends TestSuite {
  // Utility function to create a temporary file with known content
  def createTempFile(content: String): File = {
    val tempFile = Files.createTempFile(null, null).toFile
    val fos = new FileOutputStream(tempFile)
    try {
      fos.write(content.getBytes("UTF-8"))
    } finally {
      fos.close()
    }
    tempFile
  }

  // Test content and large test content
  val testContent =
    "Hello, World! This is a test file with various content to thoroughly test the BufferedRandomAccessFile."
  val largeTestContent: String = Random.alphanumeric.take(100000).mkString // 100k characters
  val tempFile: File = createTempFile(testContent)
  val largeTempFile: File = createTempFile(largeTestContent)

  val tests: Tests = Tests {
    test("readChar") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 10)

      // Normal operation
      assert(bufferedFile.readChar(0) == 'H')
      assert(bufferedFile.readChar(7) == 'W')

      // Boundary conditions
      assert(bufferedFile.readChar(testContent.length - 1) == '.')
      assertThrows[IndexOutOfBoundsException] {
        bufferedFile.readChar(testContent.length)
      }

      bufferedFile.close()
    }

    test("readString") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 10)

      // Normal operation
      assert(bufferedFile.readString(0, 5) == "Hello")
      assert(bufferedFile.readString(7, 12) == "World")

      // String within buffer
      assert(bufferedFile.readString(0, 10) == "Hello, Wor")

      // String across buffer boundary
      assert(bufferedFile.readString(5, 15) == ", World! T")

      // Boundary conditions
      assert(bufferedFile.readString(testContent.length - 5, testContent.length) == "File.")
      assert(bufferedFile.readString(0, testContent.length) == testContent)
      assertThrows[IndexOutOfBoundsException] {
        bufferedFile.readString(0, testContent.length + 1)
      }

      bufferedFile.close()
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
      assert(bufferedFile.readString(3, 8) == "lo, W")

      bufferedFile.close()
    }

    test("errorHandling") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 10)

      // Test reading with invalid indices
      assertThrows[java.io.IOException] {
        bufferedFile.readChar(-1)
      }
      assertThrows[java.io.IOException] {
        bufferedFile.readString(-1, 2)
      }
      assertThrows[IndexOutOfBoundsException] {
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
      assert(mediumBufferFile.readString(7, 22) == "World! This is ")

      // Large buffer
      assert(largeBufferFile.readString(14, 40).contains("This is a test file with v"))

      smallBufferFile.close()
      mediumBufferFile.close()
      largeBufferFile.close()
    }

    test("edgeCases") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 10)

      // Read string at the very end of the file
      assert(bufferedFile.readString(testContent.length - 6, testContent.length) == "sFile.")

      // Read single character at the end of the file
      assert(bufferedFile.readChar(testContent.length - 1) == '.')

      bufferedFile.close()
    }

    test("sequentialReads") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 20)

      // Sequential reads to test buffer usage
      assert(bufferedFile.readChar(0) == 'H')
      assert(bufferedFile.readString(1, 10) == "ello, Wor")
      assert(bufferedFile.readString(10, 20) == "ld! This i")

      bufferedFile.close()
    }

    test("invalidFile") {
      // Attempting to create a BufferedRandomAccessFile with a non-existent file
      assertThrows[Exception] {
        val invalidFile = new BufferedRandomAccessFile("nonexistent.txt", 10)
        invalidFile.close()
      }
    }

    test("bufferReloadsAndEdgeReads") {
      val bufferedFile = new BufferedRandomAccessFile(tempFile.getAbsolutePath, 15)

      // Reading across buffer reloads
      assert(bufferedFile.readString(14, 29) == "This is a test ")

      // Edge case: Reading exactly at the buffer end
      assert(bufferedFile.readChar(14) == 'T') // Last char of the first buffer load

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
      assertThrows[Exception] {
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
      assert(bufferedFile.readChar(20) == 's')
      assert(bufferedFile.readString(10, 15) == "ld! T")

      bufferedFile.close()
    }
  }

  // Clean up the temporary files after tests
  override def utestAfterAll(): Unit = {
    tempFile.delete()
    largeTempFile.delete()
  }
}
