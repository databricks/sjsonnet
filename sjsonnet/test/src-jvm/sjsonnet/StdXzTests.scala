package sjsonnet

import java.util.Base64
import java.io.{ByteArrayInputStream,ByteArrayOutputStream}

import org.tukaani.xz.XZInputStream

import utest._
import TestUtils.eval

object StdXzTests extends TestSuite {
  val tests = Tests {
    test("xz"){
      testXzRoundTripString("hi")
      testXzRoundTripString("hi")
      testXzRoundTripString("hello world")
      testXzRoundTripString("hello world")
      eval("""std.xz([1, 2], compressionLevel = 0)""")
      eval("""std.xz("hi", compressionLevel = 1)""")
      val ex = intercept[Throwable] {
        // Compression level 10 is invalid
        eval("""std.xz("hi", 10)""")
      }
      assert(ex.getMessage.contains("Unsupported preset: 10"))
    }
  }

  private def testXzRoundTripString(v: String) = {
    val compressed = eval(s"""std.xz("$v")""").str
    val decompressed = new String(decompressXZBase64(compressed), "UTF-8")
    assert(decompressed == v)
  }

  private def decompressXZBase64(base64Str: String): Array[Byte] = {
    // Decode the Base64 string to bytes
    val xzBytes = Base64.getDecoder.decode(base64Str)

    // Set up streams for XZ decompression
    val inputStream = new ByteArrayInputStream(xzBytes)
    val xzInputStream = new XZInputStream(inputStream)
    val outputStream = new ByteArrayOutputStream()

    // Buffer for reading from the decompression stream
    val buffer = new Array[Byte](1024)
    var bytesRead = 0

    // Read from the stream and write to the output stream
    while ({ bytesRead = xzInputStream.read(buffer); bytesRead } != -1) {
      outputStream.write(buffer, 0, bytesRead)
    }

    // Convert the decompressed bytes to a string
    outputStream.toByteArray()
  }
}

