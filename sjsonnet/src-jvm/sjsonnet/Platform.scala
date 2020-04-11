package sjsonnet

import java.io.ByteArrayOutputStream
import java.util.Base64
import org.tukaani.xz.LZMA2Options
import org.tukaani.xz.XZOutputStream

object Platform {
  def gzipBytes(b: Array[Byte]): String = GZIP.gzipBytes(b)
  def gzipString(s: String): String = {
    gzipBytes(s.getBytes())
  }
  def xzBytes(b: Array[Byte]): String = {
    val outputStream: ByteArrayOutputStream = new ByteArrayOutputStream(b.length)
    val xz: XZOutputStream = new XZOutputStream(outputStream, new LZMA2Options())
    xz.write(b)
    xz.close()
    val xzedBase64: String = Base64.getEncoder.encodeToString(outputStream.toByteArray)
    outputStream.close()
    xzedBase64
  }
  def xzString(s: String): String = {
    xzBytes(s.getBytes())
  }
  def md5(s: String): String = {
    Md5.md5(s)
      .map{ b => String.format("%02x", new java.lang.Integer(b & 0xff))}
      .mkString
  }
}
