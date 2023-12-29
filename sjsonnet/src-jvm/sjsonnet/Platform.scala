package sjsonnet

import org.json.JSONObject

import java.io.ByteArrayOutputStream
import java.util.Base64
import java.util.zip.GZIPOutputStream
import org.tukaani.xz.LZMA2Options
import org.tukaani.xz.XZOutputStream
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

object Platform {
  def gzipBytes(b: Array[Byte]): String = {
    val outputStream: ByteArrayOutputStream = new ByteArrayOutputStream(b.length)
    val gzip: GZIPOutputStream = new GZIPOutputStream(outputStream)
    gzip.write(b)
    gzip.close()
    val gzippedBase64: String = Base64.getEncoder.encodeToString(outputStream.toByteArray)
    outputStream.close()
    gzippedBase64
  }
  def gzipString(s: String): String = {
    gzipBytes(s.getBytes())
  }

  /**
   *  Valid compression levels are 0 (no compression) to 9 (maximum compression).
   */
  def xzBytes(b: Array[Byte], compressionLevel: Option[Int]): String = {
    val outputStream: ByteArrayOutputStream = new ByteArrayOutputStream(b.length)
    // Set compression to specified level
    val level = compressionLevel.getOrElse(LZMA2Options.PRESET_DEFAULT)
    val xz: XZOutputStream = new XZOutputStream(outputStream, new LZMA2Options(level))
    xz.write(b)
    xz.close()
    val xzedBase64: String = Base64.getEncoder.encodeToString(outputStream.toByteArray)
    outputStream.close()
    xzedBase64
  }

  def xzString(s: String, compressionLevel: Option[Int]): String = {
    xzBytes(s.getBytes(), compressionLevel)
  }

  def yamlToJson(yamlString: String): String = {
    val yaml: java.util.LinkedHashMap[String, Object] = new Yaml(new Constructor(classOf[java.util.LinkedHashMap[String, Object]])).load(yamlString)
    new JSONObject(yaml).toString()
  }
  def md5(s: String): String = {
    java.security.MessageDigest.getInstance("MD5")
      .digest(s.getBytes("UTF-8"))
      .map{ b => String.format("%02x", new java.lang.Integer(b & 0xff))}
      .mkString
  }
}
