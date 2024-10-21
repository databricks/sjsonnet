package sjsonnet

import org.json.JSONObject

import java.io.{BufferedInputStream, ByteArrayOutputStream, File, FileInputStream}
import java.util.Base64
import java.util.zip.GZIPOutputStream
import net.jpountz.xxhash.{StreamingXXHash64, XXHash64, XXHashFactory}
import org.tukaani.xz.LZMA2Options
import org.tukaani.xz.XZOutputStream
import org.yaml.snakeyaml.{LoaderOptions, Yaml}
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
   * Valid compression levels are 0 (no compression) to 9 (maximum compression).
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
    val options = new LoaderOptions()
    val yaml: java.util.LinkedHashMap[String, Object] = new Yaml(new Constructor(classOf[java.util.LinkedHashMap[String, Object]], options)).load(yamlString)
    new JSONObject(yaml).toString()
  }

  def md5(s: String): String = {
    java.security.MessageDigest.getInstance("MD5")
      .digest(s.getBytes("UTF-8"))
      .map { b => String.format("%02x", Integer.valueOf(b & 0xff)) }
      .mkString
  }

  private val xxHashFactory = XXHashFactory.fastestInstance()

  def hashFile(file: File): String = {
    val buffer = new Array[Byte](8192)
    val hash: StreamingXXHash64 = xxHashFactory.newStreamingHash64(0)

    val fis = new FileInputStream(file)
    val bis = new BufferedInputStream(fis)

    try {
      var bytesRead = bis.read(buffer)
      while (bytesRead != -1) {
        hash.update(buffer, 0, bytesRead)
        bytesRead = bis.read(buffer)
      }
    } finally {
      bis.close()
      fis.close()
    }

    hash.getValue().toString
  }
}
