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
    try {
      gzip.write(b)
      Base64.getEncoder.encodeToString(outputStream.toByteArray)
    } finally {
      gzip.close()
      outputStream.close()
    }
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
    try {
      xz.write(b)
      Base64.getEncoder.encodeToString(outputStream.toByteArray)
    } finally {
      xz.close()
      outputStream.close()
    }
  }

  def xzString(s: String, compressionLevel: Option[Int]): String = {
    xzBytes(s.getBytes(), compressionLevel)
  }

  def yamlToJson(yamlString: String): String = {
    val options = new LoaderOptions()
    val yaml: java.util.LinkedHashMap[String, Object] = new Yaml(new Constructor(classOf[java.util.LinkedHashMap[String, Object]], options)).load(yamlString)
    new JSONObject(yaml).toString()
  }

  private def computeHash(algorithm: String, s: String) = {
    java.security.MessageDigest.getInstance(algorithm)
      .digest(s.getBytes("UTF-8"))
      .map { b => String.format("%02x", Integer.valueOf(b & 0xff)) }
      .mkString
  }

  def md5(s: String): String = computeHash("MD5", s)

  def sha1(s: String): String = computeHash("SHA-1", s)

  def sha256(s: String): String = computeHash("SHA-256", s)

  def sha512(s: String): String = computeHash("SHA-512", s)

  // Same as go-jsonnet https://github.com/google/go-jsonnet/blob/2b4d7535f540f128e38830492e509a550eb86d57/builtins.go#L959
  def sha3(s: String): String = computeHash("SHA3-512", s)

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

    hash.getValue.toString
  }
}
