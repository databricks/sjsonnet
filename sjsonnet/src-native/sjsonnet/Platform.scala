package sjsonnet
import java.io.File
object Platform {
  def gzipBytes(s: Array[Byte]): String = {
    throw new Exception("GZip not implemented in Scala Native")
  }
  def gzipString(s: String): String = {
    throw new Exception("GZip not implemented in Scala Native")
  }
  def xzBytes(s: Array[Byte], compressionLevel: Option[Int]): String = {
    throw new Exception("XZ not implemented in Scala Native")
  }
  def xzString(s: String, compressionLevel: Option[Int]): String = {
    throw new Exception("XZ not implemented in Scala Native")
  }
  def yamlToJson(s: String): String = {
    throw new Exception("parseYaml() not implemented in Scala Native")
  }
  def md5(s: String): String = {
    throw new Exception("MD5 not implemented in Scala Native")
  }
  def sha1(s: String): String = {
    throw new Exception("SHA1 not implemented in Scala Native")
  }
  def sha256(s: String): String = {
    throw new Exception("SHA256 not implemented in Scala Native")
  }
  def sha512(s: String): String = {
    throw new Exception("SHA512 not implemented in Scala Native")
  }
  def sha3(s: String): String = {
    throw new Exception("SHA3 not implemented in Scala Native")
  }

  def hashFile(file: File): String = {
    // File hashes in Scala Native are just the file content
    scala.io.Source.fromFile(file).mkString
  }
}
