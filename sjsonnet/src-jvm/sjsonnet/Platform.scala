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

  /*
  type Object2ObjectLinkedMap[K, V] = Object2ObjectLinkedOpenHashMap[K, V]
  type Object2BooleanLinkedMap[K] = Object2BooleanLinkedOpenHashMap[K]

  def newObjectToObjectLinkedHashMap[K, V](): Object2ObjectLinkedMap[K, V] = {
    // Use fastutils linked hash map
    new Object2ObjectLinkedOpenHashMap[K, V]()
  }

  def newObjectToBooleanLinkedHashMap[K](): Object2BooleanLinkedMap[K] = {
    // Use fastutils linked hash map
    new Object2BooleanLinkedOpenHashMap[K]()
  }
  */

  type Object2ObjectLinkedMap[K, V] = java.util.Map[K, V]
  type Object2BooleanLinkedMap[K] = java.util.Map[K, Boolean]

  def newObjectToObjectLinkedHashMap[K, V](): Object2ObjectLinkedMap[K, V] = {
    new LinkedHashMap[K, V]()
  }

  def newObjectToBooleanLinkedHashMap[K](): Object2BooleanLinkedMap[K] = {
    new LinkedHashMap[K, Boolean]()
  }

  def compactHashMap[K, V](map: Object2ObjectLinkedMap[K, V]): Object2ObjectLinkedMap[K, V] = {
    val size = map.size()
    if (size == 0) {
      // Return an empty map
      Collections.emptyMap[K, V]
    } else if (size == 1) {
      // Return a singleton map
      val entry = map.entrySet().iterator().next()
      Collections.singletonMap[K, V](entry.getKey(), entry.getValue())
    } else {
      // From LinkedHashMap javadoc: "This implementation spares its clients from the unspecified,
      // generally chaotic ordering provided by HashMap (and Hashtable), without incurring the
      // increased cost associated with TreeMap. It can be used to produce a copy of a map that has
      // the same order as the original, regardless of the original map's implementation:
      //  void foo(Map m) {
      //      Map copy = new LinkedHashMap(m);
      //      ...
      //  }
      val newMap = new LinkedHashMap[K, V](map)
      newMap
    }
  }
}
