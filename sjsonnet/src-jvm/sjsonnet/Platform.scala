package sjsonnet

import org.json.JSONObject

import java.io.ByteArrayOutputStream
import java.util.{Base64, Collections, LinkedHashMap, Map => JMap}
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

  /**
   * Compacts a [[LinkedHashMap]] as small as possible. There are 3 different cases:
   * 1. If the map is empty, return [[Collections.emptyMap]]. This is an immutable singleton - and
   *    thus is space efficient.
   * 2. If the map has 1 element, so we return an immutable single element map.
   * 3. Otherwise, we return a new [[LinkedHashMap]] with the same contents as the original map
   *    that's been compacted to the smallest size possible - subject to the desired load factor:
   *    (num elements) / load-factor + 1. The default load factor is 0.75.
   *
   * Return types from (1) & (2) are [[java.util.Map]] - where as 3 is a [[LinkedHashMap]]. So we
   * need to pick the common type - which is [[java.util.Map]].
   *
   * All returned maps preserve the insertion order of the original map. No map returned from this
   * method should be mutated.
   */
  def compactHashMap[K, V](map: LinkedHashMap[K, V]): JMap[K, V] = {
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

      // This constructor's doc: Constructs an insertion-ordered LinkedHashMap instance with the
      // same mappings as the specified map. The LinkedHashMap instance is created with a default
      // load factor (0.75) and an initial capacity sufficient to hold the mappings in the specified
      // map.
      val newMap = new LinkedHashMap[K, V](map)
      newMap
    }
  }
}
