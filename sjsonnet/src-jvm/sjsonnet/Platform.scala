package sjsonnet

import org.json.JSONObject

import scala.collection.JavaConverters._

import java.io.ByteArrayOutputStream
import java.util.{Base64, Collections, LinkedHashMap, Map => JMap}
import java.util.zip.GZIPOutputStream

import scala.reflect.runtime.universe._

import org.tukaani.xz.LZMA2Options
import org.tukaani.xz.XZOutputStream
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor
import it.unimi.dsi.fastutil.objects.{Object2ObjectLinkedOpenHashMap, Object2BooleanLinkedOpenHashMap}
import it.unimi.dsi.fastutil.objects.Object2BooleanMaps
import it.unimi.dsi.fastutil.objects.Object2ObjectMaps

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
    ""
    /*
    val outputStream: ByteArrayOutputStream = new ByteArrayOutputStream(b.length)
    val xz: XZOutputStream = new XZOutputStream(outputStream, new LZMA2Options())
    xz.write(b)
    xz.close()
    val xzedBase64: String = Base64.getEncoder.encodeToString(outputStream.toByteArray)
    outputStream.close()
    xzedBase64
    */
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
  /*
  def compactHashMap[K, V: TypeTag](map: LinkedHashMap[K, V]): JMap[K, V] = {
    val res = typeOf[V] match {
      case t if t =:= typeOf[java.lang.Boolean] => "It's an Int"
        val size = map.size()
        if (size == 0) {
          Object2BooleanMaps.emptyMap[K]()
        } else if (size == 1) {
          // Return a singleton map
          val entry = map.entrySet().iterator().next()
          Object2BooleanMaps.singleton(entry.getKey(), entry.getValue().asInstanceOf[java.lang.Boolean])
        } else {
          new Object2BooleanLinkedOpenHashMap[K](map.asInstanceOf[LinkedHashMap[K, java.lang.Boolean]])
        }
      case _ =>
        val size = map.size()
        if (size == 0) {
          Object2ObjectMaps.emptyMap[K, V]()
        } else if (size == 1) {
          val entry = map.entrySet().iterator().next()
          Object2ObjectMaps.singleton(entry.getKey(), entry.getValue())
        } else {
          new Object2ObjectLinkedOpenHashMap[K, V](map)
        }
    }
    res.asInstanceOf[JMap[K, V]]
  }
  */

  import scala.collection.immutable.VectorMap
  // import scala.collection.mutable.{LinkedHashMap => SLinkedHashMap}
  // import scala.collection.immutable.HashMap

  def compactHashMap[K, V](map: LinkedHashMap[K, V]): JMap[K, V] = {
    val size = map.size()
    if (size == 0) {
      Collections.emptyMap[K, V]()
    } else {
      VectorMap(map.asScala.toSeq: _*).asJava
    }
  }

 /*
  import scala.collection.JavaConverters._
  import com.google.common.collect.ImmutableMap
  import java.util.{Map => JMap}

  def compactHashMap[K, V](map: LinkedHashMap[K, V]): JMap[K, V] = {
    val size = map.size
    if (size == 0) {
      java.util.Collections.emptyMap[K, V]()
    } else {
      val builder = ImmutableMap.builder[K, V]()
      map.forEach { case (key, value) => builder.put(key, value) }
      builder.build()
    }
  }
  */
}
