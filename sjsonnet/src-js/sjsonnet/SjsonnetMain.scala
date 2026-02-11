package sjsonnet

import sjsonnet.stdlib.NativeRegex

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array, Uint8Array}

@JSExportTopLevel("SjsonnetMain")
object SjsonnetMain {

  /**
   * Convert JS-native binary types to Array[Byte]. Handles Uint8Array, Int8Array, ArrayBuffer, and
   * plain JS number arrays.
   */
  private def toBytesFromJs(value: Any): Option[Array[Byte]] = value match {
    case uint8: Uint8Array =>
      val bytes = new Array[Byte](uint8.length)
      var i = 0
      while (i < uint8.length) { bytes(i) = uint8(i).toByte; i += 1 }
      Some(bytes)
    case int8: Int8Array =>
      val bytes = new Array[Byte](int8.length)
      var i = 0
      while (i < int8.length) { bytes(i) = int8(i); i += 1 }
      Some(bytes)
    case buf: ArrayBuffer =>
      val uint8 = new Uint8Array(buf)
      val bytes = new Array[Byte](uint8.length)
      var i = 0
      while (i < uint8.length) { bytes(i) = uint8(i).toByte; i += 1 }
      Some(bytes)
    case jsArr: js.Array[_] =>
      if (jsArr.isEmpty) Some(Array.emptyByteArray)
      else
        try {
          val bytes = new Array[Byte](jsArr.length)
          var i = 0
          while (i < jsArr.length) {
            // JS numbers arrive as Double in Scala.js; convert via dynamic to avoid type mismatch
            bytes(i) = jsArr(i).asInstanceOf[js.Dynamic].asInstanceOf[Double].toByte
            i += 1
          }
          Some(bytes)
        } catch {
          case _: Exception => None
        }
    case _ => None
  }

  @JSExport
  def interpret(
      text: String,
      extVars: js.Any,
      tlaVars: js.Any,
      wd0: String,
      importResolver: js.Function2[String, String, String],
      importLoader: js.Function2[String, Boolean, Any],
      preserveOrder: Boolean = false): js.Any = {
    try {
      val parsedExtVars =
        try {
          ujson.WebJson.transform(extVars, ujson.Value).obj.toMap.map {
            case (k, ujson.Str(v)) => (k, v)
            case (k, _)            =>
              throw js.JavaScriptException(
                s"External variable '$k' must be a string value, got non-string"
              )
          }
        } catch {
          case e: js.JavaScriptException => throw e
          case e: Exception              =>
            val msg = s"Failed to parse external variables: ${e.getMessage}"
            js.Dynamic.global.console.error(msg, e.asInstanceOf[js.Any])
            throw js.JavaScriptException(msg)
        }

      val parsedTlaVars =
        try {
          ujson.WebJson.transform(tlaVars, ujson.Value).obj.toMap.map {
            case (k, ujson.Str(v)) => (k, v)
            case (k, _)            =>
              throw js.JavaScriptException(
                s"Top-level argument '$k' must be a string value, got non-string"
              )
          }
        } catch {
          case e: js.JavaScriptException => throw e
          case e: Exception              =>
            val msg = s"Failed to parse top-level arguments: ${e.getMessage}"
            js.Dynamic.global.console.error(msg, e.asInstanceOf[js.Any])
            throw js.JavaScriptException(msg)
        }

      val interp = new Interpreter(
        parsedExtVars,
        parsedTlaVars,
        JsVirtualPath(wd0),
        new Importer {
          def resolve(docBase: Path, importName: String): Option[Path] =
            importResolver(docBase.asInstanceOf[JsVirtualPath].path, importName) match {
              case null => None
              case s    => Some(JsVirtualPath(s))
            }
          def read(path: Path, binaryData: Boolean): Option[ResolvedFile] =
            importLoader(path.asInstanceOf[JsVirtualPath].path, binaryData) match {
              case s: String        => Some(StaticResolvedFile(s))
              case arr: Array[Byte] => Some(StaticBinaryResolvedFile(arr))
              case other            =>
                // Handle JS-native binary types: Uint8Array, ArrayBuffer, or plain JS number[]
                toBytesFromJs(other) match {
                  case Some(bytes) => Some(StaticBinaryResolvedFile(bytes))
                  case None        =>
                    val msg =
                      s"Import loader for '${path}' must return a string or byte array, got: ${
                          if (other == null) "null" else other.getClass.getName
                        }"
                    js.Dynamic.global.console.error(msg)
                    throw js.JavaScriptException(msg)
                }
            }
        },
        parseCache = new DefaultParseCache,
        settings = new Settings(preserveOrder = preserveOrder),
        std =
          new sjsonnet.stdlib.StdLibModule(nativeFunctions = Map.from(NativeRegex.functions)).module
      )
      interp.interpret0(text, JsVirtualPath("(memory)"), ujson.WebJson.Builder) match {
        case Left(msg) =>
          js.Dynamic.global.console.error("Sjsonnet evaluation error:", msg)
          throw js.JavaScriptException(msg)
        case Right(v) => v
      }
    } catch {
      case e: js.JavaScriptException => throw e
      case e: Exception              =>
        val msg = s"Sjsonnet internal error: ${e.getClass.getName}: ${e.getMessage}"
        js.Dynamic.global.console.error(msg, e.asInstanceOf[js.Any])
        throw js.JavaScriptException(msg)
    }
  }
}

final case class JsVirtualPath(path: String) extends Path {
  def relativeToString(p: Path): String = p match {
    case other: JsVirtualPath if path.startsWith(other.path) =>
      path.drop(other.path.length).stripPrefix("/")
    case _ => path
  }

  def debugRead(): Option[String] = None

  def parent(): Path = JsVirtualPath(path.split('/').dropRight(1).mkString("/"))

  def segmentCount(): Int = path.split('/').length

  def last: String = path.split('/').last

  def /(s: String): Path = {
    val aStripped = path.stripSuffix("/")
    val bStripped = s.stripPrefix("/")
    if (aStripped.isEmpty)
      JsVirtualPath(bStripped)
    else if (bStripped.isEmpty)
      JsVirtualPath(aStripped)
    else
      JsVirtualPath(aStripped + "/" + bStripped)
  }

  def renderOffsetStr(
      offset: Int,
      loadedFileContents: mutable.HashMap[Path, Array[Int]]): String = {
    path + ":" + offset
  }

  override def toString: String = path
}
