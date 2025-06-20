package sjsonnet

import sjsonnet.stdlib.NativeRegex

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("SjsonnetMain")
object SjsonnetMain {
  @JSExport
  def interpret(
      text: String,
      extVars: js.Any,
      tlaVars: js.Any,
      wd0: String,
      importResolver: js.Function2[String, String, String],
      importLoader: js.Function2[String, Boolean, Any],
      preserveOrder: Boolean = false): js.Any = {
    val interp = new Interpreter(
      ujson.WebJson.transform(extVars, ujson.Value).obj.toMap.map {
        case (k, ujson.Str(v)) => (k, v)
        case _ => throw new js.JavaScriptException("External variables must be strings")
      },
      ujson.WebJson.transform(tlaVars, ujson.Value).obj.toMap.map {
        case (k, ujson.Str(v)) => (k, v)
        case _                 => throw new js.JavaScriptException("TLA variables must be strings")
      },
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
            case _ => throw new js.JavaScriptException("Loader result must be string or byte array")
          }
      },
      parseCache = new DefaultParseCache,
      settings = new Settings(preserveOrder = preserveOrder),
      std = new Std(nativeFunctions = Map.from(new NativeRegex().functions)).Std
    )
    interp.interpret0(text, JsVirtualPath("(memory)"), ujson.WebJson.Builder) match {
      case Left(msg) => throw new js.JavaScriptException(msg)
      case Right(v)  => v
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
