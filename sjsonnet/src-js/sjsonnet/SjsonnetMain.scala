package sjsonnet

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("SjsonnetMain")
object SjsonnetMain {
  @JSExport
  def interpret(text: String,
                extVars: js.Any,
                tlaVars: js.Any,
                wd0: String,
                importResolver: js.Function2[String, String, String],
                importLoader: js.Function1[String, String],
                preserveOrder: Boolean = false): js.Any = {
    val interp = new Interpreter(
      ujson.WebJson.transform(extVars, ujson.Value).obj.toMap.map{case (k, ujson.Str(v)) => (k, v)},
      ujson.WebJson.transform(tlaVars, ujson.Value).obj.toMap.map{case (k, ujson.Str(v)) => (k, v)},
      JsVirtualPath(wd0),
      new Importer {
        def resolve(docBase: Path, importName: String): Option[Path] =
          importResolver(docBase.asInstanceOf[JsVirtualPath].path, importName) match {
            case null => None
            case s => Some(JsVirtualPath(s))
          }
        def read(path: Path): Option[String] =
          Option(importLoader(path.asInstanceOf[JsVirtualPath].path))
      },
      parseCache = new DefaultParseCache,
      new Settings(preserveOrder = preserveOrder),
    )
    interp.interpret0(text, JsVirtualPath("(memory)"), ujson.WebJson.Builder) match{
      case Left(msg) => throw new js.JavaScriptException(msg)
      case Right(v) => v
    }
  }
}


case class JsVirtualPath(path: String) extends Path{
  def relativeToString(p: Path): String = p match{
    case other: JsVirtualPath if path.startsWith(other.path) => path.drop(other.path.length)
    case _ => path
  }

  def debugRead(): Option[String] = None

  def parent(): Path = JsVirtualPath(path.split('/').dropRight(1).mkString("/"))

  def segmentCount(): Int = path.split('/').length

  def last: String = path.split('/').last

  def /(s: String): Path = JsVirtualPath(path + "/" + s)

  def renderOffsetStr(offset: Int, loadedFileContents: mutable.HashMap[Path, Array[Int]]): String = {
    path + ":" + offset
  }
}