package sjsonnet

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("SjsonnetMain")
object SjsonnetMain {
  def createParseCache() = collection.mutable.Map[String, fastparse.Parsed[(Expr, Map[String, Int])]]()
  @JSExport
  def interpret(text: String,
                extVars: js.Any,
                tlaVars: js.Any,
                wd0: String,
                importer: js.Function2[String, String, js.Array[String]]): js.Any = {
    val interp = new Interpreter(
      mutable.Map.empty,
      ujson.WebJson.transform(extVars, ujson.Value).obj.toMap,
      ujson.WebJson.transform(tlaVars, ujson.Value).obj.toMap,
      JsVirtualPath(wd0),
      importer = (wd, path) => {
        importer(wd.asInstanceOf[JsVirtualPath].path, path) match{
          case null => None
          case arr => Some((JsVirtualPath(arr(0)), arr(1)))
        }
      }
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
}