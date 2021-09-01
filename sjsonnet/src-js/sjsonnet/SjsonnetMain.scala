package sjsonnet

import fastparse.Parsed

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

// [DISCUSS]
// -- Not used in Universe or Sjsonnet however internally in /build.sbt:
//        libraryDependencies ++=
//        Seq(
//           "com.github.ben-manes.caffeine" % "caffeine"
//        )
//    which is used in universe: jars=[“:com.github.ben-manes.caffeine__caffeine__2.3.4.jar”])
// -- Needs to be cloned in databricks repository | original: https://github.com/blemale/scaffeine
import com.github.blemale.scaffeine.{ Cache, Scaffeine }
import scala.concurrent.duration._

// [DISCUSS]
// -- Single instance shared, hence declared object (and not class)
// -- Regarding thread safe, it internally uses caffeine which is thread safe so this would address our concern of cache
//    being shared among multiple threads
// -- No other implementation should be required on the universe side in SjsonnetWorker
object ParseCacheCaffeine {

  val maxNumberOfEntries = 10000

  val cache: Cache[(Path, String), Either[String, (Expr, FileScope)]] =
    Scaffeine()
      .recordStats()
      .maximumSize(maxNumberOfEntries)        // Size based eviction using "Window TinyLfu" (high hit rate and low memory footprint)
      .build[(Path, String), Either[String, (Expr, FileScope)]]()

  // [This functionality is needed]
  // From docs: If key k is defined in map ms, return its associated value.
  // Otherwise, update ms with the mapping k -> d and return d.
  def getOrElseUpdate(path: Path, txt: String) = {

    // From docs: Returns the value associated with `key` in this cache, obtaining that value from
    // `mappingFunction` if necessary. This method provides a simple substitute for the
    // conventional "if cached, return; otherwise create, cache and return" pattern.
    // def get(key: K, mappingFunction: K => V): V = underlying.get(key, mappingFunction.asJava)
    cache.get((path, txt), {
      val parsed = fastparse.parse(txt, new Parser(path).document(_)) match {
        case f @ Parsed.Failure(_, _, _) =>
          val traced = f.trace()
          val pos = new Position(new FileScope(path), traced.index)
          Left(new ParseError(traced.msg).addFrame(pos))
        case Parsed.Success(r, _) => Right(r)
      }
      parsed.flatMap { case (e, fs) => process(e, fs) }
    })
  }

  // Some other methods for benchmarking and profiling
  // -- Why need other methods for benchmarking if only getOrElseUpdate is being used?
  //    Shouldn't memory usage, response time etc.. be mesaured for both cache and compared?
}

@JSExportTopLevel("SjsonnetMain")
object SjsonnetMain {
  def createParseCache() = collection.mutable.HashMap[(Path, String), Either[String, (Expr, FileScope)]]()

  // [DISCUSS] - other places can do .getOrElseUpdate() like before
  def createParseCacheSizeLimit() = {
    ParseCacheCaffeine
  }

  @JSExport
  def interpret(text: String,
                extVars: js.Any,
                tlaVars: js.Any,
                wd0: String,
                importResolver: js.Function2[String, String, String],
                importLoader: js.Function1[String, String],
                preserveOrder: Boolean = false): js.Any = {
    val interp = new Interpreter(
      ujson.WebJson.transform(extVars, ujson.Value).obj.toMap,
      ujson.WebJson.transform(tlaVars, ujson.Value).obj.toMap,
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
      preserveOrder,
      parseCache = createParseCache()
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
