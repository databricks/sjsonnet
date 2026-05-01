package sjsonnet

import sjsonnet.stdlib.NativeRegex

import scala.collection.mutable
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.Thenable.Implicits._
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

  /** Convert the value returned by a JS import loader into a [[ResolvedFile]]. */
  private def toResolvedFile(path: String, value: Any, binaryData: Boolean): ResolvedFile =
    value match {
      case s: String        => StaticResolvedFile(s)
      case arr: Array[Byte] => StaticBinaryResolvedFile(arr)
      case other            =>
        toBytesFromJs(other) match {
          case Some(bytes) => StaticBinaryResolvedFile(bytes)
          case None        =>
            val msg =
              s"Import loader for '$path' must return a string or byte array, got: ${
                  if (other == null) "null" else other.getClass.getName
                }"
            js.Dynamic.global.console.error(msg)
            throw js.JavaScriptException(msg)
        }
    }

  /** Build the parent importer used during preload (only its `resolve` is called). */
  private def jsResolveImporter(importResolver: js.Function2[String, String, String]): Importer =
    new Importer {
      def resolve(docBase: Path, importName: String): Option[Path] =
        importResolver(docBase.asInstanceOf[JsVirtualPath].path, importName) match {
          case null => None
          case s    => Some(JsVirtualPath(s))
        }
      def read(path: Path, binaryData: Boolean): Option[ResolvedFile] =
        throw new RuntimeException(
          s"Importer.read should not be called during async preload (path=$path)"
        )
    }

  private def parseStringMap(label: String, value: js.Any): Map[String, String] =
    try {
      ujson.WebJson.transform(value, ujson.Value).obj.toMap.map {
        case (k, ujson.Str(v)) => (k, v)
        case (k, _)            =>
          throw js.JavaScriptException(s"$label '$k' must be a string value, got non-string")
      }
    } catch {
      case e: js.JavaScriptException => throw e
      case e: Exception              =>
        val msg = s"Failed to parse ${label.toLowerCase}: ${e.getMessage}"
        js.Dynamic.global.console.error(msg, e.asInstanceOf[js.Any])
        throw js.JavaScriptException(msg)
    }

  private def runInterpret(
      text: String,
      parsedExtVars: Map[String, String],
      parsedTlaVars: Map[String, String],
      wd0: String,
      importer: Importer,
      preserveOrder: Boolean): js.Any = {
    val interp = new Interpreter(
      parsedExtVars,
      parsedTlaVars,
      JsVirtualPath(wd0),
      importer,
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
      val parsedExtVars = parseStringMap("External variable", extVars)
      val parsedTlaVars = parseStringMap("Top-level argument", tlaVars)

      val importer = new Importer {
        def resolve(docBase: Path, importName: String): Option[Path] =
          importResolver(docBase.asInstanceOf[JsVirtualPath].path, importName) match {
            case null => None
            case s    => Some(JsVirtualPath(s))
          }
        def read(path: Path, binaryData: Boolean): Option[ResolvedFile] =
          Some(
            toResolvedFile(
              path.asInstanceOf[JsVirtualPath].path,
              importLoader(path.asInstanceOf[JsVirtualPath].path, binaryData),
              binaryData
            )
          )
      }

      runInterpret(text, parsedExtVars, parsedTlaVars, wd0, importer, preserveOrder)
    } catch {
      case e: js.JavaScriptException => throw e
      case e: Exception              =>
        val msg = s"Sjsonnet internal error: ${e.getClass.getName}: ${e.getMessage}"
        js.Dynamic.global.console.error(msg, e.asInstanceOf[js.Any])
        throw js.JavaScriptException(msg)
    }
  }

  /**
   * Async variant of [[interpret]]. Accepts an `importLoader` that returns a `Promise` of the file
   * contents, and returns a `Promise` resolving to the rendered output.
   *
   * Implementation: imports are statically discovered by parsing each file's AST, loaded
   * concurrently via the user-supplied async loader, and inserted into a cache. Once the transitive
   * closure is loaded, evaluation runs synchronously against the cache.
   */
  @JSExport
  def interpretAsync(
      text: String,
      extVars: js.Any,
      tlaVars: js.Any,
      wd0: String,
      importResolver: js.Function2[String, String, String],
      importLoader: js.Function2[String, Boolean, js.Promise[Any]],
      preserveOrder: Boolean = false): js.Promise[js.Any] = {
    try {
      val parsedExtVars = parseStringMap("External variable", extVars)
      val parsedTlaVars = parseStringMap("Top-level argument", tlaVars)

      val parentImporter = jsResolveImporter(importResolver)
      val preloader = new Preloader(parentImporter)
      val entryPath = JsVirtualPath("(memory)")

      preloader.add(entryPath, StaticResolvedFile(text), ImportFinder.Kind.Code) match {
        case Left(err) => return js.Promise.reject(err.getMessage)
        case Right(_)  =>
      }

      def loadOne(p: Preloader.Pending): Future[Unit] = {
        val pathStr = p.path.asInstanceOf[JsVirtualPath].path
        val promise = importLoader(pathStr, p.binaryData)
        // implicit Thenable.Implicits converts Promise[Any] to Future[Any]
        (promise: Future[Any]).map { value =>
          val resolved = toResolvedFile(pathStr, value, p.binaryData)
          preloader.add(p.path, resolved, p.kind) match {
            case Left(err) => throw js.JavaScriptException(err.getMessage)
            case Right(_)  => ()
          }
        }
      }

      def loop(): Future[Unit] = {
        val batch = preloader.takePendingImports()
        if (batch.isEmpty) Future.successful(())
        else Future.sequence(batch.map(loadOne)).flatMap(_ => loop())
      }

      val result: Future[js.Any] = loop().map { _ =>
        runInterpret(
          text,
          parsedExtVars,
          parsedTlaVars,
          wd0,
          preloader.importer,
          preserveOrder
        )
      }
      result.toJSPromise
    } catch {
      case e: js.JavaScriptException => js.Promise.reject(e.exception)
      case e: Exception              =>
        val msg = s"Sjsonnet internal error: ${e.getClass.getName}: ${e.getMessage}"
        js.Dynamic.global.console.error(msg, e.asInstanceOf[js.Any])
        js.Promise.reject(msg)
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
