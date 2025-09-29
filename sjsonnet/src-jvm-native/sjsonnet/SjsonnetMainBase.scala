package sjsonnet

import java.io.{
  BufferedOutputStream,
  InputStream,
  OutputStreamWriter,
  PrintStream,
  StringWriter,
  Writer
}
import java.nio.charset.StandardCharsets
import java.nio.file.NoSuchFileException
import scala.annotation.unused
import scala.util.Try

object SjsonnetMainBase {
  def resolveImport(
      searchRoots0: Seq[Path], // Evaluated in order, first occurrence wins
      allowedInputs: Option[Set[os.Path]] = None,
      debugImporter: Boolean = false): Importer =
    new Importer {
      def resolve(docBase: Path, importName: String): Option[Path] =
        (docBase +: searchRoots0)
          .flatMap(base =>
            os.FilePath(importName) match {
              case r: os.SubPath => Some(base.asInstanceOf[OsPath].p / r)
              case r: os.RelPath =>
                if (r.ups > base.segmentCount()) None
                else Some(base.asInstanceOf[OsPath].p / r)
              case a: os.Path => Some(a)
            }
          )
          .filter(p => {
            val allowed = allowedInputs.fold(true)(_(p))
            if (debugImporter) {
              if (allowed) System.err.println(s"[import $importName] candidate $p")
              else
                System.err.println(
                  s"[import $importName] excluded $p because it's not in $allowedInputs"
                )
            }
            allowed
          })
          .find(f => os.exists(f) && !os.isDir(f))
          .orElse({
            if (debugImporter) {
              System.err.println(s"[import $importName] none of the candidates exist")
            }
            None
          })
          .flatMap(p => {
            if (debugImporter) {
              System.err.println(
                s"[import $importName] $p is selected as it exists and is not a directory"
              )
            }
            Some(OsPath(p))
          })

      def read(path: Path, binaryData: Boolean): Option[ResolvedFile] = {
        readPath(path, binaryData, debugImporter)
      }
    }

  def main0(
      args: Array[String],
      parseCache: ParseCache,
      @unused stdin: InputStream,
      stdout: PrintStream,
      stderr: PrintStream,
      wd: os.Path,
      allowedInputs: Option[Set[os.Path]] = None,
      importer: Option[(Path, String) => Option[os.Path]] = None,
      std: Val.Obj = sjsonnet.stdlib.StdLibModule.Default.module): Int = {

    var hasWarnings = false
    def warn(isTrace: Boolean, msg: String): Unit = {
      hasWarnings = hasWarnings || !isTrace
      if (!isTrace)
        stderr.println("[warning] " + msg)
      else
        stderr.println(msg)
    }

    val parser = mainargs.ParserForClass[Config]
    val name = s"Sjsonnet ${sjsonnet.Version.version}"
    val doc = "usage: sjsonnet  [sjsonnet-options] script-file"
    val result = for {
      config <- parser.constructEither(
        args.toIndexedSeq,
        allowRepeats = true,
        customName = name,
        customDoc = doc,
        autoPrintHelpAndExit = None
      )
      file <- Right(config.file)
      outputStr <- mainConfigured(file, config, parseCache, wd, allowedInputs, importer, warn, std)
      res <- {
        if (hasWarnings && config.fatalWarnings.value) Left("")
        else Right(outputStr)
      }
    } yield (config, res)

    result match {
      case Left(err) =>
        if (err.nonEmpty) stderr.println(err)
        1
      case Right((config, str)) =>
        if (str.nonEmpty) {
          config.outputFile match {
            case None    => stdout.println(str)
            case Some(f) => os.write.over(os.Path(f, wd), str)
          }
        }

        0
    }
  }

  private def rendererForConfig(wr: Writer, config: Config, getCurrentPosition: () => Position) =
    if (config.yamlOut.value)
      new PrettyYamlRenderer(
        wr,
        indent = config.indent,
        getCurrentPosition = getCurrentPosition
      )
    else new Renderer(wr, indent = config.indent)

  private def handleWriteFile[T](f: => T): Either[String, T] =
    Try(f).toEither.left.map {
      case _: NoSuchFileException => s"open $f: no such file or directory"
      case e                      => e.toString
    }

  private def writeFile(config: Config, f: os.Path, contents: String): Either[String, Unit] =
    handleWriteFile(os.write.over(f, contents, createFolders = config.createDirs.value))

  private def writeToFile(config: Config, wd: os.Path)(
      materialize: Writer => Either[String, ?]): Either[String, String] = {
    config.outputFile match {
      case None =>
        val sw = new StringWriter
        materialize(sw).map(_ => sw.toString)

      case Some(f) =>
        handleWriteFile(
          os.write.over.outputStream(os.Path(f, wd), createFolders = config.createDirs.value)
        ).flatMap { out =>
          try {
            val buf = new BufferedOutputStream(out)
            val wr = new OutputStreamWriter(buf, StandardCharsets.UTF_8)
            val u = materialize(wr)
            wr.flush()
            u.map(_ => "")
          } finally out.close()
        }
    }
  }

  private def expectString(v: ujson.Value) = v match {
    case ujson.Str(s) => Right(s)
    case _            => Left("expected string result, got: " + v.getClass)
  }

  private def renderNormal(
      config: Config,
      interp: Interpreter,
      jsonnetCode: String,
      path: os.Path,
      wd: os.Path,
      getCurrentPosition: () => Position) = {
    writeToFile(config, wd)(writer =>
      if (config.expectString.value) {
        val res = interp.interpret(jsonnetCode, OsPath(path)).flatMap(expectString)
        res match {
          case Right(s) => writer.write(s)
          case _        =>
        }
        res
      } else {
        val renderer = rendererForConfig(writer, config, getCurrentPosition)
        val res = interp.interpret0(jsonnetCode, OsPath(path), renderer)
        if (config.yamlOut.value) writer.write('\n')
        res
      }
    )
  }

  private def isScalar(v: ujson.Value) = !v.isInstanceOf[ujson.Arr] && !v.isInstanceOf[ujson.Obj]

  private def parseBindings(
      strs: Seq[String],
      strFiles: Seq[String],
      codes: Seq[String],
      codeFiles: Seq[String],
      wd: os.Path) = {

    def split(s: String) = s.split("=", 2) match {
      case Array(x)    => (x, System.getenv(x))
      case Array(x, v) => (x, v)
      case _           => ???
    }

    def splitMap(s: Seq[String], f: String => String) =
      s.map(split).map { case (x, v) => (x, f(v)) }
    def readPath(v: String) =
      if (v == "-" || v == "/dev/stdin") io.Source.stdin.mkString else os.read(os.Path(v, wd))

    Map() ++
    splitMap(strs, v => ujson.write(v)) ++
    splitMap(strFiles, v => ujson.write(readPath(v))) ++
    splitMap(codes, identity) ++
    splitMap(
      codeFiles,
      v =>
        if (v == "-" || v == "/dev/stdin") io.Source.stdin.mkString
        else s"import @'${v.replace("'", "''")}'"
    )
  }

  /**
   * @return
   *   Right(str) if there's some string that needs to be printed to stdout or --output-file,
   *   Left(err) if there is an error to be reported
   */
  private def mainConfigured(
      file: String,
      config: Config,
      parseCache: ParseCache,
      wd: os.Path,
      allowedInputs: Option[Set[os.Path]],
      importer: Option[(Path, String) => Option[os.Path]],
      warnLogger: (Boolean, String) => Unit,
      std: Val.Obj): Either[String, String] = {

    val (jsonnetCode, path) =
      if (config.exec.value) (file, wd / Util.wrapInLessThanGreaterThan("exec"))
      // TODO: Get rid of the /dev/stdin special-casing (everywhere!) once we use scala-native
      // with https://github.com/scala-native/scala-native/issues/4384 fixed.
      else if (file == "-" || file == "/dev/stdin")
        (io.Source.stdin.mkString, wd / Util.wrapInLessThanGreaterThan("<stdin>"))
      else {
        val p = os.Path(file, wd)
        (os.read(p), p)
      }

    val extBinding = parseBindings(
      config.extStr,
      config.extStrFile,
      config.extCode,
      config.extCodeFile,
      wd
    )

    val tlaBinding = parseBindings(
      config.tlaStr,
      config.tlaStrFile,
      config.tlaCode,
      config.tlaCodeFile,
      wd
    )

    var currentPos: Position = null
    val interp = new Interpreter(
      queryExtVar = (key: String) => extBinding.get(key).map(ExternalVariable.code),
      queryTlaVar = (key: String) => tlaBinding.get(key).map(ExternalVariable.code),
      OsPath(wd),
      importer = importer match {
        case Some(i) =>
          new Importer {
            def resolve(docBase: Path, importName: String): Option[Path] =
              i(docBase, importName).map(OsPath.apply)
            def read(path: Path, binaryData: Boolean): Option[ResolvedFile] = {
              readPath(path, binaryData)
            }
          }
        case None =>
          resolveImport(
            config.getOrderedJpaths.map(os.Path(_, wd)).map(OsPath.apply),
            allowedInputs,
            config.debugImporter.value
          )
      },
      parseCache,
      settings = new Settings(
        preserveOrder = config.preserveOrder.value,
        strict = config.strict.value,
        throwErrorForInvalidSets = config.throwErrorForInvalidSets.value,
        maxParserRecursionDepth = config.maxParserRecursionDepth
      ),
      storePos = (position: Position) => if (config.yamlDebug.value) currentPos = position else (),
      logger = warnLogger,
      std = std,
      variableResolver = _ => None
    )

    (config.multi, config.yamlStream.value) match {
      case (Some(multiPath), _) =>
        interp.interpret(jsonnetCode, OsPath(path)).flatMap {
          case obj: ujson.Obj =>
            val renderedFiles: Seq[Either[String, os.FilePath]] =
              obj.value.toSeq.map { case (f, v) =>
                for {
                  rendered <- {
                    if (config.expectString.value) {
                      expectString(v)
                    } else {
                      val writer = new StringWriter()
                      val renderer = rendererForConfig(writer, config, () => currentPos)
                      ujson.transform(v, renderer)
                      Right(writer.toString)
                    }
                  }
                  relPath = (os.FilePath(multiPath) / os.RelPath(f)).asInstanceOf[os.FilePath]
                  _ <- writeFile(config, relPath.resolveFrom(wd), rendered)
                } yield relPath
              }

            renderedFiles.collect { case Left(err) => err } match {
              case Nil =>
                Right[String, String](
                  renderedFiles.collect { case Right(path) => path }.mkString("\n")
                )
              case errs =>
                Left[String, String]("rendering errors:\n" + errs.mkString("\n"))
            }

          case _ =>
            Left(
              "error: multi mode: top-level should be an object " +
              "whose keys are filenames and values hold the JSON for that file."
            )
        }
      case (None, true) =>
        // YAML stream

        interp.interpret(jsonnetCode, OsPath(path)).flatMap {
          case arr: ujson.Arr =>
            writeToFile(config, wd) { writer =>
              arr.value.toSeq match {
                case Nil         => // donothing
                case Seq(single) =>
                  val renderer = rendererForConfig(writer, config, () => currentPos)
                  single.transform(renderer)
                  writer.write(if (isScalar(single)) "\n..." else "")
                case multiple =>
                  for ((v, i) <- multiple.zipWithIndex) {
                    if (i > 0) writer.write('\n')
                    if (isScalar(v)) writer.write("--- ")
                    else if (i != 0) writer.write("---\n")
                    val renderer = rendererForConfig(
                      writer,
                      config.copy(yamlOut = mainargs.Flag(true)),
                      () => currentPos
                    )
                    v.transform(renderer)
                  }
              }
              writer.write('\n')
              Right("")
            }

          case _ => renderNormal(config, interp, jsonnetCode, path, wd, () => currentPos)
        }
      case _ => renderNormal(config, interp, jsonnetCode, path, wd, () => currentPos)

    }
  }

  /**
   * Read a path into a [[ResolvedFile]] if it exists and is a file. A resolved file acts as a layer
   * of caching on top of the underlying file system. Small files are read into memory, while large
   * files are read from disk.
   */
  private def readPath(
      path: Path,
      binaryData: Boolean,
      debugImporter: Boolean = false): Option[ResolvedFile] = {
    val osPath = path.asInstanceOf[OsPath].p
    if (os.exists(osPath) && !os.isDir(osPath)) {
      Some(
        new CachedResolvedFile(
          path.asInstanceOf[OsPath],
          memoryLimitBytes = Int.MaxValue.toLong,
          binaryData = binaryData
        )
      )
    } else {
      if (debugImporter) {
        System.err.println(s"[read $path] file does not exist or is a directory")
      }
      None
    }
  }
}
