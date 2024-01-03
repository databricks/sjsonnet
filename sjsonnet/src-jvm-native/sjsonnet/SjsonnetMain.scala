package sjsonnet

import java.io.{BufferedOutputStream, InputStream, OutputStreamWriter, PrintStream, StringWriter, Writer}
import java.nio.charset.StandardCharsets
import java.nio.file.NoSuchFileException

import scala.util.Try
import scala.util.control.NonFatal

object SjsonnetMain {
  def resolveImport(searchRoots0: Seq[Path], allowedInputs: Option[Set[os.Path]] = None) = new Importer {
    def resolve(docBase: Path, importName: String): Option[Path] =
      (docBase +: searchRoots0)
        .flatMap(base => os.FilePath(importName) match {
          case r: os.SubPath => Some(base.asInstanceOf[OsPath].p / r)
          case r: os.RelPath =>
            if (r.ups > base.segmentCount()) None
            else Some(base.asInstanceOf[OsPath].p / r)
          case a: os.Path => Some(a)
        })
        .filter(p => allowedInputs.fold(true)(_(p)))
        .find(os.exists)
        .flatMap(p => try Some(OsPath(p)) catch{case NonFatal(_) => None})

    def read(path: Path): Option[String] =
      try Some(os.read(path.asInstanceOf[OsPath].p)) catch { case NonFatal(_) => None }
  }

  def main(args: Array[String]): Unit = {
    val exitCode = main0(
      args match {
        case Array(s, _*) if s == "-i" || s == "--interactive" => args.tail
        case _ => args
      },
      new DefaultParseCache,
      System.in,
      System.out,
      System.err,
      os.pwd,
      None
    )
    System.exit(exitCode)
  }

  def main0(args: Array[String],
            parseCache: ParseCache,
            stdin: InputStream,
            stdout: PrintStream,
            stderr: PrintStream,
            wd: os.Path,
            allowedInputs: Option[Set[os.Path]] = None,
            importer: Option[(Path, String) => Option[os.Path]] = None,
            std: Val.Obj = new Std().Std): Int = {

    var hasWarnings = false
    def warn(msg: String): Unit = {
      hasWarnings = true
      stderr.println("[warning] "+msg)
    }

    val parser = mainargs.ParserForClass[Config]
    val name = s"Sjsonnet ${sjsonnet.Version.version}"
    val doc = "usage: sjsonnet  [sjsonnet-options] script-file"
    val result = for{
      config <- parser.constructEither(
        args,
        customName = name, customDoc = doc,
        autoPrintHelpAndExit = None
      )
      file <- {
        if (config.interactive.value) {
          Left("error: -i/--interactive must be passed in as the first argument")
        }else Right(config.file)
      }
      outputStr <- mainConfigured(file, config, parseCache, wd, allowedInputs, importer, warn, std)
      res <- {
        if(hasWarnings && config.fatalWarnings.value) Left("")
        else Right(outputStr)
      }
    } yield (config, res)

    result match{
      case Left(err) =>
        if (!err.isEmpty) stderr.println(err)
        1
      case Right((config, str)) =>
        if (!str.isEmpty) {
          config.outputFile match {
            case None => stdout.println(str)
            case Some(f) => os.write.over(os.Path(f, wd), str)
          }
        }

        0
    }
  }

  def rendererForConfig(wr: Writer, config: Config, getCurrentPosition: () => Position) =
    if (config.yamlOut.value) new PrettyYamlRenderer(
      wr,
      indent = config.indent,
      getCurrentPosition = getCurrentPosition
    )
    else new Renderer(wr, indent = config.indent)

  def handleWriteFile[T](f: => T): Either[String, T] =
    Try(f).toEither.left.map{
      case e: NoSuchFileException => s"open $f: no such file or directory"
      case e => e.toString
    }

  def writeFile(config: Config, f: os.Path, contents: String): Either[String, Unit] =
    handleWriteFile(os.write.over(f, contents, createFolders = config.createDirs.value))

  def writeToFile(config: Config, wd: os.Path)(materialize: Writer => Either[String, _]): Either[String, String] = {
    config.outputFile match{
      case None =>
        val sw = new StringWriter
        materialize(sw).map(_ => sw.toString)

      case Some(f) =>
        handleWriteFile(os.write.over.outputStream(os.Path(f, wd), createFolders = config.createDirs.value)).flatMap { out =>
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

  def renderNormal(config: Config, interp: Interpreter, jsonnetCode: String, path: os.Path, wd: os.Path,
                   getCurrentPosition: () => Position) = {
    writeToFile(config, wd){ writer =>
      val renderer = rendererForConfig(writer, config, getCurrentPosition)
      val res = interp.interpret0(jsonnetCode, OsPath(path), renderer)
      if (config.yamlOut.value) writer.write('\n')
      res
    }
  }

  def isScalar(v: ujson.Value) = !v.isInstanceOf[ujson.Arr] && !v.isInstanceOf[ujson.Obj]

  def parseBindings(strs: Seq[String],
                    strFiles: Seq[String],
                    codes: Seq[String],
                    codeFiles: Seq[String],
                    wd: os.Path) = {

    def split(s: String) = s.split("=", 2) match{
      case Array(x) => (x, System.getenv(x))
      case Array(x, v) => (x, v)
    }

    def splitMap(s: Seq[String], f: String => String) = s.map(split).map{case (x, v) => (x, f(v))}
    def readPath(v: String) = os.read(os.Path(v, wd))

    Map() ++
    splitMap(strs, v => ujson.write(v)) ++
    splitMap(strFiles, v => ujson.write(readPath(v))) ++
    splitMap(codes, identity) ++
    splitMap(codeFiles, readPath)
  }

  /**
   * @return Right(str) if there's some string that needs to be printed to stdout or
   *         --output-file, Left(err) if there is an error to be reported
   */
  def mainConfigured(file: String,
                     config: Config,
                     parseCache: ParseCache,
                     wd: os.Path,
                     allowedInputs: Option[Set[os.Path]] = None,
                     importer: Option[(Path, String) => Option[os.Path]] = None,
                     warnLogger: String => Unit = null,
                     std: Val.Obj = new Std().Std): Either[String, String] = {

    val (jsonnetCode, path) =
      if (config.exec.value) (file, wd / "<exec>")
      else {
        val p = os.Path(file, wd)
        (os.read(p), p)
      }

    val extBinding = parseBindings(
      config.extStr, config.extStrFile,
      config.extCode, config.extCodeFile,
      wd
    )

    val tlaBinding = parseBindings(
      config.tlaStr, config.tlaStrFile,
      config.tlaCode, config.tlaCodeFile,
      wd
    )

    var currentPos: Position = null
    val interp = new Interpreter(
      extBinding,
      tlaBinding,
      OsPath(wd),
      importer = importer match{
        case Some(i) => new Importer {
          def resolve(docBase: Path, importName: String): Option[Path] =
            i(docBase, importName).map(OsPath)
          def read(path: Path): Option[String] =
            try Some(os.read(path.asInstanceOf[OsPath].p)) catch { case NonFatal(_) => None }
        }
        case None => resolveImport(config.jpaths.map(os.Path(_, wd)).map(OsPath(_)), allowedInputs)
      },
      parseCache,
      settings = new Settings(
        preserveOrder = config.preserveOrder.value,
        strict = config.strict.value,
        noStaticErrors = config.noStaticErrors.value,
        noDuplicateKeysInComprehension = config.noDuplicateKeysInComprehension.value,
        strictImportSyntax = config.strictImportSyntax.value,
        strictInheritedAssertions = config.strictInheritedAssertions.value
      ),
      storePos = if (config.yamlDebug.value) currentPos = _ else null,
      warnLogger = warnLogger,
      std = std
    )

    (config.multi, config.yamlStream.value) match {
      case (Some(multiPath), _) =>
        interp.interpret(jsonnetCode, OsPath(path)).flatMap{
          case obj: ujson.Obj =>
            val renderedFiles: Seq[Either[String, os.FilePath]] =
              obj.value.toSeq.map{case (f, v) =>
                for{
                  rendered <- {
                    if (config.expectString.value) {
                      v match {
                        case ujson.Str(s) => Right(s)
                        case _ => Left("expected string result, got: " + v.getClass)
                      }
                    } else {
                      val writer = new StringWriter()
                      val renderer = rendererForConfig(writer, config, () => currentPos)
                      ujson.transform(v, renderer)
                      Right(writer.toString)
                    }
                  }
                  relPath = os.FilePath(multiPath) / os.RelPath(f)
                  _ <- writeFile(config, relPath.resolveFrom(wd), rendered)
                } yield relPath
              }

            renderedFiles.collect{case Left(err) => err} match{
              case Nil =>
                Right[String, String](renderedFiles.collect{case Right(path) => path}.mkString("\n"))
              case errs =>
                Left[String, String]("rendering errors:\n" + errs.mkString("\n"))
            }

          case _ =>
            Left("error: multi mode: top-level should be an object " +
              "whose keys are filenames and values hold the JSON for that file.")
        }
      case (None, true) =>
        // YAML stream

        interp.interpret(jsonnetCode, OsPath(path)).flatMap {
          case arr: ujson.Arr =>
            writeToFile(config, wd){ writer =>
              arr.value.toSeq match {
                case Nil => //donothing
                case Seq(single) =>
                  val renderer = rendererForConfig(writer, config, () => currentPos)
                  single.transform(renderer)
                  writer.write(if (isScalar(single)) "\n..." else "")
                case multiple =>
                  for((v, i) <- multiple.zipWithIndex){
                    if (i > 0) writer.write('\n')
                    if (isScalar(v)) writer.write("--- ")
                    else if (i != 0) writer.write("---\n")
                    val renderer = rendererForConfig(writer, config.copy(yamlOut = mainargs.Flag(true)), () => currentPos)
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
}
