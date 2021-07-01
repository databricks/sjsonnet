package sjsonnet

import java.io.{BufferedOutputStream, InputStream, OutputStreamWriter, PrintStream, StringWriter, Writer}
import java.nio.charset.StandardCharsets
import java.nio.file.NoSuchFileException


import scala.collection.mutable
import scala.util.Try
import scala.util.control.NonFatal

object SjsonnetMain {
  def createParseCache() = collection.mutable.HashMap[(Path, String), Either[String, (Expr, FileScope)]]()

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
    var exitCode = main0(
      args match {
        case Array(s, _*) if s == "-i" || s == "--interactive" => args.tail
        case _ => args
      },
      collection.mutable.HashMap.empty,
      System.in,
      System.out,
      System.err,
      os.pwd,
      None
    )
    System.exit(exitCode)
  }

  def main0(args: Array[String],
            parseCache: collection.mutable.HashMap[(Path, String), Either[String, (Expr, FileScope)]],
            stdin: InputStream,
            stdout: PrintStream,
            stderr: PrintStream,
            wd: os.Path,
            allowedInputs: Option[Set[os.Path]] = None,
            importer: Option[(Path, String) => Option[os.Path]] = None): Int = {

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
      outputStr <- mainConfigured(file, config, parseCache, wd, allowedInputs, importer)
    } yield outputStr

    result match{
      case Left(err) =>
        if (!err.isEmpty) stderr.println(err)
        1
      case Right(str) =>
        if (!str.isEmpty) stdout.println(str)
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

  def writeToFile[U](config: Config, wd: os.Path)(materialize: Writer => Either[String, U]): Either[String, String] = {

    config.outputFile match{
      case None => materialize(new StringWriter).map(_.toString)
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

  def renderNormal(config: Config, interp: Interpreter, path: os.Path, wd: os.Path,
                   getCurrentPosition: () => Position) = {
    writeToFile(config, wd){ writer =>
      val renderer = rendererForConfig(writer, config, getCurrentPosition)
      val res = interp.interpret0(os.read(path), OsPath(path), renderer)
      if (config.yamlOut.value) writer.write('\n')
      res
    }
  }

  def isScalar(v: ujson.Value) = !v.isInstanceOf[ujson.Arr] && !v.isInstanceOf[ujson.Obj]

  def mainConfigured(file: String,
                     config: Config,
                     parseCache: collection.mutable.HashMap[(Path, String), Either[String, (Expr, FileScope)]],
                     wd: os.Path,
                     allowedInputs: Option[Set[os.Path]] = None,
                     importer: Option[(Path, String) => Option[os.Path]] = None): Either[String, String] = {
    val path = os.Path(file, wd)
    var varBinding = Map.empty[String, ujson.Value]
    config.extStr.map(_.split('=')).foreach{
      case Array(x) => varBinding = varBinding ++ Seq(x -> ujson.Str(System.getenv(x)))
      case Array(x, v) => varBinding = varBinding ++ Seq(x -> ujson.Str(v))
    }
    config.extStrFile.map(_.split('=')).foreach {
      case Array(x, v) =>
        varBinding = varBinding ++ Seq(x -> ujson.Str(os.read(os.Path(v, wd))))
    }
    config.extCode.map(_.split('=')).foreach {
      case Array(x) => varBinding = varBinding ++ Seq(x -> ujson.read(System.getenv(x)))
      case Array(x, v) => varBinding = varBinding ++ Seq(x -> ujson.read(v))
    }
    config.extCodeFile.map(_.split('=')).foreach {
      case Array(x, v) =>
        varBinding = varBinding ++ Seq(x -> ujson.read(os.read(os.Path(v, wd))))
    }

    var tlaBinding = Map.empty[String, ujson.Value]

    config.tlaStr.map(_.split('=')).foreach{
      case Array(x) => tlaBinding = tlaBinding ++ Seq(x -> ujson.Str(System.getenv(x)))
      case Array(x, v) => tlaBinding = tlaBinding ++ Seq(x -> ujson.Str(v))
    }
    config.tlaStrFile.map(_.split('=')).foreach {
      case Array(x, v) =>
        tlaBinding = tlaBinding ++ Seq(x -> ujson.Str(os.read(os.Path(v, wd))))
    }
    config.tlaCode.map(_.split('=')).foreach {
      case Array(x) => tlaBinding = tlaBinding ++ Seq(x -> ujson.read(System.getenv(x)))
      case Array(x, v) => tlaBinding = tlaBinding ++ Seq(x -> ujson.read(v))
    }
    config.tlaCodeFile.map(_.split('=')).foreach {
      case Array(x, v) =>
        tlaBinding = tlaBinding ++ Seq(x -> ujson.read(os.read(os.Path(v, wd))))
    }
    var currentPos: Position = null
    val interp = new Interpreter(
      varBinding,
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
      preserveOrder = config.preserveOrder.value,
      strict = config.strict.value,
      storePos = if (config.yamlDebug.value) currentPos = _ else null,
      parseCache
    )

    (config.multi, config.yamlStream.value) match {
      case (Some(multiPath), _) =>
        interp.interpret(os.read(path), OsPath(path)).flatMap{
          case obj: ujson.Obj =>
            val renderedFiles: Seq[Either[String, os.RelPath]] =
              obj.value.toSeq.map{case (f, v) =>
                for{
                  rendered <- {
                    if (config.expectString.value) {
                      v match {
                        case ujson.Str(s) => Right(s)
                        case _ => Left("expected string result, got: " + v.getClass)
                      }
                    } else Right(ujson.transform(v, new Renderer(indent = config.indent)).toString)
                  }
                  relPath = os.RelPath(multiPath) / os.RelPath(f)
                  _ <- writeFile(config, wd / relPath, rendered)
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

        interp.interpret(os.read(path), OsPath(path)).flatMap {
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
                    val renderer = rendererForConfig(writer, config, () => currentPos)
                    v.transform(renderer)

                  }
              }
              writer.write('\n')
              Right("")
            }

          case _ => renderNormal(config, interp, path, wd, () => currentPos)
        }
      case _ => renderNormal(config, interp, path, wd, () => currentPos)

    }
  }
}
