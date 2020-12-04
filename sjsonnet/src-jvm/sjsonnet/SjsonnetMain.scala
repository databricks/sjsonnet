package sjsonnet

import java.io.{BufferedOutputStream, InputStream, OutputStreamWriter, PrintStream, StringWriter, Writer}
import java.nio.charset.StandardCharsets
import java.nio.file.NoSuchFileException

import sjsonnet.Cli.Config

import scala.util.Try

object SjsonnetMain {
  def createParseCache() = collection.mutable.Map[String, fastparse.Parsed[(Expr, Map[String, Int])]]()
  def resolveImport(searchRoots0: Seq[Path], allowedInputs: Option[Set[os.Path]] = None)(wd: Path, str: String) = {
    (wd +: searchRoots0)
      .flatMap(base => os.FilePath(str) match {
        case r: os.SubPath => Some(base.asInstanceOf[OsPath].p / r)
        case r: os.RelPath =>
          if (r.ups > base.segmentCount()) None
          else Some(base.asInstanceOf[OsPath].p / r)
        case a: os.Path => Some(a)
      })
      .filter(p => allowedInputs.fold(true)(_(p)))
      .find(os.exists)
      .flatMap(p => try Some((OsPath(p), os.read(p))) catch{case e: Throwable => None})
  }
  def main(args: Array[String]): Unit = {
    val exitCode = main0(
      args match {
        case Array(s, _*) if s == "-i" || s == "--interactive" => args.tail
        case _ => args
      },
      collection.mutable.Map.empty,
      System.in,
      System.out,
      System.err,
      os.pwd,
      None
    )
    System.exit(exitCode)
  }

  def main0(args: Array[String],
            parseCache: collection.mutable.Map[String, fastparse.Parsed[(Expr, Map[String, Int])]],
            stdin: InputStream,
            stdout: PrintStream,
            stderr: PrintStream,
            wd: os.Path,
            allowedInputs: Option[Set[os.Path]] = None,
            importer: Option[(Path, String) => Option[os.Path]] = None): Int = {

    val result = for{
      t <- Cli.groupArgs(args.toList, Cli.genericSignature(wd), Cli.Config()).left.map{
        err => err + "\n" + Cli.help(wd)
      }
      (config0, leftover) = t
      t2 <- {
        leftover match{
          case file :: rest => Right((file, rest))
          case _ => Left("error: Need to pass in a jsonnet file to evaluate\n" + Cli.help(wd))
        }
      }
      (file, rest) = t2
      t3 <- Cli.groupArgs(rest, Cli.genericSignature(wd), config0)
      (config, leftover) = t3
      validator <- Validator(config)
      outputStr <- {
        if (config.interactive){
          Left("error: -i/--interactive must be passed in as the first argument")
        }else if (leftover.nonEmpty) {
          Left("error: Unknown arguments: " + leftover.mkString(" "))
        }else mainConfigured(
          file, config, parseCache, wd, allowedInputs, importer, validator, stderr
        )
      }
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

  def rendererForConfig(wr: Writer, config: Config) =
    if (config.yamlOut) new PrettyYamlRenderer(wr, indent = config.indent)
    else new Renderer(wr, indent = config.indent)
  def handleWriteFile[T](f: => T): Either[String, T] =
    Try(f).toEither.left.map{
      case e: NoSuchFileException => s"open $f: no such file or directory"
      case e => e.toString
    }

  def writeFile(config: Config, f: os.Path, contents: String): Either[String, Unit] =
    handleWriteFile(os.write.over(f, contents, createFolders = config.createDirs))

  def writeToFile[U](config: Config, wd: os.Path)(materialize: Writer => Either[String, U]): Either[String, String] = {

    config.outputFile match{
      case None => materialize(new StringWriter).map(_.toString)
      case Some(f) =>
        handleWriteFile(os.write.over.outputStream(os.Path(f, wd), createFolders = config.createDirs)).flatMap { out =>
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

  def isScalar(v: ujson.Value) = !v.isInstanceOf[ujson.Arr] && !v.isInstanceOf[ujson.Obj]

  def mainConfigured(file: String,
                     config: Config,
                     parseCache: collection.mutable.Map[String, fastparse.Parsed[(Expr, Map[String, Int])]],
                     wd: os.Path,
                     allowedInputs: Option[Set[os.Path]] = None,
                     importer: Option[(Path, String) => Option[os.Path]] = None,
                     validator: Validator,
                     stderr: PrintStream): Either[String, String] = {
    val path = os.Path(file, wd)
    val interp = new Interpreter(
      parseCache,
      config.varBinding,
      config.tlaBinding,
      OsPath(wd),
      importer = importer match{
        case Some(i) => (wd: Path, str: String) => i(wd, str).map(p => (OsPath(p), os.read(p)))
        case None => resolveImport(config.jpaths.map(os.Path(_, wd)).map(OsPath(_)), allowedInputs)
      },
      preserveOrder = config.preserveOrder,
      strict = config.strict
    )

    def interpretAndValidate[T](txt: String, visitor: upickle.core.Visitor[T, T]): Either[String, T] = {
      config.outputTypeRef match {
        case Some(tpref) =>
          for {
            json <- interp.interpret0[ujson.Value](txt, OsPath(path), ujson.Value)
            valres <- validator.validate(json, tpref)
            _ <- {
              val (ok, errors) = valres
              if(errors.nonEmpty) {
                val sm = interp.interpret0(txt, OsPath(path), new SourceMapVisitor).right.get
                //sm.positions.toSeq.sortBy(_._1).foreach(println)
                errors.foreach { ve =>
                  val prefix = if(ve.sev == ValidationError.Error) "[error]" else "[warn] "
                  val (posFile, posIdxs) = sm.findNearest(ve.at)
                  stderr.println(s"$prefix Type error in ${ve.at}:")
                  stderr.println(s"$prefix     ${ve.msg}")
                  stderr.println(s"$prefix     at $posFile:$posIdxs")
                  if(ve.schemaPath.grouped.nonEmpty) {
                    stderr.println(s"$prefix     via schema ${ve.schemaPath.grouped.head}")
                    ve.schemaPath.grouped.iterator.drop(1).foreach { s =>
                      stderr.println(s"$prefix                $s")
                    }
                  }
                  stderr.println(s"$prefix     while compiling $path")
                  stderr.println(s"$prefix")
                }
              }
              if(!ok) {
                val errCount = errors.count(_.sev == ValidationError.Error)
                Left(s"$errCount schema error(s) found.")
              } else Right(())
            }
          } yield {
            visitor match {
              case _: ujson.Value => json.asInstanceOf[T]
              case _ => ujson.transform(json, visitor)
            }
          }
        case None => interp.interpret0(txt, OsPath(path), visitor)
      }
    }

    def renderNormal() = writeToFile(config, wd){ writer =>
      val renderer = rendererForConfig(writer, config)
      val res = interpretAndValidate(os.read(path), renderer)
      if (config.yamlOut) writer.write('\n')
      res
    }

    (config.multi, config.yamlStream) match {
      case (Some(multiPath), _) =>
        interpretAndValidate(os.read(path), ujson.Value).flatMap{
          case obj: ujson.Obj =>
            val renderedFiles: Seq[Either[String, os.RelPath]] =
              obj.value.toSeq.map{case (f, v) =>
                for{
                  rendered <- {
                    if (config.expectString) {
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

        interpretAndValidate(os.read(path), ujson.Value).flatMap {
          case arr: ujson.Arr =>
            writeToFile(config, wd){ writer =>
              arr.value.toSeq match {
                case Nil => //donothing
                case Seq(single) =>
                  val renderer = rendererForConfig(writer, config)
                  single.transform(renderer)
                  writer.write(if (isScalar(single)) "\n..." else "")
                case multiple =>
                  for((v, i) <- multiple.zipWithIndex){
                    if (i > 0) writer.write('\n')
                    if (isScalar(v)) writer.write("--- ")
                    else if (i != 0) writer.write("---\n")
                    val renderer = rendererForConfig(writer, config)
                    v.transform(renderer)

                  }
              }
              writer.write('\n')
              Right("")
            }

          case _ => renderNormal()
        }
      case _ => renderNormal()

    }
  }
}
