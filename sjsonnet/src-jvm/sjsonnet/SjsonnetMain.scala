package sjsonnet

import java.io.{BufferedOutputStream, InputStream, OutputStreamWriter, PrintStream, StringWriter}
import java.nio.file.NoSuchFileException

import sjsonnet.Cli.Config

import scala.collection.mutable
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
      outputStr <- {
        if (config.interactive){
          Left("error: -i/--interactive must be passed in as the first argument")
        }else if (leftover.nonEmpty) {
          Left("error: Unknown arguments: " + leftover.mkString(" "))
        }else mainConfigured(
          file, config, parseCache, stdin, stdout, stderr, wd, allowedInputs, importer
        )
      }
    } yield outputStr

    result match{
      case Left(err) =>
        if (!err.isEmpty) System.err.println(err)
        1
      case Right(str) =>
        if (!str.isEmpty) System.out.println(str)
        0
    }
  }

  def mainConfigured(file: String,
                     config: Config,
                     parseCache: collection.mutable.Map[String, fastparse.Parsed[(Expr, Map[String, Int])]],
                     stdin: InputStream,
                     stdout: PrintStream,
                     stderr: PrintStream,
                     wd: os.Path,
                     allowedInputs: Option[Set[os.Path]] = None,
                     importer: Option[(Path, String) => Option[os.Path]] = None): Either[String, String] = {
    val path = os.Path(file, wd)
    val interp = new Interpreter(
      parseCache,
      config.varBinding,
      config.tlaBinding,
      OsPath(wd),
      importer = resolveImport(
        config.jpaths.map(os.Path(_, wd)).map(OsPath(_)),
        allowedInputs
      ),
      config.preserveOrder
    )

    def writeFile(f: os.RelPath, contents: String): Either[String, Unit] = {
      Try(os.write.over(os.Path(f, wd), contents, createFolders = config.createDirs))
        .toEither
        .left
        .map{
          case e: NoSuchFileException => s"open $f: no such file or directory"
          case e => e.toString
        }
    }

    (config.multi, config.yamlStream) match {
      case (Some(multiPath), _) =>
        interp.interpret(os.read(path), OsPath(path)).flatMap{
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
                  _ <- writeFile(relPath, rendered)
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
            val renderedFiles: Seq[Either[String, String]] =
              arr.value.toSeq.map{ v =>
                Right(ujson.transform(v, new Renderer(indent = config.indent)).toString)
              }
            renderedFiles.collect{case Left(err) => err} match{
              case Nil =>
                val docs = renderedFiles.collect{case Right(yaml) => yaml}
                val stream = if (docs.isEmpty) "" else docs.mkString("---\n", "\n---\n", "\n...")
                Right[String, String](stream)
              case errs =>
                Left[String, String]("rendering errors:\n" + errs.mkString("\n"))
            }

          case _ =>
            Left("error: stream mode: top-level object should be an array " +
              "whose elements hold the JSON for each document in the stream.")
        }
      case _ =>
        val materialized = interp.interpret0(
          os.read(path),
          OsPath(path),
          new Renderer(indent = config.indent)
        )
        config.outputFile match{
          case None => materialized.map(_.toString)
          case Some(f) =>
            val filePath = os.FilePath(f) match{
              case _: os.Path => os.Path(f).relativeTo(os.pwd)
              case _ => os.RelPath(f)
            }
            for{
              materializedStr <- materialized
              _ <- writeFile(filePath, materializedStr.toString)
            } yield ""
        }
    }
  }
}
