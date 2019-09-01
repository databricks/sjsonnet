package sjsonnet

import java.io.{InputStream, PrintStream}
import java.nio.file.NoSuchFileException

import ujson.Js

import scala.collection.mutable
import scala.util.Try

object SjsonnetMain {
  def createParseCache() = collection.mutable.Map[String, fastparse.Parsed[Expr]]()
  def main(args: Array[String]): Unit = {
    val exitCode = main0(
      args match {
        case Array(s, _*) if s == "-i" || s == "--interactive" => args.tail
        case _ => args
      },
      collection.mutable.Map[String, fastparse.Parsed[Expr]](),
      System.in,
      System.out,
      System.err,
      os.pwd,
      None
    )
    System.exit(exitCode)
  }

  def main0(args: Array[String],
            parseCache: collection.mutable.Map[String, fastparse.Parsed[Expr]],
            stdin: InputStream,
            stdout: PrintStream,
            stderr: PrintStream,
            wd: os.Path,
            allowedInputs: Option[Set[os.Path]] = None,
            importer: Option[(Scope, String) => Option[os.Path]] = None): Int = {

    Cli.groupArgs(args.toList, Cli.genericSignature(wd), Cli.Config()) match{
      case Left(err) =>
        stderr.println(err)
        stderr.println(Cli.help(wd))
        1
      case Right((config, leftover)) =>
        leftover match{
          case file :: rest =>
            Cli.groupArgs(rest, Cli.genericSignature(wd), config) match{
              case Left(err) =>
                stderr.println(err)
                stderr.println(Cli.help(wd))
                1
              case Right((config, rest)) =>
                if (config.interactive){
                  stderr.println("error: -i/--interactive must be passed in as the first argument")
                  1
                }else if (rest.nonEmpty) {
                  stderr.println("error: Unknown arguments: " + rest.mkString(" "))
                  1
                }else{
                  val path = os.Path(file, wd)
                  val interp = new Interpreter(
                    parseCache,
                    Scope.standard(
                      OsPath(path),
                      OsPath(wd),
                      config.jpaths.map(os.Path(_, wd)).map(OsPath(_)),
                    ),
                    config.varBinding,
                    config.tlaBinding,
                    OsPath(wd),
                    importer = (scope, str) => {
                      (scope.currentFile.parent() :: scope.searchRoots)
                        .flatMap(base => os.FilePath(str) match {
                          case r: os.RelPath =>
                            if (r.ups > base.segmentCount()) None
                            else Some(base.asInstanceOf[OsPath].p / r)
                          case a: os.Path => Some(a)
                        })
                        .find(os.exists)
                        .flatMap(p => try Some((OsPath(p), os.read(p))) catch{case e => None})
                    }
                  )
                  interp.interpret(OsPath(path)) match{
                    case Left(errMsg) =>
                      stderr.println(errMsg)
                      1
                    case Right(materialized) =>

                      case class RenderError(msg: String)

                      def renderString(js: Js): Either[RenderError, String] = {
                        if (config.expectString) {
                          js match {
                            case Js.Str(s) => Right(s)
                            case _ =>
                              Left(RenderError("expected string result, got: " + js.getClass))
                          }
                        } else {
                          Right(ujson.transform(js, new Renderer(indent = config.indent)).toString)
                        }
                      }

                      def writeFile(f: os.RelPath, contents: String): Unit = {
                        Try(os.write.over(os.Path(f, wd), contents, createFolders = config.createDirs))
                          .recover {
                            case e: NoSuchFileException =>
                              stderr.println(s"open $f: no such file or directory")
                            case e => throw e
                          }
                      }

                      val output: String = config.multi match {
                        case Some(multiPath) =>
                          materialized match {
                          case obj: Js.Obj =>
                            val files = mutable.ListBuffer[os.RelPath]()
                            obj.value.foreach { case (f, v) =>
                              val rendered = renderString(v)
                                .fold({ err => stderr.println(err.msg); return 1 }, identity)
                              val relPath = os.RelPath(multiPath) / os.RelPath(f)
                              writeFile(relPath, rendered)
                              files += relPath
                            }
                            files.mkString("\n")
                          case _ =>
                            stderr.println(
                              """
                                |error: multi mode: top-level object was a string, should be an object
                                | whose keys are filenames and values hold the JSON for that file."""
                              .stripMargin.stripLineEnd)
                            return 1
                        }
                        case None =>
                          renderString(materialized)
                            .fold({ err => stderr.println(err.msg); return 1 }, identity)
                      }
                      config.outputFile match{
                        case None => stdout.println(output)
                        case Some(f) => writeFile(os.RelPath(f), output)
                      }
                      0
                  }
                }
            }

          case _ =>
            stderr.println("error: Need to pass in a jsonnet file to evaluate")
            stderr.println(Cli.help(wd))
            1
        }

    }
  }
}
