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
                      path,
                      wd,
                      config.jpaths.map(os.Path(_, wd)),
                    ),
                    config.varBinding,
                    config.tlaBinding,
                    wd,
                    allowedInputs,
                    importer
                  )
                  interp.interpret(path) match{
                    case Left(errMsg) =>
                      stderr.println(errMsg)
                      1
                    case Right(materialized) =>

                      def writeFile(f: String, contents: String): Unit = {
                        Try(os.write.over(os.Path(f, wd), contents, createFolders = config.createDirs))
                          .recover {
                            case e: NoSuchFileException =>
                              stderr.println(s"open $f: no such file or directory")
                            case e => throw e
                          }
                      }

                      val output: String = config.multi match {
                        case Some(multiPath) => materialized match {
                          case obj: Js.Obj =>
                            val files = mutable.ListBuffer[String]()
                            obj.value.foreach { case (f, v) =>
                              writeFile(f, ujson.transform(v, new Renderer(indent = config.indent)).toString)
                              files += f
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
                        case None => ujson.transform(materialized, new Renderer(indent = config.indent)).toString
                      }
                      config.outputFile match{
                        case None => stdout.println(output)
                        case Some(f) => writeFile(f, output)
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
