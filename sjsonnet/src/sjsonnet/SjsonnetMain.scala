package sjsonnet

import java.io.{InputStream, PrintStream}

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
                      val str = ujson.transform(materialized, new Renderer(indent = config.indent)).toString
                      config.outputFile match{
                        case None => stdout.println(str)
                        case Some(f) => os.write.over(os.Path(f, wd), str)
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
