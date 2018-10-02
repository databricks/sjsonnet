package sjsonnet

import java.io.{InputStream, PrintStream}

object SjsonnetMain {
  def main(args: Array[String]): Unit = {
    val exitCode = main0(
      args match {
        case Array(s, _*) if s == "-i" || s == "--interactive" => args.tail
        case _ => args
      },
      new Parser,
      System.in,
      System.out,
      System.err
    )
    System.exit(exitCode)
  }
  def main0(args: Array[String],
            parser: Parser,
            stdin: InputStream,
            stdout: PrintStream,
            stderr: PrintStream): Int = {

    Cli.groupArgs(args.toList, Cli.genericSignature, Cli.Config()) match{
      case Left(err) =>
        stderr.println(err)
        stderr.println(Cli.help)
        1
      case Right((config, leftover)) =>
        leftover match{
          case file :: rest =>
            Cli.groupArgs(rest, Cli.genericSignature, config) match{
              case Left(err) =>
                stderr.println(err)
                stderr.println(Cli.help)
                1
              case Right((config, rest)) =>
                if (config.interactive){
                  stderr.println("error: -i/--interactive must be passed in as the first argument")
                  1
                }else if (rest.nonEmpty) {
                  stderr.println("error: Unknown arguments: " + rest.mkString(" "))
                  1
                }else{
                  val path = ammonite.ops.Path(file, ammonite.ops.pwd)
                  val interp = new Interpreter(
                    parser,
                    Scope.standard(
                      path,
                      ammonite.ops.pwd,
                      config.jpaths.map(ammonite.ops.Path(_, ammonite.ops.pwd)).toList,
                    ),
                    config.varBinding
                  )
                  interp.interpret(path) match{
                    case Left(errMsg) =>
                      stderr.println(errMsg)
                      1
                    case Right(materialized) =>
                      val str = materialized.render(indent = 4)
                      config.outputFile match{
                        case None => stdout.println(str)
                        case Some(f) => ammonite.ops.write(ammonite.ops.Path(f, ammonite.ops.pwd), str)
                      }
                      0
                  }
                }
            }

          case _ =>
            stderr.println("error: Need to pass in a jsonnet file to evaluate")
            stderr.println(Cli.help)
            1
        }

    }
  }
}
