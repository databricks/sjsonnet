package sjsonnet

object Main {
  def main(args: Array[String]): Unit = {
    Cli.groupArgs(args.toList, Cli.genericSignature, Cli.Config()) match{
      case Left(err) =>
        println(err)
        println(Cli.help)
        System.exit(1)
      case Right((config, leftover)) =>
        leftover match{
          case file :: rest =>
            Cli.groupArgs(rest, Cli.genericSignature, config) match{
              case Left(err) =>
                println(err)
                println(Cli.help)
                System.exit(1)
              case Right((config, rest)) =>
                val path = ammonite.ops.Path(file, ammonite.ops.pwd)
                val interp = new Interpreter(
                  new Parser,
                  Scope.standard(
                    path,
                    ammonite.ops.pwd,
                    config.jpaths.map(ammonite.ops.Path(_, ammonite.ops.pwd)).toList
                  )
                )
                interp.interpret(path) match{
                  case Left(errMsg) =>
                    System.err.println(errMsg)
                    System.exit(1)
                  case Right(materialized) =>
                    val str = materialized.render(indent = 4)
                    config.outputFile match{
                      case None => println(str)
                      case Some(f) => ammonite.ops.write(ammonite.ops.Path(f, ammonite.ops.pwd), str)
                    }
                }
            }

          case _ =>
            println(Cli.help)
            System.exit(1)
        }

    }
  }
}
