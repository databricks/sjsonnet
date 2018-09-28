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
          case List(file) =>
            val parser = new Parser()
            val parsed = parser.expr.parse(ammonite.ops.read(ammonite.ops.Path(file, ammonite.ops.pwd))).get.value
            val emptyScope = new Scope(
              None, None, None, Map("std" -> Ref(Scope.Std)), List(ammonite.ops.pwd), None
            )
            val evaluator = new Evaluator(parser, emptyScope)
            val value = evaluator.visitExpr(parsed, emptyScope)
            val materialized = Materializer(value)
            val str = materialized.render(indent = 4)
            config.outputFile match{
              case None => println(str)
              case Some(f) => ammonite.ops.write(ammonite.ops.Path(f, ammonite.ops.pwd), str)
            }
          case _ =>
            println(Cli.help)
            System.exit(1)
        }

    }
  }
}
