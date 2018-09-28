package sjsonnet


import ammonite.ops.Path

import scala.annotation.tailrec




object Cli{
  case class Arg[T, V](name: String,
                       shortName: Option[Char],
                       doc: String,
                       action: (T, V) => T)
                      (implicit val reader: scopt.Read[V]){
    def runAction(t: T, s: String) = action(t, reader.reads(s))
  }
  case class Config(jpaths: List[String] = Nil,
                    outputFile: Option[String] = None,
                    varBinding: Map[String, String] = Map())


  val genericSignature = Seq(
    Arg[Config, String](
      "jpaths", Some('J'),
      "Specify an additional library search dir (right-most wins)",
      (c, v) => c.copy(jpaths = v :: c.jpaths)
    ),
    Arg[Config, String](
      "output-file", Some('o'),
      "Write to the output file rather than stdout",
      (c, v) => c.copy(outputFile = Some(v))
    ),
  )
  def showArg(arg: Arg[_, _]) =
    "  " + arg.shortName.fold("")("-" + _ + ", ") + "--" + arg.name

  def formatBlock(args: Seq[Arg[_, _]], leftMargin: Int) = {

    for(arg <- args) yield {
      showArg(arg).padTo(leftMargin, ' ').mkString +
        arg.doc.lines.mkString("\n" + " " * leftMargin)
    }
  }
  def help = {
    val leftMargin = genericSignature.map(showArg(_).length).max + 2


    s"""sjsonnet
       |usage: sjsonnet [ammonite-options] [script-file [script-options]]
       |
       |${formatBlock(genericSignature, leftMargin).mkString("\n")}
    """.stripMargin
  }

  def groupArgs[T](flatArgs: List[String],
                   args: Seq[Arg[T, _]],
                   initial: T): Either[String, (T, List[String])] = {

    val argsMap0: Seq[(String, Arg[T, _])] = args
      .flatMap{x => Seq(x.name -> x) ++ x.shortName.map(_.toString -> x)}

    val argsMap = argsMap0.toMap

    @tailrec def rec(keywordTokens: List[String],
                     current: T): Either[String, (T, List[String])] = {
      keywordTokens match{
        case head :: rest if head(0) == '-' =>
          val realName = if(head(1) == '-') head.drop(2) else head.drop(1)

          argsMap.get(realName) match {
            case Some(cliArg) =>
              if (cliArg.reader == scopt.Read.unitRead) {
                rec(rest, cliArg.runAction(current, ""))
              } else rest match{
                case next :: rest2 => rec(rest2, cliArg.runAction(current, next))
                case Nil => Left(s"Expected a value after argument $head")
              }

            case None => Right((current, keywordTokens))
          }

        case _ => Right((current, keywordTokens))

      }
    }
    rec(flatArgs, initial)
  }
}