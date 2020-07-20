package sjsonnet


import scala.annotation.tailrec

object Cli{
  case class Arg[T, V](name: String,
                       shortName: Option[Char],
                       doc: String,
                       action: (T, V) => T)
                      (implicit val reader: scopt.Read[V]){
    def runAction(t: T, s: String) = action(t, reader.reads(s))
  }
  case class Config(interactive: Boolean = false,
                    jpaths: List[String] = Nil,
                    outputFile: Option[String] = None,
                    multi: Option[String] = None,
                    createDirs: Boolean = false,
                    yamlStream: Boolean = false,
                    expectString: Boolean = false,
                    varBinding: Map[String, ujson.Value] = Map(),
                    tlaBinding: Map[String, ujson.Value] = Map(),
                    indent: Int = 3,
                    preserveOrder: Boolean = false)


  def genericSignature(wd: os.Path) = Seq(
    Arg[Config, Unit](
      "interactive", Some('i'),
      "Run Mill in interactive mode, suitable for opening REPLs and taking user input",
      (c, v) => c.copy(interactive = true)
    ),
    Arg[Config, Int](
      "indent", Some('n'),
      "How much to indent your output JSON",
      (c, v) => c.copy(indent = v)
    ),
    Arg[Config, String](
      "jpath", Some('J'),
      "Specify an additional library search dir (right-most wins)",
      (c, v) => c.copy(jpaths = v :: c.jpaths)
    ),
    Arg[Config, String](
      "output-file", Some('o'),
      "Write to the output file rather than stdout",
      (c, v) => c.copy(outputFile = Some(v))
    ),
    Arg[Config, String](
      "multi", Some('m'),
      "Write multiple files to the directory, list files on stdout",
      (c, v) => c.copy(multi = Some(v))
    ),
    Arg[Config, Unit](
      "create-output-dirs", Some('c'),
      "Automatically creates all parent directories for files",
      (c, v) => c.copy(createDirs = true)
    ),
    Arg[Config, Unit](
      "yaml-stream", Some('y'),
      "Write output as a YAML stream of JSON documents",
      (c, v) => c.copy(yamlStream = true)
    ),
    Arg[Config, Unit](
      "string", Some('S'),
      "Expect a string, manifest as plain text",
      (c, v) => c.copy(expectString = true)
    ),
    Arg[Config, String](
      "ext-str", Some('V'),
      "<var>[=<val>] Provide 'external' variable as string. 'If <val> is omitted, get from environment var <var>",
      (c, v) => v split('=') match{
        case Array(x) => c.copy(varBinding = c.varBinding ++ Seq(x -> ujson.Str(System.getenv(x))))
        case Array(x, v) => c.copy(varBinding = c.varBinding ++ Seq(x -> ujson.Str(v)))
      }
    ),
    Arg[Config, String](
      "ext-str-file", None,
      "<var>=<file> Provide 'external' variable as string from the file",
      (c, v) => v split('=') match{
        case Array(x, v) =>
          c.copy(varBinding = c.varBinding ++ Seq(x -> ujson.Str(os.read(os.Path(v, wd)))))
      }
    ),
    Arg[Config, String](
      "ext-code", None,
      "<var>[=<code>] Provide 'external' variable as Jsonnet code. If <code> is omitted, get from environment var <var>",
      (c, v) => v split('=') match{
        case Array(x) => c.copy(varBinding = c.varBinding ++ Seq(x -> ujson.read(System.getenv(x))))
        case Array(x, v) => c.copy(varBinding = c.varBinding ++ Seq(x -> ujson.read(v)))
      }
    ),
    Arg[Config, String](
      "ext-code-file", None,
      "<var>=<file> Provide 'external' variable as Jsonnet code from the file",
      (c, v) => v split('=') match{
        case Array(x, v) =>
          c.copy(varBinding = c.varBinding ++ Seq(x -> ujson.read(os.read(os.Path(v, wd)))))
      }
    ),
    Arg[Config, String](
      "tla-str", Some('A'),
      "<var>[=<val>] Provide top-level arguments as string. 'If <val> is omitted, get from environment var <var>",
      (c, v) => v split('=') match{
        case Array(x) => c.copy(tlaBinding = c.tlaBinding ++ Seq(x -> ujson.Str(System.getenv(x))))
        case Array(x, v) => c.copy(tlaBinding = c.tlaBinding ++ Seq(x -> ujson.Str(v)))
      }
    ),
    Arg[Config, String](
      "tla-str-file", None,
      "<var>=<file> Provide top-level arguments variable as string from the file",
      (c, v) => v split('=') match{
        case Array(x, v) =>
          c.copy(tlaBinding = c.tlaBinding ++ Seq(x -> ujson.Str(os.read(os.Path(v, wd)))))
      }
    ),
    Arg[Config, String](
      "tla-code", None,
      "<var>[=<val>] Provide top-level arguments as Jsonnet code. 'If <val> is omitted, get from environment var <var>",
      (c, v) => v split('=') match{
        case Array(x) => c.copy(tlaBinding = c.tlaBinding ++ Seq(x -> ujson.read(System.getenv(x))))
        case Array(x, v) => c.copy(tlaBinding = c.tlaBinding ++ Seq(x -> ujson.read(v)))
      }
    ),
    Arg[Config, String](
      "tla-code-file", None,
      "<var>=<file> Provide top-level arguments variable as Jsonnet code from the file",
      (c, v) => v split('=') match{
        case Array(x, v) =>
          c.copy(tlaBinding = c.tlaBinding ++ Seq(x -> ujson.read(os.read(os.Path(v, wd)))))
      }
    ),
    Arg[Config, Unit](
      "preserve-order", Some('p'),
      "Preserves order of keys in the resulting JSON",
      (c, v) => c.copy(preserveOrder = true)
    ),

  )
  def showArg(arg: Arg[_, _]) =
    "  " + arg.shortName.fold("")("-" + _ + ", ") + "--" + arg.name

  def formatBlock(args: Seq[Arg[_, _]], leftMargin: Int) = {

    for(arg <- args) yield {
      showArg(arg).padTo(leftMargin, ' ').mkString +
        arg.doc.linesIterator.mkString("\n" + " " * leftMargin)
    }
  }
  def help(wd: os.Path) = {
    val leftMargin = genericSignature(wd).map(showArg(_).length).max + 2


    s"""Sjsonnet ${sjsonnet.Version.version}
       |usage: sjsonnet  [sjsonnet-options] script-file
       |
       |${formatBlock(genericSignature(wd), leftMargin).mkString("\n")}
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
