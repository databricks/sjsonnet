package sjsonnet

import mainargs.{main, arg, Flag}

@main
final case class Config(
    @arg(
      name = "jpath",
      short = 'J',
      doc =
        "Specify an additional library search dir (left-most wins unless reverse-jpaths-priority is set)"
    )
    private val jpaths: List[String] = Nil,
    @arg(
      name = "debug-importer",
      doc = "Print some additional debugging information about the importer"
    )
    debugImporter: Flag = Flag(),
    @arg(
      name = "output-file",
      short = 'o',
      doc = "Write to the output file rather than stdout"
    )
    outputFile: Option[String] = None,
    @arg(
      short = 'm',
      doc = "Write multiple files to the directory, list files on stdout"
    )
    multi: Option[String] = None,
    @arg(
      name = "create-output-dirs",
      short = 'c',
      doc = "Automatically creates all parent directories for files"
    )
    createDirs: Flag = Flag(),
    @arg(
      name = "yaml-stream",
      short = 'y',
      doc = "Write output as a YAML stream of JSON documents"
    )
    yamlStream: Flag = Flag(),
    @arg(
      name = "string",
      short = 'S',
      doc = "Expect a string, manifest as plain text"
    )
    expectString: Flag = Flag(),
    @arg(
      name = "ext-str",
      short = 'V',
      doc =
        "<var>[=<val>] Provide 'external' variable as string. 'If <val> is omitted, get from environment var <var>"
    )
    extStr: Seq[String] = Nil,
    @arg(
      name = "ext-str-file",
      doc = "<var>=<file> Provide 'external' variable as string from the file"
    )
    extStrFile: Seq[String] = Nil,
    @arg(
      name = "ext-code",
      short = 'V',
      doc =
        "<var>[=<code>] Provide 'external' variable as Jsonnet code. If <code> is omitted, get from environment var <var>"
    )
    extCode: Seq[String] = Nil,
    @arg(
      name = "ext-code-file",
      doc = "<var>=<file> Provide 'external' variable as Jsonnet code from the file"
    )
    extCodeFile: Seq[String] = Nil,
    @arg(
      name = "tla-str",
      short = 'A',
      doc =
        "<var>[=<val>] Provide top-level arguments as string. 'If <val> is omitted, get from environment var <var>"
    )
    tlaStr: Seq[String] = Nil,
    @arg(
      name = "tla-str-file",
      doc = "<var>=<file> Provide top-level arguments variable as string from the file"
    )
    tlaStrFile: Seq[String] = Nil,
    @arg(
      name = "tla-code",
      short = 'V',
      doc =
        "<var>[=<val>] Provide top-level arguments as Jsonnet code. 'If <val> is omitted, get from environment var <var>"
    )
    tlaCode: Seq[String] = Nil,
    @arg(
      name = "tla-code-file",
      doc = "<var>=<file> Provide top-level arguments variable as Jsonnet code from the file"
    )
    tlaCodeFile: Seq[String] = Nil,
    @arg(
      short = 'n',
      doc = "How much to indent your output JSON"
    )
    indent: Int = 3,
    @arg(
      name = "preserve-order",
      short = 'p',
      doc = "Preserves order of keys in the resulting JSON"
    )
    preserveOrder: Flag = Flag(),
    @arg(
      doc = "Enforce some additional syntax limitations"
    )
    strict: Flag = Flag(),
    @arg(
      name = "yaml-out",
      doc = "Write output as a YAML document"
    )
    yamlOut: Flag = Flag(),
    @arg(
      name = "yaml-debug",
      doc =
        "Generate source line comments in the output YAML doc to make it easier to figure out where values come from."
    )
    yamlDebug: Flag = Flag(),
    @arg(
      name = "fatal-warnings",
      doc = "Fail if any warnings were emitted"
    )
    fatalWarnings: Flag = Flag(),
    @arg(
      short = 'e',
      doc = "Evaluate the given string as Jsonnet rather than treating it as a file name"
    )
    exec: Flag = Flag(),
    @arg(
      name = "throw-error-for-invalid-sets",
      doc = """Throw an error if a set operation is used on a non-set"""
    )
    throwErrorForInvalidSets: Flag = Flag(),
    @arg(
      name = "reverse-jpaths-priority",
      doc = """If set, reverses the import order of specified jpaths (so that the rightmost wins)"""
    )
    reverseJpathsPriority: Flag = Flag(),
    @arg(
      name = "max-parser-recursion-depth",
      doc =
        "Set maximum parser recursion depth to prevent stack overflow from deeply nested structures"
    )
    maxParserRecursionDepth: Int = 1000,
    @arg(
      name = "no-trailing-newline",
      doc = "Do not add a trailing newline to the output"
    )
    noTrailingNewline: Flag = Flag(),
    @arg(
      name = "broken-assertion-logic",
      doc =
        "Re-enable pre-0.5.5 broken assertion logic. See https://github.com/databricks/sjsonnet/issues/526."
    )
    brokenAssertionLogic: Flag = Flag(),
    @arg(
      doc = "The jsonnet file you wish to evaluate",
      positional = true
    )
    file: String
) {

  /**
   * Returns the sequence of jpaths, combining command-line flags and the JSONNET_PATH environment
   * variable.
   *
   * JSONNET_PATH directories always have lower priority than --jpath flags. Within JSONNET_PATH,
   * the left-most entry has the highest priority, matching the behavior of the C++ and Go
   * implementations.
   *
   * The --reverse-jpaths-priority flag only affects the ordering of --jpath flags (reversing them
   * so that the rightmost wins, matching go-jsonnet behavior). JSONNET_PATH entries are always
   * appended after the (possibly reversed) --jpath flags.
   *
   * For example, `JSONNET_PATH=a:b sjsonnet -J c -J d` results in search order: c, d, a, b (default
   * mode). With --reverse-jpaths-priority, the order becomes: d, c, a, b.
   *
   * See [[https://jsonnet-libs.github.io/jsonnet-training-course/lesson2.html#jsonnet_path]] for
   * details.
   */
  def getOrderedJpaths: Seq[String] = getOrderedJpaths(jsonnetPathEnv = None)

  /**
   * Returns the sequence of jpaths, combining command-line flags and the JSONNET_PATH environment
   * variable.
   *
   * @param jsonnetPathEnv
   *   If Some(value), use the given value instead of reading from the JSONNET_PATH environment
   *   variable. If None, read from System.getenv("JSONNET_PATH").
   */
  def getOrderedJpaths(jsonnetPathEnv: Option[String]): Seq[String] = {
    val envValue = jsonnetPathEnv.getOrElse(System.getenv("JSONNET_PATH"))
    val envPaths = Config.jsonnetPathEntries(envValue)
    val orderedJpaths = if (reverseJpathsPriority.value) jpaths.reverse else jpaths
    orderedJpaths ++ envPaths
  }
}

object Config {

  /**
   * Parses the JSONNET_PATH value into a sequence of directory paths. Entries are kept in their
   * original order so that, with sjsonnet's default left-to-right search, the left-most entry in
   * the environment variable has the highest priority among the JSONNET_PATH entries, matching the
   * behavior of the C++ and Go implementations.
   *
   * The separator is colon on Unix and semicolon on Windows ({@code java.io.File.pathSeparator}).
   */
  private[sjsonnet] def jsonnetPathEntries(envValue: String): Seq[String] = {
    if (envValue == null || envValue.isEmpty) Nil
    else {
      envValue.split(java.io.File.pathSeparator).filter(_.nonEmpty).toSeq
    }
  }
}
