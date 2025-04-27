package sjsonnet

import mainargs.{main, arg, Flag}

@main
final case class Config(
  @arg(
    name = "jpath", short = 'J',
    doc = "Specify an additional library search dir (left-most wins)"
  )
  jpaths: List[String] = Nil,
  @arg(
    name = "output-file", short = 'o',
    doc = "Write to the output file rather than stdout"
  )
  outputFile: Option[String] = None,
  @arg(
    short = 'm',
    doc = "Write multiple files to the directory, list files on stdout"
  )
  multi: Option[String] = None,
  @arg(
    name = "create-output-dirs", short = 'c',
    doc = "Automatically creates all parent directories for files"
  )
  createDirs: Flag = Flag(),
  @arg(
    name = "yaml-stream", short = 'y',
    doc = "Write output as a YAML stream of JSON documents"
  )
  yamlStream: Flag = Flag(),
  @arg(
    name = "string", short = 'S',
    doc = "Expect a string, manifest as plain text"
  )
  expectString: Flag = Flag(),
  @arg(
    name = "ext-str", short = 'V',
    doc = "<var>[=<val>] Provide 'external' variable as string. 'If <val> is omitted, get from environment var <var>"
  )
  extStr: Seq[String] = Nil,
  @arg(
    name = "ext-str-file",
    doc = "<var>=<file> Provide 'external' variable as string from the file"
  )
  extStrFile: Seq[String] = Nil,
  @arg(
    name = "ext-code", short = 'V',
    doc = "<var>[=<code>] Provide 'external' variable as Jsonnet code. If <code> is omitted, get from environment var <var>"
  )
  extCode: Seq[String] = Nil,
  @arg(
    name = "ext-code-file",
    doc = "<var>=<file> Provide 'external' variable as Jsonnet code from the file"
  )
  extCodeFile: Seq[String] = Nil,
  @arg(
    name = "tla-str", short = 'A',
    doc = "<var>[=<val>] Provide top-level arguments as string. 'If <val> is omitted, get from environment var <var>"
  )
  tlaStr: Seq[String] = Nil,
  @arg(
    name = "tla-str-file",
    doc = "<var>=<file> Provide top-level arguments variable as string from the file"
  )
  tlaStrFile: Seq[String] = Nil,
  @arg(
    name = "tla-code", short = 'V',
    doc = "<var>[=<val>] Provide top-level arguments as Jsonnet code. 'If <val> is omitted, get from environment var <var>"
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
    name = "preserve-order", short = 'p',
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
    doc = "Generate source line comments in the output YAML doc to make it easier to figure out where values come from."
  )
  yamlDebug: Flag = Flag(),
  @arg(
    name = "no-static-errors",
    doc = "Turn static errors into warnings"
  )
  noStaticErrors: Flag = Flag(),
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
    name = "no-duplicate-keys-in-comprehension",
    doc = "Raise an error if an object comprehension contains duplicate keys"
  )
  noDuplicateKeysInComprehension: Flag = Flag(),
  @arg(
    name = "strict-import-syntax",
    doc = """Raise an error if import expressions are used without proper parentheses, e.g. import "foo".bar rather than (import "foo").bar"""
  )
  strictImportSyntax: Flag = Flag(),
  @arg(
    name = "strict-inherited-assertions",
    doc = """Properly handle assertions defined in a Jsonnet dictionary that is extended more than once"""
  )
  strictInheritedAssertions: Flag = Flag(),
  @arg(
    name = "strict-set-operations",
    doc = """Strict set operations"""
  )
  strictSetOperations: Flag = Flag(),
  @arg(
    name = "throw-error-for-invalid-sets",
    doc = """Throw an error if a set operation is used on a non-set"""
  )
  throwErrorForInvalidSets: Flag = Flag(),
  @arg(
    doc = "The jsonnet file you wish to evaluate",
    positional = true
  )
  file: String,
)
