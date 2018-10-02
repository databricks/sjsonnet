package sjsonnet

object JsonnetTestMain {
  def main(args: Array[String]): Unit = {
    val names = Seq(
      "arith_bool",
      "arith_float",
      "arith_string",
      "array",
      "assert",
      "binary",
      "comments",
      "condition",
      "format",
      "formatting_braces",
      "formatting_braces2",
      "functions",
      "import",
      "invariant",
      "invariant_manifest",
      "local",
      "merge",
      "null",
      "object",
      "oop",
      "oop_extra",
      "parsing_edge_cases",
      "precedence",
      "recursive_import_ok",
      "recursive_object",
      "sanity",
      "sanity2",
      "shebang",
      "slice.sugar",
      "std_all_hidden",
      "text_block",
      "unicode",
      "unix_line_endings",
      "unparse",
      "verbatim_strings"
    )
    val parser = new Parser()
    val start = System.currentTimeMillis()
    var count = 0
    while(System.currentTimeMillis() - start < 2000000){
      count += 1
      for(name <- names){
//        println(name)

//        import ammonite.ops._, ImplicitWd._
//        %("jsonnet", FileTests.testSuiteRoot / s"$name.jsonnet")
        val path = FileTests.testSuiteRoot / s"$name.jsonnet"
        val interp = new Interpreter(
          parser,
          Scope.standard(path, FileTests.testSuiteRoot, Nil),
          Map(),
          ammonite.ops.pwd
        )
        interp.interpret(path)
      }
    }
    println(count)
  }
}
