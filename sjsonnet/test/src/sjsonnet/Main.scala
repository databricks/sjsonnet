package sjsonnet

object Main {
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
//      "text_block",
      "unicode",
      "unix_line_endings",
      "unparse",
      "verbatim_strings"
    )
    val parser = new Parser()
    val start = System.currentTimeMillis()
    var count = 0
    while(System.currentTimeMillis() - start < 10000){
      count += 1
      for(name <- names){
//        import ammonite.ops._, ImplicitWd._
//        %("jsonnet", FileTests.testSuiteRoot / s"$name.jsonnet")
//        println(name)
        val path = FileTests.testSuiteRoot / s"$name.jsonnet"
        val interp = new Interpreter(parser, Scope.standard(path, Nil))
        interp.interpret(path)
      }
    }
    println(count)
  }
}

