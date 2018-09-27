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
    val sources = for(name <- names) yield name -> new String(
      java.nio.file.Files.readAllBytes(
        java.nio.file.Paths.get(s"test_suite/$name.jsonnet")
      )
    )
    val parser = new Parser()
    while(true){
      for((name, source) <- sources){
//        println(name)
        val parsed = parser.expr.parse(source).get.value
        val emptyScope = new Scope(
          None, None, None, Map("std" -> Ref(Scope.Std)), ammonite.ops.pwd / "test_suite", None
        )

        val res = new Evaluator(parser, emptyScope).visitExpr(parsed, emptyScope)
      }
    }
  }
}

