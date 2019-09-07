package sjsonnet

object SjsonnetTestMain {
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
      // disabled because it spams the console a lot
//      "stdlib"
    )

    val start = System.currentTimeMillis()
    var count = 0
    val parseCache = sjsonnet.SjsonnetMain.createParseCache()
    while(System.currentTimeMillis() - start < 20000){
      count += 1
      for(name <- names){

//        println(name)
//
//        os.proc("jsonnet", FileTests.testSuiteRoot / s"$name.jsonnet").call()
        val path = FileTests.testSuiteRoot / s"$name.jsonnet"
        val interp = new Interpreter(
          parseCache,
          Map("var1" -> "test", "var2" -> ujson.Obj("x" -> 1, "y" -> 2)),
          Map("var1" -> "test", "var2" -> ujson.Obj("x" -> 1, "y" -> 2)),
          OsPath(os.pwd),
          SjsonnetMain.resolveImport(Nil, None)
        )
        interp.interpret(os.read(path))
      }
    }
    println(count)
  }
}
