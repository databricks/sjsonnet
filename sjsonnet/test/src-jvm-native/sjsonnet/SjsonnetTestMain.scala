package sjsonnet

object SjsonnetTestMain {
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
  val parseCache = sjsonnet.SjsonnetMain.createParseCache()

  def main(args: Array[String]): Unit = {
    // Warmup
    // runExternal("jsonnet", 20000)
    // for(command <- Seq("sjsonnet", "go-jsonnet", "jsonnet", "sjsonnet-native-image")) {
    //   val res = runExternal(command, 20000)
    //   println(s"$command: $res")
    // }
    
    // Warmup
    run(10000)
    println(run(20000))
  }

  def run(milliseconds: Int): Int = {
    var count = 0
    val start = System.currentTimeMillis()
    while(System.currentTimeMillis() - start < milliseconds){
      count += 1
      for(name <- names){
        val dir = os.pwd / 'sjsonnet / 'test / 'resources / 'test_suite
        val path = dir / os.RelPath(s"$name.jsonnet")
        val interp = new Interpreter(
          parseCache,
          Map("var1" -> "test", "var2" -> ujson.Obj("x" -> 1, "y" -> 2)),
          Map("var1" -> "test", "var2" -> ujson.Obj("x" -> 1, "y" -> 2)),
          OsPath(os.pwd),
          SjsonnetMain.resolveImport(Array(OsPath(dir)), None)
        )
        val res = interp.interpret(os.read(path), OsPath(path))
        assert(res.isRight)
      }
    }
    count
  }

  def runExternal(command: String, milliseconds: Int): Int = {
    var count = 0
    val start = System.currentTimeMillis()
    while(System.currentTimeMillis() - start < milliseconds){
      count += 1
      for(name <- names) {
        val dir = os.pwd / 'sjsonnet / 'test / 'resources / 'test_suite
        os.proc(command, dir / s"$name.jsonnet").call()
      }
    }
    count
  }
}
