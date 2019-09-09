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
    while(System.currentTimeMillis() - start < 20000000){
      count += 1
      for(name <- Seq(
        "kube-config/sentry/dev/sentry.jsonnet",
        "kubernetes/config/prometheus/prom-jobs/prod/azure/westus/prometheus.jsonnet",
        "kube-config/shard/multitenant/aws/dev/us-west-2/shard.jsonnet"
      )){

//        println(name)
//
//        os.proc("jsonnet", FileTests.testSuiteRoot / s"$name.jsonnet").call()
        val path = os.pwd / os.up / "universe" / os.RelPath(name)
        val interp = new Interpreter(
          parseCache,
          Map("var1" -> "test", "var2" -> ujson.Obj("x" -> 1, "y" -> 2)),
          Map("var1" -> "test", "var2" -> ujson.Obj("x" -> 1, "y" -> 2)),
          OsPath(os.pwd),
          SjsonnetMain.resolveImport(Array(OsPath(os.pwd / os.up / "universe")), None)
        )
        val res = interp.interpret(os.read(path), OsPath(path))
        assert(res.isRight)
      }
    }
    println(count)
  }
}
