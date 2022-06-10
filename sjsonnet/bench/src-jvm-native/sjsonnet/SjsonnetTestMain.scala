package sjsonnet

object SjsonnetTestMain {
  val testSuiteRoot = os.pwd / "sjsonnet" / "test" / "resources" / "test_suite"
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
      "verbatim_strings",
      "issue_127"
      // disabled because it spams the console a lot
//      "stdlib"
    )

    val start = System.currentTimeMillis()
    var count = 0
    val parseCache = new DefaultParseCache
    while(System.currentTimeMillis() - start < 20000){
      count += 1
      for(name <- names/*Seq(
        "kube-config/sentry/dev/sentry.jsonnet",
        "kube-config/runbot/staging/runbot-app.jsonnet",
        "kubernetes/config/prometheus/prom-jobs/prod/azure/westus/prometheus.jsonnet",
        "kube-config/shard/multitenant/aws/test/test-personal-shard.jsonnet"
      )*/){

//        println(name)
//
//        os.proc("jsonnet", FileTests.testSuiteRoot / s"$name.jsonnet").call()
//        val path = os.pwd / os.up / "universe" / os.RelPath(name)
        val path = testSuiteRoot / s"$name.jsonnet"
        var currentPos: Position = null
        val interp = new Interpreter(
          Map("var1" -> "test", "var2" -> ujson.Obj("x" -> 1, "y" -> 2), "isKubecfg" -> true),
          Map("var1" -> "test", "var2" -> ujson.Obj("x" -> 1, "y" -> 2)),
          OsPath(os.pwd),
          SjsonnetMain.resolveImport(Seq(), None),
          parseCache,
          storePos = currentPos = _
        )
        val writer = new java.io.StringWriter

        val renderer = new PrettyYamlRenderer(
          writer,
          indent = 4,
          getCurrentPosition = () => currentPos
        )

        val res = interp.interpret0(os.read(path), OsPath(path), renderer)
        assert(res.isRight, name + "\n" + res.left.get)
      }
    }
    println(count)
  }
}
