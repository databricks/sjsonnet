package sjsonnet

object SjsonnetTestMain {
  val parseCache = sjsonnet.SjsonnetMain.createParseCache()

  def main(args: Array[String]): Unit = {
    // Warmup
    run(10000)
    println(run(20000))
  }

  def run(milliseconds: Int): Int = {
    var count = 0
    val start = System.currentTimeMillis()
    while(System.currentTimeMillis() - start < milliseconds){
      count += 1
      for(name <- TestFiles.names){
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
}
