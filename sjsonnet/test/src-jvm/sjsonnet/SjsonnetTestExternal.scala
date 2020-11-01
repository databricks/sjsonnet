package sjsonnet

object SjsonnetTestExternal {
  def main(args: Array[String]): Unit = {
    // Warmup
    runExternal("jsonnet", 20000)
    for(command <- Seq("sjsonnet", "go-jsonnet", "jsonnet", "sjsonnet-native-image")) {
      val res = runExternal(command, 20000)
      println(s"$command: $res")
    }
  }

  def runExternal(command: String, milliseconds: Int): Int = {
    var count = 0
    val start = System.currentTimeMillis()
    while(System.currentTimeMillis() - start < milliseconds){
      count += 1
      for(name <- TestFiles.names) {
        val dir = os.pwd / 'sjsonnet / 'test / 'resources / 'test_suite
        os.proc(command, dir / s"$name.jsonnet").call()
      }
    }
    count
  }
}
