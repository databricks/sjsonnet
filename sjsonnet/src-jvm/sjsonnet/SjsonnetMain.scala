package sjsonnet

import sjsonnet.stdlib.{NativeGzip, NativeRegex, NativeXz}

object SjsonnetMain {
  def main(args: Array[String]): Unit = {
    val exitCode = SjsonnetMainBase.main0WithStdProvider(
      args,
      new DefaultParseCache,
      System.in,
      System.out,
      System.err,
      os.pwd,
      None,
      None,
      () =>
        new sjsonnet.stdlib.StdLibModule(nativeFunctions =
          Map() ++ NativeXz.functions ++ NativeGzip.functions ++ NativeRegex.functions
        ).module,
      None,
      null
    )
    System.exit(exitCode)
  }
}
