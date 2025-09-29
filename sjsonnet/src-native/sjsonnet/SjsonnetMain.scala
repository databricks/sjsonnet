package sjsonnet

import sjsonnet.stdlib.{NativeGzip, NativeRegex}

object SjsonnetMain {
  def main(args: Array[String]): Unit = {
    val exitCode = SjsonnetMainBase.main0(
      args,
      new DefaultParseCache,
      System.in,
      System.out,
      System.err,
      os.pwd,
      None,
      std = new sjsonnet.stdlib.StdLibModule(nativeFunctions =
        Map.from(NativeGzip.functions ++ NativeRegex.functions)
      ).module
    )
    System.exit(exitCode)
  }
}
