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
      std = new Std(nativeFunctions =
        Map.from(new NativeGzip().functions ++ new NativeRegex().functions)
      ).Std
    )
    System.exit(exitCode)
  }
}
