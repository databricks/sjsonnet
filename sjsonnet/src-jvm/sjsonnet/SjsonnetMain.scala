package sjsonnet

import sjsonnet.stdlib.{NativeGzip, NativeRegex, NativeXz}

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
      std = new Std(nativeFunctions = NativeXz.functions ++ NativeGzip.functions ++ NativeRegex.functions).Std
    )
    System.exit(exitCode)
  }
}
