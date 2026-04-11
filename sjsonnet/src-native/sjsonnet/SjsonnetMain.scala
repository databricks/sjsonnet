package sjsonnet

import sjsonnet.stdlib.{NativeGzip, NativeRegex}
import scala.scalanative.libc.stdio

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
      None,
      new sjsonnet.stdlib.StdLibModule(nativeFunctions =
        Map.from(NativeGzip.functions ++ NativeRegex.functions)
      ).module,
      None,
      new NativeOutputStream(stdio.stdout)
    )
    System.exit(exitCode)
  }
}
