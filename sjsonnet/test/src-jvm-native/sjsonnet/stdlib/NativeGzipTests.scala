package sjsonnet.stdlib

import sjsonnet.Std
import sjsonnet.TestUtils.eval
import utest._

object NativeGzipTests extends TestSuite {
  private val isScalaNative: Boolean = System.getenv("SCALANATIVE_THREAD_STACK_SIZE") != null
  private val javaVersion: Double =
    java.lang.Double.parseDouble(System.getProperty("java.specification.version"))
  private val std = new Std(nativeFunctions = Map() ++ new NativeGzip().functions)

  def isJava16OrLater: Boolean = {
    if (isScalaNative)
      false
    else
      javaVersion >= 16.0
  }

  val tests: Tests = Tests {
    test("gzip") {
      eval("""std.native('gzip')([1, 2])""", std = std) ==> ujson.Str(
        if (isJava16OrLater)
          // https://bugs.openjdk.org/browse/JDK-8244706
          "H4sIAAAAAAAA/2NkAgCSQsy2AgAAAA=="
        else
          "H4sIAAAAAAAAAGNkAgCSQsy2AgAAAA=="
      )
      eval("""std.native('gzip')("hi")""", std = std) ==> ujson.Str(
        if (isJava16OrLater)
          // https://bugs.openjdk.org/browse/JDK-8244706
          "H4sIAAAAAAAA/8vIBACsKpPYAgAAAA=="
        else
          "H4sIAAAAAAAAAMvIBACsKpPYAgAAAA=="
      )
    }
  }
}
