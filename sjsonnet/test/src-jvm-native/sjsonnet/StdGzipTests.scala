package sjsonnet

import sjsonnet.TestUtils.eval
import utest._

object StdGzipTests extends TestSuite {
  private val isScalaNative: Boolean = System.getenv("SCALANATIVE_THREAD_STACK_SIZE") != null
  private val javaVersion: Double =
    java.lang.Double.parseDouble(System.getProperty("java.specification.version"))

  def isJava16OrLater: Boolean = {
    if (isScalaNative)
      false
    else
      javaVersion >= 16.0
  }

  val tests: Tests = Tests {
    test("gzip") {
      eval("""std.gzip([1, 2])""") ==> ujson.Str(
        if (isJava16OrLater)
          // https://bugs.openjdk.org/browse/JDK-8244706
          "H4sIAAAAAAAA/2NkAgCSQsy2AgAAAA=="
        else
          "H4sIAAAAAAAAAGNkAgCSQsy2AgAAAA=="
      )
      eval("""std.gzip("hi")""") ==> ujson.Str(
        if (isJava16OrLater)
          // https://bugs.openjdk.org/browse/JDK-8244706
          "H4sIAAAAAAAA/8vIBACsKpPYAgAAAA=="
        else
          "H4sIAAAAAAAAAMvIBACsKpPYAgAAAA=="
      )
      eval("""std.native('gzip')([1, 2])""") ==> ujson.Str(
        if (isJava16OrLater)
          // https://bugs.openjdk.org/browse/JDK-8244706
          "H4sIAAAAAAAA/2NkAgCSQsy2AgAAAA=="
        else
          "H4sIAAAAAAAAAGNkAgCSQsy2AgAAAA=="
      )
      eval("""std.native('gzip')("hi")""") ==> ujson.Str(
        if (isJava16OrLater)
          // https://bugs.openjdk.org/browse/JDK-8244706
          "H4sIAAAAAAAA/8vIBACsKpPYAgAAAA=="
        else
          "H4sIAAAAAAAAAMvIBACsKpPYAgAAAA=="
      )
    }
  }
}
