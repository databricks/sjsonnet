package sjsonnet

import sjsonnet.TestUtils.eval
import utest._

object StdGzipTests extends TestSuite {
  val tests = Tests {
    test("gzip"){
      eval("""std.gzip([1, 2])""") ==> ujson.Str(Runtime.version().feature() match {
        // https://bugs.openjdk.org/browse/JDK-8244706
        case s if s >= 16 => "H4sIAAAAAAAA/2NkAgCSQsy2AgAAAA=="
        case _ => "H4sIAAAAAAAAAGNkAgCSQsy2AgAAAA=="
      })
      eval("""std.gzip("hi")""") ==> ujson.Str(Runtime.version().feature() match {
        // https://bugs.openjdk.org/browse/JDK-8244706
        case s if s >= 16 => "H4sIAAAAAAAA/8vIBACsKpPYAgAAAA=="
        case _ => "H4sIAAAAAAAAAMvIBACsKpPYAgAAAA=="
      })
      eval("""std.native('gzip')([1, 2])""") ==> ujson.Str(Runtime.version().feature() match {
        // https://bugs.openjdk.org/browse/JDK-8244706
        case s if s >= 16 => "H4sIAAAAAAAA/2NkAgCSQsy2AgAAAA=="
        case _ => "H4sIAAAAAAAAAGNkAgCSQsy2AgAAAA=="
      })
      eval("""std.native('gzip')("hi")""") ==> ujson.Str(Runtime.version().feature() match {
        // https://bugs.openjdk.org/browse/JDK-8244706
        case s if s >= 16 => "H4sIAAAAAAAA/8vIBACsKpPYAgAAAA=="
        case _ => "H4sIAAAAAAAAAMvIBACsKpPYAgAAAA=="
      })
    }
  }
}

