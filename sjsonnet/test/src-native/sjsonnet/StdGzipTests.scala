package sjsonnet

import sjsonnet.TestUtils.eval
import utest._

object StdGzipTests extends TestSuite {
  val tests = Tests {
    test("gzip") {
      eval("""std.gzip([1, 2])""") ==> ujson.Str("H4sIAAAAAAAAAGNkAgCSQsy2AgAAAA==")
      eval("""std.gzip("hi")""") ==> ujson.Str("H4sIAAAAAAAAAMvIBACsKpPYAgAAAA==")
      eval("""std.native('gzip')([1, 2])""") ==> ujson.Str("H4sIAAAAAAAAAGNkAgCSQsy2AgAAAA==")
      eval("""std.native('gzip')("hi")""") ==> ujson.Str("H4sIAAAAAAAAAMvIBACsKpPYAgAAAA==")
    }
  }
}
