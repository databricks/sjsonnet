package sjsonnet

import sjsonnet.TestUtils.eval
import utest._

object StdShasTests extends TestSuite {

  def tests: Tests = Tests {
    test {
      eval("std.sha1('')") ==> ujson.Str("da39a3ee5e6b4b0d3255bfef95601890afd80709")
      eval("std.sha256('')") ==> ujson.Str("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
      eval("std.sha512('')") ==> ujson.Str("cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e")
      eval("std.sha3('')") ==> ujson.Str("a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26")
    }
    test {
      eval("std.sha1('foo')") ==> ujson.Str("0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33")
      eval("std.sha256('foo')") ==> ujson.Str("2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae")
      eval("std.sha512('foo')") ==> ujson.Str("f7fbba6e0636f890e56fbbf3283e524c6fa3204ae298382d624741d0dc6638326e282c41be5e4254d8820772c5518a2c5a8c0c7f7eda19594a7eb539453e1ed7")
      eval("std.sha3('foo')") ==> ujson.Str("4bca2b137edc580fe50a88983ef860ebaca36c857b1f492839d6d7392452a63c82cbebc68e3b70a2a1480b4bb5d437a7cba6ecf9d89f9ff3ccd14cd6146ea7e7")
    }
  }
}
