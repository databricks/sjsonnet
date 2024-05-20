package sjsonnet

import sjsonnet.TestUtils.eval
import utest._

object ParseYamlTests extends TestSuite {
  def tests: Tests = Tests {
    test {
      eval("std.parseYaml('foo: bar')") ==> ujson.Value("""{"foo":"bar"}""")
    }
    test {
      eval("std.parseYaml('')") ==> ujson.Value("""{}""")
      }
    test {
      eval("std.parseYaml('foo: bar\n---\nbar: baz\n')") ==> ujson.Value("""[{"foo": "bar"}, {"bar": "baz"}]""")
    }
  }
}
