package sjsonnet

import sjsonnet.TestUtils.eval
import utest._

object ParseYamlTests extends TestSuite {
  def tests: Tests = Tests {
    test {
      eval("std.parseYaml('foo: bar')") ==> ujson.Value("""{"foo":"bar"}""")
      eval("std.parseYaml('- foo: bar')") ==> ujson.Value("""[{"foo":"bar"}]""")
    }
    test {
      eval("std.parseYaml('')") ==> ujson.Value("""{}""")
      eval("std.parseYaml(\"0777\")") ==> ujson.Value("""511""")
    }
    test {
      eval("std.parseYaml('foo: bar\n---\nbar: baz\n')") ==> ujson.Value(
        """[{"foo": "bar"}, {"bar": "baz"}]"""
      )
    }
  }
}
