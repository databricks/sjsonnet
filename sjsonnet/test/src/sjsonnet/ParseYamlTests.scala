package sjsonnet

import sjsonnet.TestUtils.eval
import utest.*

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
    test {
      eval("""std.parseYaml(@'{"a":"\"\\n\n\r\f\b\t""" + "\\u263A" + "\"}') {l: std.length(self.a)}") ==> ujson.Value(
        """{"a":"\"\\n\n\r\f\b\t""" + "\u263A" + """","l":9}""" // <doublequote> <backslash> <n> <\n> <\r> <\f> <\b> <\t> <smiley>
      )
    }
  }
}
