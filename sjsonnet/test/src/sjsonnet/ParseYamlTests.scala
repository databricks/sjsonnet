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
      eval("std.parseYaml('')") ==> ujson.Value("""null""")
      eval("std.parseYaml(\"0777\")") ==> ujson.Value("""511""")
    }
    test {
      eval("std.parseYaml('foo: bar\n---\nbar: baz\n')") ==> ujson.Value(
        """[{"foo": "bar"}, {"bar": "baz"}]"""
      )
    }
    test {
      eval("std.parseYaml('---  \nfoo: bar\n---\nbar: baz\n')") ==> ujson.Value(
        """[{"foo": "bar"}, {"bar": "baz"}]"""
      )
    }
    test {
      eval("std.parseYaml('---a: 1\nb---: 2\nc: 3---\nd: ---4')") ==> ujson.Value(
        """{"---a": 1, "b---": 2, "c": "3---", "d": "---4"}"""
      )
    }
    test {
      eval(
        """std.parseYaml(@'{"a":"\"\\n\n\r\f\b\t""" + "\\u263A" + "\"}') {l: std.length(self.a)}"
      ) ==> ujson.Value(
        """{"a":"\"\\n\n\r\f\b\t""" + "\u263A" + """","l":9}""" // <doublequote> <backslash> <n> <\n> <\r> <\f> <\b> <\t> <smiley>
      )
    }
    test {
      // Regression test for https://github.com/google/jsonnet/issues/1148
      // Empty document after --- should be treated as null
      eval("std.parseYaml('1\\n---')") ==> ujson.Value("""[1,null]""")
    }
    test {
      // Test that trailing empty document with whitespace is handled
      eval("std.parseYaml('1\\n---\\n')") ==> ujson.Value("""[1,null]""")
    }
  }
}
