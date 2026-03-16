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
    test {
      // Scalar documents can start on the same line as the document-start marker
      // "--- 3" as standalone
      eval("std.parseYaml('--- 3\\n')") ==> ujson.Value("""3""")
    }
    test {
      // Folded scalar as document
      eval("std.parseYaml('--- >\\n  hello\\n  world\\n')") ==> ujson.Value(""""hello world\n"""")
    }
    test {
      // Combined: scalar docs on same line as marker
      eval("std.parseYaml('a: 1\\n--- >\\n  hello\\n  world\\n--- 3\\n')") ==> ujson.Value(
        """[{"a": 1}, "hello world\n", 3]"""
      )
    }
    test {
      // empty doc then inline scalar
      eval("std.parseYaml('a: 1\\n---\\n--- 2\\n')") ==> ujson.Value(
        """[{"a": 1}, null, 2]"""
      )
    }
    test {
      // Bare document separator
      eval("""std.parseYaml("---")""") ==> ujson.Value("""null""")
    }
    test {
      // Folded scalar without document marker (directly)
      eval("std.parseYaml('>\\n  hello\\n  world\\n')") ==> ujson.Value(""""hello world\n"""")
    }
  }
}
