package sjsonnet

import utest._

object ErrorTests extends TestSuite{
  val testSuiteRoot = os.pwd / 'sjsonnet / 'test / 'resources / 'test_suite
  def eval(p: os.Path) = {
    val interp = new Interpreter(
      sjsonnet.SjsonnetMain.createParseCache(),
      Scope.standard(p, testSuiteRoot, Nil), Map(), Map(), os.pwd
    )
    interp.interpret(p)
  }
  def check(expected: String)(implicit tp: utest.framework.TestPath) = {
    val res = eval(testSuiteRoot / s"error.${tp.value.mkString(".")}.jsonnet")

    assert(res == Left(expected))
  }
  val tests = Tests{
    "01" - check(
      """sjsonnet.Error: foo
        |    at .(sjsonnet/test/resources/test_suite/error.01.jsonnet:17:29)
        |    at .(sjsonnet/test/resources/test_suite/error.01.jsonnet:18:36)
        |    at .(sjsonnet/test/resources/test_suite/error.01.jsonnet:19:35)
        |    at .(sjsonnet/test/resources/test_suite/error.01.jsonnet:20:7)
        |""".stripMargin
    )
    "02" - check(
      """sjsonnet.Error: Foo.
        |    at .(sjsonnet/test/resources/test_suite/error.02.jsonnet:17:1)
        |""".stripMargin
    )
    "03" - check(
      """sjsonnet.Error: foo
        |    at .(sjsonnet/test/resources/test_suite/error.03.jsonnet:17:21)
        |    at .(sjsonnet/test/resources/test_suite/error.03.jsonnet:18:7)
        |""".stripMargin
    )
    "04" - check(
      """sjsonnet.Error: foo
        |    at .(sjsonnet/test/resources/test_suite/error.04.jsonnet:17:21)
        |""".stripMargin
    )
    "05" - check(
      """sjsonnet.Error: foo
        |    at .(sjsonnet/test/resources/test_suite/error.05.jsonnet:17:21)
        |""".stripMargin
    )
    "06" - check(
      """sjsonnet.Error: division by zero
        |    at .(sjsonnet/test/resources/test_suite/error.06.jsonnet:17:15)
        |    at .(sjsonnet/test/resources/test_suite/error.06.jsonnet:18:22)
        |    at .(sjsonnet/test/resources/test_suite/error.06.jsonnet:19:2)
        |""".stripMargin
    )
    "07" - check(
      """sjsonnet.Error: sarcasm
        |    at .(sjsonnet/test/resources/test_suite/error.07.jsonnet:18:31)
        |    at .(sjsonnet/test/resources/test_suite/error.07.jsonnet:17:32)
        |    at .(sjsonnet/test/resources/test_suite/error.07.jsonnet:18:20)
        |    at .(sjsonnet/test/resources/test_suite/error.07.jsonnet:19:1)
        |""".stripMargin
    )
    "08" - check(
      """sjsonnet.Error: {"a":1,"b":2,"c":3}
        |    at .(sjsonnet/test/resources/test_suite/error.08.jsonnet:18:1)
        |""".stripMargin
    )
    "array_fractional_index" - check(
      """sjsonnet.Error: array index was not integer: 1.5
        |    at .(sjsonnet/test/resources/test_suite/error.array_fractional_index.jsonnet:17:10)
        |""".stripMargin
    )
    "array_index_string" - check(
      """sjsonnet.Error: attemped to index a array with string foo
        |    at .(sjsonnet/test/resources/test_suite/error.array_index_string.jsonnet:17:10)
        |""".stripMargin
    )
    "array_large_index" - check(
      """sjsonnet.Error: array bounds error: 1.8446744073709552E19 not within [0, 3)
        |    at .(sjsonnet/test/resources/test_suite/error.array_large_index.jsonnet:17:10)
        |""".stripMargin
    )
    "array_recursive_manifest" - check(
      """Failed to materialize recursive value""".stripMargin
    )
    "assert.fail1" - check(
      """sjsonnet.Error: Assertion failed
        |    at .(sjsonnet/test/resources/test_suite/error.assert.fail1.jsonnet:20:1)
        |""".stripMargin
    )
    "assert.fail2" - check(
      """sjsonnet.Error: Assertion failed: foo was not equal to bar
        |    at .(sjsonnet/test/resources/test_suite/error.assert.fail2.jsonnet:20:1)
        |""".stripMargin
    )
    "comprehension_spec_object" - check(
      """sjsonnet.Error: In comprehension, can only iterate over array, not object
        |    at .(sjsonnet/test/resources/test_suite/error.comprehension_spec_object.jsonnet:17:15)
        |""".stripMargin
    )
    "comprehension_spec_object2" - check(
      """sjsonnet.Error: In comprehension, can only iterate over array, not object
        |    at .(sjsonnet/test/resources/test_suite/error.comprehension_spec_object2.jsonnet:17:24)
        |""".stripMargin
    )
    "computed_field_scope" - check(
      """sjsonnet.Error: Unknown variable x
        |    at .(sjsonnet/test/resources/test_suite/error.computed_field_scope.jsonnet:17:21)
        |""".stripMargin
    )
    "divide_zero" - check(
      """sjsonnet.Error: division by zero
        |    at .(sjsonnet/test/resources/test_suite/error.divide_zero.jsonnet:17:5)
        |""".stripMargin
    )
    "equality_function" - check(
      """sjsonnet.Error: cannot test equality of functions
        |    at .(sjsonnet/test/resources/test_suite/error.equality_function.jsonnet:17:16)
        |""".stripMargin
    )
    "field_not_exist" - check(
      """sjsonnet.Error: Field does not exist: y
        |    at .(sjsonnet/test/resources/test_suite/error.field_not_exist.jsonnet:17:9)
        |""".stripMargin
    )
    "function_arg_positional_after_named" - check(
      """Parse error: Expected no positional params after named params:19:11, found ")\n"""".stripMargin
    )

    "function_duplicate_arg" - check(
      """sjsonnet.Error: Parameter passed more than once: x
        |    at .(sjsonnet/test/resources/test_suite/error.function_duplicate_arg.jsonnet:17:21)
        |    at .(sjsonnet/test/resources/test_suite/error.function_duplicate_arg.jsonnet:17:21)
        |""".stripMargin
    )
    "function_duplicate_param" - check(
      """Parse error: Expected no duplicate parameter: x:17:14, found ") x\n"""".stripMargin
    )
//    "function_infinite_default" - check(
//      """sjsonnet.Error: Parameter passed more than once: x
//        |    at .(sjsonnet/test/resources/test_suite/error.function_duplicate_arg.jsonnet:17:2)
//        |    at .(sjsonnet/test/resources/test_suite/error.function_duplicate_arg.jsonnet:17:21)
//        |""".stripMargin
//    )
    "function_too_many_args" - check(
      """sjsonnet.Error: Too many args, function has 2 parameter(s)
        |    at .(sjsonnet/test/resources/test_suite/error.function_too_many_args.jsonnet:19:4)
        |    at .(sjsonnet/test/resources/test_suite/error.function_too_many_args.jsonnet:19:4)
        |""".stripMargin
    )
    "import_empty" - check(
      """sjsonnet.Error: Couldn't import file: ""
        |    at .(sjsonnet/test/resources/test_suite/error.import_empty.jsonnet:17:1)
        |""".stripMargin
    )
    "import_folder" - check(
      """sjsonnet.Error: Couldn't import file: "lib"
        |    at .(sjsonnet/test/resources/test_suite/error.import_folder.jsonnet:17:1)
        |""".stripMargin
    )
    "import_folder_slash" - check(
      """sjsonnet.Error: Couldn't import file: "lib/"
        |    at .(sjsonnet/test/resources/test_suite/error.import_folder_slash.jsonnet:17:1)
        |""".stripMargin
    )
    "import_static-check-failure" - check(
      """sjsonnet.Error: Unknown variable x
        |    at .(sjsonnet/test/resources/test_suite/lib/static_check_failure.jsonnet:2:1)
        |""".stripMargin
    )
    "import_syntax-error" - check(
      """sjsonnet.Error: Imported file "lib/syntax_error.jsonnet" had Parse error. Expected "\"":2:1, found ""
        |    at .(sjsonnet/test/resources/test_suite/error.import_syntax-error.jsonnet:1:1)
        |""".stripMargin
    )
    "inside_equals_array" - check(
      """sjsonnet.Error: foobar
        |    at .(sjsonnet/test/resources/test_suite/error.inside_equals_array.jsonnet:18:18)
        |    at .(sjsonnet/test/resources/test_suite/error.inside_equals_array.jsonnet:19:3)
        |""".stripMargin
    )
    "inside_equals_object" - check(
      """sjsonnet.Error: foobar
        |    at .(sjsonnet/test/resources/test_suite/error.inside_equals_object.jsonnet:18:22)
        |    at .(sjsonnet/test/resources/test_suite/error.inside_equals_object.jsonnet:19:3)
        |""".stripMargin
    )
    "inside_tostring_array" - check(
      """sjsonnet.Error: foobar
        |    at .(sjsonnet/test/resources/test_suite/error.inside_tostring_array.jsonnet:17:8)
        |    at .(sjsonnet/test/resources/test_suite/error.inside_tostring_array.jsonnet:17:24)
        |""".stripMargin
    )
    "inside_tostring_object" - check(
      """sjsonnet.Error: foobar
        |    at .(sjsonnet/test/resources/test_suite/error.inside_tostring_object.jsonnet:17:12)
        |    at .(sjsonnet/test/resources/test_suite/error.inside_tostring_object.jsonnet:17:29)
        |""".stripMargin
    )
    "invariant" - {
      "avoid_output_change" - check(
        """sjsonnet.Error: Assertion failed
          |    at .(sjsonnet/test/resources/test_suite/error.invariant.avoid_output_change.jsonnet:18:15)
        |""".stripMargin
      )
      "equality" - check(
        """sjsonnet.Error: Assertion failed
          |    at .(sjsonnet/test/resources/test_suite/error.invariant.equality.jsonnet:17:10)
          |    at .(sjsonnet/test/resources/test_suite/error.invariant.equality.jsonnet:17:24)
        |""".stripMargin
      )
      "option" - check(
        """sjsonnet.Error: Assertion failed: Option "d" not in ["a","b","c"].
          |    at .(sjsonnet/test/resources/test_suite/error.invariant.option.jsonnet:19:57)
        |""".stripMargin
      )
      "simple" - check(
        """sjsonnet.Error: Assertion failed
          |    at .(sjsonnet/test/resources/test_suite/error.invariant.simple.jsonnet:18:10)
        |""".stripMargin
      )
      "simple2" - check(
        """sjsonnet.Error: Assertion failed: my error message
          |    at .(sjsonnet/test/resources/test_suite/error.invariant.simple2.jsonnet:18:12)
        |""".stripMargin
      )
      "simple3" - check(
        """sjsonnet.Error: my error message
          |    at .(sjsonnet/test/resources/test_suite/error.invariant.simple3.jsonnet:18:10)
        |""".stripMargin
      )
    }
    "native_not_found" - check(
      """sjsonnet.Error: Field does not exist: native
        |    at .(sjsonnet/test/resources/test_suite/error.native_not_found.jsonnet:17:4)
        |""".stripMargin
    )
    "obj_assert" - {
      "fail1" - check(
        """sjsonnet.Error: Assertion failed
          |    at .(sjsonnet/test/resources/test_suite/error.obj_assert.fail1.jsonnet:20:25)
          |    at .(sjsonnet/test/resources/test_suite/error.obj_assert.fail1.jsonnet:20:38)
        |""".stripMargin
      )
      "fail2" - check(
        """sjsonnet.Error: Assertion failed: foo was not equal to bar
          |    at .(sjsonnet/test/resources/test_suite/error.obj_assert.fail2.jsonnet:20:25)
          |    at .(sjsonnet/test/resources/test_suite/error.obj_assert.fail2.jsonnet:20:74)
        |""".stripMargin
      )
    }
    "obj_recursive" - check(
      """Failed to materialize recursive value""".stripMargin
    )
    "obj_recursive_manifest" - check(
      """Failed to materialize recursive value""".stripMargin
    )

//    "overflow" - check(
//      """sjsonnet.Error: my error message
//        |    at .(sjsonnet/test/resources/test_suite/error.invariant.simple3.jsonnet:18:10)
//        |""".stripMargin
//    )
//    "overflow2" - check(
//      """sjsonnet.Error: my error message
//        |    at .(sjsonnet/test/resources/test_suite/error.invariant.simple3.jsonnet:18:10)
//        |""".stripMargin
//    )
//    "overflow3" - check(
//      """sjsonnet.Error: my error message
//        |    at .(sjsonnet/test/resources/test_suite/error.invariant.simple3.jsonnet:18:10)
//        |""".stripMargin
//    )
  }
}
