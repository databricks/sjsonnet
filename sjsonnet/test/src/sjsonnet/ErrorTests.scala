package sjsonnet

import utest._

object ErrorTests extends TestSuite{
  val testSuiteRoot = ammonite.ops.pwd / 'sjsonnet / 'test / 'resources / 'test_suite
  def eval(p: ammonite.ops.Path) = {
    val s = ammonite.ops.read(p)
    val scope = new Scope(None, None, None, Map("std" -> Ref(Scope.Std)), p, Nil, None)
    val parser = new Parser

    new Evaluator(parser, scope).visitExpr(
      parser.expr.parse(s).get.value,
      scope
    )
  }
  def check(expected: String)(implicit tp: utest.framework.TestPath) = {
    val res = intercept[Exception]{
      Materializer(eval(testSuiteRoot / s"error.${tp.value.last}.jsonnet"))
    }
    res match{
      case EvaluatorError(_, _, Some(e)) => e.printStackTrace()
      case _ => //do nothing
    }
    val err = res.toString + "\n" + res.getStackTrace.mkString("\n")
    assert(err == expected)
  }
  val tests = Tests{
    "01" - check(
      """sjsonnet.EvaluatorError: foo
        |.(sjsonnet/test/resources/test_suite/error.01.jsonnet:17:29)
        |.(sjsonnet/test/resources/test_suite/error.01.jsonnet:18:36)
        |.(sjsonnet/test/resources/test_suite/error.01.jsonnet:19:35)
        |.(sjsonnet/test/resources/test_suite/error.01.jsonnet:20:7)""".stripMargin
    )
    "02" - check(
      """sjsonnet.EvaluatorError: Foo.
        |.(sjsonnet/test/resources/test_suite/error.02.jsonnet:17:1)""".stripMargin
    )
    "03" - check(
      """sjsonnet.EvaluatorError: foo
        |.(sjsonnet/test/resources/test_suite/error.03.jsonnet:17:21)
        |.(sjsonnet/test/resources/test_suite/error.03.jsonnet:18:7)""".stripMargin
    )
    "04" - check(
      """sjsonnet.EvaluatorError: foo
        |.(sjsonnet/test/resources/test_suite/error.04.jsonnet:17:21)""".stripMargin
    )
    "05" - check(
      """sjsonnet.EvaluatorError: foo
        |.(sjsonnet/test/resources/test_suite/error.05.jsonnet:17:21)""".stripMargin
    )
    "06" - check(
      """sjsonnet.EvaluatorError: division by zero
        |.(sjsonnet/test/resources/test_suite/error.06.jsonnet:17:15)
        |.(sjsonnet/test/resources/test_suite/error.06.jsonnet:18:22)
        |.(sjsonnet/test/resources/test_suite/error.06.jsonnet:19:2)""".stripMargin
    )
    "07" - check(
      """sjsonnet.EvaluatorError: sarcasm
        |.(sjsonnet/test/resources/test_suite/error.07.jsonnet:18:31)
        |.(sjsonnet/test/resources/test_suite/error.07.jsonnet:17:32)
        |.(sjsonnet/test/resources/test_suite/error.07.jsonnet:18:20)
        |.(sjsonnet/test/resources/test_suite/error.07.jsonnet:19:1)""".stripMargin
    )
    "08" - check(
      """sjsonnet.EvaluatorError: {"a":1,"b":2,"c":3}
        |.(sjsonnet/test/resources/test_suite/error.08.jsonnet:18:1)""".stripMargin
    )
    "array_fractional_index" - check(
      """sjsonnet.EvaluatorError: array index was not integer: 1.5
        |.(sjsonnet/test/resources/test_suite/error.array_fractional_index.jsonnet:17:10)""".stripMargin
    )
    "array_index_string" - check(
      """sjsonnet.EvaluatorError: attemped to index a array with string foo
        |.(sjsonnet/test/resources/test_suite/error.array_index_string.jsonnet:17:10)""".stripMargin
    )
    "array_large_index" - check(
      """sjsonnet.EvaluatorError: array bounds error: 1.8446744073709552E19 not within [0, 3)
        |.(sjsonnet/test/resources/test_suite/error.array_large_index.jsonnet:17:10)""".stripMargin
    )
//    "array_recursive_manifest" - check(
//      """sjsonnet.EvaluatorError: array bounds error: 1.8446744073709552E19 not within [0, 3)
//        |.(sjsonnet/test/resources/test_suite/error.array_large_index.jsonnet:17:10)""".stripMargin
//    )
    "assert.fail1" - check(
      """sjsonnet.EvaluatorError: Assertion failed
        |.(sjsonnet/test/resources/test_suite/error.assert.fail1.jsonnet:20:1)""".stripMargin
    )
    "assert.fail2" - check(
      """sjsonnet.EvaluatorError: Assertion failed: foo was not equal to bar
        |.(sjsonnet/test/resources/test_suite/error.assert.fail2.jsonnet:20:1)""".stripMargin
    )
    "comprehension_spec_object" - check(
      """sjsonnet.EvaluatorError: In comprehension, can only iterate over array, not object
        |.(sjsonnet/test/resources/test_suite/error.comprehension_spec_object.jsonnet:17:15)""".stripMargin
    )
    "comprehension_spec_object2" - check(
      """sjsonnet.EvaluatorError: In comprehension, can only iterate over array, not object
        |.(sjsonnet/test/resources/test_suite/error.comprehension_spec_object2.jsonnet:17:24)""".stripMargin
    )
    "computed_field_scope" - check(
      """sjsonnet.EvaluatorError: Unknown variable x
        |.(sjsonnet/test/resources/test_suite/error.computed_field_scope.jsonnet:17:21)""".stripMargin
    )
    "divide_zero" - check(
      """sjsonnet.EvaluatorError: division by zero
        |.(sjsonnet/test/resources/test_suite/error.divide_zero.jsonnet:17:5)""".stripMargin
    )
    "equality_function" - check(
      """sjsonnet.EvaluatorError: cannot test equality of functions
        |.(sjsonnet/test/resources/test_suite/error.equality_function.jsonnet:17:16)""".stripMargin
    )
    "field_not_exist" - check(
      """sjsonnet.EvaluatorError: Field does not exist: y
        |.(sjsonnet/test/resources/test_suite/error.field_not_exist.jsonnet:17:9)""".stripMargin
    )
//    "function_arg_positional_after_named" - check(
//      """sjsonnet.EvaluatorError: Field does not exist: y
//        |.(sjsonnet/test/resources/test_suite/error.field_not_exist.jsonnet:17:9)""".stripMargin
//    )

    "function_duplicate_arg" - check(
      """sjsonnet.EvaluatorError: Parameter passed more than once: x
        |.(sjsonnet/test/resources/test_suite/error.function_duplicate_arg.jsonnet:17:2)
        |.(sjsonnet/test/resources/test_suite/error.function_duplicate_arg.jsonnet:17:21)""".stripMargin
    )
//    "function_duplicate_param" - check(
//      """sjsonnet.EvaluatorError: Parameter passed more than once: x
//        |.(sjsonnet/test/resources/test_suite/error.function_duplicate_arg.jsonnet:17:2)
//        |.(sjsonnet/test/resources/test_suite/error.function_duplicate_arg.jsonnet:17:21)""".stripMargin
//    )
//    "function_infinite_default" - check(
//      """sjsonnet.EvaluatorError: Parameter passed more than once: x
//        |.(sjsonnet/test/resources/test_suite/error.function_duplicate_arg.jsonnet:17:2)
//        |.(sjsonnet/test/resources/test_suite/error.function_duplicate_arg.jsonnet:17:21)""".stripMargin
//    )
    "function_too_many_args" - check(
      """sjsonnet.EvaluatorError: Too many args, function has 2 parameter(s)
        |.(sjsonnet/test/resources/test_suite/error.function_too_many_args.jsonnet:17:7)
        |.(sjsonnet/test/resources/test_suite/error.function_too_many_args.jsonnet:19:4)""".stripMargin
    )
    "import_empty" - check(
      """sjsonnet.EvaluatorError: Couldn't import file: ""
        |.(sjsonnet/test/resources/test_suite/error.import_empty.jsonnet:17:1)""".stripMargin
    )
    "import_folder" - check(
      """sjsonnet.EvaluatorError: Couldn't import file: "lib"
        |.(sjsonnet/test/resources/test_suite/error.import_folder.jsonnet:17:1)""".stripMargin
    )
    "import_folder_slash" - check(
      """sjsonnet.EvaluatorError: Couldn't import file: "lib/"
        |.(sjsonnet/test/resources/test_suite/error.import_folder_slash.jsonnet:17:1)""".stripMargin
    )
    "import_static-check-failure" - check(
      """sjsonnet.EvaluatorError: Unknown variable x
        |.(sjsonnet/test/resources/test_suite/lib/static_check_failure.jsonnet:2:1)""".stripMargin
    )
    "import_syntax-error" - check(
      """sjsonnet.EvaluatorError: Imported file "lib/syntax_error.jsonnet" had syntax error string:1:1 ..."\"\n"
        |.(sjsonnet/test/resources/test_suite/error.import_syntax-error.jsonnet:1:1)""".stripMargin
    )
    "inside_equals_array" - check(
      """sjsonnet.EvaluatorError: foobar
        |.(sjsonnet/test/resources/test_suite/error.inside_equals_array.jsonnet:18:18)
        |.(sjsonnet/test/resources/test_suite/error.inside_equals_array.jsonnet:19:3)""".stripMargin
    )
    "inside_equals_object" - check(
      """sjsonnet.EvaluatorError: foobar
        |.(sjsonnet/test/resources/test_suite/error.inside_equals_object.jsonnet:18:22)
        |.(sjsonnet/test/resources/test_suite/error.inside_equals_object.jsonnet:19:3)""".stripMargin
    )
    "inside_tostring_array" - check(
      """sjsonnet.EvaluatorError: foobar
        |.(sjsonnet/test/resources/test_suite/error.inside_tostring_array.jsonnet:17:8)
        |.(sjsonnet/test/resources/test_suite/error.inside_tostring_array.jsonnet:17:24)""".stripMargin
    )
    "inside_tostring_object" - check(
      """sjsonnet.EvaluatorError: foobar
        |.(sjsonnet/test/resources/test_suite/error.inside_tostring_object.jsonnet:17:12)
        |.(sjsonnet/test/resources/test_suite/error.inside_tostring_object.jsonnet:17:29)""".stripMargin
    )
  }
}
