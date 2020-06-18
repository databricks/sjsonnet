package sjsonnet

import utest._

object ErrorTests extends TestSuite{
  val testSuiteRoot = os.pwd / "sjsonnet" / "test" / "resources" / "test_suite"
  def eval(p: os.Path) = {
    val interp = new Interpreter(
      sjsonnet.SjsonnetMain.createParseCache(),
      Map(),
      Map(),
      OsPath(os.pwd),
      importer = sjsonnet.SjsonnetMain.resolveImport(Array.empty[Path]),
    )
    interp.interpret(os.read(p), OsPath(p))
  }
  def check(expected: String)(implicit tp: utest.framework.TestPath) = {
    val res = eval(testSuiteRoot / s"error.${tp.value.mkString(".")}.jsonnet")

    assert(res == Left(expected))
  }

  def checkImports(expected: String)(implicit tp: utest.framework.TestPath) = {
    val res = eval(os.pwd / "sjsonnet" / "test" / "resources" / "imports" / s"error.${tp.value.mkString(".")}.jsonnet")

    assert(res == Left(expected))
  }


  val tests = Tests{
    test("01") - check(
      """sjsonnet.Error: foo
        |    at .(sjsonnet/test/resources/test_suite/error.01.jsonnet:17:29)
        |    at .(sjsonnet/test/resources/test_suite/error.01.jsonnet:18:36)
        |    at .(sjsonnet/test/resources/test_suite/error.01.jsonnet:19:35)
        |    at .(sjsonnet/test/resources/test_suite/error.01.jsonnet:20:7)
        |""".stripMargin
    )
    test("02") - check(
      """sjsonnet.Error: Foo.
        |    at .(sjsonnet/test/resources/test_suite/error.02.jsonnet:17:1)
        |""".stripMargin
    )
    test("03") - check(
      """sjsonnet.Error: foo
        |    at .(sjsonnet/test/resources/test_suite/error.03.jsonnet:17:21)
        |""".stripMargin
    )
    test("04") - check(
      """sjsonnet.Error: foo
        |    at .(sjsonnet/test/resources/test_suite/error.04.jsonnet:17:21)
        |""".stripMargin
    )
    test("05") - check(
      """sjsonnet.Error: foo
        |    at .(sjsonnet/test/resources/test_suite/error.05.jsonnet:17:21)
        |""".stripMargin
    )
    test("06") - check(
      """sjsonnet.Error: division by zero
        |    at .(sjsonnet/test/resources/test_suite/error.06.jsonnet:17:15)
        |    at .(sjsonnet/test/resources/test_suite/error.06.jsonnet:18:22)
        |    at .(sjsonnet/test/resources/test_suite/error.06.jsonnet:19:2)
        |""".stripMargin
    )
    test("07") - check(
      """sjsonnet.Error: sarcasm
        |    at .(sjsonnet/test/resources/test_suite/error.07.jsonnet:18:31)
        |    at .(sjsonnet/test/resources/test_suite/error.07.jsonnet:17:32)
        |    at .(sjsonnet/test/resources/test_suite/error.07.jsonnet:18:20)
        |    at .(sjsonnet/test/resources/test_suite/error.07.jsonnet:19:1)
        |""".stripMargin
    )
    test("08") - check(
      """sjsonnet.Error: {"a": 1, "b": 2, "c": 3}
        |    at .(sjsonnet/test/resources/test_suite/error.08.jsonnet:18:1)
        |""".stripMargin
    )
    test("array_fractional_index") - check(
      """sjsonnet.Error: array index was not integer: 1.5
        |    at .(sjsonnet/test/resources/test_suite/error.array_fractional_index.jsonnet:17:10)
        |""".stripMargin
    )
    test("array_index_string") - check(
      """sjsonnet.Error: attempted to index a array with string foo
        |    at .(sjsonnet/test/resources/test_suite/error.array_index_string.jsonnet:17:10)
        |""".stripMargin
    )
    test("array_large_index") - check(
      """sjsonnet.Error: array bounds error: 1.8446744073709552E19 not within [0, 3)
        |    at .(sjsonnet/test/resources/test_suite/error.array_large_index.jsonnet:17:10)
        |""".stripMargin
    )
    test("array_recursive_manifest") - check(
      """Stackoverflow while materializing, possibly due to recursive value""".stripMargin
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
    test("comprehension_spec_object") - check(
      """sjsonnet.Error: In comprehension, can only iterate over array, not object
        |    at .(sjsonnet/test/resources/test_suite/error.comprehension_spec_object.jsonnet:17:15)
        |""".stripMargin
    )
    test("comprehension_spec_object2") - check(
      """sjsonnet.Error: In comprehension, can only iterate over array, not object
        |    at .(sjsonnet/test/resources/test_suite/error.comprehension_spec_object2.jsonnet:17:24)
        |""".stripMargin
    )
    test("computed_field_scope") - check(
      """sjsonnet.Error: Unknown variable x
        |    at .(sjsonnet/test/resources/test_suite/error.computed_field_scope.jsonnet:17:21)
        |""".stripMargin
    )
    test("divide_zero") - check(
      """sjsonnet.Error: division by zero
        |    at .(sjsonnet/test/resources/test_suite/error.divide_zero.jsonnet:17:5)
        |""".stripMargin
    )
    test("equality_function") - check(
      """sjsonnet.Error: cannot test equality of functions
        |    at .(sjsonnet/test/resources/test_suite/error.equality_function.jsonnet:17:16)
        |""".stripMargin
    )
    test("field_not_exist") - check(
      """sjsonnet.Error: Field does not exist: y
        |    at .(sjsonnet/test/resources/test_suite/error.field_not_exist.jsonnet:17:9)
        |""".stripMargin
    )
    test("function_arg_positional_after_named") - check(
      """Parse error: Expected no positional params after named params:19:11, found ")\n"""".stripMargin
    )

    test("function_duplicate_arg") - check(
      """sjsonnet.Error: Function parameter x passed more than once
        |    at .(sjsonnet/test/resources/test_suite/error.function_duplicate_arg.jsonnet:17:21)
        |    at .(sjsonnet/test/resources/test_suite/error.function_duplicate_arg.jsonnet:17:21)
        |""".stripMargin
    )
    test("function_duplicate_param") - check(
      """Parse error: Expected no duplicate parameter: x:17:14, found ") x\n"""".stripMargin
    )
//    test("function_infinite_default") - check(
//      """sjsonnet.Error: Parameter passed more than once: x
//        |    at .(sjsonnet/test/resources/test_suite/error.function_duplicate_arg.jsonnet:17:2)
//        |    at .(sjsonnet/test/resources/test_suite/error.function_duplicate_arg.jsonnet:17:21)
//        |""".stripMargin
//    )
    test("function_too_many_args") - check(
      """sjsonnet.Error: Too many args, function has 2 parameter(s)
        |    at .(sjsonnet/test/resources/test_suite/error.function_too_many_args.jsonnet:19:4)
        |    at .(sjsonnet/test/resources/test_suite/error.function_too_many_args.jsonnet:19:4)
        |""".stripMargin
    )
    test("import_empty") - check(
      """sjsonnet.Error: Couldn't import file: ""
        |    at .(sjsonnet/test/resources/test_suite/error.import_empty.jsonnet:17:1)
        |""".stripMargin
    )
    test("import_folder") - check(
      """sjsonnet.Error: Couldn't import file: "lib"
        |    at .(sjsonnet/test/resources/test_suite/error.import_folder.jsonnet:17:1)
        |""".stripMargin
    )
    test("import_folder_slash") - check(
      """sjsonnet.Error: Couldn't import file: "lib/"
        |    at .(sjsonnet/test/resources/test_suite/error.import_folder_slash.jsonnet:17:1)
        |""".stripMargin
    )
    "import_static-check-failure" - check(
      """sjsonnet.Error: Unknown variable x
        |    at .(sjsonnet/test/resources/test_suite/lib/static_check_failure.jsonnet:2:1)
        |    at .(sjsonnet/test/resources/test_suite/error.import_static-check-failure.jsonnet:1:1)
        |""".stripMargin
    )
    "import_syntax-error" - check(
      """sjsonnet.Error: Imported file "lib/syntax_error.jsonnet" had Parse error. Expected "\"":2:1, found ""
        |    at .(sjsonnet/test/resources/test_suite/error.import_syntax-error.jsonnet:1:1)
        |""".stripMargin
    )
    test("inside_equals_array") - check(
      """sjsonnet.Error: foobar
        |    at .(sjsonnet/test/resources/test_suite/error.inside_equals_array.jsonnet:18:18)
        |    at .(sjsonnet/test/resources/test_suite/error.inside_equals_array.jsonnet:19:3)
        |""".stripMargin
    )
    test("inside_equals_object") - check(
      """sjsonnet.Error: foobar
        |    at .(sjsonnet/test/resources/test_suite/error.inside_equals_object.jsonnet:18:22)
        |    at .(sjsonnet/test/resources/test_suite/error.inside_equals_object.jsonnet:19:3)
        |""".stripMargin
    )
    test("inside_tostring_array") - check(
      """sjsonnet.Error: foobar
        |    at .(sjsonnet/test/resources/test_suite/error.inside_tostring_array.jsonnet:17:8)
        |    at .(sjsonnet/test/resources/test_suite/error.inside_tostring_array.jsonnet:17:24)
        |""".stripMargin
    )
    test("inside_tostring_object") - check(
      """sjsonnet.Error: foobar
        |    at .(sjsonnet/test/resources/test_suite/error.inside_tostring_object.jsonnet:17:12)
        |    at .(sjsonnet/test/resources/test_suite/error.inside_tostring_object.jsonnet:17:29)
        |""".stripMargin
    )
    test("invariant") - {
      test("avoid_output_change") - check(
        """sjsonnet.Error: Assertion failed
          |    at .(sjsonnet/test/resources/test_suite/error.invariant.avoid_output_change.jsonnet:18:15)
        |""".stripMargin
      )
      test("equality") - check(
        """sjsonnet.Error: Assertion failed
          |    at .(sjsonnet/test/resources/test_suite/error.invariant.equality.jsonnet:17:10)
          |    at .(sjsonnet/test/resources/test_suite/error.invariant.equality.jsonnet:17:24)
        |""".stripMargin
      )
      test("option") - check(
        """sjsonnet.Error: Assertion failed: Option "d" not in ["a","b","c"].
          |    at .(sjsonnet/test/resources/test_suite/error.invariant.option.jsonnet:19:57)
        |""".stripMargin
      )
      test("simple") - check(
        """sjsonnet.Error: Assertion failed
          |    at .(sjsonnet/test/resources/test_suite/error.invariant.simple.jsonnet:18:10)
        |""".stripMargin
      )
      test("simple2") - check(
        """sjsonnet.Error: Assertion failed: my error message
          |    at .(sjsonnet/test/resources/test_suite/error.invariant.simple2.jsonnet:18:12)
        |""".stripMargin
      )
      test("simple3") - check(
        """sjsonnet.Error: my error message
          |    at .(sjsonnet/test/resources/test_suite/error.invariant.simple3.jsonnet:18:10)
        |""".stripMargin
      )
    }
    test("native_not_found") - check(
      """sjsonnet.Error: Field does not exist: native
        |    at .(sjsonnet/test/resources/test_suite/error.native_not_found.jsonnet:17:4)
        |""".stripMargin
    )
    test("obj_assert") - {
      test("fail1") - check(
        """sjsonnet.Error: Assertion failed
          |    at .(sjsonnet/test/resources/test_suite/error.obj_assert.fail1.jsonnet:20:25)
          |    at .(sjsonnet/test/resources/test_suite/error.obj_assert.fail1.jsonnet:20:38)
        |""".stripMargin
      )
      test("fail2") - check(
        """sjsonnet.Error: Assertion failed: foo was not equal to bar
          |    at .(sjsonnet/test/resources/test_suite/error.obj_assert.fail2.jsonnet:20:25)
          |    at .(sjsonnet/test/resources/test_suite/error.obj_assert.fail2.jsonnet:20:74)
        |""".stripMargin
      )
    }
    test("obj_recursive") - check(
      """Stackoverflow while materializing, possibly due to recursive value""".stripMargin
    )
    test("obj_recursive_manifest") - check(
      """Stackoverflow while materializing, possibly due to recursive value""".stripMargin
    )

    test("import_wrong_nr_args") - checkImports(
      """|sjsonnet.Error: Function parameter y not bound in call
         |    at .(sjsonnet/test/resources/imports/defsite.jsonnet:4:23)
         |    at .(sjsonnet/test/resources/imports/error.import_wrong_nr_args.jsonnet:3:6)
         |""".stripMargin
    )

    test("wrong_named_arg") - checkImports(
      """|sjsonnet.Error: Function has no parameter z
         |    at .(sjsonnet/test/resources/imports/error.wrong_named_arg.jsonnet:3:6)
         |    at .(sjsonnet/test/resources/imports/error.wrong_named_arg.jsonnet:3:6)
         |""".stripMargin
    )

    test("too_many_arg") - checkImports(
      """|sjsonnet.Error: Too many args, function has 2 parameter(s)
         |    at .(sjsonnet/test/resources/imports/error.too_many_arg.jsonnet:3:6)
         |    at .(sjsonnet/test/resources/imports/error.too_many_arg.jsonnet:3:6)
         |""".stripMargin
    )
    //    test("overflow") - check(
//      """sjsonnet.Error: my error message
//        |    at .(sjsonnet/test/resources/test_suite/error.invariant.simple3.jsonnet:18:10)
//        |""".stripMargin
//    )
//    test("overflow2") - check(
//      """sjsonnet.Error: my error message
//        |    at .(sjsonnet/test/resources/test_suite/error.invariant.simple3.jsonnet:18:10)
//        |""".stripMargin
//    )
//    test("overflow3") - check(
//      """sjsonnet.Error: my error message
//        |    at .(sjsonnet/test/resources/test_suite/error.invariant.simple3.jsonnet:18:10)
//        |""".stripMargin
//    )
  }
}
