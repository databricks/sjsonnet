package sjsonnet

import java.nio.charset.StandardCharsets
import scala.scalajs.js
import utest._

object ErrorTests extends TestSuite {
  def joinPath(a: String, b: String) = {
    val aStripped = if (a.endsWith("/")) a.substring(0, a.length - 1) else a
    val bStripped = if (b.startsWith("/")) b.substring(1) else b
    if (aStripped.isEmpty)
      bStripped
    else if (bStripped.isEmpty)
      aStripped
    else
      aStripped + "/" + bStripped
  }

  def eval(fileName: String, suite: String) = {
    SjsonnetMain.interpret(
      new String(TestResources.files(joinPath(suite, fileName)), StandardCharsets.UTF_8),
      js.Dictionary(),
      js.Dictionary(),
      "",
      (wd: String, path: String) => {
        val p = joinPath(suite, joinPath(wd, path))
        if (TestResources.files.contains(p)) {
          p
        } else {
          null
        }
      },
      (path: String, binaryData: Boolean) =>
        if (binaryData) {
          Right(TestResources.files(path))
        } else {
          Left(new String(TestResources.files(path), StandardCharsets.UTF_8))
        }
    )
  }

  def check(expected: String, suite: String = "test_suite")(implicit
      tp: utest.framework.TestPath): Unit = {
    val fileName = s"error.${tp.value.mkString(".")}.jsonnet"
    try {
      ujson.WebJson.transform(eval(fileName, suite), ujson.Value)
      assert(false)
    } catch {
      case e: js.JavaScriptException =>
        System.err.println(e.getMessage)
        val msg = e.getMessage
          .replaceAll(
            "\\(memory\\)",
            joinPath("sjsonnet/test/resources", joinPath(suite, fileName))
          )
          .replaceAll("\\(test_suite", "(sjsonnet/test/resources/test_suite")
          .replaceAll("  ", "    ")
        assert(msg == expected)
    }
  }

  val tests: Tests = Tests {
    test("01") - check(
      """sjsonnet.Error: foo
        |    at [Error].(sjsonnet/test/resources/test_suite/error.01.jsonnet:17:29)
        |    at [Apply1].(sjsonnet/test/resources/test_suite/error.01.jsonnet:18:36)
        |    at [Apply1].(sjsonnet/test/resources/test_suite/error.01.jsonnet:19:35)
        |    at [Apply1].(sjsonnet/test/resources/test_suite/error.01.jsonnet:20:7)
        |""".stripMargin
    )
    test("02") - check(
      """sjsonnet.Error: Foo.
        |    at [Error].(sjsonnet/test/resources/test_suite/error.02.jsonnet:17:1)
        |""".stripMargin
    )
    test("03") - check(
      """sjsonnet.Error: foo
        |    at [Error].(sjsonnet/test/resources/test_suite/error.03.jsonnet:17:21)
        |    at [Select x].(sjsonnet/test/resources/test_suite/error.03.jsonnet:18:7)
        |""".stripMargin
    )
    test("04") - check(
      """sjsonnet.Error: foo
        |    at [Error].(sjsonnet/test/resources/test_suite/error.04.jsonnet:17:21)
        |""".stripMargin
    )
    test("05") - check(
      """sjsonnet.Error: foo
        |    at [Error].(sjsonnet/test/resources/test_suite/error.05.jsonnet:17:21)
        |""".stripMargin
    )
    test("06") - check(
      """sjsonnet.Error: division by zero
        |    at [BinaryOp /].(sjsonnet/test/resources/test_suite/error.06.jsonnet:17:15)
        |    at [ValidId err].(sjsonnet/test/resources/test_suite/error.06.jsonnet:18:22)
        |    at [Apply0].(sjsonnet/test/resources/test_suite/error.06.jsonnet:19:2)
        |    at [BinaryOp +].(sjsonnet/test/resources/test_suite/error.06.jsonnet:19:5)
        |""".stripMargin
    )
    test("07") - check(
      """sjsonnet.Error: sarcasm
        |    at [Error].(sjsonnet/test/resources/test_suite/error.07.jsonnet:18:31)
        |    at [Lookup].(sjsonnet/test/resources/test_suite/error.07.jsonnet:17:32)
        |    at [Apply1].(sjsonnet/test/resources/test_suite/error.07.jsonnet:18:20)
        |    at [ValidId toxic].(sjsonnet/test/resources/test_suite/error.07.jsonnet:19:1)
        |    at [BinaryOp +].(sjsonnet/test/resources/test_suite/error.07.jsonnet:19:7)
        |""".stripMargin
    )
    test("08") - check(
      """sjsonnet.Error: {"a": 1, "b": 2, "c": 3}
        |    at [Error].(sjsonnet/test/resources/test_suite/error.08.jsonnet:18:1)
        |""".stripMargin
    )
    test("array_fractional_index") - check(
      """sjsonnet.Error: array index was not integer: 1.5
        |    at [Lookup].(sjsonnet/test/resources/test_suite/error.array_fractional_index.jsonnet:17:10)
        |""".stripMargin
    )
    test("array_index_string") - check(
      """sjsonnet.Error: attempted to index a array with string foo
        |    at [Select foo].(sjsonnet/test/resources/test_suite/error.array_index_string.jsonnet:17:10)
        |""".stripMargin
    )
    test("array_large_index") - check(
      """sjsonnet.Error: array index was not integer: 18446744073709552000
        |    at [Lookup].(sjsonnet/test/resources/test_suite/error.array_large_index.jsonnet:17:10)
        |""".stripMargin
    )
    "assert.fail1" - check(
      """sjsonnet.Error: Assertion failed
        |    at [AssertExpr].(sjsonnet/test/resources/test_suite/error.assert.fail1.jsonnet:20:1)
        |""".stripMargin
    )
    "assert.fail2" - check(
      """sjsonnet.Error: Assertion failed: foo was not equal to bar
        |    at [AssertExpr].(sjsonnet/test/resources/test_suite/error.assert.fail2.jsonnet:20:1)
        |""".stripMargin
    )
    test("comprehension_spec_object") - check(
      """sjsonnet.Error: In comprehension, can only iterate over array, not object
        |    at [ForSpec].(sjsonnet/test/resources/test_suite/error.comprehension_spec_object.jsonnet:17:4)
        |    at [Comp].(sjsonnet/test/resources/test_suite/error.comprehension_spec_object.jsonnet:17:2)
        |""".stripMargin
    )
    test("comprehension_spec_object2") - check(
      """sjsonnet.Error: In comprehension, can only iterate over array, not object
        |    at [ForSpec].(sjsonnet/test/resources/test_suite/error.comprehension_spec_object2.jsonnet:17:13)
        |""".stripMargin
    )
    test("computed_field_scope") - {
      check(
        """sjsonnet.StaticError: Unknown variable: x
          |    at [Id x].(sjsonnet/test/resources/test_suite/error.computed_field_scope.jsonnet:17:21)
          |""".stripMargin
      )
    }
    test("divide_zero") - check(
      """sjsonnet.Error: division by zero
        |    at [BinaryOp /].(sjsonnet/test/resources/test_suite/error.divide_zero.jsonnet:17:5)
        |""".stripMargin
    )
    test("equality_function") - check(
      """sjsonnet.Error: cannot test equality of functions
        |    at [BinaryOp ==].(sjsonnet/test/resources/test_suite/error.equality_function.jsonnet:17:16)
        |""".stripMargin
    )
    test("field_not_exist") - check(
      """sjsonnet.Error: Field does not exist: y
        |    at [Select y].(sjsonnet/test/resources/test_suite/error.field_not_exist.jsonnet:17:9)
        |""".stripMargin
    )
    test("function_arg_positional_after_named") - check(
      """sjsonnet.ParseError: Expected no positional params after named params:19:11, found ")\n"
        |    at .(sjsonnet/test/resources/test_suite/error.function_arg_positional_after_named.jsonnet:19:11)
        |""".stripMargin
    )

    test("function_duplicate_arg") - check(
      """sjsonnet.Error: binding parameter a second time: x
        |    at [Apply].(sjsonnet/test/resources/test_suite/error.function_duplicate_arg.jsonnet:17:21)
        |""".stripMargin
    )
    test("function_duplicate_param") - check(
      """sjsonnet.ParseError: Expected no duplicate parameter: x:17:14, found ") x\n"
        |    at .(sjsonnet/test/resources/test_suite/error.function_duplicate_param.jsonnet:17:14)
        |""".stripMargin
    )
    test("function_too_many_args") - check(
      """sjsonnet.Error: Too many args, function has 2 parameter(s)
        |    at [Apply3].(sjsonnet/test/resources/test_suite/error.function_too_many_args.jsonnet:19:4)
        |""".stripMargin
    )
    test("import_empty") - check(
      """sjsonnet.Error: Couldn't import file: ""
        |    at [Import].(sjsonnet/test/resources/test_suite/error.import_empty.jsonnet:17:1)
        |""".stripMargin
    )
    test("import_folder") - check(
      """sjsonnet.Error: Couldn't import file: "lib"
        |    at [Import].(sjsonnet/test/resources/test_suite/error.import_folder.jsonnet:17:1)
        |""".stripMargin
    )
    test("import_folder_slash") - check(
      """sjsonnet.Error: Couldn't import file: "lib/"
        |    at [Import].(sjsonnet/test/resources/test_suite/error.import_folder_slash.jsonnet:17:1)
        |""".stripMargin
    )
    "import_static-check-failure" - {
      check(
        """sjsonnet.StaticError: Unknown variable: x
          |    at [Id x].(sjsonnet/test/resources/test_suite/lib/static_check_failure.jsonnet:2:1)
          |    at [Import].(sjsonnet/test/resources/test_suite/error.import_static-check-failure.jsonnet:1:1)
          |""".stripMargin
      )
    }
    "import_syntax-error" - check(
      """sjsonnet.ParseError: Expected "\"":2:1, found ""
        |    at .(sjsonnet/test/resources/test_suite/lib/syntax_error.jsonnet:2:1)
        |    at [Import].(sjsonnet/test/resources/test_suite/error.import_syntax-error.jsonnet:1:1)
        |""".stripMargin
    )
    test("inside_equals_array") - check(
      """sjsonnet.Error: foobar
        |    at [Error].(sjsonnet/test/resources/test_suite/error.inside_equals_array.jsonnet:18:18)
        |    at [BinaryOp ==].(sjsonnet/test/resources/test_suite/error.inside_equals_array.jsonnet:19:3)
        |""".stripMargin
    )
    test("inside_equals_object") - check(
      """sjsonnet.Error: foobar
        |    at [Error].(sjsonnet/test/resources/test_suite/error.inside_equals_object.jsonnet:18:22)
        |    at [BinaryOp ==].(sjsonnet/test/resources/test_suite/error.inside_equals_object.jsonnet:19:3)
        |""".stripMargin
    )
    test("inside_tostring_array") - check(
      """sjsonnet.Error: foobar
        |    at [Error].(sjsonnet/test/resources/test_suite/error.inside_tostring_array.jsonnet:17:8)
        |    at [BinaryOp +].(sjsonnet/test/resources/test_suite/error.inside_tostring_array.jsonnet:17:24)
        |""".stripMargin
    )
    test("inside_tostring_object") - check(
      """sjsonnet.Error: foobar
        |    at [Error].(sjsonnet/test/resources/test_suite/error.inside_tostring_object.jsonnet:17:12)
        |    at [BinaryOp +].(sjsonnet/test/resources/test_suite/error.inside_tostring_object.jsonnet:17:29)
        |""".stripMargin
    )
    test("invariant") - {
      test("avoid_output_change") - check(
        """sjsonnet.Error: Assertion failed
          |    at [Assert].(sjsonnet/test/resources/test_suite/error.invariant.avoid_output_change.jsonnet:18:15)
          |""".stripMargin
      )
      test("equality") - check(
        """sjsonnet.Error: Assertion failed
          |    at [Assert].(sjsonnet/test/resources/test_suite/error.invariant.equality.jsonnet:17:10)
          |    at [BinaryOp ==].(sjsonnet/test/resources/test_suite/error.invariant.equality.jsonnet:17:24)
          |""".stripMargin
      )
      test("option") - check(
        """sjsonnet.Error: Assertion failed: Option "d" not in ["a", "b", "c"].
          |    at [Assert].(sjsonnet/test/resources/test_suite/error.invariant.option.jsonnet:19:57)
          |""".stripMargin
      )
      test("simple") - check(
        """sjsonnet.Error: Assertion failed
          |    at [Assert].(sjsonnet/test/resources/test_suite/error.invariant.simple.jsonnet:18:10)
          |""".stripMargin
      )
      test("simple2") - check(
        """sjsonnet.Error: Assertion failed: my error message
          |    at [Assert].(sjsonnet/test/resources/test_suite/error.invariant.simple2.jsonnet:18:12)
          |""".stripMargin
      )
      test("simple3") - check(
        """sjsonnet.Error: my error message
          |    at [Error].(sjsonnet/test/resources/test_suite/error.invariant.simple3.jsonnet:18:10)
          |""".stripMargin
      )
    }
    test("native_not_found") - check(
      """sjsonnet.Error: Native function non_existent_native not found
        |    at [std.native].(sjsonnet/test/resources/test_suite/error.native_not_found.jsonnet:17:11)
        |""".stripMargin
    )
    test("obj_assert") - {
      test("fail1") - check(
        """sjsonnet.Error: Assertion failed
          |    at [Assert].(sjsonnet/test/resources/test_suite/error.obj_assert.fail1.jsonnet:20:25)
          |    at [BinaryOp ==].(sjsonnet/test/resources/test_suite/error.obj_assert.fail1.jsonnet:20:38)
          |    at [And].(sjsonnet/test/resources/test_suite/error.obj_assert.fail1.jsonnet:20:50)
          |""".stripMargin
      )
      test("fail2") - check(
        """sjsonnet.Error: Assertion failed: foo was not equal to bar
          |    at [Assert].(sjsonnet/test/resources/test_suite/error.obj_assert.fail2.jsonnet:20:25)
          |    at [BinaryOp ==].(sjsonnet/test/resources/test_suite/error.obj_assert.fail2.jsonnet:20:74)
          |    at [And].(sjsonnet/test/resources/test_suite/error.obj_assert.fail2.jsonnet:20:86)
          |""".stripMargin
      )
    }

    test("import_wrong_nr_args") - check(
      """|sjsonnet.Error: Function parameter y not bound in call
         |    at [Apply1].(sjsonnet/test/resources/imports/error.import_wrong_nr_args.jsonnet:3:6)
         |""".stripMargin,
      suite = "imports"
    )

    test("wrong_named_arg") - check(
      """|sjsonnet.Error: Function has no parameter z
         |    at [Apply].(sjsonnet/test/resources/imports/error.wrong_named_arg.jsonnet:3:6)
         |""".stripMargin,
      suite = "imports"
    )

    test("tailstrict_stack") - check(
      """|sjsonnet.Error: n is 0
         |    at [Error].(sjsonnet/test/resources/test_suite/error.tailstrict_stack.jsonnet:10:9)
         |    at [Apply2].(sjsonnet/test/resources/test_suite/error.tailstrict_stack.jsonnet:12:26)
         |    at [Apply2].(sjsonnet/test/resources/test_suite/error.tailstrict_stack.jsonnet:14:37)
         |    at [ValidId result].(sjsonnet/test/resources/test_suite/error.tailstrict_stack.jsonnet:15:5)
         |    at [Apply1].(sjsonnet/test/resources/test_suite/error.tailstrict_stack.jsonnet:19:18)
         |""".stripMargin
    )

    test("too_many_arg") - check(
      """|sjsonnet.Error: Too many args, function has 2 parameter(s)
         |    at [Apply].(sjsonnet/test/resources/imports/error.too_many_arg.jsonnet:3:6)
         |""".stripMargin,
      suite = "imports"
    )

    test("too_many_arg_with_named_arg") - check(
      """|sjsonnet.Error: binding parameter a second time: x
         |    at [Apply].(sjsonnet/test/resources/imports/error.too_many_arg_with_named_arg.jsonnet:2:2)
         |""".stripMargin,
      suite = "imports"
    )

    test("format_func") - check(
      """|sjsonnet.Error: Cannot format function value
         |    at [Function].(sjsonnet/test/resources/db/error.format_func.jsonnet:1:9)
         |    at [std.format].(sjsonnet/test/resources/db/error.format_func.jsonnet:1:7)
         |""".stripMargin,
      suite = "db"
    )

    test("manifest_toml_wrong_type") - check(
      """|sjsonnet.Error: Wrong parameter type: expected Object, got array
         |    at [std.manifestTomlEx].(sjsonnet/test/resources/test_suite/error.manifest_toml_wrong_type.jsonnet:17:19)
         |""".stripMargin
    )

    test("manifest_toml_null_value") - check(
      """|sjsonnet.Error: Tried to manifest "null"
         |    at [std.manifestTomlEx].(sjsonnet/test/resources/test_suite/error.manifest_toml_null_value.jsonnet:17:19)
         |""".stripMargin
    )

    test("integer_conversion") - check(
      """|sjsonnet.Error: numeric value outside safe integer range for bitwise operation
         |    at [BinaryOp <<].(sjsonnet/test/resources/test_suite/error.integer_conversion.jsonnet:3:12)
         |""".stripMargin
    )

    test("integer_left_shift") - check(
      """|sjsonnet.Error: numeric value outside safe integer range for bitwise operation
         |    at [BinaryOp <<].(sjsonnet/test/resources/test_suite/error.integer_left_shift.jsonnet:3:13)
         |""".stripMargin
    )

    test("integer_left_shift_runtime") - check(
      """|sjsonnet.Error: numeric value outside safe integer range for bitwise operation
         |    at [BinaryOp <<].(sjsonnet/test/resources/test_suite/error.integer_left_shift_runtime.jsonnet:2:13)
         |""".stripMargin
    )
  }
}
