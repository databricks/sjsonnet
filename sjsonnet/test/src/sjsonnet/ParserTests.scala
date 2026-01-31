package sjsonnet

import scala.collection.mutable
import utest._
import Expr._
import fastparse.Parsed
import Val.{Num, True}
import sjsonnet.Expr.FieldName.Fixed
object ParserTests extends TestSuite {
  def parse(s: String): Expr = fastparse
    .parse(
      s,
      new Parser(null, mutable.HashMap.empty, mutable.HashMap.empty).document(_)
    )
    .get
    .value
    ._1
  def parseErr(s: String): String = fastparse
    .parse(
      s,
      new Parser(null, mutable.HashMap.empty, mutable.HashMap.empty)
        .document(_),
      verboseFailures = true
    )
    .asInstanceOf[Parsed.Failure]
    .msg
  val dummyFS = new FileScope(null)
  def pos(i: Int) = new Position(dummyFS, i)
  def tests: Tests = Tests {
    test("hello") {
      parse("true") ==> True(pos(0))

      parse("123 + 456 + 789") ==>
      BinaryOp(
        pos(10),
        BinaryOp(pos(4), Num(pos(0), 123), BinaryOp.OP_+, Num(pos(6), 456)),
        BinaryOp.OP_+,
        Num(pos(12), 789)
      )

      parse("1 * 2 + 3") ==>
      BinaryOp(
        pos(6),
        BinaryOp(pos(2), Num(pos(0), 1), BinaryOp.OP_*, Num(pos(4), 2)),
        BinaryOp.OP_+,
        Num(pos(8), 3)
      )

      parse("1 + 2 * 3") ==>
      BinaryOp(
        pos(2),
        Num(pos(0), 1),
        BinaryOp.OP_+,
        BinaryOp(pos(6), Num(pos(4), 2), BinaryOp.OP_*, Num(pos(8), 3))
      )
    }
    test("duplicateFields") {
      parseErr("{ a: 1, a: 2 }") ==> """Expected no duplicate field: a:1:14, found "}""""
    }
    test("localInObj") {
      parse("""{
              |local x = 1,
              |a: x,
              |}""".stripMargin).toString ==> (ObjBody
        .MemberList(
          pos(0),
          Array(Bind(pos(8), "x", null, Num(pos(12), 1))),
          Array(
            Member
              .Field(pos(15), Fixed("a"), false, null, Member.Visibility.Normal, Id(pos(18), "x"))
          ),
          null
        ))
        .toString
      parseErr("""{
                 |local x = 1,
                 |local x = x + 1,
                 |a: x,
                 |}""".stripMargin) ==> """Expected no duplicate local: x:5:1, found "}""""
    }
    test("givenDuplicateFieldsInListComprehension_expectError") {
      parseErr(
        """{ ["bar"]: x for x in [1, 2]}"""
      ) ==> """Expected no duplicate field: "bar" :1:29, found "}""""
    }

    test("computedImports") {
      parse("""local foo = import "foo"; 0""") ==>
      LocalExpr(pos(6), Array(Bind(pos(6), "foo", null, Import(pos(12), "foo"))), Num(pos(26), 0.0))
      parse("""local foo = (import "foo") + bar; 0""") ==>
      LocalExpr(
        pos(6),
        Array(
          Bind(
            pos(6),
            "foo",
            null,
            BinaryOp(pos(27), Import(pos(13), "foo"), 3, Id(pos(29), "bar"))
          )
        ),
        Num(pos(34), 0.0)
      )

      parseErr("""import "foo".bar""")
      parseErr("""import "foo"[1]""")

      parseErr("""local foo = import ("foo" + bar); 0""") ==>
      """Expected string literal (computed imports are not allowed):1:33, found "; 0""""
      parseErr("""local foo = import "foo" + bar; 0""") ==>
      """Expected string literal (computed imports are not allowed):1:31, found "; 0""""
    }

    test("id starts with number") {
      parseErr("""{1_n: "foo",}""") ==> """Expected "}":1:2, found "1_n: \"foo\"""""
    }

    test("identifier underscore") {
      // From go-jsonnet TestIdentifierUnderscore
      // _123 should parse as an identifier, not a number
      parse("_123") ==> Id(pos(0), "_123")
    }

    test("underscore digit separators") {
      // Valid cases from go-jsonnet TestNumberSeparators
      // {"123_456", "", Tokens{{kind: tokenNumber, data: "123456"}}}
      parse("123_456") ==> Num(pos(0), 123456)
      // {"1_750_000", "", Tokens{{kind: tokenNumber, data: "1750000"}}}
      parse("1_750_000") ==> Num(pos(0), 1750000)
      // {"1_2_3", "", Tokens{{kind: tokenNumber, data: "123"}}}
      parse("1_2_3") ==> Num(pos(0), 123)
      // {"3.141_592", "", Tokens{{kind: tokenNumber, data: "3.141592"}}}
      parse("3.141_592") ==> Num(pos(0), 3.141592)
      // {"1_200.0", "", Tokens{{kind: tokenNumber, data: "1200.0"}}}
      parse("1_200.0") ==> Num(pos(0), 1200.0)
      // {"0e1_01", "", Tokens{{kind: tokenNumber, data: "0e101"}}}
      parse("0e1_01") ==> Num(pos(0), 0e101)
      // {"10_10e3", "", Tokens{{kind: tokenNumber, data: "1010e3"}}}
      parse("10_10e3") ==> Num(pos(0), 1010e3)
      // {"2_3e1_2", "", Tokens{{kind: tokenNumber, data: "23e12"}}}
      parse("2_3e1_2") ==> Num(pos(0), 23e12)
      // {"1.1_2e100", "", Tokens{{kind: tokenNumber, data: "1.12e100"}}}
      parse("1.1_2e100") ==> Num(pos(0), 1.12e100)
      // {"1.1e-10_1", "", Tokens{{kind: tokenNumber, data: "1.1e-101"}}}
      parse("1.1e-10_1") ==> Num(pos(0), 1.1e-101)
      // {"9.109_383_56e-31", "", Tokens{{kind: tokenNumber, data: "9.10938356e-31"}}}
      parse("9.109_383_56e-31") ==> Num(pos(0), 9.10938356e-31)

      // {"01_100", "", Tokens{{kind: tokenNumber, data: "0"}, {kind: tokenNumber, data: "1100"}}}
      // go-jsonnet lexer produces: 0, 1100 (two number tokens)
      // In sjsonnet, 0 is parsed as number (leading zero rule), then 1_100 is parsed as number
      // Two adjacent values without operator = parse error
      parse("0") ==> Num(pos(0), 0)
      parse("1_100") ==> Num(pos(0), 1100)
      parseErr("01_100")

      // {"1_2.3_4.5_6.7_8", "", Tokens{
      //   {kind: tokenNumber, data: "12.34"},
      //   {kind: tokenDot, data: "."},
      //   {kind: tokenNumber, data: "56.78"},
      // }}
      // go-jsonnet lexer produces: 12.34, ., 56.78
      // In sjsonnet parser, after 12.34, the . starts field access
      // But 5_6 starts with digit which is not a valid identifier
      // So 1_2.3_4.5_6.7_8 as a complete expression should fail
      // We verify individual parts parse correctly:
      parse("1_2.3_4") ==> Num(pos(0), 12.34)
      parse("5_6.7_8") ==> Num(pos(0), 56.78)
      // And the combined expression fails (field name can't start with digit)
      parseErr("1_2.3_4.5_6.7_8")

      // {"1e2_3e4", "", Tokens{
      //   {kind: tokenNumber, data: "1e23"},
      //   {kind: tokenIdentifier, data: "e4"},
      // }}
      // go-jsonnet lexer produces: 1e23, e4 (identifier)
      // In sjsonnet, 1e2_3 is parsed as number 1e23, then e4 is identifier
      // Two adjacent values without operator = parse error
      parse("1e2_3") ==> Num(pos(0), 1e23)
      parseErr("1e2_3e4")

      // Error cases from go-jsonnet TestNumberSeparators
      // {"0_5", "snippet:1:2 Couldn't lex number, _ not allowed after leading 0", Tokens{}}
      // 0 followed by _5 (identifier) - two adjacent expressions without operator
      parseErr("0_5")

      // {"123456_!", "snippet:1:8 Couldn't lex number, junk after '_': '!'", Tokens{}}
      // Trailing underscore not followed by digit
      parseErr("123456_")

      // {"123__456", "snippet:1:5 Couldn't lex number, junk after '_': '_'", Tokens{}}
      // Consecutive underscores not allowed
      parseErr("123__456")

      // {"1_200_.0", "snippet:1:7 Couldn't lex number, junk after '_': '.'", Tokens{}}
      // In sjsonnet this parses as (1200)._0 (field access) which is syntactically valid
      // So we don't test this as an error

      // {"1_200._0", "snippet:1:7 Couldn't lex number, junk after decimal point: '_'", Tokens{}}
      // In sjsonnet this parses as (1200)._0 (field access) which is syntactically valid
      // So we don't test this as an error

      // {"1_200_e2", "snippet:1:7 Couldn't lex number, junk after '_': 'e'", Tokens{}}
      // 1_200 followed by _e2 (identifier) - two adjacent expressions without operator
      parseErr("1_200_e2")

      // {"1_200e_2", "snippet:1:7 Couldn't lex number, junk after 'E': '_'", Tokens{}}
      // 1_200 followed by e_2 (identifier) - two adjacent expressions without operator
      parseErr("1_200e_2")

      // {"200e-_2", "snippet:1:6 Couldn't lex number, junk after exponent sign: '_'", Tokens{}}
      // 200 followed by e-_2 - parses as 200 e - _2 (subtraction expression with identifiers)
      // This actually parses! So we need to verify it differently
      // Actually: 200 is number, then "e" is identifier, then "-" is operator, then "_2" is identifier
      // This is: 200 followed by identifier "e" - two adjacent values = parse error
      parseErr("200e-_2")

      // {"200e+_2", "snippet:1:6 Couldn't lex number, junk after exponent sign: '_'", Tokens{}}
      // Similar to above: 200 followed by identifier "e" - two adjacent values = parse error
      parseErr("200e+_2")
    }
  }
}
