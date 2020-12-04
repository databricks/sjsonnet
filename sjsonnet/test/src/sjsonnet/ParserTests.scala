package sjsonnet
import utest._
import Expr._
import fastparse.Parsed
object ParserTests extends TestSuite{
  def parse(s: String) = fastparse.parse(s, Parser.document(_)).get.value._1
  def parseErr(s: String) = fastparse.parse(s, Parser.document(_), verboseFailures = true).asInstanceOf[Parsed.Failure].msg
  def tests = Tests{
    test("hello") {
      parse("true") ==> True(0)

      parse("123 + 456 + 789") ==>
        BinaryOp(10, BinaryOp(4, Num(0, 123), BinaryOp.`+`, Num(6, 456)), BinaryOp.`+`, Num(12, 789))

      parse("1 * 2 + 3") ==>
        BinaryOp(6, BinaryOp(2, Num(0, 1), BinaryOp.`*`, Num(4, 2)), BinaryOp.`+`, Num(8, 3))

      parse("1 + 2 * 3") ==>
        BinaryOp(2, Num(0, 1), BinaryOp.`+`, BinaryOp(6, Num(4, 2), BinaryOp.`*`, Num(8, 3)))
    }
    test("duplicateFields") {
      parseErr("{ a: 1, a: 2 }") ==> """Expected no duplicate field: a:1:14, found "}""""
    }
    test("givenDuplicateFieldsInListComprehension_expectError") {
      parseErr("""{ ["bar"]: x for x in [1, 2]}""") ==> """Expected no duplicate field: "bar" :1:29, found "}""""
    }
  }
}
