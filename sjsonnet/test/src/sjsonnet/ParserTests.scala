package sjsonnet
import utest._
import Expr._
import fastparse.Parsed
import Val.{True, Num}
object ParserTests extends TestSuite{
  def parse(s: String) = fastparse.parse(s, new Parser(null).document(_)).get.value._1
  def parseErr(s: String) = fastparse.parse(s, new Parser(null).document(_), verboseFailures = true).asInstanceOf[Parsed.Failure].msg
  val dummyFS = new FileScope(null, Map.empty)
  def pos(i: Int) = new Position(dummyFS, i)
  def tests = Tests{
    test("hello") {
      parse("true") ==> True(pos(0))

      parse("123 + 456 + 789") ==>
        BinaryOp(pos(10), BinaryOp(pos(4), Num(pos(0), 123), BinaryOp.OP_+, Num(pos(6), 456)), BinaryOp.OP_+, Num(pos(12), 789))

      parse("1 * 2 + 3") ==>
        BinaryOp(pos(6), BinaryOp(pos(2), Num(pos(0), 1), BinaryOp.OP_*, Num(pos(4), 2)), BinaryOp.OP_+, Num(pos(8), 3))

      parse("1 + 2 * 3") ==>
        BinaryOp(pos(2), Num(pos(0), 1), BinaryOp.OP_+, BinaryOp(pos(6), Num(pos(4), 2), BinaryOp.OP_*, Num(pos(8), 3)))
    }
    test("duplicateFields") {
      parseErr("{ a: 1, a: 2 }") ==> """Expected no duplicate field: a:1:14, found "}""""
    }
    test("givenDuplicateFieldsInListComprehension_expectError") {
      parseErr("""{ ["bar"]: x for x in [1, 2]}""") ==> """Expected no duplicate field: "bar" :1:29, found "}""""
    }
  }
}
