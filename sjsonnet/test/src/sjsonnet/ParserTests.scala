package sjsonnet
import utest._
import Expr._
import fastparse.Parsed
import Val.{True, Num}
object ParserTests extends TestSuite{
  def parse(s: String) = fastparse.parse(s, new Parser(null).document(_)).get.value._1
  def parseErr(s: String) = fastparse.parse(s, new Parser(null).document(_), verboseFailures = true).asInstanceOf[Parsed.Failure].msg
  val dummyFS = new FileScope(null)
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
    test("computedImports") {
      parse("""local foo = import "foo"; 0""") ==>
        LocalExpr(pos(6), Array(Bind(pos(6), "foo", null, Import(pos(12), "foo"))), Num(pos(26),0.0))
      parse("""local foo = (import "foo") + bar; 0""") ==>
        LocalExpr(pos(6), Array(Bind(pos(6), "foo", null, BinaryOp(pos(27), Import(pos(13), "foo"), 3, Id(pos(29), "bar")))), Num(pos(34),0.0))
      parseErr("""local foo = import ("foo" + bar); 0""") ==> """Expected string literal (computed imports are not allowed):1:33, found "; 0""""
      parseErr("""local foo = import "foo" + bar; 0""") ==> """Expected string literal (computed imports are not allowed):1:31, found "; 0""""
    }
  }
}
