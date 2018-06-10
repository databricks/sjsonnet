package sjsonnet
import utest._
import Expr._
object ParserTests extends TestSuite{
  def parse(s: String) = Parser.expr.parse(s).get.value
  def tests = Tests{
    'hello - {
      parse("true") ==> True

      parse("123 + 456 + 789") ==>
        BinaryOp(BinaryOp(Num(123), "+", Num(456)), "+", Num(789))

      parse("1 * 2 + 3") ==>
        BinaryOp(BinaryOp(Num(1), "*", Num(2)), "+", Num(3))

      parse("1 + 2 * 3") ==>
        BinaryOp(Num(1), "+", BinaryOp(Num(2), "*", Num(3)))

      parse("2 | 3 * 2 + 3 | 4") ==>
        BinaryOp(
          BinaryOp(
            Num(2),
            "|",
            BinaryOp(
              BinaryOp(
                Num(3),
                "*",
                Num(2)
              ),
              "+",
              Num(3)
            )
          ),
          "|",
          Num(4)
        )
    }

    'array - {
      parse("[]") ==> Arr(Nil)
      parse("[true]") ==> Arr(Seq(True))
      parse("[1, 2]") ==> Arr(Seq(Num(1), Num(2)))
      parse("[1, [2, 3], 4]") ==> Arr(Seq(Num(1), Arr(Seq(Num(2), Num(3))), Num(4)))
    }
  }

}