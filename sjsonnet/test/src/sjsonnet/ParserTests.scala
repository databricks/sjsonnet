package sjsonnet
import utest._
import Expr._
object ParserTests extends TestSuite{
  def tests = Tests{
    'hello - {
      Parser.expr.parse("true").get.value ==> True

      Parser.expr.parse("123 + 456 + 789").get.value ==>
        BinaryOp(BinaryOp(Num(123), "+", Num(456)), "+", Num(789))

      Parser.expr.parse("1 * 2 + 3").get.value ==>
        BinaryOp(BinaryOp(Num(1), "*", Num(2)), "+", Num(3))

      Parser.expr.parse("1 + 2 * 3").get.value ==>
        BinaryOp(Num(1), "+", BinaryOp(Num(2), "*", Num(3)))

      Parser.expr.parse("2 | 3 * 2 + 3 | 4").get.value ==>
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
  }

}