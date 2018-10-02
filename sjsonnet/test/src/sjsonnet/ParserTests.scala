package sjsonnet
import utest._
import Expr._
object ParserTests extends TestSuite{
  def parse(s: String) = new Parser().parse(s).get.value
  def tests = Tests{
    'hello - {
      parse("true") ==> True(0)

      parse("123 + 456 + 789") ==>
        BinaryOp(10, BinaryOp(4, Num(0, 123), BinaryOp.`+`, Num(6, 456)), BinaryOp.`+`, Num(12, 789))

      parse("1 * 2 + 3") ==>
        BinaryOp(6, BinaryOp(2, Num(0, 1), BinaryOp.`*`, Num(4, 2)), BinaryOp.`+`, Num(8, 3))

      parse("1 + 2 * 3") ==>
        BinaryOp(2, Num(0, 1), BinaryOp.`+`, BinaryOp(6, Num(4, 2), BinaryOp.`*`, Num(8, 3)))
//
//      parse("2 | 3 * 2 + 3 | 4") ==>
//        BinaryOp(
//          BinaryOp(
//            Num(2),
//            "|",
//            BinaryOp(
//              BinaryOp(
//                Num(3),
//                "*",
//                Num(2)
//              ),
//              "+",
//              Num(3)
//            )
//          ),
//          "|",
//          Num(4)
//        )
//    }
//
//    'array - {
//      parse("[]") ==> Arr(Nil)
//      parse("[true]") ==> Arr(Seq(True))
//      parse("[1, 2]") ==> Arr(Seq(Num(1), Num(2)))
//      parse("[1, [2, 3], 4]") ==> Arr(Seq(Num(1), Arr(Seq(Num(2), Num(3))), Num(4)))
    }
  }
}
