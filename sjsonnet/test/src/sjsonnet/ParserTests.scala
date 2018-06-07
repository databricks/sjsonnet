package sjsonnet
import utest._
object ParserTests extends TestSuite{
  def tests = Tests{
    'hello - {
      Parser.expr
    }
  }

}