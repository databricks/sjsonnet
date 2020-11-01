package sjsonnet

import utest._
import FileTests._

object FileTestsJvmOnly extends TestSuite{
  def tests = Tests{
    test("stdlib") - check()
  }
}
