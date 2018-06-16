import mill._, scalalib._

object sjsonnet extends ScalaModule{
  def scalaVersion = "2.12.6"
  def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:1.0.0",
    ivy"com.lihaoyi::pprint:0.5.3",
    ivy"com.lihaoyi::ammonite-ops:1.1.2",
    ivy"com.lihaoyi::ujson:0.6.6"
  )
  object test extends Tests{
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.6.4")
    def testFrameworks = Seq("utest.runner.Framework")
  }
}
