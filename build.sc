import mill._
import scalalib._

val sjsonnetVersion = "0.1.0"
object sjsonnet extends ScalaModule{
  def scalaVersion = "2.12.6"
  def mainClass = Some("sjsonnet.SjsonnetMain")
  def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:1.0.0",
    ivy"com.lihaoyi::pprint:0.5.3",
    ivy"com.lihaoyi::ammonite-ops:1.1.2",
    ivy"com.lihaoyi::ujson:0.6.7",
    ivy"com.lihaoyi::scalatags:0.6.7",
    ivy"com.github.scopt::scopt:3.5.0"
  )
  object test extends Tests{
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.6.4")
    def testFrameworks = Seq("utest.runner.Framework")
  }
}

object client extends JavaModule{
  def ivyDeps = Agg(
    ivy"org.scala-sbt.ipcsocket:ipcsocket:1.0.0".exclude(
      "net.java.dev.jna" -> "jna",
      "net.java.dev.jna" -> "jna-platform"
    )
  )
  object test extends Tests{
    def testFrameworks = Seq("com.novocode.junit.JUnitFramework")
    def ivyDeps = Agg(ivy"com.novocode:junit-interface:0.11")
  }
}

object launcher extends ScalaModule{
  def scalaVersion = "2.12.6"
  def moduleDeps = Seq(sjsonnet, client)
  def ivyDeps = Agg(
    ivy"org.scala-sbt.ipcsocket:ipcsocket:1.0.0".exclude(
      "net.java.dev.jna" -> "jna",
      "net.java.dev.jna" -> "jna-platform"
    ),
    ivy"net.java.dev.jna:jna:4.5.0",
    ivy"net.java.dev.jna:jna-platform:4.5.0"
  )


  override def prependShellScript = mill.modules.Jvm.universalScript(
    shellCommands = {
      def java(mainClass: String) =
        s"""exec java -DSJSONNET_EXECUTABLE=$$0 -DSJSONNET_VERSION=$sjsonnetVersion $$JAVA_OPTS -cp $$0 $mainClass "$$@""""

      s"""case "$$1" in
         |  -i | --interactive )
         |    ${java("sjsonnet.SjsonnetMain")}
         |    ;;
         |  *)
         |    ${java("sjsonnet.client.SjsonnetClientMain")}
         |    ;;
         |esac""".stripMargin
    },
    cmdCommands = {
      def java(mainClass: String) =
        s"""java -DSJSONNET_EXECUTABLE=%~dpnx0  -DSJSONNET_VERSION=$sjsonnetVersion %JAVA_OPTS% -cp %~dpnx0 $mainClass %*"""

      s"""if "%1" == "-i" set _I_=true
         |if "%1" == "--interactive" set _I_=true
         |if defined _I_ (
         |  ${java("sjsonnet.SjsonnetMain")}
         |) else (
         |  ${java("sjsonnet.client.SjsonnetClientMain")}
         |)""".stripMargin
    }
  )
  object test extends Tests{
    def testFrameworks = Seq("com.novocode.junit.JUnitFramework")
    def ivyDeps = Agg(ivy"com.novocode:junit-interface:0.11")
  }
}
