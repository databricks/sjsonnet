import mill._, scalalib._, publish._
val sjsonnetVersion = "0.1.0"

trait SjsonnetJavaModule extends PublishModule{
  def publishVersion = sjsonnetVersion

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.lihaoyi",
    url = "https://github.com/lihaoyi/sjsonnet",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("lihaoyi", "sjsonnet"),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi","https://github.com/lihaoyi")
    )
  )

}
trait SjsonnetScalaModule extends SjsonnetJavaModule with ScalaModule{
  def scalaVersion = "2.12.6"
//  def compileIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.1.7")
//  def scalacOptions = Seq("-P:acyclic:force")
//  def scalacPluginIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.1.7")
}
object sjsonnet extends SjsonnetScalaModule{
  def mainClass = Some("sjsonnet.SjsonnetMain")
  def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:2.0.0b1",
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


  object client extends SjsonnetJavaModule {
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

  object server extends SjsonnetScalaModule{
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

}
