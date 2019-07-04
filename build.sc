import mill._, scalalib._, publish._
val sjsonnetVersion = "0.1.4"

object sjsonnet extends Cross[SjsonnetModule]("2.12.8", "2.13.0")
class SjsonnetModule(val crossScalaVersion: String) extends CrossScalaModule with PublishModule {
  def mainClass = Some("sjsonnet.SjsonnetMain")
  def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:2.1.3",
    ivy"com.lihaoyi::pprint:0.5.5",
    ivy"com.lihaoyi::os-lib:0.3.0",
    ivy"com.lihaoyi::ujson:0.7.5",
    ivy"com.lihaoyi::scalatags:0.7.0",
    ivy"com.github.scopt::scopt:3.7.1",
    ivy"org.scala-lang.modules::scala-collection-compat:2.0.0"
  )
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

  object test extends Tests{
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.1")
    def testFrameworks = Seq("utest.runner.Framework")
  }


  object client extends JavaModule {
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

  object server extends ScalaModule{
    def scalaVersion = crossScalaVersion
    def moduleDeps = Seq(SjsonnetModule.this, client)
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
