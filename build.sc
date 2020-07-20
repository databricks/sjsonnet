import mill._, scalalib._, publish._, scalajslib.ScalaJSModule
val sjsonnetVersion = "0.2.6"

object sjsonnet extends Cross[SjsonnetModule]("2.12.12", "2.13.3")
class SjsonnetModule(val crossScalaVersion: String) extends Module {
  def millSourcePath = super.millSourcePath / ammonite.ops.up
  trait SjsonnetCrossModule extends CrossScalaModule with PublishModule{

    def artifactName = "sjsonnet"
    def platformSegment: String
    def sources = T.sources(
      millSourcePath / "src",
      millSourcePath / s"src-$platformSegment"
    )
    def crossScalaVersion = SjsonnetModule.this.crossScalaVersion

    def ivyDeps = Agg(
      ivy"com.lihaoyi::fastparse::2.3.0",
      ivy"com.lihaoyi::pprint::0.5.9",
      ivy"com.lihaoyi::ujson::1.2.0",
      ivy"com.lihaoyi::scalatags::0.9.1",
      ivy"org.scala-lang.modules::scala-collection-compat::2.1.4"
    )
    def publishVersion = sjsonnetVersion

    def generatedSources = T{
      os.write(
        T.ctx().dest / "Version.scala",
        s"""package sjsonnet
           |object Version{
           |  val version = ${pprint.Util.literalize(sjsonnetVersion)}
           |}
           |""".stripMargin
      )
      Seq(PathRef(T.ctx().dest / "Version.scala"))
    }
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
    trait CrossTests extends ScalaModule with TestModule {
      def platformSegment = SjsonnetCrossModule.this.platformSegment
      def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.7.4")
      def testFrameworks = Seq("utest.runner.Framework")
      def sources = T.sources(
        millSourcePath / "src",
        millSourcePath / s"src-$platformSegment"
      )
    }
  }
  object js extends SjsonnetCrossModule with ScalaJSModule{
    def scalaJSVersion = "1.1.1"
    def platformSegment = "js"
    object test extends Tests with CrossTests
  }
  object jvm extends SjsonnetCrossModule {
    def mainClass = Some("sjsonnet.SjsonnetMain")
    def platformSegment = "jvm"
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.lihaoyi::os-lib:0.7.1",
      ivy"com.github.scopt::scopt::3.7.1",
    )
    def compileIvyDeps = Agg( ivy"com.lihaoyi::acyclic:0.2.0")
    def scalacOptions = Seq("-P:acyclic:force")
    def scalacPluginIvyDeps = Agg( ivy"com.lihaoyi::acyclic:0.2.0")
    object test extends Tests with CrossTests{
      def compileIvyDeps = Agg( ivy"com.lihaoyi::acyclic:0.2.0")
      def scalacOptions = Seq("-P:acyclic:force")
      def scalacPluginIvyDeps = Agg( ivy"com.lihaoyi::acyclic:0.2.0")
      def forkOptions = Seq("-Xss100m")
    }
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
    def moduleDeps = Seq(SjsonnetModule.this.jvm, client)
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
