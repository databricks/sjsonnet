import mill._, scalalib._, publish._, scalajslib._, scalanativelib._, scalanativelib.api._
val sjsonnetVersion = "0.4.4-SNAPSHOT"

object sjsonnet extends Cross[SjsonnetModule]("2.12.13", "2.13.10")
class SjsonnetModule(val crossScalaVersion: String) extends Module {

  override def millSourcePath = super.millSourcePath / os.up
  trait SjsonnetJvmNative extends SjsonnetCrossModule {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.lihaoyi::os-lib::0.9.1",
      ivy"com.lihaoyi::mainargs::0.4.0"
    )
  }
  trait SjsonnetCrossModule extends CrossScalaModule with PublishModule{

    override def artifactName = "sjsonnet"
    def crossScalaVersion = SjsonnetModule.this.crossScalaVersion

    override def ivyDeps = Agg(
      ivy"com.lihaoyi::fastparse::3.0.1",
      ivy"com.lihaoyi::pprint::0.8.1",
      ivy"com.lihaoyi::ujson::3.1.0",
      ivy"com.lihaoyi::scalatags::0.12.0",
      ivy"org.scala-lang.modules::scala-collection-compat::2.9.0"
    )
    def publishVersion = sjsonnetVersion

    override def generatedSources = T{
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
      organization = "com.databricks",
      url = "https://github.com/lihaoyi/sjsonnet",
      licenses = Seq(License.MIT),
      versionControl = VersionControl.github("lihaoyi", "sjsonnet"),
      developers = Seq(
        Developer("lihaoyi", "Li Haoyi","https://github.com/lihaoyi")
      )
    )
    trait CrossTests extends ScalaModule with TestModule {
      override def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.8.1")
      override def testFramework = "utest.runner.Framework"
    }
  }
  object js extends SjsonnetCrossModule with ScalaJSModule{
    def scalaJSVersion = "1.13.1"
    override def sources = T.sources(
      millSourcePath / "src",
      millSourcePath / "src-js",
      millSourcePath / "src-jvm-js"
    )
    object test extends Tests with CrossTests {
      override def sources = T.sources(
        millSourcePath / "src",
        millSourcePath / "src-js",
        millSourcePath / "src-jvm-js"
      )
    }
  }
  object native extends SjsonnetCrossModule with SjsonnetJvmNative with ScalaNativeModule{
    def scalaNativeVersion = "0.4.12"
    override def sources = T.sources(
      millSourcePath / "src",
      millSourcePath / "src-native",
      millSourcePath / "src-jvm-native"
    )
    override def releaseMode = ReleaseMode.ReleaseFast
    override def nativeLTO = LTO.Thin
    object test extends Tests with CrossTests {
      override def releaseMode = ReleaseMode.Debug
      override def nativeLTO = LTO.None
      override def sources = T.sources(
        millSourcePath / "src",
        millSourcePath / "src-native",
        millSourcePath / "src-jvm-native"
      )
    }
    object bench extends ScalaNativeModule{
      override def releaseMode = ReleaseMode.ReleaseFast
      override def nativeLTO = LTO.Thin
      override def moduleDeps = Seq(native)
      def scalaNativeVersion = "0.4.12"
      def scalaVersion = crossScalaVersion
      override def sources = T.sources(
        millSourcePath / "src",
        millSourcePath / "src-native",
        millSourcePath / "src-jvm-native"
      )
    }
  }
  object jvm extends SjsonnetCrossModule with SjsonnetJvmNative {
    override def mainClass = Some("sjsonnet.SjsonnetMain")
    override def sources = T.sources(
      millSourcePath / "src",
      millSourcePath / "src-jvm",
      millSourcePath / "src-jvm-native",
      millSourcePath / "src-jvm-js"
    )
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.tukaani:xz::1.9"
    )
    override def scalacOptions = Seq("-opt:l:inline", "-opt-inline-from:sjsonnet.**")
    //def compileIvyDeps = Agg( ivy"com.lihaoyi::acyclic:0.2.0")
    //def scalacOptions = Seq("-P:acyclic:force")
    //def scalacPluginIvyDeps = Agg( ivy"com.lihaoyi::acyclic:0.2.0")
    object test extends Tests with CrossTests{
      //def compileIvyDeps = Agg( ivy"com.lihaoyi::acyclic:0.2.0")
      //def scalacOptions = Seq("-P:acyclic:force")
      //def scalacPluginIvyDeps = Agg( ivy"com.lihaoyi::acyclic:0.2.0")
      def forkOptions = Seq("-Xss100m")
      override def sources = T.sources(
        millSourcePath / "src",
        millSourcePath / "src-jvm",
        millSourcePath / "src-jvm-native"
      )
    }
    object bench extends ScalaModule {
      override def moduleDeps = Seq(jvm)
      def scalaVersion = crossScalaVersion
      override def sources = T.sources(
        millSourcePath / "src",
        millSourcePath / "src-jvm",
        millSourcePath / "src-jvm-native"
      )
    }
  }

  object client extends JavaModule {
    override def ivyDeps = Agg(
      ivy"org.scala-sbt.ipcsocket:ipcsocket:1.6.2".exclude(
        "net.java.dev.jna" -> "jna",
        "net.java.dev.jna" -> "jna-platform"
      )
    )
    object test extends Tests{
      override def testFramework = "com.novocode.junit.JUnitFramework"
      override def ivyDeps = Agg(ivy"com.novocode:junit-interface:0.11")
    }
  }

  object server extends ScalaModule{
    def scalaVersion = crossScalaVersion
    override def moduleDeps = Seq(SjsonnetModule.this.jvm, client)
    override def ivyDeps = Agg(
      ivy"org.scala-sbt.ipcsocket:ipcsocket:1.6.2".exclude(
        "net.java.dev.jna" -> "jna",
        "net.java.dev.jna" -> "jna-platform"
      ),
      ivy"net.java.dev.jna:jna:5.13.0",
      ivy"net.java.dev.jna:jna-platform:5.13.0"
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
      override def testFramework = "com.novocode.junit.JUnitFramework"
      override def ivyDeps = Agg(ivy"com.novocode:junit-interface:0.11")
    }
  }

}
