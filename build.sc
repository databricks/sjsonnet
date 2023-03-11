import mill._, scalalib._, publish._, scalajslib._, scalanativelib._, scalanativelib.api._
val sjsonnetVersion = "0.4.5"

object sjsonnet extends Cross[SjsonnetModule]("2.12.17", "2.13.10")
class SjsonnetModule(val crossScalaVersion: String) extends Module {
  def millSourcePath = super.millSourcePath / os.up
  trait SjsonnetJvmNative extends SjsonnetCrossModule {
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.lihaoyi::os-lib::0.7.2",
      ivy"com.lihaoyi::mainargs::0.2.0"
    )
  }
  trait SjsonnetCrossModule extends CrossScalaModule with PublishModule{

    def artifactName = "sjsonnet"
    def crossScalaVersion = SjsonnetModule.this.crossScalaVersion

    def ivyDeps = Agg(
      ivy"com.lihaoyi::fastparse::2.3.1",
      ivy"com.lihaoyi::pprint::0.6.1",
      ivy"com.lihaoyi::ujson::1.3.7",
      ivy"com.lihaoyi::scalatags::0.9.3",
      ivy"org.scala-lang.modules::scala-collection-compat::2.9.0"
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
      organization = "io.github.jam01",
      url = "https://github.com/jam01/sjsonnet",
      licenses = Seq(License.`Apache-2.0`),
      versionControl = VersionControl.github("jam01", "sjsonnet"),
      developers = Seq(
        Developer("jam01", "Jose Montoya","https://github.com/jam01")
      )
    )
    def sonatypeUri: String = "https://s01.oss.sonatype.org/service/local"
    def sonatypeSnapshotUri: String = "https://s01.oss.sonatype.org/content/repositories/snapshots"

    trait CrossTests extends ScalaModule with TestModule {
      def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.8.1")
      def testFramework: T[String] = T("utest.runner.Framework")
    }
  }
  object js extends SjsonnetCrossModule with ScalaJSModule{
    def scalaJSVersion = "1.12.0"
    def sources = T.sources(
      millSourcePath / "src",
      millSourcePath / "src-js",
      millSourcePath / "src-jvm-js"
    )
    object test extends Tests with CrossTests {
      def sources = T.sources(
        millSourcePath / "src",
        millSourcePath / "src-js",
        millSourcePath / "src-jvm-js"
      )
    }
  }
  object native extends SjsonnetCrossModule with SjsonnetJvmNative with ScalaNativeModule{
    def scalaNativeVersion = "0.4.10"
    def sources = T.sources(
      millSourcePath / "src",
      millSourcePath / "src-native",
      millSourcePath / "src-jvm-native"
    )
    def releaseMode = ReleaseMode.ReleaseFast
    def nativeLTO = LTO.Thin
    object test extends Tests with CrossTests {
      def releaseMode = ReleaseMode.Debug
      def nativeLTO = LTO.None
      def sources = T.sources(
        millSourcePath / "src",
        millSourcePath / "src-native",
        millSourcePath / "src-jvm-native"
      )
    }
    object bench extends ScalaNativeModule{
      def releaseMode = ReleaseMode.ReleaseFast
      def nativeLTO = LTO.Thin
      def moduleDeps = Seq(native)
      def scalaNativeVersion = "0.4.0"
      def scalaVersion = crossScalaVersion
      def sources = T.sources(
        millSourcePath / "src",
        millSourcePath / "src-native",
        millSourcePath / "src-jvm-native"
      )
    }
  }
  object jvm extends SjsonnetCrossModule with SjsonnetJvmNative {
    def mainClass = Some("sjsonnet.SjsonnetMain")
    def sources = T.sources(
      millSourcePath / "src",
      millSourcePath / "src-jvm",
      millSourcePath / "src-jvm-native",
      millSourcePath / "src-jvm-js"
    )
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.tukaani:xz::1.8"
    )
    def scalacOptions = Seq("-opt:l:inline", "-opt-inline-from:sjsonnet.**")
    //def compileIvyDeps = Agg( ivy"com.lihaoyi::acyclic:0.2.0")
    //def scalacOptions = Seq("-P:acyclic:force")
    //def scalacPluginIvyDeps = Agg( ivy"com.lihaoyi::acyclic:0.2.0")
    object test extends Tests with CrossTests{
      //def compileIvyDeps = Agg( ivy"com.lihaoyi::acyclic:0.2.0")
      //def scalacOptions = Seq("-P:acyclic:force")
      //def scalacPluginIvyDeps = Agg( ivy"com.lihaoyi::acyclic:0.2.0")
      def forkOptions = Seq("-Xss100m")
      def sources = T.sources(
        millSourcePath / "src",
        millSourcePath / "src-jvm",
        millSourcePath / "src-jvm-native"
      )
    }
    object bench extends ScalaModule {
      def moduleDeps = Seq(jvm)
      def scalaVersion = crossScalaVersion
      def sources = T.sources(
        millSourcePath / "src",
        millSourcePath / "src-jvm",
        millSourcePath / "src-jvm-native"
      )
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
      def testFramework: T[String] = T("com.novocode.junit.JUnitFramework")
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
      def testFramework: T[String] = T("com.novocode.junit.JUnitFramework")
      def ivyDeps = Agg(ivy"com.novocode:junit-interface:0.11")
    }
  }

}
