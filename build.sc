
import mill._, scalalib._, publish._, scalajslib._, scalanativelib._, mill.scalanativelib.api._
val sjsonnetVersion = "0.2.7"

val crossScalaVersions = Seq("2.13.4", "2.12.13")

object sjsonnet extends Module{
  
  object native extends Cross[SjsonnetNativeModule](crossScalaVersions:_*)
  class SjsonnetNativeModule(val crossScalaVersion: String) extends SjsonnetCrossModule with CommonJvmNative with ScalaNativeModule{
    def releaseMode = ReleaseMode.ReleaseFast
    def nativeLinkStubs = true
    def libcrypto = T(os.walk(os.root / "usr").filter(_.last == "libcrypto.a").head.toString)
    def staticLinkLibcrypto = T { true }
    def libcryptoLinkingOptions = T {
      if(staticLinkLibcrypto()) {
        Array(libcrypto())
      } else {
        val libcryptoDir = os.Path(libcrypto()) / os.up
        Array(s"-L$libcryptoDir", "-lcrypto")
      }
    }
    def forkArgs = Seq("-Xmx4g")
    def nativeLinkingOptions = T { libcryptoLinkingOptions() ++ super.nativeLinkingOptions() }
    def scalaNativeVersion = "0.4.0"
    def platformSegment = "native"
    def sources = T.sources(
      millSourcePath / "src",
      millSourcePath / "src-native",
      millSourcePath / "src-jvm-native"
    )
    object test extends Tests with CrossTests{
      def releaseMode = ReleaseMode.ReleaseFast
      def nativeLinkingOptions = SjsonnetNativeModule.this.nativeLinkingOptions()
      def nativeLinkStubs = true
      def forkArgs = Seq("-Xmx4g")

      def sources = T.sources(
        millSourcePath / "src",
        millSourcePath / "src-native",
        millSourcePath / "src-jvm-native"
      )
    }
  }
  object js extends Cross[SjsonnetJsModule](crossScalaVersions:_*)
  class SjsonnetJsModule(val crossScalaVersion: String) extends SjsonnetCrossModule with ScalaJSModule{
    def scalaJSVersion = "1.4.0"
    def platformSegment = "js"
    def sources = T.sources(
      millSourcePath / "src",
      millSourcePath / "src-js",
      millSourcePath / "src-jvm-js",
    )
    object test extends Tests with CrossTests {
      def sources = T.sources(
        millSourcePath / "src",
        millSourcePath / "src-js",
        millSourcePath / "src-jvm-js"
      )
    }
  }
  object jvm extends Cross[SjsonnetJvmModule](crossScalaVersions:_*)
  class SjsonnetJvmModule(val crossScalaVersion: String) extends SjsonnetCrossModule with CommonJvmNative{
    def platformSegment = "jvm"
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.tukaani:xz::1.8"
    )
    def sources = T.sources(
      millSourcePath / "src",
      millSourcePath / "src-jvm",
      millSourcePath / "src-jvm-native"
    )
    def nativeImage = T {
      val classpath = runClasspath().map(_.path).mkString(":")
      val outPath = T.ctx.dest / 'out
        os.proc(
          "native-image",
          "--no-fallback",
          "--allow-incomplete-classpath",
          "--report-unsupported-elements-at-runtime",
          "-cp",
          classpath,
          s"-H:Class=${mainClass().get}",
          s"-H:Name=$outPath"
        ).call()
      PathRef(outPath)
    }
    def compileIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.2.0")
    def scalacOptions = Seq("-P:acyclic:force")
    def scalacPluginIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.2.0")
    object test extends Tests with CrossTests{
      def sources = T.sources(
        millSourcePath / "src",
        millSourcePath / "src-jvm",
        millSourcePath / "src-jvm-native",
        millSourcePath / "src-jvm-js"
      )
      def compileIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.2.0")
      def scalacOptions = Seq("-P:acyclic:force")
      def scalacPluginIvyDeps = Agg( ivy"com.lihaoyi::acyclic:0.2.0")
      def forkArgs = Seq("-Xss100m")
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
    

    object server extends ScalaModule{
      def scalaVersion = crossScalaVersion
      def moduleDeps = Seq(SjsonnetJvmModule.this, client)
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
}

trait SjsonnetCrossModule extends CrossScalaModule with PublishModule{
  def artifactName = "sjsonnet"
  def platformSegment: String
  def crossScalaVersion = SjsonnetCrossModule.this.crossScalaVersion
  def millSourcePath = super.millSourcePath / os.up

  def ivyDeps = T { super.ivyDeps() ++
    Agg(
      ivy"com.lihaoyi::fastparse::2.3.1",
      ivy"com.lihaoyi::pprint::0.5.10",
      ivy"com.lihaoyi::ujson::1.2.3",
      ivy"com.lihaoyi::scalatags::0.9.3",
      ivy"org.scala-lang.modules::scala-collection-compat::2.4.0"
    )
  }
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
  trait CrossTests extends ScalaModule with TestModule{
    def platformSegment = SjsonnetCrossModule.this.platformSegment
    def ivyDeps = super.ivyDeps() ++ Agg(ivy"com.lihaoyi::utest::0.7.4")
    def testFrameworks = Seq("utest.runner.Framework")
    def sources = T.sources(
      millSourcePath / "src",
      millSourcePath / s"src-$platformSegment"
    )
  }
}

trait CommonJvmNative extends ScalaModule{
  def mainClass = Some("sjsonnet.SjsonnetMain")
  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.lihaoyi::os-lib::0.7.2",
    ivy"com.github.scopt::scopt::4.0.0"
  )
}
