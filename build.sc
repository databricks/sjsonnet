import mill._, scalalib._, publish._, scalajslib._, scalanativelib._, scalanativelib.api._, scalajslib.api._
import $ivy.`com.lihaoyi::mill-contrib-jmh:`
import contrib.jmh.JmhModule
import java.util.Base64

val sjsonnetVersion = "0.4.15"

val scalaVersions = Seq("2.12.20", "2.13.16")

trait SjsonnetCrossModule extends CrossScalaModule with PublishModule {
  def crossValue: String
  def artifactName = "sjsonnet"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse::2.3.3",
    ivy"com.lihaoyi::pprint::0.6.6",
    ivy"com.lihaoyi::ujson::1.3.15",
    ivy"com.lihaoyi::scalatags::0.9.4",
    ivy"org.scala-lang.modules::scala-collection-compat::2.11.0"
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
    organization = "com.databricks",
    url = "https://github.com/databricks/sjsonnet",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("databricks", "sjsonnet"),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi","https://github.com/lihaoyi")
    )
  )
  trait CrossTests extends ScalaModule with TestModule.Utest {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.8.2")
  }
}

trait SjsonnetJvmNative extends ScalaModule {
  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.lihaoyi::os-lib::0.7.8",
    ivy"com.lihaoyi::mainargs::0.2.5"
  )
}

object sjsonnet extends Module {
  object js extends Cross[SjsonnetJsModule](scalaVersions)
  trait SjsonnetJsModule extends SjsonnetCrossModule with ScalaJSModule{
    def millSourcePath = super.millSourcePath / os.up
    def scalaJSVersion = "1.17.0"
    def esVersion = ESVersion.ES2018
    def sources = T.sources(
      this.millSourcePath / "src",
      this.millSourcePath / "src-js",
      this.millSourcePath / "src-jvm-js"
    )
    object test extends ScalaJSTests with CrossTests {
      def jsEnvConfig = JsEnvConfig.NodeJs(args=List("--stack-size=" + 100 * 1024))
      def sources = T.sources(
        this.millSourcePath / "src",
        this.millSourcePath / "src-js",
        this.millSourcePath / "src-jvm-js"
      )
      def generatedSources = T{
        val files = os.walk(this.millSourcePath / "resources").filterNot(os.isDir).map(p => p.relativeTo(this.millSourcePath / "resources") -> os.read.bytes(p)).toMap
        os.write(
          T.ctx().dest / "TestResources.scala",
          s"""package sjsonnet
             |
             |object TestResources{
             |  val files = Map(
             |""".stripMargin)
        for((k, v) <- files) {
          val name = k.toString.replaceAll("/", "_").replaceAll("\\.", "_").replaceAll("-", "_")
          val values = Base64.getEncoder().encodeToString(v).grouped(65535).toSeq
          os.write(
            T.ctx().dest / s"$name.scala",
            s"""package sjsonnet
               |
               |import java.util.Base64
               |
               |object $name {
               |  def contentArr = Seq(
               |    ${values.map("\"" + _ + "\"").mkString(",\n    ")}
               |  )
               |  def content = Base64.getDecoder().decode(contentArr.mkString)
               |}
               |""".stripMargin)
          os.write.append(
            T.ctx().dest / "TestResources.scala",
            s"""    "$k" -> $name.content,
               |""".stripMargin
          )
        }
        os.write.append(
          T.ctx().dest / "TestResources.scala",
          s"""  )
             |}
             |""".stripMargin
        )
        Seq(PathRef(T.ctx().dest / "TestResources.scala")) ++ files.keys.map(p => PathRef(T.ctx().dest / s"${p.toString.replaceAll("/", "_").replaceAll("\\.", "_").replaceAll("-", "_")}.scala"))
      }
    }
  }

  object native extends Cross[SjsonnetNativeModule](scalaVersions)
  trait SjsonnetNativeModule extends SjsonnetCrossModule with ScalaNativeModule with SjsonnetJvmNative {
    def millSourcePath = super.millSourcePath / os.up
    def scalaNativeVersion = "0.4.17"
    def sources = T.sources(
      this.millSourcePath / "src",
      this.millSourcePath / "src-native",
      this.millSourcePath / "src-jvm-native"
    )
    def releaseMode = ReleaseMode.ReleaseFast
    def nativeLTO = LTO.Thin

    object test extends ScalaNativeTests with CrossTests {
      def releaseMode = ReleaseMode.Debug
      def scalaNativeVersion = SjsonnetNativeModule.this.scalaNativeVersion
      def nativeLTO = LTO.None
      def sources = T.sources(
        this.millSourcePath / "src",
        this.millSourcePath / "src-native",
        this.millSourcePath / "src-jvm-native"
      )
    }
  }

  object jvm extends Cross[SjsonnnetJvmModule](scalaVersions)
  trait SjsonnnetJvmModule extends SjsonnetCrossModule with ScalaModule with SjsonnetJvmNative {
    def millSourcePath = super.millSourcePath / os.up
    def mainClass = Some("sjsonnet.SjsonnetMain")
    def sources = T.sources(
      this.millSourcePath / "src",
      this.millSourcePath / "src-jvm",
      this.millSourcePath / "src-jvm-native",
    )
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.json:json:20250107",
      ivy"org.tukaani:xz::1.10",
      ivy"org.lz4:lz4-java::1.8.0",
      ivy"org.yaml:snakeyaml::1.33",
      ivy"com.google.re2j:re2j:1.8",
    )
    def scalacOptions = Seq("-opt:l:inline", "-opt-inline-from:sjsonnet.*,sjsonnet.**")

    object test extends ScalaTests with CrossTests {
      def forkArgs = Seq("-Xss100m")
      def sources = T.sources(
        this.millSourcePath / "src",
        this.millSourcePath / "src-jvm",
        this.millSourcePath / "src-jvm-native"
      )
    }

    object client extends JavaModule {
      def ivyDeps = Agg(
        ivy"org.scala-sbt.ipcsocket:ipcsocket:1.0.0".exclude(
          "net.java.dev.jna" -> "jna",
          "net.java.dev.jna" -> "jna-platform"
        )
      )
      object test extends JavaModuleTests with TestModule.Junit4
    }

    object server extends ScalaModule{
      def scalaVersion = SjsonnnetJvmModule.this.crossValue
      def moduleDeps = Seq(SjsonnnetJvmModule.this, client)
      def ivyDeps = Agg(
        ivy"org.scala-sbt.ipcsocket:ipcsocket:1.0.0".exclude(
          "net.java.dev.jna" -> "jna",
          "net.java.dev.jna" -> "jna-platform"
        ),
        ivy"net.java.dev.jna:jna:4.5.0",
        ivy"net.java.dev.jna:jna-platform:4.5.0"
      )

      override def prependShellScript = mill.util.Jvm.universalScript(
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
      object test extends ScalaTests with TestModule.Junit4
    }

    object bench extends ScalaModule with JmhModule with SjsonnetJvmNative {
      def scalaVersion = SjsonnnetJvmModule.this.crossValue
      def moduleDeps = Seq(SjsonnnetJvmModule.this)
      def jmhCoreVersion = "1.37"
      def sources = T.sources(
        this.millSourcePath / os.up / os.up / "bench" / "src",
        this.millSourcePath / os.up / "test" / "src" / "sjsonnet" / "OldRenderer.scala",
        this.millSourcePath / os.up / "test" / "src" / "sjsonnet" / "OldYamlRenderer.scala",
      )
      def scalacOptions = SjsonnnetJvmModule.this.scalacOptions
    }
  }
}