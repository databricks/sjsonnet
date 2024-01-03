import mill._, scalalib._, publish._, scalajslib._, scalanativelib._, scalanativelib.api._
val sjsonnetVersion = "0.4.7"

object sjsonnet extends Cross[SjsonnetModule]("2.12.13", "2.13.4")
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
      ivy"org.scala-lang.modules::scala-collection-compat::2.4.0"
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
      url = "https://github.com/lihaoyi/sjsonnet",
      licenses = Seq(License.MIT),
      versionControl = VersionControl.github("lihaoyi", "sjsonnet"),
      developers = Seq(
        Developer("lihaoyi", "Li Haoyi","https://github.com/lihaoyi")
      )
    )
    trait CrossTests extends ScalaModule with TestModule {
      def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.7.7")
      def testFrameworks = Seq("utest.runner.Framework")
    }
  }
  object js extends SjsonnetCrossModule with ScalaJSModule{
    def scalaJSVersion = "1.4.0"
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
    def scalaNativeVersion = "0.4.0"
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
      ivy"org.json:json:20211205",
      ivy"org.tukaani:xz::1.8",
      ivy"org.yaml:snakeyaml::1.30",
      ivy"com.github.blemale::scaffeine:4.1.0"
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

    override def resources = T.sources {
      val resourceDir = T.ctx().dest / "7z"
      val osArchToUrl = Map(
        "windows" -> "https://www.7-zip.org/a/7zr.exe",
        "linux-x86" -> "https://www.7-zip.org/a/7z2301-linux-x64.tar.xz",
        "linux-arm" -> "https://www.7-zip.org/a/7z2301-linux-arm64.tar.xz",
        "osx" -> "https://www.7-zip.org/a/7z2301-mac.tar.xz"
      )

      val downloadedFiles = for ((osArch, url) <- osArchToUrl) yield {
        val targetDir = resourceDir / (if (osArch.startsWith("linux")) "linux" else osArch)
        val fileName = if (osArch == "windows") "7z.exe" else "7z.tar.xz"
        val file = targetDir / fileName
        val targetFile =  if (osArch == "windows") {
          targetDir / "7z.exe"
        } else if (osArch.startsWith("linux")) {
          val executableName = if (osArch.contains("x86")) "7z-x86" else "7z-arm"
          targetDir / executableName
        } else {
          targetDir / "7z"
        }

        if (!os.exists(targetFile)) {
          os.makeDir.all(targetDir)
          T.log.info(s"Downloading $url to ${file}")
          os.write(file, requests.get.stream(url), createFolders = true)

          if (osArch != "windows") {
            // Extract a single file, 7zz, from the tarball
            os.proc("tar", "-xf", file, "-C", targetDir, "7zz").call()
            os.remove(file)

            if (osArch.startsWith("linux")) {
              os.move(targetDir / "7zz", targetFile)
            } else {
              os.move(targetDir / "7zz", targetFile)
            }
          }
        }
        targetFile
      }
      downloadedFiles.foreach(f => assert(os.exists(f)))
      Seq(PathRef(T.ctx().dest))
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
