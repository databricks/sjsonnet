val sjsonnetVersion = IO.readLines(new File("sjsonnet/version")).head.trim
cancelable in Global := true

val options = Seq("-Wconf:origin=scala.collection.compat.*:s", "-Xlint:all")

lazy val commonSettings = Seq(
  scalaVersion := "3.3.7",
  scalacOptions ++= options
)

lazy val main = (project in file("sjsonnet"))
  .settings(commonSettings: _*)
  .settings(
    Test / fork := true,
    Test / javaOptions += "-Xss100m",
    Test / baseDirectory := (ThisBuild / baseDirectory).value,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "3.1.1",
      "com.lihaoyi" %% "pprint" % "0.9.3",
      "com.lihaoyi" %% "ujson" % "4.4.1",
      "com.lihaoyi" %% "scalatags" % "0.13.1",
      "com.lihaoyi" %% "os-lib" % "0.11.4",
      "com.lihaoyi" %% "mainargs" % "0.7.6",
      "org.lz4" % "lz4-java" % "1.8.0",
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.13.0",
      "org.tukaani" % "xz" % "1.10",
      "org.yaml" % "snakeyaml" % "2.5",
      "com.google.re2j" % "re2j" % "1.8"
    ),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "utest" % "0.9.1"
    ).map(_ % "test"),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    (Compile / unmanagedSourceDirectories) := Seq(
      baseDirectory.value / "src",
      baseDirectory.value / "src-jvm",
      baseDirectory.value / "src-jvm-native"
    ),
    (Test / unmanagedSourceDirectories) := Seq(
      baseDirectory.value / "test/src",
      baseDirectory.value / "test/src-jvm",
      baseDirectory.value / "test/src-jvm-native"
    ),
    (Test / unmanagedResourceDirectories) := Seq(
      baseDirectory.value / "test/resources"
    ),
    (Compile / sourceGenerators) += Def.task {
      val file = (Compile / sourceManaged).value / "jsonnet" / "Version.scala"
      IO.write(
        file,
        s"""package sjsonnet
           |object Version{
           |  val version = "${sjsonnetVersion}"
           |}
           |""".stripMargin
      )
      Seq(file)
    }.taskValue
  )

lazy val bench = (project in file("bench"))
  .dependsOn(main % "compile->test")
  .enablePlugins(JmhPlugin)
  .settings(commonSettings: _*)
  .settings(
    run / fork := true,
    run / javaOptions += "-Xss100m"
  )
