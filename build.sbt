val sjsonnetVersion = "0.4.15.2"

scalaVersion in Global := "2.13.16"

cancelable in Global := true

lazy val main = (project in file("sjsonnet"))
  .settings(
    Compile / scalacOptions ++= Seq("-opt:l:inline", "-opt-inline-from:sjsonnet.*,sjsonnet.**"),
    Test / fork := true,
    Test / javaOptions += "-Xss100m",
    Test / baseDirectory := (ThisBuild / baseDirectory).value,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "2.3.3",
      "com.lihaoyi" %% "pprint" % "0.6.6",
      "com.lihaoyi" %% "ujson" % "1.3.15",
      "com.lihaoyi" %% "scalatags" % "0.9.4",
      "com.lihaoyi" %% "os-lib" % "0.7.8",
      "com.lihaoyi" %% "mainargs" % "0.2.5",
      "org.lz4" % "lz4-java" % "1.8.0",
      "org.json" % "json" % "20250107",
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.11.0",
      "org.tukaani" % "xz" % "1.10",
      "org.yaml" % "snakeyaml" % "1.33",
      "com.google.re2j" % "re2j" % "1.8",
    ),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "utest" % "0.8.2",
    ).map(_ % "test"),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    (Compile / unmanagedSourceDirectories) := Seq(
      baseDirectory.value / "src",
      baseDirectory.value / "src-jvm",
      baseDirectory.value / "src-jvm-native",
    ),
    (Test / unmanagedSourceDirectories) := Seq(
      baseDirectory.value / "test/src",
      baseDirectory.value / "test/src-jvm",
      baseDirectory.value / "test/src-jvm-native",
    ),
    (Test / unmanagedResourceDirectories) := Seq(
      baseDirectory.value / "test/resources",
    ),
    (Compile / sourceGenerators) += Def.task {
      val file = (Compile / sourceManaged).value / "jsonnet" / "Version.scala"
      IO.write(file,
        s"""package sjsonnet
           |object Version{
           |  val version = "${sjsonnetVersion}"
           |}
           |""".stripMargin)
      Seq(file)
    }.taskValue
  )

lazy val bench = (project in file("bench"))
  .dependsOn(main % "compile->test")
  .enablePlugins(JmhPlugin)
  .settings(
    run / fork := true,
  )
