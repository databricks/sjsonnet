val sjsonnetVersion = "0.4.4"

scalaVersion in Global := "2.13.10"

cancelable in Global := true

lazy val main = (project in file("sjsonnet"))
  .settings(
    scalacOptions in Compile ++= Seq("-opt:l:inline", "-opt-inline-from:sjsonnet.*,sjsonnet.**"),
    fork in Test := true,
    baseDirectory in Test := (baseDirectory in ThisBuild).value,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "3.0.1",
      "com.lihaoyi" %% "pprint" % "0.8.1",
      "com.lihaoyi" %% "ujson" % "3.1.0",
      "com.lihaoyi" %% "scalatags" % "0.12.0",
      "com.lihaoyi" %% "os-lib" % "0.9.1",
      "com.lihaoyi" %% "mainargs" % "0.4.0",
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.9.0",
      "org.tukaani" % "xz" % "1.8",
    ),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "utest" % "0.8.1",
    ).map(_ % "test"),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    (unmanagedSourceDirectories in Compile) := Seq(
      baseDirectory.value / "src",
      baseDirectory.value / "src-jvm",
      baseDirectory.value / "src-jvm-native",
    ),
    (unmanagedSourceDirectories in Test) := Seq(
      baseDirectory.value / "test/src",
      baseDirectory.value / "test/src-jvm",
      baseDirectory.value / "test/src-jvm-native",
    ),
    (unmanagedResourceDirectories in Test) := Seq(
      baseDirectory.value / "test/resources",
    ),
    (sourceGenerators in Compile) += Def.task {
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
    fork in run := true,
  )
