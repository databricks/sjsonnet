val sjsonnetVersion = "0.4.4"

val scala213 = "2.13.15"
val scala3 = "3.5.1"

val commonOptions: Seq[String] = Seq(
  "-opt:l:inline",
  "-opt-inline-from:sjsonnet.*,sjsonnet.**",
)

cancelable in Global := true
publish / skip := true

lazy val main = (project in file("sjsonnet"))
  .settings(
    name := "sjsonnet",

    // Enable cross-compilation
    scalaVersion := scala3,
    crossScalaVersions := Seq(scala213, scala3),
    scalacOptions ++= {
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          commonOptions ++ Seq(
            // options dedicated for cross build / migration to Scala 3
            "-source:3.5-migration"
          )
        case _ =>
          commonOptions ++ Seq(
            "-Xsource:3"
          )
      })
    },


    Test / fork := true,
    Test / baseDirectory := (ThisBuild / baseDirectory).value,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "3.1.1",
      "com.lihaoyi" %% "pprint" % "0.9.0",
      "com.lihaoyi" %% "ujson" % "4.0.0",
      "com.lihaoyi" %% "scalatags" % "0.12.0",
      "com.lihaoyi" %% "os-lib" % "0.10.3",
      "com.lihaoyi" %% "mainargs" % "0.7.5",
      "org.lz4" % "lz4-java" % "1.8.0",
      "org.json" % "json" % "20240303",
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.12.0",
      "org.tukaani" % "xz" % "1.9",
      "org.yaml" % "snakeyaml" % "2.0",
    ),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "utest" % "0.8.3",
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
    // Do not cross-compile the benchmark
    scalaVersion := scala3,
  )

lazy val root = (project in file("."))
  .aggregate(main)
  .settings(
    publishArtifact := false
  )