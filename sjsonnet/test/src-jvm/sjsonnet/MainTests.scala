package sjsonnet

import utest.*

import java.io.{ByteArrayOutputStream, File, PrintStream}
import java.util

object MainTests extends TestSuite {
  val workspaceRoot: os.Path = sys.env.get("MILL_WORKSPACE_ROOT").map(os.Path(_)).getOrElse(os.pwd)
  val testSuiteRoot: os.Path = workspaceRoot / "sjsonnet" / "test" / "resources"
  // stdout mode uses println so it has an extra platform-specific line separator at the end
  val eol: String = java.lang.System.lineSeparator()

  val tests: Tests = Tests {
    // Compare writing to stdout with writing to a file
    test("writeToFile") {
      val source = testSuiteRoot / "test_suite" / "local.jsonnet"
      val outF = File.createTempFile("sjsonnet", ".json")
      val out = new ByteArrayOutputStream()
      val pout = new PrintStream(out)
      SjsonnetMain.main0(
        Array(source.toString),
        new DefaultParseCache,
        System.in,
        pout,
        System.err,
        workspaceRoot,
        None
      )
      pout.flush()
      SjsonnetMain.main0(
        Array("-o", outF.getAbsolutePath, source.toString),
        new DefaultParseCache,
        System.in,
        System.out,
        System.err,
        workspaceRoot,
        None
      )
      val stdoutBytes = out.toByteArray
      val fileBytes = os.read(os.Path(outF)).getBytes

      // println(stdoutBytes.map(_.toInt).mkString(","))
      // println(fileBytes.map(_.toInt).mkString(","))
      assert(util.Arrays.equals(fileBytes ++ eol.getBytes, stdoutBytes))
    }

    test("warnings") {
      val source = testSuiteRoot / "db" / "unused_illegal_var.jsonnet"
      val (res1, out1, err1) = runMain(source)
      val (res3, out3, err3) = runMain("--fatal-warnings", source)
      assert(res1 == 1, res3 == 1)
      assert(out1.isEmpty, out3.isEmpty)
      assert(err1.nonEmpty, err3.nonEmpty)
    }

    val streamedOut =
      """--- 1
        |--- 2
        |--- 3
        |--- 4
        |--- 5
        |""".stripMargin

    val expectedWorldDestStr =
      """[
        |   2,
        |   "three",
        |   true
        |]""".stripMargin

    test("yamlStream") {
      val source = testSuiteRoot / "db" / "stream.jsonnet"
      val (res, out, err) = runMain(source, "--yaml-stream")
      assert((res, out, err) == ((0, streamedOut + "\n", "")))
    }

    test("exec") {
      val source = "local x = [1]; local y = [2]; x + y"
      val (res, out, err) = runMain(source, "--exec")
      val expectedJson =
        """[
          |   1,
          |   2
          |]
          |""".stripMargin
      assert((res, out, err) == ((0, expectedJson, "")))
    }
    test("execYaml") {
      val source = "local x = [1]; local y = [2]; x + y"
      val (res, out, err) = runMain(source, "--exec", "--yaml-out")
      val expectedYaml =
        """- 1
          |- 2
          |
          |""".stripMargin
      assert((res, out, err) == ((0, expectedYaml, "")))
    }

    test("yamlStreamOutputFile") {
      val source = testSuiteRoot / "db" / "stream.jsonnet"
      val dest = os.temp()
      val (res, out, err) = runMain(source, "--yaml-stream", "--output-file", dest)
      assert((res, out, err) == ((0, "", "")))
      val destStr = os.read(dest)
      assert(destStr == streamedOut)
    }

    test("multi") {
      val source = testSuiteRoot / "db" / "multi.jsonnet"
      val multiDest = os.temp.dir()
      val (res, out, err) = runMain(source, "--multi", multiDest)
      val expectedOut = s"$multiDest/hello\n$multiDest/world\n"
      assert((res, out, err) == ((0, expectedOut, "")))

      val helloDestStr = os.read(multiDest / "hello")
      assert(helloDestStr == "1")

      val worldDestStr = os.read(multiDest / "world")
      assert(worldDestStr == expectedWorldDestStr)
    }

    test("multiOutputFile") {
      val source = testSuiteRoot / "db" / "multi.jsonnet"
      val dest = os.temp()
      val multiDest = os.temp.dir()
      val (res, out, err) = runMain(source, "--multi", multiDest, "--output-file", dest)
      val expectedOut = s"$multiDest/hello\n$multiDest/world"
      assert((res, out, err) == ((0, "", "")))

      val destStr = os.read(dest)
      assert(destStr == expectedOut)

      val helloDestStr = os.read(multiDest / "hello")
      assert(helloDestStr == "1")

      val worldDestStr = os.read(multiDest / "world")
      assert(worldDestStr == expectedWorldDestStr)
    }

    test("multiYamlOut") {
      val source = testSuiteRoot / "db" / "multi.jsonnet"
      val multiDest = os.temp.dir()
      val (res, out, err) = runMain(source, "--multi", multiDest, "--yaml-out")
      val expectedOut = s"$multiDest/hello\n$multiDest/world\n"
      assert((res, out, err) == ((0, expectedOut, "")))

      val helloDestStr = os.read(multiDest / "hello")
      assert(helloDestStr == "1")

      val worldDestStr = os.read(multiDest / "world")
      assert(
        worldDestStr ==
        """- 2
          |- three
          |- true""".stripMargin
      )
    }
  }

  def runMain(args: os.Shellable*): (Int, String, String) = {
    val err = new ByteArrayOutputStream()
    val perr = new PrintStream(err, true, "UTF-8")
    val out = new ByteArrayOutputStream()
    val pout = new PrintStream(out, true, "UTF-8")
    val res = SjsonnetMain.main0(
      args.toArray.flatMap(_.value),
      new DefaultParseCache,
      System.in,
      pout,
      perr,
      workspaceRoot,
      None
    )
    (res, new String(out.toByteArray, "UTF-8"), new String(err.toByteArray, "UTF-8"))
  }
}
