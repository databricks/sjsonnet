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
      SjsonnetMainBase.main0(
        Array(source.toString),
        new DefaultParseCache,
        System.in,
        pout,
        System.err,
        workspaceRoot,
        None
      )
      pout.flush()
      SjsonnetMainBase.main0(
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
      assert(helloDestStr == "1\n")

      val worldDestStr = os.read(multiDest / "world")
      assert(worldDestStr == expectedWorldDestStr + "\n")
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
      assert(helloDestStr == "1\n")

      val worldDestStr = os.read(multiDest / "world")
      assert(worldDestStr == expectedWorldDestStr + "\n")
    }

    test("multiYamlOut") {
      val source = testSuiteRoot / "db" / "multi.jsonnet"
      val multiDest = os.temp.dir()
      val (res, out, err) = runMain(source, "--multi", multiDest, "--yaml-out")
      val expectedOut = s"$multiDest/hello\n$multiDest/world\n"
      assert((res, out, err) == ((0, expectedOut, "")))

      val helloDestStr = os.read(multiDest / "hello")
      assert(helloDestStr == "1\n")

      val worldDestStr = os.read(multiDest / "world")
      assert(
        worldDestStr ==
        """- 2
          |- three
          |- true
          |""".stripMargin
      )
    }

    // -- Default trailing newline behavior (with newline) --

    test("execString") {
      val source = """"hello""""
      val (res, out, err) = runMain(source, "--exec", "--string")
      assert((res, out, err) == ((0, "hello\n", "")))
    }

    test("multiStringOutput") {
      val source = testSuiteRoot / "db" / "multi_string.jsonnet"
      val multiDest = os.temp.dir()
      val (res, out, err) = runMain(source, "--multi", multiDest, "--string")
      assert(res == 0)
      assert(err.isEmpty)

      val helloDestStr = os.read(multiDest / "hello.txt")
      assert(helloDestStr == "hello world\n")

      val barDestStr = os.read(multiDest / "bar.txt")
      assert(barDestStr == "bar\n")
    }

    // -- No trailing newline behavior --

    test("noTrailingNewline") {
      // Simple scalar output — default has trailing newline
      val (resDefault, outDefault, _) = runMain("42", "--exec")
      assert((resDefault, outDefault) == ((0, "42\n")))

      // Simple scalar output — no trailing newline
      val (res1, out1, err1) = runMain("42", "--exec", "--no-trailing-newline")
      assert((res1, out1, err1) == ((0, "42", "")))

      // Object output — default has trailing newline
      val (resObj, outObj, _) = runMain("""{"a": 1, "b": 2}""", "--exec")
      val expectedJsonWithNewline =
        """{
          |   "a": 1,
          |   "b": 2
          |}
          |""".stripMargin
      assert((resObj, outObj) == ((0, expectedJsonWithNewline)))

      // Object output — no trailing newline
      val (res2, out2, err2) =
        runMain("""{"a": 1, "b": 2}""", "--exec", "--no-trailing-newline")
      val expectedJson =
        """{
          |   "a": 1,
          |   "b": 2
          |}""".stripMargin
      assert((res2, out2, err2) == ((0, expectedJson, "")))

      // String output — default has trailing newline
      val (resStr, outStr, _) = runMain(""""hello"""", "--exec", "--string")
      assert((resStr, outStr) == ((0, "hello\n")))

      // String output — no trailing newline
      val (res3, out3, err3) =
        runMain(""""hello"""", "--exec", "--string", "--no-trailing-newline")
      assert((res3, out3, err3) == ((0, "hello", "")))
    }

    test("noTrailingNewlineMulti") {
      // Default multi — files have trailing newline
      val source = testSuiteRoot / "db" / "multi.jsonnet"
      val multiDestDefault = os.temp.dir()
      val (resDefault, _, _) = runMain(source, "--multi", multiDestDefault)
      assert(resDefault == 0)
      assert(os.read(multiDestDefault / "hello") == "1\n")
      assert(os.read(multiDestDefault / "world") == expectedWorldDestStr + "\n")

      // No trailing newline multi — files have no trailing newline,
      // but the file list on stdout still ends with \n (matching go-jsonnet behavior)
      val multiDest = os.temp.dir()
      val (res, out, err) =
        runMain(source, "--multi", multiDest, "--no-trailing-newline")
      val expectedOut = s"$multiDest/hello\n$multiDest/world\n"
      assert((res, out, err) == ((0, expectedOut, "")))
      assert(os.read(multiDest / "hello") == "1")
      assert(os.read(multiDest / "world") == expectedWorldDestStr)
    }

    test("noTrailingNewlineMultiString") {
      val source = testSuiteRoot / "db" / "multi_string.jsonnet"

      // Default multi+string — files have trailing newline
      val multiDestDefault = os.temp.dir()
      val (resDefault, _, _) = runMain(source, "--multi", multiDestDefault, "--string")
      assert(resDefault == 0)
      assert(os.read(multiDestDefault / "hello.txt") == "hello world\n")
      assert(os.read(multiDestDefault / "bar.txt") == "bar\n")

      // No trailing newline multi+string — files have no trailing newline,
      // but the file list on stdout still ends with \n (matching go-jsonnet behavior)
      val multiDest = os.temp.dir()
      val (res, out, err) =
        runMain(source, "--multi", multiDest, "--string", "--no-trailing-newline")
      val expectedOut = s"$multiDest/bar.txt\n$multiDest/hello.txt\n"
      assert((res, out, err) == ((0, expectedOut, "")))
      assert(os.read(multiDest / "hello.txt") == "hello world")
      assert(os.read(multiDest / "bar.txt") == "bar")
    }

    test("noTrailingNewlineOutputFile") {
      // Default output-file — file has content without trailing newline (file mode)
      val source = "42"
      val destDefault = os.temp()
      val (resDefault, _, _) = runMain(source, "--exec", "--output-file", destDefault)
      assert(resDefault == 0)
      val defaultContent = os.read(destDefault)
      assert(defaultContent == "42")

      // No trailing newline output-file — same behavior
      val dest = os.temp()
      val (res, out, err) =
        runMain(source, "--exec", "--no-trailing-newline", "--output-file", dest)
      assert((res, out, err) == ((0, "", "")))
      assert(os.read(dest) == "42")
    }

    test("noTrailingNewlineYamlStreamError") {
      val source = testSuiteRoot / "db" / "stream.jsonnet"
      val (res, out, err) =
        runMain(source, "--yaml-stream", "--no-trailing-newline")
      assert(res == 1)
      assert(out.isEmpty)
      assert(err.contains("cannot use --no-trailing-newline with --yaml-stream"))
    }

    test("noTrailingNewlineYamlOut") {
      // Default yaml-out — has trailing newline
      val source = "local x = [1]; local y = [2]; x + y"
      val (resDefault, outDefault, _) = runMain(source, "--exec", "--yaml-out")
      val expectedYamlWithNewline =
        """- 1
          |- 2
          |
          |""".stripMargin
      assert((resDefault, outDefault) == ((0, expectedYamlWithNewline)))

      // No trailing newline yaml-out — no trailing newline
      val (res, out, err) =
        runMain(source, "--exec", "--yaml-out", "--no-trailing-newline")
      val expectedYaml =
        """- 1
          |- 2""".stripMargin
      assert((res, out, err) == ((0, expectedYaml, "")))
    }
  }

  def runMain(args: os.Shellable*): (Int, String, String) = {
    val err = new ByteArrayOutputStream()
    val perr = new PrintStream(err, true, "UTF-8")
    val out = new ByteArrayOutputStream()
    val pout = new PrintStream(out, true, "UTF-8")
    val res = SjsonnetMainBase.main0(
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
