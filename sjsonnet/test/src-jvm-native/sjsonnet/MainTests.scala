package sjsonnet

import java.io.{ByteArrayOutputStream, File, PrintStream}
import java.util.Arrays

import utest._

object MainTests extends TestSuite {

  val testSuiteRoot = os.pwd / "sjsonnet" / "test" / "resources"
  // stdout mode uses println so it has an extra platform-specific line separator at the end
  val eol = System.getProperty("line.separator")

  val tests = Tests {
    // Compare writing to stdout with writing to a file
    test("writeToFile") {
      val source = (testSuiteRoot / "test_suite" / "local.jsonnet").toString()
      val outF = File.createTempFile("sjsonnet", ".json")
      val out = new ByteArrayOutputStream()
      val pout = new PrintStream(out)
      SjsonnetMain.main0(Array(source), new DefaultParseCache, System.in, pout, System.err, os.pwd, None)
      pout.flush()
      SjsonnetMain.main0(Array("-o", outF.getAbsolutePath, source), new DefaultParseCache, System.in, System.out, System.err, os.pwd, None)
      val stdoutBytes = out.toByteArray
      val fileBytes = os.read(os.Path(outF)).getBytes

      //println(stdoutBytes.map(_.toInt).mkString(","))
      //println(fileBytes.map(_.toInt).mkString(","))
      assert(Arrays.equals(fileBytes ++ eol.getBytes, stdoutBytes))
    }

    test("warnings") {
      val source = (testSuiteRoot / "db" / "unused_illegal_var.jsonnet").toString()
      val (res1, out1, err1) = runMain(Seq(source))
      val (res2, out2, err2) = runMain(Seq("--no-static-errors", source))
      val (res3, out3, err3) = runMain(Seq("--no-static-errors", "--fatal-warnings", source))
      assert(res1 == 1, res2 == 0, res3 == 1)
      assert(out1.isEmpty, out2 == "true" + eol, out3.isEmpty)
      assert(!err1.isEmpty, !err2.isEmpty, !err3.isEmpty)
    }
  }

  def runMain(args: Seq[String]): (Int, String, String) = {
    val err = new ByteArrayOutputStream()
    val perr = new PrintStream(err, true, "UTF-8")
    val out = new ByteArrayOutputStream()
    val pout = new PrintStream(out, true, "UTF-8")
    val res = SjsonnetMain.main0(args.toArray, collection.mutable.HashMap.empty, System.in, pout, perr, os.pwd, None)
    (res, new String(out.toByteArray, "UTF-8"), new String(err.toByteArray, "UTF-8"))
  }
}
