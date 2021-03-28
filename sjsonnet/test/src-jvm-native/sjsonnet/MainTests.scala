package sjsonnet

import java.io.{ByteArrayOutputStream, File, PrintStream}
import java.util.Arrays

import utest._

object MainTests extends TestSuite {

  val testSuiteRoot = os.pwd / "sjsonnet" / "test" / "resources" / "test_suite"

  val tests = Tests {
    // Compare writing to stdout with writing to a file
    test("writeToFile") {
      val source = (testSuiteRoot / "local.jsonnet").toString()
      val outF = File.createTempFile("sjsonnet", ".json")
      val out = new ByteArrayOutputStream()
      val pout = new PrintStream(out)
      SjsonnetMain.main0(Array(source), collection.mutable.HashMap.empty, System.in, pout, System.err, os.pwd, None)
      pout.flush()
      SjsonnetMain.main0(Array("-o", outF.getAbsolutePath, source), collection.mutable.HashMap.empty, System.in, System.out, System.err, os.pwd, None)
      val stdoutBytes = out.toByteArray
      val fileBytes = os.read(os.Path(outF)).getBytes
      // stdout mode uses println so it has an extra platform-specific line separator at the end
      val eol = System.getProperty("line.separator").getBytes

      //println(stdoutBytes.map(_.toInt).mkString(","))
      //println(fileBytes.map(_.toInt).mkString(","))
      assert(Arrays.equals(fileBytes ++ eol, stdoutBytes))
    }
  }
}
