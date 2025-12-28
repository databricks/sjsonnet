package sjsonnet

import org.graalvm.polyglot._
import utest._
import scala.collection.JavaConverters._

object PythonInteropTest extends TestSuite {
  
  lazy val context = Context.newBuilder("python")
    .allowAllAccess(true)
    .build()

  def tests = Tests {
    test("importpy_shared_context") {
      val wd = OsPath(os.pwd)
      val importer = new Importer {
        def resolve(docBase: Path, importName: String): Option[Path] = Some(docBase / importName)
        def read(path: Path, binaryData: Boolean): Option[ResolvedFile] = {
          val p = path.asInstanceOf[OsPath].p
          if (os.exists(p)) Some(StaticResolvedFile(os.read(p))) else None
        }
      }
      val pyManager = Platform.makePythonContextManager()
      try {
        val interp = new Interpreter(
          queryExtVar = _ => None,
          queryTlaVar = _ => None,
          wd = wd,
          importer = importer,
          parseCache = new DefaultParseCache,
          settings = Settings.default,
          storePos = _ => (),
          logger = null,
          std = sjsonnet.stdlib.StdLibModule.Default.module,
          variableResolver = {
            case "importpy" if pyManager.isDefined =>
              Some(Platform.makePythonImportFunc(pyManager.get, importer))
            case _ => None
          }
        )
        
        os.write(os.pwd / "state.py", 
          """
          |counter = 0
          |def inc():
          |  global counter
          |  counter += 1
          |  return counter
          """.stripMargin)
          
        val jsonnetSrc = 
          """
          |local s1 = importpy("state.py");
          |local s2 = importpy("state.py");
          |{
          |  v1: s1.inc(),
          |  v2: s2.inc(),
          |  v3: s1.inc()
          |}
          """.stripMargin
          
        val result = interp.interpret(jsonnetSrc, OsPath(os.pwd / "main.jsonnet"))
        
        os.remove(os.pwd / "state.py")
        
        if (result.isLeft) {
           println("Interpretation failed: " + result.left.get)
        }
        val json = result.right.get
        assert(json("v1").num == 1)
        assert(json("v2").num == 2)
        assert(json("v3").num == 3)
      } finally {
        pyManager.foreach(Platform.closePythonContextManager)
      }
    }
  }
}
