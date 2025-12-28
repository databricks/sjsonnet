package sjsonnet

import org.graalvm.polyglot._
import utest._
import scala.collection.JavaConverters._

object PythonInteropTest extends TestSuite {
  
  lazy val context = Context.newBuilder("python")
    .allowAllAccess(true)
    .build()

  def tests = Tests {
    test("importpy_functionality") {
      val wd = OsPath(os.pwd)
      val interp = new Interpreter(
        queryExtVar = _ => None,
        queryTlaVar = _ => None,
        wd = wd,
        importer = Importer.empty,
        parseCache = new DefaultParseCache,
        settings = Settings.default,
        storePos = _ => (),
        logger = null,
        std = sjsonnet.stdlib.StdLibModule.Default.module,
        variableResolver = {
          case "importpy" => Some(new Val.Builtin1("importpy", "path") {
            // We need to manage the PythonEvaluator lifecycle. 
            // Ideally, the Interpreter or EvalScope should own it.
            // For this test, we create one here.
            val pyEval = new PythonEvaluator(new Importer {
                def resolve(docBase: Path, importName: String): Option[Path] = {
                   // Simple resolution relative to wd
                   Some(wd / importName)
                }
                def read(path: Path, binaryData: Boolean): Option[ResolvedFile] = {
                   if (os.exists(path.asInstanceOf[OsPath].p)) 
                     Some(StaticResolvedFile(os.read(path.asInstanceOf[OsPath].p)))
                   else None
                }
            }, new FileScope(wd))

            def evalRhs(arg1: Lazy, ev: EvalScope, pos: Position): Val = {
               val pathStr = arg1.force match {
                 case Val.Str(_, s) => s
                 case _ => Error.fail("path must be a string", pos)(ev)
               }
               // Resolve path relative to current file if possible, or wd
               val currentFile = pos.fileScope.currentFile
               val resolvedPath = currentFile match {
                 case p: OsPath => p.parent() / pathStr
                 case _ => wd / pathStr
               }
               pyEval.eval(resolvedPath, pos)
            }
          })
          case _ => None
        }
      )
      
      // Create a python file
      os.write(os.pwd / "utils.py", 
        """
        |def add(a, b):
        |  return a + b
        |
        |MY_CONST = 100
        """.stripMargin)
        
      val jsonnetSrc = 
        """
        |local utils = importpy("utils.py");
        |{
        |  sum: utils.add(10, 20),
        |  const: utils.MY_CONST
        |}
        """.stripMargin
        
      val result = interp.interpret(jsonnetSrc, OsPath(os.pwd / "main.jsonnet"))
      
      // cleanup
      os.remove(os.pwd / "utils.py")
      
      if (result.isLeft) {
         println("Interpretation failed: " + result.left.get)
      }
      val json = result.right.get
      assert(json("sum").num == 30)
      assert(json("const").num == 100)
    }
  }
}
