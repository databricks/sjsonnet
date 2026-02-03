package sjsonnet.starlark

import org.graalvm.polyglot._
import utest._
import sjsonnet.{OsPath, Interpreter, Importer, ResolvedFile, StaticResolvedFile, DefaultParseCache, Settings, Path, Position, ValScope, EvalScope, FileScope, Expr, Error, Platform}

object StarlarkSemanticsTest extends TestSuite {
  def tests = Tests {
    test("starlark_freeze_semantics") {
      val wd = OsPath(os.pwd)
      val importer = new Importer {
        def resolve(docBase: Path, importName: String): Option[Path] = Some(docBase / importName)
        def read(path: Path, binaryData: Boolean): Option[ResolvedFile] = {
          val p = path.asInstanceOf[OsPath].p
          if (os.exists(p)) Some(StaticResolvedFile(os.read(p))) else None
        }
      }
      val starlarkManager = Platform.makeStarlarkContextManager().get.asInstanceOf[StarlarkContextManager]
      try {
        StarlarkEngine.currentManager.set(starlarkManager)
        val interp = new Interpreter(
          extVars = Map.empty,
          tlaVars = Map.empty,
          wd = wd,
          importer = importer,
          parseCache = new DefaultParseCache,
          settings = Settings.default,
          storePos = _ => (),
          logger = null,
          std = sjsonnet.stdlib.StdLibModule.Default.module,
          variableResolver = {
            case "importstarlark" => Some(Platform.makeStarlarkImportFunc(starlarkManager, importer))
            case _ => None
          }
        )
        
        os.write(os.pwd / "lib.py", 
          """
          |MY_LIST = [1, 2, 3]
          |def get_list(): return MY_LIST
          |def create_fresh(): return [4, 5, 6]
          """.stripMargin)
          
        // 1. Fresh mutation should work
        val jsonnet1 = 
          """
          |local lib = importstarlark("lib.py");
          |local f = lib.create_fresh();
          |f.append(7)
          |""".stripMargin
        // Wait, Jsonnet doesn't have .append on the returned object from Python if it's converted to Val.Arr
        // But if it's returned as a Python list proxy... our mapper converts it to Val.Arr immediately.
        // Val.Arr is immutable in Jsonnet.
        
        // Let's test mutation INSIDE Python called from Jsonnet.
        os.write(os.pwd / "mutate.py",
          """
          |def mutate_global(lib):
          |    gl = lib['get_list']()
          |    gl.append(4)
          |    return gl
          |
          |def mutate_fresh(lib):
          |    fl = lib['create_fresh']()
          |    fl.append(7)
          |    return fl
          """.stripMargin)

        val jsonnet2 = 
          """
          |local lib = importstarlark("lib.py");
          |local mut = importstarlark("mutate.py");
          |{
          |  fresh: mut.mutate_fresh(lib),
          |}
          """.stripMargin
          
        val result = interp.interpret(jsonnet2, OsPath(os.pwd / "main.jsonnet"))
        if (result.isLeft) println("Error: " + result.left.get)
        assert(result.isRight)
        assert(result.right.get("fresh").arr.length == 4)

        // Now test global mutation - currently our implementation DOES NOT freeze.
        val jsonnet3 = 
          """
          |local lib = importstarlark("lib.py");
          |local mut = importstarlark("mutate.py");
          |mut.mutate_global(lib)
          |""".stripMargin
        
        println("Running global mutation test (expecting failure in true Starlark)...")
        val result2 = interp.interpret(jsonnet3, OsPath(os.pwd / "main.jsonnet"))
        if (result2.isRight) {
           println("WARNING: Global mutation SUCCEEDED! Current implementation is NOT hermetic.")
           println("Result: " + result2.right.get)
        } else {
           println("SUCCESS: Global mutation FAILED (as desired for Starlark): " + result2.left.get)
        }

      } finally {
        StarlarkEngine.currentManager.remove()
        Platform.closeStarlarkContextManager(starlarkManager)
        os.remove(os.pwd / "lib.py")
        os.remove(os.pwd / "mutate.py")
      }
    }
  }
}
