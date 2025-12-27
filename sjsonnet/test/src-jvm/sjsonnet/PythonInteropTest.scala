package sjsonnet

import org.graalvm.polyglot._
import utest._
import scala.collection.JavaConverters._

object PythonInteropTest extends TestSuite {
  
  lazy val context = Context.newBuilder("python")
    .allowAllAccess(true)
    .build()

  def tests = Tests {
    test("python_function_call") {
      // 1. Define Python function
      val pythonSrc = 
        """
        |def my_len(d):
        |  return len(d)
        """.stripMargin
      context.eval("python", pythonSrc)
      val pyBindings = context.getBindings("python")
      val pyLen = pyBindings.getMember("my_len")

      // 2. Define Scala binding
      // We implement a custom Val.Builtin that calls the Python function
      class PyLenFunc extends Val.Builtin1("my_len", "d") {
        def evalRhs(arg1: Lazy, ev: EvalScope, pos: Position): Val = {
           val v = arg1.force
           val pyArg = valToPy(v, ev)
           val result = pyLen.execute(pyArg)
           if (result.isNumber) Val.Num(pos, result.asDouble())
           else throw new Exception("Expected number from python")
        }
      }

      // 3. Helper to convert Val to Java objects for Graal
      def valToPy(v: Val, ev: EvalScope): Object = v match {
        case o: Val.Obj =>
          val map = new java.util.HashMap[String, Object]()
          o.foreachElement(false, o.pos) { (k, v) =>
             map.put(k, valToPy(v, ev))
          }(ev)
          map
        case s: Val.Str => s.value
        case n: Val.Num => Double.box(n.asDouble)
        case b: Val.Bool => Boolean.box(b.asBoolean)
        case Val.Null(_) => null
        case _ => throw new Exception(s"Unsupported type for conversion: ${v.getClass}")
      }

      // 4. Run Interpreter with this external variable
      // Interpreter's constructor taking extVars Map[String, String] converts them to Code.
      // We need the constructor that takes `String => Option[ExternalVariable[?]]` to pass an Expr.
      
      val customInterp = new Interpreter(
        queryExtVar = {
          case "my_len" => Some(ExternalVariable.expr(new PyLenFunc))
          case _ => None
        },
        queryTlaVar = _ => None,
        wd = OsPath(os.pwd),
        importer = Importer.empty,
        parseCache = new DefaultParseCache,
        settings = Settings.default,
        storePos = _ => (),
        logger = null,
        std = sjsonnet.stdlib.StdLibModule.Default.module,
        variableResolver = _ => None
      )

      val jsonnetSrc = 
        """
        |local my_len = std.extVar("my_len");
        |my_len({a: 1, b: 2, c: 3})
        """.stripMargin

      val result = customInterp.interpret(jsonnetSrc, OsPath(os.pwd / "test.jsonnet"))
      
      assert(result == Right(ujson.Num(3)))
    }
  }
}
