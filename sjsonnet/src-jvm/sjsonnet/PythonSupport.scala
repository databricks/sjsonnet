package sjsonnet

import org.graalvm.polyglot._
import java.util.concurrent.ConcurrentHashMap
import sjsonnet.Expr.Member.Visibility

object PythonEngine {
  // Shared engine to enable JIT code sharing across contexts
  lazy val engine: Engine = Engine.newBuilder()
    .option("engine.WarnInterpreterOnly", "false")
    .build()
}

class PythonEvaluator(importer: Importer, fileScope: FileScope) {
  // One context per evaluator/request
  private val context: Context = Context.newBuilder("python")
    .engine(PythonEngine.engine)
    .allowAllAccess(true) // For now, refine later
    .build()

  def close(): Unit = context.close()

  def eval(path: Path, pos: Position): Val = {
    // 1. Resolve and read the file
    // We reuse the existing Importer infrastructure to read the file content
    val resolvedFile = importer.read(path, binaryData = false) match {
      case Some(r) => r
      case None => Error.fail(s"Could not read python file: ${path}", pos)(new EvalErrorScope {
          def settings: Settings = Settings.default
          def trace(msg: String): Unit = ()
          def warn(e: Error): Unit = ()
          def extVars: String => Option[Expr] = _ => None
          def importer: CachedImporter = null
          def wd: Path = OsPath(os.pwd)
      })
    }
    
    val sourceCode = resolvedFile.readString()
    
    // 2. Create Graal Source object
    // Using the path as the name helps Graal cache the compilation
    val source = Source.newBuilder("python", sourceCode, path.toString).build()

    // 3. Evaluate
    try {
      context.eval(source)
      
      // 4. Extract exports (globals)
      
      val loaderShim = 
        """
        |import types
        |def load_module(name, code):
        |    mod = types.ModuleType(name)
        |    exec(code, mod.__dict__)
        |    return mod
        """.stripMargin
        
      context.eval("python", loaderShim)
      val loader = context.getBindings("python").getMember("load_module")
      
      val moduleName = path.last // simplistic module name
      val moduleObj = loader.execute(moduleName, sourceCode)
      
      // 5. Convert exported members to Val.Obj
      PythonMapper.pyToVal(moduleObj, pos)
      
    } catch {
      case e: PolyglotException => 
        Error.fail(s"Python evaluation failed: ${e.getMessage}", pos)(new EvalErrorScope {
          def settings: Settings = Settings.default
          def trace(msg: String): Unit = ()
          def warn(e: Error): Unit = ()
          def extVars: String => Option[Expr] = _ => None
          def importer: CachedImporter = null
          def wd: Path = OsPath(os.pwd)
        })
    }
  }
}

object PythonMapper {
  import scala.collection.JavaConverters._

  def pyToVal(v: Value, pos: Position): Val = {
    if (v.isNull) return Val.Null(pos)
    if (v.isBoolean) return Val.bool(pos, v.asBoolean())
    if (v.isNumber) return Val.Num(pos, v.asDouble())
    if (v.isString) return Val.Str(pos, v.asString())
    
    if (v.hasArrayElements) {
      val len = v.getArraySize
      val arr = new Array[Lazy](len.toInt)
      var i = 0
      while (i < len) {
        val elem = v.getArrayElement(i)
        arr(i) = new LazyWithComputeFunc(() => pyToVal(elem, pos))
        i += 1
      }
      return Val.Arr(pos, arr)
    }
    
    if (v.canExecute) {
       return new PythonFunc(v, pos)
    }

    // Treat Python modules and objects as Jsonnet Objects
    if (v.hasMembers) {
       val keys = v.getMemberKeys
       val builder = new java.util.LinkedHashMap[String, Val.Obj.Member]
       
       for (k <- keys.asScala) {
         if (!k.startsWith("__")) {
           val member = v.getMember(k)
           val isModule = try {
              // This is a heuristic. A better way might be checking type(v) == type(sys)
              member.getMetaObject.getMetaSimpleName == "module"
           } catch { case _: Exception => false }
           
           if (!isModule) {
              builder.put(k, new Val.Obj.Member(false, Visibility.Normal) {
                def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = {
                  pyToVal(member, pos)
                }
              })
           }
         }
       }
       return new Val.Obj(pos, builder, false, null, null)
    }
    
    if (v.canExecute) {
       return new PythonFunc(v, pos)
    }
    
    Val.Str(pos, s"<python object: $v>")
  }
  
  class PythonFunc(v: Value, defSitePos: Position) extends Val.Func(defSitePos, ValScope.empty, Expr.Params(Array.empty, Array.empty)) {
      override def apply(argsL: Array[? <: Lazy], namedNames: Array[String], outerPos: Position)(implicit
            ev: EvalScope,
            tailstrictMode: TailstrictMode): Val = {
            
            // force args
            val args = argsL.map(_.force)
            val pyArgs = args.map(valToPy(_, ev))
            
            if (namedNames != null && namedNames.length > 0) {
              Error.fail("Named arguments not yet supported for Python functions", outerPos)
            }
            
            try {
              val res = v.execute(pyArgs: _*)
              pyToVal(res, outerPos)
            } catch {
              case e: PolyglotException => Error.fail(s"Python execution failed: ${e.getMessage}", outerPos)
            }
      }
      
      def evalRhs(scope: ValScope, ev: EvalScope, fs: FileScope, pos: Position): Val = Val.Null(pos) // Should not be called
  }

  def valToPy(v: Val, ev: EvalScope): Object = v match {
    case s: Val.Str => s.value
    case n: Val.Num => Double.box(n.asDouble)
    case b: Val.Bool => Boolean.box(b.asBoolean)
    case Val.Null(_) => null
    case a: Val.Arr => 
       // Convert to Java List or Array
       a.asStrictArray.map(valToPy(_, ev)).toArray
    case o: Val.Obj =>
       val map = new java.util.HashMap[String, Object]()
       o.foreachElement(false, o.pos) { (k, v) =>
         map.put(k, valToPy(v, ev))
       }(ev)
       map
    case _ => v.toString // Fallback
  }
}