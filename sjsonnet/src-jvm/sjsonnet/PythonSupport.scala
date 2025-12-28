package sjsonnet

import org.graalvm.polyglot._
import java.util.concurrent.ConcurrentHashMap
import sjsonnet.Expr.Member.Visibility

object PythonEngine {
  // Shared engine to enable JIT code sharing across contexts
  lazy val engine: Engine = Engine.newBuilder()
    .option("engine.WarnInterpreterOnly", "false")
    .build()

  private val sourceCache = new ConcurrentHashMap[(Path, String), Source]()

  def getSource(path: Path, code: String): Source = {
    val key = (path, code)
    var src = sourceCache.get(key)
    if (src == null) {
      src = Source.newBuilder("python", code, path.toString).build()
      val existing = sourceCache.putIfAbsent(key, src)
      if (existing != null) src = existing
    }
    src
  }
}

class PythonContextManager {
  private var context: Context = _
  private var loader: Value = _
  private val moduleCache = new java.util.HashMap[Path, Val]()

  def getContext: Context = {
    if (context == null) {
      context = Context.newBuilder("python")
        .engine(PythonEngine.engine)
        .allowAllAccess(true)
        .build()
      
      val loaderShim = 
        """
        |import types
        |def load_module(name, code, path):
        |    mod = types.ModuleType(name)
        |    mod.__file__ = path
        |    exec(code, mod.__dict__)
        |    return mod
        """.stripMargin
        
      context.eval("python", loaderShim)
      loader = context.getBindings("python").getMember("load_module")
    }
    context
  }

  def loadModel(path: Path, pos: Position, importer: Importer)(implicit ev: EvalErrorScope): Val = {
    val cached = moduleCache.get(path)
    if (cached != null) return cached

    val ctx = getContext
    val resolvedFile = importer.read(path, binaryData = false).getOrElse(
      Error.fail(s"Could not read python file: ${path}", pos)
    )
    val code = resolvedFile.readString()
    
    // Ensure the source is registered in the engine for JIT
    PythonEngine.getSource(path, code)

    try {
      val moduleName = path.last
      val moduleObj = loader.execute(moduleName, code, path.toString)
      val result = PythonMapper.pyToVal(moduleObj, pos)
      moduleCache.put(path, result)
      result
    } catch {
      case e: PolyglotException => 
        Error.fail(s"Python evaluation failed: ${e.getMessage}", pos)
    }
  }

  def close(): Unit = {
    if (context != null) {
      context.close()
      context = null
    }
    moduleCache.clear()
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
    
    Val.Str(pos, s"<python object: $v>")
  }
  
  class PythonFunc(v: Value, defSitePos: Position) extends Val.Func(defSitePos, ValScope.empty, Expr.Params(Array.empty, Array.empty)) {
      override def apply(argsL: Array[? <: Lazy], namedNames: Array[String], outerPos: Position)(implicit
            ev: EvalScope,
            tailstrictMode: TailstrictMode): Val = {
            
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

      override def apply0(outerPos: Position)(implicit ev: EvalScope, tailstrictMode: TailstrictMode): Val =
        apply(Array.empty, null, outerPos)

      override def apply1(argVal: Lazy, outerPos: Position)(implicit ev: EvalScope, tailstrictMode: TailstrictMode): Val =
        apply(Array(argVal), null, outerPos)

      override def apply2(argVal1: Lazy, argVal2: Lazy, outerPos: Position)(implicit ev: EvalScope, tailstrictMode: TailstrictMode): Val =
        apply(Array(argVal1, argVal2), null, outerPos)

      override def apply3(argVal1: Lazy, argVal2: Lazy, argVal3: Lazy, outerPos: Position)(implicit ev: EvalScope, tailstrictMode: TailstrictMode): Val =
        apply(Array(argVal1, argVal2, argVal3), null, outerPos)
      
      def evalRhs(scope: ValScope, ev: EvalScope, fs: FileScope, pos: Position): Val = Val.Null(pos)
  }

  def valToPy(v: Val, ev: EvalScope): Object = v match {
    case s: Val.Str => s.value
    case n: Val.Num => Double.box(n.asDouble)
    case b: Val.Bool => Boolean.box(b.asBoolean)
    case Val.Null(_) => null
    case a: Val.Arr => 
       a.asStrictArray.map(valToPy(_, ev)).toArray
    case o: Val.Obj =>
       val map = new java.util.HashMap[String, Object]()
       o.foreachElement(false, o.pos) { (k, v) =>
         map.put(k, valToPy(v, ev))
       }(ev)
       map
    case _ => v.toString
  }
}

class PythonImportFunc(manager: PythonContextManager, importer: Importer) extends Val.Builtin1("importpy", "path") {
  def evalRhs(arg1: Lazy, ev: EvalScope, pos: Position): Val = {
    val pathStr = arg1.force.asString
    val currentFile = pos.fileScope.currentFile
    val resolvedPath = currentFile.parent() / pathStr
    manager.loadModel(resolvedPath, pos, importer)(ev)
  }
}
