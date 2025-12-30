package sjsonnet.starlark

import java.util.concurrent.ConcurrentHashMap

// import scala.jdk.CollectionConverters._

import org.graalvm.polyglot._
import org.graalvm.polyglot.proxy.ProxyExecutable

import sjsonnet.Expr.Member.Visibility
import sjsonnet.{
  Error,
  EvalErrorScope,
  EvalScope,
  Expr,
  FileScope,
  Importer,
  Lazy,
  LazyWithComputeFunc,
  Path,
  Position,
  TailstrictMode,
  Val
}

object StarlarkEngine {

  // Shared engine to enable JIT code sharing across contexts
  lazy val engine: Engine = Engine.newBuilder()
    // Emit a warning if we're running on a JVM that does not support Truffle optimizations
    .option("engine.WarnInterpreterOnly", "true")
    .build()

  private val sourceCache = new ConcurrentHashMap[(Path, String), Source]()

  private val globalValCache = new ConcurrentHashMap[(Path, Seq[String]), Val]()

  val currentManager = new ThreadLocal[StarlarkContextManager]()

  def getSource(path: Path, code: String): Source = {
    sourceCache.computeIfAbsent((path, code), _ =>
      Source.newBuilder("python", code, path.toString).build()
    )
  }

  def getCachedVal(path: Path, members: Seq[String]): Val = globalValCache.get((path, members))

  def cacheVal(path: Path, members: Seq[String], v: Val): Val = {
    val existing = globalValCache.putIfAbsent((path, members), v)
    if (existing != null) existing else v
  }
}

class StarlarkContextManager {
  private var context: Context = _
  private var loader: Value = _
  private val moduleValueCache = new java.util.HashMap[Path, Value]()

  def getContext: Context = {
    if (context == null) {
      context = Context.newBuilder("python")
        .engine(StarlarkEngine.engine)
        .allowAllAccess(true)
        .build()
      
      val loaderShim = 
        """
        |import types
        |
        |def freeze(obj, seen=None):
        |    if seen is None: seen = set()
        |    if id(obj) in seen: return obj
        |    seen.add(id(obj))
        |    if isinstance(obj, list):
        |        return tuple(freeze(i, seen) for i in obj)
        |    if isinstance(obj, dict):
        |        return types.MappingProxyType({k: freeze(v, seen) for k, v in obj.items()})
        |    return obj
        |
        |def load_module(name, code, path):
        |    mod = types.ModuleType(name)
        |    mod.__file__ = path
        |    exec(code, mod.__dict__)
        |    # Freeze user-defined globals
        |    for k in list(mod.__dict__.keys()):
        |        if not k.startswith("__"):
        |            mod.__dict__[k] = freeze(mod.__dict__[k])
        |    return mod
        """.stripMargin
        
      context.eval("python", loaderShim)
      loader = context.getBindings("python").getMember("load_module")
    }
    context
  }

  def getModuleValue(path: Path, code: String): Value = {
    var mod = moduleValueCache.get(path)
    if (mod == null) {
      getContext
      mod = loader.execute(path.last, code, path.toString)
      moduleValueCache.put(path, mod)
    }
    mod
  }

  def getNestedValue(path: Path, members: Seq[String], code: String): Value = {
    var v = getModuleValue(path, code)
    for (m <- members) {
      v = v.getMember(m)
    }
    v
  }

  def loadModel(path: Path, pos: Position, importer: Importer)(implicit ev: EvalErrorScope): Val = {
    val resolvedFile = importer.read(path, binaryData = false).getOrElse(
      Error.fail(s"Could not read starlark file: ${path}", pos)
    )
    val code = resolvedFile.readString()
    
    try {
      getModuleValue(path, code)
      StarlarkMapper.getGlobalVal(path, Nil, pos, code)
    } catch {
      case e: PolyglotException => 
        Error.fail(s"Starlark evaluation failed: ${e.getMessage}", pos)
    }
  }

  def close(): Unit = {
    if (context != null) {
      context.close()
      context = null
    }
    moduleValueCache.clear()
  }
}

object StarlarkMapper {
  def getGlobalVal(path: Path, members: Seq[String], pos: Position, code: String): Val = {
    val cached = StarlarkEngine.getCachedVal(path, members)
    if (cached != null) return cached

    val manager = StarlarkEngine.currentManager.get()
    val v = manager.getNestedValue(path, members, code)

    val res = if (v.isNull) Val.Null(pos)
    else if (v.isBoolean) Val.bool(pos, v.asBoolean())
    else if (v.isNumber) Val.Num(pos, v.asDouble())
    else if (v.isString) Val.Str(pos, v.asString())
    else if (v.canExecute) new GlobalStarlarkFunc(path, members, pos, code)
    else if (v.hasArrayElements) {
      val len = v.getArraySize.toInt
      val arr = new Array[Lazy](len)
      for (i <- 0 until len) {
        arr(i) = new LazyWithComputeFunc(() => {
          val m = StarlarkEngine.currentManager.get()
          val vv = m.getNestedValue(path, members, code).getArrayElement(i.toLong)
          pyToVal(vv, pos)
        })
      }
      Val.Arr(pos, arr)
    }
    else if (v.hasMembers) {
      val isModule = v.getMetaObject.getMetaSimpleName == "module"
      val moduleName = if (isModule) v.getMember("__name__").asString() else null
      
      val builder = new java.util.LinkedHashMap[String, Val.Obj.Member]
      
      v.getMemberKeys.forEach { k =>
        if (!k.startsWith("__")) {
          val member = v.getMember(k)
          val shouldExport = if (isModule) {
            try {
              val memberMod = member.getMember("__module__")
              memberMod != null && memberMod.asString() == moduleName
            } catch { case _: Exception => true }
          } else true

          if (shouldExport) {
            builder.put(k, new Val.Obj.Member(false, Visibility.Normal) {
              def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = {
                getGlobalVal(path, members :+ k, pos, code)
              }
            })
          }
        }
      }
      new Val.Obj(pos, builder, false, null, null)
    }
    else Val.Str(pos, s"<starlark object: $v>")

    StarlarkEngine.cacheVal(path, members, res)
  }

  def pyToVal(v: Value, pos: Position): Val = {
    if (v.isNull) return Val.Null(pos)
    if (v.isBoolean) return Val.bool(pos, v.asBoolean())
    if (v.isNumber) return Val.Num(pos, v.asDouble())
    if (v.isString) return Val.Str(pos, v.asString())
    if (v.hasArrayElements) {
      val len = v.getArraySize.toInt
      val arr = new Array[Lazy](len)
      for (i <- 0 until len) {
        val elem = v.getArrayElement(i.toLong)
        arr(i) = new LazyWithComputeFunc(() => pyToVal(elem, pos))
      }
      return Val.Arr(pos, arr)
    }
    if (v.canExecute) return new LocalStarlarkFunc(v, pos)
    if (v.hasMembers) {
       val builder = new java.util.LinkedHashMap[String, Val.Obj.Member]
       v.getMemberKeys.forEach { k =>
         if (!k.startsWith("__")) {
           val member = v.getMember(k)
           builder.put(k, new Val.Obj.Member(false, Visibility.Normal) {
             def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = pyToVal(member, pos)
           })
         }
       }
       return new Val.Obj(pos, builder, false, null, null)
    }
    Val.Str(pos, s"<starlark object: $v>")
  }

  class LocalStarlarkFunc(v: Value, defSitePos: Position) extends Val.Func(defSitePos, sjsonnet.ValScope.empty, Expr.Params(Array.empty, Array.empty)) {
      override def apply(argsL: Array[? <: Lazy], namedNames: Array[String], outerPos: Position)(implicit
            ev: EvalScope,
            tailstrictMode: TailstrictMode): Val = {
            val args = argsL.map(_.force)
            val pyArgs = args.map(valToPy(_, ev))
            if (namedNames != null && namedNames.length > 0) Error.fail("Named arguments not supported", outerPos)
            try {
              val res = v.execute(pyArgs: _*)
              pyToVal(res, outerPos)
            } catch {
              case e: PolyglotException => Error.fail(s"Starlark execution failed: ${e.getMessage}", outerPos)
            }
      }
      override def apply0(outerPos: Position)(implicit ev: EvalScope, tailstrictMode: TailstrictMode): Val = apply(Array.empty, null, outerPos)
      override def apply1(argVal: Lazy, outerPos: Position)(implicit ev: EvalScope, tailstrictMode: TailstrictMode): Val = apply(Array(argVal), null, outerPos)
      override def apply2(argVal1: Lazy, argVal2: Lazy, outerPos: Position)(implicit ev: EvalScope, tailstrictMode: TailstrictMode): Val = apply(Array(argVal1, argVal2), null, outerPos)
      override def apply3(argVal1: Lazy, argVal2: Lazy, argVal3: Lazy, outerPos: Position)(implicit ev: EvalScope, tailstrictMode: TailstrictMode): Val = apply(Array(argVal1, argVal2, argVal3), null, outerPos)
      def evalRhs(scope: sjsonnet.ValScope, ev: EvalScope, fs: FileScope, pos: Position): Val = Val.Null(pos)
  }

  class GlobalStarlarkFunc(val path: Path, val members: Seq[String], defSitePos: Position, val code: String) extends Val.Func(defSitePos, sjsonnet.ValScope.empty, Expr.Params(Array.empty, Array.empty)) {
      override def apply(argsL: Array[? <: Lazy], namedNames: Array[String], outerPos: Position)(implicit
            ev: EvalScope,
            tailstrictMode: TailstrictMode): Val = {
            val manager = StarlarkEngine.currentManager.get()
            val v = manager.getNestedValue(path, members, code)
            val args = argsL.map(_.force)
            val pyArgs = args.map(valToPy(_, ev))
            try {
              val res = v.execute(pyArgs: _*)
              pyToVal(res, outerPos)
            } catch {
              case e: PolyglotException => Error.fail(s"Starlark execution failed: ${e.getMessage}", outerPos)
            }
      }
      override def apply0(outerPos: Position)(implicit ev: EvalScope, tailstrictMode: TailstrictMode): Val = apply(Array.empty, null, outerPos)
      override def apply1(argVal: Lazy, outerPos: Position)(implicit ev: EvalScope, tailstrictMode: TailstrictMode): Val = apply(Array(argVal), null, outerPos)
      override def apply2(argVal1: Lazy, argVal2: Lazy, outerPos: Position)(implicit ev: EvalScope, tailstrictMode: TailstrictMode): Val = apply(Array(argVal1, argVal2), null, outerPos)
      override def apply3(argVal1: Lazy, argVal2: Lazy, argVal3: Lazy, outerPos: Position)(implicit ev: EvalScope, tailstrictMode: TailstrictMode): Val = apply(Array(argVal1, argVal2, argVal3), null, outerPos)
      def evalRhs(scope: sjsonnet.ValScope, ev: EvalScope, fs: FileScope, pos: Position): Val = Val.Null(pos)
  }

  def valToPy(v: Val, ev: EvalScope): Object = v match {
    case s: Val.Str => s.value
    case n: Val.Num => Double.box(n.asDouble)
    case b: Val.Bool => Boolean.box(b.asBoolean)
    case Val.Null(_) => null
    case f: GlobalStarlarkFunc => 
      val manager = StarlarkEngine.currentManager.get()
      manager.getNestedValue(f.path, f.members, f.code)
    case f: Val.Func => new ProxyExecutable {
      override def execute(args: Value*): Object = {
        val jsonnetArgs = new Array[Val](args.length)
        var i = 0
        while (i < args.length) {
          jsonnetArgs(i) = pyToVal(args(i), null)
          i += 1
        }
        val res = f.apply(jsonnetArgs.map(v => v: Lazy), null, null)(ev, sjsonnet.TailstrictModeDisabled)
        valToPy(res, ev)
      }
    }
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

class StarlarkImportFunc(manager: StarlarkContextManager, importer: Importer) extends Val.Builtin1("importstarlark", "path") {
  def evalRhs(arg1: Lazy, ev: EvalScope, pos: Position): Val = {
    val pathStr = arg1.force.asString
    val currentFile = pos.fileScope.currentFile
    val resolvedPath = currentFile.parent() / pathStr
    manager.loadModel(resolvedPath, pos, importer)(ev)
  }
}
