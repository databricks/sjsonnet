package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.Expr.Member.Visibility
import sjsonnet.functions.{AbstractFunctionModule, FunctionModule}

/**
 * Main standard library module that combines all the individual stdlib modules.
 *
 * Uses lazy initialization: only function names (cheap string arrays) are registered at startup.
 * The actual Val.Builtin objects are created on first access, per-module granularity.
 */
final class StdLibModule(
    private val nativeFunctions: Map[String, Val.Func] = Map.empty,
    private val additionalStdFunctions: Map[String, Val.Func] = Map.empty
) extends FunctionModule {
  import StdLibModule._

  def name = "std"

  // Override the native function to use the provided native functions
  private def nativeFunction = new Val.Builtin1("native", "name") {
    def evalRhs(name: Eval, ev: EvalScope, pos: Position): Val =
      nativeFunctions.getOrElse(name.value.asString, Val.Null(pos))
  }

  val module: Val.Obj = {
    // Estimate total size: module functions + additional std functions + native/trace/extVar + pi/thisFile
    val totalSize =
      sharedLazyMembers.size + additionalStdFunctions.size + 3 + additionalStdMembers.size
    val entries = Util.preSizedJavaLinkedHashMap[String, Val.Obj.Member](totalSize)

    // Shared lazy members — created once in companion object, reused across all instances.
    // After first StdLibModule evaluation, LazyConstMember._val is populated and subsequent
    // instances get the cached values directly (single null-check fast path).
    entries.putAll(sharedLazyMembers)

    // Additional std functions (eager — typically empty or small)
    for ((k, v) <- additionalStdFunctions)
      entries.put(k, new Val.Obj.ConstMember(false, Visibility.Hidden, v))

    // Core functions (eager — always needed, only 3)
    entries.put("native", new Val.Obj.ConstMember(false, Visibility.Hidden, nativeFunction))
    entries.put("trace", new Val.Obj.ConstMember(false, Visibility.Hidden, traceFunction))
    entries.put("extVar", new Val.Obj.ConstMember(false, Visibility.Hidden, extVarFunction))

    // Non-function members
    for ((k, v) <- additionalStdMembers) entries.put(k, v)

    new Val.Obj(null, entries, false, null, null)
  }
}

object StdLibModule {
  // All stdlib modules — referenced but NOT initialized (functions are lazy val)
  private val modules: Array[AbstractFunctionModule] = Array(
    ArrayModule,
    StringModule,
    ObjectModule,
    MathModule,
    TypeModule,
    EncodingModule,
    ManifestModule,
    SetModule,
    NativeRegex
  )

  // Build name→module index using only cheap functionNames arrays (no Val.Builtin created)
  private val nameToModule: java.util.LinkedHashMap[String, AbstractFunctionModule] = {
    val m = new java.util.LinkedHashMap[String, AbstractFunctionModule](256)
    var i = 0
    while (i < modules.length) {
      val mod = modules(i)
      val names = mod.functionNames
      var j = 0
      while (j < names.length) {
        m.put(names(j), mod)
        j += 1
      }
      i += 1
    }
    m
  }

  // Shared LazyConstMember objects — created once at class loading, reused across all
  // StdLibModule instances. After first evaluation, each member caches its resolved Val.Func,
  // so subsequent instances pay only a null-check per member access (no closure or Map lookup).
  private val sharedLazyMembers: java.util.LinkedHashMap[String, Val.Obj.Member] = {
    val m = new java.util.LinkedHashMap[String, Val.Obj.Member](256)
    val iter = nameToModule.entrySet().iterator()
    while (iter.hasNext) {
      val e = iter.next()
      val n = e.getKey
      val mod = e.getValue
      m.put(n, new Val.Obj.LazyConstMember(false, Visibility.Hidden, () => mod.getFunction(n)))
    }
    m
  }

  // Core std library functions that belong directly in StdLibModule
  private val traceFunction = new Val.Builtin2("trace", "str", "rest") {
    def evalRhs(str: Eval, rest: Eval, ev: EvalScope, pos: Position): Val = {
      ev.trace(
        s"TRACE: ${pos.fileScope.currentFileLastPathElement} " + str.value.asString
      )
      rest.value
    }
  }

  private val extVarFunction = new Val.Builtin1("extVar", "x") {
    def evalRhs(_x: Eval, ev: EvalScope, pos: Position): Val = {
      val x = _x.value.asString
      ev.visitExpr(ev.extVars(x).getOrElse(Error.fail("Unknown extVar: " + x)))(ValScope.empty)
    }

    override def staticSafe = false
  }

  private val additionalStdMembers = Seq(
    (
      "thisFile",
      new Val.Obj.Member(false, Visibility.Hidden, cached = false, deprecatedSkipAsserts = true) {
        def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val =
          Val.Str(self.pos, fs.currentFile.relativeToString(ev.wd))
      }
    ),
    (
      "pi",
      new Val.Obj.Member(false, Visibility.Hidden, cached = false, deprecatedSkipAsserts = true) {
        def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val =
          Val.Num(self.pos, math.Pi)
      }
    )
  )

  /**
   * The default standard library module instance
   */
  val Default = new StdLibModule()
}
