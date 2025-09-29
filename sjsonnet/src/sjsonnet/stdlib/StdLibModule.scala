package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.Expr.Member.Visibility
import sjsonnet.functions.FunctionModule

/**
 * Main standard library module that combines all the individual stdlib modules
 */
final class StdLibModule(
    private val nativeFunctions: Map[String, Val.Func] = Map.empty,
    private val additionalStdFunctions: Map[String, Val.Func] = Map.empty
) extends FunctionModule {
  import StdLibModule._

  def name = "std"

  // Override the native function to use the provided native functions
  private def nativeFunction = new Val.Builtin1("native", "name") {
    def evalRhs(name: Lazy, ev: EvalScope, pos: Position): Val =
      nativeFunctions.getOrElse(name.force.asString, Val.Null(pos))
  }

  // All functions including native and additional functions
  val functions: Map[String, Val.Func] = allModuleFunctions ++
    additionalStdFunctions +
    ("native" -> nativeFunction) +
    ("trace" -> traceFunction) +
    ("extVar" -> extVarFunction)

  val module: Val.Obj = Val.Obj.mk(
    null,
    functions.size + additionalStdMembers.size,
    functions.view.map { case (k, v) =>
      (
        k,
        new Val.Obj.ConstMember(false, Visibility.Hidden, v)
      )
    },
    additionalStdMembers
  )
}

object StdLibModule {
  // Combine all functions from all modules
  private val allModuleFunctions: Map[String, Val.Func] = (
    ArrayModule.functions ++
      StringModule.functions ++
      ObjectModule.functions ++
      MathModule.functions ++
      TypeModule.functions ++
      EncodingModule.functions ++
      ManifestModule.functions ++
      SetModule.functions ++
      NativeRegex.functions
  ).toMap

  // Core std library functions that belong directly in StdLibModule
  private val traceFunction = new Val.Builtin2("trace", "str", "rest") {
    def evalRhs(str: Lazy, rest: Lazy, ev: EvalScope, pos: Position): Val = {
      ev.trace(
        s"TRACE: ${pos.fileScope.currentFileLastPathElement} " + str.force.asString
      )
      rest.force
    }
  }

  private val extVarFunction = new Val.Builtin1("extVar", "x") {
    def evalRhs(_x: Lazy, ev: EvalScope, pos: Position): Val = {
      val x = _x.force.asString
      ev.visitExpr(ev.extVars(x).getOrElse(Error.fail("Unknown extVar: " + x)))(ValScope.empty)
    }

    override def staticSafe = false
  }

  private val additionalStdMembers = Seq(
    (
      "thisFile",
      new Val.Obj.Member(false, Visibility.Hidden, cached = false) {
        def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val =
          Val.Str(self.pos, fs.currentFile.relativeToString(ev.wd))
      }
    ),
    (
      "pi",
      new Val.Obj.Member(false, Visibility.Hidden, cached = false) {
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
