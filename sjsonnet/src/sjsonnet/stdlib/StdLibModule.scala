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
    def evalRhs(name: Eval, ev: EvalScope, pos: Position): Val =
      nativeFunctions.getOrElse(name.value.asString, Val.Null(pos))
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

/**
 * Native implementations for Jsonnet standard-library entries in this module.
 *
 * Official Jsonnet stdlib documentation links for this module:
 *
 *   - [[https://jsonnet.org/ref/stdlib.html#std-trace std.trace(str, rest)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-extVar std.extVar(x)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-thisFile std.thisFile]]
 *   - [[https://jsonnet.org/ref/stdlib.html#math std.pi]]
 */
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
  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-trace std.trace(str, rest)]].
   *
   * Since: 0.11.0. Group: Debugging.
   *
   * Outputs the given string str to stderr and returns rest as the result.
   */
  private val traceFunction = new Val.Builtin2("trace", "str", "rest") {
    def evalRhs(str: Eval, rest: Eval, ev: EvalScope, pos: Position): Val = {
      ev.trace(
        s"TRACE: ${pos.fileScope.currentFileLastPathElement} " + str.value.asString
      )
      rest.value
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-extVar std.extVar(x)]].
   *
   * Since: 0.10.0. Group: External Variables.
   *
   * If an external variable with the given name was defined, return its value. Otherwise, raise an
   * error.
   */
  private val extVarFunction = new Val.Builtin1("extVar", "x") {
    def evalRhs(_x: Eval, ev: EvalScope, pos: Position): Val = {
      val x = _x.value.asString
      ev.visitExpr(ev.extVars(x).getOrElse(Error.fail("Unknown extVar: " + x)))(ValScope.empty)
    }

    override def staticSafe = false
  }

  private val additionalStdMembers = Seq(
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-thisFile std.thisFile]].
     *
     * Since: 0.10.0. Group: Types and Reflection.
     *
     * Note that this is a field. It contains the current Jsonnet filename as a string.
     */
    (
      "thisFile",
      new Val.Obj.Member(false, Visibility.Hidden, cached = false, deprecatedSkipAsserts = true) {
        def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val =
          Val.Str(self.pos, fs.currentFile.relativeToString(ev.wd))
      }
    ),
    /**
     * [[https://jsonnet.org/ref/stdlib.html#math std.pi]].
     *
     * Since: 0.21.0. Group: Mathematical Utilities.
     *
     * The constant std.pi is available as the mathematical constant pi.
     */
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
