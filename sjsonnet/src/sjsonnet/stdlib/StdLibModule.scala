package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.Expr.Member.Visibility
import sjsonnet.functions.FunctionModule

/**
 * Main standard library module that combines all the individual stdlib modules
 */
class StdLibModule(
    private val nativeFunctions: Map[String, Val.Func] = Map.empty,
    private val additionalStdFunctions: Map[String, Val.Func] = Map.empty
) extends FunctionModule {

  def name = "std"

  // Individual modules
  private val arrayModule = new ArrayModule()
  private val stringModule = new StringModule()
  private val objectModule = new ObjectModule()
  private val mathModule = new MathModule()
  private val typeModule = new TypeModule()
  private val encodingModule = new EncodingModule()
  private val manifestModule = new ManifestModule()
  private val setModule = new SetModule()
  private val regexModule = new NativeRegex()

  // Combine all functions from all modules
  private val allModuleFunctions: Map[String, Val.Func] = (
    arrayModule.functions ++
      stringModule.functions ++
      objectModule.functions ++
      mathModule.functions ++
      typeModule.functions ++
      encodingModule.functions ++
      manifestModule.functions ++
      setModule.functions ++
      regexModule.functions
  ).toMap

  // Override the native function to use the provided native functions
  private val nativeFunction = new Val.Builtin1("native", "name") {
    def evalRhs(name: Lazy, ev: EvalScope, pos: Position): Val =
      nativeFunctions.getOrElse(name.force.asString, Val.Null(pos))
  }

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

  // All functions including native and additional functions
  val functions: Map[String, Val.Func] = allModuleFunctions ++
    additionalStdFunctions +
    ("native" -> nativeFunction) +
    ("trace" -> traceFunction) +
    ("extVar" -> extVarFunction)

  val module: Val.Obj = Val.Obj.mk(
    null,
    (functions ++ additionalStdFunctions).toSeq
      .map { case (k, v) =>
        (
          k,
          new Val.Obj.ConstMember(false, Visibility.Hidden, v)
        )
      } ++ Seq(
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
    ): _*
  )
}
