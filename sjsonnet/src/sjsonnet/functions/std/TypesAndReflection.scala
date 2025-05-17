package sjsonnet.functions.std

import sjsonnet.functions.AbstractFunctionModule
import sjsonnet.{EvalScope, Lazy, Position, Val}

/**
 * Types and Reflection functions [[https://jsonnet.org/ref/stdlib.html#types_reflection]]
 */
object TypesAndReflection extends AbstractFunctionModule {
  override val name: String = "Types and Reflection "

  /**
   * https://jsonnet.org/ref/stdlib.html#std-type
   */
  private object Type extends Val.Builtin1("type", "x") {
    def evalRhs(x: Lazy, ev: EvalScope, pos: Position): Val = Val.Str(pos, x.force.prettyName)
  }

  override val functions: Seq[(String, Val.Func)] = Seq(
    builtin(Type)
  )
}
