package sjsonnet.functions.std

import sjsonnet.{Error, EvalScope, Lazy, Position, Val, ValScope}
import sjsonnet.functions.AbstractFunctionModule

/**
 * External Variables functions [[https://jsonnet.org/ref/stdlib.html#ext_vars]]
 */
object ExternalVariables extends AbstractFunctionModule {
  override val name: String = "External Variables"

  /**
   * Available since version 0.10.0.
   *
   * If an external variable with the given name was defined, return its value. Otherwise, raise an
   * error.
   *
   * https://jsonnet.org/ref/stdlib.html#std-extVar
   */
  private object ExtVar extends Val.Builtin1("extVar", "x") {
    def evalRhs(_x: Lazy, ev: EvalScope, pos: Position): Val = {
      val x = _x.force.asString
      ev.visitExpr(ev.extVars(x).getOrElse(Error.fail("Unknown extVar: " + x)))(ValScope.empty)
    }
    override def staticSafe = false
  }

  override val functions: Seq[(String, Val.Func)] = Seq(
    builtin(ExtVar)
  )
}
