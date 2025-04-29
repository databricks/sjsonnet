package sjsonnet.functions

import sjsonnet.Val

/**
 * An abstract base class for function modules.
 */
abstract class AbstractFunctionModule extends FunctionModule {

  /**
   * module's functions
   */
  val functions: Seq[(String, Val.Func)]

  /**
   * the hodler of the module object
   */
  override final lazy val module: Val.Obj = moduleFromFunctions(functions: _*)
}
