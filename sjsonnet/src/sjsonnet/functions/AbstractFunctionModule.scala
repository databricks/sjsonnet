package sjsonnet.functions

import sjsonnet.Val

/**
 * An abstract base class for function modules.
 */
abstract class AbstractFunctionModule extends FunctionModule {

  /**
   * Module function names — cheap string array for lazy registration. No Val.Func objects created.
   */
  def functionNames: Array[String]

  /**
   * module's functions — override as lazy val in concrete modules to defer Val.Builtin creation.
   */
  def functions: Seq[(String, Val.Func)]

  /**
   * Lazy function lookup map, built on first access to any function in this module.
   */
  private lazy val functionMap: Map[String, Val.Func] = functions.toMap

  /**
   * Get a function by name, triggering module initialization if needed.
   */
  def getFunction(name: String): Val.Func = functionMap(name)

  /**
   * the holder of the module object
   */
  override final lazy val module: Val.Obj = moduleFromFunctions(functions: _*)
}
