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
   * Unsynchronized lazy function lookup map — single-threaded, no synchronization needed.
   */
  private var _functionMap: Map[String, Val.Func] = _

  /**
   * Get a function by name, triggering module initialization if needed.
   */
  def getFunction(name: String): Val.Func = {
    var m = _functionMap
    if (m eq null) { m = functions.toMap; _functionMap = m }
    m(name)
  }

  /**
   * the holder of the module object
   */
  override final lazy val module: Val.Obj = moduleFromFunctions(functions: _*)
}
