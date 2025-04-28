package sjsonnet.functions

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Val.Obj
import sjsonnet.{Position, Val}

/**
 * Function modules are collections of functions that can be used in Jsonnet.
 */
trait FunctionModule extends FunctionBuilder {
  protected val position = new Position(null, 0)

  /**
   * module name
   * */
  def name: String

  /**
   * function module object
   * */
  def module: Val.Obj

  /**
   * Create a module from a sequence of functions.
   *
   * @param functions the functions to include in the module
   * @return the module object
   */
  def moduleFromFunctions(functions: (String, Val.Func)*): Val.Obj = {
    Val.Obj.mk(position, functions.map { case (k, v) => (k, memberOf(v)) }: _*)
  }

  /**
   * Create a module from a sequence of sub-modules.
   *
   * @param subModules the sub-modules to include in the module
   * @return the module object
   */
  def moduleFromModules(subModules: FunctionModule*): Val.Obj = {
    Val.Obj.mk(position, subModules.map { module => (module.name, memberOf(module.module)) }: _*)
  }

  /**
   * Create a member of the module.
   *
   * @param value the value to include in the module
   * @return the member object
   */
  def memberOf(value: Val): Obj.Member = new Obj.ConstMember(false, Visibility.Normal, value)
}
