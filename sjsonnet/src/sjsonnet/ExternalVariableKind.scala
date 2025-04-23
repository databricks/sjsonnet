package sjsonnet

/**
 * An external variable kind indicates the type of external variable.
 * */
sealed trait ExternalVariableKind[T]

object ExternalVariableKind {
  def code: ExternalVariableKind[String] = Code

  def expr: ExternalVariableKind[sjsonnet.Expr] = Expr

  def variable: ExternalVariableKind[String] = Variable

  case object Variable extends ExternalVariableKind[String] {
    override def toString: String = "variable"
  }

  case object Code extends ExternalVariableKind[String] {
    override def toString: String = "code"
  }

  case object Expr extends ExternalVariableKind[sjsonnet.Expr] {
    override def toString: String = "expr"
  }
}

case class ExternalVariable[T](kind: ExternalVariableKind[T], value: T) {
  override def toString: String = s"ExternalVariable($kind, $value)"
}

object ExternalVariable {

  /**
   * the external variable is a code snippet
   * */
  def code(value: String): ExternalVariable[String] = ExternalVariable(ExternalVariableKind.code, value)

  /**
   * the external variable is a parsed jsonnet expression
   * */
  def expr(value: sjsonnet.Expr): ExternalVariable[sjsonnet.Expr] = ExternalVariable(ExternalVariableKind.expr, value)

  /**
   * the external variable is a variable, string literal
   * */
  def variable(value: String): ExternalVariable[String] = ExternalVariable(ExternalVariableKind.variable, value)
}