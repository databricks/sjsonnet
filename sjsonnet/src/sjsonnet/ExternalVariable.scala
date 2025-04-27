package sjsonnet

/**
 * An external variable kind indicates the type of external variable.
 * */
sealed trait ExternalVariableKind[T]

object ExternalVariableKind {
  /**
   * Get an instance of ExternalVariableKind for a code snippet.
   * */
  def code: ExternalVariableKind[String] = Code

  /**
   * Get an instance of ExternalVariableKind for an expr.
   * */
  def expr: ExternalVariableKind[sjsonnet.Expr] = Expr

  /**
   * Get an instance of ExternalVariableKind for a variable.
   * */
  def variable: ExternalVariableKind[String] = Variable

  /**
   * Indicates that the external variable is a code snippet.
   * */
  case object Variable extends ExternalVariableKind[String] {
    override def toString: String = "variable"
  }

  /**
   * Indicates that the external variable is a string literal.
   * */
  case object Code extends ExternalVariableKind[String] {
    override def toString: String = "code"
  }

  /**
   * Indicates that the external variable is a parsed jsonnet expression.
   * */
  case object Expr extends ExternalVariableKind[sjsonnet.Expr] {
    override def toString: String = "expr"
  }
}

final case class ExternalVariable[T](kind: ExternalVariableKind[T], value: T) {
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
