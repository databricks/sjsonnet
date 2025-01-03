package sjsonnet

/** Settings for the interpreter. This is a subset of Config which is used in the inner layers
 * of the interpreters and shared between all platforms.  */
class Settings(
  val preserveOrder: Boolean = false,
  val strict: Boolean = false,
  val noStaticErrors: Boolean = false,
  val noDuplicateKeysInComprehension: Boolean = false,
  val strictImportSyntax: Boolean = false,
  val strictInheritedAssertions: Boolean = false,
  val strictSetOperations: Boolean = false,
  val throwErrorForInvalidSets: Boolean = false,
  val disableBuiltinSpecialization: Boolean = false,
  val disableStaticApplyForBuiltinFunctions: Boolean = false
)

object Settings {
  val default = new Settings()
}
