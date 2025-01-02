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
)

object Settings {
  val default = new Settings()
  def fromConfig(config: Config): Settings = {
    new Settings(
      preserveOrder = config.preserveOrder.value,
      strict = config.strict.value,
      noStaticErrors = config.noStaticErrors.value,
      noDuplicateKeysInComprehension = config.noDuplicateKeysInComprehension.value,
      strictImportSyntax = config.strictImportSyntax.value,
      strictInheritedAssertions = config.strictInheritedAssertions.value,
      strictSetOperations = config.strictSetOperations.value,
      throwErrorForInvalidSets = config.throwErrorForInvalidSets.value,
      disableBuiltinSpecialization = config.disableBuiltinSpecialization.value
    )
  }
}
