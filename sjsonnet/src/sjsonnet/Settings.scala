package sjsonnet

/**
 * Settings for the interpreter. This is a subset of Config which is used in the inner layers of the
 * interpreters and shared between all platforms.
 */
final case class Settings(
    preserveOrder: Boolean = false,
    strict: Boolean = false,
    throwErrorForInvalidSets: Boolean = false,
    useNewEvaluator: Boolean = false,
    maxParserRecursionDepth: Int = 1000,
    brokenAssertionLogic: Boolean = false
)

object Settings {
  val default = new Settings()
}
