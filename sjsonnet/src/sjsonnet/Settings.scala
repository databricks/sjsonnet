package sjsonnet

/**
 * Settings for the interpreter. This is a subset of Config which is used in the inner layers of the
 * interpreters and shared between all platforms.
 */
final case class Settings(
    val preserveOrder: Boolean = false,
    val strict: Boolean = false,
    val throwErrorForInvalidSets: Boolean = false,
    val useNewEvaluator: Boolean = false,
    val maxParserRecursionDepth: Int = 1000
)

object Settings {
  val default = new Settings()
}
