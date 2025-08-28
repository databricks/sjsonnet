package sjsonnet

/**
 * Settings for the interpreter. This is a subset of Config which is used in the inner layers of the
 * interpreters and shared between all platforms.
 */
case class Settings(
    val preserveOrder: Boolean = false,
    val strict: Boolean = false,
    val throwErrorForInvalidSets: Boolean = false,
    val useNewEvaluator: Boolean = false
)

object Settings {
  val default = new Settings()
}
