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
    brokenAssertionLogic: Boolean = false,
    maxMaterializeDepth: Int = 1000,
    materializeRecursiveDepthLimit: Int = 64
)

object Settings {
  val default = new Settings()
}
