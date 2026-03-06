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
    materializeRecursiveDepthLimit: Int = 128,
    maxStack: Int = 500,
    /**
     * Enable aggressive static optimizations in the optimization phase, including: constant folding
     * for arithmetic, comparison, bitwise, and shift operators; branch elimination for if-else with
     * constant conditions; short-circuit elimination for And/Or with constant lhs. These reduce AST
     * node count, benefiting long-running Jsonnet programs.
     */
    aggressiveStaticOptimization: Boolean = false
)

object Settings {
  val default = new Settings()
}
