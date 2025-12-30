# Starlark Integration for Sjsonnet

This module provides support for a Starlark-like dialect using GraalPy. This allows Jsonnet users to call deterministic Python logic for complex calculations while maintaining the evaluation model of Sjsonnet.

## Architecture: Engine & Context Management

*   **Engine Strategy:** A single, static, shared `org.graalvm.polyglot.Engine` is used. This persists JIT-compiled machine code and ASTs across the application lifecycle for maximum performance.
*   **Context Strategy:** A single `Context` is created per Jsonnet evaluation request. This ensures isolation between requests while allowing shared state within a single evaluation (e.g., via `importstarlark`).

## Module Loading & Caching

*   **Custom Load Function:** Starlark files are loaded as modules using a custom shim. Each file gets its own module object to prevent global namespace pollution.
*   **importstarlark:** A native Jsonnet function `importstarlark(path)` is provided to load these modules.

## Global Structural Caching

To optimize performance, this integration employs a unique **Context-Independent Proxy** caching mechanism:

1.  **Proxies:** Instead of caching context-specific GraalVM `Value` objects, we cache Scala `Val` objects (Proxies) that store only the module path and member traversal path.
2.  **Global Cache:** These Proxies are stored in a global `ConcurrentHashMap`. The expensive traversal of Python module structures happens only **once per module version**.
3.  **On-Demand Resolution:** When a Jsonnet evaluation calls a function on a cached Proxy, the Proxy uses a `ThreadLocal` to find the current evaluation's `Context` and resolves the actual GraalVM `Value` on-the-fly.

This ensures that while Python state is isolated per evaluation, the **structure** of your Starlark modules is shared across the entire JVM, significantly reducing the overhead of Polyglot interop.

## Data Interop

*   **Scala -> Python:** Data passed to Python functions is automatically converted to Python-compatible Java objects (Maps, Lists, etc.).
*   **Python -> Sjsonnet:** Results from Python are recursively mapped back to Sjsonnet `Val` objects.
*   **Export Filtering:** Only members defined in the loaded module itself are exported to Jsonnet (transitive imports are filtered out by default).

## Security & Sandboxing

The runtime environment is restricted to maintain hermeticity:
*   Standard Python `import` machinery is intercepted/restricted.
*   The dialect is intended to be deterministic and side-effect free (though currently implemented via GraalPy with `allowAllAccess(true)` for development, it will be tightened in the future).
