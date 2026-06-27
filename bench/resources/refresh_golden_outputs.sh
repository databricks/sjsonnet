#!/bin/bash
set -euo pipefail

ROOT_DIR="$(git rev-parse --show-toplevel)"
pushd "$ROOT_DIR" || exit 1

# Use the same Scala version the bench actually runs under (scalaVersions.head) so
# goldens never drift from the runtime. Pinning a fixed version here previously broke
# refreshes once that version stopped building.
SCALA_VERSION="$(./mill show bench.scalaVersion 2>/dev/null | tr -d '"[:space:]')"
echo "Using Scala $SCALA_VERSION (bench.scalaVersion)"
./mill show "sjsonnet.jvm[$SCALA_VERSION].assembly"
SJSONNET="out/sjsonnet/jvm/$SCALA_VERSION/assembly.dest/out.jar"

for suite in bench/resources/*_suite; do
  suite_name=$(basename "$suite")
  echo "Refreshing golden outputs for suite: $suite_name"
  for f in "$suite"/*.jsonnet; do
    echo "  Processing file: $f"
    java -Xss100m -Xmx2g -jar "$SJSONNET" --max-stack 100000 -J "$suite" "$f" > "$f.golden"
  done
done

popd || exit 1