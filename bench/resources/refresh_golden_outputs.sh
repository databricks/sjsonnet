#!/bin/bash
set -euo pipefail

ROOT_DIR="$(git rev-parse --show-toplevel)"
pushd "$ROOT_DIR" || exit 1

./mill show "sjsonnet.jvm[3.3.7].assembly"
SJSONNET="out/sjsonnet/jvm/3.3.7/assembly.dest/out.jar"

for suite in bench/resources/*_suite; do
  suite_name=$(basename "$suite")
  echo "Refreshing golden outputs for suite: $suite_name"
  for f in "$suite"/*.jsonnet; do
    echo "  Processing file: $f"
    java -Xss100m -Xmx2g -jar "$SJSONNET" -J "$suite" "$f" > "$f.golden"
  done
done

popd || exit 1