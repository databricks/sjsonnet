#!/bin/bash
set -euo pipefail

ROOT_DIR="$(git rev-parse --show-toplevel)"
pushd "$ROOT_DIR" || exit 1

./mill show "sjsonnet.jvm[3.3.7].assembly"
SJSONNET="out/sjsonnet/jvm/3.3.7/assembly.dest/out.jar"

JAVA_OPTS="-Xss100m -Xms2g -XX:+UseG1GC"
PARAMS="--strict-import-syntax --strict-inherited-assertions --strict-set-operations"

SCRIPT_DIR="sjsonnet/test/resources"

# Process test_suite
suite="$SCRIPT_DIR/test_suite"
echo "Refreshing golden outputs for suite: test_suite"
for f in "$suite"/*.jsonnet; do
  # Skip files that don't exist (in case no matches)
  [ -e "$f" ] || continue

  echo "  Processing file: $f"

  EXT_PARAMS=""
  TLA_PARAMS=""
  if [[ $f == */tla.* ]] ; then
    TLA_PARAMS="--tla-str=var1=test --tla-code=var2={x:1,y:2}"
  else
    EXT_PARAMS="--ext-str=var1=test --ext-code=var2={x:1,y:2}"
  fi

  java $JAVA_OPTS -jar "$SJSONNET" $PARAMS $EXT_PARAMS $TLA_PARAMS "$f" > "$f.golden" 2>&1 || true
done

# Process go_test_suite
suite="$SCRIPT_DIR/go_test_suite"
echo "Refreshing golden outputs for suite: go_test_suite"
for f in "$suite"/*.jsonnet; do
  # Skip files that don't exist (in case no matches)
  [ -e "$f" ] || continue

  echo "  Processing file: $f"
  java $JAVA_OPTS -jar "$SJSONNET" $PARAMS "$f" > "$f.golden" 2>&1 || true
done

popd || exit 1

echo "Done refreshing golden outputs for test_suite and go_test_suite."

