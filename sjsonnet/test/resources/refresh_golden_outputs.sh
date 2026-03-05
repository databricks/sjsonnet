#!/bin/bash
set -euo pipefail

ROOT_DIR="$(git rev-parse --show-toplevel)"
pushd "$ROOT_DIR" || exit 1

./mill show "sjsonnet.jvm[3.3.7].assembly"
SJSONNET="$ROOT_DIR/out/sjsonnet/jvm/3.3.7/assembly.dest/out.jar"

JAVA_OPTS="-Xss100m -Xms2g -XX:+UseParallelGC"
PARAMS=""

SCRIPT_DIR="sjsonnet/test/resources"

# If specific files are passed as arguments, regenerate only those
if [ $# -gt 0 ]; then
  for f in "$@"; do
    [ -e "$f" ] || { echo "File not found: $f"; continue; }
    echo "  Processing file: $f"

    EXT_PARAMS=""
    TLA_PARAMS=""
    base="$(basename "$f")"
    if [[ $base == tla.* ]] ; then
      TLA_PARAMS="--tla-str=var1=test --tla-code=var2={x:1,y:2}"
    elif [[ $f == *test_suite/* ]] && [[ $f != *go_test_suite/* ]] && [[ $f != *new_test_suite/* ]]; then
      EXT_PARAMS="--ext-str=var1=test --ext-code=var2={x:1,y:2}"
    fi

    java $JAVA_OPTS -jar "$SJSONNET" $PARAMS $EXT_PARAMS $TLA_PARAMS "$f" > "$f.golden" 2>&1 || true
  done
  popd || exit 1
  echo "Done refreshing specified golden outputs."
  exit 0
fi

# Process test_suite
suite="$SCRIPT_DIR/test_suite"
pushd "$suite" || exit 1
echo "Refreshing golden outputs for suite: test_suite"
for f in *.jsonnet; do
  [ -e "$f" ] || continue

  echo "  Processing file: $f"

  EXT_PARAMS=""
  TLA_PARAMS=""
  if [[ $f == tla.* ]] ; then
    TLA_PARAMS="--tla-str=var1=test --tla-code=var2={x:1,y:2}"
  else
    EXT_PARAMS="--ext-str=var1=test --ext-code=var2={x:1,y:2}"
  fi

  java $JAVA_OPTS -jar "$SJSONNET" $PARAMS $EXT_PARAMS $TLA_PARAMS "$f" > "$f.golden" 2>&1 || true
done
popd || exit 1

# Process go_test_suite
suite="$SCRIPT_DIR/go_test_suite"
pushd "$suite" || exit 1
echo "Refreshing golden outputs for suite: go_test_suite"
for f in *.jsonnet; do
  [ -e "$f" ] || continue

  echo "  Processing file: $f"
  java $JAVA_OPTS -jar "$SJSONNET" $PARAMS "$f" > "$f.golden" 2>&1 || true
done
popd || exit 1

# Process new_test_suite
suite="$SCRIPT_DIR/new_test_suite"
pushd "$suite" || exit 1
echo "Refreshing golden outputs for suite: new_test_suite"
for f in *.jsonnet; do
  [ -e "$f" ] || continue

  echo "  Processing file: $f"
  java $JAVA_OPTS -jar "$SJSONNET" $PARAMS "$f" > "$f.golden" 2>&1 || true
done
popd || exit 1

popd || exit 1

echo "Done refreshing golden outputs for test_suite, go_test_suite, and new_test_suite."
