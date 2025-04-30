#!/bin/bash
set -euo pipefail

# TODO: move that into mill.

VERSION="0.5.0"
SCALA_VERSION="3.3.5"
OS=$(uname -s)
ARCH=$(uname -m)
TMPDIR=$(mktemp -d)

echo "Building assets for Scala $SCALA_VERSION on $OS/$ARCH (for native)"

./mill "sjsonnet.js[$SCALA_VERSION].fullOpt"
./mill "sjsonnet.jvm[$SCALA_VERSION].assembly"
./mill "sjsonnet.native[$SCALA_VERSION].nativeLink"

cp -r ./out/sjsonnet/js/$SCALA_VERSION/fullOpt.dest/out.js "$TMPDIR/sjsonnet-$VERSION.js"
cp -r ./out/sjsonnet/js/$SCALA_VERSION/fullOpt.dest/out.js.map "$TMPDIR/sjsonnet-$VERSION.js.map"
cp -r ./out/sjsonnet/jvm/$SCALA_VERSION/assembly.dest/out.jar "$TMPDIR/sjsonnet-$VERSION.jar"
cp -r ./out/sjsonnet/native/$SCALA_VERSION/nativeLink.dest/out "$TMPDIR/sjsonnet-$VERSION-$OS-$ARCH"

pushd "$TMPDIR" || exit 1
sha256sum sjsonnet-* > sums.sha
popd || exit 1

echo "Assets built and checksums generated in $TMPDIR"