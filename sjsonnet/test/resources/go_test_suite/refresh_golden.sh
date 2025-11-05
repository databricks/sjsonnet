#!/usr/bin/env bash

# Copyright 2015 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

set -e

JAVA_OPTS="-Xss100m -Xms2g -XX:+UseG1GC"
ROOTDIR=$(git rev-parse --show-toplevel)
JSONNET_BIN="$ROOTDIR/out/sjsonnet/jvm/3.3.7/assembly.dest/out.jar"
PARAMS="--no-duplicate-keys-in-comprehension --strict-import-syntax --strict-inherited-assertions --strict-set-operations"

if [ $# -eq 0 ] ; then
    echo "Usage: $0 <filename.jsonnet>" 2>&1
    exit 1
fi

for FILE in "$@" ; do
    if [[ $FILE == *.golden ]]; then
        echo "Specified file $FILE is already golden."
        echo "Please specify the input file instead."
        exit 1
    fi

    if [ ! -r "$FILE" ] ; then
        echo "Could not read: \"$FILE\"" 2>&1
        exit 1
    fi

    JSONNET_CMD="JAVA_OPTS=\"$JAVA_OPTS\" $JSONNET_BIN $PARAMS"

    # Avoid set -e terminating us if the run fails.
    GOLDEN_FILE="${FILE%.jsonnet}.golden"
    if grep -q 'ERROR' "$GOLDEN_FILE" ; then
        eval "$JSONNET_CMD" "$FILE" > "$FILE.golden" 2>&1 || true
    fi

done


