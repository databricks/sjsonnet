#!/usr/bin/env bash

# Sync test suites from upstream google/jsonnet and google/go-jsonnet repositories.
#
# This script copies .jsonnet source files and .golden files from upstream repos
# into sjsonnet's test resource directories. Only *.jsonnet and *.golden files are
# synced. Lint-related files (*.linter.*) and formatter-related files (*.fmt.*)
# from google/jsonnet are excluded. Files listed in per-suite .sync_ignore files
# are also excluded (one .jsonnet filename per line, comments start with '#').
# A .jsonnet file is only synced if it has a
# corresponding valid .golden file (upstream or already present locally), so that
# Scala tests always find a matching .jsonnet.golden for each .jsonnet file.
# For new .jsonnet files that have no golden file after syncing,
# golden files are generated using sjsonnet via the per-suite refresh_golden.sh scripts.
#
# Usage:
#   ./sync_test_suites.sh
#
# After syncing, run tests:
#   ./mill __.test
#
# Reference: https://github.com/CertainLach/jrsonnet/commit/36e84a6e688e8b1f6aeac1349e8772ddbceef4e3

set -euo pipefail

ROOT_DIR="$(git rev-parse --show-toplevel)"
cd "$ROOT_DIR"

# --- Configuration ---
CPP_TEST_SUITE_DIR="sjsonnet/test/resources/test_suite"
GO_TEST_SUITE_DIR="sjsonnet/test/resources/go_test_suite"

# --- Step 1: Clone upstream repositories into a temporary directory ---
echo "=== Cloning upstream repositories ==="
TEMP_DIR=$(mktemp -d)
trap 'rm -rf "$TEMP_DIR"' EXIT

echo "  Cloning google/jsonnet (depth=1)..."
git clone --depth=1 --quiet https://github.com/google/jsonnet.git "$TEMP_DIR/jsonnet"

echo "  Cloning google/go-jsonnet (depth=1)..."
git clone --depth=1 --quiet https://github.com/google/go-jsonnet.git "$TEMP_DIR/go-jsonnet"

# --- Step 2: Sync .jsonnet and .golden files (excluding lint-related golden) ---
echo ""
echo "=== Syncing test files ==="

sync_test_files() {
  local source_dir="$1"
  local target_dir="$2"
  local suite_name="$3"
  local extra_skip_pattern="${4:-}"  # Optional glob pattern to skip (e.g. "*.fmt.*")

  if [ ! -d "$source_dir" ]; then
    echo "  WARNING: Source directory not found: $source_dir"
    return
  fi

  # Load .sync_ignore file from target directory (if it exists)
  local ignore_file="$target_dir/.sync_ignore"
  local ignore_stems_file
  ignore_stems_file=$(mktemp)
  if [ -f "$ignore_file" ]; then
    # Strip comments and blank lines, extract stems (remove .jsonnet extension)
    grep -v '^\s*#' "$ignore_file" | grep -v '^\s*$' | sed 's/\.jsonnet$//' > "$ignore_stems_file"
    local ignore_count
    ignore_count=$(wc -l < "$ignore_stems_file" | tr -d ' ')
    echo "  Syncing $suite_name... ($ignore_count file(s) in .sync_ignore)"
  else
    echo "  Syncing $suite_name..."
  fi

  local before_jsonnet_count
  before_jsonnet_count=$(find "$target_dir" -maxdepth 1 -name '*.jsonnet' 2>/dev/null | wc -l | tr -d ' ')
  local before_golden_count
  before_golden_count=$(find "$target_dir" -maxdepth 1 -name '*.golden' 2>/dev/null | wc -l | tr -d ' ')

  local new_jsonnet=0
  local updated_jsonnet=0
  local new_golden=0
  local updated_golden=0
  local skipped_no_golden=0

  # --- Phase 1: Build a set of .jsonnet stems that have a valid golden file upstream ---
  # A "valid" golden file is one that is not *.linter.* and not matching extra_skip_pattern.
  # Golden files can be in two formats upstream:
  #   - a.jsonnet.golden (already in sjsonnet convention)
  #   - a.golden (needs renaming to a.jsonnet.golden)
  local golden_stems_file
  golden_stems_file=$(mktemp)

  # Check *.jsonnet.golden files (only regular files, skip directories)
  for src_file in "$source_dir"/*.jsonnet.golden; do
    [ -e "$src_file" ] || continue
    [ -d "$src_file" ] && continue
    local basename
    basename=$(basename "$src_file")
    [[ "$basename" == *.linter.* ]] && continue
    if [ -n "$extra_skip_pattern" ] && [[ "$basename" == $extra_skip_pattern ]]; then
      continue
    fi
    # Extract stem: a.jsonnet.golden -> a
    local stem="${basename%.jsonnet.golden}"
    # Skip stems listed in .sync_ignore
    if grep -Fqx "$stem" "$ignore_stems_file" 2>/dev/null; then
      continue
    fi
    echo "$stem" >> "$golden_stems_file"
  done

  # Check *.golden files (that are not *.jsonnet.golden, skip directories)
  for src_entry in "$source_dir"/*.golden; do
    [ -e "$src_entry" ] || continue
    [ -d "$src_entry" ] && continue
    local basename
    basename=$(basename "$src_entry")
    [[ "$basename" == *.jsonnet.golden ]] && continue
    [[ "$basename" == *.linter.* ]] && continue
    if [ -n "$extra_skip_pattern" ] && [[ "$basename" == $extra_skip_pattern ]]; then
      continue
    fi
    # Extract stem: a.golden -> a
    local stem="${basename%.golden}"
    # Skip stems listed in .sync_ignore
    if grep -qx "$stem" "$ignore_stems_file" 2>/dev/null; then
      continue
    fi
    # Only count if corresponding .jsonnet exists upstream
    if [ -f "$source_dir/${stem}.jsonnet" ]; then
      echo "$stem" >> "$golden_stems_file"
    fi
  done

  # Also count stems that already have golden files in the target directory (skip directories)
  for existing_golden in "$target_dir"/*.jsonnet.golden; do
    [ -e "$existing_golden" ] || continue
    [ -d "$existing_golden" ] && continue
    local basename
    basename=$(basename "$existing_golden")
    echo "${basename%.jsonnet.golden}" >> "$golden_stems_file"
  done

  # Deduplicate and sort the stems file
  if [ -f "$golden_stems_file" ]; then
    sort -u "$golden_stems_file" -o "$golden_stems_file"
  fi

  # --- Phase 2: Sync .jsonnet files (only if they have a valid golden file) ---
  for src_file in "$source_dir"/*.jsonnet; do
    [ -f "$src_file" ] || continue
    local basename
    basename=$(basename "$src_file")

    # Skip files matching extra skip pattern (e.g. *.fmt.*)
    if [ -n "$extra_skip_pattern" ] && [[ "$basename" == $extra_skip_pattern ]]; then
      continue
    fi

    # Extract stem: a.jsonnet -> a
    local stem="${basename%.jsonnet}"

    # Skip files listed in .sync_ignore
    if grep -Fqx "$stem" "$ignore_stems_file" 2>/dev/null; then
      continue
    fi

    # Skip .jsonnet files that have no valid golden file (upstream or local)
    if ! grep -Fqx "$stem" "$golden_stems_file" 2>/dev/null; then
      skipped_no_golden=$((skipped_no_golden + 1))
      continue
    fi

    local dest_file="$target_dir/$basename"

    if [ ! -f "$dest_file" ]; then
      cp "$src_file" "$dest_file"
      new_jsonnet=$((new_jsonnet + 1))
    elif ! diff -q "$src_file" "$dest_file" > /dev/null 2>&1; then
      cp "$src_file" "$dest_file"
      updated_jsonnet=$((updated_jsonnet + 1))
    fi
  done

  # --- Phase 3: Sync golden files ---
  # Never overwrite existing golden files — sjsonnet golden files use a different error
  # format than upstream C++/Go implementations, so existing files must be preserved.

  # 1) Sync *.jsonnet.golden files (already in correct naming format, skip directories)
  for src_file in "$source_dir"/*.jsonnet.golden; do
    [ -e "$src_file" ] || continue
    [ -d "$src_file" ] && continue
    local basename
    basename=$(basename "$src_file")

    # Skip lint-related golden files
    [[ "$basename" == *.linter.* ]] && continue

    # Skip files matching extra skip pattern
    if [ -n "$extra_skip_pattern" ] && [[ "$basename" == $extra_skip_pattern ]]; then
      continue
    fi

    # Skip files listed in .sync_ignore
    local stem="${basename%.jsonnet.golden}"
    if grep -Fqx "$stem" "$ignore_stems_file" 2>/dev/null; then
      continue
    fi

    local dest_file="$target_dir/$basename"

    # Only copy new golden files, never overwrite existing ones
    if [ ! -e "$dest_file" ]; then
      cp -r "$src_file" "$dest_file"
      new_golden=$((new_golden + 1))
    fi
  done

  # 2) Sync *.golden files that have a corresponding *.jsonnet,
  #    renaming a.golden -> a.jsonnet.golden (skip directories)
  for src_entry in "$source_dir"/*.golden; do
    [ -e "$src_entry" ] || continue
    [ -d "$src_entry" ] && continue
    local basename
    basename=$(basename "$src_entry")

    # Skip if already in *.jsonnet.golden format (handled above)
    [[ "$basename" == *.jsonnet.golden ]] && continue

    # Skip lint-related files
    [[ "$basename" == *.linter.* ]] && continue

    # Skip files matching extra skip pattern
    if [ -n "$extra_skip_pattern" ] && [[ "$basename" == $extra_skip_pattern ]]; then
      continue
    fi

    # Derive the corresponding .jsonnet filename: a.golden -> a.jsonnet
    local stem="${basename%.golden}"

    # Skip files listed in .sync_ignore
    if grep -Fqx "$stem" "$ignore_stems_file" 2>/dev/null; then
      continue
    fi

    local jsonnet_file="$source_dir/${stem}.jsonnet"

    # Only sync if the corresponding .jsonnet file exists upstream
    if [ -f "$jsonnet_file" ]; then
      local dest_file="$target_dir/${stem}.jsonnet.golden"

      # Only copy new golden files, never overwrite existing ones
      if [ ! -e "$dest_file" ]; then
        cp -r "$src_entry" "$dest_file"
        new_golden=$((new_golden + 1))
      fi
    fi
  done

  local after_jsonnet_count
  after_jsonnet_count=$(find "$target_dir" -maxdepth 1 -name '*.jsonnet' 2>/dev/null | wc -l | tr -d ' ')
  local after_golden_count
  after_golden_count=$(find "$target_dir" -maxdepth 1 -name '*.golden' 2>/dev/null | wc -l | tr -d ' ')

  echo "    .jsonnet: Before=$before_jsonnet_count, After=$after_jsonnet_count (New=$new_jsonnet, Updated=$updated_jsonnet, Skipped(no golden)=$skipped_no_golden)"
  echo "    .golden:  Before=$before_golden_count, After=$after_golden_count (New=$new_golden, Updated=$updated_golden)"

  rm -f "$golden_stems_file"
  rm -f "$ignore_stems_file"
}

# Sync C++ test suite (google/jsonnet test_suite -> sjsonnet test_suite)
sync_test_files "$TEMP_DIR/jsonnet/test_suite" "$CPP_TEST_SUITE_DIR" \
  "C++ test suite (google/jsonnet)" "*.fmt.*"

# Sync Go test suite (google/go-jsonnet testdata -> sjsonnet go_test_suite)
sync_test_files "$TEMP_DIR/go-jsonnet/testdata" "$GO_TEST_SUITE_DIR" \
  "Go test suite (google/go-jsonnet)"

# --- Step 3: Generate golden files for new .jsonnet files missing golden ---
echo ""
echo "=== Checking for missing golden files ==="

generate_missing_golden() {
  local suite_dir="$1"
  local suite_name="$2"
  local missing_files=()

  for jsonnet_file in "$suite_dir"/*.jsonnet; do
    [ -f "$jsonnet_file" ] || continue
    if [ ! -e "${jsonnet_file}.golden" ]; then
      missing_files+=("$jsonnet_file")
    fi
  done

  if [ ${#missing_files[@]} -eq 0 ]; then
    echo "  $suite_name: All .jsonnet files have golden files."
    return
  fi

  echo "  $suite_name: ${#missing_files[@]} files missing golden, generating with sjsonnet..."

  local jsonnet_bin="$ROOT_DIR/out/sjsonnet/jvm/3.3.7/assembly.dest/out.jar"
  if [ ! -f "$jsonnet_bin" ]; then
    echo "    WARNING: sjsonnet assembly jar not found at $jsonnet_bin"
    echo "    Build it first: ./mill 'sjsonnet.jvm[3.3.7].assembly'"
    echo "    Missing files:"
    for f in "${missing_files[@]}"; do
      echo "      $(basename "$f")"
    done
    return
  fi

  for f in "${missing_files[@]}"; do
    echo "    Generating golden for $(basename "$f")..."
    # Try stdout-only first (success case); fall back to capturing stderr (error case)
    if java -Xss100m -jar "$jsonnet_bin" "$f" > "${f}.golden" 2>/dev/null; then
      true
    else
      java -Xss100m -jar "$jsonnet_bin" "$f" > "${f}.golden" 2>&1 || true
      # Remove Java runtime warnings from golden output
      sed -i '' '/^WARNING:/d' "${f}.golden"
    fi
  done
}

generate_missing_golden "$CPP_TEST_SUITE_DIR" "C++ test suite"
generate_missing_golden "$GO_TEST_SUITE_DIR" "Go test suite"

# --- Step 4: Final summary ---
echo ""
echo "=== Sync complete ==="

final_missing=0
for suite_dir in "$CPP_TEST_SUITE_DIR" "$GO_TEST_SUITE_DIR"; do
  for jsonnet_file in "$suite_dir"/*.jsonnet; do
    [ -f "$jsonnet_file" ] || continue
    if [ ! -e "${jsonnet_file}.golden" ]; then
      echo "  STILL MISSING: ${jsonnet_file}.golden"
      final_missing=$((final_missing + 1))
    fi
  done
done

if [ "$final_missing" -gt 0 ]; then
  echo ""
  echo "WARNING: $final_missing files still missing golden files."
  echo "You may need to build sjsonnet first:"
  echo "  ./mill sjsonnet.jvm[3.3.7].assembly"
  echo "Then re-run this script."
else
  echo ""
  echo "All files have golden files. Run tests with:"
  echo "  ./mill __.test"
fi
