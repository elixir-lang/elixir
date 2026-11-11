#!/usr/bin/env bash
# Unless explicitly stated otherwise all files in this repository are licensed under the MIT License.
# This product includes software developed at Datadog (https://www.datadoghq.com/) Copyright 2026-Present Datadog, Inc.

# build.sh <src-dir> <build-dir> [coverage]
# Copies the Elixir tree into <build-dir>/elixir (Elixir has no out-of-tree build), runs
# `make compile` there, builds the Rust driver into <build-dir>/cargo and copies the
# Elixir worker script next to it. Incremental: rsync keeps mtimes, make and cargo are
# incremental. `coverage` additionally writes <build-dir>/coverage.json (ercover kind:
# the worker cover-compiles Elixir.Module.Types* at load time when ERCOVER_DIR is set).
set -euo pipefail
SRC=$(realpath "$1")
mkdir -p "$2"
BUILD=$(realpath "$2")
MODE=${3:-}
HERE=$(dirname "$(realpath "$0")")
# OTP_BIN (optional): the Erlang/OTP bin directory to build and run with; otherwise erl from PATH.
[ -n "${OTP_BIN:-}" ] && export PATH="$OTP_BIN:$PATH"
OTP=$(dirname "$(command -v erl)")

mkdir -p "$BUILD/elixir"
# no --delete: make's outputs (ebin, generated parser) live in the copy
rsync -a --exclude .git "$SRC/" "$BUILD/elixir/"
(cd "$BUILD/elixir" && make compile > "$BUILD/make.log" 2>&1) || { tail -30 "$BUILD/make.log"; exit 1; }
"$BUILD/elixir/bin/elixir" --version | tail -1

# Rust driver
export CARGO_TARGET_DIR="$BUILD/cargo"
cargo build --release --manifest-path "$HERE/driver/Cargo.toml" > "$BUILD/cargo.log" 2>&1 \
  || { tail -30 "$BUILD/cargo.log"; exit 1; }
mkdir -p "$BUILD/harness"
cp "$CARGO_TARGET_DIR/release/elixir-fuzz" "$BUILD/harness/elixir-fuzz.tmp"
mv -f "$BUILD/harness/elixir-fuzz.tmp" "$BUILD/harness/elixir-fuzz"  # replace, even while a run uses it
cp "$HERE/worker.exs" "$BUILD/harness/worker.exs"

if [ "$MODE" = coverage ]; then
  cat > "$BUILD/coverage.json" <<EOF
{"kind": "ercover", "erl": "$OTP/erl", "ebin": ["$BUILD/elixir/lib/elixir/ebin"], "prefixes": ["Elixir.Module.Types"]}
EOF
fi
echo "build ok: $BUILD"
