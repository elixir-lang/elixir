#!/usr/bin/env bash
# Unless explicitly stated otherwise all files in this repository are licensed under the MIT License.
# This product includes software developed at Datadog (https://www.datadoghq.com/) Copyright 2026-Present Datadog, Inc.

# run.sh <build-dir> <seed> <seconds> <out-dir>: one search process (3 Elixir worker VMs).
# The environment (ERCOVER_DIR for coverage runs) is passed down to every VM.
set -euo pipefail
B=$(realpath "$1"); SEED=$2; SECS=$3; OUT=$4
mkdir -p "$OUT"
exec "$B/harness/elixir-fuzz" --build "$B" --seed "$SEED" --secs "$SECS" --out "$OUT" --workers "${FUZZ_WORKERS:-3}"
