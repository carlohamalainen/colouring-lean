#!/bin/sh

set -eu
cd "$(dirname "$0")/.."
mkdir -p build
docker build --platform linux/amd64 -t colouring-lean-build -f deploy/Dockerfile.build .
docker run --rm --platform linux/amd64 \
  -v "$PWD":/src:ro \
  -v colouring-lean-work:/work \
  -v "$PWD/build":/out \
  colouring-lean-build sh -eu -c '
    cp /src/Main.lean /src/lakefile.toml /src/lake-manifest.json /src/lean-toolchain /src/index.html .
    mkdir -p static && cp /src/static/datastar.js static/
    # Lake does not notice changes to files pulled in with include_str, so always rebuild Main.
    rm -f .lake/build/lib/lean/Main.* .lake/build/ir/Main.*
    lake build
    strip -o /out/colouring-linux-amd64 .lake/build/bin/colouring
    echo "--- linked against:"; ldd /out/colouring-linux-amd64
  '
ls -lh build/colouring-linux-amd64
