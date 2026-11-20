#!/usr/bin/env bash
set -eu
# FPC=/path/to/compiler bash run.sh [target and RTL/FCL compiler options]
test_dir=$(cd -- "$(dirname -- "$0")" && pwd)
source_dir=$(cd -- "$test_dir/../../../bgrabitmap" && pwd)
build=$(mktemp -d)
mkdir "$build/units" "$build/client"
compiler=${FPC:-fpc}
echo "Build directory: $build"
cd "$build"
"$compiler" -Cn -dBGRABITMAP_DONT_USE_LAZFREETYPE "$@" "-Fu$source_dir" "-FU$build/units" "$test_dir/build_units.lpr"
cp "$test_dir/consumer.lpr" "$build/client/"
cd "$build/client"
"$compiler" -Cn -dBGRABITMAP_DONT_USE_LAZFREETYPE "$@" "-Fu$build/units" "-FU$build/units" consumer.lpr
echo 'PPU-only consumer compiled successfully (including 3D units).'
