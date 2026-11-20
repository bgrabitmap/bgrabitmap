# Circular-dependency PPU regression

Run with a configured FPC installation, including RTL and FCL:

```sh
FPC=/path/to/compiler bash run.sh -Twin32
```

Additional compiler options, for example `-n` and explicit RTL/FCL unit paths,
can follow the script name. Do not include a BGRABitmap source or unit search
path in these additional options: the consumer must only see the freshly
compiled units. Also ensure the compiler configuration does not add such paths.

The script compiles BGRABitmapTypes, BGRAUnits and the 3D units into a fresh
temporary directory, then compiles a consumer without access to their sources.
`-Cn` skips linking; no executable is run. Existing build outputs are not used.

Expected: both compilations succeed. With the original i386 assembler routine
in BGRABitmapTypes, the consumer can instead fail with `checksum changed` and
`Can't find unit BGRAUnits used by BGRABitmapTypes` (FPC issue #41984,
BGRABitmap issue #358). Including the 3D units checks that relocating the
assembler routine does not simply move the PPU failure to those units.

This test checks unit compilation and loading, not rendering or performance.
