# Regression programs

These programs verify library behavior and terminate without interactive input.
They are not examples. Exit code **0** means success; any nonzero exit code
means failure. A test must catch expected exceptions itself and must return a
nonzero status if a check fails. Cocoa tests run offscreen, without GUI input.

The normal example build excludes this directory. Linux and Windows CI run
`.github/workflows/regression.pas` in a separate step after building examples:

```sh
instantfpc .github/workflows/regression.pas
```

Run from the repository root. `tests.txt` explicitly registers each source and
its compilation mode:

- `core`: FPC with `BGRABITMAP_CORE`, no LCL or display required.
- `webp`: the same core compilation, with a libwebp runtime required. Linux CI
  installs libwebp; on Windows the runner copies the bundled 32/64-bit DLL
  beside the executable, retaining the normal automatic-loading test. macOS
  needs an installed libwebp runtime in its standard library search path.
- `nogui`: build the adjacent `.lpi` using LCL NoGUI, then run it headlessly.
- `win32`: build the adjacent `.lpi` using Win32 and run on Windows only.
  The runner reports it as skipped on other systems.
- `cocoa`: build the adjacent `.lpi` using Cocoa, then run it on macOS only.
  The runner explicitly reports these tests as skipped on Linux/Windows.

LCL projects must place the executable beside the project, using the source
basename (and `.exe` on Windows). Core executables and units go into the build
directory. LCL tests receive an absolute writable results directory as their
first argument; core tests receive no arguments, including the `webp` test. Each program runs with its results directory as cwd,
so it must not assume that cwd contains the executable or project sources.

The runner saves `build.log`, `run.log` and generated results under
`.regression-build/<mode>/<test-name>/`, ignored by Git. CI uploads the logs.
Compilation errors, launch failures, nonzero statuses and timeouts fail the
step. Tests default to a 120-second timeout; compilation is limited to ten
minutes per program. The summary reports passed, failed and skipped tests.
An empty selection or malformed manifest is an error.

Useful local options (all paths can be absolute):

```sh
instantfpc .github/workflows/regression.pas \
  --only=coretext/ --lazbuild=/Applications/Lazarus/lazbuild \
  --pcp=/path/to/lazarus-profile

# Exercise the portable selection on a Mac as well:
instantfpc .github/workflows/regression.pas --portable --only=coretext/
```

Other options: `--root=...`, `--manifest=...`, `--build-dir=...`, `--fpc=...`,
`--timeout=seconds`. Adding a test also requires adding it to `tests.txt`.
A successful portable run does not establish native Cocoa behavior; conversely,
a Mac run does not replace native Linux/Windows CI.
