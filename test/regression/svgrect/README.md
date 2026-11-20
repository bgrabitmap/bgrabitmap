# SVG rectangle omitted radii regression

SVG rectangles with only `rx` or only `ry` specified must use that radius
for both axes. An explicitly specified zero must not be treated as an omitted
attribute. See [SVG 1.1 rectangle geometry](https://www.w3.org/TR/SVG11/shapes.html#RectElement).

Build from the repository root with FPC and no LCL dependency:

```sh
mkdir -p test/regression/svgrect/lib/test
fpc -B -dBGRABITMAP_CORE -Fubgrabitmap \
  -FUtest/regression/svgrect/lib/test -FEtest/regression/svgrect/lib/test \
  test/regression/svgrect/test_svgrect.lpr
```

Run `test/regression/svgrect/lib/test/test_svgrect` (add `.exe` on Windows).
The exit status is nonzero on failure. Add `-gh -gl` to enable heap tracing.

The 300 checks cover 15 cases, each drawn twice using both SVG drawing and
`CopyPathTo` followed by canvas filling:

- Only `rx`, only `ry`, both radii and neither radius.
- Explicit zero on either axis, including zero as the only radius.
- Percentage radii on a nonsquare 200x100 viewport: resolve the specified
  radius on its own axis before copying that length to the omitted radius.
- Clamping to half the rectangle width/height, after resolving the omission.
- Physical units at 96 DPI.

Each case checks exact image equality with explicit radii, filled center,
rounded/square corner, unchanged canvas transform and unchanged serialized
SVG attributes. Tests use the real renderer, not extracted implementation.
Public attribute getters and serialization retain the original attributes;
the omitted radius is resolved only when constructing the rendered path.

On Windows / FPC 3.2.3 (i386-win32), the unpatched development baseline
`4b65cf2` produces 64 failed checks; the fix passes all 300, with no unfreed
blocks reported by heaptrc. The existing SVG use suite (100 checks) and SVG
tag tests also pass. These are core renderer checks, not native UI tests.

This change targets omitted attributes. It does not claim complete SVG 2
CSS geometry or invalid-value handling.
