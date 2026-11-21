# Arc precision and full turns

This portable regression is registered in `../tests.txt` as a core test.
It needs FPC and BGRABitmap sources, with `BGRABITMAP_CORE`; no LCL, display,
filesystem fixtures or external runtime library is needed.

From the repository root:

```sh
instantfpc .github/workflows/regression.pas --portable --only=arcprecision/
```

The existing cases cover high-quality arc lengths, points on circles, ellipse
perimeters, transformed paths, and exactly computed endpoints.

The full-turn regression additionally covers 26 cases: equal angles, signed
one/two-turn differences, offset starting angles, and `ComputeArc` in both
directions. It checks circumference, points on the circle, and exact endpoints.
Six genuinely tiny nonzero arcs must still yield just two close points.

Commit 405dbd2 moved the `$ffff` reduction before the tiny-arc check in
`ComputeArcRad`. This erased the distinction between angles separated by a
whole turn and angles in the same unwrapped quantization interval. Both then
matched the tiny-arc condition, reducing full ellipses to two points.

The fix restores the previous ordering: compare unwrapped rounded angles first,
then apply the mask for point-count/sweep computation. This preserves the newer
floating-point point generation and exact endpoints, without comparing a Single
angle difference for exact equality with the mathematical constant `2*Pi`.
