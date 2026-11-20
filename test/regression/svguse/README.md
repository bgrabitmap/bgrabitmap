# SVG use, geometry and line gradients

Build with FPC 3.2.2 or Lazarus. The test renders real SVG documents into
BGRABitmap images; it does not extract or simulate the SVG implementation.
No LCL windows or native input are needed.

From the repository root (create the output directory first):

```sh
mkdir -p test/regression/svguse/lib/linux
fpc -dBGRABITMAP_CORE -Fubgrabitmap \
  -FUtest/regression/svguse/lib/linux -FEtest/regression/svguse/lib/linux \
  test/regression/svguse/test_svguse.lpr
timeout 30 test/regression/svguse/lib/linux/test_svguse
```

On Windows use a separate output directory (for example `lib/windows`) and
run `test_svguse.exe`. Alternatively, build `test_svguse.lpi`; its output paths
separate the target OS. Exit status is nonzero on failure.

The 100 checks cover:

- Local `href` and `xlink:href` references, translation, independent instances,
  inherited fill and explicit styles on the referenced shape.
- Nested references, referenced groups, group/use transforms, missing IDs,
  self-reference, indirect cycles and ancestor cycles. Repeated draws check
  that temporary state and the canvas transform are restored.
- SVG 2 `href` precedence when both link attributes are present, including an
  explicitly empty `href` (see [SVG linking](https://www.w3.org/TR/SVG2/linking.html)).
- Bounding boxes for rectangles, circles, ellipses, lines, polygons, paths,
  translated references and cyclic references.
- Diagonal object-bounding-box gradients and horizontal/vertical lines with
  user-space gradients.
- The pre-existing three-argument `TSVGContent` constructor.

Also run `test/svg_tags/test_tags.lpr` to retain the newer polygon/polyline
constructor and ActualDOMTag regressions when integrating the old SVG branch.

This integration does not claim complete SVG/SVG 2 support. In particular,
`symbol`/referenced viewport sizing and group bounding boxes are not implemented
by this change; external-resource loading and text/font bounding boxes are not
covered by this test. Native example rendering should be checked on each target
widgetset separately. A successful core test is not a native UI validation.
