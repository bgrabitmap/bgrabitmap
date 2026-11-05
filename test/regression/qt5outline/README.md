# Exact glyph outlines on Qt5

With the LCL Qt5 widgetset, `TBGRASystemFontRenderer.GetTextOutline` gives the exact outline of
a single-line text (see `BGRAQt5Text`). The outline comes from `QPainterPath::addText`, with the
font of the LCL, so it uses the same text engine as the one that draws the text of bitmaps
(kerning, shaping, font fallback). Lines and cubic curves are kept. `TBGRAVectorizedFont` uses it
when it is available, like on GTK2, GTK3 and Windows, and vectorizes a bitmap of the glyph otherwise.

The elements of the path are read with `QPainterPath_elementPositionAt` and
`QPainterPath_elementTypeAt` of libQt5Pas when they exist (requested in
https://gitlab.com/freepascal.org/lazarus/lazarus/-/work_items/42704). With a version of
libQt5Pas that does not have them, `QPainterPath::elementAt` is resolved in `libQt5Gui.so.5`
(Linux only). If none is available, the text is refused and the bitmap is vectorized as before.

`test_qt5outline.lpr` checks, for several fonts, styles and scripts, that the outline has the same
amount of ink and covers the same pixels as the text drawn by Qt, and that the texts that are
refused (empty, with a line break) leave the path unchanged. It is skipped when the program is
not compiled for Qt5. It is not run by the CI, which has no Qt5 widgetset: it is meant to be run by hand.

Compile with Lazarus using the Qt5 widgetset (`lazbuild --ws=qt5`, the package BGRABitmapPack must be
known to Lazarus) and run the resulting program from a console, with a display (Xvfb is enough).
