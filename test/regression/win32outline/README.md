# Exact glyph outlines on Windows

On Windows with the LCL, `TBGRASystemFontRenderer.GetTextOutline` gives the exact
outline of a single-line text (see `BGRAWin32Text`). The text is drawn with `DrawText`
between `BeginPath` and `EndPath`, which records its outline instead of drawing it, and
the outline is read with `GetPath`. The text is drawn with a copy of the font that is much
larger, because the coordinates of the path are integers, and scaled back. It is the same
text engine as the one that draws the text of the bitmaps: the kerning, the shaping of the
script (ligatures, combining marks, joining of arabic letters, right-to-left text) and the
font fallback of GDI are the same, and lines and curves are kept as Bezier curves.
`TBGRAVectorizedFont` uses it when it is available, like on GTK2 and GTK3, and vectorizes a
bitmap of the glyph otherwise (for example with a bitmap font, which GDI cannot draw as
an outline).

`test_win32outline.lpr` checks, for several fonts, styles and scripts (letters, words, pairs
of letters with kerning, `fi`, a letter with a combining accent, Hebrew, Arabic, ideographs
that need the font fallback), that the outline has the same amount of ink and covers the same
pixels as the text drawn by GDI, that blank texts give an empty outline, and that the
texts that are refused (empty, with a line break) leave the path unchanged. It is skipped on
other platforms.

Compile with Lazarus (`lazbuild test_win32outline.lpi`, the package BGRABitmapPack must be
known to Lazarus) and run the resulting program from a console.

A related test is `../pathglyph`: the glyphs with an exact outline are filled with the
winding rule, because the contours of some fonts overlap (for example Segoe UI), which
the alternate rule, the default one of the canvas, would leave empty.
