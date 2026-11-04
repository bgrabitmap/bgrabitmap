# Exact glyph outlines on Windows

On Windows with the LCL, `TBGRASystemFontRenderer.GetTextOutline` gives the exact
outline of a simple character (see `BGRAWin32Text`), obtained with the GDI function
`GetGlyphOutline` on a copy of the font that is much larger, without hinting.
`TBGRAVectorizedFont` uses it when it is available, like on GTK2 and GTK3, and
vectorizes a bitmap of the glyph otherwise: for several characters, characters that
need shaping (combining marks, right-to-left and Indic scripts, controls), characters
outside of the Basic Multilingual Plane, and characters that are not in the font
(`GetGlyphOutline` does not do font fallback, which the vectorization of a bitmap does).

`test_win32outline.lpr` checks, for several fonts and styles, that the outline has the
same amount of ink and the same position as the text drawn by GDI, that blank characters
give an empty outline, and that the characters that are not handled are refused and leave
the path unchanged. It is skipped on other platforms.

Compile with Lazarus (`lazbuild test_win32outline.lpi`, the package BGRABitmapPack must be
known to Lazarus) and run the resulting program from a console.

A related test is `../pathglyph`: the glyphs with an exact outline are filled with the
winding rule, because the contours of some fonts overlap (for example Segoe UI), which
the alternate rule, the default one of the canvas, would leave empty.
