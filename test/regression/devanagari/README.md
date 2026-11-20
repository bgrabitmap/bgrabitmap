# Devanagari conjuncts

A consonant followed by a virama (halant, U+094D) combines with the next consonant
into a single glyph, for example `म्` + `च` gives `म्च`. Up to four consonants can be
combined this way (issue #270).

`test_devanagari.lpr` checks that `TBGRACustomTypeWriter.BrowseGlyphs` and
`TGlyphCursorUtf8` both group the consonants (and report a merged glyph so that
an editor can place the caret proportionally inside it), that a fifth consonant starts
a new group, and that consonants without virama, a trailing virama, a virama followed by a
space and Latin ligatures are unchanged. No GUI and no font are required.

Compile with `-dBGRABITMAP_CORE -Fu../../../bgrabitmap` and use separate build
directories for units and executables (`-FU` and `-FE`). Run the resulting program.
