# Cursor looping over closed shapes

`TBGRAPathCursor` with `LoopClosedShapes` goes back to the start of a closed
shape when it reaches the closing element. The closing element stores the
position of the matching `moveTo` (`LoopDataPos`).

The size reserved for each kind of element (`PathElementSize`) had the entries
of `peLineTo` and `peCloseSubPath` swapped: a line reserved room for a closing
element and a closing element only had room for a point. `LoopDataPos`, the
last field of the closing element, was thus written after the reserved space
and overwritten as soon as another element was added, so the cursor jumped to
a meaningless position (empty coordinates) when a closed shape was followed by
other elements. The write could also go past the allocated memory.

`test_pathloop.lpr` moves a cursor forward and backward around a closed shape,
alone and followed by another closed shape, and checks that it stays on the
first shape. No GUI is required.

Compile with `-dBGRABITMAP_CORE -Fu../../../bgrabitmap` and use separate build
directories for units and executables (`-FU` and `-FE`). Run the resulting program.
