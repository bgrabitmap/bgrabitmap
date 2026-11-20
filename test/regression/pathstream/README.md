# Reversing and saving TBGRAPath

`TBGRAPath.copyToReversed(dest)` copies a path while traversing each sub-path in
the opposite direction. Lines, quadratic and cubic curves and arcs are reversed
exactly (control points are swapped, arcs get their angles swapped and the
direction flipped). Closed sub-paths stay closed. Splines are converted to
lines. The order of the sub-paths is kept. This is useful when a shape is drawn
with a mirror transformation: the outline keeps its original orientation, so
holes still cancel the outer contour when several shapes overlap.

`TBGRAPath.SaveToStream` and `LoadFromStream` write and read a path in a
portable binary format: one byte for the type of each element followed by its
data, all numbers little endian, terminated by a zero byte. The format does not
depend on the pointer size. New kinds of elements can be added at the end of
`TBGRAPathElementType` without breaking existing data. The current
transformation is not saved, the coordinates are written as stored.

`test_pathstream.lpr` checks that reversing twice gives back the original path,
that the orientation of a sub-path is inverted while the rendering (including a
hole) is identical, the exact bytes written for a simple path, the round trip
of every kind of element, and that invalid or truncated data raises an
exception. No GUI is required.

Compile with `-dBGRABITMAP_CORE -Fu../../../bgrabitmap` and use separate build
directories for units and executables (`-FU` and `-FE`). Run the resulting program.
