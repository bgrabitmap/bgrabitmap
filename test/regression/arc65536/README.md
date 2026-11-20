# ComputeArc65536 with an origin and two axis points

`ComputeArc65536(AOrigin, AXAxis, AYAxis, start65536, end65536, quality)` converts
its angles, given in 65536th of a turn, to radians before calling `ComputeArcRad`.
The conversion used `Pi/326768` instead of `Pi/32768`, so the arc covered about a
tenth of the requested angle (9 degrees instead of 90 for 16384). The version with
a center and a radius was not affected and is the one used internally.

`test_arc65536.lpr` checks the first and last points, that the arc passes in the middle and that every point
is on the circle, for a quarter turn, a half turn around another origin, and an arc
that goes through angle zero. No GUI is required.

Compile with `-dBGRABITMAP_CORE -Fu../../../bgrabitmap` and use separate build
directories for units and executables (`-FU` and `-FE`). Run the resulting program.
