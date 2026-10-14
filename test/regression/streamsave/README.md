# Saving images at a stream offset (#279)

`test_streamsave.lpr` checks that PNG and BMP saving preserves an existing
prefix, leaves the position immediately after the encoded image, and produces
the same bytes as a standalone image. It tests memory and file streams, and
also reads the embedded PNG back and checks its pixels. No GUI is required.

Compile with `-dBGRABITMAP_CORE -Fu../../../bgrabitmap` and use separate build
directories for units and executables (`-FU` and `-FE`). Run the resulting program.
