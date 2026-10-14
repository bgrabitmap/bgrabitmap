# PNG / APNG regression

`test_apng.lpr` checks APNG chunk mapping and a two-frame write/read round trip,
including pixels and loop count. It requires no GUI.

Compile with `-dBGRABITMAP_CORE -Fu../../../bgrabitmap` and put compiled units
and the executable in a separate build directory (`-FU` / `-FE`).

Run with both an FPC `PNGComn` that lacks native APNG chunks and one that provides
them. The `3.3.1` version number alone does not identify these capabilities (#348).
