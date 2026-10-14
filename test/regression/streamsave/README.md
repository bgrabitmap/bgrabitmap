# Saving images at a stream offset (#279)

Existing calls to `SaveToStream`, `SaveToStreamAs` and `SaveToStreamAsPng` keep
their original truncating behavior. Pass `False` as the last argument to write
at the current stream position and preserve other data, for example:

```pascal
Bitmap.SaveToStreamAsPng(Stream, False);
Bitmap.SaveToStreamAs(Stream, ifBMP, False);
Bitmap.SaveToStream(Stream, Writer, False);
```

The non-owning stream adapter maps image-relative seeks to the starting position
and prevents truncation, without buffering the encoded image. It is also needed
at position zero to preserve any existing data after the image. The caller's
stream and writer remain owned by the caller.

`test_streamsave.lpr` checks default and explicit truncation modes, preservation
of prefixes and trailing data, final position, and identical PNG/BMP image bytes.
It tests memory and file streams, and reads the embedded PNG back to check pixels.
No GUI is required.

Compile with `-dBGRABITMAP_CORE -Fu../../../bgrabitmap` and use separate build
directories for units and executables (`-FU` and `-FE`). Run the resulting program.
