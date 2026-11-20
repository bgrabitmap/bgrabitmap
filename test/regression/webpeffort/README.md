# WebP encoding effort

Requires FPC, BGRABitmap and a libwebp encoder with ABI major 2 and the advanced
encoding API (tested with libwebp 1.6.0). No GUI or image files are used.

From the repository root:

```sh
mkdir -p test/regression/webpeffort/lib/native
fpc -dBGRABITMAP_CORE -Fubgrabitmap \
  -FUtest/regression/webpeffort/lib/native \
  -FEtest/regression/webpeffort/lib/native \
  test/regression/webpeffort/test_effort.lpr
test/regression/webpeffort/lib/native/test_effort
```

An optional first argument selects an explicit library path. On Windows, run
`test_effort.exe`. Exit status is nonzero on failure.

The test performs actual WebP encoding/decoding at all effort levels in both
modes, with mixed alpha values and top-down/bottom-up source storage. It checks
lossless visible pixels, exact alpha, source preservation, default lossy output
byte-for-byte against the old simple API at qualities 0, 60 and 100, subclass
configuration (including transparent RGB preservation and the old lossless
4/70 settings), invalid effort/configuration and output-stream failure recovery.
It does not assert monotonic file sizes or timing, which depend on image data.

## Optional C ABI comparison

With matching libwebp development headers and a C compiler, build `abi_layout.c`
and `abi_layout.lpr`, run each and compare their text output. Sizes and selected
field offsets must match. For example:

```sh
cc test/regression/webpeffort/abi_layout.c -o abi_c
fpc -Fubgrabitmap -FUtest/regression/webpeffort/lib/native \
  -FEtest/regression/webpeffort/lib/native \
  test/regression/webpeffort/abi_layout.lpr
./abi_c > abi-c.txt
test/regression/webpeffort/lib/native/abi_layout > abi-pascal.txt
diff abi-c.txt abi-pascal.txt
```

This verifies the C boundary separately from the image round trips; it is
particularly useful on a different pointer size or platform.

## API example

```pascal
Writer.Lossless := False;
Writer.QualityPercent := 60;
Writer.CompressionEffort := 0; // fastest method
Bitmap.SaveToStream(Stream, Writer);
```

`CompressionEffort` accepts 0..9 and defaults to 6. Lossy encoding maps it to
method 0..6 with `(Effort*2+1) div 3`, keeping quality independent. Lossless
encoding uses `WebPConfigLosslessPreset`: default 6 means method 4 / quality 75
instead of the old simple API's method 4 / quality 70. Lossless output can thus
change in size/bytes. Invisible RGB is still discarded by default (`exact=0`).

For precise settings, derive `TBGRAWriterWebP` and override its protected hook:

```pascal
procedure TMyWebPWriter.ConfigureEncoder(var AConfig: TWebPConfig);
begin
  inherited ConfigureEncoder(AConfig);
  AConfig.method := 4;
  if AConfig.lossless <> 0 then AConfig.quality := 70;
  AConfig.exact := 1; // preserve RGB even where alpha is zero
end;
```

Add `libwebp` to the unit's uses clause for `TWebPConfig`. The hook receives an
initialized configuration; the resulting values are validated before encoding.
Setting `exact` to 0 with method 4 / lossless quality 70 recovers the old lossless
configuration. Older ABI-incompatible libraries fail with an explicit error;
the Pascal encoder records now describe the modern C ABI, not the obsolete
major-0 encoder layout. Direct low-level clients must recompile with these
records and the corrected `size_t` callback/result signatures.
