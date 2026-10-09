# WebP dynamic loader regression

```sh
fpc -Fu../../../bgrabitmap test_webploader.lpr
./test_webploader
```

Install the platform's normal libwebp runtime. The test checks automatic
loading, repeated-load reference counting, a lossless RGBA encode/decode
round trip, unloading and an explicitly missing library path.

Linux keeps `FindLinuxLibrary`, which already resolves newer sonames from
the linker cache. BSD uses the system loader and `libwebp.so`, the alias
provided by the FreeBSD webp port. Application-local libraries remain a
fallback, including the previously used `libwebp.so.6` name.

To exercise the BSD selection logic on Linux with a real libwebp.so.7,
compile into separate unit/output directories with `-uLINUX -dBSD`.
This validates the binding and search behavior, but is not a native BSD
ABI or runtime validation. The same test can be run directly on FreeBSD.

This addresses [issue #326](https://github.com/bgrabitmap/bgrabitmap/issues/326).
