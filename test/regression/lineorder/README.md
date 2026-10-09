# Duplicate line order regression

From this directory, build and run the console test:

```sh
fpc -dBGRABITMAP_CORE -Fu../../../bgrabitmap test_lineorder.lpr
./test_lineorder
```

Use a separate `-FU` output directory when sharing the checkout between
Windows and Linux builds.

The test wraps external pixel buffers with `TBGRAPtrBitmap` in both line
orders, then compares every logical pixel after duplication and same-size
resampling, with and without copying properties. The destination keeps its
class's normal line order; its visible image must match the source.

This reproduces [issue #328](https://github.com/bgrabitmap/bgrabitmap/issues/328).
