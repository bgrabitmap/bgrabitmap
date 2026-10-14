GTK3 alpha-composition regression test
=====================================

Build test_alpha.lpi with lazbuild --ws=gtk3. Run with a GTK display, or:

    xvfb-run -a ./test_alpha

The test draws onto a Cairo ARGB32 image surface through a GTK3 device context.
It deliberately sets CAIRO_OPERATOR_SOURCE before drawing, reproducing the
inherited operator reported for Wayland in bgrabitmap/bgracontrols#270.
It checks that bitmap drawing blends over the background, leaves surrounding
pixels intact, and restores the caller's Cairo operator.

Cases cover Draw and DataDrawTransparent, alpha 0/64/128/255, the all-opaque
fast path, negative source strides, and padded source rows. Every output pixel
is checked against the expected composition over an opaque blue background.

Validated with Lazarus 4.8, FPC 3.2.2, GTK3 under Xvfb. This deterministic test
covers the Cairo state responsible for the Wayland issue; it does not exercise
a native Wayland desktop or compositor.
Other widgetsets compile a skip-only program, so cross-platform example builds
remain supported. Lazarus 4.8 GTK3 emits a Gdk critical during widgetset
finalization (its default context requests a zero-size pixbuf); it occurs both
before and after the fix, after all pixel assertions complete.
