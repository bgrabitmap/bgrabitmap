# Changelog

## 11.6.9

Changes since 11.6.8:

- Fix invalid PNG IHDR padding when BGRABitmap registers its PNG writer globally (#359, #360).
- Detect PNG/APNG capabilities instead of relying on the FPC development version number (#348, #349).
- Preserve the stream prefix when saving images at a nonzero position, and add an opt-in non-truncating save overload (#279, #351).
- Restore configured compiler optimizations after targeted workarounds and keep those workarounds around complete methods (#314, #352).
- Apply the GTK2 drawable offset when drawing BGRA bitmaps (#354).
- Fix GTK3 alpha composition and premultiplied pixel conversion (#355).
- Add ActualDOMTag and fix SVG polyline/polygon construction, default open polylines, and SVG closure placement (#357).
- Fix Windows CI downloads and build failure reporting (#350), and reuse up-to-date packages when compiling CI examples (#353).
