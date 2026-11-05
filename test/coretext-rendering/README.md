# Cocoa system text checks

Native offscreen tests for the CoreText route in BGRAText. They require the
Cocoa LCL widgetset and the Apple frameworks; they do not automate GUI input.
Create an output directory, build each `.lpi` with `lazbuild --ws=cocoa`, then
run the executable with the absolute output directory as its first argument.

- `check_coretext`: compares public `TBGRABitmap.TextOut` against an independent
  CoreGraphics/CoreText AA render, on opaque and transparent backgrounds, with
  opaque and translucent colors (six sizes). Transparent references compare
  premultiplied components. Allows two levels of integer blend rounding.
  Also checks empty/transparent no-ops and fractional negative cropping.
- `check_layout`: three fonts, regular/bold/italic, six sizes and all five font
  qualities. Checks coherent native metrics, solid textures, ClipRect,
  fractional negative cropping, center/right alignment and fitting with
  ligatures, combining characters and a supplementary-plane character.
- `check_mask_clip`: checks RGB/BGR subpixel mask composition independently of
  fonts, against cropping a full render, including one-pixel clipping rectangles
  and offsets in thirds of a pixel. It can also be built with another LCL
  widgetset. No output directory is needed.

The CoreText route uses the LCL-selected native font at its requested size.
Gray AA uses the existing linear blend to preserve native coverage contrast.
Fine RGB/BGR uses a horizontally scaled 3x1 mask and the BGRA subpixel composer.
There is no text/glyph cache, coverage normalization, or additional vertical
filter. All five qualities measure at native size on this route; the two
system qualities use gray AA on modern macOS (no native LCD subpixel mask).

Rotation, tabs, multiline low-level calls, prefix decorations, underline,
strikeout and explicitly non-antialiased fonts retain the LCL fallback.
Ordinary multiline layout is still performed by the existing BGRA routines;
its individual undecorated horizontal lines can use CoreText.
These tests do not establish identical rendering on other operating systems,
macOS versions, fonts or device scales.

The two Cocoa-only harnesses compile to an explicit SKIP program on other
widgetsets. The Objective-C mode switch and Apple framework imports are guarded
by LCLCOCOA. The mask clipping test remains enabled on all widgetsets.
