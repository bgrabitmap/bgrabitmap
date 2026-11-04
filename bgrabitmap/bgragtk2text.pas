// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Text rendering for GTK2 without going through a LCL TBitmap.

  The LCL GTK2 widgetset represents a font as a PangoLayout. The same layout
  is used here to render the text with Pango/Cairo directly into the memory
  of a BGRA bitmap, which avoids creating a TBitmap, the round trips with
  the X server and the conversion of the result.

  This unit should NOT be added to the **uses** clause of applications. }
unit BGRAGtk2Text;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

{$IFDEF LCLgtk2}
uses
  Types, Graphics, BGRABitmapTypes;

{ Draws white text on the given black opaque bitmap, with the font height
  multiplied by AFontHeightFactor, the top-left of the text being at ACoord.
  The text is rotated around this point by AOrientationTenthDeg (counter-clockwise).
  Returns false if this kind of text is not handled, in which case the bitmap
  is left unchanged and the caller must use the generic way. }
function BGRAGtk2DrawText(AFont: TFont; AFontHeightFactor: integer;
  const AText: string; ACoord: TPoint; ADest: TBGRACustomBitmap;
  AOrientationTenthDeg: integer = 0): boolean;
{$ENDIF}

implementation

{$IFDEF LCLgtk2}
uses
  SysUtils, glib2, gtk2def, pango, cairo, pangocairo, BGRAClasses;

const
  PangoLibName = {$IFDEF WINDOWS}'libpango-1.0-0.dll'{$ELSE}'libpango-1.0.so.0'{$ENDIF};

// not declared in the Pango bindings of all FPC versions
function pango_find_base_dir_(text: PChar; length: longint): TPangoDirection;
  cdecl; external PangoLibName name 'pango_find_base_dir';

var
  WorkFont: TFont;
  WorkFontLock: TRTLCriticalSection;

function BGRAGtk2DrawText(AFont: TFont; AFontHeightFactor: integer;
  const AText: string; ACoord: TPoint; ADest: TBGRACustomBitmap;
  AOrientationTenthDeg: integer = 0): boolean;
var
  gdiFont: PGdiObject;
  lcllayout, layout: PPangoLayout;
  surface: Pcairo_surface_t;
  cr: Pcairo_t;
  options: Pcairo_font_options_t;
  resolution: double;
begin
  result := false;
  if (ADest = nil) or (ADest.Width = 0) or (ADest.Height = 0) or (ADest.Data = nil) or
     (ADest.LineOrder <> riloTopToBottom) or (AText = '') then exit;
  // multiple lines are handled by the LCL
  if (pos(#10, AText) <> 0) or (pos(#13, AText) <> 0) then exit;

  EnterCriticalSection(WorkFontLock);
  try
    if WorkFont = nil then WorkFont := TFont.Create;
    WorkFont.Assign(AFont);
    WorkFont.Orientation := 0;
    WorkFont.Height := AFont.Height * AFontHeightFactor;
    gdiFont := PGdiObject(WorkFont.Handle);
    if gdiFont = nil then exit;
    lcllayout := gdiFont^.GDIFontObject;
    if lcllayout = nil then exit;

    surface := cairo_image_surface_create_for_data(PByte(ADest.Data),
      CAIRO_FORMAT_ARGB32, ADest.Width, ADest.Height, ADest.Width * sizeof(TBGRAPixel));
    try
      cr := cairo_create(surface);
      try
        // A private layout is used, because the context of the LCL layout is
        // shared with other fonts and must not be modified.
        layout := pango_cairo_create_layout(cr);
        try
          // same context settings as the LCL layout
          resolution := pango_cairo_context_get_resolution(pango_layout_get_context(lcllayout));
          if resolution > 0 then
            pango_cairo_context_set_resolution(pango_layout_get_context(layout), resolution);
          options := pango_cairo_context_get_font_options(pango_layout_get_context(lcllayout));
          if options <> nil then
            pango_cairo_context_set_font_options(pango_layout_get_context(layout), options);
          // same font settings as the LCL layout
          pango_layout_set_font_description(layout, pango_layout_get_font_description(lcllayout));
          pango_layout_set_attributes(layout, pango_layout_get_attributes(lcllayout));
          pango_layout_set_single_paragraph_mode(layout, true);
          pango_layout_set_width(layout, -1);
          pango_layout_set_alignment(layout, PANGO_ALIGN_LEFT);
          pango_layout_set_text(layout, PChar(AText), Length(AText));
          pango_context_set_base_dir(pango_layout_get_context(layout),
            pango_find_base_dir_(PChar(AText), Length(AText)));

          cairo_set_source_rgb(cr, 1, 1, 1);
          pango_cairo_update_layout(cr, layout);
          if AOrientationTenthDeg <> 0 then
          begin
            cairo_translate(cr, ACoord.X, ACoord.Y);
            cairo_rotate(cr, -AOrientationTenthDeg*Pi/1800);
            cairo_move_to(cr, 0, 0);
          end else
            cairo_move_to(cr, ACoord.X, ACoord.Y);
          pango_cairo_show_layout(cr, layout);
        finally
          g_object_unref(layout);
        end;
      finally
        cairo_destroy(cr);
      end;
      cairo_surface_flush(surface);
    finally
      cairo_surface_destroy(surface);
    end;
    ADest.InvalidateBitmap;
    result := true;
  finally
    LeaveCriticalSection(WorkFontLock);
  end;
end;

initialization
  InitCriticalSection(WorkFontLock);

finalization
  FreeAndNil(WorkFont);
  DoneCriticalSection(WorkFontLock);
{$ENDIF}

end.
