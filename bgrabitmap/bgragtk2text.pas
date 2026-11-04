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

{ Computes the size of a single-line text with the same font settings as
  BGRAGtk2DrawText. Returns false if this kind of text is not handled. }
function BGRAGtk2TextSize(AFont: TFont; AFontHeightFactor: integer;
  const AText: string; out ASize: TSize): boolean;
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
  MeasureSurface: Pcairo_surface_t;
  MeasureCr: Pcairo_t;
  SharedLayout: PPangoLayout;
  // settings of the text prepared in the shared layout, to avoid preparing it
  // again when the text is drawn just after being measured
  LastName: string;
  LastHeight: integer;
  LastStyle: TFontStyles;
  LastQuality: TFontQuality;
  LastText: string;
  LastValid: boolean;

{ Prepares a private layout, copying the settings of the layout of the LCL font.
  A private layout is used, because the context of the LCL layout is shared
  with other fonts and must not be modified. The same layout is reused for
  each call, because creating it is expensive. It must not be freed by the
  caller. The lock must be held. }
function CreatePrivateLayout(cr: Pcairo_t; AFont: TFont;
  AFontHeightFactor: integer; const AText: string): PPangoLayout;
var
  gdiFont: PGdiObject;
  lcllayout: PPangoLayout;
  options: Pcairo_font_options_t;
  resolution: double;
begin
  result := nil;
  if LastValid and (SharedLayout <> nil) and (AText = LastText) and
     (AFont.Name = LastName) and (AFont.Height * AFontHeightFactor = LastHeight) and
     (AFont.Style = LastStyle) and (AFont.Quality = LastQuality) then
  begin
    result := SharedLayout;
    exit;
  end;
  LastValid := false;
  if WorkFont = nil then WorkFont := TFont.Create;
  WorkFont.Assign(AFont);
  WorkFont.Orientation := 0;
  WorkFont.Height := AFont.Height * AFontHeightFactor;
  gdiFont := PGdiObject(WorkFont.Handle);
  if gdiFont = nil then exit;
  lcllayout := gdiFont^.GDIFontObject;
  if lcllayout = nil then exit;

  if SharedLayout = nil then SharedLayout := pango_cairo_create_layout(cr);
  result := SharedLayout;
  // same context settings as the LCL layout
  resolution := pango_cairo_context_get_resolution(pango_layout_get_context(lcllayout));
  if resolution > 0 then
    pango_cairo_context_set_resolution(pango_layout_get_context(result), resolution);
  options := pango_cairo_context_get_font_options(pango_layout_get_context(lcllayout));
  if options <> nil then
    pango_cairo_context_set_font_options(pango_layout_get_context(result), options);
  // same font settings as the LCL layout
  pango_layout_set_font_description(result, pango_layout_get_font_description(lcllayout));
  pango_layout_set_attributes(result, pango_layout_get_attributes(lcllayout));
  pango_layout_set_single_paragraph_mode(result, true);
  pango_layout_set_width(result, -1);
  pango_layout_set_alignment(result, PANGO_ALIGN_LEFT);
  pango_layout_set_text(result, PChar(AText), Length(AText));
  pango_context_set_base_dir(pango_layout_get_context(result),
    pango_find_base_dir_(PChar(AText), Length(AText)));
  pango_cairo_update_layout(cr, result);
  LastName := AFont.Name;
  LastHeight := AFont.Height * AFontHeightFactor;
  LastStyle := AFont.Style;
  LastQuality := AFont.Quality;
  LastText := AText;
  LastValid := true;
end;

function BGRAGtk2DrawText(AFont: TFont; AFontHeightFactor: integer;
  const AText: string; ACoord: TPoint; ADest: TBGRACustomBitmap;
  AOrientationTenthDeg: integer = 0): boolean;
var
  layout: PPangoLayout;
  surface: Pcairo_surface_t;
  cr: Pcairo_t;
begin
  result := false;
  if (ADest = nil) or (ADest.Width = 0) or (ADest.Height = 0) or (ADest.Data = nil) or
     (ADest.LineOrder <> riloTopToBottom) or (AText = '') then exit;
  // multiple lines are handled by the LCL
  if (pos(#10, AText) <> 0) or (pos(#13, AText) <> 0) then exit;

  EnterCriticalSection(WorkFontLock);
  try
    surface := cairo_image_surface_create_for_data(PByte(ADest.Data),
      CAIRO_FORMAT_ARGB32, ADest.Width, ADest.Height, ADest.Width * sizeof(TBGRAPixel));
    try
      cr := cairo_create(surface);
      try
        layout := CreatePrivateLayout(cr, AFont, AFontHeightFactor, AText);
        if layout = nil then exit;
        cairo_set_source_rgb(cr, 1, 1, 1);
        if AOrientationTenthDeg <> 0 then
        begin
          cairo_translate(cr, ACoord.X, ACoord.Y);
          cairo_rotate(cr, -AOrientationTenthDeg*Pi/1800);
          cairo_move_to(cr, 0, 0);
        end else
          cairo_move_to(cr, ACoord.X, ACoord.Y);
        pango_cairo_show_layout(cr, layout);
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

function BGRAGtk2TextSize(AFont: TFont; AFontHeightFactor: integer;
  const AText: string; out ASize: TSize): boolean;
var
  layout: PPangoLayout;
  w, h: longint;
begin
  result := false;
  ASize := Size(0, 0);
  if (pos(#10, AText) <> 0) or (pos(#13, AText) <> 0) then exit;
  if AText = '' then exit(true);

  EnterCriticalSection(WorkFontLock);
  try
    if MeasureCr = nil then
    begin
      MeasureSurface := cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 1, 1);
      MeasureCr := cairo_create(MeasureSurface);
    end;
    layout := CreatePrivateLayout(MeasureCr, AFont, AFontHeightFactor, AText);
    if layout = nil then exit;
    pango_layout_get_pixel_size(layout, @w, @h);
    ASize := Size(w, h);
    result := true;
  finally
    LeaveCriticalSection(WorkFontLock);
  end;
end;

initialization
  InitCriticalSection(WorkFontLock);

finalization
  if SharedLayout <> nil then g_object_unref(SharedLayout);
  if MeasureCr <> nil then cairo_destroy(MeasureCr);
  if MeasureSurface <> nil then cairo_surface_destroy(MeasureSurface);
  FreeAndNil(WorkFont);
  DoneCriticalSection(WorkFontLock);
{$ENDIF}

end.
