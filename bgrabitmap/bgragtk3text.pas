// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Text rendering for GTK3 without going through a LCL TBitmap.

  The LCL GTK3 widgetset represents a font as a PangoLayout (TGtk3Font.Layout)
  and draws text on a Cairo context with pango_cairo_show_layout. The same
  layout is used here to render the text with Pango/Cairo directly into the
  memory of a BGRA bitmap, which avoids creating a TBitmap and converting
  the result.

  This unit should NOT be added to the **uses** clause of applications. }
unit BGRAGtk3Text;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

{$IFDEF LCLgtk3}
uses
  Types, Graphics, BGRABitmapTypes;

{ Draws white text on the given black opaque bitmap, with the font height
  multiplied by AFontHeightFactor, the top-left of the text being at ACoord.
  The text is rotated around this point by AOrientationTenthDeg (counter-clockwise).
  Returns false if this kind of text is not handled, in which case the bitmap
  is left unchanged and the caller must use the generic way. }
function BGRAPangoDrawText(AFont: TFont; AFontHeightFactor: integer;
  const AText: string; ACoord: TPoint; ADest: TBGRACustomBitmap;
  AOrientationTenthDeg: integer = 0): boolean;

{ Computes the size of a single-line text with the same font settings as
  BGRAPangoDrawText. Returns false if this kind of text is not handled. }
function BGRAPangoTextSize(AFont: TFont; AFontHeightFactor: integer;
  const AText: string; out ASize: TSize): boolean;
{$ENDIF}

implementation

{$IFDEF LCLgtk3}
uses
  SysUtils, LazPango1, LazPangoCairo1, LazCairo1, Gtk3Objects, BGRAClasses;

var
  WorkFont: TFont;
  WorkFontLock: TRTLCriticalSection;

{ Gives the layout of the LCL font, with the text set. The same layout is
  used by the LCL to draw text, and it sets the text each time. The lock
  must be held. }
function PrepareLayout(AFont: TFont; AFontHeightFactor: integer;
  const AText: string): PPangoLayout;
var
  current: PChar;
begin
  result := nil;
  if WorkFont = nil then WorkFont := TFont.Create;
  WorkFont.Assign(AFont);
  WorkFont.Orientation := 0;
  WorkFont.Height := AFont.Height * AFontHeightFactor;
  if WorkFont.Handle = 0 then exit;
  result := TGtk3Font(WorkFont.Handle).Layout;
  if result = nil then exit;
  // do not set the same text again, it would be laid out again
  current := result^.get_text;
  if (current = nil) or (StrLen(current) <> Length(AText)) or
     not CompareMem(current, PChar(AText), Length(AText)) then
    result^.set_text(PChar(AText), Length(AText));
end;

function BGRAPangoDrawText(AFont: TFont; AFontHeightFactor: integer;
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
    layout := PrepareLayout(AFont, AFontHeightFactor, AText);
    if layout = nil then exit;
    surface := cairo_image_surface_create_for_data(PByte(ADest.Data),
      CAIRO_FORMAT_ARGB32, ADest.Width, ADest.Height, ADest.Width * sizeof(TBGRAPixel));
    try
      cr := cairo_create(surface);
      try
        // same sequence as the LCL
        cairo_move_to(cr, ACoord.X, ACoord.Y);
        if AOrientationTenthDeg <> 0 then
          cairo_rotate(cr, -Pi * (AOrientationTenthDeg / 10) / 180);
        cairo_set_source_rgb(cr, 1, 1, 1);
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

function BGRAPangoTextSize(AFont: TFont; AFontHeightFactor: integer;
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
    layout := PrepareLayout(AFont, AFontHeightFactor, AText);
    if layout = nil then exit;
    layout^.get_pixel_size(@w, @h);
    ASize := Size(w, h);
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
