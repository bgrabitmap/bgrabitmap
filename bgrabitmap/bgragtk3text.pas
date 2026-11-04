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
  SysUtils, Types, Graphics, BGRABitmapTypes;

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

{ Gets the exact outline of a single-line text, without hinting, with its
  top-left corner at (0,0) and in pixels. The commands are 0 = move to,
  1 = line to, 2 = cubic curve to (3 points), 3 = close path. ASize is the size
  of the text. Returns false if this kind of text is not handled. }
function BGRAPangoTextPath(AFont: TFont; const AText: string;
  out ACommands: TBytes; out APoints: ArrayOfTPointF; out ASize: TPointF): boolean;
{$ENDIF}

implementation

{$IFDEF LCLgtk3}
uses
  LazGObject2, LazPango1, LazPangoCairo1, LazCairo1, Gtk3Objects, BGRAClasses;

type
  // the cairo path is opaque in the bindings
  PCairoPathData = ^TCairoPathData;
  TCairoPathData = record
    case integer of
      0: (kind, count: longint);
      1: (x, y: double);
  end;
  PCairoPath = ^TCairoPath;
  TCairoPath = record
    status: longint;
    data: PCairoPathData;
    num_data: longint;
  end;

function cairo_recording_surface_create_(content: longint; extents: pointer): Pcairo_surface_t;
  cdecl; external LIB_CAIRO name 'cairo_recording_surface_create';
function cairo_copy_path_(cr: Pcairo_t): PCairoPath;
  cdecl; external LIB_CAIRO name 'cairo_copy_path';
procedure cairo_path_destroy_(path: PCairoPath);
  cdecl; external LIB_CAIRO name 'cairo_path_destroy';

var
  WorkFont: TFont;
  WorkFontLock: TRTLCriticalSection;
  // unbounded surface, otherwise the glyphs outside of it are not in the path
  OutlineSurface: Pcairo_surface_t;
  OutlineCr: Pcairo_t;

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

function BGRAPangoTextPath(AFont: TFont; const AText: string;
  out ACommands: TBytes; out APoints: ArrayOfTPointF; out ASize: TPointF): boolean;
var
  lclLayout, layout: PPangoLayout;
  options: Pcairo_font_options_t;
  path: PCairoPath;
  d: PCairoPathData;
  i, nbCmd, nbPts: integer;
  logical: TPangoRectangle;
begin
  result := false;
  ACommands := nil;
  APoints := nil;
  ASize := PointF(0, 0);
  if (AText = '') or (pos(#10, AText) <> 0) or (pos(#13, AText) <> 0) then exit;

  EnterCriticalSection(WorkFontLock);
  try
    if WorkFont = nil then WorkFont := TFont.Create;
    WorkFont.Assign(AFont);
    WorkFont.Orientation := 0;
    if WorkFont.Handle = 0 then exit;
    lclLayout := TGtk3Font(WorkFont.Handle).Layout;
    if lclLayout = nil then exit;

    if OutlineCr = nil then
    begin
      OutlineSurface := cairo_recording_surface_create_($3000, nil); // color and alpha
      OutlineCr := cairo_create(OutlineSurface);
    end;
    // a private layout is used, to choose the options of the outline
    layout := pango_cairo_create_layout(OutlineCr);
    try
      layout^.set_font_description(lclLayout^.get_font_description);
      layout^.set_attributes(lclLayout^.get_attributes);
      layout^.set_single_paragraph_mode(true);
      layout^.set_width(-1);
      layout^.set_text(PChar(AText), Length(AText));
      // the exact outline is wanted, not the one adjusted to the pixel grid
      options := cairo_font_options_create;
      cairo_font_options_set_hint_style(options, CAIRO_HINT_STYLE_NONE);
      cairo_font_options_set_hint_metrics(options, CAIRO_HINT_METRICS_OFF);
      pango_cairo_context_set_font_options(layout^.get_context, options);
      cairo_font_options_destroy(options);
      pango_cairo_update_layout(OutlineCr, layout);

      layout^.get_extents(nil, @logical);
      ASize := PointF(logical.width / PANGO_SCALE, logical.height / PANGO_SCALE);

      cairo_new_path(OutlineCr);
      cairo_move_to(OutlineCr, 0, 0);
      pango_cairo_layout_path(OutlineCr, layout);
      path := cairo_copy_path_(OutlineCr);
      cairo_new_path(OutlineCr);
      try
        if path^.status <> 0 then exit;
        nbCmd := 0; nbPts := 0;
        i := 0;
        while i < path^.num_data do
        begin
          d := path^.data + i;
          inc(nbCmd);
          case d^.kind of
            0, 1: inc(nbPts);
            2: inc(nbPts, 3);
          end;
          inc(i, d^.count);
        end;
        setlength(ACommands, nbCmd);
        setlength(APoints, nbPts);
        nbCmd := 0; nbPts := 0;
        i := 0;
        while i < path^.num_data do
        begin
          d := path^.data + i;
          case d^.kind of
            0, 1: // move, line
              begin
                ACommands[nbCmd] := d^.kind;
                APoints[nbPts] := PointF((d+1)^.x, (d+1)^.y);
                inc(nbPts);
              end;
            2: // curve
              begin
                ACommands[nbCmd] := 2;
                APoints[nbPts] := PointF((d+1)^.x, (d+1)^.y);
                APoints[nbPts+1] := PointF((d+2)^.x, (d+2)^.y);
                APoints[nbPts+2] := PointF((d+3)^.x, (d+3)^.y);
                inc(nbPts, 3);
              end;
            3: ACommands[nbCmd] := 3; // close
          end;
          inc(nbCmd);
          inc(i, d^.count);
        end;
        result := true;
      finally
        cairo_path_destroy_(path);
      end;
    finally
      g_object_unref(PGObject(layout));
    end;
  finally
    LeaveCriticalSection(WorkFontLock);
  end;
end;

initialization
  InitCriticalSection(WorkFontLock);

finalization
  if OutlineCr <> nil then cairo_destroy(OutlineCr);
  if OutlineSurface <> nil then cairo_surface_destroy(OutlineSurface);
  FreeAndNil(WorkFont);
  DoneCriticalSection(WorkFontLock);
{$ENDIF}

end.
