// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Exact outlines of a text with the Windows GDI.

  The text is drawn between BeginPath and EndPath, which records its outline
  instead of drawing it. This is done by the same text engine as TextOut, so
  the outline follows the shaping of the script (ligatures, combining marks,
  joining of arabic letters, right-to-left text) and the font fallback that
  GDI does for the characters that the font does not have. The text is drawn
  with a copy of the font that is much larger, because the coordinates of the
  path are integers, and scaled back. The curves of the fonts (lines,
  quadratic and cubic curves) are kept as Bezier curves.

  This unit should NOT be added to the **uses** clause of applications. }
unit BGRAWin32Text;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

{$IFDEF LCLwin32}
uses
  SysUtils, Graphics, BGRABitmapTypes;

{ Adds to ADest the exact outline of a single-line text, in pixels, with its
  top-left corner at (0,0). This is the position of the text if it is drawn
  with its top-left corner at (0,0). Returns false if GDI does not provide an
  outline for this text (for example with a bitmap font), in which case ADest
  is left unchanged. }
function BGRAWin32TextPath(AFont: TFont; const AText: string;
  ADest: IBGRAPath): boolean;
{$ENDIF}

implementation

{$IFDEF LCLwin32}
uses
  Windows, BGRAUTF8, BGRAUnicode, BGRAPath;

const
  // flags of the types given by GetPath
  PT_CLOSEFIGURE_ = 1;
  PT_LINETO_ = 2;
  PT_BEZIERTO_ = 4;
  PT_MOVETO_ = 6;
  // size in pixels of the font used to get the outline
  OutlineFontSize = 2048;

// not declared in the FPC windows unit
function AbortPath_(DC: HDC): BOOL; stdcall; external 'gdi32' name 'AbortPath';

function IsBlankText(const AText: string): boolean;
var
  p: PChar;
  len: integer;
begin
  result := true;
  p := PChar(AText);
  while p^ <> #0 do
  begin
    len := UTF8CharacterLength(p);
    if not IsUnicodeSpace(UTF8CodepointToUnicode(p, len)) then exit(false);
    inc(p, len);
  end;
end;

function BGRAWin32TextPath(AFont: TFont; const AText: string;
  ADest: IBGRAPath): boolean;
var
  lf: LOGFONTW;
  hf, oldFont: HFONT;
  dc: HDC;
  scale, count, i, curvePoints: integer;
  wide: UnicodeString;
  points: array of TPoint;
  types: array of byte;
  curve: array[0..2] of TPointF;
  outline: TBGRAPath;
  bounds: TRect;

  function ToPixels(const APt: TPoint): TPointF;
  begin
    result := PointF(APt.x/scale, APt.y/scale);
  end;

begin
  result := false;
  if AText = '' then exit;
  if (pos(#10, AText) <> 0) or (pos(#13, AText) <> 0) then exit;
  if AFont.Handle = 0 then exit;

  // the font that is used for the text, with a much larger size
  FillChar(lf, SizeOf(lf), 0);
  if GetObjectW(AFont.Handle, SizeOf(lf), @lf) <> SizeOf(lf) then exit;
  if lf.lfHeight = 0 then exit;
  scale := OutlineFontSize div abs(lf.lfHeight);
  if scale < 1 then scale := 1;
  lf.lfHeight := lf.lfHeight*scale;
  lf.lfWidth := lf.lfWidth*scale;
  lf.lfEscapement := 0;
  lf.lfOrientation := 0;
  hf := CreateFontIndirectW(lf);
  if hf = 0 then exit;

  dc := CreateCompatibleDC(0);
  if dc = 0 then
  begin
    DeleteObject(hf);
    exit;
  end;
  oldFont := SelectObject(dc, hf);
  try
    wide := UTF8ToUTF16(AText);
    SetBkMode(dc, TRANSPARENT);
    if not BeginPath(dc) then exit;
    // DrawText, like the text of the bitmaps (it applies the kerning of the font)
    bounds.Left := 0;
    bounds.Top := 0;
    bounds.Right := 1 shl 24;
    bounds.Bottom := 1 shl 24;
    DrawTextW(dc, PWideChar(wide), length(wide), bounds,
              DT_LEFT or DT_TOP or DT_SINGLELINE or DT_NOPREFIX or DT_NOCLIP);
    if not EndPath(dc) then
    begin
      AbortPath_(dc);
      exit;
    end;
    count := GetPath(dc, nil, nil, 0);
    if count < 0 then exit;
    if count = 0 then
    begin
      // nothing was recorded: normal for a blank text, but it also happens
      // with the fonts that do not have an outline (bitmap fonts)
      result := IsBlankText(AText);
      exit;
    end;
    setlength(points, count);
    setlength(types, count);
    if GetPath(dc, @points[0], @types[0], count) <> count then exit;

    // the destination is only modified if the outline is valid
    outline := TBGRAPath.Create;
    try
      curvePoints := 0;
      for i := 0 to count-1 do
      begin
        case types[i] and not PT_CLOSEFIGURE_ of
          PT_MOVETO_: outline.moveTo(ToPixels(points[i]));
          PT_LINETO_: outline.lineTo(ToPixels(points[i]));
          PT_BEZIERTO_:
            begin
              curve[curvePoints] := ToPixels(points[i]);
              inc(curvePoints);
              if curvePoints = 3 then
              begin
                outline.bezierCurveTo(curve[0], curve[1], curve[2]);
                curvePoints := 0;
              end;
            end;
          else exit;
        end;
        if (types[i] and PT_CLOSEFIGURE_) <> 0 then outline.closePath;
      end;
      if curvePoints <> 0 then exit;
      outline.copyTo(ADest);
    finally
      outline.Free;
    end;
    result := true;
  finally
    SelectObject(dc, oldFont);
    DeleteDC(dc);
    DeleteObject(hf);
  end;
end;
{$ENDIF}

end.
