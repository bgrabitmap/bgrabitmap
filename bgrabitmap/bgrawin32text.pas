// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Exact glyph outlines with the Windows GDI.

  The outline of a character is requested with GetGlyphOutline, on a copy of
  the font that is much larger, so that the coordinates, which are integers,
  are precise once scaled back. The outline is not hinted. Lines and quadratic
  curves of TrueType fonts (and cubic curves of PostScript fonts) are added to
  a path without being converted.

  Only simple characters are handled: GetGlyphOutline does not do font
  fallback, nor the shaping needed by some scripts (ligatures, combining marks,
  right-to-left scripts), so the caller must use the generic way for the other
  cases.

  This unit should NOT be added to the **uses** clause of applications. }
unit BGRAWin32Text;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

{$IFDEF LCLwin32}
uses
  SysUtils, Graphics, BGRABitmapTypes;

{ Adds to ADest the exact outline of a text that is made of a single character,
  without hinting, with its top-left corner at (0,0) and in pixels. This is
  the position of the glyph if the text is drawn with its top-left corner at
  (0,0). Returns false if the character is not handled (more than one
  character, character that needs shaping, character that is not in the font,
  font that does not provide an outline), in which case ADest is left
  unchanged. }
function BGRAWin32TextPath(AFont: TFont; const AText: string;
  ADest: IBGRAPath): boolean;
{$ENDIF}

implementation

{$IFDEF LCLwin32}
uses
  Windows, BGRAUTF8, BGRAPath;

const
  // not declared in the FPC windows unit
  GGO_UNHINTED_ = $0100;
  GGI_MARK_NONEXISTING_GLYPHS_ = 1;
  MISSING_GLYPH_INDEX = $FFFF;
  // sizes of the structures of GetGlyphOutline
  POINTFX_SIZE = 8;
  TTPOLYGONHEADER_SIZE = 16;
  TTPOLYCURVE_HEADER_SIZE = 4;
  TT_POLYGON_TYPE_ = 24;
  TT_PRIM_LINE_ = 1;
  TT_PRIM_QSPLINE_ = 2;
  TT_PRIM_CSPLINE_ = 3;
  // size in pixels of the font used to get the outline
  OutlineFontSize = 2048;

function GetGlyphIndicesW_(DC: HDC; Str: PWideChar; Count: Integer;
  Indices: PWord; Flags: DWORD): DWORD; stdcall;
  external 'gdi32' name 'GetGlyphIndicesW';

{ Characters for which the glyph given by GetGlyphOutline is the one drawn by
  the text functions: no combining mark, no script needing shaping or
  right-to-left, no control character. Characters outside the Basic
  Multilingual Plane are not handled. }
function IsSimpleCharacter(u: LongWord): boolean;
begin
  result :=
    ((u >= $0020) and (u <= $007E)) or     // Basic Latin
    ((u >= $00A0) and (u <= $02FF)) or     // Latin-1, Latin Extended, IPA, modifiers
    ((u >= $0370) and (u <= $058F)) or     // Greek, Cyrillic, Armenian
    ((u >= $1E00) and (u <= $1FFF)) or     // Latin Extended Additional, Greek Extended
    ((u >= $2010) and (u <= $2027)) or     // punctuation
    ((u >= $2030) and (u <= $205E)) or
    ((u >= $2070) and (u <= $27BF)) or     // symbols, arrows, mathematics, dingbats
    ((u >= $2900) and (u <= $2BFF)) or
    ((u >= $3000) and (u <= $30FF)) or     // CJK symbols, hiragana, katakana
    ((u >= $3400) and (u <= $4DBF)) or     // CJK ideographs
    ((u >= $4E00) and (u <= $9FFF)) or
    ((u >= $AC00) and (u <= $D7A3)) or     // Hangul syllables
    ((u >= $FF01) and (u <= $FF60)) or     // fullwidth forms
    ((u >= $FFE0) and (u <= $FFE6));
end;

function FixedToSingle(const AValue: FIXED): single;
begin
  result := AValue.value + AValue.fract/65536;
end;

{ Reads the outline given by GetGlyphOutline in the GGO_NATIVE format and adds
  it to the path. AScale is the ratio between the coordinates of the outline
  and the pixels, and AAscent the distance in outline coordinates from the top
  of the text to the baseline. Returns false if the data is not valid. }
function AddNativeOutline(const AData: array of byte; ADataSize: integer;
  AAscent, AScale: single; ADest: IBGRAPath): boolean;
var
  p, polygonEnd, curveEnd: integer;
  i, count, curveType: integer;
  startPt: TPointF;
  pts: array of TPointF;

  function ReadPoint(AOffset: integer): TPointF;
  var pfx: POINTFX;
  begin
    move(AData[AOffset], pfx, POINTFX_SIZE);
    result := PointF(FixedToSingle(pfx.x)/AScale,
                     (AAscent - FixedToSingle(pfx.y))/AScale);
  end;

  function ReadWord(AOffset: integer): integer;
  begin
    result := AData[AOffset] or (AData[AOffset+1] shl 8);
  end;

  function ReadDWord(AOffset: integer): integer;
  begin
    result := AData[AOffset] or (AData[AOffset+1] shl 8) or
              (AData[AOffset+2] shl 16) or (AData[AOffset+3] shl 24);
  end;

  procedure AddQuadraticSpline(const APts: array of TPointF; var ACurrent: TPointF);
  var k: integer;
    endPt: TPointF;
  begin
    // consecutive off-curve points have an implicit on-curve point between them,
    // the last point is on the curve
    for k := 0 to high(APts)-1 do
    begin
      if k < high(APts)-1 then
        endPt := PointF((APts[k].x+APts[k+1].x)/2, (APts[k].y+APts[k+1].y)/2)
      else
        endPt := APts[high(APts)];
      ADest.quadraticCurveTo(APts[k], endPt);
      ACurrent := endPt;
    end;
  end;

var
  current: TPointF;
begin
  result := false;
  p := 0;
  while p < ADataSize do
  begin
    if p + TTPOLYGONHEADER_SIZE > ADataSize then exit;
    polygonEnd := p + ReadDWord(p);
    if (ReadDWord(p+4) <> TT_POLYGON_TYPE_) or (polygonEnd > ADataSize) or
       (polygonEnd < p + TTPOLYGONHEADER_SIZE) then exit;
    startPt := ReadPoint(p+8);
    ADest.moveTo(startPt);
    current := startPt;
    inc(p, TTPOLYGONHEADER_SIZE);
    while p < polygonEnd do
    begin
      if p + TTPOLYCURVE_HEADER_SIZE > polygonEnd then exit;
      curveType := ReadWord(p);
      count := ReadWord(p+2);
      curveEnd := p + TTPOLYCURVE_HEADER_SIZE + count*POINTFX_SIZE;
      if (count = 0) or (curveEnd > polygonEnd) then exit;
      setlength(pts, count);
      for i := 0 to count-1 do
        pts[i] := ReadPoint(p + TTPOLYCURVE_HEADER_SIZE + i*POINTFX_SIZE);
      case curveType of
        TT_PRIM_LINE_:
          for i := 0 to count-1 do ADest.lineTo(pts[i]);
        TT_PRIM_QSPLINE_:
          if count = 1 then ADest.lineTo(pts[0])
          else AddQuadraticSpline(pts, current);
        TT_PRIM_CSPLINE_:
          begin
            if count mod 3 <> 0 then exit;
            i := 0;
            while i < count do
            begin
              ADest.bezierCurveTo(pts[i], pts[i+1], pts[i+2]);
              inc(i, 3);
            end;
          end;
        else exit;
      end;
      p := curveEnd;
    end;
    ADest.closePath;
  end;
  result := true;
end;

function BGRAWin32TextPath(AFont: TFont; const AText: string;
  ADest: IBGRAPath): boolean;
var
  u: LongWord;
  lf: LOGFONTW;
  hf, oldFont: HFONT;
  dc: HDC;
  scale: integer;
  wc: WideChar;
  glyphIndex: WORD;
  tm: TEXTMETRICW;
  gm: TGlyphMetrics;
  mat: TMat2;
  dataSize: DWORD;
  data: array of byte;
  outline: TBGRAPath;
begin
  result := false;
  if (AText = '') or (UTF8Length(AText) <> 1) then exit;
  u := UTF8CodepointToUnicode(PChar(AText), length(AText));
  if not IsSimpleCharacter(u) then exit;
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
    // GetGlyphOutline does not use font fallback: if the font does not have
    // the character, the caller must use the generic way
    wc := WideChar(u);
    glyphIndex := MISSING_GLYPH_INDEX;
    if (GetGlyphIndicesW_(dc, @wc, 1, @glyphIndex, GGI_MARK_NONEXISTING_GLYPHS_) = GDI_ERROR)
       or (glyphIndex = MISSING_GLYPH_INDEX) then exit;
    if not GetTextMetricsW(dc, @tm) then exit;

    FillChar(mat, SizeOf(mat), 0);
    mat.eM11.value := 1;
    mat.eM22.value := 1;
    FillChar(gm, SizeOf(gm), 0);
    dataSize := GetGlyphOutlineW(dc, u, GGO_NATIVE or GGO_UNHINTED_, gm, 0, nil, mat);
    if dataSize = GDI_ERROR then exit;
    if dataSize = 0 then exit(true); // blank character
    setlength(data, dataSize);
    if GetGlyphOutlineW(dc, u, GGO_NATIVE or GGO_UNHINTED_, gm, dataSize, @data[0], mat) = GDI_ERROR then exit;

    // the destination is only modified if the outline is valid
    outline := TBGRAPath.Create;
    try
      if not AddNativeOutline(data, dataSize, tm.tmAscent, scale, outline) then exit;
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
