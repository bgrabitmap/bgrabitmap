// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Exact text outlines for Qt5.

  The LCL Qt5 widgetset represents a font as a QFont (TQtFont). The outline of
  a text is obtained with QPainterPath::addText, which uses the same font and
  the same text layout engine as the one that draws the text. To read the
  elements of the path, the functions QPainterPath_elementPositionAt and
  QPainterPath_elementTypeAt of libQt5Pas are used when they exist (they are
  declared here with the same names as in the bindings of the LCL). With a
  version of libQt5Pas that does not have them, QPainterPath::elementAt is
  called directly from libQt5Gui. If none is available, the text is refused and
  the caller uses the generic way.

  This unit should NOT be added to the **uses** clause of applications. }
unit BGRAQt5Text;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

{$IFDEF LCLqt5}
uses
  SysUtils, Graphics, BGRABitmapTypes;

{ Adds the exact outline of a single-line text to ADest, without hinting,
  with its top-left corner at (0,0) and in pixels. The outline is made of
  lines and cubic curves. Returns false if this kind of text is not handled
  or if the elements of a path cannot be read, in which case ADest may have
  been partially filled. }
function BGRAQt5TextPath(AFont: TFont; const AText: string;
  ADest: IBGRAPath): boolean;
{$ENDIF}

implementation

{$IFDEF LCLqt5}
uses
  dynlibs, qt5, qtobjects, BGRAClasses;

type
  TQtPathElementPositionAt = procedure(handle: QPainterPathH; retval: PQtPointF;
    i: Integer); cdecl;
  TQtPathElementTypeAt = function(handle: QPainterPathH; i: Integer): Integer; cdecl;
  // QPainterPath::Element returned by value, so by a hidden pointer
  TQtPathElementAtByValue = procedure(retval: pointer; handle: QPainterPathH;
    i: Integer); cdecl;
  TQtPathElement = record
    x, y: double;
    kind: longint;
    pad: longint;
  end;

var
  ElementPositionAt: TQtPathElementPositionAt;
  ElementTypeAt: TQtPathElementTypeAt;
  ElementAtByValue: TQtPathElementAtByValue;
  Resolved: boolean;
  WorkFont: TFont;
  WorkFontLock: TRTLCriticalSection;

procedure ResolveFunctions;
var
  lib: TLibHandle;
begin
  Resolved := true;
  lib := LoadLibrary(Qt5PasLib);
  if lib <> NilHandle then
  begin
    Pointer(ElementPositionAt) := GetProcAddress(lib, 'QPainterPath_elementPositionAt');
    Pointer(ElementTypeAt) := GetProcAddress(lib, 'QPainterPath_elementTypeAt');
  end;
  if Assigned(ElementPositionAt) and Assigned(ElementTypeAt) then exit;
  ElementPositionAt := nil;
  ElementTypeAt := nil;
  {$IFDEF LINUX}
  // libQt5Pas without these functions: QPainterPath::elementAt(int) const
  lib := LoadLibrary('libQt5Gui.so.5');
  if lib <> NilHandle then
    Pointer(ElementAtByValue) := GetProcAddress(lib, '_ZNK12QPainterPath9elementAtEi');
  {$ENDIF}
end;

procedure GetElement(APath: QPainterPathH; AIndex: integer;
  out APoint: TPointF; out AKind: integer);
var
  pt: TQtPointF;
  e: TQtPathElement;
begin
  if Assigned(ElementPositionAt) then
  begin
    ElementPositionAt(APath, @pt, AIndex);
    APoint := PointF(pt.X, pt.Y);
    AKind := ElementTypeAt(APath, AIndex);
  end else
  begin
    ElementAtByValue(@e, APath, AIndex);
    APoint := PointF(e.x, e.y);
    AKind := e.kind;
  end;
end;

function BGRAQt5TextPath(AFont: TFont; const AText: string;
  ADest: IBGRAPath): boolean;
var
  qFont: QFontH;
  metrics: QFontMetricsH;
  path: QPainterPathH;
  wideText: WideString;
  i, count, kind, ascent: integer;
  pt, c1, c2, c3: TPointF;
begin
  result := false;
  if (AText = '') or (pos(#10, AText) <> 0) or (pos(#13, AText) <> 0) then exit;
  if not Resolved then ResolveFunctions;
  if not (Assigned(ElementPositionAt) or Assigned(ElementAtByValue)) then exit;

  EnterCriticalSection(WorkFontLock);
  try
    if WorkFont = nil then WorkFont := TFont.Create;
    WorkFont.Assign(AFont);
    WorkFont.Orientation := 0;
    if WorkFont.Handle = 0 then exit;
    qFont := TQtFont(WorkFont.Handle).FHandle;
    if qFont = nil then exit;

    // addText gives the outline with the baseline at the given position
    metrics := QFontMetrics_Create(qFont);
    try
      ascent := QFontMetrics_ascent(metrics);
    finally
      QFontMetrics_Destroy(metrics);
    end;

    path := QPainterPath_Create;
    try
      wideText := UTF8Decode(AText);
      QPainterPath_addText(path, 0, ascent, qFont, @wideText);
      count := QPainterPath_elementCount(path);
      i := 0;
      while i < count do
      begin
        GetElement(path, i, pt, kind);
        case kind of
          0: ADest.moveTo(pt);
          1: ADest.lineTo(pt);
          2:
            begin
              // a curve is made of three elements: two control points and the end point
              if i + 2 >= count then exit;
              c1 := pt;
              GetElement(path, i+1, c2, kind);
              GetElement(path, i+2, c3, kind);
              ADest.bezierCurveTo(c1, c2, c3);
              inc(i, 2);
            end;
        end;
        inc(i);
      end;
      result := true;
    finally
      QPainterPath_Destroy(path);
    end;
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
