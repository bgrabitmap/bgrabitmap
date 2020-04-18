unit BGRAClasses;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  {$IFDEF BGRABITMAP_USE_MSEGUI}Types, Classes, mclasses, msegraphutils, mseguiglob{$ELSE}Types, Classes{$ENDIF};

type
  //types always imported from Classes
  TFPList = Classes.TFPList;
  TList = Classes.TList;
  TNotifyEvent = Classes.TNotifyEvent;
  EInvalidOperation = Classes.EInvalidOperation;
  EFCreateError = Classes.EFCreateError;
  EFOpenError = Classes.EFOpenError;
  TAlignment = Classes.TAlignment;
  TSeekOrigin = Classes.TSeekOrigin;

  TPersistent = {$IFDEF BGRABITMAP_USE_MSEGUI}mclasses{$ELSE}Classes{$ENDIF}.TPersistent;
  TComponent = {$IFDEF BGRABITMAP_USE_MSEGUI}mclasses{$ELSE}Classes{$ENDIF}.TComponent;
  TStream = {$IFDEF BGRABITMAP_USE_MSEGUI}mclasses{$ELSE}Classes{$ENDIF}.TStream;
  TResourceStream = {$IFDEF BGRABITMAP_USE_MSEGUI}mclasses{$ELSE}Classes{$ENDIF}.TResourceStream;
  TMemoryStream = {$IFDEF BGRABITMAP_USE_MSEGUI}mclasses{$ELSE}Classes{$ENDIF}.TMemoryStream;
  THandleStream = {$IFDEF BGRABITMAP_USE_MSEGUI}mclasses{$ELSE}Classes{$ENDIF}.THandleStream;
  TStringStream = {$IFDEF BGRABITMAP_USE_MSEGUI}mclasses{$ELSE}Classes{$ENDIF}.TStringStream;
  TStrings = {$IFDEF BGRABITMAP_USE_MSEGUI}mclasses{$ELSE}Classes{$ENDIF}.TStrings;
  TStringList = {$IFDEF BGRABITMAP_USE_MSEGUI}mclasses{$ELSE}Classes{$ENDIF}.TStringList;

type
  {$IFDEF BGRABITMAP_USE_MSEGUI}
  TPoint = msegraphutils.pointty;
  TSize = msegraphutils.sizety;
  TRect = Classes.TRect;
  TClassesPoint = Classes.TPoint;
  TMSERect = msegraphutils.rectty;
  {$ELSE}
  TPoint = Types.TPoint;
  TSize = Types.TSize;
  TRect = Types.TRect;
  {$ENDIF}
  PPoint = ^TPoint;
  PSize = ^TSize;
  PRect = ^TRect;

  {$IF FPC_FULLVERSION>=030001}
  TPointF = Types.TPointF;
  TRectF = Types.TRectF;
  {$ELSE}
  TPointF = record x : single; y : single; end;

  {$define BGRA_DEFINE_TRECTF}
  TRectF =
  {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
  {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record
  private
    function GetHeight: single;
    function GetWidth: Single;
  public
    property Width: Single read GetWidth;
    property Height: single read GetHeight;
    procedure Offset (const dx,dy : Single);
    case Integer of
     0: (Left, Top, Right, Bottom: Single);
     1: (TopLeft, BottomRight: TPointF);
  end;
  {$ENDIF}

const
  //types always imported from Classes
  taLeftJustify = Classes.taLeftJustify;
  taRightJustify = Classes.taRightJustify;
  taCenter = Classes.taCenter;

type
  {$IFDEF BGRABITMAP_USE_MSEGUI}
  TShiftState = mseguiglob.shiftstatesty;
  {$ELSE}
  TShiftState = Classes.TShiftState;
  {$ENDIF}

const
  {$IFDEF BGRABITMAP_USE_MSEGUI}
  ssShift = mseguiglob.ss_shift;
  ssAlt = mseguiglob.ss_alt;
  ssCtrl = mseguiglob.ss_ctrl;
  ssLeft = mseguiglob.ss_left;
  ssRight = mseguiglob.ss_right;
  ssMiddle = mseguiglob.ss_middle;
  ssDouble = mseguiglob.ss_double;
  ssTriple = mseguiglob.ss_triple;
  {$ELSE}
  ssShift = Classes.ssShift;
  ssAlt = Classes.ssAlt;
  ssCtrl = Classes.ssCtrl;
  ssLeft = Classes.ssLeft;
  ssRight = Classes.ssRight;
  ssMiddle = Classes.ssMiddle;
  ssDouble = Classes.ssDouble;
  ssTriple = Classes.ssTriple;
  {$ENDIF}

  soBeginning = Classes.soBeginning;
  soCurrent = Classes.soCurrent;
  soEnd = Classes.soEnd;

  {$IFDEF BGRABITMAP_USE_MSEGUI}
  fmCreate        = $FF00;
  fmOpenRead      = 0;
  fmOpenWrite     = 1;
  fmOpenReadWrite = 2;

  soFromBeginning = 0;
  soFromCurrent = 1;
  soFromEnd = 2;
  {$ELSE}
  fmCreate = Classes.fmCreate;
  fmOpenRead = Classes.fmOpenRead;
  fmOpenWrite = Classes.fmOpenWrite;
  fmOpenReadWrite = Classes.fmOpenReadWrite;
  soFromBeginning = Classes.soFromBeginning;
  soFromCurrent = Classes.soFromCurrent;
  soFromEnd = Classes.soFromEnd;
  {$ENDIF}

function Rect(ALeft, ATop, ARight, ABottom: Integer): TRect;
function Point(AX, AY: Integer): TPoint;
function Size(AWidth, AHeight: Integer): TSize;

implementation

function Rect(ALeft, ATop, ARight, ABottom: Integer): TRect;
begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
  end;
end;

{$IFDEF BGRA_DEFINE_TRECTF}
{ TRectF }

function TRectF.GetHeight: single;
begin
  result := Bottom-Top;
end;

function TRectF.GetWidth: Single;
begin
  result := Right-Left;
end;

procedure TRectF.Offset(const dx, dy: Single);
begin
  left:=left+dx; right:=right+dx;
  bottom:=bottom+dy; top:=top+dy;
end;
{$ENDIF}

function Point(AX, AY: Integer): TPoint;
begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;

function Size(AWidth, AHeight: Integer): TSize;
begin
  Result.cx := AWidth;
  Result.cy := AHeight;
end;

end.

