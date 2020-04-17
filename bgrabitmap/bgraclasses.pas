unit BGRAClasses;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Types, Classes;

type
  TFPList = Classes.TFPList;
  TList = Classes.TList;
  TNotifyEvent = Classes.TNotifyEvent;
  EInvalidOperation = Classes.EInvalidOperation;
  EFCreateError = Classes.EFCreateError;
  EFOpenError = Classes.EFOpenError;
  TAlignment = Classes.TAlignment;
  TSeekOrigin = Classes.TSeekOrigin;
  TPersistent = Classes.TPersistent;
  TComponent = Classes.TComponent;
  TStream = Classes.TStream;
  TResourceStream = Classes.TResourceStream;
  TMemoryStream = Classes.TMemoryStream;
  THandleStream = Classes.THandleStream;
  TStringStream = Classes.TStringStream;
  TStrings = Classes.TStrings;
  TStringList = Classes.TStringList;

type
  TPoint = Types.TPoint;
  TSize = Types.TSize;
  TRect = Types.TRect;
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
  taLeftJustify = Classes.taLeftJustify;
  taRightJustify = Classes.taRightJustify;
  taCenter = Classes.taCenter;

type
  TShiftState = Classes.TShiftState;

const
  ssShift = Classes.ssShift;
  ssAlt = Classes.ssAlt;
  ssCtrl = Classes.ssCtrl;
  ssLeft = Classes.ssLeft;
  ssRight = Classes.ssRight;
  ssMiddle = Classes.ssMiddle;
  ssDouble = Classes.ssDouble;
  ssTriple = Classes.ssTriple;

  soBeginning = Classes.soBeginning;
  soCurrent = Classes.soCurrent;
  soEnd = Classes.soEnd;

  fmCreate = Classes.fmCreate;
  fmOpenRead = Classes.fmOpenRead;
  fmOpenWrite = Classes.fmOpenWrite;
  fmOpenReadWrite = Classes.fmOpenReadWrite;
  soFromBeginning = Classes.soFromBeginning;
  soFromCurrent = Classes.soFromCurrent;
  soFromEnd = Classes.soFromEnd;

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

