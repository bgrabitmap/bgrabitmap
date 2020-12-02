// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRALayerOriginal;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

uses
  BGRAClasses, SysUtils, BGRABitmap, BGRABitmapTypes, BGRATransform, BGRAMemDirectory, fgl
  {$IFDEF BGRABITMAP_USE_LCL},LCLType{$ENDIF};

type
  PRectF = BGRABitmapTypes.PRectF;
  TAffineMatrix = BGRATransform.TAffineMatrix;
  TBGRALayerCustomOriginal = class;
  TBGRAOriginalDiff = class;
  TBGRALayerOriginalAny = class of TBGRALayerCustomOriginal;
  TOriginalMovePointEvent = procedure(ASender: TObject; APrevCoord, ANewCoord: TPointF; AShift: TShiftState) of object;
  TOriginalStartMovePointEvent = procedure(ASender: TObject; AIndex: integer; AShift: TShiftState) of object;
  TOriginalClickPointEvent = procedure(ASender: TObject; AIndex: integer; AShift: TShiftState) of object;
  TOriginalHoverPointEvent = procedure(ASender: TObject; AIndex: integer) of object;
  TOriginalChangeEvent = procedure(ASender: TObject; ABounds: PRectF; var ADiff: TBGRAOriginalDiff) of object;
  TOriginalEditingChangeEvent = procedure(ASender: TObject) of object;
  TOriginalEditorCursor = (oecDefault, oecMove, oecMoveW, oecMoveE, oecMoveN, oecMoveS,
                           oecMoveNE, oecMoveSW, oecMoveNW, oecMoveSE, oecHandPoint, oecText);
  TSpecialKey = (skUnknown, skBackspace, skTab, skReturn, skEscape,
                 skPageUp, skPageDown, skHome, skEnd,
                 skLeft, skUp, skRight, skDown,
                 skInsert, skDelete,
                 skNum0, skNum1, skNum2, skNum3, skNum4, skNum5, skNum6, skNum7, skNum8, skNum9,
                 skF1, skF2, skF3, skF4, skF5, skF6, skF7, skF8, skF9, skF10, skF11, skF12,
                 skA, skB, skC, skD, skE, skF, skG, skH, skI, skJ, skK, skL, skM, skN, skO, skP, skQ, skR, skS, skT, skU, skV, skW, skX, skY, skZ,
                 sk0, sk1, sk2, sk3, sk4, sk5, sk6, sk7, sk8, sk9,
                 skShift, skCtrl, skAlt);

const
  SpecialKeyStr: array[TSpecialKey] of string =
    ('Unknown', 'Backspace', 'Tab', 'Return', 'Escape',
     'PageUp', 'PageDown', 'Home', 'End',
     'Left', 'Up', 'Right', 'Down',
     'Insert', 'Delete',
     'Num0', 'Num1', 'Num2', 'Num3', 'Num4', 'Num5', 'Num6', 'Num7', 'Num8', 'Num9',
     'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10', 'F11', 'F12',
     'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
     '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
     'Shift', 'Ctrl', 'Alt');

{$IFDEF BGRABITMAP_USE_LCL}
const
  SpecialKeyToLCL: array[TSpecialKey] of Word =
    (VK_UNKNOWN, VK_BACK,VK_TAB,VK_RETURN,VK_ESCAPE,
     VK_PRIOR,VK_NEXT,VK_HOME,VK_END,
     VK_LEFT,VK_UP,VK_RIGHT,VK_DOWN,
     VK_INSERT,VK_DELETE,
     VK_NUMPAD0,VK_NUMPAD1,VK_NUMPAD2,VK_NUMPAD3,VK_NUMPAD4,VK_NUMPAD5,VK_NUMPAD6,VK_NUMPAD7,VK_NUMPAD8,VK_NUMPAD9,
     VK_F1,VK_F2,VK_F3,VK_F4,VK_F5,VK_F6,VK_F7,VK_F8,VK_F9,VK_F10,VK_F11,VK_F12,
     VK_A, VK_B, VK_C, VK_D, VK_E, VK_F, VK_G, VK_H, VK_I, VK_J, VK_K, VK_L, VK_M, VK_N, VK_O, VK_P, VK_Q, VK_R, VK_S, VK_T, VK_U, VK_V, VK_W, VK_X, VK_Y, VK_Z,
     VK_0, VK_1, VK_2, VK_3, VK_4, VK_5, VK_6, VK_7, VK_8, VK_9,
     VK_SHIFT, VK_CONTROL, VK_MENU);

  function LCLKeyToSpecialKey(AKey: Word; AShift: TShiftState): TSpecialKey;
{$ENDIF}

type
  TStartMoveHandlers = specialize TFPGList<TOriginalStartMovePointEvent>;
  TClickPointHandlers = specialize TFPGList<TOriginalClickPointEvent>;
  THoverPointHandlers = specialize TFPGList<TOriginalHoverPointEvent>;
  TBGRAOriginalPolylineStyle = (opsNone, opsSolid, opsDash, opsDashWithShadow);

  { TBGRAOriginalEditor }

  TBGRAOriginalEditor = class
  private
    FFocused: boolean;
    FOnFocusChanged: TNotifyEvent;
    function GetIsMovingPoint: boolean;
    function GetPointCoord(AIndex: integer): TPointF;
    function GetPointCount: integer;
    function GetPointHighlighted(AIndex: integer): boolean;
    procedure SetFocused(AValue: boolean);
    procedure SetPointHighlighted(AIndex: integer; AValue: boolean);
  protected
    FMatrix,FMatrixInverse: TAffineMatrix;          //view matrix from original coord
    FGridMatrix,FGridMatrixInverse: TAffineMatrix;  //grid matrix in original coord
    FGridActive: boolean;
    FPoints: array of record
      Origin, Coord: TPointF;
      OnMove, OnAlternateMove: TOriginalMovePointEvent;
      RightButton, Highlighted: boolean;
      SnapToPoint: integer;
      HitBox: TAffineBox;
    end;
    FPolylines: array of record
      Coords: array of TPointF;
      Closed: boolean;
      Style: TBGRAOriginalPolylineStyle;
      BackColor: TBGRAPixel;
    end;
    FPointSize: single;
    FPointMoving: integer;
    FPointWasMoved: boolean;
    FPointCoordDelta: TPointF;
    FMovingRightButton: boolean;
    FPrevMousePos: TPointF;
    FStartMoveHandlers: TStartMoveHandlers;
    FCurHoverPoint: integer;
    FHoverPointHandlers: THoverPointHandlers;
    FClickPointHandlers: TClickPointHandlers;
    function RenderPoint(ADest: TBGRABitmap; ACoord: TPointF; AAlternateColor: boolean; AHighlighted: boolean): TRect; virtual;
    function GetRenderPointBounds(ACoord: TPointF; AHighlighted: boolean): TRect; virtual;
    function RenderArrow(ADest: TBGRABitmap; AOrigin, AEndCoord: TPointF): TRect; virtual;
    function GetRenderArrowBounds(AOrigin, AEndCoord: TPointF): TRect; virtual;
    function RenderPolygon(ADest: TBGRABitmap; ACoords: array of TPointF; AClosed: boolean; AStyle: TBGRAOriginalPolylineStyle; ABackColor: TBGRAPixel): TRect; virtual;
    function GetRenderPolygonBounds(ACoords: array of TPointF): TRect;
    procedure SetMatrix(AValue: TAffineMatrix);
    procedure SetGridMatrix(AValue: TAffineMatrix);
    procedure SetGridActive(AValue: boolean);
    function GetMoveCursor(APointIndex: integer): TOriginalEditorCursor; virtual;
    function GetFixedShiftForButton(AShift: TShiftState; ARightDown: boolean): TShiftState;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure AddStartMoveHandler(AOnStartMove: TOriginalStartMovePointEvent);
    procedure AddClickPointHandler(AOnClickPoint: TOriginalClickPointEvent);
    procedure AddHoverPointHandler(AOnHoverPoint: TOriginalHoverPointEvent);
    function AddPoint(const ACoord: TPointF; AOnMove: TOriginalMovePointEvent; ARightButton: boolean = false; ASnapToPoint: integer = -1): integer;
    procedure AddPointAlternateMove(AIndex: integer; AOnAlternateMove: TOriginalMovePointEvent);
    function AddFixedPoint(const ACoord: TPointF; ARightButton: boolean = false): integer;
    function AddArrow(const AOrigin, AEndCoord: TPointF; AOnMoveEnd: TOriginalMovePointEvent; ARightButton: boolean = false): integer;
    function AddPolyline(const ACoords: array of TPointF; AClosed: boolean; AStyle: TBGRAOriginalPolylineStyle): integer; overload;
    function AddPolyline(const ACoords: array of TPointF; AClosed: boolean; AStyle: TBGRAOriginalPolylineStyle; ABackColor: TBGRAPixel): integer; overload;
    procedure SetHitBox(AIndex: integer; AHitBox: TAffineBox);
    procedure MouseMove(Shift: TShiftState; ViewX, ViewY: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); virtual;
    procedure MouseDown(RightButton: boolean; Shift: TShiftState; ViewX, ViewY: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); virtual;
    procedure MouseUp(RightButton: boolean; {%H-}Shift: TShiftState; {%H-}ViewX, {%H-}ViewY: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); virtual;
    procedure KeyDown({%H-}Shift: TShiftState; {%H-}Key: TSpecialKey; out AHandled: boolean); virtual;
    procedure KeyUp({%H-}Shift: TShiftState; {%H-}Key: TSpecialKey; out AHandled: boolean); virtual;
    procedure KeyPress({%H-}UTF8Key: string; out AHandled: boolean); virtual;
    function GetPointAt(const ACoord: TPointF; ARightButton: boolean): integer;
    function Render(ADest: TBGRABitmap; const {%H-}ALayoutRect: TRect): TRect; virtual;
    function GetRenderBounds(const {%H-}ALayoutRect: TRect): TRect; virtual;
    function SnapToGrid(const ACoord: TPointF; AIsViewCoord: boolean): TPointF;
    function OriginalCoordToView(const AImageCoord: TPointF): TPointF;
    function ViewCoordToOriginal(const AViewCoord: TPointF): TPointF;
    property Matrix: TAffineMatrix read FMatrix write SetMatrix;
    property GridMatrix: TAffineMatrix read FGridMatrix write SetGridMatrix;
    property GridActive: boolean read FGridActive write SetGridActive;
    property Focused: boolean read FFocused write SetFocused;
    property PointSize: single read FPointSize write FPointSize;
    property PointCount: integer read GetPointCount;
    property PointCoord[AIndex: integer]: TPointF read GetPointCoord;
    property PointHighlighted[AIndex: integer]: boolean read GetPointHighlighted write SetPointHighlighted;
    property OnFocusChanged: TNotifyEvent read FOnFocusChanged write FOnFocusChanged;
    property IsMovingPoint: boolean read GetIsMovingPoint;
  end;

  TBGRACustomOriginalStorage = class;
  ArrayOfSingle = array of single;

  TBGRAOriginalDiff = class
    procedure Apply(AOriginal: TBGRALayerCustomOriginal); virtual; abstract;
    procedure Unapply(AOriginal: TBGRALayerCustomOriginal); virtual; abstract;
    function CanAppend(ADiff: TBGRAOriginalDiff): boolean; virtual; abstract;
    procedure Append(ADiff: TBGRAOriginalDiff); virtual; abstract;
    function IsIdentity: boolean; virtual; abstract;
  end;

  { TBGRALayerCustomOriginal }

  TBGRALayerCustomOriginal = class
  private
    FOnChange: TOriginalChangeEvent;
    FOnEditingChange: TOriginalEditingChangeEvent;
    FRenderStorage: TBGRACustomOriginalStorage;
    function GetDiffExpected: boolean;
    procedure SetOnChange(AValue: TOriginalChangeEvent);
    procedure SetRenderStorage(AValue: TBGRACustomOriginalStorage);
  protected
    FGuid: TGuid;
    function GetGuid: TGuid;
    procedure SetGuid(AValue: TGuid);
    procedure NotifyChange(ADiff: TBGRAOriginalDiff = nil); overload;
    procedure NotifyChange(ABounds: TRectF; ADiff: TBGRAOriginalDiff = nil); overload;
    procedure NotifyEditorChange;
    property DiffExpected: boolean read GetDiffExpected;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //one of the two Render functions must be overriden
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); virtual;
    procedure Render(ADest: TBGRABitmap; ARenderOffset: TPoint; AMatrix: TAffineMatrix; ADraft: boolean); virtual;
    function GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix): TRect; virtual; abstract;
    procedure ConfigureEditor({%H-}AEditor: TBGRAOriginalEditor); virtual;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); virtual; abstract;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); virtual; abstract;
    procedure LoadFromFile(AFilenameUTF8: string); virtual;
    procedure LoadFromStream(AStream: TStream); virtual;
    procedure LoadFromResource(AFilename: string);
    procedure SaveToFile(AFilenameUTF8: string); virtual;
    procedure SaveToStream(AStream: TStream); virtual;
    function CreateEditor: TBGRAOriginalEditor; virtual;
    class function StorageClassName: RawByteString; virtual; abstract;
    class function CanConvertToSVG: boolean; virtual;
    function IsInfiniteSurface: boolean; virtual;
    function ConvertToSVG(const {%H-}AMatrix: TAffineMatrix; out AOffset: TPoint): TObject; virtual;
    function Duplicate: TBGRALayerCustomOriginal; virtual;
    property Guid: TGuid read GetGuid write SetGuid;
    property OnChange: TOriginalChangeEvent read FOnChange write SetOnChange;
    property OnEditingChange: TOriginalEditingChangeEvent read FOnEditingChange write FOnEditingChange;
    property RenderStorage: TBGRACustomOriginalStorage read FRenderStorage write SetRenderStorage;
  end;

  TBGRALayerImageOriginal = class;

  { TBGRAImageOriginalDiff }

  TBGRAImageOriginalDiff = class(TBGRAOriginalDiff)
  protected
    FContentVersionBefore,FContentVersionAfter: integer;
    FImageBefore,FImageAfter: TBGRABitmap;
    FJpegStreamBefore,FJpegStreamAfter: TMemoryStream;
  public
    constructor Create(AFromOriginal: TBGRALayerImageOriginal);
    destructor Destroy; override;
    procedure ComputeDiff(AToOriginal: TBGRALayerImageOriginal);
    procedure Apply(AOriginal: TBGRALayerCustomOriginal); override;
    procedure Unapply(AOriginal: TBGRALayerCustomOriginal); override;
    function CanAppend(ADiff: TBGRAOriginalDiff): boolean; override;
    procedure Append(ADiff: TBGRAOriginalDiff); override;
    function IsIdentity: boolean; override;
  end;

  { TBGRALayerImageOriginal }

  TBGRALayerImageOriginal = class(TBGRALayerCustomOriginal)
  private
    function GetImageHeight: integer;
    function GetImageWidth: integer;
  protected
    FImage: TBGRABitmap;
    FJpegStream: TMemoryStream;
    FContentVersion: integer;
    FDiff: TBGRAImageOriginalDiff;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure InternalLoadImageFromStream(AStream: TStream; AUpdate: boolean);
    procedure InternalClear;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ConvertToSVG(const AMatrix: TAffineMatrix; out AOffset: TPoint): TObject; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix): TRect; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure Clear;
    procedure LoadImageFromStream(AStream: TStream);
    procedure SaveImageToStream(AStream: TStream);
    procedure AssignImage(AImage: TBGRACustomBitmap);
    function GetImageCopy: TBGRABitmap;
    class function StorageClassName: RawByteString; override;
    class function CanConvertToSVG: boolean; override;
    property Width: integer read GetImageWidth;
    property Height: integer read GetImageHeight;
  end;

  { TBGRACustomOriginalStorage }

  TBGRACustomOriginalStorage = class
  protected
    FFormats: TFormatSettings;
    function GetBool(AName: utf8string): boolean;
    function GetBoolDef(AName: utf8string; ADefault: boolean): boolean;
    function GetColorArray(AName: UTF8String): ArrayOfTBGRAPixel;
    function GetInteger(AName: utf8string): integer;
    function GetIntegerDef(AName: utf8string; ADefault: integer): integer;
    function GetPointF(AName: utf8string): TPointF;
    function GetRectF(AName: utf8string): TRectF;
    function GetRect(AName: utf8string): TRect;
    function GetAffineMatrix(AName: utf8string): TAffineMatrix;
    function GetRawString(AName: utf8string): RawByteString; virtual; abstract;
    function GetSingle(AName: utf8string): single;
    function GetSingleArray(AName: utf8string): ArrayOfSingle;
    function GetSingleDef(AName: utf8string; ADefault: single): single;
    function GetColor(AName: UTF8String): TBGRAPixel;
    procedure SetBool(AName: utf8string; AValue: boolean);
    procedure SetColorArray(AName: UTF8String; AValue: ArrayOfTBGRAPixel);
    procedure SetInteger(AName: utf8string; AValue: integer);
    procedure SetPointF(AName: utf8string; AValue: TPointF);
    procedure SetRectF(AName: utf8string; AValue: TRectF);
    procedure SetRect(AName: utf8string; AValue: TRect);
    procedure SetAffineMatrix(AName: utf8string; const AValue: TAffineMatrix);
    procedure SetRawString(AName: utf8string; AValue: RawByteString); virtual; abstract;
    procedure SetSingle(AName: utf8string; AValue: single);
    procedure SetSingleArray(AName: utf8string; AValue: ArrayOfSingle);
    procedure SetColor(AName: UTF8String; AValue: TBGRAPixel);
    function GetDelimiter: char;
    function GetEmpty: boolean; virtual; abstract;
  public
    constructor Create;
    function Duplicate: TBGRACustomOriginalStorage; virtual; abstract;
    procedure RemoveAttribute(AName: utf8string); virtual; abstract;
    function HasAttribute(AName: utf8string): boolean; virtual; abstract;
    procedure RemoveObject(AName: utf8string); virtual; abstract;
    function CreateObject(AName: utf8string): TBGRACustomOriginalStorage; virtual; abstract;
    function OpenObject(AName: utf8string): TBGRACustomOriginalStorage; virtual; abstract;
    function ObjectExists(AName: utf8string): boolean; virtual; abstract;
    procedure EnumerateObjects(AList: TStringList); virtual; abstract;
    procedure EnumerateFiles(AList: TStringList); virtual; abstract;
    procedure RemoveFile(AName: utf8string); virtual; abstract;
    function GetFileStream(AName: UTF8String): TStream; virtual; abstract;
    function ReadFile(AName: UTF8String; ADest: TStream): boolean; virtual; abstract;
    function ReadBitmap(AName: UTF8String; ADest: TCustomUniversalBitmap): boolean; virtual; abstract;
    procedure WriteFile(AName: UTF8String; ASource: TStream; ACompress: boolean; AOwnStream: boolean = false); virtual; abstract;
    function FileExists(AName: UTF8String): boolean; virtual; abstract;
    function FloatEquals(AName: utf8string; AValue: single): boolean;
    function PointFEquals(AName: utf8string; const AValue: TPointF): boolean;
    function AffineMatrixEquals(AName: utf8string; const AValue: TAffineMatrix): boolean;
    property RawString[AName: utf8string]: RawByteString read GetRawString write SetRawString;
    property Int[AName: utf8string]: integer read GetInteger write SetInteger;
    property IntDef[AName: utf8string; ADefault: integer]: integer read GetIntegerDef;
    property Bool[AName: utf8string]: boolean read GetBool write SetBool;
    property BoolDef[AName: utf8string; ADefault: boolean]: boolean read GetBoolDef;
    property Float[AName: utf8string]: single read GetSingle write SetSingle;
    property FloatArray[AName: utf8string]: ArrayOfSingle read GetSingleArray write SetSingleArray;
    property FloatDef[AName: utf8string; ADefault: single]: single read GetSingleDef;
    property PointF[AName: utf8string]: TPointF read GetPointF write SetPointF;
    property RectangleF[AName: utf8string]: TRectF read GetRectF write SetRectF;
    property Rectangle[AName: utf8string]: TRect read GetRect write SetRect;
    property AffineMatrix[AName: utf8string]: TAffineMatrix read GetAffineMatrix write SetAffineMatrix;
    property Color[AName: UTF8String]: TBGRAPixel read GetColor write SetColor;
    property ColorArray[AName: UTF8String]: ArrayOfTBGRAPixel read GetColorArray write SetColorArray;
    property Empty: boolean read GetEmpty;
  end;

  { TBGRAMemOriginalStorage }

  TBGRAMemOriginalStorage = class(TBGRACustomOriginalStorage)
  protected
    FMemDir: TMemDirectory;
    FMemDirOwned: boolean;
    function GetRawString(AName: utf8string): RawByteString; override;
    procedure SetRawString(AName: utf8string; AValue: RawByteString); override;
    function GetEmpty: boolean; override;
  public
    destructor Destroy; override;
    constructor Create;
    constructor Create(AMemDir: TMemDirectory; AMemDirOwned: boolean = false);
    function Equals(Obj: TObject): boolean; override;
    function Duplicate: TBGRACustomOriginalStorage; override;
    procedure RemoveAttribute(AName: utf8string); override;
    function HasAttribute(AName: utf8string): boolean; override;
    procedure RemoveObject(AName: utf8string); override;
    function CreateObject(AName: utf8string): TBGRACustomOriginalStorage; override;
    function OpenObject(AName: utf8string): TBGRACustomOriginalStorage; override;
    function ObjectExists(AName: utf8string): boolean; override;
    procedure EnumerateObjects(AList: TStringList); override;
    procedure EnumerateFiles(AList: TStringList); override;
    procedure RemoveFile(AName: utf8string); override;
    function GetFileStream(AName: UTF8String): TStream; override;
    function ReadBitmap(AName: UTF8String; ADest: TCustomUniversalBitmap): boolean; override;
    function ReadFile(AName: UTF8String; ADest: TStream): boolean; override;
    procedure WriteFile(AName: UTF8String; ASource: TStream; ACompress: boolean; AOwnStream: boolean = false); override;
    function FileExists(AName: UTF8String): boolean; override;
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromResource(AFilename: string);
    procedure CopyTo(AMemDir: TMemDirectory);
  end;

procedure RegisterLayerOriginal(AClass: TBGRALayerOriginalAny);
function FindLayerOriginalClass(AStorageClassName: string): TBGRALayerOriginalAny;

implementation

uses BGRAPolygon, math, BGRAMultiFileType, BGRAUTF8, BGRAGraphics, BGRASVG, BGRASVGShapes;

{$IFDEF BGRABITMAP_USE_LCL}
function LCLKeyToSpecialKey(AKey: Word; AShift: TShiftState): TSpecialKey;
var
  sk: TSpecialKey;
begin
  if (((AKey >= VK_A) and (AKey <= VK_Z)) or
     ((AKey >= VK_0) and (AKey <= VK_9))) and (AShift*[ssCtrl,ssAlt]=[]) then exit(skUnknown);
  for sk := low(TSpecialKey) to high(TSpecialKey) do
    if AKey = SpecialKeyToLCL[sk] then exit(sk);
  exit(skUnknown);
end;
{$ENDIF}

var
  LayerOriginalClasses: array of TBGRALayerOriginalAny;

procedure RegisterLayerOriginal(AClass: TBGRALayerOriginalAny);
begin
  setlength(LayerOriginalClasses, length(LayerOriginalClasses)+1);
  LayerOriginalClasses[high(LayerOriginalClasses)] := AClass;
end;

function FindLayerOriginalClass(AStorageClassName: string): TBGRALayerOriginalAny;
var
  i: Integer;
begin
  for i := 0 to high(LayerOriginalClasses) do
    if LayerOriginalClasses[i].StorageClassName = AStorageClassName then
      exit(LayerOriginalClasses[i]);
  exit(nil);
end;

{ TBGRAImageOriginalDiff }

constructor TBGRAImageOriginalDiff.Create(AFromOriginal: TBGRALayerImageOriginal);
begin
  FImageBefore := AFromOriginal.FImage.NewReference;
  if Assigned(AFromOriginal.FJpegStream) then
  begin
    FJpegStreamBefore := TMemoryStream.Create;
    AFromOriginal.FJpegStream.Position:= 0;
    FJpegStreamBefore.CopyFrom(AFromOriginal.FJpegStream, AFromOriginal.FJpegStream.Size);
  end;
  FContentVersionBefore:= AFromOriginal.FContentVersion;
end;

procedure TBGRAImageOriginalDiff.ComputeDiff(
  AToOriginal: TBGRALayerImageOriginal);
begin
  if Assigned(FImageAfter) then FImageAfter.FreeReference;
  FImageAfter := AToOriginal.FImage.NewReference;
  FreeAndNil(FJpegStreamAfter);
  if Assigned(AToOriginal.FJpegStream) then
  begin
    FJpegStreamAfter := TMemoryStream.Create;
    AToOriginal.FJpegStream.Position:= 0;
    FJpegStreamAfter.CopyFrom(AToOriginal.FJpegStream, AToOriginal.FJpegStream.Size);
  end;
  FContentVersionAfter:= AToOriginal.FContentVersion;
end;

procedure TBGRAImageOriginalDiff.Apply(AOriginal: TBGRALayerCustomOriginal);
var
  orig: TBGRALayerImageOriginal;
begin
  orig := AOriginal as TBGRALayerImageOriginal;
  orig.FImage.FreeReference;
  orig.FImage := FImageAfter.NewReference;
  FreeAndNil(orig.FJpegStream);
  if Assigned(FJpegStreamAfter) then
  begin
    orig.FJpegStream := TMemoryStream.Create;
    FJpegStreamAfter.Position := 0;
    orig.FJpegStream.CopyFrom(FJpegStreamAfter, FJpegStreamAfter.Size);
  end;
  orig.FContentVersion := FContentVersionAfter;
end;

procedure TBGRAImageOriginalDiff.Unapply(AOriginal: TBGRALayerCustomOriginal);
var
  orig: TBGRALayerImageOriginal;
begin
  orig := AOriginal as TBGRALayerImageOriginal;
  orig.FImage.FreeReference;
  orig.FImage := FImageBefore.NewReference;
  FreeAndNil(orig.FJpegStream);
  if Assigned(FJpegStreamBefore) then
  begin
    orig.FJpegStream := TMemoryStream.Create;
    FJpegStreamBefore.Position := 0;
    orig.FJpegStream.CopyFrom(FJpegStreamBefore, FJpegStreamBefore.Size);
  end;
  orig.FContentVersion := FContentVersionBefore;
end;

function TBGRAImageOriginalDiff.CanAppend(ADiff: TBGRAOriginalDiff): boolean;
begin
  result := (ADiff is TBGRAImageOriginalDiff) and
    (TBGRAImageOriginalDiff(ADiff).FContentVersionAfter >= FContentVersionAfter);
end;

procedure TBGRAImageOriginalDiff.Append(ADiff: TBGRAOriginalDiff);
var
  next: TBGRAImageOriginalDiff;
begin
  next := ADiff as TBGRAImageOriginalDiff;
  if next.FContentVersionAfter < FContentVersionAfter then
    raise exception.Create('Cannot append diff made before this one.');
  FImageAfter.FreeReference;
  FImageAfter := next.FImageAfter.NewReference;
  FreeAndNil(FJpegStreamAfter);
  if Assigned(next.FJpegStreamAfter) then
  begin
    FJpegStreamAfter := TMemoryStream.Create;
    next.FJpegStreamAfter.Position:= 0;
    FJpegStreamAfter.CopyFrom(next.FJpegStreamAfter, next.FJpegStreamAfter.Size);
  end;
  FContentVersionAfter:= next.FContentVersionAfter;
end;

function TBGRAImageOriginalDiff.IsIdentity: boolean;
begin
  result := FImageBefore.Equals(FImageAfter) and
    ( ((FJpegStreamBefore=nil) and (FJpegStreamAfter=nil)) or
      (Assigned(FJpegStreamBefore) and Assigned(FJpegStreamAfter) and
       (FJpegStreamBefore.Size = FJpegStreamAfter.Size) and
       CompareMem(FJpegStreamBefore.Memory,FJpegStreamBefore.Memory,FJpegStreamBefore.Size)) );

end;

destructor TBGRAImageOriginalDiff.Destroy;
begin
  FImageBefore.FreeReference;
  FImageAfter.FreeReference;
  FJpegStreamBefore.Free;
  FJpegStreamAfter.Free;
  inherited Destroy;
end;

{ TBGRAOriginalEditor }

procedure TBGRAOriginalEditor.SetMatrix(AValue: TAffineMatrix);
begin
  if FMatrix=AValue then Exit;
  FMatrix:=AValue;
  FMatrixInverse := AffineMatrixInverse(FMatrix);
end;

function TBGRAOriginalEditor.GetMoveCursor(APointIndex: integer): TOriginalEditorCursor;
var
  d: TPointF;
  ratio: single;
begin
  if (APointIndex < 0) or (APointIndex >= PointCount) then result := oecDefault else
  if isEmptyPointF(FPoints[APointIndex].Origin) then
  begin
    if Assigned(FPoints[APointIndex].OnMove) then
      result := oecMove
    else
      result := oecHandPoint;
  end else
  begin
    d := AffineMatrixLinear(FMatrix)*(FPoints[APointIndex].Coord - FPoints[APointIndex].Origin);
    ratio := sin(Pi/8);
    if (d.x = 0) and (d.y = 0) then result := oecMove else
    if abs(d.x)*ratio >= abs(d.y) then
    begin
      if d.x >= 0 then result := oecMoveE else result := oecMoveW
    end else
    if abs(d.y)*ratio >= abs(d.x) then
    begin
      if d.y >= 0 then result := oecMoveS else result := oecMoveN
    end else
    if (d.x > 0) and (d.y > 0) then result := oecMoveSE else
    if (d.x < 0) and (d.y < 0) then result := oecMoveNW else
    if (d.x > 0) and (d.y < 0) then result := oecMoveNE
    else result := oecMoveSW;
  end;
end;

function TBGRAOriginalEditor.GetFixedShiftForButton(AShift: TShiftState;
  ARightDown: boolean): TShiftState;
begin
  result := AShift - [ssLeft,ssMiddle,ssRight];
  if ARightDown then include(result, ssRight)
  else include(result, ssLeft);
end;

function TBGRAOriginalEditor.GetPointCoord(AIndex: integer): TPointF;
begin
  if (AIndex < 0) or (AIndex >= PointCount) then raise exception.Create('Index out of bounds');
  result := FPoints[AIndex].Coord;
end;

function TBGRAOriginalEditor.GetIsMovingPoint: boolean;
begin
  result := FPointMoving <> -1;
end;

function TBGRAOriginalEditor.GetPointCount: integer;
begin
  result := length(FPoints);
end;

function TBGRAOriginalEditor.GetPointHighlighted(AIndex: integer): boolean;
begin
  if (AIndex < 0) or (AIndex >= PointCount) then raise exception.Create('Index out of bounds');
  result := FPoints[AIndex].Highlighted;
end;

procedure TBGRAOriginalEditor.SetFocused(AValue: boolean);
begin
  if FFocused=AValue then Exit;
  FFocused:=AValue;
  if Assigned(FOnFocusChanged) then FOnFocusChanged(self);
end;

procedure TBGRAOriginalEditor.SetPointHighlighted(AIndex: integer;
  AValue: boolean);
begin
  if (AIndex < 0) or (AIndex >= PointCount) then raise exception.Create('Index out of bounds');
  FPoints[AIndex].Highlighted := AValue;
end;

procedure TBGRAOriginalEditor.SetGridActive(AValue: boolean);
begin
  if FGridActive=AValue then Exit;
  FGridActive:=AValue;
end;

procedure TBGRAOriginalEditor.SetGridMatrix(AValue: TAffineMatrix);
begin
  if FGridMatrix=AValue then Exit;
  FGridMatrix:=AValue;
  FGridMatrixInverse := AffineMatrixInverse(FGridMatrix);
end;

function TBGRAOriginalEditor.RenderPoint(ADest: TBGRABitmap; ACoord: TPointF; AAlternateColor: boolean; AHighlighted: boolean): TRect;
const alpha = 192;
var filler: TBGRAMultishapeFiller;
  c: TBGRAPixel;
  penScale: Single;
  oldClip: TRect;
begin
  result := GetRenderPointBounds(ACoord, AHighlighted);
  if not isEmptyPointF(ACoord) then
  begin
    oldClip := ADest.ClipRect;
    ADest.IntersectClip(result);
    if AAlternateColor then c := BGRA(255,128,128,alpha)
      else if AHighlighted then c := BGRA(96,170,255,alpha)
      else c := BGRA(255,255,255,alpha);
    if AHighlighted then
      ADest.GradientFill(result.Left, result.Top, result.Right, result.Bottom,
                  c, BGRAPixelTransparent,
                  gtRadial, PointF(ACoord.x,ACoord.y), PointF(result.right,ACoord.y),
                  dmDrawWithTransparency);
    penScale := FPointSize / 6;
    if penScale < 1 then penScale := 1;
    filler := TBGRAMultishapeFiller.Create;
    filler.AddEllipseBorder(ACoord.x,ACoord.y, FPointSize-2,FPointSize-2, 3.5*penScale, BGRA(0,0,0,alpha));
    filler.AddEllipseBorder(ACoord.x,ACoord.y, FPointSize-2,FPointSize-2, 1*penScale, c);
    filler.PolygonOrder:= poLastOnTop;
    filler.Draw(ADest);
    filler.Free;
    ADest.ClipRect := oldClip;
  end;
end;

function TBGRAOriginalEditor.GetRenderPointBounds(ACoord: TPointF; AHighlighted: boolean): TRect;
var
  r, penScale: Single;
begin
  if isEmptyPointF(ACoord) then
    result := EmptyRect
  else
  begin
    penScale := FPointSize / 6;
    if penScale < 1 then penScale := 1;
    r := FPointSize + (penScale-1)*4;
    if AHighlighted then r := max(r, FPointSize*2);
    result := rect(floor(ACoord.x - r + 0.5), floor(ACoord.y - r + 0.5),
                   ceil(ACoord.x + r + 0.5), ceil(ACoord.y + r + 0.5));
  end;
end;

function TBGRAOriginalEditor.RenderArrow(ADest: TBGRABitmap; AOrigin,
  AEndCoord: TPointF): TRect;
const alpha = 192;
var
  pts, ptsContour: ArrayOfTPointF;
  i: Integer;
  rF: TRectF;
  penScale: Single;
begin
  if isEmptyPointF(AOrigin) or isEmptyPointF(AEndCoord) then
    result := EmptyRect
  else
  begin
    penScale := FPointSize / 6;
    if penScale < 1 then penScale := 1;
    ADest.Pen.Arrow.EndAsClassic;
    ADest.Pen.Arrow.EndSize := PointF(FPointSize/penScale,FPointSize/penScale);
    pts := ADest.ComputeWidePolyline([AOrigin,AEndCoord],1*penScale);
    ADest.Pen.Arrow.EndAsNone;
    ptsContour := ADest.ComputeWidePolygon(pts, 2*penScale);
    ADest.FillPolyAntialias(ptsContour, BGRA(0,0,0,alpha));
    ADest.FillPolyAntialias(pts, BGRA(255,255,255,alpha));
    rF := RectF(AOrigin,AEndCoord);
    for i := 0 to high(ptsContour) do
    if not isEmptyPointF(ptsContour[i]) then
    begin
      if ptsContour[i].x < rF.Left then rF.Left := ptsContour[i].x;
      if ptsContour[i].x > rF.Right then rF.Right := ptsContour[i].x;
      if ptsContour[i].y < rF.Top then rF.Top := ptsContour[i].y;
      if ptsContour[i].y > rF.Bottom then rF.Bottom := ptsContour[i].y;
    end;
    result := rect(floor(rF.Left+0.5),floor(rF.Top+0.5),ceil(rF.Right+0.5),ceil(rF.Bottom+0.5));
  end;
end;

function TBGRAOriginalEditor.GetRenderArrowBounds(AOrigin, AEndCoord: TPointF): TRect;
var
  penScale, margin: Single;
begin
  if isEmptyPointF(AOrigin) or isEmptyPointF(AEndCoord) then
    result := EmptyRect
  else
  begin
    penScale := FPointSize / 6;
    if penScale < 1 then penScale := 1;
    margin := penScale * 1.5;
    result := Rect(floor(AOrigin.x+0.5-margin),floor(AOrigin.y+0.5-margin),
      ceil(AOrigin.x+0.5+margin),ceil(AOrigin.y+0.5+margin));
    result.Union( rect(floor(AEndCoord.x+0.5-FPointSize-margin), floor(AEndCoord.y+0.5-FPointSize-margin),
                      ceil(AEndCoord.x+0.5+FPointSize+margin), ceil(AEndCoord.y+0.5+FPointSize+margin)) );
  end;
end;

function TBGRAOriginalEditor.RenderPolygon(ADest: TBGRABitmap;
  ACoords: array of TPointF; AClosed: boolean; AStyle: TBGRAOriginalPolylineStyle; ABackColor: TBGRAPixel): TRect;
var
  dashLen: integer;
  i: integer;
  ptsF: array of TPointF;
  pts1,pts2: array of TPoint;
begin
  dashLen := round(PointSize/2);
  if dashLen < 1 then dashLen := 1;

  setlength(pts1, length(ACoords));
  for i := 0 to high(ACoords) do
    pts1[i] := ACoords[i].Round;

  setlength(ptsF, length(pts1));
  for i := 0 to high(pts1) do
    ptsF[i] := PointF(pts1[i]);

  if ABackColor.alpha <> 0 then
    ADest.FillPolyAntialias(ptsF, ABackColor);

  case AStyle of
  opsDash, opsDashWithShadow:
    begin
      if AStyle = opsDashWithShadow then
      begin
        //shadow
        setlength(pts2,length(pts1));
        for i := 0 to high(pts1) do
          if not isEmptyPoint(pts1[i]) then
            pts2[i] := Point(pts1[i].x+1,pts1[i].y+1)
          else pts2[i] := EmptyPoint;
        if AClosed then
          ADest.DrawPolygonAntialias(pts2, BGRA(0,0,0,96))
        else
          ADest.DrawPolyLineAntialias(pts2, BGRA(0,0,0,96), true);
        pts2:= nil;
      end;

      //dotted line
      if AClosed then
        ADest.DrawPolygonAntialias(pts1, CSSIvory,BGRA(70,70,50),dashLen)
      else
        ADest.DrawPolyLineAntialias(pts1, CSSIvory,BGRA(70,70,50),dashLen, true);
    end;
  opsSolid:
    begin
      ADest.JoinStyle:= pjsRound;
      ADest.LineCap:= pecRound;
      //black outline
      if AClosed then
        ADest.DrawPolygonAntialias(ptsF, BGRA(0,0,0,192), 3)
      else
        ADest.DrawPolyLineAntialias(ptsF, BGRA(0,0,0,192), 3);

      if AClosed then
        ADest.DrawPolygonAntialias(pts1, CSSIvory)
      else
        ADest.DrawPolyLineAntialias(pts1, CSSIvory, true);
    end;
  end;

  result := GetRenderPolygonBounds(ACoords);
end;

function TBGRAOriginalEditor.GetRenderPolygonBounds(ACoords: array of TPointF): TRect;
var
  first: Boolean;
  rF: TRectF;
  i: Integer;
begin
  first:= true;
  rF:= EmptyRectF;
  for i := 0 to high(ACoords) do
    if not isEmptyPointF(ACoords[i]) then
    begin
      if first then
      begin
        rF := RectF(Acoords[i],ACoords[i]);
        first:= false;
      end else
      begin
        if ACoords[i].x < rF.Left then rF.Left := ACoords[i].x;
        if ACoords[i].x > rF.Right then rF.Right := ACoords[i].x;
        if ACoords[i].y < rF.Top then rF.Top := ACoords[i].y;
        if ACoords[i].y > rF.Bottom then rF.Bottom := ACoords[i].y;
      end;
    end;
  if not first then
    result := rect(floor(rF.Left-0.5),floor(rF.Top-0.5),ceil(rF.Right+1.5),ceil(rF.Bottom+1.5))
  else
    result := EmptyRect;
end;

constructor TBGRAOriginalEditor.Create;
begin
  FPointSize:= 6;
  FMatrix := AffineMatrixIdentity;
  FMatrixInverse := AffineMatrixIdentity;
  FGridMatrix := AffineMatrixIdentity;
  FGridMatrixInverse := AffineMatrixIdentity;
  FGridActive:= false;
  FPointMoving:= -1;
  FStartMoveHandlers := TStartMoveHandlers.Create;
  FCurHoverPoint:= -1;
  FHoverPointHandlers := THoverPointHandlers.Create;
  FClickPointHandlers := TClickPointHandlers.Create;
end;

destructor TBGRAOriginalEditor.Destroy;
begin
  FreeAndNil(FStartMoveHandlers);
  FreeAndNil(FHoverPointHandlers);
  FreeAndNil(FClickPointHandlers);
  inherited Destroy;
end;

procedure TBGRAOriginalEditor.Clear;
begin
  FPoints := nil;
  FPolylines := nil;
  FStartMoveHandlers.Clear;
  FHoverPointHandlers.Clear;
  FClickPointHandlers.Clear;
end;

procedure TBGRAOriginalEditor.AddStartMoveHandler(
  AOnStartMove: TOriginalStartMovePointEvent);
begin
  FStartMoveHandlers.Add(AOnStartMove);
end;

procedure TBGRAOriginalEditor.AddClickPointHandler(
  AOnClickPoint: TOriginalClickPointEvent);
begin
  FClickPointHandlers.Add(AOnClickPoint);
end;

procedure TBGRAOriginalEditor.AddHoverPointHandler(
  AOnHoverPoint: TOriginalHoverPointEvent);
begin
  FHoverPointHandlers.Add(AOnHoverPoint);
end;

function TBGRAOriginalEditor.AddPoint(const ACoord: TPointF;
  AOnMove: TOriginalMovePointEvent; ARightButton: boolean; ASnapToPoint: integer): integer;
begin
  setlength(FPoints, length(FPoints)+1);
  result := High(FPoints);
  with FPoints[result] do
  begin
    Origin := EmptyPointF;
    Coord := ACoord;
    OnMove := AOnMove;
    OnAlternateMove:= nil;
    RightButton:= ARightButton;
    SnapToPoint:= ASnapToPoint;
    HitBox := TAffineBox.EmptyBox;
  end;
end;

procedure TBGRAOriginalEditor.AddPointAlternateMove(AIndex: integer;
  AOnAlternateMove: TOriginalMovePointEvent);
begin
  if (AIndex >= 0) and (AIndex < PointCount) then
    FPoints[AIndex].OnAlternateMove:= AOnAlternateMove;
end;

function TBGRAOriginalEditor.AddFixedPoint(const ACoord: TPointF;
  ARightButton: boolean): integer;
begin
  setlength(FPoints, length(FPoints)+1);
  result := High(FPoints);
  with FPoints[result] do
  begin
    Origin := EmptyPointF;
    Coord := ACoord;
    OnMove := nil;
    OnAlternateMove:= nil;
    RightButton:= ARightButton;
    Highlighted:= false;
    SnapToPoint:= -1;
    HitBox := TAffineBox.EmptyBox;
  end;
end;

function TBGRAOriginalEditor.AddArrow(const AOrigin, AEndCoord: TPointF;
  AOnMoveEnd: TOriginalMovePointEvent; ARightButton: boolean): integer;
begin
  setlength(FPoints, length(FPoints)+1);
  result := High(FPoints);
  with FPoints[result] do
  begin
    Origin := AOrigin;
    Coord := AEndCoord;
    OnMove := AOnMoveEnd;
    OnAlternateMove:= nil;
    RightButton:= ARightButton;
    Highlighted:= false;
    SnapToPoint:= -1;
    HitBox := TAffineBox.EmptyBox;
  end;
end;

function TBGRAOriginalEditor.AddPolyline(const ACoords: array of TPointF;
  AClosed: boolean; AStyle: TBGRAOriginalPolylineStyle): integer;
begin
  result := AddPolyline(ACoords, AClosed, AStyle, BGRAPixelTransparent);
end;

function TBGRAOriginalEditor.AddPolyline(const ACoords: array of TPointF;
  AClosed: boolean; AStyle: TBGRAOriginalPolylineStyle; ABackColor: TBGRAPixel): integer;
var
  i: Integer;
begin
  setlength(FPolylines, length(FPolylines)+1);
  result := high(FPolylines);
  setlength(FPolylines[result].Coords, length(ACoords));
  for i := 0 to high(ACoords) do
    FPolylines[result].Coords[i] := ACoords[i];
  FPolylines[result].Closed:= AClosed;
  FPolylines[result].Style := AStyle;
  FPolylines[result].BackColor := ABackColor;
end;

procedure TBGRAOriginalEditor.SetHitBox(AIndex: integer; AHitBox: TAffineBox);
begin
  if (AIndex < 0) or (AIndex >= PointCount) then raise exception.Create('Index out of bounds');
  FPoints[AIndex].HitBox := AHitBox;
end;

procedure TBGRAOriginalEditor.MouseMove(Shift: TShiftState; ViewX, ViewY: single; out
  ACursor: TOriginalEditorCursor; out AHandled: boolean);
var newMousePos, newCoord, snapCoord: TPointF;
  hoverPoint, i: Integer;
  subShift: TShiftState;
begin
  AHandled := false;
  newMousePos := ViewCoordToOriginal(PointF(ViewX,ViewY));
  if (FPointMoving <> -1) and (FPointMoving < length(FPoints)) then
  begin
    newCoord := newMousePos + FPointCoordDelta;
    if GridActive then newCoord := SnapToGrid(newCoord, false);
    if FPoints[FPointMoving].SnapToPoint <> -1 then
    begin
      snapCoord := FPoints[FPoints[FPointMoving].SnapToPoint].Coord;
      if VectLen(AffineMatrixLinear(FMatrix)*(snapCoord - newCoord)) < FPointSize then
        newCoord := snapCoord;
    end;
    if newCoord <> FPoints[FPointMoving].Coord then
    begin
      FPointWasMoved:= true;
      subShift := GetFixedShiftForButton(Shift, FMovingRightButton);
      if (FMovingRightButton xor FPoints[FPointMoving].RightButton) and
        Assigned(FPoints[FPointMoving].OnAlternateMove) then
        FPoints[FPointMoving].OnAlternateMove(self, FPoints[FPointMoving].Coord, newCoord, subShift)
      else
        FPoints[FPointMoving].OnMove(self, FPoints[FPointMoving].Coord, newCoord, subShift);
      if (FPointMoving >= 0) and (FPointMoving < length(FPoints)) then
        FPoints[FPointMoving].Coord := newCoord
      else
        FPointMoving := -1;
    end;
    ACursor := GetMoveCursor(FPointMoving);
    AHandled:= true;
  end else
  begin
    hoverPoint := GetPointAt(newMousePos, false);
    if hoverPoint <> -1 then
      ACursor := GetMoveCursor(hoverPoint)
    else
      ACursor:= oecDefault;
    if hoverPoint <> FCurHoverPoint then
    begin
      FCurHoverPoint:= hoverPoint;
      for i := 0 to FHoverPointHandlers.Count-1 do
        FHoverPointHandlers[i](self, FCurHoverPoint);
    end;
  end;
  FPrevMousePos:= newMousePos;
end;

procedure TBGRAOriginalEditor.MouseDown(RightButton: boolean;
  Shift: TShiftState; ViewX, ViewY: single; out ACursor: TOriginalEditorCursor; out
  AHandled: boolean);
var
  i, clickedPoint: Integer;
  subShift: TShiftState;
begin
  AHandled:= false;
  FPrevMousePos:= ViewCoordToOriginal(PointF(ViewX,ViewY));
  if FPointMoving = -1 then
  begin
    clickedPoint := GetPointAt(FPrevMousePos, RightButton);
    if clickedPoint <> -1 then
    begin
      subShift := GetFixedShiftForButton(Shift, RightButton);
      if Assigned(FPoints[clickedPoint].OnMove) then
      begin
        FPointMoving:= clickedPoint;
        FPointWasMoved:= false;
        FMovingRightButton:= RightButton;
        FPointCoordDelta := FPoints[FPointMoving].Coord - FPrevMousePos;
        for i := 0 to FStartMoveHandlers.Count-1 do
          FStartMoveHandlers[i](self, FPointMoving, subShift);
      end else
      begin
        for i := 0 to FClickPointHandlers.Count-1 do
          FClickPointHandlers[i](self, clickedPoint, subShift);
      end;
      AHandled:= true;
    end;
  end;
  if FPointMoving <> -1 then
  begin
    ACursor := GetMoveCursor(FPointMoving);
    AHandled:= true;
  end
  else
    ACursor := oecDefault;
end;

procedure TBGRAOriginalEditor.MouseUp(RightButton: boolean; Shift: TShiftState;
  ViewX, ViewY: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean);
var
  i: Integer;
  subShift: TShiftState;
begin
  AHandled:= false;
  if (RightButton = FMovingRightButton) and (FPointMoving <> -1) then
  begin
    if not FPointWasMoved then
    begin
      subShift := GetFixedShiftForButton(Shift, RightButton);
      for i := 0 to FClickPointHandlers.Count-1 do
        FClickPointHandlers[i](self, FPointMoving, subShift);
    end;
    FPointMoving:= -1;
    AHandled:= true;
  end;
  ACursor := oecDefault;
end;

procedure TBGRAOriginalEditor.KeyDown(Shift: TShiftState; Key: TSpecialKey; out
  AHandled: boolean);
begin
  AHandled := false;
end;

procedure TBGRAOriginalEditor.KeyUp(Shift: TShiftState; Key: TSpecialKey; out
  AHandled: boolean);
begin
  AHandled := false;
end;

procedure TBGRAOriginalEditor.KeyPress(UTF8Key: string; out AHandled: boolean);
begin
  AHandled := false;
end;

function TBGRAOriginalEditor.GetPointAt(const ACoord: TPointF; ARightButton: boolean): integer;
var v: TPointF;
  curDist,newDist: single;
  i: Integer;
  transfCoord: TPointF;
begin
  if ARightButton then
    curDist := sqr(2.25*FPointSize)
  else
    curDist := sqr(1.25*FPointSize);
  result := -1;
  transfCoord:= Matrix*ACoord;

  for i := 0 to high(FPoints) do
  if FPoints[i].RightButton = ARightButton then
  begin
    v := Matrix*FPoints[i].Coord - transfCoord;
    newDist := v*v;
    if newDist <= curDist then
    begin
      curDist:= newDist;
      result := i;
    end;
  end;
  if result <> -1 then exit;

  if not ARightButton then
    curDist := sqr(2.25*FPointSize)
  else
    curDist := sqr(1.25*FPointSize);
  for i := 0 to high(FPoints) do
  if FPoints[i].RightButton <> ARightButton then
  begin
    v := Matrix*FPoints[i].Coord - transfCoord;
    newDist := v*v;
    if newDist <= curDist then
    begin
      curDist:= newDist;
      result := i;
    end;
  end;

  for i := 0 to high(FPoints) do
  if (FPoints[i].RightButton = ARightButton)
    and FPoints[i].HitBox.Contains(ACoord) then exit(i);

  for i := 0 to high(FPoints) do
  if (FPoints[i].RightButton <> ARightButton)
    and FPoints[i].HitBox.Contains(ACoord) then exit(i);
end;

function TBGRAOriginalEditor.Render(ADest: TBGRABitmap; const ALayoutRect: TRect): TRect;
var
  i,j: Integer;
  elemRect: TRect;
  ptsF: array of TPointF;
begin
  result := EmptyRect;
  for i := 0 to high(FPoints) do
  begin
    if isEmptyPointF(FPoints[i].Origin) then
      elemRect := RenderPoint(ADest, OriginalCoordToView(FPoints[i].Coord), FPoints[i].RightButton, FPoints[i].Highlighted)
    else
      elemRect := RenderArrow(ADest, OriginalCoordToView(FPoints[i].Origin), OriginalCoordToView(FPoints[i].Coord));
    if not elemRect.IsEmpty then
    begin
      if result.IsEmpty then
        result := elemRect
      else
        result.Union(elemRect);
    end;
  end;
  for i := 0 to high(FPolylines) do
  begin
    with FPolylines[i] do
    begin
      setlength(ptsF, length(Coords));
      for j := 0 to high(Coords) do
        if IsEmptyPointF(Coords[j]) then
          ptsF[j] := EmptyPointF
        else
          ptsF[j] := OriginalCoordToView(Coords[j]);
      elemRect := RenderPolygon(ADest, ptsF, Closed, Style, BackColor);
    end;
    if not elemRect.IsEmpty then
    begin
      if result.IsEmpty then
        result := elemRect
      else
        result.Union(elemRect);
    end;
  end;
end;

function TBGRAOriginalEditor.GetRenderBounds(const ALayoutRect: TRect): TRect;
var
  i,j: Integer;
  elemRect: TRect;
  ptsF: array of TPointF;
begin
  result := EmptyRect;
  for i := 0 to high(FPoints) do
  begin
    if isEmptyPointF(FPoints[i].Origin) then
      elemRect := GetRenderPointBounds(OriginalCoordToView(FPoints[i].Coord), FPoints[i].Highlighted)
    else
      elemRect := GetRenderArrowBounds(OriginalCoordToView(FPoints[i].Origin), OriginalCoordToView(FPoints[i].Coord));
    if not elemRect.IsEmpty then
    begin
      if result.IsEmpty then
        result := elemRect
      else
        result.Union(elemRect);
    end;
  end;
  for i := 0 to high(FPolylines) do
  begin
    with FPolylines[i] do
    begin
      setlength(ptsF, length(Coords));
      for j := 0 to high(Coords) do
        if IsEmptyPointF(Coords[j]) then
          ptsF[j] := EmptyPointF
        else
          ptsF[j] := OriginalCoordToView(Coords[j]);
      elemRect := GetRenderPolygonBounds(ptsF);
    end;
    if not elemRect.IsEmpty then
    begin
      if result.IsEmpty then
        result := elemRect
      else
        result.Union(elemRect);
    end;
  end;
end;

function TBGRAOriginalEditor.SnapToGrid(const ACoord: TPointF;
  AIsViewCoord: boolean): TPointF;
var
  gridCoord: TPointF;
begin
  if AIsViewCoord then
    gridCoord := FGridMatrixInverse*ViewCoordToOriginal(ACoord)
  else
    gridCoord := FGridMatrixInverse*ACoord;
  gridCoord.x := round(gridCoord.x);
  gridCoord.y := round(gridCoord.y);
  result := FGridMatrix*gridCoord;
  if AIsViewCoord then
    result := OriginalCoordToView(result);
end;

function TBGRAOriginalEditor.OriginalCoordToView(const AImageCoord: TPointF): TPointF;
begin
  result := FMatrix*AImageCoord;
end;

function TBGRAOriginalEditor.ViewCoordToOriginal(const AViewCoord: TPointF): TPointF;
begin
  result := FMatrixInverse*AViewCoord;
end;

{ TBGRAMemOriginalStorage }

function TBGRAMemOriginalStorage.GetRawString(AName: utf8string): RawByteString;
var
  idx: Integer;
begin
  if pos('.',AName)<>0 then exit('');
  idx := FMemDir.IndexOf(AName,'',true);
  if idx = -1 then
    result := ''
  else if FMemDir.IsDirectory[idx] then
    raise exception.Create('This name refers to an object and not an attribute')
  else
    result := FMemDir.RawString[idx];
end;

procedure TBGRAMemOriginalStorage.SetRawString(AName: utf8string;
  AValue: RawByteString);
var
  idx: Integer;
begin
  if pos('.',AName)<>0 then
    raise exception.Create('Attribute name cannot contain "."');
  idx := FMemDir.IndexOf(AName,'',true);
  if idx = -1 then
    FMemDir.Add(AName,'',AValue)
  else if FMemDir.IsDirectory[idx] then
    raise exception.Create('This name refers to an existing object and so cannot be an attribute')
  else
    FMemDir.RawString[idx] := AValue;
end;

function TBGRAMemOriginalStorage.GetEmpty: boolean;
begin
  result := FMemDir.Count = 0;
end;

destructor TBGRAMemOriginalStorage.Destroy;
begin
  if FMemDirOwned then FreeAndNil(FMemDir);
  inherited Destroy;
end;

constructor TBGRAMemOriginalStorage.Create;
begin
  inherited Create;
  FMemDir := TMemDirectory.Create;
  FMemDirOwned:= true;
end;

constructor TBGRAMemOriginalStorage.Create(AMemDir: TMemDirectory; AMemDirOwned: boolean = false);
begin
  inherited Create;
  FMemDir := AMemDir;
  FMemDirOwned:= AMemDirOwned;
end;

function TBGRAMemOriginalStorage.Equals(Obj: TObject): boolean;
var
  other: TBGRAMemOriginalStorage;
begin
  if not (Obj is TBGRAMemOriginalStorage) then exit(false);
  other := TBGRAMemOriginalStorage(obj);
  result := FMemDir.Equals(other.FMemDir);
end;

function TBGRAMemOriginalStorage.Duplicate: TBGRACustomOriginalStorage;
begin
  result := TBGRAMemOriginalStorage.Create;
  CopyTo(TBGRAMemOriginalStorage(result).FMemDir);
end;

procedure TBGRAMemOriginalStorage.RemoveAttribute(AName: utf8string);
var
  idx: Integer;
begin
  if pos('.',AName)<>0 then exit;
  idx := FMemDir.IndexOf(AName,'',true);
  if idx = -1 then exit
  else if FMemDir.IsDirectory[idx] then
    raise exception.Create('This name refers to an object and not an attribute')
  else
    FMemDir.Delete(idx);
end;

function TBGRAMemOriginalStorage.HasAttribute(AName: utf8string): boolean;
var
  idx: Integer;
begin
  if pos('.',AName)<>0 then exit(false);
  idx := FMemDir.IndexOf(AName,'',true);
  if idx = -1 then exit(false)
  else exit(not FMemDir.IsDirectory[idx]);
end;

procedure TBGRAMemOriginalStorage.RemoveObject(AName: utf8string);
var
  idx: Integer;
begin
  idx := FMemDir.IndexOf(EntryFilename(AName));
  if idx = -1 then exit
  else if not FMemDir.IsDirectory[idx] then
    raise exception.Create('This name refers to an attribute and not an object')
  else
    FMemDir.Delete(idx);
end;

function TBGRAMemOriginalStorage.CreateObject(AName: utf8string): TBGRACustomOriginalStorage;
var
  dirIdx: Integer;
begin
  if pos('.',AName)<>0 then
    raise exception.Create('An object cannot contain "."');
  RemoveObject(AName);
  dirIdx := FMemDir.AddDirectory(AName,'');
  result := TBGRAMemOriginalStorage.Create(FMemDir.Directory[dirIdx]);
end;

function TBGRAMemOriginalStorage.OpenObject(AName: utf8string): TBGRACustomOriginalStorage;
var
  dir: TMemDirectory;
begin
  if pos('.',AName)<>0 then
    raise exception.Create('An object cannot contain "."');
  dir := FMemDir.FindPath(AName);
  if dir = nil then
    result := nil
  else
    result := TBGRAMemOriginalStorage.Create(dir);
end;

function TBGRAMemOriginalStorage.ObjectExists(AName: utf8string): boolean;
var
  dir: TMemDirectory;
begin
  if pos('.',AName)<>0 then exit(false);
  dir := FMemDir.FindPath(AName);
  result:= Assigned(dir);
end;

procedure TBGRAMemOriginalStorage.EnumerateObjects(AList: TStringList);
var
  i: Integer;
begin
  for i := 0 to FMemDir.Count-1 do
    if FMemDir.IsDirectory[i] then
      AList.Add(FMemDir.Entry[i].Name);
end;

procedure TBGRAMemOriginalStorage.EnumerateFiles(AList: TStringList);
var
  i: Integer;
begin
  for i := 0 to FMemDir.Count-1 do
    if not FMemDir.IsDirectory[i] then
      AList.Add(FMemDir.Entry[i].Name);
end;

procedure TBGRAMemOriginalStorage.RemoveFile(AName: utf8string);
var
  idx: Integer;
begin
  idx := FMemDir.IndexOf(EntryFilename(AName));
  if idx = -1 then exit
  else if FMemDir.IsDirectory[idx] then
    raise exception.Create('This name refers to an object and not a file')
  else
    FMemDir.Delete(idx);
end;

function TBGRAMemOriginalStorage.GetFileStream(AName: UTF8String): TStream;
var
  entryId: Integer;
begin
  entryId := FMemDir.IndexOf(EntryFilename(AName));
  if (entryId <> -1) and not FMemDir.IsDirectory[entryId] then
  begin
    with FMemDir.Entry[entryId] do
      result := GetStream;
  end
  else
    result := nil;
end;

function TBGRAMemOriginalStorage.ReadBitmap(AName: UTF8String;
  ADest: TCustomUniversalBitmap): boolean;
var
  entryId: Integer;
  stream: TStream;
begin
  entryId := FMemDir.IndexOf(EntryFilename(AName));
  if (entryId <> -1) and not FMemDir.IsDirectory[entryId] then
  begin
    if FMemDir.IsEntryCompressed[entryId] then
    begin
      stream := TMemoryStream.Create;
      try
        with FMemDir.Entry[entryId] do
        begin
          if CopyTo(stream) <> FileSize then
            result := false
          else
          begin
            stream.Position:= 0;
            ADest.LoadFromStream(stream);
            result := true;
          end;
        end;
      finally
        stream.Free;
      end;
    end else
    with FMemDir.Entry[entryId] do
    begin
      stream := GetStream;
      stream.Position:= 0;
      ADest.LoadFromStream(stream);
      result := true;
    end;
  end
  else
    result := false;
end;

function TBGRAMemOriginalStorage.ReadFile(AName: UTF8String; ADest: TStream): boolean;
var
  entryId: Integer;
begin
  entryId := FMemDir.IndexOf(EntryFilename(AName));
  if (entryId <> -1) and not FMemDir.IsDirectory[entryId] then
  begin
    with FMemDir.Entry[entryId] do
      result := CopyTo(ADest) = FileSize
  end
  else
    result := false;
end;

procedure TBGRAMemOriginalStorage.WriteFile(AName: UTF8String; ASource: TStream; ACompress: boolean; AOwnStream: boolean);
var
  idxEntry: Integer;
begin
  idxEntry := FMemDir.Add(EntryFilename(AName), ASource, true, AOwnStream);
  if ACompress then FMemDir.IsEntryCompressed[idxEntry] := true;
end;

function TBGRAMemOriginalStorage.FileExists(AName: UTF8String): boolean;
var
  entryId: Integer;
begin
  entryId := FMemDir.IndexOf(EntryFilename(AName));
  result := (entryId <> -1) and not FMemDir.IsDirectory[entryId];
end;

procedure TBGRAMemOriginalStorage.SaveToStream(AStream: TStream);
begin
  FMemDir.SaveToStream(AStream);
end;

procedure TBGRAMemOriginalStorage.LoadFromStream(AStream: TStream);
begin
  FMemDir.LoadFromStream(AStream);
end;

procedure TBGRAMemOriginalStorage.LoadFromResource(AFilename: string);
begin
  FMemDir.LoadFromResource(AFilename);
end;

procedure TBGRAMemOriginalStorage.CopyTo(AMemDir: TMemDirectory);
begin
  FMemDir.CopyTo(AMemDir, true);
end;

{ TBGRACustomOriginalStorage }

function TBGRACustomOriginalStorage.GetColor(AName: UTF8String): TBGRAPixel;
begin
  result := StrToBGRA(RawString[AName], BGRAPixelTransparent);
end;

procedure TBGRACustomOriginalStorage.SetColor(AName: UTF8String;
  AValue: TBGRAPixel);
begin
  RawString[AName] := LowerCase(BGRAToStr(AValue, CSSColors));
end;

function TBGRACustomOriginalStorage.GetDelimiter: char;
begin
  if FFormats.DecimalSeparator = ',' then
    result := ';' else result := ',';
end;

function TBGRACustomOriginalStorage.GetRectF(AName: utf8string): TRectF;
var
  a: array of Single;
begin
  a := FloatArray[AName];
  if length(a)<4 then
    result := EmptyRectF
  else
  begin
    result.Left := a[0];
    result.Top := a[1];
    result.Right := a[2];
    result.Bottom := a[3];
  end;
end;

procedure TBGRACustomOriginalStorage.SetRectF(AName: utf8string; AValue: TRectF);
var
  a: array of Single;
begin
  setlength(a,4);
  a[0] := AValue.Left;
  a[1] := AValue.Top;
  a[2] := AValue.Right;
  a[3] := AValue.Bottom;
  FloatArray[AName] := a;
end;

function TBGRACustomOriginalStorage.GetAffineMatrix(AName: utf8string): TAffineMatrix;
var
  stream: TMemoryStream;
begin
  stream:= TMemoryStream.Create;
  if ReadFile(AName, stream) and (stream.Size >= sizeof(result)) then
  begin
    stream.Position:= 0;
    {$PUSH}{$HINTS OFF}stream.ReadBuffer({%H-}result, sizeof({%H-}result));{$POP}
    LongWord(result[1,1]) := NtoLE(LongWord(result[1,1]));
    LongWord(result[2,1]) := NtoLE(LongWord(result[2,1]));
    LongWord(result[1,2]) := NtoLE(LongWord(result[1,2]));
    LongWord(result[2,2]) := NtoLE(LongWord(result[2,2]));
    LongWord(result[1,3]) := NtoLE(LongWord(result[1,3]));
    LongWord(result[2,3]) := NtoLE(LongWord(result[2,3]));
  end else
    result := AffineMatrixIdentity;
  stream.Free;
end;

procedure TBGRACustomOriginalStorage.SetAffineMatrix(AName: utf8string;
  const AValue: TAffineMatrix);
var
  stream: TMemoryStream;
begin
  stream:= TMemoryStream.Create;
  stream.WriteBuffer(AValue, sizeof(AValue));
  WriteFile(AName,stream,false,true);
end;

function TBGRACustomOriginalStorage.GetRect(AName: utf8string): TRect;
var
  rF: TRectF;
begin
  rF := RectangleF[AName];
  result := rect(round(rF.Left),round(rF.Top),round(rF.Right),round(rF.Bottom));
end;

procedure TBGRACustomOriginalStorage.SetRect(AName: utf8string; AValue: TRect);
var
  rF: TRectF;
begin
  rF := rectF(AValue.Left,AValue.Top,AValue.Right,AValue.Bottom);
  RectangleF[AName] := rF;
end;

function TBGRACustomOriginalStorage.GetBoolDef(AName: utf8string;
  ADefault: boolean): boolean;
begin
  if RawString[AName] = 'true' then result := true
  else if RawString[AName] = 'false' then result := false
  else result := ADefault;
end;

function TBGRACustomOriginalStorage.GetBool(AName: utf8string): boolean;
begin
  result := GetBoolDef(AName, false);
end;

function TBGRACustomOriginalStorage.GetSingleArray(AName: utf8string): ArrayOfSingle;
var
  textVal: String;
  values: TStringList;
  i: Integer;
begin
  textVal := Trim(RawString[AName]);
  if textVal = '' then exit(nil);
  values := TStringList.Create;
  values.StrictDelimiter := true;
  values.Delimiter:= GetDelimiter;
  values.DelimitedText:= textVal;
  setlength(result, values.Count);
  for i := 0 to high(result) do
    if CompareText(values[i],'none')=0 then
      result[i] := EmptySingle
    else
      result[i] := StrToFloatDef(values[i], 0, FFormats);
  values.Free;
end;

function TBGRACustomOriginalStorage.GetColorArray(AName: UTF8String
  ): ArrayOfTBGRAPixel;
var colorNames: TStringList;
  i: Integer;
begin
  colorNames := TStringList.Create;
  colorNames.StrictDelimiter := true;
  colorNames.Delimiter:= GetDelimiter;
  colorNames.DelimitedText:= RawString[AName];
  setlength(result, colorNames.Count);
  for i := 0 to high(result) do
    result[i] := StrToBGRA(colorNames[i],BGRAPixelTransparent);
  colorNames.Free;
end;

function TBGRACustomOriginalStorage.GetIntegerDef(AName: utf8string;
  ADefault: integer): integer;
begin
  result := StrToIntDef(RawString[AName],ADefault);
end;

function TBGRACustomOriginalStorage.GetSingleDef(AName: utf8string;
  ADefault: single): single;
begin
  result := StrToFloatDef(RawString[AName], ADefault, FFormats);
end;

procedure TBGRACustomOriginalStorage.SetBool(AName: utf8string; AValue: boolean);
begin
  RawString[AName] := BoolToStr(AValue,'true','false');
end;

procedure TBGRACustomOriginalStorage.SetSingleArray(AName: utf8string;
  AValue: ArrayOfSingle);
var
  values: TStringList;
  i: Integer;
begin
  values:= TStringList.Create;
  values.StrictDelimiter:= true;
  values.Delimiter:= GetDelimiter;
  for i := 0 to high(AValue) do
    if AValue[i] = EmptySingle then
      values.Add('none')
    else
      values.Add(FloatToStr(AValue[i], FFormats));
  RawString[AName] := values.DelimitedText;
  values.Free;
end;

procedure TBGRACustomOriginalStorage.SetColorArray(AName: UTF8String;
  AValue: ArrayOfTBGRAPixel);
var colorNames: TStringList;
  i: Integer;
begin
  colorNames := TStringList.Create;
  colorNames.StrictDelimiter := true;
  colorNames.Delimiter:= GetDelimiter;
  for i := 0 to high(AValue) do
    colorNames.Add(LowerCase(BGRAToStr(AValue[i], CSSColors)));
  RawString[AName] := colorNames.DelimitedText;
  colorNames.Free;
end;

function TBGRACustomOriginalStorage.GetInteger(AName: utf8string): integer;
begin
  result := GetIntegerDef(AName,0);
end;

function TBGRACustomOriginalStorage.GetPointF(AName: utf8string): TPointF;
var
  s: String;
  posComma: integer;
begin
  s := RawString[AName];
  posComma := pos(GetDelimiter,s);
  if posComma = 0 then
    exit(EmptyPointF);

  result.x := StrToFloat(copy(s,1,posComma-1), FFormats);
  result.y := StrToFloat(copy(s,posComma+1,length(s)-posComma), FFormats);
end;

function TBGRACustomOriginalStorage.GetSingle(AName: utf8string): single;
begin
  result := GetSingleDef(AName, EmptySingle);
end;

procedure TBGRACustomOriginalStorage.SetInteger(AName: utf8string;
  AValue: integer);
begin
  RawString[AName] := IntToStr(AValue);
end;

procedure TBGRACustomOriginalStorage.SetPointF(AName: utf8string;
  AValue: TPointF);
begin
  if isEmptyPointF(AValue) then RemoveAttribute(AName)
  else RawString[AName] := FloatToStrF(AValue.x, ffGeneral,7,3, FFormats)+GetDelimiter+FloatToStrF(AValue.y, ffGeneral,7,3, FFormats);
end;

procedure TBGRACustomOriginalStorage.SetSingle(AName: utf8string; AValue: single);
begin
  if AValue = EmptySingle then RemoveAttribute(AName)
  else RawString[AName] := FloatToStrF(AValue, ffGeneral,7,3, FFormats);
end;

constructor TBGRACustomOriginalStorage.Create;
begin
  FFormats := DefaultFormatSettings;
  FFormats.DecimalSeparator := '.';
end;

function TBGRACustomOriginalStorage.FloatEquals(AName: utf8string;
  AValue: single): boolean;
var
  curValue: Single;
begin
  curValue := Float[AName];
  if curValue = EmptySingle then
    result := (AValue = EmptySingle) else
  if AValue = EmptySingle then
    result := false else
    result := (FloatToStrF(AValue, ffGeneral,7,3, FFormats) =
              FloatToStrF(curValue, ffGeneral,7,3, FFormats));
end;

function TBGRACustomOriginalStorage.PointFEquals(AName: utf8string;
  const AValue: TPointF): boolean;
var
  curValue: TPointF;
begin
  curValue := PointF[AName];
  if isEmptyPointF(curValue) then
    result := isEmptyPointF(AValue) else
  if isEmptyPointF(AValue) then
    result := False else
    result := (FloatToStrF(AValue.x, ffGeneral,7,3, FFormats) =
              FloatToStrF(curValue.x, ffGeneral,7,3, FFormats)) and
              (FloatToStrF(AValue.y, ffGeneral,7,3, FFormats) =
              FloatToStrF(curValue.y, ffGeneral,7,3, FFormats));
end;

function TBGRACustomOriginalStorage.AffineMatrixEquals(AName: utf8string;
  const AValue: TAffineMatrix): boolean;
begin
  result := (AffineMatrix[AName] = AValue);
end;

{ TBGRALayerCustomOriginal }

procedure TBGRALayerCustomOriginal.SetOnChange(AValue: TOriginalChangeEvent);
begin
  if FOnChange=AValue then Exit;
  FOnChange:=AValue;
end;

function TBGRALayerCustomOriginal.GetDiffExpected: boolean;
begin
  result := Assigned(FOnChange);
end;

procedure TBGRALayerCustomOriginal.SetRenderStorage(AValue: TBGRACustomOriginalStorage);
begin
  if FRenderStorage=AValue then Exit;
  FRenderStorage:=AValue;
end;

function TBGRALayerCustomOriginal.GetGuid: TGuid;
begin
  result := FGuid;
end;

procedure TBGRALayerCustomOriginal.SetGuid(AValue: TGuid);
begin
  FGuid := AValue;
end;

procedure TBGRALayerCustomOriginal.NotifyChange(ADiff: TBGRAOriginalDiff);
begin
  if Assigned(FOnChange) then
    FOnChange(self, nil, ADiff);
  ADiff.Free;
end;

procedure TBGRALayerCustomOriginal.NotifyChange(ABounds: TRectF; ADiff: TBGRAOriginalDiff);
begin
  if Assigned(FOnChange) then
    FOnChange(self, @ABounds, ADiff);
  ADiff.Free;
end;

procedure TBGRALayerCustomOriginal.NotifyEditorChange;
begin
  if Assigned(FOnEditingChange) then
    FOnEditingChange(self);
end;

constructor TBGRALayerCustomOriginal.Create;
begin
  FGuid := GUID_NULL;
  FRenderStorage := nil;
end;

destructor TBGRALayerCustomOriginal.Destroy;
begin
  inherited Destroy;
end;

procedure TBGRALayerCustomOriginal.Render(ADest: TBGRABitmap;
  AMatrix: TAffineMatrix; ADraft: boolean);
begin
  Render(ADest, Point(0,0), AMatrix, ADraft);
end;

procedure TBGRALayerCustomOriginal.Render(ADest: TBGRABitmap;
  ARenderOffset: TPoint; AMatrix: TAffineMatrix; ADraft: boolean);
begin
  Render(ADest, AffineMatrixTranslation(ARenderOffset.X, ARenderOffset.Y)*AMatrix, ADraft);
end;

procedure TBGRALayerCustomOriginal.ConfigureEditor(AEditor: TBGRAOriginalEditor);
begin
  //nothing
end;

procedure TBGRALayerCustomOriginal.LoadFromFile(AFilenameUTF8: string);
var
  s: TFileStreamUTF8;
begin
  s := TFileStreamUTF8.Create(AFilenameUTF8, fmOpenRead, fmShareDenyWrite);
  try
    LoadFromStream(s);
  finally
    s.Free;
  end;
end;

procedure TBGRALayerCustomOriginal.LoadFromStream(AStream: TStream);
var storage: TBGRAMemOriginalStorage;
  memDir: TMemDirectory;
begin
  memDir := TMemDirectory.Create;
  storage := nil;
  try
    memDir.LoadFromStream(AStream);
    storage := TBGRAMemOriginalStorage.Create(memDir);
    if storage.RawString['class'] <> StorageClassName then
      raise exception.Create('Invalid class');
    LoadFromStorage(storage);
    FreeAndNil(storage);
  finally
    storage.Free;
    memDir.Free;
  end;
end;

procedure TBGRALayerCustomOriginal.LoadFromResource(AFilename: string);
var
  stream: TStream;
begin
  stream := BGRAResource.GetResourceStream(AFilename);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TBGRALayerCustomOriginal.SaveToFile(AFilenameUTF8: string);
var
  s: TFileStreamUTF8;
begin
  s := TFileStreamUTF8.Create(AFilenameUTF8, fmCreate);
  try
    SaveToStream(s);
  finally
    s.Free;
  end;
end;

procedure TBGRALayerCustomOriginal.SaveToStream(AStream: TStream);
var storage: TBGRAMemOriginalStorage;
  memDir: TMemDirectory;
begin
  memDir := TMemDirectory.Create;
  storage := nil;
  try
    storage := TBGRAMemOriginalStorage.Create(memDir);
    storage.RawString['class'] := StorageClassName;
    SaveToStorage(storage);
    FreeAndNil(storage);
    memDir.SaveToStream(AStream);
  finally
    storage.Free;
    memDir.Free;
  end;
end;

function TBGRALayerCustomOriginal.CreateEditor: TBGRAOriginalEditor;
begin
  result := TBGRAOriginalEditor.Create;
end;

class function TBGRALayerCustomOriginal.CanConvertToSVG: boolean;
begin
  result := false;
end;

function TBGRALayerCustomOriginal.IsInfiniteSurface: boolean;
begin
  result := false;
end;

function TBGRALayerCustomOriginal.ConvertToSVG(const AMatrix: TAffineMatrix; out AOffset: TPoint): TObject;
begin
  AOffset := Point(0,0);
  raise exception.Create('Not implemented');
end;

function TBGRALayerCustomOriginal.Duplicate: TBGRALayerCustomOriginal;
var
  storage: TBGRAMemOriginalStorage;
  c: TBGRALayerOriginalAny;
begin
  c := FindLayerOriginalClass(StorageClassName);
  if c = nil then raise exception.Create('Original class is not registered');
  storage := TBGRAMemOriginalStorage.Create;
  try
    SaveToStorage(storage);
    result := c.Create;
    result.LoadFromStorage(storage);
  finally
    storage.Free;
  end;
end;

{ TBGRALayerImageOriginal }

function TBGRALayerImageOriginal.GetImageHeight: integer;
begin
  if Assigned(FImage) then
    result := FImage.Height
  else
    result := 0;
end;

function TBGRALayerImageOriginal.GetImageWidth: integer;
begin
  if Assigned(FImage) then
    result := FImage.Width
  else
    result := 0;
end;

procedure TBGRALayerImageOriginal.BeginUpdate;
begin
  if DiffExpected and (FDiff=nil) then
    FDiff := TBGRAImageOriginalDiff.Create(self);
end;

procedure TBGRALayerImageOriginal.EndUpdate;
begin
  if Assigned(FDiff) then FDiff.ComputeDiff(self);
  NotifyChange(FDiff);
  FDiff := nil;
end;

procedure TBGRALayerImageOriginal.InternalLoadImageFromStream(AStream: TStream; AUpdate: boolean);
var
  newJpegStream: TMemoryStream;
  newImage: TBGRABitmap;
begin
  if DetectFileFormat(AStream) = ifJpeg then
  begin
    newJpegStream := TMemoryStream.Create;
    try
      newJpegStream.CopyFrom(AStream, AStream.Size);
      newJpegStream.Position := 0;
      newImage := TBGRABitmap.Create(newJpegStream);
      if AUpdate then BeginUpdate;
      InternalClear;
      FImage := newImage;
      FJpegStream := newJpegStream;
      newImage := nil;
      newJpegStream := nil;
      if AUpdate then
      begin
        Inc(FContentVersion);
        EndUpdate;
      end;
    finally
      newJpegStream.Free;
      newImage.Free;
    end;
  end else
  begin
    newImage := TBGRABitmap.Create(AStream);
    if AUpdate then BeginUpdate;
    InternalClear;
    FImage := newImage;
    if AUpdate then
    begin
      Inc(FContentVersion);
      EndUpdate;
    end;
  end;
end;

procedure TBGRALayerImageOriginal.InternalClear;
begin
  if Assigned(FImage) then
  begin
    FImage.FreeReference;
    FImage := nil
  end;
  FreeAndNil(FJpegStream);
end;

constructor TBGRALayerImageOriginal.Create;
begin
  inherited Create;
  FImage := nil;
  FContentVersion := 0;
  FJpegStream := nil;
end;

destructor TBGRALayerImageOriginal.Destroy;
begin
  FImage.FreeReference;
  FJpegStream.Free;
  FDiff.Free;
  inherited Destroy;
end;

function TBGRALayerImageOriginal.ConvertToSVG(const AMatrix: TAffineMatrix; out AOffset: TPoint): TObject;
var
  svg: TBGRASVG;
  img: TSVGImage;
begin
  svg := TBGRASVG.Create(Width, Height, cuPixel);
  Result:= svg;
  AOffset := Point(0,0);
  if Assigned(FJpegStream) then
    img := svg.Content.AppendImage(0,0,Width,Height,FJpegStream,'image/jpeg') else
  if Assigned(FImage) then
    img := svg.Content.AppendImage(0,0,Width,Height,FImage,false);
  img.matrix[cuCustom] := AMatrix;
end;

procedure TBGRALayerImageOriginal.Render(ADest: TBGRABitmap;
  AMatrix: TAffineMatrix; ADraft: boolean);
var resampleFilter: TResampleFilter;
begin
  if ADraft then resampleFilter := rfBox else resampleFilter:= rfCosine;
  if Assigned(FImage) then
    ADest.PutImageAffine(AMatrix, FImage, resampleFilter, dmSet);
end;

function TBGRALayerImageOriginal.GetRenderBounds(ADestRect: TRect;
  AMatrix: TAffineMatrix): TRect;
var
  aff: TAffineBox;
begin
  if Assigned(FImage) then
  begin
    aff := AMatrix*TAffineBox.AffineBox(PointF(0,0),PointF(FImage.Width,0),PointF(0,FImage.Height));
    result := aff.RectBounds;
  end else
    result := EmptyRect;
end;

procedure TBGRALayerImageOriginal.LoadFromStorage(
  AStorage: TBGRACustomOriginalStorage);
var imgStream: TMemoryStream;
  newImage: TBGRABitmap;
begin
  imgStream := TMemoryStream.Create;
  try
    if AStorage.ReadFile('content.png', imgStream) then
    begin
      imgStream.Position:= 0;
      newImage := TBGRABitmap.Create(imgStream);
      InternalClear;
      FImage := newImage;
    end else
    if AStorage.ReadFile('content.jpg', imgStream) then
    begin
      imgStream.Position:= 0;
      newImage := TBGRABitmap.Create(imgStream);
      InternalClear;
      FImage := newImage;
      FJpegStream := imgStream;
      imgStream:= nil;
    end else
      InternalClear;
    FContentVersion := AStorage.Int['content-version'];
  finally
    imgStream.Free;
  end;
end;

procedure TBGRALayerImageOriginal.SaveToStorage(
  AStorage: TBGRACustomOriginalStorage);
var imgStream: TMemoryStream;
begin
  if Assigned(FImage) then
  begin
    if FContentVersion > AStorage.Int['content-version'] then
    begin
      if Assigned(FJpegStream) then
      begin
        AStorage.WriteFile('content.jpg', FJpegStream, false);
        AStorage.RemoveFile('content.png');
        AStorage.Int['content-version'] := FContentVersion;
      end else
      begin
        imgStream := TMemoryStream.Create;
        try
          FImage.SaveToStreamAsPng(imgStream);
          AStorage.RemoveFile('content.jpg');
          AStorage.WriteFile('content.png', imgStream, false);
          AStorage.Int['content-version'] := FContentVersion;
        finally
          imgStream.Free;
        end;
      end;
    end;
  end else
  begin
    AStorage.RemoveFile('content.jpg');
    AStorage.RemoveFile('content.png');
    AStorage.Int['content-version'] := FContentVersion;
  end;
end;

procedure TBGRALayerImageOriginal.LoadFromStream(AStream: TStream);
begin
  if TMemDirectory.CheckHeader(AStream) then
    inherited LoadFromStream(AStream)
  else
  begin
    InternalLoadImageFromStream(AStream, False);
    inc(FContentVersion);
  end;
end;

procedure TBGRALayerImageOriginal.Clear;
begin
  BeginUpdate;
  InternalClear;
  Inc(FContentVersion);
  EndUpdate;
end;

procedure TBGRALayerImageOriginal.LoadImageFromStream(AStream: TStream);
begin
  InternalLoadImageFromStream(AStream, True);
end;

procedure TBGRALayerImageOriginal.SaveImageToStream(AStream: TStream);
begin
  if Assigned(FJpegStream) then
  begin
    FJpegStream.Position := 0;
    if AStream.CopyFrom(FJpegStream, FJpegStream.Size)<>FJpegStream.Size then
      raise exception.Create('Error while saving');
  end else
  if Assigned(FImage) then
    FImage.SaveToStreamAsPng(AStream)
  else raise exception.Create('No image to be saved');
end;

procedure TBGRALayerImageOriginal.AssignImage(AImage: TBGRACustomBitmap);
var
  newImage: TBGRABitmap;
begin
  newImage := TBGRABitmap.Create;
  newImage.Assign(AImage);
  BeginUpdate;
  InternalClear;
  FImage := newImage;
  Inc(FContentVersion);
  EndUpdate;
end;

function TBGRALayerImageOriginal.GetImageCopy: TBGRABitmap;
begin
  if FImage = nil then result := nil
  else result := FImage.Duplicate;
end;

class function TBGRALayerImageOriginal.StorageClassName: RawByteString;
begin
  result := 'image';
end;

class function TBGRALayerImageOriginal.CanConvertToSVG: boolean;
begin
  Result:= true;
end;

initialization

  RegisterLayerOriginal(TBGRALayerImageOriginal);

end.

