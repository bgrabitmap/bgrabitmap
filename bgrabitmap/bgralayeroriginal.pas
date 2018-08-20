unit BGRALayerOriginal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, BGRATransform, BGRAMemDirectory;

type
  TAffineMatrix = BGRATransform.TAffineMatrix;
  TBGRALayerCustomOriginal = class;
  TBGRALayerOriginalAny = class of TBGRALayerCustomOriginal;
  TOriginalMovePointEvent = procedure(ASender: TObject; APrevCoord, ANewCoord: TPointF) of object;
  TOriginalChangeEvent = procedure(ASender: TObject) of object;
  TOriginalEditorCursor = (oecDefault, oecMove);

  { TBGRAOriginalEditor }

  TBGRAOriginalEditor = class
  protected
    FMatrix: TAffineMatrix;
    FPoints: array of record
      Origin, Coord: TPointF;
      OnMove: TOriginalMovePointEvent;
      RightButton: boolean;
      SnapToPoint: integer;
    end;
    FPointSize: single;
    FPointMoving: integer;
    FPointCoordDelta: TPointF;
    FMovingRightButton: boolean;
    FPrevMousePos: TPointF;
    function RenderPoint(ADest: TBGRABitmap; ACoord: TPointF): TRect; virtual;
    function RenderArrow(ADest: TBGRABitmap; AOrigin, AEndCoord: TPointF): TRect; virtual;
    procedure SetMatrix(AValue: TAffineMatrix);
  public
    constructor Create;
    procedure Clear;
    function AddPoint(ACoord: TPointF; AOnMove: TOriginalMovePointEvent; ARightButton: boolean = false; ASnapToPoint: integer = -1): integer;
    procedure AddArrow(AOrigin, AEndCoord: TPointF; AOnMoveEnd: TOriginalMovePointEvent; ARightButton: boolean = false);
    procedure MouseMove(Shift: TShiftState; X, Y: single; out ACursor: TOriginalEditorCursor); virtual;
    procedure MouseDown(RightButton: boolean; Shift: TShiftState; X, Y: single; out ACursor: TOriginalEditorCursor); virtual;
    procedure MouseUp(RightButton: boolean; Shift: TShiftState; X, Y: single; out ACursor: TOriginalEditorCursor); virtual;
    function GetPointAt(ACoord: TPointF; ARightButton: boolean): integer;
    function Render(ADest: TBGRABitmap): TRect; virtual;
    property Matrix: TAffineMatrix read FMatrix write SetMatrix;
    property PointSize: single read FPointSize write FPointSize;
  end;

  TBGRACustomOriginalStorage = class;

  { TBGRALayerCustomOriginal }

  TBGRALayerCustomOriginal = class
  private
    FOnChange: TOriginalChangeEvent;
    procedure SetOnChange(AValue: TOriginalChangeEvent);
  protected
    FGuid: TGuid;
    function GetGuid: TGuid;
    procedure SetGuid(AValue: TGuid);
    procedure NotifyChange;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); virtual; abstract;
    function GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix): TRect; virtual; abstract;
    procedure ConfigureEditor({%H-}AEditor: TBGRAOriginalEditor); virtual;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); virtual; abstract;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); virtual; abstract;
    procedure LoadFromFile(AFilenameUTF8: string); virtual;
    procedure LoadFromStream(AStream: TStream); virtual;
    procedure SaveToFile(AFilenameUTF8: string); virtual;
    procedure SaveToStream(AStream: TStream); virtual;
    function CreateEditor: TBGRAOriginalEditor; virtual;
    class function StorageClassName: RawByteString; virtual; abstract;
    property Guid: TGuid read GetGuid write SetGuid;
    property OnChange: TOriginalChangeEvent read FOnChange write SetOnChange;
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
    procedure ContentChanged;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix): TRect; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;
    class function StorageClassName: RawByteString; override;
    property Width: integer read GetImageWidth;
    property Height: integer read GetImageHeight;
  end;

  { TBGRACustomOriginalStorage }

  TBGRACustomOriginalStorage = class
  private
    function GetBool(AName: utf8string): boolean;
    function GetIntegerDef(AName: utf8string; ADefault: integer): integer;
    function GetSingleDef(AName: utf8string; ADefault: single): single;
    procedure SetBool(AName: utf8string; AValue: boolean);
  protected
    FFormats: TFormatSettings;
    function GetColorArray(AName: UTF8String): ArrayOfTBGRAPixel;
    function GetInteger(AName: utf8string): integer;
    function GetPointF(AName: utf8string): TPointF;
    function GetRawString(AName: utf8string): RawByteString; virtual; abstract;
    function GetSingle(AName: utf8string): single;
    function GetColor(AName: UTF8String): TBGRAPixel;
    procedure SetColorArray(AName: UTF8String; AValue: ArrayOfTBGRAPixel);
    procedure SetInteger(AName: utf8string; AValue: integer);
    procedure SetPointF(AName: utf8string; AValue: TPointF);
    procedure SetRawString(AName: utf8string; AValue: RawByteString); virtual; abstract;
    procedure SetSingle(AName: utf8string; AValue: single);
    procedure SetColor(AName: UTF8String; AValue: TBGRAPixel);
  public
    constructor Create;
    procedure RemoveAttribute(AName: utf8string); virtual; abstract;
    procedure RemoveFile(AName: utf8string); virtual; abstract;
    function ReadFile(AName: UTF8String; ADest: TStream): boolean; virtual; abstract;
    procedure WriteFile(AName: UTF8String; ASource: TStream; ACompress: boolean); virtual; abstract;
    property RawString[AName: utf8string]: RawByteString read GetRawString write SetRawString;
    property Int[AName: utf8string]: integer read GetInteger write SetInteger;
    property IntDef[AName: utf8string; ADefault: integer]: integer read GetIntegerDef;
    property Bool[AName: utf8string]: boolean read GetBool write SetBool;
    property Float[AName: utf8string]: single read GetSingle write SetSingle;
    property FloatDef[AName: utf8string; ADefault: single]: single read GetSingleDef;
    property PointF[AName: utf8string]: TPointF read GetPointF write SetPointF;
    property Color[AName: UTF8String]: TBGRAPixel read GetColor write SetColor;
    property ColorArray[AName: UTF8String]: ArrayOfTBGRAPixel read GetColorArray write SetColorArray;
  end;

  { TBGRAMemOriginalStorage }

  TBGRAMemOriginalStorage = class(TBGRACustomOriginalStorage)
  protected
    FMemDir: TMemDirectory;
    function GetRawString(AName: utf8string): RawByteString; override;
    procedure SetRawString(AName: utf8string; AValue: RawByteString); override;
  public
    constructor Create(AMemDir: TMemDirectory);
    procedure RemoveAttribute(AName: utf8string); override;
    procedure RemoveFile(AName: utf8string); override;
    function ReadFile(AName: UTF8String; ADest: TStream): boolean; override;
    procedure WriteFile(AName: UTF8String; ASource: TStream; ACompress: boolean); override;
  end;

procedure RegisterLayerOriginal(AClass: TBGRALayerOriginalAny);
function FindLayerOriginalClass(AStorageClassName: string): TBGRALayerOriginalAny;

implementation

uses BGRAPolygon, math, BGRAMultiFileType, BGRAUTF8, BGRAGraphics, Types;

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

{ TBGRAOriginalEditor }

procedure TBGRAOriginalEditor.SetMatrix(AValue: TAffineMatrix);
begin
  if FMatrix=AValue then Exit;
  FMatrix:=AValue;
end;

function TBGRAOriginalEditor.RenderPoint(ADest: TBGRABitmap; ACoord: TPointF): TRect;
const alpha = 192;
var filler: TBGRAMultishapeFiller;
begin
  if isEmptyPointF(ACoord) then
    result := EmptyRect
  else
  begin
    result := rect(floor(ACoord.x - FPointSize - 2), floor(ACoord.y - FPointSize - 2), ceil(ACoord.x + FPointSize + 3), ceil(ACoord.y + FPointSize + 3));
    filler := TBGRAMultishapeFiller.Create;
    filler.AddEllipseBorder(ACoord.x,ACoord.y, FPointSize-1.5,FPointSize-1.5, 4, BGRA(0,0,0,alpha));
    filler.AddEllipseBorder(ACoord.x,ACoord.y, FPointSize-1.5,FPointSize-1.5, 1, BGRA(255,255,255,alpha));
    filler.PolygonOrder:= poLastOnTop;
    filler.Draw(ADest);
    filler.Free;
  end;
end;

function TBGRAOriginalEditor.RenderArrow(ADest: TBGRABitmap; AOrigin,
  AEndCoord: TPointF): TRect;
const alpha = 192;
var
  pts: ArrayOfTPointF;
  i: Integer;
begin
  if isEmptyPointF(AOrigin) or isEmptyPointF(AEndCoord) then
    result := EmptyRect
  else
  begin
    result := Rect(floor(AOrigin.x-1),floor(AOrigin.y-1),ceil(AOrigin.x+1),ceil(AOrigin.y+1));
    ADest.Pen.Arrow.EndAsClassic;
    ADest.Pen.Arrow.EndSize := PointF(FPointSize,FPointSize);
    pts := ADest.ComputeWidePolyline([AOrigin,AEndCoord],1);
    ADest.DrawPolygonAntialias(pts, BGRA(0,0,0,alpha),2);
    ADest.FillPolyAntialias(pts, BGRA(255,255,255,alpha));
    ADest.Pen.Arrow.EndAsNone;
    for i := 0 to high(pts) do
    if not isEmptyPointF(pts[i]) then
    begin
      if floor(pts[i].x - 1) < result.Left then result.Left := floor(pts[i].x - 1);
      if ceil(pts[i].x + 1) > result.Right then result.Right := ceil(pts[i].x - 1);
      if floor(pts[i].y - 1) < result.Top then result.Top := floor(pts[i].y - 1);
      if ceil(pts[i].y + 1) > result.Bottom then result.Bottom := ceil(pts[i].y - 1);
    end;
  end;
end;

constructor TBGRAOriginalEditor.Create;
begin
  FPointSize:= 6;
  FMatrix := AffineMatrixIdentity;
  FPointMoving:= -1;
end;

procedure TBGRAOriginalEditor.Clear;
begin
  FPoints := nil;
end;

function TBGRAOriginalEditor.AddPoint(ACoord: TPointF;
  AOnMove: TOriginalMovePointEvent; ARightButton: boolean; ASnapToPoint: integer): integer;
begin
  setlength(FPoints, length(FPoints)+1);
  result := High(FPoints);
  with FPoints[result] do
  begin
    Origin := EmptyPointF;
    Coord := ACoord;
    OnMove := AOnMove;
    RightButton:= ARightButton;
    SnapToPoint:= ASnapToPoint;
  end;
end;

procedure TBGRAOriginalEditor.AddArrow(AOrigin, AEndCoord: TPointF;
  AOnMoveEnd: TOriginalMovePointEvent; ARightButton: boolean);
begin
  setlength(FPoints, length(FPoints)+1);
  with FPoints[High(FPoints)] do
  begin
    Origin := AOrigin;
    Coord := AEndCoord;
    OnMove := AOnMoveEnd;
    RightButton:= ARightButton;
    SnapToPoint:= -1;
  end;
end;

procedure TBGRAOriginalEditor.MouseMove(Shift: TShiftState; X, Y: single; out
  ACursor: TOriginalEditorCursor);
var newMousePos, newCoord, snapCoord: TPointF;
begin
  newMousePos := Matrix*PointF(X,Y);
  if (FPointMoving <> -1) and (FPointMoving < length(FPoints)) then
  begin
    newCoord := newMousePos + FPointCoordDelta;
    if FPoints[FPointMoving].SnapToPoint <> -1 then
    begin
      snapCoord := FPoints[FPoints[FPointMoving].SnapToPoint].Coord;
      if VectLen(snapCoord - newMousePos) < FPointSize then
        newCoord := snapCoord;
    end;
    FPoints[FPointMoving].OnMove(self, FPoints[FPointMoving].Coord, newCoord);
    FPoints[FPointMoving].Coord := newCoord;
  end;
  FPrevMousePos:= newMousePos;
end;

procedure TBGRAOriginalEditor.MouseDown(RightButton: boolean;
  Shift: TShiftState; X, Y: single; out ACursor: TOriginalEditorCursor);
begin
  FPrevMousePos:= Matrix*PointF(X,Y);
  if FPointMoving = -1 then
  begin
    FPointMoving:= GetPointAt(Matrix*PointF(X,Y), RightButton);
    if FPointMoving <> -1 then
    begin
      FMovingRightButton:= RightButton;
      FPointCoordDelta := FPoints[FPointMoving].Coord - FPrevMousePos;
    end;
  end;
  if FPointMoving <> -1 then
    ACursor := oecMove
  else
    ACursor := oecDefault;
end;

procedure TBGRAOriginalEditor.MouseUp(RightButton: boolean; Shift: TShiftState;
  X, Y: single; out ACursor: TOriginalEditorCursor);
begin
  if RightButton = FMovingRightButton then
    FPointMoving:= -1;
  ACursor := oecDefault;
end;

function TBGRAOriginalEditor.GetPointAt(ACoord: TPointF; ARightButton: boolean): integer;
var v: TPointF;
  curDist,newDist: single;
  i: Integer;
begin
  curDist := 4*FPointSize*FPointSize;
  result := -1;

  for i := 0 to high(FPoints) do
  if FPoints[i].RightButton = ARightButton then
  begin
    v := FPoints[i].Coord - ACoord;
    newDist := v*v;
    if newDist <= curDist then
    begin
      curDist:= newDist;
      result := i;
    end;
  end;
  if result <> -1 then exit;

  for i := 0 to high(FPoints) do
  if FPoints[i].RightButton <> ARightButton then
  begin
    v := FPoints[i].Coord - ACoord;
    newDist := v*v;
    if newDist <= curDist then
    begin
      curDist:= newDist;
      result := i;
    end;
  end;
end;

function TBGRAOriginalEditor.Render(ADest: TBGRABitmap): TRect;
var
  i: Integer;
begin
  result := EmptyRect;
  for i := 0 to high(FPoints) do
  begin
    if isEmptyPointF(FPoints[i].Origin) then
      UnionRect(result, result, RenderPoint(ADest, FMatrix*FPoints[i].Coord))
    else
      UnionRect(result, result, RenderArrow(ADest, FMatrix*FPoints[i].Origin, FMatrix*FPoints[i].Coord));
  end;
end;

{ TBGRAMemOriginalStorage }

function TBGRAMemOriginalStorage.GetRawString(AName: utf8string): RawByteString;
begin
  if pos('.',AName)<>0 then exit('');
  result := FMemDir.RawStringByFilename[AName];
end;

procedure TBGRAMemOriginalStorage.SetRawString(AName: utf8string;
  AValue: RawByteString);
begin
  if pos('.',AName)<>0 then exit;
  FMemDir.RawStringByFilename[AName] := AValue;
end;

constructor TBGRAMemOriginalStorage.Create(AMemDir: TMemDirectory);
begin
  inherited Create;
  FMemDir := AMemDir;
end;

procedure TBGRAMemOriginalStorage.RemoveAttribute(AName: utf8string);
begin
  if pos('.',AName)<>0 then exit;
  FMemDir.Delete(AName,'');
end;

procedure TBGRAMemOriginalStorage.RemoveFile(AName: utf8string);
begin
  FMemDir.Delete(EntryFilename(AName));
end;

function TBGRAMemOriginalStorage.ReadFile(AName: UTF8String; ADest: TStream): boolean;
var
  entryId: Integer;
begin
  entryId := FMemDir.IndexOf(EntryFilename(AName));
  if entryId <> -1 then
  begin
    with FMemDir.Entry[entryId] do
      result := CopyTo(ADest) = FileSize
  end
  else
    result := false;
end;

procedure TBGRAMemOriginalStorage.WriteFile(AName: UTF8String; ASource: TStream; ACompress: boolean);
var
  idxEntry: Integer;
begin
  idxEntry := FMemDir.Add(EntryFilename(AName), ASource, true, false);
  if ACompress then FMemDir.IsEntryCompressed[idxEntry] := true;
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

function TBGRACustomOriginalStorage.GetBool(AName: utf8string): boolean;
begin
  result := StrToBool(RawString[AName]);
end;

function TBGRACustomOriginalStorage.GetColorArray(AName: UTF8String
  ): ArrayOfTBGRAPixel;
var colorNames: TStringList;
  i: Integer;
begin
  colorNames := TStringList.Create;
  colorNames.StrictDelimiter := true;
  colorNames.Delimiter:= ',';
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

procedure TBGRACustomOriginalStorage.SetColorArray(AName: UTF8String;
  AValue: ArrayOfTBGRAPixel);
var colorNames: TStringList;
  i: Integer;
begin
  colorNames := TStringList.Create;
  colorNames.StrictDelimiter := true;
  colorNames.Delimiter:= ',';
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
  posComma := pos(',',s);
  if posComma = 0 then
    exit(EmptyPointF);

  result.x := StrToFloat(copy(s,1,posComma-1));
  result.y := StrToFloat(copy(s,posComma+1,length(s)-posComma));
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
  else RawString[AName] := FloatToStrF(AValue.x, ffGeneral,7,3, FFormats)+','+FloatToStrF(AValue.y, ffGeneral,7,3, FFormats);
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

{ TBGRALayerCustomOriginal }

procedure TBGRALayerCustomOriginal.SetOnChange(AValue: TOriginalChangeEvent);
begin
  if FOnChange=AValue then Exit;
  FOnChange:=AValue;
end;

function TBGRALayerCustomOriginal.GetGuid: TGuid;
begin
  result := FGuid;
end;

procedure TBGRALayerCustomOriginal.SetGuid(AValue: TGuid);
begin
  FGuid := AValue;
end;

procedure TBGRALayerCustomOriginal.NotifyChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

constructor TBGRALayerCustomOriginal.Create;
begin
  FGuid := GUID_NULL;
end;

destructor TBGRALayerCustomOriginal.Destroy;
begin
  inherited Destroy;
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

{ TBGRALayerImageOriginal }

function TBGRALayerImageOriginal.GetImageHeight: integer;
begin
  result := FImage.Height;
end;

function TBGRALayerImageOriginal.GetImageWidth: integer;
begin
  result := FImage.Width;
end;

procedure TBGRALayerImageOriginal.ContentChanged;
begin
  FContentVersion += 1;
  NotifyChange;
end;

constructor TBGRALayerImageOriginal.Create;
begin
  inherited Create;
  FImage := TBGRABitmap.Create;
  FContentVersion := 0;
  FJpegStream := nil;
end;

destructor TBGRALayerImageOriginal.Destroy;
begin
  FImage.Free;
  FJpegStream.Free;
  inherited Destroy;
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
begin
  if not Assigned(FImage) then FImage := TBGRABitmap.Create;
  imgStream := TMemoryStream.Create;
  try
    if AStorage.ReadFile('content.png', imgStream) then
    begin
      imgStream.Position:= 0;
      FImage.LoadFromStream(imgStream);
      FreeAndNil(FJpegStream);
    end else
    if AStorage.ReadFile('content.jpg', imgStream) then
    begin
      FreeAndNil(FJpegStream);
      FJpegStream := imgStream;
      imgStream:= nil;

      FJpegStream.Position:= 0;
      FImage.LoadFromStream(FJpegStream);
    end else
    begin
      FImage.SetSize(0,0);
      FreeAndNil(FJpegStream);
    end;
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
  end;
end;

procedure TBGRALayerImageOriginal.LoadFromStream(AStream: TStream);
var
  newJpegStream: TMemoryStream;
begin
  if DetectFileFormat(AStream) = ifJpeg then
  begin
    newJpegStream := TMemoryStream.Create;
    try
      newJpegStream.CopyFrom(AStream, AStream.Size);
      newJpegStream.Position := 0;
      FImage.LoadFromStream(newJpegStream);
      FJpegStream.Free;
      FJpegStream := newJpegStream;
      newJpegStream := nil;
    finally
      newJpegStream.Free;
    end;
  end else
  begin
    FreeAndNil(FJpegStream);
    FImage.LoadFromStream(AStream);
  end;
  ContentChanged;
end;

procedure TBGRALayerImageOriginal.SaveToStream(AStream: TStream);
begin
  if Assigned(FJpegStream) then
  begin
    FJpegStream.Position := 0;
    if AStream.CopyFrom(FJpegStream, FJpegStream.Size)<>FJpegStream.Size then
      raise exception.Create('Error while saving');
  end else
    FImage.SaveToStreamAsPng(AStream);
end;

class function TBGRALayerImageOriginal.StorageClassName: RawByteString;
begin
  result := 'image';
end;

initialization

  RegisterLayerOriginal(TBGRALayerImageOriginal);

end.

