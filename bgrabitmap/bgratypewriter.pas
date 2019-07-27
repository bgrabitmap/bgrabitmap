unit BGRATypewriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AvgLvlTree, BGRABitmapTypes, BGRACanvas2D, BGRATransform;

type
  TGlyphBoxes = array of record
    Glyph: string;
    Box: TAffineBox;
  end;

  { TBGRAGlyph }

  TBGRAGlyph = class
  protected
    FIdentifier: string;
    procedure WriteHeader(AStream: TStream; AName: string; AContentSize: longint);
    class procedure ReadHeader(AStream: TStream; out AName: string; out AContentSize: longint); static;
    function ContentSize: integer; virtual;
    function HeaderName: string; virtual;
    procedure WriteContent(AStream: TStream); virtual;
    procedure ReadContent(AStream: TStream); virtual;
  public
    Width,Height: single;
    constructor Create(AIdentifier: string); virtual;
    constructor Create(AStream: TStream); virtual;
    procedure Path({%H-}ADest: IBGRAPath; {%H-}AMatrix: TAffineMatrix; {%H-}AReverse: boolean= false); virtual;
    property Identifier: string read FIdentifier;
    procedure SaveToStream(AStream: TStream);
    class function LoadFromStream(AStream: TStream): TBGRAGlyph; static;
  end;

  TGlyphPointCurveMode= TEasyBezierCurveMode;

const
  cmAuto = TEasyBezierCurveMode.cmAuto;
  cmCurve = TEasyBezierCurveMode.cmCurve;
  cmAngle = TEasyBezierCurveMode.cmAngle;

type
  { TBGRAPolygonalGlyph }

  TBGRAPolygonalGlyph = class(TBGRAGlyph)
  private
    function GetClosed: boolean;
    function GetMinimumDotProduct: single;
    function GetPoint(AIndex: integer): TPointF;
    function GetPointCount: integer;
    procedure SetClosed(AValue: boolean);
    procedure SetMinimumDotProduct(AValue: single);
    procedure SetPoint(AIndex: integer; AValue: TPointF);
    procedure SetQuadraticCurves(AValue: boolean);
  protected
    FQuadraticCurves: boolean;
    FEasyBezier: TEasyBezierCurve;
    function ContentSize: integer; override;
    function HeaderName: string; override;
    procedure WriteContent(AStream: TStream); override;
    procedure ReadContent(AStream: TStream); override;
    function PointTransformMatrix(APoint: PPointF; AData: pointer): TPointF;
    procedure Init;
  public
    Offset: TPointF;
    constructor Create(AIdentifier: string); override;
    constructor Create(AStream: TStream); override;
    constructor Create(AStream: TStream; AQuadratic: boolean);
    procedure SetPoints(const APoints: array of TPointF); overload;
    procedure SetPoints(const APoints: array of TPointF; const ACurveMode: array of TGlyphPointCurveMode); overload;
    procedure Path(ADest: IBGRAPath; AMatrix: TAffineMatrix; AReverse: boolean = false); override;
    property QuadraticCurves: boolean read FQuadraticCurves write SetQuadraticCurves;
    property Closed: boolean read GetClosed write SetClosed;
    property MinimumDotProduct: single read GetMinimumDotProduct write SetMinimumDotProduct;
    property Point[AIndex: integer]: TPointF read GetPoint write SetPoint;
    property PointCount: integer read GetPointCount;
  end;

  TBGRACustomTypeWriterHeader = record
    HeaderName: String;
    NbGlyphs: integer;
  end;

  TBGRAGlyphDisplayInfo = record
    Glyph: TBGRAGlyph;
    Matrix: TAffineMatrix;
    Mirrored: boolean;
  end;

  TBGRATextDisplayInfo = array of TBGRAGlyphDisplayInfo;
  TBrowseGlyphCallbackFlag = (gcfMirrored, gcfMerged, gcfRightToLeft);
  TBrowseGlyphCallbackFlags = set of TBrowseGlyphCallbackFlag;
  TBrowseGlyphCallback = procedure(ATextUTF8: string; AGlyph: TBGRAGlyph;
    AFlags: TBrowseGlyphCallbackFlags; AData: Pointer; out AContinue: boolean) of object;

  TTextFitInfoCallbackData = record
    WidthAccumulator, MaxWidth: single;
    CharCount: integer;
    ByteCount: integer;
  end;

  TDisplayInfoCallbackData = record
    Align: TBGRATypeWriterAlignment;
    Matrix: TAffineMatrix;
    Info: TBGRATextDisplayInfo;
    InfoIndex: integer;
  end;

  { TBGRACustomTypeWriter }

  TBGRACustomTypeWriter = class
  private
    FBidiMode: TFontBidiMode;
    FGlyphs: TAvgLvlTree;
    procedure GlyphCallbackForDisplayInfo({%H-}ATextUTF8: string;
      AGlyph: TBGRAGlyph; AFlags: TBrowseGlyphCallbackFlags; AData: Pointer; out
      AContinue: boolean);
    procedure GlyphCallbackForTextFitInfoBeforeTransform(ATextUTF8: string;
      AGlyph: TBGRAGlyph; AFlags: TBrowseGlyphCallbackFlags; AData: Pointer; out AContinue: boolean);
    procedure SetBidiMode(AValue: TFontBidiMode);
    procedure GlyphCallbackForTextSizeBeforeTransform({%H-}ATextUTF8: string;
      AGlyph: TBGRAGlyph; {%H-}AFlags: TBrowseGlyphCallbackFlags; AData: pointer; out AContinue: boolean);
  protected
    TypeWriterMatrix: TAffineMatrix;
    function CompareGlyph({%H-}Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    function FindGlyph(AIdentifier: string): TAvgLvlTreeNode;
    function GetGlyph(AIdentifier: string): TBGRAGlyph; virtual;
    procedure SetGlyph(AIdentifier: string; AValue: TBGRAGlyph);
    function GetDisplayInfo(ATextUTF8: string; X,Y: Single;
                  AAlign: TBGRATypeWriterAlignment): TBGRATextDisplayInfo;
    procedure GlyphPath(ADest: TBGRACanvas2D; AIdentifier: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft; AMirrored: boolean = false);
    procedure DrawLastPath(ADest: TBGRACanvas2D);
    procedure ClearGlyphs;
    procedure RemoveGlyph(AIdentifier: string);
    procedure AddGlyph(AGlyph: TBGRAGlyph);
    function GetGlyphMatrix(AGlyph: TBGRAGlyph; X,Y: Single; AAlign: TBGRATypeWriterAlignment): TAffineMatrix;
    function GetTextMatrix(ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment): TAffineMatrix;
    property Glyph[AIdentifier: string]: TBGRAGlyph read GetGlyph write SetGlyph;
    function CustomHeaderSize: integer; virtual;
    procedure WriteCustomHeader(AStream: TStream); virtual;
    function ReadCustomTypeWriterHeader(AStream: TStream): TBGRACustomTypeWriterHeader;
    procedure ReadAdditionalHeader({%H-}AStream: TStream); virtual;
    function HeaderName: string; virtual;
    procedure BrowseGlyphs(ATextUTF8: string; ACallback: TBrowseGlyphCallback; AData: pointer; ADisplayOrder: boolean);
    procedure BrowseAllGlyphs(ACallback: TBrowseGlyphCallback; AData: pointer);
  public
    OutlineMode: TBGRATypeWriterOutlineMode;
    DrawGlyphsSimultaneously : boolean;
    LigatureWithF: boolean;
    constructor Create;
    function GetTextSizeBeforeTransform(ATextUTF8 :string): TPointF;
    procedure TextFitInfoBeforeTransform(ATextUTF8: string; AMaxWidth: single; out ACharCount, AByteCount: integer; out AUsedWidth: single);
    procedure SaveGlyphsToFile(AFilenameUTF8: string);
    procedure SaveGlyphsToStream(AStream: TStream);
    procedure LoadGlyphsFromFile(AFilenameUTF8: string);
    procedure LoadGlyphsFromStream(AStream: TStream);
    procedure DrawGlyph(ADest: TBGRACanvas2D; AIdentifier: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft; AMirrored: boolean = false);
    procedure DrawText(ADest: TBGRACanvas2D; ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft); virtual;
    procedure CopyTextPathTo(ADest: IBGRAPath; ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft); virtual;
    function GetGlyphBox(AIdentifier: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft): TAffineBox;
    function GetTextBox(ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft): TAffineBox;
    function GetTextGlyphBoxes(ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft): TGlyphBoxes;
    procedure NeedGlyphRange(AUnicodeFrom, AUnicodeTo: Cardinal);
    procedure NeedGlyphAnsiRange;
    destructor Destroy; override;
    property BidiMode: TFontBidiMode read FBidiMode write SetBidiMode;
  end;

function ComputeEasyBezier(APoints: array of TPointF; AClosed: boolean; AMinimumDotProduct: single = 0.707): ArrayOfTPointF; overload;
function ComputeEasyBezier(APoints: array of TPointF; ACurveMode: array of TGlyphPointCurveMode; AClosed: boolean; AMinimumDotProduct: single = 0.707): ArrayOfTPointF; overload;

implementation

uses BGRAUTF8, BGRAUnicode, math;

const
  aleph = 'ا';
  alephHamzaBelow = 'إ';
  alephHamzaAbove = 'أ';
  alephMaddaAbove = 'آ';
  lam = 'ل';

procedure LEWritePointF(Stream: TStream; AValue: TPointF);
begin
  LEWriteSingle(Stream,AValue.x);
  LEWriteSingle(Stream,AValue.y);
end;

function LEReadPointF(Stream: TStream): TPointF;
begin
  result.x := LEReadSingle(Stream);
  result.y := LEReadSingle(Stream);
end;

function ComputeEasyBezier(APoints: array of TPointF; AClosed: boolean; AMinimumDotProduct: single = 0.707): ArrayOfTPointF;
var
  glyph: TBGRAPolygonalGlyph;
  canvas2D: TBGRACanvas2D;
  i: integer;
begin
  if length(APoints) <= 2 then
  begin
    setlength(result, length(APoints));
    for i := 0 to high(result) do
      result[i] := APoints[i];
    exit;
  end;
  glyph := TBGRAPolygonalGlyph.Create('');
  glyph.QuadraticCurves := true;
  glyph.Closed:= AClosed;
  glyph.MinimumDotProduct := AMinimumDotProduct;
  glyph.SetPoints(APoints);
  canvas2D := TBGRACanvas2D.Create(nil);
  canvas2D.pixelCenteredCoordinates := true;
  glyph.Path(canvas2D,AffineMatrixIdentity);
  glyph.Free;
  result := canvas2D.currentPath;
  canvas2D.free;
end;

function ComputeEasyBezier(APoints: array of TPointF;
  ACurveMode: array of TGlyphPointCurveMode; AClosed: boolean;
  AMinimumDotProduct: single): ArrayOfTPointF;
var
  glyph: TBGRAPolygonalGlyph;
  canvas2D: TBGRACanvas2D;
  i: integer;
begin
  if length(APoints) <= 2 then
  begin
    setlength(result, length(APoints));
    for i := 0 to high(result) do
      result[i] := APoints[i];
    exit;
  end;
  glyph := TBGRAPolygonalGlyph.Create('');
  glyph.QuadraticCurves := true;
  glyph.Closed:= AClosed;
  glyph.MinimumDotProduct := AMinimumDotProduct;
  glyph.SetPoints(APoints, ACurveMode);
  canvas2D := TBGRACanvas2D.Create(nil);
  canvas2D.pixelCenteredCoordinates := true;
  glyph.Path(canvas2D,AffineMatrixIdentity);
  glyph.Free;
  result := canvas2D.currentPath;
  canvas2D.free;
end;

{ TBGRAPolygonalGlyph }

function TBGRAPolygonalGlyph.GetClosed: boolean;
begin
  result := FEasyBezier.Closed;
end;

function TBGRAPolygonalGlyph.GetMinimumDotProduct: single;
begin
  result := FEasyBezier.MinimumDotProduct;
end;

function TBGRAPolygonalGlyph.GetPoint(AIndex: integer): TPointF;
begin
  result := FEasyBezier.Point[AIndex];
end;

function TBGRAPolygonalGlyph.GetPointCount: integer;
begin
  result := FEasyBezier.PointCount;
end;

procedure TBGRAPolygonalGlyph.SetClosed(AValue: boolean);
begin
  FEasyBezier.Closed := AValue;
end;

procedure TBGRAPolygonalGlyph.SetMinimumDotProduct(AValue: single);
begin
  FEasyBezier.MinimumDotProduct := AValue;
end;

procedure TBGRAPolygonalGlyph.SetPoint(AIndex: integer; AValue: TPointF);
begin
  FEasyBezier.Point[AIndex] := AValue;
end;

procedure TBGRAPolygonalGlyph.SetQuadraticCurves(AValue: boolean);
begin
  if FQuadraticCurves=AValue then Exit;
  FQuadraticCurves:=AValue;
end;

function TBGRAPolygonalGlyph.ContentSize: integer;
begin
  Result:= (inherited ContentSize) + sizeof(single)*2 + 4 + sizeof(single)*2*PointCount;
end;

function TBGRAPolygonalGlyph.HeaderName: string;
begin
  if FQuadraticCurves then
    Result:='TBGRAEasyBezierGlyph'
  else
    Result:='TBGRAPolygonalGlyph'
end;

procedure TBGRAPolygonalGlyph.WriteContent(AStream: TStream);
var i: integer;
begin
  inherited WriteContent(AStream);
  LEWritePointF(AStream, Offset);
  LEWriteLongint(AStream,PointCount);
  for i := 0 to PointCount-1 do
    LEWritePointF(AStream, FEasyBezier.Point[i]);
  if FQuadraticCurves then
    for i := 0 to PointCount-1 do
      LEWriteLongint(AStream, ord(FEasyBezier.CurveMode[i]));
end;

procedure TBGRAPolygonalGlyph.ReadContent(AStream: TStream);
var i: integer;
  tempPts: array of TPointF;
  flags: LongInt;
begin
  inherited ReadContent(AStream);
  Offset := LEReadPointF(AStream);
  SetLength(tempPts, LEReadLongint(AStream));
  for i := 0 to high(tempPts) do
    tempPts[i] := LEReadPointF(AStream);
  SetPoints(tempPts);
  if FQuadraticCurves then
  begin
    for i := 0 to high(tempPts) do
    begin
      flags := LEReadLongint(AStream);
      FEasyBezier.CurveMode[i] := TEasyBezierCurveMode(flags and 255);
    end;
  end;
end;

function TBGRAPolygonalGlyph.PointTransformMatrix(APoint: PPointF;
  AData: pointer): TPointF;
begin
  result := TAffineMatrix(AData^) * APoint^;
end;

procedure TBGRAPolygonalGlyph.Init;
begin
  FEasyBezier.Init;
  Closed := True;
  Offset := PointF(0,0);
  FQuadraticCurves:= False;
end;

constructor TBGRAPolygonalGlyph.Create(AIdentifier: string);
begin
  Init;
  inherited Create(AIdentifier);
end;

constructor TBGRAPolygonalGlyph.Create(AStream: TStream);
begin
  Init;
  inherited Create(AStream);
end;

constructor TBGRAPolygonalGlyph.Create(AStream: TStream; AQuadratic: boolean);
begin
  Init;
  FQuadraticCurves:= AQuadratic;
  inherited Create(AStream);
end;

procedure TBGRAPolygonalGlyph.SetPoints(const APoints: array of TPointF);
begin
  FEasyBezier.SetPoints(APoints, cmAuto);
end;

procedure TBGRAPolygonalGlyph.SetPoints(const APoints: array of TPointF;
  const ACurveMode: array of TGlyphPointCurveMode);
begin
  if length(APoints) <> length(ACurveMode) then
    raise exception.Create('Dimension mismatch');
  FEasyBezier.SetPoints(APoints, ACurveMode);
end;

procedure TBGRAPolygonalGlyph.Path(ADest: IBGRAPath; AMatrix: TAffineMatrix;
  AReverse: boolean);
var
  nextMove: boolean;

  procedure DoPoint(AIndex: integer);
  begin
    if isEmptyPointF(Point[AIndex]) then
    begin
      if not nextMove and Closed then ADest.closePath;
      nextMove := true;
    end else
    begin
      if nextMove then
      begin
        ADest.moveTo(AMatrix*Point[AIndex]);
        nextMove := false;
      end else
        ADest.lineTo(AMatrix*Point[AIndex]);
    end;
  end;

var
  i: integer;
begin
  AMatrix := AMatrix*AffineMatrixTranslation(Offset.X,Offset.Y);
  if not FQuadraticCurves then
  begin
    nextMove := true;
    if AReverse then
    begin
      for i := PointCount-1 downto 0 do
        DoPoint(i);
    end else
      for i := 0 to PointCount-1 do
        DoPoint(i);
    if not nextmove and Closed then ADest.closePath;
  end else
    FEasyBezier.CopyToPath(ADest, @PointTransformMatrix, @AMatrix, AReverse);
end;

{ TBGRAGlyph }

procedure TBGRAGlyph.WriteHeader(AStream: TStream; AName: string;
  AContentSize: longint);
begin
  LEWriteByte(AStream, length(AName));
  AStream.Write(AName[1],length(AName));
  LEWriteLongint(AStream, AContentSize);
end;

class procedure TBGRAGlyph.ReadHeader(AStream: TStream; out AName: string; out
  AContentSize: longint);
var NameLength: integer;
begin
  NameLength := LEReadByte(AStream);
  setlength(AName,NameLength);
  AStream.Read(AName[1],length(AName));
  AContentSize := LEReadLongint(AStream);
end;

function TBGRAGlyph.ContentSize: integer;
begin
  result := 4+length(FIdentifier)+sizeof(single)*2;
end;

function TBGRAGlyph.HeaderName: string;
begin
  result := 'TBGRAGlyph';
end;

procedure TBGRAGlyph.WriteContent(AStream: TStream);
begin
  LEWriteLongint(AStream,length(FIdentifier));
  AStream.Write(FIdentifier[1],length(FIdentifier));
  LEWriteSingle(AStream,Width);
  LEWriteSingle(AStream,Height);
end;

procedure TBGRAGlyph.ReadContent(AStream: TStream);
var lIdentifierLength: integer;
begin
  lIdentifierLength:= LEReadLongint(AStream);
  setlength(FIdentifier, lIdentifierLength);
  AStream.Read(FIdentifier[1],length(FIdentifier));
  Width := LEReadSingle(AStream);
  Height := LEReadSingle(AStream);
end;

constructor TBGRAGlyph.Create(AIdentifier: string);
begin
  FIdentifier:= AIdentifier;
end;

constructor TBGRAGlyph.Create(AStream: TStream);
begin
  ReadContent(AStream);
end;

procedure TBGRAGlyph.Path(ADest: IBGRAPath; AMatrix: TAffineMatrix;
  AReverse: boolean);
begin
  //nothing
end;

procedure TBGRAGlyph.SaveToStream(AStream: TStream);
begin
  WriteHeader(AStream, HeaderName, ContentSize);
  WriteContent(AStream);
end;

class function TBGRAGlyph.LoadFromStream(AStream: TStream) : TBGRAGlyph;
var lName: string;
  lContentSize: integer;
  EndPosition: Int64;
begin
  ReadHeader(AStream,lName,lContentSize);
  EndPosition := AStream.Position + lContentSize;
  if lName = 'TBGRAPolygonalGlyph' then
    result := TBGRAPolygonalGlyph.Create(AStream)
  else if lName = 'TBGRAEasyBezierGlyph' then
    result := TBGRAPolygonalGlyph.Create(AStream, true)
  else if lName = 'TBGRAGlyph' then
    result := TBGRAGlyph.Create(AStream)
  else
    raise exception.Create('Unknown glyph type (' + lName + ')');
  AStream.Position:= EndPosition;
end;

{ TBGRACustomTypeWriter }

function TBGRACustomTypeWriter.GetGlyph(AIdentifier: string): TBGRAGlyph;
var Node: TAvgLvlTreeNode;
begin
  Node := FindGlyph(AIdentifier);
  if Node = nil then
    result := nil
  else
    result := TBGRAGlyph(Node.Data);
end;

procedure TBGRACustomTypeWriter.SetGlyph(AIdentifier: string; AValue: TBGRAGlyph);
var Node: TAvgLvlTreeNode;
begin
  if AValue.Identifier <> AIdentifier then
    raise exception.Create('Identifier mismatch');
  Node := FindGlyph(AIdentifier);
  if Node <> nil then
  begin
    if pointer(AValue) <> Node.Data then
      TBGRAGlyph(Node.Data).Free;
    Node.Data := AValue;
  end else
    FGlyphs.Add(pointer(AValue));
end;

procedure TBGRACustomTypeWriter.SetBidiMode(AValue: TFontBidiMode);
begin
  if FBidiMode=AValue then Exit;
  FBidiMode:=AValue;
end;

procedure TBGRACustomTypeWriter.GlyphCallbackForTextFitInfoBeforeTransform(
  ATextUTF8: string; AGlyph: TBGRAGlyph; AFlags: TBrowseGlyphCallbackFlags;
  AData: Pointer; out AContinue: boolean);
var
  newWidth: Single;
  partialCharCount, charLen: Integer;
  p,pEnd: PChar;
begin
  AContinue := true;
  with TTextFitInfoCallbackData(AData^) do
  begin
    if Assigned(AGlyph) then
    begin
      newWidth := WidthAccumulator+AGlyph.Width;
      if newWidth < MaxWidth then
      begin
        WidthAccumulator := newWidth;
        inc(ByteCount, length(ATextUTF8));
        inc(CharCount, UTF8Length(ATextUTF8));
      end else
      begin
        AContinue := false;
        if gcfMerged in AFlags then
        begin
          partialCharCount := Trunc(UTF8Length(ATextUTF8)*(MaxWidth-WidthAccumulator)/AGlyph.Width);
          p := @ATextUTF8[1];
          pEnd := p+length(ATextUTF8);
          while (p<pEnd) and (partialCharCount > 0) do
          begin
            charLen := UTF8CharacterLength(p);
            inc(p, charLen);
            inc(ByteCount, charLen);
            inc(CharCount);
            dec(partialCharCount);
          end;
        end;
      end;
    end;
  end;
end;

procedure TBGRACustomTypeWriter.GlyphCallbackForDisplayInfo(ATextUTF8: string;
  AGlyph: TBGRAGlyph; AFlags: TBrowseGlyphCallbackFlags; AData: Pointer; out AContinue: boolean);
var
  m2: TAffineMatrix;
begin
  with TDisplayInfoCallbackData(AData^) do
  begin
    if Align in [twaLeft,twaMiddle,twaRight] then
      m2 := Matrix*AffineMatrixTranslation(0,-AGlyph.Height/2) else
    if Align in [twaBottomLeft,twaBottom,twaBottomRight] then
      m2 := Matrix*AffineMatrixTranslation(0,-AGlyph.Height)
    else
      m2 := Matrix;

    if gcfMirrored in AFlags then
      m2 := m2*AffineMatrixTranslation(AGlyph.Width,0)*AffineMatrixScale(-1,1);

    Info[InfoIndex].Glyph := AGlyph;
    Info[InfoIndex].Mirrored:= gcfMirrored in AFlags;
    Info[InfoIndex].Matrix := m2;

    Matrix := Matrix*AffineMatrixTranslation(AGlyph.Width,0);
    inc(InfoIndex);
  end;
  AContinue:= true;
end;

function TBGRACustomTypeWriter.CompareGlyph(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
begin
  result := CompareStr(TBGRAGlyph(Data1).Identifier,TBGRAGlyph(Data2).Identifier);
end;

function TBGRACustomTypeWriter.FindGlyph(AIdentifier: string): TAvgLvlTreeNode;
var Comp: integer;
  Node: TAvgLvlTreeNode;
begin
  Node:=FGlyphs.Root;
  while (Node<>nil) do begin
    Comp:=CompareStr(AIdentifier,TBGRAGlyph(Node.Data).Identifier);
    if Comp=0 then break;
    if Comp<0 then begin
      Node:=Node.Left
    end else begin
      Node:=Node.Right
    end;
  end;
  result := Node;
end;

constructor TBGRACustomTypeWriter.Create;
begin
  FGlyphs := TAvgLvlTree.CreateObjectCompare(@CompareGlyph);
  TypeWriterMatrix := AffineMatrixIdentity;
  OutlineMode:= twoFill;
  DrawGlyphsSimultaneously := false;
end;

function TBGRACustomTypeWriter.GetTextSizeBeforeTransform(ATextUTF8: string): TPointF;
begin
  result := PointF(0,0);
  BrowseGlyphs(ATextUTF8, @GlyphCallbackForTextSizeBeforeTransform, @result, false);
end;

procedure TBGRACustomTypeWriter.TextFitInfoBeforeTransform(ATextUTF8: string; AMaxWidth: single;
  out ACharCount, AByteCount: integer; out AUsedWidth: single);
var
  data: TTextFitInfoCallbackData;
begin
  data.WidthAccumulator:= 0;
  data.MaxWidth := AMaxWidth;
  data.CharCount:= 0;
  data.ByteCount:= 0;
  BrowseGlyphs(ATextUTF8, @GlyphCallbackForTextFitInfoBeforeTransform, @data, false);
  ACharCount:= data.CharCount;
  AByteCount:= data.ByteCount;
  AUsedWidth:= data.WidthAccumulator;
end;

procedure TBGRACustomTypeWriter.DrawGlyph(ADest: TBGRACanvas2D;
  AIdentifier: string; X, Y: Single; AAlign: TBGRATypeWriterAlignment; AMirrored: boolean);
begin
  GlyphPath(ADest, AIdentifier, X,Y, AAlign, AMirrored);
  DrawLastPath(ADest);
end;

procedure TBGRACustomTypeWriter.DrawText(ADest: TBGRACanvas2D; ATextUTF8: string;
  X, Y: Single; AAlign: TBGRATypeWriterAlignment);
var
  di: TBGRATextDisplayInfo;
  i: Integer;
begin
  di := GetDisplayInfo(ATextUTF8,x,y,AAlign);

  if (OutlineMode <> twoPath) and not DrawGlyphsSimultaneously then
  begin
    //draw each glyph
    for i := 0 to high(di) do
    begin
      ADest.beginPath;
      di[i].Glyph.Path(ADest, di[i].Matrix, di[i].Mirrored);
      DrawLastPath(ADest);
    end;
  end else
  begin
    ADest.beginPath;
    for i := 0 to high(di) do
      di[i].Glyph.Path(ADest, di[i].Matrix, di[i].Mirrored);
    DrawLastPath(ADest);
  end;
end;

procedure TBGRACustomTypeWriter.CopyTextPathTo(ADest: IBGRAPath; ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft);
var
  i: integer;
  di: TBGRATextDisplayInfo;
begin
  di := GetDisplayInfo(ATextUTF8,x,y,AAlign);
  for i := 0 to high(di) do
    di[i].Glyph.Path(ADest, di[i].Matrix, di[i].Mirrored);
end;

function TBGRACustomTypeWriter.GetGlyphBox(AIdentifier: string; X, Y: Single;
  AAlign: TBGRATypeWriterAlignment): TAffineBox;
var g: TBGRAGlyph;
  m: TAffineMatrix;
begin
  g := GetGlyph(AIdentifier);
  if g = nil then result := TAffineBox.EmptyBox else
  begin
    m := GetGlyphMatrix(g,X,Y,AAlign);
    result := TAffineBox.AffineBox(m*PointF(0,0),m*PointF(g.Width,0),m*PointF(0,g.Height));
  end;
end;

function TBGRACustomTypeWriter.GetTextBox(ATextUTF8: string; X, Y: Single;
  AAlign: TBGRATypeWriterAlignment): TAffineBox;
var
  m: TAffineMatrix;
  size: TPointF;
begin
  if ATextUTF8 = '' then result := TAffineBox.EmptyBox else
  begin
    size := GetTextSizeBeforeTransform(ATextUTF8);
    m := AffineMatrixTranslation(X,Y)*TypeWriterMatrix;
    if AAlign in[twaTop,twaMiddle,twaBottom] then m := m*AffineMatrixTranslation(-size.x/2,0) else
    if AAlign in[twaTopRight, twaRight, twaBottomRight] then m := m*AffineMatrixTranslation(-size.x,0);
    if AAlign in [twaLeft,twaMiddle,twaRight] then m := m*AffineMatrixTranslation(0,-size.y/2) else
    if AAlign in [twaBottomLeft,twaBottom,twaBottomRight] then m := m*AffineMatrixTranslation(0,-size.y);
    result := TAffineBox.AffineBox(m*PointF(0,0),m*PointF(size.x,0),m*PointF(0,size.y));
  end;
end;

function TBGRACustomTypeWriter.GetTextGlyphBoxes(ATextUTF8: string; X, Y: Single;
  AAlign: TBGRATypeWriterAlignment): TGlyphBoxes;
var
  di: TBGRATextDisplayInfo;
  i: Integer;
begin
  di := GetDisplayInfo(ATextUTF8, X,Y, AAlign);
  setlength(result, length(di));
  for i := 0 to high(result) do
  with di[i] do
  begin
    result[i].Glyph := Glyph.Identifier;
    result[i].Box := TAffineBox.AffineBox(Matrix*PointF(0,0),Matrix*PointF(Glyph.Width,0),Matrix*PointF(0,Glyph.Height));
  end;
end;

procedure TBGRACustomTypeWriter.NeedGlyphRange(AUnicodeFrom, AUnicodeTo: Cardinal);
var c: cardinal;
begin
  for c := AUnicodeFrom to AUnicodeTo do
    GetGlyph(UnicodeCharToUTF8(c));
end;

procedure TBGRACustomTypeWriter.NeedGlyphAnsiRange;
var i: integer;
begin
  for i := 0 to 255 do
    GetGlyph(AnsiToUtf8(chr(i)));
end;

function TBGRACustomTypeWriter.GetDisplayInfo(ATextUTF8: string; X,
  Y: Single; AAlign: TBGRATypeWriterAlignment): TBGRATextDisplayInfo;
var
  data: TDisplayInfoCallbackData;
begin
  data.Align := AAlign;
  data.Matrix := GetTextMatrix(ATextUTF8, X,Y,AAlign);
  setlength(data.Info, UTF8Length(ATextUTF8));
  data.InfoIndex := 0;
  BrowseGlyphs(ATextUTF8, @GlyphCallbackForDisplayInfo, @data, true);
  setlength(data.Info, data.InfoIndex);
  result := data.Info;
end;

procedure TBGRACustomTypeWriter.GlyphPath(ADest: TBGRACanvas2D; AIdentifier: string;
  X, Y: Single; AAlign: TBGRATypeWriterAlignment; AMirrored: boolean);
var g: TBGRAGlyph;
begin
  ADest.beginPath;
  g := GetGlyph(AIdentifier);
  if g = nil then exit;
  if AMirrored then
    g.Path(ADest, GetGlyphMatrix(g,X,Y,AAlign), AMirrored)
  else
    g.Path(ADest, GetGlyphMatrix(g,X,Y,AAlign)*AffineMatrixTranslation(g.Width,0)*AffineMatrixScale(-1,1), AMirrored);
end;

procedure TBGRACustomTypeWriter.DrawLastPath(ADest: TBGRACanvas2D);
begin
  case OutlineMode of
  twoPath: ;
  twoFill: ADest.fill;
  twoStroke: ADest.stroke;
  twoFillOverStroke: ADest.fillOverStroke;
  twoStrokeOverFill: ADest.strokeOverFill;
  twoFillThenStroke: begin ADest.fill; ADest.stroke; end;
  twoStrokeThenFill: begin ADest.stroke; ADest.fill; end;
  end;
end;

procedure TBGRACustomTypeWriter.ClearGlyphs;
begin
  FGlyphs.FreeAndClear;
end;

procedure TBGRACustomTypeWriter.RemoveGlyph(AIdentifier: string);
var Node: TAvgLvlTreeNode;
begin
  Node := FindGlyph(AIdentifier);
  if Node <> nil then FGlyphs.Delete(Node);
end;

procedure TBGRACustomTypeWriter.AddGlyph(AGlyph: TBGRAGlyph);
begin
  Glyph[AGlyph.Identifier] := AGlyph;
end;

procedure TBGRACustomTypeWriter.SaveGlyphsToStream(AStream: TStream);
var Enumerator: TAvgLvlTreeNodeEnumerator;
begin
  LEWriteLongint(AStream,CustomHeaderSize);
  WriteCustomHeader(AStream);

  Enumerator := FGlyphs.GetEnumerator;
  while Enumerator.MoveNext do
    TBGRAGlyph(Enumerator.Current.Data).SaveToStream(AStream);
  Enumerator.Free;
end;

procedure TBGRACustomTypeWriter.LoadGlyphsFromFile(AFilenameUTF8: string);
var Stream: TFileStreamUTF8;
begin
  Stream := nil;
  try
    Stream := TFileStreamUTF8.Create(AFilenameUTF8, fmOpenRead);
    LoadGlyphsFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBGRACustomTypeWriter.LoadGlyphsFromStream(AStream: TStream);
var Header: TBGRACustomTypeWriterHeader;
  i: integer;
  g: TBGRAGlyph;
  HeaderSize: integer;
  GlyphStartPosition: Int64;
begin
  HeaderSize := LEReadLongint(AStream);
  GlyphStartPosition:= AStream.Position+HeaderSize;
  Header := ReadCustomTypeWriterHeader(AStream);
  if header.HeaderName <> HeaderName then
    raise exception.Create('Invalid file format ("'+header.HeaderName+'" should be "'+HeaderName+'")');
  ReadAdditionalHeader(AStream);
  AStream.Position:= GlyphStartPosition;
  for i := 0 to Header.NbGlyphs-1 do
  begin
    g := TBGRAGlyph.LoadFromStream(AStream);
    AddGlyph(g);
  end;
end;

procedure TBGRACustomTypeWriter.SaveGlyphsToFile(AFilenameUTF8: string);
var Stream: TFileStreamUTF8;
begin
  Stream := nil;
  try
    Stream := TFileStreamUTF8.Create(AFilenameUTF8, fmCreate or fmOpenWrite);
    SaveGlyphsToStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TBGRACustomTypeWriter.GetGlyphMatrix(AGlyph: TBGRAGlyph; X, Y: Single;
  AAlign: TBGRATypeWriterAlignment): TAffineMatrix;
var tGlyph: TPointF;
begin
  if AGlyph = nil then
  begin
    result := AffineMatrixIdentity;
    exit;
  end;
  tGlyph := PointF(0,0);
  if AAlign in [twaTop,twaMiddle,twaBottom] then tGlyph.X -= AGlyph.Width/2;
  if AAlign in [twaTopRight,twaRight,twaBottomRight] then tGlyph.X -= AGlyph.Width;
  if AAlign in [twaLeft,twaMiddle,twaRight] then tGlyph.Y -= AGlyph.Height/2;
  if AAlign in [twaBottomLeft,twaBottom,twaBottomRight] then tGlyph.Y -= AGlyph.Height;
  result := AffineMatrixTranslation(X,Y)*TypeWriterMatrix*AffineMatrixTranslation(tGlyph.X,tGlyph.Y);
end;

function TBGRACustomTypeWriter.GetTextMatrix(ATextUTF8: string; X, Y: Single;
  AAlign: TBGRATypeWriterAlignment): TAffineMatrix;
var
  tGlyph, size: TPointF;
begin
  tGlyph := PointF(0,0);
  if not (AAlign in [twaLeft,twaTopLeft,twaBottomLeft]) then
  begin
    size := GetTextSizeBeforeTransform(ATextUTF8);
    if AAlign in[twaTop,twaMiddle,twaBottom] then tGlyph.X := size.x/2 else
    if AAlign in[twaTopRight, twaRight, twaBottomRight] then tGlyph.X := size.x;
  end;
  result := AffineMatrixTranslation(X,Y)*TypeWriterMatrix*AffineMatrixTranslation(tGlyph.X,tGlyph.Y);
end;

function TBGRACustomTypeWriter.CustomHeaderSize: integer;
begin
  result := 1+length(HeaderName)+4;
end;

procedure TBGRACustomTypeWriter.WriteCustomHeader(AStream: TStream);
var lHeaderName: string;
begin
  lHeaderName:= HeaderName;
  LEWriteByte(AStream,length(lHeaderName));
  AStream.Write(lHeaderName[1],length(lHeaderName));
  LEWriteLongint(AStream,FGlyphs.Count);
end;

function TBGRACustomTypeWriter.ReadCustomTypeWriterHeader(AStream: TStream
  ): TBGRACustomTypeWriterHeader;
begin
  setlength(result.HeaderName, LEReadByte(AStream));
  AStream.Read(result.HeaderName[1],length(result.HeaderName));
  result.NbGlyphs:= LEReadLongint(AStream);
end;

procedure TBGRACustomTypeWriter.ReadAdditionalHeader(AStream: TStream);
begin
  //nothing
end;

function TBGRACustomTypeWriter.HeaderName: string;
begin
  result := 'TBGRACustomTypeWriter';
end;

procedure TBGRACustomTypeWriter.BrowseGlyphs(ATextUTF8: string;
  ACallback: TBrowseGlyphCallback; AData: pointer; ADisplayOrder: boolean);
type
  TCharInfo = record
    charStart, charEnd: integer;
    bidiInfo: PUnicodeBidiInfo;
  end;
  function CharEquals(const info: TCharInfo; text: string): boolean;
  var
    i: Integer;
  begin
    if info.charEnd-info.charStart >= length(text) then
    begin
      for i := 1 to length(text) do
        if ATextUTF8[info.charStart+i-1] <> text[i] then exit(false);
      result := true;
    end else
      result := false;
  end;
var
  bidiArray: TBidiUTF8Array;
  charInfo: array of TCharInfo;

  procedure OrderedCharInfo;
  var
    displayOrder: TUnicodeDisplayOrder;
    bidiIdx, orderIndex, nb: integer;
  begin
    displayOrder := GetUTF8DisplayOrder(bidiArray);
    orderIndex := 0;
    nb := 0;
    for orderIndex := 0 to high(displayOrder) do
      if bidiArray[displayOrder[orderIndex]].BidiInfo.IsMulticharStart then inc(nb);
    setlength(charInfo, nb);
    nb := 0;
    for orderIndex := 0 to high(displayOrder) do
    if bidiArray[displayOrder[orderIndex]].BidiInfo.IsMulticharStart then
    begin
      bidiIdx := displayOrder[orderIndex];
      charInfo[nb].charStart := bidiArray[bidiIdx].Offset+1;
      charInfo[nb].bidiInfo := @bidiArray[bidiIdx].BidiInfo;
      while (bidiIdx < high(bidiArray)) and
        not bidiArray[bidiIdx+1].BidiInfo.IsMulticharStart do inc(bidiIdx);
      if bidiIdx < high(bidiArray) then charInfo[nb].charEnd := bidiArray[bidiIdx+1].Offset+1
      else charInfo[nb].charEnd := length(ATextUTF8)+1;
      inc(nb);
    end;
  end;

  procedure UnorderedCharInfo;
  var
    i,nb: Integer;
  begin
    nb := 0;
    i := 0;
    while i <= high(bidiArray) do
    begin
      if not bidiArray[i].BidiInfo.IsRemoved then
      begin
        inc(nb);
        while (i < high(bidiArray)) and not bidiArray[i+1].BidiInfo.IsMulticharStart do inc(i);
      end;
      inc(i);
    end;
    setlength(charInfo,nb);
    nb := 0;
    i := 0;
    while i <= high(bidiArray) do
    begin
      if not bidiArray[i].BidiInfo.IsRemoved then
      begin
        charInfo[nb].charStart := bidiArray[i].Offset+1;
        charInfo[nb].bidiInfo:= @bidiArray[i].BidiInfo;
        while (i < high(bidiArray)) and not bidiArray[i+1].BidiInfo.IsMulticharStart do inc(i);
        if i < high(bidiArray) then charInfo[nb].charEnd := bidiArray[i+1].Offset+1
        else charInfo[nb].charEnd := length(ATextUTF8)+1;
        inc(nb);
      end;
      inc(i);
    end;
  end;

var
  cur,curStart: integer;
  curRTL,curRTLScript,curLigatureLeft,curLigatureRight,merged: boolean;

  procedure TryMerge(const AChars: array of string);
  var
    i: Integer;
    match: Boolean;
  begin
    if merged or (cur-1+length(AChars) > length(charInfo)) then exit;
    if length(AChars)<=1 then raise exception.Create('Expecting several characters');
    match := true;
    if not ADisplayOrder and curRTL then
    begin
      for i := 0 to high(AChars) do
        if not CharEquals(charInfo[cur-1+high(AChars)-i], AChars[i]) then match := false;
    end else
      for i := 0 to high(AChars) do
        if not CharEquals(charInfo[cur-1+i], AChars[i]) then match := false;
    if match then
    begin
      inc(cur, length(AChars)-1);
      if not ADisplayOrder and curRTL then
        curLigatureLeft:= charInfo[cur-1].bidiInfo^.HasLigatureLeft
      else
        curLigatureRight:= charInfo[cur-1].bidiInfo^.HasLigatureRight;
      merged := true;
    end;
  end;

var
  nextchar,glyphId: string;
  g: TBGRAGlyph;
  u: Cardinal;
  shouldContinue: boolean;
  flags: TBrowseGlyphCallbackFlags;
  i,charDestPos,charLen: integer;
begin
  if ATextUTF8 = '' then exit;

  if BidiMode = fbmAuto then bidiArray := AnalyzeBidiUTF8(ATextUTF8)
  else bidiArray := AnalyzeBidiUTF8(ATextUTF8, BidiMode = fbmRightToLeft);
  if ADisplayOrder then OrderedCharInfo else UnorderedCharInfo;

  cur := 0;
  while cur < length(charInfo) do
  begin
    curStart := cur;
    curRTL:= charInfo[cur].bidiInfo^.IsRightToLeft;
    curRTLScript := charInfo[cur].bidiInfo^.IsRightToLeftScript;
    curLigatureLeft:= charInfo[cur].bidiInfo^.HasLigatureLeft;
    curLigatureRight:= charInfo[cur].bidiInfo^.HasLigatureRight;
    merged := false;
    inc(cur);
    TryMerge(['f','f','i']);
    TryMerge(['f','f','l']);
    TryMerge(['f','f']);
    TryMerge(['f','i']);
    TryMerge(['f','l']);
    TryMerge([aleph,lam]);
    TryMerge([alephHamzaAbove,lam]);
    TryMerge([alephHamzaBelow,lam]);
    TryMerge([alephMaddaAbove,lam]);
    //text extract correspond to the unordered actual sequence of characters
    setlength(nextchar, max(charInfo[curStart].charEnd, charInfo[cur-1].charEnd)
                       -min(charInfo[curStart].charStart, charInfo[cur-1].charStart));
    move(ATextUTF8[min(charInfo[curStart].charStart, charInfo[cur-1].charStart)], nextchar[1], length(nextchar));
    //glyph direction corresponds to script direction
    if (curRTL and not ADisplayOrder) <> curRTLScript then
    begin
      setlength(glyphId, length(nextChar));
      charDestPos := 1;
      for i := cur-1 downto curStart do
      begin
        charLen := charInfo[i].charEnd-charInfo[i].charStart;
        move(ATextUTF8[charInfo[i].charStart], glyphId[charDestPos], charLen);
        inc(charDestPos, charLen);
      end;
    end else
    begin
      setlength(glyphId, length(nextChar));
      charDestPos := 1;
      for i := curStart to cur-1 do
      begin
        charLen := charInfo[i].charEnd-charInfo[i].charStart;
        move(ATextUTF8[charInfo[i].charStart], glyphId[charDestPos], charLen);
        inc(charDestPos, charLen);
      end;
    end;
    if (curLigatureRight and curRTLScript) or
       (curLigatureLeft and not curRTLScript) then glyphId := UnicodeCharToUTF8(UNICODE_ZERO_WIDTH_JOINER)+glyphId;
    if (curLigatureLeft and curRTLScript) or
       (curLigatureRight and not curRTLScript) then glyphId := glyphId+UnicodeCharToUTF8(UNICODE_ZERO_WIDTH_JOINER);
    g := GetGlyph(glyphId);
    if g <> nil then
    begin
      flags := [];
      if merged then include(flags, gcfMerged);
      if curRTL then include(flags, gcfRightToLeft);
      if curRTL and (UTF8Length(glyphId)=1) then
      begin
        u := UTF8CodepointToUnicode(pchar(glyphId), length(glyphId));
        if IsUnicodeMirrored(u) then include(flags, gcfMirrored);
      end;
      ACallback(nextchar, g, flags, AData, shouldContinue);
      if not shouldContinue then break;
    end;
  end;
end;

procedure TBGRACustomTypeWriter.BrowseAllGlyphs(
  ACallback: TBrowseGlyphCallback; AData: pointer);
var
  g: TAvgLvlTreeNode;
  shouldContinue: boolean;
begin
  g := FGlyphs.FindLowest;
  shouldContinue:= true;
  while Assigned(g) and shouldContinue do
  begin
    ACallback(TBGRAGlyph(g.Data).Identifier, TBGRAGlyph(g.Data), [],
      AData, shouldContinue);
    g := g.Successor;
  end;
end;

procedure TBGRACustomTypeWriter.GlyphCallbackForTextSizeBeforeTransform(
  ATextUTF8: string; AGlyph: TBGRAGlyph; AFlags: TBrowseGlyphCallbackFlags;
  AData: pointer; out AContinue: boolean);
var
  gSizeY: Single;
begin
  if Assigned(AGlyph) then
  begin
    with TPointF(AData^) do
    begin
      x += AGlyph.Width;
      gSizeY := AGlyph.Height;
      if gSizeY > y then y := gSizeY;
    end;
  end;
  AContinue:= true;
end;

destructor TBGRACustomTypeWriter.Destroy;
begin
  FGlyphs.FreeAndClear;
  FGlyphs.Free;
  inherited Destroy;
end;

end.

