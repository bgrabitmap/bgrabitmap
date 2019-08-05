unit BGRASVGOriginal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, BGRABitmap, BGRASVG, BGRATransform, BGRALayerOriginal;

type
  TBGRALayerSVGOriginal = class;

  { TBGRASVGOriginalDiff }

  TBGRASVGOriginalDiff = class(TBGRAOriginalDiff)
  protected
    FContentVersionBefore,FContentVersionAfter: integer;
    FSvgStreamBefore,FSvgStreamAfter: TMemoryStream;
    FDpiBefore, FDpiAfter: single;
  public
    constructor Create(AFromOriginal: TBGRALayerSVGOriginal);
    procedure ComputeDiff(AToOriginal: TBGRALayerSVGOriginal);
    procedure Apply(AOriginal: TBGRALayerCustomOriginal); override;
    procedure Unapply(AOriginal: TBGRALayerCustomOriginal); override;
    function CanAppend(ADiff: TBGRAOriginalDiff): boolean; override;
    procedure Append(ADiff: TBGRAOriginalDiff); override;
    function IsIdentity: boolean; override;
    destructor Destroy; override;
  end;

  { TBGRALayerSVGOriginal }

  TBGRALayerSVGOriginal = class(TBGRALayerCustomOriginal)
  private
    function GetDPI: single;
    function GetSvgHeight: single;
    function GetSvgWidth: single;
    procedure SetDPI(AValue: single);
  protected
    FSVG: TBGRASVG;
    FDiff: TBGRASVGOriginalDiff;
    FContentVersion: integer;
    procedure BeginUpdate;
    procedure EndUpdate;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix): TRect; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveSVGToStream(AStream: TStream);
    procedure LoadSVGFromStream(AStream: TStream);
    class function StorageClassName: RawByteString; override;
    property Width: single read GetSvgWidth;
    property Height: single read GetSvgHeight;
    property DPI: single read GetDPI write SetDPI;
  end;

implementation

uses BGRACanvas2D, BGRAMemDirectory;

{ TBGRASVGOriginalDiff }

constructor TBGRASVGOriginalDiff.Create(AFromOriginal: TBGRALayerSVGOriginal);
begin
  if Assigned(AFromOriginal.FSVG) then
  begin
    FSvgStreamBefore := TMemoryStream.Create;
    AFromOriginal.FSVG.SaveToStream(FSvgStreamBefore);
  end;
  FContentVersionBefore:= AFromOriginal.FContentVersion;
  FDpiBefore:= AFromOriginal.DPI;
end;

procedure TBGRASVGOriginalDiff.ComputeDiff(AToOriginal: TBGRALayerSVGOriginal);
begin
  FreeAndNil(FSvgStreamAfter);
  if Assigned(AToOriginal.FSVG) then
  begin
    FSvgStreamAfter := TMemoryStream.Create;
    AToOriginal.FSVG.SaveToStream(FSvgStreamAfter);
  end;
  FContentVersionAfter:= AToOriginal.FContentVersion;
  FDpiAfter:= AToOriginal.DPI;
end;

procedure TBGRASVGOriginalDiff.Apply(AOriginal: TBGRALayerCustomOriginal);
var
  orig: TBGRALayerSVGOriginal;
begin
  orig := AOriginal as TBGRALayerSVGOriginal;
  if Assigned(FSvgStreamAfter) then
  begin
    FSvgStreamAfter.Position:= 0;
    orig.FSVG.LoadFromStream(FSvgStreamAfter);
  end else
    orig.FSVG.Content.Clear;
  orig.FContentVersion := FContentVersionAfter;
end;

procedure TBGRASVGOriginalDiff.Unapply(AOriginal: TBGRALayerCustomOriginal);
var
  orig: TBGRALayerSVGOriginal;
begin
  orig := AOriginal as TBGRALayerSVGOriginal;
  if Assigned(FSvgStreamBefore) then
  begin
    FSvgStreamBefore.Position:= 0;
    orig.FSVG.LoadFromStream(FSvgStreamBefore);
  end else
    orig.FSVG.Content.Clear;
  orig.FContentVersion := FContentVersionBefore;
end;

function TBGRASVGOriginalDiff.CanAppend(ADiff: TBGRAOriginalDiff): boolean;
begin
  result := (ADiff is TBGRASVGOriginalDiff) and
    (TBGRASVGOriginalDiff(ADiff).FContentVersionAfter >= FContentVersionAfter);
end;

procedure TBGRASVGOriginalDiff.Append(ADiff: TBGRAOriginalDiff);
var
  next: TBGRASVGOriginalDiff;
begin
  next := ADiff as TBGRASVGOriginalDiff;
  if next.FContentVersionAfter < FContentVersionAfter then
    raise exception.Create('Cannot append diff made before this one.');
  FDpiAfter:= next.FDpiAfter;
  FreeAndNil(FSvgStreamAfter);
  if Assigned(next.FSvgStreamAfter) then
  begin
    FSvgStreamAfter := TMemoryStream.Create;
    next.FSvgStreamAfter.Position:= 0;
    FSvgStreamAfter.CopyFrom(next.FSvgStreamAfter, next.FSvgStreamAfter.Size);
  end;
  FContentVersionAfter:= next.FContentVersionAfter;
end;

function TBGRASVGOriginalDiff.IsIdentity: boolean;
begin
  result := (FDpiBefore = FDpiAfter) and
    ( ((FSvgStreamBefore=nil) and (FSvgStreamAfter=nil)) or
      (Assigned(FSvgStreamBefore) and Assigned(FSvgStreamAfter) and
       (FSvgStreamBefore.Size = FSvgStreamAfter.Size) and
       CompareMem(FSvgStreamBefore.Memory,FSvgStreamAfter.Memory,FSvgStreamBefore.Size)) );
end;

destructor TBGRASVGOriginalDiff.Destroy;
begin
  FSvgStreamBefore.Free;
  inherited Destroy;
end;

{ TBGRALayerSVGOriginal }

function TBGRALayerSVGOriginal.GetDPI: single;
begin
  result := FSVG.DefaultDpi;
end;

function TBGRALayerSVGOriginal.GetSvgHeight: single;
begin
  result := FSVG.ViewSizeInUnit[cuPixel].y;
end;

function TBGRALayerSVGOriginal.GetSvgWidth: single;
begin
  result := FSVG.ViewSizeInUnit[cuPixel].x;
end;

procedure TBGRALayerSVGOriginal.SetDPI(AValue: single);
begin
  BeginUpdate;
  FSVG.DefaultDpi:= AValue;
  EndUpdate;
end;

procedure TBGRALayerSVGOriginal.BeginUpdate;
begin
  if DiffExpected and (FDiff=nil) then
    FDiff := TBGRASVGOriginalDiff.Create(self);
end;

procedure TBGRALayerSVGOriginal.EndUpdate;
begin
  if Assigned(FDiff) then FDiff.ComputeDiff(self);
  NotifyChange(FDiff);
  FDiff := nil;
end;

constructor TBGRALayerSVGOriginal.Create;
begin
  inherited Create;
  FSVG := TBGRASVG.Create;
  FContentVersion := 0;
end;

destructor TBGRALayerSVGOriginal.Destroy;
begin
  FSVG.Free;
  FDiff.Free;
  inherited Destroy;
end;

procedure TBGRALayerSVGOriginal.Render(ADest: TBGRABitmap;
  AMatrix: TAffineMatrix; ADraft: boolean);
var
  c2D: TBGRACanvas2D;
begin
  if Assigned(FSVG) then
  begin
    c2D := TBGRACanvas2D.Create(ADest);
    c2D.transform(AMatrix);
    if ADraft then c2D.antialiasing := false;
    FSVG.Draw(c2D,0,0);
    c2D.Free;
  end;
end;

function TBGRALayerSVGOriginal.GetRenderBounds(ADestRect: TRect;
  AMatrix: TAffineMatrix): TRect;
var
  aff: TAffineBox;
  min, size: TPointF;
begin
  if Assigned(FSVG) then
  begin
    min := FSVG.ViewMinInUnit[cuPixel];
    size := FSVG.ViewSizeInUnit[cuPixel];
    aff := AMatrix*TAffineBox.AffineBox(min,PointF(min.x+size.x,min.y),PointF(min.x,min.y+size.y));
    result := aff.RectBounds;
  end else
    result := EmptyRect;
end;

procedure TBGRALayerSVGOriginal.LoadFromStorage(
  AStorage: TBGRACustomOriginalStorage);
var svgStream: TMemoryStream;
  valDpi: Single;
begin
  svgStream := TMemoryStream.Create;
  try
    if AStorage.ReadFile('content.svg', svgStream) then
    begin
      if not Assigned(FSVG) then FSVG := TBGRASVG.Create;
      svgStream.Position:= 0;
      FSVG.LoadFromStream(svgStream);
    end else
    begin
      FreeAndNil(FSVG);
      FSVG := TBGRASVG.Create;
    end;
    FContentVersion:= AStorage.Int['content-version'];
  finally
    svgStream.Free;
  end;
  valDpi := AStorage.Float['dpi'];
  if valDpi <> EmptySingle then
    FSVG.DefaultDpi:= valDpi;
end;

procedure TBGRALayerSVGOriginal.SaveToStorage(
  AStorage: TBGRACustomOriginalStorage);
var svgStream: TMemoryStream;
begin
  if Assigned(FSVG) then
  begin
    if FContentVersion > AStorage.Int['content-version'] then
    begin
      svgStream := TMemoryStream.Create;
      try
        FSVG.SaveToStream(svgStream);
        AStorage.WriteFile('content.svg', svgStream, true, true);
        svgStream := nil;
        AStorage.Int['content-version'] := FContentVersion;
      finally
        svgStream.Free;
      end;
    end;
    AStorage.Float['dpi'] := FSVG.DefaultDpi;
  end;
end;

procedure TBGRALayerSVGOriginal.LoadFromStream(AStream: TStream);
begin
  if TMemDirectory.CheckHeader(AStream) then
    inherited LoadFromStream(AStream)
  else
    LoadSVGFromStream(AStream);
end;

procedure TBGRALayerSVGOriginal.SaveSVGToStream(AStream: TStream);
begin
  FSVG.SaveToStream(AStream);
end;

procedure TBGRALayerSVGOriginal.LoadSVGFromStream(AStream: TStream);
begin
  BeginUpdate;
  FSVG.LoadFromStream(AStream);
  Inc(FContentVersion);
  EndUpdate;
end;

class function TBGRALayerSVGOriginal.StorageClassName: RawByteString;
begin
  result := 'svg';
end;

initialization

  RegisterLayerOriginal(TBGRALayerSVGOriginal);

end.

