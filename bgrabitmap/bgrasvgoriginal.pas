// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
unit BGRASVGOriginal;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, BGRABitmap, BGRASVG, BGRATransform,
  BGRALayerOriginal, BGRAUnits;

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
    FPresentationMatrix: TAffineMatrix;
    FDiff: TBGRASVGOriginalDiff;
    FContentVersion: integer;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ComputePresentation(AContainerWidth, AContainerHeight: integer);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix): TRect; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveSVGToStream(AStream: TStream);
    procedure SetSVG(ASVG: TBGRASVG; AContainerWidth: integer = 640;
      AContainerHeight: integer = 480);
    procedure LoadSVGFromStream(AStream: TStream; AContainerWidth: integer = 640;
      AContainerHeight: integer = 480);
    function GetSVGCopy: TBGRASVG;
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
  result := FSVG.HeightAsPixel;
end;

function TBGRALayerSVGOriginal.GetSvgWidth: single;
begin
  result := FSVG.WidthAsPixel;
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

procedure TBGRALayerSVGOriginal.ComputePresentation(AContainerWidth, AContainerHeight: integer);
var
  compWidth, compHeight: TFloatWithCSSUnit;
  presentationRect: TRectF;
  sx, sy, visualWidth, visualHeight: Single;
begin
  FSVG.Units.ContainerWidth := FloatWithCSSUnit(AContainerWidth, cuPixel);
  FSVG.Units.ContainerHeight := FloatWithCSSUnit(AContainerHeight, cuPixel);
  compWidth := FSVG.ComputedWidth;
  compHeight := FSVG.ComputedHeight;
  FSVG.Width := compWidth;
  FSVG.Height := compHeight;
  presentationRect := FSVG.GetStretchRectF(0, 0, FSVG.WidthAsPixel, FSVG.HeightAsPixel);
  visualWidth := FSVG.Units.ConvertWidth(FSVG.VisualWidth, cuPixel).value;
  visualHeight := FSVG.Units.ConvertWidth(FSVG.VisualHeight, cuPixel).value;
  if FSVG.WidthAsPixel > 0 then sx := presentationRect.Width/visualWidth else sx := 1;
  if FSVG.HeightAsPixel > 0 then sy := presentationRect.Height/visualHeight else sy := 1;
  FPresentationMatrix := AffineMatrixTranslation(presentationRect.Left, presentationRect.Top)
                       * AffineMatrixScale(sx, sy);
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
    c2D.transform(AMatrix*FPresentationMatrix);
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
    aff := AMatrix * FPresentationMatrix
          * TAffineBox.AffineBox(min,PointF(min.x+size.x,min.y),PointF(min.x,min.y+size.y));
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

procedure TBGRALayerSVGOriginal.SetSVG(ASVG: TBGRASVG; AContainerWidth: integer;
      AContainerHeight: integer);
begin
  BeginUpdate;
  FSVG.Free;
  FSVG := ASVG;
  ComputePresentation(AContainerWidth, AContainerHeight);
  Inc(FContentVersion);
  EndUpdate;
end;

procedure TBGRALayerSVGOriginal.LoadSVGFromStream(AStream: TStream; AContainerWidth: integer; AContainerHeight: integer);
begin
  BeginUpdate;
  FSVG.LoadFromStream(AStream);
  ComputePresentation(AContainerWidth, AContainerHeight);
  Inc(FContentVersion);
  EndUpdate;
end;

function TBGRALayerSVGOriginal.GetSVGCopy: TBGRASVG;
var
  stream: TMemoryStream;
  svg: TBGRASVG;
begin
  stream := TMemoryStream.Create;
  svg := nil;
  try
    FSVG.SaveToStream(stream);
    stream.Position:= 0;
    svg := TBGRASVG.Create;
    svg.LoadFromStream(stream);
    result := svg;
    svg := nil;
  finally
    svg.Free;
    stream.Free
  end;
end;

class function TBGRALayerSVGOriginal.StorageClassName: RawByteString;
begin
  result := 'svg';
end;

initialization

  RegisterLayerOriginal(TBGRALayerSVGOriginal);

end.

