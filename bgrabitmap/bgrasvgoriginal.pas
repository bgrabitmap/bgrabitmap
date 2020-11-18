// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
unit BGRASVGOriginal;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, BGRABitmap, BGRASVG, BGRATransform,
  BGRALayerOriginal, BGRAUnits, BGRALayers;

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
    procedure ComputePresentation(AContainerWidth, AContainerHeight: integer; AScaleDPI: single);
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
      AContainerHeight: integer = 480; AScaleDPI: single = 1);
    procedure LoadSVGFromStream(AStream: TStream; AContainerWidth: integer = 640;
      AContainerHeight: integer = 480; AScaleDPI: single = 1);
    function GetSVGCopy: TBGRASVG;
    class function StorageClassName: RawByteString; override;
    property Width: single read GetSvgWidth;
    property Height: single read GetSvgHeight;
    property DPI: single read GetDPI write SetDPI;
    property PresentationMatrix: TAffineMatrix read FPresentationMatrix;
  end;

  { TBGRALayeredSVG }

  TBGRALayeredSVG = class(TBGRALayeredBitmap)
    protected
      function GetMimeType: string; override;
      procedure InternalLoadFromStream(AStream: TStream);
      procedure InternalSaveToStream(AStream: TStream);
    public
      ContainerWidth, ContainerHeight, DefaultSvgDPI, DPI: integer;
      DefaultLayerName: string;
      constructor Create; overload; override;
      constructor Create(AWidth, AHeight: integer); overload; override;
      procedure LoadFromStream(AStream: TStream); override;
      procedure LoadFromFile(const filenameUTF8: string); override;
      procedure SaveToStream(AStream: TStream); override;
      procedure SaveToFile(const filenameUTF8: string); override;
  end;

implementation

uses BGRACanvas2D, BGRAMemDirectory, BGRAUTF8, BGRASVGShapes, math, BGRASVGType,
  BGRAVectorize;

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

procedure TBGRALayerSVGOriginal.ComputePresentation(AContainerWidth, AContainerHeight: integer;
  AScaleDPI: single);
var
  compWidth, compHeight: single;
begin
  FSVG.Units.ContainerWidth := FloatWithCSSUnit(AContainerWidth / AScaleDPI, cuPixel);
  FSVG.Units.ContainerHeight := FloatWithCSSUnit(AContainerHeight / AScaleDPI, cuPixel);
  compWidth := FSVG.WidthAsPixel;
  compHeight := FSVG.HeightAsPixel;
  FSVG.WidthAsPixel := compWidth * AScaleDPI;
  FSVG.HeightAsPixel := compHeight * AScaleDPI;
  FPresentationMatrix := FSVG.PresentationMatrix[cuPixel, true];
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
    c2D.fontRenderer := TBGRAVectorizedFontRenderer.Create;
    if ADraft then c2D.antialiasing := false;
    FSVG.Draw(c2D, 0, 0, cuPixel, False);
    c2D.Free;
  end;
end;

function TBGRALayerSVGOriginal.GetRenderBounds(ADestRect: TRect;
  AMatrix: TAffineMatrix): TRect;
var
  aff: TAffineBox;
  r: TRectF;
begin
  if Assigned(FSVG) then
  begin
    with FSVG.ViewBox do
      r := FSVG.GetStretchRectF(0, 0, size.x, size.y);
    aff := AMatrix * FPresentationMatrix * TAffineBox.AffineBox(r);
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
    FPresentationMatrix := FSVG.PresentationMatrix[cuPixel, true];
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
      AContainerHeight: integer; AScaleDPI: single);
begin
  BeginUpdate;
  FSVG.Free;
  FSVG := ASVG;
  ComputePresentation(AContainerWidth, AContainerHeight, AScaleDPI);
  Inc(FContentVersion);
  EndUpdate;
end;

procedure TBGRALayerSVGOriginal.LoadSVGFromStream(AStream: TStream; AContainerWidth: integer;
      AContainerHeight: integer; AScaleDPI: single);
begin
  BeginUpdate;
  FSVG.LoadFromStream(AStream);
  ComputePresentation(AContainerWidth, AContainerHeight, AScaleDPI);
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
    svg.DefaultDpi:= DPI;
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

{ TBGRALayeredSVG }

function TBGRALayeredSVG.GetMimeType: string;
begin
  Result:= 'image/svg+xml';
end;

procedure TBGRALayeredSVG.InternalLoadFromStream(AStream: TStream);
var
  svg, svgLayer: TBGRASVG;
  visualWidth, visualHeight: single;
  svgOrig: TBGRALayerSVGOriginal;
  idx, i, j: Integer;
  layer: TSVGGroup;
  prefix: String;
  originalViewBox: TSVGViewBox;
begin
  svg := TBGRASVG.Create;
  try
    svg.DefaultDpi := DefaultSvgDPI;
    svg.LoadFromStream(AStream);
    svg.Units.ContainerWidth := FloatWithCSSUnit(ContainerWidth / DPI * svg.DefaultDpi, cuPixel);
    svg.Units.ContainerHeight := FloatWithCSSUnit(ContainerHeight / DPI * svg.DefaultDpi, cuPixel);
    //keep only viewbox
    visualWidth := svg.VisualWidthAsPixel * DPI / svg.DefaultDpi;
    visualHeight := svg.VisualHeightAsPixel * DPI / svg.DefaultDpi;
    svg.WidthAsPixel := visualWidth;
    svg.HeightAsPixel := visualHeight;
    Clear;
    SetSize(floor(visualWidth + 0.95),floor(visualHeight + 0.95));
    if svg.LayerCount > 0 then
    begin
      for i := 0 to svg.LayerCount-1 do
      begin
        layer := svg.Layer[i];
        svgLayer := TBGRASVG.Create;
        svgLayer.DefaultDpi:= svg.DefaultDpi;
        svgLayer.WidthAsPixel := visualWidth;
        svgLayer.HeightAsPixel := visualHeight;
        for j := 0 to svg.NamespaceCount-1 do
        begin
          prefix := svg.NamespacePrefix[j];
          svgLayer.NamespaceURI[prefix] := svg.NamespaceURI[prefix];
        end;
        try
          svgLayer.ViewBox := svg.ViewBox;
          if layer.DOMElement.hasAttribute('bgra:originalViewBox') then
          begin
            originalViewBox := TSVGViewBox.Parse(layer.DOMElement.GetAttribute('bgra:originalViewBox'));
            svgLayer.WidthAsPixel := originalViewBox.size.x;
            svgLayer.HeightAsPixel := originalViewBox.size.y;
            svgLayer.ViewBox := originalViewBox;
          end;
          for j := 0 to svg.Content.IndexOfElement(layer)-1 do
            if svg.Content.ElementObject[j] is TSVGDefine then
              svgLayer.Content.CopyElement(svg.Content.ElementObject[j]);
          for j := 0 to layer.Content.ElementCount-1 do
            svgLayer.Content.CopyElement(layer.Content.ElementObject[j]);
          svgOrig := TBGRALayerSVGOriginal.Create;
          svgOrig.SetSVG(svgLayer);
          svgLayer := nil;
          idx := AddLayerFromOwnedOriginal(svgOrig);
          LayerName[idx] := layer.Name;
          LayerVisible[idx] := layer.Visible;
          LayerOpacity[idx] := min(255,max(0,round(layer.opacity*255)));
          BlendOperation[idx] := layer.mixBlendMode;
          LayerOriginalMatrix[idx] := layer.matrix[cuPixel];
          RenderLayerFromOriginal(idx);
        finally
          svgLayer.Free;
        end;
      end;
    end else
    begin
      svgOrig := TBGRALayerSVGOriginal.Create;
      svgOrig.SetSVG(svg);
      svg := nil;
      idx := AddLayerFromOwnedOriginal(svgOrig);
      LayerName[idx] := DefaultLayerName+'1';
      RenderLayerFromOriginal(idx);
    end;
  finally
    svg.Free;
  end;
end;

procedure TBGRALayeredSVG.InternalSaveToStream(AStream: TStream);

  procedure StoreLayerBitmap(ABitmap: TBGRABitmap; AOwned: boolean; const AMatrix: TAffineMatrix; AContent: TSVGContent);
  var
    img: TSVGImage;
  begin
    img := AContent.AppendImage(AMatrix[1,3], AMatrix[2, 3], ABitmap.Width, ABitmap.Height, ABitmap, AOwned);
    img.matrix[cuCustom] := AffineMatrixLinear(AMatrix);
  end;

  procedure StoreLayer(ALayerIndex: integer; ASVG: TBGRASVG; ADestElem: TSVGCustomElement;
        ADest: TSVGContent; out AMatrix: TAffineMatrix);
  var
    c: TBGRALayerOriginalAny;
    bmp: TBGRABitmap;
    layerSvg: TBGRASVG;
    minCoord: TPointF;
    i: Integer;
    prefix: String;
    origViewBox: TSVGViewBox;
  begin
    AMatrix := AffineMatrixIdentity;
    if LayerOriginalKnown[ALayerIndex] then
      c:= LayerOriginalClass[ALayerIndex]
      else c := nil;

    if c = TBGRALayerSVGOriginal then
    begin
      layerSvg := (LayerOriginal[ALayerIndex] as TBGRALayerSVGOriginal).GetSVGCopy;
      origViewBox := layerSvg.ViewBox;
      try
        minCoord := layerSvg.ViewMinInUnit[cuCustom];
        AMatrix:= LayerOriginalMatrix[ALayerIndex] * layerSvg.PresentationMatrix[cuPixel, true] *
          AffineMatrixTranslation(-minCoord.X, -minCoord.Y);
        layerSvg.ConvertToUnit(cuCustom);
        if ADestElem is TSVGGroup then
        with TSVGGroup(ADestElem) do
        begin
          DOMElement.SetAttribute('xmlns:bgra', 'https://wiki.freepascal.org/LazPaint_SVG_format');
          DOMElement.SetAttribute('bgra:originalViewBox', origViewBox.ToString);
        end;
        for i := 0 to layerSvg.Content.ElementCount-1 do
          ADest.CopyElement(layerSvg.Content.ElementObject[i]);
        for i := 0 to layerSvg.NamespaceCount-1 do
        begin
          prefix := layerSvg.NamespacePrefix[i];
          ASVG.NamespaceURI[prefix] := layerSvg.NamespaceURI[prefix];
        end;
      finally
        layerSvg.Free;
      end;
    end else
    if c = TBGRALayerImageOriginal then
    begin
      bmp := (LayerOriginal[ALayerIndex] as TBGRALayerImageOriginal).GetImageCopy;
      StoreLayerBitmap(bmp, true, LayerOriginalMatrix[ALayerIndex], ADest);
    end else
      StoreLayerBitmap(LayerBitmap[ALayerIndex], false,
        AffineMatrixTranslation(LayerOffset[ALayerIndex].X, LayerOffset[ALayerIndex].Y),
        ADest);
  end;

var
  svg: TBGRASVG;
  vb : TSVGViewBox;
  i: Integer;
  g: TSVGGroup;
  m: TAffineMatrix;
begin
  svg := TBGRASVG.Create;
  try
    svg.WidthAsPixel := Width;
    svg.HeightAsPixel := Height;
    vb.min := PointF(0, 0);
    vb.size := PointF(Width, Height);
    svg.ViewBox := vb;
    if (NbLayers = 1) and (LayerOpacity[0] = 255) and LayerVisible[0] and
       ((LayerOriginalGuid[0] = GUID_NULL) or IsAffineMatrixIdentity(LayerOriginalMatrix[0])) then
    begin
      StoreLayer(0, svg, svg, svg.Content, m);
    end else
    begin
      svg.NamespaceURI['inkscape'] := 'http://www.inkscape.org/namespaces/inkscape';
      for i := 0 to NbLayers-1 do
      begin
        g := svg.Content.AppendGroup;
        g.IsLayer := true;
        g.Name:= LayerName[i];
        g.opacity:= LayerOpacity[i]/255;
        g.Visible:= LayerVisible[i];
        g.mixBlendMode:= BlendOperation[i];
        StoreLayer(i, svg, g, g.Content, m);
        g.matrix[cuPixel] := m;
      end;
    end;
    svg.SaveToStream(AStream);
  finally
    svg.Free;
  end;
end;

constructor TBGRALayeredSVG.Create;
begin
  inherited Create;
  ContainerWidth:= 640;
  ContainerHeight:= 480;
  DefaultLayerName := 'Layer';
  DPI := 96;
  DefaultSvgDPI:= 96;
end;

constructor TBGRALayeredSVG.Create(AWidth, AHeight: integer);
begin
  inherited Create(AWidth, AHeight);
  ContainerWidth:= 640;
  ContainerHeight:= 480;
  DefaultLayerName := 'Layer';
end;

procedure TBGRALayeredSVG.LoadFromStream(AStream: TStream);
begin
  OnLayeredBitmapLoadFromStreamStart;
  try
    InternalLoadFromStream(AStream);
  finally
    OnLayeredBitmapLoaded;
  end;
end;

procedure TBGRALayeredSVG.LoadFromFile(const filenameUTF8: string);
var AStream: TFileStreamUTF8;
begin
  AStream := TFileStreamUTF8.Create(filenameUTF8,fmOpenRead or fmShareDenyWrite);
  OnLayeredBitmapLoadStart(filenameUTF8);
  try
    LoadFromStream(AStream);
  finally
    OnLayeredBitmapLoaded;
    AStream.Free;
  end;
end;

procedure TBGRALayeredSVG.SaveToStream(AStream: TStream);
begin
  OnLayeredBitmapSaveToStreamStart;
  try
    InternalSaveToStream(AStream);
  finally
    OnLayeredBitmapSaved;
  end;
end;

procedure TBGRALayeredSVG.SaveToFile(const filenameUTF8: string);
var AStream: TFileStreamUTF8;
begin
  AStream := TFileStreamUTF8.Create(filenameUTF8,fmCreate);
  OnLayeredBitmapSaveStart(filenameUTF8);
  try
    InternalSaveToStream(AStream);
  finally
    OnLayeredBitmapSaved;
    AStream.Free;
  end;
end;

procedure RegisterLayeredSvgFormat;
begin
  RegisterLayeredBitmapReader('svg', TBGRALayeredSVG);
  RegisterLayeredBitmapWriter('svg', TBGRALayeredSVG);
end;

initialization

  RegisterLayerOriginal(TBGRALayerSVGOriginal);
  RegisterLayeredSvgFormat;

end.

