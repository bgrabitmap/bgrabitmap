unit LinearRGBABitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, UniversalDrawer;

type

  { TLinearRGBABitmap }

  TLinearRGBABitmap = class(specialize TGenericUniversalBitmap<TLinearRGBA,TLinearRGBAColorspace>)
  protected
    function InternalNew: TCustomUniversalBitmap; override;
  public
    class procedure SolidBrush(out ABrush: TUniversalBrush; const AColor: TLinearRGBA; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    class procedure ScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode = dmDrawWithTransparency;
                                 AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure EraseBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
    class procedure AlphaBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
  end;

const
  LinearRGBATransparent : TLinearRGBA = (red:0; green:0; blue:0; alpha:0);

operator = (const c1, c2: TLinearRGBA): boolean; inline;

implementation

operator = (const c1, c2: TLinearRGBA): boolean;
begin
  if (c1.alpha = 0) and (c2.alpha = 0) then
    Result := True
  else
    Result := (c1.alpha = c2.alpha) and (c1.red = c2.red) and
      (c1.green = c2.green) and (c1.blue = c2.blue);
end;

procedure LinearRGBASolidBrushSkipPixels({%H-}AFixedData: Pointer;
    AContextData: PUniBrushContext; {%H-}AAlpha: Word; ACount: integer);
begin
  inc(PLinearRGBA(AContextData^.Dest), ACount);
end;

procedure LinearRGBASolidBrushSetPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
const oneOver65535 = 1/65535;
var
  pSrc,pDest: PLinearRGBA;
  alphaOver, finalAlpha, finalAlphaInv, residualAlpha: single;
begin
  if AAlpha=0 then
  begin
    inc(PLinearRGBA(AContextData^.Dest), ACount);
    exit;
  end;
  pDest := PLinearRGBA(AContextData^.Dest);
  if AAlpha=65535 then
  begin
    while ACount > 0 do
    begin
      pDest^ := PLinearRGBA(AFixedData)^;
      inc(pDest);
      dec(ACount);
    end;
  end else
  begin
    pSrc := PLinearRGBA(AFixedData);
    alphaOver := AAlpha*single(oneOver65535);
    while ACount > 0 do
    begin
      residualAlpha := pDest^.alpha*(1-alphaOver);
      finalAlpha := residualAlpha + pSrc^.alpha*alphaOver;
      if finalAlpha <= 0 then pDest^ := LinearRGBATransparent else
      begin
        pDest^.alpha:= finalAlpha;
        finalAlphaInv := 1/finalAlpha;
        pDest^.red := (pDest^.red*residualAlpha +
                        pSrc^.red*(finalAlpha-residualAlpha) ) * finalAlphaInv;
        pDest^.green := (pDest^.green*residualAlpha +
                         pSrc^.green*(finalAlpha-residualAlpha) ) * finalAlphaInv;
        pDest^.blue := (pDest^.blue*residualAlpha +
                        pSrc^.blue*(finalAlpha-residualAlpha) ) * finalAlphaInv;
      end;
      inc(pDest);
      dec(ACount);
    end;
  end;
  PLinearRGBA(AContextData^.Dest) := pDest;
end;

procedure LinearRGBASolidBrushDrawPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
const oneOver65535 = 1/65535;
var
  pSrc,pDest: PLinearRGBA;
  alphaOver, finalAlpha, finalAlphaInv, residualAlpha: single;
begin
  if AAlpha=0 then
  begin
    inc(PLinearRGBA(AContextData^.Dest), ACount);
    exit;
  end;
  pSrc := PLinearRGBA(AFixedData);
  pDest := PLinearRGBA(AContextData^.Dest);
  alphaOver := pSrc^.alpha*AAlpha*single(oneOver65535);
  while ACount > 0 do
  begin
    residualAlpha := pDest^.alpha*(1-alphaOver);
    finalAlpha := residualAlpha + alphaOver;
    if finalAlpha <= 0 then pDest^ := LinearRGBATransparent else
    begin
      pDest^.alpha:= finalAlpha;
      finalAlphaInv := 1/finalAlpha;
      pDest^.red := (pDest^.red*residualAlpha +
                      pSrc^.red*alphaOver ) * finalAlphaInv;
      pDest^.green := (pDest^.green*residualAlpha +
                       pSrc^.green*alphaOver ) * finalAlphaInv;
      pDest^.blue := (pDest^.blue*residualAlpha +
                      pSrc^.blue*alphaOver ) * finalAlphaInv;
    end;
    inc(pDest);
    dec(ACount);
  end;
  PLinearRGBA(AContextData^.Dest) := pDest;
end;

type
  PLinearRGBAScannerBrushFixedData = ^TLinearRGBAScannerBrushFixedData;
  TLinearRGBAScannerBrushFixedData = record
    Scanner: Pointer; //avoid ref count by using pointer type
    OffsetX, OffsetY: integer;
  end;

procedure LinearRGBAScannerBrushInitContext(AFixedData: Pointer;
  AContextData: PUniBrushContext);
begin
  with PLinearRGBAScannerBrushFixedData(AFixedData)^ do
    IBGRAScanner(Scanner).ScanMoveTo(AContextData^.Ofs.X + OffsetX,
                                     AContextData^.Ofs.Y + OffsetY);
end;

procedure LinearRGBAScannerBrushSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  src: TLinearRGBA;
begin
  with PLinearRGBAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PLinearRGBA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    while ACount > 0 do
    begin
      src := IBGRAScanner(Scanner).ScanNextExpandedPixel.ToLinearRGBA;
      LinearRGBASolidBrushSetPixels(@src, AContextData, AAlpha, 1);
      dec(ACount);
    end;
  end;
end;

procedure LinearRGBAScannerBrushSetPixelsExceptTransparent(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  src: TLinearRGBA;
  expPix: TExpandedPixel;
begin
  with PLinearRGBAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PLinearRGBA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    while ACount > 0 do
    begin
      expPix := IBGRAScanner(Scanner).ScanNextExpandedPixel;
      if expPix.alpha = 65535 then
      begin
        src := expPix.ToLinearRGBA;
        LinearRGBASolidBrushSetPixels(@src, AContextData, AAlpha, 1);
      end else
        inc(PLinearRGBA(AContextData^.Dest));
      dec(ACount);
    end;
  end;
end;

procedure LinearRGBAScannerBrushDrawPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  src: TLinearRGBA;
  expPix: TExpandedPixel;
begin
  with PLinearRGBAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PLinearRGBA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    while ACount > 0 do
    begin
      expPix := IBGRAScanner(Scanner).ScanNextExpandedPixel;
      if expPix.alpha = 65535 then
      begin
        src := expPix.ToLinearRGBA;
        LinearRGBASolidBrushSetPixels(@src, AContextData, AAlpha, 1);
      end else if expPix.alpha > 0 then
      begin
        src := expPix.ToLinearRGBA;
        LinearRGBASolidBrushDrawPixels(@src, AContextData, AAlpha, 1);
      end else
        inc(PLinearRGBA(AContextData^.Dest));
      dec(ACount);
    end;
  end;
end;

procedure LinearRGBAAlphaBrushSetPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
const oneOver65535 = 1/65535;
var
  pDest: PLinearRGBA;
  alphaOver, residualAlpha, finalAlpha: single;
begin
  if AAlpha=0 then
  begin
    inc(PLinearRGBA(AContextData^.Dest), ACount);
    exit;
  end;
  pDest := PLinearRGBA(AContextData^.Dest);
  if AAlpha=65535 then
  begin
    finalAlpha := PSingle(AFixedData)^;
    while ACount > 0 do
    begin
      pDest^.alpha := finalAlpha;
      inc(pDest);
      dec(ACount);
    end;
  end else
  begin
    alphaOver := AAlpha*single(oneOver65535);
    while ACount > 0 do
    begin
      residualAlpha := pDest^.alpha*(1-alphaOver);
      finalAlpha := residualAlpha + PSingle(AFixedData)^*alphaOver;
      pDest^.alpha:= finalAlpha;
      inc(pDest);
      dec(ACount);
    end;
  end;
  PLinearRGBA(AContextData^.Dest) := pDest;
end;

procedure LinearRGBAAlphaBrushDrawPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
const oneOver65535 = 1/65535;
var
  pDest: PLinearRGBA;
  alphaMul, finalAlpha: single;
begin
  if AAlpha=0 then
  begin
    inc(PLinearRGBA(AContextData^.Dest), ACount);
    exit;
  end;
  pDest := PLinearRGBA(AContextData^.Dest);
  if AAlpha<>65535 then
    alphaMul := 1-PSingle(AFixedData)^*AAlpha*single(oneOver65535)
  else
    alphaMul := 1-PSingle(AFixedData)^;
  while ACount > 0 do
  begin
    finalAlpha := pDest^.alpha*alphaMul;
    if finalAlpha <= 0 then pDest^ := LinearRGBATransparent else
      pDest^.alpha:= finalAlpha;
    inc(pDest);
    dec(ACount);
  end;
  PLinearRGBA(AContextData^.Dest) := pDest;
end;

{ TLinearRGBABitmap }

function TLinearRGBABitmap.InternalNew: TCustomUniversalBitmap;
begin
  Result:= TLinearRGBABitmap.Create;
end;

class procedure TLinearRGBABitmap.SolidBrush(out ABrush: TUniversalBrush;
  const AColor: TLinearRGBA; ADrawMode: TDrawMode);
begin
  ABrush.Colorspace:= TLinearRGBAColorspace;
  PLinearRGBA(@ABrush.FixedData)^ := AColor;
  ABrush.InternalInitContext:= nil;
  case ADrawMode of
    dmSet: ABrush.InternalPutNextPixels:= @LinearRGBASolidBrushSetPixels;

    dmSetExceptTransparent:
      if AColor.alpha < 1 then
        ABrush.InternalPutNextPixels:= @LinearRGBASolidBrushSkipPixels
      else
        ABrush.InternalPutNextPixels:= @LinearRGBASolidBrushSetPixels;

    dmDrawWithTransparency,dmLinearBlend:
      if AColor.alpha<=0 then
        ABrush.InternalPutNextPixels:= @LinearRGBASolidBrushSkipPixels
      else if AColor.alpha>=1 then
        ABrush.InternalPutNextPixels:= @LinearRGBASolidBrushSetPixels
      else
        ABrush.InternalPutNextPixels:= @LinearRGBASolidBrushDrawPixels;

    dmXor: raise exception.Create('Xor mode not available with floating point values');
  end;
end;

class procedure TLinearRGBABitmap.ScannerBrush(out ABrush: TUniversalBrush;
  AScanner: IBGRAScanner; ADrawMode: TDrawMode;
  AOffsetX: integer; AOffsetY: integer);
begin
  ABrush.Colorspace:= TLinearRGBAColorspace;
  with PLinearRGBAScannerBrushFixedData(@ABrush.FixedData)^ do
  begin
    Scanner := Pointer(AScanner);
    OffsetX := AOffsetX;
    OffsetY := AOffsetY;
  end;
  ABrush.InternalInitContext:= @LinearRGBAScannerBrushInitContext;
  case ADrawMode of
    dmSet: ABrush.InternalPutNextPixels:= @LinearRGBAScannerBrushSetPixels;
    dmSetExceptTransparent: ABrush.InternalPutNextPixels:= @LinearRGBAScannerBrushSetPixelsExceptTransparent;
    dmDrawWithTransparency,dmLinearBlend:
      ABrush.InternalPutNextPixels:= @LinearRGBAScannerBrushDrawPixels;
    dmXor: raise exception.Create('Xor mode not available with floating point values');
  end;
end;

class procedure TLinearRGBABitmap.EraseBrush(out ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  if AAlpha = 0 then
  begin
    SolidBrush(ABrush, LinearRGBATransparent, dmDrawWithTransparency);
    exit;
  end;
  ABrush.Colorspace:= TLinearRGBAColorspace;
  PSingle(@ABrush.FixedData)^ := AAlpha/65535;
  ABrush.InternalInitContext:= nil;
  ABrush.InternalPutNextPixels:= @LinearRGBAAlphaBrushDrawPixels;
end;

class procedure TLinearRGBABitmap.AlphaBrush(out ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  if AAlpha = 0 then
  begin
    SolidBrush(ABrush, LinearRGBATransparent, dmDrawWithTransparency);
    exit;
  end;
  ABrush.Colorspace:= TLinearRGBAColorspace;
  PSingle(@ABrush.FixedData)^ := AAlpha/65535;
  ABrush.InternalInitContext:= nil;
  ABrush.InternalPutNextPixels:= @LinearRGBAAlphaBrushSetPixels;
end;

end.

