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
    procedure AssignTransparentPixel(out ADest); override;
  public
    class procedure SolidBrush(out ABrush: TUniversalBrush; const AColor: TLinearRGBA; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    class procedure ScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode = dmDrawWithTransparency;
                                 AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure MaskBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner;
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

procedure LinearRGBAChunkSetPixels(
    ASource: PLinearRGBA; ADest: PLinearRGBA;
    AAlpha: Word; ACount: integer; ASourceStride: integer); inline;
const oneOver65535 = 1/65535;
var
  alphaOver, finalAlpha, finalAlphaInv, residualAlpha: single;
begin
  if AAlpha=0 then exit;
  if AAlpha=65535 then
  begin
    while ACount > 0 do
    begin
      ADest^ := ASource^;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end else
  begin
    alphaOver := AAlpha*single(oneOver65535);
    while ACount > 0 do
    begin
      residualAlpha := ADest^.alpha*(1-alphaOver);
      finalAlpha := residualAlpha + ASource^.alpha*alphaOver;
      if finalAlpha <= 0 then ADest^ := LinearRGBATransparent else
      begin
        ADest^.alpha:= finalAlpha;
        finalAlphaInv := 1/finalAlpha;
        ADest^.red := (ADest^.red*residualAlpha +
                        ASource^.red*(finalAlpha-residualAlpha) ) * finalAlphaInv;
        ADest^.green := (ADest^.green*residualAlpha +
                         ASource^.green*(finalAlpha-residualAlpha) ) * finalAlphaInv;
        ADest^.blue := (ADest^.blue*residualAlpha +
                        ASource^.blue*(finalAlpha-residualAlpha) ) * finalAlphaInv;
      end;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end;
end;

procedure LinearRGBASolidBrushSetPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PLinearRGBA;
begin
  pDest := PLinearRGBA(AContextData^.Dest);
  LinearRGBAChunkSetPixels( PLinearRGBA(AFixedData), pDest, AAlpha, ACount, 0);
  inc(pDest, ACount);
  AContextData^.Dest := pDest;
end;

procedure LinearRGBAChunkDrawPixels(
    ASource: PLinearRGBA; ADest: PLinearRGBA;
    AAlpha: Word; ACount: integer; ASourceStride: integer); inline;
const oneOver65535 = 1/65535;
var
  alphaOver, srcAlphaOver, finalAlpha, finalAlphaInv, residualAlpha: single;
begin
  if AAlpha=0 then exit;
  alphaOver := AAlpha*single(oneOver65535);
  while ACount > 0 do
  begin
    srcAlphaOver := ASource^.alpha*alphaOver;
    if srcAlphaOver >= 1 then
      ADest^ := ASource^
    else
    begin
      residualAlpha := ADest^.alpha*(1-srcAlphaOver);
      finalAlpha := residualAlpha + srcAlphaOver;
      if finalAlpha <= 0 then ADest^ := LinearRGBATransparent else
      begin
        ADest^.alpha:= finalAlpha;
        finalAlphaInv := 1/finalAlpha;
        ADest^.red := (ADest^.red*residualAlpha +
                        ASource^.red*srcAlphaOver ) * finalAlphaInv;
        ADest^.green := (ADest^.green*residualAlpha +
                         ASource^.green*srcAlphaOver ) * finalAlphaInv;
        ADest^.blue := (ADest^.blue*residualAlpha +
                        ASource^.blue*srcAlphaOver ) * finalAlphaInv;
      end;
    end;
    inc(ADest);
    dec(ACount);
    inc(PByte(ASource), ASourceStride);
  end;
end;

procedure LinearRGBASolidBrushDrawPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PLinearRGBA;
begin
  pDest := PLinearRGBA(AContextData^.Dest);
  LinearRGBAChunkDrawPixels( PLinearRGBA(AFixedData), pDest, AAlpha, ACount, 0);
  inc(pDest, ACount);
  AContextData^.Dest := pDest;
end;

type
  PLinearRGBAScannerBrushFixedData = ^TLinearRGBAScannerBrushFixedData;
  TLinearRGBAScannerBrushFixedData = record
    Scanner: Pointer; //avoid ref count by using pointer type
    OffsetX, OffsetY: integer;
    Conversion: TBridgedConversion;
  end;

procedure LinearRGBAScannerBrushInitContext(AFixedData: Pointer;
  AContextData: PUniBrushContext);
begin
  with PLinearRGBAScannerBrushFixedData(AFixedData)^ do
    IBGRAScanner(Scanner).ScanMoveTo(AContextData^.Ofs.X + OffsetX,
                                     AContextData^.Ofs.Y + OffsetY);
end;

procedure LinearRGBAScannerConvertBrushSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PLinearRGBA;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TLinearRGBA;
begin
  with PLinearRGBAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PLinearRGBA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PLinearRGBA(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TLinearRGBA), nil);
      LinearRGBAChunkSetPixels(@buf, pDest, AAlpha, qty, sizeof(TLinearRGBA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure LinearRGBAScannerChunkBrushSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PLinearRGBA;
  qty: Integer;
begin
  with PLinearRGBAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PLinearRGBA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PLinearRGBA(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      LinearRGBAChunkSetPixels(PLinearRGBA(psrc), pDest, AAlpha, qty, sizeof(TLinearRGBA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure LinearRGBAChunkSetPixelsExceptTransparent(
    ASource: PLinearRGBA; ADest: PLinearRGBA;
    AAlpha: Word; ACount: integer; ASourceStride: integer); inline;
const oneOver65535 = 1/65535;
var
  alphaOver, finalAlpha, finalAlphaInv, residualAlpha: single;
begin
  if AAlpha=0 then exit;
  if AAlpha=65535 then
  begin
    while ACount > 0 do
    begin
      if ASource^.alpha >= 1 then
        ADest^ := ASource^;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end else
  begin
    alphaOver := AAlpha*single(oneOver65535);
    while ACount > 0 do
    begin
      if ASource^.alpha >= 1 then
      begin
        residualAlpha := ADest^.alpha*(1-alphaOver);
        finalAlpha := residualAlpha + ASource^.alpha*alphaOver;
        if finalAlpha <= 0 then ADest^ := LinearRGBATransparent else
        begin
          ADest^.alpha:= finalAlpha;
          finalAlphaInv := 1/finalAlpha;
          ADest^.red := (ADest^.red*residualAlpha +
                          ASource^.red*(finalAlpha-residualAlpha) ) * finalAlphaInv;
          ADest^.green := (ADest^.green*residualAlpha +
                           ASource^.green*(finalAlpha-residualAlpha) ) * finalAlphaInv;
          ADest^.blue := (ADest^.blue*residualAlpha +
                          ASource^.blue*(finalAlpha-residualAlpha) ) * finalAlphaInv;
        end;
      end;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end;
end;

procedure LinearRGBAScannerChunkBrushSetPixelsExceptTransparent(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PLinearRGBA;
  qty: Integer;
  psrc: Pointer;
begin
  with PLinearRGBAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PLinearRGBA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PLinearRGBA(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      LinearRGBAChunkSetPixelsExceptTransparent(PLinearRGBA(psrc), pDest, AAlpha, qty, sizeof(TLinearRGBA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure LinearRGBAScannerConvertBrushSetPixelsExceptTransparent(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PLinearRGBA;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TLinearRGBA;
begin
  with PLinearRGBAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PLinearRGBA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PLinearRGBA(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TLinearRGBA), nil);
      LinearRGBAChunkSetPixelsExceptTransparent(@buf, pDest, AAlpha, qty, sizeof(TLinearRGBA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure LinearRGBAScannerChunkBrushDrawPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  qty: Integer;
  pDest: PLinearRGBA;
begin
  with PLinearRGBAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PLinearRGBA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PLinearRGBA(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      LinearRGBAChunkDrawPixels(PLinearRGBA(psrc), pDest, AAlpha, qty, sizeof(TLinearRGBA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure LinearRGBAScannerConvertBrushDrawPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PLinearRGBA;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TLinearRGBA;
begin
  with PLinearRGBAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PLinearRGBA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PLinearRGBA(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TLinearRGBA), nil);
      LinearRGBAChunkDrawPixels(@buf, pDest, AAlpha, qty, sizeof(TLinearRGBA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure LinearRGBAMaskBrushApply(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PLinearRGBA;
  qty, maskStride: Integer;
  pMask: PByteMask;
  factor: single;
begin
  with PLinearRGBAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PLinearRGBA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PLinearRGBA(AContextData^.Dest);
    factor := AAlpha/(65535*255);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextMaskChunk(qty, pMask, maskStride);
      dec(ACount,qty);
      while qty > 0 do
      begin
        pDest^.alpha := pDest^.alpha*pMask^.gray*factor;
        if pDest^.alpha = 0 then pDest^ := LinearRGBATransparent;
        inc(pDest);
        inc(pMask, maskStride);
        dec(qty);
      end;
    end;
    PLinearRGBA(AContextData^.Dest) := pDest;
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

procedure LinearRGBAAlphaBrushErasePixels(AFixedData: Pointer;
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

procedure TLinearRGBABitmap.AssignTransparentPixel(out ADest);
begin
  TLinearRGBA(ADest) := LinearRGBATransparent;
end;

class procedure TLinearRGBABitmap.SolidBrush(out ABrush: TUniversalBrush;
  const AColor: TLinearRGBA; ADrawMode: TDrawMode);
begin
  ABrush.Colorspace:= TLinearRGBAColorspace;
  PLinearRGBA(@ABrush.FixedData)^ := AColor;
  case ADrawMode of
    dmSet: ABrush.InternalPutNextPixels:= @LinearRGBASolidBrushSetPixels;

    dmSetExceptTransparent:
      if AColor.alpha < 1 then
        ABrush.InternalPutNextPixels:= @LinearRGBASolidBrushSkipPixels
      else
      begin
        ABrush.InternalPutNextPixels:= @LinearRGBASolidBrushSetPixels;
        ABrush.DoesNothing := true;
      end;

    dmDrawWithTransparency,dmLinearBlend:
      if AColor.alpha<=0 then
      begin
        ABrush.InternalPutNextPixels:= @LinearRGBASolidBrushSkipPixels;
        ABrush.DoesNothing := true;
      end
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
var
  sourceSpace: TColorspaceAny;
begin
  ABrush.Colorspace:= TLinearRGBAColorspace;
  with PLinearRGBAScannerBrushFixedData(@ABrush.FixedData)^ do
  begin
    Scanner := Pointer(AScanner);
    OffsetX := AOffsetX;
    OffsetY := AOffsetY;
  end;
  ABrush.InternalInitContext:= @LinearRGBAScannerBrushInitContext;
  sourceSpace := AScanner.GetScanCustomColorspace;
  if sourceSpace = TLinearRGBAColorspace then
  begin
    case ADrawMode of
      dmSet: ABrush.InternalPutNextPixels:= @LinearRGBAScannerChunkBrushSetPixels;
      dmSetExceptTransparent: ABrush.InternalPutNextPixels:= @LinearRGBAScannerChunkBrushSetPixelsExceptTransparent;
      dmDrawWithTransparency,dmLinearBlend:
        ABrush.InternalPutNextPixels:= @LinearRGBAScannerChunkBrushDrawPixels;
      dmXor: raise exception.Create('Xor mode not available with floating point values');
    end;
  end else
  begin
    with PLinearRGBAScannerBrushFixedData(@ABrush.FixedData)^ do
      Conversion := sourceSpace.GetBridgedConversion(TLinearRGBAColorspace);
    case ADrawMode of
      dmSet: ABrush.InternalPutNextPixels:= @LinearRGBAScannerConvertBrushSetPixels;
      dmSetExceptTransparent: ABrush.InternalPutNextPixels:= @LinearRGBAScannerConvertBrushSetPixelsExceptTransparent;
      dmDrawWithTransparency,dmLinearBlend:
        ABrush.InternalPutNextPixels:= @LinearRGBAScannerConvertBrushDrawPixels;
      dmXor: raise exception.Create('Xor mode not available with floating point values');
    end;
  end;
end;

class procedure TLinearRGBABitmap.MaskBrush(out ABrush: TUniversalBrush;
  AScanner: IBGRAScanner; AOffsetX: integer; AOffsetY: integer);
begin
  ABrush.Colorspace:= TLinearRGBAColorspace;
  with PLinearRGBAScannerBrushFixedData(@ABrush.FixedData)^ do
  begin
    Scanner := Pointer(AScanner);
    OffsetX := AOffsetX;
    OffsetY := AOffsetY;
  end;
  ABrush.InternalInitContext:= @LinearRGBAScannerBrushInitContext;
  ABrush.InternalPutNextPixels:= @LinearRGBAMaskBrushApply;
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
  ABrush.InternalPutNextPixels:= @LinearRGBAAlphaBrushErasePixels;
end;

class procedure TLinearRGBABitmap.AlphaBrush(out ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  if AAlpha = 0 then
  begin
    SolidBrush(ABrush, LinearRGBATransparent, dmSet);
    exit;
  end;
  ABrush.Colorspace:= TLinearRGBAColorspace;
  PSingle(@ABrush.FixedData)^ := AAlpha/65535;
  ABrush.InternalInitContext:= nil;
  ABrush.InternalPutNextPixels:= @LinearRGBAAlphaBrushSetPixels;
end;

end.

