unit ExpandedBitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, UniversalDrawer;

type

  { TExpandedBitmap }

  TExpandedBitmap = class(specialize TGenericUniversalBitmap<TExpandedPixel,TExpandedPixelColorspace>)
  protected
    function InternalNew: TCustomUniversalBitmap; override;
    procedure AssignTransparentPixel(out ADest); override;
  public
    class procedure SolidBrush(out ABrush: TUniversalBrush; const AColor: TExpandedPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    class procedure ScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode = dmDrawWithTransparency;
                                 AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure MaskBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner;
                              AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure EraseBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
    class procedure AlphaBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
  end;

const
  ExpandedPixelTransparent : TExpandedPixel = (red:0; green:0; blue:0; alpha:0);

operator = (const c1, c2: TExpandedPixel): boolean; inline;

implementation

uses XYZABitmap;

operator = (const c1, c2: TExpandedPixel): boolean;
begin
  if (c1.alpha = 0) and (c2.alpha = 0) then
    Result := True
  else
    Result := (c1.alpha = c2.alpha) and (c1.red = c2.red) and
      (c1.green = c2.green) and (c1.blue = c2.blue);
end;

procedure ExpandedSolidBrushSkipPixels({%H-}AFixedData: Pointer;
    AContextData: PUniBrushContext; {%H-}AAlpha: Word; ACount: integer);
begin
  inc(PExpandedPixel(AContextData^.Dest), ACount);
end;

procedure ExpandedChunkSetPixels(
    ASource: PExpandedPixel; ADest: PExpandedPixel;
    AAlpha: Word; ACount: integer; ASourceStride: integer); inline;
var
  alphaOver: NativeUInt;
  finalAlpha, residualAlpha, finalAlphaDiv2: NativeUInt;
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
    if AAlpha > 32768 then alphaOver := AAlpha+1 else alphaOver := AAlpha;
    while ACount > 0 do
    begin
      residualAlpha := (ADest^.alpha*NativeUInt(65536-alphaOver)+32768) shr 16;
      finalAlpha := residualAlpha + ((ASource^.alpha*alphaOver+32768) shr 16);
      if finalAlpha <= 0 then ADest^ := ExpandedPixelTransparent else
      begin
        if finalAlpha > 65535 then finalAlpha := 65535;
        finalAlphaDiv2 := finalAlpha shr 1;
        ADest^.alpha:= finalAlpha;
        ADest^.red := (ADest^.red*residualAlpha +
                     ASource^.red*NativeUInt(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
        ADest^.green := (ADest^.green*residualAlpha +
                     ASource^.green*NativeUInt(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
        ADest^.blue := (ADest^.blue*residualAlpha +
                     ASource^.blue*NativeUInt(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
      end;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end;
end;

procedure ExpandedSolidBrushSetPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PExpandedPixel;
begin
  pDest := PExpandedPixel(AContextData^.Dest);
  ExpandedChunkSetPixels( PExpandedPixel(AFixedData), pDest, AAlpha, ACount, 0);
  inc(pDest, ACount);
  AContextData^.Dest := pDest;
end;

procedure ExpandedChunkDrawPixels(
    ASource: PExpandedPixel; ADest: PExpandedPixel;
    AAlpha: Word; ACount: integer; ASourceStride: integer); inline;
var
  alphaOver, srcAlphaOver, finalAlpha, finalAlphaDiv2, residualAlpha: NativeUInt;
begin
  if AAlpha=0 then exit;
  if AAlpha >= 32768 then alphaOver := AAlpha+1 else alphaOver := AAlpha;
  while ACount > 0 do
  begin
    srcAlphaOver := (ASource^.alpha*alphaOver+32768) shr 16;
    if srcAlphaOver >= 65535 then
      ADest^ := ASource^
    else
    begin
      if srcAlphaOver >= 32768 then inc(srcAlphaOver);
      residualAlpha := (ADest^.alpha*NativeUInt(65536-srcAlphaOver)+32768) shr 16;
      finalAlpha := residualAlpha + srcAlphaOver;
      if finalAlpha <= 0 then ADest^ := ExpandedPixelTransparent else
      begin
        if finalAlpha > 65535 then finalAlpha := 65535;
        ADest^.alpha:= finalAlpha;
        finalAlphaDiv2 := finalAlpha shr 1;
        ADest^.red := (ADest^.red*residualAlpha +
                     ASource^.red*srcAlphaOver + finalAlphaDiv2) div finalAlpha;
        ADest^.green := (ADest^.green*residualAlpha +
                     ASource^.green*srcAlphaOver + finalAlphaDiv2) div finalAlpha;
        ADest^.blue := (ADest^.blue*residualAlpha +
                     ASource^.blue*srcAlphaOver + finalAlphaDiv2) div finalAlpha;
      end;
    end;
    inc(ADest);
    dec(ACount);
    inc(PByte(ASource), ASourceStride);
  end;
end;

procedure ExpandedSolidBrushDrawPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PExpandedPixel;
begin
  pDest := PExpandedPixel(AContextData^.Dest);
  ExpandedChunkDrawPixels( PExpandedPixel(AFixedData), pDest, AAlpha, ACount, 0);
  inc(pDest, ACount);
  AContextData^.Dest := pDest;
end;

procedure ExpandedChunkXorPixels(
    ASource: PExpandedPixel; ADest: PExpandedPixel;
    AAlpha: Word; ACount: integer; ASourceStride: integer); inline;
var
  alphaOver: NativeUInt;
  finalAlpha, residualAlpha, finalAlphaDiv2: NativeUInt;
  xored: TExpandedPixel;
begin
  if AAlpha=0 then exit;
  if AAlpha=65535 then
  begin
    while ACount > 0 do
    begin
      PQWord(ADest)^ := PQWord(ADest)^ xor PQWord(ASource)^;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end else
  begin
    if AAlpha > 32768 then alphaOver := AAlpha+1 else alphaOver := AAlpha;
    while ACount > 0 do
    begin
      PQWord(@xored)^ := PQWord(ADest)^ xor PQWord(ASource)^;
      residualAlpha := (ADest^.alpha*NativeUInt(65536-alphaOver)+32768) shr 16;
      finalAlpha := residualAlpha + ((xored.alpha*alphaOver+32768) shr 16);
      if finalAlpha <= 0 then ADest^ := ExpandedPixelTransparent else
      begin
        if finalAlpha > 65535 then finalAlpha := 65535;
        finalAlphaDiv2 := finalAlpha shr 1;
        ADest^.alpha:= finalAlpha;
        ADest^.red := (ADest^.red*residualAlpha +
                     xored.red*NativeUInt(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
        ADest^.green := (ADest^.green*residualAlpha +
                     xored.green*NativeUInt(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
        ADest^.blue := (ADest^.blue*residualAlpha +
                     xored.blue*NativeUInt(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
      end;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end;
end;

procedure ExpandedSolidBrushXorPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PExpandedPixel;
begin
  pDest := PExpandedPixel(AContextData^.Dest);
  ExpandedChunkXorPixels( PExpandedPixel(AFixedData), pDest, AAlpha, ACount, 0);
  inc(pDest, ACount);
  AContextData^.Dest := pDest;
end;

type
  PExpandedPixelScannerBrushFixedData = ^TExpandedScannerBrushFixedData;
  TExpandedScannerBrushFixedData = record
    Scanner: Pointer; //avoid ref count by using pointer type
    OffsetX, OffsetY: integer;
    Conversion: TBridgedConversion;
  end;

procedure ExpandedScannerBrushInitContext(AFixedData: Pointer;
  AContextData: PUniBrushContext);
begin
  with PExpandedPixelScannerBrushFixedData(AFixedData)^ do
    IBGRAScanner(Scanner).ScanMoveTo(AContextData^.Ofs.X + OffsetX,
                                     AContextData^.Ofs.Y + OffsetY);
end;

procedure ExpandedScannerConvertBrushSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PExpandedPixel;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TExpandedPixel;
begin
  with PExpandedPixelScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PExpandedPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PExpandedPixel(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TExpandedPixel), nil);
      ExpandedChunkSetPixels(@buf, pDest, AAlpha, qty, sizeof(TExpandedPixel) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure ExpandedScannerChunkBrushSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PExpandedPixel;
  qty: Integer;
begin
  with PExpandedPixelScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PExpandedPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PExpandedPixel(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      ExpandedChunkSetPixels(PExpandedPixel(psrc), pDest, AAlpha, qty, sizeof(TExpandedPixel) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure ExpandedChunkSetPixelsExceptTransparent(
    ASource: PExpandedPixel; ADest: PExpandedPixel;
    AAlpha: Word; ACount: integer; ASourceStride: integer); inline;
var
  alphaOver: NativeUInt;
  finalAlpha, residualAlpha, finalAlphaDiv2: NativeUInt;
begin
  if AAlpha=0 then exit;
  if AAlpha=65535 then
  begin
    while ACount > 0 do
    begin
      if ASource^.alpha = 65535 then ADest^ := ASource^;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end else
  begin
    if AAlpha > 32768 then alphaOver := AAlpha+1 else alphaOver := AAlpha;
    while ACount > 0 do
    begin
      if ASource^.alpha = 65535 then
      begin
        residualAlpha := (ADest^.alpha*NativeUInt(65536-alphaOver)+32768) shr 16;
        finalAlpha := residualAlpha + AAlpha;
        if finalAlpha <= 0 then ADest^ := ExpandedPixelTransparent else
        begin
          if finalAlpha > 65535 then finalAlpha := 65535;
          finalAlphaDiv2 := finalAlpha shr 1;
          ADest^.alpha:= finalAlpha;
          ADest^.red := (ADest^.red*residualAlpha +
                       ASource^.red*NativeUInt(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
          ADest^.green := (ADest^.green*residualAlpha +
                       ASource^.green*NativeUInt(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
          ADest^.blue := (ADest^.blue*residualAlpha +
                       ASource^.blue*NativeUInt(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
        end;
      end;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end;
end;

procedure ExpandedScannerChunkBrushSetPixelsExceptTransparent(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PExpandedPixel;
  qty: Integer;
  psrc: Pointer;
begin
  with PExpandedPixelScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PExpandedPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PExpandedPixel(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      ExpandedChunkSetPixelsExceptTransparent(PExpandedPixel(psrc), pDest, AAlpha, qty, sizeof(TExpandedPixel) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure ExpandedScannerConvertBrushSetPixelsExceptTransparent(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PExpandedPixel;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TExpandedPixel;
begin
  with PExpandedPixelScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PExpandedPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PExpandedPixel(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TExpandedPixel), nil);
      ExpandedChunkSetPixelsExceptTransparent(@buf, pDest, AAlpha, qty, sizeof(TExpandedPixel) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure ExpandedScannerChunkBrushDrawPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  qty: Integer;
  pDest: PExpandedPixel;
begin
  with PExpandedPixelScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PExpandedPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PExpandedPixel(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      ExpandedChunkDrawPixels(PExpandedPixel(psrc), pDest, AAlpha, qty, sizeof(TExpandedPixel) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure ExpandedScannerConvertBrushDrawPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PExpandedPixel;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TExpandedPixel;
begin
  with PExpandedPixelScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PExpandedPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PExpandedPixel(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TExpandedPixel), nil);
      ExpandedChunkDrawPixels(@buf, pDest, AAlpha, qty, sizeof(TExpandedPixel) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure ExpandedScannerChunkBrushXorPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  qty: Integer;
  pDest: PExpandedPixel;
begin
  with PExpandedPixelScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PExpandedPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PExpandedPixel(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      ExpandedChunkXorPixels(PExpandedPixel(psrc), pDest, AAlpha, qty, sizeof(TExpandedPixel) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure ExpandedScannerConvertBrushXorPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PExpandedPixel;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TExpandedPixel;
begin
  with PExpandedPixelScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PExpandedPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PExpandedPixel(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TExpandedPixel), nil);
      ExpandedChunkXorPixels(@buf, pDest, AAlpha, qty, sizeof(TExpandedPixel) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure ExpandedMaskBrushApply(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PExpandedPixel;
  qty, maskStride: Integer;
  pMask: PByteMask;
  factor: NativeUInt;
begin
  with PExpandedPixelScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PExpandedPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PExpandedPixel(AContextData^.Dest);
    if AAlpha = 65535 then
    begin
      while ACount > 0 do
      begin
        qty := ACount;
        IBGRAScanner(Scanner).ScanNextMaskChunk(qty, pMask, maskStride);
        dec(ACount,qty);
        while qty > 0 do
        begin
          if pMask^.gray >= 128 then
            pDest^.alpha := (pDest^.alpha*(pMask^.gray+1)) shr 8
          else pDest^.alpha := pDest^.alpha*pMask^.gray shr 8;
          if pDest^.alpha = 0 then pDest^ := ExpandedPixelTransparent;
          inc(pDest);
          inc(pMask, maskStride);
          dec(qty);
        end;
      end;
    end else
    begin
      factor := AAlpha + (AAlpha shr 8) + (AAlpha shr 14);
      while ACount > 0 do
      begin
        qty := ACount;
        IBGRAScanner(Scanner).ScanNextMaskChunk(qty, pMask, maskStride);
        dec(ACount,qty);
        while qty > 0 do
        begin
          pDest^.alpha := (pDest^.alpha*((factor*pMask^.gray+128) shr 8)) shr 16;
          if pDest^.alpha = 0 then pDest^ := ExpandedPixelTransparent;
          inc(pDest);
          inc(pMask, maskStride);
          dec(qty);
        end;
      end;
    end;
    PExpandedPixel(AContextData^.Dest) := pDest;
  end;
end;

procedure ExpandedAlphaBrushSetPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PExpandedPixel;
  alphaOver, residualAlpha, finalAlpha: NativeUInt;
begin
  if AAlpha=0 then
  begin
    inc(PExpandedPixel(AContextData^.Dest), ACount);
    exit;
  end;
  pDest := PExpandedPixel(AContextData^.Dest);
  if AAlpha=65535 then
  begin
    finalAlpha := PWord(AFixedData)^;
    while ACount > 0 do
    begin
      pDest^.alpha := finalAlpha;
      inc(pDest);
      dec(ACount);
    end;
  end else
  begin
    if AAlpha >= 32768 then alphaOver := AAlpha+1
    else alphaOver := AAlpha;
    while ACount > 0 do
    begin
      residualAlpha := (pDest^.alpha*NativeUInt(65536-alphaOver)+32768) shr 16;
      finalAlpha := residualAlpha + (PWord(AFixedData)^*alphaOver+32768) shr 16;
      if finalAlpha > 65535 then finalAlpha := 65535;
      pDest^.alpha:= finalAlpha;
      inc(pDest);
      dec(ACount);
    end;
  end;
  PExpandedPixel(AContextData^.Dest) := pDest;
end;

procedure ExpandedAlphaBrushErasePixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PExpandedPixel;
  alphaMul, finalAlpha: NativeUInt;
begin
  if AAlpha=0 then
  begin
    inc(PExpandedPixel(AContextData^.Dest), ACount);
    exit;
  end;
  pDest := PExpandedPixel(AContextData^.Dest);
  if AAlpha<>65535 then
    alphaMul := 65535-((PWord(AFixedData)^*AAlpha+32767) div 65535)
  else
    alphaMul := 65535-PWord(AFixedData)^;
  if alphaMul >= 32768 then inc(alphaMul);
  while ACount > 0 do
  begin
    finalAlpha := (pDest^.alpha*alphaMul+32768) shr 16;
    if finalAlpha <= 0 then pDest^ := ExpandedPixelTransparent else
      pDest^.alpha:= finalAlpha;
    inc(pDest);
    dec(ACount);
  end;
  PExpandedPixel(AContextData^.Dest) := pDest;
end;

{ TExpandedBitmap }

function TExpandedBitmap.InternalNew: TCustomUniversalBitmap;
begin
  Result:= TExpandedBitmap.Create;
end;

procedure TExpandedBitmap.AssignTransparentPixel(out ADest);
begin
  TExpandedPixel(ADest) := ExpandedPixelTransparent;
end;

class procedure TExpandedBitmap.SolidBrush(out ABrush: TUniversalBrush;
  const AColor: TExpandedPixel; ADrawMode: TDrawMode);
begin
  ABrush.Colorspace:= TExpandedPixelColorspace;
  PExpandedPixel(@ABrush.FixedData)^ := AColor;
  case ADrawMode of
    dmSet: ABrush.InternalPutNextPixels:= @ExpandedSolidBrushSetPixels;

    dmSetExceptTransparent:
      if AColor.alpha < 65535 then
        ABrush.InternalPutNextPixels:= @ExpandedSolidBrushSkipPixels
      else
      begin
        ABrush.InternalPutNextPixels:= @ExpandedSolidBrushSetPixels;
        ABrush.DoesNothing := true;
      end;

    dmDrawWithTransparency,dmLinearBlend:
      if AColor.alpha<=0 then
      begin
        ABrush.InternalPutNextPixels:= @ExpandedSolidBrushSkipPixels;
        ABrush.DoesNothing := true;
      end
      else if AColor.alpha>=1 then
        ABrush.InternalPutNextPixels:= @ExpandedSolidBrushSetPixels
      else
        ABrush.InternalPutNextPixels:= @ExpandedSolidBrushDrawPixels;

    dmXor: if PQWord(@AColor)^ = 0 then
           begin
             ABrush.InternalPutNextPixels:= @ExpandedSolidBrushSkipPixels;
             ABrush.DoesNothing := true;
           end else
             ABrush.InternalPutNextPixels:= @ExpandedSolidBrushXorPixels;
  end;
end;

class procedure TExpandedBitmap.ScannerBrush(out ABrush: TUniversalBrush;
  AScanner: IBGRAScanner; ADrawMode: TDrawMode;
  AOffsetX: integer; AOffsetY: integer);
var
  sourceSpace: TColorspaceAny;
begin
  ABrush.Colorspace:= TExpandedPixelColorspace;
  with PExpandedPixelScannerBrushFixedData(@ABrush.FixedData)^ do
  begin
    Scanner := Pointer(AScanner);
    OffsetX := AOffsetX;
    OffsetY := AOffsetY;
  end;
  ABrush.InternalInitContext:= @ExpandedScannerBrushInitContext;
  sourceSpace := AScanner.GetScanCustomColorspace;
  if sourceSpace = TExpandedPixelColorspace then
  begin
    case ADrawMode of
      dmSet: ABrush.InternalPutNextPixels:= @ExpandedScannerChunkBrushSetPixels;
      dmSetExceptTransparent: ABrush.InternalPutNextPixels:= @ExpandedScannerChunkBrushSetPixelsExceptTransparent;
      dmDrawWithTransparency,dmLinearBlend:
        ABrush.InternalPutNextPixels:= @ExpandedScannerChunkBrushDrawPixels;
      dmXor: ABrush.InternalPutNextPixels:= @ExpandedScannerChunkBrushXorPixels;
    end;
  end else
  begin
    with PExpandedPixelScannerBrushFixedData(@ABrush.FixedData)^ do
      Conversion := sourceSpace.GetBridgedConversion(TExpandedPixelColorspace);
    case ADrawMode of
      dmSet: ABrush.InternalPutNextPixels:= @ExpandedScannerConvertBrushSetPixels;
      dmSetExceptTransparent: ABrush.InternalPutNextPixels:= @ExpandedScannerConvertBrushSetPixelsExceptTransparent;
      dmDrawWithTransparency,dmLinearBlend:
        ABrush.InternalPutNextPixels:= @ExpandedScannerConvertBrushDrawPixels;
      dmXor: ABrush.InternalPutNextPixels:= @ExpandedScannerConvertBrushXorPixels;
    end;
  end;
end;

class procedure TExpandedBitmap.MaskBrush(out ABrush: TUniversalBrush;
  AScanner: IBGRAScanner; AOffsetX: integer; AOffsetY: integer);
begin
  ABrush.Colorspace:= TExpandedPixelColorspace;
  with PExpandedPixelScannerBrushFixedData(@ABrush.FixedData)^ do
  begin
    Scanner := Pointer(AScanner);
    OffsetX := AOffsetX;
    OffsetY := AOffsetY;
  end;
  ABrush.InternalInitContext:= @ExpandedScannerBrushInitContext;
  ABrush.InternalPutNextPixels:= @ExpandedMaskBrushApply;
end;

class procedure TExpandedBitmap.EraseBrush(out ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  if AAlpha = 0 then
  begin
    SolidBrush(ABrush, ExpandedPixelTransparent, dmDrawWithTransparency);
    exit;
  end;
  ABrush.Colorspace:= TExpandedPixelColorspace;
  PWord(@ABrush.FixedData)^ := AAlpha;
  ABrush.InternalInitContext:= nil;
  ABrush.InternalPutNextPixels:= @ExpandedAlphaBrushErasePixels;
end;

class procedure TExpandedBitmap.AlphaBrush(out ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  if AAlpha = 0 then
  begin
    SolidBrush(ABrush, ExpandedPixelTransparent, dmSet);
    exit;
  end;
  ABrush.Colorspace:= TExpandedPixelColorspace;
  PWord(@ABrush.FixedData)^ := AAlpha;
  ABrush.InternalInitContext:= nil;
  ABrush.InternalPutNextPixels:= @ExpandedAlphaBrushSetPixels;
end;

end.

