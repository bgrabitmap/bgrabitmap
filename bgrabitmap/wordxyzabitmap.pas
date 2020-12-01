// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit WordXYZABitmap;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, UniversalDrawer;

type

  { TWordXYZABitmap }

  TWordXYZABitmap = class(specialize TGenericUniversalBitmap<TWordXYZA,TWordXYZAColorspace>)
  protected
    function InternalNew: TCustomUniversalBitmap; override;
    procedure AssignTransparentPixel(out ADest); override;
  public
    class procedure SolidBrush(out ABrush: TUniversalBrush; const AColor: TWordXYZA; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    class procedure ScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode = dmDrawWithTransparency;
                                 AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure MaskBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner;
                              AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure EraseBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
    class procedure AlphaBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
    procedure ReplaceImaginary(const AAfter: TWordXYZA);
  end;

const
  WordXYZATransparent : TWordXYZA = (X:0; Y:0; Z:0; alpha:0);

operator = (const c1, c2: TWordXYZA): boolean; inline;

implementation

uses XYZABitmap;

operator = (const c1, c2: TWordXYZA): boolean;
begin
  if (c1.alpha = 0) and (c2.alpha = 0) then
    Result := True
  else
    Result := (c1.alpha = c2.alpha) and (c1.X = c2.X) and
      (c1.Y = c2.Y) and (c1.Z = c2.Z);
end;

procedure WordXYZASolidBrushSkipPixels({%H-}AFixedData: Pointer;
    AContextData: PUniBrushContext; {%H-}AAlpha: Word; ACount: integer);
begin
  inc(PWordXYZA(AContextData^.Dest), ACount);
end;

procedure WordXYZAChunkSetPixels(
    ASource: PWordXYZA; ADest: PWordXYZA;
    AAlpha: Word; ACount: integer; ASourceStride: integer); inline;
var
  alphaOver: UInt32or64;
  finalAlpha, residualAlpha, finalAlphaDiv2: UInt32or64;
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
      residualAlpha := (ADest^.alpha*UInt32or64(65536-alphaOver)+32768) shr 16;
      finalAlpha := residualAlpha + ((ASource^.alpha*alphaOver+32768) shr 16);
      if finalAlpha <= 0 then ADest^ := WordXYZATransparent else
      begin
        if finalAlpha > 65535 then finalAlpha := 65535;
        finalAlphaDiv2 := finalAlpha shr 1;
        ADest^.alpha:= finalAlpha;
        ADest^.X := (ADest^.X*residualAlpha +
                     ASource^.X*UInt32or64(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
        ADest^.Y := (ADest^.Y*residualAlpha +
                     ASource^.Y*UInt32or64(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
        ADest^.Z := (ADest^.Z*residualAlpha +
                     ASource^.Z*UInt32or64(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
      end;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end;
end;

procedure WordXYZASolidBrushSetPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PWordXYZA;
begin
  pDest := PWordXYZA(AContextData^.Dest);
  WordXYZAChunkSetPixels( PWordXYZA(AFixedData), pDest, AAlpha, ACount, 0);
  inc(pDest, ACount);
  AContextData^.Dest := pDest;
end;

procedure WordXYZAChunkDrawPixels(
    ASource: PWordXYZA; ADest: PWordXYZA;
    AAlpha: Word; ACount: integer; ASourceStride: integer); inline;
var
  alphaOver, srcAlphaOver, finalAlpha, finalAlphaDiv2, residualAlpha: UInt32or64;
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
      residualAlpha := (ADest^.alpha*UInt32or64(65536-srcAlphaOver)+32768) shr 16;
      finalAlpha := residualAlpha + srcAlphaOver;
      if finalAlpha <= 0 then ADest^ := WordXYZATransparent else
      begin
        if finalAlpha > 65535 then finalAlpha := 65535;
        ADest^.alpha:= finalAlpha;
        finalAlphaDiv2 := finalAlpha shr 1;
        ADest^.X := (ADest^.X*residualAlpha +
                     ASource^.X*srcAlphaOver + finalAlphaDiv2) div finalAlpha;
        ADest^.Y := (ADest^.Y*residualAlpha +
                     ASource^.Y*srcAlphaOver + finalAlphaDiv2) div finalAlpha;
        ADest^.Z := (ADest^.Z*residualAlpha +
                     ASource^.Z*srcAlphaOver + finalAlphaDiv2) div finalAlpha;
      end;
    end;
    inc(ADest);
    dec(ACount);
    inc(PByte(ASource), ASourceStride);
  end;
end;

procedure WordXYZASolidBrushDrawPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PWordXYZA;
begin
  pDest := PWordXYZA(AContextData^.Dest);
  WordXYZAChunkDrawPixels( PWordXYZA(AFixedData), pDest, AAlpha, ACount, 0);
  inc(pDest, ACount);
  AContextData^.Dest := pDest;
end;

procedure WordXYZAChunkXorPixels(
    ASource: PWordXYZA; ADest: PWordXYZA;
    AAlpha: Word; ACount: integer; ASourceStride: integer); inline;
var
  alphaOver: UInt32or64;
  finalAlpha, residualAlpha, finalAlphaDiv2: UInt32or64;
  xored: TWordXYZA;
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
      residualAlpha := (ADest^.alpha*UInt32or64(65536-alphaOver)+32768) shr 16;
      finalAlpha := residualAlpha + ((xored.alpha*alphaOver+32768) shr 16);
      if finalAlpha <= 0 then ADest^ := WordXYZATransparent else
      begin
        if finalAlpha > 65535 then finalAlpha := 65535;
        finalAlphaDiv2 := finalAlpha shr 1;
        ADest^.alpha:= finalAlpha;
        ADest^.X := (ADest^.X*residualAlpha +
                     xored.X*UInt32or64(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
        ADest^.Y := (ADest^.Y*residualAlpha +
                     xored.Y*UInt32or64(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
        ADest^.Z := (ADest^.Z*residualAlpha +
                     xored.Z*UInt32or64(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
      end;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end;
end;

procedure WordXYZASolidBrushXorPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PWordXYZA;
begin
  pDest := PWordXYZA(AContextData^.Dest);
  WordXYZAChunkXorPixels( PWordXYZA(AFixedData), pDest, AAlpha, ACount, 0);
  inc(pDest, ACount);
  AContextData^.Dest := pDest;
end;

type
  PWordXYZAScannerBrushFixedData = ^TWordXYZAScannerBrushFixedData;
  TWordXYZAScannerBrushFixedData = record
    Scanner: Pointer; //avoid ref count by using pointer type
    OffsetX, OffsetY: integer;
    Conversion: TBridgedConversion;
  end;

procedure WordXYZAScannerBrushInitContext(AFixedData: Pointer;
  AContextData: PUniBrushContext);
begin
  with PWordXYZAScannerBrushFixedData(AFixedData)^ do
    IBGRAScanner(Scanner).ScanMoveTo(AContextData^.Ofs.X + OffsetX,
                                     AContextData^.Ofs.Y + OffsetY);
end;

procedure WordXYZAScannerConvertBrushSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PWordXYZA;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TWordXYZA;
begin
  with PWordXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PWordXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PWordXYZA(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TWordXYZA), nil);
      WordXYZAChunkSetPixels(@buf, pDest, AAlpha, qty, sizeof(TWordXYZA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure WordXYZAScannerChunkBrushSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PWordXYZA;
  qty: Integer;
begin
  with PWordXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PWordXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PWordXYZA(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      WordXYZAChunkSetPixels(PWordXYZA(psrc), pDest, AAlpha, qty, sizeof(TWordXYZA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure WordXYZAChunkSetPixelsExceptTransparent(
    ASource: PWordXYZA; ADest: PWordXYZA;
    AAlpha: Word; ACount: integer; ASourceStride: integer); inline;
var
  alphaOver: UInt32or64;
  finalAlpha, residualAlpha, finalAlphaDiv2: UInt32or64;
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
        residualAlpha := (ADest^.alpha*UInt32or64(65536-alphaOver)+32768) shr 16;
        finalAlpha := residualAlpha + AAlpha;
        if finalAlpha <= 0 then ADest^ := WordXYZATransparent else
        begin
          if finalAlpha > 65535 then finalAlpha := 65535;
          finalAlphaDiv2 := finalAlpha shr 1;
          ADest^.alpha:= finalAlpha;
          ADest^.X := (ADest^.X*residualAlpha +
                       ASource^.X*UInt32or64(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
          ADest^.Y := (ADest^.Y*residualAlpha +
                       ASource^.Y*UInt32or64(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
          ADest^.Z := (ADest^.Z*residualAlpha +
                       ASource^.Z*UInt32or64(finalAlpha-residualAlpha) + finalAlphaDiv2) div finalAlpha;
        end;
      end;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end;
end;

procedure WordXYZAScannerChunkBrushSetPixelsExceptTransparent(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PWordXYZA;
  qty: Integer;
  psrc: Pointer;
begin
  with PWordXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PWordXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PWordXYZA(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      WordXYZAChunkSetPixelsExceptTransparent(PWordXYZA(psrc), pDest, AAlpha, qty, sizeof(TWordXYZA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure WordXYZAScannerConvertBrushSetPixelsExceptTransparent(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PWordXYZA;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TWordXYZA;
begin
  with PWordXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PWordXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PWordXYZA(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TWordXYZA), nil);
      WordXYZAChunkSetPixelsExceptTransparent(@buf, pDest, AAlpha, qty, sizeof(TWordXYZA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure WordXYZAScannerChunkBrushDrawPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  qty: Integer;
  pDest: PWordXYZA;
begin
  with PWordXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PWordXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PWordXYZA(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      WordXYZAChunkDrawPixels(PWordXYZA(psrc), pDest, AAlpha, qty, sizeof(TWordXYZA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure WordXYZAScannerConvertBrushDrawPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PWordXYZA;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TWordXYZA;
begin
  with PWordXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PWordXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PWordXYZA(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TWordXYZA), nil);
      WordXYZAChunkDrawPixels(@buf, pDest, AAlpha, qty, sizeof(TWordXYZA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure WordXYZAScannerChunkBrushXorPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  qty: Integer;
  pDest: PWordXYZA;
begin
  with PWordXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PWordXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PWordXYZA(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      WordXYZAChunkXorPixels(PWordXYZA(psrc), pDest, AAlpha, qty, sizeof(TWordXYZA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure WordXYZAScannerConvertBrushXorPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PWordXYZA;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TWordXYZA;
begin
  with PWordXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PWordXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PWordXYZA(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TWordXYZA), nil);
      WordXYZAChunkXorPixels(@buf, pDest, AAlpha, qty, sizeof(TWordXYZA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure WordXYZAMaskBrushApply(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PWordXYZA;
  qty, maskStride: Integer;
  pMask: PByteMask;
  factor: UInt32or64;
begin
  with PWordXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PWordXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PWordXYZA(AContextData^.Dest);
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
          if pDest^.alpha = 0 then pDest^ := WordXYZATransparent;
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
          if pDest^.alpha = 0 then pDest^ := WordXYZATransparent;
          inc(pDest);
          inc(pMask, maskStride);
          dec(qty);
        end;
      end;
    end;
    PWordXYZA(AContextData^.Dest) := pDest;
  end;
end;

procedure WordXYZAAlphaBrushSetPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PWordXYZA;
  alphaOver, residualAlpha, finalAlpha: UInt32or64;
begin
  if AAlpha=0 then
  begin
    inc(PWordXYZA(AContextData^.Dest), ACount);
    exit;
  end;
  pDest := PWordXYZA(AContextData^.Dest);
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
      residualAlpha := (pDest^.alpha*UInt32or64(65536-alphaOver)+32768) shr 16;
      finalAlpha := residualAlpha + (PWord(AFixedData)^*alphaOver+32768) shr 16;
      if finalAlpha > 65535 then finalAlpha := 65535;
      pDest^.alpha:= finalAlpha;
      inc(pDest);
      dec(ACount);
    end;
  end;
  PWordXYZA(AContextData^.Dest) := pDest;
end;

procedure WordXYZAAlphaBrushErasePixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PWordXYZA;
  alphaMul, finalAlpha: UInt32or64;
begin
  if AAlpha=0 then
  begin
    inc(PWordXYZA(AContextData^.Dest), ACount);
    exit;
  end;
  pDest := PWordXYZA(AContextData^.Dest);
  if AAlpha<>65535 then
    alphaMul := 65535-((PWord(AFixedData)^*AAlpha+32767) div 65535)
  else
    alphaMul := 65535-PWord(AFixedData)^;
  if alphaMul >= 32768 then inc(alphaMul);
  while ACount > 0 do
  begin
    finalAlpha := (pDest^.alpha*alphaMul+32768) shr 16;
    if finalAlpha <= 0 then pDest^ := WordXYZATransparent else
      pDest^.alpha:= finalAlpha;
    inc(pDest);
    dec(ACount);
  end;
  PWordXYZA(AContextData^.Dest) := pDest;
end;

{ TWordXYZABitmap }

function TWordXYZABitmap.InternalNew: TCustomUniversalBitmap;
begin
  Result:= TWordXYZABitmap.Create;
end;

procedure TWordXYZABitmap.AssignTransparentPixel(out ADest);
begin
  TWordXYZA(ADest) := WordXYZATransparent;
end;

class procedure TWordXYZABitmap.SolidBrush(out ABrush: TUniversalBrush;
  const AColor: TWordXYZA; ADrawMode: TDrawMode);
begin
  ABrush.Colorspace:= TWordXYZAColorspace;
  PWordXYZA(@ABrush.FixedData)^ := AColor;
  case ADrawMode of
    dmSet: ABrush.InternalPutNextPixels:= @WordXYZASolidBrushSetPixels;

    dmSetExceptTransparent:
      if AColor.alpha < 65535 then
        ABrush.InternalPutNextPixels:= @WordXYZASolidBrushSkipPixels
      else
      begin
        ABrush.InternalPutNextPixels:= @WordXYZASolidBrushSetPixels;
        ABrush.DoesNothing := true;
      end;

    dmDrawWithTransparency,dmLinearBlend:
      if AColor.alpha<=0 then
      begin
        ABrush.InternalPutNextPixels:= @WordXYZASolidBrushSkipPixels;
        ABrush.DoesNothing := true;
      end
      else if AColor.alpha>=1 then
        ABrush.InternalPutNextPixels:= @WordXYZASolidBrushSetPixels
      else
        ABrush.InternalPutNextPixels:= @WordXYZASolidBrushDrawPixels;

    dmXor: if PQWord(@AColor)^ = 0 then
           begin
             ABrush.InternalPutNextPixels:= @WordXYZASolidBrushSkipPixels;
             ABrush.DoesNothing := true;
           end else
             ABrush.InternalPutNextPixels:= @WordXYZASolidBrushXorPixels;
  end;
end;

class procedure TWordXYZABitmap.ScannerBrush(out ABrush: TUniversalBrush;
  AScanner: IBGRAScanner; ADrawMode: TDrawMode;
  AOffsetX: integer; AOffsetY: integer);
var
  sourceSpace: TColorspaceAny;
begin
  ABrush.Colorspace:= TWordXYZAColorspace;
  with PWordXYZAScannerBrushFixedData(@ABrush.FixedData)^ do
  begin
    Scanner := Pointer(AScanner);
    OffsetX := AOffsetX;
    OffsetY := AOffsetY;
  end;
  ABrush.InternalInitContext:= @WordXYZAScannerBrushInitContext;
  sourceSpace := AScanner.GetScanCustomColorspace;
  if sourceSpace = TWordXYZAColorspace then
  begin
    case ADrawMode of
      dmSet: ABrush.InternalPutNextPixels:= @WordXYZAScannerChunkBrushSetPixels;
      dmSetExceptTransparent: ABrush.InternalPutNextPixels:= @WordXYZAScannerChunkBrushSetPixelsExceptTransparent;
      dmDrawWithTransparency,dmLinearBlend:
        ABrush.InternalPutNextPixels:= @WordXYZAScannerChunkBrushDrawPixels;
      dmXor: ABrush.InternalPutNextPixels:= @WordXYZAScannerChunkBrushXorPixels;
    end;
  end else
  begin
    with PWordXYZAScannerBrushFixedData(@ABrush.FixedData)^ do
      Conversion := sourceSpace.GetBridgedConversion(TWordXYZAColorspace);
    case ADrawMode of
      dmSet: ABrush.InternalPutNextPixels:= @WordXYZAScannerConvertBrushSetPixels;
      dmSetExceptTransparent: ABrush.InternalPutNextPixels:= @WordXYZAScannerConvertBrushSetPixelsExceptTransparent;
      dmDrawWithTransparency,dmLinearBlend:
        ABrush.InternalPutNextPixels:= @WordXYZAScannerConvertBrushDrawPixels;
      dmXor: ABrush.InternalPutNextPixels:= @WordXYZAScannerConvertBrushXorPixels;
    end;
  end;
end;

class procedure TWordXYZABitmap.MaskBrush(out ABrush: TUniversalBrush;
  AScanner: IBGRAScanner; AOffsetX: integer; AOffsetY: integer);
begin
  ABrush.Colorspace:= TWordXYZAColorspace;
  with PWordXYZAScannerBrushFixedData(@ABrush.FixedData)^ do
  begin
    Scanner := Pointer(AScanner);
    OffsetX := AOffsetX;
    OffsetY := AOffsetY;
  end;
  ABrush.InternalInitContext:= @WordXYZAScannerBrushInitContext;
  ABrush.InternalPutNextPixels:= @WordXYZAMaskBrushApply;
end;

class procedure TWordXYZABitmap.EraseBrush(out ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  if AAlpha = 0 then
  begin
    SolidBrush(ABrush, WordXYZATransparent, dmDrawWithTransparency);
    exit;
  end;
  ABrush.Colorspace:= TWordXYZAColorspace;
  PWord(@ABrush.FixedData)^ := AAlpha;
  ABrush.InternalInitContext:= nil;
  ABrush.InternalPutNextPixels:= @WordXYZAAlphaBrushErasePixels;
end;

class procedure TWordXYZABitmap.AlphaBrush(out ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  if AAlpha = 0 then
  begin
    SolidBrush(ABrush, WordXYZATransparent, dmSet);
    exit;
  end;
  ABrush.Colorspace:= TWordXYZAColorspace;
  PWord(@ABrush.FixedData)^ := AAlpha;
  ABrush.InternalInitContext:= nil;
  ABrush.InternalPutNextPixels:= @WordXYZAAlphaBrushSetPixels;
end;

procedure TWordXYZABitmap.ReplaceImaginary(const AAfter: TWordXYZA);
var
  p: PWordXYZA;
  n: integer;
begin
  p := Data;
  for n := NbPixels - 1 downto 0 do
  begin
    if (p^.alpha>0) and not IsRealColor(p^) then p^ := AAfter;
    Inc(p);
  end;
  InvalidateBitmap;
end;

end.

