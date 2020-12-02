// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit XYZABitmap;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, UniversalDrawer;

type

  { TXYZABitmap }

  TXYZABitmap = class(specialize TGenericUniversalBitmap<TXYZA,TXYZAColorspace>)
  protected
    function InternalNew: TCustomUniversalBitmap; override;
    procedure AssignTransparentPixel(out ADest); override;
  public
    class procedure SolidBrush(out ABrush: TUniversalBrush; const AColor: TXYZA; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    class procedure ScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode = dmDrawWithTransparency;
                                 AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure MaskBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner;
                              AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure EraseBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
    class procedure AlphaBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
    procedure ReplaceImaginary(const AAfter: TXYZA);
  end;

const
  XYZATransparent : TXYZA = (X:0; Y:0; Z:0; alpha:0);

operator = (const c1, c2: TXYZA): boolean; inline;
function IsRealColor(xyza: TXYZA): boolean;

implementation

uses BGRAFillInfo;

operator = (const c1, c2: TXYZA): boolean;
begin
  if (c1.alpha = 0) and (c2.alpha = 0) then
    Result := True
  else
    Result := (c1.alpha = c2.alpha) and (c1.X = c2.X) and
      (c1.Y = c2.Y) and (c1.Z = c2.Z);
end;

var
  xyHorseshoePolygon: TFillShapeInfo;

procedure MakeXYHorseshoePolygon;
var
  pts: array of TPointF;
  i: Integer;
  n: Single;
begin
  setlength(pts, length(SpectralLocus));
  for i := 0 to high(pts) do
  begin
    n := SpectralLocus[i].X+SpectralLocus[i].Y+SpectralLocus[i].Z;
    pts[i].x := SpectralLocus[i].X/n;
    pts[i].y := SpectralLocus[i].Y/n;
  end;
  xyHorseshoePolygon := TFillPolyInfo.Create(pts, false);
  pts := nil;
end;

function IsRealColor(xyza: TXYZA): boolean;
const dim = 0.015;
var
  n: Single;
begin
  xyza.ChromaticAdapt(GetReferenceWhiteIndirect^, ReferenceWhite2E);
  if (xyza.Y < 0) or (xyza.Y > 1) or (xyza.X < 0) or (xyza.Z < 0) then exit(false);
  if (xyza.Y = 0) then exit((xyza.X=0) and (xyza.Z=0));
  if xyHorseshoePolygon = nil then MakeXYHorseshoePolygon;
  n := xyza.X + xyza.Y + xyza.Z;
  result := xyHorseshoePolygon.IsPointInside(xyza.X/n*(1-dim)+1/3*dim, xyza.Y/n*(1-dim)+1/3*dim, false);
end;

procedure XYZASolidBrushSkipPixels({%H-}AFixedData: Pointer;
    AContextData: PUniBrushContext; {%H-}AAlpha: Word; ACount: integer);
begin
  inc(PXYZA(AContextData^.Dest), ACount);
end;

procedure XYZAChunkSetPixels(
    ASource: PXYZA; ADest: PXYZA;
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
      if finalAlpha <= 0 then ADest^ := XYZATransparent else
      begin
        if finalAlpha > 1 then finalAlpha := 1;
        ADest^.alpha:= finalAlpha;
        finalAlphaInv := 1/finalAlpha;
        ADest^.X := (ADest^.X*residualAlpha +
                     ASource^.X*(finalAlpha-residualAlpha) ) * finalAlphaInv;
        ADest^.Y := (ADest^.Y*residualAlpha +
                     ASource^.Y*(finalAlpha-residualAlpha) ) * finalAlphaInv;
        ADest^.Z := (ADest^.Z*residualAlpha +
                     ASource^.Z*(finalAlpha-residualAlpha) ) * finalAlphaInv;
      end;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end;
end;

procedure XYZASolidBrushSetPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PXYZA;
begin
  pDest := PXYZA(AContextData^.Dest);
  XYZAChunkSetPixels( PXYZA(AFixedData), pDest, AAlpha, ACount, 0);
  inc(pDest, ACount);
  AContextData^.Dest := pDest;
end;

procedure XYZAChunkDrawPixels(
    ASource: PXYZA; ADest: PXYZA;
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
      if finalAlpha <= 0 then ADest^ := XYZATransparent else
      begin
        if finalAlpha > 1 then finalAlpha := 1;
        ADest^.alpha:= finalAlpha;
        finalAlphaInv := 1/finalAlpha;
        ADest^.X := (ADest^.X*residualAlpha +
                     ASource^.X*srcAlphaOver ) * finalAlphaInv;
        ADest^.Y := (ADest^.Y*residualAlpha +
                     ASource^.Y*srcAlphaOver ) * finalAlphaInv;
        ADest^.Z := (ADest^.Z*residualAlpha +
                     ASource^.Z*srcAlphaOver ) * finalAlphaInv;
      end;
    end;
    inc(ADest);
    dec(ACount);
    inc(PByte(ASource), ASourceStride);
  end;
end;

procedure XYZASolidBrushDrawPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PXYZA;
begin
  pDest := PXYZA(AContextData^.Dest);
  XYZAChunkDrawPixels( PXYZA(AFixedData), pDest, AAlpha, ACount, 0);
  inc(pDest, ACount);
  AContextData^.Dest := pDest;
end;

type
  PXYZAScannerBrushFixedData = ^TXYZAScannerBrushFixedData;
  TXYZAScannerBrushFixedData = record
    Scanner: Pointer; //avoid ref count by using pointer type
    OffsetX, OffsetY: integer;
    Conversion: TBridgedConversion;
  end;

procedure XYZAScannerBrushInitContext(AFixedData: Pointer;
  AContextData: PUniBrushContext);
begin
  with PXYZAScannerBrushFixedData(AFixedData)^ do
    IBGRAScanner(Scanner).ScanMoveTo(AContextData^.Ofs.X + OffsetX,
                                     AContextData^.Ofs.Y + OffsetY);
end;

procedure XYZAScannerConvertBrushSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PXYZA;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TXYZA;
begin
  with PXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PXYZA(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TXYZA), nil);
      XYZAChunkSetPixels(@buf, pDest, AAlpha, qty, sizeof(TXYZA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure XYZAScannerChunkBrushSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PXYZA;
  qty: Integer;
begin
  with PXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PXYZA(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      XYZAChunkSetPixels(PXYZA(psrc), pDest, AAlpha, qty, sizeof(TXYZA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure XYZAChunkSetPixelsExceptTransparent(
    ASource: PXYZA; ADest: PXYZA;
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
        finalAlpha := residualAlpha + alphaOver;
        if finalAlpha <= 0 then ADest^ := XYZATransparent else
        begin
          ADest^.alpha:= finalAlpha;
          finalAlphaInv := 1/finalAlpha;
          ADest^.X := (ADest^.X*residualAlpha +
                       ASource^.X*(finalAlpha-residualAlpha) ) * finalAlphaInv;
          ADest^.Y := (ADest^.Y*residualAlpha +
                       ASource^.Y*(finalAlpha-residualAlpha) ) * finalAlphaInv;
          ADest^.Z := (ADest^.Z*residualAlpha +
                       ASource^.Z*(finalAlpha-residualAlpha) ) * finalAlphaInv;
        end;
      end;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end;
end;

procedure XYZAScannerChunkBrushSetPixelsExceptTransparent(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PXYZA;
  qty: Integer;
  psrc: Pointer;
begin
  with PXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PXYZA(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      XYZAChunkSetPixelsExceptTransparent(PXYZA(psrc), pDest, AAlpha, qty, sizeof(TXYZA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure XYZAScannerConvertBrushSetPixelsExceptTransparent(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PXYZA;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TXYZA;
begin
  with PXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PXYZA(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TXYZA), nil);
      XYZAChunkSetPixelsExceptTransparent(@buf, pDest, AAlpha, qty, sizeof(TXYZA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure XYZAScannerChunkBrushDrawPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  qty: Integer;
  pDest: PXYZA;
begin
  with PXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PXYZA(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      XYZAChunkDrawPixels(PXYZA(psrc), pDest, AAlpha, qty, sizeof(TXYZA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure XYZAScannerConvertBrushDrawPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PXYZA;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TXYZA;
begin
  with PXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PXYZA(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TXYZA), nil);
      XYZAChunkDrawPixels(@buf, pDest, AAlpha, qty, sizeof(TXYZA) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure XYZAMaskBrushApply(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PXYZA;
  qty, maskStride: Integer;
  pMask: PByteMask;
  factor: single;
begin
  with PXYZAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PXYZA(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PXYZA(AContextData^.Dest);
    factor := AAlpha/(65535*255);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextMaskChunk(qty, pMask, maskStride);
      dec(ACount,qty);
      while qty > 0 do
      begin
        pDest^.alpha := pDest^.alpha*pMask^.gray*factor;
        if pDest^.alpha = 0 then pDest^ := XYZATransparent;
        inc(pDest);
        inc(pMask, maskStride);
        dec(qty);
      end;
    end;
    PXYZA(AContextData^.Dest) := pDest;
  end;
end;

procedure XYZAAlphaBrushSetPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
const oneOver65535 = 1/65535;
var
  pDest: PXYZA;
  alphaOver, residualAlpha, finalAlpha: single;
begin
  if AAlpha=0 then
  begin
    inc(PXYZA(AContextData^.Dest), ACount);
    exit;
  end;
  pDest := PXYZA(AContextData^.Dest);
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
  PXYZA(AContextData^.Dest) := pDest;
end;

procedure XYZAAlphaBrushErasePixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
const oneOver65535 = 1/65535;
var
  pDest: PXYZA;
  alphaMul, finalAlpha: single;
begin
  if AAlpha=0 then
  begin
    inc(PXYZA(AContextData^.Dest), ACount);
    exit;
  end;
  pDest := PXYZA(AContextData^.Dest);
  if AAlpha<>65535 then
    alphaMul := 1-PSingle(AFixedData)^*AAlpha*single(oneOver65535)
  else
    alphaMul := 1-PSingle(AFixedData)^;
  while ACount > 0 do
  begin
    finalAlpha := pDest^.alpha*alphaMul;
    if finalAlpha <= 0 then pDest^ := XYZATransparent else
      pDest^.alpha:= finalAlpha;
    inc(pDest);
    dec(ACount);
  end;
  PXYZA(AContextData^.Dest) := pDest;
end;

{ TXYZABitmap }

function TXYZABitmap.InternalNew: TCustomUniversalBitmap;
begin
  Result:= TXYZABitmap.Create;
end;

procedure TXYZABitmap.AssignTransparentPixel(out ADest);
begin
  TXYZA(ADest) := XYZATransparent;
end;

class procedure TXYZABitmap.SolidBrush(out ABrush: TUniversalBrush;
  const AColor: TXYZA; ADrawMode: TDrawMode);
begin
  ABrush.Colorspace:= TXYZAColorspace;
  PXYZA(@ABrush.FixedData)^ := AColor;
  case ADrawMode of
    dmSet: ABrush.InternalPutNextPixels:= @XYZASolidBrushSetPixels;

    dmSetExceptTransparent:
      if AColor.alpha < 1 then
        ABrush.InternalPutNextPixels:= @XYZASolidBrushSkipPixels
      else
      begin
        ABrush.InternalPutNextPixels:= @XYZASolidBrushSetPixels;
        ABrush.DoesNothing := true;
      end;

    dmDrawWithTransparency,dmLinearBlend:
      if AColor.alpha<=0 then
      begin
        ABrush.InternalPutNextPixels:= @XYZASolidBrushSkipPixels;
        ABrush.DoesNothing := true;
      end
      else if AColor.alpha>=1 then
        ABrush.InternalPutNextPixels:= @XYZASolidBrushSetPixels
      else
        ABrush.InternalPutNextPixels:= @XYZASolidBrushDrawPixels;

    dmXor: raise exception.Create('Xor mode not available with floating point values');
  end;
end;

class procedure TXYZABitmap.ScannerBrush(out ABrush: TUniversalBrush;
  AScanner: IBGRAScanner; ADrawMode: TDrawMode;
  AOffsetX: integer; AOffsetY: integer);
var
  sourceSpace: TColorspaceAny;
begin
  ABrush.Colorspace:= TXYZAColorspace;
  with PXYZAScannerBrushFixedData(@ABrush.FixedData)^ do
  begin
    Scanner := Pointer(AScanner);
    OffsetX := AOffsetX;
    OffsetY := AOffsetY;
  end;
  ABrush.InternalInitContext:= @XYZAScannerBrushInitContext;
  sourceSpace := AScanner.GetScanCustomColorspace;
  if sourceSpace = TXYZAColorspace then
  begin
    case ADrawMode of
      dmSet: ABrush.InternalPutNextPixels:= @XYZAScannerChunkBrushSetPixels;
      dmSetExceptTransparent: ABrush.InternalPutNextPixels:= @XYZAScannerChunkBrushSetPixelsExceptTransparent;
      dmDrawWithTransparency,dmLinearBlend:
        ABrush.InternalPutNextPixels:= @XYZAScannerChunkBrushDrawPixels;
      dmXor: raise exception.Create('Xor mode not available with floating point values');
    end;
  end else
  begin
    with PXYZAScannerBrushFixedData(@ABrush.FixedData)^ do
      Conversion := sourceSpace.GetBridgedConversion(TXYZAColorspace);
    case ADrawMode of
      dmSet: ABrush.InternalPutNextPixels:= @XYZAScannerConvertBrushSetPixels;
      dmSetExceptTransparent: ABrush.InternalPutNextPixels:= @XYZAScannerConvertBrushSetPixelsExceptTransparent;
      dmDrawWithTransparency,dmLinearBlend:
        ABrush.InternalPutNextPixels:= @XYZAScannerConvertBrushDrawPixels;
      dmXor: raise exception.Create('Xor mode not available with floating point values');
    end;
  end;
end;

class procedure TXYZABitmap.MaskBrush(out ABrush: TUniversalBrush;
  AScanner: IBGRAScanner; AOffsetX: integer; AOffsetY: integer);
begin
  ABrush.Colorspace:= TXYZAColorspace;
  with PXYZAScannerBrushFixedData(@ABrush.FixedData)^ do
  begin
    Scanner := Pointer(AScanner);
    OffsetX := AOffsetX;
    OffsetY := AOffsetY;
  end;
  ABrush.InternalInitContext:= @XYZAScannerBrushInitContext;
  ABrush.InternalPutNextPixels:= @XYZAMaskBrushApply;
end;

class procedure TXYZABitmap.EraseBrush(out ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  if AAlpha = 0 then
  begin
    SolidBrush(ABrush, XYZATransparent, dmDrawWithTransparency);
    exit;
  end;
  ABrush.Colorspace:= TXYZAColorspace;
  PSingle(@ABrush.FixedData)^ := AAlpha/65535;
  ABrush.InternalInitContext:= nil;
  ABrush.InternalPutNextPixels:= @XYZAAlphaBrushErasePixels;
end;

class procedure TXYZABitmap.AlphaBrush(out ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  if AAlpha = 0 then
  begin
    SolidBrush(ABrush, XYZATransparent, dmSet);
    exit;
  end;
  ABrush.Colorspace:= TXYZAColorspace;
  PSingle(@ABrush.FixedData)^ := AAlpha/65535;
  ABrush.InternalInitContext:= nil;
  ABrush.InternalPutNextPixels:= @XYZAAlphaBrushSetPixels;
end;

procedure TXYZABitmap.ReplaceImaginary(const AAfter: TXYZA);
var
  p: PXYZA;
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

finalization

  xyHorseshoePolygon.Free;

end.

