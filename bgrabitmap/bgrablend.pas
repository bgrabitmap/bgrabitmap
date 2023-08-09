// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRABlend;

{ This unit contains pixel blending functions. They take a destination adress as parameter,
  and draw pixels at this address with different blending modes. These functions are used
  by many functions in BGRABitmap library to do the low level drawing. }

{$mode objfpc}{$H+}

interface

uses
  BGRABitmapTypes;

{ Brush providers }

procedure BGRASolidBrushIndirect(out ABrush: TUniversalBrush; AColor: Pointer; ADrawMode: TDrawMode = dmDrawWithTransparency);
procedure BGRAScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode = dmDrawWithTransparency;
                           AOffsetX: integer = 0; AOffsetY: integer = 0);
procedure BGRAMaskBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; AOffsetX: integer = 0; AOffsetY: integer = 0);
procedure BGRAEraseBrush(out ABrush: TUniversalBrush; AAlpha: Word);
procedure BGRAAlphaBrush(out ABrush: TUniversalBrush; AAlpha: Word);

{ Draw one pixel with alpha blending }
procedure DrawPixelInlineWithAlphaCheck(dest: PBGRAPixel; const c: TBGRAPixel); inline; overload;
procedure DrawPixelInlineWithAlphaCheck(dest: PBGRAPixel; c: TBGRAPixel; appliedOpacity: byte); inline; overload;
procedure DrawExpandedPixelInlineWithAlphaCheck(dest: PBGRAPixel; const ec: TExpandedPixel); inline; overload;
procedure DrawPixelInlineExpandedOrNotWithAlphaCheck(dest: PBGRAPixel; const ec: TExpandedPixel; c: TBGRAPixel); inline; overload;  //alpha in 'c' parameter
procedure DrawPixelInlineNoAlphaCheck(dest: PBGRAPixel; const c: TBGRAPixel); inline; overload;
procedure DrawExpandedPixelInlineNoAlphaCheck(dest: PBGRAPixel; const ec: TExpandedPixel; calpha: byte); inline; overload;
procedure ClearTypeDrawPixel(pdest: PBGRAPixel; Cr, Cg, Cb: byte; Color: TBGRAPixel); inline;
procedure InterpolateBilinear(pUpLeft,pUpRight,pDownLeft,pDownRight: PBGRAPixel;
                iFactX,iFactY: Integer; ADest: PBGRAPixel);
procedure InterpolateBilinearMask(pUpLeft,pUpRight,pDownLeft,pDownRight: PByteMask;
                iFactX,iFactY: Integer; ADest: PByteMask);

procedure CopyPixelsWithOpacity(dest,src: PBGRAPixel; opacity: byte; Count: integer); inline;
function ApplyOpacity(opacity1,opacity2: byte): byte; inline;
function FastRoundDiv255(value: LongWord): LongWord; inline;

{ Draw a series of pixels with alpha blending }
procedure PutPixels(pdest: PBGRAPixel; psource: PBGRAPixel; copycount: integer; mode: TDrawMode; AOpacity:byte);
procedure DrawPixelsInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer); inline; overload;
procedure DrawExpandedPixelsInline(dest: PBGRAPixel; ec: TExpandedPixel; Count: integer); inline; overload;
procedure DrawPixelsInlineExpandedOrNot(dest: PBGRAPixel; ec: TExpandedPixel; c: TBGRAPixel; Count: integer); inline; overload;  //alpha in 'c' parameter

{ Draw one pixel with linear alpha blending }
procedure FastBlendPixelInline(dest: PBGRAPixel; const c: TBGRAPixel); inline; overload;
procedure FastBlendPixelInline(dest: PBGRAPixel; c: TBGRAPixel; appliedOpacity: byte); inline; overload;

{ Draw a series of pixels with linear alpha blending }
procedure FastBlendPixelsInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer); inline;

{ Replace a series of pixels }
procedure FillInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer); inline;

{ Xor a series of pixels }
procedure XorInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer); inline;
procedure XorPixels(pdest, psrc: PBGRAPixel; count: integer);

{ Set alpha value for a series of pixels }
procedure AlphaFillInline(dest: PBGRAPixel; alpha: byte; Count: integer); inline;

{ Erase a series of pixels, i.e. decrease alpha value }
procedure ErasePixelInline(dest: PBGRAPixel; alpha: byte); inline;

{ Draw a pixel to the extent the current pixel is close enough to compare value.
  It should not be called on pixels that have not been checked to be close enough }
procedure DrawPixelInlineDiff(dest: PBGRAPixel; c, compare: TBGRAPixel;
  maxDiff: byte); inline;
{ Draw a series of pixel to the extent the current pixel is close enough to compare value }
procedure DrawPixelsInlineDiff(dest: PBGRAPixel; c: TBGRAPixel;
  Count: integer; compare: TBGRAPixel; maxDiff: byte); inline;

{ Blend pixels with scanner content }
procedure ScannerPutPixels(scan: IBGRAScanner; pdest: PBGRAPixel; count: integer; mode: TDrawMode);

{ Perform advanced blending operation }
procedure BlendPixels(pdest: PBGRAPixel; psrc: PBGRAPixel;
  blendOp: TBlendOperation; Count: integer; excludeChannels: TChannels = []);

{ Perform blending operation and merge over destination }
procedure BlendPixelsOver(pdest: PBGRAPixel; psrc: PBGRAPixel;
  blendOp: TBlendOperation; Count: integer; opacity: byte; linearBlend: boolean = false;
  excludeChannels: TChannels = []);

//layer blend modes
//- http://www.pegtop.net/delphi/articles/blendmodes/
//- http://www.w3.org/TR/2009/WD-SVGCompositing-20090430/#comp-op
//- http://docs.gimp.org/en/gimp-concepts-layer-modes.html
procedure LinearMultiplyPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure AddPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearAddPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure ColorBurnPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure ColorDodgePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure DividePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure ReflectPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure NonLinearReflectPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure GlowPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure NiceGlowPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure OverlayPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearOverlayPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure DifferencePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearDifferencePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure ExclusionPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearExclusionPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearSubtractPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearSubtractInversePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure SubtractPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure SubtractInversePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure NegationPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearNegationPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LightenPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure DarkenPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure ScreenPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure SoftLightPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure SvgSoftLightPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure HardLightPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure BlendXorPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure BlendMaskPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearMultiplySaturationInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearHueInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearColorInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearLightnessInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearSaturationInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure CorrectedHueInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure CorrectedColorInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure CorrectedLightnessInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure CorrectedSaturationInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure BGRAFillClearTypeMask(dest: TBGRACustomBitmap; x,y: integer; xThird: integer; mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner; RGBOrder: boolean);
procedure BGRAFillClearTypeRGBMask(dest: TBGRACustomBitmap; x, y: integer;
  mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner;
  KeepRGBOrder: boolean);
procedure BGRAFillClearTypeMaskPtr(dest: TBGRACustomBitmap; x,y: integer; xThird: integer; maskData: PByte; maskPixelSize: Int32or64; maskRowSize: Int32or64; maskWidth,maskHeight: integer; color: TBGRAPixel; texture: IBGRAScanner; RGBOrder: boolean);

implementation

type
  PBGRASolidBrushFixedData = ^TBGRASolidBrushFixedData;
  TBGRASolidBrushFixedData = record
    BGRA: TBGRAPixel;
    Expanded: TExpandedPixel;
  end;

procedure BGRASolidBrushSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PBGRAPixel;
  bAlpha: Byte;
begin
  if AAlpha <= $80 then
  begin
    inc(PBGRAPixel(AContextData^.Dest), ACount);
    exit;
  end;
  pDest := PBGRAPixel(AContextData^.Dest);
  if AAlpha >= $ff7f then
  begin
    FillDWord(pDest^, ACount, PLongWord(@PBGRASolidBrushFixedData(AFixedData)^.BGRA)^);
    inc(pDest, ACount);
  end else
  begin
    with PBGRASolidBrushFixedData(AFixedData)^ do
    begin
      if BGRA.alpha = 255 then
      begin
        bAlpha := FastRoundDiv257(AAlpha);
        while ACount > 0 do
        begin
          DrawExpandedPixelInlineNoAlphaCheck(pDest, Expanded, bAlpha);
          inc(pDest);
          dec(ACount);
        end;
      end
      else
      begin
        while ACount > 0 do
        begin
          pDest^ := GammaCompression(MergeBGRA(GammaExpansion(pDest^), not AAlpha, Expanded, AAlpha));
          inc(pDest);
          dec(ACount);
        end;
      end;
    end;
  end;
  PBGRAPixel(AContextData^.Dest) := pDest;
end;

procedure BGRASolidBrushSkipPixels({%H-}AFixedData: Pointer;
  AContextData: PUniBrushContext; {%H-}AAlpha: Word; ACount: integer);
begin
  inc(PBGRAPixel(AContextData^.Dest), ACount);
end;

procedure BGRASolidBrushDrawPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  bAlpha: byte;
  pDest: PBGRAPixel;
begin
  if AAlpha <= $80 then
  begin
    inc(PBGRAPixel(AContextData^.Dest), ACount);
    exit;
  end;
  with PBGRASolidBrushFixedData(AFixedData)^ do
  begin
    pDest := PBGRAPixel(AContextData^.Dest);
    bAlpha := FastRoundDiv257(Expanded.alpha*AAlpha shr 16);
    if bAlpha = 255 then
    begin
      FillDWord(pDest^, ACount, PLongWord(@BGRA)^);
      inc(pDest, ACount);
    end else
    begin
      while ACount > 0 do
      begin
        DrawExpandedPixelInlineNoAlphaCheck(pDest, Expanded, bAlpha);
        inc(pDest);
        dec(ACount);
      end;
    end;
    PBGRAPixel(AContextData^.Dest) := pDest;
  end;
end;

procedure BGRASolidBrushLinearDrawPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  c: TBGRAPixel;
  pDest: PBGRAPixel;
begin
  if AAlpha <= $80 then
  begin
    inc(PBGRAPixel(AContextData^.Dest), ACount);
    exit;
  end;
  with PBGRASolidBrushFixedData(AFixedData)^ do
  begin
    pDest := PBGRAPixel(AContextData^.Dest);
    if AAlpha >= $ff7f then
    begin
      while ACount > 0 do
      begin
        FastBlendPixelInline(pDest, BGRA);
        inc(pDest);
        dec(ACount);
      end;
    end else
    begin
      c := BGRA;
      c.alpha := FastRoundDiv257(c.alpha*AAlpha shr 8);
      while ACount > 0 do
      begin
        FastBlendPixelInline(pDest, c);
        inc(pDest);
        dec(ACount);
      end;
    end;
    PBGRAPixel(AContextData^.Dest) := pDest;
  end;
end;

procedure BGRASolidBrushXorPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  c: TBGRAPixel;
  pDest: PBGRAPixel;
begin
  if AAlpha <= $80 then
  begin
    inc(PBGRAPixel(AContextData^.Dest), ACount);
    exit;
  end;
  with PBGRASolidBrushFixedData(AFixedData)^ do
  begin
    pDest := PBGRAPixel(AContextData^.Dest);
    if AAlpha >= $ff7f then
    begin
      while ACount > 0 do
      begin
        PLongWord(pdest)^ := PLongWord(pdest)^ xor PLongWord(@BGRA)^;
        inc(pDest);
        dec(ACount);
      end;
    end else
    begin
      while ACount > 0 do
      begin
        PLongWord(@c)^ := PLongWord(pdest)^ xor PLongWord(@BGRA)^;
        pDest^ := MergeBGRA(pDest^, not AAlpha, c, AAlpha);
        inc(pDest);
        dec(ACount);
      end;
    end;
    PBGRAPixel(AContextData^.Dest) := pDest;
  end;
end;

procedure BGRASolidBrushIndirect(out ABrush: TUniversalBrush; AColor: Pointer;
  ADrawMode: TDrawMode);
begin
  ABrush.Colorspace:= TBGRAPixelColorspace;
  with PBGRASolidBrushFixedData(@ABrush.FixedData)^ do
  begin
    BGRA := PBGRAPixel(AColor)^;
    if not (ADrawMode in[dmLinearBlend,dmXor]) then
      Expanded := GammaExpansion(BGRA);
  end;
  ABrush.InternalInitContext:= nil;
  case ADrawMode of
    dmSet: ABrush.InternalPutNextPixels:= @BGRASolidBrushSetPixels;

    dmSetExceptTransparent: if PBGRAPixel(AColor)^.alpha <> 255 then
      begin
        ABrush.InternalPutNextPixels:= @BGRASolidBrushSkipPixels;
        ABrush.DoesNothing := true;
      end else ABrush.InternalPutNextPixels:= @BGRASolidBrushSetPixels;

    dmDrawWithTransparency: if PBGRAPixel(AColor)^.alpha = 0 then
      begin
        ABrush.InternalPutNextPixels:= @BGRASolidBrushSkipPixels;
        ABrush.DoesNothing := true;
      end else ABrush.InternalPutNextPixels:= @BGRASolidBrushDrawPixels;

    dmLinearBlend: if PBGRAPixel(AColor)^.alpha = 0 then
      begin
        ABrush.InternalPutNextPixels:= @BGRASolidBrushSkipPixels;
        ABrush.DoesNothing := true;
      end
        else ABrush.InternalPutNextPixels:= @BGRASolidBrushLinearDrawPixels;

    dmXor: if PLongWord(AColor)^ = 0 then
      begin
        ABrush.InternalPutNextPixels:= @BGRASolidBrushSkipPixels;
        ABrush.DoesNothing := true;
      end else ABrush.InternalPutNextPixels:= @BGRASolidBrushXorPixels;
  end;

end;

type
  PBGRAScannerBrushFixedData = ^TBGRAScannerBrushFixedData;
  TBGRAScannerBrushFixedData = record
    Scanner: Pointer; //avoid ref count by using pointer type
    OffsetX, OffsetY: integer;
    case boolean of
    true: (HasPutPixels: boolean);           //BGRA
    false: (Conversion: TBridgedConversion); //other
  end;

procedure BRGBAScannerBrushInitContext(AFixedData: Pointer;
  AContextData: PUniBrushContext);
begin
  with PBGRAScannerBrushFixedData(AFixedData)^ do
    IBGRAScanner(Scanner).ScanMoveTo(AContextData^.Ofs.X + OffsetX,
                                     AContextData^.Ofs.Y + OffsetY);
end;

procedure BGRAScannerBrushSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  bAlpha: Byte;
  pDest: PBGRAPixel;
  buf: packed array[0..3] of TBGRAPixel;
begin
  with PBGRAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha <= $80 then
    begin
      inc(PBGRAPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PBGRAPixel(AContextData^.Dest);
    if AAlpha >= $ff7f then
    begin
      if HasPutPixels then
      begin
        IBGRAScanner(Scanner).ScanPutPixels(pDest, ACount, dmSet);
        inc(pDest, ACount);
      end else
        while ACount > 0 do
        begin
          pDest^ := IBGRAScanner(Scanner).ScanNextPixel;
          inc(pDest);
          dec(ACount);
        end;
    end else
    begin
      bAlpha := FastRoundDiv257(AAlpha);
      if HasPutPixels then
      begin
        while ACount > 3 do
        begin
          IBGRAScanner(Scanner).ScanPutPixels({%H-}buf, 4, dmSet);
          pDest^ := MergeBGRAWithGammaCorrection(pDest^, not bAlpha, buf[0], bAlpha);
          (pDest+1)^ := MergeBGRAWithGammaCorrection((pDest+1)^, not bAlpha, buf[1], bAlpha);
          (pDest+2)^ := MergeBGRAWithGammaCorrection((pDest+2)^, not bAlpha, buf[2], bAlpha);
          (pDest+3)^ := MergeBGRAWithGammaCorrection((pDest+3)^, not bAlpha, buf[3], bAlpha);
          inc(pDest, 4);
          dec(ACount, 4);
        end;
      end;
      while ACount > 0 do
      begin
        pDest^ := MergeBGRAWithGammaCorrection(pDest^, not bAlpha, IBGRAScanner(Scanner).ScanNextPixel, bAlpha);
        inc(pDest);
        dec(ACount);
      end;
    end;
  end;
  PBGRAPixel(AContextData^.Dest) := pDest;
end;

procedure BGRAScannerConvertBrushSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pBuf,pDest: PBGRAPixel;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TBGRAPixel;
  bAlpha: Byte;
begin
  with PBGRAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PBGRAPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
    end else
    begin
      pDest := PBGRAPixel(AContextData^.Dest);
      pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
      if AAlpha >= $ff7f then
      begin
        while ACount > 0 do
        begin
          qty := ACount;
          IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
          Conversion.Convert(psrc, pDest, qty, pixSize, sizeof(TBGRAPixel), nil);
          inc(pDest, qty);
          dec(ACount, qty);
        end;
      end else
      begin
        bAlpha := FastRoundDiv257(AAlpha);
        while ACount > 0 do
        begin
          if ACount > length(buf) then qty := length(buf) else qty := ACount;
          IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
          Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TBGRAPixel), nil);
          pBuf := @buf;
          dec(ACount, qty);
          while qty > 0 do
          begin
            pDest^ := MergeBGRAWithGammaCorrection(pDest^, not bAlpha, pBuf^, bAlpha);
            inc(pDest);
            inc(pBuf);
            dec(qty);
          end;
        end;
      end;
      AContextData^.Dest := pDest;
    end;
  end;
end;

procedure BGRAScannerBrushSetPixelsExceptTransparent(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  bAlpha: Byte;
  pDest: PBGRAPixel;
  buf: packed array[0..3] of TBGRAPixel;
begin
  with PBGRAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha <= $80 then
    begin
      inc(PBGRAPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PBGRAPixel(AContextData^.Dest);
    if AAlpha >= $ff7f then
    begin
      if HasPutPixels then
      begin
        IBGRAScanner(Scanner).ScanPutPixels(pDest, ACount, dmSetExceptTransparent);
        inc(pDest, ACount);
      end else
        while ACount > 0 do
        begin
          buf[0] := IBGRAScanner(Scanner).ScanNextPixel;
          if buf[0].alpha = 255 then pDest^ := buf[0];
          inc(pDest);
          dec(ACount);
        end;
    end else
    begin
      bAlpha := FastRoundDiv257(AAlpha);
      if HasPutPixels then
      begin
        while ACount > 3 do
        begin
          IBGRAScanner(Scanner).ScanPutPixels({%H-}buf, 4, dmSet);
          if buf[0].alpha = 255 then pDest^ := MergeBGRAWithGammaCorrection(pDest^, not bAlpha, buf[0], bAlpha);
          if buf[1].alpha = 255 then (pDest+1)^ := MergeBGRAWithGammaCorrection((pDest+1)^, not bAlpha, buf[1], bAlpha);
          if buf[2].alpha = 255 then (pDest+2)^ := MergeBGRAWithGammaCorrection((pDest+2)^, not bAlpha, buf[2], bAlpha);
          if buf[3].alpha = 255 then (pDest+3)^ := MergeBGRAWithGammaCorrection((pDest+3)^, not bAlpha, buf[3], bAlpha);
          inc(pDest, 4);
          dec(ACount, 4);
        end;
      end;
      while ACount > 0 do
      begin
        buf[0] := IBGRAScanner(Scanner).ScanNextPixel;
        if buf[0].alpha = 255 then
          pDest^ := MergeBGRAWithGammaCorrection(pDest^, not bAlpha, buf[0], bAlpha);
        inc(pDest);
        dec(ACount);
      end;
    end;
  end;
  PBGRAPixel(AContextData^.Dest) := pDest;
end;

procedure BGRAScannerConvertBrushSetPixelsExceptTransparent(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pBuf,pDest: PBGRAPixel;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TBGRAPixel;
  bAlpha: Byte;
begin
  with PBGRAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PBGRAPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
    end else
    begin
      pDest := PBGRAPixel(AContextData^.Dest);
      pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
      if AAlpha >= $ff7f then
      begin
        while ACount > 0 do
        begin
          if ACount > length(buf) then qty := length(buf) else qty := ACount;
          IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
          Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TBGRAPixel), nil);
          pBuf := @buf;
          dec(ACount, qty);
          while qty > 0 do
          begin
            if pBuf^.alpha = 255 then pDest^ := pBuf^;
            inc(pDest);
            inc(pBuf);
            dec(qty);
          end;
        end;
      end else
      begin
        bAlpha := FastRoundDiv257(AAlpha);
        while ACount > 0 do
        begin
          if ACount > length(buf) then qty := length(buf) else qty := ACount;
          IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
          Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TBGRAPixel), nil);
          pBuf := @buf;
          dec(ACount, qty);
          while qty > 0 do
          begin
            if pBuf^.alpha = 255 then
              pDest^ := MergeBGRAWithGammaCorrection(pDest^, not bAlpha, pBuf^, bAlpha);
            inc(pDest);
            inc(pBuf);
            dec(qty);
          end;
        end;
      end;
      AContextData^.Dest := pDest;
    end;
  end;
end;

procedure BGRAScannerBrushDrawPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  bAlpha: Byte;
  pDest: PBGRAPixel;
  buf: packed array[0..3] of TBGRAPixel;
begin
  with PBGRAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha <= $80 then
    begin
      inc(PBGRAPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PBGRAPixel(AContextData^.Dest);
    if AAlpha >= $ff7f then
    begin
      if HasPutPixels then
      begin
        IBGRAScanner(Scanner).ScanPutPixels(pDest, ACount, dmDrawWithTransparency);
        inc(pDest, ACount);
      end else
        while ACount > 0 do
        begin
          buf[0] := IBGRAScanner(Scanner).ScanNextPixel;
          DrawPixelInlineWithAlphaCheck(pDest, buf[0]);
          inc(pDest);
          dec(ACount);
        end;
    end else
    begin
      bAlpha := FastRoundDiv257(AAlpha);
      if HasPutPixels then
      begin
        while ACount > 3 do
        begin
          IBGRAScanner(Scanner).ScanPutPixels({%H-}buf, 4, dmSet);
          DrawPixelInlineWithAlphaCheck(pDest, buf[0], bAlpha);
          DrawPixelInlineWithAlphaCheck(pDest+1, buf[1], bAlpha);
          DrawPixelInlineWithAlphaCheck(pDest+2, buf[2], bAlpha);
          DrawPixelInlineWithAlphaCheck(pDest+3, buf[3], bAlpha);
          inc(pDest, 4);
          dec(ACount, 4);
        end;
      end;
      while ACount > 0 do
      begin
        buf[0] := IBGRAScanner(Scanner).ScanNextPixel;
        DrawPixelInlineWithAlphaCheck(pDest, buf[0], bAlpha);
        inc(pDest);
        dec(ACount);
      end;
    end;
  end;
  PBGRAPixel(AContextData^.Dest) := pDest;
end;

procedure BGRAScannerConvertBrushDrawPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pBuf,pDest: PBGRAPixel;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TBGRAPixel;
  bAlpha: Byte;
begin
  with PBGRAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PBGRAPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
    end else
    begin
      pDest := PBGRAPixel(AContextData^.Dest);
      pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
      if AAlpha >= $ff7f then
      begin
        while ACount > 0 do
        begin
          if ACount > length(buf) then qty := length(buf) else qty := ACount;
          IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
          Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TBGRAPixel), nil);
          pBuf := @buf;
          dec(ACount, qty);
          while qty > 0 do
          begin
            DrawPixelInlineWithAlphaCheck(pDest, pBuf^);
            inc(pDest);
            inc(pBuf);
            dec(qty);
          end;
        end;
      end else
      begin
        bAlpha := FastRoundDiv257(AAlpha);
        while ACount > 0 do
        begin
          if ACount > length(buf) then qty := length(buf) else qty := ACount;
          IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
          Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TBGRAPixel), nil);
          pBuf := @buf;
          dec(ACount, qty);
          while qty > 0 do
          begin
            DrawPixelInlineWithAlphaCheck(pDest, pBuf^, bAlpha);
            inc(pDest);
            inc(pBuf);
            dec(qty);
          end;
        end;
      end;
      AContextData^.Dest := pDest;
    end;
  end;
end;

procedure BGRAScannerBrushLinearDrawPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  bAlpha: Byte;
  pDest: PBGRAPixel;
  buf: packed array[0..3] of TBGRAPixel;
begin
  with PBGRAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha <= $80 then
    begin
      inc(PBGRAPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PBGRAPixel(AContextData^.Dest);
    if AAlpha >= $ff7f then
    begin
      if HasPutPixels then
      begin
        IBGRAScanner(Scanner).ScanPutPixels(pDest, ACount, dmLinearBlend);
        inc(pDest, ACount);
      end else
        while ACount > 0 do
        begin
          buf[0] := IBGRAScanner(Scanner).ScanNextPixel;
          FastBlendPixelInline(pDest, buf[0]);
          inc(pDest);
          dec(ACount);
        end;
    end else
    begin
      bAlpha := FastRoundDiv257(AAlpha);
      if HasPutPixels then
      begin
        while ACount > 3 do
        begin
          IBGRAScanner(Scanner).ScanPutPixels({%H-}buf, 4, dmSet);
          FastBlendPixelInline(pDest, buf[0], bAlpha);
          FastBlendPixelInline(pDest+1, buf[1], bAlpha);
          FastBlendPixelInline(pDest+2, buf[2], bAlpha);
          FastBlendPixelInline(pDest+3, buf[3], bAlpha);
          inc(pDest, 4);
          dec(ACount, 4);
        end;
      end;
      while ACount > 0 do
      begin
        buf[0] := IBGRAScanner(Scanner).ScanNextPixel;
        FastBlendPixelInline(pDest, buf[0], bAlpha);
        inc(pDest);
        dec(ACount);
      end;
    end;
  end;
  PBGRAPixel(AContextData^.Dest) := pDest;
end;

procedure BGRAScannerConvertBrushLinearDrawPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pBuf,pDest: PBGRAPixel;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TBGRAPixel;
  bAlpha: Byte;
begin
  with PBGRAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PBGRAPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
    end else
    begin
      pDest := PBGRAPixel(AContextData^.Dest);
      pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
      if AAlpha >= $ff7f then
      begin
        while ACount > 0 do
        begin
          if ACount > length(buf) then qty := length(buf) else qty := ACount;
          IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
          Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TBGRAPixel), nil);
          pBuf := @buf;
          dec(ACount, qty);
          while qty > 0 do
          begin
            FastBlendPixelInline(pDest, pBuf^);
            inc(pDest);
            inc(pBuf);
            dec(qty);
          end;
        end;
      end else
      begin
        bAlpha := FastRoundDiv257(AAlpha);
        while ACount > 0 do
        begin
          if ACount > length(buf) then qty := length(buf) else qty := ACount;
          IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
          Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TBGRAPixel), nil);
          pBuf := @buf;
          dec(ACount, qty);
          while qty > 0 do
          begin
            FastBlendPixelInline(pDest, pBuf^, bAlpha);
            inc(pDest);
            inc(pBuf);
            dec(qty);
          end;
        end;
      end;
      AContextData^.Dest := pDest;
    end;
  end;
end;

procedure BGRAScannerBrushXorPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  bAlpha: byte;
  pDest: PBGRAPixel;
  buf: packed array[0..3] of TBGRAPixel;
begin
  with PBGRAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha <= $80 then
    begin
      inc(PBGRAPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PBGRAPixel(AContextData^.Dest);
    if AAlpha >= $ff7f then
    begin
      if HasPutPixels then
      begin
        IBGRAScanner(Scanner).ScanPutPixels(pDest, ACount, dmXor);
        inc(pDest, ACount);
      end else
        while ACount > 0 do
        begin
          buf[0] := IBGRAScanner(Scanner).ScanNextPixel;
          PLongWord(pdest)^ := PLongWord(pdest)^ xor PLongWord(@buf[0])^;
          inc(pDest);
          dec(ACount);
        end;
    end else
    begin
      bAlpha := FastRoundDiv257(AAlpha);
      if HasPutPixels then
      begin
        while ACount > 3 do
        begin
          IBGRAScanner(Scanner).ScanPutPixels({%H-}buf, 4, dmSet);
          PLongWord(@buf[0])^ := PLongWord(pdest)^ xor PLongWord(@buf[0])^;
          PLongWord(@buf[1])^ := PLongWord(pdest+1)^ xor PLongWord(@buf[1])^;
          PLongWord(@buf[2])^ := PLongWord(pdest+2)^ xor PLongWord(@buf[2])^;
          PLongWord(@buf[3])^ := PLongWord(pdest+3)^ xor PLongWord(@buf[3])^;
          pDest^ := MergeBGRA(pDest^, not bAlpha, buf[0], bAlpha);
          (pDest+1)^ := MergeBGRA((pDest+1)^, not bAlpha, buf[1], bAlpha);
          (pDest+2)^ := MergeBGRA((pDest+2)^, not bAlpha, buf[2], bAlpha);
          (pDest+3)^ := MergeBGRA((pDest+3)^, not bAlpha, buf[3], bAlpha);
          inc(pDest, 4);
          dec(ACount, 4);
        end;
      end;
      while ACount > 0 do
      begin
        buf[0] := IBGRAScanner(Scanner).ScanNextPixel;
        PLongWord(@buf[0])^ := PLongWord(pdest)^ xor PLongWord(@buf[0])^;
        pDest^ := MergeBGRA(pDest^, not bAlpha, buf[0], bAlpha);
        inc(pDest);
        dec(ACount);
      end;
    end;
  end;
  PBGRAPixel(AContextData^.Dest) := pDest;
end;

procedure BGRAScannerConvertBrushXorPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pBuf,pDest: PBGRAPixel;
  qty, pixSize: Integer;
  buf: packed array[0..7] of TBGRAPixel;
  bAlpha: Byte;
begin
  with PBGRAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PBGRAPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
    end else
    begin
      pDest := PBGRAPixel(AContextData^.Dest);
      pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
      if AAlpha >= $ff7f then
      begin
        while ACount > 0 do
        begin
          if ACount > length(buf) then qty := length(buf) else qty := ACount;
          IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
          Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TBGRAPixel), nil);
          pBuf := @buf;
          dec(ACount, qty);
          while qty > 0 do
          begin
            PLongWord(pDest)^ := PLongWord(pDest)^ xor PLongWord(pBuf)^;
            inc(pDest);
            inc(pBuf);
            dec(qty);
          end;
        end;
      end else
      begin
        bAlpha := FastRoundDiv257(AAlpha);
        while ACount > 0 do
        begin
          if ACount > length(buf) then qty := length(buf) else qty := ACount;
          IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
          Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TBGRAPixel), nil);
          pBuf := @buf;
          dec(ACount, qty);
          while qty > 0 do
          begin
            PLongWord(pBuf)^ := PLongWord(pDest)^ xor PLongWord(pBuf)^;
            pDest^ := MergeBGRA(pDest^, not bAlpha, pBuf^, bAlpha);
            inc(pDest);
            inc(pBuf);
            dec(qty);
          end;
        end;
      end;
      AContextData^.Dest := pDest;
    end;
  end;
end;

procedure BGRAScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner;
  ADrawMode: TDrawMode; AOffsetX: integer = 0; AOffsetY: integer = 0);
var
  sourceSpace: TColorspaceAny;
begin
  ABrush.Colorspace:= TBGRAPixelColorspace;
  with PBGRAScannerBrushFixedData(@ABrush.FixedData)^ do
  begin
    Scanner := Pointer(AScanner);
    HasPutPixels:= AScanner.IsScanPutPixelsDefined;
    OffsetX := AOffsetX;
    OffsetY := AOffsetY;
  end;
  ABrush.InternalInitContext:= @BRGBAScannerBrushInitContext;
  sourceSpace := AScanner.GetScanCustomColorspace;
  if (AScanner.IsScanPutPixelsDefined) or (sourceSpace = TBGRAPixelColorspace)
    or (sourceSpace = TExpandedPixelColorspace) then
  begin
    case ADrawMode of
      dmSet: ABrush.InternalPutNextPixels:= @BGRAScannerBrushSetPixels;
      dmSetExceptTransparent: ABrush.InternalPutNextPixels:= @BGRAScannerBrushSetPixelsExceptTransparent;
      dmDrawWithTransparency: ABrush.InternalPutNextPixels:= @BGRAScannerBrushDrawPixels;
      dmLinearBlend: ABrush.InternalPutNextPixels:= @BGRAScannerBrushLinearDrawPixels;
      dmXor: ABrush.InternalPutNextPixels:= @BGRAScannerBrushXorPixels;
    end;
  end else
  begin
    with PBGRAScannerBrushFixedData(@ABrush.FixedData)^ do
      Conversion := sourceSpace.GetBridgedConversion(TBGRAPixelColorspace);
    case ADrawMode of
      dmSet: ABrush.InternalPutNextPixels:= @BGRAScannerConvertBrushSetPixels;
      dmSetExceptTransparent: ABrush.InternalPutNextPixels:= @BGRAScannerConvertBrushSetPixelsExceptTransparent;
      dmLinearBlend: ABrush.InternalPutNextPixels:= @BGRAScannerConvertBrushLinearDrawPixels;
      dmDrawWithTransparency: ABrush.InternalPutNextPixels:= @BGRAScannerConvertBrushDrawPixels;
      dmXor: ABrush.InternalPutNextPixels:= @BGRAScannerConvertBrushXorPixels;
    end;
  end;
end;

procedure BGRASolidBrushErasePixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  bAlpha: byte;
  pDest: PBGRAPixel;
begin
  if AAlpha <= $80 then
  begin
    inc(PBGRAPixel(AContextData^.Dest), ACount);
    exit;
  end;
  pDest := PBGRAPixel(AContextData^.Dest);
  bAlpha := PByte(AFixedData)^ * AAlpha shr 16;
  while ACount > 0 do
  begin
    ErasePixelInline(pDest, bAlpha);
    inc(pDest);
    dec(ACount);
  end;
  PBGRAPixel(AContextData^.Dest) := pDest;
end;

procedure BGRAMaskBrushApply(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  bAlpha: Byte;
  pDest: PBGRAPixel;
  qty, maskStride: Integer;
  pMask: PByteMask;
begin
  with PBGRAScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha <= $80 then
    begin
      inc(PBGRAPixel(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PBGRAPixel(AContextData^.Dest);
    if AAlpha >= $ff7f then
    begin
      while ACount > 0 do
      begin
        qty := ACount;
        IBGRAScanner(Scanner).ScanNextMaskChunk(qty, pMask, maskStride);
        dec(ACount,qty);
        while qty > 0 do
        begin
          pDest^.alpha := ApplyOpacity(pDest^.alpha, pMask^.gray);
          if pDest^.alpha = 0 then pDest^ := BGRAPixelTransparent;
          inc(pDest);
          inc(pMask, maskStride);
          dec(qty);
        end;
      end;
    end else
    begin
      bAlpha := FastRoundDiv257(AAlpha);
      while ACount > 0 do
      begin
        qty := ACount;
        IBGRAScanner(Scanner).ScanNextMaskChunk(qty, pMask, maskStride);
        dec(ACount,qty);
        while qty > 0 do
        begin
          pDest^.alpha := ApplyOpacity(pDest^.alpha, ApplyOpacity(pMask^.gray, bAlpha));
          if pDest^.alpha = 0 then pDest^ := BGRAPixelTransparent;
          inc(pDest);
          inc(pMask, maskStride);
          dec(qty);
        end;
      end;
    end;
  end;
  PBGRAPixel(AContextData^.Dest) := pDest;
end;

procedure BGRAMaskBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner;
  AOffsetX: integer; AOffsetY: integer);
begin
  ABrush.Colorspace:= TBGRAPixelColorspace;
  with PBGRAScannerBrushFixedData(@ABrush.FixedData)^ do
  begin
    Scanner := Pointer(AScanner);
    OffsetX := AOffsetX;
    OffsetY := AOffsetY;
  end;
  ABrush.InternalInitContext:= @BRGBAScannerBrushInitContext;
  ABrush.InternalPutNextPixels:= @BGRAMaskBrushApply;
end;

procedure BGRAEraseBrush(out ABrush: TUniversalBrush; AAlpha: Word);
var
  bAlpha: Byte;
begin
  if AAlpha >= $ff7f then
    BGRASolidBrushIndirect(ABrush, @BGRAPixelTransparent, dmSet)
  else
  begin
    ABrush.Colorspace:= TBGRAPixelColorspace;
    bAlpha := FastRoundDiv257(AAlpha);
    PByte(@ABrush.FixedData)^ := bAlpha;
    ABrush.InternalInitContext:= nil;
    if bAlpha = 0 then
      ABrush.InternalPutNextPixels:= @BGRASolidBrushSkipPixels
    else
      ABrush.InternalPutNextPixels:= @BGRASolidBrushErasePixels;
  end;
end;

procedure BGRASolidBrushAlphaPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  alphaAdd: Word;
  pDest: PBGRAPixel;
begin
  if AAlpha <= $80 then
  begin
    inc(PBGRAPixel(AContextData^.Dest), ACount);
    exit;
  end else
  if AAlpha >= $ff7f then
  begin
    AlphaFillInline(PBGRAPixel(AContextData^.Dest), PByte(AFixedData)^, ACount);
    inc(PBGRAPixel(AContextData^.Dest), ACount);
  end else
  begin
    pDest := PBGRAPixel(AContextData^.Dest);
    alphaAdd := PByte(AFixedData)^*AAlpha;
    while ACount > 0 do
    begin
      pDest^.alpha := (pDest^.alpha*(not AAlpha) + alphaAdd) shr 16;
      inc(pDest);
      dec(ACount);
    end;
    PBGRAPixel(AContextData^.Dest) := pDest;
  end;
end;

procedure BGRAAlphaBrush(out ABrush: TUniversalBrush; AAlpha: Word);
var
  bAlpha: Byte;
begin
  if AAlpha <= $80 then
    BGRASolidBrushIndirect(ABrush, @BGRAPixelTransparent, dmSet)
  else
  begin
    ABrush.Colorspace:= TBGRAPixelColorspace;
    bAlpha := FastRoundDiv257(AAlpha);
    PByte(@ABrush.FixedData)^ := bAlpha;
    ABrush.InternalInitContext:= nil;
    ABrush.InternalPutNextPixels:= @BGRASolidBrushAlphaPixels;
  end;
end;

procedure BGRAFillClearTypeMaskPtr(dest: TBGRACustomBitmap; x,y: integer; xThird: integer; maskData: PByte; maskPixelSize: Int32or64; maskRowSize: Int32or64; maskWidth,maskHeight: integer; color: TBGRAPixel; texture: IBGRAScanner; RGBOrder: boolean);
var
  pdest: PBGRAPixel;
  ClearTypePixel: array[0..2] of byte;
  curThird: integer;

  procedure OutputPixel; inline;
  begin
    if texture <> nil then
      color := texture.ScanNextPixel;
    if RGBOrder then
      ClearTypeDrawPixel(pdest, ClearTypePixel[0],ClearTypePixel[1],ClearTypePixel[2], color)
    else
      ClearTypeDrawPixel(pdest, ClearTypePixel[2],ClearTypePixel[1],ClearTypePixel[0], color);
  end;

  procedure NextAlpha(alphaValue: byte); inline;
  begin
    ClearTypePixel[curThird] := alphaValue;
    inc(curThird);
    if curThird = 3 then
    begin
      OutputPixel;
      curThird := 0;
      Fillchar(ClearTypePixel, sizeof(ClearTypePixel),0);
      inc(pdest);
    end;
  end;

  procedure EndRow; inline;
  begin
    if curThird > 0 then OutputPixel;
  end;

var
  yMask,n: integer;
  a: byte;
  pmask: PByte;
  dx:integer;
  miny,maxy,minx,minxThird,maxx,alphaMinX,alphaMaxX,alphaLineLen: integer;
  leftOnSide, rightOnSide: boolean;
  countBetween: integer;
  v1,v2,v3: byte;

  procedure StartRow; inline;
  begin
    pdest := dest.Scanline[yMask+y]+minx;
    if texture <> nil then
      texture.ScanMoveTo(minx,yMask+y);

    curThird := minxThird;
    ClearTypePixel[0] := 0;
    ClearTypePixel[1] := 0;
    ClearTypePixel[2] := 0;
  end;

begin
  alphaLineLen := maskWidth+2;

  dec(xThird); //for first subpixel

  if xThird >= 0 then dx := xThird div 3
   else dx := -((-xThird+2) div 3);
  inc(x, dx);
  dec(xThird, dx*3);

  if y >= dest.ClipRect.Top then miny := 0
    else miny := dest.ClipRect.Top-y;
  if y+maskHeight-1 < dest.ClipRect.Bottom then
    maxy := maskHeight-1 else
      maxy := dest.ClipRect.Bottom-1-y;

  if x >= dest.ClipRect.Left then
  begin
    minx := x;
    minxThird := xThird;
    alphaMinX := 0;
    leftOnSide := false;
  end else
  begin
    minx := dest.ClipRect.Left;
    minxThird := 0;
    alphaMinX := (dest.ClipRect.Left-x)*3 - xThird;
    leftOnSide := true;
  end;

  if x*3+xThird+maskWidth-1 < dest.ClipRect.Right*3 then
  begin
    maxx := (x*3+xThird+maskWidth-1) div 3;
    alphaMaxX := alphaLineLen-1;
    rightOnSide := false;
  end else
  begin
    maxx := dest.ClipRect.Right-1;
    alphaMaxX := maxx*3+2 - (x*3+xThird);
    rightOnSide := true;
  end;

  countBetween := alphaMaxX-alphaMinX-1;

  if (alphaMinX <= alphaMaxX) then
  begin
    for yMask := miny to maxy do
    begin
      StartRow;

      if leftOnSide then
      begin
        pmask := maskData + (yMask*maskRowSize)+ (alphaMinX-1)*maskPixelSize;
        a := pmask^ div 3;
        v1 := a+a;
        v2 := a;
        v3 := 0;
        inc(pmask, maskPixelSize);
      end else
      begin
        pmask := maskData + (yMask*maskRowSize);
        v1 := 0;
        v2 := 0;
        v3 := 0;
      end;

      for n := countBetween-1 downto 0 do
      begin
        a := pmask^ div 3;
        inc(v1, a);
        inc(v2, a);
        inc(v3, a);
        inc(pmask, maskPixelSize);

        NextAlpha(v1);
        v1 := v2;
        v2 := v3;
        v3 := 0;
      end;

      if rightOnSide then
      begin
        a := pmask^ div 3;
        inc(v1, a);
        inc(v2, a+a);
      end;

      NextAlpha(v1);
      NextAlpha(v2);

      EndRow;
    end;
  end;
end;

procedure BGRAFillClearTypeMask(dest: TBGRACustomBitmap; x,y: integer; xThird: integer; mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner; RGBOrder: boolean);
var delta: Int32or64;
begin
  delta := mask.Width*sizeof(TBGRAPixel);
  if mask.LineOrder = riloBottomToTop then
    delta := -delta;
  BGRAFillClearTypeMaskPtr(dest,x,y,xThird,pbyte(mask.ScanLine[0])+1,sizeof(TBGRAPixel),delta,mask.Width,mask.Height,color,texture,RGBOrder);
end;

procedure BGRAFillClearTypeRGBMask(dest: TBGRACustomBitmap; x, y: integer;
  mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner;
  KeepRGBOrder: boolean);
var
  minx,miny,maxx,maxy,countx,n,yb: integer;
  pdest,psrc: PBGRAPixel;
begin
  if y >= dest.ClipRect.Top then miny := 0
    else miny := dest.ClipRect.Top-y;
  if y+mask.Height-1 < dest.ClipRect.Bottom then
    maxy := mask.Height-1 else
      maxy := dest.ClipRect.Bottom-1-y;

  if x >= dest.ClipRect.Left then minx := 0
    else minx := dest.ClipRect.Left-x;
  if x+mask.Width-1 < dest.ClipRect.Right then
    maxx := mask.Width-1 else
      maxx := dest.ClipRect.Right-1-x;

  countx := maxx-minx+1;
  if countx <= 0 then exit;

  for yb := miny to maxy do
  begin
    pdest := dest.ScanLine[y+yb]+(x+minx);
    psrc := mask.ScanLine[yb]+minx;
    if texture <> nil then
      texture.ScanMoveTo(x+minx, y+yb);
    if KeepRGBOrder then
    begin
      for n := countx-1 downto 0 do
      begin
        if texture <> nil then color := texture.ScanNextPixel;
        ClearTypeDrawPixel(pdest, psrc^.red, psrc^.green, psrc^.blue, color);
        inc(pdest);
        inc(psrc);
      end;
    end else
    begin
      for n := countx-1 downto 0 do
      begin
        if texture <> nil then color := texture.ScanNextPixel;
        ClearTypeDrawPixel(pdest, psrc^.blue, psrc^.green, psrc^.red, color);
        inc(pdest);
        inc(psrc);
      end;
    end;
  end;
end;

procedure ClearTypeDrawPixel(pdest: PBGRAPixel; Cr, Cg, Cb: byte; Color: TBGRAPixel);
var merge,mergeClearType: TBGRAPixel;
    acc: word;
    keep,dont_keep: byte;
begin
  Cr := ApplyOpacity(Cr,color.alpha);
  Cg := ApplyOpacity(Cg,color.alpha);
  Cb := ApplyOpacity(Cb,color.alpha);
  acc := Cr+Cg+Cb;
  if acc = 0 then exit;

  merge := pdest^;
  mergeClearType.red := GammaCompressionTab[(GammaExpansionTab[merge.red] * (not byte(Cr)) +
                GammaExpansionTab[color.red] * Cr + 128) div 255];
  mergeClearType.green := GammaCompressionTab[(GammaExpansionTab[merge.green] * (not byte(Cg)) +
                GammaExpansionTab[color.green] * Cg + 128) div 255];
  mergeClearType.blue := GammaCompressionTab[(GammaExpansionTab[merge.blue] * (not byte(Cb)) +
                GammaExpansionTab[color.blue] * Cb + 128) div 255];
  mergeClearType.alpha := merge.alpha;

  if (mergeClearType.alpha = 255) then
    pdest^:= mergeClearType
  else
  begin
    if Cg <> 0 then
      DrawPixelInlineWithAlphaCheck(@merge, color, Cg);
    dont_keep := mergeClearType.alpha;
    if dont_keep > 0 then
    begin
      keep := not dont_keep;
      merge.red := GammaCompressionTab[(GammaExpansionTab[merge.red] * keep + GammaExpansionTab[mergeClearType.red] * dont_keep) div 255];
      merge.green := GammaCompressionTab[(GammaExpansionTab[merge.green] * keep + GammaExpansionTab[mergeClearType.green] * dont_keep) div 255];
      merge.blue := GammaCompressionTab[(GammaExpansionTab[merge.blue] * keep + GammaExpansionTab[mergeClearType.blue] * dont_keep) div 255];
      merge.alpha := mergeClearType.alpha + ApplyOpacity(merge.alpha, not mergeClearType.alpha);
    end;
    pdest^ := merge;
  end;
end;

procedure InterpolateBilinear(pUpLeft, pUpRight, pDownLeft,
  pDownRight: PBGRAPixel; iFactX,iFactY: Integer; ADest: PBGRAPixel);
var
  w1,w2,w3,w4,alphaW: LongWord;
  rSum, gSum, bSum: LongWord; //rgbDiv = aSum
  aSum, aDiv: LongWord;
begin
  rSum   := 0;
  gSum   := 0;
  bSum   := 0;
  aSum   := 0;
  aDiv   := 0;

  w4 := (iFactX*iFactY+127) shr 8;
  w3 := iFactY-w4;
  {$PUSH}{$HINTS OFF}
  w1 := (256-iFactX)-w3;
  {$POP}
  w2 := iFactX-w4;

  if (pUpLeft <> nil) and (pUpRight <> nil) and (pDownLeft <> nil) and (pDownRight <> nil) and
     (pUpLeft^.alpha = pUpRight^.alpha) and (pUpLeft^.alpha = pDownLeft^.alpha) and
     (pUpRight^.alpha = pDownRight^.alpha) then
  begin
    if pUpLeft^.alpha = 0 then
      ADest^ := BGRAPixelTransparent
    else
    begin
      ADest^.red := (pUpLeft^.red*w1 + pUpRight^.red*w2 + pDownLeft^.red*w3 + pDownRight^.red*w4 + 128) shr 8;
      ADest^.green := (pUpLeft^.green*w1 + pUpRight^.green*w2 + pDownLeft^.green*w3 + pDownRight^.green*w4 + 128) shr 8;
      ADest^.blue := (pUpLeft^.blue*w1 + pUpRight^.blue*w2 + pDownLeft^.blue*w3 + pDownRight^.blue*w4 + 128) shr 8;
      ADest^.alpha := pUpLeft^.alpha;
    end;
    exit;
  end;

  { For each pixel around the coordinate, compute
    the weight for it and multiply values by it before
    adding to the sum }
  if pUpLeft <> nil then
  with pUpLeft^ do
  begin
    alphaW := alpha * w1;
    inc(aDiv, w1);
    inc(aSum, alphaW);
    inc(rSum, red * alphaW);
    inc(gSum, green * alphaW);
    inc(bSum, blue * alphaW);
  end;
  if pUpRight <> nil then
  with pUpRight^ do
  begin
    alphaW := alpha * w2;
    inc(aDiv, w2);
    inc(aSum, alphaW);
    inc(rSum, red * alphaW);
    inc(gSum, green * alphaW);
    inc(bSum, blue * alphaW);
  end;
  if pDownLeft <> nil then
  with pDownLeft^ do
  begin
    alphaW := alpha * w3;
    inc(aDiv, w3);
    inc(aSum, alphaW);
    inc(rSum, red * alphaW);
    inc(gSum, green * alphaW);
    inc(bSum, blue * alphaW);
  end;
  if pDownRight <> nil then
  with pDownRight^ do
  begin
    alphaW := alpha * w4;
    inc(aDiv, w4);
    inc(aSum, alphaW);
    inc(rSum, red * alphaW);
    inc(gSum, green * alphaW);
    inc(bSum, blue * alphaW);
  end;

  if aSum < 128 then //if there is no alpha
    ADest^ := BGRAPixelTransparent
  else
  with ADest^ do
  begin
    red   := (rSum + aSum shr 1) div aSum;
    green := (gSum + aSum shr 1) div aSum;
    blue  := (bSum + aSum shr 1) div aSum;
    if aDiv = 256 then
      alpha := (aSum + 128) shr 8
    else
      alpha := (aSum + aDiv shr 1) div aDiv;
  end;
end;

procedure InterpolateBilinearMask(pUpLeft, pUpRight, pDownLeft,
  pDownRight: PByteMask; iFactX, iFactY: Integer; ADest: PByteMask);
var
  w1,w2,w3,w4,sum: LongWord;
begin
  w4 := (iFactX*iFactY+127) shr 8;
  w3 := iFactY-w4;
  {$PUSH}{$HINTS OFF}
  w1 := (256-iFactX)-w3;
  {$POP}
  w2 := iFactX-w4;

  if (pUpLeft <> nil) and (pUpRight <> nil) and (pDownLeft <> nil) and (pDownRight <> nil) then
    ADest^.gray := (pUpLeft^.gray*w1 + pUpRight^.gray*w2 + pDownLeft^.gray*w3 + pDownRight^.gray*w4 + 128) shr 8
  else
  begin
    sum := 0;
    if pUpLeft <> nil then inc(sum, pUpLeft^.gray*w1);
    if pUpRight <> nil then inc(sum, pUpRight^.gray*w2);
    if pDownLeft <> nil then inc(sum, pDownLeft^.gray*w3);
    if pDownRight <> nil then inc(sum, pDownRight^.gray*w4);
    ADest^.gray := (sum + 128) shr 8;
  end;
end;

procedure ScannerPutPixels(scan: IBGRAScanner; pdest: PBGRAPixel; count: integer; mode: TDrawMode);
var c : TBGRAPixel;
  i: Integer;
  scanNextFunc: function(): TBGRAPixel of object;
begin
  if scan.IsScanPutPixelsDefined then
    scan.ScanPutPixels(pdest,count,mode) else
  begin
    scanNextFunc := @scan.ScanNextPixel;
    case mode of
      dmLinearBlend:
        for i := 0 to count-1 do
        begin
          FastBlendPixelInline(pdest, scanNextFunc());
          inc(pdest);
        end;
      dmDrawWithTransparency:
        for i := 0 to count-1 do
        begin
          DrawPixelInlineWithAlphaCheck(pdest, scanNextFunc());
          inc(pdest);
        end;
      dmSet:
        for i := 0 to count-1 do
        begin
          pdest^ := scanNextFunc();
          inc(pdest);
        end;
      dmXor:
        for i := 0 to count-1 do
        begin
          PLongWord(pdest)^ := PLongWord(pdest)^ xor LongWord(scanNextFunc());
          inc(pdest);
        end;
      dmSetExceptTransparent:
        for i := 0 to count-1 do
        begin
          c := scanNextFunc();
          if c.alpha = 255 then pdest^ := c;
          inc(pdest);
        end;
    end;
  end;
end;

procedure XorInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer);
begin
  while Count > 0 do
  begin
    PLongWord(dest)^ := PLongWord(dest)^ xor LongWord(c);
    Inc(dest);
    Dec(Count);
  end;
end;

procedure XorPixels(pdest, psrc: PBGRAPixel; count: integer);
begin
  while Count > 0 do
  begin
    PLongWord(pdest)^ := PLongWord(psrc)^ xor PLongWord(pdest)^;
    Inc(pdest);
    Inc(psrc);
    Dec(Count);
  end;
end;

{$i blendpixels.inc}

procedure AlphaFillInline(dest: PBGRAPixel; alpha: byte; Count: integer); inline;
begin
  while Count > 0 do
  begin
    dest^.alpha := alpha;
    Inc(dest);
    Dec(Count);
  end;
end;

procedure FillInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer); inline;
begin
  FillDWord(dest^, Count, LongWord(c));
end;

procedure FastBlendPixelsInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer);
var
  n: integer;
begin
  if c.alpha = 0 then exit;
  for n := Count - 1 downto 0 do
  begin
    FastBlendPixelInline(dest, c);
    Inc(dest);
  end;
end;

procedure PutPixels(pdest: PBGRAPixel; psource: PBGRAPixel; copycount: integer;
  mode: TDrawMode; AOpacity: byte);
var i: integer; tempPixel: TBGRAPixel;
begin
  case mode of
    dmSet:
    begin
      if AOpacity <> 255 then
        CopyPixelsWithOpacity(pdest, psource, AOpacity, copycount)
      else
        move(psource^, pdest^, copycount * sizeof(TBGRAPixel));
    end;
    dmSetExceptTransparent:
    begin
        if AOpacity <> 255 then
        begin
          for i := copycount - 1 downto 0 do
          begin
            if psource^.alpha = 255 then
            begin
              tempPixel := psource^;
              tempPixel.alpha := ApplyOpacity(tempPixel.alpha,AOpacity);
              FastBlendPixelInline(pdest,tempPixel);
            end;
            Inc(pdest);
            Inc(psource);
          end;
        end else
          for i := copycount - 1 downto 0 do
          begin
            if psource^.alpha = 255 then
              pdest^ := psource^;
            Inc(pdest);
            Inc(psource);
          end;
    end;
    dmDrawWithTransparency:
    begin
        if AOpacity <> 255 then
        begin
          for i := copycount - 1 downto 0 do
          begin
            DrawPixelInlineWithAlphaCheck(pdest, psource^, AOpacity);
            Inc(pdest);
            Inc(psource);
          end;
        end
        else
          for i := copycount - 1 downto 0 do
          begin
            DrawPixelInlineWithAlphaCheck(pdest, psource^);
            Inc(pdest);
            Inc(psource);
          end;
    end;
    dmFastBlend:
    begin
        if AOpacity <> 255 then
        begin
          for i := copycount - 1 downto 0 do
          begin
            FastBlendPixelInline(pdest, psource^, AOpacity);
            Inc(pdest);
            Inc(psource);
          end;
        end else
          for i := copycount - 1 downto 0 do
          begin
            FastBlendPixelInline(pdest, psource^);
            Inc(pdest);
            Inc(psource);
          end;
    end;
    dmXor:
    begin
      if AOpacity <> 255 then
      begin
          for i := copycount - 1 downto 0 do
          begin
            PLongWord(@tempPixel)^ := PLongWord(pdest)^ xor PLongWord(psource)^;
            FastBlendPixelInline(pdest, tempPixel, AOpacity);
            Inc(pdest);
            Inc(psource);
          end;
      end else
          XorPixels(pdest, psource, copycount);
    end;
  end;
end;

procedure DrawPixelsInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer);
var
  n: integer;
  ec: TExpandedPixel;
begin
  if c.alpha = 0 then exit;
  if c.alpha = 255 then
  begin
    filldword(dest^,count,LongWord(c));
    exit;
  end;
  ec := GammaExpansion(c);
  for n := Count - 1 downto 0 do
  begin
    DrawExpandedPixelInlineNoAlphaCheck(dest, ec,c.alpha);
    Inc(dest);
  end;
end;

procedure DrawExpandedPixelsInline(dest: PBGRAPixel; ec: TExpandedPixel;
  Count: integer);
var
  n: integer;
  c: TBGRAPixel;
begin
  if ec.alpha < $0100 then exit;
  if ec.alpha >= $FF00 then
  begin
    c := GammaCompression(ec);
    filldword(dest^,count,LongWord(c));
    exit;
  end;
  for n := Count - 1 downto 0 do
  begin
    DrawExpandedPixelInlineNoAlphaCheck(dest, ec, ec.alpha shr 8);
    Inc(dest);
  end;
end;

procedure DrawPixelsInlineExpandedOrNot(dest: PBGRAPixel; ec: TExpandedPixel; c: TBGRAPixel; Count: integer);
var
  n: integer;
begin
  if c.alpha = 0 then exit;
  if c.alpha = 255 then
  begin
    filldword(dest^,count,LongWord(c));
    exit;
  end;
  for n := Count - 1 downto 0 do
  begin
    DrawExpandedPixelInlineNoAlphaCheck(dest, ec, c.alpha);
    Inc(dest);
  end;
end;

procedure DrawPixelsInlineDiff(dest: PBGRAPixel; c: TBGRAPixel;
  Count: integer; compare: TBGRAPixel; maxDiff: byte); inline;
var
  n: integer;
begin
  for n := Count - 1 downto 0 do
  begin
    DrawPixelInlineDiff(dest, c, compare, maxDiff);
    Inc(dest);
  end;
end;

procedure DrawPixelInlineWithAlphaCheck(dest: PBGRAPixel; const c: TBGRAPixel);
begin
  case c.alpha of
  0: ;
  255: dest^ := c;
  else
    DrawPixelInlineNoAlphaCheck(dest,c);
  end;
end;

procedure DrawPixelInlineWithAlphaCheck(dest: PBGRAPixel; c: TBGRAPixel; appliedOpacity: byte);
begin
  c.alpha := ApplyOpacity(c.alpha,appliedOpacity);
  DrawPixelInlineWithAlphaCheck(dest, c);
end;

procedure CopyPixelsWithOpacity(dest, src: PBGRAPixel; opacity: byte;
  Count: integer);
begin
  while count > 0 do
  begin
    dest^ := MergeBGRAWithGammaCorrection(src^,opacity,dest^,not opacity);
    inc(src);
    inc(dest);
    dec(count);
  end;
end;

function ApplyOpacity(opacity1, opacity2: byte): byte;
begin
  result := opacity1*(opacity2+1) shr 8;
end;

function FastRoundDiv255(value: LongWord): LongWord; inline;
begin
  result := (value + (value shr 7)) shr 8;
end;

procedure DrawExpandedPixelInlineWithAlphaCheck(dest: PBGRAPixel; const ec: TExpandedPixel);
var
  calpha: byte;
begin
  calpha := ec.alpha shr 8;
  case calpha of
  0: ;
  255: dest^ := GammaCompression(ec);
  else
    DrawExpandedPixelInlineNoAlphaCheck(dest,ec,calpha);
  end;
end;

procedure DrawPixelInlineExpandedOrNotWithAlphaCheck(dest: PBGRAPixel; const ec: TExpandedPixel; c: TBGRAPixel);
begin
  case c.alpha of
  0: ;
  255: dest^ := c;
  else
    DrawExpandedPixelInlineNoAlphaCheck(dest,ec,c.alpha);
  end;
end;

procedure DrawPixelInlineNoAlphaCheck(dest: PBGRAPixel; const c: TBGRAPixel);
var
  a1f, a2f, a12, a12m, alphaCorr: UInt32or64;
begin
  case dest^.alpha of
    0: dest^ := c;
    255:
      begin
        alphaCorr := c.alpha;
        if alphaCorr >= 128 then inc(alphaCorr);
        dest^.red := GammaCompressionTab[(GammaExpansionTab[dest^.red] * UInt32or64(256-alphaCorr) + GammaExpansionTab[c.red]*alphaCorr) shr 8];
        dest^.green := GammaCompressionTab[(GammaExpansionTab[dest^.green] * UInt32or64(256-alphaCorr) + GammaExpansionTab[c.green]*alphaCorr) shr 8];
        dest^.blue := GammaCompressionTab[(GammaExpansionTab[dest^.blue] * UInt32or64(256-alphaCorr) + GammaExpansionTab[c.blue]*alphaCorr) shr 8];
      end;
    else
    begin
      {$HINTS OFF}
      a12  := 65025 - (not dest^.alpha) * (not c.alpha);
      {$HINTS ON}
      a12m := a12 shr 1;

      a1f := dest^.alpha * (not c.alpha);
      a2f := (c.alpha shl 8) - c.alpha;

      PLongWord(dest)^ := ((GammaCompressionTab[(GammaExpansionTab[dest^.red] * a1f +
                         GammaExpansionTab[c.red] * a2f + a12m) div a12]) shl TBGRAPixel_RedShift) or
                       ((GammaCompressionTab[(GammaExpansionTab[dest^.green] * a1f +
                         GammaExpansionTab[c.green] * a2f + a12m) div a12]) shl TBGRAPixel_GreenShift) or
                       ((GammaCompressionTab[(GammaExpansionTab[dest^.blue] * a1f +
                         GammaExpansionTab[c.blue] * a2f + a12m) div a12]) shl TBGRAPixel_BlueShift) or
                       (((a12 + a12 shr 7) shr 8) shl TBGRAPixel_AlphaShift);
    end;
  end;
end;

procedure DrawExpandedPixelInlineNoAlphaCheck(dest: PBGRAPixel;
  const ec: TExpandedPixel; calpha: byte);
var
  a1f, a2f, a12, a12m, alphaCorr: UInt32or64;
begin
  case dest^.alpha of
    0: begin
         dest^.red := GammaCompressionTab[ec.red];
         dest^.green := GammaCompressionTab[ec.green];
         dest^.blue := GammaCompressionTab[ec.blue];
         dest^.alpha := calpha;
      end;
    255:
      begin
        alphaCorr := calpha;
        if alphaCorr >= 128 then inc(alphaCorr);
        dest^.red := GammaCompressionTab[(GammaExpansionTab[dest^.red] * UInt32or64(256-alphaCorr) + ec.red*alphaCorr) shr 8];
        dest^.green := GammaCompressionTab[(GammaExpansionTab[dest^.green] * UInt32or64(256-alphaCorr) + ec.green*alphaCorr) shr 8];
        dest^.blue := GammaCompressionTab[(GammaExpansionTab[dest^.blue] * UInt32or64(256-alphaCorr) + ec.blue*alphaCorr) shr 8];
      end;
    else
    begin
      {$HINTS OFF}
      a12  := 65025 - (not dest^.alpha) * (not calpha);
      {$HINTS ON}
      a12m := a12 shr 1;

      a1f := dest^.alpha * (not calpha);
      a2f := (calpha shl 8) - calpha;

      PLongWord(dest)^ := ((GammaCompressionTab[(GammaExpansionTab[dest^.red] * a1f +
                         ec.red * a2f + a12m) div a12]) shl TBGRAPixel_RedShift) or
                       ((GammaCompressionTab[(GammaExpansionTab[dest^.green] * a1f +
                         ec.green * a2f + a12m) div a12]) shl TBGRAPixel_GreenShift) or
                       ((GammaCompressionTab[(GammaExpansionTab[dest^.blue] * a1f +
                         ec.blue * a2f + a12m) div a12]) shl TBGRAPixel_BlueShift) or
                       (((a12 + a12 shr 7) shr 8) shl TBGRAPixel_AlphaShift);
    end;
  end;
end;

procedure FastBlendPixelInline(dest: PBGRAPixel; const c: TBGRAPixel);
var
  a1f, a2f, a12, a12m, alphaCorr: UInt32or64;
begin
  case c.alpha of
    0: ;
    255: dest^ := c;
    else
    begin
      case dest^.alpha of
        0: dest^ := c;
        255:
        begin
          alphaCorr := c.alpha;
          if alphaCorr >= 128 then inc(alphaCorr);
          dest^.red := (dest^.red * UInt32or64(256-alphaCorr) + c.red*(alphaCorr+1)) shr 8;
          dest^.green := (dest^.green * UInt32or64(256-alphaCorr) + c.green*(alphaCorr+1)) shr 8;
          dest^.blue := (dest^.blue * UInt32or64(256-alphaCorr) + c.blue*(alphaCorr+1)) shr 8;
        end;
        else
        begin
          {$HINTS OFF}
          a12  := 65025 - (not dest^.alpha) * (not c.alpha);
          {$HINTS ON}
          a12m := a12 shr 1;

          a1f := dest^.alpha * (not c.alpha);
          a2f := (c.alpha shl 8) - c.alpha;

          PLongWord(dest)^ := (((dest^.red * a1f + c.red * a2f + a12m) div a12) shl TBGRAPixel_RedShift) or
                           (((dest^.green * a1f + c.green * a2f + a12m) div a12) shl TBGRAPixel_GreenShift) or
                           (((dest^.blue * a1f + c.blue * a2f + a12m) div a12) shl TBGRAPixel_BlueShift) or
                           (((a12 + a12 shr 7) shr 8) shl TBGRAPixel_AlphaShift);
        end;
      end;
    end;
  end;
end;

procedure FastBlendPixelInline(dest: PBGRAPixel; c: TBGRAPixel;
  appliedOpacity: byte);
begin
  c.alpha := ApplyOpacity(c.alpha,appliedOpacity);
  FastBlendPixelInline(dest,c);
end;

procedure DrawPixelInlineDiff(dest: PBGRAPixel; c, compare: TBGRAPixel;
  maxDiff: byte); inline;
var alpha: Int32or64;
begin
  alpha := (c.alpha * (maxDiff + 1 - BGRADiff(dest^, compare)) + (maxDiff + 1) shr 1) div
    (maxDiff + 1);
  if alpha > 0 then
    DrawPixelInlineWithAlphaCheck(dest, BGRA(c.red, c.green, c.blue, alpha));
end;

procedure ErasePixelInline(dest: PBGRAPixel; alpha: byte); inline;
var
  newAlpha: byte;
begin
  newAlpha := ApplyOpacity(dest^.alpha, not alpha);
  if newAlpha = 0 then
    dest^ := BGRAPixelTransparent
  else
    dest^.alpha := newAlpha;
end;

{$i blendpixelsover.inc}

{$i blendpixelinline.inc}

end.

