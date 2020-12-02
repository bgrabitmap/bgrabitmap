// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAGrayscaleMask;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, BGRAGraphics, SysUtils, BGRABitmapTypes, BGRAResample, {%H-}UniversalDrawer;

type
  { TGrayscaleMask }

  TGrayscaleMask = class(specialize TGenericUniversalBitmap<TByteMask,TByteMaskColorspace>)
  private
     function GetScanLine(Y: Integer): PByte; inline;
  protected
     function InternalNew: TCustomUniversalBitmap; override;
     procedure AssignTransparentPixel(out ADest); override;
     function InternalGetPixelCycle256(ix,iy: int32or64; iFactX,iFactY: int32or64): TByteMask;
     function InternalGetPixel256(ix,iy: int32or64; iFactX,iFactY: int32or64; smoothBorder: boolean): TByteMask;
     procedure Init; override;
  public
     ScanInterpolationFilter: TResampleFilter;

     constructor Create(AWidth,AHeight: Integer; AValue: byte); overload;
     constructor Create(ABitmap: TBGRACustomBitmap; AChannel: TChannel); overload;
     constructor CreateDownSample(ABitmap: TBGRACustomBitmap; AWidth,AHeight: integer);
     constructor CreateDownSample(ABitmap: TGrayscaleMask; AWidth,AHeight: integer);
     constructor CreateDownSample(ABitmap: TBGRACustomBitmap; AWidth,AHeight: integer; ASourceRect: TRect);
     constructor CreateDownSample(ABitmap: TGrayscaleMask; AWidth,AHeight: integer; ASourceRect: TRect);
     procedure CopyFrom(ABitmap: TGrayscaleMask); overload;
     procedure CopyFrom(ABitmap: TBGRACustomBitmap; AChannel: TChannel); overload;
     procedure CopyPropertiesTo(ABitmap: TCustomUniversalBitmap); override;
     function GetImageBounds: TRect; overload; override;
     function GetImageBoundsWithin(const ARect: TRect; Channel: TChannel = cAlpha; ANothingValue: Byte = 0): TRect; overload; override;
     function GetImageBoundsWithin(const ARect: TRect; Channels: TChannels; ANothingValue: Byte = 0): TRect; overload; override;

     class procedure SolidBrush(out ABrush: TUniversalBrush; const AColor: TByteMask; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
     class procedure ScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode = dmDrawWithTransparency;
                                  AOffsetX: integer = 0; AOffsetY: integer = 0); override;
     class procedure MaskBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner;
                               AOffsetX: integer = 0; AOffsetY: integer = 0); override;
     class procedure EraseBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
     class procedure AlphaBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;

     procedure Draw(ABitmap: TBGRACustomBitmap; X,Y: Integer; AGammaCorrection: boolean = false);
     procedure DrawAsAlpha(ABitmap: TBGRACustomBitmap; X,Y: Integer; const c: TBGRAPixel); overload;
     procedure DrawAsAlpha(ABitmap: TBGRACustomBitmap; X,Y: Integer; texture: IBGRAScanner); overload;
     function GetPixel(X,Y: integer): byte; overload;
     procedure SetPixel(X,Y: integer; AValue: byte);
     property ScanLine[Y: Integer]: PByte read GetScanLine;
     property Data: PByte read FDataByte;

     function GetPixel(x, y: single; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TByteMask; overload;
     function GetPixel256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TByteMask;

     procedure ScanNextMaskChunk(var ACount: integer; out AMask: PByteMask; out AStride: integer); override;
     function ScanAtIntegerMask(X,Y: integer): TByteMask; override;
     function ScanAtMask(X,Y: Single): TByteMask; override;
     function ScanAtInteger(X, Y: integer): TBGRAPixel; override;
     function ScanAt(X, Y: Single): TBGRAPixel; override;

     {inplace filters}
     procedure Negative;
     procedure NegativeRect(ABounds: TRect);
     procedure InplaceNormalize; overload;
     procedure InplaceNormalize(ABounds: TRect); overload;

     //return type helpers
     function NewBitmap: TGrayscaleMask; overload; override;
     function NewBitmap(AWidth, AHeight: integer): TGrayscaleMask; overload; override;
     function NewBitmap(AWidth, AHeight: integer; const Color: TByteMask): TGrayscaleMask; overload; override;
     function NewBitmap(AWidth, AHeight: integer; AColor: Pointer): TGrayscaleMask; overload; override;
     function NewReference: TGrayscaleMask; override;
     function GetUnique: TGrayscaleMask; override;
     function Duplicate(DuplicateProperties: Boolean = False): TGrayscaleMask; overload; override;
     function GetPart(const ARect: TRect): TGrayscaleMask; override;
     function CreateBrushTexture(ABrushStyle: TBrushStyle; APatternColor, ABackgroundColor: TByteMask;
                 AWidth: integer = 8; AHeight: integer = 8; APenWidth: single = 1): TGrayscaleMask; override;
     function RotateCW: TGrayscaleMask; override;
     function RotateCCW: TGrayscaleMask; override;
     function RotateUD: TGrayscaleMask; override;
     function FilterContour(ABorderValue: byte = 0): TGrayscaleMask;
     function FilterBlurRadial(radius: single; blurType: TRadialBlurType): TGrayscaleMask; overload; override;
     function FilterBlurRadial(const ABounds: TRect; radius: single; blurType: TRadialBlurType): TGrayscaleMask; overload; override;
     function FilterBlurRadial(radiusX, radiusY: single; blurType: TRadialBlurType): TGrayscaleMask; overload; override;
     function FilterBlurRadial(const ABounds: TRect; radiusX, radiusY: single; blurType: TRadialBlurType): TGrayscaleMask; overload; override;
     function FilterBlurMotion(distance: single; angle: single; oriented: boolean): TGrayscaleMask; overload; override;
     function FilterBlurMotion(const ABounds: TRect; distance: single; angle: single; oriented: boolean): TGrayscaleMask; overload; override;
     function FilterCustomBlur(mask: TCustomUniversalBitmap): TGrayscaleMask; overload; override;
     function FilterCustomBlur(const ABounds: TRect; mask: TCustomUniversalBitmap): TGrayscaleMask; overload; override;
     function FilterSphere: TGrayscaleMask;
     function FilterCylinder: TGrayscaleMask;
  end;

procedure DownSamplePutImageGrayscale(sourceData: PByte; sourcePixelSize: Int32or64; sourceRowDelta: Int32or64; sourceWidth, sourceHeight: Int32or64; dest: TGrayscaleMask; ADestRect: TRect); overload;
procedure DownSamplePutImageGrayscale(source: TBGRACustomBitmap; dest: TGrayscaleMask; ADestRect: TRect); overload;
procedure DownSamplePutImageGrayscale(source: TGrayscaleMask; dest: TGrayscaleMask; ADestRect: TRect); overload;
procedure DownSamplePutImageGrayscale(source: TBGRACustomBitmap; dest: TGrayscaleMask; ADestRect: TRect; ASourceRect: TRect); overload;
procedure DownSamplePutImageGrayscale(source: TGrayscaleMask; dest: TGrayscaleMask; ADestRect: TRect; ASourceRect: TRect); overload;

procedure BGRAFillClearTypeGrayscaleMask(dest: TBGRACustomBitmap; x,
  y: integer; xThird: integer; mask: TGrayscaleMask; color: TBGRAPixel;
  texture: IBGRAScanner; RGBOrder: boolean);

const
  ByteMaskBlack : TByteMask = (gray:0);
  ByteMaskWhite : TByteMask = (gray:255);

operator = (const c1, c2: TByteMask): boolean; inline;

implementation

uses BGRABlend, BGRATransform;

operator = (const c1, c2: TByteMask): boolean;
begin
  result := c1.gray = c2.gray;
end;

procedure BGRAFillClearTypeGrayscaleMask(dest: TBGRACustomBitmap; x,
  y: integer; xThird: integer; mask: TGrayscaleMask; color: TBGRAPixel;
  texture: IBGRAScanner; RGBOrder: boolean);
var delta: Int32or64;
begin
  delta := mask.Width;
  BGRABlend.BGRAFillClearTypeMaskPtr(dest,x,y,xThird,mask.ScanLineByte[0],1,delta,mask.Width,mask.Height,color,texture,RGBOrder);
end;

procedure ByteMaskSolidBrushSkipPixels({%H-}AFixedData: Pointer;
    AContextData: PUniBrushContext; {%H-}AAlpha: Word; ACount: integer);
begin
  inc(PByteMask(AContextData^.Dest), ACount);
end;

procedure ByteMaskChunkSetPixels(
    ASource: PByteMask; ADest: PByteMask;
    AAlpha: Word; ACount: integer; ASourceStride: integer); inline;
var
  alphaOver: UInt32or64;
begin
  if AAlpha=0 then exit;
  if AAlpha=65535 then
  begin
    if ASourceStride = 1 then
    begin
      move(ASource^, ADest^, ACount);
      inc(ASource, ACount);
    end else
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
      ADest^.gray := (ADest^.gray*UInt32or64(65536-alphaOver) + ASource^.gray*alphaOver + 32768) shr 16;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end;
end;

procedure ByteMaskChunkXorPixels(
    ASource: PByteMask; ADest: PByteMask;
    AAlpha: Word; ACount: integer; ASourceStride: integer); inline;
var
  alphaOver: UInt32or64;
  temp: Byte;
begin
  if AAlpha=0 then exit;
  if AAlpha=65535 then
  begin
    if ASourceStride = 1 then
    begin
      move(ASource^, ADest^, ACount);
      inc(ASource, ACount);
    end else
      while ACount > 0 do
      begin
        ADest^.gray := ADest^.gray xor ASource^.gray;
        inc(ADest);
        dec(ACount);
        inc(PByte(ASource), ASourceStride);
      end;
  end else
  begin
    if AAlpha > 32768 then alphaOver := AAlpha+1 else alphaOver := AAlpha;
    while ACount > 0 do
    begin
      temp := ADest^.gray xor ASource^.gray;
      ADest^.gray := (ADest^.gray*UInt32or64(65536-alphaOver) + temp*alphaOver + 32768) shr 16;
      inc(ADest);
      dec(ACount);
      inc(PByte(ASource), ASourceStride);
    end;
  end;
end;

procedure ByteMaskSolidBrushSetPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PByteMask;
begin
  pDest := PByteMask(AContextData^.Dest);
  ByteMaskChunkSetPixels( PByteMask(AFixedData), pDest, AAlpha, ACount, 0);
  inc(pDest, ACount);
  AContextData^.Dest := pDest;
end;

procedure ByteMaskSolidBrushXorPixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PByteMask;
begin
  pDest := PByteMask(AContextData^.Dest);
  ByteMaskChunkXorPixels( PByteMask(AFixedData), pDest, AAlpha, ACount, 0);
  inc(pDest, ACount);
  AContextData^.Dest := pDest;
end;

type
  PByteMaskScannerBrushFixedData = ^TByteMaskScannerBrushFixedData;
  TByteMaskScannerBrushFixedData = record
    Scanner: Pointer; //avoid ref count by using pointer type
    OffsetX, OffsetY: integer;
    Conversion: TBridgedConversion;
  end;

procedure ByteMaskScannerBrushInitContext(AFixedData: Pointer;
  AContextData: PUniBrushContext);
begin
  with PByteMaskScannerBrushFixedData(AFixedData)^ do
    IBGRAScanner(Scanner).ScanMoveTo(AContextData^.Ofs.X + OffsetX,
                                     AContextData^.Ofs.Y + OffsetY);
end;

procedure ByteMaskScannerConvertBrushSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PByteMask;
  qty, pixSize: Integer;
  buf: packed array[0..31] of TByteMask;
begin
  with PByteMaskScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PByteMask(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PByteMask(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TByteMask), nil);
      ByteMaskChunkSetPixels(@buf, pDest, AAlpha, qty, sizeof(TByteMask) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure ByteMaskScannerConvertBrushXorPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PByteMask;
  qty, pixSize: Integer;
  buf: packed array[0..31] of TByteMask;
begin
  with PByteMaskScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PByteMask(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PByteMask(AContextData^.Dest);
    pixSize := IBGRAScanner(Scanner).GetScanCustomColorspace.GetSize;
    while ACount > 0 do
    begin
      if ACount > length(buf) then qty := length(buf) else qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      Conversion.Convert(psrc, @buf, qty, pixSize, sizeof(TByteMask), nil);
      ByteMaskChunkXorPixels(@buf, pDest, AAlpha, qty, sizeof(TByteMask) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure ByteMaskScannerChunkBrushSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PByteMask;
  qty: Integer;
begin
  with PByteMaskScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PByteMask(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PByteMask(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      ByteMaskChunkSetPixels(PByteMask(psrc), pDest, AAlpha, qty, sizeof(TByteMask) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure ByteMaskScannerChunkBrushXorPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  psrc: Pointer;
  pDest: PByteMask;
  qty: Integer;
begin
  with PByteMaskScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PByteMask(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PByteMask(AContextData^.Dest);
    while ACount > 0 do
    begin
      qty := ACount;
      IBGRAScanner(Scanner).ScanNextCustomChunk(qty, psrc);
      ByteMaskChunkXorPixels(PByteMask(psrc), pDest, AAlpha, qty, sizeof(TByteMask) );
      inc(pDest, qty);
      dec(ACount, qty);
    end;
    AContextData^.Dest := pDest;
  end;
end;

procedure ByteMaskMaskBrushApply(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PByteMask;
  qty, maskStride: Integer;
  pMask: PByteMask;
  factor: UInt32or64;
begin
  with PByteMaskScannerBrushFixedData(AFixedData)^ do
  begin
    if AAlpha = 0 then
    begin
      inc(PByteMask(AContextData^.Dest), ACount);
      IBGRAScanner(Scanner).ScanSkipPixels(ACount);
      exit;
    end;
    pDest := PByteMask(AContextData^.Dest);
    if AAlpha = 65535 then
    begin
      while ACount > 0 do
      begin
        qty := ACount;
        IBGRAScanner(Scanner).ScanNextMaskChunk(qty, pMask, maskStride);
        dec(ACount,qty);
        while qty > 0 do
        begin
          pDest^.gray := ApplyOpacity(pDest^.gray, pMask^.gray);
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
          pDest^.gray := (pDest^.gray*((factor*pMask^.gray+128) shr 8)) shr 16;
          inc(pDest);
          inc(pMask, maskStride);
          dec(qty);
        end;
      end;
    end;
    PByteMask(AContextData^.Dest) := pDest;
  end;
end;

procedure ByteMaskBrushErasePixels(AFixedData: Pointer;
    AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PByteMask;
  alphaMul,eraseMul: UInt32or64;
begin
  pDest := PByteMask(AContextData^.Dest);
  if AAlpha>=32768 then alphaMul := AAlpha+1 else alphaMul := AAlpha;
  eraseMul := PWord(AFixedData)^;
  if eraseMul>=32768 then inc(eraseMul);
  eraseMul := 65536 - (eraseMul*alphaMul shr 16);
  while ACount > 0 do
  begin
    pDest^.gray:= pDest^.gray*eraseMul shr 16;
    dec(ACount);
    inc(pDest);
  end;
  AContextData^.Dest := pDest;
end;

{ TGrayscaleMask }

function TGrayscaleMask.InternalNew: TCustomUniversalBitmap;
begin
  Result:= TGrayscaleMask.Create;
end;

procedure TGrayscaleMask.AssignTransparentPixel(out ADest);
begin
  TByteMask(ADest).gray := 0;
end;

function TGrayscaleMask.InternalGetPixelCycle256(ix, iy: int32or64; iFactX,
  iFactY: int32or64): TByteMask;
var
  ixMod2: int32or64;
  pUpLeft, pUpRight, pDownLeft, pDownRight: PByteMask;
  scan: PByteMask;
begin
  scan := GetScanlineFast(iy);

  pUpLeft := (scan + ix);
  ixMod2 := ix+1;
  if ixMod2=Width then ixMod2 := 0;
  pUpRight := (scan + ixMod2);

  Inc(iy);
  if iy = Height then iy := 0;
  scan := GetScanlineFast(iy);
  pDownLeft := (scan + ix);
  pDownRight := (scan + ixMod2);

  InterpolateBilinearMask(pUpLeft, pUpRight, pDownLeft,
          pDownRight, iFactX, iFactY, @result);
end;

function TGrayscaleMask.InternalGetPixel256(ix, iy: int32or64; iFactX,
  iFactY: int32or64; smoothBorder: boolean): TByteMask;
var
  pUpLeft, pUpRight, pDownLeft, pDownRight: PByteMask;
  scan: PByteMask;
begin
  if (iy >= 0) and (iy < FHeight) then
  begin
    scan := GetScanlineFast(iy);

    if (ix >= 0) and (ix < FWidth) then
      pUpLeft := scan+ix
    else if smoothBorder then
      pUpLeft := @ByteMaskBlack
    else
      pUpLeft := nil;

    if (ix+1 >= 0) and (ix+1 < FWidth) then
      pUpRight := scan+(ix+1)
    else if smoothBorder then
      pUpRight := @ByteMaskBlack
    else
      pUpRight := nil;
  end else
  if smoothBorder then
  begin
    pUpLeft := @ByteMaskBlack;
    pUpRight := @ByteMaskBlack;
  end else
  begin
    pUpLeft := nil;
    pUpRight := nil;
  end;

  if (iy+1 >= 0) and (iy+1 < FHeight) then
  begin
    scan := GetScanlineFast(iy+1);

    if (ix >= 0) and (ix < FWidth) then
      pDownLeft := scan+ix
    else if smoothBorder then
      pDownLeft := @ByteMaskBlack
    else
      pDownLeft := nil;

    if (ix+1 >= 0) and (ix+1 < FWidth) then
      pDownRight := scan+(ix+1)
    else if smoothBorder then
      pDownRight := @ByteMaskBlack
    else
      pDownRight := nil;
  end else
  if smoothBorder then
  begin
    pDownLeft := @ByteMaskBlack;
    pDownRight := @ByteMaskBlack;
  end else
  begin
    pDownLeft := nil;
    pDownRight := nil;
  end;

  InterpolateBilinearMask(pUpLeft, pUpRight, pDownLeft,
          pDownRight, iFactX, iFactY, @result);
end;

procedure TGrayscaleMask.Init;
begin
  inherited Init;
  ScanInterpolationFilter := rfLinear;
end;

function TGrayscaleMask.GetScanLine(Y: Integer): PByte;
begin
  result := PByte(GetScanLineByte(y));
end;

procedure TGrayscaleMask.CopyFrom(ABitmap: TBGRACustomBitmap; AChannel: TChannel);
var psrc: PByte;
  pdest: PByte;
  x,y: integer;
  ofs: Int32or64;
begin
  SetSize(ABitmap.Width, ABitmap.Height);
  if NbPixels > 0 then
  begin
    pdest := DataByte;
    ofs := TBGRAPixel_ChannelByteOffset[AChannel];
    for y := 0 to FHeight-1 do
    begin
      psrc := PByte(ABitmap.ScanLine[y])+ofs;
      for x := FWidth-1 downto 0 do
      begin
        pdest^ := psrc^;
        inc(pdest);
        inc(psrc,sizeof(TBGRAPixel));
      end;
    end;
  end;
end;

procedure TGrayscaleMask.CopyPropertiesTo(ABitmap: TCustomUniversalBitmap);
begin
  inherited CopyPropertiesTo(ABitmap);
  if ABitmap is TGrayscaleMask then
  begin
    TGrayscaleMask(ABitmap).ScanInterpolationFilter:= self.ScanInterpolationFilter;
  end;
end;

function TGrayscaleMask.GetImageBounds: TRect;
begin
  Result:= GetImageBounds(cGreen);
end;

function TGrayscaleMask.GetImageBoundsWithin(const ARect: TRect;
  Channel: TChannel; ANothingValue: Byte): TRect;
var
  minx, miny, maxx, maxy: integer;
  xb, xb2, yb: integer;
  p: PByte;
  actualRect: TRect;
begin
  if Channel = cAlpha then raise exception.Create('Channel not found');
  actualRect := TRect.Intersect(ARect,rect(0,0,self.Width,self.Height));
  maxx := actualRect.Left-1;
  maxy := actualRect.Top-1;
  minx := actualRect.Right;
  miny := actualRect.Bottom;
  for yb := actualRect.Top to actualRect.Bottom-1 do
  begin
    p := GetPixelAddress(actualRect.Left,yb);
    for xb := actualRect.Left to actualRect.Right - 1 do
    begin
      if p^<>ANothingValue then
      begin
        if xb < minx then minx := xb;
        if yb < miny then miny := yb;
        if xb > maxx then maxx := xb;
        if yb > maxy then maxy := yb;

        inc(p, actualRect.Right-1-xb);
        for xb2 := actualRect.Right-1 downto xb+1 do
        begin
          if p^ <> ANothingValue then
          begin
            if xb2 > maxx then maxx := xb2;
            break;
          end;
          dec(p);
        end;
        break;
      end;
      Inc(p);
    end;
  end;
  if minx > maxx then
  begin
    Result.left   := 0;
    Result.top    := 0;
    Result.right  := 0;
    Result.bottom := 0;
  end
  else
  begin
    Result.left   := minx;
    Result.top    := miny;
    Result.right  := maxx + 1;
    Result.bottom := maxy + 1;
  end;
end;

function TGrayscaleMask.GetImageBoundsWithin(const ARect: TRect;
  Channels: TChannels; ANothingValue: Byte): TRect;
begin
  if cAlpha in Channels then raise exception.Create('Channel not found')
  else if Channels = [] then result := EmptyRect
  else result := GetImageBoundsWithin(ARect, cGreen, ANothingValue);
end;

class procedure TGrayscaleMask.SolidBrush(out ABrush: TUniversalBrush;
  const AColor: TByteMask; ADrawMode: TDrawMode);
begin
  ABrush.Colorspace := TByteMaskColorspace;
  PByteMask(@ABrush.FixedData)^ := AColor;
  if ADrawMode <> dmXor then
    ABrush.InternalPutNextPixels:= @ByteMaskSolidBrushSetPixels
  else
    ABrush.InternalPutNextPixels:= @ByteMaskSolidBrushXorPixels;
end;

class procedure TGrayscaleMask.ScannerBrush(out ABrush: TUniversalBrush;
  AScanner: IBGRAScanner; ADrawMode: TDrawMode; AOffsetX: integer;
  AOffsetY: integer);
var
  sourceSpace: TColorspaceAny;
begin
  ABrush.Colorspace:= TByteMaskColorspace;
  with PByteMaskScannerBrushFixedData(@ABrush.FixedData)^ do
  begin
    Scanner := Pointer(AScanner);
    OffsetX := AOffsetX;
    OffsetY := AOffsetY;
  end;
  ABrush.InternalInitContext:= @ByteMaskScannerBrushInitContext;
  sourceSpace := AScanner.GetScanCustomColorspace;
  if sourceSpace = TByteMaskColorspace then
  begin
    if ADrawMode <> dmXor then
      ABrush.InternalPutNextPixels:= @ByteMaskScannerChunkBrushSetPixels
    else
      ABrush.InternalPutNextPixels:= @ByteMaskScannerChunkBrushXorPixels;
  end else
  begin
    with PByteMaskScannerBrushFixedData(@ABrush.FixedData)^ do
      Conversion := sourceSpace.GetBridgedConversion(TByteMaskColorspace);
    if ADrawMode <> dmXor then
      ABrush.InternalPutNextPixels:= @ByteMaskScannerConvertBrushSetPixels
    else
      ABrush.InternalPutNextPixels:= @ByteMaskScannerConvertBrushXorPixels;
  end;
end;

class procedure TGrayscaleMask.MaskBrush(out ABrush: TUniversalBrush;
  AScanner: IBGRAScanner; AOffsetX: integer; AOffsetY: integer);
begin
  ABrush.Colorspace:= TByteMaskColorspace;
  with PByteMaskScannerBrushFixedData(@ABrush.FixedData)^ do
  begin
    Scanner := Pointer(AScanner);
    OffsetX := AOffsetX;
    OffsetY := AOffsetY;
  end;
  ABrush.InternalInitContext:= @ByteMaskScannerBrushInitContext;
  ABrush.InternalPutNextPixels:= @ByteMaskMaskBrushApply;
end;

class procedure TGrayscaleMask.EraseBrush(out ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  ABrush.Colorspace := TByteMaskColorspace;
  PWord(@ABrush.FixedData)^ := AAlpha;
  ABrush.InternalPutNextPixels:= @ByteMaskBrushErasePixels;
end;

class procedure TGrayscaleMask.AlphaBrush(out ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  ABrush.Colorspace := TByteMaskColorspace;
  PWord(@ABrush.FixedData)^ := not AAlpha;
  ABrush.InternalPutNextPixels:= @ByteMaskBrushErasePixels;
end;

constructor TGrayscaleMask.Create(AWidth, AHeight: Integer; AValue: byte);
begin
  inherited Create(AWidth, AHeight, TByteMask.New(AValue));
end;

constructor TGrayscaleMask.Create(ABitmap: TBGRACustomBitmap; AChannel: TChannel);
begin
  inherited Create(0,0);
  CopyFrom(ABitmap, AChannel);
end;

constructor TGrayscaleMask.CreateDownSample(ABitmap: TBGRACustomBitmap; AWidth,
  AHeight: integer);
begin
  CreateDownSample(ABitmap, AWidth, AHeight, rect(0,0,ABitmap.Width,ABitmap.Height));
end;

constructor TGrayscaleMask.CreateDownSample(ABitmap: TGrayscaleMask; AWidth,
  AHeight: integer);
begin
  CreateDownSample(ABitmap, AWidth, AHeight, rect(0,0,ABitmap.Width,ABitmap.Height));
end;

constructor TGrayscaleMask.CreateDownSample(ABitmap: TBGRACustomBitmap; AWidth,
  AHeight: integer; ASourceRect: TRect);
begin
  inherited Create(0,0);
  if (AWidth = ABitmap.Width) and (AHeight = ABitmap.Height) then
    CopyFrom(ABitmap,cGreen)
  else
  begin
    if (ABitmap.Width < AWidth) or (ABitmap.Height < AHeight) then
      raise exception.Create('Original size smaller');
    SetSize(AWidth,AHeight);
    if NbPixels > 0 then
      DownSamplePutImageGrayscale(ABitmap, self, rect(0,0,FWidth,FHeight), ASourceRect);
  end;
end;

constructor TGrayscaleMask.CreateDownSample(ABitmap: TGrayscaleMask; AWidth,
  AHeight: integer; ASourceRect: TRect);
begin
  inherited Create(0,0);
  if (AWidth = ABitmap.Width) and (AHeight = ABitmap.Height) then
    CopyFrom(ABitmap)
  else
  begin
    if (ABitmap.Width < AWidth) or (ABitmap.Height < AHeight) then
      raise exception.Create('Original size smaller');
    SetSize(AWidth,AHeight);
    if NbPixels > 0 then
      DownSamplePutImageGrayscale(ABitmap, self, rect(0,0,FWidth,FHeight), ASourceRect);
  end;
end;

procedure TGrayscaleMask.CopyFrom(ABitmap: TGrayscaleMask);
begin
  SetSize(ABitmap.Width, ABitmap.Height);
  if NbPixels > 0 then
    move(ABitmap.Data^, Data^, NbPixels);
end;

procedure TGrayscaleMask.Draw(ABitmap: TBGRACustomBitmap; X, Y: Integer; AGammaCorrection: boolean = false);
var
  yb, minxb, minyb, maxxb, maxyb, ignoreleft, copycount,
  i, delta_source, delta_dest: integer;
  pdest: PBGRAPixel;
  psource: PByte;
  value: byte;
begin
  if not CheckPutImageBounds(x,y,FWidth,Fheight,minxb,minyb,maxxb,maxyb,ignoreleft,ABitmap.ClipRect) then exit;
  copycount := maxxb - minxb + 1;

  psource := ScanLineByte[minyb - y] + ignoreleft;
  delta_source := FWidth;

  pdest := ABitmap.Scanline[minyb] + minxb;
  if ABitmap.LineOrder = riloBottomToTop then
    delta_dest := -ABitmap.Width
  else
    delta_dest := ABitmap.Width;

  Dec(delta_source, copycount);
  Dec(delta_dest, copycount);
  for yb := minyb to maxyb do
  begin
    if AGammaCorrection then
    begin
      for i := copycount -1 downto 0 do
      begin
        value := GammaCompressionTab[psource^ + (psource^ shl 8)];
        pdest^ := BGRA(value,value,value,255);
        inc(psource);
        inc(pdest);
      end;
    end else
    begin
      for i := copycount -1 downto 0 do
      begin
        value := psource^;
        pdest^ := BGRA(value,value,value,255);
        inc(psource);
        inc(pdest);
      end;
    end;
    Inc(psource, delta_source);
    Inc(pdest, delta_dest);
  end;
  ABitmap.InvalidateBitmap;
end;

procedure TGrayscaleMask.DrawAsAlpha(ABitmap: TBGRACustomBitmap; X, Y: Integer; const c: TBGRAPixel);
begin
  ABitmap.FillMask(x,y, self, c, dmDrawWithTransparency);
end;

procedure TGrayscaleMask.DrawAsAlpha(ABitmap: TBGRACustomBitmap; X, Y: Integer; texture: IBGRAScanner);
begin
  ABitmap.FillMask(x,y, self, texture, dmDrawWithTransparency);
end;

function TGrayscaleMask.GetPixel(X, Y: integer): byte;
begin
  if (x < 0) or (x >= FWidth) then
    raise ERangeError.Create('GetPixel: out of bounds');
  result := (ScanLineByte[Y]+X)^;
end;

procedure TGrayscaleMask.SetPixel(X, Y: integer; AValue: byte);
begin
  if (x < 0) or (x >= FWidth) then
    raise ERangeError.Create('SetPixel: out of bounds');
  (ScanLineByte[Y]+X)^ := AValue;
end;

function TGrayscaleMask.GetPixel(x, y: single;
  AResampleFilter: TResampleFilter; smoothBorder: boolean): TByteMask;
var
  ix, iy: Int32or64;
  iFactX,iFactY: Int32or64;
begin
  ix := round(x*256);
  if (ix<= -256) or (ix>=Width shl 8) then
  begin
    result := ByteMaskBlack;
    exit;
  end;
  iy := round(y*256);
  if (iy<= -256) or (iy>=Height shl 8) then
  begin
    result := ByteMaskBlack;
    exit;
  end;

  iFactX := ix and 255; //distance from integer coordinate
  iFactY := iy and 255;
  if ix<0 then ix := -1 else ix := ix shr 8;
  if iy<0 then iy := -1 else iy := iy shr 8;

  //if the coordinate is integer, then call standard GetPixel function
  if (iFactX = 0) and (iFactY = 0) then
  begin
    Result := (GetScanlineFast(iy)+ix)^;
    exit;
  end;

  result := InternalGetPixel256(ix,iy, FineInterpolation256(iFactX, AResampleFilter),
              FineInterpolation256(iFactY, AResampleFilter), smoothBorder);
end;

function TGrayscaleMask.GetPixel256(x, y, fracX256, fracY256: int32or64;
  AResampleFilter: TResampleFilter; smoothBorder: boolean): TByteMask;
begin
  if (fracX256 = 0) and (fracY256 = 0) then
    result := GetPixel(x,y)
  else if AResampleFilter = rfBox then
  begin
    if fracX256 >= 128 then inc(x);
    if fracY256 >= 128 then inc(y);
    result := GetPixel(x,y);
  end else
    result := InternalGetPixel256(x,y, FineInterpolation256(fracX256,AResampleFilter),
                FineInterpolation256(fracY256,AResampleFilter), smoothBorder);
end;

procedure TGrayscaleMask.ScanNextMaskChunk(var ACount: integer; out
  AMask: PByteMask; out AStride: integer);
var
  pPixels: Pointer;
begin
  ScanNextCustomChunk(ACount, pPixels);
  AMask := PByteMask(pPixels);
  AStride := sizeof(TByteMask);
end;

function TGrayscaleMask.ScanAtIntegerMask(X, Y: integer): TByteMask;
begin
  if (FScanWidth <> 0) and (FScanHeight <> 0) then
    result := GetPixelAddress(PositiveMod(X+ScanOffset.X, FScanWidth),
                             PositiveMod(Y+ScanOffset.Y, FScanHeight))^
  else
    result := ByteMaskBlack;
end;

function TGrayscaleMask.ScanAtMask(X, Y: Single): TByteMask;
var
  ix, iy: Int32or64;
  iFactX,iFactY: Int32or64;
begin
  if (FScanWidth = 0) or (FScanHeight = 0) then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  LoadFromBitmapIfNeeded;
  ix := round(x*256);
  iy := round(y*256);
  if ScanInterpolationFilter = rfBox then
  begin
    ix := PositiveMod((ix+128)+(ScanOffset.X shl 8), FScanWidth shl 8) shr 8;
    iy := PositiveMod((iy+128)+(ScanOffset.Y shl 8), FScanHeight shl 8) shr 8;
    result := (GetScanlineFast(iy)+ix)^;
    exit;
  end;
  iFactX := ix and 255;
  iFactY := iy and 255;
  ix := PositiveMod(ix+(ScanOffset.X shl 8), FScanWidth shl 8) shr 8;
  iy := PositiveMod(iy+(ScanOffset.Y shl 8), FScanHeight shl 8) shr 8;
  if (iFactX = 0) and (iFactY = 0) then
  begin
    result := (GetScanlineFast(iy)+ix)^;
    exit;
  end;
  if ScanInterpolationFilter <> rfLinear then
  begin
    iFactX := FineInterpolation256( iFactX, ScanInterpolationFilter );
    iFactY := FineInterpolation256( iFactY, ScanInterpolationFilter );
  end;
  result := InternalGetPixelCycle256(ix,iy, iFactX,iFactY);
end;

function TGrayscaleMask.ScanAtInteger(X, Y: integer): TBGRAPixel;
begin
  Result:= MaskToBGRA(ScanAtIntegerMask(X, Y));
end;

function TGrayscaleMask.ScanAt(X, Y: Single): TBGRAPixel;
begin
  Result:= MaskToBGRA(ScanAtMask(X, Y));
end;

procedure TGrayscaleMask.Negative;
begin
  NegativeRect(rect(0, 0, Width, Height));
end;

procedure TGrayscaleMask.NegativeRect(ABounds: TRect);
var
  yb, w, xb: LongInt;
  p: PByte;
begin
  ABounds.Intersect(ClipRect);
  w := ABounds.Width;
  for yb := ABounds.Top to ABounds.Bottom-1 do
  begin
    p := GetPixelAddress(ABounds.Left, yb);
    for xb := w-1 downto 0 do
    begin
      p^ := not p^;
      inc(p);
    end;
  end;
end;

procedure TGrayscaleMask.InplaceNormalize;
begin
  InplaceNormalize(rect(0, 0, Width, Height));
end;

procedure TGrayscaleMask.InplaceNormalize(ABounds: TRect);
var
  yb, w, xb: LongInt;
  p: PByte;
  minVal, maxVal, spread: byte;
begin
  ABounds.Intersect(ClipRect);
  if ABounds.IsEmpty then exit;
  minVal := 255;
  maxVal := 0;
  w := ABounds.Width;
  for yb := ABounds.Top to ABounds.Bottom-1 do
  begin
    p := GetPixelAddress(ABounds.Left, yb);
    for xb := w-1 downto 0 do
    begin
      if p^ < minVal then minVal := p^;
      if p^ > maxVal then maxVal := p^;
      inc(p);
    end;
  end;
  if (minVal > 0) or (maxVal < 255) then
  begin
    if minVal = maxVal then
    begin
      if (minVal > 0) and (minVal < 255) then
        FillRect(ABounds, TByteMask.New(255));
    end else
    begin
      spread := maxVal - minVal;
      for yb := ABounds.Top to ABounds.Bottom-1 do
      begin
        p := GetPixelAddress(ABounds.Left, yb);
        for xb := w-1 downto 0 do
        begin
          p^ := (p^ - minVal) * 255 div spread;
          inc(p);
        end;
      end;
    end;
  end;
end;

function TGrayscaleMask.NewBitmap: TGrayscaleMask;
begin
  Result:=inherited NewBitmap as TGrayscaleMask;
end;

function TGrayscaleMask.NewBitmap(AWidth, AHeight: integer): TGrayscaleMask;
begin
  Result:=inherited NewBitmap(AWidth, AHeight) as TGrayscaleMask;
end;

function TGrayscaleMask.NewBitmap(AWidth, AHeight: integer;
  const Color: TByteMask): TGrayscaleMask;
begin
  Result:=inherited NewBitmap(AWidth, AHeight, Color) as TGrayscaleMask;
end;

function TGrayscaleMask.NewBitmap(AWidth, AHeight: integer; AColor: Pointer
  ): TGrayscaleMask;
begin
  Result:=inherited NewBitmap(AWidth, AHeight, AColor) as TGrayscaleMask;
end;

function TGrayscaleMask.NewReference: TGrayscaleMask;
begin
  Result:=inherited NewReference as TGrayscaleMask;
end;

function TGrayscaleMask.GetUnique: TGrayscaleMask;
begin
  Result:=inherited GetUnique as TGrayscaleMask;
end;

function TGrayscaleMask.Duplicate(DuplicateProperties: Boolean): TGrayscaleMask;
begin
  Result:=inherited Duplicate(DuplicateProperties) as TGrayscaleMask;
end;

function TGrayscaleMask.GetPart(const ARect: TRect): TGrayscaleMask;
begin
  Result:=inherited GetPart(ARect) as TGrayscaleMask;
end;

function TGrayscaleMask.CreateBrushTexture(ABrushStyle: TBrushStyle;
  APatternColor, ABackgroundColor: TByteMask; AWidth: integer;
  AHeight: integer; APenWidth: single): TGrayscaleMask;
begin
  Result:=inherited CreateBrushTexture(ABrushStyle, APatternColor,
    ABackgroundColor, AWidth, AHeight, APenWidth) as TGrayscaleMask;
end;

function TGrayscaleMask.RotateCW: TGrayscaleMask;
begin
  Result:=inherited RotateCW as TGrayscaleMask;
end;

function TGrayscaleMask.RotateCCW: TGrayscaleMask;
begin
  Result:=inherited RotateCCW as TGrayscaleMask;
end;

function TGrayscaleMask.RotateUD: TGrayscaleMask;
begin
  Result:=inherited RotateUD as TGrayscaleMask;
end;

function TGrayscaleMask.FilterContour(ABorderValue: byte = 0): TGrayscaleMask;
var
  pDest: PByte;

  procedure ComputeDiff(x: integer; pPrevRow, pCurRow, pNextRow: PByte); inline;
  var diff: Integer;
  begin
    diff := (abs((pCurRow+x+1)^ - (pCurRow+x-1)^) +
            abs((pPrevRow+x-1)^ - (pNextRow+x+1)^) +
            abs((pPrevRow+x)^ - (pNextRow+x)^) +
            abs((pPrevRow+x+1)^ - (pNextRow+x-1)^)) div 3;
    if diff > 255 then
      (pDest+x)^ := 0
      else (pDest+x)^ := not Byte(diff);
  end;

  procedure ComputeDiffLeft(x: integer; pPrevRow, pCurRow, pNextRow: PByte); inline;
  var diff: Integer;
  begin
    diff := (abs((pCurRow+x+1)^ - ABorderValue) +
            abs(ABorderValue - (pNextRow+x+1)^) +
            abs((pPrevRow+x)^ - (pNextRow+x)^) +
            abs((pPrevRow+x+1)^ - ABorderValue)) div 3;
    if diff > 255 then
      (pDest+x)^ := 0
      else (pDest+x)^ := not Byte(diff);
  end;

  procedure ComputeDiffRight(x: integer; pPrevRow, pCurRow, pNextRow: PByte); inline;
  var diff: Integer;
  begin
    diff := (abs(ABorderValue - (pCurRow+x-1)^) +
            abs((pPrevRow+x-1)^ - ABorderValue) +
            abs((pPrevRow+x)^ - (pNextRow+x)^) +
            abs(ABorderValue - (pNextRow+x-1)^)) div 3;
    if diff > 255 then
      (pDest+x)^ := 0
      else (pDest+x)^ := not Byte(diff);
  end;

  procedure ComputeDiffLeftRight(x: integer; pPrevRow, pCurRow, pNextRow: PByte); inline;
  var diff: Integer;
  begin
    diff := abs((pPrevRow+x)^ - (pNextRow+x)^) div 3;
    if diff > 255 then
      (pDest+x)^ := 0
      else (pDest+x)^ := not Byte(diff);
  end;

var
  pPrevRow, pCurRow, pNextRow, pBorder: PByte;
  border: packed array of byte;
  yb, xb: Integer;

begin
  if NbPixels = 0 then exit;
  result := TGrayscaleMask.Create;
  result.SetSize(Width, Height);
  setlength(border, Width);
  for xb := 0 to Width-1 do
    border[xb] := ABorderValue;
  pBorder := @border[0];
  pPrevRow := nil;
  pCurRow := nil;
  pNextRow := ScanLine[0];
  for yb := 0 to Height-1 do
  begin
    pPrevRow := pCurRow;
    pCurRow := pNextRow;
    if yb < Height-1 then
      pNextRow := ScanLine[yb+1]
      else pNextRow := nil;
    pDest := result.ScanLine[yb];

    if pPrevRow = nil then
    begin
      if pNextRow = nil then
      begin
        if Width = 1 then
          ComputeDiffLeftRight(0, pBorder, pCurRow, pBorder) else
        begin
          ComputeDiffLeft(0, pBorder, pCurRow, pBorder);
          for xb := 1 to Width-2 do
            ComputeDiff(xb, pBorder, pCurRow, pBorder);
          ComputeDiffRight(Width-1, pBorder, pCurRow, pBorder);
        end;
      end else
      begin
        if Width = 1 then
          ComputeDiffLeftRight(0, pBorder, pCurRow, pNextRow) else
        begin
          ComputeDiffLeft(0, pBorder, pCurRow, pNextRow);
          for xb := 1 to Width-2 do
            ComputeDiff(xb, pBorder, pCurRow, pNextRow);
          ComputeDiffRight(Width-1, pBorder, pCurRow, pNextRow);
        end;
      end;
    end else
    if pNextRow = nil then
    begin
      if Width = 1 then
        ComputeDiffLeftRight(0, pPrevRow, pCurRow, pBorder) else
      begin
        ComputeDiffLeft(0, pPrevRow, pCurRow, pBorder);
        for xb := 1 to Width-2 do
          ComputeDiff(xb, pPrevRow, pCurRow, pBorder);
        ComputeDiffRight(Width-1, pPrevRow, pCurRow, pBorder);
      end;
    end else
    begin
      if Width = 1 then
        ComputeDiffLeftRight(0, pPrevRow, pCurRow, pNextRow) else
      begin
        ComputeDiffLeft(0, pPrevRow, pCurRow, pNextRow);
        for xb := 1 to Width-2 do
          ComputeDiff(xb, pPrevRow, pCurRow, pNextRow);
        ComputeDiffRight(Width-1, pPrevRow, pCurRow, pNextRow);
      end;
    end;
  end;
end;

function TGrayscaleMask.FilterBlurRadial(radius: single;
  blurType: TRadialBlurType): TGrayscaleMask;
begin
  Result:=inherited FilterBlurRadial(radius, blurType) as TGrayscaleMask;
end;

function TGrayscaleMask.FilterBlurRadial(const ABounds: TRect; radius: single;
  blurType: TRadialBlurType): TGrayscaleMask;
begin
  Result:=inherited FilterBlurRadial(ABounds, radius, blurType) as TGrayscaleMask;
end;

function TGrayscaleMask.FilterBlurRadial(radiusX, radiusY: single;
  blurType: TRadialBlurType): TGrayscaleMask;
begin
  Result:=inherited FilterBlurRadial(radiusX, radiusY, blurType) as TGrayscaleMask;
end;

function TGrayscaleMask.FilterBlurRadial(const ABounds: TRect; radiusX,
  radiusY: single; blurType: TRadialBlurType): TGrayscaleMask;
begin
  Result:=inherited FilterBlurRadial(ABounds, radiusX, radiusY, blurType) as TGrayscaleMask;
end;

function TGrayscaleMask.FilterBlurMotion(distance: single; angle: single;
  oriented: boolean): TGrayscaleMask;
begin
  Result:=inherited FilterBlurMotion(distance, angle, oriented) as TGrayscaleMask;
end;

function TGrayscaleMask.FilterBlurMotion(const ABounds: TRect;
  distance: single; angle: single; oriented: boolean): TGrayscaleMask;
begin
  Result:=inherited FilterBlurMotion(ABounds, distance, angle, oriented) as TGrayscaleMask;
end;

function TGrayscaleMask.FilterCustomBlur(mask: TCustomUniversalBitmap
  ): TGrayscaleMask;
begin
  Result:=inherited FilterCustomBlur(mask) as TGrayscaleMask;
end;

function TGrayscaleMask.FilterCustomBlur(const ABounds: TRect;
  mask: TCustomUniversalBitmap): TGrayscaleMask;
begin
  Result:=inherited FilterCustomBlur(ABounds, mask) as TGrayscaleMask;
end;

function TGrayscaleMask.FilterSphere: TGrayscaleMask;
var
  cx, cy: single;
  scanner: TBGRASphereDeformationScanner;
begin
  Result := NewBitmap(Width, Height);
  cx     := Width / 2 - 0.5;
  cy     := Height / 2 - 0.5;
  scanner := TBGRASphereDeformationScanner.Create(self, PointF(cx,cy), Width/2, Height/2);
  result.FillEllipseAntialias(cx, cy, Width/2-0.5, Height/2-0.5, scanner);
  scanner.Free;
end;

function TGrayscaleMask.FilterCylinder: TGrayscaleMask;
var
  cx: single;
  scanner: TBGRAVerticalCylinderDeformationScanner;
begin
  Result := NewBitmap(Width, Height);
  cx     := Width / 2 - 0.5;
  scanner := TBGRAVerticalCylinderDeformationScanner.Create(self, cx, Width/2);
  result.Fill(scanner, dmSet);
  scanner.Free;
end;

procedure DownSamplePutImageGrayscale(sourceData: PByte;
  sourcePixelSize: Int32or64; sourceRowDelta: Int32or64; sourceWidth,
  sourceHeight: Int32or64; dest: TGrayscaleMask; ADestRect: TRect);
var
  x_dest,y_dest: integer;
  pdest: PByte;
  nbPix,sum: UInt32or64;
  prev_x_src,x_src,x_src_nb,xb: Int32or64;
  x_src_inc,x_src_acc,x_src_div,x_src_rest: Int32or64;
  prev_y_src,y_src,y_src_nb,yb: Int32or64;
  y_src_inc,y_src_acc,y_src_div,y_src_rest: Int32or64;
  psrc,psrc2,psrc3: PByte;
begin
  y_src_div := ADestRect.Bottom-ADestRect.Top;
  y_src_inc := sourceHeight div y_src_div;
  y_src_rest := sourceHeight mod y_src_div;
  x_src_div := ADestRect.Right-ADestRect.Left;
  x_src_inc := sourceWidth div x_src_div;
  x_src_rest := sourceWidth mod x_src_div;

  if (x_src_rest = 0) and (y_src_rest = 0) then
  begin
    x_src_nb := x_src_inc;
    y_src_nb := y_src_inc;
    nbPix := x_src_nb*y_src_nb;
    y_src := 0;
    for y_dest := ADestRect.Top to ADestRect.Bottom-1 do
    begin
      pdest := dest.GetPixelAddress(ADestRect.Left, y_dest);
      psrc := sourceData + y_src*sourceRowDelta;
      inc(y_src,y_src_inc);

      for x_dest := ADestRect.Right-ADestRect.Left-1 downto 0 do
      begin
        sum := 0;
        psrc2 := psrc;
        for xb := x_src_nb-1 downto 0 do
        begin
          psrc3 := psrc2;
          for yb := y_src_nb-1 downto 0 do
          begin
            inc(sum, psrc3^);
            inc(psrc3, sourceRowDelta);
          end;
          inc(psrc2, sourcePixelSize);
        end;
        pdest^ := sum div nbPix;

        psrc := psrc2;
        inc(pdest);
      end;
    end;
  end else
  begin
    y_src := 0;
    y_src_acc := 0;
    for y_dest := ADestRect.Top to ADestRect.Bottom-1 do
    begin
      pdest := dest.GetPixelAddress(ADestRect.Left, y_dest);
      psrc := sourceData + y_src*sourceRowDelta;

      prev_y_src := y_src;
      inc(y_src,y_src_inc);
      inc(y_src_acc,y_src_rest);
      if y_src_acc >= y_src_div then
      begin
        dec(y_src_acc,y_src_div);
        inc(y_src);
      end;
      y_src_nb := y_src-prev_y_src;

      x_src := 0;
      x_src_acc := 0;
      for x_dest := ADestRect.Right-ADestRect.Left-1 downto 0 do
      begin
        prev_x_src := x_src;
        inc(x_src,x_src_inc);
        inc(x_src_acc,x_src_rest);
        if x_src_acc >= x_src_div then
        begin
          dec(x_src_acc,x_src_div);
          inc(x_src);
        end;
        x_src_nb := x_src-prev_x_src;

        sum := 0;
        nbPix := 0;
        psrc2 := psrc;
        for xb := x_src_nb-1 downto 0 do
        begin
          psrc3 := psrc2;
          for yb := y_src_nb-1 downto 0 do
          begin
            inc(nbPix);
            inc(sum, psrc3^);
            inc(psrc3, sourceRowDelta);
          end;
          inc(psrc2, sourcePixelSize);
        end;
        pdest^ := sum div nbPix;

        psrc := psrc2;
        inc(pdest);
      end;
    end;
  end;
end;

procedure DownSamplePutImageGrayscale(source: TBGRACustomBitmap;
  dest: TGrayscaleMask; ADestRect: TRect);
begin
  DownSamplePutImageGrayscale(source, dest, ADestRect, rect(0,0,source.Width,source.Height));
end;

procedure DownSamplePutImageGrayscale(source: TGrayscaleMask; dest: TGrayscaleMask; ADestRect: TRect); overload;
begin
  DownSamplePutImageGrayscale(source, dest, ADestRect, rect(0,0,source.Width,source.Height));
end;

procedure DownSamplePutImageGrayscale(source: TBGRACustomBitmap;
  dest: TGrayscaleMask; ADestRect: TRect; ASourceRect: TRect);
var delta: Int32or64;
begin
  delta := source.Width*sizeof(TBGRAPixel);
  if source.LineOrder = riloBottomToTop then
    delta := -delta;
  DownSamplePutImageGrayscale(
       source.GetPixelAddress(ASourceRect.Left, ASourceRect.Top) + TBGRAPixel_GreenByteOffset,
       sizeof(TBGRAPixel), delta, ASourceRect.Width, ASourceRect.Height, dest, ADestRect);
end;

procedure DownSamplePutImageGrayscale(source: TGrayscaleMask;
  dest: TGrayscaleMask; ADestRect: TRect; ASourceRect: TRect);
var delta: Int32or64;
begin
  delta := source.Width;
  if source.LineOrder = riloBottomToTop then
    delta := -delta;
  DownSamplePutImageGrayscale(source.GetPixelAddress(ASourceRect.Left, ASourceRect.Top), 1,
    delta, ASourceRect.Width, ASourceRect.Height, dest, ADestRect);
end;

end.

