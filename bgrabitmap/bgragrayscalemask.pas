unit BGRAGrayscaleMask;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, {%H-}UniversalDrawer;

type
  { TGrayscaleMask }

  TGrayscaleMask = class(specialize TGenericUniversalBitmap<TByteMask,TByteMaskColorspace>)
  private
     function GetScanLine(Y: Integer): PByte; inline;
  protected
     function InternalNew: TCustomUniversalBitmap; override;
     procedure AssignTransparentPixel(out ADest); override;
  public
     constructor Create(AWidth,AHeight: Integer; AValue: byte); overload;
     constructor Create(ABitmap: TBGRACustomBitmap; AChannel: TChannel); overload;
     constructor CreateDownSample(ABitmap: TBGRACustomBitmap; AWidth,AHeight: integer);
     procedure CopyFrom(ABitmap: TBGRACustomBitmap; AChannel: TChannel);
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

     procedure Draw(ABitmap: TBGRACustomBitmap; X,Y: Integer);
     procedure DrawAsAlpha(ABitmap: TBGRACustomBitmap; X,Y: Integer; const c: TBGRAPixel); overload;
     procedure DrawAsAlpha(ABitmap: TBGRACustomBitmap; X,Y: Integer; texture: IBGRAScanner); overload;
     function GetPixel(X,Y: integer): byte;
     procedure SetPixel(X,Y: integer; AValue: byte);
     property ScanLine[Y: Integer]: PByte read GetScanLine;
     property Data: PByte read FDataByte;

     procedure ScanNextMaskChunk(var ACount: integer; out AMask: PByteMask; out AStride: integer); override;
     function ScanAtIntegerMask(X,Y: integer): TByteMask; override;
     function ScanAtMask(X,Y: Single): TByteMask; override;
  end;

procedure DownSamplePutImageGrayscale(sourceData: PByte; sourcePixelSize: NativeInt; sourceRowDelta: NativeInt; sourceWidth, sourceHeight: NativeInt; dest: TGrayscaleMask; ADestRect: TRect); overload;
procedure DownSamplePutImageGrayscale(source: TBGRACustomBitmap; dest: TGrayscaleMask; ADestRect: TRect); overload;

procedure BGRAFillClearTypeGrayscaleMask(dest: TBGRACustomBitmap; x,
  y: integer; xThird: integer; mask: TGrayscaleMask; color: TBGRAPixel;
  texture: IBGRAScanner; RGBOrder: boolean);

const
  ByteMaskBlack : TByteMask = (gray:0);

operator = (const c1, c2: TByteMask): boolean; inline;

implementation

uses BGRABlend;

operator = (const c1, c2: TByteMask): boolean;
begin
  result := c1.gray = c2.gray;
end;

procedure BGRAFillClearTypeGrayscaleMask(dest: TBGRACustomBitmap; x,
  y: integer; xThird: integer; mask: TGrayscaleMask; color: TBGRAPixel;
  texture: IBGRAScanner; RGBOrder: boolean);
var delta: NativeInt;
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
  alphaOver: NativeUInt;
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
      ADest^.gray := (ADest^.gray*NativeUInt(65536-alphaOver) + ASource^.gray*alphaOver + 32768) shr 16;
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
  alphaOver: NativeUInt;
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
      ADest^.gray := (ADest^.gray*NativeUInt(65536-alphaOver) + temp*alphaOver + 32768) shr 16;
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
  factor: NativeUInt;
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
  alphaMul,eraseMul: NativeUInt;
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

function TGrayscaleMask.GetScanLine(Y: Integer): PByte;
begin
  result := PByte(GetScanLineByte(y));
end;

procedure TGrayscaleMask.CopyFrom(ABitmap: TBGRACustomBitmap; AChannel: TChannel
  );
var psrc: PByte;
  pdest: PByte;
  x,y: integer;
  ofs: NativeInt;
begin
  SetSize(ABitmap.Width,ABitmap.Height);
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
  inherited Create(0,0);
  if (AWidth = ABitmap.Width) and (AHeight = ABitmap.Height) then
    CopyFrom(ABitmap,cGreen)
  else
  begin
    if (ABitmap.Width < AWidth) or (ABitmap.Height < AHeight) then
      raise exception.Create('Original size smaller');
    SetSize(AWidth,AHeight);
    if NbPixels > 0 then
      DownSamplePutImageGrayscale(ABitmap, self, rect(0,0,FWidth,FHeight));
  end;
end;

procedure TGrayscaleMask.Draw(ABitmap: TBGRACustomBitmap; X, Y: Integer);
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
    for i := copycount -1 downto 0 do
    begin
      value := psource^;
      pdest^ := BGRA(value,value,value,255);
      inc(psource);
      inc(pdest);
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
begin
  Result:= ScanAtInteger(round(X),round(Y));
end;

procedure DownSamplePutImageGrayscale(sourceData: PByte;
  sourcePixelSize: NativeInt; sourceRowDelta: NativeInt; sourceWidth,
  sourceHeight: NativeInt; dest: TGrayscaleMask; ADestRect: TRect);
var
  x_dest,y_dest: integer;
  pdest: PByte;
  nbPix,sum: NativeUInt;
  prev_x_src,x_src,x_src_nb,xb: NativeInt;
  x_src_inc,x_src_acc,x_src_div,x_src_rest: NativeInt;
  prev_y_src,y_src,y_src_nb,yb: NativeInt;
  y_src_inc,y_src_acc,y_src_div,y_src_rest: NativeInt;
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
var delta: NativeInt;
begin
  delta := source.Width*sizeof(TBGRAPixel);
  if source.LineOrder = riloBottomToTop then
    delta := -delta;
  DownSamplePutImageGrayscale(PByte(source.ScanLine[0])+TBGRAPixel_GreenByteOffset,
       sizeof(TBGRAPixel),delta,source.Width,source.Height,dest,ADestRect);
end;

end.

