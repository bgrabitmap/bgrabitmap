{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}

type
  { TGenericUniversalBitmap }

  generic TGenericUniversalBitmap<TPixel,TColorspace> = class(TCustomUniversalBitmap)
  private
    type PPixel = ^TPixel;
    function GetDataPtr: PPixel; inline;
    function GetScanLine(y: integer): PPixel; inline;
  protected
    class function DefaultColorspace: TColorspaceAny; override;
    procedure Init; override; //ensure colorspace
    function GetScanlineFast(y: integer): PPixel; inline; //typed function
    procedure InternalCopyPixels(ASource,ADest: PByte; ASourceStride,ADestStride: PtrInt; ACount: integer); override;
    procedure InternalSwapPixels(ABuf1,ABuf2: PByte; AStride1,AStride2: PtrInt; ACount: integer); override;
  public
    //typed functions

    {** Creates an image of dimensions ''AWidth'' and ''AHeight'' and fills it with ''Color'' }
    constructor Create(AWidth, AHeight: integer; const AColor: TPixel);
    class procedure SolidBrush(out ABrush: TUniversalBrush; const AColor: TPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); overload;
    procedure SetPixel(x,y: int32or64; AColor: TPixel); overload; inline;
    function GetPixel(x,y: int32or64): TPixel; inline;
    function GetPixelCycle(x,y: int32or64): TPixel; inline;
    {** Returns the address of the left-most pixel of any line.
        The parameter y ranges from 0 to Height-1 }
    property ScanLine[y: integer]: PPixel Read GetScanLine;
    property Data: PPixel read GetDataPtr;

    //optimized
    procedure SetPixelIndirect(x,y: int32or64; AColor: pointer); override;
    procedure GetPixelIndirect(x,y: int32or64; AColor: pointer); override;
    procedure GetPixelCycleIndirect(x,y: int32or64; AColor: pointer); override;
    function GetPixelAddress(x, y: integer): PByte; override;
    procedure HorizontalFlip(ARect: TRect); overload; override;
    procedure RotateUDInplace(ARect: TRect); overload; override;
  end;

{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}

{ TGenericUniversalBitmap }

function TGenericUniversalBitmap.GetDataPtr: PPixel;
begin
  result := PPixel(GetDataBytePtr);
end;

function TGenericUniversalBitmap.GetScanLine(y: integer): PPixel;
begin
  result := PPixel(GetScanLineByte(y));
end;

class function TGenericUniversalBitmap.DefaultColorspace: TColorspaceAny;
begin
  Result:= TColorspace;
end;

procedure TGenericUniversalBitmap.Init;
begin
  inherited Init;
  if FColorspace <> DefaultColorspace then raise exception.Create('Unexpected colorspace');
end;

{ Get scanline without checking bounds nor updated from bitmap }
function TGenericUniversalBitmap.GetScanlineFast(y: integer): PPixel;
begin
  if FLineOrder = riloBottomToTop then y := FHeight - 1 - y;
  result := PPixel(FDataByte+FRowSize*y);
end;

procedure TGenericUniversalBitmap.InternalCopyPixels(ASource, ADest: PByte;
  ASourceStride, ADestStride: PtrInt; ACount: integer);
begin
  while ACount>0 do
  begin
    PPixel(ADest)^ := PPixel(ASource)^;
    inc(ASource, ASourceStride);
    inc(ADest, ADestStride);
    dec(ACount);
  end;
end;

procedure TGenericUniversalBitmap.InternalSwapPixels(ABuf1, ABuf2: PByte;
  AStride1, AStride2: PtrInt; ACount: integer);
var temp: TPixel;
begin
  while ACount>0 do
  begin
    temp := PPixel(ABuf1)^;
    PPixel(ABuf1)^ := PPixel(ABuf2)^;
    PPixel(ABuf2)^ := temp;
    inc(ABuf1, AStride1);
    inc(ABuf2, AStride2);
    dec(ACount);
  end;
end;

constructor TGenericUniversalBitmap.Create(AWidth, AHeight: integer;
  const AColor: TPixel);
var
  p: PByte;
  i: Integer;
begin
  inherited Create(0, 0);
  SetSize(AWidth,AHeight);
  p := DataByte;
  for i := NbPixels-1 downto 0 do
  begin
    PPixel(p)^ := AColor;
    inc(p, sizeof(TPixel));
  end;
end;

class procedure TGenericUniversalBitmap.SolidBrush(out ABrush: TUniversalBrush;
  const AColor: TPixel; ADrawMode: TDrawMode);
begin
  SolidBrushIndirect(ABrush, @AColor, ADrawMode);
end;

procedure TGenericUniversalBitmap.SetPixelIndirect(x, y: int32or64;
  AColor: pointer);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  (GetScanlineFast(y)+x)^ := PPixel(AColor)^;
  InvalidateBitmap;
end;

procedure TGenericUniversalBitmap.GetPixelIndirect(x, y: int32or64;
  AColor: pointer);
begin
  if (x < 0) or (x >= FWidth) or (y < 0) or (y >= FHeight) then //it is possible to read pixels outside of the cliprect
    FillByte(AColor^, FPixelSize, 0) else
  begin
    LoadFromBitmapIfNeeded;
    PPixel(AColor)^ := (GetScanlineFast(y)+x)^;
  end;
end;

procedure TGenericUniversalBitmap.GetPixelCycleIndirect(x, y: int32or64;
  AColor: pointer);
begin
  if (Width = 0) or (Height = 0) then
    FillByte(AColor^, FPixelSize, 0) else
  begin
    LoadFromBitmapIfNeeded;
    PPixel(AColor)^ := (GetScanlineFast(PositiveMod(y, Height)) + PositiveMod(x, Width))^;
  end;
end;

procedure TGenericUniversalBitmap.SetPixel(x, y: int32or64; AColor: TPixel);
begin
  SetPixelIndirect(x,y,@AColor);
end;

function TGenericUniversalBitmap.GetPixel(x, y: int32or64): TPixel;
begin
  GetPixelIndirect(x,y, @result);
end;

function TGenericUniversalBitmap.GetPixelCycle(x, y: int32or64): TPixel;
begin
  GetPixelCycleIndirect(x,y, @result);
end;

function TGenericUniversalBitmap.GetPixelAddress(x, y: integer): PByte;
begin
  if FLineOrder = riloBottomToTop then y := FHeight - 1 - y;
  result := FDataByte + FRowSize * y + IntPtr(x)*sizeof(TPixel);
end;

procedure TGenericUniversalBitmap.HorizontalFlip(ARect: TRect);
begin
  inherited HorizontalFlip(ARect);
end;

procedure TGenericUniversalBitmap.RotateUDInplace(ARect: TRect);
begin
  inherited RotateUDInplace(ARect);
end;

{$ENDIF}
