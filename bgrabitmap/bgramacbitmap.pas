// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAMacBitmap;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRALCLBitmap, BGRAGraphics, BGRABitmapTypes,
  BGRADefaultBitmap;

type

  { TBGRAMacBitmap }

  TBGRAMacBitmap = class(TBGRALCLBitmap)
    procedure DataDrawOpaque(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    function MakeBitmapCopy(BackgroundColor: TColor; AMasked: boolean=False): TBitmap; override;
    procedure TakeScreenshotOfPrimaryMonitor; override;
    procedure TakeScreenshot(ARect: TRect); override;
  end;

implementation

uses LCLType, GraphType, LCLIntf, FPimage;

procedure DataDrawOpaqueImplementation(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
type
  PARGB = ^TARGB;
  TARGB = packed record
    alpha,red,green,blue: byte;
  end;

var
  Temp:      TBitmap;
  RawImage:  TRawImage;
  BitmapHandle, MaskHandle: HBitmap;
  CreateResult: boolean;
  psrc: PBGRAPixel;
  pdest: PARGB;
  n: Integer;
begin
  if (AHeight = 0) or (AWidth = 0) then
    exit;

  RawImage.Init;
  RawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(AWidth,AHeight);
  RawImage.Description.Depth := 24;
  RawImage.Description.AlphaPrec := 0;
  RawImage.Description.LineOrder := ALineOrder;
  RawImage.Description.LineEnd := rileDWordBoundary;
  RawImage.CreateData(False);
  psrc := PBGRAPixel(AData);
  pdest := PARGB(RawImage.Data);
  for n := AWidth*AHeight-1 downto 0 do
  begin
    pdest^.alpha := 255;
    pdest^.red := psrc^.red;
    pdest^.green := psrc^.green;
    pdest^.blue := psrc^.blue;
    inc(pdest);
    inc(psrc);
  end;
  CreateResult := RawImage_CreateBitmaps(RawImage, BitmapHandle, MaskHandle, False);
  RawImage.FreeData;

  if not CreateResult then
    raise FPImageException.Create('Failed to create bitmap handle');

  Temp := TBitmap.Create;
  Temp.Handle := BitmapHandle;
  Temp.MaskHandle := MaskHandle;
  ACanvas.StretchDraw(Rect, Temp);
  Temp.Free;
end;

{ TBGRAMacBitmap }

procedure TBGRAMacBitmap.DataDrawOpaque(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
begin
  DataDrawOpaqueImplementation(ACanvas, Rect, AData, ALineOrder, AWidth, AHeight);
end;

function TBGRAMacBitmap.MakeBitmapCopy(BackgroundColor: TColor; AMasked: boolean): TBitmap;
var
  temp: TBGRADefaultBitmap;
  x, y: Integer;
  psrc, pdest: PBGRAPixel;
begin
  if not AMasked or not HasTransparentPixels then
     Result:=inherited MakeBitmapCopy(BackgroundColor, AMasked)
  else
  begin
    if not HasSemiTransparentPixels then
    begin
      result := TBitmap.Create;
      result.Assign(Bitmap);
    end else
    begin
      temp := NewBitmap(Width, Height, ColorToBGRA(BackgroundColor));
      try
        temp.PutImage(0, 0, self, dmDrawWithTransparency);
        for y := 0 to Height-1 do
        begin
          psrc := ScanLine[y];
          pdest := temp.ScanLine[y];
          for x := 0 to Width-1 do
          begin
            if psrc^.alpha < 128 then
               pdest^ := BGRAPixelTransparent;
            inc(psrc);
            inc(pdest);
          end;
        end;
        result := TBitmap.Create;
        result.Assign(temp.Bitmap);
      finally
        temp.Free;
      end;
    end;
  end;
end;

procedure TBGRAMacBitmap.TakeScreenshotOfPrimaryMonitor;
var primaryDC: THandle;
begin
  primaryDC := LCLIntf.GetDC(0);
  try
    LoadFromDevice(primaryDC, rect(0,0,2560,1440));
  finally
    LCLIntf.ReleaseDC(0, primaryDC);
  end;
end;

procedure TBGRAMacBitmap.TakeScreenshot(ARect: TRect);
var all: TBGRAMacBitmap;
begin
  all := TBGRAMacBitmap.Create;
  all.TakeScreenshotOfPrimaryMonitor;
  SetSize(ARect.Width, ARect.Height);
  FillTransparent;
  PutImage(-ARect.Left, -ARect.Top, all, dmSet);
  all.Free;
end;

end.

