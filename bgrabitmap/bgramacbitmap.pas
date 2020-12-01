// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAMacBitmap;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRALCLBitmap, BGRAGraphics, BGRABitmapTypes;

type

  { TBGRAMacBitmap }

  TBGRAMacBitmap = class(TBGRALCLBitmap)
     procedure DataDrawOpaque(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
       ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
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

end.

