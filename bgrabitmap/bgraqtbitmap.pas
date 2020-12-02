// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
 /**************************************************************************\
                             bgraqtbitmap.pas
                             -----------------
                 This unit should NOT be added to the 'uses' clause.
                 It contains patches for Qt.
}

unit BGRAQtBitmap;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRALCLBitmap, Graphics,
  GraphType, BGRABitmapTypes;

type
  { TBGRAQtBitmap }

  TBGRAQtBitmap = class(TBGRALCLBitmap)
  private
    procedure SlowDrawTransparent(ABitmap: TBGRACustomBitmap;
      ACanvas: TCanvas; ARect: TRect);
  public
    procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
      AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure DataDrawOpaque(ACanvas: TCanvas; ARect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean = True); override;
    procedure Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean = True); override;
    procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); override;
  end;

implementation

uses LCLType,
  LCLIntf, IntfGraphics,
  qtobjects, {$ifdef LCLqt5}qt5{$else}qt4{$endif},
  FPImage;

procedure TBGRAQtBitmap.SlowDrawTransparent(ABitmap: TBGRACustomBitmap;
  ACanvas: TCanvas; ARect: TRect);
begin
  ACanvas.StretchDraw(ARect, ABitmap.Bitmap);
end;

procedure TBGRAQtBitmap.DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var
  Temp: TBGRALCLPtrBitmap;
begin
  Temp := TBGRALCLPtrBitmap.Create(AWidth, AHeight, AData);
  Temp.LineOrder := ALineOrder;
  SlowDrawTransparent(Temp, ACanvas, Rect);
  Temp.Free;
end;

procedure TBGRAQtBitmap.DataDrawOpaque(ACanvas: TCanvas; ARect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
{$IFDEF DARWIN}
var
  psrc,pdest: PBGRAPixel;
  bmp: TBGRAQtBitmap;
  x, y: integer;
{$ENDIF}
begin
  {$IFDEF DARWIN}
  bmp := TBGRAQtBitmap.Create(AWidth,AHeight);
  try
    if ALineOrder = riloTopToBottom then psrc := AData
    else psrc := PBGRAPixel(AData) + (AWidth*(AHeight-1));
    for y := 0 to AHeight-1 do
    begin
      pdest := bmp.ScanLine[y];
      for x := 0 to AWidth-1 do
      begin
        pdest^.red := psrc^.red;
        pdest^.green:= psrc^.green;
        pdest^.blue := psrc^.blue;
        pdest^.alpha := 255;
        inc(psrc);
        inc(pdest);
      end;
      if ALineOrder = riloBottomToTop then dec(psrc, 2*AWidth);
    end;
    bmp.Draw(ACanvas, ARect, false);
  finally
    bmp.Free;
  end;
  {$ELSE}
  inherited DataDrawOpaque(ACanvas, ARect, AData, ALineOrder, AWidth, AHeight);
  {$ENDIF}
end;

procedure TBGRAQtBitmap.Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean);
begin
  if self = nil then
    exit;
  if Opaque then
    DataDrawOpaque(ACanvas, Rect(X, Y, X + Width, Y + Height), Data, FLineOrder,
      FWidth, FHeight)
  else
    SlowDrawTransparent(Self, ACanvas, Rect(X, Y, X + Width, Y + Height));
end;

procedure TBGRAQtBitmap.Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean);
begin
  if self = nil then
    exit;
  if Opaque then
    DataDrawOpaque(ACanvas, Rect, Data, FLineOrder, FWidth, FHeight)
  else
    SlowDrawTransparent(Self, ACanvas, Rect);
end;

procedure TBGRAQtBitmap.GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer);
var
  bmp: TBitmap;
  Ofs: TPoint;
  SrcX, SrcY: integer;
  dcSource, dcDest: TQtDeviceContext;
  B: Boolean;
begin
  DiscardBitmapChange;
  bmp    := TBitmap.Create;
  bmp.PixelFormat := pf24bit;
  bmp.Width := Width;
  bmp.Height := Height;
  dcDest := TQtDeviceContext(bmp.Canvas.handle);

  dcSource := TQtDeviceContext(CanvasSource.Handle);
  LCLIntf.GetWindowOrgEx(CanvasSource.Handle, @Ofs);

  SrcX     := x + Ofs.X;
  SrcY     := y + Ofs.Y;

  if (dcSource.vImage <> nil) and (dcSource.vImage.Handle <> nil) then
  begin
    // we must stop painting on device
    B := QPainter_isActive(dcDest.Widget);
    if B then
      QPainter_end(dcDest.Widget);
    TQtImage(bmp.Handle).CopyFrom(dcSource.vImage.Handle,
      SrcX, SrcY, Width, Height);
    if B then
      QPainter_begin(dcDest.Widget, TQtImage(bmp.Handle).Handle);
  end;

  LoadFromRawImage(bmp.RawImage, 255, True);
  bmp.Free;
  InvalidateBitmap;
end;

end.

