// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAMSEguiBitmap;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, BGRAClasses, BGRAGraphics, BGRABitmapTypes, BGRADefaultBitmap,
  BGRAText, msebitmap;

type

  { TBGRAMSEguiBitmap }

  TBGRAMSEguiBitmap = class(TBGRADefaultBitmap)
  protected
    procedure CopyDataToBitmap(AData: Pointer; AWidth,AHeight: integer; ALineOrder: TRawImageLineOrder; ABitmap: TBitmap);
    procedure RebuildBitmap; override;
    procedure DoLoadFromBitmap; override;
    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; override;
    function LoadFromRawImage({%H-}ARawImage: TRawImage; {%H-}DefaultOpacity: byte;
      {%H-}AlwaysReplaceAlpha: boolean=False; {%H-}RaiseErrorOnInvalidPixelFormat: boolean
      =True): boolean; override;
    procedure FreeBitmap; override;
    procedure NotAvailable;
    procedure InternalAssignBitmapPixels(ASource: TBitmap);
    function GetCanvas: TCanvas; override;
  public
    procedure Assign(ASource: TPersistent); override;
    destructor Destroy; override;
    procedure GetImageFromCanvas({%H-}CanvasSource: TCanvas; {%H-}x, {%H-}y: integer); override; //not available
    procedure DataDrawTransparent({%H-}ACanvas: TCanvas; {%H-}Rect: TRect; {%H-}AData: Pointer;
      {%H-}ALineOrder: TRawImageLineOrder; {%H-}AWidth, {%H-}AHeight: integer); override;
    procedure DataDrawOpaque({%H-}ACanvas: TCanvas; {%H-}Rect: TRect; {%H-}AData: Pointer;
      {%H-}ALineOrder: TRawImageLineOrder; {%H-}AWidth, {%H-}AHeight: integer); override;
    procedure TakeScreenshot({%H-}ARect: TRect); override; //not available
    procedure TakeScreenshotOfPrimaryMonitor; override; //not available
    procedure LoadFromDevice({%H-}DC: HDC); override; //not available
    procedure LoadFromDevice({%H-}DC: HDC; {%H-}ARect: TRect); override; //not available
  end;
  
type
  { TBitmapTracker }

  TBitmapTracker = class(TMaskedBitmap)
  protected
    FUser: TBGRADefaultBitmap;
    procedure DoChange; override;
  public
    constructor Create(AUser: TBGRADefaultBitmap); overload;
  end;  

implementation

uses msegraphics, msegraphutils, math;

{ TBitmapTracker }

constructor TBitmapTracker.Create(AUser: TBGRADefaultBitmap);
begin
  FUser := AUser;
  inherited Create(bmk_rgb);
end;

procedure TBitmapTracker.DoChange;
begin
  if FUser <> nil then
    FUser.NotifyBitmapChange;
  inherited DoChange;
end;

{ TBGRAMSEguiBitmap }

procedure TBGRAMSEguiBitmap.CopyDataToBitmap(AData: Pointer; AWidth,AHeight: integer; ALineOrder: TRawImageLineOrder; ABitmap: TBitmap);
var
  ppix: PLongword;
  pbmp: PLongword;
  pmask: PByte;
  x,y,yBmp : integer;
  setMask: boolean;  
begin
  ABitmap.Size := Size(AWidth, AHeight);
  ppix := AData;
  pmask := nil;
  if ABitmap is TMaskedBitmap then
  begin  
    setMask := true;
    TMaskedBitmap(ABitmap).Masked := true;
    TMaskedBitmap(ABitmap).MaskKind := bmk_gray;  
  end else
    setMask := false;
  for y := 0 to Height-1 do
  begin
    if ALineOrder = riloTopToBottom then
      yBmp := y else yBmp := Height-1-y;
    pbmp := ABitmap.Scanline[yBmp];
    if setMask then pmask := TMaskedBitmap(ABitmap).Mask.Scanline[yBmp];
    for x := 0 to Width-1 do
    begin
      pbmp^ := ppix^ and $ffffff;
      if setMask then 
      begin
        pmask^ := ppix^ shr 24;
        inc(pmask);
      end;
      inc(ppix);
      inc(pbmp);
    end;
  end;
end;

procedure TBGRAMSEguiBitmap.RebuildBitmap;
begin
  if FBitmap = nil then
    FBitmap := TBitmapTracker.Create(self);

  CopyDataToBitmap(Data, Width, Height, LineOrder, FBitmap);
end;

procedure TBGRAMSEguiBitmap.InternalAssignBitmapPixels(ASource: TBitmap);
var 
  ppix,pbmp: PLongword;
  pmask: PByte;
  getMask: boolean;
  x,y,wm1: integer;
begin
  if ASource is TMaskedBitmap then
    getMask := TMaskedBitmap(ASource).Masked
    else getMask := false;
  wm1 := min(Width-1, ASource.Width-1);
  for y := 0 to min(Height-1, ASource.Height-1) do
  begin
    ppix := plongword(GetScanlineFast(y));
    pbmp := ASource.Scanline[y];
    if getMask then pmask := TMaskedBitmap(ASource).Mask.Scanline[y];
    for x := 0 to wm1 do
    begin
      if getMask then
      begin
        ppix^ := (pbmp^ and $ffffff) or (pmask^ shl 24); 
        inc(pmask);
      end else
        ppix^ := (pbmp^ and $ffffff) or $ff000000;
      inc(ppix);
      inc(pbmp);
    end;
  end;
end;

function TBGRAMSEguiBitmap.GetCanvas: TCanvas; 
begin
  result := inherited GetCanvas;
  NotifyBitmapChange;
end;

procedure TBGRAMSEguiBitmap.Assign(ASource: TPersistent); 
var bmp: TBitmap;
begin
  if ASource is TBitmap then
  begin
    bmp := TBitmap(ASource);
    SetSize(bmp.Width, bmp.Height);
    InternalAssignBitmapPixels(bmp);
    InvalidateBitmap;
  end else
    inherited Assign(ASource);
end;

procedure TBGRAMSEguiBitmap.DoLoadFromBitmap; 
begin
  if Assigned(FBitmap) then
    InternalAssignBitmapPixels(FBitmap);
end;

function TBGRAMSEguiBitmap.CreateDefaultFontRenderer: TBGRACustomFontRenderer;
begin
  result := TMSEFontRenderer.Create;
end;

function TBGRAMSEguiBitmap.LoadFromRawImage(ARawImage: TRawImage;
  DefaultOpacity: byte; AlwaysReplaceAlpha: boolean;
  RaiseErrorOnInvalidPixelFormat: boolean): boolean;
begin
  self.Assign(ARawImage);
  result := true;
end;

procedure TBGRAMSEguiBitmap.FreeBitmap;
begin
  FreeAndNil(FBitmap);
end;

procedure TBGRAMSEguiBitmap.NotAvailable;
begin
  raise exception.Create('Function not available with MSEgui');
end;

destructor TBGRAMSEguiBitmap.Destroy;
begin
  inherited Destroy;
end;

procedure TBGRAMSEguiBitmap.GetImageFromCanvas(CanvasSource: TCanvas; x,
  y: integer);
begin
  (Bitmap as TMaskedBitmap).Masked := false;
  Bitmap.Canvas.CopyArea(CanvasSource, MakeRect(x,y,Bitmap.Width,Bitmap.Height),
              Point(0,0));              
end;

procedure TBGRAMSEguiBitmap.DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var
  bmp: TMaskedBitmap;
begin
  bmp := TMaskedBitmap.Create(bmk_rgb);
  CopyDataToBitmap(Data, Width, Height, ALineOrder, bmp);
  bmp.Paint(ACanvas, Rect);  
  bmp.Free;
end;

procedure TBGRAMSEguiBitmap.DataDrawOpaque(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create(bmk_rgb);
  CopyDataToBitmap(AData, AWidth, AHeight, ALineOrder, bmp);
  bmp.Paint(ACanvas, Rect);  
  bmp.Free;
end;

procedure TBGRAMSEguiBitmap.TakeScreenshot(ARect: TRect);
begin
  NotAvailable;
end;

procedure TBGRAMSEguiBitmap.TakeScreenshotOfPrimaryMonitor;
begin
  NotAvailable;
end;

procedure TBGRAMSEguiBitmap.LoadFromDevice(DC: HDC);
begin
  NotAvailable;
end;

procedure TBGRAMSEguiBitmap.LoadFromDevice(DC: HDC; ARect: TRect);
begin
  NotAvailable;
end;

end.

