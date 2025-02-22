// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Implementation of BGRABitmap for Gtk }
unit BGRAGtkBitmap;
{ This unit should NOT be added to the **uses** clause. }

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRALCLBitmap, Graphics,
  GraphType;

type
  { Implementation of TBGRABitmap for Gtk }
  TBGRAGtkBitmap = class(TBGRALCLBitmap)
  private
    FPixBuf: Pointer;
    procedure DrawTransparent(ACanvas: TCanvas; ARect: TRect);
    procedure DrawOpaque(ACanvas: TCanvas; ARect: TRect);
  protected
    procedure ReallocData; override;
    procedure FreeData; override;
  public
    procedure DataDrawTransparent(ACanvas: TCanvas; ARect: TRect;
      AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
      override;
    procedure DataDrawTransparent(ACanvas: TCanvas; ARect: TRect; ADataFirstRow: Pointer;
      ARowStride: integer; AWidth, AHeight: integer); overload;
    procedure DrawPart(ARect: TRect; ACanvas: TCanvas; x, y: integer; Opaque: boolean); override;
    procedure Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean = True); override;
    procedure Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean = True); override;
    procedure DataDrawOpaque(ACanvas: TCanvas; ARect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); overload; override;
    procedure DataDrawOpaque(ACanvas: TCanvas; ARect: TRect; ADataFirstRow: Pointer;
      ARowStride: integer; AWidth, AHeight: integer); overload;
    procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); override;
  end;

implementation

uses math, BGRABitmapTypes, BGRADefaultBitmap, BGRAFilterScanner, LCLType,
  LCLIntf, IntfGraphics,
  {$IFDEF LCLgtk3}
  LazGdk3, Gtk3Objects, LazGdkPixbuf2, LazGObject2, LazGLib2, Lazcairo1,
  {$ENDIF}
  {$IFDEF LCLgtk2}
  gdk2, gtk2def, gdk2pixbuf, glib2,
  {$ENDIF}
  {$IFDEF LCLgtk}
  gdk, gtkdef, gtkProc, gdkpixbuf, glib,
  {$ENDIF}
  FPImage, Dialogs;

procedure TBGRAGtkBitmap.ReallocData;
begin
  {$IFNDEF LCLgtk}
  If FPixBuf <> nil then g_object_unref(FPixBuf);
  {$ELSE}
  If FPixBuf <> nil then gdk_pixbuf_unref(FPixBuf);
  {$ENDIF}
  FPixBuf := nil;  
  inherited ReallocData;
  if (FWidth <> 0) and (FHeight <> 0) then
  begin  
    FPixbuf := gdk_pixbuf_new_from_data(pguchar(FDataByte),
      GDK_COLORSPACE_RGB, True, 8, Width, Height, Width*Sizeof(TBGRAPixel), nil, nil);
    if FPixbuf = nil then
      raise Exception.Create('Error initializing Pixbuf');
  end;
end;

procedure TBGRAGtkBitmap.FreeData;
begin
  {$IFNDEF LCLgtk}
  If FPixBuf <> nil then g_object_unref(FPixBuf);
  {$ELSE}
  If FPixBuf <> nil then gdk_pixbuf_unref(FPixBuf);
  {$ENDIF}
  FPixBuf := nil;
  inherited FreeData;
end;

procedure TBGRAGtkBitmap.DrawTransparent(ACanvas: TCanvas; ARect: TRect);
var DrawWidth,DrawHeight: integer;
    stretched: TBGRAGtkBitmap;
    P: TPoint;
    {$IFDEF LCLgtk3}
    cr: Pcairo_t;
    {$ENDIF}
begin
  DrawWidth := abs(ARect.Right-ARect.Left);
  DrawHeight := abs(ARect.Bottom-ARect.Top);
  if (Height = 0) or (Width = 0) or (DrawWidth = 0) or (DrawHeight = 0)
     or (ACanvas.Handle = 0) then
    exit;

  {$IFNDEF LCLgtk3}
  if (DrawWidth <> Width) or (DrawHeight <> Height) then
  begin
    stretched := Resample(DrawWidth,DrawHeight,rmSimpleStretch) as TBGRAGtkBitmap;
    try
      stretched.DrawTransparent(ACanvas,ARect);
    finally
      stretched.Free;
    end;
    exit;
  end;
  {$ENDIF}

  LoadFromBitmapIfNeeded;

  {$PUSH}{$WARNINGS OFF}If not TBGRAPixel_RGBAOrder then SwapRedBlue;{$POP}

  {$IFDEF LCLgtk3}
  P := ARect.TopLeft;
  LPToDP(ACanvas.Handle, P, 1);
  cr := TGtk3DeviceContext(ACanvas.Handle).pcr;
  cairo_save(cr);
  cairo_translate(cr, P.X, P.Y);
  cairo_scale(cr,
    (ARect.Right - ARect.Left) / Width,
    (ARect.Bottom - ARect.Top) / Height);
  gdk_cairo_set_source_pixbuf(cr, FPixBuf, 0, 0);
  cairo_paint(cr);
  cairo_restore(cr);
  {$ELSE}
  P := Point(min(ARect.Left,ARect.Right), min(ARect.Top,ARect.Bottom));
  LPToDP(ACanvas.Handle, P, 1);
  gdk_pixbuf_render_to_drawable(FPixBuf,
    TGtkDeviceContext(ACanvas.Handle).Drawable,
    TGtkDeviceContext(ACanvas.Handle).GC,
    0,0, P.X,P.Y,
    Width,Height,
    //GDK_PIXBUF_ALPHA_FULL, 1,
    GDK_RGB_DITHER_NORMAL,0,0);
  {$ENDIF}

  {$PUSH}{$WARNINGS OFF}If not TBGRAPixel_RGBAOrder then SwapRedBlue;{$POP}
end;

procedure TBGRAGtkBitmap.DrawOpaque(ACanvas: TCanvas; ARect: TRect);
begin
  DataDrawOpaque(ACanvas,ARect,Data,LineOrder,Width,Height);
end;

procedure TBGRAGtkBitmap.DataDrawTransparent(ACanvas: TCanvas; ARect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var
  rowStride: Integer;
  firstRow: Pointer;
begin
  if ALineOrder = riloTopToBottom then
  begin
    rowStride := AWidth*sizeof(TBGRAPixel);
    firstRow := AData;
  end
  else
  begin
    rowStride := -AWidth*sizeof(TBGRAPixel);
    firstRow := PBGRAPixel(AData) + (AWidth*(AHeight-1));
  end;

  DataDrawTransparent(ACanvas, ARect, firstRow, rowStride, AWidth, AHeight);
end;

procedure TBGRAGtkBitmap.DataDrawTransparent(ACanvas: TCanvas; ARect: TRect;
  ADataFirstRow: Pointer; ARowStride: integer; AWidth, AHeight: integer);

  {$IFDEF LCLgtk3}
  procedure DrawGtk3;

    function IsDataOpaque: boolean;
    var
      curRow: PByte;
      y, x: Integer;
      p: PBGRAPixel;
    begin
      curRow := PByte(ADataFirstRow);
      for y := AHeight-1 downto 0 do
      begin
        p := PBGRAPixel(curRow);
        for x := AWidth-1 downto 0 do
        begin
          if p^.alpha <> 255 then exit(false);
          inc(p);
        end;
        curRow += ARowStride;
      end;
      exit(true);
    end;

    function PremultiplyByAlpha: Pointer;
    var
      curRow: PByte;
      y, x: Integer;
      p, pDest: PBGRAPixel;
      alpha256: Byte;
    begin
      getmem(result, AWidth*AHeight*sizeof(TBGRAPixel));
      pDest := PBGRAPixel(result);

      curRow := PByte(ADataFirstRow);
      for y := AHeight-1 downto 0 do
      begin
        p := PBGRAPixel(curRow);
        for x := AWidth-1 downto 0 do
        begin
          alpha256 := p^.alpha + byte(p^.alpha >= 128);
          pDest^.red := (p^.red * alpha256 + 128) shr 8;
          pDest^.green := (p^.green * alpha256 + 128) shr 8;
          pDest^.blue := (p^.blue * alpha256 + 128) shr 8;
          pDest^.alpha := p^.alpha;
          inc(p);
        end;
        curRow += ARowStride;
      end;
    end;

  var
    temp: integer;
    dataStart, premultipliedData: Pointer;
    cr: Pcairo_t;
    surface: Pcairo_surface_t;
  begin
    if IsDataOpaque then
    begin
      // avoid premultiplication by alpha if possible
      DataDrawOpaque(ACanvas, ARect, ADataFirstRow, ARowStride, AWidth, AHeight);
      exit;
    end;

    LPtoDP(ACanvas.Handle, ARect, 2);

    if ARowStride < 0 then
    begin
      dataStart := PByte(ADataFirstRow) + ARowStride * (AHeight - 1);
      temp := ARect.Top;
      ARect.Top := ARect.Bottom;
      ARect.Bottom := temp;
    end else
      dataStart := ADataFirstRow;

    premultipliedData := PremultiplyByAlpha;

    surface := cairo_image_surface_create_for_data(PByte(premultipliedData),
      CAIRO_FORMAT_ARGB32, AWidth, AHeight, Abs(ARowStride));
    if cairo_surface_status(surface) <> CAIRO_STATUS_SUCCESS then
    begin
      freemem(premultipliedData);
      raise Exception.Create('Error creating Cairo surface from pixel data');
    end;

    cr := TGtk3DeviceContext(ACanvas.Handle).pcr;
    cairo_save(cr);
    cairo_translate(cr, ARect.Left, ARect.Top);
    cairo_scale(cr,
      (ARect.Right - ARect.Left) / AWidth,
      (ARect.Bottom - ARect.Top) / AHeight);
    cairo_set_source_surface(cr, surface, 0, 0);
    cairo_paint(cr);
    cairo_restore(cr);

    cairo_surface_destroy(surface);
    freemem(premultipliedData);
    ACanvas.Changed;
  end;
  {$ELSE}
  procedure DrawGtk2;

    procedure DataSwapRedBlue;
    var
      y: Integer;
      p: PByte;
    begin
      p := PByte(ADataFirstRow);
      for y := 0 to AHeight-1 do
      begin
        TBGRAFilterScannerSwapRedBlue.ComputeFilterAt(PBGRAPixel(p),PBGRAPixel(p),AWidth,False);
        inc(p, ARowStride);
      end;
    end;

    procedure DrawStretchedSoftware;
    var
      dataStart: Pointer;
      ptr: TBGRAPtrBitmap;
      stretched: TBGRACustomBitmap;
    begin
      if ARowStride < 0 then
        dataStart := PByte(ADataFirstRow) + ARowStride*(Height-1)
      else
        dataStart := ADataFirstRow;

      if ARowStride <> abs(AWidth*sizeof(TBGRAPixel)) then
        raise exception.Create('DataDrawTransparent not supported when using custom row stride and resample');

      ptr := TBGRAPtrBitmap.Create(AWidth,AHeight,dataStart);
      if ARowStride < 0 then
        ptr.LineOrder := riloBottomToTop
      else
        ptr.LineOrder := riloTopToBottom;
      stretched := ptr.Resample(ARect.Right-ARect.Left,ARect.Bottom-ARect.Top, rmSimpleStretch);
      ptr.free;
      try
        DataDrawTransparent(ACanvas,ARect,stretched.Data,stretched.LineOrder,stretched.Width,stretched.Height);
      finally
        stretched.Free;
      end;
    end;

  var
    temp: integer;
    tempPixbuf: PGdkPixbuf;
  begin
    {$PUSH}{$WARNINGS OFF}if not TBGRAPixel_RGBAOrder then DataSwapRedBlue;{$POP}

    if ARect.Right < ARect.Left then
    begin
      temp := ARect.Left;
      ARect.Left := ARect.Right;
      ARect.Right := temp;
    end;

    if ARect.Bottom < ARect.Top then
    begin
      temp := ARect.Top;
      ARect.Top := ARect.Bottom;
      ARect.Bottom := temp;
    end;

    if (AWidth <> ARect.Right-ARect.Left) or (AHeight <> ARect.Bottom-ARect.Top) then
      DrawStretchedSoftware
    else
    begin
      LPtoDP(ACanvas.Handle, ARect, 2);
      tempPixbuf := gdk_pixbuf_new_from_data(pguchar(ADataFirstRow),
        GDK_COLORSPACE_RGB, True, 8, AWidth, AHeight, AWidth*Sizeof(TBGRAPixel), nil, nil);
      if tempPixbuf = nil then
        raise Exception.Create('Error initializing Pixbuf');

      gdk_pixbuf_render_to_drawable(tempPixbuf,
        TGtkDeviceContext(ACanvas.Handle).Drawable,
        TGtkDeviceContext(ACanvas.Handle).GC,
        0,0, ARect.Left,ARect.Top,
        AWidth,AHeight,
        //GDK_PIXBUF_ALPHA_FULL, 1,
        GDK_RGB_DITHER_NORMAL,0,0);

      {$IFNDEF LCLgtk}
      If tempPixbuf <> nil then g_object_unref(tempPixbuf);
      {$ELSE}
      If tempPixbuf <> nil then gdk_pixbuf_unref(tempPixbuf);
      {$ENDIF}
      ACanvas.Changed;
    end;
    {$PUSH}{$WARNINGS OFF}if not TBGRAPixel_RGBAOrder then DataSwapRedBlue;{$POP}
  end;
  {$ENDIF}

begin
  if (AHeight = 0) or (AWidth = 0) or (ARect.Left = ARect.Right) or
    (ARect.Top = ARect.Bottom) or (ACanvas.Handle = 0) then exit;

  {$IFDEF LCLgtk3}
  DrawGtk3;
  {$ELSE}
  DrawGtk2;
  {$ENDIF}
end;

procedure TBGRAGtkBitmap.DrawPart(ARect: TRect; ACanvas: TCanvas; x,
  y: integer; Opaque: boolean);
var
  rowStride,w,h: Integer;
begin
  if Opaque then
  begin
    if LineOrder = riloTopToBottom then
      rowStride := Width*sizeof(TBGRAPixel)
    else
      rowStride := -Width*sizeof(TBGRAPixel);
    w:= ARect.Right-ARect.Left;
    h:= ARect.Bottom-ARect.Top;
    DataDrawOpaque(ACanvas, rect(x,y,x+w,y+h), Scanline[ARect.Top]+ARect.Left, rowStride, w,h);
  end
  else
    inherited DrawPart(ARect, ACanvas, x, y, Opaque);
end;

procedure TBGRAGtkBitmap.Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean);
begin
  if self = nil then
    exit;
  if Opaque then
    DrawOpaque(ACanvas, Rect(X, Y, X + Width, Y + Height))
  else
    DrawTransparent(ACanvas, Rect(X, Y, X + Width, Y + Height));
end;

procedure TBGRAGtkBitmap.Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean);
begin
  if self = nil then
    exit;
  if Opaque then
    DrawOpaque(ACanvas, Rect)
  else
    DrawTransparent(ACanvas, Rect);
end;

procedure TBGRAGtkBitmap.DataDrawOpaque(ACanvas: TCanvas; ARect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var
  rowStride: Integer;
  firstRow: Pointer;
begin
  if ALineOrder = riloTopToBottom then
  begin
    rowStride := AWidth*sizeof(TBGRAPixel);
    firstRow := AData;
  end
  else
  begin
    rowStride := -AWidth*sizeof(TBGRAPixel);
    firstRow := PBGRAPixel(AData) + (AWidth*(AHeight-1));
  end;

  DataDrawOpaque(ACanvas, ARect, firstRow, rowStride, AWidth, AHeight);
end;

procedure TBGRAGtkBitmap.DataDrawOpaque(ACanvas: TCanvas; ARect: TRect;
  ADataFirstRow: Pointer; ARowStride: integer; AWidth, AHeight: integer);

  {$IFDEF LCLgtk3}
  procedure DrawGtk3;
  var
    temp: integer;
    dataStart: Pointer;
    cr: Pcairo_t;
    surface: Pcairo_surface_t;
  begin
    LPtoDP(ACanvas.Handle, ARect, 2);

    if ARowStride < 0 then
    begin
      dataStart := PByte(ADataFirstRow) + ARowStride * (AHeight - 1);
      temp := ARect.Top;
      ARect.Top := ARect.Bottom;
      ARect.Bottom := temp;
    end else
      dataStart := ADataFirstRow;

    surface := cairo_image_surface_create_for_data(PByte(dataStart),
      CAIRO_FORMAT_RGB24, AWidth, AHeight, Abs(ARowStride));
    if cairo_surface_status(surface) <> CAIRO_STATUS_SUCCESS then
      raise Exception.Create('Error creating Cairo surface from pixel data');

    cr := TGtk3DeviceContext(ACanvas.Handle).pcr;
    cairo_save(cr);
    cairo_translate(cr, ARect.Left, ARect.Top);
    cairo_scale(cr,
      (ARect.Right - ARect.Left) / AWidth,
      (ARect.Bottom - ARect.Top) / AHeight);
    cairo_set_source_surface(cr, surface, 0, 0);
    cairo_paint(cr);
    cairo_restore(cr);

    cairo_surface_destroy(surface);
    ACanvas.Changed;
  end;
  {$ELSE}
  procedure DrawGtk2;

    procedure DataSwapRedBlue;
    var
      y: Integer;
      p: PByte;
    begin
      p := PByte(ADataFirstRow);
      for y := 0 to AHeight-1 do
      begin
        TBGRAFilterScannerSwapRedBlue.ComputeFilterAt(PBGRAPixel(p),PBGRAPixel(p),AWidth,False);
        inc(p, ARowStride);
      end;
    end;

    procedure DrawStretchedSoftware;
    var
      dataStart: Pointer;
      ptr: TBGRAPtrBitmap;
      stretched: TBGRACustomBitmap;
    begin
      if ARowStride < 0 then
        dataStart := PByte(ADataFirstRow) + ARowStride*(Height-1)
      else
        dataStart := ADataFirstRow;

      if ARowStride <> abs(AWidth*sizeof(TBGRAPixel)) then
        raise exception.Create('DataDrawOpaque not supported when using custom row stride and resample');

      ptr := TBGRAPtrBitmap.Create(AWidth,AHeight,dataStart);
      if ARowStride < 0 then
        ptr.LineOrder := riloBottomToTop
      else
        ptr.LineOrder := riloTopToBottom;
      stretched := ptr.Resample(ARect.Right-ARect.Left,ARect.Bottom-ARect.Top, rmSimpleStretch);
      ptr.free;
      DataDrawOpaque(ACanvas,ARect,stretched.Data,stretched.LineOrder,stretched.Width,stretched.Height);
      stretched.Free;
    end;

  var
    temp: integer;
  begin
    {$PUSH}{$WARNINGS OFF}if not TBGRAPixel_RGBAOrder then DataSwapRedBlue;{$POP}

    if ARect.Right < ARect.Left then
    begin
      temp := ARect.Left;
      ARect.Left := ARect.Right;
      ARect.Right := temp;
    end;

    if ARect.Bottom < ARect.Top then
    begin
      temp := ARect.Top;
      ARect.Top := ARect.Bottom;
      ARect.Bottom := temp;
    end;

    if (AWidth <> ARect.Right-ARect.Left) or (AHeight <> ARect.Bottom-ARect.Top) then
      DrawStretchedSoftware
    else
    begin
      LPtoDP(ACanvas.Handle, ARect, 2);
      gdk_draw_rgb_32_image(TGtkDeviceContext(ACanvas.Handle).Drawable,
        TGtkDeviceContext(ACanvas.Handle).GC, ARect.Left, ARect.Top,
        AWidth,AHeight, GDK_RGB_DITHER_NORMAL,
        ADataFirstRow, ARowStride);
      ACanvas.Changed;
    end;
    {$PUSH}{$WARNINGS OFF}if not TBGRAPixel_RGBAOrder then DataSwapRedBlue;{$POP}
  end;
  {$ENDIF}

begin
  if (AHeight = 0) or (AWidth = 0) or (ARect.Left = ARect.Right) or
    (ARect.Top = ARect.Bottom) or (ACanvas.Handle = 0) then exit;

  {$IFDEF LCLgtk3}
  DrawGtk3;
  {$ELSE}
  DrawGtk2;
  {$ENDIF}
end;

procedure TBGRAGtkBitmap.GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer);
var
  subBmp: TBGRACustomBitmap;
  subRect: TRect;
  cw,ch: integer;
  P: TPoint;
begin
  cw := CanvasSource.Width;
  ch := CanvasSource.Height;
  if (x < 0) or (y < 0) or (x+Width > cw) or
    (y+Height > ch) then
  begin
    FillTransparent;
    if (x+Width <= 0) or (y+Height <= 0) or
      (x >= cw) or (y >= ch) then
      exit;

    if (x > 0) then subRect.Left := x else subRect.Left := 0;
    if (y > 0) then subRect.Top := y else subRect.Top := 0;
    if (x+Width > cw) then subRect.Right := cw else
      subRect.Right := x+Width;
    if (y+Height > ch) then subRect.Bottom := ch else
      subRect.Bottom := y+Height;

    subBmp := NewBitmap(subRect.Right-subRect.Left,subRect.Bottom-subRect.Top);
    subBmp.GetImageFromCanvas(CanvasSource,subRect.Left,subRect.Top);
    PutImage(subRect.Left-x,subRect.Top-y,subBmp,dmSet);
    subBmp.Free;
    exit;
  end;

  P := Point(x,y);
  LPToDP(CanvasSource.Handle, P, 1);
  {$IFDEF LCLgtk3}
  gdk_pixbuf_get_from_window(TGtk3DeviceContext(
    CanvasSource.Handle).Window, p.X, p.Y, Width, Height);
  {$ELSE}
  gdk_pixbuf_get_from_drawable(FPixBuf,
    TGtkDeviceContext(CanvasSource.Handle).Drawable,
    nil, P.X,P.Y,0,0,Width,Height);
  {$ENDIF}
  {$PUSH}{$WARNINGS OFF}If not TBGRAPixel_RGBAOrder then SwapRedBlue;{$POP}
  InvalidateBitmap;
end;


end.


