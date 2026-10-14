program TestGtk3Alpha;
{$mode objfpc}{$H+}
{$IFDEF LCLgtk3}
uses Interfaces, Forms, Graphics, SysUtils, Types, LCLType,
  Gtk3Objects, Lazcairo1, BGRABitmap, BGRABitmapTypes;
var Failures: Integer = 0;
procedure Check(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then begin Inc(Failures); WriteLn('FAIL: ', AMessage); end;
end;
procedure RunCase(AMode: Integer; ABottomUp, APadded, AOpaque: Boolean);
const W=8; H=6;
var
  Surface: Pcairo_surface_t;
  Cr: Pcairo_t;
  DC: TGtk3DeviceContext;
  Canvas: TCanvas;
  Bitmap: TBGRABitmap;
  Source: array[0..7] of TBGRAPixel;
  First: PBGRAPixel;
  Stride, RowPixels, X, Y, Index: Integer;
  Pixels: PLongWord;
  Actual, Expected: LongWord;
  P: TBGRAPixel;
  LabelText: String;
begin
  LabelText := Format('mode=%d bottomup=%s padded=%s opaque=%s',
    [AMode,BoolToStr(ABottomUp,True),BoolToStr(APadded,True),BoolToStr(AOpaque,True)]);
  Surface := cairo_image_surface_create(CAIRO_FORMAT_ARGB32,W,H);
  Cr := cairo_create(Surface);
  DC := TGtk3DeviceContext.CreateFromCairo(nil,Cr);
  Canvas := TCanvas.Create;
  Bitmap := TBGRABitmap.Create(2,2);
  try
    Canvas.Handle := HDC(DC);
    Bitmap.SetPixel(0,0,BGRA(255,0,0,0));
    Bitmap.SetPixel(1,0,BGRA(255,0,0,128));
    Bitmap.SetPixel(0,1,BGRA(255,0,0,255));
    Bitmap.SetPixel(1,1,BGRA(255,0,0,64));
    if AOpaque then Bitmap.Fill(BGRA(255,0,0));
    RowPixels:=2;
    if APadded then RowPixels:=4;
    FillChar(Source,SizeOf(Source),0);
    for Y:=0 to 1 do
      for X:=0 to 1 do
      begin
        if ABottomUp then Index:=(1-Y)*RowPixels+X else Index:=Y*RowPixels+X;
        Source[Index]:=Bitmap.GetPixel(X,Y);
      end;
    Stride:=RowPixels*SizeOf(TBGRAPixel);
    First:=@Source[0];
    if ABottomUp then begin First:=@Source[RowPixels]; Stride:=-Stride; end;
    cairo_set_source_rgba(Cr,0,0,1,1);
    cairo_paint(Cr);
    // Model the operator inherited from GTK3's Wayland paint context.
    cairo_set_operator(Cr,CAIRO_OPERATOR_SOURCE);
    if AMode=0 then Bitmap.Draw(Canvas,2,2,False)
    else Bitmap.DataDrawTransparent(Canvas,Rect(2,2,4,4),First,Stride,2,2);
    Check(cairo_get_operator(Cr)=CAIRO_OPERATOR_SOURCE,LabelText+' restores operator');
    cairo_surface_flush(Surface);
    Pixels:=PLongWord(cairo_image_surface_get_data(Surface));
    for Y:=0 to H-1 do
      for X:=0 to W-1 do
      begin
        Expected:=$FF0000FF;
        if (X>=2) and (X<4) and (Y>=2) and (Y<4) then
        begin
          P:=Bitmap.GetPixel(X-2,Y-2);
          Expected:=$FF000000 or (LongWord(P.alpha) shl 16) or (255-P.alpha);
        end;
        Actual:=Pixels[Y*W+X];
        Check(Actual=Expected,LabelText+Format(' pixel (%d,%d) expected %.8x got %.8x',[X,Y,Expected,Actual]));
      end;
  finally
    Canvas.Handle:=0;
    Bitmap.Free;
    Canvas.Free;
    DC.Free;
    cairo_destroy(Cr);
    cairo_surface_destroy(Surface);
  end;
end;
begin
  Application.Initialize;
  RunCase(0,False,False,False);
  RunCase(0,False,False,True);
  RunCase(1,False,False,False);
  RunCase(1,True,False,False);
  RunCase(1,False,True,False);
  RunCase(1,True,True,False);
  RunCase(1,False,False,True);
  WriteLn('failures=',Failures);
  if Failures>0 then Halt(1);
{$ELSE}
begin
  WriteLn('SKIP: GTK3 widgetset required');
{$ENDIF}
end.