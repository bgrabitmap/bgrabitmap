unit utest7;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, utest, Graphics, BGRABitmap, BGRABitmapTypes, BGRAGradientScanner;

type
  { TTest7 }

  TTest7 = class(TTest)
  protected
    pacman,virtualScreen,background: TBGRABitmap;
    time: single;
    multigrad: TBGRACustomGradient;
    pixels: array of record
         x,y,dx,dy: single;
      end;
    procedure DrawPacman(x,y: integer; angle: single);
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer({%H-}Width,{%H-}Height: Integer; ElapsedSec: Double); override;
  end;

implementation

{ TTest7 }

procedure TTest7.DrawPacman(x, y: integer; angle: single);
var temp: TBGRABitmap;
begin
   temp := pacman.FilterRotate(pointF((pacman.width-1)/2,(pacman.height-1)/2),angle);
   virtualScreen.PutImage(x-temp.width div 2,y-temp.height div 2,temp,dmDrawWithTransparency);
   temp.Free;
end;

constructor TTest7.Create;
var temp: TBGRABitmap;
begin
  inherited Create;
  Name := 'Smart zoom x3 and rotate';
  virtualScreen := nil;
  background := nil;
  pacman := TBGRABitmap.Create(ResourceDir+'pac_d1.bmp');
  pacman.ReplaceColor(pacman.GetPixel(0,0),BGRAPixelTransparent);
  temp := pacman.FilterSmartZoom3(moMediumSmooth);
  pacman.Free;
  pacman := temp;
  time := 0;
  multigrad := TBGRAHueGradient.Create(0,0,65535,40000,[hgoRepeat,hgoPositiveDirection,hgoLightnessCorrection]);
end;

destructor TTest7.Destroy;
begin
  virtualScreen.Free;
  pacman.free;
  multigrad.Free;
  background.Free;
  inherited Destroy;
end;

procedure TTest7.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var
  i: Integer;
  smallW,smallH: integer;
  pixBmp: TBGRABitmap;
begin
  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
  begin
    FreeAndNil(virtualScreen);
    FreeAndNil(background);
  end;

  smallW := (Width+4) div 8;
  smallH := (Height+4) div 8;

  if virtualscreen = nil then
  begin
    virtualscreen := TBGRABitmap.Create(Width,Height);
    background := TBGRABitmap.Create(Width,Height);
    background.GradientFill(0,0,Width,Height,multigrad,gtRadial,PointF(0,0),PointF(Width/2,Height/2),dmSet,False);

    setlength(pixels, round(sqrt(smallW*smallH)) );
    for i := 0 to high(pixels) do
    begin
      pixels[i].x := random(smallW);
      pixels[i].y := random(smallH);
      pixels[i].dx := (random(1001)-500)/500;
      pixels[i].dy := (random(1001)-500)/500;
    end;
  end;

  virtualscreen.PutImage(0,0,background,dmSet);

  pixBmp := TBGRABitmap.Create(smallW, smallH);
  for i := 0 to high(pixels) do
  begin
    pixBmp.DrawPixelF(pixels[i].x, pixels[i].y, BGRABlack, dmDrawWithTransparency);
    IncF(pixels[i].x, pixels[i].dx);
    IncF(pixels[i].y, pixels[i].dy);
    if pixels[i].x <= -1 then pixels[i].x := smallW else
    if pixels[i].x >= smallW then pixels[i].x := -1;
    if pixels[i].y <= -1 then pixels[i].y := smallH else
    if pixels[i].y >= smallH then pixels[i].y := -1;
  end;
  virtualScreen.StretchPutImage(rect(0,0,Width,Height), pixBmp, dmDrawWithTransparency);
  pixBmp.Free;

  DrawPacman(virtualScreen.Width div 4,virtualScreen.Height div 2, ln(time+1)*100);
  DrawPacman(3*virtualScreen.Width div 4,virtualScreen.Height div 2, -time*4);

  virtualscreen.Draw(Canvas,Left,Top,OpaqueDraw);
end;

procedure TTest7.OnTimer(Width, Height: Integer; ElapsedSec: Double);
begin
  time := time+ElapsedSec*5;
end;

end.

