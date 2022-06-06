
unit main;

{$IFDEF FPC}{$MODE objfpc}{$H+}{$ENDIF}

interface

uses
  mseglob,
  mseguiglob,
  mseguiintf,
  mseapplication,
  msestat,
  msemenus,
  msegui,
  msegraphics,
  msegraphutils,
  mseevent,
  mseclasses,
  mseforms,
  msesimplewidgets,
  msewidgets,
  Math;

type
  tmainfo = class(tmainform)
    tpaintbox1: tpaintbox;
    procedure tpaintbox1_onpaint(const Sender: twidget; const acanvas: tcanvas);
  end;

const
  deg_to_rad = pi / 180;

var
  mainfo: tmainfo;

implementation

uses
  BGRABitmap,
  BGRABitmapTypes,
  BGRAGraphics,
  BGRAClasses,
  BGRAUTF8,
  main_mfm;

procedure drawTree(x1, y1, angle, depth, multiplier: single; bgra: tbgrabitmap);
var
  x2, y2: single;
begin
  if (depth > 0) then
  begin
    x2 := x1 + (cos(angle * deg_to_rad) * depth * multiplier);
    y2 := y1 + (sin(angle * deg_to_rad) * depth * multiplier);

    bgra.DrawLineAntialias(x1, y1, x2, y2, BGRABlack, depth, False);

    // Use even values without randomness to get a 'real' fractal image

    drawTree(x2, y2, angle - randomrange(15, 50), depth - 1.44, multiplier, bgra);
    drawTree(x2, y2, angle + randomrange(10, 25), depth - 0.72, multiplier, bgra);
    drawTree(x2, y2, angle - randomrange(10, 25), depth - 3, multiplier, bgra);
    drawTree(x2, y2, angle + randomrange(15, 50), depth - 4, multiplier, bgra);
  end;
end;

procedure tmainfo.tpaintbox1_onpaint(const Sender: twidget; const acanvas: tcanvas);
const
  SampleText = 'HellÔ green tree';
var
  bmp: tbgrabitmap;
  multiplier: single;
begin
  bmp := tbgrabitmap.Create(Sender.bounds_cx, Sender.bounds_cy);

  multiplier := Sender.bounds_cy / 50;

  bmp.GradientFill(0, 0, bmp.Width, bmp.Height, cl_dkgreen, BGRA(0, 125, 0),
    gtLinear, PointF(0, 0), PointF(0, bmp.Height), dmSet);

  drawTree(bmp.Width div 2, bmp.Height, -91, 9, multiplier, bmp);

  bmp.FontFullHeight := 30;
  bmp.TextOut(20, 20, SampleText, BGRAWhite);
  bmp.HorizLine(20, 20, 20 + bmp.TextSize(SampleText).cx - 1, BGRAWhite);
  bmp.HorizLine(20, 20 + bmp.FontFullHeight, 20 + bmp.TextSize(SampleText).cx - 1, BGRAWhite);

  bmp.draw(acanvas, 0, 0, False);
  bmp.Free;
end;

end.

