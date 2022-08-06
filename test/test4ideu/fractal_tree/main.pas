
unit main;

{$IFDEF FPC}{$MODE objfpc}{$H+}{$ENDIF}

interface
  
uses
  mseglob, mseguiglob, mseguiintf, mseapplication, msestat, msemenus, msegui,
  msegraphics, msegraphutils, mseevent, mseclasses, mseforms, msesimplewidgets,
  msewidgets, math;

type
  tmainfo = class(tmainform)
    tpaintbox1: tpaintbox;
    procedure tpaintbox1_onpaint(const sender: twidget; const acanvas: tcanvas);
  end;

const
  deg_to_rad = pi / 180;            

var  
  mainfo: tmainfo;
  
implementation

uses
 BGRABitmap, BGRABitmapTypes, BGRAGraphics, BGRAClasses, BGRAUTF8, 
 BGRAGradientScanner, main_mfm;

procedure drawTree(x1, y1, angle, depth, multiplier: single; bgra : tbgrabitmap);
var
  x2, y2: single;
begin
  if (depth > 0) then
  begin
    x2 := x1 + (cos(angle * deg_to_rad) * depth * multiplier);
    y2 := y1 + (sin(angle * deg_to_rad) * depth * multiplier);
 
    bgra.DrawLineAntialias(x1, y1, x2, y2, BGRABlack, depth, False);
    
    // Use even values without randomness to get a 'real' fractal image
    
    drawTree(x2, y2, angle - randomrange(15,50), depth - 1.44, multiplier, bgra);
    drawTree(x2, y2, angle + randomrange(10,25), depth - 0.72, multiplier, bgra);
    drawTree(x2, y2, angle - randomrange(10,25), depth - 3, multiplier, bgra);
    drawTree(x2, y2, angle + randomrange(15,50), depth - 4, multiplier, bgra);
  end;
end;       
  
procedure tmainfo.tpaintbox1_onpaint(const sender: twidget; const acanvas: tcanvas);
const 
  SampleText = 'HellÔ green tree';
  MarginX = 20;
  MarginY = 20;
var
  bmp : tbgrabitmap;
  multiplier : single;
  grad: TBGRACustomGradient;
  scan: TBGRACustomScanner;
  textWidth: integer;
begin
  bmp := tbgrabitmap.create(sender.bounds_cx, sender.bounds_cy);
  
  multiplier := sender.bounds_cy / 50;
 
  bmp.GradientFill(0, 0, bmp.Width, bmp.Height, cl_dkgreen, BGRA(0,125,0), 
  gtLinear, PointF(0,0), PointF(0, bmp.Height), dmSet);

  drawTree(bmp.Width div 2, bmp.Height, -91, 9, multiplier, bmp); 
  
  bmp.FontFullHeight := 30;
  textWidth := bmp.TextSize(SampleText).cx;
  bmp.TextOut(MarginX, MarginY, SampleText, BGRAWhite);
  bmp.HorizLine(MarginX, MarginY, MarginX + bmp.TextSize(SampleText).cx-1, BGRAWhite);
  bmp.HorizLine(MarginX, MarginY + bmp.FontFullHeight, MarginX + textWidth-1, BGRAWhite);
  
  bmp.FontFullHeight := 40;
  bmp.FontStyle := [fsBold];
  textWidth := bmp.TextSize(SampleText).cx;
  grad := TBGRAHueGradient.Create(0,0,65535,40000,[hgoRepeat,hgoPositiveDirection,hgoLightnessCorrection]);
  scan := TBGRAGradientScanner.Create(grad, gtLinear, PointF(0, bmp.Height - MarginY), PointF(0, bmp.Height - MarginY - textWidth));
  bmp.TextOutAngle(bmp.Width - MarginX - bmp.FontFullHeight, bmp.Height - MarginY, 900, SampleText, scan);
  scan.Free;
  grad.Free;
  
  bmp.Rectangle(0, 0, bmp.Width, bmp.Height, BGRABlack);

  bmp.Rectangle(5, 5, bmp.Width - 5, bmp.Height - 5, BGRABlack);

  bmp.draw(acanvas, 0, 0, false);
  bmp.free;
end;

end.
