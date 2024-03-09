program testcore;

uses BGRABitmap, BGRABitmapTypes, BGRACanvas, BGRACanvas2D, SysUtils;

var bmp: TBGRABitmap;
  canvas: TBGRACanvas;
  canvas2d: TBGRACanvas2D;
  filename: String;

begin
  bmp := TBGRABitmap.Create(400, 400, CSSSilver);
  canvas := TBGRACanvas.Create(bmp);
  with canvas do
  begin
    Brush.BGRAColor := CSSGreen;
    FillRect(100, 100, 300, 300);
    Free;
  end;
  canvas2d := TBGRACanvas2D.Create(bmp);
  with canvas2d do
  begin
    shadowBlur:= 5;
    shadowOffset:= PointF(5, 5);
    shadowColor(CSSBlack);
    circle(200,200,50);
    fillStyle(CSSBlue);
    fill;
    free;
  end;
  filename := ExtractFilePath(paramStr(0))+'square.png';
  bmp.SaveToFile(filename);
  bmp.Free;
end.

