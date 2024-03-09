program testcore;

uses BGRABitmap, BGRABitmapTypes, BGRACanvas, BGRACanvas2d, BGRAVectorize, SysUtils;

var bmp: TBGRABitmap;
  canvas: TBGRACanvas;
  canvas2d: TBGRACanvas2D;
  appDir: String;

begin
  appDir := ExtractFilePath(paramStr(0));

  // create image with solid background
  bmp := TBGRABitmap.Create(400, 400, CSSSilver);

  // draw rectangle
  canvas := TBGRACanvas.Create(bmp);
  with canvas do
  begin
    Brush.BGRAColor := CSSGreen;
    FillRect(100, 100, 300, 300);
    Free;
  end;

  // draw text
  bmp.FontRenderer := TBGRAVectorizedFontRenderer.Create(appDir);
  bmp.FontName:= 'Arial';
  bmp.FontFullHeight:= 30;
  bmp.TextOut(0,0, 'Hello World WORLD Vamos', CSSBlack);

  // draw a disk with shadow
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

  bmp.SaveToFile(appDir + 'square.png');
  bmp.Free;
end.

