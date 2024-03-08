program testcore;

uses BGRABitmap, BGRABitmapTypes, BGRACanvas, SysUtils;

var bmp: TBGRABitmap;
  canvas: TBGRACanvas;
  filename: String;

begin
  bmp := TBGRABitmap.Create(400, 400, CSSSilver);
  canvas := TBGRACanvas.Create(bmp);
  canvas.Brush.BGRAColor := CSSGreen;
  canvas.FillRect(100, 100, 300, 300);
  canvas.Free;
  filename := ExtractFilePath(paramStr(0))+'square.png';
  bmp.SaveToFile(filename);
  bmp.Free;
end.

