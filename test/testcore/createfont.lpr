program createfont;

uses Interfaces, BGRABitmap, BGRAVectorize, SysUtils;

var
  f: TBGRAVectorizedFont;

begin
  f := TBGRAVectorizedFont.Create;
  f.Name:= 'Arial';
  f.FullHeight:= 30;
  f.NeedAsciiRange(true);
  f.SaveGlyphsToFile(ExtractFilePath(ParamStr(0))+'arial.glyphs');
  f.Free;
end.

