program TestWin32Outline;
{$mode objfpc}{$H+}

uses
  {$IFDEF LCLwin32}Interfaces, Graphics, Types,{$ENDIF}
  SysUtils, BGRABitmapTypes, BGRABitmap, BGRAPath
  {$IFDEF LCLwin32}, BGRAWin32Text{$ENDIF};

{$IFDEF LCLwin32}
var
  failures: integer = 0;

procedure Check(ACond: boolean; const AMsg: string);
begin
  if ACond then writeln('  ok   ', AMsg)
  else begin writeln('  FAIL ', AMsg); inc(failures); end;
end;

function Coverage(ABitmap: TBGRABitmap): double;
var i: integer;
begin
  result := 0;
  for i := 0 to ABitmap.NbPixels-1 do result += ABitmap.Data[i].alpha/255;
end;

// bounds of the pixels that are mostly opaque
function RasterBounds(ABitmap: TBGRABitmap): TRect;
var x, y: integer;
begin
  result := Rect(ABitmap.Width, ABitmap.Height, -1, -1);
  for y := 0 to ABitmap.Height-1 do
    for x := 0 to ABitmap.Width-1 do
      if ABitmap.GetPixel(x, y).alpha >= 128 then
      begin
        if x < result.Left then result.Left := x;
        if y < result.Top then result.Top := y;
        if x > result.Right then result.Right := x;
        if y > result.Bottom then result.Bottom := y;
      end;
end;

procedure TestCharacter(AFont: TFont; const AText: string);
var
  path: TBGRAPath;
  fromOutline, fromGdi: TBGRABitmap;
  b: TRectF;
  r: TRect;
  covOutline, covGdi: double;
begin
  writeln('character ', AText, ' (', AFont.Name, ')');
  path := TBGRAPath.Create;
  Check(BGRAWin32TextPath(AFont, AText, path), 'outline is provided');
  Check(not path.IsEmpty, 'outline is not empty');

  fromOutline := TBGRABitmap.Create(160, 160, BGRAPixelTransparent);
  path.fill(fromOutline, BGRABlack);
  fromGdi := TBGRABitmap.Create(160, 160, BGRAPixelTransparent);
  fromGdi.FontName := AFont.Name;
  fromGdi.FontHeight := -AFont.Height;
  fromGdi.FontStyle := AFont.Style;
  fromGdi.FontQuality := fqFineAntialiasing;
  fromGdi.TextOut(0, 0, AText, BGRABlack);

  covOutline := Coverage(fromOutline);
  covGdi := Coverage(fromGdi);
  Check((covGdi > 100) and (abs(covOutline/covGdi - 1) < 0.04),
    'same amount of ink as the text drawn by GDI ('+FloatToStrF(covOutline,ffFixed,8,1)+' / '+FloatToStrF(covGdi,ffFixed,8,1)+')');

  // same position: the top-left corner of the text is at (0,0)
  b := path.GetBounds;
  r := RasterBounds(fromGdi);
  Check((abs(b.Left - r.Left) <= 2) and (abs(b.Right - (r.Right+1)) <= 2),
    'same horizontal position as the text drawn by GDI');
  Check((abs(b.Top - r.Top) <= 2) and (abs(b.Bottom - (r.Bottom+1)) <= 2),
    'same vertical position as the text drawn by GDI');
  fromOutline.Free; fromGdi.Free; path.Free;
end;

procedure TestRefused(AFont: TFont; const AText, AReason: string);
var path: TBGRAPath;
begin
  path := TBGRAPath.Create;
  Check(not BGRAWin32TextPath(AFont, AText, path), AReason);
  Check(path.IsEmpty, AReason + ': the path is left unchanged');
  path.Free;
end;

var
  font: TFont;
  path: TBGRAPath;
begin
  font := TFont.Create;
  font.Name := 'Arial';
  font.Height := -100;
  font.Style := [];

  TestCharacter(font, 'O');
  TestCharacter(font, 'g');
  TestCharacter(font, #$C3#$A9); // e acute

  font.Style := [fsBold, fsItalic];
  TestCharacter(font, 'A');
  font.Style := [];
  font.Name := 'Times New Roman';
  TestCharacter(font, 'R');
  font.Name := 'Arial';

  // a blank character has an empty outline
  writeln('space');
  path := TBGRAPath.Create;
  Check(BGRAWin32TextPath(font, ' ', path) and path.IsEmpty, 'space: provided and empty');
  path.Free;

  writeln('characters that are not handled');
  TestRefused(font, '', 'empty text');
  TestRefused(font, 'AB', 'several characters');
  TestRefused(font, 'e'#$CC#$81, 'character with a combining mark');
  TestRefused(font, #$D7#$90, 'Hebrew letter (right-to-left)');
  TestRefused(font, #$EE#$80#$80, 'character that is not in the font (private use)');
  TestRefused(font, #$F0#$9F#$98#$80, 'character outside of the Basic Multilingual Plane');

  font.Free;
  writeln;
  if failures = 0 then writeln('ALL TESTS PASSED')
  else begin writeln(failures, ' FAILURE(S)'); halt(1); end;
{$ELSE}
begin
  writeln('SKIPPED: this test needs the LCL for Windows (compile with lazbuild)');
{$ENDIF}
end.
