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

// how much two shapes overlap: 1 for identical shapes, 0 for disjoint shapes
function Overlap(A, B: TBGRABitmap): double;
var i: integer;
  inter, union: double;
begin
  inter := 0; union := 0;
  for i := 0 to A.NbPixels-1 do
  begin
    if A.Data[i].alpha < B.Data[i].alpha then
    begin
      inter += A.Data[i].alpha/255;
      union += B.Data[i].alpha/255;
    end else
    begin
      inter += B.Data[i].alpha/255;
      union += A.Data[i].alpha/255;
    end;
  end;
  if union = 0 then result := 1 else result := inter/union;
end;

procedure TestText(AFont: TFont; const AText, ADescription: string);
var
  path: TBGRAPath;
  fromOutline, fromGdi: TBGRABitmap;
  covOutline, covGdi, ov: double;
begin
  writeln(ADescription, ' (', AFont.Name, ')');
  path := TBGRAPath.Create;
  Check(BGRAWin32TextPath(AFont, AText, path), 'outline is provided');
  Check(not path.IsEmpty, 'outline is not empty');

  fromOutline := TBGRABitmap.Create(900, 160, BGRAPixelTransparent);
  path.fill(fromOutline, BGRABlack);
  fromGdi := TBGRABitmap.Create(900, 160, BGRAPixelTransparent);
  fromGdi.FontName := AFont.Name;
  fromGdi.FontHeight := -AFont.Height;
  fromGdi.FontStyle := AFont.Style;
  fromGdi.FontQuality := fqFineAntialiasing;
  fromGdi.TextOut(0, 0, AText, BGRABlack);

  covOutline := Coverage(fromOutline);
  covGdi := Coverage(fromGdi);
  ov := Overlap(fromOutline, fromGdi);
  Check((covGdi > 100) and (abs(covOutline/covGdi - 1) < 0.04),
    'same amount of ink as the text drawn by GDI ('+FloatToStrF(covOutline,ffFixed,8,1)+' / '+FloatToStrF(covGdi,ffFixed,8,1)+')');
  // same shapes at the same position (the edges are antialiased differently,
  // a text that is shifted, for example without the kerning, is far below)
  Check(ov > 0.8, 'same shapes and position as the text drawn by GDI (overlap '+FloatToStrF(ov,ffFixed,6,3)+')');
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

  // one character
  TestText(font, 'O', 'letter');
  TestText(font, 'g', 'letter with a descender');
  TestText(font, #$C3#$A9, 'precomposed accented letter');

  // several characters, which GetGlyphOutline cannot do
  TestText(font, 'Hello', 'word');
  TestText(font, 'AVAVTo', 'pairs of letters');
  TestText(font, 'fi', 'f and i (ligature)');
  TestText(font, 'e'#$CC#$81, 'letter and combining accent');
  TestText(font, #$D7#$A9#$D7#$9C#$D7#$95#$D7#$9D, 'Hebrew (right-to-left)');
  TestText(font, #$D9#$85#$D8#$B1#$D8#$AD#$D8#$A8#$D8#$A7, 'Arabic (joined letters)');
  TestText(font, #$E2#$80#$8D#$D9#$85#$E2#$80#$8D, 'Arabic letter in its medial form (joiners)');
  // these characters are not in Arial: GDI uses another font
  TestText(font, #$E6#$BC#$A2#$E5#$AD#$97, 'ideographs (font fallback)');

  font.Style := [fsBold, fsItalic];
  TestText(font, 'A', 'bold italic letter');
  font.Style := [];
  font.Name := 'Times New Roman';
  TestText(font, 'Ray', 'serif font');
  font.Name := 'Segoe UI';
  TestText(font, 'Text xW', 'font with overlapping contours');
  font.Name := 'Arial';

  // a blank text has an empty outline
  writeln('space');
  path := TBGRAPath.Create;
  Check(BGRAWin32TextPath(font, '  ', path) and path.IsEmpty, 'spaces: provided and empty');
  path.Free;

  writeln('texts that are refused');
  TestRefused(font, '', 'empty text');
  TestRefused(font, 'a'#10'b', 'text with a line break');

  font.Free;
  writeln;
  if failures = 0 then writeln('ALL TESTS PASSED')
  else begin writeln(failures, ' FAILURE(S)'); halt(1); end;
{$ELSE}
begin
  writeln('SKIPPED: this test needs the LCL for Windows (compile with lazbuild)');
{$ENDIF}
end.
