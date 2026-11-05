program TestQt5Outline;
{$mode objfpc}{$H+}

uses
  {$IFDEF LCLqt5}Interfaces, Graphics,{$ENDIF}
  SysUtils, BGRABitmapTypes, BGRABitmap, BGRAPath
  {$IFDEF LCLqt5}, BGRAQt5Text{$ENDIF};

{$IFDEF LCLqt5}
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
  fromOutline, fromQt: TBGRABitmap;
  covOutline, covQt, ov: double;
begin
  writeln(ADescription, ' (', AFont.Name, ')');
  path := TBGRAPath.Create;
  Check(BGRAQt5TextPath(AFont, AText, path), 'outline is provided');
  Check(not path.IsEmpty, 'outline is not empty');

  fromOutline := TBGRABitmap.Create(1000, 200, BGRAPixelTransparent);
  path.fill(fromOutline, BGRABlack);
  fromQt := TBGRABitmap.Create(1000, 200, BGRAPixelTransparent);
  fromQt.FontName := AFont.Name;
  fromQt.FontHeight := -AFont.Height;
  fromQt.FontStyle := AFont.Style;
  fromQt.FontQuality := fqFineAntialiasing;
  fromQt.TextOut(0, 0, AText, BGRABlack);

  covOutline := Coverage(fromOutline);
  covQt := Coverage(fromQt);
  ov := Overlap(fromOutline, fromQt);
  Check((covQt > 100) and (abs(covOutline/covQt - 1) < 0.06),
    'same amount of ink as the text drawn by Qt ('+FloatToStrF(covOutline,ffFixed,8,1)+' / '+FloatToStrF(covQt,ffFixed,8,1)+')');
  Check(ov > 0.8, 'same shapes and position as the text drawn by Qt (overlap '+FloatToStrF(ov,ffFixed,6,3)+')');
  fromOutline.Free; fromQt.Free; path.Free;
end;

procedure TestRefused(AFont: TFont; const AText, AReason: string);
var path: TBGRAPath;
begin
  path := TBGRAPath.Create;
  Check(not BGRAQt5TextPath(AFont, AText, path), AReason);
  Check(path.IsEmpty, AReason + ': the path is left unchanged');
  path.Free;
end;

var
  font: TFont;
begin
  font := TFont.Create;
  font.Name := 'DejaVu Sans';
  font.Height := -100;
  font.Style := [];

  TestText(font, 'O', 'letter');
  TestText(font, 'g', 'letter with a descender');
  TestText(font, #$C3#$A9, 'precomposed accented letter');
  TestText(font, 'Hello', 'word');
  TestText(font, 'AVAVTo', 'pairs of letters');
  TestText(font, 'e'#$CC#$81, 'letter and combining accent');
  TestText(font, #$D9#$85#$D8#$B1#$D8#$AD#$D8#$A8#$D8#$A7, 'Arabic (joined letters)');
  font.Style := [fsBold, fsItalic];
  TestText(font, 'A', 'bold italic letter');
  font.Style := [];
  font.Name := 'Liberation Serif';
  TestText(font, 'Ray', 'serif font');

  TestRefused(font, '', 'empty text');
  TestRefused(font, 'a'#10'b', 'text with a line break');

  font.Free;
  if failures > 0 then
  begin
    writeln(failures, ' check(s) failed');
    halt(1);
  end;
  writeln('PASS: Qt5 outlines');
{$ELSE}
begin
  writeln('SKIP: Qt5 only');
{$ENDIF}
end.
