program TestPathGlyph;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  SysUtils, Classes, BGRABitmapTypes, BGRABitmap, BGRATransform, BGRAPath, BGRATypewriter;

var
  failures: integer = 0;

procedure Check(ACond: boolean; const AMsg: string);
begin
  if ACond then writeln('  ok   ', AMsg)
  else begin writeln('  FAIL ', AMsg); inc(failures); end;
end;

// an "O": outer square clockwise, inner square (hole) counter-clockwise,
// with a curved side, in glyph units (height 1)
function CreateOutline: TBGRAPath;
begin
  result := TBGRAPath.Create;
  result.moveTo(0.1, 0.1);
  result.lineTo(0.7, 0.1);
  result.bezierCurveTo(0.9, 0.1, 0.9, 0.9, 0.7, 0.9);
  result.lineTo(0.1, 0.9);
  result.closePath;
  result.moveTo(0.3, 0.3);
  result.lineTo(0.3, 0.7);
  result.lineTo(0.6, 0.7);
  result.lineTo(0.6, 0.3);
  result.closePath;
end;

function Coverage(APath: TBGRAPath; AWidth, AHeight: integer): integer;
var
  b: TBGRABitmap;
  i: integer;
begin
  b := TBGRABitmap.Create(AWidth, AHeight, BGRAPixelTransparent);
  try
    APath.fill(b, BGRABlack);
    result := 0;
    for i := 0 to b.NbPixels-1 do inc(result, b.Data[i].alpha);
  finally
    b.Free;
  end;
end;

function SamePixels(a, b: TBGRABitmap): boolean;
var i: integer;
begin
  result := (a.Width = b.Width) and (a.Height = b.Height);
  if result then
    for i := 0 to a.NbPixels-1 do
      if a.Data[i] <> b.Data[i] then begin result := false; exit; end;
end;

function RenderGlyph(AGlyph: TBGRAGlyph; const AMatrix: TAffineMatrix; AReverse: boolean): TBGRABitmap;
var p: TBGRAPath;
begin
  result := TBGRABitmap.Create(80, 80, BGRAPixelTransparent);
  p := TBGRAPath.Create;
  AGlyph.Path(p, AMatrix, AReverse);
  p.fill(result, BGRABlack);
  p.Free;
end;

procedure TestGlyph;
var
  g, g2: TBGRAPathGlyph;
  m, mMirror: TAffineMatrix;
  a, b: TBGRABitmap;
  s: TMemoryStream;
  loaded: TBGRAGlyph;
begin
  writeln('TBGRAPathGlyph');
  g := TBGRAPathGlyph.Create('O');
  g.SetOutline(CreateOutline);
  g.Width := 0.8;
  g.Height := 1;
  m := AffineMatrixTranslation(5, 5)*AffineMatrixScale(60, 60);
  mMirror := AffineMatrixTranslation(5, 5)*AffineMatrixScale(60, 60)
             *AffineMatrixTranslation(g.Width, 0)*AffineMatrixScale(-1, 1);

  a := RenderGlyph(g, m, false);
  b := RenderGlyph(g, m, true);
  Check(a.GetPixel(5+round(0.2*60), 5+round(0.5*60)).alpha = 255, 'glyph is drawn');
  Check(a.GetPixel(5+round(0.45*60), 5+round(0.5*60)).alpha = 0, 'hole is empty');
  Check(SamePixels(a, b), 'reversed glyph (same matrix): same rendering, hole kept');
  a.Free; b.Free;

  a := RenderGlyph(g, mMirror, true);
  b := RenderGlyph(g, mMirror, false);
  Check(SamePixels(a, b), 'mirrored glyph: same rendering with or without reversing');
  a.Free; b.Free;

  // stream round trip
  s := TMemoryStream.Create;
  g.SaveToStream(s);
  s.Position := 0;
  loaded := TBGRAGlyph.LoadFromStream(s);
  Check(s.Position = s.Size, 'whole stream consumed');
  Check(loaded is TBGRAPathGlyph, 'loaded glyph is a TBGRAPathGlyph');
  g2 := loaded as TBGRAPathGlyph;
  Check((g2.Width = g.Width) and (g2.Height = g.Height), 'width and height are kept');
  a := RenderGlyph(g, m, false);
  b := RenderGlyph(g2, m, false);
  Check(SamePixels(a, b), 'same rendering after loading');
  a.Free; b.Free;
  a := RenderGlyph(g, mMirror, true);
  b := RenderGlyph(g2, mMirror, true);
  Check(SamePixels(a, b), 'same mirrored rendering after loading');
  a.Free; b.Free;
  g2.Free; s.Free;
  g.Free;
end;

procedure TestOverlap;
var
  g: TBGRAPathGlyph;
  outline: TBGRAPath;
  dest: TBGRAPath;
  cover1, coverRev, coverMirrorNoRev: integer;
  mirrorFlip: TAffineMatrix;
begin
  writeln('overlapping mirrored glyphs');
  g := TBGRAPathGlyph.Create('B');
  outline := TBGRAPath.Create;
  outline.polygon([PointF(0,0), PointF(40,0), PointF(40,40), PointF(0,40)]);
  g.SetOutline(outline);
  g.Width := 40; g.Height := 40;
  // an unmirrored copy and a mirrored one over it
  mirrorFlip := AffineMatrixTranslation(30+40, 0)*AffineMatrixScale(-1, 1);

  dest := TBGRAPath.Create;
  g.Path(dest, AffineMatrixTranslation(0, 0), false);
  cover1 := Coverage(dest, 80, 50);
  g.Path(dest, mirrorFlip, true);
  coverRev := Coverage(dest, 80, 50);
  dest.Free;

  dest := TBGRAPath.Create;
  g.Path(dest, AffineMatrixTranslation(0, 0), false);
  g.Path(dest, mirrorFlip, false);
  coverMirrorNoRev := Coverage(dest, 80, 50);
  dest.Free;

  Check(coverRev > cover1, 'second copy adds its area ('+IntToStr(cover1)+' -> '+IntToStr(coverRev)+')');
  Check(coverRev > coverMirrorNoRev, 'without restoring the direction the overlap would be dug ('+IntToStr(coverMirrorNoRev)+' < '+IntToStr(coverRev)+')');
  g.Free;
end;

begin
  TestGlyph;
  TestOverlap;
  writeln;
  if failures = 0 then writeln('ALL TESTS PASSED')
  else begin writeln(failures, ' FAILURE(S)'); halt(1); end;
end.
