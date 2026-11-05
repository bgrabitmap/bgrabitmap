program test_svgrect;

{$mode objfpc}{$H+}

uses
  {$ifdef UNIX}cthreads, cwstring,{$endif}
  SysUtils, BGRABitmap, BGRABitmapTypes, BGRACanvas2D, BGRASVG,
  BGRASVGType, BGRATransform;

var
  Checks, Failures: integer;

procedure Check(OK: boolean; const Name: string);
begin
  Inc(Checks);
  if not OK then
  begin
    Inc(Failures);
    WriteLn('FAIL ', Name);
  end;
end;

function Document(const Radii: string): TBGRASVG;
begin
  result := TBGRASVG.CreateFromString('<svg xmlns="http://www.w3.org/2000/svg" ' +
    'width="200" height="100" viewBox="0 0 200 100">' +
    '<rect x="20" y="10" width="160" height="80" fill="red" ' + Radii + '/></svg>');
  result.DefaultDpi := 96;
end;

procedure Draw(svg: TBGRASVG; ctx: TBGRACanvas2D; CopyPath: boolean);
begin
  if CopyPath then
  begin
    ctx.beginPath;
    svg.Content.Element[0].CopyPathTo(ctx, cuPixel);
    ctx.fillStyle('red');
    ctx.fill;
  end
  else
    svg.Draw(ctx, 0, 0);
end;

function SamePixels(a, b: TBGRABitmap): boolean;
var
  x, y: integer;
begin
  for y := 0 to a.Height - 1 do
    for x := 0 to a.Width - 1 do
      if a.GetPixel(x, y) <> b.GetPixel(x, y) then exit(false);
  result := true;
end;

procedure RadiusCase(const Name, ActualRadii, ExpectedRadii: string; Rounded: boolean);
var
  actual, expected: TBGRASVG;
  a, b: TBGRABitmap;
  ca, cb: TBGRACanvas2D;
  mode, pass: integer;
  prefix, original: string;
begin
  actual := Document(ActualRadii);
  expected := Document(ExpectedRadii);
  a := TBGRABitmap.Create(200, 100);
  b := TBGRABitmap.Create(200, 100);
  ca := TBGRACanvas2D.Create(a);
  cb := TBGRACanvas2D.Create(b);
  try
    original := actual.AsUTF8String;
    for mode := 0 to 1 do
      for pass := 1 to 2 do
      begin
        prefix := Name + ' mode ' + IntToStr(mode) + ' pass ' + IntToStr(pass);
        a.Fill(BGRAPixelTransparent);
        b.Fill(BGRAPixelTransparent);
        Draw(actual, ca, mode = 1);
        Draw(expected, cb, mode = 1);
        Check(SamePixels(a, b), prefix + ' matches explicit radii');
        Check(a.GetPixel(100, 50) = BGRA(255, 0, 0), prefix + ' center filled');
        if Rounded then
          Check(a.GetPixel(20, 10) = BGRAPixelTransparent, prefix + ' rounded corner')
        else
          Check(a.GetPixel(20, 10) = BGRA(255, 0, 0), prefix + ' square corner');
        Check(ca.matrix = AffineMatrixIdentity, prefix + ' restores transform');
        Check(actual.AsUTF8String = original, prefix + ' leaves attributes unchanged');
      end;
  finally
    cb.Free;
    ca.Free;
    b.Free;
    a.Free;
    expected.Free;
    actual.Free;
  end;
end;

begin
  RadiusCase('rx only', 'rx="16"', 'rx="16" ry="16"', true);
  RadiusCase('ry only', 'ry="16"', 'rx="16" ry="16"', true);
  RadiusCase('both radii', 'rx="24" ry="12"', 'rx="24" ry="12"', true);
  RadiusCase('neither radius', '', 'rx="0" ry="0"', false);
  RadiusCase('explicit zero ry', 'rx="16" ry="0"', 'rx="0" ry="0"', false);
  RadiusCase('explicit zero rx', 'rx="0" ry="16"', 'rx="0" ry="0"', false);
  RadiusCase('zero rx only', 'rx="0"', 'rx="0" ry="0"', false);
  RadiusCase('zero ry only', 'ry="0"', 'rx="0" ry="0"', false);

  // Use a nonsquare viewport: resolve percentages on the specified axis
  // before using that length for the omitted radius.
  RadiusCase('percentage rx', 'rx="10%"', 'rx="20" ry="20"', true);
  RadiusCase('percentage ry', 'ry="10%"', 'rx="10" ry="10"', true);
  RadiusCase('both percentages', 'rx="10%" ry="10%"', 'rx="20" ry="10"', true);

  RadiusCase('clamped rx', 'rx="120"', 'rx="80" ry="40"', true);
  RadiusCase('clamped ry', 'ry="120"', 'rx="80" ry="40"', true);
  RadiusCase('physical rx', 'rx="0.25in"', 'rx="24" ry="24"', true);
  RadiusCase('physical ry', 'ry="0.25in"', 'rx="24" ry="24"', true);

  WriteLn(Checks, ' checks, ', Failures, ' failures');
  Halt(Ord(Failures <> 0));
end.
