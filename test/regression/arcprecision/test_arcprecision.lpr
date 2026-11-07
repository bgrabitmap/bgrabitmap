program TestArcPrecision;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  SysUtils, Math, BGRABitmapTypes, BGRAPath, BGRATransform;

// Issue #371: arc points must not be quantized by the integer sine table.

procedure Fail(const AMsg: string);
begin
  raise Exception.Create(AMsg);
end;

function PolyLen(const APts: ArrayOfTPointF): double;
var i: integer;
begin
  result := 0;
  for i := 1 to high(APts) do result += VectLen(APts[i] - APts[i-1]);
end;

procedure CheckArc(AAntiClockwise: boolean; AQuality: single);
var
  arc: TArcDef;
  pts: ArrayOfTPointF;
  i: integer;
  len, expected: double;
begin
  arc.center := PointF(0, 0);
  arc.radius := PointF(100, 100);
  arc.xAngleRadCW := 0;
  if AAntiClockwise then
  begin
    arc.startAngleRadCW := 0; arc.endAngleRadCW := -Pi/2; arc.anticlockwise := true;
  end else
  begin
    arc.startAngleRadCW := -Pi/2; arc.endAngleRadCW := 0; arc.anticlockwise := false;
  end;
  pts := ComputeArc(arc, AQuality);
  for i := 0 to high(pts) do
    if Abs(VectLen(pts[i]) - 100) > 0.0005 then
      Fail('quality ' + FloatToStr(AQuality) + ': point ' + IntToStr(i) + ' is off the circle');
  len := PolyLen(pts);
  expected := 100*Pi/2;
  if (len > expected + 1e-4) or (len < expected - 0.01*(10/AQuality)-1e-3) then
    Fail('quality ' + FloatToStr(AQuality) + ': length ' + FloatToStr(len) + ' instead of ' + FloatToStr(expected));
  if (VectLen(pts[0] - PointF(IfThen(AAntiClockwise,100,0), IfThen(AAntiClockwise,0,-100))) > 1e-3) then
    Fail('wrong first point');
end;

var
  path, path2: TBGRAPath;
  pts: ArrayOfTPointF;
begin
  CheckArc(false, 10);
  CheckArc(false, 100);
  CheckArc(false, 1000);
  CheckArc(true, 100);
  CheckArc(true, 1000);

  // ellipse perimeter (Ramanujan: 857.8422 for 200x50)
  pts := ComputeEllipse(0, 0, 200, 50, 100);
  if Abs(PolyLen(pts) - 857.8422) > 0.01 then
    Fail('ellipse length ' + FloatToStr(PolyLen(pts)));
  pts := ComputeEllipse(0, 0, 100, 100, 1000);
  if Abs(PolyLen(pts) - 200*Pi) > 0.01 then
    Fail('circle length ' + FloatToStr(PolyLen(pts)));

  // CopyTo with a large scale and a small deviation
  path := TBGRAPath.Create;
  path2 := TBGRAPath.Create;
  path.arc(0, 0, 1, 0, Pi/2, false);
  path.copyTo(path2, AffineMatrixScale(100, 100), 0.001);
  if Abs(path2.ComputeLength(0.001) - 100*Pi/2) > 0.02 then
    Fail('copyTo length ' + FloatToStr(path2.ComputeLength(0.001)));
  path2.Free;
  path.Free;

  WriteLn('PASS: ComputeArc points are not quantized');
end.
