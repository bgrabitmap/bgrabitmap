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

procedure ExpectedEnd(const APt: TPointF; AAngle: single);
var x, y, rx, ry, ex, ey: single;
begin
  x := 10; y := 20; rx := 100; ry := 50;
  ex := x + cos(AAngle)*rx;
  ey := y - sin(AAngle)*ry;
  if (APt.x <> ex) or (APt.y <> ey) then
    Fail('end point is not computed exactly');
end;

// Full turns must not be mistaken for tiny arcs after angle normalization.
procedure CheckFullTurnPoints(const APts: ArrayOfTPointF; AStart, AEnd: single);
var I: Integer; Expected: TPointF; EX, EY: Single;
begin
  if Length(APts) < 5 then Fail('full turn collapsed to two points');
  if Abs(PolyLen(APts) - 200*Pi) > 0.01 then Fail('incorrect full-turn length');
  for I := 0 to High(APts) do
    if Abs(VectLen(APts[I] - PointF(10,20)) - 100) > 0.0005 then
      Fail('full-turn point is off the circle');
  EX := 10 + cos(AStart)*100; EY := 20 - sin(AStart)*100;
  Expected := PointF(EX,EY);
  if (APts[0].x <> Expected.x) or (APts[0].y <> Expected.y) then
    Fail('full turn lost its exact start point');
  EX := 10 + cos(AEnd)*100; EY := 20 - sin(AEnd)*100;
  Expected := PointF(EX,EY);
  if (APts[High(APts)].x <> Expected.x) or (APts[High(APts)].y <> Expected.y) then
    Fail('full turn lost its exact end point');
end;

procedure CheckFullTurns;
const Starts: array[0..3] of Single = (0, 0.3, -1.7, 6.283185307179586);
var I, Turns, Count: Integer; A, B: Single; Arc: TArcDef;
begin
  Count := 0;
  for I := Low(Starts) to High(Starts) do
    for Turns := -2 to 2 do
    begin
      A := Starts[I]; B := A + Turns*2*Pi;
      CheckFullTurnPoints(ComputeArcRad(10,20,100,100,A,B,10), A, B);
      Inc(Count);
    end;
  // The public path API changes signs and order for its clockwise convention.
  Arc.center := PointF(10,20); Arc.radius := PointF(100,100);
  Arc.xAngleRadCW := 0;
  for I := 0 to 1 do
    for Turns := -1 to 1 do
    begin
      Arc.anticlockwise := I <> 0;
      Arc.startAngleRadCW := 0; Arc.endAngleRadCW := Turns*2*Pi;
      if Abs(PolyLen(ComputeArc(Arc,10)) - 200*Pi) > 0.01 then
        Fail('ComputeArc full turn collapsed');
      Inc(Count);
    end;
  WriteLn('PASS: ',Count,' full-turn cases (signed turns, offsets, both directions)');
end;

procedure CheckTinyArcs;
const Starts: array[0..2] of Single = (0, 0.3, -1.7);
var I, Direction: Integer; A, B: Single; Pts: ArrayOfTPointF;
begin
  for I := Low(Starts) to High(Starts) do
    for Direction := -1 to 1 do
      if Direction <> 0 then
      begin
        A := Starts[I]; B := A + Direction*0.000001;
        Pts := ComputeArcRad(0,0,100,100,A,B,10);
        if Length(Pts) <> 2 then Fail('tiny arc became a full circle');
        if PolyLen(Pts) > 0.001 then Fail('tiny arc is too long');
      end;
  WriteLn('PASS: 6 nonzero tiny arcs remain tiny');
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

  // end points use the same computation as any connected shape
  pts := ComputeArcRad(10, 20, 100, 50, 0.3, 2.1, 1000);
  ExpectedEnd(pts[0], 0.3);
  ExpectedEnd(pts[high(pts)], 2.1);

  CheckFullTurns;
  CheckTinyArcs;
  WriteLn('PASS: ComputeArc points are not quantized');
end.
