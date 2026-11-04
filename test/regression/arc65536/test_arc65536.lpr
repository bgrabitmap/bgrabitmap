program TestArc65536;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  SysUtils, Math, BGRABitmapTypes, BGRAPath;

procedure Fail(const AMsg: string);
begin
  raise Exception.Create(AMsg);
end;

procedure CheckPoint(const APt: TPointF; AX, AY: single; const AContext: string);
begin
  if (Abs(APt.x - AX) > 0.05) or (Abs(APt.y - AY) > 0.05) then
    Fail(AContext + ': expected (' + FloatToStr(AX) + ', ' + FloatToStr(AY) +
      ') but got (' + FloatToStr(APt.x) + ', ' + FloatToStr(APt.y) + ')');
end;

procedure CheckPassesNear(const APts: ArrayOfTPointF; AX, AY: single; const AContext: string);
var
  i: Integer;
  best: single;
begin
  best := 1e10;
  for i := 0 to High(APts) do
    best := Min(best, VectLen(APts[i] - PointF(AX, AY)));
  if best > 0.5 then
    Fail(AContext + ': no point near (' + FloatToStr(AX) + ', ' + FloatToStr(AY) + ')');
end;

procedure CheckOnCircle(const APts: ArrayOfTPointF; ACenter: TPointF;
  ARadius: single; const AContext: string);
var
  i: Integer;
begin
  for i := 0 to High(APts) do
    if Abs(VectLen(APts[i] - ACenter) - ARadius) > 0.05 then
      Fail(AContext + ': point ' + IntToStr(i) + ' is not on the circle');
end;

var
  Pts: ArrayOfTPointF;
begin
  // 65536 units make a full turn: 16384 is a quarter of a turn,
  // counter-clockwise on a screen where y goes down
  Pts := ComputeArc65536(PointF(0, 0), PointF(100, 0), PointF(0, 100), 0, 16384, 10);
  CheckPoint(Pts[0], 100, 0, 'quarter turn, first point');
  CheckPoint(Pts[High(Pts)], 0, -100, 'quarter turn, last point');
  CheckPassesNear(Pts, 100 * Cos(Pi / 4), -100 * Sin(Pi / 4), 'quarter turn, middle');
  CheckOnCircle(Pts, PointF(0, 0), 100, 'quarter turn');

  // origin and axes
  Pts := ComputeArc65536(PointF(50, 20), PointF(80, 20), PointF(50, 50), 0, 32768, 10);
  CheckPoint(Pts[0], 80, 20, 'half turn around (50,20), first point');
  CheckPoint(Pts[High(Pts)], 20, 20, 'half turn around (50,20), last point');
  CheckOnCircle(Pts, PointF(50, 20), 30, 'half turn around (50,20)');

  // an arc that goes through the origin of the angles
  Pts := ComputeArc65536(PointF(0, 0), PointF(100, 0), PointF(0, 100), 49152, 0, 10);
  CheckPoint(Pts[0], 0, 100, 'three quarters to zero, first point');
  CheckPoint(Pts[High(Pts)], 100, 0, 'three quarters to zero, last point');
  CheckOnCircle(Pts, PointF(0, 0), 100, 'three quarters to zero');

  // same arc as with the other version of the function
  Pts := ComputeArc65536(0, 0, 100, 100, 0, 16384, 10);
  CheckPoint(Pts[High(Pts)], 0, -100, 'center and radius version, last point');

  WriteLn('PASS: ComputeArc65536 with an origin and two axis points covers the right angle');
end.
