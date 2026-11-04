program TestPathStream;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  SysUtils, Classes, BGRABitmapTypes, BGRABitmap, BGRAPath;

var
  failures: integer = 0;

procedure Check(ACond: boolean; const AMsg: string);
begin
  if ACond then writeln('  ok   ', AMsg)
  else begin writeln('  FAIL ', AMsg); inc(failures); end;
end;

// shape with a hole, lines and cubic/quadratic curves, one open sub-path
procedure BuildShape(p: TBGRAPath);
begin
  p.beginPath;
  // outer contour: clockwise, curved on top
  p.moveTo(10, 40);
  p.lineTo(10, 20);
  p.bezierCurveTo(10, 5, 50, 5, 50, 20);
  p.quadraticCurveTo(55, 40, 40, 55);
  p.lineTo(10, 55);
  p.closePath;
  // hole: counter-clockwise
  p.moveTo(20, 30);
  p.lineTo(20, 45);
  p.lineTo(35, 45);
  p.lineTo(35, 30);
  p.closePath;
end;

function SamePixels(a, b: TBGRABitmap): boolean;
var i: integer;
begin
  result := (a.Width = b.Width) and (a.Height = b.Height);
  if result then
    for i := 0 to a.NbPixels-1 do
      if a.Data[i] <> b.Data[i] then begin result := false; exit; end;
end;

function Render(p: TBGRAPath): TBGRABitmap;
begin
  result := TBGRABitmap.Create(64, 64, BGRAPixelTransparent);
  p.fill(result, BGRABlack);
end;

function Coverage(b: TBGRABitmap): integer;
var i: integer;
begin
  result := 0;
  for i := 0 to b.NbPixels-1 do inc(result, b.Data[i].alpha);
end;

// signed area of the first sub-path of ToPoints (stops at the empty point)
function FirstSubPathArea(const pts: ArrayOfTPointF): single;
var i, n: integer;
begin
  n := 0;
  while (n < length(pts)) and not isEmptyPointF(pts[n]) do inc(n);
  result := 0;
  for i := 0 to n-1 do
    result += pts[i].x*pts[(i+1) mod n].y - pts[(i+1) mod n].x*pts[i].y;
  result := result/2;
end;

procedure TestReverse;
var
  p, r, rr: TBGRAPath;
  bp, br: TBGRABitmap;
  ap, ar: single;
begin
  writeln('copyToReversed');
  p := TBGRAPath.Create; r := TBGRAPath.Create; rr := TBGRAPath.Create;
  BuildShape(p);
  p.copyToReversed(r);
  r.copyToReversed(rr);
  Check(r.SvgString <> p.SvgString, 'reversed path differs from the original');
  Check(rr.SvgString = p.SvgString, 'reversing twice gives the original');
  ap := FirstSubPathArea(p.ToPoints);
  ar := FirstSubPathArea(r.ToPoints);
  Check((ap <> 0) and (abs(ap + ar) < abs(ap)*0.01), 'first sub-path orientation is inverted ('+FloatToStr(ap)+' / '+FloatToStr(ar)+')');
  bp := Render(p); br := Render(r);
  Check(Coverage(bp) > 500, 'shape is rendered');
  Check(SamePixels(bp, br), 'same rendering, hole preserved');
  bp.Free; br.Free;
  p.Free; r.Free; rr.Free;

  // open sub-path and arc
  p := TBGRAPath.Create; r := TBGRAPath.Create; rr := TBGRAPath.Create;
  p.moveTo(5, 5);
  p.lineTo(20, 5);
  p.arc(20, 20, 15, -Pi/2, 0);
  p.lineTo(35, 50);
  p.copyToReversed(r);
  r.copyToReversed(rr);
  Check(rr.SvgString = p.SvgString, 'open path with arc: reversing twice gives the original');
  Check(r.SvgString <> p.SvgString, 'open path with arc: reversed differs');
  p.Free; r.Free; rr.Free;

  // two sub-paths: the second one starts after a close without moveTo
  p := TBGRAPath.Create; r := TBGRAPath.Create; rr := TBGRAPath.Create;
  p.polygon([PointF(0,0), PointF(10,0), PointF(10,10)]);
  p.lineTo(20, 20);
  p.lineTo(30, 10);
  p.copyToReversed(r);
  r.copyToReversed(rr);
  bp := Render(p); br := Render(r);
  Check(Coverage(bp) > 50, 'two sub-paths are rendered');
  Check(SamePixels(bp, br), 'implicit sub-path after close: same rendering when reversed');
  bp.Free; br.Free;
  bp := Render(p); br := Render(rr);
  Check(SamePixels(bp, br), 'implicit sub-path after close: same rendering after reversing twice');
  bp.Free; br.Free;
  p.Free; r.Free; rr.Free;

  // splines are flattened but must keep the shape
  p := TBGRAPath.Create; r := TBGRAPath.Create;
  p.closedSpline([PointF(10,10), PointF(50,10), PointF(50,50), PointF(10,50)], ssOutside);
  p.copyToReversed(r);
  bp := Render(p); br := Render(r);
  Check(Coverage(bp) > 500, 'closed spline is rendered');
  Check(abs(Coverage(bp) - Coverage(br)) < Coverage(bp)*0.02, 'closed spline: same coverage when reversed ('+IntToStr(Coverage(bp))+' / '+IntToStr(Coverage(br))+')');
  bp.Free; br.Free; p.Free; r.Free;

  // empty path
  p := TBGRAPath.Create; r := TBGRAPath.Create;
  p.copyToReversed(r);
  Check(r.IsEmpty, 'empty path stays empty');
  p.Free; r.Free;
end;

procedure TestStream;
var
  p, q: TBGRAPath;
  s: TMemoryStream;
  b: PByte;
  bp, bq: TBGRABitmap;
  raised: boolean;
begin
  writeln('SaveToStream / LoadFromStream');
  // exact byte layout, little endian: moveTo(1,2) lineTo(1,2.5)
  p := TBGRAPath.Create;
  p.moveTo(1, 2);
  p.lineTo(1, 2.5);
  s := TMemoryStream.Create;
  p.SaveToStream(s);
  b := PByte(s.Memory);
  Check(s.Size = 1+8+1+8+1, 'size is 19 bytes (got '+IntToStr(s.Size)+')');
  Check((b[0] = 1) and (b[1] = 0) and (b[2] = 0) and (b[3] = $80) and (b[4] = $3F)
    and (b[5] = 0) and (b[6] = 0) and (b[7] = 0) and (b[8] = $40), 'moveTo(1,2) is little endian');
  Check((b[9] = 2) and (b[10] = 0) and (b[11] = 0) and (b[12] = $80) and (b[13] = $3F)
    and (b[14] = 0) and (b[15] = 0) and (b[16] = $20) and (b[17] = $40) and (b[18] = 0),
    'lineTo(1,2.5) is little endian and terminated');
  s.Free; p.Free;

  // round trip with all kinds of elements
  p := TBGRAPath.Create;
  BuildShape(p);
  p.moveTo(5, 5);
  p.lineTo(20, 5);
  p.arc(20, 20, 15, -Pi/2, 0);
  p.openedSpline([PointF(30,30), PointF(40,35), PointF(45,50), PointF(55,55)], ssInside);
  p.moveTo(2, 60);
  p.closedSpline([PointF(10,10), PointF(50,10), PointF(50,50), PointF(10,50)], ssOutside);
  p.rotateDeg(30); // the transformation is not saved
  s := TMemoryStream.Create;
  p.SaveToStream(s);
  s.Position := 0;
  q := TBGRAPath.Create;
  q.moveTo(99, 99);
  q.LoadFromStream(s);
  Check(s.Position = s.Size, 'whole stream consumed');
  Check(q.SvgString = p.SvgString, 'same SvgString after round trip');
  bp := Render(p); bq := Render(q);
  Check(SamePixels(bp, bq), 'same rendering after round trip');
  bp.Free; bq.Free;
  // the loaded path can still be modified and closed
  q.lineTo(1, 1);
  q.closePath;
  Check(not q.IsEmpty, 'loaded path can be extended');
  q.Free; s.Free; p.Free;

  // empty path
  p := TBGRAPath.Create; s := TMemoryStream.Create;
  p.SaveToStream(s);
  Check(s.Size = 1, 'empty path is one byte');
  s.Position := 0;
  q := TBGRAPath.Create;
  q.lineTo(1, 1);
  q.LoadFromStream(s);
  Check(q.IsEmpty, 'loading an empty path clears the path');
  p.Free; q.Free; s.Free;

  // invalid data
  s := TMemoryStream.Create;
  s.WriteByte(200);
  s.Position := 0;
  q := TBGRAPath.Create;
  raised := false;
  try q.LoadFromStream(s); except raised := true; end;
  Check(raised, 'unknown element type raises an exception');
  s.Free;
  s := TMemoryStream.Create;
  s.WriteByte(1); s.WriteByte(0); s.WriteByte(0); // truncated moveTo
  s.Position := 0;
  raised := false;
  try q.LoadFromStream(s); except raised := true; end;
  Check(raised, 'truncated stream raises an exception');
  s.Free; q.Free;
end;

begin
  TestReverse;
  TestStream;
  writeln;
  if failures = 0 then writeln('ALL TESTS PASSED')
  else begin writeln(failures, ' FAILURE(S)'); halt(1); end;
end.
