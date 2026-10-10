program TestPointOperators;
{$mode objfpc}{$H+}
uses SysUtils, BGRABitmapTypes;
var A, B, Scaled: TPointF; Dot: Single;
procedure Require(Value: Boolean; const Description: string);
begin
  if not Value then raise Exception.Create(Description);
end;
procedure CheckProduct(Value: Single); overload;
begin
  Require(Abs(Value - 23) < 0.0001, 'Legacy scalar product *');
  WriteLn('Point-by-point * returns a scalar');
end;
procedure CheckProduct(const Value: TPointF); overload;
begin
  Require((Value.x = 8) and (Value.y = 15), 'Component-wise product *');
  WriteLn('Point-by-point * returns a point');
end;
begin
  try
    A := PointF(2, 3);
    B := PointF(4, 5);
    Dot := A ** B;
    Require(Abs(Dot - 23) < 0.0001, 'Scalar product **');
    // FPC branches have supplied different result types at the same version.
    CheckProduct(A * B);
    Scaled := A * 2;
    Require((Scaled.x = 4) and (Scaled.y = 6), 'Point multiplied by scalar');
    Scaled := 2 * A;
    Require((Scaled.x = 4) and (Scaled.y = 6), 'Scalar multiplied by point');
    WriteLn('PASS: point operators with FPC ', {$I %FPCVERSION%});
  except
    on E: Exception do begin WriteLn('FAIL: ', E.Message); Halt(1); end;
  end;
end.
