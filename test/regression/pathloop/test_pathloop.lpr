program TestPathLoop;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  SysUtils, BGRABitmapTypes, BGRAPath;

procedure CheckInside(const APt: TPointF; const AContext: string);
begin
  if IsEmptyPointF(APt) or (APt.x < -0.01) or (APt.x > 10.01) or
     (APt.y < -0.01) or (APt.y > 10.01) then
    raise Exception.Create(AContext + ': cursor left the first shape at (' +
      FloatToStr(APt.x) + ', ' + FloatToStr(APt.y) + ')');
end;

procedure CheckLoop(APath: TBGRAPath; const AName: string);
var
  Cursor: TBGRAPathCursor;
  I: Integer;
begin
  Cursor := APath.CreateCursor;
  try
    Cursor.LoopClosedShapes := True;
    // the first shape is a triangle of perimeter ~34: go around it several times
    for I := 1 to 12 do
    begin
      Cursor.MoveForward(10);
      CheckInside(Cursor.CurrentCoordinate, AName + ', forward step ' + IntToStr(I));
    end;
    for I := 1 to 12 do
    begin
      Cursor.MoveBackward(10);
      CheckInside(Cursor.CurrentCoordinate, AName + ', backward step ' + IntToStr(I));
    end;
  finally
    Cursor.Free;
  end;
end;

var
  Path: TBGRAPath;
begin
  Path := TBGRAPath.Create;
  try
    Path.moveTo(0, 0); Path.lineTo(10, 0); Path.lineTo(10, 10); Path.closePath;
    CheckLoop(Path, 'closed shape alone');
    // a closed shape followed by other elements must keep its loop position
    Path.moveTo(50, 50); Path.lineTo(60, 50); Path.lineTo(60, 60); Path.closePath;
    CheckLoop(Path, 'closed shape followed by another one');
  finally
    Path.Free;
  end;
  WriteLn('PASS: cursor loops over a closed shape followed by other shapes');
end.
