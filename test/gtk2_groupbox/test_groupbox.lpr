program ProbeGroupBox;
{$mode objfpc}{$H+}
uses Interfaces, Forms, Controls, StdCtrls, ExtCtrls, Graphics, Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes;
type
  TMarker = class(TGraphicControl)
    procedure Paint; override;
  end;
var DrawMode: Integer;
procedure TMarker.Paint;
var B: TBGRABitmap;
begin
  Canvas.Brush.Color := clBlue;
  Canvas.FillRect(Rect(0,0,30,30));
  if DrawMode=3 then B := TBGRABitmap.Create(15,15,BGRA(255,0,0))
  else B := TBGRABitmap.Create(30,30,BGRA(255,0,0));
  try
    case DrawMode of
      0: begin
        B.Draw(Canvas,40,0,False);
        B.Draw(Canvas,80,0,True);
      end;
      1: begin
        B.DataDrawTransparent(Canvas,Rect(40,0,70,30),B.Data,B.LineOrder,30,30);
        B.DataDrawOpaque(Canvas,Rect(80,0,110,30),B.Data,B.LineOrder,30,30);
      end;
      2: begin
        B.DrawPart(Rect(0,0,30,30),Canvas,40,0,False);
        B.DrawPart(Rect(0,0,30,30),Canvas,80,0,True);
      end;
      3: begin
        B.Draw(Canvas,Rect(40,0,70,30),False);
        B.Draw(Canvas,Rect(80,0,110,30),True);
      end;
    end;
  finally B.Free; end;
end;
var
  F: TForm;
  G: array[0..5] of TCustomGroupBox;
  M: array[0..13] of TMarker;
  Shot: TBGRABitmap;
  P, Base: TPoint;
  I, X, Y, Failures: Integer;
  C: TBGRAPixel;
begin
  RequireDerivedFormResource := False;
  DrawMode := StrToIntDef(ParamStr(2),0);
  Application.Initialize;
  F := TForm.Create(nil);
  F.SetBounds(30,30,650,720);
  F.Color := clWhite;
  for I := 0 to 5 do
  begin
        case I div 2 of
      0: G[I] := TGroupBox.Create(F);
      1: G[I] := TRadioGroup.Create(F);
      2: G[I] := TCheckGroup.Create(F);
    end;
    G[I].Parent := F;
    G[I].SetBounds(10+(I mod 2)*315,130+(I div 2)*180,305,170);
    if I mod 2=0 then TGroupBox(G[I]).Caption := 'Caption';
  end;
  for I := 0 to 13 do
  begin
    M[I] := TMarker.Create(F);
    if I<2 then M[I].Parent:=F else M[I].Parent:=G[(I-2) div 2];
    M[I].SetBounds(5,(I mod 2)*60,120,35);
  end;
  F.Show;
  for I:=1 to 20 do begin Application.ProcessMessages; Sleep(20); end;
  Base:=F.ClientToScreen(Point(0,0));
  Shot := TBGRABitmap.Create;
  Failures := 0;
  try
    Shot.TakeScreenshot(Rect(Base.X,Base.Y,Base.X+F.ClientWidth,Base.Y+F.ClientHeight));
    Shot.SaveToFile(ParamStr(1));
    for I:=0 to 13 do
    begin
      for X:=0 to 2 do
      begin
        case X of
          0..2: P:=M[I].ClientToScreen(Point(X*40+10,0));
        end;
        for Y:=5 to 24 do
        begin
          C:=Shot.GetPixel(P.X-Base.X,P.Y-Base.Y+Y);
          if ((X=0) and (C<>BGRA(0,0,255))) or
             ((X in [1,2]) and (C<>BGRA(255,0,0))) then Inc(Failures);
        end;
      end;
    end;
    WriteLn('pixel mismatches=', Failures);
  finally Shot.Free; F.Free; end;
  if Failures>0 then Halt(1);
end.
