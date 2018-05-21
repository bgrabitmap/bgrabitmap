unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes;

const
  ballRadius = 20;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Panel1: TPanel;
    SpinEdit1: TSpinEdit;
    VirtualScreen: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure FormDestroy(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure InitBalls(ACount: integer);
    function GetBallRect(AIndex: integer): TRect;
  public
    background: TBGRABitmap;
    balls: array of record
          ballPos: TPoint;
          ballSpeed: TPoint;
          ballColor: TBGRAPixel;
      end;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses BGRAGradients;

{ TForm1 }

procedure TForm1.VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  i: Integer;
begin
  Bitmap.FillRect(rect(0,0,Bitmap.Width,Bitmap.Height), background, dmSet);
  for i := 0 to high(balls) do
    with balls[i] do
      Bitmap.EllipseAntialias(ballPos.x,ballPos.y, ballRadius,ballRadius, BGRABlack, 1, ballColor);
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  initBalls(SpinEdit1.Value);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  background.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  initBalls(SpinEdit1.Value);
  background := CreateCyclicPerlinNoiseMap(256,256);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var r: TRect;
  i: Integer;
  rects: array of TRect;
  nbRects: integer;
begin
  Timer1.Enabled:= false;
  setlength(rects, length(balls)*2);
  nbRects := 0;

  for i := 0 to high(balls) do
  with balls[i] do
  begin
    //rect to clear
    rects[nbRects] := GetBallRect(i);
    inc(nbRects);

    ballSpeed.Y += 1;
    ballPos.X += ballSpeed.x;
    ballPos.Y += ballSpeed.y;
    if BallPos.Y >= VirtualScreen.Height-ballRadius then
    begin
      ballPos.Y := VirtualScreen.Height-ballRadius;
      ballSpeed.Y := -abs(ballSpeed.Y);

      if (BallPos.X < -ballRadius) or (BallPos.X > VirtualScreen.Width+ballRadius) then
      begin
        ballPos := Point(random(VirtualScreen.Width), - ballRadius);
        ballSpeed.Y := 0;
        Continue;
      end;
    end else
    begin
      if BallPos.X+ballRadius >= VirtualScreen.Width then
      begin
        BallPos.X := VirtualScreen.Width-ballRadius;
        ballSpeed.X := -abs(ballSpeed.x);
      end else
      if BallPos.X <= ballRadius then
      begin
        BallPos.X := ballRadius;
        ballSpeed.X := abs(ballSpeed.x);
      end;
    end;

    //rect to redraw
    rects[nbRects] := GetBallRect(i);
    inc(nbRects);
  end;
  if CheckBox1.Checked then
    VirtualScreen.RedrawBitmap
  else
    VirtualScreen.RedrawBitmap(slice(rects,nbRects));
  Timer1.Enabled:= true;
end;

procedure TForm1.InitBalls(ACount: integer);
var
  i: Integer;
begin
  randomize;
  VirtualScreen.DiscardBitmap;
  setlength(balls, ACount);
  for i := 0 to high(balls) do
  with balls[i] do
  begin
    ballPos := Point(random(VirtualScreen.Width), (i*VirtualScreen.Height div length(balls)) - ballRadius);
    ballSpeed := Point(random(5)-2, 0);
    ballColor := BGRA(random(256),random(256),random(256));
    if ballColor.Lightness > 48000 then ballColor.Lightness:= 48000;
  end;
end;

function TForm1.GetBallRect(AIndex: integer): TRect;
begin
  with balls[AIndex] do
    result := Rect(ballPos.X-ballRadius, ballPos.Y-ballRadius,
                   ballPos.X+ballRadius+1, ballPos.Y+ballRadius+1);
end;

end.

