unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, BGRABitmapTypes, BGRABitmap, BGRACanvas2D;

type

  { TForm1 }

  TForm1 = class(TForm)
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    procedure FloatSpinEdit1Change(Sender: TObject);
    procedure FloatSpinEdit2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
  private
    Center: TPoint;
    B, B2: TRationalQuadraticBezierCurve;
    CurPoint: integer;
    PrevMouse: TPoint;
    Img : TBGRABitmap;
    procedure UpdateLength;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  B:=BezierCurve(PointF(-150,80), PointF(0,0), PointF(150,80), FloatSpinEdit1.Value);
  B2:=BezierCurve(PointF(-150,80), PointF(0,0), PointF(150,80), -FloatSpinEdit1.Value);
  UpdateLength;
  Img := TBGRABitmap.Create;
  CurPoint := -1;
end;

procedure TForm1.FloatSpinEdit1Change(Sender: TObject);
begin
  B.weight := FloatSpinEdit1.Value;
  B2.weight := -FloatSpinEdit1.Value;
  UpdateLength;
  invalidate;
end;

procedure TForm1.FloatSpinEdit2Change(Sender: TObject);
begin
  Invalidate;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Img.free
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MinDist: single;

  function TryPoint(APoint: TPointF): boolean;
  var
    dist: single;
  begin
    dist:= sqr(APoint.x-x)+sqr(APoint.y-y);
    if dist < MinDist then
    begin
      MinDist := dist;
      exit(true)
    end
    else exit(false);
  end;

begin
  dec(y, Center.Y);
  dec(x, Center.X);
  if Button = mbLeft then
  begin
    CurPoint:= -1;
    MinDist := sqr(15);
    if TryPoint(B.p1) then CurPoint := 0;
    if TryPoint(B.c) then CurPoint := 1;
    if TryPoint(B.p2) then CurPoint := 2;
    PrevMouse := Point(X,Y);
  end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  d: TPointF;
begin
  dec(y, Center.Y);
  dec(x, Center.X);
  if CurPoint <> -1 then
  begin
    d := PointF(X-PrevMouse.x,Y-PrevMouse.y);
    case CurPoint of
    0: begin B.p1 += d; B2.p1 += d; end;
    1: begin B.c += d; B2.c += d; end;
    2: begin B.p2 += d; B2.p2 += d; end;
    end;
    PrevMouse := Point(X,Y);
    UpdateLength;
    Invalidate;
  end;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then CurPoint := -1;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  f: TBGRACanvas2D;
  R, boundsF: TrectF;
  Aleft, Aright : TRationalQuadraticBezierCurve;
  precision: single;
begin
  precision := FloatSpinEdit2.Value;
  Img.SetSize(ClientWidth,ClientHeight-Panel1.Height);
  Img.Fill(clWhite);
  f := Img.Canvas2D;
  Center := Point(ClientWidth div 2, (ClientHeight - Panel1.Height) div 2 + Panel1.Height);
  boundsF := RectF(0,0, Img.Width,Img.Height);
  boundsF.Offset(-Center.X, -Center.Y + Panel1.Height);
  f.resetTransform;
  f.translate(Center.X,Center.Y - Panel1.Height);
  f.lineJoinLCL:= pjsBevel;
  // arc d'ellipse en rouge, poids 0.4 (petit arc)
  f.beginPath;
  f.moveto(B.p1);
  f.lineTo(B.c);
  f.lineTo(B.p2);
  f.moveto(B2.p1);
  f.lineTo(B2.c);
  f.lineTo(B2.p2);
  f.moveto(B.p1.x+5,B.p1.y);
  f.circle(B.p1.x,B.p1.y,5);
  f.moveto(B.c.x+5,B.c.y);
  f.circle(B.c.x,B.c.y,5);
  f.moveto(B.p2.x+5,B.p2.y);
  f.circle(B.p2.x,B.p2.y,5);
  f.strokeStyle(clblack);
  f.linewidth := 1;
  f.stroke();
  f.beginPath;
  f.lineWidth := 4;
  f.strokeStyle(BGRA(255,0,96,255));
  f.moveTo(B.p1);
  f.polylineTo(B.ToPoints(boundsF,precision));
  f.stroke();
  // arc d'ellipse en vert, poids -0.4 (grand arc, complétant le précédent)
  f.beginPath;
  f.strokeStyle(BGRA(96,160,0,255));
  f.polylineTo(B2.ToPoints(boundsF,precision));
  f.stroke();
  if not B2.IsInfinite then
  begin
    // arc en bleu, c'est la deuxième moitié de l'arc en vert
    B2.Split(Aleft, Aright);
    f.strokeStyle(BGRA(0,96,255,255));
    f.beginPath;
    f.moveTo(Aright.p1);
    f.polylineTo(Aright.ToPoints(boundsF,precision*2));
    f.stroke;

    // bounding box de l'arc en vert
    R:=B2.GetBounds();
    f.beginPath;
    f.rect(round(R.Left)-1, round(R.Top)-1, round(R.Width)+2, round(R.Height)+2);
    f.strokeStyle(BGRABlack);
    f.lineWidth := 1;
    f.stroke();
  end;
  Img.draw(Canvas,0,Panel1.Height)
end;

procedure TForm1.UpdateLength;
var
  len: Single;
begin
  len := B2.ComputeLength;
  if len = EmptySingle then
    Label1.caption:='Green arc length = infinity'
  else
    Label1.caption:='Green arc length = '+FloatToStrF(len, ffFixed, 7,1);
  len := B.ComputeLength;
  if len = EmptySingle then
    Label2.caption:='Red arc length = infinity'
  else
    Label2.caption:='Red arc length = '+FloatToStrF(len, ffFixed, 7,1);
end;

end.

