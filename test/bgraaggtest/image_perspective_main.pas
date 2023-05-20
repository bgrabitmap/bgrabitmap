unit image_perspective_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, BGRABitmap, BGRABitmapTypes, LMessages, EpikTimer;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Radio_InterpBox: TRadioButton;
    Radio_InterpLinear: TRadioButton;
    Radio_InterpHalfCosine: TRadioButton;
    Radio_InterpCosine: TRadioButton;
    Radio_Perspective: TRadioButton;
    Radio_LinearAntialias: TRadioButton;
    Radio_Linear: TRadioButton;
    Radio_AffineAntialias: TRadioButton;
    Radio_Affine: TRadioButton;
    Radio_PerspectiveAntialias: TRadioButton;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    { private declarations }
    procedure FormPaint(Sender: TObject);
    procedure RadioButtonChange(Sender: TObject);
    procedure WMEraseBkgnd(var {%H-}Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  public
    { public declarations }
    MovingPointIndex: Integer;
    MovingOrigin: TPointF;
    pts: array[0..3] of TPointF;
    image: TBGRABitmap;
    stopwatch: TEpikTimer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure NicePoint(bmp: TBGRABitmap; x, y: single; scale: single = 1);
begin
    bmp.EllipseAntialias(x,y,4*scale,4*scale,BGRA(0,0,0,192),scale);
    bmp.EllipseAntialias(x,y,3*scale,3*scale,BGRA(255,255,255,192),scale);
    bmp.EllipseAntialias(x,y,2*scale,2*scale,BGRA(0,0,0,192),scale);
end;

{ TForm1 }

procedure TForm1.FormPaint(Sender: TObject);
var bmp: TBGRABitmap;
    tx,ty,i: Integer;
    texPos, scaledPts: array of TPointF;
    scale: double;
begin
  tx := ClientWidth;
  ty := clientHeight;

  if Radio_InterpBox.Checked then
    image.ScanInterpolationFilter := rfBox else
  if Radio_InterpLinear.Checked then
    image.ScanInterpolationFilter := rfLinear else
  if Radio_InterpHalfCosine.Checked then
    image.ScanInterpolationFilter := rfHalfCosine else
  if Radio_InterpCosine.Checked then
    image.ScanInterpolationFilter := rfCosine;

  If Radio_Affine.Checked or Radio_AffineAntialias.Checked then
    pts[2] := pts[1]+(pts[3]-pts[0]);

  scale := GetCanvasScaleFactor;
  bmp := TBGRABitmap.Create(round(tx*scale),round(ty*scale),BGRAWhite);
  setLength({%H-}scaledPts, length(pts));
  for i := 0 to high(pts) do scaledPts[i] := scale*pts[i];

  stopwatch.clear;
  stopwatch.start;

  texPos := PointsF([PointF(0,0),PointF(image.width-1,0),
              PointF(image.width-1,image.Height-1),PointF(0,image.Height-1)]);
  if Radio_Perspective.Checked or Radio_PerspectiveAntialias.Checked then
  begin
    if Radio_PerspectiveAntialias.Checked then
      bmp.FillQuadPerspectiveMappingAntialias(scaledPts[0],scaledPts[1],scaledPts[2],scaledPts[3], image,
                texPos[0],texPos[1],texPos[2],texPos[3])
    else
      bmp.FillQuadPerspectiveMapping(scaledPts[0],scaledPts[1],scaledPts[2],scaledPts[3], image,
              texPos[0],texPos[1],texPos[2],texPos[3]);
  end else
  if Radio_LinearAntialias.Checked then
  begin
    bmp.FillQuadLinearMappingAntialias(scaledPts[0],scaledPts[1],scaledPts[2],scaledPts[3], image,
        texPos[0],texPos[1],texPos[2],texPos[3]);
  end
  else if Radio_Linear.Checked then
  begin
    bmp.FillQuadLinearMapping(scaledPts[0],scaledPts[1],scaledPts[2],scaledPts[3], image,
        texPos[0],texPos[1],texPos[2],texPos[3], true, fcNone, false);
  end
  else if Radio_Affine.Checked then
  begin
    bmp.FillQuadAffineMapping(scaledPts[0],scaledPts[1],scaledPts[3],image);
  end
  else if Radio_AffineAntialias.checked then
  begin
    bmp.FillQuadAffineMappingAntialias(scaledPts[0],scaledPts[1],scaledPts[3],image);
  end;

  stopwatch.stop;
  //bmp.DrawPolygonAntialias(scaledPts,BGRA(0,0,0,64),scale);
  bmp.FontHeight:= round(bmp.FontHeight*scale);
  bmp.textOut(0,0,inttostr(round(stopwatch.Elapsed*1000))+' ms',BGRABlack);

  for i := 0 to 3 do
    NicePoint(bmp,scaledPts[i].x,scaledPts[i].y, scale);
  bmp.Draw(Canvas,rect(0,0,tx,ty));

  bmp.free;
end;

procedure TForm1.RadioButtonChange(Sender: TObject);
begin
  invalidate;
end;

procedure TForm1.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  //
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  appPath: String;
begin
  pts[0] := PointF(50,50);
  pts[1] := PointF(clientwidth-150,50);
  pts[2] := PointF(clientwidth-150,clientheight-150);
  pts[3] := PointF(120,clientheight-200);
  MovingPointIndex := -1;
  appPath := ExtractFilePath(ParamStr(0));
  image := TBGRABitmap.Create(appPath+'spheres.png');
  stopwatch := TEpikTimer.Create(Self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  image.free;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var maxDist,dist: single;
    mousePos,vect: TPointF;
    i: Integer;
begin
  if Button <> mbLeft then exit;

  //select point to move
  MovingPointIndex := -1;
  maxDist := 10;
  mousePos := PointF(X,Y);
  MovingOrigin := mousePos;

  for i := 0 to high(pts) do
  begin
    vect := pts[i] - mousePos;
    dist := sqrt(vect*vect);
    if dist < maxDist then
    begin
      maxDist := dist;
      MovingPointIndex := i;
    end;
  end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  mousePos: TPointF;
  i: Integer;
begin
  if ssLeft in Shift then
  begin
    mousePos := PointF(X,Y);
    if MovingPointIndex <> -1 then
      pts[MovingPointIndex].Offset(mousePos-MovingOrigin) else
    begin
      for i := 0 to high(pts) do
        pts[i].Offset(mousePos-MovingOrigin);
    end;
    Invalidate;
    MovingOrigin := mousePos;
  end;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then MovingPointIndex := -1;
end;

end.

