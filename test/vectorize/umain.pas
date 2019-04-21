unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BCPanel, BCLabel, BCTrackbarUpdown, BGRAVirtualScreen, BGRABitmap,
  BGRABitmapTypes;

type

  { TFMain }

  TFMain = class(TForm)
    BCLabel1: TBCLabel;
    BCLabel2: TBCLabel;
    BCPanel1: TBCPanel;
    BCTrackbarZoom: TBCTrackbarUpdown;
    BCTrackbarDiagonal: TBCTrackbarUpdown;
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    CheckBoxIntermediate: TCheckBox;
    procedure BCTrackbarChange(Sender: TObject; AByUser: boolean);
    procedure BGRAVirtualScreen1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BGRAVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure CheckBoxIntermediateChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

    function GetRawBitmap: TBGRABitmap;
    function GetPosAt(X,Y: Integer): integer;
  public
    pos: array of TPoint;
    pointMoving: integer;
    pointMoveMouseStart, pointMoveCoordStart: TPoint;
  end;

var
  FMain: TFMain;

implementation

uses BGRAVectorize, BGRATransform;

{$R *.lfm}

{ TFMain }

procedure TFMain.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  bmp: TBGRABitmap;
  pts: ArrayOfTPointF;
  zoom, i: Integer;
  radius: single;
  m: TAffineMatrix;
begin
  bmp := GetRawBitmap;
  zoom := BCTrackbarZoom.Value;
  Bitmap.StretchPutImage(rect(0,0,bmp.Width*zoom,bmp.Height*zoom), bmp, dmSet);
  if zoom >= 4 then
  begin
    for i := 1 to bmp.Height-1 do
      Bitmap.HorizLine(0,i*zoom-1,bmp.Width*zoom-1,BGRA(144,144,144,128), dmDrawWithTransparency);
    for i := 1 to bmp.Width-1 do
      Bitmap.VertLine(i*zoom-1,0,bmp.Height*zoom-1,BGRA(144,144,144,128), dmDrawWithTransparency);
  end;
  pts := VectorizeMonochrome(bmp, 1, true, true, BCTrackbarDiagonal.Value, CheckBoxIntermediate.Checked);

  bmp.FillTransparent;
  bmp.FillPolyAntialias(pts, BGRA(0,128,0,128));
  Bitmap.StretchPutImage(rect(0,0,bmp.Width*zoom,bmp.Height*zoom), bmp, dmDrawWithTransparency);

  m := AffineMatrixTranslation(-0.5,-0.5)*AffineMatrixScale(zoom,zoom)*AffineMatrixTranslation(0.5,0.5);
  for i := 0 to high(pts) do pts[i] := m * pts[i];

  Bitmap.DrawPolygonAntialias(pts, CSSRed, 2);
  if zoom > 6 then
  begin
    radius := zoom/6;
    if radius < 2.5 then radius := 2.5;
    for i := 0 to high(pts) do
      Bitmap.EllipseAntialias(pts[i].x,pts[i].y, radius,radius, BGRA(0,0,0,192),1,CSSOrange);
  end;
 for i := 0 to high(pos) do
  begin
    Bitmap.EllipseAntialias(pos[i].x*zoom,pos[i].y*zoom,8,8,BGRABlack,4);
    Bitmap.EllipseAntialias(pos[i].x*zoom,pos[i].y*zoom,8,8,BGRAWhite,2);
  end;
  bmp.Free;
end;

procedure TFMain.CheckBoxIntermediateChange(Sender: TObject);
begin
  BGRAVirtualScreen1.DiscardBitmap;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  setlength(pos, 4);
  pos[0] := Point(10,10);
  pos[1] := Point(60,10);
  pos[2] := Point(10,20);
  pos[3] := Point(60,60);
  pointMoving := -1;
end;

procedure TFMain.BCTrackbarChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser then BGRAVirtualScreen1.DiscardBitmap;
end;

procedure TFMain.BGRAVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  idx: Integer;
begin
  if Button = mbLeft then
  begin
    idx := GetPosAt(X,Y);
    pointMoving := idx;
    pointMoveMouseStart := Point(X,Y);
    pointMoveCoordStart := pos[idx];
    BGRAVirtualScreen1.Cursor := crHandPoint;
  end;

end;

procedure TFMain.BGRAVirtualScreen1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  idx, zoom: Integer;
  newPos: TPoint;
begin
  if pointMoving <> -1 then
  begin
    zoom := BCTrackbarZoom.Value;
    newPos := Point(pointMoveCoordStart.x + round((X-pointMoveMouseStart.x)/zoom),
                    pointMoveCoordStart.y + round((Y-pointMoveMouseStart.y)/zoom));
    if (newPos.X <> pos[pointMoving].x) or (newPos.Y <> pos[pointMoving].y) then
    begin
      pos[pointMoving] := newPos;
      BGRAVirtualScreen1.DiscardBitmap;
    end;
  end else
  begin
    idx := GetPosAt(X,Y);
    if idx <> -1 then BGRAVirtualScreen1.Cursor := crHandPoint else BGRAVirtualScreen1.Cursor := crDefault;
  end;
end;

procedure TFMain.BGRAVirtualScreen1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then pointMoving:= -1;
end;

function TFMain.GetRawBitmap: TBGRABitmap;
var
  zoom: Integer;
begin
  zoom := BCTrackbarZoom.Value;
  result := TBGRABitmap.Create(BGRAVirtualScreen1.Width div zoom,BGRAVirtualScreen1.Height div zoom, BGRAWhite);
  result.FillEllipseInRect(rect(pos[2].x,pos[2].y,pos[3].x,pos[3].y), BGRABlack, dmSet);
  result.DrawLine(pos[0].x,pos[0].y,pos[1].x,pos[1].y, BGRABlack, true);
end;

function TFMain.GetPosAt(X, Y: Integer): integer;
var
  minDist,dist, i, zoom: integer;
begin
  minDist := sqr(8);
  result := -1;
  zoom := BCTrackbarZoom.Value;
  for i := 0 to high(pos) do
  begin
    dist := sqr(X-pos[i].X*zoom)+sqr(Y-pos[i].Y*zoom);
    if dist < minDist then
    begin
      minDist := dist;
      result := i;
    end;
  end;
end;

end.

