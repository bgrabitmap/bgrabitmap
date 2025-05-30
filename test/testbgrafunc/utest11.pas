unit utest11;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utest, Graphics, BGRABitmap, BGRABitmapTypes, BGRAGrayscaleMask;

const
  nbPoints = 6;

type
  { TTest11 }

  TTest11 = class(TTest)
  protected
    virtualScreen: TBGRABitmap;
    mask: TGrayscaleMask;
    pts: array of TPointF;
    dirs: array of TPointF;
    FFilter: string;

  public
    constructor Create(filter: string);
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  end;

implementation

{ TTest11 }

constructor TTest11.Create(filter: string);
begin
  inherited Create;
  Name := 'Antialiased lines and splines';
  if filter <> '' then Name := Name + ' with filter '+filter;
  randomize;
  virtualScreen := nil;
  FFilter := filter;
end;

destructor TTest11.Destroy;
begin
  virtualScreen.Free;
  inherited Destroy;
end;

procedure TTest11.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var filtered: TBGRABitmap;
  filteredMask: TGrayscaleMask;
begin
  if pts = nil then exit;

  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
    FreeAndNil(virtualScreen);
  if (mask <> nil) and ((mask.width <> width) or (mask.Height <> height)) then
    FreeAndNil(mask);

  if virtualscreen = nil then
    virtualscreen := TBGRABitmap.Create(Width,Height);

  if ffilter = 'Emboss' then
  begin
    virtualScreen.Fill(BGRABlack);
    virtualScreen.DrawPolyLineAntialias(virtualScreen.ComputeOpenedSpline(pts,ssCrossing),BGRAWhite,(width+height)/80,True);
    filtered := virtualScreen.FilterEmbossHighlight(True);
    virtualScreen.Fill(clForm);
    virtualScreen.PutImage(0,0,filtered,dmDrawWithTransparency);
    filtered.Free;
    virtualscreen.Draw(Canvas,Left,Top,OpaqueDraw);
  end else
  if ffilter = 'Contour' then
  begin
    if mask = nil then
      mask := TGrayscaleMask.Create(virtualScreen.Width, virtualScreen.Height);
    mask.Fill(ByteMaskBlack);
    mask.DrawPolyLineAntialias(virtualScreen.ComputeOpenedSpline(pts,ssCrossing),
      ByteMaskWhite, (width+height)/80, True);
    filteredMask := mask.FilterContour;
    filteredMask.Draw(virtualScreen, 0, 0, true);
    filteredMask.Free;
    virtualscreen.Draw(Canvas,Left,Top,OpaqueDraw);
  end else
  begin
    virtualScreen.Fill(BGRAWhite);
    virtualScreen.DrawPolyLineAntialias(virtualScreen.ComputeOpenedSpline(pts,ssCrossing),BGRA(0,0,0,128),(width+height)/80,True);
    virtualScreen.DrawPolyLineAntialias(pts,BGRA(0,0,0,128),(width+height)/800,True);
    virtualscreen.Draw(Canvas,Left,Top,OpaqueDraw);
  end;
end;

procedure TTest11.OnTimer(Width, Height: Integer; ElapsedSec: Double);
var i: integer;
begin
  if pts = nil then
  begin
    setlength(pts,nbPoints);
    setlength(dirs,nbPoints);
    for i := 0 to NbPoints-1 do
    begin
      pts[i] := pointf(random(Width),random(Height));
      dirs[i] := pointf((random(Width)-width/2)/20,(random(Height)-height/2)/20);
    end;
  end;
  for i := 0 to NbPoints-1 do
  begin
    pts[i].x := pts[i].x+dirs[i].x;
    if pts[i].x < 0 then
    begin
      pts[i].x := 0;
      dirs[i].x := abs(dirs[i].x);
    end;
    if pts[i].x > width-1 then
    begin
      pts[i].x := width-1;
      dirs[i].x := -abs(dirs[i].x);
    end;
    pts[i].y := pts[i].y+dirs[i].y;
    if pts[i].y < 0 then
    begin
      pts[i].y := 0;
      dirs[i].y := abs(dirs[i].y);
    end;
    if pts[i].y > height-1 then
    begin
      pts[i].y := height-1;
      dirs[i].y := -abs(dirs[i].y);
    end;
  end;
end;

end.

