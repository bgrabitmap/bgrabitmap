unit utest3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, Graphics, utestpacrect;

type
  { TTest3 }

  TTest3 = class(TTestPacRect)
  public
    constructor Create;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
  end;

implementation

{ TTest3 }

constructor TTest3.Create;
begin
  inherited Create;
  Name := 'TBGRABitmap.Draw(Form). You should see flickering Pacmans walking on the form with a rectangle. The opacity of the rectangle depends on the rendering capacities of BGRABitmap on windows.';
end;

procedure TTest3.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var i: integer;
begin
  if backgroundImg = nil then exit;

  //draw background opaque on canvas
  backgroundImg.Draw(Canvas,Left,Top,OpaqueDraw);

  //draw sprites transparent on canvas
  for i := 0 to high(pacLoc) do
    pacImg[numPacImg].Draw(Canvas, RectWithSize(Left+pacLoc[i].x,Top+pacLoc[i].y,
      pacImg[numPacImg].Width * (80 + (i * 40 div NbPacman)) div 100,
      pacImg[numPacImg].Height * (80 + (i * 40 div NbPacman)) div 100),false);
end;

end.

