unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

  { TFMain }

  TFMain = class(TForm)
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

uses Math,BGRABitmapTypes,BGRABitmap,BGRATypewriter,BGRATransform;

{ TFMain }

procedure TFMain.FormPaint(Sender: TObject);
const GoneCount = 8;
var
  radius: single;
  bmp: TBGRABitmap;

  procedure TestGlyph(center: TPointF; curve: boolean);
  var
    glyph: TBGRAPolygonalGlyph;
    pts: array of TPointF;
    mem: TMemoryStream;
    i: Integer;
  begin
    setlength(pts, GoneCount);
    for i := 0 to GoneCount-1 do
      pts[i] := AffineMatrixRotationDeg((i+0.5)/GoneCount*360)*PointF(0,radius) + center;
    glyph := TBGRAPolygonalGlyph.Create('o');
    glyph.SetPoints(pts);
    glyph.QuadraticCurves:= curve;
    mem := TMemoryStream.Create;
    glyph.SaveToStream(mem);
    glyph.Free;
    mem.Position:= 0;
    glyph := TBGRAGlyph.LoadFromStream(mem) as TBGRAPolygonalGlyph;
    mem.Free;
    bmp.Canvas2D.lineWidth:= radius*0.02;
    bmp.Canvas2D.strokeStyle(BGRABlack);
    glyph.Path(bmp.Canvas2D, AffineMatrixIdentity);
    glyph.Free;
    bmp.Canvas2D.stroke;
  end;

begin
  bmp := TBGRABitmap.Create(ClientWidth,ClientHeight,BGRAWhite);
  radius := min(bmp.Width*0.45,bmp.Height*0.9)*0.5;
  TestGlyph(PointF(bmp.Width/4,bmp.Height/2),false);
  TestGlyph(PointF(bmp.Width*3/4,bmp.Height/2),true);
  bmp.Draw(Canvas,0,0);
end;

end.

