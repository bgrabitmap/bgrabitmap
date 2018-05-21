unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Label1: TLabel;
    Label2: TLabel;
    Label_GammaValue: TLabel;
    Panel1: TPanel;
    TrackBar_Gamma: TTrackBar;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar_GammaChange(Sender: TObject);
  private
    { private declarations }
    FInitialised: boolean;
  public
    { public declarations }
    procedure UpdateLabelGamma;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FInitialised:= false;
  UpdateLabelGamma;
  TrackBar_Gamma.Position := round(BGRAGetGamma*20);
  FInitialised:= true;
end;

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
const stripSize = 20;
var c: TBGRAPixel;
  pattern: TBGRABitmap;
  i: Integer;
begin
  c := clWhite;
  c.Lightness := 32768;
  pattern := TBGRABitmap.Create(2,2);
  pattern.SetPixel(0,0,BGRABlack);
  pattern.SetPixel(1,0,BGRAWhite);
  pattern.SetPixel(0,1,BGRAWhite);
  pattern.SetPixel(1,1,BGRABlack);
  for i := 0 to Bitmap.Width div stripSize do
  begin
    Bitmap.FillRect(stripSize*i,0,stripSize*i+ stripSize div 2,Bitmap.Height,c,dmSet);
    Bitmap.FillRect(stripSize*i+ stripSize div 2,0,stripSize*(i+1),Bitmap.Height,pattern,dmSet);
  end;
  pattern.Free;
end;

procedure TForm1.TrackBar_GammaChange(Sender: TObject);
begin
  if FInitialised then
  begin
    BGRASetGamma(TrackBar_Gamma.Position/20);
    UpdateLabelGamma;
    BGRAVirtualScreen1.RedrawBitmap;
  end;
end;

procedure TForm1.UpdateLabelGamma;
begin
  Label_GammaValue.Caption:= FloatToStrF(BGRAGetGamma, ffFixed, 2,2);
end;

end.

