unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, ExtDlgs;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    btFileSel: TButton;
    cbTransparent: TCheckBox;
    cbImageFile: TCheckBox;
    edW: TSpinEdit;
    edH: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    openPict: TOpenPictureDialog;
    Panel1: TPanel;
    PanelDraw: TPanel;
    rbFull: TRadioButton;
    rbCustom: TRadioButton;
    edX: TSpinEdit;
    edY: TSpinEdit;
    procedure btImageFileClick(Sender: TObject);
    procedure DrawPaint(Sender: TObject);
    procedure rbFullClick(Sender: TObject);
  private
    { private declarations }
    Filename: String;

  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses BGRABitmap, BGRABitmapTypes;

{$R *.lfm}

procedure DrawEllipseHello(bmp: TBGRABitmap);
var br: TBGRACustomBitmap;
begin
  bmp.Fill(BGRABlack);
  bmp.CustomPenStyle := BGRAPenStyle(2,1);
  bmp.FillEllipseLinearColorAntialias(bmp.Width/2,bmp.Height/2,bmp.Width/2-5,bmp.Height/2-5, BGRAPixelTransparent, BGRAWhite);
  bmp.EllipseAntialias(bmp.Width/2,bmp.Height/2,bmp.Width/2-5,bmp.Height/2-5,CSSRed,5);
  if bmp.Height div 10 < 10 then
    bmp.FontHeight := 10
  else
    bmp.FontHeight := bmp.Height div 10;
  with bmp.FontPixelMetric do
    bmp.TextOut(bmp.Width/2,bmp.Height/2 - (CapLine+Baseline)/2,'Hello world', BGRABlack, taCenter);
  bmp.Canvas.Pen.Color := clBlue;
  bmp.Canvas.MoveTo(0,0);
  bmp.Canvas.LineTo(bmp.Width,bmp.Height);
  br := bmp.CreateBrushTexture(bsDiagCross, CSSYellow,CSSRed);
  bmp.FillPieInRect(rect(10,10,100,100),0,3*Pi/2,br);
  bmp.TextOutAngle(50,50, -300, 'Test angle', CSSGreen, taLeftJustify);
  br.Free;
end;

{ TForm1 }

procedure TForm1.DrawPaint(Sender: TObject);
var
  bmp: TBGRABitmap;
  ARect: TRect;

begin
  try
  if cbImageFile.Checked and FileExists(Filename)
  then bmp:= TBGRABitmap.Create(Filename)
  else begin
         bmp:= TBGRABitmap.Create(PanelDraw.Width,PanelDraw.Height);
         DrawEllipseHello(bmp);
       end;

  if rbFull.Checked
  then ARect:=Rect(0, 0, PanelDraw.Width,PanelDraw.Height)
  else ARect:=Rect(edX.Value, edY.Value, edX.Value+edW.Value, edY.Value+edH.Value);

  bmp.Draw(PanelDraw.Canvas, ARect, not(cbTransparent.Checked));

  finally
    bmp.Free;
  end;
end;

procedure TForm1.btImageFileClick(Sender: TObject);
begin
  if openPict.Execute then
  begin
    Filename:=openPict.FileName;
    PanelDraw.Invalidate;
  end;
end;

procedure TForm1.rbFullClick(Sender: TObject);
begin
  PanelDraw.Invalidate;
end;


end.

