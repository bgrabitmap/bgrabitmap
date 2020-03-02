unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
  private

  public
    mx,my: integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses BGRABitmap, BGRABitmapTypes, BGRAText, BGRAPath;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  mx := x;
  my := y;
  Invalidate;
end;

procedure TForm1.FormPaint(Sender: TObject);
const zoom = 1;
  TestText = 'في XHTML 1.0 يتم تحقيق ذلك بإضافة + العنصر المضمن bdo.';//'أب+';
  TestText2 = 'before مصر after سلام كَعْك.';
var
  image: TBGRABitmap;
  TS, tempTS: TTextStyle;
  y, prevHeight: Integer;
  s: TSize;
  p: TBGRAPath;
begin
  image := TBGRABitmap.Create(ClientWidth div zoom, ClientHeight div zoom, BGRAWhite);
  image.FontName := Font.Name;
  image.FontHeight := round(Font.Height*96/PixelsPerInch);
  image.FontQuality := fqSystemClearType;

  Canvas.Brush.Style := bsClear;

  fillchar(TS, sizeof(TS), 0);
  TS.Opaque := False;
  TS.Clipping:= true;

  TS.RightToLeft := False;
  TS.ShowPrefix := false;
  ts.Alignment:= taRightJustify;
  image.TextRect(Rect(5, 25, 240, 45), 5, 25, 'BGRA With RTL=False', TS, BGRA(0,0,0));
  ts.Alignment:= taLeftJustify;
  image.TextRect(Rect(250, 25, image.Width, 45), 250, 25, TestText, TS, BGRA(0,0,0));

  TS.RightToLeft := True;
  TS.ShowPrefix := true;
  ts.Alignment:= taRightJustify;
  image.TextRect(Rect(5, 95, 240, 135), 5, 95, 'BGRA' + LineEnding + 'With RTL=True',
                 TS, BGRA(0,0,0));
  ts.Alignment:= taLeftJustify;
  image.TextRect(Rect(250, 95, image.Width, 135), 250, 95, TestText, TS, BGRA(0,0,0));

  image.TextOut(240,190, 'BGRA TextOut LTR', BGRABlack, taRightJustify);
  image.TextOut(250,190, TestText, BGRABlack, false);
  image.TextOut(240,210, 'BGRA TextOut RTL', BGRABlack, taRightJustify);
  image.TextOut(250,210, TestText, BGRABlack, true);

  image.TextOut(240,230, 'BGRA TextOut auto', BGRABlack, taRightJustify);
  image.TextOut(250,230, TestText, BGRABlack);
  image.TextOut(240,250, 'BGRA TextOut auto', BGRABlack, taRightJustify);
  image.TextOut(250,250, TestText2, BGRABlack);

  y := 280;
  ts.Wordbreak := true;
  ts.RightToLeft := false;
  s := image.TextSize(TestText, image.Width-250, ts.RightToLeft);
  image.TextOut(240,y, 'BGRA WordWrap LTR (fit ' + inttostr(image.TextFitInfo(TestText, image.Width-250)) + ')', BGRABlack, taRightJustify);
  image.Rectangle(250, y, 250+s.cx,y+s.cy, BGRA(255,0,0,128), dmDrawWithTransparency);
  image.TextRect(Rect(250, y, 250+s.cx,y+s.cy), 250,y, TestText, ts, BGRABlack);
  inc(y, s.cy);

  ts.RightToLeft := true;
  s := image.TextSize(TestText, image.Width-250, ts.RightToLeft);
  image.TextOut(240,y, 'BGRA WordWrap RTL', BGRABlack, taRightJustify);
  image.Rectangle(250, y, 250+s.cx,y+s.cy, BGRA(255,0,0,128), dmDrawWithTransparency);
  image.TextRect(Rect(250, y, 250+s.cx,y+s.cy), 250,y, TestText, ts, BGRABlack);
  inc(y, s.cy);
  ts.Wordbreak := false;

  image.TextOut(240,y, 'BGRA TextOutAngle LTR', BGRABlack, taRightJustify);
  image.FontStyle := [fsUnderline];
  image.TextOutAngle(250,y, -150, TestText2, BGRABlack, taLeftJustify, False);
  image.FontStyle := [];
  inc(y,20);

  image.TextOut(240,y, 'BGRA TextOutAngle RTL', BGRABlack, taRightJustify);
  image.FontStyle := [fsUnderline];
  image.TextOutAngle(250,y, -150, TestText2, BGRABlack, taLeftJustify, True);
  image.FontStyle := [];
  inc(y,20);

  image.TextOut(240,y, 'BGRA TextOutAngle auto', BGRABlack, taRightJustify);
  image.FontStyle := [fsUnderline];
  image.TextOutAngle(250,y, -150, TestText2, BGRABlack, taLeftJustify);
  image.FontStyle := [];
  inc(y,40);

  image.FontHeight := 25;
  image.TextOut(240,y, 'Curved LTR', BGRABlack, taRightJustify);
  p := TBGRAPath.Create;
  p.translate(image.FontFullHeight/2, image.FontFullHeight/2);
  p.moveTo(250,y);
  p.arc(500,y, 250,50, 0, Pi,0, true);
  image.FontVerticalAnchor:= fvaCapCenter;
  image.FontBidiMode:= fbmLeftToRight;
  image.TextOutCurved(p, TestText2, BGRABlack, taLeftJustify, 0);
  image.FontVerticalAnchor:= fvaTop;
  p.Free;
  inc(y,round(image.FontFullHeight*1.2));

  image.TextOut(240,y, 'Curved RTL', BGRABlack, taRightJustify);
  p := TBGRAPath.Create;
  p.translate(image.FontFullHeight/2, image.FontFullHeight/2);
  p.moveTo(250,y);
  p.arc(500,y, 250,50, 0, Pi,0, true);
  image.FontVerticalAnchor:= fvaCapCenter;
  image.FontBidiMode:= fbmRightToLeft;
  image.TextOutCurved(p, TestText2, BGRABlack, taLeftJustify, 0);
  image.FontVerticalAnchor:= fvaTop;
  p.Free;
  inc(y,round(image.FontFullHeight*1.2));

  image.TextOut(240,y, 'Curved auto', BGRABlack, taRightJustify);
  p := TBGRAPath.Create;
  p.translate(image.FontFullHeight/2, image.FontFullHeight/2);
  p.moveTo(250,y);
  p.arc(500,y, 250,50, 0, Pi,0, true);
  image.FontVerticalAnchor:= fvaCapCenter;
  image.FontBidiMode:= fbmAuto;
  image.TextOutCurved(p, TestText2, BGRABlack, taLeftJustify, 0);
  image.FontVerticalAnchor:= fvaTop;
  p.Free;
  inc(y,round(image.FontFullHeight*1.2));


  image.FontFullHeight := 40;
  ts.Alignment := taLeftJustify;
  TS.EndEllipsis := true;
  TS.RightToLeft := false;
  TS.Wordbreak := false;
  image.TextRect(rect(0,0,ClientWidth,ClientHeight), mx,my-50, 'BGRA test ellipsis', ts, CSSGray);
  TS.EndEllipsis:= false;

  image.Draw(Canvas, 0,0, true);

  image.Free;

  Canvas.Font.Height := round(Font.Height*96/PixelsPerInch);
  Canvas.Font.Color := clBlack;
  TS.RightToLeft := False;
  TS.ShowPrefix := false;
  ts.Alignment:= taRightJustify;
  Canvas.TextRect(Rect(5, 5, 240, 25), 5, 5, 'TCanvas With RTL=False', TS);
  ts.Alignment:= taLeftJustify;
  Canvas.TextRect(Rect(250, 5, Canvas.Width, 25), 250, 5, TestText, TS);

  TS.RightToLeft := True;
  TS.ShowPrefix := true;
  ts.Alignment:= taRightJustify;
  Canvas.TextRect(Rect(5, 55, 240, 95), 5, 55, 'TCanvas'+LineEnding+'With &RTL=True', TS);
  ts.Alignment:= taLeftJustify;
  Canvas.TextRect(Rect(250, 55, Canvas.Width, 95), 250, 55, TestText, TS);

  ts.Alignment:= taRightJustify;
  Canvas.TextRect(Rect(5, 140, 240, 160), 5, 140, 'TCanvas TextOut LTR', TS);
  Canvas.TextOut(250,140, TestText);
  tempTS := Canvas.TextStyle;
  tempTS.RightToLeft := true;
  Canvas.TextStyle := tempTS;
  Canvas.TextRect(Rect(5, 160, 240, 180), 5, 160, 'TCanvas TextOut RTL', TS);
  Canvas.TextOut(250,160, TestText);
  tempTS := Canvas.TextStyle;
  tempTS.RightToLeft := false;
  Canvas.TextStyle := tempTS;

  prevHeight := Canvas.Font.Height;
  Canvas.Font.Color := clGray;
  Canvas.Font.Height := FontFullHeightSign * 40;
  ts.Alignment := taLeftJustify;
  TS.EndEllipsis := true;
  TS.RightToLeft := false;
  TS.Wordbreak := false;
  Canvas.TextRect(rect(0,0,ClientWidth,ClientHeight), mx,my, 'TCanvas test ellipsis', ts);
  TS.EndEllipsis:= false;
  Canvas.Font.Height := prevHeight;
end;

end.

