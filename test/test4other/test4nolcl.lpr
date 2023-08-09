program test4nolcl;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  BGRAGraphics,
  BGRABitmap,
  BGRABitmapTypes;

type

  { TTestBGRANoLCL }

  TTestBGRANoLCL = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure DrawEllipseHello(bmp: TBGRABitmap);
begin
  bmp.Fill(BGRABlack);
  bmp.CustomPenStyle := BGRAPenStyle(2,1);
  bmp.FillEllipseLinearColorAntialias(bmp.Width/2,bmp.Height/2,bmp.Width/2-5,bmp.Height/2-5, BGRAPixelTransparent, BGRAWhite);
  bmp.EllipseAntialias(bmp.Width/2,bmp.Height/2,bmp.Width/2-5,bmp.Height/2-5,CSSRed,5);
  if bmp.Height div 10 < 10 then
    bmp.FontHeight := 10
  else
    bmp.FontHeight := bmp.Height div 10;
  bmp.Canvas.Pen.Color := clBlue;
  bmp.Canvas.MoveTo(0,0);
  bmp.Canvas.LineTo(bmp.Width,bmp.Height);
end;

{ TTestBGRANoLCL }

procedure TTestBGRANoLCL.DoRun;
var
  bmp: TBGRABitmap;
begin
  TBGRABitmap.AddFreeTypeFontFolder(GetCurrentDir);

  bmp := TBGRABitmap.Create(800,600,BGRABlack);
  DrawEllipseHello(bmp);
  bmp.SaveToFile('test.png');
  bmp.free;

  Terminate;
end;

constructor TTestBGRANoLCL.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TTestBGRANoLCL.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TTestBGRANoLCL;

begin
  Application:=TTestBGRANoLCL.Create(nil);
  Application.Title:='TestBGRANoLCL';
  Application.Run;
  Application.Free;
end.

