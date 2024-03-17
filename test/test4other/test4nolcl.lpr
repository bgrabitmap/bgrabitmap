program test4nolcl_freetype;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cwstring,
  {$ENDIF}
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
  bmp.Canvas.Pen.Color := clBlue;
  bmp.Canvas.MoveTo(0,0);
  bmp.Canvas.LineTo(bmp.Width,bmp.Height);
end;

{ TTestBGRANoLCL }

procedure TTestBGRANoLCL.DoRun;
var
  bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create(800,600,BGRABlack);
  DrawEllipseHello(bmp);
  bmp.SaveToFile(ExtractFilePath(ExeName)+'test.png');
  bmp.free;

  // stop program loop
  Terminate;
end;

constructor TTestBGRANoLCL.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTestBGRANoLCL.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TTestBGRANoLCL;

{$R *.res}

begin
  Application:=TTestBGRANoLCL.Create(nil);
  Application.Title:='TestBGRANoLCL';
  Application.Run;
  Application.Free;
end.

