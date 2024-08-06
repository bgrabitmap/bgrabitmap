unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, BGLVirtualScreen, BGRAOpenGL, BGRABlurGL;

type
  TForm1 = class(TForm)
    BGLVirtualScreen1: TBGLVirtualScreen;
    Label1: TLabel;
    Direction: TLabel;
    Panel1: TPanel;
    SpinEdit_Radius: TSpinEdit;
    SpinEdit_Direction: TSpinEdit;
    procedure BGLVirtualScreen1LoadTextures(Sender: TObject;
      BGLContext: TBGLContext);
    procedure BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
    procedure BGLVirtualScreen1UnloadTextures(Sender: TObject;
      BGLContext: TBGLContext);
    procedure SpinEdit_DirectionChange(Sender: TObject);
    procedure SpinEdit_RadiusChange(Sender: TObject);
  private
    FBackground: IBGLTexture;
    FBlurShader: TBGLBlurShader;
  public

  end;

var
  Form1: TForm1;

implementation

uses BGRATransform, BGRABitmapTypes, GL;

{$R *.lfm}

{ TForm1 }

procedure TForm1.BGLVirtualScreen1Redraw(Sender: TObject;
  BGLContext: TBGLContext);
var blurred: IBGLTexture;
begin
  FBlurShader.Radius:= SpinEdit_Radius.Value;
  blurred := FBlurShader.FilterBlurMotion(FBackground,
    AffineMatrixRotationDeg(SpinEdit_Direction.Value) * PointF(1, 0));
  BGLContext.Canvas.StretchPutImage(0, 0, BGLContext.Canvas.Width, BGLContext.Canvas.Height, blurred);
end;

procedure TForm1.BGLVirtualScreen1LoadTextures(Sender: TObject;
  BGLContext: TBGLContext);
begin
  FBackground := BGLTexture(ResourceFile('Background1024.jpg'));
  FBlurShader := TBGLBlurShader.Create(BGLContext.Canvas, rbNormal);
end;

procedure TForm1.BGLVirtualScreen1UnloadTextures(Sender: TObject;
  BGLContext: TBGLContext);
begin
  FBackground := nil;
  FreeAndNil(FBlurShader);
end;

procedure TForm1.SpinEdit_DirectionChange(Sender: TObject);
begin
  BGLVirtualScreen1.Invalidate;
end;

procedure TForm1.SpinEdit_RadiusChange(Sender: TObject);
begin
  BGLVirtualScreen1.Invalidate;
end;

end.

