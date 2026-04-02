unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGLVirtualScreen, BGRAOpenGL;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGLVirtualScreen1: TBGLVirtualScreen;
    Timer1: TTimer;
    procedure BGLVirtualScreen1Elapse(Sender: TObject; BGLContext: TBGLContext;
      ElapsedMs: integer);
    procedure BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
    procedure Timer1Timer(Sender: TObject);
  private
    FTime: single;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses BGRABitmapTypes, BGRAOpenGLType;

{ TForm1 }

procedure TForm1.BGLVirtualScreen1Redraw(Sender: TObject;
  BGLContext: TBGLContext);
var
  i, j, tx, ty: Integer;
  previousFramebuffer, framebuffer: TBGLCustomFrameBuffer;
  texture: IBGLTexture;
  time: single;
begin
  BGLContext.Canvas.Fill(BGRABlack);

  previousFramebuffer := BGLContext.Canvas.ActiveFrameBuffer;
  framebuffer := TBGLFrameBuffer.Create(256, 256);
  BGLContext.Canvas.ActiveFrameBuffer := framebuffer;
  BGLContext.Canvas.Fill(BGRABlack);
  BGLContext.Canvas.FillEllipse(128, 128, 128, 128, BGRAWhite);
  BGLContext.Canvas.FillEllipse(128 + cos(FTime * 2*Pi/3)*32,
    128 + sin(FTime * 2*Pi/3)*32, 64, 64, BGRABlack);
  BGLContext.Canvas.ActiveFrameBuffer := previousFramebuffer;
  texture := framebuffer.MakeTextureAndFree;

  tx := texture.Width div 2;
  ty := texture.Height div 2;
  texture.BlendMode := obmAdd;
  for i := -1 to (BGLContext.Canvas.Width+tx-1) div tx do
    for j := -1 to (BGLContext.Canvas.Width+ty-1) div ty do
      BGLContext.Canvas.PutImage(i * tx + cos((j + FTime) * 2*Pi/6)*tx/2,
        j * ty + sin((i + FTime*0.7) * 2*Pi/6)*tx/2, texture,
        BGRA((i and 3)*40, (j and 3)*40, ((i+j+1) and 3)*40));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  BGLVirtualScreen1.Invalidate;
end;

procedure TForm1.BGLVirtualScreen1Elapse(Sender: TObject;
  BGLContext: TBGLContext; ElapsedMs: integer);
begin
  FTime += ElapsedMs/1000;
end;

end.
