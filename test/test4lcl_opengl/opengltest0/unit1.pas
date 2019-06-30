unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BGLVirtualScreen, BGRAOpenGL;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGLVirtualScreen1: TBGLVirtualScreen;
    procedure BGLVirtualScreen1DblClick(Sender: TObject);
    procedure BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
  private
    OriginalBounds: TRect;
    OriginalWindowState: TWindowState;
    procedure SwitchFullScreen;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses BGRABitmapTypes;

{ TForm1 }

procedure TForm1.SwitchFullScreen;
begin
  if BorderStyle <> bsNone then begin
    // To full screen
    OriginalWindowState := WindowState;
    OriginalBounds := BoundsRect;

    BorderStyle := bsNone;
    BoundsRect := Screen.MonitorFromWindow(Handle).BoundsRect;
  end else begin
    // From full screen
    BorderStyle := bsSizeable;
    if OriginalWindowState = wsMaximized then
      WindowState := wsMaximized
    else
      BoundsRect := OriginalBounds;
  end;
end;

procedure TForm1.BGLVirtualScreen1Redraw(Sender: TObject;
  BGLContext: TBGLContext);
begin
  BGLContext.Canvas.FillRect(10,10,100,100, CSSRed);
end;

// double-clic to switch to fullscreen
procedure TForm1.BGLVirtualScreen1DblClick(Sender: TObject);
begin
  SwitchFullScreen;
end;

end.
