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
    procedure BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses BGRABitmapTypes;

{ TForm1 }

procedure TForm1.BGLVirtualScreen1Redraw(Sender: TObject;
  BGLContext: TBGLContext);
begin
  BGLContext.Canvas.FillRect(10,10,100,100, CSSRed);
end;

end.

