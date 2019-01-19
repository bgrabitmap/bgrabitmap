unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLType;

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

uses GraphType, LCLIntf;

{$R *.lfm}

{ TFMain }

procedure TFMain.FormPaint(Sender: TObject);

  procedure DrawTestBitmap(Left,Top,Right,Bottom: integer; AName: string);
  var
    bmp: TBitmap;
    imgWidth,imgHeight, ty: integer;
    RawImage: TRawImage;
    BitmapHandle, MaskHandle: HBitmap;
    p: pbyte;
    cx,cy,x,y: integer;
    RedOfs,GreenOfs,BlueOfs,AlphaOfs: integer;
  begin
    ty := (Bottom-Top) div 8;
    Canvas.Font.Height := ty*7 div 10;
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(Left,Top,AName);

    RedOfs := pos('R',AName)-1;
    GreenOfs := pos('G',AName)-1;
    BlueOfs := pos('B',AName)-1;
    AlphaOfs := pos('A',AName)-1;

    imgWidth := Right-Left;
    imgHeight := Bottom-(Top+ty);
    bmp := TBitmap.Create;
    RawImage.Init;
    RawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(imgWidth, imgHeight);
    RawImage.Description.RedShift := RedOfs*8;
    RawImage.Description.GreenShift := GreenOfs*8;
    RawImage.Description.BlueShift := BlueOfs*8;
    if AlphaOfs >= 0 then
      RawImage.Description.AlphaShift := AlphaOfs*8
    else
    begin
      RawImage.Description.Depth := 24;
      RawImage.Description.AlphaShift := 0;
      RawImage.Description.AlphaPrec := 0;
    end;
    RawImage.CreateData(true);
    for y := 0 to imgHeight-1 do
    begin
      cy := y*9 div imgHeight;
      p := RawImage.GetLineStart(y);
      for x := 0 to imgWidth-1 do
      begin
        cx := x*9 div imgWidth;
        (p+RedOfs)^ := (cx mod 3)*255 div 2;
        (p+GreenOfs)^ := (cx div 3)*255 div 2;
        (p+BlueOfs)^ := (cy mod 3)*255 div 2;
        if AlphaOfs >= 0 then (p+AlphaOfs)^ := (cy div 3)*255 div 2;
        inc(p,4);
      end;
    end;
    if RawImage_CreateBitmaps(RawImage, BitmapHandle, MaskHandle, False) then
    begin
      bmp := TBitmap.Create;
      bmp.Handle := BitmapHandle;
      bmp.MaskHandle := MaskHandle;
      Canvas.Draw(Left,Top+ty,bmp);
      bmp.Free;
    end else
      Canvas.TextOut(Left,Top+ty,'Unable to create');
    RawImage.FreeData;
  end;

begin
  DrawTestBitmap(0,0,ClientWidth div 4,ClientHeight div 2,'RGBA');
  DrawTestBitmap(ClientWidth div 4,0,ClientWidth div 2,ClientHeight div 2,'BGRA');
  DrawTestBitmap(0,ClientHeight div 2,ClientWidth div 4,ClientHeight,'ARGB');
  DrawTestBitmap(ClientWidth div 4,ClientHeight div 2,ClientWidth div 2,ClientHeight,'ABGR');
  DrawTestBitmap(ClientWidth div 2,0,3*ClientWidth div 4,ClientHeight div 2,'RGB.');
  DrawTestBitmap(3*ClientWidth div 4,0,ClientWidth,ClientHeight div 2,'BGR.');
  DrawTestBitmap(ClientWidth div 2,ClientHeight div 2,3*ClientWidth div 4,ClientHeight,'.RGB');
  DrawTestBitmap(3*ClientWidth div 4,ClientHeight div 2,ClientWidth,ClientHeight,'.BGR');
end;

end.

