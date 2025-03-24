// SPDX-License-Identifier: LGPL-3.0-linking-exception
{*****************************************************************************}
{
  2023-06  - Massimo Magnano
           - added Resolution support
}
{*****************************************************************************}
{ Imports the writer for the BMP image format }
unit BGRAWriteBMP;
{$mode objfpc}
{$h+}

interface

uses SysUtils, Classes, FPImage, FPWriteBMP, BGRABitmapTypes;

const
   BMP_BitsValidValues: array[0..6] of byte = (1,4,8,15,16,24,32);

type
  { TBGRAWriterBMP }

  TBGRAWriterBMP = class (TFPWriterBMP)
  protected
    FGrayScale: boolean;

    procedure SetGrayScale(AValue: boolean); virtual;

    function SaveHeader(Stream:TStream; Img: TFPCustomImage):boolean; override;
    procedure InternalWrite(Stream:TStream; Img: TFPCustomImage); override;

  public
    constructor Create; override;

  published
    property GrayScale: boolean read FGrayscale write SetGrayScale;
    property BitsPerPixel;  //: byte rw [1,4,8,15,16,24,32];
    property RLECompress;   //: boolean rw;
  end;


implementation

procedure TBGRAWriterBMP.SetGrayScale(AValue: boolean);
begin
  if FGrayscale=AValue then Exit;
  FGrayscale:=AValue;
  BitsPerPixel:= 8;
end;

function TBGRAWriterBMP.SaveHeader(Stream:TStream; Img : TFPCustomImage):boolean;
begin
  {$IF FPC_FULLVERSION<30203}
  if (Img is TCustomUniversalBitmap) then
  with TCustomUniversalBitmap(Img) do
  begin
    ResolutionUnit :=ruPixelsPerCentimeter;
    Self.XPelsPerMeter :=Trunc(ResolutionX*100);
    Self.YPelsPerMeter :=Trunc(ResolutionY*100);
  end;
  {$ENDIF}

  Result:= inherited SaveHeader(Stream, Img);
end;

procedure TBGRAWriterBMP.InternalWrite(Stream: TStream; Img: TFPCustomImage);
var
   newPal: TFPPalette;

begin
  if FGrayscale then
  begin
    try
       newPal:= CreateGrayScalePalette;
       Img.UsePalette:= True;
       Img.Palette.Copy(newPal);
       newPal.Free;
    except
       newPal.Free;
    end;
  end;

  inherited InternalWrite(Stream, Img);
end;

constructor TBGRAWriterBMP.Create;
begin
  inherited Create;

  FGrayScale:= False;
end;

initialization
  BGRARegisterImageWriter(ifBMP, TBGRAWriterBMP, True, 'BMP Format', 'bmp');

end.
