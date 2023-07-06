// SPDX-License-Identifier: LGPL-3.0-linking-exception
{*****************************************************************************}
{
  2023-06  - Massimo Magnano
           - added Resolution support
}
{*****************************************************************************}
unit BGRAWriteBMP;
{$mode objfpc}
{$h+}

interface

uses SysUtils, Classes, FPImage, FPWriteBMP, BGRABitmapTypes;

type

  TBGRAWriterBMP = class (TFPWriterBMP)
  protected
    function SaveHeader(Stream:TStream; Img: TFPCustomImage):boolean; override;
  end;


implementation

function TBGRAWriterBMP.SaveHeader(Stream:TStream; Img : TFPCustomImage):boolean;
begin
  if (Img is TCustomUniversalBitmap) then
  with TCustomUniversalBitmap(Img) do
  begin
    ResolutionUnit :=ruPixelsPerCentimeter;
    Self.XPelsPerMeter :=Trunc(ResolutionX*100);
    Self.YPelsPerMeter :=Trunc(ResolutionY*100);
  end;

  Result:=Inherited SaveHeader(Stream, Img);
end;

initialization
  if ImageHandlers.ImageWriter['BMP Format']=nil
  then ImageHandlers.RegisterImageWriter ('BMP Format', 'bmp', TBGRAWriterBMP);
  DefaultBGRAImageWriter[ifBMP] := TBGRAWriterBMP;

end.
