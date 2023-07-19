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
    {$IF FPC_FULLVERSION<30301}
    function SaveHeader(Stream:TStream; Img: TFPCustomImage):boolean; override;
    {$ENDIF}
  end;


implementation

{$IF FPC_FULLVERSION<30301}
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
{$ENDIF}

initialization
  if ImageHandlers.ImageWriter['BMP Format']=nil
  then ImageHandlers.RegisterImageWriter ('BMP Format', 'bmp', TBGRAWriterBMP);
  DefaultBGRAImageWriter[ifBMP] := TBGRAWriterBMP;

end.
