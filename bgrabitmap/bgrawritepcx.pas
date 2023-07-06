// SPDX-License-Identifier: LGPL-3.0-linking-exception
{*****************************************************************************}
{
  2023-06  - Massimo Magnano
           - added Resolution support
}
{*****************************************************************************}
unit BGRAWritePCX;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, FPImage, FPWritePCX, BGRABitmapTypes;

type

  TBGRAWriterPCX = class(TFPWriterPCX)
  protected
    function SaveHeader(Stream: TStream; Img: TFPCustomImage): boolean; override;
  end;

implementation

uses pcxcomn;

function TBGRAWriterPCX.SaveHeader(Stream: TStream; Img: TFPCustomImage): boolean;
var
  Header: TPCXHeader;
begin
  //Code copied from FPWriterPCX because FillChar may not be done
  Result := False;
  FillChar(Header, SizeOf(Header), 0);
  with Header do
  begin
    FileID  := $0a;
    Version := 5;
    if Compressed then
      Encoding := 1
    else
      Encoding := 0;
    BitsPerPixel := 8;
    XMin := 0;
    YMin := 0;
    XMax := Img.Width - 1;
    YMax := Img.Height - 1;

    //Resolution
    if (Img is TCustomUniversalBitmap)
    then with TCustomUniversalBitmap(Img) do
         begin
           ResolutionUnit :=ruPixelsPerInch;
           HRes :=Trunc(ResolutionX);
           VRes :=Trunc(ResolutionY);
         end
    else begin
           HRes := 300;
           VRes := 300;
         end;

    ColorPlanes := 3;
    BytesPerLine := Img.Width;
    PaletteType := 1;
  end;
  Stream.WriteBuffer(Header, SizeOf(Header));
  Result := True;
end;

initialization
  if ImageHandlers.ImageWriter['PCX Format']=nil
  then ImageHandlers.RegisterImageWriter('PCX Format', 'pcx', TBGRAWriterPCX);
  DefaultBGRAImageWriter[ifPcx] := TBGRAWriterPCX;


end.
