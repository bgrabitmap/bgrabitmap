// SPDX-License-Identifier: LGPL-3.0-linking-exception
{*****************************************************************************}
{
  2023-06  - Massimo Magnano
           - added Resolution support
}
{*****************************************************************************}
{ Imports the writer for the PCX image format }
unit BGRAWritePCX;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, FPImage, FPWritePCX, BGRABitmapTypes;

type
  {* Extends the TFPWriterPCX to save resolution }
  TBGRAWriterPCX = class(TFPWriterPCX)
  protected
    {$IF FPC_FULLVERSION<30203}
    function SaveHeader(Stream: TStream; Img: TFPCustomImage): boolean; override;
    {$ENDIF}

  published
    // MaxM: TO-DO TFPWriterPCX alway write at 8 BitsPerPixel
    //             It might be useful to write the other modes
    //property GrayScale: Boolean
    //property BitsPerPixel: byte // [1, 4, 8, 24]
    property Compressed;  //: boolean rw;
  end;

implementation

{$IF FPC_FULLVERSION<30203}
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
{$ENDIF}

initialization
  BGRARegisterImageWriter(ifPcx, TBGRAWriterPCX, True, 'PCX Format', 'pcx');


end.
