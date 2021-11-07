// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAWriteAvif;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPimage;

type
  { TBGRAWriterAvif }

  TBGRAWriterAvif = class(TFPCustomImageWriter)
  protected
    FLossless: boolean;
    FQualityPercent: Single;
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    property QualityPercent: single read FQualityPercent write FQualityPercent;
    { If Lossless is set to True, the QualityPercent property is ignored }
    property Lossless: boolean read FLossless write FLossless;

  end;

implementation

uses avifbgra,libavif{$ifdef linux}, linuxlib{$endif}, BGRABitmapTypes, BGRABitmap;

var
  MyLibAvifLoaded: boolean;

procedure NeedLibAvif;
begin
  if not MyLibAvifLoaded then
  begin
    if not LibAvifLoad({$ifdef linux}FindLinuxLibrary('libavif.so', 0){$else}LibAvifFilename{$endif}) then
      raise exception.Create('Cannot find libavif library ('+LibAvifFilename+')');
    MyLibAvifLoaded:= true;
  end;
end;

{ TBGRAWriterAvif }

procedure TBGRAWriterAvif.InternalWrite(Stream: TStream; Img: TFPCustomImage);
var
  saveFrom: TBGRACustomBitmap;
  outSize: LongWord;
  quality:integer;
begin
  NeedLibAvif;
  saveFrom := BGRABitmapFactory.Create(Img);
  try
    quality:=Trunc(QualityPercent);
    if LossLess then
      quality:=100;
    outsize:=avifSaveToStream(TBGRABitmap(saveFrom),Stream,quality);
    if outSize = 0 then
      raise exception.Create('Error encoding WebP');
  finally
    saveFrom.Free;
  end;
end;

constructor TBGRAWriterAvif.Create;
begin
  inherited Create;
//TODO: I thing that better default values are
// FQualityPercent := 60;
// FLossless:= False;
  FQualityPercent := 100;
  FLossless:= True;
end;

initialization

  DefaultBGRAImageWriter[ifAvif] := TBGRAWriterAvif;

finalization

  if MyLibAvifLoaded then
  begin
    LibAvifUnload;
    MyLibAvifLoaded:= false;
  end;

end.

