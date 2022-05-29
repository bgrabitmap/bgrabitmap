// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAWriteAvif;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPimage, avifbgra;

type
  { TBGRAWriterAvif }

  TBGRAWriterAvif = class(TFPCustomImageWriter)
  protected
    FLossless: boolean;
    FQualityPercent: Single;
    FSpeed: integer;
    FPixelFormat:avifPixelFormat;
    FIgnoreAlpha:boolean;
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    property QualityPercent: single read FQualityPercent write FQualityPercent;
    { If Lossless is set to True, the QualityPercent property is ignored }
    property Lossless: boolean read FLossless write FLossless;
    property Speed: integer read FSpeed write FSpeed;
    property PixelFormat: avifPixelFormat read FPixelFormat write FPixelFormat;
    property IgnoreAlpha: boolean read FIgnoreAlpha write FIgnoreAlpha;
  end;

implementation

uses libavif, BGRABitmapTypes, BGRABitmap;

var
  MyLibAvifLoaded: boolean;

procedure NeedLibAvif;
begin
  if not MyLibAvifLoaded then
  begin
    if not LibAvifLoad then
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
    outsize:=avifSaveToStream(TBGRABitmap(saveFrom),Stream,quality,Speed,PixelFormat,IgnoreAlpha);
    if outSize = 0 then
      raise exception.Create('Error encoding WebP');
  finally
    saveFrom.Free;
  end;
end;

constructor TBGRAWriterAvif.Create;
begin
  inherited Create;
  FQualityPercent := 100;
  FLossless:= True;
  FSpeed:=AVIF_SPEED_DEFAULT;
  FPixelFormat:=AVIF_PIXEL_FORMAT_YUV420;
  FIgnoreAlpha:=false;
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

