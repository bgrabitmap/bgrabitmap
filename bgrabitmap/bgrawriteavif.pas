// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Implements the writer for the AVIF format (relies on external libavif library) }
unit BGRAWriteAvif;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPimage, avifbgra;

type
  {* Extends the TFPCustomImageWriter to write the AVIF image format }
  TBGRAWriterAvif = class(TFPCustomImageWriter)
  protected
    FLossless: boolean;
    FQualityPercent: Single;
    FQualityAlphaPercent: Single;
    FCodec: avifCodecChoice;
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
    property QualityAlphaPercent: Single read FQualityAlphaPercent write FQualityAlphaPercent;
    property Codec: avifCodecChoice read FCodec write FCodec;
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
  qualityAlpha:integer;
begin
  NeedLibAvif;
  saveFrom := BGRABitmapFactory.Create(Img);
  try
    quality:=Trunc(QualityPercent);
    qualityAlpha:=Trunc(QualityAlphaPercent);
    if LossLess then
    begin
      quality:=100;
      qualityAlpha:=100;
    end;
    if (FCodec<>AVIF_CODEC_CHOICE_AUTO) or (qualityAlpha <> 100) then
      outsize:=avifSaveToStream(TBGRABitmap(saveFrom),Stream,IgnoreAlpha,quality,qualityAlpha,PixelFormat,FCodec,Speed)
    else
      outsize:=avifSaveToStream(TBGRABitmap(saveFrom),Stream,quality,Speed,PixelFormat,IgnoreAlpha);
    if outSize = 0 then
      raise exception.Create('Error encoding Avif');
  finally
    saveFrom.Free;
  end;
end;

constructor TBGRAWriterAvif.Create;
begin
  inherited Create;
  FQualityPercent := 100;
  FQualityAlphaPercent := 100;
  FLossless:= True;
  FSpeed:=AVIF_SPEED_DEFAULT;
  FPixelFormat:=AVIF_PIXEL_FORMAT_YUV420;
  FIgnoreAlpha:=false;
  FCodec := AVIF_CODEC_CHOICE_AUTO;
end;

initialization
  BGRARegisterImageWriter(ifAvif, TBGRAWriterAvif, True, 'AVIF Still Image Format', 'avif');

finalization

  if MyLibAvifLoaded then
  begin
    LibAvifUnload;
    MyLibAvifLoaded:= false;
  end;

end.

