// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAWriteWebP;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPimage;

type
  { TBGRAWriterWebP }

  TBGRAWriterWebP = class(TFPCustomImageWriter)
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

uses libwebp, BGRABitmapTypes;

var
  MyLibWebPLoaded: boolean;

procedure NeedLibWebP;
begin
  if not MyLibWebPLoaded then
  begin
    if not LibWebPLoad then
      raise exception.Create('Cannot find libwebp library ('+LibWebPFilename+')');
    MyLibWebPLoaded:= true;
  end;
end;

{ TBGRAWriterWebP }

procedure TBGRAWriterWebP.InternalWrite(Stream: TStream; Img: TFPCustomImage);
const
  CopySize = 65536;
var
  saveFrom: TBGRACustomBitmap;
  outSize, remain, toWrite: LongWord;
  outData, p: PByte;
begin
  NeedLibWebP;
  saveFrom := BGRABitmapFactory.Create(Img);
  outData := nil;
  try
    if saveFrom.LineOrder = riloBottomToTop then
      saveFrom.VerticalFlip;

    {$PUSH}{$WARNINGS OFF}
    if Lossless then
    begin
      if TBGRAPixel_RGBAOrder then
        outSize := WebPEncodeLosslessRGBA(saveFrom.DataByte, saveFrom.Width, saveFrom.Height,
                saveFrom.RowSize, outData{%H-})
      else
        outSize := WebPEncodeLosslessBGRA(saveFrom.DataByte, saveFrom.Width, saveFrom.Height,
                saveFrom.RowSize, outData{%H-});
    end else
    begin
      if TBGRAPixel_RGBAOrder then
        outSize := WebPEncodeRGBA(saveFrom.DataByte, saveFrom.Width, saveFrom.Height,
                saveFrom.RowSize, QualityPercent, outData{%H-})
      else
        outSize := WebPEncodeBGRA(saveFrom.DataByte, saveFrom.Width, saveFrom.Height,
                saveFrom.RowSize, QualityPercent, outData{%H-});
    end;
    {$POP}
    if outSize = 0 then
      raise exception.Create('Error encoding WebP');

    remain := outSize;
    p := outData;
    while remain > 0 do
    begin
      if remain > CopySize then toWrite := CopySize
      else toWrite := remain;
      Stream.WriteBuffer(p^, toWrite);
      inc(p, toWrite);
      dec(remain, toWrite);
    end;
  finally
    if Assigned(outData) then WebPFree(outData);
    saveFrom.Free;
  end;
end;

constructor TBGRAWriterWebP.Create;
begin
  inherited Create;
  FQualityPercent := 100;
  FLossless:= True;
end;

initialization

  DefaultBGRAImageWriter[ifWebP] := TBGRAWriterWebP;

finalization

  if MyLibWebPLoaded then
  begin
    LibWebPUnload;
    MyLibWebPLoaded:= false;
  end;

end.

