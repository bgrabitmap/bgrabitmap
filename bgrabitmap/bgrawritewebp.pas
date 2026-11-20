// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Implements the writer for the WebP format (relies on external libwebp library) }
unit BGRAWriteWebP;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPimage, libwebp;

type
  {* Extends the TFPCustomImageWriter to write the WebP image format }
  TBGRAWriterWebP = class(TFPCustomImageWriter)
  protected
    FLossless: boolean;
    FQualityPercent: Single;
    FCompressionEffort: Integer;
    procedure SetCompressionEffort(AValue: Integer);
    {** Configure an initialized libwebp encoder. Override and optionally call
        inherited before setting exact method, quality or other WebP options.
        The resulting configuration is validated before importing pixels. }
    procedure ConfigureEncoder(var AConfig: TWebPConfig); virtual;
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;

  public
    constructor Create; override;

  published
    {** Defines the quality when saving the file, 100 being the maximum but not
        necessarily lossless }
    property QualityPercent: single read FQualityPercent write FQualityPercent;
    {** If Lossless is set to True, the _QualityPercent_ property is ignored }
    property Lossless: boolean read FLossless write FLossless;
    {** Encoding effort from 0 (fastest) to 9 (most compression), default 6.
        In lossless mode, selects libwebp's lossless preset. In lossy mode,
        selects method (CompressionEffort*2+1) div 3 without changing quality. }
    property CompressionEffort: Integer read FCompressionEffort write SetCompressionEffort default 6;
  end;

implementation

uses BGRABitmapTypes;

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
  if not (Assigned(WebPConfigInitInternal) and Assigned(WebPConfigLosslessPreset)
    and Assigned(WebPValidateConfig) and Assigned(WebPPictureInitInternal)
    and Assigned(WebPPictureImportRGBA) and Assigned(WebPPictureImportBGRA)
    and Assigned(WebPPictureFree) and Assigned(WebPEncode)
    and Assigned(WebPMemoryWriterInit) and Assigned(WebPMemoryWriterClear)
    and Assigned(WebPMemoryWrite)) then
    raise exception.Create('libwebp does not provide the required encoder API');
end;

{ TBGRAWriterWebP }

procedure TBGRAWriterWebP.SetCompressionEffort(AValue: Integer);
begin
  if (AValue < 0) or (AValue > 9) then
    raise ERangeError.Create('WebP compression effort must be between 0 and 9');
  FCompressionEffort := AValue;
end;

procedure TBGRAWriterWebP.ConfigureEncoder(var AConfig: TWebPConfig);
begin
  if Lossless then
  begin
    if WebPConfigLosslessPreset(@AConfig, CompressionEffort) = 0 then
      raise exception.Create('Invalid WebP lossless compression effort');
  end else
  begin
    AConfig.lossless := 0;
    AConfig.quality := QualityPercent;
    AConfig.method := (CompressionEffort * 2 + 1) div 3;
  end;
end;

procedure TBGRAWriterWebP.InternalWrite(Stream: TStream; Img: TFPCustomImage);
const
  CopySize = 65536;
var
  saveFrom: TBGRACustomBitmap;
  remain, toWrite: SizeUInt;
  p: PByte;
  config: TWebPConfig;
  picture: TWebPPicture;
  output: TWebPMemoryWriter;
  imported: Integer;
begin
  NeedLibWebP;
  FillChar(config, SizeOf(config), 0);
  if WebPConfigInit(@config) = 0 then
    raise exception.Create('Incompatible libwebp encoder ABI');
  ConfigureEncoder(config);
  if WebPValidateConfig(@config) = 0 then
    raise exception.Create('Invalid WebP encoder configuration');
  if WebPPictureInit(@picture) = 0 then
    raise exception.Create('Incompatible libwebp picture ABI');
  WebPMemoryWriterInit(@output);
  saveFrom := nil;
  try
    saveFrom := BGRABitmapFactory.Create(Img);
    if saveFrom.LineOrder = riloBottomToTop then
      saveFrom.VerticalFlip;

    picture.use_argb := config.lossless;
    picture.width := saveFrom.Width;
    picture.height := saveFrom.Height;
    picture.writer := WebPMemoryWrite;
    picture.custom_ptr := @output;
    if TBGRAPixel_RGBAOrder then
      imported := WebPPictureImportRGBA(@picture, saveFrom.DataByte, saveFrom.RowSize)
    else
      imported := WebPPictureImportBGRA(@picture, saveFrom.DataByte, saveFrom.RowSize);
    if imported = 0 then
      raise exception.CreateFmt('Error importing WebP pixels (code %d)', [picture.error_code]);
    if WebPEncode(@config, @picture) = 0 then
      raise exception.CreateFmt('Error encoding WebP (code %d)', [picture.error_code]);

    remain := output.size;
    p := output.mem;
    while remain > 0 do
    begin
      if remain > CopySize then toWrite := CopySize
      else toWrite := remain;
      Stream.WriteBuffer(p^, toWrite);
      inc(p, toWrite);
      dec(remain, toWrite);
    end;
  finally
    WebPMemoryWriterClear(@output);
    WebPPictureFree(@picture);
    saveFrom.Free;
  end;
end;

constructor TBGRAWriterWebP.Create;
begin
  inherited Create;
  FQualityPercent := 100;
  FLossless:= True;
  FCompressionEffort := 6;
end;

initialization
  BGRARegisterImageWriter(ifWebP, TBGRAWriterWebP, True, 'WebP Image Format', 'webp');

finalization

  if MyLibWebPLoaded then
  begin
    LibWebPUnload;
    MyLibWebPLoaded:= false;
  end;

end.

