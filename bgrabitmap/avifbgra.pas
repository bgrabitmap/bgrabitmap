// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Encode/Decode avif images from/to BGRABitmap. }
{ Author: Domingo Galmes <dgalmesp@gmail.com>  01-11-2021 }

unit avifbgra;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, libavif;

type
  EAvifException = class(Exception);
  avifPixelFormat = libavif.avifPixelFormat;

const
  AVIF_DEFAULT_QUALITY = 30;

procedure AvifLoadFromStream(AStream: TStream; aBitmap: TBGRACustomBitmap);
procedure AvifLoadFromFile(const AFilename: string; aBitmap: TBGRACustomBitmap);
procedure AvifLoadFromFileNative(const AFilename: string; aBitmap: TBGRACustomBitmap);
procedure AvifLoadFromMemory(AData: Pointer; ASize: cardinal; aBitmap: TBGRACustomBitmap);
//a Quality0to100=100 LOSSLESS
function AvifSaveToStream(aBitmap: TBGRACustomBitmap; AStream: TStream; aQuality0to100: integer = 30;aSpeed0to10:integer=AVIF_SPEED_DEFAULT;aPixelFormat:avifPixelFormat=AVIF_PIXEL_FORMAT_YUV420;aIgnoreAlpha:boolean=false): NativeUInt;
function AvifSaveToFile(aBitmap: TBGRACustomBitmap; const AFilename: string; aQuality0to100: integer = 30;aSpeed0to10:integer=AVIF_SPEED_DEFAULT;aPixelFormat:avifPixelFormat=AVIF_PIXEL_FORMAT_YUV420;aIgnoreAlpha:boolean=false): NativeUInt;
//returns size of the resulting bitmap.
function AvifSaveToMemory(aBitmap: TBGRACustomBitmap; AData: Pointer; ASize: cardinal; aQuality0to100: integer = 30;aSpeed0to10:integer=AVIF_SPEED_DEFAULT;aPixelFormat:avifPixelFormat=AVIF_PIXEL_FORMAT_YUV420;aIgnoreAlpha:boolean=false): NativeUInt;
//aBuffer  12 first bytes of file.
function AvifValidateHeaderSignature(aBuffer: Pointer): boolean;

implementation

uses
  Math;

type
  PavifIOStreamReader = ^avifIOStreamReader;

  avifIOStreamReader = record
    io: avifIO; // this must be the first member for easy casting to avifIO*
    buffer: avifRWData;
    Stream: TStream;
  end;

  TAvifImageBase = class
  public
    procedure Init(aImagePtr: Pointer); virtual; abstract;
    function GetTransformFlags: longword; virtual; abstract;
    function GetImirMode: uint8; virtual; abstract;
    procedure SetColorPrimaries(AColorPrimaries: avifColorPrimaries); virtual; abstract;
    procedure SetTransferCharacteristics(ATC: avifTransferCharacteristics); virtual; abstract;
    procedure SetMatrixCoefficients(AMC: avifMatrixCoefficients); virtual; abstract;
    procedure SetYuvRange(AYR: avifRange); virtual; abstract;
    procedure SetTransformFlags(ATF: avifTransformFlags); virtual; abstract;
    procedure SetImirMode(AIM: uint8); virtual; abstract;
  end;

  generic TAvifImage<T> = class(TAvifImageBase)
  protected
    FImage: T;
  public
    constructor Create(aImagePtr: Pointer = nil);
    procedure Init(aImagePtr: Pointer); override;
    function GetTransformFlags: longword; override;
    function GetImirMode: uint8; override;
    procedure SetColorPrimaries(AColorPrimaries: avifColorPrimaries); override;
    procedure SetTransferCharacteristics(ATC: avifTransferCharacteristics); override;
    procedure SetMatrixCoefficients(AMC: avifMatrixCoefficients); override;
    procedure SetYuvRange(AYR: avifRange); override;
    procedure SetTransformFlags(ATF: avifTransformFlags); override;
    procedure SetImirMode(AIM: uint8); override;
  end;

  TDecoderBase = class
  public
    procedure Init(aDecoderPtr: Pointer); virtual; abstract;
    function GetImage: PavifImage; virtual; abstract;
    function GetIo:PavifIO; virtual; abstract;
    function GetDecoder:PavifDecoder; virtual; abstract;
  end;

  generic TDecoder<T> = class(TDecoderBase)
  protected
    FDecoder: T;
  public
    constructor Create(aDecoderPtr: Pointer = nil);
    procedure Init(aDecoderPtr: Pointer); override;
    function GetImage: PavifImage; override;
    function GetIo:PavifIO; override;
    function GetDecoder:PavifDecoder; override;
  end;

  TEncoderBase = class
  public
    procedure Init(aEncoderPtr: Pointer); virtual; abstract;
    procedure SetMaxThreads(AMT: integer); virtual; abstract;
    procedure SetSpeed(ASpeed: integer); virtual; abstract;
    procedure SetMinQuantizer(AMinQ: integer); virtual; abstract;
    procedure SetMaxQuantizer(AMQ: integer); virtual; abstract;
    procedure SetMinQuantizerAlpha(AMinQ: integer); virtual; abstract;
    procedure SetMaxQuantizerAlpha(AMQ: integer); virtual; abstract;
    procedure SetCodecChoice(AMQ: avifCodecChoice); virtual; abstract;
  end;

  generic TEncoder<T> = class(TEncoderBase)
  protected
    FEncoder: T;
  public
    constructor Create(aEncoderPtr: Pointer = nil);
    procedure Init(aEncoderPtr: Pointer); override;
    procedure SetMaxThreads(AMT: integer); override;
    procedure SetSpeed(ASpeed: integer); override;
    procedure SetMinQuantizer(AMinQ: integer); override;
    procedure SetMaxQuantizer(AMQ: integer); override;
    procedure SetMinQuantizerAlpha(AMinQ: integer); override;
    procedure SetMaxQuantizerAlpha(AMQ: integer); override;
    procedure SetCodecChoice(ACC: avifCodecChoice); override;
  end;

  TAvifRGBImageBase = class
  public
    function GetRgbImage: PavifRGBImage; virtual; abstract;
    function GetWidth:UInt32; virtual; abstract;
    function GetHeight:UInt32; virtual; abstract;
    procedure SetWidth(aWidth:UInt32); virtual; abstract;
    procedure SetHeight(aHeight:UInt32); virtual; abstract;
    procedure SetPixels(aPixels: PUInt8); virtual; abstract;
    procedure SetDepth(aDepth:UInt32); virtual; abstract;
    procedure SetFormat(aFormat:avifRGBFormat); virtual; abstract;
    procedure SetRowBytes(aRowBytes : UInt32); virtual; abstract;
    procedure SetIgnoreAlpha(AIgnoreAlpha : avifBool); virtual; abstract;
  end;

  TAvifRGBImageField = record
    case integer of
      0: (rgb0_8: avifRGBImage0_8_4);
      1: (rgb0_10: avifRGBImage0_10_0);
      2: (rgb0_11: avifRGBImage0_11_0);
  end;

  { TAvifRGBImage }

  generic TAvifRGBImage<T> = class(TAvifRGBImageBase)
  protected
    wrgb: TAvifRGBImageField;
    FImage: T;
  public
    constructor Create;
    function GetRgbImage: PavifRGBImage; override;
    function GetWidth:UInt32; override;
    function GetHeight:UInt32; override;
    procedure SetWidth(aWidth:UInt32); override;
    procedure SetHeight(aHeight:UInt32); override;
    procedure SetPixels(aPixels: PUInt8); override;
    procedure SetDepth(aDepth:UInt32); override;
    procedure SetFormat(aFormat:avifRGBFormat); override;
    procedure SetRowBytes(aRowBytes : UInt32); override;
    procedure SetIgnoreAlpha(AIgnoreAlpha : avifBool); override;
  end;

function TAvifImageFactory(aImagePtr: Pointer): TAvifImageBase;
begin
  if AVIF_VERSION >= AVIF_VERSION_0_11_0 then
    Result := specialize TAvifImage<PavifImage0_11_0>.Create(aImagePtr)
  else
    Result := specialize TAvifImage<PavifImage0_8_4>.Create(aImagePtr);
end;

function TDecoderFactory(aDecoderPtr: Pointer): TDecoderBase;
begin
  if AVIF_VERSION >= AVIF_VERSION_0_11_0 then
     result := specialize TDecoder<PAvifDecoder0_11_0>.Create(aDecoderPtr)
  else if AVIF_VERSION >= AVIF_VERSION_0_10_0 then
     result := specialize TDecoder<PAvifDecoder0_10_0>.Create(aDecoderPtr)
  else if AVIF_VERSION >= AVIF_VERSION_0_9_3 then
     result := specialize TDecoder<PAvifDecoder0_9_3>.Create(aDecoderPtr)
  else if AVIF_VERSION >= AVIF_VERSION_0_9_2 then
     result := specialize TDecoder<PAvifDecoder0_9_2>.Create(aDecoderPtr)
  else
     result := specialize TDecoder<PAvifDecoder0_8_4>.Create(aDecoderPtr);
end;

function TEncoderFactory(aEncoderPtr: Pointer): TEncoderBase;
begin
  if AVIF_VERSION >= AVIF_VERSION_0_11_0 then
    result:=specialize TEncoder<PAvifEncoder0_11_0>.Create(aEncoderPtr)
  else
    result:=specialize TEncoder<PAvifEncoder>.Create(aEncoderPtr);
end;

function TAvifRGBImageFactory(): TAvifRGBImageBase;
begin
  if AVIF_VERSION >= AVIF_VERSION_0_11_0 then
    result:=specialize TAvifRGBImage<PavifRGBImage0_11_0>.Create
  else if AVIF_VERSION >= AVIF_VERSION_0_10_0 then
    result:=specialize TAvifRGBImage<PavifRGBImage0_10_0>.Create
  else
    result:=specialize TAvifRGBImage<PavifRGBImage0_8_4>.Create;
end;

constructor TDecoder.Create(aDecoderPtr:Pointer);
begin
  Init(aDecoderPtr);
end;

procedure TDecoder.Init(aDecoderPtr:Pointer);
begin
  FDecoder:=T(aDecoderPtr);
end;

function TDecoder.GetImage:PavifImage;
begin
  result := FDecoder^.image;
end;

function TDecoder.GetIo:PavifIO;
begin
  result := FDecoder^.io;
end;

function TDecoder.GetDecoder:PavifDecoder;
begin
  result := FDecoder;
end;

constructor TAvifRGBImage.Create;
begin
  wrgb := Default(TAvifRGBImageField);
  FImage:= T(@wrgb);
end;

function TAvifRGBImage.GetRgbImage: Pointer;
begin
  result := FImage;
end;

function TAvifRGBImage.GetWidth:UInt32;
begin
  result := FImage^.width;
end;

function TAvifRGBImage.GetHeight:UInt32;
begin
  result := FImage^.height;
end;

procedure TAvifRGBImage.SetWidth(aWidth:UInt32);
begin
  FImage^.width:=aWidth;
end;

procedure TAvifRGBImage.SetHeight(aHeight:UInt32);
begin
  FImage^.height:=aHeight;
end;

procedure TAvifRGBImage.SetPixels(aPixels:PUInt8);
begin
  FImage^.pixels:=aPixels;
end;

procedure TAvifRGBImage.SetDepth(aDepth:UInt32);
begin
  FImage^.depth:=aDepth;
end;

procedure TAvifRGBImage.SetFormat(aFormat:avifRGBFormat);
begin
  FImage^.format:=aFormat;
end;

procedure TAvifRGBImage.SetRowBytes(aRowBytes:UInt32);
begin
  FImage^.rowBytes:=aRowBytes;
end;

procedure TAvifRGBImage.SetIgnoreAlpha(AIgnoreAlpha:avifBool);
begin
  FImage^.ignoreAlpha:=aIgnoreAlpha;
end;

constructor TEncoder.Create(aEncoderPtr: Pointer);
begin
  Init(aEncoderPtr);
end;

procedure TEncoder.Init(aEncoderPtr: Pointer);
begin
  FEncoder := T(aEncoderPtr);
end;

procedure TEncoder.SetMaxThreads(AMT: integer);
begin
  FEncoder^.maxThreads := AMT;
end;

procedure TEncoder.SetSpeed(ASpeed: integer);
begin
  FEncoder^.speed := ASpeed;
end;

procedure TEncoder.SetMinQuantizer(AMinQ: integer);
begin
  FEncoder^.minQuantizer := AMinQ;
end;

procedure TEncoder.SetMaxQuantizer(AMQ: integer);
begin
  FEncoder^.maxQuantizer := AMQ;
end;

procedure TEncoder.SetMinQuantizerAlpha(AMinQ: integer);
begin
  FEncoder^.minQuantizerAlpha := AMinQ;
end;

procedure TEncoder.SetMaxQuantizerAlpha(AMQ: integer);
begin
  FEncoder^.maxQuantizerAlpha := AMQ;
end;

procedure TEncoder.SetCodecChoice(ACC: avifCodecChoice);
begin
  FEncoder^.codecChoice := ACC;
end;

constructor TAvifImage.Create(aImagePtr: Pointer);
begin
  Init(aImagePtr);
end;

procedure TAvifImage.Init(aImagePtr: Pointer);
begin
  FImage := T(aImagePtr);
end;

function TAvifImage.GetTransformFlags: longword;
begin
  Result := FImage^.transformFlags;
end;

function TAvifImage.GetImirMode: uint8;
begin
  Result := FImage^.imir.mode;
end;

procedure TAvifImage.SetColorPrimaries(AColorPrimaries: avifColorPrimaries);
begin
  FImage^.colorPrimaries := AColorPrimaries;
end;

procedure TAvifImage.SetTransferCharacteristics(ATC: avifTransferCharacteristics);
begin
  FImage^.transferCharacteristics := ATC;
end;

procedure TAvifImage.SetMatrixCoefficients(AMC: avifMatrixCoefficients);
begin
  FImage^.matrixCoefficients := AMC;
end;

procedure TAvifImage.SetYuvRange(AYR: avifRange);
begin
  FImage^.yuvRange := AYR;
end;

procedure TAvifImage.SetTransformFlags(ATF: avifTransformFlags);
begin
  FImage^.transformFlags := ATF;
end;

procedure TAvifImage.SetImirMode(AIM: uint8);
begin
  FImage^.imir.mode := AIM;
end;

//aBuffer  12 first bytes of file.
function AvifValidateHeaderSignature(aBuffer: Pointer): boolean;
begin
  if CompareMem(aBuffer + 4, pansichar('ftyp'), 4) then
  begin
    if CompareMem(aBuffer + 8, pansichar('avif'), 4) then
      exit(True);
    if CompareMem(aBuffer + 8, pansichar('avis'), 4) then
      exit(True);
    if CompareMem(aBuffer + 8, pansichar('mif1'), 4) then
      exit(True);
  end;
  Result := False;
end;

function avifIOStreamReaderRead(io: PavifIO; readFlags: uint32; offset: uint64; size: size_type; output: PavifROData): avifResult; cdecl;
var
  reader: PavifIOStreamReader;
  availableSize: uint64;
  bytesRead: size_type;
begin
  if readFlags <> 0 then
    exit(AVIF_RESULT_IO_ERROR);  // Unsupported readFlags
  reader := PavifIOStreamReader(io);
  // Sanitize/clamp incoming request
  if offset > reader^.io.sizeHint then
    exit(AVIF_RESULT_IO_ERROR);  // The offset is past the EOF.
  availableSize := reader^.io.sizeHint - offset;
  if size > availableSize then
    size := availableSize;
  if size > 0 then
  begin
    if (offset > MaxLongInt) then
      exit(AVIF_RESULT_IO_ERROR);
    if reader^.buffer.size < size then
      avifRWDataRealloc(@reader^.buffer, size);
    if (reader^.Stream.Seek(offset, soFromBeginning) <> offset) then
      exit(AVIF_RESULT_IO_ERROR);
    bytesRead := reader^.Stream.Read(reader^.buffer.Data^, size);
    if size <> bytesRead then
      size := bytesRead;
  end;
  output^.Data := reader^.buffer.Data;
  output^.size := size;
  exit(AVIF_RESULT_OK);
end;

procedure avifIOStreamReaderDestroy(io: PavifIO); cdecl;
var
  reader: PavifIOStreamReader;
begin
  reader := PavifIOStreamReader(io);
  avifRWDataFree(@reader^.buffer);
  avifFree(io);
end;

function avifIOCreateStreamReader(aStream: TStream): PavifIO; cdecl;
var
  reader: PavifIOStreamReader;
  filesize: longint;
begin
  filesize := aStream.Size;
  //aStream.Position:=0;
  reader := avifAlloc(sizeof(avifIOStreamReader));
  FillChar(reader^, sizeof(avifIOStreamReader), 0);
  reader^.Stream := aStream;
  reader^.io.Destroy := @avifIOStreamReaderDestroy;
  reader^.io.Read := @avifIOStreamReaderRead;
  reader^.io.sizeHint := fileSize;
  reader^.io.persistent := AVIF_FALSE;
  avifRWDataRealloc(@reader^.buffer, 1024);
  exit(PavifIO(reader));
end;

function avifDecoderSetIOStream(decoderWrap: TDecoderBase; aStream: TStream): avifResult; cdecl;
var
  io: PavifIO;
begin
  io := avifIOCreateStreamReader(aStream);
  if io = nil then
    exit(AVIF_RESULT_IO_ERROR);
  avifDecoderSetIO(decoderWrap.GetDecoder, io);
  if decoderWrap.GetIo = nil then
    raise EAvifException.Create('Failed to set input. Could be due to incompatible version of AVIF library.');
  exit(AVIF_RESULT_OK);
end;

procedure AvifDecode(decoderWrap: TDecoderBase; aBitmap: TBGRACustomBitmap);
var
  res: avifResult;
  imageWrap: TAvifImageBase;
  rgbImageWrap: TAvifRgbImageBase;
begin
  res := avifDecoderParse(decoderWrap.GetDecoder);
  if res <> AVIF_RESULT_OK then
    raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
  //  Memo1.Lines.Add(Format('Parsed AVIF: %ux%u (%ubpc)', [decoder^.image^.Width, decoder^.image^.Height, decoder^.image^.depth]));
  if avifDecoderNextImage(decoderWrap.GetDecoder) = AVIF_RESULT_OK then
  begin
    if decoderWrap.GetImage = nil then
      raise EAvifException.Create('No image data recieved from AVIF library.');

    imageWrap:=nil;
    rgbImageWrap:=nil;
    try
      rgbImageWrap:=TAvifRgbImageFactory();
      imageWrap:=TAvifImageFactory(decoderWrap.GetImage);
      avifRGBImageSetDefaults(rgbImageWrap.GetRgbImage, decoderWrap.GetImage);
      //aBitmap.LineOrder:=riloTopToBottom;
      aBitmap.SetSize(rgbImageWrap.GetWidth, rgbImageWrap.GetHeight);
      rgbImageWrap.SetPixels(PUint8(aBitmap.databyte));
      rgbImageWrap.SetDepth(8);
      {$push}{$warn 6018 off}//unreachable code
      if TBGRAPixel_RGBAOrder then
        rgbImageWrap.SetFormat(AVIF_RGB_FORMAT_RGBA)
      else
        rgbImageWrap.SetFormat(AVIF_RGB_FORMAT_BGRA);
      {$pop}
      rgbImageWrap.SetRowBytes(rgbImageWrap.GetWidth * 4);
      //if aBitmap.LineOrder<>riloTopToBottom then
      //begin
      //  decoder^.image^.transformFlags:=decoder^.image^.transformFlags + Uint32(AVIF_TRANSFORM_IMIR);
      //  decoder^.image^.imir.mode:=0;
      //end;
      //decoder^.image^.imir.axis:=0; //vertical mirror
      res := avifImageYUVToRGB(decoderWrap.GetImage, rgbImageWrap.GetRgbImage);
      if res <> AVIF_RESULT_OK then
        raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
      if (aBitmap.LineOrder <> riloTopToBottom) and not
        (( imageWrap.GetTransformFlags and longword(AVIF_TRANSFORM_IMIR) ) = longword(AVIF_TRANSFORM_IMIR) ) and
        (imageWrap.GetImirMode = 0) then
        aBitmap.VerticalFlip;
      aBitmap.InvalidateBitmap;
    finally
      imageWrap.Free;
      rgbImageWrap.Free;
    end;
  end
  else
    raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
end;

procedure AvifLoadFromStream(AStream: TStream; aBitmap: TBGRACustomBitmap);
var
  decoder: PavifDecoder;
  res: avifResult;
  decoderWrap: TDecoderBase;
begin
  decoderWrap:=nil;
  decoder := avifDecoderCreate();
  decoderWrap:=TDecoderFactory(decoder);
  try
    // Override decoder defaults here (codecChoice, requestedSource, ignoreExif, ignoreXMP, etc)
    //decoder^.maxThreads := 1;
    // decoder^.codecChoice := AVIF_CODEC_CHOICE_AUTO;
    // decoder^.imageSizeLimit := AVIF_DEFAULT_IMAGE_SIZE_LIMIT;
    // decoder^.strictFlags := UInt32( AVIF_STRICT_ENABLED);
    // decoder^.allowProgressive := AVIF_FALSE;
    res := avifDecoderSetIOStream(decoderWrap, aStream);
    if res = AVIF_RESULT_OK then
      AvifDecode(decoderWrap, aBitmap)
    else
      raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
  finally
    avifDecoderDestroy(decoder);
    decoderWrap.Free;
  end;
end;

procedure AvifLoadFromFile(const AFilename: string; aBitmap: TBGRACustomBitmap);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    AvifLoadFromStream(Stream,aBitmap);
  finally
    Stream.Free;
  end;
end;

procedure AvifLoadFromFileNative(const AFilename: string; aBitmap: TBGRACustomBitmap);
var
  decoder: PavifDecoder;
  res: avifResult;
  decoderWrap: TDecoderBase;
begin
  decoderWrap := nil;
  decoder := avifDecoderCreate();
  decoderWrap:=TDecoderFactory(decoder);
  try
    // Override decoder defaults here (codecChoice, requestedSource, ignoreExif, ignoreXMP, etc)
    //decoder^.maxThreads := 1;
    // decoder^.codecChoice := AVIF_CODEC_CHOICE_AUTO;
    // decoder^.imageSizeLimit := AVIF_DEFAULT_IMAGE_SIZE_LIMIT;
    // decoder^.strictFlags := UInt32( AVIF_STRICT_ENABLED);
    // decoder^.allowProgressive := AVIF_FALSE;
    res := avifDecoderSetIOFile(decoder, pansichar(AFilename));
    if res = AVIF_RESULT_OK then
      AvifDecode(decoderWrap, aBitmap)
    else
      raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
  finally
    avifDecoderDestroy(decoder);
    decoderWrap.Free;
  end;
end;

procedure AvifLoadFromMemory(AData: Pointer; ASize: cardinal; aBitmap: TBGRACustomBitmap);
var
  decoder: PavifDecoder;
  res: avifResult;
  decoderWrap: TDecoderBase;
begin
  decoderWrap := nil;
  decoder := avifDecoderCreate();
  decoderWrap:=TDecoderFactory(decoder);
  try
    // Override decoder defaults here (codecChoice, requestedSource, ignoreExif, ignoreXMP, etc)
    //decoder^.maxThreads := 1;
    // decoder^.codecChoice := AVIF_CODEC_CHOICE_AUTO;
    // decoder^.imageSizeLimit := AVIF_DEFAULT_IMAGE_SIZE_LIMIT;
    // decoder^.strictFlags := UInt32( AVIF_STRICT_ENABLED);
    // decoder^.allowProgressive := AVIF_FALSE;
    res := avifDecoderSetIOMemory(decoder, AData, ASize);
    if res = AVIF_RESULT_OK then
      AvifDecode(decoderWrap, aBitmap)
    else
      raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
  finally
    avifDecoderDestroy(decoder);
    decoderWrap.Free;
  end;
end;

function Interpolate(x: double; x1, x2: double; v1, v2: double): double;
begin
  Result := v1 + (((v2 - v1) / (x2 - x1)) * (x - x1));
end;


function clamp(aValue:integer;aMin:integer;aMax:integer):integer;
begin
  result:=aValue;
  if result<aMin then
    result:=aMin;
  if result>aMax then
    result:=aMax;
end;

//https://github.com/AOMediaCodec/libavif/issues/545
//https://gitmemory.com/issue/AOMediaCodec/libavif/545/802934788
// aQuality0to100   0 worst quality, 100 best quality ( lossless ).
procedure AvifEncode(aBitmap: TBGRACustomBitmap; aQuality0to100: integer; aSpeed0to10:integer; var avifOutput: avifRWData;aPixelFormat:avifPixelFormat=AVIF_PIXEL_FORMAT_YUV420;aIgnoreAlpha:boolean=false);
var
  encoder: PavifEncoder;
  image: PavifImage;
  imageWrap:TAvifImageBase;
  encoderWrap:TEncoderBase;
  convertResult, addImageResult, finishResult: avifResult;
  alpha_quantizer, min_quantizer, max_quantizer: integer;
  quality: integer;
  rgbImageWrap:TAvifRgbImageBase;
const
  LOSS_LESS_IMAGE_QUALITY = 100;
begin
  encoder := nil;
  rgbImageWrap:=nil;
  try
    if aQuality0to100 = LOSS_LESS_IMAGE_QUALITY then
      image := avifImageCreate(aBitmap.Width, aBitmap.Height, 8, AVIF_PIXEL_FORMAT_YUV444)
    else
      image := avifImageCreate(aBitmap.Width, aBitmap.Height, 8, aPixelFormat{AVIF_PIXEL_FORMAT_YUV420}{AVIF_PIXEL_FORMAT_YUV444});
    imageWrap := TAvifImageFactory(image);
    // these values dictate what goes into the final AVIF
    // Configure image here: (see avif/avif.h)
    // * colorPrimaries
    // * transferCharacteristics
    // * matrixCoefficients
    // * avifImageSetProfileICC()
    // * avifImageSetMetadataExif()
    // * avifImageSetMetadataXMP()
    // * yuvRange
    // * alphaRange
    // * alphaPremultiplied
    // * transforms (transformFlags, pasp, clap, irot, imir)
    imageWrap.SetColorPrimaries(AVIF_COLOR_PRIMARIES_BT709);
    imageWrap.SetTransferCharacteristics(AVIF_TRANSFER_CHARACTERISTICS_SRGB);
    imageWrap.SetMatrixCoefficients(AVIF_MATRIX_COEFFICIENTS_BT601);

    // Override RGB(A)->YUV(A) defaults here: depth, format, chromaUpsampling, ignoreAlpha, alphaPremultiplied, libYUVUsage, etc
    // Alternative: set rgb.pixels and rgb.rowBytes yourself, which should match your chosen rgb.format
    // Be sure to use uint16_t* instead of uint8_t* for rgb.pixels/rgb.rowBytes if (rgb.depth > 8)
    rgbImageWrap:=TAvifRgbImageFactory();
    imageWrap:=TAvifImageFactory(image);
    // If you have RGB(A) data you want to encode, use this path
    avifRGBImageSetDefaults(rgbImageWrap.GetRgbImage, image);
    rgbImageWrap.SetWidth(aBitmap.Width);
    rgbImageWrap.SetHeight(aBitmap.Height);
    {$push}{$warn 6018 off}//unreachable code
    if TBGRAPixel_RGBAOrder then
      rgbImageWrap.SetFormat(AVIF_RGB_FORMAT_RGBA)
    else
      rgbImageWrap.SetFormat(AVIF_RGB_FORMAT_BGRA);
    {$pop}
    if aIgnoreAlpha then
      rgbImageWrap.SetIgnoreAlpha(AVIF_TRUE)
    else
      rgbImageWrap.SetIgnoreAlpha(AVIF_FALSE);
    rgbImageWrap.SetPixels(aBitmap.DataByte);
    rgbImageWrap.SetRowBytes(aBitmap.Width * 4);
    if aQuality0to100 = LOSS_LESS_IMAGE_QUALITY then
    begin
      imageWrap.SetYuvRange(AVIF_RANGE_FULL);
      imageWrap.SetMatrixCoefficients(AVIF_MATRIX_COEFFICIENTS_IDENTITY); // this is key for lossless
    end;
    convertResult := avifImageRGBToYUV(image, rgbImageWrap.GetRgbImage);
    if convertResult <> AVIF_RESULT_OK then
      raise EAvifException.Create('Failed to convert to YUV(A): ' + avifResultToString(convertResult));
    encoder := avifEncoderCreate();
    encoderWrap := TEncoderFactory(encoder);

    // Configure your encoder here (see avif/avif.h):
    // * maxThreads
    // * minQuantizer
    // * maxQuantizer
    // * minQuantizerAlpha
    // * maxQuantizerAlpha
    // * tileRowsLog2
    // * tileColsLog2
    // * speed
    // * keyframeInterval
    // * timescale

    aQuality0to100:=clamp(aQuality0to100,0,100);
    if aSpeed0to10<>AVIF_SPEED_DEFAULT then
      aSpeed0to10:=clamp(aSpeed0to10,AVIF_SPEED_SLOWEST,AVIF_SPEED_FASTEST);

    encoderWrap.SetMaxThreads(2);
    encoderWrap.SetSpeed(aSpeed0to10);
    if aQuality0to100 = LOSS_LESS_IMAGE_QUALITY then
    begin
      // Set defaults, and warn later on if anything looks incorrect
      //input.requestedFormat = AVIF_PIXEL_FORMAT_YUV444; // don't subsample when using AVIF_MATRIX_COEFFICIENTS_IDENTITY
      encoderWrap.SetMinQuantizer(AVIF_QUANTIZER_LOSSLESS);
      encoderWrap.SetMaxQuantizer(AVIF_QUANTIZER_LOSSLESS);
      encoderWrap.SetMinQuantizerAlpha(AVIF_QUANTIZER_LOSSLESS);
      encoderWrap.SetMaxQuantizerAlpha(AVIF_QUANTIZER_LOSSLESS);
      encoderWrap.SetCodecChoice(AVIF_CODEC_CHOICE_AOM);              // rav1e doesn't support lossless transform yet:
      // https://github.com/xiph/rav1e/issues/151
      imageWrap.SetYuvRange(AVIF_RANGE_FULL); // avoid limited range
      imageWrap.SetMatrixCoefficients(AVIF_MATRIX_COEFFICIENTS_IDENTITY); // this is key for lossless
    end
    else
    begin
      // CONVERT 0..100  TO 63..0
      quality := Trunc(interpolate(aQuality0to100, 0, 100, AVIF_QUANTIZER_WORST_QUALITY, AVIF_QUANTIZER_BEST_QUALITY));
      max_quantizer := quality;
      min_quantizer := 0;
      alpha_quantizer := 0;
      if (max_quantizer > 20) then
      begin
        min_quantizer := max_quantizer - 20;
        if (max_quantizer > 40) then
          alpha_quantizer := max_quantizer - 40;
      end;
      encoderWrap.SetMinQuantizer(min_quantizer);
      encoderWrap.SetMaxQuantizer(max_quantizer);
      encoderWrap.SetMinQuantizerAlpha(0);
      encoderWrap.SetMaxQuantizerAlpha(alpha_quantizer);
    end;

    if aBitmap.LineOrder <> riloTopToBottom then   //vertical mirror.
    begin
      imageWrap.SetTransformFlags(imageWrap.GetTransformFlags + uint32(AVIF_TRANSFORM_IMIR));
      imageWrap.SetImirMode( 0 );
    end;

    // Call avifEncoderAddImage() for each image in your sequence
    // Only set AVIF_ADD_IMAGE_FLAG_SINGLE if you're not encoding a sequence
    // Use avifEncoderAddImageGrid() instead with an array of avifImage* to make a grid image
    addImageResult := avifEncoderAddImage(encoder, image, 1, uint32(AVIF_ADD_IMAGE_FLAG_SINGLE));
    if addImageResult <> AVIF_RESULT_OK then
      raise EAvifException.Create('Failed to add image to encoder: ' + avifResultToString(addImageResult));
    finishResult := avifEncoderFinish(encoder, @avifOutput);
    if finishResult <> AVIF_RESULT_OK then
      raise EAvifException.Create('Failed to finish encode: ' + avifResultToString(finishResult));
    //Memo1.Lines.Add('Encode success, total bytes: ' + IntToStr(avifOutput.size));
  finally
    if image <> nil then
      avifImageDestroy(image);
    if encoder <> nil then
      avifEncoderDestroy(encoder);
    imageWrap.Free;
    encoderWrap.Free;
    rgbImageWrap.Free;
  end;
end;

function AvifSaveToFile(aBitmap: TBGRACustomBitmap; const AFilename: string; aQuality0to100: integer;aSpeed0to10:integer;aPixelFormat:avifPixelFormat;aIgnoreAlpha:boolean): NativeUInt;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    Result := AvifSaveToStream(aBitmap, Stream, aQuality0to100, aSpeed0to10,aPixelFormat,aIgnoreAlpha);
  finally
    Stream.Free;
  end;
end;

function AvifSaveToStream(aBitmap: TBGRACustomBitmap; AStream: TStream; aQuality0to100: integer;aSpeed0to10:integer;aPixelFormat:avifPixelFormat;aIgnoreAlpha:boolean): NativeUInt;
var
  avifOutput: avifRWData;
  p:PByte;
  remain,toWrite:LongWord;
const
  CopySize=65535;
begin
  try
    avifOutput := AVIF_DATA_EMPTY;
    AvifEncode(aBitmap, aQuality0to100,aSpeed0to10, avifOutput,aPixelFormat,aIgnoreAlpha);
    Result := avifOutput.size;
    if avifOutput.Data <> nil then
    begin
      //AStream.WriteBuffer(avifOutput.Data^, avifOutput.size)
      remain := avifOutput.size;
      p := avifOutput.Data;
      while remain > 0 do
      begin
        if remain > CopySize then
          toWrite := CopySize
        else
          toWrite := remain;
        AStream.WriteBuffer(p^, toWrite);
        inc(p, toWrite);
        dec(remain, toWrite);
      end;
    end
    else
      Result := 0;
  finally
    avifRWDataFree(@avifOutput);
  end;
end;

//returns the size of the resulting bitmap.
function AvifSaveToMemory(aBitmap: TBGRACustomBitmap; AData: Pointer; ASize: cardinal; aQuality0to100: integer;aSpeed0to10:integer;aPixelFormat:avifPixelFormat;aIgnoreAlpha:boolean): NativeUInt;
var
  avifOutput: avifRWData;
begin
  try
    avifOutput := AVIF_DATA_EMPTY;
    AvifEncode(aBitmap, aQuality0to100, aSpeed0to10, avifOutput,aPixelFormat,aIgnoreAlpha);
    Result := avifOutput.size;
    if avifOutput.Data <> nil then
      Move(avifOutput.Data^, aData^, min(ASize, avifOutput.size));
  finally
    avifRWDataFree(@avifOutput);
  end;
end;

end.
