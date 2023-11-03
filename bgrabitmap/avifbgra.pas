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
  avifCodecChoice = libavif.avifCodecChoice;

const
  DEFAULT_TIMESCALE = 30;
  DEFAULT_ENCODER = AVIF_CODEC_CHOICE_AUTO;
  DEFAULT_QUALITY = 30;
  DEFAULT_QUALITY_ALPHA = 50;

type
  { To read one or more images }

  TAvifReader = class
  protected
    FDecoder: PavifDecoder;
    FDecoderWrap: TObject; //TDecoderBase;
    FStream: TStream;
    FImageIndex: integer;
    FImageCount: uint32;
    FImageDurationSeconds: double;
    FImageDurationTimescales: uint64;
    FSequenceDuration: double;
    FRepetitionCount: integer;
    FInitOk: boolean;
    FWidth: uint32;
    FHeight: uint32;
    FTimescale: uint64;   // if all images have 1 timescale of duration is the same as FPS.
    procedure Init(AStream: TStream);
  public
    constructor Create(AFileName: string); virtual; overload;
    constructor Create(AStream: TStream); virtual; overload;
    destructor Destroy; override;
    //WARNING: Before unload the libavif we MUST close or Free all readers.
    procedure Close;

    function GetNextImage(AOutBitmap: TBGRACustomBitmap): boolean;
    function GetNthImage(AOutBitmap: TBGRACustomBitmap; AImageIndex: uint32): boolean;
    procedure SetDecoder(ACodec: avifCodecChoice);
    property ImageIndex: integer read FImageIndex;
    property ImageCount: uint32 read FImageCount;
    property ImageDurationSeconds: double read FImageDurationSeconds;
    property ImageDurationTimescales: uint64 read FImageDurationTimescales;
    property SequenceDuration: double read FSequenceDuration;
    property RepetitionCount: integer read FRepetitionCount;
    property InitOk: boolean read FInitOk;
    property Width: uint32 read FWidth;
    property Height: uint32 read FHeight;
    property Timescale: uint64 read FTimescale;
  end;


  { To encode one or more images in a sequence }

  TAvifWriter = class
  protected
    FEncoder: PAvifEncoder;
    FEncoderWrap: TObject; //TEncoderBase;
    FInitOk: boolean;
    FQuality0to100: integer;
    FPixelFormat: avifPixelFormat;
    FIgnoreAlpha: boolean;
    FTimescale: uint64;
    FAvifOutput: avifRWData;
    FOnlyOneImage: boolean;
    FImagesCount: uint32;
    procedure EncoderFinish;
    procedure SetMaxThreads(AMT: integer);
    procedure SetMinQuantizer(AMinQ: integer);
    procedure SetMaxQuantizer(AMaxQ: integer);
    procedure SetMinQuantizerAlpha(AMinQ: integer);
    procedure SetMaxQuantizerAlpha(AMaxQ: integer);
    procedure SetIgnoreAlpha(AValue: boolean);
    function GetMaxThreads: integer;
    function GetMinQuantizer: integer;
    function GetMaxQuantizer: integer;
    function GetMinQuantizerAlpha: integer;
    function GetMaxQuantizerAlpha: integer;
  public
    constructor Create(AQuality0to100: integer = DEFAULT_QUALITY; ASpeed0to10: integer = AVIF_SPEED_DEFAULT; APixelFormat: avifPixelFormat = AVIF_PIXEL_FORMAT_YUV420;
      AIgnoreAlpha: boolean = False;ACodec:avifCodecChoice=AVIF_CODEC_CHOICE_AUTO); virtual; overload;
    procedure Close;
    destructor Destroy; override;
    procedure AddImage(ABitmap: TBGRACustomBitmap; ADurationMs: cardinal=0);
    function SaveToFile(AFileName: string): NativeUInt;
    function SaveToStream(AStream: TStream): NativeUInt;
    function SaveToMemory(AData: Pointer; ASize: NativeUInt): NativeUInt;
    function GetOutputSize: NativeUInt;
    procedure SetEncoder(ACodec: avifCodecChoice);
    procedure SetTimeScale(ATimeScale: uint64);
    procedure SetQuality(AValue: integer);
    procedure SetQualityAlpha(AValue: integer);
    property InitOk: boolean read FInitOk;
    property OnlyOneImage: boolean read FOnlyOneImage write FOnlyOneImage;
    property MaxThreads: integer read GetMaxThreads write SetMaxThreads;
    property MinQuantizer: integer read GetMinQuantizer write SetMinQuantizer;
    property MaxQuantizer: integer read GetMaxQuantizer write SetMaxQuantizer;
    property MinQuantizerAlpha: integer read GetMinQuantizerAlpha write SetMinQuantizerAlpha;
    property MaxQuantizerAlpha: integer read GetMaxQuantizerAlpha write SetMaxQuantizerAlpha;
    property TimeScale: uint64 read FTimeScale write SetTimeScale;
    property IgnoreAlpha: boolean read FIgnoreAlpha write SetIgnoreAlpha;
  end;


procedure AvifLoadFromStream(AStream: TStream; aBitmap: TBGRACustomBitmap);
procedure AvifLoadFromFile(const AFilename: string; aBitmap: TBGRACustomBitmap);
procedure AvifLoadFromFileNative(const AFilename: string; aBitmap: TBGRACustomBitmap);
procedure AvifLoadFromMemory(AData: Pointer; ASize: cardinal; aBitmap: TBGRACustomBitmap);
//a Quality0to100=100 LOSSLESS
function AvifSaveToStream(aBitmap: TBGRACustomBitmap; AStream: TStream; aQuality0to100: integer = 30;aSpeed0to10:integer=AVIF_SPEED_DEFAULT;aPixelFormat:avifPixelFormat=AVIF_PIXEL_FORMAT_YUV420;aIgnoreAlpha:boolean=false): NativeUInt;
function AvifSaveToFile(aBitmap: TBGRACustomBitmap; const AFilename: string; aQuality0to100: integer = 30;aSpeed0to10:integer=AVIF_SPEED_DEFAULT;aPixelFormat:avifPixelFormat=AVIF_PIXEL_FORMAT_YUV420;aIgnoreAlpha:boolean=false): NativeUInt;
//returns size of the resulting bitmap.
function AvifSaveToMemory(aBitmap: TBGRACustomBitmap; AData: Pointer; ASize: cardinal; aQuality0to100: integer = 30;aSpeed0to10:integer=AVIF_SPEED_DEFAULT;aPixelFormat:avifPixelFormat=AVIF_PIXEL_FORMAT_YUV420;aIgnoreAlpha:boolean=false): NativeUInt;

function AvifSaveToStream(ABitmap: TBGRACustomBitmap; AStream: TStream; AIgnoreAlpha: boolean = False;
  AQuality0to100: integer = DEFAULT_QUALITY; AQualityAlpha0to100: integer = DEFAULT_QUALITY_ALPHA; APixelFormat: avifPixelFormat = AVIF_PIXEL_FORMAT_YUV420;
  ACodec: avifCodecChoice = DEFAULT_ENCODER; ASpeed0to10: integer = AVIF_SPEED_DEFAULT): nativeuint; overload;
function AvifSaveToFile(ABitmap: TBGRACustomBitmap; const AFilename: string; AIgnoreAlpha: boolean = False;
  AQuality0to100: integer = DEFAULT_QUALITY; AQualityAlpha0to100: integer = DEFAULT_QUALITY_ALPHA; APixelFormat: avifPixelFormat = AVIF_PIXEL_FORMAT_YUV420;
  ACodec: avifCodecChoice = DEFAULT_ENCODER; ASpeed0to10: integer = AVIF_SPEED_DEFAULT): nativeuint; overload;
function AvifSaveToMemory(ABitmap: TBGRACustomBitmap; AData: Pointer; ASize: nativeuint; AIgnoreAlpha: boolean = False;
  AQuality0to100: integer = DEFAULT_QUALITY; AQualityAlpha0to100: integer = DEFAULT_QUALITY_ALPHA; APixelFormat: avifPixelFormat = AVIF_PIXEL_FORMAT_YUV420;
  ACodec: avifCodecChoice = DEFAULT_ENCODER; ASpeed0to10: integer = AVIF_SPEED_DEFAULT): nativeuint; overload;

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
    function GetSequenceDuration: double; virtual; abstract;
    function GetImageCount: longint; virtual; abstract;
    function GetImageIndex: longint; virtual; abstract;
    function GetImageDurationSeconds: double; virtual; abstract;
    function GetImageDurationTimescales: UInt64;virtual; abstract;
    function GetTimescale: UInt64; virtual; abstract;
    procedure SetCodecChoice(ACodec:avifCodecChoice); virtual; abstract;
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
    function GetSequenceDuration: double; override;
    function GetImageCount: longint; override;
    function GetImageIndex: longint; override;
    function GetImageDurationSeconds: double; override;
    function GetImageDurationTimescales: UInt64;override;
    function GetTimescale: UInt64; override;
    procedure SetCodecChoice(ACodec:avifCodecChoice); override;
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
    procedure SetTimeScale(aValue:UInt64); virtual; abstract;
    function GetMaxThreads: integer; virtual; abstract;
    function GetMinQuantizer: integer; virtual; abstract;
    function GetMaxQuantizer: integer; virtual; abstract;
    function GetMinQuantizerAlpha: integer; virtual; abstract;
    function GetMaxQuantizerAlpha: integer; virtual; abstract;
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
    procedure SetTimeScale(aValue:UInt64); override;
    function GetMaxThreads: integer; override;
    function GetMinQuantizer: integer; override;
    function GetMaxQuantizer: integer; override;
    function GetMinQuantizerAlpha: integer; override;
    function GetMaxQuantizerAlpha: integer; override;
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
    procedure SetAlphaPremultiplied(aValue:avifBool);virtual; abstract;
  end;

  TAvifRGBImageField = record
    case integer of
      0: (rgb0_8: avifRGBImage0_8_4);
      1: (rgb0_10: avifRGBImage0_10_0);
      2: (rgb0_11: avifRGBImage0_11_0);
      3: (rgb1_00: avifRGBImage1_0_0);
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
    procedure SetAlphaPremultiplied(aValue:avifBool);override;
  end;

function TAvifImageFactory(aImagePtr: Pointer): TAvifImageBase;
begin
  if AVIF_VERSION >= AVIF_VERSION_1_0_0 then
    Result := specialize TAvifImage<PavifImage1_0_0>.Create(aImagePtr)
  else if AVIF_VERSION >= AVIF_VERSION_0_11_0 then
    Result := specialize TAvifImage<PavifImage0_11_0>.Create(aImagePtr)
  else
    Result := specialize TAvifImage<PavifImage0_8_4>.Create(aImagePtr);
end;

function TDecoderFactory(aDecoderPtr: Pointer): TDecoderBase;
begin
  if AVIF_VERSION >= AVIF_VERSION_1_0_0 then
     result := specialize TDecoder<PAvifDecoder1_0_0>.Create(aDecoderPtr)
  else if AVIF_VERSION >= AVIF_VERSION_0_11_0 then
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
  if AVIF_VERSION >= AVIF_VERSION_1_0_0 then
    result:=specialize TEncoder<PAvifEncoder1_0_0>.Create(aEncoderPtr)
  else if AVIF_VERSION >= AVIF_VERSION_0_11_0 then
    result:=specialize TEncoder<PAvifEncoder0_11_0>.Create(aEncoderPtr)
  else
    result:=specialize TEncoder<PAvifEncoder>.Create(aEncoderPtr);
end;

function TAvifRGBImageFactory(): TAvifRGBImageBase;
begin
  if AVIF_VERSION >= AVIF_VERSION_1_0_0 then
    result:=specialize TAvifRGBImage<PavifRGBImage1_0_0>.Create
  else if AVIF_VERSION >= AVIF_VERSION_0_11_0 then
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

function TDecoder.GetImageIndex:longint;
begin
  result := FDecoder^.imageIndex;
end;

function TDecoder.GetImageDurationSeconds: double;
begin
  result := FDecoder^.imageTiming.duration;
end;

function TDecoder.GetImageDurationTimescales: UInt64;
begin
  result := FDecoder^.imageTiming.durationInTimescales;
end;

function TDecoder.GetTimescale: UInt64;
begin
  result := FDecoder^.timescale;
end;

procedure TDecoder.SetCodecChoice(ACodec: avifCodecChoice);
begin
  FDecoder^.codecChoice := ACodec;
end;

function TDecoder.GetImageCount:longint;
begin
  result := FDecoder^.imageCount;
end;

function TDecoder.GetSequenceDuration:double;
begin
  result := FDecoder^.duration;
end;

constructor TAvifRGBImage.Create;
begin
  wrgb := Default(TAvifRGBImageField);
  FImage:= T(@wrgb);
end;

function TAvifRGBImage.GetRgbImage: PavifRGBImage;
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

procedure TAvifRGBImage.SetAlphaPremultiplied(aValue: avifBool);
begin
  FImage^.alphaPremultiplied:=aVAlue;
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

procedure TEncoder.SetTimeScale(aValue: UInt64);
begin
  FEncoder^.timescale:=aValue;
end;

function TEncoder.GetMaxThreads: integer;
begin
  result:=FEncoder^.maxThreads;
end;

function TEncoder.GetMinQuantizer: integer;
begin
  result:=FEncoder^.minQuantizer;
end;

function TEncoder.GetMaxQuantizer: integer;
begin
  result:=FEncoder^.maxQuantizer;
end;

function TEncoder.GetMinQuantizerAlpha: integer;
begin
  result:=FEncoder^.minQuantizerAlpha;
end;

function TEncoder.GetMaxQuantizerAlpha: integer;
begin
  result:=FEncoder^.maxQuantizerAlpha;
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

procedure AvifImageToBGRABitmap(aAvifImage:PAvifImage; aBitmap: TBGRACustomBitmap);
var
  res: avifResult;
  imageWrap: TAvifImageBase;
  rgbImageWrap: TAvifRgbImageBase;
begin
  imageWrap:=nil;
  rgbImageWrap:=nil;
  try
    rgbImageWrap:=TAvifRgbImageFactory();
    imageWrap:=TAvifImageFactory(aAvifImage);
    avifRGBImageSetDefaults(rgbImageWrap.GetRgbImage, aAvifImage);
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
    res := avifImageYUVToRGB(aAvifImage, rgbImageWrap.GetRgbImage);

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
end;

procedure AvifDecode(decoderWrap: TDecoderBase; aBitmap: TBGRACustomBitmap);
var
  res: avifResult;
  image:PAvifImage;
begin
  res := avifDecoderParse(decoderWrap.GetDecoder);
  if res <> AVIF_RESULT_OK then
    raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
  //  Memo1.Lines.Add(Format('Parsed AVIF: %ux%u (%ubpc)', [decoder^.image^.Width, decoder^.image^.Height, decoder^.image^.depth]));
  res := avifDecoderNextImage(decoderWrap.GetDecoder);
  if res = AVIF_RESULT_OK then
  begin
    image := decoderWrap.GetImage;
    if image = nil then
      raise EAvifException.Create('No image data recieved from AVIF library.');
    AvifImageToBGRABitmap(image,aBitmap);
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

function AvifSaveToFile(aBitmap: TBGRACustomBitmap; const AFilename: string;
  aQuality0to100: integer; aSpeed0to10: integer; aPixelFormat: avifPixelFormat;
  aIgnoreAlpha: boolean): NativeUInt;
var
  writer: TAvifWriter;
begin
  Result := 0;
  writer := TAvifWriter.Create( aQuality0to100, aSpeed0to10, aPixelFormat, aIgnoreAlpha);
  try
    writer.OnlyOneImage := True;
    writer.AddImage(aBitmap, 0);
    Result := writer.SaveToFile(AFileName);
  finally
    writer.Free;
  end;
end;

function AvifSaveToStream(aBitmap: TBGRACustomBitmap; AStream: TStream;
  aQuality0to100: integer; aSpeed0to10: integer; aPixelFormat: avifPixelFormat;
  aIgnoreAlpha: boolean): NativeUInt;
var
  writer: TAvifWriter;
begin
  Result := 0;
  writer := TAvifWriter.Create(aQuality0to100, aSpeed0to10, aPixelFormat, aIgnoreAlpha);
  try
    writer.OnlyOneImage := True;
    writer.AddImage(aBitmap, 0);
    Result := writer.SaveToStream(AStream);
  finally
    writer.Free;
  end;
end;
//returns the size of the resulting bitmap.
function AvifSaveToMemory(aBitmap: TBGRACustomBitmap; AData: Pointer; ASize: cardinal; aQuality0to100: integer;aSpeed0to10:integer;aPixelFormat:avifPixelFormat;aIgnoreAlpha:boolean): NativeUInt;
var
  writer:TAvifWriter;
begin
  result:=0;
  writer:=TAvifWriter.Create(aQuality0to100,aSpeed0to10,aPixelFormat,aIgnoreAlpha);
  try
    writer.OnlyOneImage := True;
    writer.AddImage(aBitmap,0);
    result:=writer.SaveToMemory(AData,ASize);
  finally
    writer.Free;
  end;
end;


function AvifSaveToStream(ABitmap: TBGRACustomBitmap; AStream: TStream;
  AIgnoreAlpha: boolean; AQuality0to100: integer; AQualityAlpha0to100: integer;
  APixelFormat: avifPixelFormat; ACodec: avifCodecChoice; ASpeed0to10: integer
  ): nativeuint;
var
  writer: TAvifWriter;
begin
  Result := 0;
  writer := TAvifWriter.Create( AQuality0to100, ASpeed0to10, APixelFormat, AIgnoreAlpha);
  try
    writer.SetEncoder(ACodec);
    writer.OnlyOneImage := True;
    writer.SetQuality(AQuality0to100);
    writer.SetQualityAlpha(AQualityAlpha0to100);
    writer.AddImage(aBitmap, 0);
    Result := writer.SaveToStream(AStream);
  finally
    writer.Free;
  end;
end;

function AvifSaveToFile(ABitmap: TBGRACustomBitmap; const AFilename: string;
  AIgnoreAlpha: boolean; AQuality0to100: integer; AQualityAlpha0to100: integer;
  APixelFormat: avifPixelFormat; ACodec: avifCodecChoice; ASpeed0to10: integer
  ): nativeuint;
var
  writer: TAvifWriter;
begin
  Result := 0;
  writer := TAvifWriter.Create( AQuality0to100, ASpeed0to10, APixelFormat, AIgnoreAlpha);
  try
    writer.SetEncoder(ACodec);
    writer.MaxThreads := 16;
    writer.OnlyOneImage := True;
    writer.SetQuality(AQuality0to100);
    writer.SetQualityAlpha(AQualityAlpha0to100);
    writer.AddImage(ABitmap, 0);
    Result := writer.SaveToFile(AFileName);
  finally
    writer.Free;
  end;
end;

function AvifSaveToMemory(ABitmap: TBGRACustomBitmap; AData: Pointer;
  ASize: nativeuint; AIgnoreAlpha: boolean; AQuality0to100: integer;
  AQualityAlpha0to100: integer; APixelFormat: avifPixelFormat;
  ACodec: avifCodecChoice; ASpeed0to10: integer): nativeuint;
var
  writer: TAvifWriter;
begin
  Result := 0;
  writer := TAvifWriter.Create( AQuality0to100, ASpeed0to10, APixelFormat, AIgnoreAlpha);
  try
    writer.SetEncoder(ACodec);
    writer.OnlyOneImage := True;
    writer.SetQuality(AQuality0to100);
    writer.SetQualityAlpha(AQualityAlpha0to100);
    writer.AddImage(ABitmap, 0);
    Result := writer.SaveToMemory(AData,ASize);
  finally
    writer.Free;
  end;
end;

{ TAvifReader }

constructor TAvifReader.Create(AFileName: string);
begin
  FStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  Init(FStream);
end;

constructor TAvifReader.Create(AStream: TStream);
begin
  Init(AStream);
end;

procedure TAvifReader.Init(AStream: TStream);
var
  res: avifResult;
  DecoderWrap:TDecoderBase;
begin
  FDecoder := avifDecoderCreate();
  if FDecoder = nil then
    Exit;
  DecoderWrap:=TDecoderFactory(FDecoder);
  FDecoderWrap:=DecoderWrap;
  res := avifDecoderSetIOStream(DecoderWrap, AStream);
  if res = AVIF_RESULT_OK then
    res := avifDecoderParse(DecoderWrap.GetDecoder);
  if res <> AVIF_RESULT_OK then
      raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
  FImageCount:=DecoderWrap.GetImageCount;
  FImageIndex:=DecoderWrap.GetImageIndex;
  FSequenceDuration := DecoderWrap.GetSequenceDuration;
  FTimescale := DecoderWrap.GetTimescale;
  FInitOk:=True;
end;

procedure TAvifReader.Close;
begin
  //WARNING: if the user unload libavif before clossing the reader then Access violation.
  if (FDecoder <> nil) and LibAvifLoaded then
  begin
    //avifDecoderReset(FDecoder);
    avifDecoderDestroy(FDecoder);
    FDecoder := nil;
  end;
  FreeAndNil(FDecoderWrap);
  FreeAndNil(FStream);
end;

destructor TAvifReader.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TAvifReader.GetNextImage(AOutBitmap: TBGRACustomBitmap): boolean;
var
  image: PAvifImage;
  res: avifResult;
begin
  Result := False;
  FImageDurationSeconds := -1;
  FImageDurationTimescales := 0;
  if (not InitOk) or (AOutBitmap = nil) or (FImageIndex >= (FImageCount - 1)) then
    Exit;
  res := avifDecoderNextImage(TDecoderBase(FDecoderWrap).GetDecoder);
  if res = AVIF_RESULT_OK then
  begin
    image := TDecoderBase(FDecoderWrap).GetImage;
    if image = nil then
      raise EAvifException.Create('No image data recieved from AVIF library.');
    AvifImageToBGRABitmap(image, aOutBitmap);
    FImageIndex := TDecoderBase(FDecoderWrap).GetImageIndex;
    FImageDurationSeconds := TDecoderBase(FDecoderWrap).GetImageDurationSeconds;
    FImageDurationTimescales := TDecoderBase(FDecoderWrap).GetImageDurationTimescales;
    if FImageIndex = 0 then
    begin
      FWidth := aOutBitmap.Width;
      FHeight := aOutBitmap.Height;
    end;
    Result := True;
  end
  else
    raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
end;

function TAvifReader.GetNthImage(AOutBitmap: TBGRACustomBitmap;
  AImageIndex: uint32): boolean;
var
  image: PAvifImage;
  res: avifResult;
begin
  Result := False;
  FImageDurationSeconds := -1;
  if (not InitOk) or (AOutBitmap = nil) or (aImageIndex >= FImageCount) then
    Exit;
  res := avifDecoderNthImage(TDecoderBase(FDecoderWrap).GetDecoder, aImageIndex);
  if res = AVIF_RESULT_OK then
  begin
    image := TDecoderBase(FDecoderWrap).GetImage;
    if image = nil then
      raise EAvifException.Create('No image data recieved from AVIF library.');
    AvifImageToBGRABitmap(image, aOutBitmap);
    FImageIndex := TDecoderBase(FDecoderWrap).GetImageIndex;
    FImageDurationSeconds := TDecoderBase(FDecoderWrap).GetImageDurationSeconds;
    if FImageIndex = 0 then
    begin
      FWidth := aOutBitmap.Width;
      FHeight := aOutBitmap.Height;
    end;
    Result := True;
  end
  else
    raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
end;

procedure TAvifReader.SetDecoder(ACodec: avifCodecChoice);
var
  lDecoderWrap:TDecoderBase;
begin
  lDecoderWrap := TDecoderBase(FDecoderWrap);
  lDecoderWrap.SetCodecChoice(ACodec);
end;

{ TAvifWriter }

constructor TAvifWriter.Create(AQuality0to100: integer; ASpeed0to10: integer;
  APixelFormat: avifPixelFormat; AIgnoreAlpha: boolean; ACodec: avifCodecChoice
  );
var
  alpha_quantizer, min_quantizer, max_quantizer: integer;
  quality: integer;
  lEncoderWrap: TEncoderBase;
const
  LOSS_LESS_IMAGE_QUALITY = 100;
begin
  FAvifOutput := AVIF_DATA_EMPTY;
  FEncoder := avifEncoderCreate();
  if FEncoder = nil then
    raise EAvifException.Create('Avif Error: creating encoder');
  FEncoderWrap := TEncoderFactory(FEncoder);
  lEncoderWrap := TEncoderBase(FEncoderWrap);
  FPixelFormat := APixelFormat;
  FIgnoreAlpha := AIgnoreAlpha;
  FTimescale := DEFAULT_TIMESCALE;
  lEncoderWrap.SetTimeScale(FTimescale);

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

  FQuality0to100 := clamp(AQuality0to100, 0, 100);
  if aSpeed0to10 <> AVIF_SPEED_DEFAULT then
    aSpeed0to10 := clamp(ASpeed0to10, AVIF_SPEED_SLOWEST, AVIF_SPEED_FASTEST);

  lEncoderWrap.SetMaxThreads(2);
  lEncoderWrap.SetSpeed(ASpeed0to10);
  if FQuality0to100 = LOSS_LESS_IMAGE_QUALITY then
  begin
    // Set defaults, and warn later on if anything looks incorrect
    //input.requestedFormat = AVIF_PIXEL_FORMAT_YUV444; // don't subsample when using AVIF_MATRIX_COEFFICIENTS_IDENTITY
    lEncoderWrap.SetMinQuantizer(AVIF_QUANTIZER_LOSSLESS);
    lEncoderWrap.SetMaxQuantizer(AVIF_QUANTIZER_LOSSLESS);
    lEncoderWrap.SetMinQuantizerAlpha(AVIF_QUANTIZER_LOSSLESS);
    lEncoderWrap.SetMaxQuantizerAlpha(AVIF_QUANTIZER_LOSSLESS);
    lEncoderWrap.SetCodecChoice(AVIF_CODEC_CHOICE_AOM);              // rav1e doesn't support lossless transform yet:
  end
  else
  begin
    // CONVERT 0..100  TO 63..0
    quality := Trunc(interpolate(AQuality0to100, 0, 100, AVIF_QUANTIZER_WORST_QUALITY, AVIF_QUANTIZER_BEST_QUALITY));
    max_quantizer := quality;
    min_quantizer := 0;
    alpha_quantizer := 0;
    if (max_quantizer > 20) then
    begin
      min_quantizer := max_quantizer - 20;
      if (max_quantizer > 40) then
        alpha_quantizer := max_quantizer - 40;
    end;
    lEncoderWrap.SetMinQuantizer(min_quantizer);
    lEncoderWrap.SetMaxQuantizer(max_quantizer);
    lEncoderWrap.SetMinQuantizerAlpha(0);
    lEncoderWrap.SetMaxQuantizerAlpha(alpha_quantizer);
    //lEncoderWrap.SetCodecChoice(aCodec);
  end;
  FInitOk := True;
end;

procedure TAvifWriter.Close;
begin
  EncoderFinish;
end;

procedure TAvifWriter.EncoderFinish;
var
  finishResult: avifResult;
begin
  if (FEncoder <> nil) and LibAvifLoaded then
  begin
    try
      if FInitOk and (FImagesCount > 0) then
      begin
        finishResult := avifEncoderFinish(FEncoder, @FAvifOutput);
        if finishResult <> AVIF_RESULT_OK then
          raise EAvifException.Create('Failed to finish encode: ' + avifResultToString(finishResult));
      end;
    finally
      avifEncoderDestroy(FEncoder);
      FreeAndNil(FEncoderWrap);
      FEncoder := nil;
    end;
  end;
end;

procedure TAvifWriter.SetMaxThreads(AMT: integer);
var
  lEncoderWrap:TEncoderBase;
begin
  lEncoderWrap := TEncoderBase(FEncoderWrap);
  lEncoderWrap.SetMaxThreads(AMT);
end;

procedure TAvifWriter.SetMinQuantizer(AMinQ: integer);
var
  lEncoderWrap:TEncoderBase;
begin
  lEncoderWrap := TEncoderBase(FEncoderWrap);
  lEncoderWrap.SetMinQuantizer(AMinQ);
end;

procedure TAvifWriter.SetMaxQuantizer(AMaxQ: integer);
var
  lEncoderWrap:TEncoderBase;
begin
  lEncoderWrap := TEncoderBase(FEncoderWrap);
  lEncoderWrap.SetMaxQuantizer(AMaxQ);
end;

procedure TAvifWriter.SetMinQuantizerAlpha(AMinQ: integer);
var
  lEncoderWrap:TEncoderBase;
begin
  lEncoderWrap := TEncoderBase(FEncoderWrap);
  lEncoderWrap.SetMinQuantizerAlpha(AMinQ);
end;

procedure TAvifWriter.SetMaxQuantizerAlpha(AMaxQ: integer);
var
  lEncoderWrap:TEncoderBase;
begin
  lEncoderWrap := TEncoderBase(FEncoderWrap);
  lEncoderWrap.SetMaxQuantizerAlpha(AMaxQ);
end;

procedure TAvifWriter.SetIgnoreAlpha(AValue: boolean);
begin
  FIgnoreAlpha := AValue;
end;

function TAvifWriter.GetMaxThreads: integer;
var
  lEncoderWrap:TEncoderBase;
begin
  lEncoderWrap := TEncoderBase(FEncoderWrap);
  result:=lEncoderWrap.GetMaxThreads;
end;

function TAvifWriter.GetMinQuantizer: integer;
var
  lEncoderWrap:TEncoderBase;
begin
  lEncoderWrap := TEncoderBase(FEncoderWrap);
  result:=lEncoderWrap.GetMinQuantizer;
end;

function TAvifWriter.GetMaxQuantizer: integer;
var
  lEncoderWrap:TEncoderBase;
begin
  lEncoderWrap := TEncoderBase(FEncoderWrap);
  result:=lEncoderWrap.GetMaxQuantizer;
end;

function TAvifWriter.GetMinQuantizerAlpha: integer;
var
  lEncoderWrap:TEncoderBase;
begin
  lEncoderWrap := TEncoderBase(FEncoderWrap);
  result:=lEncoderWrap.GetMinQuantizerAlpha;
end;

function TAvifWriter.GetMaxQuantizerAlpha: integer;
var
  lEncoderWrap:TEncoderBase;
begin
  lEncoderWrap := TEncoderBase(FEncoderWrap);
  result:=lEncoderWrap.GetMaxQuantizerAlpha;
end;

destructor TAvifWriter.Destroy;
begin
  if LibAvifLoaded then
  begin
    try
      EncoderFinish;
    finally
      avifRWDataFree(@FAvifOutput);
    end;
  end;
  inherited Destroy;
end;

procedure TAvifWriter.AddImage(ABitmap: TBGRACustomBitmap; ADurationMs: cardinal
  );
var
  image: PavifImage;
  imageWrap: TAvifImageBase;
  rgbImageWrap: TAvifRgbImageBase;
  convertResult, addImageResult: avifResult;
  durationTimescales: uint64;
  imageFlags: uint32;
const
  LOSS_LESS_IMAGE_QUALITY = 100;
begin
  if (not FInitOk) or (FEncoder = nil) then
    Exit;
  if (FImagesCount > 0) and FOnlyOneImage then
    raise EAvifException.Create('Only one image is allowed. ');
  rgbImageWrap := nil;
  imageWrap := nil;
  try
    if FQuality0to100 = LOSS_LESS_IMAGE_QUALITY then
      image := avifImageCreate(ABitmap.Width, ABitmap.Height, 8, AVIF_PIXEL_FORMAT_YUV444)
    else
      image := avifImageCreate(ABitmap.Width, ABitmap.Height, 8, FPixelFormat{AVIF_PIXEL_FORMAT_YUV420}{AVIF_PIXEL_FORMAT_YUV444});
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
    rgbImageWrap := TAvifRgbImageFactory();
    // If you have RGB(A) data you want to encode, use this path
    avifRGBImageSetDefaults(rgbImageWrap.GetRgbImage, image);
    rgbImageWrap.SetWidth(ABitmap.Width);
    rgbImageWrap.SetHeight(ABitmap.Height);
    {$push}{$warn 6018 off}//unreachable code
    if TBGRAPixel_RGBAOrder then
      rgbImageWrap.SetFormat(AVIF_RGB_FORMAT_RGBA)
    else
      rgbImageWrap.SetFormat(AVIF_RGB_FORMAT_BGRA);
    {$pop}
    if FIgnoreAlpha then
      rgbImageWrap.SetIgnoreAlpha(AVIF_TRUE)
    else
      rgbImageWrap.SetIgnoreAlpha(AVIF_FALSE);
    rgbImageWrap.SetPixels(ABitmap.DataByte);
    rgbImageWrap.SetRowBytes(ABitmap.Width * 4);
    if FQuality0to100 = LOSS_LESS_IMAGE_QUALITY then
    begin
      // https://github.com/xiph/rav1e/issues/151
      imageWrap.SetYuvRange(AVIF_RANGE_FULL); // avoid limited range
      imageWrap.SetMatrixCoefficients(AVIF_MATRIX_COEFFICIENTS_IDENTITY); // this is key for lossless
    end;
    if aBitmap.LineOrder <> riloTopToBottom then   //vertical mirror.
    begin
      imageWrap.SetTransformFlags(imageWrap.GetTransformFlags + uint32(AVIF_TRANSFORM_IMIR));
      imageWrap.SetImirMode(0);
    end;
    convertResult := avifImageRGBToYUV(image, rgbImageWrap.GetRgbImage);
    if convertResult <> AVIF_RESULT_OK then
      raise EAvifException.Create('Failed to convert to YUV(A): ' + avifResultToString(convertResult));

    if FTimesCale <> 0 then
      durationTimescales := Trunc(((ADurationMs / 1000) * FTimescale) + 0.5)
    else
      durationTimescales := 1;
    if durationTimesCales < 1 then
      durationTimescales := 1;

    imageFlags := uint32(AVIF_ADD_IMAGE_FLAG_NONE);
    if FOnlyOneImage then
      imageFlags := uint32(AVIF_ADD_IMAGE_FLAG_SINGLE);
    addImageResult := avifEncoderAddImage(FEncoder, image, durationTimescales, imageFlags);
    if addImageResult <> AVIF_RESULT_OK then
      raise EAvifException.Create('Failed to add image to encoder: ' + avifResultToString(addImageResult));
    Inc(FImagesCount);
  finally
    if image <> nil then
      avifImageDestroy(image);
    imageWrap.Free;
    rgbImageWrap.Free;
  end;
end;

function TAvifWriter.SaveToFile(AFileName: string): NativeUInt;
var
  lStream: TFileStream;
begin
  Result := 0;
  if not InitOk then
    Exit;
  lStream := TFileStream.Create(AFileName, fmCreate or fmShareExclusive);
  try
    Result := SaveToStream(lStream);
  finally
    lSTream.Free;
  end;
end;

function TAvifWriter.SaveToStream(AStream: TStream): NativeUInt;
var
  p: pbyte;
  remain, toWrite: longword;
const
  CopySize = 65535;
begin
  Result := 0;
  if not InitOk then
    Exit;
  EncoderFinish;
  Result := FAvifOutput.Size;
  if FAvifOutput.Data <> nil then
  begin
    //AStream.WriteBuffer(avifOutput.Data^, avifOutput.size)
    remain := FAvifOutput.size;
    p := FAvifOutput.Data;
    while remain > 0 do
    begin
      if remain > CopySize then
        toWrite := CopySize
      else
        toWrite := remain;
      aStream.WriteBuffer(p^, toWrite);
      Inc(p, toWrite);
      Dec(remain, toWrite);
    end;
  end;
end;

//returns the size of the resulting bitmap.
function TAvifWriter.SaveToMemory(AData: Pointer; ASize: NativeUInt
  ): NativeUInt;
begin
  Result := 0;
  if not InitOk then
    Exit;
  EncoderFinish;
  Result := FAvifOutput.size;
  if FAvifOutput.Data <> nil then
    Move(FAvifOutput.Data^, AData^, min(ASize, FAvifOutput.size));
end;

function TAvifWriter.GetOutputSize: NativeUInt;
begin
  EncoderFinish;
  Result := FAvifOutput.Size;
end;

procedure TAvifWriter.SetEncoder(ACodec: avifCodecChoice);
var
  lEncoderWrap:TEncoderBase;
begin
  lEncoderWrap := TEncoderBase(FEncoderWrap);
  lEncoderWrap.SetCodecChoice(ACodec);
end;

procedure TAvifWriter.SetTimeScale(ATimeScale: uint64);
begin
  FTimeScale := ATimeScale;
  TEncoderBase(FEncoderWrap).SetTimeScale(FTimescale);
end;

procedure TAvifWriter.SetQuality(AValue: integer);
var
  QV:integer;
begin
  if AVIF_VERSION >= AVIF_VERSION_1_0_0 then
    PavifEncoder1_0_0(FEncoder)^.quality:=clamp(AValue,AVIF_QUALITY_WORST,AVIF_QUALITY_BEST)
  else
  begin
    //0..100 -> 63..0
    QV:=Trunc(Interpolate(AValue,AVIF_QUALITY_WORST,AVIF_QUALITY_BEST,AVIF_QUANTIZER_WORST_QUALITY,AVIF_QUANTIZER_BEST_QUALITY));
    SetMinQuantizer(QV);
    SetMaxQuantizer(QV);
  end;
end;

procedure TAvifWriter.SetQualityAlpha(AValue: integer);
var
  QV:integer;
begin
  if AVIF_VERSION >= AVIF_VERSION_1_0_0 then
    PavifEncoder1_0_0(FEncoder)^.qualityAlpha:=clamp(AValue,AVIF_QUALITY_WORST,AVIF_QUALITY_BEST)
  else
  begin
    //0..100 -> 63..0
    QV:=Trunc(Interpolate(AValue,AVIF_QUALITY_WORST,AVIF_QUALITY_BEST,AVIF_QUANTIZER_WORST_QUALITY,AVIF_QUANTIZER_BEST_QUALITY));
    SetMinQuantizerAlpha(QV);
    SetMaxQuantizerAlpha(QV);
  end;
end;

end.
