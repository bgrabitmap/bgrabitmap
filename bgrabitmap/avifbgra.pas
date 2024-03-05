// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ @abstract(Easy to use classes and functions to read/write images in AVIF format.)

  It supports multi-image files.

  Note that it requires libavif library. }
unit avifbgra;

{ Author: Domingo Galmes <dgalmesp@gmail.com>  01-11-2021 }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, libavif;

type
  {* Exception when using libavif library }
  EAvifException = class(Exception);
  {* Pixel format to use when encoding the image }
  avifPixelFormat = libavif.avifPixelFormat;

const
  {** This format uses YUV color space with a 4:4:4 chroma subsampling.
    In this format, each component (Y for luminance, U, and V for chrominance) has the same
    sample rate, meaning there's no chroma subsampling. This results in high-quality images
    because it retains all the color information. }
  AVIF_PIXEL_FORMAT_YUV444 = libavif.AVIF_PIXEL_FORMAT_YUV444;
  {** This format also uses the YUV color space but with 4:2:2 chroma subsampling.
    Here, the horizontal resolution of the chroma channels is halved
    compared to the luminance channel, reducing the image size while still maintaining good quality.
    It strikes a balance between compression and image quality. }
  AVIF_PIXEL_FORMAT_YUV422 = libavif.AVIF_PIXEL_FORMAT_YUV422;
  {** Utilizing the YUV color space with 4:2:0 chroma subsampling, this format reduces both
    the horizontal and vertical resolution of the chroma channels by half relative
    to the luminance channel. This is a commonly used format for digital video compression,
    offering significant file size reduction at the cost of some image quality,
    especially in areas with high color detail. }
  AVIF_PIXEL_FORMAT_YUV420 = libavif.AVIF_PIXEL_FORMAT_YUV420;
  {** This is a monochrome format where only the Y (luminance) component is used, and there are
    no U and V (chrominance) components. Essentially, it's a grayscale image, which can
    significantly reduce the file size while being appropriate for images that
    don't require color. }
  AVIF_PIXEL_FORMAT_YUV400 = libavif.AVIF_PIXEL_FORMAT_YUV400;

type
  {* Codec choices for encoding and/or decoding AVIF }
  avifCodecChoice = libavif.avifCodecChoice;

const
  { AOM stands for Alliance for Open Media, which is the consortium
    that developed the AV1 codec. This choice indicates the use of AOM's reference implementation
    for both encoding and decoding AVIF images. }
  AVIF_CODEC_CHOICE_AOM = libavif.AVIF_CODEC_CHOICE_AOM;
  { This decoding-only codec is focused on decoding AV1 content. Developed by the VideoLAN, VLC, and FFmpeg
    communities, dav1d is known for its speed and efficiency in decoding AV1 streams. }
  AVIF_CODEC_CHOICE_DAV1D = libavif.AVIF_CODEC_CHOICE_DAV1D;
  { This decoding-only codec is developed by Google. It is designed for efficiency and is used in
     various Google products for decoding AV1 content. }
  AVIF_CODEC_CHOICE_LIBGAV1 = libavif.AVIF_CODEC_CHOICE_LIBGAV1;
  { This encoding-only codec is designed to offer efficient encoding of video content. }
  AVIF_CODEC_CHOICE_RAV1E = libavif.AVIF_CODEC_CHOICE_RAV1E;
  { This encoding-only codec focuses on offering high performance and scalability. SVT stands for
    Scalable Video Technology. }
  AVIF_CODEC_CHOICE_SVT = libavif.AVIF_CODEC_CHOICE_SVT;
  { Ongoing development in the next-generation video compression technology beyond AV1. }
  AVIF_CODEC_CHOICE_AVM = libavif.AVIF_CODEC_CHOICE_AVM;

const
  { Default number of allocated threads for processing }
  AVIF_BGRA_DEFAULT_MAX_THREADS = 2;

  { Default timescale of the media (in Hz), not relevant for images }
  AVIF_BGRA_DEFAULT_TIMESCALE = 30;

  { Default quality of color compression }
  AVIF_BGRA_DEFAULT_QUALITY = 30;
  { Default difference between color quality and alpha quality }
  AVIF_BGRA_DEFAULT_QUALITY_ALPHA_DELTA = 25;
  { Default quality of compression of the alpha channel }
  AVIF_BGRA_DEFAULT_QUALITY_ALPHA = AVIF_BGRA_DEFAULT_QUALITY + AVIF_BGRA_DEFAULT_QUALITY_ALPHA_DELTA;
  { Specify that no information will be lost in the compression process }
  AVIF_BGRA_LOSSLESS_QUALITY = 100;

  { Let the encoder choose the adequate speed }
  AVIF_BGRA_SPEED_DEFAULT = AVIF_SPEED_DEFAULT;

  { Common format for image compression with half the resolution for chroma channels
    (ignored when used with AVIF_BGRA_LOSSLESS_QUALITY) }
  AVIF_BGRA_PIXEL_FORMAT_DEFAULT = AVIF_PIXEL_FORMAT_YUV420;
  { Let the encoder choose the adequate codec }
  AVIF_BGRA_CODEC_CHOICE_AUTO = AVIF_CODEC_CHOICE_AUTO;

type
  { Reader for AVIF images or animations (not derived from TFPCustomImageReader) }
  TAvifReader = class
  private
    function GetImageCount: uint32;
    function GetImageIndex: integer;
    function GetRepetitionCount: integer;
    function GetSequenceDuration: double;
    function GetTimescale: uint64;
  protected
    FDecoder: PavifDecoder;
    FDecoderWrap: TObject; //TDecoderBase;
    FStream: TStream;
    FStreamOwned: boolean;
    FWidth: uint32;
    FHeight: uint32;
    FImageDurationSeconds: double;
    FImageDurationTimescales: UInt64;
    procedure Init(AStream: TStream; AStreamOwned: boolean);
    procedure SetDecoder(ACodec: avifCodecChoice);
    function GetDecoder: avifCodecChoice;
    procedure Close; // call before unloading libavif
  public
    constructor Create(AFileName: string); virtual; overload;
    constructor Create(AStream: TStream; AStreamOwned: boolean = false); virtual; overload;
    destructor Destroy; override;
    class function CreateDecoder: PavifDecoder;
    class procedure DestroyDecoder(var ADecoder: PavifDecoder);

    function GetNextImage(AOutBitmap: TBGRACustomBitmap): boolean;
    function GetNthImage(AOutBitmap: TBGRACustomBitmap; AImageIndex: uint32): boolean;

    property Decoder: avifCodecChoice read GetDecoder write SetDecoder;
    property ImageIndex: integer read GetImageIndex;
    property ImageCount: uint32 read GetImageCount;
    property ImageDurationSeconds: double read FImageDurationSeconds;
    property ImageDurationTimescales: uint64 read FImageDurationTimescales;
    property SequenceDuration: double read GetSequenceDuration;
    property RepetitionCount: integer read GetRepetitionCount;
    property Width: uint32 read FWidth;
    property Height: uint32 read FHeight;
    property Timescale: uint64 read GetTimescale; // if all images have 1 timescale of duration is the same as FPS
  end;

  { Writer for AVIF images or animations (not derived from TFPCustomImageWriter) }
  TAvifWriter = class
  protected
    FEncoder: PAvifEncoder;
    FEncoderWrap: TObject; //TEncoderBase;
    FQuality0to100: integer;
    FPixelFormat: avifPixelFormat;
    FQualityAlpha0to100: integer;
    FIgnoreAlpha: boolean;
    FAvifOutput: avifRWData;
    FOnlyOneImage: boolean;
    FImagesCount: uint32;
    FLossless: boolean;
    procedure EncoderFinish;
    procedure SetMaxThreads(AMT: integer);
    procedure SetIgnoreAlpha(AValue: boolean);
    function GetMaxThreads: integer;
    procedure SetEncoder(ACodec: avifCodecChoice);
    function GetEncoder: avifCodecChoice;
    function GetLossless: boolean;
    procedure SetLossless(AValue: boolean);
    procedure SetTimescale(ATimescale: uint64);
    function GetTimescale: uint64;
    procedure SetSpeed(ASpeed: integer);
    function GetSpeed: integer;
    procedure SetQuality(AValue: integer);
    procedure SetQualityAlpha(AValue: integer);
    procedure ApplyQuality;
  public
    constructor Create(
      AQuality0to100: integer = AVIF_BGRA_DEFAULT_QUALITY;
      ASpeed0to10: integer = AVIF_BGRA_SPEED_DEFAULT;
      APixelFormat: avifPixelFormat = AVIF_BGRA_PIXEL_FORMAT_DEFAULT;
      AIgnoreAlpha: boolean = False); virtual; overload;
    procedure Close;
    destructor Destroy; override;
    class function CreateEncoder: PAvifEncoder;
    class procedure DestroyEncoder(var AEncoder: PAvifEncoder);

    procedure AddImage(ABitmap: TBGRACustomBitmap; ADurationMs: cardinal=0);
    function SaveToFile(AFileName: string): NativeUInt;
    function SaveToStream(AStream: TStream): NativeUInt;
    function SaveToMemory(AData: Pointer; ASize: NativeUInt): NativeUInt;
    function GetOutputSize: NativeUInt;

    property Encoder: avifCodecChoice read GetEncoder write SetEncoder;
    property OnlyOneImage: boolean read FOnlyOneImage write FOnlyOneImage;
    property MaxThreads: integer read GetMaxThreads write SetMaxThreads;
    property PixelFormat: avifPixelFormat read FPixelFormat write FPixelFormat;
    property Quality: integer read FQuality0to100 write SetQuality;
    property QualityAlpha: integer read FQualityAlpha0to100 write SetQualityAlpha;
    property Speed: integer read GetSpeed write SetSpeed;
    property Timescale: uint64 read GetTimescale write SetTimescale; // frequency in Hertz
    property IgnoreAlpha: boolean read FIgnoreAlpha write SetIgnoreAlpha;
    property Lossless: boolean read GetLossless write SetLossless;
  end;

{ Load an AVIF image from the given stream }
procedure AvifLoadFromStream(AStream: TStream; aBitmap: TBGRACustomBitmap);
{ Load an AVIF image from the given file }
procedure AvifLoadFromFile(const AFilename: string; aBitmap: TBGRACustomBitmap);
{ Load an AVIF image from the given file without using the reader class }
procedure AvifLoadFromFileNative(const AFilename: string; aBitmap: TBGRACustomBitmap);
{ Load an AVIF image from memory, without using the reader class }
procedure AvifLoadFromMemory(AData: Pointer; ASize: cardinal; aBitmap: TBGRACustomBitmap);

{ Save an image into a stream using AVIF format. Return the number of bytes needed. }
function AvifSaveToStream(aBitmap: TBGRACustomBitmap; AStream: TStream;
  aQuality0to100: integer = AVIF_BGRA_DEFAULT_QUALITY;
  aSpeed0to10: integer = AVIF_BGRA_SPEED_DEFAULT;
  aPixelFormat:avifPixelFormat = AVIF_BGRA_PIXEL_FORMAT_DEFAULT;
  aIgnoreAlpha:boolean = false): NativeUInt;

{ Save an image into a file using AVIF format. Return the number of bytes needed. }
function AvifSaveToFile(aBitmap: TBGRACustomBitmap; const AFilename: string;
  aQuality0to100: integer = AVIF_BGRA_DEFAULT_QUALITY;
  aSpeed0to10: integer = AVIF_BGRA_SPEED_DEFAULT;
  aPixelFormat: avifPixelFormat = AVIF_BGRA_PIXEL_FORMAT_DEFAULT;
  aIgnoreAlpha: boolean = false): NativeUInt;

{ Save an image to memory using AVIF format. Return the number of bytes needed. }
function AvifSaveToMemory(aBitmap: TBGRACustomBitmap; AData: Pointer; ASize: cardinal;
  aQuality0to100: integer = AVIF_BGRA_DEFAULT_QUALITY;
  aSpeed0to10:integer = AVIF_BGRA_SPEED_DEFAULT;
  aPixelFormat:avifPixelFormat = AVIF_BGRA_PIXEL_FORMAT_DEFAULT;
  aIgnoreAlpha:boolean = false): NativeUInt;

{ Save an image into a stream using AVIF format. Return the number of bytes needed. }
function AvifSaveToStream(ABitmap: TBGRACustomBitmap; AStream: TStream;
  AIgnoreAlpha: boolean = False;
  AQuality0to100: integer = AVIF_BGRA_DEFAULT_QUALITY;
  AQualityAlpha0to100: integer = AVIF_BGRA_DEFAULT_QUALITY_ALPHA;
  APixelFormat: avifPixelFormat = AVIF_BGRA_PIXEL_FORMAT_DEFAULT;
  ACodec: avifCodecChoice = AVIF_BGRA_CODEC_CHOICE_AUTO;
  ASpeed0to10: integer = AVIF_BGRA_SPEED_DEFAULT): nativeuint; overload;

{ Save an image into a file using AVIF format. Return the number of bytes needed. }
function AvifSaveToFile(ABitmap: TBGRACustomBitmap; const AFilename: string;
  AIgnoreAlpha: boolean = False;
  AQuality0to100: integer = AVIF_BGRA_DEFAULT_QUALITY;
  AQualityAlpha0to100: integer = AVIF_BGRA_DEFAULT_QUALITY_ALPHA;
  APixelFormat: avifPixelFormat = AVIF_BGRA_PIXEL_FORMAT_DEFAULT;
  ACodec: avifCodecChoice = AVIF_BGRA_CODEC_CHOICE_AUTO;
  ASpeed0to10: integer = AVIF_BGRA_SPEED_DEFAULT): nativeuint; overload;

{ Save an image to memory using AVIF format. Return the number of bytes needed. }
function AvifSaveToMemory(ABitmap: TBGRACustomBitmap; AData: Pointer; ASize: nativeuint;
  AIgnoreAlpha: boolean = False;
  AQuality0to100: integer = AVIF_BGRA_DEFAULT_QUALITY;
  AQualityAlpha0to100: integer = AVIF_BGRA_DEFAULT_QUALITY_ALPHA;
  APixelFormat: avifPixelFormat = AVIF_BGRA_PIXEL_FORMAT_DEFAULT;
  ACodec: avifCodecChoice = AVIF_BGRA_CODEC_CHOICE_AUTO;
  ASpeed0to10: integer = AVIF_BGRA_SPEED_DEFAULT): nativeuint; overload;

{ Checks that the signature of the memory block correspond to a valid AVIF header }
function AvifValidateHeaderSignature(
  aBuffer: Pointer // at least 12 first bytes of file
  ): boolean;

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
    function GetRepetitionCount: integer; virtual; abstract;
    function GetTimescale: UInt64; virtual; abstract;
    procedure SetCodecChoice(ACodec:avifCodecChoice); virtual; abstract;
    function GetCodecChoice: avifCodecChoice; virtual; abstract;
  end;

  { TDecoder }

  generic TDecoder<T> = class(TDecoderBase)
  protected
    FDecoder: T;
  public
    constructor Create(aDecoderPtr: Pointer = nil); virtual;
    procedure Init(aDecoderPtr: Pointer); override;
    function GetImage: PavifImage; override;
    function GetIo:PavifIO; override;
    function GetDecoder:PavifDecoder; override;
    function GetSequenceDuration: double; override;
    function GetImageCount: longint; override;
    function GetImageIndex: longint; override;
    function GetImageDurationSeconds: double; override;
    function GetImageDurationTimescales: UInt64; override;
    function GetRepetitionCount: integer; override;
    function GetTimescale: UInt64; override;
    procedure SetCodecChoice(ACodec:avifCodecChoice); override;
    function GetCodecChoice: avifCodecChoice; override;
  end;

  { TDecoderWithRepetition }

  generic TDecoderWithRepetition<T> = class(specialize TDecoder<T>)
    constructor Create(aDecoderPtr: Pointer = nil); override;
    function GetRepetitionCount: integer; override;
  end;

  TEncoderBase = class
  public
    procedure Init(aEncoderPtr: Pointer); virtual; abstract;
    procedure SetMaxThreads(AMT: integer); virtual; abstract;
    procedure SetSpeed(ASpeed: integer); virtual; abstract;
    function GetSpeed: integer; virtual; abstract;
    procedure SetQuality(AQuality: integer); virtual; abstract;
    function GetQuality: integer; virtual; abstract;
    procedure SetQualityAlpha(AQuality: integer); virtual; abstract;
    function GetQualityAlpha: integer; virtual; abstract;
    function HasQuality: boolean; virtual; abstract;
    procedure SetMinQuantizer(AMinQ: integer); virtual; abstract;
    procedure SetMaxQuantizer(AMQ: integer); virtual; abstract;
    procedure SetMinQuantizerAlpha(AMinQ: integer); virtual; abstract;
    procedure SetMaxQuantizerAlpha(AMQ: integer); virtual; abstract;
    procedure SetCodecChoice(AMQ: avifCodecChoice); virtual; abstract;
    function GetCodecChoice: avifCodecChoice; virtual; abstract;
    procedure SetTimescale(aValue:UInt64); virtual; abstract;
    function GetTimescale:UInt64; virtual; abstract;
    function GetMaxThreads: integer; virtual; abstract;
    function GetMinQuantizer: integer; virtual; abstract;
    function GetMaxQuantizer: integer; virtual; abstract;
    function GetMinQuantizerAlpha: integer; virtual; abstract;
    function GetMaxQuantizerAlpha: integer; virtual; abstract;
  end;

  { TEncoder }

  generic TEncoder<T> = class(TEncoderBase)
  protected
    FEncoder: T;
  public
    constructor Create(aEncoderPtr: Pointer = nil); virtual;
    procedure Init(aEncoderPtr: Pointer); override;
    procedure SetMaxThreads(AMT: integer); override;
    procedure SetSpeed(ASpeed: integer); override;
    function GetSpeed: integer; override;
    procedure SetMinQuantizer(AMinQ: integer); override;
    procedure SetMaxQuantizer(AMQ: integer); override;
    procedure SetMinQuantizerAlpha(AMinQ: integer); override;
    procedure SetMaxQuantizerAlpha(AMQ: integer); override;
    procedure SetCodecChoice(ACC: avifCodecChoice); override;
    function GetCodecChoice: avifCodecChoice; override;
    procedure SetTimescale(aValue:UInt64); override;
    function GetTimescale:UInt64; override;
    function GetMaxThreads: integer; override;
    procedure SetQuality(AQuality: integer); override;
    function GetQuality: integer;override;
    procedure SetQualityAlpha(AQuality: integer); override;
    function GetQualityAlpha: integer; override;
    function HasQuality: boolean; override;
    function GetMinQuantizer: integer; override;
    function GetMaxQuantizer: integer; override;
    function GetMinQuantizerAlpha: integer; override;
    function GetMaxQuantizerAlpha: integer; override;
  end;

  { TEncoderWithQuality }

  generic TEncoderWithQuality<T> = class(specialize TEncoder<T>)
    constructor Create(aEncoderPtr: Pointer = nil); override;
    procedure SetQuality(AQuality: integer); override;
    function GetQuality: integer;override;
    procedure SetQualityAlpha(AQuality: integer); override;
    function GetQualityAlpha: integer; override;
    function HasQuality: boolean; override;
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
     result := specialize TDecoderWithRepetition<PAvifDecoder1_0_0>.Create(aDecoderPtr)
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
    result:=specialize TEncoderWithQuality<PAvifEncoder1_0_0>.Create(aEncoderPtr)
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

{ TDecoderWithRepetition }

constructor TDecoderWithRepetition.Create(aDecoderPtr: Pointer);
begin
  inherited Create(aDecoderPtr);
end;

function TDecoderWithRepetition.GetRepetitionCount: integer;
begin
  result := FDecoder^.repetitionCount;
end;

{ TEncoderWithQuality }

constructor TEncoderWithQuality.Create(aEncoderPtr: Pointer);
begin
  inherited Create(aEncoderPtr);
end;

procedure TEncoderWithQuality.SetQuality(AQuality: integer);
begin
  FEncoder^.quality := AQuality;
end;

function TEncoderWithQuality.GetQuality: integer;
begin
  result := FEncoder^.quality;
end;

procedure TEncoderWithQuality.SetQualityAlpha(AQuality: integer);
begin
  FEncoder^.qualityAlpha := AQuality;
end;

function TEncoderWithQuality.GetQualityAlpha: integer;
begin
  result := FEncoder^.qualityAlpha;
end;

function TEncoderWithQuality.HasQuality: boolean;
begin
  Result:= true;
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

function TDecoder.GetRepetitionCount: integer;
begin
  result := 0;
end;

function TDecoder.GetTimescale: UInt64;
begin
  result := FDecoder^.timescale;
end;

procedure TDecoder.SetCodecChoice(ACodec: avifCodecChoice);
begin
  FDecoder^.codecChoice := ACodec;
end;

function TDecoder.GetCodecChoice: avifCodecChoice;
begin
  result := FDecoder^.codecChoice;
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

function TEncoder.GetSpeed: integer;
begin
  result := FEncoder^.speed;
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

function TEncoder.GetCodecChoice: avifCodecChoice;
begin
  result := FEncoder^.codecChoice;
end;

procedure TEncoder.SetTimescale(aValue: UInt64);
begin
  FEncoder^.timescale:=aValue;
end;

function TEncoder.GetTimescale: UInt64;
begin
  result := FEncoder^.timescale;
end;

function TEncoder.GetMaxThreads: integer;
begin
  result:= FEncoder^.maxThreads;
end;

procedure TEncoder.SetQuality(AQuality: integer);
begin
  raise EAvifException.Create('Quality not available in this version of libavif');
end;

function TEncoder.GetQuality: integer;
begin
  result := 0;
  raise EAvifException.Create('Quality not available in this version of libavif');
end;

procedure TEncoder.SetQualityAlpha(AQuality: integer);
begin
  raise EAvifException.Create('Quality not available in this version of libavif');
end;

function TEncoder.GetQualityAlpha: integer;
begin
  result := 0;
  raise EAvifException.Create('Quality not available in this version of libavif');
end;

function TEncoder.HasQuality: boolean;
begin
  result := false;
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
  sourceLineOrder: TRawImageLineOrder;
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
    if ( (imageWrap.GetTransformFlags and longword(AVIF_TRANSFORM_IMIR)) <> 0) and
       (imageWrap.GetImirMode = 0) then
      sourceLineOrder := riloBottomToTop
    else sourceLineOrder := riloTopToBottom;
    if aBitmap.LineOrder <> sourceLineOrder then
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
  decoderWrap:= nil;
  decoder := TAvifReader.CreateDecoder;
  try
    decoderWrap:=TDecoderFactory(decoder);
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
    decoderWrap.Free;
    TAvifReader.DestroyDecoder(decoder);
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
  decoder := TAvifReader.CreateDecoder;
  try
    decoderWrap:=TDecoderFactory(decoder);
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
    decoderWrap.Free;
    TAvifReader.DestroyDecoder(decoder);
  end;
end;

procedure AvifLoadFromMemory(AData: Pointer; ASize: cardinal; aBitmap: TBGRACustomBitmap);
var
  decoder: PavifDecoder;
  res: avifResult;
  decoderWrap: TDecoderBase;
begin
  decoderWrap := nil;
  decoder := TAvifReader.CreateDecoder;
  try
    decoderWrap:= TDecoderFactory(decoder);
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
    decoderWrap.Free;
    TAvifReader.DestroyDecoder(decoder);
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
  writer:=TAvifWriter.Create(aQuality0to100, aSpeed0to10, aPixelFormat, aIgnoreAlpha);
  try
    writer.OnlyOneImage := True;
    writer.AddImage(aBitmap,0);
    result:=writer.SaveToMemory(AData, ASize);
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
    writer.Encoder := ACodec;
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
    writer.Encoder := ACodec;
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
    writer.Encoder := ACodec;
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
  Init(TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite), true);
end;

constructor TAvifReader.Create(AStream: TStream; AStreamOwned: boolean);
begin
  Init(AStream, AStreamOwned);
end;

function TAvifReader.GetImageCount: uint32;
begin
  result := TDecoderBase(FDecoderWrap).GetImageCount;
end;

function TAvifReader.GetImageIndex: integer;
begin
  result := TDecoderBase(FDecoderWrap).GetImageIndex;
end;

function TAvifReader.GetRepetitionCount: integer;
begin
  result := TDecoderBase(FDecoderWrap).GetRepetitionCount;
end;

function TAvifReader.GetSequenceDuration: double;
begin
  result := TDecoderBase(FDecoderWrap).GetSequenceDuration;
end;

function TAvifReader.GetTimescale: uint64;
begin
  result := TDecoderBase(FDecoderWrap).GetTimescale;
end;

procedure TAvifReader.Init(AStream: TStream; AStreamOwned: boolean);
var
  res: avifResult;
  lDecoderWrap : TDecoderBase;
begin
  FStream := AStream;
  FStreamOwned:= AStreamOwned;
  FDecoder := CreateDecoder;
  lDecoderWrap := TDecoderFactory(FDecoder);
  FDecoderWrap := lDecoderWrap;
  res := avifDecoderSetIOStream(lDecoderWrap, AStream);
  if res = AVIF_RESULT_OK then
    res := avifDecoderParse(lDecoderWrap.GetDecoder);
  if res <> AVIF_RESULT_OK then
  begin
    Close;
    raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
  end;
end;

procedure TAvifReader.Close;
begin
  DestroyDecoder(FDecoder);
  FreeAndNil(FDecoderWrap);
  if FStreamOwned then
    FreeAndNil(FStream)
  else
    FStream := nil;
end;

destructor TAvifReader.Destroy;
begin
  Close;
  inherited Destroy;
end;

class function TAvifReader.CreateDecoder: PavifDecoder;
begin
  result := nil;
  if not LibAvifLoad then
    raise EAvifException.Create('Cannot load libavif');
  try
    result := avifDecoderCreate();
  finally
    if not Assigned(result) then
      LibAvifUnload;
  end;
  if not Assigned(result) then
    raise EOutOfMemory.Create('Memory allocation failure');
end;

class procedure TAvifReader.DestroyDecoder(var ADecoder: PavifDecoder);
begin
  if not Assigned(ADecoder) then exit;
  avifDecoderDestroy(ADecoder);
  ADecoder := nil;
  LibAvifUnload;
end;

function TAvifReader.GetNextImage(AOutBitmap: TBGRACustomBitmap): boolean;
var
  image: PAvifImage;
  res: avifResult;
begin
  Result := False;
  FImageDurationSeconds := 0;
  FImageDurationTimescales := 0;
  if (AOutBitmap = nil) or (ImageIndex >= (ImageCount - 1)) then
    Exit;
  res := avifDecoderNextImage(TDecoderBase(FDecoderWrap).GetDecoder);
  if res = AVIF_RESULT_OK then
  begin
    image := TDecoderBase(FDecoderWrap).GetImage;
    if image = nil then
      raise EAvifException.Create('No image data recieved from AVIF library.');
    AvifImageToBGRABitmap(image, aOutBitmap);
    FImageDurationSeconds := TDecoderBase(FDecoderWrap).GetImageDurationSeconds;
    FImageDurationTimescales := TDecoderBase(FDecoderWrap).GetImageDurationTimescales;
    if ImageIndex = 0 then
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
  FImageDurationSeconds := 0;
  FImageDurationTimescales := 0;
  if (AOutBitmap = nil) or (aImageIndex >= ImageCount) then
    Exit;
  res := avifDecoderNthImage(TDecoderBase(FDecoderWrap).GetDecoder, aImageIndex);
  if res = AVIF_RESULT_OK then
  begin
    image := TDecoderBase(FDecoderWrap).GetImage;
    if image = nil then
      raise EAvifException.Create('No image data recieved from AVIF library.');
    AvifImageToBGRABitmap(image, aOutBitmap);
    FImageDurationSeconds := TDecoderBase(FDecoderWrap).GetImageDurationSeconds;
    FImageDurationTimescales := TDecoderBase(FDecoderWrap).GetImageDurationTimescales;
    if ImageIndex = 0 then
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
begin
  if Assigned(FDecoderWrap) then
    TDecoderBase(FDecoderWrap).SetCodecChoice(ACodec);
end;

function TAvifReader.GetDecoder: avifCodecChoice;
begin
  if Assigned(FDecoderWrap) then
    result := TDecoderBase(FDecoderWrap).GetCodecChoice
  else
    result := AVIF_CODEC_CHOICE_AUTO;
end;

{ TAvifWriter }

constructor TAvifWriter.Create(AQuality0to100: integer; ASpeed0to10: integer;
  APixelFormat: avifPixelFormat; AIgnoreAlpha: boolean);
var
  alpha_quantizer, min_quantizer, max_quantizer: integer;
  lEncoderWrap: TEncoderBase;
const
  AVIF_BGRA_LOSSLESS_QUALITY = 100;
begin
  FAvifOutput := AVIF_DATA_EMPTY;
  FEncoder := CreateEncoder;
  if FEncoder = nil then
    raise EAvifException.Create('Avif Error: creating encoder');
  FEncoderWrap := TEncoderFactory(FEncoder);
  lEncoderWrap := TEncoderBase(FEncoderWrap);
  FPixelFormat := APixelFormat;
  IgnoreAlpha := AIgnoreAlpha;

  // specifying max quality is a shorthand for lossless quality
  if AQuality0to100 = AVIF_BGRA_LOSSLESS_QUALITY then
    Lossless := true
  else
  begin
    Quality := AQuality0to100;
    QualityAlpha := AQuality0to100 + AVIF_BGRA_DEFAULT_QUALITY_ALPHA_DELTA;
  end;

  Timescale := AVIF_BGRA_DEFAULT_TIMESCALE;
  MaxThreads := AVIF_BGRA_DEFAULT_MAX_THREADS;
  Speed := ASpeed0to10;
  Encoder := AVIF_BGRA_CODEC_CHOICE_AUTO;

  // * tileRowsLog2
  // * tileColsLog2
  // * keyframeInterval
end;

procedure TAvifWriter.Close;
begin
  EncoderFinish;
end;

function TAvifWriter.GetLossless: boolean;
begin
  result := FLossless;
end;

procedure TAvifWriter.SetLossless(AValue: boolean);
begin
  FLossless := AValue;
  if AValue then
  begin
    // lossless quality has an effect on quality
    // but also indirectly on pixel format and encoder
    Quality := AVIF_BGRA_LOSSLESS_QUALITY;
    QualityAlpha := AVIF_BGRA_LOSSLESS_QUALITY;
  end;
end;

procedure TAvifWriter.EncoderFinish;
var
  finishResult: avifResult;
begin
  if Assigned(FEncoder) then
  begin
    try
      if FImagesCount > 0 then
      begin
        finishResult := avifEncoderFinish(FEncoder, @FAvifOutput);
        if finishResult <> AVIF_RESULT_OK then
          raise EAvifException.Create('Failed to finish encode: ' + avifResultToString(finishResult));
      end;
    finally
      DestroyEncoder(FEncoder);
      FreeAndNil(FEncoderWrap);
    end;
  end;
end;

procedure TAvifWriter.SetMaxThreads(AMT: integer);
begin
  if Assigned(FEncoderWrap) then
    TEncoderBase(FEncoderWrap).SetMaxThreads(AMT);
end;


procedure TAvifWriter.SetIgnoreAlpha(AValue: boolean);
begin
  FIgnoreAlpha := AValue;
end;

function TAvifWriter.GetMaxThreads: integer;
var
  lEncoderWrap:TEncoderBase;
begin
  if Assigned(FEncoderWrap) then
    result:= TEncoderBase(FEncoderWrap).GetMaxThreads
  else
    result := AVIF_BGRA_DEFAULT_MAX_THREADS;
end;

destructor TAvifWriter.Destroy;
begin
  try
    EncoderFinish;
  finally
    if LibAvifLoaded then
      avifRWDataFree(@FAvifOutput);
  end;
  inherited Destroy;
end;

class function TAvifWriter.CreateEncoder: PAvifEncoder;
begin
  result := nil;
  if not LibAvifLoad then
    raise EAvifException.Create('Cannot load libavif');
  try
    result := avifEncoderCreate();
  finally
    if not Assigned(result) then
      LibAvifUnload;
  end;
  if not Assigned(result) then
    raise EOutOfMemory.Create('Memory allocation failure');
end;

class procedure TAvifWriter.DestroyEncoder(var AEncoder: PAvifEncoder);
begin
  if not Assigned(AEncoder) then exit;
  avifEncoderDestroy(AEncoder);
  AEncoder := nil;
  LibAvifUnload;
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
begin
  if (FImagesCount > 0) and FOnlyOneImage then
    raise EAvifException.Create('Only one image is allowed. ');
  rgbImageWrap := nil;
  imageWrap := nil;
  try
    ApplyQuality;
    if Lossless then
      image := avifImageCreate(ABitmap.Width, ABitmap.Height, 8, AVIF_PIXEL_FORMAT_YUV444)
    else
      image := avifImageCreate(ABitmap.Width, ABitmap.Height, 8, FPixelFormat{AVIF_PIXEL_FORMAT_YUV420});
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
    if Lossless then
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

    if Timescale <> 0 then
      durationTimescales := Trunc(((ADurationMs / 1000) * Timescale) + 0.5)
    else
      durationTimescales := 1;
    if durationTimescales < 1 then
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
  Result := GetOutputSize;
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
  Result := GetOutputSize;
  if FAvifOutput.Data <> nil then
    Move(FAvifOutput.Data^, AData^, min(ASize, FAvifOutput.size));
end;

function TAvifWriter.GetOutputSize: NativeUInt;
begin
  EncoderFinish;
  Result := FAvifOutput.Size;
end;

procedure TAvifWriter.SetQuality(AValue: integer);
begin
  FQuality0to100 := clamp(AValue, AVIF_QUALITY_WORST, AVIF_QUALITY_BEST);
end;

procedure TAvifWriter.SetQualityAlpha(AValue: integer);
begin
  FQualityAlpha0to100 := clamp(AValue, AVIF_QUALITY_WORST, AVIF_QUALITY_BEST);
end;

procedure TAvifWriter.SetEncoder(ACodec: avifCodecChoice);
begin
  TEncoderBase(FEncoderWrap).SetCodecChoice(ACodec);
end;

function TAvifWriter.GetEncoder: avifCodecChoice;
begin
  result := TEncoderBase(FEncoderWrap).GetCodecChoice;
end;

procedure TAvifWriter.SetTimescale(ATimescale: uint64);
begin
  if Assigned(FEncoderWrap) then
    TEncoderBase(FEncoderWrap).SetTimescale(ATimescale);
end;

function TAvifWriter.GetTimescale: uint64;
begin
  if Assigned(FEncoderWrap) then
    result := TEncoderBase(FEncoderWrap).GetTimescale
  else
    result := AVIF_BGRA_DEFAULT_TIMESCALE;
end;

procedure TAvifWriter.SetSpeed(ASpeed: integer);
begin
  if ASpeed <> AVIF_SPEED_DEFAULT then
    ASpeed := clamp(ASpeed, AVIF_SPEED_SLOWEST, AVIF_SPEED_FASTEST);

  if Assigned(FEncoderWrap) then
    TEncoderBase(FEncoderWrap).SetSpeed(ASpeed);
end;

function TAvifWriter.GetSpeed: integer;
begin
  if Assigned(FEncoderWrap) then
    result := TEncoderBase(FEncoderWrap).GetSpeed
  else
    result := AVIF_SPEED_DEFAULT;
end;

procedure TAvifWriter.ApplyQuality;

  procedure QualityToQuantizerMinMax(AQuality: integer; out AMinQuantizer, AMaxQuantizer: integer);
  begin
    AMaxQuantizer :=
      Trunc(Interpolate(AQuality,
                        AVIF_QUALITY_WORST, AVIF_QUALITY_BEST,
                        AVIF_QUANTIZER_WORST_QUALITY, AVIF_QUANTIZER_BEST_QUALITY));
    if AMaxQuantizer > 20 then
      AMinQuantizer := AMaxQuantizer - 20
    else
      AMinQuantizer := 0;
  end;

var minQ, maxQ: integer;
  lEncoder : TEncoderBase;

begin
  lEncoder := TEncoderBase(FEncoderWrap);

  if Lossless and (Encoder = AVIF_CODEC_CHOICE_AUTO) then
    Encoder := AVIF_CODEC_CHOICE_AOM;

  if lEncoder.HasQuality then
  begin
    lEncoder.SetQuality(Quality);
    lEncoder.SetQualityAlpha(QualityAlpha);
  end
  else
  begin
    QualityToQuantizerMinMax(Quality, minQ, maxQ);
    lEncoder.SetMinQuantizer(minQ);
    lEncoder.SetMaxQuantizer(maxQ);

    QualityToQuantizerMinMax(QualityAlpha, minQ, maxQ);
    lEncoder.SetMinQuantizerAlpha(minQ);
    lEncoder.SetMaxQuantizerAlpha(maxQ);
  end;
end;

end.
