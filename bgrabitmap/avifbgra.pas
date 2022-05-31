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


function avifDecoderSetIOStream(decoder: PavifDecoder; aStream: TStream): avifResult; cdecl;
var
  io: PavifIO;
  setIo: PavifIO;
begin
  io := avifIOCreateStreamReader(aStream);
  if io = nil then
    exit(AVIF_RESULT_IO_ERROR);
  avifDecoderSetIO(decoder, io);

  if AVIF_VERSION >= AVIF_VERSION_0_10_0 then
     setIo := avifDecoder0_10_0(decoder^).io
  else if AVIF_VERSION >= AVIF_VERSION_0_9_3 then
     setIo := avifDecoder0_9_3(decoder^).io
  else if AVIF_VERSION >= AVIF_VERSION_0_9_2 then
     setIo := avifDecoder0_9_2(decoder^).io
  else
     setIo := avifDecoder0_8_4(decoder^).io;

  if setIo = nil then
    raise EAvifException.Create('Failed to set input. Could be due to incompatible version of AVIF library.');
  exit(AVIF_RESULT_OK);
end;

procedure AvifDecode(decoder: PavifDecoder; aBitmap: TBGRACustomBitmap);
var
  res: avifResult;
  wrgb0_8: avifRGBImage0_8_4;
  wrgb0_10: avifRGBImage0_10_0;
  prgb: PavifRGBImage;

function decoderImage: PavifImage;
begin
  if AVIF_VERSION >= AVIF_VERSION_0_10_0 then
     result := avifDecoder0_10_0(decoder^).image
  else if AVIF_VERSION >= AVIF_VERSION_0_9_3 then
     result := avifDecoder0_9_3(decoder^).image
  else if AVIF_VERSION >= AVIF_VERSION_0_9_2 then
     result := avifDecoder0_9_2(decoder^).image
  else
     result := avifDecoder0_8_4(decoder^).image;
end;

begin
  res := avifDecoderParse(decoder);
  if res <> AVIF_RESULT_OK then
    raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
  //  Memo1.Lines.Add(Format('Parsed AVIF: %ux%u (%ubpc)', [decoder^.image^.Width, decoder^.image^.Height, decoder^.image^.depth]));
  if avifDecoderNextImage(decoder) = AVIF_RESULT_OK then
  begin
    if decoderImage = nil then
      raise EAvifException.Create('No image data recieved from AVIF library.');
    if AVIF_VERSION >= AVIF_VERSION_0_10_0 then
    begin
      wrgb0_10:=Default(avifRGBImage0_10_0);
      prgb:= @wrgb0_10;
      avifRGBImageSetDefaults(prgb, decoderImage);
      //aBitmap.LineOrder:=riloTopToBottom;
      aBitmap.SetSize(wrgb0_10.Width, wrgb0_10.Height);
      wrgb0_10.pixels := PUint8(aBitmap.databyte);
      wrgb0_10.depth := 8;
      {$push}{$warn 6018 off} //unreachable code
      if TBGRAPixel_RGBAOrder then
        wrgb0_10.format := AVIF_RGB_FORMAT_RGBA
      else
        wrgb0_10.format := AVIF_RGB_FORMAT_BGRA;
      {$pop}
      wrgb0_10.rowBytes := wrgb0_10.Width * 4;
    end else
    begin
      wrgb0_8:=Default(avifRGBImage0_8_4);
      prgb:= @wrgb0_8;
      avifRGBImageSetDefaults(prgb, decoderImage);
      //aBitmap.LineOrder:=riloTopToBottom;
      aBitmap.SetSize(wrgb0_8.Width, wrgb0_8.Height);
      wrgb0_8.pixels := PUint8(aBitmap.databyte);
      wrgb0_8.depth := 8;
      {$push}{$warn 6018 off} //unreachable code
      if TBGRAPixel_RGBAOrder then
        wrgb0_8.format := AVIF_RGB_FORMAT_RGBA
      else
        wrgb0_8.format := AVIF_RGB_FORMAT_BGRA;
      {$pop}
      wrgb0_8.rowBytes := wrgb0_8.Width * 4;
    end;
    //if aBitmap.LineOrder<>riloTopToBottom then
    //begin
    //  decoder^.image^.transformFlags:=decoder^.image^.transformFlags + Uint32(AVIF_TRANSFORM_IMIR);
    //  decoder^.image^.imir.mode:=0;
    //end;
    //decoder^.image^.imir.axis:=0; //vertical mirror
    res := avifImageYUVToRGB(decoderImage, prgb);
    if res <> AVIF_RESULT_OK then
      raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
    if (aBitmap.LineOrder <> riloTopToBottom) and not
      (((longword(decoderImage^.transformFlags) and longword(AVIF_TRANSFORM_IMIR))) = longword(AVIF_TRANSFORM_IMIR)) and
      (decoderImage^.imir.mode = 0) then
      aBitmap.VerticalFlip;
    aBitmap.InvalidateBitmap;
  end
  else
    raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
end;

procedure AvifLoadFromStream(AStream: TStream; aBitmap: TBGRACustomBitmap);
var
  decoder: PavifDecoder;
  res: avifResult;
begin
  decoder := avifDecoderCreate();
  try
    // Override decoder defaults here (codecChoice, requestedSource, ignoreExif, ignoreXMP, etc)
    //decoder^.maxThreads := 1;
    // decoder^.codecChoice := AVIF_CODEC_CHOICE_AUTO;
    // decoder^.imageSizeLimit := AVIF_DEFAULT_IMAGE_SIZE_LIMIT;
    // decoder^.strictFlags := UInt32( AVIF_STRICT_ENABLED);
    // decoder^.allowProgressive := AVIF_FALSE;
    res := avifDecoderSetIOStream(decoder, aStream);
    if res = AVIF_RESULT_OK then
      AvifDecode(decoder, aBitmap)
    else
      raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
  finally
    avifDecoderDestroy(decoder);
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
begin
  decoder := avifDecoderCreate();
  try
    // Override decoder defaults here (codecChoice, requestedSource, ignoreExif, ignoreXMP, etc)
    //decoder^.maxThreads := 1;
    // decoder^.codecChoice := AVIF_CODEC_CHOICE_AUTO;
    // decoder^.imageSizeLimit := AVIF_DEFAULT_IMAGE_SIZE_LIMIT;
    // decoder^.strictFlags := UInt32( AVIF_STRICT_ENABLED);
    // decoder^.allowProgressive := AVIF_FALSE;
    res := avifDecoderSetIOFile(decoder, pansichar(AFilename));
    if res = AVIF_RESULT_OK then
      AvifDecode(decoder, aBitmap)
    else
      raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
  finally
    avifDecoderDestroy(decoder);
  end;
end;

procedure AvifLoadFromMemory(AData: Pointer; ASize: cardinal; aBitmap: TBGRACustomBitmap);
var
  decoder: PavifDecoder;
  res: avifResult;
begin
  decoder := avifDecoderCreate();
  try
    // Override decoder defaults here (codecChoice, requestedSource, ignoreExif, ignoreXMP, etc)
    //decoder^.maxThreads := 1;
    // decoder^.codecChoice := AVIF_CODEC_CHOICE_AUTO;
    // decoder^.imageSizeLimit := AVIF_DEFAULT_IMAGE_SIZE_LIMIT;
    // decoder^.strictFlags := UInt32( AVIF_STRICT_ENABLED);
    // decoder^.allowProgressive := AVIF_FALSE;
    res := avifDecoderSetIOMemory(decoder, AData, ASize);
    if res = AVIF_RESULT_OK then
      AvifDecode(decoder, aBitmap)
    else
      raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
  finally
    avifDecoderDestroy(decoder);
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
  wrgb0_8: avifRGBImage0_8_4;
  wrgb0_10: avifRGBImage0_10_0;
  prgb: PavifRGBImage;
  image: PavifImage;
  convertResult, addImageResult, finishResult: avifResult;
  alpha_quantizer, min_quantizer, max_quantizer: integer;
  quality: integer;
const
  LOSS_LESS_IMAGE_QUALITY = 100;
begin
  encoder := nil;
  //FillChar(wrgb, sizeof(wrgb), 0);
  try
    if aQuality0to100 = LOSS_LESS_IMAGE_QUALITY then
      image := avifImageCreate(aBitmap.Width, aBitmap.Height, 8, AVIF_PIXEL_FORMAT_YUV444)
    else
      image := avifImageCreate(aBitmap.Width, aBitmap.Height, 8, aPixelFormat{AVIF_PIXEL_FORMAT_YUV420}{AVIF_PIXEL_FORMAT_YUV444});

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
    image^.colorPrimaries := AVIF_COLOR_PRIMARIES_BT709;
    image^.transferCharacteristics := AVIF_TRANSFER_CHARACTERISTICS_SRGB;
    image^.matrixCoefficients := AVIF_MATRIX_COEFFICIENTS_BT601;

    // Override RGB(A)->YUV(A) defaults here: depth, format, chromaUpsampling, ignoreAlpha, alphaPremultiplied, libYUVUsage, etc
    // Alternative: set rgb.pixels and rgb.rowBytes yourself, which should match your chosen rgb.format
    // Be sure to use uint16_t* instead of uint8_t* for rgb.pixels/rgb.rowBytes if (rgb.depth > 8)
    if AVIF_VERSION >= AVIF_VERSION_0_10_0 then
    begin
      wrgb0_10:=Default(avifRGBImage0_10_0);
      prgb:= @wrgb0_10;
      // If you have RGB(A) data you want to encode, use this path
      avifRGBImageSetDefaults(prgb, image);
      wrgb0_10.Width := aBitmap.Width;
      wrgb0_10.Height := aBitmap.Height;
      {$push}{$warn 6018 off} //unreachable code
      if TBGRAPixel_RGBAOrder then
        wrgb0_10.format := AVIF_RGB_FORMAT_RGBA
      else
        wrgb0_10.format := AVIF_RGB_FORMAT_BGRA;
      {$pop}
      if aIgnoreAlpha then
        wrgb0_10.ignoreAlpha := AVIF_TRUE
      else
        wrgb0_10.ignoreAlpha := AVIF_FALSE;
      wrgb0_10.pixels := aBitmap.DataByte;
      wrgb0_10.rowBytes := aBitmap.Width * 4;
    end else
    begin
      wrgb0_8:=Default(avifRGBImage0_8_4);
      prgb:= @wrgb0_8;
      // If you have RGB(A) data you want to encode, use this path
      avifRGBImageSetDefaults(prgb, image);
      wrgb0_8.Width := aBitmap.Width;
      wrgb0_8.Height := aBitmap.Height;
      {$push}{$warn 6018 off} //unreachable code
      if TBGRAPixel_RGBAOrder then
        wrgb0_8.format := AVIF_RGB_FORMAT_RGBA
      else
        wrgb0_8.format := AVIF_RGB_FORMAT_BGRA;
      {$pop}
      if aIgnoreAlpha then
        wrgb0_8.ignoreAlpha := AVIF_TRUE
      else
        wrgb0_8.ignoreAlpha := AVIF_FALSE;
      wrgb0_8.pixels := aBitmap.DataByte;
      wrgb0_8.rowBytes := aBitmap.Width * 4;
    end;
    if aQuality0to100 = LOSS_LESS_IMAGE_QUALITY then
    begin
      image^.yuvRange := AVIF_RANGE_FULL;
      image^.matrixCoefficients := AVIF_MATRIX_COEFFICIENTS_IDENTITY; // this is key for lossless
    end;
    convertResult := avifImageRGBToYUV(image, prgb);
    if convertResult <> AVIF_RESULT_OK then
      raise EAvifException.Create('Failed to convert to YUV(A): ' + avifResultToString(convertResult));
    encoder := avifEncoderCreate();
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

    encoder^.maxThreads := 2;
    encoder^.speed := aSpeed0to10;
    if aQuality0to100 = LOSS_LESS_IMAGE_QUALITY then
    begin
      // Set defaults, and warn later on if anything looks incorrect
      //input.requestedFormat = AVIF_PIXEL_FORMAT_YUV444; // don't subsample when using AVIF_MATRIX_COEFFICIENTS_IDENTITY
      encoder^.minQuantizer := AVIF_QUANTIZER_LOSSLESS;
      encoder^.maxQuantizer := AVIF_QUANTIZER_LOSSLESS;
      encoder^.minQuantizerAlpha := AVIF_QUANTIZER_LOSSLESS;
      encoder^.maxQuantizerAlpha := AVIF_QUANTIZER_LOSSLESS;
      encoder^.codecChoice := AVIF_CODEC_CHOICE_AOM;              // rav1e doesn't support lossless transform yet:
      // https://github.com/xiph/rav1e/issues/151
      image^.yuvRange := AVIF_RANGE_FULL; // avoid limited range
      image^.matrixCoefficients := AVIF_MATRIX_COEFFICIENTS_IDENTITY; // this is key for lossless
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
      encoder^.minQuantizer := min_quantizer;
      encoder^.maxQuantizer := max_quantizer;
      encoder^.minQuantizerAlpha := 0;
      encoder^.maxQuantizerAlpha := alpha_quantizer;
    end;

    if aBitmap.LineOrder <> riloTopToBottom then   //vertical mirror.
    begin
      image^.transformFlags := image^.transformFlags + uint32(AVIF_TRANSFORM_IMIR);
      image^.imir.mode := 0;
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
