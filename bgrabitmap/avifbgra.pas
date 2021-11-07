{  Encode/Decode avif images from/to BGRABitmap. }
{* Author: Domingo Galmes <dgalmesp@gmail.com>  01-11-2021
******************************************************************************
* Copyright (c) 2021 Domingo Galmés
*
* Permission is hereby granted, free of charge, to any person obtaining a
* copy of this software and associated documentation files (the "Software"),
* to deal in the Software without restriction, including without limitation
* the rights to use, copy, modify, merge, publish, distribute, sublicense,
* and/or sell copies of the Software, and to permit persons to whom the
* Software is furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included
* in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
* OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO COORD SHALL
* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
* DEALINGS IN THE SOFTWARE.
*****************************************************************************}

unit avifbgra;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, bgrabitmap;

type
  EAvifException = class(Exception);

const
  AVIF_DEFAULT_QUALITY = 30;

procedure AvifLoadFromStream(AStream: TStream; aBitmap: TBGRABitmap);
procedure AvifLoadFromFile(const AFilename: string; aBitmap: TBGRABitmap);
procedure AvifLoadFromFileNative(const AFilename: string; aBitmap: TBGRABitmap);
procedure AvifLoadFromMemory(AData: Pointer; ASize: cardinal; aBitmap: TBGRABitmap);
//100 LOSSLESS
function AvifSaveToStream(aBitmap: TBGRABitmap; AStream: TStream; aQuality0to100: integer = 30;aSpeed0to10:integer=6): cardinal;
function AvifSaveToFile(aBitmap: TBGRABitmap; const AFilename: string; aQuality0to100: integer = 30;aSpeed0to10:integer=6): cardinal;
//returns size of the resulting bitmap.
function AvifSaveToMemory(aBitmap: TBGRABitmap; AData: Pointer; ASize: cardinal; aQuality0to100: integer = 30;aSpeed0to10:integer=6): cardinal;
//aBuffer  12 first bytes of file.
function AvifValidateHeaderSignature(aBuffer: Pointer): boolean;

implementation

uses
  libavif, Math, BGRABitmapTypes;

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
begin
  io := avifIOCreateStreamReader(aStream);
  if io = nil then
    exit(AVIF_RESULT_IO_ERROR);
  avifDecoderSetIO(decoder, io);
  exit(AVIF_RESULT_OK);
end;

procedure AvifDecode(decoder: PavifDecoder; aBitmap: TBGRABitmap);
var
  res: avifResult;
  wrgb: avifRGBImage;
begin
  res := avifDecoderParse(decoder);
  if res <> AVIF_RESULT_OK then
    raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
  //  Memo1.Lines.Add(Format('Parsed AVIF: %ux%u (%ubpc)', [decoder^.image^.Width, decoder^.image^.Height, decoder^.image^.depth]));
  if avifDecoderNextImage(decoder) = AVIF_RESULT_OK then
  begin
    //fillchar(wrgb, sizeof(wrgb), 0);
    wrgb:=Default(avifRGBImage);
    avifRGBImageSetDefaults(@wrgb, decoder^.image);
    //aBitmap.LineOrder:=riloTopToBottom;
    aBitmap.SetSize(wrgb.Width, wrgb.Height);
    wrgb.pixels := PUint8(aBitmap.databyte);
    wrgb.depth := 8;
    {$push}{$warn 6018 off} //unreachable code
    if TBGRAPixel_RGBAOrder then
      wrgb.format := AVIF_RGB_FORMAT_RGBA
    else
      wrgb.format := AVIF_RGB_FORMAT_BGRA;
    {$pop}
    wrgb.rowBytes := wrgb.Width * 4;
    //if aBitmap.LineOrder<>riloTopToBottom then
    //begin
    //  decoder^.image^.transformFlags:=decoder^.image^.transformFlags + Uint32(AVIF_TRANSFORM_IMIR);
    //  decoder^.image^.imir.mode:=0;
    //end;
    //decoder^.image^.imir.axis:=0; //vertical mirror
    res := avifImageYUVToRGB(decoder^.image, @wrgb);
    if res <> AVIF_RESULT_OK then
      raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
    if (aBitmap.LineOrder <> riloTopToBottom) and not
      (((longword(decoder^.image^.transformFlags) and longword(AVIF_TRANSFORM_IMIR))) = longword(AVIF_TRANSFORM_IMIR)) and
      (decoder^.image^.imir.mode = 0) then
      aBitmap.VerticalFlip;
    aBitmap.InvalidateBitmap;
  end
  else
    raise EAvifException.Create('Avif Error: ' + avifResultToString(res));
end;

procedure AvifLoadFromStream(AStream: TStream; aBitmap: TBGRABitmap);
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

procedure AvifLoadFromFile(const AFilename: string; aBitmap: TBGRABitmap);
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

procedure AvifLoadFromFileNative(const AFilename: string; aBitmap: TBGRABitmap);
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

procedure AvifLoadFromMemory(AData: Pointer; ASize: cardinal; aBitmap: TBGRABitmap);
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
procedure AvifEncode(aBitmap: TBGRABitmap; aQuality0to100: integer; aSpeed0to10:integer; var avifOutput: avifRWData);
var
  encoder: PavifEncoder;
  wrgb: avifRGBImage;
  image: PavifImage;
  convertResult, addImageResult, finishResult: avifResult;
  alpha_quantizer, min_quantizer, max_quantizer: integer;
  quality: integer;
const
  LOSS_LESS_IMAGE_QUALITY = 100;
begin
  encoder := nil;
  //FillChar(wrgb, sizeof(wrgb), 0);
  wrgb:=Default(avifRGBImage);
  try
    if aQuality0to100 = LOSS_LESS_IMAGE_QUALITY then
      image := avifImageCreate(aBitmap.Width, aBitmap.Height, 8, AVIF_PIXEL_FORMAT_YUV444)
    else
      image := avifImageCreate(aBitmap.Width, aBitmap.Height, 8, AVIF_PIXEL_FORMAT_YUV420{AVIF_PIXEL_FORMAT_YUV444});

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

    // If you have RGB(A) data you want to encode, use this path
    avifRGBImageSetDefaults(@wrgb, image);
    // Override RGB(A)->YUV(A) defaults here: depth, format, chromaUpsampling, ignoreAlpha, alphaPremultiplied, libYUVUsage, etc
    // Alternative: set rgb.pixels and rgb.rowBytes yourself, which should match your chosen rgb.format
    // Be sure to use uint16_t* instead of uint8_t* for rgb.pixels/rgb.rowBytes if (rgb.depth > 8)
    wrgb.Width := aBitmap.Width;
    wrgb.Height := aBitmap.Height;
    {$push}{$warn 6018 off} //unreachable code
    if TBGRAPixel_RGBAOrder then
      wrgb.format := AVIF_RGB_FORMAT_RGBA
    else
      wrgb.format := AVIF_RGB_FORMAT_BGRA;
    {$pop}
    wrgb.ignoreAlpha := 0;
    wrgb.pixels := aBitmap.DataByte;
    wrgb.rowBytes := aBitmap.Width * 4;
    if aQuality0to100 = LOSS_LESS_IMAGE_QUALITY then
    begin
      image^.yuvRange := AVIF_RANGE_FULL;
      image^.matrixCoefficients := AVIF_MATRIX_COEFFICIENTS_IDENTITY; // this is key for lossless
    end;
    convertResult := avifImageRGBToYUV(image, @wrgb);
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

function AvifSaveToFile(aBitmap: TBGRABitmap; const AFilename: string; aQuality0to100: integer;aSpeed0to10:integer=6): cardinal;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    Result := AvifSaveToStream(aBitmap, Stream, aQuality0to100, aSpeed0to10);
  finally
    Stream.Free;
  end;
end;

function AvifSaveToStream(aBitmap: TBGRABitmap; AStream: TStream; aQuality0to100: integer;aSpeed0to10:integer): cardinal;
var
  avifOutput: avifRWData;
  p:PByte;
  remain,toWrite:LongWord;
const
  CopySize=65535;
begin
  try
    avifOutput := AVIF_DATA_EMPTY;
    AvifEncode(aBitmap, aQuality0to100,aSpeed0to10, avifOutput);
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
function AvifSaveToMemory(aBitmap: TBGRABitmap; AData: Pointer; ASize: cardinal; aQuality0to100: integer;aSpeed0to10:integer=6): cardinal;
var
  avifOutput: avifRWData;
begin
  try
    avifOutput := AVIF_DATA_EMPTY;
    AvifEncode(aBitmap, aQuality0to100, aSpeed0to10, avifOutput);
    Result := avifOutput.size;
    if avifOutput.Data <> nil then
      Move(avifOutput.Data^, aData^, min(ASize, avifOutput.size));
  finally
    avifRWDataFree(@avifOutput);
  end;
end;

end.
