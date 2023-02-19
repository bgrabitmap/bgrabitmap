// SPDX-License-Identifier: LGPL-3.0-linking-exception
// Copyright 2019 Joe Drago. All rights reserved (originally as BSD-2-Clause)

{ FreePascal wrappper for libavif dynamic library                       }
{ For the latest libavif visit https://github.com/AOMediaCodec/libavif  }
{ Author: Domingo Galmes <dgalmesp@gmail.com>  01-11-2021               }

unit libavif;

interface

var
 AVIF_VERSION, AVIF_VERSION_MAJOR, AVIF_VERSION_MINOR, AVIF_VERSION_PATCH : integer;

const
  AVIF_VERSION_0_8_4 = 00080400;
  AVIF_VERSION_0_9_2 = 00090200;
  AVIF_VERSION_0_9_3 = 00090300;
  AVIF_VERSION_0_10_0 = 00100000;
  AVIF_VERSION_0_10_1 = 00100100;
  AVIF_VERSION_0_11_0 = 00110000;

{
  For windows there are prebuild dlls in
  https://packages.msys2.org/queue

  Install gcc/msys2 from
  https://www.msys2.org/
  and install

  // 32 BITS
  pacman -S --needed base-devel mingw-w64-i686-toolchain
  pacman -S --needed mingw-w64-i686-libavif
  (note that has many dependencies).

  // 64 BITS
  pacman -S --needed base-devel mingw-w64-x86_64-toolchain
  pacman -S --needed mingw-w64-x86_64-libavif

  THE dlls are in the bin folder.
  C:\mingw64_32msys2\mingw64\bin

}

{$define LOAD_DYNAMICALLY}

// if load_dynamically use () to call functions or procedures without parameters
// wS:=avifVersion();  //ok  wS:=aviVersion;  //error.

{$IFDEF LOAD_DYNAMICALLY}
  {$DEFINE LD}
{$ENDIF}

//const APICALLTYPE=cdecl;    cdecl

{
  Partially converted by H2Pas 1.0.0 from avif.h
  The following command line parameters were used:
    -p
    -D
    -l
    LIBAFIV
    avif.h
}

    const
      LibAvifFilename =
      {$if defined(Win32)}
        'libavif.dll'
      {$elseif defined(Win64)}
        'libavif.dll'
      {$elseif defined(Darwin)}
        'libavif.9.dylib'
      {$elseif defined(Unix)}
        'libavif.so.9'      // version 0.8.4 in Debian bullseye
      {$else}
        ''
      {$endif};

    type
      size_type = NativeUInt;
      float = single;

{$IFDEF FPC}
{$PACKRECORDS C}
//{$PACKENUM 4}

{$ENDIF}


{ C++ extern C conditionnal removed }
  { --------------------------------------------------------------------------- }
  { Export macros }
  { AVIF_BUILDING_SHARED_LIBS should only be defined when libavif is being built }
  { as a shared library. }
  { AVIF_DLL should be defined if libavif is a shared library. If you are using }
  { libavif as CMake dependency, through CMake package config file or through }
  { pkg-config, this is defined automatically. }
  { }
  { Here's what AVIF_API will be defined as in shared build: }
  { |       |        Windows        |                  Unix                  | }
  { | Build | __declspec(dllexport) | __attribute__((visibility("default"))) | }
  { |  Use  | __declspec(dllimport) |                                        | }
  { }
  { For static build, AVIF_API is always defined as nothing. }

  { --------------------------------------------------------------------------- }

    type
      PavifBool = ^avifBool;
      avifBool = longint;

    const
      AVIF_TRUE = 1;      
      AVIF_FALSE = 0;      
      AVIF_DIAGNOSTICS_ERROR_BUFFER_SIZE = 256;      
    { A reasonable default for maximum image size to avoid out-of-memory errors or integer overflow in }
    { (32-bit) int or unsigned int arithmetic operations. }
      AVIF_DEFAULT_IMAGE_SIZE_LIMIT = 16384*16384;
    { A reasonable default for maximum image dimension (width or height). }
      AVIF_DEFAULT_IMAGE_DIMENSION_LIMIT = 32768;
    { a 12 hour AVIF image sequence, running at 60 fps (a basic sanity check as this is quite ridiculous) }
      AVIF_DEFAULT_IMAGE_COUNT_LIMIT = (12*3600)*60;      
      AVIF_QUANTIZER_LOSSLESS = 0;      
      AVIF_QUANTIZER_BEST_QUALITY = 0;      
      AVIF_QUANTIZER_WORST_QUALITY = 63;      
      AVIF_PLANE_COUNT_YUV = 3;      
      AVIF_SPEED_DEFAULT = -(1);      
      AVIF_SPEED_SLOWEST = 0;      
      AVIF_SPEED_FASTEST = 10;      

    type
      TAvifArrayOf256AnsiChar = array[0..255] of AnsiChar;
      TAvifArrayOf8Float= array[0..7] of float;

      PavifPlanesFlag = ^avifPlanesFlag;
      avifPlanesFlag = (AVIF_PLANES_YUV := 1 shl 0,AVIF_PLANES_A := 1 shl 1,
        AVIF_PLANES_ALL := $ff);

      PavifPlanesFlags = ^avifPlanesFlags;
      avifPlanesFlags = UInt32;
    { rgbPlanes }
    { yuvPlanes }
    { These can be used as the index for the yuvPlanes and yuvRowBytes arrays in avifImage.}
      avifChannelIndex = (AVIF_CHAN_Y := 0,
        AVIF_CHAN_U := 1,AVIF_CHAN_V := 2
        );
    { --------------------------------------------------------------------------- }
    { Version }
    {$IFDEF LD}var{$ELSE}function{$ENDIF} avifVersion{$IFDEF LD}: function{$ENDIF}:PAnsiChar;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
    {$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifCodecVersions{$IFDEF LD}: procedure{$ENDIF}(var outBuffer:TAvifArrayOf256AnsiChar);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
    {$IFDEF LD}var{$ELSE}function{$ENDIF} avifLibYUVVersion{$IFDEF LD}: function{$ENDIF}:cardinal;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF} // returns 0 if libavif wasn't compiled with libyuv support
    { Memory management }
    {$IFDEF LD}var{$ELSE}function{$ENDIF} avifAlloc{$IFDEF LD}: function{$ENDIF}(size:size_type):pointer;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
    {$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifFree{$IFDEF LD}: procedure{$ENDIF}(p:Pointer);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
    { --------------------------------------------------------------------------- }
    { avifResult }
    { the avifIO field of avifDecoder is not set }
    { similar to EAGAIN/EWOULDBLOCK, this means the avifIO doesn't have necessary data available yet }
    { an argument passed into this function is invalid }
    { a requested code path is not (yet) implemented }
type
      PavifResult = ^avifResult;
      avifResult = (AVIF_RESULT_OK := 0,AVIF_RESULT_UNKNOWN_ERROR,
        AVIF_RESULT_INVALID_FTYP,AVIF_RESULT_NO_CONTENT,
        AVIF_RESULT_NO_YUV_FORMAT_SELECTED,
        AVIF_RESULT_REFORMAT_FAILED,AVIF_RESULT_UNSUPPORTED_DEPTH,
        AVIF_RESULT_ENCODE_COLOR_FAILED,AVIF_RESULT_ENCODE_ALPHA_FAILED,
        AVIF_RESULT_BMFF_PARSE_FAILED,AVIF_RESULT_NO_AV1_ITEMS_FOUND,
        AVIF_RESULT_DECODE_COLOR_FAILED,AVIF_RESULT_DECODE_ALPHA_FAILED,
        AVIF_RESULT_COLOR_ALPHA_SIZE_MISMATCH,
        AVIF_RESULT_ISPE_SIZE_MISMATCH,AVIF_RESULT_NO_CODEC_AVAILABLE,
        AVIF_RESULT_NO_IMAGES_REMAINING,AVIF_RESULT_INVALID_EXIF_PAYLOAD,
        AVIF_RESULT_INVALID_IMAGE_GRID,AVIF_RESULT_INVALID_CODEC_SPECIFIC_OPTION,
        AVIF_RESULT_TRUNCATED_DATA,AVIF_RESULT_IO_NOT_SET,
        AVIF_RESULT_IO_ERROR,AVIF_RESULT_WAITING_ON_IO,
        AVIF_RESULT_INVALID_ARGUMENT,AVIF_RESULT_NOT_IMPLEMENTED,
        AVIF_RESULT_OUT_OF_MEMORY,
        AVIF_RESULT_CANNOT_CHANGE_SETTING, // a setting that can't change is changed during encoding
        AVIF_RESULT_INCOMPATIBLE_IMAGE     // the image is incompatible with already encoded images
        );

    {$IFDEF LD}var{$ELSE}function{$ENDIF} avifResultToString{$IFDEF LD}: function{$ENDIF}(AResult:avifResult):PAnsiChar;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
    { --------------------------------------------------------------------------- }
    { avifROData/avifRWData: Generic raw memory storage }
type
      PavifROData = ^avifROData;
      avifROData = record
          data : PUInt8;
          size : size_type;
        end;
    { Note: Use avifRWDataFree() if any avif*() function populates one of these. }

      PavifRWData = ^avifRWData;
      avifRWData = record
          data : PUInt8;
          size : size_type;
        end;
    { Initialize avifROData/avifRWData on the stack with this }

//#define AVIF_DATA_EMPTY { NULL, 0 }
const AVIF_DATA_EMPTY:avifRWData=(data:nil;size:0);

  { The avifRWData input must be zero-initialized before being manipulated with these functions. }
  {$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifRWDataRealloc{$IFDEF LD}: procedure{$ENDIF}(raw:PavifRWData;newSize:size_type);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
  {$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifRWDataSet{$IFDEF LD}: procedure{$ENDIF}(raw:PavifRWData; const data:PByte;len:size_type);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
  {$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifRWDataFree{$IFDEF LD}: procedure{$ENDIF}(raw:PavifRWData);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

    { --------------------------------------------------------------------------- }
    { avifPixelFormat }
    //
    // Note to libavif maintainers: The lookup tables in avifImageYUVToRGBLibYUV
    // rely on the ordering of this enum values for their correctness. So changing
    // the values in this enum will require auditing avifImageYUVToRGBLibYUV for
    // correctness.
    { No pixels are present }
type
      PavifPixelFormat = ^avifPixelFormat;
      avifPixelFormat = (AVIF_PIXEL_FORMAT_NONE := 0,AVIF_PIXEL_FORMAT_YUV444,
        AVIF_PIXEL_FORMAT_YUV422,AVIF_PIXEL_FORMAT_YUV420,
        AVIF_PIXEL_FORMAT_YUV400,AVIF_PIXEL_FORMAT_COUNT);

    {$IFDEF LD}var{$ELSE}function{$ENDIF} avifPixelFormatToString{$IFDEF LD}: function{$ENDIF}(format:avifPixelFormat):PAnsiChar;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
type
      PavifPixelFormatInfo = ^avifPixelFormatInfo;
      avifPixelFormatInfo = record
          monochrome : avifBool;
          chromaShiftX : longint;
          chromaShiftY : longint;
        end;
  {$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifGetPixelFormatInfo{$IFDEF LD}: procedure{$ENDIF}(format: avifPixelFormat;info:PavifPixelFormatInfo);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
    { --------------------------------------------------------------------------- }
    { avifChromaSamplePosition }
type
      PavifChromaSamplePosition = ^avifChromaSamplePosition;
      avifChromaSamplePosition = (AVIF_CHROMA_SAMPLE_POSITION_UNKNOWN := 0,
        AVIF_CHROMA_SAMPLE_POSITION_VERTICAL := 1,
        AVIF_CHROMA_SAMPLE_POSITION_COLOCATED := 2
        );
    { --------------------------------------------------------------------------- }
    { avifRange }

      PavifRange = ^avifRange;
      avifRange = (AVIF_RANGE_LIMITED := 0,AVIF_RANGE_FULL := 1
        );
    { --------------------------------------------------------------------------- }
    { CICP enums - https://www.itu.int/rec/T-REC-H.273-201612-I/en }


    { This is actually reserved, but libavif uses it as a sentinel value. }
const
    AVIF_COLOR_PRIMARIES_UNKNOWN = 0;

    AVIF_COLOR_PRIMARIES_BT709 = 1;
    AVIF_COLOR_PRIMARIES_IEC61966_2_4 = 1;
    AVIF_COLOR_PRIMARIES_UNSPECIFIED = 2;
    AVIF_COLOR_PRIMARIES_BT470M = 4;
    AVIF_COLOR_PRIMARIES_BT470BG = 5;
    AVIF_COLOR_PRIMARIES_BT601 = 6;
    AVIF_COLOR_PRIMARIES_SMPTE240 = 7;
    AVIF_COLOR_PRIMARIES_GENERIC_FILM = 8;
    AVIF_COLOR_PRIMARIES_BT2020 = 9;
    AVIF_COLOR_PRIMARIES_XYZ = 10;
    AVIF_COLOR_PRIMARIES_SMPTE431 = 11;
    AVIF_COLOR_PRIMARIES_SMPTE432 = 12; // DCI P3
    AVIF_COLOR_PRIMARIES_EBU3213 = 22;
type

      PavifColorPrimaries = ^avifColorPrimaries;
      avifColorPrimaries = UInt16;
    { AVIF_COLOR_PRIMARIES_* }
    { outPrimaries: rX, rY, gX, gY, bX, bY, wX, wY }

{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifColorPrimariesGetValues{$IFDEF LD}: procedure{$ENDIF}(acp:avifColorPrimaries;var outPrimaries:TAvifArrayOf8Float);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifColorPrimariesFind{$IFDEF LD}: function{$ENDIF}(var inPrimaries:TAvifArrayOf8Float;outName:PPAnsiChar):avifColorPrimaries;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

const
    // This is actually reserved, but libavif uses it as a sentinel value.
    AVIF_TRANSFER_CHARACTERISTICS_UNKNOWN = 0;

    AVIF_TRANSFER_CHARACTERISTICS_BT709 = 1;
    AVIF_TRANSFER_CHARACTERISTICS_UNSPECIFIED = 2;
    AVIF_TRANSFER_CHARACTERISTICS_BT470M = 4;  // 2.2 gamma
    AVIF_TRANSFER_CHARACTERISTICS_BT470BG = 5; // 2.8 gamma
    AVIF_TRANSFER_CHARACTERISTICS_BT601 = 6;
    AVIF_TRANSFER_CHARACTERISTICS_SMPTE240 = 7;
    AVIF_TRANSFER_CHARACTERISTICS_LINEAR = 8;
    AVIF_TRANSFER_CHARACTERISTICS_LOG100 = 9;
    AVIF_TRANSFER_CHARACTERISTICS_LOG100_SQRT10 = 10;
    AVIF_TRANSFER_CHARACTERISTICS_IEC61966 = 11;
    AVIF_TRANSFER_CHARACTERISTICS_BT1361 = 12;
    AVIF_TRANSFER_CHARACTERISTICS_SRGB = 13;
    AVIF_TRANSFER_CHARACTERISTICS_BT2020_10BIT = 14;
    AVIF_TRANSFER_CHARACTERISTICS_BT2020_12BIT = 15;
    AVIF_TRANSFER_CHARACTERISTICS_SMPTE2084 = 16; // PQ
    AVIF_TRANSFER_CHARACTERISTICS_SMPTE428 = 17;
    AVIF_TRANSFER_CHARACTERISTICS_HLG = 18;
type

      PavifTransferCharacteristics = ^avifTransferCharacteristics;
      avifTransferCharacteristics = UInt16;
    { AVIF_TRANSFER_CHARACTERISTICS_* }
const 
    AVIF_MATRIX_COEFFICIENTS_IDENTITY = 0;
    AVIF_MATRIX_COEFFICIENTS_BT709 = 1;
    AVIF_MATRIX_COEFFICIENTS_UNSPECIFIED = 2;
    AVIF_MATRIX_COEFFICIENTS_FCC = 4;
    AVIF_MATRIX_COEFFICIENTS_BT470BG = 5;
    AVIF_MATRIX_COEFFICIENTS_BT601 = 6;
    AVIF_MATRIX_COEFFICIENTS_SMPTE240 = 7;
    AVIF_MATRIX_COEFFICIENTS_YCGCO = 8;
    AVIF_MATRIX_COEFFICIENTS_BT2020_NCL = 9;
    AVIF_MATRIX_COEFFICIENTS_BT2020_CL = 10;
    AVIF_MATRIX_COEFFICIENTS_SMPTE2085 = 11;
    AVIF_MATRIX_COEFFICIENTS_CHROMA_DERIVED_NCL = 12;
    AVIF_MATRIX_COEFFICIENTS_CHROMA_DERIVED_CL = 13;
    AVIF_MATRIX_COEFFICIENTS_ICTCP = 14;

type
      PavifMatrixCoefficients = ^avifMatrixCoefficients;
      avifMatrixCoefficients = UInt16; { AVIF_MATRIX_COEFFICIENTS_* }
    
    { --------------------------------------------------------------------------- }
    { avifDiagnostics }
    { Upon receiving an error from any non-const libavif API call, if the toplevel structure used }
    { in the API call (avifDecoder, avifEncoder) contains a diag member, this buffer may be }
    { populated with a NULL-terminated, freeform error string explaining the most recent error in }
    { more detail. It will be cleared at the beginning of every non-const API call. }

      PavifDiagnostics = ^avifDiagnostics;
      avifDiagnostics = record
          error : array[0..(AVIF_DIAGNOSTICS_ERROR_BUFFER_SIZE)-1] of char;
        end;

{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifDiagnosticsClearError{$IFDEF LD}: procedure{$ENDIF}(diag:PavifDiagnostics );cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
    { --------------------------------------------------------------------------- }
    { Optional transformation structs }
type
      PavifTransformFlag = ^avifTransformFlag;
      avifTransformFlag = (AVIF_TRANSFORM_NONE := 0,AVIF_TRANSFORM_PASP := 1 shl 0,
        AVIF_TRANSFORM_CLAP := 1 shl 1,AVIF_TRANSFORM_IROT := 1 shl 2,
        AVIF_TRANSFORM_IMIR := 1 shl 3);

      PavifTransformFlags = ^avifTransformFlags;
      avifTransformFlags = UInt32;
    { 'pasp' from ISO/IEC 14496-12:2015 12.1.4.3 }
    { define the relative width and height of a pixel }

      PavifPixelAspectRatioBox = ^avifPixelAspectRatioBox;
      avifPixelAspectRatioBox = record
          hSpacing : UInt32;
          vSpacing : UInt32;
        end;

      PavifCleanApertureBox = ^avifCleanApertureBox;
      avifCleanApertureBox = record
    { 'clap' from ISO/IEC 14496-12:2015 12.1.4.3 }
    { a fractional number which defines the exact clean aperture width, in counted pixels, of the video image }	  
          widthN : UInt32;
          widthD : UInt32;
    { a fractional number which defines the exact clean aperture height, in counted pixels, of the video image }		  
          heightN : UInt32;
          heightD : UInt32;
    { a fractional number which defines the horizontal offset of clean aperture centre minus (width-1)/2. Typically 0. }	  
          horizOffN : UInt32;
          horizOffD : UInt32;
    { a fractional number which defines the vertical offset of clean aperture centre minus (height-1)/2. Typically 0. } 
          vertOffN : UInt32;
          vertOffD : UInt32;
        end;

      PavifImageRotation = ^avifImageRotation;
      avifImageRotation = record
    { 'irot' from ISO/IEC 23008-12:2017 6.5.10 }
    { angle * 90 specifies the angle (in anti-clockwise direction) in units of degrees. } 
          angle : UInt8;      { legal values: [0-3] }
        end;
    { 'imir' from ISO/IEC 23008-12:2017 6.5.12 (Draft Amendment 2): }
    { }
    {     'mode' specifies how the mirroring is performed: }
    { }
    {     0 indicates that the top and bottom parts of the image are exchanged; }
    {     1 specifies that the left and right parts are exchanged. }
    { }
    {     NOTE In Exif, orientation tag can be used to signal mirroring operations. Exif }
    {     orientation tag 4 corresponds to mode = 0 of ImageMirror, and Exif orientation tag 2 }
    {     corresponds to mode = 1 accordingly. }
    { }
    { Legal values: [0, 1] }
    { }
    { NOTE: As of HEIF Draft Amendment 2, the name of this variable has changed from 'axis' to 'mode' as }
    {       the logic behind it has been *inverted*. Please use the wording above describing the legal }
    {       values for 'mode' and update any code that previously may have used `axis` to use }
    {       the *opposite* value (0 now means top-to-bottom, where it used to mean left-to-right). }

      PavifImageMirror = ^avifImageMirror;
      avifImageMirror = record
          mode : UInt8;
        end;
    { --------------------------------------------------------------------------- }
    { avifCropRect - Helper struct/functions to work with avifCleanApertureBox }

      PavifCropRect = ^avifCropRect;
      avifCropRect = record
          x : UInt32;
          y : UInt32;
          width : UInt32;
          height : UInt32;
        end;
    { These will return AVIF_FALSE if the resultant values violate any standards, and if so, the output }
    { values are not guaranteed to be complete or correct and should not be used. }
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifCropRectConvertCleanApertureBox{$IFDEF LD}: function{$ENDIF}(cropRect:PavifCropRect;
                                                      clap:PavifCleanApertureBox;
                                                      imageW:UInt32;
                                                      imageH:UInt32;
                                                      yuvFormat:avifPixelFormat;
                                                      diag:PavifDiagnostics):avifBool;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

{$IFDEF LD}var{$ELSE}function{$ENDIF} avifCleanApertureBoxConvertCropRect{$IFDEF LD}: function{$ENDIF}(clap:PavifCleanApertureBox;
                                                      cropRect:PavifCropRect;
                                                      imageW:UInt32; 
                                                      imageH:UInt32; 
                                                      yuvFormat:avifPixelFormat;
                                                      diag:PavifDiagnostics):avifBool;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

    { --------------------------------------------------------------------------- }
    { avifImage }
type
      PavifImage0_11_0 = ^avifImage0_11_0;
      PPavifImage0_11_0 = ^PavifImage0_11_0;
      avifImage0_11_0 = record
    { Image information }
          width : UInt32;
          height : UInt32;
          depth : UInt32;      { all planes must share this depth; if depth>8, all planes are UInt16 internally }
          yuvFormat : avifPixelFormat;
          yuvRange : avifRange;
          yuvChromaSamplePosition : avifChromaSamplePosition;
          yuvPlanes : array[0..(AVIF_PLANE_COUNT_YUV)-1] of PUInt8;
          yuvRowBytes : array[0..(AVIF_PLANE_COUNT_YUV)-1] of UInt32;
          imageOwnsYUVPlanes : avifBool;
          alphaPlane : PUInt8;
          alphaRowBytes : UInt32;
          imageOwnsAlphaPlane : avifBool;
          alphaPremultiplied : avifBool;
    { ICC Profile }
          icc : avifRWData;
    { CICP information: }
    { These are stored in the AV1 payload and used to signal YUV conversion. Additionally, if an }
    { ICC profile is not specified, these will be stored in the AVIF container's `colr` box with }
    { a type of `nclx`. If your system supports ICC profiles, be sure to check for the existence }
    { of one (avifImage.icc) before relying on the values listed here! }

          colorPrimaries : avifColorPrimaries;
          transferCharacteristics : avifTransferCharacteristics;
          matrixCoefficients : avifMatrixCoefficients;
    { Transformations - These metadata values are encoded/decoded when transformFlags are set }
    { appropriately, but do not impact/adjust the actual pixel buffers used (images won't be }
    { pre-cropped or mirrored upon decode). Basic explanations from the standards are offered in }
    { comments above, but for detailed explanations, please refer to the HEIF standard (ISO/IEC }
    { 23008-12:2017) and the BMFF standard (ISO/IEC 14496-12:2015). }
    { }
    { To encode any of these boxes, set the values in the associated box, then enable the flag in }
    { transformFlags. On decode, only honor the values in boxes with the associated transform flag set. }
          transformFlags : avifTransformFlags;
          pasp : avifPixelAspectRatioBox;
          clap : avifCleanApertureBox;
          irot : avifImageRotation;
          imir : avifImageMirror;
          { Metadata - set with avifImageSetMetadata*() before write, check .size>0 for existence after read }
          exif : avifRWData;
          xmp : avifRWData;
        end;

      PavifImage = Pointer;
      PPavifImage = ^PavifImage;
      PavifImage0_8_4= ^avifImage0_8_4;
      PPavifImage0_8_4 = ^PavifImage0_8_4;
      avifImage0_8_4 = record
    { Image information }	  
          width : UInt32;
          height : UInt32;
          depth : UInt32;      { all planes must share this depth; if depth>8, all planes are UInt16 internally }
          yuvFormat : avifPixelFormat;
          yuvRange : avifRange;
          yuvChromaSamplePosition : avifChromaSamplePosition;
          yuvPlanes : array[0..(AVIF_PLANE_COUNT_YUV)-1] of PUInt8;
          yuvRowBytes : array[0..(AVIF_PLANE_COUNT_YUV)-1] of UInt32;
          imageOwnsYUVPlanes : avifBool;
          alphaRange : avifRange;
          alphaPlane : PUInt8;
          alphaRowBytes : UInt32;
          imageOwnsAlphaPlane : avifBool;
          alphaPremultiplied : avifBool;
    { ICC Profile }		  
          icc : avifRWData;
    { CICP information: }
    { These are stored in the AV1 payload and used to signal YUV conversion. Additionally, if an }
    { ICC profile is not specified, these will be stored in the AVIF container's `colr` box with }
    { a type of `nclx`. If your system supports ICC profiles, be sure to check for the existence }
    { of one (avifImage.icc) before relying on the values listed here! }
		  
          colorPrimaries : avifColorPrimaries;
          transferCharacteristics : avifTransferCharacteristics;
          matrixCoefficients : avifMatrixCoefficients;
    { Transformations - These metadata values are encoded/decoded when transformFlags are set }
    { appropriately, but do not impact/adjust the actual pixel buffers used (images won't be }
    { pre-cropped or mirrored upon decode). Basic explanations from the standards are offered in }
    { comments above, but for detailed explanations, please refer to the HEIF standard (ISO/IEC }
    { 23008-12:2017) and the BMFF standard (ISO/IEC 14496-12:2015). }
    { }
    { To encode any of these boxes, set the values in the associated box, then enable the flag in }
    { transformFlags. On decode, only honor the values in boxes with the associated transform flag set. }
          transformFlags : avifTransformFlags;
          pasp : avifPixelAspectRatioBox;
          clap : avifCleanApertureBox;
          irot : avifImageRotation;
          imir : avifImageMirror;
          { Metadata - set with avifImageSetMetadata*() before write, check .size>0 for existence after read }
          exif : avifRWData;
          xmp : avifRWData;
        end;
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifImageCreate{$IFDEF LD}: function{$ENDIF}(width:uint32;height:uint32;depth:uint32;yuvFormat:avifPixelFormat):PavifImage;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifImageCreateEmpty{$IFDEF LD}: function{$ENDIF}:PavifImage;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF} // helper for making an image to decode into
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifImageCopy{$IFDEF LD}: function{$ENDIF}(dstImage:PavifImage;srcImage:PavifImage;planes:avifPlanesFlags):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF} // deep copy
{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifImageDestroy{$IFDEF LD}: procedure{$ENDIF}(image:PavifImage);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifImageSetProfileICC{$IFDEF LD}: procedure{$ENDIF}(image:PavifImage;icc:PByte;iccSize:size_type);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{ Sets Exif metadata. Attempts to parse the Exif metadata for Exif orientation. Sets
image->transformFlags, image->irot and image->imir if the Exif metadata is parsed successfully,
otherwise leaves image->transformFlags, image->irot and image->imir unchanged.
Warning: If the Exif payload is set and invalid, avifEncoderWrite() may return AVIF_RESULT_INVALID_EXIF_PAYLOAD. }
{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifImageSetMetadataExif{$IFDEF LD}: procedure{$ENDIF}(image:PavifImage;exif:PByte;exifSize:size_type);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{Sets XMP metadata. }
{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifImageSetMetadataXMP{$IFDEF LD}: procedure{$ENDIF}(image:PavifImage;xmp:PByte;xmpSize:size_type);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifImageAllocatePlanes{$IFDEF LD}: function{$ENDIF}(image:PavifImage;planes:avifPlanesFlags):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF} // Ignores any pre-existing planes
{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifImageFreePlanes{$IFDEF LD}: procedure{$ENDIF}(image:PavifImage;planes:avifPlanesFlags);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}     // Ignores already-freed planes
{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifImageStealPlanes{$IFDEF LD}: procedure{$ENDIF}(dstImage:PavifImage;srcImage:PavifImage;planes:avifPlanesFlags);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
    { --------------------------------------------------------------------------- }
    { Understanding maxThreads }
    { }
    { libavif's structures and API use the setting 'maxThreads' in a few places. The intent of this }
    { setting is to limit concurrent thread activity/usage, not necessarily to put a hard ceiling on }
    { how many sleeping threads happen to exist behind the scenes. The goal of this setting is to }
    { ensure that at any given point during libavif's encoding or decoding, no more than *maxThreads* }
    { threads are simultaneously **active and taking CPU time**. }
    { }
    { As an important example, when encoding an image sequence that has an alpha channel, two }
    { long-lived underlying AV1 encoders must simultaneously exist (one for color, one for alpha). For }
    { each additional frame fed into libavif, its YUV planes are fed into one instance of the AV1 }
    { encoder, and its alpha plane is fed into another. These operations happen serially, so only one }
    { of these AV1 encoders is ever active at a time. However, the AV1 encoders might pre-create a }
    { pool of worker threads upon initialization, so during this process, twice the amount of worker }
    { threads actually simultaneously exist on the machine, but half of them are guaranteed to be }
    { sleeping. }
    { }
    { This design ensures that AV1 implementations are given as many threads as possible to ensure a }
    { speedy encode or decode, despite the complexities of occasionally needing two AV1 codec instances }
    { (due to alpha payloads being separate from color payloads). If your system has a hard ceiling on }
    { the number of threads that can ever be in flight at a given time, please account for this }
    { accordingly. }
    { --------------------------------------------------------------------------- }
    { Optional YUV<->RGB support }
    { To convert to/from RGB, create an avifRGBImage on the stack, call avifRGBImageSetDefaults() on }
    { it, and then tweak the values inside of it accordingly. At a minimum, you should populate }
    { ->pixels and ->rowBytes with an appropriately sized pixel buffer, which should be at least }
    { (->rowBytes * ->height) bytes, where ->rowBytes is at least (->width * avifRGBImagePixelSize()). }
    { If you don't want to supply your own pixel buffer, you can use the }
    { avifRGBImageAllocatePixels()/avifRGBImageFreePixels() convenience functions. }
    { avifImageRGBToYUV() and avifImageYUVToRGB() will perform depth rescaling and limited<->full range }
    { conversion, if necessary. Pixels in an avifRGBImage buffer are always full range, and conversion }
    { routines will fail if the width and height don't match the associated avifImage. }
    { If libavif is built with a version of libyuv offering a fast conversion between RGB and YUV for }
    { the given inputs, libavif will use it. See reformat_libyuv.c for the details. }
    { libyuv is faster but may have slightly less precision than built-in conversion, so avoidLibYUV }
    { can be set to AVIF_TRUE when AVIF_CHROMA_UPSAMPLING_BEST_QUALITY or }
    { AVIF_CHROMA_DOWNSAMPLING_BEST_QUALITY is used, to get the most precise but slowest results. }

    { Note to libavif maintainers: The lookup tables in avifImageYUVToRGBLibYUV }
    { rely on the ordering of this enum values for their correctness. So changing }
    { the values in this enum will require auditing avifImageYUVToRGBLibYUV for }
    { correctness. }
type
      PavifRGBFormat = ^avifRGBFormat;
      avifRGBFormat = (AVIF_RGB_FORMAT_RGB := 0,AVIF_RGB_FORMAT_RGBA,
        AVIF_RGB_FORMAT_ARGB,AVIF_RGB_FORMAT_BGR,
        AVIF_RGB_FORMAT_BGRA,AVIF_RGB_FORMAT_ABGR,
        // RGB_565 format uses five bits for the red and blue components and six
        // bits for the green component. Each RGB pixel is 16 bits (2 bytes), which
        // is packed as follows:
        //   uint16_t: [r4 r3 r2 r1 r0 g5 g4 g3 g2 g1 g0 b4 b3 b2 b1 b0]
        //   r4 and r0 are the MSB and LSB of the red component respectively.
        //   g5 and g0 are the MSB and LSB of the green component respectively.
        //   b4 and b0 are the MSB and LSB of the blue component respectively.
        // This format is only supported for YUV -> RGB conversion and when
        // avifRGBImage.depth is set to 8.
        AVIF_RGB_FORMAT_RGB_565,
        AVIF_RGB_FORMAT_COUNT
      );

{$IFDEF LD}var{$ELSE}function{$ENDIF} avifRGBFormatChannelCount{$IFDEF LD}: function{$ENDIF}(format:avifRGBFormat):UInt32;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifRGBFormatHasAlpha{$IFDEF LD}: function{$ENDIF}(format:avifRGBFormat):avifBool;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
type
      PavifChromaUpsampling = ^avifChromaUpsampling;
      avifChromaUpsampling = (
        AVIF_CHROMA_UPSAMPLING_AUTOMATIC = 0,    // Chooses best trade off of speed/quality (uses BILINEAR libyuv if available,
                                                 // or falls back to NEAREST libyuv if available, or falls back to BILINEAR built-in)
        AVIF_CHROMA_UPSAMPLING_FASTEST = 1,      // Chooses speed over quality (same as NEAREST)
        AVIF_CHROMA_UPSAMPLING_BEST_QUALITY = 2, // Chooses the best quality upsampling, given settings (same as BILINEAR)
        AVIF_CHROMA_UPSAMPLING_NEAREST = 3,      // Uses nearest-neighbor filter
        AVIF_CHROMA_UPSAMPLING_BILINEAR = 4      // Uses bilinear filter
      );
      avifChromaDownsampling = (
        AVIF_CHROMA_DOWNSAMPLING_AUTOMATIC = 0,    // Chooses best trade off of speed/quality (same as AVERAGE)
        AVIF_CHROMA_DOWNSAMPLING_FASTEST = 1,      // Chooses speed over quality (same as AVERAGE)
        AVIF_CHROMA_DOWNSAMPLING_BEST_QUALITY = 2, // Chooses the best quality upsampling (same as AVERAGE)
        AVIF_CHROMA_DOWNSAMPLING_AVERAGE = 3,      // Uses averaging filter
        AVIF_CHROMA_DOWNSAMPLING_SHARP_YUV = 4     // Uses sharp yuv filter (libsharpyuv), available for 4:2:0 only, ignored for 4:2:2
      );

      PavifRGBImage = pointer;
      PavifRGBImage0_8_4 = ^avifRGBImage0_8_4;
      avifRGBImage0_8_4 = record
          width : UInt32;      { must match associated avifImage }
          height : UInt32;    { must match associated avifImage }
          depth : UInt32;    { legal depths [8, 10, 12, 16]. if depth>8, pixels must be UInt16 internally }
          format : avifRGBFormat;  { all channels are always full range }
          chromaUpsampling : avifChromaUpsampling;     { Defaults to AVIF_CHROMA_UPSAMPLING_AUTOMATIC: How to upsample non-4:4:4 UV (ignored for 444) when converting to RGB. }
    { Unused when converting to YUV. avifRGBImageSetDefaults() prefers quality over speed. }
    { Used for XRGB formats, treats formats containing alpha (such as ARGB) as if they were }
    { RGB, treating the alpha bits as if they were all 1. }
          ignoreAlpha : avifBool;
          alphaPremultiplied : avifBool;     { indicates if RGB value is pre-multiplied by alpha. Default: false }
          pixels : PUInt8;
          rowBytes : UInt32;
        end;

      PavifRGBImage0_10_0 = ^avifRGBImage0_10_0;
      avifRGBImage0_10_0 = record
          width : UInt32;      { must match associated avifImage }
          height : UInt32;    { must match associated avifImage }
          depth : UInt32;    { legal depths [8, 10, 12, 16]. if depth>8, pixels must be UInt16 internally }
          format : avifRGBFormat;  { all channels are always full range }
          chromaUpsampling : avifChromaUpsampling;     { Defaults to AVIF_CHROMA_UPSAMPLING_AUTOMATIC: How to upsample non-4:4:4 UV (ignored for 444) when converting to RGB. }
    { Unused when converting to YUV. avifRGBImageSetDefaults() prefers quality over speed. }
    { Used for XRGB formats, treats formats containing alpha (such as ARGB) as if they were }
    { RGB, treating the alpha bits as if they were all 1. }
          ignoreAlpha : avifBool;
          alphaPremultiplied : avifBool;     { indicates if RGB value is pre-multiplied by alpha. Default: false }
          isFloat : avifBool;
          pixels : PUInt8;
          rowBytes : UInt32;
          padding: packed array[0..63] of byte;  // to prevent buffer overflow with future changes
        end;

      PavifRGBImage0_11_0 = ^avifRGBImage0_11_0;
      avifRGBImage0_11_0 = record
          width : UInt32;      { must match associated avifImage }
          height : UInt32;    { must match associated avifImage }
          depth : UInt32;    { legal depths [8, 10, 12, 16]. if depth>8, pixels must be UInt16 internally }
          format : avifRGBFormat;  { all channels are always full range }
          chromaUpsampling : avifChromaUpsampling; // How to upsample from 4:2:0 or 4:2:2 UV when converting to RGB (ignored for 4:4:4 and 4:0:0).
                                             // Ignored when converting to YUV. Defaults to AVIF_CHROMA_UPSAMPLING_AUTOMATIC.
          chromaDownsampling: avifChromaDownsampling; // How to downsample to 4:2:0 or 4:2:2 UV when converting from RGB (ignored for 4:4:4 and 4:0:0).
                                                 // Ignored when converting to RGB. Defaults to AVIF_CHROMA_DOWNSAMPLING_AUTOMATIC.
          avoidLibYUV: avifBool;   { If AVIF_FALSE and libyuv conversion between RGB and YUV (including upsampling or downsampling if any) }
                            { is available for the avifImage/avifRGBImage combination, then libyuv is used. Default is AVIF_FALSE. }
          ignoreAlpha : avifBool;  { Used for XRGB formats, treats formats containing alpha (such as ARGB) as if they were RGB, treating }
                                   { the alpha bits as if they were all 1. }
          alphaPremultiplied : avifBool;     { indicates if RGB value is pre-multiplied by alpha. Default: false }
          isFloat : avifBool;    { indicates if RGBA values are in half float (f16) format. Valid only when depth == 16. Default: false  }
          pixels : PUInt8;
          rowBytes : UInt32;
          padding: packed array[0..63] of byte;  // to prevent buffer overflow with future changes
        end;

    { Sets rgb->width, rgb->height, and rgb->depth to image->width, image->height, and image->depth. }
    { Sets rgb->pixels to NULL and rgb->rowBytes to 0. Sets the other fields of 'rgb' to default }
    { values. }

{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifRGBImageSetDefaults{$IFDEF LD}: procedure{$ENDIF}(rgb:PavifRGBImage;image:PavifImage);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifRGBImagePixelSize{$IFDEF LD}: function{$ENDIF}(rgb:PavifRGBImage):UInt32;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

    { Convenience functions. If you supply your own pixels/rowBytes, you do not need to use these. }
{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifRGBImageAllocatePixels{$IFDEF LD}: procedure{$ENDIF}(rgb:PavifRGBImage);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifRGBImageFreePixels{$IFDEF LD}: procedure{$ENDIF}(rgb:PavifRGBImage);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

    { The main conversion functions }

{$IFDEF LD}var{$ELSE}function{$ENDIF} avifImageRGBToYUV{$IFDEF LD}: function{$ENDIF}(image:PavifImage;rgb:PavifRGBImage):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifImageYUVToRGB{$IFDEF LD}: function{$ENDIF}(image:PavifImage;rgb:PavifRGBImage):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

    { Premultiply handling functions. }
    { (Un)premultiply is automatically done by the main conversion functions above, }
    { so usually you don't need to call these. They are there for convenience. }

{$IFDEF LD}var{$ELSE}function{$ENDIF} avifRGBImagePremultiplyAlpha{$IFDEF LD}: function{$ENDIF}(rgb:PavifRGBImage):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifRGBImageUnpremultiplyAlpha{$IFDEF LD}: function{$ENDIF}(rgb:PavifRGBImage):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

    { --------------------------------------------------------------------------- }
    { YUV Utils }

{$IFDEF LD}var{$ELSE}function{$ENDIF} avifFullToLimitedY{$IFDEF LD}: function{$ENDIF}(depth:integer;v:integer):integer;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifFullToLimitedUV{$IFDEF LD}: function{$ENDIF}(depth:integer;v:integer):integer;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifLimitedToFullY{$IFDEF LD}: function{$ENDIF}(depth:integer;v:integer):integer;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifLimitedToFullUV{$IFDEF LD}: function{$ENDIF}(depth:integer;v:integer):integer;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

    { --------------------------------------------------------------------------- }
    { Codec selection }
type
      PavifCodecChoice = ^avifCodecChoice;
      avifCodecChoice = (AVIF_CODEC_CHOICE_AUTO := 0,AVIF_CODEC_CHOICE_AOM,
        AVIF_CODEC_CHOICE_DAV1D,     { Decode only }
		AVIF_CODEC_CHOICE_LIBGAV1,     { Decode only }
        AVIF_CODEC_CHOICE_RAV1E,    { Encode only }
		AVIF_CODEC_CHOICE_SVT    { Encode only }
      );

      PavifCodecFlag = ^avifCodecFlag;
      avifCodecFlag = (
	  AVIF_CODEC_FLAG_CAN_DECODE := 1 shl 0,
	  AVIF_CODEC_FLAG_CAN_ENCODE := 1 shl 1
        );

      PavifCodecFlags = ^avifCodecFlags;
      avifCodecFlags = UInt32;
    { If this returns NULL, the codec choice/flag combination is unavailable }

{$IFDEF LD}var{$ELSE}function{$ENDIF} avifCodecName{$IFDEF LD}: function{$ENDIF}(choice:avifCodecChoice;requiredFlags:avifCodecFlags):PAnsiChar;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

{$IFDEF LD}var{$ELSE}function{$ENDIF} avifCodecChoiceFromName{$IFDEF LD}: function{$ENDIF}(name:PAnsiChar):avifCodecChoice;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
type
    { avifIO }
      PavifIO = ^avifIO;
      //avifIO = record
      //    {undefined structure}
      //  end;

    { Destroy must completely destroy all child structures *and* free the avifIO object itself. }
    { This function pointer is optional, however, if the avifIO object isn't intended to be owned by }
    { a libavif encoder/decoder. }

      avifIODestroyFunc = procedure (io:PavifIO);cdecl;
    { This function should return a block of memory that *must* remain valid until another read call to }
    { this avifIO struct is made (reusing a read buffer is acceptable/expected). }
    { }
    { * If offset exceeds the size of the content (past EOF), return AVIF_RESULT_IO_ERROR. }
    { * If offset is *exactly* at EOF, provide a 0-byte buffer and return AVIF_RESULT_OK. }
    { * If (offset+size) exceeds the contents' size, it must truncate the range to provide all }
    {   bytes from the offset to EOF. }
    { * If the range is unavailable yet (due to network conditions or any other reason), }
    {   return AVIF_RESULT_WAITING_ON_IO. }
    { * Otherwise, provide the range and return AVIF_RESULT_OK. }

      avifIOReadFunc = function (io:PavifIO; readFlags:UInt32; offset:UInt64; size:size_type; output:PavifROData):avifResult;cdecl;

      avifIOWriteFunc = function (io:PavifIO; writeFlags:UInt32; offset:UInt64; data:PUInt8; size:size_type):avifResult;cdecl;

//      PavifIO = ^avifIO;
      avifIO = record
          destroy : avifIODestroyFunc;
          read : avifIOReadFunc;
    { This is reserved for future use - but currently ignored. Set it to a null pointer. }		  
          write : avifIOWriteFunc;
    { If non-zero, this is a hint to internal structures of the max size offered by the content }
    { this avifIO structure is reading. If it is a static memory source, it should be the size of }
    { the memory buffer; if it is a file, it should be the file's size. If this information cannot }
    { be known (as it is streamed-in), set a reasonable upper boundary here (larger than the file }
    { can possibly be for your environment, but within your environment's memory constraints). This }
    { is used for sanity checks when allocating internal buffers to protect against }
    { malformed/malicious files. }		  
          sizeHint : UInt64;
    { If true, *all* memory regions returned from *all* calls to read are guaranteed to be }
    { persistent and exist for the lifetime of the avifIO object. If false, libavif will make }
    { in-memory copies of samples and metadata content, and a memory region returned from read must }
    { only persist until the next call to read. }		  
          persistent : avifBool;
    { The contents of this are defined by the avifIO implementation, and should be fully destroyed }
    { by the implementation of the associated destroy function, unless it isn't owned by the avifIO }
    { struct. It is not necessary to use this pointer in your implementation. }		  
          data : pointer;
        end;

{$IFDEF LD}var{$ELSE}function{$ENDIF} avifIOCreateMemoryReader{$IFDEF LD}: function{$ENDIF}(data:PByte;size:size_type): PavifIO;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifIOCreateFileReader{$IFDEF LD}: function{$ENDIF}(filename:PAnsiChar):PavifIO;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifIODestroy{$IFDEF LD}: procedure{$ENDIF}(io:PavifIO);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

    { --------------------------------------------------------------------------- }
    { avifDecoder }
    { Some encoders (including very old versions of avifenc) do not implement the AVIF standard }
    { perfectly, and thus create invalid files. However, these files are likely still recoverable / }
    { decodable, if it wasn't for the strict requirements imposed by libavif's decoder. These flags }
    { allow a user of avifDecoder to decide what level of strictness they want in their project. }

type

      PavifStrictFlag = ^avifStrictFlag;
      avifStrictFlag = (
    { Disables all strict checks. }	  
	  AVIF_STRICT_DISABLED := 0,
    { Requires the PixelInformationProperty ('pixi') be present in AV1 image items. libheif v1.11.0 }
    { or older does not add the 'pixi' item property to AV1 image items. If you need to decode AVIF }
    { images encoded by libheif v1.11.0 or older, be sure to disable this bit. (This issue has been }
    { corrected in libheif v1.12.0.) }
	  
	  AVIF_STRICT_PIXI_REQUIRED := 1 shl 0,
    { This demands that the values surfaced in the clap box are valid, determined by attempting to }
    { convert the clap box to a crop rect using avifCropRectConvertCleanApertureBox(). If this }
    { function returns AVIF_FALSE and this strict flag is set, the decode will fail. }
	  
        AVIF_STRICT_CLAP_VALID := 1 shl 1,
    { Requires the ImageSpatialExtentsProperty ('ispe') be present in alpha auxiliary image items. }
    { avif-serialize 0.7.3 or older does not add the 'ispe' item property to alpha auxiliary image }
    { items. If you need to decode AVIF images encoded by the cavif encoder with avif-serialize }
    { 0.7.3 or older, be sure to disable this bit. (This issue has been corrected in avif-serialize }
    { 0.7.4.) See https://github.com/kornelski/avif-serialize/issues/3 and }
    { https://crbug.com/1246678. }
		
		AVIF_STRICT_ALPHA_ISPE_REQUIRED := 1 shl 2,
    { Maximum strictness; enables all bits above. This is avifDecoder's default. }
		
        AVIF_STRICT_ENABLED := (AVIF_STRICT_PIXI_REQUIRED + AVIF_STRICT_CLAP_VALID) + AVIF_STRICT_ALPHA_ISPE_REQUIRED);

      PavifStrictFlags = ^avifStrictFlags;
      avifStrictFlags = UInt32;
    { Useful stats related to a read/write }

      PavifIOStats = ^avifIOStats;
      avifIOStats = record
          colorOBUSize : size_type;
          alphaOBUSize : size_type;
        end;
      PavifDecoderData = ^avifDecoderData;
      avifDecoderData = record
          {undefined structure}
        end;


      PavifDecoderSource = ^avifDecoderSource;
      avifDecoderSource = (
    { Honor the major brand signaled in the beginning of the file to pick between an AVIF sequence }
    { ('avis', tracks-based) or a single image ('avif', item-based). If the major brand is neither }
    { of these, prefer the AVIF sequence ('avis', tracks-based), if present. } 
	  AVIF_DECODER_SOURCE_AUTO := 0,
    { Use the primary item and the aux (alpha) item in the avif(s). }
    { This is where single-image avifs store their image. }
	  AVIF_DECODER_SOURCE_PRIMARY_ITEM,
    { Use the chunks inside primary/aux tracks in the moov block. }
    { This is where avifs image sequences store their images. }
	  
        AVIF_DECODER_SOURCE_TRACKS
    { Decode the thumbnail item. Currently unimplemented. }
    { AVIF_DECODER_SOURCE_THUMBNAIL_ITEM }
	);
	
    { Information about the timing of a single image in an image sequence }

      PavifImageTiming = ^avifImageTiming;
      avifImageTiming = record
          timescale : UInt64;     { timescale of the media (Hz) }
          pts : double;       { presentation timestamp in seconds (ptsInTimescales / timescale) }
          ptsInTimescales : UInt64;      { presentation timestamp in "timescales" }
          duration : double;        { in seconds (durationInTimescales / timescale) }
          durationInTimescales : UInt64;       { duration in "timescales" }
        end;
		

      PavifProgressiveState = ^avifProgressiveState;
      avifProgressiveState = (
    { The current AVIF/Source does not offer a progressive image. This will always be the state }
    { for an image sequence. }
	  AVIF_PROGRESSIVE_STATE_UNAVAILABLE := 0,
     { The current AVIF/Source offers a progressive image, but avifDecoder.allowProgressive is not }
    { enabled, so it will behave as if the image was not progressive and will simply decode the }
    { best version of this item. }     
	  AVIF_PROGRESSIVE_STATE_AVAILABLE,
    { The current AVIF/Source offers a progressive image, and avifDecoder.allowProgressive is true. }
    { In this state, avifDecoder.imageCount will be the count of all of the available progressive }
    { layers, and any specific layer can be decoded using avifDecoderNthImage() as if it was an }
    { image sequence, or simply using repeated calls to avifDecoderNextImage() to decode better and }
    { better versions of this image. }
	  
	  AVIF_PROGRESSIVE_STATE_ACTIVE
        );

{$IFDEF LD}var{$ELSE}function{$ENDIF} avifProgressiveStateToString{$IFDEF LD}: function{$ENDIF}( progressiveState:avifProgressiveState):PAnsiChar;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

type
      PavifDecoder = pointer;
      PavifDecoder0_8_4 = ^avifDecoder0_8_4;
      avifDecoder0_8_4 = record
    { Inputs ------------------------------------------------------------------------------------- }
    { Defaults to AVIF_CODEC_CHOICE_AUTO: Preference determined by order in availableCodecs table (avif.c) }
          codecChoice : avifCodecChoice;
    { Defaults to 1. -- NOTE: Please see the "Understanding maxThreads" comment block above }
          maxThreads : longint;
    { avifs can have multiple sets of images in them. This specifies which to decode. }
    { Set this via avifDecoderSetSource(). }
          requestedSource : avifDecoderSource;
    { Outputs ------------------------------------------------------------------------------------ }
          image : PavifImage;
    { Counts and timing for the current image in an image sequence. Uninteresting for single image files. }
          imageIndex : longint;      { 0-based }
          imageCount : longint;      { Always 1 for non-progressive, non-sequence AVIFs. }
          imageTiming : avifImageTiming;      { }
          timescale : UInt64;      { timescale of the media (Hz) }
          duration : double;      { in seconds (durationInTimescales / timescale) }
          durationInTimescales : UInt64;    { duration in "timescales" }
          alphaPresent : avifBool;
          ignoreExif : avifBool;
          ignoreXMP : avifBool;
          { stats from the most recent read, possibly 0s if reading an image sequence }
          ioStats : avifIOStats;
          { Use one of the avifDecoderSetIO*() functions to set this }
          io : PavifIO;
          { Internals used by the decoder }
          data : PavifDecoderData;
      end;

      PavifDecoder0_9_2 = ^avifDecoder0_9_2;
      avifDecoder0_9_2 = record
    { Inputs ------------------------------------------------------------------------------------- }
    { Defaults to AVIF_CODEC_CHOICE_AUTO: Preference determined by order in availableCodecs table (avif.c) }
          codecChoice : avifCodecChoice;
    { Defaults to 1. -- NOTE: Please see the "Understanding maxThreads" comment block above }
          maxThreads : longint;
    { avifs can have multiple sets of images in them. This specifies which to decode. }
    { Set this via avifDecoderSetSource(). }
          requestedSource : avifDecoderSource;
    { Outputs ------------------------------------------------------------------------------------ }
          image : PavifImage;
    { Counts and timing for the current image in an image sequence. Uninteresting for single image files. }
          imageIndex : longint;      { 0-based }
          imageCount : longint;      { Always 1 for non-progressive, non-sequence AVIFs. }
          imageTiming : avifImageTiming;      { }
          timescale : UInt64;      { timescale of the media (Hz) }
          duration : double;      { in seconds (durationInTimescales / timescale) }
          durationInTimescales : UInt64;    { duration in "timescales" }
          alphaPresent : avifBool;
          ignoreExif : avifBool;
          ignoreXMP : avifBool;
          imageCountLimit : UInt32;
          strictFlags : avifStrictFlags;
          { stats from the most recent read, possibly 0s if reading an image sequence }
          ioStats : avifIOStats;
          { Use one of the avifDecoderSetIO*() functions to set this }
          io : PavifIO;
          { Additional diagnostics (such as detailed error state) }
          diag : avifDiagnostics;
          { Internals used by the decoder }
          data : PavifDecoderData;
      end;

      PavifDecoder0_9_3 = ^avifDecoder0_9_3;
      avifDecoder0_9_3 = record
    { Inputs ------------------------------------------------------------------------------------- }
    { Defaults to AVIF_CODEC_CHOICE_AUTO: Preference determined by order in availableCodecs table (avif.c) }
          codecChoice : avifCodecChoice;
    { Defaults to 1. -- NOTE: Please see the "Understanding maxThreads" comment block above }
          maxThreads : longint;
    { avifs can have multiple sets of images in them. This specifies which to decode. }
    { Set this via avifDecoderSetSource(). }
          requestedSource : avifDecoderSource;
    { If this is true and a progressive AVIF is decoded, avifDecoder will behave as if the AVIF is }
    { an image sequence, in that it will set imageCount to the number of progressive frames }
    { available, and avifDecoderNextImage()/avifDecoderNthImage() will allow for specific layers }
    { of a progressive image to be decoded. To distinguish between a progressive AVIF and an AVIF }
    { image sequence, inspect avifDecoder.progressiveState. }
          allowProgressive : avifBool;
    { Enable any of these to avoid reading and surfacing specific data to the decoded avifImage. }
    { These can be useful if your avifIO implementation heavily uses AVIF_RESULT_WAITING_ON_IO for }
    { streaming data, as some of these payloads are (unfortunately) packed at the end of the file, }
    { which will cause avifDecoderParse() to return AVIF_RESULT_WAITING_ON_IO until it finds them. }
    { If you don't actually leverage this data, it is best to ignore it here. }
          ignoreExif : avifBool;
          ignoreXMP : avifBool;
    { This represents the maximum size of a image (in pixel count) that libavif and the underlying }
    { AV1 decoder should attempt to decode. It defaults to AVIF_DEFAULT_IMAGE_SIZE_LIMIT, and can be }
    { set to a smaller value. The value 0 is reserved. }
    { Note: Only some underlying AV1 codecs support a configurable size limit (such as dav1d). }
          imageSizeLimit : UInt32;
    { This provides an upper bound on how many images the decoder is willing to attempt to decode, }
    { to provide a bit of protection from malicious or malformed AVIFs citing millions upon }
    { millions of frames, only to be invalid later. The default is AVIF_DEFAULT_IMAGE_COUNT_LIMIT }
    { (see comment above), and setting this to 0 disables the limit. }
          imageCountLimit : UInt32;
    { Strict flags. Defaults to AVIF_STRICT_ENABLED. See avifStrictFlag definitions above. }
          strictFlags : avifStrictFlags;
    { Outputs ------------------------------------------------------------------------------------ }
          image : PavifImage;
    { Counts and timing for the current image in an image sequence. Uninteresting for single image files. }
          imageIndex : longint;      { 0-based }
          imageCount : longint;      { Always 1 for non-progressive, non-sequence AVIFs. }
          progressiveState : avifProgressiveState;      { See avifProgressiveState declaration }
          imageTiming : avifImageTiming;      { }
          timescale : UInt64;      { timescale of the media (Hz) }
          duration : double;      { in seconds (durationInTimescales / timescale) }
          durationInTimescales : UInt64;    { duration in "timescales" }
          alphaPresent : avifBool;
    { stats from the most recent read, possibly 0s if reading an image sequence }
          ioStats : avifIOStats;
    { Additional diagnostics (such as detailed error state) }
          diag : avifDiagnostics;
    { Use one of the avifDecoderSetIO*() functions to set this }
          io : PavifIO;
    { Internals used by the decoder }
          data : PavifDecoderData;
        end;

      PavifDecoder0_10_0 = ^avifDecoder0_10_0;
      avifDecoder0_10_0 = record
    { Inputs ------------------------------------------------------------------------------------- }
          codecChoice : avifCodecChoice;
          maxThreads : longint;
          requestedSource : avifDecoderSource;
          allowProgressive : avifBool;
    { If this is false, avifDecoderNextImage() will start decoding a frame only after there are     }
    { enough input bytes to decode all of that frame. If this is true, avifDecoder will decode each }
    { subimage or grid cell as soon as possible. The benefits are: grid images may be partially     }
    { displayed before being entirely available, and the overall decoding may finish earlier.       }
    { Must be set before calling avifDecoderNextImage() or avifDecoderNthImage(). }
    { WARNING: Experimental feature. }
          allowIncremental : avifBool;
          ignoreExif : avifBool;
          ignoreXMP : avifBool;
          imageSizeLimit : UInt32;
          imageCountLimit : UInt32;
    { Strict flags. Defaults to AVIF_STRICT_ENABLED. See avifStrictFlag definitions above. }
          strictFlags : avifStrictFlags;
    { Outputs ------------------------------------------------------------------------------------ }
          image : PavifImage;
    { Counts and timing for the current image in an image sequence. Uninteresting for single image files. }
          imageIndex : longint;      { 0-based }
          imageCount : longint;      { Always 1 for non-progressive, non-sequence AVIFs. }
          progressiveState : avifProgressiveState;      { See avifProgressiveState declaration }
          imageTiming : avifImageTiming;      { }
          timescale : UInt64;      { timescale of the media (Hz) }
          duration : double;      { in seconds (durationInTimescales / timescale) }
          durationInTimescales : UInt64;    { duration in "timescales" }
          alphaPresent : avifBool;
    { stats from the most recent read, possibly 0s if reading an image sequence }
          ioStats : avifIOStats;
    { Additional diagnostics (such as detailed error state) }
          diag : avifDiagnostics;
    { Use one of the avifDecoderSetIO*() functions to set this }
          io : PavifIO;
    { Internals used by the decoder }
          data : PavifDecoderData;
        end;

      PavifDecoder0_11_0 = ^avifDecoder0_11_0;
      avifDecoder0_11_0 = record
    { Inputs ------------------------------------------------------------------------------------- }
          codecChoice : avifCodecChoice;
          maxThreads : longint;
          requestedSource : avifDecoderSource;
          allowProgressive : avifBool;
    { If this is false, avifDecoderNextImage() will start decoding a frame only after there are     }
    { enough input bytes to decode all of that frame. If this is true, avifDecoder will decode each }
    { subimage or grid cell as soon as possible. The benefits are: grid images may be partially     }
    { displayed before being entirely available, and the overall decoding may finish earlier.       }
    { Must be set before calling avifDecoderNextImage() or avifDecoderNthImage(). }
    { WARNING: Experimental feature. }
          allowIncremental : avifBool;
          ignoreExif : avifBool;
          ignoreXMP : avifBool;
          // This represents the maximum size of an image (in pixel count) that libavif and the underlying
          // AV1 decoder should attempt to decode. It defaults to AVIF_DEFAULT_IMAGE_SIZE_LIMIT, and can
          // be set to a smaller value. The value 0 is reserved.
          // Note: Only some underlying AV1 codecs support a configurable size limit (such as dav1d).
          imageSizeLimit : UInt32;
          // This represents the maximum dimension of an image (width or height) that libavif should
          // attempt to decode. It defaults to AVIF_DEFAULT_IMAGE_DIMENSION_LIMIT. Set it to 0 to ignore
          // the limit.
          imageDimensionLimit: UInt32;
          // This provides an upper bound on how many images the decoder is willing to attempt to decode,
          // to provide a bit of protection from malicious or malformed AVIFs citing millions upon
          // millions of frames, only to be invalid later. The default is AVIF_DEFAULT_IMAGE_COUNT_LIMIT
          // (see comment above), and setting this to 0 disables the limit.
          imageCountLimit : UInt32;
    { Strict flags. Defaults to AVIF_STRICT_ENABLED. See avifStrictFlag definitions above. }
          strictFlags : avifStrictFlags;
    { Outputs ------------------------------------------------------------------------------------ }
          image : PavifImage;
    { Counts and timing for the current image in an image sequence. Uninteresting for single image files. }
          imageIndex : longint;      { 0-based }
          imageCount : longint;      { Always 1 for non-progressive, non-sequence AVIFs. }
          progressiveState : avifProgressiveState;      { See avifProgressiveState declaration }
          imageTiming : avifImageTiming;      { }
          timescale : UInt64;      { timescale of the media (Hz) }
          duration : double;      { in seconds (durationInTimescales / timescale) }
          durationInTimescales : UInt64;    { duration in "timescales" }
          alphaPresent : avifBool;
    { stats from the most recent read, possibly 0s if reading an image sequence }
          ioStats : avifIOStats;
    { Additional diagnostics (such as detailed error state) }
          diag : avifDiagnostics;
    { Use one of the avifDecoderSetIO*() functions to set this }
          io : PavifIO;
    { Internals used by the decoder }
          data : PavifDecoderData;
        end;

{$IFDEF LD}var{$ELSE}function{$ENDIF} avifDecoderCreate{$IFDEF LD}: function{$ENDIF}:PavifDecoder;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifDecoderDestroy{$IFDEF LD}: procedure{$ENDIF}(decoder:PavifDecoder);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

    { Simple interfaces to decode a single image, independent of the decoder afterwards (decoder may be destroyed). }

{$IFDEF LD}var{$ELSE}function{$ENDIF} avifDecoderRead{$IFDEF LD}: function{$ENDIF}(decoder:PavifDecoder;image:PavifImage):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF} // call avifDecoderSetIO*() first
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifDecoderReadMemory{$IFDEF LD}: function{$ENDIF}(decoder:PavifDecoder; image:PavifImage;data:PByte;size:size_type):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifDecoderReadFile{$IFDEF LD}: function{$ENDIF}(decoder:PavifDecoder;image:PavifImage;filename:PAnsiChar):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

    { Multi-function alternative to avifDecoderRead() for image sequences and gaining direct access }
    { to the decoder's YUV buffers (for performance's sake). Data passed into avifDecoderParse() is NOT }
    { copied, so it must continue to exist until the decoder is destroyed. }
    { }
    { Usage / function call order is: }
    { * avifDecoderCreate() }
    { * avifDecoderSetSource() - optional, the default (AVIF_DECODER_SOURCE_AUTO) is usually sufficient }
    { * avifDecoderSetIO*() }
    { * avifDecoderParse() }
    { * avifDecoderNextImage() - in a loop, using decoder->image after each successful call }
    { * avifDecoderDestroy() }
    { }
    { NOTE: Until avifDecoderParse() returns AVIF_RESULT_OK, no data in avifDecoder should }
    {       be considered valid, and no queries (such as Keyframe/Timing/MaxExtent) should be made. }
    { }
    { You can use avifDecoderReset() any time after a successful call to avifDecoderParse() }
    { to reset the internal decoder back to before the first frame. Calling either }
    { avifDecoderSetSource() or avifDecoderParse() will automatically Reset the decoder. }
    { }
    { avifDecoderSetSource() allows you not only to choose whether to parse tracks or }
    { items in a file containing both, but switch between sources without having to }
    { Parse again. Normally AVIF_DECODER_SOURCE_AUTO is enough for the common path. }

{$IFDEF LD}var{$ELSE}function{$ENDIF} avifDecoderSetSource{$IFDEF LD}: function{$ENDIF}(decoder:PavifDecoder;source:avifDecoderSource):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

    { Note: When avifDecoderSetIO() is called, whether 'decoder' takes ownership of 'io' depends on }
    { whether io->destroy is set. avifDecoderDestroy(decoder) calls avifIODestroy(io), which calls }
    { io->destroy(io) if io->destroy is set. Therefore, if io->destroy is not set, then }
    { avifDecoderDestroy(decoder) has no effects on 'io'. }

{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifDecoderSetIO{$IFDEF LD}: procedure{$ENDIF}(decoder:PavifDecoder;io:PavifIO);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifDecoderSetIOMemory{$IFDEF LD}: function{$ENDIF}(decoder:PavifDecoder; data:PByte;size:size_type):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifDecoderSetIOFile{$IFDEF LD}: function{$ENDIF}(decoder:PavifDecoder;filename:PAnsiChar):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifDecoderParse{$IFDEF LD}: function{$ENDIF}(decoder:PavifDecoder):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifDecoderNextImage{$IFDEF LD}: function{$ENDIF}(decoder:PavifDecoder):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifDecoderNthImage{$IFDEF LD}: function{$ENDIF}(decoder:PavifDecoder;frameIndex: UInt32):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifDecoderReset{$IFDEF LD}: function{$ENDIF}(decoder:PavifDecoder):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

    { Keyframe information }
    { frameIndex - 0-based, matching avifDecoder->imageIndex, bound by avifDecoder->imageCount }
    { "nearest" keyframe means the keyframe prior to this frame index (returns frameIndex if it is a keyframe) }
    { These functions may be used after a successful call (AVIF_RESULT_OK) to avifDecoderParse(). }

{$IFDEF LD}var{$ELSE}function{$ENDIF} avifDecoderIsKeyframe{$IFDEF LD}: function{$ENDIF}(decoder:PavifDecoder; frameindex:UInt32):avifBool;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifDecoderNearestKeyframe{$IFDEF LD}: function{$ENDIF}(decoder:PavifDecoder; frameIndex:UInt32):UInt32;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
 
    { Timing helper - This does not change the current image or invoke the codec (safe to call repeatedly) }
    { This function may be used after a successful call (AVIF_RESULT_OK) to avifDecoderParse(). }
 
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifDecoderNthImageTiming{$IFDEF LD}: function{$ENDIF}(decoder:PavifDecoder; frameIndex:UInt32;outTiming:PavifImageTiming):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

    { --------------------------------------------------------------------------- }
    { avifExtent }
type
      PavifExtent = ^avifExtent;
      avifExtent = record
          offset : UInt64;
          size : size_type;
        end;
    { Streaming data helper - Use this to calculate the maximal AVIF data extent encompassing all AV1 }
    { sample data needed to decode the Nth image. The offset will be the earliest offset of all }
    { required AV1 extents for this frame, and the size will create a range including the last byte of }
    { the last AV1 sample needed. Note that this extent may include non-sample data, as a frame's }
    { sample data may be broken into multiple extents and interleaved with other data, or in }
    { non-sequential order. This extent will also encompass all AV1 samples that this frame's sample }
    { depends on to decode (such as samples for reference frames), from the nearest keyframe up to this }
    { Nth frame. }
    { }
    { If avifDecoderNthImageMaxExtent() returns AVIF_RESULT_OK and the extent's size is 0 bytes, this }
    { signals that libavif doesn't expect to call avifIO's Read for this frame's decode. This happens if }
    { data for this frame was read as a part of avifDecoderParse() (typically in an idat box inside of }
    { a meta box). }
    { }
    { This function may be used after a successful call (AVIF_RESULT_OK) to avifDecoderParse(). }

{$IFDEF LD}var{$ELSE}function{$ENDIF} avifDecoderNthImageMaxExtent{$IFDEF LD}: function{$ENDIF}(decoder:PavifDecoder; frameIndex:UInt32;outExtend:PavifExtent):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
type
    { --------------------------------------------------------------------------- }
    { avifEncoder }
      PavifEncoderData = ^avifEncoderData;
      avifEncoderData = record
          {undefined structure}
        end;

      PavifCodecSpecificOptions = ^avifCodecSpecificOptions;
      avifCodecSpecificOptions = record
          {undefined structure}
        end;

    { Notes: }
    { * If avifEncoderWrite() returns AVIF_RESULT_OK, output must be freed with avifRWDataFree() }
    { * If (maxThreads < 2), multithreading is disabled }
    {   * NOTE: Please see the "Understanding maxThreads" comment block above }
    { * Quality range: [AVIF_QUANTIZER_BEST_QUALITY - AVIF_QUANTIZER_WORST_QUALITY] }
    { * To enable tiling, set tileRowsLog2 > 0 and/or tileColsLog2 > 0. }
    {   Tiling values range [0-6], where the value indicates a request for 2^n tiles in that dimension. }
    {   If autoTiling is set to AVIF_TRUE, libavif ignores tileRowsLog2 and tileColsLog2 and }
    {   automatically chooses suitable tiling values. }
    { * Speed range: [AVIF_SPEED_SLOWEST - AVIF_SPEED_FASTEST]. Slower should make for a better quality }
    {   image in less bytes. AVIF_SPEED_DEFAULT means "Leave the AV1 codec to its default speed settings"./ }
    {   If avifEncoder uses rav1e, the speed value is directly passed through (0-10). If libaom is used, }
    {   a combination of settings are tweaked to simulate this speed range. }
    { * Some encoder settings can be changed after encoding starts. Changes will take effect in the next }
    {   call to avifEncoderAddImage(). }
      PavifEncoder = ^avifEncoder;
      avifEncoder = record
    { Defaults to AVIF_CODEC_CHOICE_AUTO: Preference determined by order in availableCodecs table (avif.c) }	  
          codecChoice : avifCodecChoice;
    { settings (see Notes above) }		  
          maxThreads : longint;
          minQuantizer : longint;
          maxQuantizer : longint;
          minQuantizerAlpha : longint;
          maxQuantizerAlpha : longint;
          tileRowsLog2 : longint;
          tileColsLog2 : longint;
          speed : longint;
          keyframeInterval : longint;       { How many frames between automatic forced keyframes; 0 to disable (default). }
          timescale : UInt64;      { timescale of the media (Hz) }
    { stats from the most recent write }		  
          ioStats : avifIOStats;
    { Additional diagnostics (such as detailed error state) }		  
          diag : avifDiagnostics;
    { Internals used by the encoder }		  
          data : PavifEncoderData;
          csOptions : PavifCodecSpecificOptions;
        end;

      PavifEncoder0_11_0 = ^avifEncoder0_11_0;
      avifEncoder0_11_0 = record
    { Defaults to AVIF_CODEC_CHOICE_AUTO: Preference determined by order in availableCodecs table (avif.c) }
          codecChoice : avifCodecChoice;
    { settings (see Notes above) }
          maxThreads : longint;
          speed : longint;
          keyframeInterval : longint;       { How many frames between automatic forced keyframes; 0 to disable (default). }
          timescale : UInt64;      { timescale of the media (Hz) }
          // changeable encoder settings
          minQuantizer : longint;
          maxQuantizer : longint;
          minQuantizerAlpha : longint;
          maxQuantizerAlpha : longint;
          tileRowsLog2 : longint;
          tileColsLog2 : longint;
          autoTiling : avifBool;
    { stats from the most recent write }
          ioStats : avifIOStats;
    { Additional diagnostics (such as detailed error state) }
          diag : avifDiagnostics;
    { Internals used by the encoder }
          data : PavifEncoderData;
          csOptions : PavifCodecSpecificOptions;
        end;


{$IFDEF LD}var{$ELSE}function{$ENDIF} avifEncoderCreate{$IFDEF LD}: function{$ENDIF}:PavifEncoder;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifEncoderWrite{$IFDEF LD}: function{$ENDIF}(encoder:PavifEncoder;image:PavifImage;output:PavifRWData):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifEncoderDestroy{$IFDEF LD}: procedure{$ENDIF}(encoder:PavifEncoder);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
type
    { Force this frame to be a keyframe (sync frame). }
    { Use this flag when encoding a single image. Signals "still_picture" to AV1 encoders, which }
    { tweaks various compression rules. This is enabled automatically when using the }
    { avifEncoderWrite() single-image encode path. }
      PavifAddImageFlag = ^avifAddImageFlag;
      avifAddImageFlag = (AVIF_ADD_IMAGE_FLAG_NONE := 0,AVIF_ADD_IMAGE_FLAG_FORCE_KEYFRAME := 1 shl 0,
        AVIF_ADD_IMAGE_FLAG_SINGLE := 1 shl 1);

      PavifAddImageFlags = ^avifAddImageFlags;
      avifAddImageFlags = UInt32;
{ Multi-function alternative to avifEncoderWrite() for image sequences. }
{ }
{ Usage / function call order is: }
{ * avifEncoderCreate() }
{ * Set encoder->timescale (Hz) correctly }
{ * avifEncoderAddImage() ... [repeatedly; at least once] }
{   OR }
{ * avifEncoderAddImageGrid() [exactly once, AVIF_ADD_IMAGE_FLAG_SINGLE is assumed] }
{ * avifEncoderFinish() }
{ * avifEncoderDestroy() }

{ durationInTimescales is ignored if AVIF_ADD_IMAGE_FLAG_SINGLE is set in addImageFlags.}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifEncoderAddImage{$IFDEF LD}: function{$ENDIF}(encoder:PavifEncoder; image:PavifImage;durationInTimescales:UInt64;addImageFlags:avifAddImageFlags):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifEncoderAddImageGrid{$IFDEF LD}: function{$ENDIF}(encoder:PavifEncoder;
                                            gridCols:UInt32;
                                            gridRows:UInt32;
                                            cellImages:PPavifImage;
                                            addImageFlags:avifAddImageFlags):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifEncoderFinish{$IFDEF LD}: function{$ENDIF}(encoder:PavifEncoder; output:PavifRWData):avifResult;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

{ Codec-specific, optional "advanced" tuning settings, in the form of string key/value pairs, }
{ to be consumed by the codec in the next avifEncoderAddImage() call. }
{ See the codec documentation to know if a setting is persistent or applied only to the next frame. }
{ key must be non-NULL, but passing a NULL value will delete the pending key, if it exists. }
{ Setting an incorrect or unknown option for the current codec will cause errors of type }
{ AVIF_RESULT_INVALID_CODEC_SPECIFIC_OPTION from avifEncoderWrite() or avifEncoderAddImage(). }
{$IFDEF LD}var{$ELSE}procedure{$ENDIF} avifEncoderSetCodecSpecificOption{$IFDEF LD}: procedure{$ENDIF}(encoder:PavifEncoder;key:PAnsiChar;value:PAnsiChar);cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

{ Helpers }
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifImageUsesU16{$IFDEF LD}: function{$ENDIF}(image:PavifImage):avifBool;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}
{ Returns AVIF_TRUE if input begins with a valid FileTypeBox (ftyp) that supports }
{ either the brand 'avif' or 'avis' (or both), without performing any allocations. }
{$IFDEF LD}var{$ELSE}function{$ENDIF} avifPeekCompatibleFileType{$IFDEF LD}: function{$ENDIF}(input:PavifROData):avifBool ;cdecl;{$IFNDEF LD}external LibAvifFilename;{$ENDIF}

function LibAvifLoaded : boolean;
Function LibAvifLoad(const libfilename:string = ''): boolean; // load the lib
Procedure LibAvifUnload; // unload and frees the lib from memory : do not forget to call it before close application.

implementation
{$IFDEF LOAD_DYNAMICALLY}

uses
    SysUtils, Classes, DynLibs{$ifdef linux}, linuxlib{$endif}{$ifdef darwin}, darwinlib{$endif};
var
  LibAvifHandle: TLibHandle = dynlibs.NilHandle; // this will hold our handle for the lib; it functions nicely as a mutli-lib prevention unit as well...
  LibAvifRefCount : LongWord = 0;  // Reference counter


function LibAvifLoaded: boolean;
begin
  Result := (LibAvifHandle <> dynlibs.NilHandle);
end;

procedure LibAvifRetreiveVersion;
var version: TStringList;
begin
  version := TStringList.Create;
  version.Delimiter:= '.';
  version.DelimitedText:= string(avifVersion());
  AVIF_VERSION_MAJOR := StrToInt(version[0]);
  if version.Count >= 2 then AVIF_VERSION_MINOR := StrToInt(version[1]) else
    AVIF_VERSION_MINOR := 0;
  if version.Count >= 3 then AVIF_VERSION_PATCH := StrToInt(version[2]) else
    AVIF_VERSION_PATCH := 0;
  AVIF_VERSION := AVIF_VERSION_MAJOR * 1000000 + AVIF_VERSION_MINOR * 10000 + AVIF_VERSION_PATCH * 100;
  version.Free;
end;

Function LibAvifLoad (const libfilename:string) :boolean;
var
  thelib: string;
begin
  Result := False;
  if LibAvifHandle<>0 then
  begin
   Inc(LibAvifRefCount);
   result:=true {is it already there ?}
  end else
  begin {go & load the library}
    if libfilename <> '' then
    begin
      thelib := libfilename;
      if Pos(DirectorySeparator, thelib)=0 then
        thelib := ExtractFilePath(ParamStr(0)) + DirectorySeparator + thelib;
      LibAvifHandle := DynLibs.SafeLoadLibrary(libfilename); // obtain the handle we want
    end else
    begin
      {$ifdef linux}thelib := FindLinuxLibrary(LibAvifFilename);{$else}
      {$ifdef darwin}thelib := FindDarwinLibrary(LibAvifFilename);{$else}
      thelib := ExtractFilePath(ParamStr(0)) + DirectorySeparator + LibAvifFilename;
      {$endif}{$endif}
      if thelib <> '' then
        LibAvifHandle := DynLibs.SafeLoadLibrary(thelib); // obtain the handle we want
    end;
    if LibAvifHandle <> DynLibs.NilHandle then
    begin {now we tie the functions to the VARs from above}
      Pointer(avifAlloc):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifAlloc'));
      Pointer(avifCleanApertureBoxConvertCropRect):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifCleanApertureBoxConvertCropRect'));
      Pointer(avifCodecChoiceFromName):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifCodecChoiceFromName'));
      Pointer(avifCodecName):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifCodecName'));
      Pointer(avifCodecVersions):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifCodecVersions'));
      Pointer(avifColorPrimariesFind):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifColorPrimariesFind'));
      Pointer(avifColorPrimariesGetValues):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifColorPrimariesGetValues'));
      Pointer(avifCropRectConvertCleanApertureBox):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifCropRectConvertCleanApertureBox'));
      Pointer(avifDecoderCreate):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderCreate'));
      Pointer(avifDecoderDestroy):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderDestroy'));
      Pointer(avifDecoderIsKeyframe):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderIsKeyframe'));
      Pointer(avifDecoderNearestKeyframe):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderNearestKeyframe'));
      Pointer(avifDecoderNextImage):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderNextImage'));
      Pointer(avifDecoderNthImage):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderNthImage'));
      Pointer(avifDecoderNthImageMaxExtent):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderNthImageMaxExtent'));
      Pointer(avifDecoderNthImageTiming):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderNthImageTiming'));
      Pointer(avifDecoderParse):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderParse'));
      Pointer(avifDecoderRead):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderRead'));
      Pointer(avifDecoderReadFile):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderReadFile'));
      Pointer(avifDecoderReadMemory):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderReadMemory'));
      Pointer(avifDecoderReset):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderReset'));
      Pointer(avifDecoderSetIO):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderSetIO'));
      Pointer(avifDecoderSetIOFile):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderSetIOFile'));
      Pointer(avifDecoderSetIOMemory):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderSetIOMemory'));
      Pointer(avifDecoderSetSource):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDecoderSetSource'));
      Pointer(avifDiagnosticsClearError):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifDiagnosticsClearError'));
      Pointer(avifEncoderAddImage):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifEncoderAddImage'));
      Pointer(avifEncoderAddImageGrid):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifEncoderAddImageGrid'));
      Pointer(avifEncoderCreate):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifEncoderCreate'));
      Pointer(avifEncoderDestroy):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifEncoderDestroy'));
      Pointer(avifEncoderFinish):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifEncoderFinish'));
      Pointer(avifEncoderSetCodecSpecificOption):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifEncoderSetCodecSpecificOption'));
      Pointer(avifEncoderWrite):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifEncoderWrite'));
      Pointer(avifFree):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifFree'));
      Pointer(avifFullToLimitedUV):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifFullToLimitedUV'));
      Pointer(avifFullToLimitedY):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifFullToLimitedY'));
      Pointer(avifGetPixelFormatInfo):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifGetPixelFormatInfo'));
      Pointer(avifImageAllocatePlanes):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifImageAllocatePlanes'));
      Pointer(avifImageCopy):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifImageCopy'));
      Pointer(avifImageCreate):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifImageCreate'));
      Pointer(avifImageCreateEmpty):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifImageCreateEmpty'));
      Pointer(avifImageDestroy):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifImageDestroy'));
      Pointer(avifImageFreePlanes):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifImageFreePlanes'));
      Pointer(avifImageRGBToYUV):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifImageRGBToYUV'));
      Pointer(avifImageSetMetadataExif):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifImageSetMetadataExif'));
      Pointer(avifImageSetMetadataXMP):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifImageSetMetadataXMP'));
      Pointer(avifImageSetProfileICC):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifImageSetProfileICC'));
      Pointer(avifImageStealPlanes):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifImageStealPlanes'));
      Pointer(avifImageUsesU16):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifImageUsesU16'));
      Pointer(avifImageYUVToRGB):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifImageYUVToRGB'));
      Pointer(avifIOCreateFileReader):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifIOCreateFileReader'));
      Pointer(avifIOCreateMemoryReader):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifIOCreateMemoryReader'));
      Pointer(avifIODestroy):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifIODestroy'));
      Pointer(avifLibYUVVersion):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifLibYUVVersion'));
      Pointer(avifLimitedToFullUV):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifLimitedToFullUV'));
      Pointer(avifLimitedToFullY):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifLimitedToFullY'));
      Pointer(avifPeekCompatibleFileType):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifPeekCompatibleFileType'));
      Pointer(avifPixelFormatToString):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifPixelFormatToString'));
      Pointer(avifProgressiveStateToString):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifProgressiveStateToString'));
      Pointer(avifResultToString):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifResultToString'));
      Pointer(avifRGBFormatChannelCount):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifRGBFormatChannelCount'));
      Pointer(avifRGBFormatHasAlpha):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifRGBFormatHasAlpha'));
      Pointer(avifRGBImageAllocatePixels):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifRGBImageAllocatePixels'));
      Pointer(avifRGBImageFreePixels):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifRGBImageFreePixels'));
      Pointer(avifRGBImagePixelSize):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifRGBImagePixelSize'));
      Pointer(avifRGBImagePremultiplyAlpha):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifRGBImagePremultiplyAlpha'));
      Pointer(avifRGBImageSetDefaults):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifRGBImageSetDefaults'));
      Pointer(avifRGBImageUnpremultiplyAlpha):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifRGBImageUnpremultiplyAlpha'));
      Pointer(avifRWDataFree):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifRWDataFree'));
      Pointer(avifRWDataRealloc):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifRWDataRealloc'));
      Pointer(avifRWDataSet):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifRWDataSet'));
      Pointer(avifVersion):=DynLibs.GetProcedureAddress(LibAvifHandle,PAnsiChar('avifVersion'));
      LibAvifRetreiveVersion;
    end;
    Result := LibAvifLoaded;
    LibAvifRefCount:=1;
  end;
end;

Procedure LibAvifUnload;
begin
  // < Reference counting
  if LibAvifRefCount > 0 then
    dec(LibAvifRefCount);
  if LibAvifRefCount > 0 then
    exit;
  // >
  if LibAvifLoaded then
  begin
    DynLibs.UnloadLibrary(LibAvifHandle);
    LibAvifHandle:=DynLibs.NilHandle;
  end;
end;
{$ELSE}
function LibAvifLoaded: boolean;
begin
  result:=true;
end;
function LibAvifLoad (const libfilename:string) :boolean;
begin
  //do nothing
end;

procedure LibAvifUnload;
begin
  //do nothing
end;
{$ENDIF}

end.
