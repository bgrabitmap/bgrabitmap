unit libwebp;

// Copyright 2010 Google Inc.
//
// This code is licensed under the same terms as WebM:
//  Software License Agreement:  http://www.webmproject.org/license/software/
//  Additional IP Rights Grant:  http://www.webmproject.org/license/additional/
//
//  Delphi API by Henri Gourvest <hgourvest@gmail.com>
// -----------------------------------------------------------------------------
// This is the dynamic loader version of libwebp.pas by fredvs

{$ALIGN ON}
{$MINENUMSIZE 4}

interface

uses
  dynlibs;

var
  LibWebPFilename : string =
  {$if defined(Win32)}
    'libwebp32.dll'
  {$elseif defined(Win64)}
    'libwebp64.dll'
  {$elseif defined(Darwin)}
    'libwebp.6.dylib'
  {$elseif defined(Unix)}
    'libwebp.so.6'
  {$else}
    ''
  {$endif};

//-----------------------------------------------------------------------------

type
// Output colorspaces
  WEBP_CSP_MODE = (
    MODE_RGB = 0,
    MODE_RGBA = 1,
    MODE_BGR = 2,
    MODE_BGRA = 3,
    MODE_YUV = 4);

// Enumeration of the status codes
  TVP8StatusCode = (
    VP8_STATUS_OK = 0,
    VP8_STATUS_OUT_OF_MEMORY,
    VP8_STATUS_INVALID_PARAM,
    VP8_STATUS_BITSTREAM_ERROR,
    VP8_STATUS_UNSUPPORTED_FEATURE,
    VP8_STATUS_SUSPENDED,
    VP8_STATUS_USER_ABORT,
    VP8_STATUS_NOT_ENOUGH_DATA
  );

  TDecState = (
    STATE_HEADER = 0,
    STATE_PARTS0 = 1,
    STATE_DATA = 2,
    STATE_DONE = 3,
    STATE_ERROR = 4);

  // Decoding output parameters.
  PWebPDecParams = ^TWebPDecParams;
  TWebPDecParams = record
    output: PByte;              // rgb(a) or luma
    u, v: PByte;                // chroma u/v
    top_y, top_u, top_v: PByte; // cache for the fancy upscaler
    stride: Integer;            // rgb(a) stride or luma stride
    u_stride: Integer;          // chroma-u stride
    v_stride: Integer;          // chroma-v stride
    mode: WEBP_CSP_MODE;        // rgb(a) or yuv
    last_y: Integer;            // coordinate of the line that was last output
    output_size: Integer;       // size of 'output' buffer
    output_u_size: Integer;     // size of 'u' buffer
    output_v_size: Integer;     // size of 'v' buffer
    external_buffer: Integer;   // If true, the output buffers are externally owned
  end;

  PWebPIDecoder = ^TWebPIDecoder;
  TWebPIDecoder = record
    state_: TDecState;         // current decoding state
    w_, h_: integer;           // width and height
    params_: TWebPDecParams;   // Params to store output info
    dec_: Pointer;
  end;

  // Input / Output
  PVP8Io = ^VP8Io;
  VP8Io = record
    // set by VP8GetHeaders()
    width, height: Integer;    // picture dimensions, in pixels

    // set before calling put()
    mb_y: Integer;                  // position of the current rows (in pixels)
    mb_h: Integer;                  // number of rows in the sample
    y, u, v: PByte;                 // rows to copy (in yuv420 format)
    y_stride: Integer;              // row stride for luma
    uv_stride: Integer;             // row stride for chroma

    opaque: Pointer;              // user data

    // called when fresh samples are available. Currently, samples are in
    // YUV420 format, and can be up to width x 24 in size (depending on the
    // in-loop filtering level, e.g.). Should return false in case of error
    // or abort request.
    put: function(const io: PVP8Io): Integer; cdecl;

    // called just before starting to decode the blocks.
    // Should returns 0 in case of error.
    setup: function(io: PVP8Io): Integer; cdecl;

    // called just after block decoding is finished (or when an error occurred).
    teardown: procedure(const io: PVP8Io); cdecl;

    // this is a recommendation for the user-side yuv->rgb converter. This flag
    // is set when calling setup() hook and can be overwritten by it. It then
    // can be taken into consideration during the put() method.
    fancy_upscaling: Integer;

    // Input buffer.
    data_size: LongWord;
    data: PByte;

    // If true, in-loop filtering will not be performed even if present in the
    // bitstream. Switching off filtering may speed up decoding at the expense
    // of more visible blocking. Note that output will also be non-compliant
    // with the VP8 specifications.
    bypass_filtering: Integer;
  end;

  // Main decoding object. This is an opaque structure.
  PVP8Decoder = ^VP8Decoder;
  VP8Decoder = record end;

//-----------------------------------------------------------------------------
// Coding parameters

  PWebPConfig = ^TWebPConfig;
  TWebPConfig = record
    quality: Single;            // between 0 (smallest file) and 100 (biggest)
    target_size: Integer;       // if non-zero, set the desired target size in bytes.
                                // Takes precedence over the 'compression' parameter.
    target_PSNR: Single;        // if non-zero, specifies the minimal distortion to
                                // try to achieve. Takes precedence over target_size.
    method: Integer;            // quality/speed trade-off (0=fast, 6=slower-better)
    segments: Integer;          // maximum number of segments to use, in [1..4]
    sns_strength: Integer;      // Spatial Noise Shaping. 0=off, 100=maximum.
    filter_strength: Integer;   // range: [0 = off .. 100 = strongest]
    filter_sharpness: Integer;  // range: [0 = off .. 7 = least sharp]
    filter_type: Integer;       // filtering type: 0 = simple, 1 = strong
                                // (only used if filter_strength > 0 or autofilter > 0)
    autofilter: Integer;        // Auto adjust filter's strength [0 = off, 1 = on]
    pass: Integer;              // number of entropy-analysis passes (in [1..10]).

    show_compressed: Integer;   // if true, export the compressed picture back.
                                // In-loop filtering is not applied.
    preprocessing: Integer;     // preprocessing filter (0=none, 1=segment-smooth)
    partitions: Integer;        // log2(number of token partitions) in [0..3]
                                // Default is set to 0 for easier progressive decoding.
  end;

// Enumerate some predefined settings for WebPConfig, depending on the type
// of source picture. These presets are used when calling WebPConfigPreset().
  TWebPPreset = (
    WEBP_PRESET_DEFAULT = 0,  // default preset.
    WEBP_PRESET_PICTURE,      // digital picture, like portrait, inner shot
    WEBP_PRESET_PHOTO,        // outdoor photograph, with natural lighting
    WEBP_PRESET_DRAWING,      // hand or line drawing, with high-contrast details
    WEBP_PRESET_ICON,         // small-sized colorful images
    WEBP_PRESET_TEXT          // text-like
  );

  PWebPPicture = ^TWebPPicture;
  //TWebPPicture = record end; // main structure for I/O

  // non-essential structure for storing auxilliary statistics
  PWebPAuxStats = ^TWebPAuxStats;
  TWebPAuxStats = record
    PSNR: array[0..3] of Single;                   // peak-signal-to-noise ratio for Y/U/V/All
    coded_size: Integer;                           // final size
    block_count: array[0..2] of Integer;           // number of intra4/intra16/skipped macroblocks
    header_bytes: array[0..1] of Integer;          // approximative number of bytes spent for header
                                                   // and mode-partition #0
    residual_bytes: array[0..2, 0..3] of Integer;  // approximative number of bytes spent for
                                                   // DC/AC/uv coefficients for each (0..3) segments.
    segment_size: array[0..3] of Integer;          // number of macroblocks in each segments
    segment_quant: array[0..3] of Integer;         // quantizer values for each segments
    segment_level: array[0..3] of Integer;         // filtering strength for each segments [0..63]
  end;

  // Signature for output function. Should return 1 if writing was successful.
  // data/data_size is the segment of data to write, and 'picture' is for
  // reference (and so one can make use of picture->custom_ptr).
  TWebPWriterFunction = function(const data: PByte; data_size: LongWord;
    const picture: PWebPPicture): Integer; cdecl;

  TWebPPicture = record
    // input
    colorspace: Integer;            // colorspace: should be 0 for now (=Y'CbCr).
    width, height: Integer;         // dimensions.
    y, u, v: PByte;                 // pointers to luma/chroma planes.
    y_stride, uv_stride: Integer;   // luma/chroma strides.
    a: PByte;                       // pointer to the alpha plane (unused for now).

    // output
    writer: TWebPWriterFunction ;   // can be NULL
    custom_ptr: Pointer;            // can be used by the writer.

    // map for extra information
    extra_info_type: Integer;    // 1: intra type, 2: segment, 3: quant
                                 // 4: intra-16 prediction mode,
                                 // 5: chroma prediction mode,
                                 // 6: bit cost, 7: distortion
    extra_info: PByte;           // if not NULL, points to an array of size
                                 // ((width + 15) / 16) * ((height + 15) / 16) that
                                 // will be filled with a macroblock map, depending
                                 // on extra_info_type.

    // where to store statistics, if not NULL:
    stats: PWebPAuxStats;
  end;


(******************************************************************************
  decode.h
  Main decoding functions for WEBP images.
 ******************************************************************************)
 // Dynamic load : Vars that will hold our dynamically loaded functions...


// *************************** functions *******************************
var

// Return the decoder's version number, packed in hexadecimal using 8bits for
// each of major/minor/revision. E.g: v2.5.7 is 0x020507.
WebPGetDecoderVersion: function(): Integer; cdecl;

// Retrieve basic header information: width, height.
// This function will also validate the header and return 0 in
// case of formatting error.
// Pointers *width/*height can be passed NULL if deemed irrelevant.
WebPGetInfo: function(const data: PByte; data_size: LongWord;
  width, height: PInteger): Integer; cdecl;

// Decodes WEBP images pointed to by *data and returns RGB samples, along
// with the dimensions in *width and *height.
// The returned pointer should be deleted calling free().
// Returns NULL in case of error.
WebPDecodeRGB: function(const data: PByte; data_size: LongWord;
  width, height: PInteger): PByte; cdecl;

// Same as WebPDecodeRGB, but returning RGBA data.
WebPDecodeRGBA: function(const data: PByte; data_size: LongWord;
  width, height: PInteger): PByte; cdecl;

// This variant decode to BGR instead of RGB.
WebPDecodeBGR: function(const data: PByte; data_size: LongWord;
  width, height: PInteger): PByte; cdecl;
// This variant decodes to BGRA instead of RGBA.
WebPDecodeBGRA: function(const data: PByte; data_size: LongWord;
  width, height: PInteger): PByte; cdecl;

// Decode WEBP images stored in *data in Y'UV format(*). The pointer returned is
// the Y samples buffer. Upon return, *u and *v will point to the U and V
// chroma data. These U and V buffers need NOT be free()'d, unlike the returned
// Y luma one. The dimension of the U and V planes are both (*width + 1) / 2
// and (*height + 1)/ 2.
// Upon return, the Y buffer has a stride returned as '*stride', while U and V
// have a common stride returned as '*uv_stride'.
// Return NULL in case of error.
// (*) Also named Y'CbCr. See: http://en.wikipedia.org/wiki/YCbCr
WebPDecodeYUV: function(const data: PByte; data_size: LongWord; width, height: PInteger;
  var u, v: PByte; stride, uv_stride: PInteger): PByte; cdecl;

// Releases memory returned by the WebPDecode*() functions above.
WebPFree: procedure(const data: PByte); cdecl;

// These three functions are variants of the above ones, that decode the image
// directly into a pre-allocated buffer 'output_buffer'. The maximum storage
// available in this buffer is indicated by 'output_buffer_size'. If this
// storage is not sufficient (or an error occurred), NULL is returned.
// Otherwise, output_buffer is returned, for convenience.
// The parameter 'output_stride' specifies the distance (in bytes)
// between scanlines. Hence, output_buffer_size is expected to be at least
// output_stride x picture-height.
WebPDecodeRGBInto: function(const data: PByte; data_size: LongWord;
  output_buffer: PByte; output_buffer_size, output_stride: Integer): PByte; cdecl;

WebPDecodeRGBAInto: function(const data: PByte; data_size: LongWord;
  output_buffer: PByte; output_buffer_size, output_stride: Integer): PByte; cdecl;

// BGR variants
WebPDecodeBGRInto: function(const data: PByte; data_size: LongWord;
  output_buffer: PByte; output_buffer_size, output_stride: Integer): PByte; cdecl;

WebPDecodeBGRAInto: function(const data: PByte; data_size: LongWord;
  output_buffer: PByte; output_buffer_size, output_stride: Integer): PByte; cdecl;

// WebPDecodeYUVInto() is a variant of WebPDecodeYUV() that operates directly
// into pre-allocated luma/chroma plane buffers. This function requires the
// strides to be passed: one for the luma plane and one for each of the
// chroma ones. The size of each plane buffer is passed as 'luma_size',
// 'u_size' and 'v_size' respectively.
// Pointer to the luma plane ('*luma') is returned or NULL if an error occurred
// during decoding (or because some buffers were found to be too small).
WebPDecodeYUVInto: function(const data: PByte; data_size: LongWord;
                           luma: PByte; luma_size, luma_stride: Integer;
                           u: PByte; u_size, u_stride: Integer;
                           v: PByte; v_size, v_stride: Integer): PByte; cdecl;

//-----------------------------------------------------------------------------
// Incremental decoding
//
//  This API allows streamlined decoding of partial data.
//  Picture can be incrementally decoded as data become available thanks to the
// WebPIDecoder object. This object can be left in a SUSPENDED state if the
// picture is only partially decoded, pending additional input.
// Code example:
//
//   WebPIDecoder* const idec = WebPINew(mode);
//   while (has_more_data) {
//     // ... (get additional data)
//     status = WebPIAppend(idec, new_data, new_data_size);
//     if (status != VP8_STATUS_SUSPENDED ||
//       break;
//     }
//
//     // The above call decodes the current available buffer.
//     // Part of the image can now be refreshed by calling to
//     // WebPIDecGetRGB()/WebPIDecGetYUV() etc.
//   }
//   WebPIDelete(idec);

// Creates a WebPIDecoder object. Returns NULL in case of failure.
WebPINew: function(mode: WEBP_CSP_MODE): PWebPIDecoder; cdecl;

// This function allocates and initializes an incremental-decoder object, which
// will output the r/g/b(/a) samples specified by 'mode' into a preallocated
// buffer 'output_buffer'. The size of this buffer is at least
// 'output_buffer_size' and the stride (distance in bytes between two scanlines)
// is specified by 'output_stride'. Returns NULL if the allocation failed.
WebPINewRGB: function(mode: WEBP_CSP_MODE; output_buffer: PByte;
  output_buffer_size, output_stride: Integer): PWebPIDecoder; cdecl;

// This function allocates and initializes an incremental-decoder object, which
// will output the raw luma/chroma samples into a preallocated planes. The luma
// plane is specified by its pointer 'luma', its size 'luma_size' and its stride
// 'luma_stride'. Similarly, the chroma-u plane is specified by the 'u',
// 'u_size' and 'u_stride' parameters, and the chroma-v plane by 'v', 'v_size'
// and 'v_size'.
// Returns NULL if the allocation failed.
WebPINewYUV: function(luma: PByte; luma_size, luma_stride: Integer;
                          u: PByte; u_size, u_stride: Integer;
                          v: PByte; v_size, v_stride: Integer): PWebPIDecoder; cdecl;

// Deletes the WebpBuffer object and associated memory. Must always be called
// if WebPINew, WebPINewRGB or WebPINewYUV succeeded.
WebPIDelete: procedure(const idec: PWebPIDecoder); cdecl;

// Copies and decodes the next available data. Returns VP8_STATUS_OK when
// the image is successfully decoded. Returns VP8_STATUS_SUSPENDED when more
// data is expected. Returns error in other cases.
WebPIAppend: function(const idec: PWebPIDecoder; const data: PByte;
  data_size: LongWord): TVP8StatusCode; cdecl;

// A variant of the above function to be used when data buffer contains
// partial data from the beginning. In this case data buffer is not copied
// to the internal memory.
// Note that the value of the 'data' pointer can change between calls to
// WebPIUpdate, for instance when the data buffer is resized to fit larger data.
WebPIUpdate: function(const idec: PWebPIDecoder; const data: PByte;
  data_size: LongWord): TVP8StatusCode; cdecl;

// Returns the RGB image decoded so far. Returns NULL if output params are not
// initialized yet. *last_y is the index of last decoded row in raster scan
// order. Some pointers (*last_y, *width etc.) can be NULL if corresponding
// information is not needed.
WebPIDecGetRGB: function(const idec: PWebPIDecoder; last_y, width,
  height, stride: PInteger): PByte; cdecl;

// Same as above function to get YUV image. Returns pointer to the luma plane
// or NULL in case of error.
WebPIDecGetYUV: function(const idec: PWebPIDecoder; last_y: PInteger;
  var u, v: PByte; width, height, stride, uv_stride: PInteger): PByte; cdecl;

(******************************************************************************
  WebP encoder: main interface
 ******************************************************************************)

// Return the encoder's version number, packed in hexadecimal using 8bits for
// each of major/minor/revision. E.g: v2.5.7 is 0x020507.
WebPGetEncoderVersion: function(): Integer; cdecl;

//-----------------------------------------------------------------------------
// One-stop-shop call! No questions asked:

// Returns the size of the compressed data (pointed to by *output), or 0 if
// an error occurred. The compressed data must be released by the caller
// using the call 'WebPFree(*output)'.
// These functions compress using the lossy format, and the quality_factor
// can go from 0 (smaller output, lower quality) to 100 (best quality,
// larger output).

WebPEncodeRGB: function(const rgb: PByte; width, height, stride: Integer;
  quality_factor: single; var output: PByte): LongWord; cdecl;

WebPEncodeBGR: function(const bgr: PByte; width, height, stride: Integer;
  quality_factor: Single; var output: PByte): LongWord; cdecl;

WebPEncodeRGBA: function(const rgba: PByte; width, height, stride: Integer;
  quality_factor: Single; var output: PByte): LongWord; cdecl;

WebPEncodeBGRA: function(const bgra: PByte; width, height, stride: Integer;
  quality_factor: Single; var output: PByte): LongWord; cdecl;

// These functions are the equivalent of the above, but compressing in a
// lossless manner. Files are usually larger than lossy format, but will
// not suffer any compression loss.
// Note these functions, like the lossy versions, use the library's default
// settings. For lossless this means 'exact' is disabled. RGB values in
// transparent areas will be modified to improve compression. To avoid this,
// use WebPEncode() and set WebPConfig::exact to 1.

WebPEncodeLosslessRGB: function(const rgb: PByte; width, height, stride: Integer;
  var output: PByte): LongWord; cdecl;

WebPEncodeLosslessBGR: function(const bgr: PByte; width, height, stride: Integer;
  var output: PByte): LongWord; cdecl;

WebPEncodeLosslessRGBA: function(const rgba: PByte; width, height, stride: Integer;
  var output: PByte): LongWord; cdecl;

WebPEncodeLosslessBGRA: function(const bgra: PByte; width, height, stride: Integer;
  var output: PByte): LongWord; cdecl;

// Should always be called, to initialize a fresh WebPConfig structure before
// modification. Returns 0 in case of version mismatch. WebPConfigInit() must
// have succeeded before using the 'config' object.
function WebPConfigInit(const config: PWebPConfig): Integer;

// This function will initialize the configuration according to a predefined
// set of parameters (referred to by 'preset') and a given quality factor.
// This function can be called as a replacement to WebPConfigInit(). Will
// return 0 in case of error.
function WebPConfigPreset(const config: PWebPConfig; preset: TWebPPreset;
  quality: Single): Integer;

var
// Returns 1 if all parameters are in valid range and the configuration is OK.
WebPValidateConfig: function(const config: PWebPConfig): Integer; cdecl;

// Should always be called, to initialize the structure. Returns 0 in case of
// version mismatch. WebPPictureInit() must have succeeded before using the
// 'picture' object.
function WebPPictureInit(const picture: PWebPPicture): Integer;

//-----------------------------------------------------------------------------
// WebPPicture utils
var
// Convenience allocation / deallocation based on picture->width/height:
// Allocate y/u/v buffers as per width/height specification.
// Note! This function will free the previous buffer if needed.
// Returns 0 in case of memory error.
WebPPictureAlloc: function(const picture: PWebPPicture): Integer; cdecl;

// Release memory allocated by WebPPictureAlloc() or WebPPictureImport*()
// Note that this function does _not_ free the memory pointed to by 'picture'.
WebPPictureFree: procedure(const picture: PWebPPicture); cdecl;

// Copy the pixels of *src into *dst, using WebPPictureAlloc.
// Returns 0 in case of memory allocation error.
WebPPictureCopy: function(const src, dst: PWebPPicture): Integer; cdecl;

// self-crops a picture to the rectangle defined by top/left/width/height.
// Returns 0 in case of memory allocation error, or if the rectangle is
// outside of the source picture.
WebPPictureCrop: function(const picture: PWebPPicture;
  left, top, width, height: Integer): Integer; cdecl;

// Colorspace conversion function. Previous buffer will be free'd, if any.
// *rgb buffer should have a size of at least height * rgb_stride.
// Returns 0 in case of memory error.
WebPPictureImportRGB: function(const picture: PWebPPicture;
  const rgb: PByte; rgb_stride: Integer): Integer; cdecl;

// Same, but for RGBA buffer. Alpha information is ignored.
WebPPictureImportRGBA: function(const picture: PWebPPicture;
  const rgba: PByte; rgba_stride: Integer): Integer; cdecl;

// Variant of the above, but taking BGR input:
WebPPictureImportBGR: function(const picture: PWebPPicture;
  const bgr: PByte; bgr_stride: Integer): Integer; cdecl;

WebPPictureImportBGRA: function(const picture: PWebPPicture;
  const bgra: PByte; bgra_stride: Integer): Integer; cdecl;

//-----------------------------------------------------------------------------
// Main call

// Main encoding call, after config and picture have been initialiazed.
// 'picture' must be less than 16384x16384 in dimension, and the 'config' object
// must be a valid one.
// Returns false in case of error, true otherwise.
WebPEncode: function(const config: PWebPConfig; const picture: PWebPPicture): Integer; cdecl;

WebPConfigInitInternal: function(const conf: PWebPConfig; preset: TWebPPreset;
  quality: single; version: Integer): Integer; cdecl;

// Internal, version-checked, entry point
WebPPictureInitInternal: function(const picture: PWebPPicture; version: Integer): Integer; cdecl;
{Special methods for dynamic loading of lib ...}

var
  LibWebPHandle: TLibHandle = dynlibs.NilHandle; // this will hold our handle for the lib; it functions nicely as a mutli-lib prevention unit as well...
  LibWebPRefCount : LongWord = 0;  // Reference counter

function LibWebPLoaded : boolean; inline;
Function LibWebPLoad(const libfilename:string = ''): boolean; // load the lib
Procedure LibWebPUnload; // unload and frees the lib from memory : do not forget to call it before close application.

implementation

uses sysutils;

// Internal, version-checked, entry point
const
  WEBP_ENCODER_ABI_VERSION = $0001;

function WebPConfigInit(const config: PWebPConfig): Integer;
begin
  Result := WebPConfigInitInternal(config, WEBP_PRESET_DEFAULT, 75.0,  WEBP_ENCODER_ABI_VERSION);
end;

function WebPConfigPreset(const config: PWebPConfig; preset: TWebPPreset;
  quality: Single): Integer;
begin
  Result := WebPConfigInitInternal(config, preset, quality, WEBP_ENCODER_ABI_VERSION);
end;

function WebPPictureInit(const picture: PWebPPicture): Integer;
begin
  Result := WebPPictureInitInternal(picture, WEBP_ENCODER_ABI_VERSION);
end;

function LibWebPLoaded: boolean;
begin
 Result := (LibWebPHandle <> dynlibs.NilHandle);
end;

Function LibWebPLoad (const libfilename:string) :boolean;
var
  thelib: string;
begin
  Result := False;
  if LibWebPHandle<>0 then
  begin
   Inc(LibWebPRefCount);
   result:=true {is it already there ?}
  end else
  begin {go & load the library}
    if Length(libfilename) = 0 then
    begin
      thelib := LibWebPFilename;
      if thelib = '' then exit(false);
      thelib := ExtractFilePath(ParamStr(0)) + DirectorySeparator + thelib;
    end else thelib := libfilename;
    LibWebPHandle := DynLibs.SafeLoadLibrary(thelib); // obtain the handle we want
    {$IFDEF WINDOWS}
    // second try on Windows without 32/64 suffix
    if LibWebPHandle = DynLibs.NilHandle then
    begin
      thelib := ExtractFilePath(ParamStr(0)) + DirectorySeparator + 'libwebp.dll';
      LibWebPHandle := DynLibs.SafeLoadLibrary(thelib); // obtain the handle we want
    end;
    {$ENDIF}
    if LibWebPHandle <> DynLibs.NilHandle then
    begin {now we tie the functions to the VARs from above}

Pointer(WebPGetDecoderVersion):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPGetDecoderVersion'));
Pointer(WebPGetInfo):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPGetInfo'));
Pointer(WebPDecodeRGB):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPDecodeRGB'));
Pointer(WebPDecodeRGBA):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPDecodeRGBA'));
Pointer(WebPDecodeBGR):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPDecodeBGR'));
Pointer(WebPDecodeBGRA):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPDecodeBGRA'));
Pointer(WebPDecodeYUV):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPDecodeYUV'));
Pointer(WebPFree):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPFree'));
Pointer(WebPDecodeRGBInto):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPDecodeRGBInto'));
Pointer(WebPDecodeRGBAInto):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPDecodeRGBAInto'));
Pointer(WebPDecodeBGRInto):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPDecodeBGRInto'));
Pointer(WebPDecodeBGRAInto):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPDecodeBGRAInto'));
Pointer(WebPDecodeYUVInto):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPDecodeYUVInto'));
Pointer(WebPINew):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPINew'));
Pointer(WebPINewRGB):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPINewRGB'));
Pointer(WebPINewYUV):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPINewYUV'));
Pointer(WebPIDelete):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPIDelete'));
Pointer(WebPIAppend):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPIAppend'));
Pointer(WebPIUpdate):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPIUpdate'));
Pointer(WebPIDecGetRGB):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPIDecGetRGB'));
Pointer(WebPIDecGetYUV):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPIDecGetYUV'));
Pointer(WebPGetEncoderVersion):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPGetEncoderVersion'));
Pointer(WebPEncodeRGB):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPEncodeRGB'));
Pointer(WebPEncodeBGR):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPEncodeBGR'));
Pointer(WebPEncodeRGBA):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPEncodeRGBA'));
Pointer(WebPEncodeBGRA):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPEncodeBGRA'));
Pointer(WebPEncodeLosslessRGB):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPEncodeLosslessRGB'));
Pointer(WebPEncodeLosslessBGR):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPEncodeLosslessBGR'));
Pointer(WebPEncodeLosslessRGBA):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPEncodeLosslessRGBA'));
Pointer(WebPEncodeLosslessBGRA):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPEncodeLosslessBGRA'));
//Pointer(WebPConfigInit):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPConfigInit'));
//Pointer(WebPConfigPreset):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPConfigPreset'));
//Pointer(WebPPictureInit):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPPictureInit'));
Pointer(WebPPictureAlloc):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPPictureAlloc'));
Pointer(WebPPictureFree):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPPictureFree'));
Pointer(WebPPictureCopy):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPPictureCopy'));
Pointer(WebPPictureCrop):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPPictureCrop'));
Pointer(WebPPictureImportRGB):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPPictureImportRGB'));
Pointer(WebPPictureImportRGBA):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPPictureImportRGBA'));
Pointer(WebPPictureImportBGR):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPPictureImportBGR'));
Pointer(WebPPictureImportBGRA):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPPictureImportBGRA'));
Pointer(WebPEncode):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPEncode'));
Pointer(WebPConfigInitInternal):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPConfigInitInternal'));
Pointer(WebPPictureInitInternal):=DynLibs.GetProcedureAddress(LibWebPHandle,PChar('WebPPictureInitInternal'));

    end;
    Result := LibWebPLoaded;
    LibWebPRefCount:=1;
  end;
end;

Procedure LibWebPUnload;
begin
  // < Reference counting
  if LibWebPRefCount > 0 then
    dec(LibWebPRefCount);
  if LibWebPRefCount > 0 then
    exit;
  // >
  if LibWebPLoaded then
  begin
    DynLibs.UnloadLibrary(LibWebPHandle);
    LibWebPHandle:=DynLibs.NilHandle;
  end;
end;

end.

