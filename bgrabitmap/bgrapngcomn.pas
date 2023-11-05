unit BGRAPNGComn;

{$mode objfpc}{$H+}

interface

uses
  PNGComn;

type
  PNGImageException = PNGComn.PNGImageException;
  TPNGSignature = array[0..7] of byte;
  TChunk = PNGComn.TChunk;
  TChunkCode = PNGComn.TChunkCode;
  TChunkHeader = PNGComn.TChunkHeader;

  // static PNG chunks
  TChunkTypes = PNGComn.TChunkTypes;
  THeaderChunk = packed record
    Width, height : LongWord;
    BitDepth, ColorType, Compression, Filter, Interlace : byte;
  end;
  EightLong = PNGComn.EightLong;

  TPNGPhysicalDimensions = packed record
    X_Pixels, Y_Pixels :DWord;
    Unit_Specifier :Byte;
  end;
  PPNGPhysicalDimensions=^TPNGPhysicalDimensions;

  // animated PNG chunks
  TAnimatedChunkTypes = {extends TChunkTypes} (
  ctacTL = 128, // Animation Control: Specifies number of frames and repeat count
  ctfcTL,       // Frame Control: Position, delay and render mode of the next frame
  ctfdAT        // Frame Data: Contains image data for one frame
  );

  PAnimationControlChunk = ^TAnimationControlChunk;
  TAnimationControlChunk = record
    FrameCount : longword;
    RepeatCount : longword;
  end;

  PFrameControlChunk = ^TFrameControlChunk;
  TFrameControlChunk = packed record
    SequenceNumber: longword;
    Width, Height: longword;
    OffsetX, OffsetY: longword;
    DelayNum, DelayDenom: word;
    DisposeOp, BlendOp: byte;
  end;

  PFrameDataChunk = ^TFrameDataChunk;
  TFrameDataChunk = record
    SequenceNumber: longword;
    // followed by frame data
  end;

const
  MaxChunkLength = PNGComn.MaxChunkLength;

  // static PNG chunks
  ctIHDR = PNGComn.ctIHDR;  // Image Header: Contains image's size, depth and compression method
  ctcHRM = PNGComn.ctcHRM;  // Chromaticity: Provides the chromaticity coordinates
  ctgAMA = PNGComn.ctgAMA;  // Gamma factor: Specifies gamma correction.
  ctsBIT = PNGComn.ctsBIT;  // Significant Bits: Indicates the color-accuracy of the source data.
  ctPLTE = PNGComn.ctPLTE;  // Palette: Lists the colors in the image palette.
  ctbKGD = PNGComn.ctbKGD;  // Background Color: Specifies the background color.
  cthIST = PNGComn.cthIST;  // Histogram: Provides a histogram of the color usage in the image.
  cttRNS = PNGComn.cttRNS;  // Transparency: Contains transparency information.
  ctoFFs = PNGComn.ctoFFs;  // Offset: Gives position on a printed page.
  ctpHYs = PNGComn.ctpHYs;  // Physical Dimensions: Specifies the intended pixel size or aspect ratio for display.
  ctIDAT = PNGComn.ctIDAT;  // Image Data: Contains image data which is compressed and possibly filtered.
  cttIME = PNGComn.cttIME;  // Image Last-Modification Time: Stores the time that the image was last changed.
  ctsCAL = PNGComn.ctsCAL;  // Physical Scale: Provides physical scale information of the image.
  cttEXt = PNGComn.cttEXt;  // Textual Data: Stores text data associated with a keyword.
  ctzTXt = PNGComn.ctzTXt;  // Compressed Textual Data: Similar to tEXt but the text is compressed.
  ctIEND = PNGComn.ctIEND;  // Image End: Marks the end of the PNG data stream.
  ctsRGB = PNGComn.ctsRGB;  // Standard RGB Color Space: Indicates that the image uses the sRGB color space.
  ctiCCP = PNGComn.ctiCCP;  // ICC Profile: Contains an ICC color profile.
  ctiTXt = PNGComn.ctiTXt;  // International Textual Data: Allows embedding text data with character encoding information.
  ctsPLT = PNGComn.ctsPLT;  // Suggested Palette: Suggests a palette to use if the full range of colors is unavailable.
  ctUnknown = PNGComn.ctUnknown; // Unknown: Represents an unrecognized chunk.

  // animated PNG chunks
  AnimatedChunkTypes : array[low(TAnimatedChunkTypes)..high(TAnimatedChunkTypes)] of TChunkCode = (
    'acTL',  'fcTL',  'fdAT'
  );

  APNG_DISPOSE_OP_NONE = 0;
  APNG_DISPOSE_OP_BACKGROUND = 1;
  APNG_DISPOSE_OP_PREVIOUS = 2;

  APNG_BLEND_OP_SOURCE = 0;
  APNG_BLEND_OP_OVER = 1;

function CheckSignature(const ASignature: TPNGSignature): boolean;
function GetSignature: TPNGSignature;
function GetChunkCode(AChunkType: TChunkTypes): TChunkCode;
function GetChunkType(AChunkCode: TChunkCode): TChunkTypes;
function IsAnimatedChunkType(AChunkType: TChunkTypes): boolean;
function CalculateChunkCRC(AChunkCode: TChunkCode; AData: Pointer; ALength: integer): LongWord;

implementation

uses FPImgCmn;

function CheckSignature(const ASignature: TPNGSignature): boolean;
begin
  result := QWord(ASignature) = QWord(PNGComn.Signature);
end;

function GetSignature: TPNGSignature;
begin
  result := PNGComn.Signature;
end;

function GetChunkCode(AChunkType: TChunkTypes): TChunkCode;
begin
  if AChunkType <= high(PNGComn.ChunkTypes) then
    result := PNGComn.ChunkTypes[AChunkType]
  else if IsAnimatedChunkType(AChunkType) then
    result := AnimatedChunkTypes[TAnimatedChunkTypes(AChunkType)]
  else
    raise PNGImageException.Create('Unknown chunk type');
end;

function GetChunkType(AChunkCode: TChunkCode): TChunkTypes;
var
  t: TChunkTypes;
  at: TAnimatedChunkTypes;
begin
  for t := low(ChunkTypes) to high(ChunkTypes) do
    if ChunkTypes[t] = AChunkCode then exit(t);
  for at := low(AnimatedChunkTypes) to high(AnimatedChunkTypes) do
    if AnimatedChunkTypes[at] = AChunkCode then exit(TChunkTypes(at));
  result := ctUnknown;
end;

function IsAnimatedChunkType(AChunkType: TChunkTypes): boolean;
begin
  result := TAnimatedChunkTypes(AChunkType) in [low(AnimatedChunkTypes)..high(AnimatedChunkTypes)];
end;

function CalculateChunkCRC(AChunkCode: TChunkCode; AData: Pointer; ALength: integer): LongWord;
begin
  result := CalculateCRC (PNGComn.All1Bits, AChunkCode, sizeOf(AChunkCode));
  result := CalculateCRC (result, AData^, ALength);
  result := result xor PNGComn.All1Bits;
end;

end.

