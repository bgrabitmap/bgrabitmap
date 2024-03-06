// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
    The original file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    PNG writer class modified by circular.

 **********************************************************************

 Fix for images with grayscale and alpha,
 and for images with transparent pixels
 }
{*****************************************************************************}
{
  2023-06  - Massimo Magnano
           - added Resolution support
}
{*****************************************************************************}

{ Implements the writer for the PNG image format }
unit BGRAWritePNG;

{$mode objfpc}{$H+}

interface

uses sysutils, BGRAClasses, FPImage, FPImgCmn, BGRAPNGComn, ZStream, BGRABitmapTypes;

type
  { Information about frame to write in PNG }
  TPNGFrameToWrite = record
    FrameControl: TFrameControlChunk;
    Image: TFPCustomImage;
  end;
  TPNGArrayOfFrameToWrite = array of TPNGFrameToWrite;

  TGetPixelFunc = function (x,y : LongWord) : TColorData of object;
  TGetPixelBGRAFunc = function (p: PBGRAPixel) : TColorData of object;

  TColorFormatFunction = function (color:TFPColor) : TColorData of object;

  {* Extends the TFPCustomImageWriter to write the PNG image format }
  TBGRAWriterPNG = class (TBGRACustomWriterPNG)
    private
      FCompressedText, FWordSized, FIndexed,
      FUseAlpha, FGrayScale : boolean;
      FCustomPalette: TFPPalette;

      FSourceImage: TFPCustomImage;
      FAnimation: array of TPNGFrameToWrite;
      FAnimationChunkCount: Longword;
      FRepeatCount: integer;

      FUsetRNS: boolean;
      FByteWidth : byte;
      FChunk : TChunk;
      CFmt : TColorFormat; // format of the colors to convert from
      FFmtColor : TColorFormatFunction;
      FTransparentColor : TFPColor;
      FTransparentColorOk: boolean;
      FSwitchLine, FCurrentLine, FPreviousLine : pByteArray;
      FPalette : TFPPalette;
      OwnsPalette : boolean;
      FHeader : THeaderChunk;
      FGetPixel : TGetPixelFunc;
      FGetPixelBGRA : TGetPixelBGRAFunc;
      FDatalineLength : LongWord;
      ZData : TMemoryStream;  // holds uncompressed data until all blocks are written
      Compressor : TCompressionStream; // compresses the data
      FCompressionLevel : TCompressionLevel;
      procedure WriteChunk;
      function GetColorPixel (x,y:LongWord) : TColorData;
      function GetPalettePixel (x,y:LongWord) : TColorData;
      function GetColPalPixel (x,y:LongWord) : TColorData;
      function GetColorPixelBGRA (p: PBGRAPixel) : TColorData;
      function GetPalettePixelBGRA (p: PBGRAPixel) : TColorData;
      function GetColPalPixelBGRA (p: PBGRAPixel) : TColorData;
      procedure InitWriteImageData;
      procedure GatherData;
      procedure WriteCompressedData(AImageDataChunkCode: TChunkCode);
      procedure FinalWriteImageData;
    protected
      property Header : THeaderChunk read FHeader;
      procedure InternalWrite ({%H-}Str:TStream; {%H-}Img:TFPCustomImage); override;
      function GetUseAlpha: boolean; override;
      procedure SetUseAlpha(AValue: boolean); override;
      procedure WriteIHDR; virtual;
      procedure WritePLTE; virtual;
      procedure WriteResolutionValues; virtual;
      procedure WritetRNS; virtual;
      procedure WriteImageData(AImage: TFPCustomImage; AImageDataChunkCode: TChunkCode); virtual;
      procedure WriteIDAT; virtual;
      procedure WritefdAT(AIndex: integer); virtual;
      procedure WriteTexts; virtual;
      procedure WriteacTL; virtual;
      procedure WritefcTL(AIndex: integer); virtual;
      procedure WriteIEND; virtual;
      function CurrentLine (x:LongWord) : byte; inline;
      function PrevSample (x:LongWord): byte; inline;
      function PreviousLine (x:LongWord) : byte; inline;
      function PrevLinePrevSample (x:LongWord): byte; inline;
      function  DoFilter (LineFilter:byte;index:LongWord; b:byte) : byte; virtual;
      procedure SetChunkLength (aValue : LongWord);
      procedure SetChunkType (ct : TChunkTypes); overload;
      procedure SetChunkType (ct : TChunkCode); overload;
      function DecideGetPixel : TGetPixelFunc; virtual;
      function DecideGetPixelBGRA : TGetPixelBGRAFunc; virtual;
      procedure DetermineHeader (var AHeader : THeaderChunk); virtual;
      function DetermineFilter ({%H-}Current, {%H-}Previous:PByteArray; {%H-}linelength:LongWord):byte; virtual;
      procedure FillScanLine (y : integer; ScanLine : pByteArray); virtual;
      function ColorDataGrayB(color:TFPColor) : TColorData;
      function ColorDataColorB(color:TFPColor) : TColorData;
      function ColorDataGrayW(color:TFPColor) : TColorData;
      function ColorDataColorW(color:TFPColor) : TColorData;
      function ColorDataGrayAB(color:TFPColor) : TColorData;
      function ColorDataColorAB(color:TFPColor) : TColorData;
      function ColorDataGrayAW(color:TFPColor) : TColorData;
      function ColorDataColorAW(color:TFPColor) : TColorData;
      property ChunkDataBuffer : pByteArray read FChunk.data;
      property UsetRNS : boolean read FUsetRNS;
      property SingleTransparentColor : TFPColor read FTransparentColor;
      property SingleTransparentColorOk : boolean read FTransparentColorOk;
      property ThePalette : TFPPalette read FPalette;
      property ColorFormat : TColorformat read CFmt;
      property ColorFormatFunc : TColorFormatFunction read FFmtColor;
      property byteWidth : byte read FByteWidth;
      property DatalineLength : LongWord read FDatalineLength;
    public
      constructor Create; override;
      destructor Destroy; override;
      procedure AnimationWrite(AStream: TStream;
        AImage:TFPCustomImage;  // default image (may be as well in the animation)
        AAnimation: TPNGArrayOfFrameToWrite;
        ARepeatCount: integer = 0); // loop count (0 for infinite loop)
      property GrayScale : boolean read FGrayscale write FGrayScale;
      property Indexed : boolean read FIndexed write FIndexed;
      property CustomPalette: TFPPalette read FCustomPalette write FCustomPalette;
      property CompressedText : boolean read FCompressedText write FCompressedText;
      property WordSized : boolean read FWordSized write FWordSized;
      property CompressionLevel : TCompressionLevel read FCompressionLevel write FCompressionLevel;
  end;

implementation

uses BGRAReadPNG;

constructor TBGRAWriterPNG.Create;
begin
  inherited;
  Fchunk.acapacity := 0;
  Fchunk.data := nil;
  FGrayScale := False;
  FIndexed := False;
  FCustomPalette := nil;
  FAnimation := nil;
  FRepeatCount := -1;
  FCompressedText := True;
  FWordSized := False;
  FUseAlpha := True;
  FCompressionLevel:=clDefault;
end;

destructor TBGRAWriterPNG.Destroy;
begin
  if OwnsPalette then FreeAndNil(FPalette);
  with Fchunk do
    if acapacity > 0 then
      freemem (data);
  inherited;
end;

procedure TBGRAWriterPNG.AnimationWrite(AStream: TStream;
  AImage: TFPCustomImage; AAnimation: TPNGArrayOfFrameToWrite;
  ARepeatCount: integer);
begin
  if Indexed and not Assigned(FCustomPalette) then
    raise FPImageException.Create('Palette must be specified for indexed color');
  if ARepeatCount < 0 then
    raise FPImageException.Create('Repeat count must be positive');
  FAnimation := AAnimation;
  FRepeatCount:= ARepeatCount;
  try
    ImageWrite(AStream, AImage);
  finally
    FAnimation := nil;
    FRepeatCount:= -1;
  end;
end;

procedure TBGRAWriterPNG.WriteChunk;
var chead : TChunkHeader;
    c : LongWord;
begin
  with FChunk do
    begin
    {$IFDEF ENDIAN_LITTLE}
    chead.CLength := swap (alength);
    {$ELSE}
    chead.CLength := alength;
    {$ENDIF}
	if (ReadType = '') then
      if atype <> ctUnknown then
        chead.CType := GetChunkCode(aType)
      else
        raise PNGImageException.create ('Doesn''t have a chunktype to write')
    else
      chead.CType := ReadType;
    c := CalculateChunkCRC (ReadType, data, alength);
    {$IFDEF ENDIAN_LITTLE}
    crc := swap(c);
    {$ENDIF}
    with TheStream do
      begin
      Write (chead, sizeof(chead));
      if alength > 0 then
        Write (data^[0], alength);
      Write (crc, sizeof(crc));
      end;
    end;
end;

procedure TBGRAWriterPNG.SetChunkLength(aValue : LongWord);
begin
  with Fchunk do
    begin
    alength := aValue;
    if aValue > acapacity then
      begin
      if acapacity > 0 then
        freemem (data);
      GetMem (data, alength);
      acapacity := alength;
      end;
    end;
end;

procedure TBGRAWriterPNG.SetChunkType (ct : TChunkTypes);
begin
  with Fchunk do
    begin
    aType := ct;
    ReadType := GetChunkCode(ct);
    end;
end;

procedure TBGRAWriterPNG.SetChunkType (ct : TChunkCode);
begin
  with FChunk do
    begin
    ReadType := ct;
    aType := GetChunkType(ct);
    end;
end;

function TBGRAWriterPNG.CurrentLine(x:LongWord):byte;
begin
  result := FCurrentLine^[x];
end;

function TBGRAWriterPNG.PrevSample (x:LongWord): byte;
begin
  if x < byteWidth then
    result := 0
  else
    result := FCurrentLine^[x - bytewidth];
end;

function TBGRAWriterPNG.PreviousLine (x:LongWord) : byte;
begin
  result := FPreviousline^[x];
end;

function TBGRAWriterPNG.PrevLinePrevSample (x:LongWord): byte;
begin
  if x < byteWidth then
    result := 0
  else
    result := FPreviousLine^[x - bytewidth];
end;

function TBGRAWriterPNG.DoFilter(LineFilter:byte;index:LongWord; b:byte) : byte;
var diff : byte;
  procedure FilterSub;
  begin
    diff := PrevSample(index);
  end;
  procedure FilterUp;
  begin
    diff := PreviousLine(index);
  end;
  procedure FilterAverage;
  var l, p : word;
  begin
    l := PrevSample(index);
    p := PreviousLine(index);
    Diff := (l + p) div 2;
  end;
  procedure FilterPath;
  var dl, dp, dlp : word; // index for previous and distances for:
      l, p, lp : byte;  // r:predictor, Left, Previous, LeftPrevious
      r : integer;
  begin
    l := PrevSample(index);
    lp := PrevLinePrevSample(index);
    p := PreviousLine(index);
    r := Int32or64(l) + Int32or64(p) - Int32or64(lp);
    dl := abs (r - l);
    dlp := abs (r - lp);
    dp := abs (r - p);
    if (dl <= dp) and (dl <= dlp) then
      diff := l
    else if dp <= dlp then
      diff := p
    else
      diff := lp;
  end;
begin
  diff := 0;
  case LineFilter of
    1 : FilterSub;
    2 : FilterUp;
    3 : FilterAverage;
    4 : FilterPath;
  end;
  if diff > b then
    result := (b + $100 - diff)
  else
    result := b - diff;
end;

procedure TBGRAWriterPNG.DetermineHeader (var AHeader : THeaderChunk);
var c : integer;

  function ReducedColorEquals(const c1,c2: TFPColor): boolean;
  var g1,g2: word;
  begin
    if FGrayScale then
      begin
        g1 := CalculateGray(c1);
        g2 := CalculateGray(c2);
        if fwordsized then
          result := (g1 = g2)
        else
          result := (g1 shr 8 = g2 shr 8);
      end else
      begin
        if FWordSized then
          result := (c1.red = c2.red) and (c1.green = c2.green) and (c1.blue = c2.blue)
        else
          result := (c1.red shr 8 = c2.red shr 8) and (c1.green shr 8 = c2.green shr 8) and (c1.blue shr 8 = c2.blue shr 8);
      end;
  end;

  function CountAlphas : integer;
  var none, half : boolean;
      maxTransparentAlpha: word;

    procedure CountFromPalettedImage(AImage: TFPCustomImage);
    var
      p : integer;
      a : word;
      c : TFPColor;
    begin
      with AImage.Palette do
        begin
        p := count-1;
        FTransparentColor.alpha := alphaOpaque;
        while (p >= 0) do
          begin
          c := color[p];
          a := c.Alpha;
          if a < FTransparentColor.alpha then //we're looking for the most transparent color
            FTransparentColor := c;
          if a <= maxTransparentAlpha then none := true
          else if a <> alphaOpaque then half := true;
          dec (p);
          end;

        //check transparent color is used consistently
        p := count-1;
        while (p >= 0) do
          begin
          c := color[p];
          if c.alpha > maxTransparentAlpha then
          begin
            if ReducedColorEquals(c, FTransparentColor) then
              begin
              FTransparentColorOk := false;
              break;
              end;
          end
          else
          begin
            if not ReducedColorEquals(c, FTransparentColor) then
              begin
              FTransparentColorOk := false;
              break;
              end;
          end;
          dec(p);
          end;
        end;
    end;

    procedure CountFromRGBImage(AImage: TFPCustomImage);
    var
      a : word;
      c : TFPColor;
      x,y : longint;  // checks on < 0
    begin
      with AImage do
        begin
        x := width-1;
        y := height-1;
        FTransparentColor.alpha := alphaOpaque;
        while (y >= 0) and not half do //we stop if we already need a full alpha
          begin
          c := colors[x,y];
          a := c.Alpha;
          if a < FTransparentColor.alpha then //we're looking for the most transparent color
            FTransparentColor := c;
          if a <= maxTransparentAlpha then none := true
          else if a <> alphaOpaque then half := true;
          dec (x);
          if (x < 0) then
            begin
            dec (y);
            x := width-1;
            end;
          end;

        //check transparent color is used consistently
        x := width-1;
        y := height-1;
        while (y >= 0) do
          begin
          c := colors[x,y];
          if c.alpha > maxTransparentAlpha then
          begin
            if ReducedColorEquals(c, FTransparentColor) then
              begin
              FTransparentColorOk := false;
              break;
              end;
          end
          else
          begin
            if not ReducedColorEquals(c, FTransparentColor) then
              begin
              FTransparentColorOk := false;
              break;
              end;
          end;
          dec (x);
          if (x < 0) then
            begin
            dec (y);
            x := width-1;
            end;
          end;
        end;
    end;

  var i: integer;
  begin
    FTransparentColorOk := true;
    if FWordSized then maxTransparentAlpha := 0
    else maxTransparentAlpha := $00ff;
    half := false;
    none := false;
    if TheImage.UsePalette then
      CountFromPalettedImage(TheImage)
    else
      CountFromRGBImage(TheImage);

    for i := 0 to high(FAnimation) do
    begin
      if FAnimation[i].Image <> TheImage then
      begin
        if FAnimation[i].Image.UsePalette then
          CountFromPalettedImage(FAnimation[i].Image)
        else
          CountFromRGBImage(FAnimation[i].Image);
      end;
    end;

    if half then
      result := 3
    else
    if none then
      begin
      if FTransparentColorOk then
        result := 2
      else
        result := 3;
      end
    else
      result := 1;
  end;
  procedure DetermineColorFormat;
  begin
    with AHeader do
      case colortype of
        0 : if FWordSized then
              begin
              FFmtColor := @ColorDataGrayW;
              FByteWidth := 2;
              //CFmt := cfGray16
              end
            else
              begin
              FFmtColor := @ColorDataGrayB;
              FByteWidth := 1;
              //CFmt := cfGray8;
              end;
        2 : if FWordSized then
              begin
              FFmtColor := @ColorDataColorW;
              FByteWidth := 6;
              //CFmt := cfBGR48
              end
            else
              begin
              FFmtColor := @ColorDataColorB;
              FByteWidth := 3;
              //CFmt := cfBGR24;
              end;
        4 : if FWordSized then
              begin
              FFmtColor := @ColorDataGrayAW;
              FByteWidth := 4;
              //CFmt := cfGrayA32
              end
            else
              begin
              FFmtColor := @ColorDataGrayAB;
              FByteWidth := 2;
              //CFmt := cfGrayA16;
              end;
        6 : if FWordSized then
              begin
              FFmtColor := @ColorDataColorAW;
              FByteWidth := 8;
              //CFmt := cfABGR64
              end
            else
              begin
              FFmtColor := @ColorDataColorAB;
              FByteWidth := 4;
              //CFmt := cfABGR32;
              end;
      end;
  end;
begin
  with AHeader do
    begin
    {$IFDEF ENDIAN_LITTLE}
    // problem: TheImage has integer width, PNG header LongWord width.
    //          Integer Swap can give negative value
    Width := swap (LongWord(TheImage.Width));
    height := swap (LongWord(TheImage.Height));
    {$ELSE}
    Width := TheImage.Width;
    height := TheImage.Height;
    {$ENDIF}
    if FUseAlpha then
      c := CountAlphas
    else
      c := 0;
    if FIndexed then
      begin
      if OwnsPalette then FreeAndNil(FPalette);
      if Assigned(FCustomPalette) then
      begin
        OwnsPalette:= false;
        FPalette := FCustomPalette;
      end else
      begin
        if Assigned(FAnimation) then
          raise FPImageException.Create('Palette must be specified for indexed color');
        OwnsPalette := not TheImage.UsePalette;
        if OwnsPalette then
          begin
          FPalette := TFPPalette.Create (16);
          FPalette.Build (TheImage);
          end
        else
          FPalette := TheImage.Palette;
      end;
      if ThePalette.count > 256 then
        raise PNGImageException.Create ('Too many colors to use indexed PNG color type');
      ColorType := 3;
      FUsetRNS := C > 1;
      BitDepth := 8;
      FByteWidth := 1;
      end
    else
      begin
      if c = 3 then
        ColorType := 4;
      FUsetRNS := (c = 2);
      if not FGrayScale then
        ColorType := ColorType + 2;
      if FWordSized then
        BitDepth := 16
      else
        BitDepth := 8;
      DetermineColorFormat;
      end;
    Compression := 0;
    Filter := 0;
    Interlace := 0;
    end;
end;

procedure TBGRAWriterPNG.WriteIHDR;
var signature: TPNGSignature;
begin
  // signature for PNG
  signature := GetSignature;
  TheStream.writeBuffer(signature, sizeof(signature));
  // Determine all settings for filling the header
  fillchar(fheader,sizeof(fheader),#0);
  DetermineHeader (FHeader);
  // write the header chunk
  SetChunkLength (sizeof(FHeader));
  move (FHeader, ChunkDataBuffer^, sizeof(FHeader));
  SetChunkType (ctIHDR);
  WriteChunk;
end;

{ Color convertions }

function TBGRAWriterPNG.ColorDataGrayB(color:TFPColor) : TColorData;
var t : word;
begin
  t := CalculateGray (color);
  result := hi(t);
end;

function TBGRAWriterPNG.ColorDataGrayW(color:TFPColor) : TColorData;
begin
  result := CalculateGray (color);
end;

function TBGRAWriterPNG.ColorDataGrayAB(color:TFPColor) : TColorData;
begin
  result := ColorDataGrayB (color);
  result := (color.Alpha and $ff00) or result;
end;

function TBGRAWriterPNG.ColorDataGrayAW(color:TFPColor) : TColorData;
begin
  result := ColorDataGrayW (color);
  result := (color.Alpha shl 16) or result;
end;

function TBGRAWriterPNG.ColorDataColorB(color:TFPColor) : TColorData;
begin
  {$PUSH}{$HINTS OFF}
  with color do
    result := hi(red) + (green and $FF00) + (hi(blue) shl 16);
  {$POP}
end;

function TBGRAWriterPNG.ColorDataColorW(color:TFPColor) : TColorData;
begin
  {$PUSH}{$HINTS OFF}
  with color do
    result := red + (green shl 16) + (qword(blue) shl 32);
  {$POP}
end;

function TBGRAWriterPNG.ColorDataColorAB(color:TFPColor) : TColorData;
begin
  {$PUSH}{$HINTS OFF}
  with color do
    result := hi(red) + (green and $FF00) + (hi(blue) shl 16) + (hi(alpha) shl 24);
  {$POP}
end;

function TBGRAWriterPNG.ColorDataColorAW(color:TFPColor) : TColorData;
begin
  {$PUSH}{$HINTS OFF}
  with color do
    result := red + (green shl 16) + (qword(blue) shl 32) + (qword(alpha) shl 48);
  {$POP}
end;

{ Data making routines }

function TBGRAWriterPNG.GetColorPixel (x,y:LongWord) : TColorData;
begin
  result := FFmtColor (FSourceImage[x,y]);
end;

function TBGRAWriterPNG.GetPalettePixel (x,y:LongWord) : TColorData;
begin
  result := FSourceImage.Pixels[x,y];
end;

function TBGRAWriterPNG.GetColPalPixel (x,y:LongWord) : TColorData;
begin
  result := ThePalette.IndexOf (FSourceImage.Colors[x,y]);
end;

function TBGRAWriterPNG.GetColorPixelBGRA(p: PBGRAPixel): TColorData;
begin
  result := FFmtColor(p^.ToFPColor);
end;

function TBGRAWriterPNG.GetPalettePixelBGRA(p: PBGRAPixel): TColorData;
begin
  result := FSourceImage.Palette.IndexOf(p^.ToFPColor);
end;

function TBGRAWriterPNG.GetColPalPixelBGRA(p: PBGRAPixel): TColorData;
begin
  result := ThePalette.IndexOf(p^.ToFPColor);
end;

function TBGRAWriterPNG.DecideGetPixel : TGetPixelFunc;
begin
  case Fheader.colortype of
    3 : if (FAnimation = nil) and TheImage.UsePalette then
          result := @GetPalettePixel
          else result := @GetColPalPixel;
    else  result := @GetColorPixel;
  end;
end;

function TBGRAWriterPNG.DecideGetPixelBGRA: TGetPixelBGRAFunc;
begin
  case Fheader.colortype of
    3 : if (FAnimation = nil) and TheImage.UsePalette then
          result := @GetPalettePixelBGRA
          else result := @GetColPalPixelBGRA;
    else  result := @GetColorPixelBGRA;
  end;
end;

procedure TBGRAWriterPNG.WritePLTE;
var r,t : integer;
    c : TFPColor;
begin
  with ThePalette do
    begin
    SetChunkLength (count*3);
    SetChunkType (ctPLTE);
    t := 0;
    For r := 0 to count-1 do
      begin
      c := Color[r];
      ChunkdataBuffer^[t] := c.red div 256;
      inc (t);
      ChunkdataBuffer^[t] := c.green div 256;
      inc (t);
      ChunkdataBuffer^[t] := c.blue div 256;
      inc (t);
      end;
    end;
  WriteChunk;
end;

procedure TBGRAWriterPNG.WriteResolutionValues;
begin
  {$IF FPC_FULLVERSION<30203}
  if (TheImage is TCustomUniversalBitmap) then
  with TCustomUniversalBitmap(TheImage) do
  {$ELSE}
  with TheImage do
  {$ENDIF}
  begin
     if (ResolutionX = 0) or (ResolutionY = 0) then exit;
     SetChunkLength(sizeof(TPNGPhysicalDimensions));
     SetChunkType(ctpHYs);

     with PPNGPhysicalDimensions(ChunkDataBuffer)^ do
     begin
       if (ResolutionUnit=ruPixelsPerInch)
       then ResolutionUnit :=ruPixelsPerCentimeter;
       if (ResolutionUnit=ruPixelsPerCentimeter)
       then begin
              Unit_Specifier:=1;
              X_Pixels :=Trunc(ResolutionX*100);
              Y_Pixels :=Trunc(ResolutionY*100);
            end
       else begin //ruNone
              Unit_Specifier:=0;
              X_Pixels :=Trunc(ResolutionX);
              Y_Pixels :=Trunc(ResolutionY);
          end;

       {$IFDEF ENDIAN_LITTLE}
       X_Pixels :=swap(X_Pixels);
       Y_Pixels :=swap(Y_Pixels);
       {$ENDIF}
     end;

     WriteChunk;
  end;
end;

procedure TBGRAWriterPNG.InitWriteImageData;
begin
  FDatalineLength := FSourceImage.Width*ByteWidth;
  GetMem (FPreviousLine, FDatalineLength);
  GetMem (FCurrentLine, FDatalineLength);
  fillchar (FCurrentLine^,FDatalineLength,0);
  ZData := TMemoryStream.Create;
  Compressor := TCompressionStream.Create (FCompressionLevel,ZData);
  FGetPixel := DecideGetPixel;
  FGetPixelBGRA := DecideGetPixelBGRA;
end;

procedure TBGRAWriterPNG.FinalWriteImageData;
begin
  ZData.Free;
  FreeMem (FPreviousLine);
  FreeMem (FCurrentLine);
end;

function TBGRAWriterPNG.DetermineFilter (Current, Previous:PByteArray; linelength:LongWord) : byte;
begin
  result := 0;
end;

procedure TBGRAWriterPNG.FillScanLine (y : integer; ScanLine : pByteArray);
var x : integer;
    cd : TColorData;
    r, index : LongWord;
    b : byte;
    p : PBGRAPixel;
begin
  index := 0;
  if FSourceImage is TBGRACustomBitmap then
  begin
    p := TBGRACustomBitmap(FSourceImage).ScanLine[y];
    if FHeader.BitDepth <> 16 then
      case FByteWidth of
        1: for x := pred(FSourceImage.Width) downto 0 do
           begin
             cd := FGetPixelBGRA(p);
             ScanLine^[index] := cd;
             inc (index);
             inc(p);
           end;
        2: for x := pred(FSourceImage.Width) downto 0 do
           begin
             cd := FGetPixelBGRA(p);
             ScanLine^[index] := cd and $ff;
             ScanLine^[index+1] := cd shr 8;
             inc (index,2);
             inc(p);
           end;
        3: for x := pred(FSourceImage.Width) downto 0 do
           begin
             ScanLine^[index] := p^.red;
             ScanLine^[index+1] := p^.green;
             ScanLine^[index+2] := p^.blue;
             inc (index,3);
             inc(p);
           end;
        4: for x := pred(FSourceImage.Width) downto 0 do
           begin
             ScanLine^[index] := p^.red;
             ScanLine^[index+1] := p^.green;
             ScanLine^[index+2] := p^.blue;
             ScanLine^[index+3] := p^.alpha;
             inc (index,4);
             inc(p);
           end;
        else raise exception.Create('Unexpected byte width');
      end else
      for x := pred(FSourceImage.Width) downto 0 do
      begin
        cd := FGetPixelBGRA(p);
        {$IFDEF ENDIAN_BIG}
        cd:=swap(cd);
        {$ENDIF}
        move (cd, ScanLine^[index], FBytewidth);
        if WordSized then
        begin
          r := 0;
          while (r+1 < FByteWidth) do
            begin
            b := Scanline^[index+r+1];
            Scanline^[index+r+1] := Scanline^[index+r];
            Scanline^[index+r] := b;
            inc (r,2);
            end;
        end;
        inc (index, FByteWidth);
        inc(p);
      end;
  end
  else
  for x := 0 to pred(FSourceImage.Width) do
    begin
    cd := FGetPixel (x,y);
    {$IFDEF ENDIAN_BIG}
    cd:=swap(cd);
    {$ENDIF}
    move (cd, ScanLine^[index], FBytewidth);
    if WordSized then
      begin
      r := 0;
      while (r+1 < FByteWidth) do
        begin
        b := Scanline^[index+r+1];
        Scanline^[index+r+1] := Scanline^[index+r];
        Scanline^[index+r] := b;
        inc (r,2);
        end;
      end;
    inc (index, FByteWidth);
    end;
end;

procedure TBGRAWriterPNG.Gatherdata;
var x,y : integer;
    lf : byte;
begin
  for y := 0 to pred(FSourceImage.height) do
    begin
    FSwitchLine := FCurrentLine;
    FCurrentLine := FPreviousLine;
    FPreviousLine := FSwitchLine;
    FillScanLine (y, FCurrentLine);
    lf := DetermineFilter (FCurrentLine, FpreviousLine, FDataLineLength);
    if lf <> 0 then
      for x := 0 to FDatalineLength-1 do
        FCurrentLine^[x] := DoFilter (lf, x, FCurrentLine^[x]);
    Compressor.Write (lf, sizeof(lf));
    Compressor.Write (FCurrentLine^, FDataLineLength);
    end;
end;

procedure TBGRAWriterPNG.WriteCompressedData(AImageDataChunkCode: TChunkCode);
var l : LongWord;
begin
  Compressor.Free;  // Close compression and finish the writing in ZData
  l := ZData.position;
  ZData.position := 0;
  SetChunkLength(l);
  SetChunkType (AImageDataChunkCode);
  ZData.Read (ChunkdataBuffer^, l);
  WriteChunk;
end;

procedure TBGRAWriterPNG.WriteImageData(AImage: TFPCustomImage;
  AImageDataChunkCode: TChunkCode);
var sequenceNumber: DWord;
begin
  FSourceImage := AImage;
  InitWriteImageData;
  if AImageDataChunkCode = AnimatedChunkTypes[ctfdAT] then
  begin
    sequenceNumber := NtoBE(FAnimationChunkCount);
    inc(FAnimationChunkCount);
    ZData.WriteBuffer(sequenceNumber, sizeOf(sequenceNumber));
  end;
  GatherData;
  WriteCompressedData(AImageDataChunkCode);
  FinalWriteImageData;
  FSourceImage := nil;
end;

procedure TBGRAWriterPNG.WriteIDAT;
begin
  WriteImageData(TheImage, GetChunkCode(ctIDAT));
end;

procedure TBGRAWriterPNG.WritefdAT(AIndex: integer);
begin
  WriteImageData(FAnimation[AIndex].Image, AnimatedChunkTypes[ctfdAT]);
end;

procedure TBGRAWriterPNG.WritetRNS;
  procedure PaletteAlpha;
  var r : integer;
  begin
    with ThePalette do
      begin
      // search last palette entry with transparency
      r := count;
      repeat
        dec (r);
      until (r < 0) or (color[r].alpha <> alphaOpaque);
      if r >= 0 then // there is at least 1 transparent color
        begin
        // from this color we go to the first palette entry
        SetChunkLength (r+1);
        repeat
          chunkdatabuffer^[r] := (color[r].alpha shr 8);
          dec (r);
        until (r < 0);
        end;
      writechunk;
      end;
  end;
  procedure GrayAlpha;
  var g : word;
  begin
    SetChunkLength(2);
    if WordSized then
      g := CalculateGray (SingleTransparentColor)
    else
      g := hi (CalculateGray(SingleTransparentColor));
    {$IFDEF ENDIAN_LITTLE}
    g := swap (g);
    {$ENDIF}
    move (g,ChunkDataBuffer^[0],2);
    WriteChunk;
  end;
  procedure ColorAlpha;
  var g : TFPColor;
  begin
    SetChunkLength(6);
    g := SingleTransparentColor;
    with g do
      if WordSized then
        begin
        {$IFDEF ENDIAN_LITTLE}
        red := swap (red);
        green := swap (green);
        blue := swap (blue);
        {$ENDIF}
        move (g, ChunkDatabuffer^[0], 6);
        end
      else
        begin
        ChunkDataBuffer^[0] := 0;
        ChunkDataBuffer^[1] := red shr 8;
        ChunkDataBuffer^[2] := 0;
        ChunkDataBuffer^[3] := green shr 8;
        ChunkDataBuffer^[4] := 0;
        ChunkDataBuffer^[5] := blue shr 8;
        end;
    WriteChunk;
  end;
begin
  SetChunkType (cttRNS);
  case fheader.colortype of
    6,4 : raise PNGImageException.create ('tRNS chunk forbidden for full alpha channels');
    3 : PaletteAlpha;
    2 : ColorAlpha;
    0 : GrayAlpha;
  end;
end;

procedure TBGRAWriterPNG.WriteTexts;
begin
end;

procedure TBGRAWriterPNG.WriteacTL;
var animControl: TAnimationControlChunk;
begin
  animControl.FrameCount := NtoBE(longword(length(FAnimation)));
  if FRepeatCount >= 0 then
    animControl.RepeatCount := NtoBE(longword(FRepeatCount))
  else
    animControl.RepeatCount := 0;
  SetChunkType(AnimatedChunkTypes[ctacTL]);
  SetChunkLength(sizeof(TAnimationControlChunk));
  move (animControl, ChunkDataBuffer^, sizeof(TAnimationControlChunk));
  WriteChunk;
end;

procedure TBGRAWriterPNG.WritefcTL(AIndex: integer);
var
  frameControl: TFrameControlChunk;
begin
  frameControl := FAnimation[AIndex].FrameControl;
  frameControl.SequenceNumber:= NtoBE(FAnimationChunkCount);
  inc(FAnimationChunkCount);
  frameControl.Width:= NtoBE(frameControl.Width);
  frameControl.Height:= NtoBE(frameControl.Height);
  frameControl.OffsetX:= NtoBE(frameControl.OffsetX);
  frameControl.OffsetY:= NtoBE(frameControl.OffsetY);
  frameControl.DelayNum:= NtoBE(frameControl.DelayNum);
  frameControl.DelayDenom:= NtoBE(frameControl.DelayDenom);
  SetChunkType(AnimatedChunkTypes[ctfcTL]);
  SetChunkLength(sizeof(TFrameControlChunk));
  move (frameControl, ChunkDataBuffer^, sizeof(TFrameControlChunk));
  WriteChunk;
end;

procedure TBGRAWriterPNG.WriteIEND;
begin
  SetChunkLength(0);
  SetChunkType (ctIEND);
  WriteChunk;
end;

procedure TBGRAWriterPNG.InternalWrite (Str:TStream; Img:TFPCustomImage);
var mainImageIndex, i: integer;
begin
  WriteIHDR;
  if Fheader.colorType = 3 then
    WritePLTE;
  if FUsetRNS then
    WritetRNS;
  WriteResolutionValues;
  WriteTexts;

  FAnimationChunkCount := 0;
  mainImageIndex := -1;
  for i := 0 to high(FAnimation) do
    if FAnimation[i].Image = Img then
      mainImageIndex := i;

  if length(FAnimation) > 0 then
    WriteacTL;

  if mainImageIndex = -1 then
    WriteIDAT;

  if length(FAnimation) > 0 then
  begin
    for i := 0 to high(FAnimation) do
    begin
      WritefcTL(i);
      if i = mainImageIndex then
        WriteIDAT
      else
        WritefdAT(i)
    end;
  end;
  WriteIEND;
end;

function TBGRAWriterPNG.GetUseAlpha: boolean;
begin
  result := FUseAlpha;
end;

procedure TBGRAWriterPNG.SetUseAlpha(AValue: boolean);
begin
  FUseAlpha := AValue;
end;

initialization

  DefaultBGRAImageWriter[ifPng] := TBGRAWriterPNG;

end.
