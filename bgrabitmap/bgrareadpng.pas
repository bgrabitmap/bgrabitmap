// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
    This file is originally part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    PNG reader implementation modified by circular.

 **********************************************************************

  Optimisations applied:
  - using "const" parameter for TColorData
  - direct pixel access with TBGRABitmap when possible
  - some fixes of hints and of initializations
  - vertical shrink option with MinifyHeight, OriginalHeight and VerticalShrinkFactor (useful for thumbnails)
 }
{*****************************************************************************}
{
  2023-06  - Massimo Magnano
           - added Resolution support
}
{*****************************************************************************}
{$mode objfpc}{$h+}

{ PNG reader implementation }
unit BGRAReadPng;

interface

uses
  SysUtils, BGRAClasses, FPImage, FPImgCmn, BGRAPNGComn, ZStream, BGRABitmapTypes, fgl;

Type
  TSetPixelProc = procedure (x,y:integer; const CD : TColordata) of object;
  TConvertColorProc = function (const CD:TColorData) : TFPColor of object;
  TBGRAConvertColorProc = function (const CD:TColorData) : TBGRAPixel of object;
  THandleScanLineProc = procedure (const y : integer; const ScanLine : PByteArray) of object;

  { Frame in an animated PNG stream }
  TPNGFrame = class
    FrameControl: TFrameControlChunk;
    FrameData: TMemoryStream;
    constructor Create;
    destructor Destroy; override;
  end;
  TPNGFrameList = specialize TFPGObjectList<TPNGFrame>;

  { Reader for PNG image format }
  TBGRAReaderPNG = class (TBGRAImageReader)
    private
      FHeader : THeaderChunk;
      FTargetImage: TFPCustomImage;   // target image being decompressed
      FMainImageData: TMemoryStream;  // holds compressed data until all blocks are read
      FMainImageFrameIndex: integer;  // index of the frame containing the main image or -1
      FFrames : TPNGFrameList;
      FFrameCount: integer;
      FLoopCount: integer;
      FIndexed : boolean;             // if palette is used
      FCountScanlines : EightLong; // Number of scanlines to process for each pass
      FScanLineLength : EightLong; // Length of scanline for each pass
      FCurrentPass : byte;
      ByteWidth : byte;          // number of bytes to read for pixel information
      BitsUsed : EightLong;      // bitmasks to use to split a byte into smaller parts
      BitShift : byte;           // shift right to do of the bits extracted with BitsUsed for 1 element
      CountBitsUsed : byte;      // number of bit groups (1 pixel) per byte (when bytewidth = 1)
      StartX,StartY, DeltaX,DeltaY, StartPass,EndPass : integer;  // number and format of passes
      FPalette : TFPPalette;
      FSetPixel : TSetPixelProc;
      FConvertColor : TConvertColorProc;
      FBGRAConvertColor : TBGRAConvertColorProc;
      FHandleScanLine: THandleScanLineProc;
      FMinifyHeight: integer;
      FVerticalShrinkMask: LongWord;
      FVerticalShrinkShr: Integer;
      FGammaCorrection: single;
      FGammaCorrectionTable: packed array of word;
      FGammaCorrectionTableComputed: boolean;
      FResolutionUnit: TResolutionUnit;
      FResolutionX, FResolutionY : single;
      function GetFrameControl(AIndex: Integer): TFrameControlChunk;
      function GetOriginalHeight: integer;
      function GetOriginalWidth: integer;
      function GetVerticalShrinkFactor: integer;
      function ReadChunk: boolean;
      procedure InvalidChunkLength;
      function ColorGray1 (const CD:TColorData) : TFPColor;
      function ColorGray2 (const CD:TColorData) : TFPColor;
      function ColorGray4 (const CD:TColorData) : TFPColor;
      function ColorGray8 (const CD:TColorData) : TFPColor;
      function ColorGray16 (const CD:TColorData) : TFPColor;
      function ColorGrayAlpha8 (const CD:TColorData) : TFPColor;
      function ColorGrayAlpha16 (const CD:TColorData) : TFPColor;
      function ColorColor8 (const CD:TColorData) : TFPColor;
      function ColorColor16 (const CD:TColorData) : TFPColor;
      function ColorColorAlpha8 (const CD:TColorData) : TFPColor;
      function ColorColorAlpha16 (const CD:TColorData) : TFPColor;
      function CheckGammaCorrection: boolean;
      procedure ApplyGammaCorrection(var AColor: TFPColor);

      function BGRAColorGray1 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGray2 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGray4 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGray8 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGray16 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGrayAlpha8 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGrayAlpha16 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorColor8 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorColor16 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorColorAlpha8 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorColorAlpha16 (const CD:TColorData) : TBGRAPixel;
    protected
      Chunk : TChunk;
      UseTransparent, EndOfFile : boolean;
      TransparentDataValue : TColorData;
      UsingBitGroup : byte;
      DataIndex : LongWord;
      DataBytes : TColorData;

      procedure HandleChunk; virtual;
      procedure HandlePalette; virtual;
      procedure HandleAlpha; virtual;
      procedure HandlePhysicalDimensions; virtual;
      procedure HandleStdRGB; virtual;
      procedure HandleGamma; virtual;
      procedure HandleData; virtual;
      procedure HandleUnknown; virtual;

      procedure HandleAnimationControl; virtual;
      procedure HandleFrameControl; virtual;
      procedure HandleFrameData; virtual;

      procedure PredefinedResolutionValues; virtual;
      procedure AssignPalette;
      procedure AssignResolutionValues;
      procedure DoLoadImage(AImage: TFPCustomImage; AData: TStream; AWidth, AHeight: integer); virtual;

      procedure DoDecompress(ACompressedData: TStream; AWidth, AHeight: integer); virtual;
      function CalcColor(const ScanLine : PByteArray): TColorData;
      procedure HandleScanLine (const y : integer; const ScanLine : PByteArray); virtual;
      procedure BGRAHandleScanLine(const y: integer; const ScanLine: PByteArray);
      procedure BGRAHandleScanLineTr(const y: integer; const ScanLine: PByteArray);
      procedure SetPalettePixel (x,y:integer; const CD : TColordata);
      procedure SetPalColPixel (x,y:integer; const CD : TColordata);
      procedure SetColorPixel (x,y:integer; const CD : TColordata);
      procedure SetColorTrPixel (x,y:integer; const CD : TColordata);
      procedure SetBGRAColorPixel (x,y:integer; const CD : TColordata);
      procedure SetBGRAColorTrPixel (x,y:integer; const CD : TColordata);
      function DecideSetPixel : TSetPixelProc; virtual;
      property ConvertColor : TConvertColorProc read FConvertColor;

      procedure InternalRead  ({%H-}Str:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Str:TStream) : boolean; override;

      property Header : THeaderChunk read FHeader;
      property CurrentPass : byte read FCurrentPass;
      property CountScanlines : EightLong read FCountScanlines;
      property ScanLineLength : EightLong read FScanLineLength;
    public
      constructor Create; override;
      destructor Destroy; override;
      function GetQuickInfo(AStream: TStream): TQuickImageInfo; override;
      function GetBitmapDraft(AStream: TStream; {%H-}AMaxWidth, AMaxHeight: integer;
        out AOriginalWidth,AOriginalHeight: integer): TBGRACustomBitmap; override;
      procedure LoadFrame(AIndex: integer; AImage: TFPCustomImage);

      // image format
      property OriginalWidth: integer read GetOriginalWidth;
      property OriginalHeight: integer read GetOriginalHeight;
      property Indexed : boolean read FIndexed;
      property ThePalette : TFPPalette read FPalette;

      // thumbnail reduction
      property MinifyHeight: integer read FMinifyHeight write FMinifyHeight;
      property VerticalShrinkFactor: integer read GetVerticalShrinkFactor;

      // animation
      property FrameCount : integer read FFrameCount;
      property LoopCount : integer read FLoopCount;
      property FrameControl[AIndex: Integer]: TFrameControlChunk read GetFrameControl;
      property MainImageFrameIndex: integer read FMainImageFrameIndex;
  end;

implementation

uses math;

const StartPoints : array[0..7, 0..1] of word =
         ((0,0),(0,0),(4,0),(0,4),(2,0),(0,2),(1,0),(0,1));
      Delta : array[0..7,0..1] of word =
         ((1,1),(8,8),(8,8),(4,8),(4,4),(2,4),(2,2),(1,2));
      BitsUsed1Depth : EightLong = ($80,$40,$20,$10,$08,$04,$02,$01);
      BitsUsed2Depth : EightLong = ($C0,$30,$0C,$03,0,0,0,0);
      BitsUsed4Depth : EightLong = ($F0,$0F,0,0,0,0,0,0);

{ TPNGFrame }

constructor TPNGFrame.Create;
begin
  FrameData := nil;
end;

destructor TPNGFrame.Destroy;
begin
  inherited Destroy;
  FrameData.Free;
end;

constructor TBGRAReaderPNG.Create;
begin
  inherited;
  chunk.acapacity := 0;
  chunk.data := nil;
  FMainImageData := nil;
  FMainImageFrameIndex := -1;
  UseTransparent := False;
  FFrames := TPNGFrameList.Create;
  FFrameCount := 0;
  FLoopCount := 0;
  FTargetImage := nil;
end;

destructor TBGRAReaderPNG.Destroy;
begin
  with chunk do
    if acapacity > 0 then
      freemem (data);
  FFrames.Free;
  FPalette.Free;
  inherited;
end;

function TBGRAReaderPNG.GetQuickInfo(AStream: TStream): TQuickImageInfo;
const headerChunkSize = 13;
var
  {%H-}FileHeader : packed array[0..7] of byte;
  {%H-}ChunkHeader : TChunkHeader;
  {%H-}HeaderChunk : THeaderChunk;
begin
  {$PUSH}{$HINTS OFF}fillchar({%H-}result, sizeof({%H-}result), 0);{$POP}
  if (AStream.Read({%H-}FileHeader, sizeof(FileHeader)) <> sizeof(FileHeader))
    or not CheckSignature(FileHeader) then exit;
  if AStream.Read({%H-}ChunkHeader, sizeof(ChunkHeader)) <> sizeof(ChunkHeader) then exit;
  if ChunkHeader.CType <> GetChunkCode(ctIHDR) then exit;
  if BEtoN(ChunkHeader.CLength) < headerChunkSize then exit;
  if AStream.Read({%H-}HeaderChunk, headerChunkSize) <> headerChunkSize then exit;
  result.width:= BEtoN(HeaderChunk.Width);
  result.height:= BEtoN(HeaderChunk.height);
  case HeaderChunk.ColorType and 3 of
    0,3: {grayscale, palette}
      if HeaderChunk.BitDepth > 8 then
        result.colorDepth := 8
      else
        result.colorDepth := HeaderChunk.BitDepth;

    2: {color} result.colorDepth := HeaderChunk.BitDepth*3;
  end;
  if (HeaderChunk.ColorType and 4) = 4 then
    result.alphaDepth := HeaderChunk.BitDepth
  else
    result.alphaDepth := 0;
end;

function TBGRAReaderPNG.GetBitmapDraft(AStream: TStream; AMaxWidth,
  AMaxHeight: integer; out AOriginalWidth, AOriginalHeight: integer): TBGRACustomBitmap;
var
  png: TBGRAReaderPNG;
begin
  png:= TBGRAReaderPNG.Create;
  result := BGRABitmapFactory.Create;
  try
    png.MinifyHeight := AMaxHeight;
    result.LoadFromStream(AStream, png);
    AOriginalWidth:= png.OriginalWidth;
    AOriginalHeight:= png.OriginalHeight;
  finally
    png.Free;
  end;
end;

procedure TBGRAReaderPNG.LoadFrame(AIndex: integer; AImage: TFPCustomImage);
begin
  with FFrames[AIndex] do
    DoLoadImage(AImage, FrameData, FrameControl.Width, FrameControl.Height);
end;

function TBGRAReaderPNG.ReadChunk: boolean;
var {%H-}ChunkHeader : TChunkHeader;
    readCRC : LongWord;
    l : LongWord;
    readCount: integer;
begin
  readCount := TheStream.Read ({%H-}ChunkHeader,sizeof(ChunkHeader));
  if readCount <> sizeof(TChunkHeader) then
    raise PNGImageException.Create('Unexpected end of stream when reading next chunk');
  with chunk do
    begin
    // chunk header
    with ChunkHeader do
      begin
      {$IFDEF ENDIAN_LITTLE}
      alength := swap(CLength);
      {$ELSE}
      alength := CLength;
      {$ENDIF}
      ReadType := CType;
      end;
    aType := GetChunkType(ReadType);
    if alength > MaxChunkLength then
      raise PNGImageException.Create ('Length exceed maximum for ' + GetChunkCode(aType) + ' chunk');
    if alength > acapacity then
      begin
      if acapacity > 0 then
        freemem (data);
      GetMem (data, alength);
      acapacity := alength;
      end;
    l := TheStream.read (data^, alength);
    if l <> alength then
      raise PNGImageException.Create ('Length exceeds stream length for ' + GetChunkCode(aType) + ' chunk');
    readCRC := 0;
    TheStream.Read (readCRC, sizeof(ReadCRC));
    l := CalculateChunkCRC(ReadType, data, alength);
    {$IFDEF ENDIAN_LITTLE}
    l := swap(l);
    {$ENDIF}
    if ReadCRC <> l then
      begin
        //if chunk is essential, then raise an error
        if ReadType[0] = upcase(ReadType[0]) then
          raise PNGImageException.Create ('CRC check failed for ' + GetChunkCode(aType) + ' chunk')
        else
          result := false;
      end
      else result := true;
    end;
end;

procedure TBGRAReaderPNG.InvalidChunkLength;
begin
  raise PNGImageException.Create('Invalid length for ' + GetChunkCode(chunk.atype) + ' chunk');
end;

function TBGRAReaderPNG.GetVerticalShrinkFactor: integer;
begin
  result := 1 shl FVerticalShrinkShr;
end;

function TBGRAReaderPNG.GetOriginalHeight: integer;
begin
  result := Header.height;
end;

function TBGRAReaderPNG.GetFrameControl(AIndex: Integer): TFrameControlChunk;
begin
  if not Assigned(FFrames) then
    raise PNGImageException.Create('This PNG is not animated');
  result := FFrames[AIndex].FrameControl;
end;

function TBGRAReaderPNG.GetOriginalWidth: integer;
begin
  result := header.Width;
end;

procedure TBGRAReaderPNG.HandleData;
var
  frame: TPNGFrame;
begin
  if not Assigned(FMainImageData) then
  begin
    FMainImageData := TMemoryStream.Create;

    // main image can also be a frame in the animation
    if (FFrames.Count > 0) and not Assigned(FFrames.Last.FrameData) then
    begin
      frame := FFrames.Last;
      if (frame.FrameControl.Width <> Header.Width) or
         (frame.FrameControl.Height <> Header.Height) or
         (frame.FrameControl.OffsetX <> 0) or
         (frame.FrameControl.OffsetY <> 0) then
        raise PNGImageException.Create('Main frame must match the image dimensions');

      frame.FrameData := FMainImageData;
      FMainImageFrameIndex := FFrames.Count - 1;
    end else
      FMainImageFrameIndex := -1;
  end;
  FMainImageData.WriteBuffer(chunk.Data^, chunk.aLength);
end;

procedure TBGRAReaderPNG.HandleAlpha;
  procedure PaletteAlpha;
    var r : integer;
        a : word;
        c : TFPColor;
    begin
      with chunk do
        begin
        if alength > LongWord(ThePalette.count) then
          raise PNGImageException.create ('To much alpha values for palette');
        for r := 0 to alength-1 do
          begin
          c := ThePalette[r];
          a := data^[r];
          c.alpha := (a shl 8) + a;
          ThePalette[r] := c;
          end;
        end;
    end;
  procedure TransparentGray;
    var {%H-}a : word;
    begin
      move (chunk.data^[0], {%H-}a, 2);
      {$IFDEF ENDIAN_LITTLE}
      a := swap (a);
      {$ENDIF}
      TransparentDataValue := a;
      UseTransparent := True;
    end;
  procedure TransparentColor;
    var d : byte;
        {%H-}r,{%H-}g,{%H-}b : word;
        a : TColorData;
    begin
      with chunk do
        begin
        move (data^[0], {%H-}r, 2);
        move (data^[2], {%H-}g, 2);
        move (data^[4], {%H-}b, 2);
        end;
      {$IFDEF ENDIAN_LITTLE}
      r := swap (r);
      g := swap (g);
      b := swap (b);
      {$ENDIF}
      d := header.bitdepth;
      a := (TColorData(b) shl d) shl d;
      a := a + (TColorData(g) shl d) + r;
      TransparentDataValue := a;
      UseTransparent := True;
    end;
begin
  case header.ColorType of
    3 : PaletteAlpha;
    0 : TransparentGray;
    2 : TransparentColor;
  end;
end;

procedure TBGRAReaderPNG.HandleStdRGB;
begin
  FGammaCorrection:= 1;
  FGammaCorrectionTableComputed:= false;
end;

procedure TBGRAReaderPNG.HandleGamma;
var
  invGammaInt: Longword;
begin
  invGammaInt := BEtoN(PLongword(chunk.data)^);
  FGammaCorrection:= invGammaInt/45455;  { 1/2.2 is default }
  FGammaCorrectionTableComputed:= false;
end;

procedure TBGRAReaderPNG.HandleAnimationControl;
begin
  if chunk.alength < sizeof(TAnimationControlChunk) then
    InvalidChunkLength;
  with PAnimationControlChunk(chunk.data)^ do
  begin
    FFrameCount:= BEtoN(FrameCount);
    if FFrameCount < 1 then raise PNGImageException.Create('Invalid frame count');
    FLoopCount:= BEtoN(RepeatCount);
    if FLoopCount < 0 then raise PNGImageException.Create('Invalid loop count');
  end;
end;

procedure TBGRAReaderPNG.HandleFrameControl;
var
  frame: TPNGFrame;
begin
  if FFrameCount = 0 then exit; // ignore if animation not specified

  if FFrames.Count >= FFrameCount then
    raise PNGImageException.Create('Actual frame count exceed defined count');
  if Chunk.alength < sizeof(TFrameControlChunk) then
    InvalidChunkLength;

  frame := TPNGFrame.Create;
  frame.FrameControl := PFrameControlChunk(Chunk.data)^;
  frame.FrameControl.Width:= BEtoN(frame.FrameControl.Width);
  frame.FrameControl.Height:= BEtoN(frame.FrameControl.Height);
  frame.FrameControl.OffsetX:= BEtoN(frame.FrameControl.OffsetX);
  frame.FrameControl.OffsetY:= BEtoN(frame.FrameControl.OffsetY);
  frame.FrameControl.DelayNum:= BEtoN(frame.FrameControl.DelayNum);
  frame.FrameControl.DelayDenom:= BEtoN(frame.FrameControl.DelayDenom);
  if frame.FrameControl.DelayDenom = 0 then
    frame.FrameControl.DelayDenom:= 100;
  FFrames.Add(frame);
end;

procedure TBGRAReaderPNG.HandleFrameData;
var
  frame: TPNGFrame;
begin
  if FFrameCount = 0 then exit; // ignore if animation not specified

  if FFrames.Count = 0 then
    raise PNGImageException.Create('Missing Frame Control chunk');
  if Chunk.alength < sizeof(TFrameDataChunk) then
    InvalidChunkLength;

  frame := FFrames.Last;
  if not Assigned(frame.FrameData) then
    frame.FrameData := TMemoryStream.Create;
  frame.FrameData.WriteBuffer((PByte(chunk.data) + sizeof(TFrameDataChunk))^,
                              chunk.alength - sizeof(TFrameDataChunk));
end;

procedure TBGRAReaderPNG.PredefinedResolutionValues;
begin
  //According to Standard: If the pHYs chunk is not present, pixels are assumed to be square
  FResolutionUnit :=ruNone;
  FResolutionX :=1;
  FResolutionY :=1;
end;

procedure TBGRAReaderPNG.HandlePhysicalDimensions;
begin
  if (chunk.alength<>sizeof(TPNGPhysicalDimensions))
  then InvalidChunkLength;

  if (PPNGPhysicalDimensions(chunk.data)^.Unit_Specifier = 1)
  then FResolutionUnit :=ruPixelsPerCentimeter
  else FResolutionUnit :=ruNone;

  FResolutionX :=BEtoN(PPNGPhysicalDimensions(chunk.data)^.X_Pixels)/100;
  FResolutionY :=BEtoN(PPNGPhysicalDimensions(chunk.data)^.Y_Pixels)/100;
end;

procedure TBGRAReaderPNG.AssignPalette;
begin
  if Assigned(FPalette) and FTargetImage.UsePalette then
    FTargetImage.Palette.Copy(FPalette);
end;

procedure TBGRAReaderPNG.AssignResolutionValues;
begin
  {$IF FPC_FULLVERSION<30203}
  if (FTargetImage is TCustomUniversalBitmap) then
  with TCustomUniversalBitmap(FTargetImage) do
  {$ELSE}
  with FTargetImage do
  {$ENDIF}
  begin
    ResolutionUnit := FResolutionUnit;
    ResolutionX:= FResolutionX;
    ResolutionY:= FResolutionY;
  end;
end;

procedure TBGRAReaderPNG.HandlePalette;
var r : LongWord;
    c : TFPColor;
    t : word;
begin
  if header.colortype = 3 then
    with chunk do
    begin
      if assigned(FPalette) then
        raise PNGImageException.Create('Palette specified multiple times');
      FPalette := TFPPalette.Create(0);
      c.Alpha := AlphaOpaque;
      if (aLength mod 3) > 0 then
        InvalidChunkLength;
      r := 0;
      ThePalette.count := 0;
      while r < alength do
      begin
        t := data^[r];
        c.red := t + (t shl 8);
        inc (r);
        t := data^[r];
        c.green := t + (t shl 8);
        inc (r);
        t := data^[r];
        c.blue := t + (t shl 8);
        inc (r);
        ApplyGammaCorrection(c);
        ThePalette.Add (c);
      end;
    end;
end;

procedure TBGRAReaderPNG.SetPalettePixel (x,y:integer; const CD : TColordata);
begin  // both PNG and Img have palette
  FTargetImage.Pixels[x,y] := CD;
end;

procedure TBGRAReaderPNG.SetPalColPixel (x,y:integer; const CD : TColordata);
begin  // PNG with palette, Img without
  FTargetImage.Colors[x,y] := ThePalette[CD];
end;

procedure TBGRAReaderPNG.SetColorPixel (x,y:integer; const CD : TColordata);
var c : TFPColor;
begin  // both PNG and Img work without palette, and no transparency colordata
  // c := ConvertColor (CD,CFmt);
  c := ConvertColor (CD);
  ApplyGammaCorrection(c);
  FTargetImage.Colors[x,y] := c;
end;

procedure TBGRAReaderPNG.SetColorTrPixel (x,y:integer; const CD : TColordata);
var c : TFPColor;
begin  // both PNG and Img work without palette, and there is a transparency colordata
  //c := ConvertColor (CD,CFmt);
  c := ConvertColor (CD);
  ApplyGammaCorrection(c);
  if TransparentDataValue = CD then
    c.alpha := alphaTransparent;
  FTargetImage.Colors[x,y] := c;
end;

procedure TBGRAReaderPNG.SetBGRAColorPixel(x, y: integer; const CD: TColordata);
var c: TBGRAPixel;
begin
  c := FBGRAConvertColor(CD);
  if c.alpha = 0 then TBGRACustomBitmap(FTargetImage).SetPixel(x,y,BGRAPixelTransparent)
  else TBGRACustomBitmap(FTargetImage).SetPixel(x,y,c);
end;

procedure TBGRAReaderPNG.SetBGRAColorTrPixel(x, y: integer; const CD: TColordata);
var c: TBGRAPixel;
begin
  if TransparentDataValue = CD then
    TBGRACustomBitmap(FTargetImage).SetPixel(x,y,BGRAPixelTransparent) else
  begin
    c := FBGRAConvertColor(CD);
    if c.alpha = 0 then TBGRACustomBitmap(FTargetImage).SetPixel(x,y,BGRAPixelTransparent)
    else TBGRACustomBitmap(FTargetImage).SetPixel(x,y,c);
  end;
end;

function TBGRAReaderPNG.DecideSetPixel : TSetPixelProc;
begin
  if Indexed then
    if FTargetImage.UsePalette then
      result := @SetPalettePixel
    else
      result := @SetPalColPixel
  else
    if UseTransparent then
    begin
      if FTargetImage is TBGRACustomBitmap then
        result := @SetBGRAColorTrPixel
      else
        result := @SetColorTrPixel
    end
    else
    begin
      if FTargetImage is TBGRACustomBitmap then
        result := @SetBGRAColorPixel
      else
        result := @SetColorPixel
    end;
end;

function TBGRAReaderPNG.CalcColor(const ScanLine : PByteArray): TColorData;
var cd : LongWord;
    r : word;
    p : pbyte;
begin
  if UsingBitGroup = 0 then
    begin
    Databytes := 0;
    if Header.BitDepth = 16 then
      begin
       p := @Databytes;
       for r:=0 to bytewidth shr 1 - 1 do
       begin
        p^ := ScanLine^[Dataindex+(r shl 1)+1];
        (p+1)^ := ScanLine^[Dataindex+(r shl 1)];
        inc(p,2);
       end;
      end
    else move (ScanLine^[DataIndex], Databytes, bytewidth);
    {$IFDEF ENDIAN_BIG}
    Databytes:=swap(Databytes);
    {$ENDIF}
    inc (DataIndex,bytewidth);
    end;
  if bytewidth = 1 then
    begin
    cd := (Databytes and BitsUsed[UsingBitGroup]);
    result := cd shr ((CountBitsUsed-UsingBitGroup-1) * BitShift);
    inc (UsingBitgroup);
    if UsingBitGroup >= CountBitsUsed then
      UsingBitGroup := 0;
    end
  else
    result := Databytes;
end;

procedure TBGRAReaderPNG.HandleScanLine (const y : integer; const ScanLine : PByteArray);
var x, rx : integer;
    c : TColorData;
begin
  UsingBitGroup := 0;
  DataIndex := 0;
  X := StartX;
  if (UsingBitGroup = 0) and (Header.BitDepth <> 16) then
    case ByteWidth of
      1: if BitsUsed[0] = $ff then
         begin
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             FSetPixel (x,y,ScanLine^[DataIndex]);
             Inc(X, deltaX);
             inc(DataIndex);
           end;
           exit;
         end;
      2: begin
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             {$IFDEF ENDIAN_BIG}
             FSetPixel (x,y,swap(PWord(@ScanLine^[DataIndex])^));
             {$ELSE}
             FSetPixel (x,y,PWord(@ScanLine^[DataIndex])^);
             {$ENDIF}
             Inc(X, deltaX);
             inc(DataIndex,2);
           end;
           exit;
         end;
      4: begin
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             {$IFDEF ENDIAN_BIG}
             FSetPixel (x,y,swap(PLongWord(@ScanLine^[DataIndex])^));
             {$ELSE}
             FSetPixel (x,y,PLongWord(@ScanLine^[DataIndex])^);
             {$ENDIF}
             Inc(X, deltaX);
             inc(DataIndex,4);
           end;
           exit;
         end;
      8: begin
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             {$IFDEF ENDIAN_BIG}
             FSetPixel (x,y,swap(PQWord(@ScanLine^[DataIndex])^));
             {$ELSE}
             FSetPixel (x,y,PQWord(@ScanLine^[DataIndex])^);
             {$ENDIF}
             Inc(X, deltaX);
             inc(DataIndex,8);
           end;
           exit;
         end;
    end;

  for rx := 0 to ScanlineLength[CurrentPass]-1 do
    begin
    c := CalcColor(ScanLine);
    FSetPixel (x,y,c);
    Inc(X, deltaX);
    end
end;

procedure TBGRAReaderPNG.BGRAHandleScanLine (const y : integer; const ScanLine : PByteArray);
var rx : integer;
    pdest: PBGRAPixel;
begin
  UsingBitGroup := 0;
  DataIndex := 0;
  {$PUSH}{$RANGECHECKS OFF} //because PByteArray is limited to 32767
  if (UsingBitGroup = 0) and (Header.BitDepth <> 16) then
    case ByteWidth of
      1: if BitsUsed[0] = $ff then
         begin
           pdest := TBGRACustomBitmap(FTargetImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             pdest^ := FBGRAConvertColor(ScanLine^[DataIndex]);
             if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
             Inc(pdest, deltaX);
             inc(DataIndex);
           end;
           exit;
         end;
      2: begin
           pdest := TBGRACustomBitmap(FTargetImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             pdest^ := FBGRAConvertColor(
             {$IFDEF ENDIAN_BIG}
             swap(PWord(@ScanLine^[DataIndex])^)
             {$ELSE}
             PWord(@ScanLine^[DataIndex])^
             {$ENDIF} );
             if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
             Inc(pdest, deltaX);
             inc(DataIndex,2);
           end;
           exit;
         end;
      3: begin
           pdest := TBGRACustomBitmap(FTargetImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             pdest^.red := ScanLine^[DataIndex];
             pdest^.green := ScanLine^[DataIndex+1];
             pdest^.blue := ScanLine^[DataIndex+2];
             pdest^.alpha := 255;
             Inc(pdest, deltaX);
             inc(DataIndex, 3);
           end;
           exit;
         end;
      4: begin
           pdest := TBGRACustomBitmap(FTargetImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             pdest^ := FBGRAConvertColor(
             {$IFDEF ENDIAN_BIG}
             swap(PLongWord(@ScanLine^[DataIndex])^)
             {$ELSE}
             PLongWord(@ScanLine^[DataIndex])^
             {$ENDIF}  );
             if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
             Inc(pdest, deltaX);
             inc(DataIndex,4);
           end;
           exit;
         end;
    end;
  {$POP}

  pdest := TBGRACustomBitmap(FTargetImage).ScanLine[y]+StartX;
  for rx := 0 to ScanlineLength[CurrentPass]-1 do
    begin
    pdest^ := FBGRAConvertColor(CalcColor(ScanLine));
    Inc(pdest, deltaX);
    end
end;

procedure TBGRAReaderPNG.BGRAHandleScanLineTr(const y: integer;
  const ScanLine: PByteArray);
var rx : integer;
    c : TColorData;
    pdest: PBGRAPixel;
begin
  UsingBitGroup := 0;
  DataIndex := 0;
  if (UsingBitGroup = 0) and (Header.BitDepth <> 16) then
    case ByteWidth of
      1: if BitsUsed[0] = $ff then
         begin
           pdest := TBGRACustomBitmap(FTargetImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             c := ScanLine^[DataIndex];
             if c = TransparentDataValue then
               pdest^ := BGRAPixelTransparent else
               begin
                 pdest^ := FBGRAConvertColor(c);
                 if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
             end;
             Inc(pdest, deltaX);
             inc(DataIndex);
           end;
           exit;
         end;
      2: begin
           pdest := TBGRACustomBitmap(FTargetImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             c :=
             {$IFDEF ENDIAN_BIG}
             swap(PWord(@ScanLine^[DataIndex])^)
             {$ELSE}
             PWord(@ScanLine^[DataIndex])^
             {$ENDIF} ;
             if c = TransparentDataValue then
               pdest^ := BGRAPixelTransparent else
               begin
                 pdest^ := FBGRAConvertColor(c);
                 if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
               end;
             Inc(pdest, deltaX);
             inc(DataIndex,2);
           end;
           exit;
         end;
      4: begin
           pdest := TBGRACustomBitmap(FTargetImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             c :=
             {$IFDEF ENDIAN_BIG}
             swap(PLongWord(@ScanLine^[DataIndex])^)
             {$ELSE}
             PLongWord(@ScanLine^[DataIndex])^
             {$ENDIF}  ;
             if c = TransparentDataValue then
               pdest^ := BGRAPixelTransparent else
               begin
                 pdest^ := FBGRAConvertColor(c);
                 if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
               end;
             Inc(pdest, deltaX);
             inc(DataIndex,4);
           end;
           exit;
         end;
      8: begin
           pdest := TBGRACustomBitmap(FTargetImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             c :=
             {$IFDEF ENDIAN_BIG}
             swap(PQWord(@ScanLine^[DataIndex])^)
             {$ELSE}
             PQWord(@ScanLine^[DataIndex])^
             {$ENDIF}  ;
             if c = TransparentDataValue then
               pdest^ := BGRAPixelTransparent else
               begin
                 pdest^ := FBGRAConvertColor(c);
                 if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
               end;
             Inc(pdest, deltaX);
             inc(DataIndex,8);
           end;
           exit;
         end;
    end;

  pdest := TBGRACustomBitmap(FTargetImage).ScanLine[y]+StartX;
  for rx := 0 to ScanlineLength[CurrentPass]-1 do
    begin
    c := CalcColor(ScanLine);
    if c = TransparentDataValue then
      pdest^ := BGRAPixelTransparent
      else pdest^ := FBGRAConvertColor(c);
    Inc(pdest, deltaX);
    end
end;

procedure TBGRAReaderPNG.DoLoadImage(AImage: TFPCustomImage; AData: TStream; AWidth, AHeight: integer);
var
  outputHeight: Integer;
begin
  if not Assigned(AData) then
    raise PNGImageException.Create('Image data not present');
  FTargetImage := AImage;
  outputHeight := (AHeight + FVerticalShrinkMask) shr FVerticalShrinkShr;
  FTargetImage.SetSize(AWidth, outputHeight);
  AssignResolutionValues;
  AssignPalette;
  AData.position:= 0;
  DoDecompress(AData, AWidth, AHeight);
  FTargetImage := nil;
end;

function TBGRAReaderPNG.ColorGray1(const CD: TColorData): TFPColor;
begin
  if CD = 0 then
    result := colBlack
  else
    result := colWhite;
end;

function TBGRAReaderPNG.ColorGray2(const CD: TColorData): TFPColor;
var c : UInt32or64;
begin
  c := CD and 3;
  c := c + (c shl 2);
  c := c + (c shl 4);
  c := c + (c shl 8);
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := alphaOpaque;
    end;
end;

function TBGRAReaderPNG.ColorGray4(const CD: TColorData): TFPColor;
var c : UInt32or64;
begin
  c := CD and $F;
  c := c + (c shl 4);
  c := c + (c shl 8);
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := alphaOpaque;
    end;
end;

function TBGRAReaderPNG.ColorGray8(const CD: TColorData): TFPColor;
var c : UInt32or64;
begin
  c := CD and $FF;
  c := c + (c shl 8);
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := alphaOpaque;
    end;
end;

function TBGRAReaderPNG.ColorGray16(const CD: TColorData): TFPColor;
var c : UInt32or64;
begin
  c := CD and $FFFF;
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := alphaOpaque;
    end;
end;

function TBGRAReaderPNG.ColorGrayAlpha8 (const CD:TColorData) : TFPColor;
var c : UInt32or64;
begin
  c := CD and $00FF;
  c := c + (c shl 8);
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    c := CD and $FF00;
    alpha := c + (c shr 8);
    end;
end;

function TBGRAReaderPNG.ColorGrayAlpha16 (const CD:TColorData) : TFPColor;
var c : UInt32or64;
begin
  c := CD and $FFFF;
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := (CD shr 16) and $FFFF;
    end;
end;

function TBGRAReaderPNG.ColorColor8 (const CD:TColorData) : TFPColor;
var c : UInt32or64;
begin
  with result do
    begin
    c := CD and $FF;
    red := c + (c shl 8);
    c := (CD shr 8) and $FF;
    green := c + (c shl 8);
    c := (CD shr 16) and $FF;
    blue := c + (c shl 8);
    alpha := alphaOpaque;
    end;
end;

function TBGRAReaderPNG.ColorColor16 (const CD:TColorData) : TFPColor;
begin
  with result do
    begin
    red := CD and $FFFF;
    green := (CD shr 16) and $FFFF;
    blue := (CD shr 32) and $FFFF;
    alpha := alphaOpaque;
    end;
end;

function TBGRAReaderPNG.ColorColorAlpha8 (const CD:TColorData) : TFPColor;
var c : UInt32or64;
begin
  with result do
    begin
    c := CD and $FF;
    red := c + (c shl 8);
    c := (CD shr 8) and $FF;
    green := c + (c shl 8);
    c := (CD shr 16) and $FF;
    blue := c + (c shl 8);
    c := (CD shr 24) and $FF;
    alpha := c + (c shl 8);
    end;
end;

function TBGRAReaderPNG.ColorColorAlpha16 (const CD:TColorData) : TFPColor;
begin
  with result do
    begin
    red := CD and $FFFF;
    green := (CD shr 16) and $FFFF;
    blue := (CD shr 32) and $FFFF;
    alpha := (CD shr 48) and $FFFF;
    end;
end;

function TBGRAReaderPNG.CheckGammaCorrection: boolean;
var
  i: Integer;
begin
  if not FGammaCorrectionTableComputed then
  begin
    if abs(FGammaCorrection-1) < 0.01 then
    begin
      FGammaCorrectionTable := nil;
    end else
    begin
      setlength(FGammaCorrectionTable, 65536);
      FGammaCorrectionTable[0] := 0;
      i := 1;
      while i <= 65535 do
      begin
        if i+3 <= 65535 then
        begin
          FGammaCorrectionTable[i+3] := Round(Power((i+3)/65535, FGammaCorrection)*65535);
          FGammaCorrectionTable[i] := (FGammaCorrectionTable[i-1]*3+FGammaCorrectionTable[i+3]+2) shr 2;
          FGammaCorrectionTable[i+1] := (FGammaCorrectionTable[i-1]+FGammaCorrectionTable[i+3]+1) shr 1;
          FGammaCorrectionTable[i+2] := (FGammaCorrectionTable[i-1]+FGammaCorrectionTable[i+3]*3+2) shr 2;
          inc(i,4);
        end else
        begin
          FGammaCorrectionTable[i] := Round(Power(i/65535, FGammaCorrection)*65535);
          inc(i);
        end;
      end;
    end;
    FGammaCorrectionTableComputed:= true;
  end;
  result := FGammaCorrectionTable<>nil;
end;

procedure TBGRAReaderPNG.ApplyGammaCorrection(var AColor: TFPColor);
begin
  if FGammaCorrectionTable<>nil then
  begin
    AColor.red := FGammaCorrectionTable[AColor.red];
    AColor.green := FGammaCorrectionTable[AColor.green];
    AColor.blue := FGammaCorrectionTable[AColor.blue];
  end;
end;

function TBGRAReaderPNG.BGRAColorGray1(const CD: TColorData): TBGRAPixel;
begin
  if CD = 0 then
    result := BGRABlack
  else
    result := BGRAWhite;
end;

function TBGRAReaderPNG.BGRAColorGray2(const CD: TColorData): TBGRAPixel;
var c : UInt32or64;
begin
  c := CD and 3;
  c := c + (c shl 2);
  c := c + (c shl 4);
  result := BGRA(c,c,c);
end;

function TBGRAReaderPNG.BGRAColorGray4(const CD: TColorData): TBGRAPixel;
var c : UInt32or64;
begin
  c := CD and $F;
  c := c + (c shl 4);
  result := BGRA(c,c,c);
end;

function TBGRAReaderPNG.BGRAColorGray8(const CD: TColorData): TBGRAPixel;
var c : UInt32or64;
begin
  c := CD and $FF;
  result := BGRA(c,c,c);
end;

function TBGRAReaderPNG.BGRAColorGray16(const CD: TColorData): TBGRAPixel;
var c : UInt32or64;
begin
  c := (CD shr 8) and $FF;
  result := BGRA(c,c,c);
end;

function TBGRAReaderPNG.BGRAColorGrayAlpha8(const CD: TColorData): TBGRAPixel;
var c : UInt32or64;
begin
  c := CD and $00FF;
  result := BGRA(c,c,c,(CD shr 8) and $FF);
end;

function TBGRAReaderPNG.BGRAColorGrayAlpha16(const CD: TColorData): TBGRAPixel;
var c : UInt32or64;
begin
  c := (CD shr 8) and $FF;
  result := BGRA(c,c,c,(CD shr 24) and $FF);
end;

function TBGRAReaderPNG.BGRAColorColor8(const CD: TColorData): TBGRAPixel;
var temp: LongWord;
begin
  temp := CD;
  result := BGRA(temp and $ff, (temp shr 8) and $ff, (temp shr 16) and $ff);
end;

function TBGRAReaderPNG.BGRAColorColor16(const CD: TColorData): TBGRAPixel;
begin
  result := BGRA(CD shr 8 and $FF,(CD shr 24) and $FF,(CD shr 40) and $FF);
end;

function TBGRAReaderPNG.BGRAColorColorAlpha8(const CD: TColorData): TBGRAPixel;
var temp: LongWord;
begin
  temp := CD;
  result := BGRA(temp and $ff, (temp shr 8) and $ff, (temp shr 16) and $ff, temp shr 24);
end;

function TBGRAReaderPNG.BGRAColorColorAlpha16(const CD: TColorData): TBGRAPixel;
begin
  result := BGRA(CD shr 8 and $FF,(CD shr 24) and $FF,(CD shr 40) and $FF, CD shr 56);
end;

procedure TBGRAReaderPNG.DoDecompress(ACompressedData: TStream; AWidth, AHeight: integer);
var
  decompressStream : TDeCompressionStream; // decompresses the data

  procedure initVars;
  var r,d : integer;
  begin
    with Header do
      begin
      if interlace=0 then
        begin
        StartPass := 0;
        EndPass := 0;
        FCountScanlines[0] := AHeight;
        FScanLineLength[0] := AWidth;
        end
      else
        begin
        StartPass := 1;
        EndPass := 7;
        for r := 1 to 7 do
          begin
          d := AHeight div delta[r,1];
          if (AHeight mod delta[r,1]) > startpoints[r,1] then
            inc (d);
          FCountScanlines[r] := d;
          d := AWidth div delta[r,0];
          if (AWidth mod delta[r,0]) > startpoints[r,0] then
            inc (d);
          FScanLineLength[r] := d;
          end;
        end;
      FIndexed := (ColorType = 3);
      case ColorType of
        0 : case Bitdepth of
              1  : begin
                   FConvertColor := @ColorGray1; //CFmt := cfMono;
                   FBGRAConvertColor := @BGRAColorGray1; //CFmt := cfMono;
                   ByteWidth := 1;
                   end;
              2  : begin
                   FConvertColor := @ColorGray2; //CFmt := cfGray2;
                   FBGRAConvertColor := @BGRAColorGray2; //CFmt := cfGray2;
                   ByteWidth := 1;
                   end;
              4  : begin
                   FConvertColor := @ColorGray4; //CFmt := cfGray4;
                   FBGRAConvertColor := @BGRAColorGray4; //CFmt := cfGray4;
                   ByteWidth := 1;
                   end;
              8  : begin
                   FConvertColor := @ColorGray8; //CFmt := cfGray8;
                   FBGRAConvertColor := @BGRAColorGray8; //CFmt := cfGray8;
                   ByteWidth := 1;
                   end;
              16 : begin
                   FConvertColor := @ColorGray16; //CFmt := cfGray16;
                   FBGRAConvertColor := @BGRAColorGray16; //CFmt := cfGray16;
                   ByteWidth := 2;
                   end;
            end;
        2 : if BitDepth = 8 then
              begin
              FConvertColor := @ColorColor8; //CFmt := cfBGR24
              FBGRAConvertColor := @BGRAColorColor8; //CFmt := cfBGR24
              ByteWidth := 3;
              end
            else
              begin
              FConvertColor := @ColorColor16; //CFmt := cfBGR48;
              FBGRAConvertColor := @BGRAColorColor16; //CFmt := cfBGR48;
              ByteWidth := 6;
              end;
        3 : if BitDepth = 16 then
              ByteWidth := 2
            else
              ByteWidth := 1;
        4 : if BitDepth = 8 then
              begin
              FConvertColor := @ColorGrayAlpha8; //CFmt := cfGrayA16
              FBGRAConvertColor := @BGRAColorGrayAlpha8; //CFmt := cfGrayA16
              ByteWidth := 2;
              end
            else
              begin
              FConvertColor := @ColorGrayAlpha16; //CFmt := cfGrayA32;
              FBGRAConvertColor := @BGRAColorGrayAlpha16; //CFmt := cfGrayA32;
              ByteWidth := 4;
              end;
        6 : if BitDepth = 8 then
              begin
              FConvertColor := @ColorColorAlpha8; //CFmt := cfABGR32
              FBGRAConvertColor := @BGRAColorColorAlpha8; //CFmt := cfABGR32
              ByteWidth := 4;
              end
            else
              begin
              FConvertColor := @ColorColorAlpha16; //CFmt := cfABGR64;
              FBGRAConvertColor := @BGRAColorColorAlpha16; //CFmt := cfABGR64;
              ByteWidth := 8;
              end;
      end;
      //ByteWidth := BytesNeeded[CFmt];
      case BitDepth of
        1 : begin
            CountBitsUsed := 8;
            BitShift := 1;
            BitsUsed := BitsUsed1Depth;
            end;
        2 : begin
            CountBitsUsed := 4;
            BitShift := 2;
            BitsUsed := BitsUsed2Depth;
            end;
        4 : begin
            CountBitsUsed := 2;
            BitShift := 4;
            BitsUsed := BitsUsed4Depth;
            end;
        8 : begin
            CountBitsUsed := 1;
            BitShift := 0;
            BitsUsed[0] := $FF;
            end;
        end;
      end;
  end;

  procedure FilterSub(p: PByte; Count: Int32or64; bw: Int32or64);
  begin
    inc(p,bw);
    dec(Count,bw);
    while Count > 0 do
    begin
      {$push}{$r-}
      inc(p^, (p-bw)^);
      {$pop}
      inc(p);
      dec(Count);
    end;
  end;

  procedure FilterUp(p,pPrev: PByte; Count: UInt32or64);
  var Count4: Int32or64;
  begin
    Count4 := Count shr 2;
    dec(Count, Count4 shl 2);
    while Count4 > 0 do
    begin
      {$push}{$r-}{$q-}
      PLongWord(p)^ := (((PLongWord(pPrev)^ and $00FF00FF) + (PLongWord(p)^ and $00FF00FF)) and $00FF00FF)
        or (((PLongWord(pPrev)^ and $FF00FF00) + (PLongWord(p)^ and $FF00FF00)) and $FF00FF00);
      {$pop}
      inc(p,4);
      inc(pPrev,4);
      dec(Count4);
    end;
    while Count > 0 do
    begin
      {$push}{$r-}
      inc(p^, pPrev^);
      {$pop}

      inc(p);
      inc(pPrev);
      dec(Count);
    end;
  end;

  procedure FilterAverage(p,pPrev: PByte; Count: UInt32or64; bw: Int32or64);
  var CountBW: Int32or64;
  begin
    CountBW := bw;
    dec(Count,CountBW);
    while CountBW > 0 do
    begin
      {$push}{$r-}
      inc(p^, pPrev^ shr 1);
      {$pop}
      inc(p);
      inc(pPrev);
      dec(CountBW);
    end;

    while Count > 0 do
    begin
      {$push}{$r-}
      inc(p^, (pPrev^+(p-bw)^) shr 1);
      {$pop}
      inc(p);
      inc(pPrev);
      dec(Count);
    end;
  end;

  procedure FilterPaeth(p,pPrev: PByte; Count: UInt32or64; bw: Int32or64);
  var
    rx, dl, dp, dlp : Int32or64;
    diag,left: UInt32or64;
  begin
    for rx := 0 to bw-1 do
    begin
      {$push}{$r-}
      inc(p^, pPrev^);
      {$pop}
      inc(p);
      inc(pPrev);
    end;
    dec(Count,bw);
    while Count > 0 do
    begin
      diag := (pPrev-bw)^;
      left := (p - bw)^;
      dl := pPrev^ - Int32or64(diag);
      dp := Int32or64(left) - Int32or64(diag);
      dlp := abs(dl+dp);
      if dl < 0 then dl := -dl;
      if dp < 0 then dp := -dp;
      {$push}{$r-}
      if dp <= dlp then
      begin
        if dl <= dp then
          inc(p^, left)
        else
          inc(p^, pPrev^)
      end
      else
      if dl <= dlp then
        inc(p^, left)
      else
        inc(p^, diag);
      {$pop}
      inc(p);
      inc(pPrev);
      dec(Count);
     end;
  end;

  procedure Decode;
  var y, rp, ry, l : Int32or64;
      lf : byte;
      switchLine, currentLine, previousLine : pByteArray;
  begin
    FSetPixel := DecideSetPixel;
    if not Indexed and (FTargetImage is TBGRACustomBitmap) and
      not CheckGammaCorrection then
    begin
      if UseTransparent then
        FHandleScanLine := @BGRAHandleScanLineTr
      else
        FHandleScanLine := @BGRAHandleScanLine;
    end else
      FHandleScanLine := @HandleScanLine;
    for rp := StartPass to EndPass do
      begin
      FCurrentPass := rp;
      StartX := StartPoints[rp,0];
      StartY := StartPoints[rp,1];
      DeltaX := Delta[rp,0];
      DeltaY := Delta[rp,1];
      if bytewidth = 1 then
        begin
        l := (ScanLineLength[rp] div CountBitsUsed);
        if (ScanLineLength[rp] mod CountBitsUsed) > 0 then
          inc (l);
        end
      else
        l := ScanLineLength[rp]*ByteWidth;
      if (l>0) then
        begin
        GetMem (previousLine, l);
        GetMem (currentLine, l);
        fillchar (currentLine^,l,0);
        try
          for ry := 0 to CountScanlines[rp]-1 do
            begin
            switchLine := currentLine;
            currentLine := previousLine;
            previousLine := switchLine;
            Y := StartY + (ry * deltaY);
            lf := 0;
            decompressStream.Read (lf, sizeof(lf));
            decompressStream.Read (currentLine^, l);

            case lf of
              1: FilterSub(PByte(currentLine), l, ByteWidth);
              2: FilterUp(PByte(currentLine), PByte(previousLine), l);
              3: FilterAverage(PByte(currentLine), PByte(previousLine), l, ByteWidth);
              4: FilterPaeth(PByte(currentLine), PByte(previousLine), l, ByteWidth);
            end;

            if FVerticalShrinkShr <> 0 then
              begin
                if (y and FVerticalShrinkMask) = 0 then
                  FHandleScanLine (y shr FVerticalShrinkShr, currentLine);
              end else
                FHandleScanLine (y, currentLine);
            end;
        finally
          freemem (previousLine);
          freemem (currentLine);
        end;
        end;
      end;
  end;

begin
  decompressStream := TDecompressionStream.Create (ACompressedData);
  try
    InitVars;
    DeCode;
  finally
    decompressStream.Free;
  end;
end;

procedure TBGRAReaderPNG.HandleChunk;
begin
  if IsAnimatedChunkType(chunk.AType) then
  case TAnimatedChunkTypes(chunk.AType) of
    ctacTL: HandleAnimationControl;
    ctfcTL: HandleFrameControl;
    ctfdAT: HandleFrameData;
  end else
  case chunk.AType of
    ctIHDR : raise PNGImageException.Create ('Second IHDR chunk found');
    ctPLTE : HandlePalette;
    ctIDAT : HandleData;
    ctIEND : EndOfFile := True;
    cttRNS : HandleAlpha;
    ctsRGB : HandleStdRGB;
    ctgAMA : HandleGamma;
    ctpHYs : HandlePhysicalDimensions;
    else HandleUnknown;
  end;
end;

procedure TBGRAReaderPNG.HandleUnknown;
begin
  if (chunk.readtype[0] in ['A'..'Z']) then
    raise PNGImageException.Create('Critical chunk '+chunk.readtype+' not recognized');
end;

procedure TBGRAReaderPNG.InternalRead (Str:TStream; Img:TFPCustomImage);
var outputHeight: integer;
begin
  {$ifdef FPC_Debug_Image}
  if Str<>TheStream then
    writeln('WARNING: TBGRAReaderPNG.InternalRead Str<>TheStream');
  {$endif}
  with Header do
  begin
    FVerticalShrinkShr := 0;
    FVerticalShrinkMask := 0;
    outputHeight := Height;
    if MinifyHeight <> 0 then
      begin
        while (outputHeight shr 1 >= MinifyHeight) and (FVerticalShrinkShr < 8) do
          begin
            outputHeight:= outputHeight shr 1;
            Inc(FVerticalShrinkShr);
          end;
        FVerticalShrinkMask:= (1 shl FVerticalShrinkShr)-1;
      end;
  end;

  FreeAndNil(FPalette);
  FFrames.Clear;
  FMainImageFrameIndex := -1;
  FGammaCorrection := 1;
  FGammaCorrectionTableComputed:= false;
  //Resolution: If the pHYs chunk is not present, pixels are assumed to be square
  PredefinedResolutionValues;

  try
    EndOfFile := false;
    while not EndOfFile do
      begin
      if ReadChunk then
        HandleChunk;
      end;

    if Assigned(FMainImageData) then
      DoLoadImage(TheImage, FMainImageData, Header.Width, Header.Height)
    else
      raise PNGImageException.Create('Missing image data');

    // if not all frames are found, update the frame count accordingly
    if FFrames.Count < FFrameCount then
      FFrameCount := FFrames.Count;
  finally
    if FMainImageFrameIndex = -1 then
      FreeAndNil(FMainImageData);
  end;
end;

function  TBGRAReaderPNG.InternalCheck (Str:TStream) : boolean;
var {%H-}SigCheck : TPNGSignature;
    r : integer;
begin
  try
    // Check Signature
    if Str.Read({%H-}SigCheck, SizeOf(SigCheck)) <> SizeOf(SigCheck) then
      raise PNGImageException.Create('This is not PNG-data');
    if not CheckSignature(SigCheck) then
        raise PNGImageException.Create('This is not PNG-data');
    // Check IHDR
    ReadChunk;
    if chunk.aType <> ctIHDR then
      raise PNGImageException.Create('Header chunk expected but '+chunk.ReadType+' found');
    fillchar(FHeader, sizeof(FHeader), 0);
    move (chunk.data^, FHeader, min(sizeof(Header), chunk.alength));
    with header do
      begin
      {$IFDEF ENDIAN_LITTLE}
      Width := swap(width);
      height := swap (height);
      {$ENDIF}
      result := (width > 0) and (height > 0) and (compression = 0)
                and (filter = 0) and (Interlace in [0,1]);
      end;
  except
    result := false;
  end;
end;

initialization

  DefaultBGRAImageReader[ifPng] := TBGRAReaderPNG;

end.

