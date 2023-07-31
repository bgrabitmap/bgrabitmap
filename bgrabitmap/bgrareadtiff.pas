// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
    The original file is part of the Free Pascal run time library.
    Copyright (c) 2012-2013 by the Free Pascal development team

    Tiff reader for fpImage modified by circular.

 **********************************************************************

 Working:
   Sample bitdepth: 1, 4, 8, 12, 16
   Color format: black and white, grayscale, RGB, colormap, L*a*b*
   Alpha channel: none, premultiplied, separated
   Compression: packbits, LZW, deflate
   Endian-ness: little endian and big endian
   Orientation: any corner can be (0,0) and x/y can be flipped
   Planar configuration: 1 (channels together)
   Fill order: any (for 1 bit per sample images)
   Skipping thumbnail by reading biggest image
   Multiple images
   Strips and tiles

 ToDo:
   Compression: FAX, Jpeg...
   Color format: YCbCr, ITU L*a*b*
   PlanarConfiguration: 2 (one chunk for each channel)
   XMP tag 700
   ICC profile tag 34675

 Not to do:
   Separate mask (deprecated)

}
{*****************************************************************************}
{
  2023-06  - Massimo Magnano
           - added Resolution support
       07  - added BigTif (only from fpc 3.3.1),
             solved Read 16 bit data with predictor fails
             conditional compilation for 3.3.1 (now the class is derived from TFPReaderTiff)
}
{*****************************************************************************}
unit BGRAReadTiff;

{$mode objfpc}{$H+}

{$inline on}

interface

uses
  Math, BGRAClasses, SysUtils, ctypes, zinflate, zbase, FPimage, FPTiffCmn,
  BGRABitmapTypes {$IF FPC_FULLVERSION>=30301}, FPReadTiff{$ENDIF};

type
  {$IF FPC_FULLVERSION<30301}
  TBGRAReaderTiff = class;

  TTiffCreateCompatibleImgEvent = procedure(Sender: TBGRAReaderTiff;
                                            ImgFileDir: TTiffIFD) of object;

  TTiffCheckIFDOrder = (
    tcioSmart,
    tcioAlways,
    tcioNever
    );

  { TBGRAReaderTiff }

  TBGRAReaderTiff = class(TFPCustomImageReader)
  private
    FCheckIFDOrder: TTiffCheckIFDOrder;
    FFirstIFDStart: LongWord;
    FOnCreateImage: TTiffCreateCompatibleImgEvent;
    {$ifdef FPC_Debug_Image}
    FDebug: boolean;
    {$endif}
    FIFDList: TFPList;
    FReverseEndian: Boolean;
    fStartPos: int64;
    s: TStream;
    function GetImages(Index: integer): TTiffIFD;
    procedure TiffError(Msg: string);
    procedure SetStreamPos(p: LongWord);
    function ReadTiffHeader(QuickTest: boolean; out IFDStart: LongWord): boolean; // returns IFD: offset to first IFD
    function ReadIFD(Start: LongWord; IFD: TTiffIFD): LongWord;// Image File Directory
    procedure ReadDirectoryEntry(var EntryTag: Word; IFD: TTiffIFD);
    function ReadEntryUnsigned: LongWord;
    function ReadEntrySigned: Cint32;
    function ReadEntryRational: TTiffRational;
    function ReadEntryString: string;
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadDWord: LongWord;
    procedure ReadValues(StreamPos: LongWord;
                         out EntryType: word; out EntryCount: LongWord;
                         out Buffer: Pointer; out ByteCount: PtrUInt);
    procedure ReadShortOrLongValues(StreamPos: LongWord;
                                    out Buffer: PLongWord; out Count: LongWord);
    procedure ReadShortValues(StreamPos: LongWord;
                              out Buffer: PWord; out Count: LongWord);
    procedure ReadImageSampleProperties(IFD: TTiffIFD; out AlphaChannel: integer; out PremultipliedAlpha: boolean;
      out SampleCnt: LongWord; out SampleBits: PWord; out SampleBitsPerPixel: LongWord;
      out PaletteCnt: LongWord; out PaletteValues: PWord);
    procedure ReadImgValue(BitCount: Word;
      var Run: Pointer; var BitPos: Byte; FillOrder: LongWord;
      Predictor: word; var LastValue: word; out Value: Word);
    function FixEndian(w: Word): Word; inline;
    function FixEndian(d: LongWord): LongWord; inline;
    procedure SetFPImgExtras(CurImg: TFPCustomImage; IFD: TTiffIFD);
    procedure DecodePackBits(var Buffer: Pointer; var Count: PtrInt);
    procedure DecodeLZW(var Buffer: Pointer; var Count: PtrInt);
    procedure DecodeDeflate(var Buffer: Pointer; var Count: PtrInt; ExpectedCount: PtrInt);
  protected
    procedure InternalRead(Str: TStream; AnImage: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
    procedure DoCreateImage(ImgFileDir: TTiffIFD); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;

    procedure LoadFromStream(aStream: TStream; AutoClear: boolean = true); //load all images (you need to handle OnCreateImage event and assign ImgFileDir.Img)
    {$ifdef FPC_Debug_Image}
    property Debug: boolean read FDebug write FDebug;
    {$endif}
    property OnCreateImage: TTiffCreateCompatibleImgEvent read FOnCreateImage
                                                          write FOnCreateImage;
    property CheckIFDOrder: TTiffCheckIFDOrder read FCheckIFDOrder write FCheckIFDOrder; //check order of IFD entries or not
    function FirstImg: TTiffIFD;
    function GetBiggestImage: TTiffIFD;
    function ImageCount: integer;
    property Images[Index: integer]: TTiffIFD read GetImages; default;

  public //advanced
    ImageList: TFPList; // list of TTiffIFD
    procedure LoadHeaderFromStream(aStream: TStream);
    procedure LoadIFDsFromStream;                  // call LoadHeaderFromStream before
    procedure LoadImageFromStream(Index: integer); // call LoadIFDsFromStream before
    procedure LoadImageFromStream(IFD: TTiffIFD);  // call LoadIFDsFromStream before
    procedure ReleaseStream;
    property StartPos: int64 read fStartPos;
    property TheStream: TStream read s;
    property FirstIFDStart: LongWord read FFirstIFDStart;
  end;

procedure DecompressPackBits(Buffer: Pointer; Count: PtrInt;
  out NewBuffer: Pointer; out NewCount: PtrInt);
procedure DecompressLZW(Buffer: Pointer; Count: PtrInt;
  out NewBuffer: PByte; out NewCount: PtrInt);
function DecompressDeflate(Compressed: PByte; CompressedCount: LongWord;
  out Decompressed: PByte; var DecompressedCount: LongWord;
  ErrorMsg: PAnsiString = nil): boolean;

function TifResolutionUnitToResolutionUnit(ATifResolutionUnit: DWord): TResolutionUnit;
function ResolutionUnitToTifResolutionUnit(AResolutionUnit: TResolutionUnit): DWord;
{$ELSE}
  { TBGRAReaderTiff }

  TBGRAReaderTiff = class(TFPReaderTiff)
  public
     procedure LoadImageFromStream(IFD: TTiffIFD); override;
  end;

{$ENDIF}

implementation

{$IF FPC_FULLVERSION<30301}
function CMYKToFPColor(C,M,Y,K: Word): TFPColor;
var R, G, B : LongWord;
begin
   R := $ffff - ((LongWord(C)*($ffff-LongWord(K))) shr 16) - LongWord(K) ;
   G := $ffff - ((LongWord(M)*($ffff-LongWord(K))) shr 16) - LongWord(K) ;
   B := $ffff - ((LongWord(Y)*($ffff-LongWord(K))) shr 16) - LongWord(K) ;
   Result := FPColor(R and $ffff,G and $ffff,B and $ffff);
end ;

procedure TBGRAReaderTiff.TiffError(Msg: string);
begin
  Msg:=Msg+' at position '+IntToStr(s.Position);
  if fStartPos>0 then
    Msg:=Msg+' (TiffPosition='+IntToStr(fStartPos)+')';
  raise Exception.Create(Msg);
end;

function TBGRAReaderTiff.GetImages(Index: integer): TTiffIFD;
begin
  Result:=TTiffIFD(ImageList[Index]);
end;

procedure TBGRAReaderTiff.ReadImageSampleProperties(IFD: TTiffIFD;
  out AlphaChannel: integer; out PremultipliedAlpha: boolean;
  out SampleCnt: LongWord; out SampleBits: PWord; out SampleBitsPerPixel: LongWord;
  out PaletteCnt: LongWord; out PaletteValues: PWord);
var
  BytesPerPixel: Word;
  i: Integer;
  ExtraSampleCnt, RegularSampleCnt: LongWord;
  ExtraSamples: PWord;
begin
  ReadShortValues(IFD.BitsPerSample, SampleBits, SampleCnt);
  if SampleCnt<>IFD.SamplesPerPixel then
  begin
    ReAllocMem(SampleBits, 0);
    TiffError('Samples='+IntToStr(SampleCnt)+' <> SamplesPerPixel='+IntToStr(IFD
      .SamplesPerPixel));
  end;

  BytesPerPixel:=0;
  SampleBitsPerPixel:=0;
  PaletteCnt:= 0;
  PaletteValues:= nil;

  AlphaChannel:= -1;
  PremultipliedAlpha:= false;
  IFD.AlphaBits:= 0;

  //looking for alpha channel in extra samples
  if IFD.ExtraSamples>0 then
    ReadShortValues(IFD.ExtraSamples, ExtraSamples, ExtraSampleCnt)
  else begin
    ExtraSamples := nil;
    ExtraSampleCnt:= 0;
  end;

  if ExtraSampleCnt>=SampleCnt then
  begin
    ReAllocMem(SampleBits, 0);
    ReAllocMem(ExtraSamples, 0);
    TiffError('Samples='+IntToStr(SampleCnt)+' ExtraSampleCnt='+IntToStr(
      ExtraSampleCnt));
  end;

  RegularSampleCnt := SampleCnt - ExtraSampleCnt;

  for i:=0 to ExtraSampleCnt-1 do begin
    if ExtraSamples[i] in [1, 2] then begin
      AlphaChannel := RegularSampleCnt+i;
      PremultipliedAlpha:= ExtraSamples[i]=1;
      IFD.AlphaBits:=SampleBits[AlphaChannel];
    end;
  end;

  ReAllocMem(ExtraSamples, 0);  //end of extra samples

  for i:=0 to SampleCnt-1 do begin
    if SampleBits[i]>16 then
      TiffError('Samples bigger than 16 bit not supported');
    if not (SampleBits[i] in [1, 4, 8, 12, 16]) then
      TiffError('Only samples of 1, 4, 8, 12 and 16 bit are supported');
    if (i <> 0) and ((SampleBits[i] = 1) xor (SampleBits[0] = 1)) then
      TiffError('Cannot mix 1 bit samples with other sample sizes');
    inc(SampleBitsPerPixel, SampleBits[i]);
  end;

  BytesPerPixel:= SampleBitsPerPixel div 8;
  IFD.BytesPerPixel:=BytesPerPixel;
  {$ifdef FPC_Debug_Image}
  if Debug then
    writeln('BytesPerPixel=', BytesPerPixel);
  {$endif}

  case IFD.PhotoMetricInterpretation of
  0, 1:
    begin
      if RegularSampleCnt<>1 then
        TiffError('gray images expect one sample per pixel, but found '+
          IntToStr(SampleCnt));

      IFD.GrayBits:=SampleBits[0];
    end;
  2:
    begin
      if (RegularSampleCnt<>3) and (RegularSampleCnt<>4) then
        TiffError('rgb(a) images expect three or four samples per pixel, but found '+
          IntToStr(SampleCnt));

      IFD.RedBits:=SampleBits[0];
      IFD.GreenBits:=SampleBits[1];
      IFD.BlueBits:=SampleBits[2];
      if RegularSampleCnt=4 then begin
        if (AlphaChannel <> -1) then
          TiffError('Alpha channel specified twice');
        AlphaChannel:= 3;
        PremultipliedAlpha:= false;
        IFD.AlphaBits:=SampleBits[AlphaChannel];
      end;
    end;
  3:
    begin
      if RegularSampleCnt<>1 then
        TiffError('palette images expect one sample per pixel, but found '+
          IntToStr(SampleCnt));

      if IFD.ColorMap > 0 then
      begin
        ReadShortValues(IFD.ColorMap, PaletteValues, PaletteCnt);
        if PaletteCnt <> (1 shl SampleBits[0])*3 then
        begin
          ReAllocMem(PaletteValues, 0);
          TiffError('Palette size mismatch');
        end;
      end else
        TiffError('Palette not supplied')
    end;
  4:
    begin
      if RegularSampleCnt<>1 then
        TiffError('mask images expect one sample per pixel, but found '+
          IntToStr(SampleCnt));
      TiffError('Mask images not handled');
    end;
  5:
    begin
      if RegularSampleCnt<>4 then
        TiffError('cmyk images expect four samples per pixel, but found '+
          IntToStr(SampleCnt));

      IFD.RedBits:=SampleBits[0];   //cyan
      IFD.GreenBits:=SampleBits[1]; //magenta
      IFD.BlueBits:=SampleBits[2];   //yellow
      IFD.GrayBits:=SampleBits[3];  //black
      PremultipliedAlpha:= false;
    end;
  8,9:
    begin
      if (RegularSampleCnt<>1) and (RegularSampleCnt<>3) then
        TiffError('L*a*b* colorspace needs either one component for grayscale or three components, but found '+inttostr(RegularSampleCnt));
      if RegularSampleCnt = 3 then
      begin
        IFD.GreenBits:=SampleBits[0];
        if (IFD.GreenBits <> 8) and (IFD.GreenBits <> 16) then TiffError('Only 8 bit and 16 bit depth allowed for L* component');
        IFD.RedBits:=SampleBits[1];
        IFD.BlueBits:=SampleBits[2]; //in fact inverse blue so more like yellow
        if ((IFD.RedBits <> 8) and (IFD.RedBits <> 16))
        or ((IFD.BlueBits <> 8) and (IFD.BlueBits <> 16)) then TiffError('Only 8 bit and 16 bit depth allowed for a* and b* component');
      end else
      begin
        IFD.GrayBits:=SampleBits[0];
        if (IFD.GrayBits <> 8) and (IFD.GrayBits <> 16) then TiffError('Only 8 bit and 16 bit depth allowed for L* component');
      end;
      PremultipliedAlpha:= false;
    end
  else
    TiffError('Photometric interpretation not handled (' + inttostr(IFD.PhotoMetricInterpretation)+')');
  end;
end;

procedure TBGRAReaderTiff.SetFPImgExtras(CurImg: TFPCustomImage; IFD: TTiffIFD);
begin
  ClearTiffExtras(CurImg);
  // set Tiff extra attributes
  CurImg.Extra[TiffPhotoMetric]:=IntToStr(IFD.PhotoMetricInterpretation);
  //writeln('TBGRAReaderTiff.SetFPImgExtras PhotoMetric=',CurImg.Extra[TiffPhotoMetric]);
  if IFD.Artist<>'' then
    CurImg.Extra[TiffArtist]:=IFD.Artist;
  if IFD.Copyright<>'' then
    CurImg.Extra[TiffCopyright]:=IFD.Copyright;
  if IFD.DocumentName<>'' then
    CurImg.Extra[TiffDocumentName]:=IFD.DocumentName;
  if IFD.DateAndTime<>'' then
    CurImg.Extra[TiffDateTime]:=IFD.DateAndTime;
  if IFD.HostComputer<>'' then
    CurImg.Extra[TiffHostComputer]:=IFD.HostComputer;
  if IFD.ImageDescription<>'' then
    CurImg.Extra[TiffImageDescription]:=IFD.ImageDescription;
  if IFD.Make_ScannerManufacturer<>'' then
    CurImg.Extra[TiffMake_ScannerManufacturer]:=IFD.Make_ScannerManufacturer;
  if IFD.Model_Scanner<>'' then
    CurImg.Extra[TiffModel_Scanner]:=IFD.Model_Scanner;
  if IFD.Software<>'' then
    CurImg.Extra[TiffSoftware]:=IFD.Software;
  if not (IFD.Orientation in [1..8]) then
    IFD.Orientation:=1;
  CurImg.Extra[TiffOrientation]:=IntToStr(IFD.Orientation);
  if IFD.ResolutionUnit<>0 then
    CurImg.Extra[TiffResolutionUnit]:=IntToStr(IFD.ResolutionUnit);
  if (IFD.XResolution.Numerator<>0) or (IFD.XResolution.Denominator<>0) then
    CurImg.Extra[TiffXResolution]:=TiffRationalToStr(IFD.XResolution);
  if (IFD.YResolution.Numerator<>0) or (IFD.YResolution.Denominator<>0) then
    CurImg.Extra[TiffYResolution]:=TiffRationalToStr(IFD.YResolution);
  CurImg.Extra[TiffRedBits]:=IntToStr(IFD.RedBits);
  CurImg.Extra[TiffGreenBits]:=IntToStr(IFD.GreenBits);
  CurImg.Extra[TiffBlueBits]:=IntToStr(IFD.BlueBits);
  CurImg.Extra[TiffGrayBits]:=IntToStr(IFD.GrayBits);
  CurImg.Extra[TiffAlphaBits]:=IntToStr(IFD.AlphaBits);
  if IFD.PageCount>0 then begin
    CurImg.Extra[TiffPageNumber]:=IntToStr(IFD.PageNumber);
    CurImg.Extra[TiffPageCount]:=IntToStr(IFD.PageCount);
  end;
  if IFD.PageName<>'' then
    CurImg.Extra[TiffPageName]:=IFD.PageName;
  if IFD.ImageIsThumbNail then
    CurImg.Extra[TiffIsThumbnail]:='1';
  if IFD.ImageIsMask then
    CurImg.Extra[TiffIsMask]:='1';
  if IFD.Compression<>TiffCompressionNone then
    CurImg.Extra[TiffCompression]:=IntToStr(IFD.Compression);

  {$ifdef FPC_Debug_Image}
  if Debug then
    WriteTiffExtras('SetFPImgExtras', CurImg);
  {$endif}
end;

procedure TBGRAReaderTiff.ReadImgValue(BitCount: Word;
  var Run: Pointer; var BitPos: Byte; FillOrder: LongWord;
  Predictor: word; var LastValue: word; out Value: Word);
var
  BitNumber: byte;
  Byte1, Byte2: byte;
begin
  case BitCount of
  1:
    begin
      if FillOrder = 2 then
        BitNumber:=BitPos    //Leftmost pixel starts with bit 0
      else
        BitNumber:=7-BitPos; //Leftmost pixel starts with bit 7
      Value:=((PCUInt8(Run)^) and (1 shl BitNumber) shr BitNumber);
      inc(BitPos);
      if BitPos = 8 then
      begin
        BitPos := 0;
        inc(Run); //next byte when all bits read
      end;
      if Predictor = 2 then Value := (LastValue+Value) and 1;
      LastValue:=Value;
      if Value > 0 then Value := $ffff;
    end;
  4:
    begin
      if BitPos = 0 then
      begin
        Value := PCUInt8(Run)^ shr 4;
        BitPos := 4;
      end
      else
      begin
        Value := PCUInt8(Run)^ and 15;
        BitPos := 0;
        Inc(Run);
      end;
      if Predictor = 2 then Value := (LastValue+Value) and $f;
      LastValue:=Value;
      Value := Value + (value shl 4) + (value shl 8) + (value shl 12);
    end;
  8:
    begin
      Value:=PCUInt8(Run)^;
      inc(Run);
      if Predictor = 2 then Value := (LastValue+Value) and $ff;
      LastValue:=Value;
      Value:=Value shl 8+Value;
    end;
  12:
    begin
      Byte1 := PCUInt8(Run)^;
      Byte2 := PCUInt8(Run+1)^;
      if BitPos = 0 then begin
        Value := (Byte1 shl 4) or (Byte2 shr 4);
        inc(Run);
        BitPos := 4;
      end else begin
        Value := ((Byte1 and $0F) shl 8) or Byte2;
        inc(Run, 2);
        BitPos := 0;
      end;
      if Predictor = 2 then Value := (LastValue+Value) and $fff;
      LastValue:=Value;
      Value := (Value shl 4) + (Value shr 8);
    end;
  16:
    begin
      Value:=FixEndian(PCUInt16(Run)^);
      inc(Run,2);
      if Predictor = 2 then Value := (LastValue+Value) and $ffff;
      LastValue:=Value;
    end;
  end;
end;

procedure TBGRAReaderTiff.SetStreamPos(p: LongWord);
var
  NewPosition: int64;
begin
  NewPosition:=Int64(p)+fStartPos;
  if NewPosition>s.Size then
    TiffError('Offset outside of stream');
  s.Position:=NewPosition;
end;

procedure TBGRAReaderTiff.LoadFromStream(aStream: TStream; AutoClear: boolean);
var
  i: Integer;
  aContinue: Boolean;
begin
  if AutoClear then
    Clear;
  aContinue:=true;
  Progress(psStarting, 0, False, Rect(0,0,0,0), '', aContinue);
  if not aContinue then exit;
  LoadHeaderFromStream(aStream);
  LoadIFDsFromStream;
  for i := 0 to ImageCount-1 do
  begin
    Progress(psRunning, (i+1)*100 div (ImageCount+1), False, Rect(0,0,0,0),
             IntToStr(i+1)+'/'+IntToStr(ImageCount), aContinue);
    LoadImageFromStream(i);
  end;
  Progress(psEnding, 100, False, Rect(0,0,0,0), '', aContinue);
  ReleaseStream;
end;

procedure TBGRAReaderTiff.LoadHeaderFromStream(aStream: TStream);
begin
  FFirstIFDStart:=0;
  s:=aStream;
  fStartPos:=s.Position;
  ReadTiffHeader(false,FFirstIFDStart);
end;

procedure TBGRAReaderTiff.LoadIFDsFromStream;
var
  i,j: Integer;
  IFDStart: LongWord;
  IFD: TTiffIFD;
begin
  IFDStart:=FirstIFDStart;
  i:=0;
  while IFDStart>0 do begin
    for j := 0 to i-1 do
      if Images[j].IFDStart = IFDStart then exit; //IFD cycle detected

    if ImageCount=i then
    begin
      IFD := TTiffIFD.Create;
      ImageList.Add(IFD);
    end else
      IFD:=Images[i];
    IFDStart:=ReadIFD(IFDStart, IFD);
    inc(i);
  end;
end;

function TBGRAReaderTiff.FirstImg: TTiffIFD;
begin
  Result:=nil;
  if (ImageList=nil) or (ImageList.Count=0) then exit;
  Result:=TTiffIFD(ImageList[0]);
end;

function TBGRAReaderTiff.GetBiggestImage: TTiffIFD;
var
  Size: Int64;
  IFD: TTiffIFD;
  CurSize: int64;
  i: Integer;
begin
  Result:=nil;
  Size:=0;
  for i:=0 to ImageCount-1 do begin
    IFD:=Images[i];
    CurSize:=Int64(IFD.ImageWidth)*IFD.ImageHeight;
    if CurSize<Size then continue;
    Size:=CurSize;
    Result:=IFD;
  end;
end;

function TBGRAReaderTiff.ImageCount: integer;
begin
  Result:=ImageList.Count;
end;

function TBGRAReaderTiff.ReadTiffHeader(QuickTest: boolean; out IFDStart: LongWord): boolean;
var
  ByteOrder: String;
  BigEndian: Boolean;
  FortyTwo: Word;
begin
  Result:=false;
  // read byte order  II low endian, MM big endian
  ByteOrder:='  ';
  s.Read(ByteOrder[1],2);
  //debugln(['TForm1.ReadTiffHeader ',dbgstr(ByteOrder)]);
  if ByteOrder='II' then
    BigEndian:=false
  else if ByteOrder='MM' then
    BigEndian:=true
  else if QuickTest then
    exit
  else
  begin
    TiffError('expected II or MM');
    exit; // indicate that flow stops
  end;
  FReverseEndian:={$ifdef FPC_BIG_ENDIAN}not{$endif} BigEndian;
  {$ifdef FPC_Debug_Image}
  if Debug then
    writeln('TBGRAReaderTiff.ReadTiffHeader Endian Big=',BigEndian,' ReverseEndian=',FReverseEndian);
  {$endif}
  // read magic number 42
  FortyTwo:=ReadWord;
  if FortyTwo<>42 then begin
    if QuickTest then
      exit
    else
      TiffError('expected 42, because of its deep philosophical impact, but found '+IntToStr(FortyTwo));
  end;
  // read offset to first IFD
  IFDStart:=ReadDWord;
  //debugln(['TForm1.ReadTiffHeader IFD=',IFD]);
  Result:=true;
end;

function TBGRAReaderTiff.ReadIFD(Start: LongWord; IFD: TTiffIFD): LongWord;
var
  Count: Word;
  i: Integer;
  EntryTag: Word;
  p: Int64;
begin
  {$ifdef FPC_Debug_Image}
  if Debug then
    writeln('ReadIFD Start=',Start);
  {$endif}

  Result:=0;
  SetStreamPos(Start);
  IFD.IFDStart:=Start;
  Count:=ReadWord;
  EntryTag:=0;
  p:=s.Position;
  for i:=1 to Count do begin
    ReadDirectoryEntry(EntryTag, IFD);
    inc(p,12);
    s.Position:=p;
  end;

  //fix IFD if it is supposed to use tiles but provide chunks as strips
  if IFD.TileWidth > 0 then
  begin
    if (IFD.TileOffsets=0) and (IFD.StripOffsets <> 0) then
    begin
      IFD.TileOffsets := IFD.StripOffsets;
      IFD.StripOffsets := 0;
    end;
    if (IFD.TileByteCounts=0) and (IFD.StripByteCounts <> 0) then
    begin
      IFD.TileByteCounts := IFD.StripByteCounts;
      IFD.StripByteCounts:= 0;
    end;
  end else
  begin
    //if not specified, the strip is the whole image
    if IFD.RowsPerStrip = 0 then IFD.RowsPerStrip:= IFD.ImageHeight;
  end;

  // read start of next IFD
  IFD.IFDNext:= ReadDWord;
  Result:= IFD.IFDNext;
end;

procedure TBGRAReaderTiff.ReadDirectoryEntry(var EntryTag: Word; IFD: TTiffIFD);
var
  EntryType: Word;
  EntryCount: LongWord;
  EntryStart: LongWord;
  NewEntryTag: Word;
  UValue: LongWord;
  SValue: integer;
  WordBuffer: PWord;
  Count: LongWord;
  i: Integer;

  function GetPos: LongWord;
  begin
     Result:=LongWord(s.Position-fStartPos-2)
  end;

begin
  NewEntryTag:=ReadWord;
  if (NewEntryTag<EntryTag) then begin
    // the TIFF specification insists on ordered entry tags in each IFD
    // This allows to spot damaged files.
    // But some programs like 'GraphicConverter' do not order the extension tags
    // properly.
    {$ifdef FPC_Debug_Image}
    if Debug then
      writeln('WARNING: Tags must be in ascending order: Last='+IntToStr(EntryTag)+' Next='+IntToStr(NewEntryTag));
    {$endif}
    case CheckIFDOrder of
    tcioAlways: TiffError('Tags must be in ascending order: Last='+IntToStr(EntryTag)+' Next='+IntToStr(NewEntryTag));
    tcioSmart:
      if NewEntryTag<30000 then
        TiffError('Tags must be in ascending order: Last='+IntToStr(EntryTag)+' Next='+IntToStr(NewEntryTag));
    end;
  end;
  EntryTag:=NewEntryTag;
  case EntryTag of
  254:
    begin
      // NewSubFileType
      UValue:=ReadEntryUnsigned;
      IFD.ImageIsThumbNail:=UValue and 1<>0;
      IFD.ImageIsPage:=UValue and 2<>0;
      IFD.ImageIsMask:=UValue and 4<>0;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 254: NewSubFileType ThumbNail=',IFD.ImageIsThumbNail,' Page=',IFD.ImageIsPage,' Mask=',IFD.ImageIsMask);
      {$endif}
    end;
  255:
    begin
      // SubFileType (deprecated)
      UValue:=ReadEntryUnsigned;
      IFD.ImageIsThumbNail:=false;
      IFD.ImageIsPage:=false;
      IFD.ImageIsMask:=false;
      case UValue of
      1: ;
      2: IFD.ImageIsThumbNail:=true;
      3: IFD.ImageIsPage:=true;
      else
        TiffError('SubFileType expected, but found '+IntToStr(UValue));
      end;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 255: SubFileType ThumbNail=',IFD.ImageIsThumbNail,' Page=',IFD.ImageIsPage,' Mask=',IFD.ImageIsMask);
      {$endif}
    end;
  256:
    begin
      // fImageWidth
      IFD.ImageWidth:=ReadEntryUnsigned;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 256: ImageWidth=',IFD.ImageWidth);
      {$endif}
    end;
  257:
    begin
      // ImageLength according to TIFF spec, here used as imageheight
      IFD.ImageHeight:=ReadEntryUnsigned;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 257: ImageHeight=',IFD.ImageHeight);
      {$endif}
    end;
  258:
    begin
      // BitsPerSample
      IFD.BitsPerSample:=GetPos;
      ReadShortValues(IFD.BitsPerSample,WordBuffer,Count);
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        write('TBGRAReaderTiff.ReadDirectoryEntry Tag 258: BitsPerSample: ');
        for i:=0 to Count-1 do
          write(IntToStr(WordBuffer[i]),' ');
        writeln;
      end;
      {$endif}
      try
        SetLength(IFD.BitsPerSampleArray,Count);
        for i:=0 to Count-1 do
          IFD.BitsPerSampleArray[i]:=WordBuffer[i];
      finally
        ReAllocMem(WordBuffer,0);
      end;
    end;
  259:
    begin
      // Compression
      UValue:=ReadEntryUnsigned;
      case UValue of
      TiffCompressionNone,
      TiffCompressionCCITTRLE,
      TiffCompressionCCITTFAX3,
      TiffCompressionCCITTFAX4,
      TiffCompressionLZW,
      TiffCompressionOldJPEG,
      TiffCompressionJPEG,
      TiffCompressionDeflateAdobe,
      TiffCompressionJBIGBW,
      TiffCompressionJBIGCol,
      TiffCompressionNeXT,
      TiffCompressionCCITTRLEW,
      TiffCompressionPackBits,
      TiffCompressionThunderScan,
      TiffCompressionIT8CTPAD,
      TiffCompressionIT8LW,
      TiffCompressionIT8MP,
      TiffCompressionIT8BL,
      TiffCompressionPixarFilm,
      TiffCompressionPixarLog,
      TiffCompressionDeflateZLib,
      TiffCompressionDCS,
      TiffCompressionJBIG,
      TiffCompressionSGILog,
      TiffCompressionSGILog24,
      TiffCompressionJPEG2000: ;
      else
        TiffError('expected Compression, but found '+IntToStr(UValue));
      end;
      IFD.Compression:=UValue;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 259: Compression=',IntToStr(IFD.Compression),'=',TiffCompressionName(IFD.Compression));
      {$endif}
    end;
  262:
    begin
      // PhotometricInterpretation
      UValue:=ReadEntryUnsigned;
      if UValue > 65535 then
        TiffError('expected PhotometricInterpretation, but found '+IntToStr(UValue));
      IFD.PhotoMetricInterpretation:=UValue;
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        write('TBGRAReaderTiff.ReadDirectoryEntry Tag 262: PhotometricInterpretation=');
        case IFD.PhotoMetricInterpretation of
        0: write('0=bilevel grayscale 0 is white');
        1: write('1=bilevel grayscale 0 is black');
        2: write('2=RGB 0,0,0 is black');
        3: write('3=Palette color');
        4: write('4=Transparency Mask');
        5: write('5=CMYK 8bit');
        8: write('8=L*a*b* with a and b [-128;127]');
        9: write('9=L*a*b* with a and b [0;255]');
        end;
        writeln;
      end;
      {$endif}
    end;
  263:
    begin
      // Tresholding
      UValue:=ReadEntryUnsigned;
      case UValue of
      1: ; // no dithering or halftoning was applied
      2: ; // an ordered dithering or halftoning was applied
      3: ; // a randomized dithering or halftoning was applied
      else
        TiffError('expected Tresholding, but found '+IntToStr(UValue));
      end;
      IFD.Tresholding:=UValue;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 263: Tresholding=',IFD.Tresholding);
      {$endif}
    end;
  264:
    begin
      // CellWidth
      IFD.CellWidth:=ReadEntryUnsigned;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 264: CellWidth=',IFD.CellWidth);
      {$endif}
    end;
  265:
    begin
      // CellLength
      IFD.CellLength:=ReadEntryUnsigned;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 265: CellLength=',IFD.CellLength);
      {$endif}
    end;
  266:
    begin
      // FillOrder
      UValue:=ReadEntryUnsigned;
      case UValue of
      1: IFD.FillOrder:=1; // left to right = high to low
      2: IFD.FillOrder:=2; // left to right = low to high
      else
        TiffError('expected FillOrder, but found '+IntToStr(UValue));
      end;
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        write('TBGRAReaderTiff.ReadDirectoryEntry Tag 266: FillOrder=',IntToStr(IFD.FillOrder),'=');
        case IFD.FillOrder of
        1: write('left to right = high to low');
        2: write('left to right = low to high');
        end;
        writeln;
      end;
      {$endif}
    end;
  269:
    begin
      // DocumentName
      IFD.DocumentName:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 269: DocumentName=',IFD.DocumentName);
      {$endif}
    end;
  270:
    begin
      // ImageDescription
      IFD.ImageDescription:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 270: ImageDescription=',IFD.ImageDescription);
      {$endif}
    end;
  271:
    begin
      // Make - scanner manufacturer
      IFD.Make_ScannerManufacturer:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 271: Make_ScannerManufacturer=',IFD.Make_ScannerManufacturer);
      {$endif}
    end;
  272:
    begin
      // Model - scanner model
      IFD.Model_Scanner:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 272: Model_Scanner=',IFD.Model_Scanner);
      {$endif}
    end;
  273:
    begin
      // StripOffsets (store offset to entity, not the actual contents of the offsets)
      IFD.StripOffsets:=GetPos; //Store position of entity so we can look up multiple offsets later
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 273: StripOffsets, offset for entry=',IFD.StripOffsets);
      {$endif}
    end;
  274:
    begin
      // Orientation
      UValue:=ReadEntryUnsigned;
      case UValue of
      1: ;// 0,0 is left, top
      2: ;// 0,0 is right, top
      3: ;// 0,0 is right, bottom
      4: ;// 0,0 is left, bottom
      5: ;// 0,0 is top, left (rotated)
      6: ;// 0,0 is top, right (rotated)
      7: ;// 0,0 is bottom, right (rotated)
      8: ;// 0,0 is bottom, left (rotated)
      else
        TiffError('expected Orientation, but found '+IntToStr(UValue));
      end;
      IFD.Orientation:=UValue;
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        write('TBGRAReaderTiff.ReadDirectoryEntry Tag 274: Orientation=',IntToStr(IFD.Orientation),'=');
        case IFD.Orientation of
        1: write('0,0 is left, top');
        2: write('0,0 is right, top');
        3: write('0,0 is right, bottom');
        4: write('0,0 is left, bottom');
        5: write('0,0 is top, left (rotated)');
        6: write('0,0 is top, right (rotated)');
        7: write('0,0 is bottom, right (rotated)');
        8: write('0,0 is bottom, left (rotated)');
        end;
        writeln;
      end;
      {$endif}
    end;
  277:
    begin
      // SamplesPerPixel
      IFD.SamplesPerPixel:=ReadEntryUnsigned;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 277: SamplesPerPixel=',IFD.SamplesPerPixel);
      {$endif}
    end;
  278:
    begin
      // RowsPerStrip
      UValue:=ReadEntryUnsigned;
      if UValue=0 then
        TiffError('expected RowsPerStrip, but found '+IntToStr(UValue));
      IFD.RowsPerStrip:=UValue;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 278: RowsPerStrip=',IFD.RowsPerStrip);
      {$endif}
    end;
  279:
    begin
      // StripByteCounts (the number of bytes in each strip).
      // We're storing the position of the tag, not the various bytecounts themselves
      IFD.StripByteCounts:=GetPos;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 279: StripByteCounts, offset for entry=',IFD.StripByteCounts);
      {$endif}
    end;
  280:
    begin
      // MinSampleValue
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 280: skipping MinSampleValue');
      {$endif}
    end;
  281:
    begin
      // MaxSampleValue
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 281: skipping MaxSampleValue');
      {$endif}
    end;
  282:
    begin
      // XResolution
      IFD.XResolution:=ReadEntryRational;
      {$ifdef FPC_Debug_Image}
      try
        if Debug then
          writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 282: XResolution=',IFD.XResolution.Numerator,'/',IFD.XResolution.Denominator,'=',IFD.XResolution.Numerator/IFD.XResolution.Denominator);
      except
        //ignore division by 0
      end;
      {$endif}
    end;
  283:
    begin
      // YResolution
      IFD.YResolution:=ReadEntryRational;
      {$ifdef FPC_Debug_Image}
      try
        if Debug then
          writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 283: YResolution=',IFD.YResolution.Numerator,'/',IFD.YResolution.Denominator,'=',IFD.YResolution.Numerator/IFD.YResolution.Denominator);
      except
        //ignore division by 0
      end;      {$endif}
    end;
  284:
    begin
      // PlanarConfiguration
      SValue:=ReadEntrySigned;
      case SValue of
      TiffPlanarConfigurationChunky: ; // 1
      TiffPlanarConfigurationPlanar: ; // 2
      else
        TiffError('expected PlanarConfiguration, but found '+IntToStr(SValue));
      end;
      IFD.PlanarConfiguration:=SValue;
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        write('TBGRAReaderTiff.ReadDirectoryEntry Tag 284: PlanarConfiguration=');
        case SValue of
        TiffPlanarConfigurationChunky: write('chunky format');
        TiffPlanarConfigurationPlanar: write('planar format');
        end;
        writeln;
      end;
      {$endif}
    end;
  285:
    begin
      // PageName
      IFD.PageName:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 285: PageName="'+IFD.PageName+'"');
      {$endif}
    end;
  288:
    begin
      // FreeOffsets
      // The free bytes in a tiff file are described with FreeByteCount and FreeOffsets
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 288: skipping FreeOffsets');
      {$endif}
    end;
  289:
    begin
      // FreeByteCount
      // The free bytes in a tiff file are described with FreeByteCount and FreeOffsets
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 289: skipping FreeByteCount');
      {$endif}
    end;
  290:
    begin
      // GrayResponseUnit
      // precision of GrayResponseCurve
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 290: skipping GrayResponseUnit');
      {$endif}
    end;
  291:
    begin
      // GrayResponseCurve
      // the optical density for each possible pixel value
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 291: skipping GrayResponseCurve');
      {$endif}
    end;
  296:
    begin
      // fResolutionUnit
      UValue:=ReadEntryUnsigned;
      case UValue of
      1: IFD.ResolutionUnit:=1; // none
      2: IFD.ResolutionUnit:=2; // inch
      3: IFD.ResolutionUnit:=3; // centimeter
      else
        TiffError('expected ResolutionUnit, but found '+IntToStr(UValue));
      end;
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        write('TBGRAReaderTiff.ReadDirectoryEntry Tag 296: ResolutionUnit=');
        case IFD.ResolutionUnit of
        1: write('none');
        2: write('inch');
        3: write('centimeter');
        end;
        writeln;
      end;
      {$endif}
    end;
  297:
    begin
      // page number (starting at 0) and total number of pages
      UValue:=GetPos;
      ReadShortValues(UValue,WordBuffer,Count);
      try
        if Count<>2 then begin
          {$ifdef FPC_Debug_Image}
          if Debug then begin
            write('TBGRAReaderTiff.ReadDirectoryEntry Tag 297: PageNumber/Count: ');
            for i:=0 to Count-1 do
              write(IntToStr(WordBuffer[i]),' ');
            writeln;
          end;
          {$endif}
          TiffError('PageNumber Count=2 expected, but found '+IntToStr(Count));
        end;
        IFD.PageNumber:=WordBuffer[0];
        IFD.PageCount:=WordBuffer[1];
        if IFD.PageNumber>=IFD.PageCount then begin
          // broken order => repair
          UValue:=IFD.PageNumber;
          IFD.PageNumber:=IFD.PageCount;
          IFD.PageCount:=UValue;
        end;
      finally
        ReAllocMem(WordBuffer,0);
      end;
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 297: PageNumber=',IFD.PageNumber,'/',IFD.PageCount);
      end;
      {$endif}
    end;
  305:
    begin
      // Software
      IFD.Software:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 305: Software="',IFD.Software,'"');
      {$endif}
    end;
  306:
    begin
      // DateAndTime
      IFD.DateAndTime:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 306: DateAndTime="',IFD.DateAndTime,'"');
      {$endif}
    end;
  315:
    begin
      // Artist
      IFD.Artist:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 315: Artist="',IFD.Artist,'"');
      {$endif}
    end;
  316:
    begin
      // HostComputer
      IFD.HostComputer:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 316: HostComputer="',IFD.HostComputer,'"');
      {$endif}
    end;
  317:
    begin
      // Predictor
      UValue:=word(ReadEntryUnsigned);
      case UValue of
      1: ;
      2: ;
      else TiffError('expected Predictor, but found '+IntToStr(UValue));
      end;
      IFD.Predictor:=UValue;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 317: Predictor="',IFD.Predictor,'"');
      {$endif}
    end;
  320:
    begin
      // ColorMap: N = 3*2^BitsPerSample
      IFD.ColorMap:=GetPos;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 320: skipping ColorMap');
      {$endif}
    end;
  322:
    begin
      // TileWidth
      IFD.TileWidth:=ReadEntryUnsigned;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 322: TileWidth=',IFD.TileWidth);
      {$endif}
      if IFD.TileWidth=0 then
        TiffError('TileWidth=0');
    end;
  323:
    begin
      // TileLength = TileHeight
      IFD.TileLength:=ReadEntryUnsigned;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 323: TileLength=',IFD.TileLength);
      {$endif}
      if IFD.TileLength=0 then
        TiffError('TileLength=0');
    end;
  324:
    begin
      // TileOffsets
      IFD.TileOffsets:=GetPos;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 324: TileOffsets=',IFD.TileOffsets);
      {$endif}
      if IFD.TileOffsets=0 then
        TiffError('TileOffsets=0');
    end;
  325:
    begin
      // TileByteCounts
      IFD.TileByteCounts:=GetPos;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 325: TileByteCounts=',IFD.TileByteCounts);
      {$endif}
      if IFD.TileByteCounts=0 then
        TiffError('TileByteCounts=0');
    end;
  338:
    begin
      // ExtraSamples: if SamplesPerPixel is bigger than PhotometricInterpretation
      // then ExtraSamples is an array defining the extra samples
      // 0=unspecified
      // 1=alpha (premultiplied)
      // 2=alpha (unassociated)
      IFD.ExtraSamples:=GetPos;
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        ReadShortValues(IFD.ExtraSamples,WordBuffer,Count);
        write('TBGRAReaderTiff.ReadDirectoryEntry Tag 338: ExtraSamples: ');
        for i:=0 to Count-1 do
          write(IntToStr(WordBuffer[i]),' ');
        writeln;
        ReAllocMem(WordBuffer,0);
      end;
      {$endif}
    end;
  347:
    begin
      // ToDo: JPEGTables
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 347: skipping JPEG Tables');
      {$endif}
    end;
  512:
    begin
      // ToDo: JPEGProc
      // short
      // 1 = baseline sequential
      // 14 = lossless process with Huffman encoding
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 512: skipping JPEGProc');
      {$endif}
    end;
  513:
    begin
      // ToDo: JPEGInterchangeFormat
      // long
      // non zero: start of start of image SOI marker
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 513: skipping JPEGInterchangeFormat');
      {$endif}
    end;
  514:
    begin
      // ToDo: JPEGInterchangeFormatLength
      // long
      // length in bytes of 513
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 514: skipping JPEGInterchangeFormatLength');
      {$endif}
    end;
  515:
    begin
      // ToDo: JPEGRestartInterval
      // short
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 515: skipping JPEGRestartInterval');
      {$endif}
    end;
  517:
    begin
      // ToDo: JPEGLosslessPredictor
      // short
      // Count: SamplesPerPixels
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 517: skipping JPEGLosslessPredictor');
      {$endif}
    end;
  518:
    begin
      // ToDo: JPEGPointTransforms
      // short
      // Count: SamplesPerPixels
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 518: skipping JPEGPointTransforms');
      {$endif}
    end;
  519:
    begin
      // ToDo: JPEGQTables
      // long
      // Count: SamplesPerPixels
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 519: skipping JPEGQTables');
      {$endif}
    end;
  520:
    begin
      // ToDo: JPEGDCTables
      // long
      // Count: SamplesPerPixels
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 520: skipping JPEGDCTables');
      {$endif}
    end;
  521:
    begin
      // ToDo: JPEGACTables
      // long
      // Count: SamplesPerPixels
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 521: skipping JPEGACTables');
      {$endif}
    end;
  530:
    begin
      // ToDo: YCbCrSubSampling alias ChromaSubSampling
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 530: skipping YCbCrSubSampling alias ChromaSubSampling');
      {$endif}
    end;
  700:
    begin
      // ToDo: XMP
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 700: skipping XMP');
      {$endif}
    end;
  33432:
    begin
      // Copyright
      IFD.Copyright:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 33432: Copyright="',IFD.Copyright,'"');
      {$endif}
    end;
  34675:
    begin
      // ToDo: ICC Profile
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag 34675: skipping ICC profile');
      {$endif}
    end;
  else
    begin
      EntryType:=ReadWord;
      EntryCount:=ReadDWord;
      EntryStart:=ReadDWord;
      if (EntryType=0) and (EntryCount=0) and (EntryStart=0) then ;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.ReadDirectoryEntry Tag=',EntryTag,' Type=',EntryType,' Count=',EntryCount,' ValuesStart=',EntryStart);
      {$endif}
    end;
  end;
end;

function TBGRAReaderTiff.ReadEntryUnsigned: LongWord;
var
  EntryCount: LongWord;
  EntryType: Word;
begin
  Result:=0;
  EntryType:=ReadWord;
  EntryCount:=ReadDWord;
  if EntryCount<>1 then
    TiffError('EntryCount=1 expected, but found '+IntToStr(EntryCount));
  //writeln('TBGRAReaderTiff.ReadEntryUnsigned Tag=',EntryTag,' Type=',EntryType,' Count=',EntryCount,' ValuesStart=',EntryStart]);
  case EntryType of
  1: begin
      // byte: 8bit unsigned
      Result:=ReadByte;
    end;
  3: begin
      // short: 16bit unsigned
      Result:=ReadWord;
    end;
  4: begin
      // long: 32bit unsigned long
      Result:=ReadDWord;
    end;
  else
    TiffError('expected single unsigned value, but found type='+IntToStr(EntryType));
  end;
end;

function TBGRAReaderTiff.ReadEntrySigned: Cint32;
var
  EntryCount: LongWord;
  EntryType: Word;
begin
  Result:=0;
  EntryType:=ReadWord;
  EntryCount:=ReadDWord;
  if EntryCount<>1 then
    TiffError('EntryCount+1 expected, but found '+IntToStr(EntryCount));
  //writeln('TBGRAReaderTiff.ReadEntrySigned Tag=',EntryTag,' Type=',EntryType,' Count=',EntryCount,' ValuesStart=',EntryStart]);
  case EntryType of
  1: begin
      // byte: 8bit unsigned
      Result:=cint8(ReadByte);
    end;
  3: begin
      // short: 16bit unsigned
      Result:=cint16(ReadWord);
    end;
  4: begin
      // long: 32bit unsigned long
      Result:=cint32(ReadDWord);
    end;
  6: begin
      // sbyte: 8bit signed
      Result:=cint8(ReadByte);
    end;
  8: begin
      // sshort: 16bit signed
      Result:=cint16(ReadWord);
    end;
  9: begin
      // slong: 32bit signed long
      Result:=cint32(ReadDWord);
    end;
  else
    TiffError('expected single signed value, but found type='+IntToStr(EntryType));
  end;
end;

function TBGRAReaderTiff.ReadEntryRational: TTiffRational;
var
  EntryCount: LongWord;
  EntryStart: LongWord;
  EntryType: Word;
begin
  Result:=TiffRational0;
  EntryType:=ReadWord;
  EntryCount:=ReadDWord;
  if EntryCount<>1 then
    TiffError('EntryCount+1 expected, but found '+IntToStr(EntryCount));
  //writeln('TBGRAReaderTiff.ReadEntryUnsigned Tag=',EntryTag,' Type=',EntryType,' Count=',EntryCount,' ValuesStart=',EntryStart]);
  case EntryType of
  1: begin
      // byte: 8bit unsigned
      Result.Numerator:=ReadByte;
    end;
  3: begin
      // short: 16bit unsigned
      Result.Numerator:=ReadWord;
    end;
  4: begin
      // long: 32bit unsigned long
      Result.Numerator:=ReadDWord;
    end;
  5: begin
      // rational: Two longs: numerator + denominator
      // this does not fit into 4 bytes
      EntryStart:=ReadDWord;
      SetStreamPos(EntryStart);
      Result.Numerator:=ReadDWord;
      Result.Denominator:=ReadDWord;
    end;
  else
    TiffError('expected rational unsigned value, but found type='+IntToStr(EntryType));
  end;
end;

function TBGRAReaderTiff.ReadEntryString: string;
var
  EntryType: Word;
  EntryCount: LongWord;
  EntryStart: LongWord;
begin
  Result:='';
  EntryType:=ReadWord;
  if EntryType<>2 then
    TiffError('asciiz expected, but found '+IntToStr(EntryType));
  EntryCount:=ReadDWord;
  SetLength(Result,EntryCount-1);
  if EntryCount>4 then begin
    // long string -> next 4 LongWord is the offset
    EntryStart:=ReadDWord;
    SetStreamPos(EntryStart);
    s.Read(Result[1],EntryCount-1);
  end else begin
    // short string -> stored directly in the next 4 bytes
    if Result<>'' then
      s.Read(Result[1],length(Result));
    // skip rest of 4 bytes
    if length(Result)<4 then
      s.Read(EntryStart,4-length(Result));
  end;
end;

function TBGRAReaderTiff.ReadByte: Byte;
begin
  Result:=s.ReadByte;
end;

function TBGRAReaderTiff.ReadWord: Word;
begin
  Result:=FixEndian(s.ReadWord);
end;

function TBGRAReaderTiff.ReadDWord: LongWord;
begin
  Result:=FixEndian(s.ReadDWord);
end;

procedure TBGRAReaderTiff.ReadValues(StreamPos: LongWord; out EntryType: word; out
  EntryCount: LongWord; out Buffer: Pointer; out ByteCount: PtrUInt);
var
  EntryStart: LongWord;
begin
  Buffer:=nil;
  ByteCount:=0;
  EntryType:=0;
  EntryCount:=0;
  SetStreamPos(StreamPos);
  ReadWord; // skip tag
  EntryType:=ReadWord;
  EntryCount:=ReadDWord;
  if EntryCount=0 then exit;
  case EntryType of
  1,6,7: ByteCount:=EntryCount; // byte
  2: ByteCount:=EntryCount; // asciiz
  3,8: ByteCount:=2*EntryCount; // short
  4,9: ByteCount:=4*EntryCount; // long
  5,10: ByteCount:=8*EntryCount; // rational
  11: ByteCount:=4*EntryCount; // single
  12: ByteCount:=8*EntryCount; // double
  else
    TiffError('invalid EntryType '+IntToStr(EntryType));
  end;
  if ByteCount>4 then begin
    EntryStart:=ReadDWord;
    SetStreamPos(EntryStart);
  end;
  GetMem(Buffer,ByteCount);
  s.Read(Buffer^,ByteCount);
end;

procedure TBGRAReaderTiff.ReadShortOrLongValues(StreamPos: LongWord; out
  Buffer: PLongWord; out Count: LongWord);
var
  p: Pointer;
  ByteCount: PtrUInt;
  EntryType: word;
  i: LongWord;
begin
  Buffer:=nil;
  Count:=0;
  p:=nil;
  try
    ReadValues(StreamPos,EntryType,Count,p,ByteCount);
    if Count=0 then exit;
    if EntryType=3 then begin
      // short
      GetMem(Buffer,SizeOf(LongWord)*Count);
      for i:=0 to Count-1 do
        Buffer[i]:=FixEndian(PWord(p)[i]);
    end else if EntryType=4 then begin
      // long
      Buffer:=p;
      p:=nil;
      if FReverseEndian then
        for i:=0 to Count-1 do
          Buffer[i]:=FixEndian(PLongWord(Buffer)[i]);
    end else
      TiffError('only short or long allowed');
  finally
    if p<>nil then FreeMem(p);
  end;
end;

procedure TBGRAReaderTiff.ReadShortValues(StreamPos: LongWord; out Buffer: PWord;
  out Count: LongWord);
var
  p: Pointer;
  ByteCount: PtrUInt;
  EntryType: word;
  i: LongWord;
begin
  Buffer:=nil;
  Count:=0;
  p:=nil;
  try
    ReadValues(StreamPos,EntryType,Count,p,ByteCount);
    //writeln('ReadShortValues ',FReverseEndian,' ',EntryType,' Count=',Count,' ByteCount=',ByteCount);
    if Count=0 then exit;
    if EntryType=3 then begin
      // short
      Buffer:=p;
      p:=nil;
      if FReverseEndian then
        for i:=0 to Count-1 do
          Buffer[i]:=FixEndian(Buffer[i]);
      //for i:=0 to Count-1 do writeln(i,' ',Buffer[i]);
    end else
      TiffError('only short allowed, but found '+IntToStr(EntryType));
  finally
    if p<>nil then FreeMem(p);
  end;
end;

procedure TBGRAReaderTiff.LoadImageFromStream(Index: integer);
var
  IFD: TTiffIFD;
begin
  {$ifdef FPC_Debug_Image}
  if Debug then
    writeln('TBGRAReaderTiff.LoadImageFromStream Index=',Index);
  {$endif}
  IFD:=Images[Index];
  LoadImageFromStream(IFD);
end;

procedure TBGRAReaderTiff.LoadImageFromStream(IFD: TTiffIFD);
var
  SampleCnt: LongWord;
  SampleBits: PWord;
  ChannelValues, LastChannelValues: array of word;
  All8Bit, All16Bit: boolean;

  procedure CheckBitCount;
  var
    Channel: LongWord;
  begin
    All8Bit := true;
    All16Bit := true;
    for Channel := 0 to SampleCnt-1 do
    begin
      if SampleBits[Channel] <> 8 then All8Bit:= false;
      if SampleBits[Channel] <> 16 then All16Bit:= false;
    end;
  end;

var
  PaletteCnt,PaletteStride: LongWord;
  PaletteValues: PWord;

  AlphaChannel: integer;
  PremultipliedAlpha: boolean;

  procedure InitColor;
  var Channel: LongWord;
  begin
    SetLength(ChannelValues, SampleCnt);
    SetLength(LastChannelValues, SampleCnt);
    for Channel := 0 to SampleCnt-1 do
      LastChannelValues[Channel] := 0;
  end;

  procedure ReadNext16BitData(var Run: Pointer);
  var Channel: PtrUInt;
  begin
    if FReverseEndian then
    begin
      if IFD.Predictor=2 then
      begin
        for Channel := 0 to SampleCnt-1 do
        begin
          {$PUSH}{$Q-}
          //Inc(ChannelValues[Channel],Swap(Word(Run^)));   original
          LastChannelValues[Channel] := LastChannelValues[Channel]+Swap(Word(Run^));
          ChannelValues[Channel] :=LastChannelValues[Channel];
          {$POP}
          inc(Run, 2);
        end;
      end else
      begin
        for Channel := 0 to SampleCnt-1 do
        begin
          ChannelValues[Channel] := Swap(Word(Run^));
          inc(Run, 2);
        end;
      end;
    end else
    begin
      if IFD.Predictor=2 then
      begin
        for Channel := 0 to SampleCnt-1 do
        begin
          {$PUSH}{$Q-}
          //Inc(ChannelValues[Channel],Word(Run^)); original
          LastChannelValues[Channel] := (LastChannelValues[Channel]+Word(Run^));
          ChannelValues[Channel] :=LastChannelValues[Channel];
          {$POP}
          inc(Run, 2);
        end;
      end else
      begin
        for Channel := 0 to SampleCnt-1 do
        begin
          ChannelValues[Channel] := Word(Run^);
          inc(Run, 2);
        end;
      end;
    end;
  end;

  procedure ReadNext8BitData(var Run: Pointer);
  var Channel: PtrUInt;
  begin
    if IFD.Predictor=2 then
    begin
      for Channel := 0 to SampleCnt-1 do
      begin
        {$PUSH}{$Q-}
        LastChannelValues[Channel] := (LastChannelValues[Channel]+Byte(Run^)) and $ff;
        ChannelValues[Channel] := LastChannelValues[Channel]+(LastChannelValues[Channel] shl 8);
        {$POP}
        inc(Run);
      end;
    end else
    begin
      for Channel := 0 to SampleCnt-1 do
      begin
        ChannelValues[Channel] := Byte(Run^)+(Byte(Run^) shl 8);
        inc(Run);
      end;
    end;
  end;

  procedure ReadNextPixelData(var Run: Pointer; var BitPos: byte);
  var Channel: LongWord;
  begin
    for Channel := 0 to SampleCnt-1 do
      ReadImgValue(SampleBits[Channel], Run,BitPos,IFD.FillOrder,
                   IFD.Predictor,LastChannelValues[Channel],
                   ChannelValues[Channel]);
  end;

  procedure GetPixelAsLab(out lab: TLabA);
  begin
    lab.L := 0;
    lab.a := 0;
    lab.b := 0;
    lab.alpha := 1;

    case IFD.PhotoMetricInterpretation of
    8: begin
         case IFD.GrayBits of
           8,16: lab.L := ChannelValues[0]*(100/65535);
           0:begin
               lab.L := ChannelValues[0]*(100/65535);
               case IFD.RedBits of
                 16: lab.a := SmallInt(ChannelValues[1])/256;
                 8: lab.a := ShortInt(ChannelValues[1] shr 8);
               end;
               case IFD.BlueBits of
                 16: lab.b := SmallInt(ChannelValues[2])/256;
                 8: lab.b := ShortInt(ChannelValues[2] shr 8);
               end;
             end;
         end;
       end;
    9: begin
         case IFD.GrayBits of
           16: lab.L := ChannelValues[0]*(100/65280);
           8: lab.L := ChannelValues[0]*(100/65535);
           0:begin
               case IFD.GreenBits of
                 16: lab.L := ChannelValues[0]*(100/65280);
                 8: lab.L := ChannelValues[0]*(100/65535);
               end;
               case IFD.RedBits of
                 16: lab.a := (ChannelValues[1]-32768)/256;
                 8: lab.a := (ChannelValues[1] shr 8)-128;
               end;
               case IFD.BlueBits of
                 16: lab.b := (ChannelValues[2]-32768)/256;
                 8: lab.b := (ChannelValues[2] shr 8)-128;
               end;
             end;
         end;
       end;
     //10: ITULAB: ITU L*a*b*
     //32844: LOGL: CIE Log2(L)
     //32845: LOGLUV: CIE Log2(L) (u',v')
    else
      TiffError('PhotometricInterpretation='+IntToStr(IFD.PhotoMetricInterpretation)+' not supported');
    end;

    if AlphaChannel >= 0 then
      lab.alpha:= ChannelValues[AlphaChannel]/65535;
  end;

var
  FPColorValue: TFPColor;

  procedure GetPixelAsFPColor;
  var PaletteIndex: LongWord;
    GrayValue: Word;
    lab: TLabA;
    cmyk: TStdCMYK;
  begin
    if IFD.PhotoMetricInterpretation >= 8 then
    begin
      GetPixelAsLab(lab);
      FPColorValue.FromLabA(lab);
      exit;
    end;

    case IFD.PhotoMetricInterpretation of
    0,1: // 0:bilevel grayscale 0 is white; 1:0 is black
      begin
        GrayValue := ChannelValues[0];
        if IFD.PhotoMetricInterpretation=0 then
          GrayValue:=$ffff-GrayValue;
        FPColorValue.red  := GrayValue;
        FPColorValue.green:= GrayValue;
        FPColorValue.blue := GrayValue;
        FPColorValue.alpha := alphaOpaque;
      end;

    2: // RGB(A)
      begin
        FPColorValue.red  := ChannelValues[0];
        FPColorValue.green:= ChannelValues[1];
        FPColorValue.blue := ChannelValues[2];
        FPColorValue.alpha := alphaOpaque;
      end;

    3: //3 Palette/color map indexed
      begin
        PaletteIndex := ChannelValues[0] shr (16 - SampleBits[0]);
        FPColorValue.red  := PaletteValues[PaletteIndex];
        FPColorValue.green:= PaletteValues[PaletteIndex+PaletteStride];
        FPColorValue.blue := PaletteValues[PaletteIndex+2*PaletteStride];
        FPColorValue.alpha := alphaOpaque;
      end;

    //4 Mask/holdout mask (obsolete by TIFF 6.0 specification)

    5: // CMYK plus optional alpha
      begin
        //FPColorValue:=CMYKToFPColor(ChannelValues[0],ChannelValues[1],ChannelValues[2],ChannelValues[3]); test the differences
        cmyk :=TStdCMYK.New(ChannelValues[0]/$ffff, ChannelValues[1]/$ffff, ChannelValues[2]/$ffff, ChannelValues[3]/$ffff);
        FPColorValue :=cmyk.ToExpandedPixel.ToFPColor(true); //MaxM: in Future we can use GammaCompression
      end;

     //6: YCBCR: CCIR 601
    else
      TiffError('PhotometricInterpretation='+IntToStr(IFD.PhotoMetricInterpretation)+' not supported');
    end;

    if AlphaChannel >= 0 then
    begin
      FPColorValue.alpha:= ChannelValues[AlphaChannel];
      if PremultipliedAlpha and (FPColorValue.alpha <> alphaOpaque) and (FPColorValue.alpha <> 0) then
      begin
        FPColorValue.red := (FPColorValue.red * alphaOpaque + FPColorValue.alpha div 2) div FPColorValue.alpha;
        FPColorValue.green := (FPColorValue.green * alphaOpaque + FPColorValue.alpha div 2) div FPColorValue.alpha;
        FPColorValue.blue := (FPColorValue.blue * alphaOpaque + FPColorValue.alpha div 2) div FPColorValue.alpha;
      end;
    end;
  end;

var
  ChunkOffsets: PLongWord;
  ChunkByteCounts: PLongWord;
  Chunk: PByte;
  ChunkCount: LongWord;
  ChunkIndex: LongWord;
  CurCount: LongWord;
  CurOffset: LongWord;
  CurByteCnt: PtrInt;
  Run: PByte;
  BitPos: Byte;
  x, y, cx, cy, dx1,dy1, dx2,dy2, sx, sy: integer;
  SampleBitsPerPixel: LongWord;
  CurFPImg: TFPCustomImage;
  aContinue, ConvertFromLab: Boolean;
  ExpectedChunkLength: PtrInt;
  ChunkType: TTiffChunkType;
  TilesAcross, TilesDown: LongWord;
  ChunkLeft, ChunkTop, ChunkWidth, ChunkHeight: LongWord;
  ChunkBytesPerLine: LongWord;

  LabArray: array of TLabA;
  ConversionFromLab: TBridgedConversion;
  DestStride: PtrInt;
  PDest: PByte;
  CurPixelValue: TBGRAPixel;

  procedure ComputeDestStride;
  begin
    DestStride := dy1*TCustomUniversalBitmap(CurFPImg).RowSize;
    if TCustomUniversalBitmap(CurFPImg).LineOrder = riloBottomToTop then
      DestStride := -DestStride;
    inc(DestStride, dx1*PtrInt(TCustomUniversalBitmap(CurFPImg).Colorspace.GetSize));
  end;

  procedure ReadResolutionValues;
  begin
    {$IF FPC_FULLVERSION<30301}
    if (CurFPImg is TCustomUniversalBitmap) then
    with TCustomUniversalBitmap(CurFPImg) do
    {$ELSE}
    with CurFPImg do
    {$ENDIF}
    begin
        ResolutionUnit :=TifResolutionUnitToResolutionUnit(IFD.ResolutionUnit);
        if (IFD.XResolution.Denominator>0)
        then ResolutionX :=IFD.XResolution.Numerator/IFD.XResolution.Denominator
        else ResolutionX :=IFD.XResolution.Numerator;
        if (IFD.YResolution.Denominator>0)
        then ResolutionY :=IFD.YResolution.Numerator/IFD.YResolution.Denominator
        else ResolutionY :=IFD.YResolution.Numerator;
     end;
  end;

begin
  if (IFD.ImageWidth=0) or (IFD.ImageHeight=0) then
    exit;

  if IFD.PhotoMetricInterpretation=High(IFD.PhotoMetricInterpretation) then
    TiffError('missing PhotometricInterpretation');
  if IFD.BitsPerSample=0 then
    TiffError('missing BitsPerSample');
  if IFD.TileWidth>0 then begin
    ChunkType:=tctTile;
    if IFD.TileLength=0 then
      TiffError('missing TileLength');
    if IFD.TileOffsets=0 then
      TiffError('missing TileOffsets');
    if IFD.TileByteCounts=0 then
      TiffError('missing TileByteCounts');
  end else begin
    ChunkType:=tctStrip;
    if IFD.RowsPerStrip=0 then
      TiffError('missing RowsPerStrip');
    if IFD.StripOffsets=0 then
      TiffError('missing StripOffsets');
    if IFD.StripByteCounts=0 then
      TiffError('missing StripByteCounts');
  end;

  if IFD.PlanarConfiguration > 1 then
     TiffError('Planar configuration not handled');

  {$ifdef FPC_Debug_Image}
  if Debug then
    writeln('TBGRAReaderTiff.LoadImageFromStream reading ...');
  {$endif}

  ChunkOffsets:=nil;
  ChunkByteCounts:=nil;
  Chunk:=nil;
  SampleBits:=nil;
  try
    // read chunk starts and sizes
    if ChunkType=tctTile then begin
      TilesAcross:=(IFD.ImageWidth+IFD.TileWidth-1) div IFD.TileWidth;
      TilesDown:=(IFD.ImageHeight+IFD.TileLength-1) div IFD.TileLength;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.LoadImageFromStream TilesAcross=',TilesAcross,' TilesDown=',TilesDown);
      {$endif}
      ChunkCount := TilesAcross * TilesDown;
      ReadShortOrLongValues(IFD.TileOffsets,ChunkOffsets,CurCount);
      if CurCount<ChunkCount then
        TiffError('number of TileCounts is wrong');
      ReadShortOrLongValues(IFD.TileByteCounts,ChunkByteCounts,CurCount);
      if CurCount<ChunkCount then
        TiffError('number of TileByteCounts is wrong');
    end else begin //strip
      ChunkCount:=((IFD.ImageHeight-1) div IFD.RowsPerStrip)+1;
      ReadShortOrLongValues(IFD.StripOffsets,ChunkOffsets,CurCount);
      if CurCount<ChunkCount then
        TiffError('number of StripCounts is wrong');
      ReadShortOrLongValues(IFD.StripByteCounts,ChunkByteCounts,CurCount);
      if CurCount<ChunkCount then
        TiffError('number of StripByteCounts is wrong');
    end;

    // read image sample structure
    ReadImageSampleProperties(IFD, AlphaChannel, PremultipliedAlpha,
      SampleCnt, SampleBits, SampleBitsPerPixel,
      PaletteCnt, PaletteValues);

    PaletteStride := PaletteCnt div 3;
    CheckBitCount;

    // create FPimage
    DoCreateImage(IFD);
    if IFD.Img=nil then
    begin
      IFD.Img := BGRABitmapFactory.Create;
      IFD.FreeImg := true;
    end;
    CurFPImg:=IFD.Img;
    if CurFPImg=nil then exit;

    //Resolution
    ReadResolutionValues;

    SetFPImgExtras(CurFPImg, IFD);

    case IFD.Orientation of
    0,1..4: CurFPImg.SetSize(IFD.ImageWidth,IFD.ImageHeight);
    5..8: CurFPImg.SetSize(IFD.ImageHeight,IFD.ImageWidth);
    end;

    {$ifdef FPC_Debug_Image}
    if Debug then
      writeln('TBGRAReaderTiff.LoadImageFromStream SampleBitsPerPixel=',SampleBitsPerPixel);
    {$endif}

    LabArray := nil;
    if (IFD.PhotoMetricInterpretation >= 8) and
       (CurFPImg is TCustomUniversalBitmap) then
    begin
      ConvertFromLab := true;
      ConversionFromLab := TLabAColorspace.GetBridgedConversion(TCustomUniversalBitmap(CurFPImg).Colorspace)
    end else
      ConvertFromLab := false;

    // read chunks
    for ChunkIndex:=0 to ChunkCount-1 do begin
      CurOffset:=ChunkOffsets[ChunkIndex];
      CurByteCnt:=ChunkByteCounts[ChunkIndex];
      //writeln('TBGRAReaderTiff.LoadImageFromStream CurOffset=',CurOffset,' CurByteCnt=',CurByteCnt);
      if CurByteCnt<=0 then continue;
      ReAllocMem(Chunk,CurByteCnt);
      SetStreamPos(CurOffset);
      s.Read(Chunk^,CurByteCnt);

      // decompress
      if ChunkType=tctTile then
        ExpectedChunkLength:=(SampleBitsPerPixel*IFD.TileWidth+7) div 8*IFD.TileLength
      else
        ExpectedChunkLength:=((SampleBitsPerPixel*IFD.ImageWidth+7) div 8)*IFD.RowsPerStrip;
      case IFD.Compression of
      TiffCompressionNone: ;
      TiffCompressionPackBits: DecodePackBits(Chunk,CurByteCnt);
      TiffCompressionLZW: DecodeLZW(Chunk,CurByteCnt);
      TiffCompressionDeflateAdobe,
      TiffCompressionDeflateZLib: DecodeDeflate(Chunk,CurByteCnt,ExpectedChunkLength);
      else
        TiffError('compression '+TiffCompressionName(IFD.Compression)+' not supported yet');
      end;
      if CurByteCnt<=0 then continue;

      // compute current chunk area
      if ChunkType=tctTile then begin
        ChunkLeft:=(ChunkIndex mod TilesAcross)*IFD.TileWidth;
        ChunkTop:=(ChunkIndex div TilesAcross)*IFD.TileLength;
        ChunkWidth:=Min(IFD.TileWidth,IFD.ImageWidth-ChunkLeft);
        ChunkHeight:=Min(IFD.TileLength,IFD.ImageHeight-ChunkTop);
        ChunkBytesPerLine:=(SampleBitsPerPixel*ChunkWidth+7) div 8;
        ExpectedChunkLength:=ChunkBytesPerLine*ChunkHeight;
        if CurByteCnt<ExpectedChunkLength then begin
          //writeln('TBGRAReaderTiff.LoadImageFromStream SampleBitsPerPixel=',SampleBitsPerPixel,' IFD.ImageWidth=',IFD.ImageWidth,' IFD.ImageHeight=',IFD.ImageHeight,' y=',y,' IFD.TileWidth=',IFD.TileWidth,' IFD.TileLength=',IFD.TileLength,' ExpectedChunkLength=',ExpectedChunkLength,' CurByteCnt=',CurByteCnt);
          TiffError('TBGRAReaderTiff.LoadImageFromStream Tile too short ByteCnt='+IntToStr(CurByteCnt)+' ChunkWidth='+IntToStr(ChunkWidth)+' ChunkHeight='+IntToStr(ChunkHeight)+' expected='+IntToStr(ExpectedChunkLength));
        end else if CurByteCnt>ExpectedChunkLength then begin
          // boundary tiles have padding
          ChunkBytesPerLine:=(SampleBitsPerPixel*IFD.TileWidth+7) div 8;
        end;
      end else begin //tctStrip
        ChunkLeft:=0;
        ChunkTop:=IFD.RowsPerStrip*ChunkIndex;
        ChunkWidth:=IFD.ImageWidth;
        ChunkHeight:=Min(IFD.RowsPerStrip,IFD.ImageHeight-ChunkTop);
        ChunkBytesPerLine:=(SampleBitsPerPixel*ChunkWidth+7) div 8;
        ExpectedChunkLength:=ChunkBytesPerLine*ChunkHeight;
        //writeln('TBGRAReaderTiff.LoadImageFromStream SampleBitsPerPixel=',SampleBitsPerPixel,' IFD.ImageWidth=',IFD.ImageWidth,' IFD.ImageHeight=',IFD.ImageHeight,' y=',y,' IFD.RowsPerStrip=',IFD.RowsPerStrip,' ExpectedChunkLength=',ExpectedChunkLength,' CurByteCnt=',CurByteCnt);
        if CurByteCnt<ExpectedChunkLength then
          TiffError('TBGRAReaderTiff.LoadImageFromStream Strip too short ByteCnt='+IntToStr(CurByteCnt)+' ChunkWidth='+IntToStr(ChunkWidth)+' ChunkHeight='+IntToStr(ChunkHeight)+' expected='+IntToStr(ExpectedChunkLength));
      end;

      // progress
      aContinue:=true;
      Progress(psRunning, 0, false, Rect(0,0,IFD.ImageWidth,ChunkTop), '', aContinue);
      if not aContinue then break;

      // Orientation
      if IFD.Orientation in [1..4] then begin
        sx:=ChunkLeft; sy:=ChunkTop;
        dy1 := 0; dx2 := 0;
        case IFD.Orientation of
        1: begin dx1:=1; dy2:=1; end;// 0,0 is left, top
        2: begin sx:=IFD.ImageWidth-sx-1; dx1:=-1; dy2:=1; end;// 0,0 is right, top
        3: begin sx:=IFD.ImageWidth-sx-1; dx1:=-1; sy:=IFD.ImageHeight-sy-1; dy2:=-1; end;// 0,0 is right, bottom
        4: begin dx1:=1; sy:=IFD.ImageHeight-sy-1; dy2:=-1; end;// 0,0 is left, bottom
        end;
      end else begin
        // rotated
        sx:=ChunkTop; sy:=ChunkLeft;
        dx1 := 0; dy2 := 0;
        case IFD.Orientation of
        5: begin dy1:=1; dx2:=1; end;// 0,0 is top, left (rotated)
        6: begin dy1:=1; sx:=IFD.ImageWidth-sx-1; dx2:=-1; end;// 0,0 is top, right (rotated)
        7: begin sy:=IFD.ImageHeight-sy-1; dy1:=-1; sx:=IFD.ImageHeight-sx-1; dx2:=-1; end;// 0,0 is bottom, right (rotated)
        8: begin sy:=IFD.ImageHeight-sy-1; dy1:=-1; dx2:=1; end;// 0,0 is bottom, left (rotated)
        end;
      end;

      //writeln('TBGRAReaderTiff.LoadImageFromStream Chunk ',ChunkIndex,' ChunkLeft=',ChunkLeft,' ChunkTop=',ChunkTop,' IFD.ImageWidth=',IFD.ImageWidth,' IFD.ImageHeight=',IFD.ImageHeight,' ChunkWidth=',ChunkWidth,' ChunkHeight=',ChunkHeight,' PaddingRight=',PaddingRight);
      for cy:=0 to ChunkHeight-1 do begin
        Run:=Chunk+ChunkBytesPerLine*cy;
        BitPos := 0;
        InitColor;
        //writeln('TBGRAReaderTiff.LoadImageFromStream (x,y)=(',sx,',',sy,')');

        if ConvertFromLab then
        begin
          if length(LabArray)<ChunkWidth then setlength(LabArray, ChunkWidth);

          if All16Bit then
          begin
            for cx:=0 to ChunkWidth-1 do begin
              ReadNext16BitData(Run);
              GetPixelAsLab(LabArray[cx]);
            end;
          end else
          if All8Bit then
          begin
            for cx:=0 to ChunkWidth-1 do begin
              ReadNext8BitData(Run);
              GetPixelAsLab(LabArray[cx]);
            end;
          end else
          begin
            for cx:=0 to ChunkWidth-1 do begin
              ReadNextPixelData(Run,BitPos);
              GetPixelAsLab(LabArray[cx]);
            end;
          end;

          ComputeDestStride;
          ConversionFromLab.Convert(@LabArray[0], TCustomUniversalBitmap(CurFPImg).GetPixelAddress(sx,sy),
                                    ChunkWidth, sizeof(TLabA), DestStride, nil);
        end else
        begin
          x:= sx;
          y:= sy;
          if All16Bit then
          begin
            if (CurFPImg is TBGRACustomBitmap) and (IFD.Predictor <> 2) then
            begin
              ComputeDestStride;
              PDest := TBGRACustomBitmap(CurFPImg).GetPixelAddress(sx,sy);
              if (IFD.PhotoMetricInterpretation = 0) and (SampleCnt = 1) then
              begin
                for cx:=0 to ChunkWidth-1 do begin
                  PBGRAPixel(PDest)^.red:= FastRoundDiv257(not (PWord(Run)^));
                  PBGRAPixel(PDest)^.green:= PBGRAPixel(PDest)^.red;
                  PBGRAPixel(PDest)^.blue:= PBGRAPixel(PDest)^.red;
                  PBGRAPixel(PDest)^.alpha:= 255;
                  inc(Run, 2);
                  inc(PDest, DestStride);
                end;
              end else
              if (IFD.PhotoMetricInterpretation = 1) and (SampleCnt = 1) then
              begin
                for cx:=0 to ChunkWidth-1 do begin
                  PBGRAPixel(PDest)^.red:= FastRoundDiv257(PWord(Run)^);
                  PBGRAPixel(PDest)^.green:= PBGRAPixel(PDest)^.red;
                  PBGRAPixel(PDest)^.blue:= PBGRAPixel(PDest)^.red;
                  PBGRAPixel(PDest)^.alpha:= 255;
                  inc(Run, 2);
                  inc(PDest, DestStride);
                end;
              end else
              if (IFD.PhotoMetricInterpretation = 2) and (SampleCnt = 3) then
              begin
                for cx:=0 to ChunkWidth-1 do begin
                  PBGRAPixel(PDest)^.red:= FastRoundDiv257(PWord(Run)^);
                  PBGRAPixel(PDest)^.green:= FastRoundDiv257(PWord(Run+2)^);
                  PBGRAPixel(PDest)^.blue:= FastRoundDiv257(PWord(Run+4)^);
                  PBGRAPixel(PDest)^.alpha:= 255;
                  inc(Run, 6);
                  inc(PDest, DestStride);
                end;
              end else
              for cx:=0 to ChunkWidth-1 do begin
                 ReadNext16BitData(Run);
                 GetPixelAsFPColor;
                 PBGRAPixel(PDest)^.red:= FastRoundDiv257(FPColorValue.red);
                 PBGRAPixel(PDest)^.green:= FastRoundDiv257(FPColorValue.green);
                 PBGRAPixel(PDest)^.blue:= FastRoundDiv257(FPColorValue.blue);
                 PBGRAPixel(PDest)^.alpha:= FastRoundDiv257(FPColorValue.alpha);
                 inc(PDest, DestStride);
               end;
            end else
              for cx:=0 to ChunkWidth-1 do begin
                ReadNext16BitData(Run);
                GetPixelAsFPColor;
                CurFPImg.Colors[x,y]:= FPColorValue;
                // next column
                inc(x,dx1);
                inc(y,dy1);
              end;
          end else
          if All8Bit then
          begin
            if CurFPImg is TBGRACustomBitmap then
            begin
              ComputeDestStride;
              PDest := TBGRACustomBitmap(CurFPImg).GetPixelAddress(sx,sy);
              if (IFD.PhotoMetricInterpretation = 0) and (SampleCnt = 1) then
              begin
                if IFD.Predictor = 2 then
                begin
                  CurPixelValue := BGRAPixelTransparent;
                  for cx:=0 to ChunkWidth-1 do begin
                    {$PUSH}{$R-}inc(CurPixelValue.green, run^);{$POP}
                    PBGRAPixel(PDest)^.red:= not CurPixelValue.green;
                    PBGRAPixel(PDest)^.green:= not CurPixelValue.green;
                    PBGRAPixel(PDest)^.blue:= not CurPixelValue.green;
                    PBGRAPixel(PDest)^.alpha:= 255;
                    inc(Run);
                    inc(PDest, DestStride);
                  end;
                end else
                for cx:=0 to ChunkWidth-1 do begin
                  PBGRAPixel(PDest)^.red:= not (Run^);
                  PBGRAPixel(PDest)^.green:= not (Run^);
                  PBGRAPixel(PDest)^.blue:= not (Run^);
                  PBGRAPixel(PDest)^.alpha:= 255;
                  inc(Run);
                  inc(PDest, DestStride);
                end;
              end else
              if (IFD.PhotoMetricInterpretation = 1) and (SampleCnt = 1) then
              begin
                if IFD.Predictor = 2 then
                begin
                  CurPixelValue := BGRAPixelTransparent;
                  for cx:=0 to ChunkWidth-1 do begin
                    {$PUSH}{$R-}inc(CurPixelValue.green, run^);{$POP}
                    PBGRAPixel(PDest)^.red:= CurPixelValue.green;
                    PBGRAPixel(PDest)^.green:= CurPixelValue.green;
                    PBGRAPixel(PDest)^.blue:= CurPixelValue.green;
                    PBGRAPixel(PDest)^.alpha:= 255;
                    inc(Run);
                    inc(PDest, DestStride);
                  end;
                end else
                for cx:=0 to ChunkWidth-1 do begin
                  PBGRAPixel(PDest)^.red:= Run^;
                  PBGRAPixel(PDest)^.green:= Run^;
                  PBGRAPixel(PDest)^.blue:= Run^;
                  PBGRAPixel(PDest)^.alpha:= 255;
                  inc(Run);
                  inc(PDest, DestStride);
                end;
              end else
              if (IFD.PhotoMetricInterpretation = 2) and (SampleCnt = 3) then
              begin
                if IFD.Predictor = 2 then
                begin
                  CurPixelValue := BGRAPixelTransparent;
                  for cx:=0 to ChunkWidth-1 do begin
                    {$PUSH}{$R-}inc(CurPixelValue.red, run^);{$POP}
                    {$PUSH}{$R-}inc(CurPixelValue.green, (run+1)^);{$POP}
                    {$PUSH}{$R-}inc(CurPixelValue.blue, (run+2)^);{$POP}
                    PBGRAPixel(PDest)^.red:= CurPixelValue.red;
                    PBGRAPixel(PDest)^.green:= CurPixelValue.green;
                    PBGRAPixel(PDest)^.blue:= CurPixelValue.blue;
                    PBGRAPixel(PDest)^.alpha:= 255;
                    inc(Run,3);
                    inc(PDest, DestStride);
                  end;
                end else
                for cx:=0 to ChunkWidth-1 do begin
                  PBGRAPixel(PDest)^.red:= Run^;
                  PBGRAPixel(PDest)^.green:= (Run+1)^;
                  PBGRAPixel(PDest)^.blue:= (Run+2)^;
                  PBGRAPixel(PDest)^.alpha:= 255;
                  inc(Run,3);
                  inc(PDest, DestStride);
                end;
              end else
              for cx:=0 to ChunkWidth-1 do begin
                 ReadNext8BitData(Run);
                 GetPixelAsFPColor;
                 PBGRAPixel(PDest)^.red:= FPColorValue.red shr 8;
                 PBGRAPixel(PDest)^.green:= FPColorValue.green shr 8;
                 PBGRAPixel(PDest)^.blue:= FPColorValue.blue shr 8;
                 PBGRAPixel(PDest)^.alpha:= FPColorValue.alpha shr 8;
                 inc(PDest, DestStride);
               end;
            end else
              for cx:=0 to ChunkWidth-1 do begin
                ReadNext8BitData(Run);
                GetPixelAsFPColor;
                CurFPImg.Colors[x,y]:= FPColorValue;
                // next column
                inc(x,dx1);
                inc(y,dy1);
              end;
          end else
          begin
            for cx:=0 to ChunkWidth-1 do begin
              ReadNextPixelData(Run,BitPos);
              GetPixelAsFPColor;
              CurFPImg.Colors[x,y]:= FPColorValue;
              // next column
              inc(x,dx1);
              inc(y,dy1);
            end;
          end;
        end;

        // next line
        inc(sx,dx2);
        inc(sy,dy2);
      end;
      // next chunk
    end;
  finally
    ReAllocMem(SampleBits,0);
    ReAllocMem(ChunkOffsets,0);
    ReAllocMem(ChunkByteCounts,0);
    ReAllocMem(Chunk,0);
    ReAllocMem(PaletteValues,0);
  end;
end;

procedure TBGRAReaderTiff.ReleaseStream;
begin
  s := nil;
end;

function TBGRAReaderTiff.FixEndian(w: Word): Word; inline;
begin
  if FReverseEndian then
    Result:= SwapEndian(w)
  else
    result:= w;
end;

function TBGRAReaderTiff.FixEndian(d: LongWord): LongWord; inline;
begin
  if FReverseEndian then
    Result:= SwapEndian(d)
  else
    result:= d;
end;

procedure TBGRAReaderTiff.DecodePackBits(var Buffer: Pointer; var Count: PtrInt);
var
  NewBuffer: Pointer;
  NewCount: PtrInt;
begin
  DecompressPackBits(Buffer,Count,NewBuffer,NewCount);
  FreeMem(Buffer);
  Buffer:=NewBuffer;
  Count:=NewCount;
end;

procedure TBGRAReaderTiff.DecodeLZW(var Buffer: Pointer; var Count: PtrInt);
var
  NewBuffer: Pointer;
  NewCount: PtrInt;
begin
  DecompressLZW(Buffer,Count,NewBuffer,NewCount);
  FreeMem(Buffer);
  Buffer:=NewBuffer;
  Count:=NewCount;
end;

procedure TBGRAReaderTiff.DecodeDeflate(var Buffer: Pointer; var Count: PtrInt;
  ExpectedCount: PtrInt);
var
  NewBuffer: PByte;
  NewCount: LongWord;
  ErrorMsg: String;
begin
  ErrorMsg:='';
  NewBuffer:=nil;
  try
    NewCount:=ExpectedCount;
    if not DecompressDeflate(Buffer,Count,NewBuffer,NewCount,@ErrorMsg) then
      TiffError(ErrorMsg);
    FreeMem(Buffer);
    Buffer:=NewBuffer;
    Count:=NewCount;
    NewBuffer:=nil;
  finally
    ReAllocMem(NewBuffer,0);
  end;
end;

procedure TBGRAReaderTiff.InternalRead(Str: TStream; AnImage: TFPCustomImage);
// read the biggest image
var
  aContinue: Boolean;
  BestIFD: TTiffIFD;
begin
  Clear;

  // read header
  aContinue:=true;
  Progress(psStarting, 0, False, Rect(0,0,0,0), '', aContinue);
  if not aContinue then exit;
  LoadHeaderFromStream(Str);
  LoadIFDsFromStream;

  // find the biggest image
  BestIFD := GetBiggestImage;
  Progress(psRunning, 25, False, Rect(0,0,0,0), '', aContinue);
  if not aContinue then exit;

  // read image
  if Assigned(BestIFD) then begin
    BestIFD.Img := AnImage;
    LoadImageFromStream(BestIFD);
  end;

  // end
  Progress(psEnding, 100, False, Rect(0,0,0,0), '', aContinue);
end;

function TBGRAReaderTiff.InternalCheck(Str: TStream): boolean;
var
  IFDStart: LongWord;
begin
  try
    s:=Str;
    fStartPos:=s.Position;
    Result:=ReadTiffHeader(true,IFDStart) and (IFDStart<>0);
    s.Position:=fStartPos;
  except
    Result:=false;
  end;
end;

procedure TBGRAReaderTiff.DoCreateImage(ImgFileDir: TTiffIFD);
begin
  if Assigned(OnCreateImage) then
    OnCreateImage(Self,ImgFileDir);
end;

constructor TBGRAReaderTiff.Create;
begin
  ImageList:=TFPList.Create;
end;

destructor TBGRAReaderTiff.Destroy;
begin
  Clear;
  FreeAndNil(ImageList);
  inherited Destroy;
end;

procedure TBGRAReaderTiff.Clear;
var
  i: Integer;
  Img: TTiffIFD;
begin
  for i:=ImageCount-1 downto 0 do begin
    Img:=Images[i];
    ImageList.Delete(i);
    Img.Free;
  end;
  FReverseEndian:=false;
  FreeAndNil(FIFDList);
end;

procedure DecompressPackBits(Buffer: Pointer; Count: PtrInt; out
  NewBuffer: Pointer; out NewCount: PtrInt);
{ Algorithm:
    while not got the expected number of bytes
      read one byte n
      if n in 0..127 copy the next n+1 bytes
      else if n in -127..-1 then copy the next byte 1-n times
      else continue
    end
}
var
  p: Pcint8;
  n: cint8;
  d: pcint8;
  i,j: integer;
  EndP: Pcint8;
begin
  // compute NewCount
  NewCount:=0;
  NewBuffer:=nil;
  if Count=0 then exit;
  p:=Pcint8(Buffer);
  EndP:=p+Count;
  while p<EndP do begin
    n:=p^;
    case n of
    0..127:   begin inc(NewCount,n+1);  inc(p,n+2); end; // copy the next n+1 bytes
    -127..-1: begin inc(NewCount,1-n); inc(p,2);   end; // copy the next byte 1-n times
    else inc(p); // noop
    end;
  end;

  // decompress
  if NewCount=0 then exit;
  GetMem(NewBuffer,NewCount);
  p:=Pcint8(Buffer);
  d:=Pcint8(NewBuffer);
  while p<EndP do begin
    n:=p^;
    case n of
    0..127:
      begin
        // copy the next n+1 bytes
        i:=n+1;
        inc(NewCount,i);
        inc(p);
        System.Move(p^,d^,i);
        inc(p,i);
        inc(d,i);
      end;
    -127..-1:
      begin
        // copy the next byte 1-n times
        i:=1-n;
        inc(NewCount,i);
        inc(p);
        n:=p^;
        for j:=0 to i-1 do
          d[j]:=n;
        inc(d,i);
        inc(p);
      end;
    else inc(p); // noop
    end;
  end;
end;

procedure DecompressLZW(Buffer: Pointer; Count: PtrInt; out NewBuffer: PByte;
  out NewCount: PtrInt);
type
  TLZWString = packed record
    Count: integer;
    Data: PByte;
    ShortData: array[0..3] of byte;
  end;
const
  ClearCode = 256; // clear table, start with 9bit codes
  EoiCode = 257;   // end of input
  NoCode = $7fff;
var
  NewCapacity: PtrInt;
  SrcPos: PtrInt;
  CodeBuffer: LongWord;
  CodeBufferLength: byte;
  CurBitLength: byte;
  Code: Word;
  Table: array[0..4096-258-1] of TLZWString;
  TableCount: integer;
  OldCode: Word;
  BigEndian: boolean;
  TableMargin: byte;

  procedure Error(const Msg: string);
  begin
    raise Exception.Create(Msg);
  end;

  function GetNextCode: Word;
  begin
    while CurBitLength > CodeBufferLength do
    begin
      if SrcPos >= Count then
      begin
        result := EoiCode;
        exit;
      end;
      If BigEndian then
        CodeBuffer := (CodeBuffer shl 8) or PByte(Buffer)[SrcPos]
      else
        CodeBuffer := CodeBuffer or (LongWord(PByte(Buffer)[SrcPos]) shl CodeBufferLength);
      Inc(SrcPos);
      Inc(CodeBufferLength, 8);
    end;

    if BigEndian then
    begin
      result := CodeBuffer shr (CodeBufferLength-CurBitLength);
      Dec(CodeBufferLength, CurBitLength);
      CodeBuffer := CodeBuffer and ((1 shl CodeBufferLength) - 1);
    end else
    begin
      result := CodeBuffer and ((1 shl CurBitLength)-1);
      Dec(CodeBufferLength, CurBitLength);
      CodeBuffer := CodeBuffer shr CurBitLength;
    end;
  end;

  procedure ClearTable;
  var
    i: Integer;
  begin
    for i:=0 to TableCount-1 do
      if Table[i].Data <> @Table[i].ShortData then
        ReAllocMem(Table[i].Data,0);
    TableCount:=0;
  end;

  procedure InitializeTable;
  begin
    CurBitLength:=9;
    ClearTable;
  end;

  function IsInTable(Code: word): boolean;
  begin
    Result:=Code<258+TableCount;
  end;

  procedure WriteStringFromCode(Code: integer; AddFirstChar: boolean = false);
  var
    s: TLZWString;
  begin
    //WriteLn('WriteStringFromCode Code=',Code,' AddFirstChar=',AddFirstChar,' x=',(NewCount div 4) mod IFD.ImageWidth,' y=',(NewCount div 4) div IFD.ImageWidth,' PixelByte=',NewCount mod 4);
    if Code<256 then begin
      // write byte
      s.ShortData[0] := code;
      s.Data:=@s.ShortData;
      s.Count:=1;
    end else if Code>=258 then begin
      // write string
      if Code-258>=TableCount then
        Error('LZW code out of bounds');
      s:=Table[Code-258];
    end else
      Error('LZW code out of bounds');
    if NewCount+s.Count+1>NewCapacity then begin
      NewCapacity:=NewCapacity*2+8;
      ReAllocMem(NewBuffer,NewCapacity);
    end;
    System.Move(s.Data^,NewBuffer[NewCount],s.Count);
    //for i:=0 to s.Count-1 do write(HexStr(NewBuffer[NewCount+i],2)); // debug
    inc(NewCount,s.Count);
    if AddFirstChar then begin
      NewBuffer[NewCount]:=s.Data^;
      //write(HexStr(NewBuffer[NewCount],2)); // debug
      inc(NewCount);
    end;
    //writeln(',WriteStringFromCode'); // debug
  end;

  procedure AddStringToTable(Code, AddFirstCharFromCode: integer);
  // add string from code plus first character of string from code as new string
  var
    s1, s2: TLZWString;
    p: PByte;
    NewCount: integer;
  begin
    //WriteLn('AddStringToTable Code=',Code,' FCFCode=',AddFirstCharFromCode,' TableCount=',TableCount);
    //check whether can store more codes or not
    if TableCount=high(Table)+1 then exit;
    // find string 1
    if Code<256 then begin
      // string is byte
      s1.ShortData[0] := code;
      s1.Data:=@s1.ShortData;
      s1.Count:=1;
    end else if Code>=258 then begin
      // normal string
      if Code-258>=TableCount then
        Error('LZW code out of bounds');
      s1:=Table[Code-258];
    end else
      Error('LZW code out of bounds');
    // find string 2
    if AddFirstCharFromCode<256 then begin
      // string is byte
      s2.ShortData[0] := AddFirstCharFromCode;
      s2.Data:=@s2.ShortData;
      s2.Count:=1;
    end else begin
      // normal string
      if AddFirstCharFromCode-258>=TableCount then
        Error('LZW code out of bounds');
      s2:=Table[AddFirstCharFromCode-258];
    end;
    // set new table entry
    NewCount := s1.Count+1;
    Table[TableCount].Count:= NewCount;
    if NewCount > 4 then
    begin
      p:=nil;
      GetMem(p,NewCount);
    end else
      p := @Table[TableCount].ShortData;
    Table[TableCount].Data:=p;
    System.Move(s1.Data^,p^,s1.Count);
    // add first character from string 2
    p[s1.Count]:=s2.Data^;
    // increase TableCount
    inc(TableCount);
    case TableCount+258+TableMargin of
    512,1024,2048: begin
        //check if there is room for a greater code
        if (Count-SrcPos) shl 3 + integer(CodeBufferLength) > integer(CurBitLength) then
          inc(CurBitLength);
      end;
    end;
  end;

begin
  NewBuffer:=nil;
  NewCount:=0;
  if Count=0 then exit;
  //WriteLn('DecompressLZW START Count=',Count);
  //for SrcPos:=0 to 19 do
  //  write(HexStr(PByte(Buffer)[SrcPos],2));
  //writeln();

  NewCapacity:=Count*2;
  ReAllocMem(NewBuffer,NewCapacity);

  if PByte(Buffer)[0] = $80 then
  begin
    BigEndian := true; //endian-ness of LZW is not necessarily consistent with the rest of the file
    TableMargin := 1; //keep one free code to be able to write EOI code
  end else
  begin
    BigEndian := false;
    TableMargin := 0;
  end;
  SrcPos:=0;
  CurBitLength:=9;
  CodeBufferLength := 0;
  CodeBuffer := 0;
  TableCount:=0;
  OldCode := NoCode;
  try
    repeat
      Code:=GetNextCode;
      //WriteLn('DecompressLZW Code=',Code);
      if Code=EoiCode then break;
      if Code=ClearCode then begin
        InitializeTable;
        Code:=GetNextCode;
        //WriteLn('DecompressLZW after clear Code=',Code);
        if Code=EoiCode then break;
        if Code=ClearCode then
          Error('LZW code out of bounds');
        WriteStringFromCode(Code);
        OldCode:=Code;
      end else begin
        if Code<TableCount+258 then begin
          WriteStringFromCode(Code);
          if OldCode <> NoCode then
            AddStringToTable(OldCode,Code);
          OldCode:=Code;
        end else if (Code=TableCount+258) and (OldCode <> NoCode) then begin
          WriteStringFromCode(OldCode,true);
          AddStringToTable(OldCode,OldCode);
          OldCode:=Code;
        end else
          Error('LZW code out of bounds');
      end;
    until false;
  finally
    ClearTable;
  end;

  ReAllocMem(NewBuffer,NewCount);
end;

function DecompressDeflate(Compressed: PByte; CompressedCount: LongWord;
  out Decompressed: PByte; var DecompressedCount: LongWord;
  ErrorMsg: PAnsiString = nil): boolean;
var
  stream : z_stream;
  err : integer;
begin
  Result:=false;

  //writeln('DecompressDeflate START');
  Decompressed:=nil;
  if CompressedCount=0 then begin
    DecompressedCount:=0;
    exit;
  end;

  err := inflateInit(stream{%H-});
  if err <> Z_OK then begin
    if ErrorMsg<>nil then
      ErrorMsg^:='inflateInit failed';
    exit;
  end;

  // set input = compressed data
  stream.avail_in := CompressedCount;
  stream.next_in  := Compressed;

  // set output = decompressed data
  if DecompressedCount=0 then
    DecompressedCount:=CompressedCount;
  Getmem(Decompressed,DecompressedCount);
  stream.avail_out := DecompressedCount;
  stream.next_out := Decompressed;

  // Finish the stream
  while TRUE do begin
    //writeln('run: total_in=',stream.total_in,' avail_in=',stream.avail_in,' total_out=',stream.total_out,' avail_out=',stream.avail_out);
    if (stream.avail_out=0) then begin
      // need more space
      if DecompressedCount<128 then
        DecompressedCount:=DecompressedCount+128
      else if DecompressedCount>High(DecompressedCount)-1024 then begin
        if ErrorMsg<>nil then
          ErrorMsg^:='inflate decompression failed, because not enough space';
        exit;
      end else
        DecompressedCount:=DecompressedCount*2;
      ReAllocMem(Decompressed,DecompressedCount);
      stream.next_out:=Decompressed+stream.total_out;
      stream.avail_out:=DecompressedCount-stream.total_out;
    end;
    err := inflate(stream, Z_NO_FLUSH);
    if err = Z_STREAM_END then
      break;
    if err<>Z_OK then begin
      if ErrorMsg<>nil then
        ErrorMsg^:='inflate finish failed';
      exit;
    end;
  end;

  //writeln('decompressed: total_in=',stream.total_in,' total_out=',stream.total_out);
  DecompressedCount:=stream.total_out;
  ReAllocMem(Decompressed,DecompressedCount);

  err := inflateEnd(stream);
  if err<>Z_OK then begin
    if ErrorMsg<>nil then
      ErrorMsg^:='inflateEnd failed';
    exit;
  end;
  Result:=true;
end;

function TifResolutionUnitToResolutionUnit(ATifResolutionUnit: DWord): TResolutionUnit;
begin
  Case ATifResolutionUnit of
  2: Result :=ruPixelsPerInch;
  3: Result :=ruPixelsPerCentimeter;
  else Result :=ruNone;
  end;
end;

function ResolutionUnitToTifResolutionUnit(AResolutionUnit: TResolutionUnit): DWord;
begin
  Case AResolutionUnit of
  ruPixelsPerInch: Result :=2;
  ruPixelsPerCentimeter: Result :=3;
  else Result :=1;
  end;
end;

{$ELSE}

procedure TBGRAReaderTiff.LoadImageFromStream(IFD: TTiffIFD);
var
  SampleCnt: SizeUInt;
  SampleBits: PWord;
  ChannelValues, LastChannelValues: array of word;
  All8Bit, All16Bit: boolean;

  procedure CheckBitCount;
  var
    Channel: LongWord;
  begin
    All8Bit := true;
    All16Bit := true;
    for Channel := 0 to SampleCnt-1 do
    begin
      if SampleBits[Channel] <> 8 then All8Bit:= false;
      if SampleBits[Channel] <> 16 then All16Bit:= false;
    end;
  end;

var
  PaletteCnt,PaletteStride: SizeUInt;
  PaletteValues: PWord;

  AlphaChannel: integer;
  PremultipliedAlpha: boolean;

  procedure InitColor;
  var Channel: LongWord;
  begin
    SetLength(ChannelValues, SampleCnt);
    SetLength(LastChannelValues, SampleCnt);
    for Channel := 0 to SampleCnt-1 do
      LastChannelValues[Channel] := 0;
  end;

  procedure ReadNext16BitData(var Run: Pointer);
  var Channel: PtrUInt;
  begin
    if ReverseEndian then
    begin
      if IFD.Predictor=2 then
      begin
        for Channel := 0 to SampleCnt-1 do
        begin
          {$PUSH}{$Q-}
          //Inc(ChannelValues[Channel],Swap(Word(Run^)));   original
          LastChannelValues[Channel] := LastChannelValues[Channel]+Swap(Word(Run^));
          ChannelValues[Channel] :=LastChannelValues[Channel];
          {$POP}
          inc(Run, 2);
        end;
      end else
      begin
        for Channel := 0 to SampleCnt-1 do
        begin
          ChannelValues[Channel] := Swap(Word(Run^));
          inc(Run, 2);
        end;
      end;
    end else
    begin
      if IFD.Predictor=2 then
      begin
        for Channel := 0 to SampleCnt-1 do
        begin
          {$PUSH}{$Q-}
          //Inc(ChannelValues[Channel],Word(Run^)); original
          LastChannelValues[Channel] := (LastChannelValues[Channel]+Word(Run^));
          ChannelValues[Channel] :=LastChannelValues[Channel];
          {$POP}
          inc(Run, 2);
        end;
      end else
      begin
        for Channel := 0 to SampleCnt-1 do
        begin
          ChannelValues[Channel] := Word(Run^);
          inc(Run, 2);
        end;
      end;
    end;
  end;

  procedure ReadNext8BitData(var Run: Pointer);
  var Channel: PtrUInt;
  begin
    if IFD.Predictor=2 then
    begin
      for Channel := 0 to SampleCnt-1 do
      begin
        {$PUSH}{$Q-}
        LastChannelValues[Channel] := (LastChannelValues[Channel]+Byte(Run^)) and $ff;
        ChannelValues[Channel] := LastChannelValues[Channel]+(LastChannelValues[Channel] shl 8);
        {$POP}
        inc(Run);
      end;
    end else
    begin
      for Channel := 0 to SampleCnt-1 do
      begin
        ChannelValues[Channel] := Byte(Run^)+(Byte(Run^) shl 8);
        inc(Run);
      end;
    end;
  end;

  procedure ReadNextPixelData(var Run: Pointer; var BitPos: byte);
  var Channel: LongWord;
  begin
    for Channel := 0 to SampleCnt-1 do
      ReadImgValue(SampleBits[Channel], Run,BitPos,IFD.FillOrder,
                   IFD.Predictor,LastChannelValues[Channel],
                   ChannelValues[Channel]);
  end;

  procedure GetPixelAsLab(out lab: TLabA);
  begin
    lab.L := 0;
    lab.a := 0;
    lab.b := 0;
    lab.alpha := 1;

    case IFD.PhotoMetricInterpretation of
    8: begin
         case IFD.GrayBits of
           8,16: lab.L := ChannelValues[0]*(100/65535);
           0:begin
               lab.L := ChannelValues[0]*(100/65535);
               case IFD.RedBits of
                 16: lab.a := SmallInt(ChannelValues[1])/256;
                 8: lab.a := ShortInt(ChannelValues[1] shr 8);
               end;
               case IFD.BlueBits of
                 16: lab.b := SmallInt(ChannelValues[2])/256;
                 8: lab.b := ShortInt(ChannelValues[2] shr 8);
               end;
             end;
         end;
       end;
    9: begin
         case IFD.GrayBits of
           16: lab.L := ChannelValues[0]*(100/65280);
           8: lab.L := ChannelValues[0]*(100/65535);
           0:begin
               case IFD.GreenBits of
                 16: lab.L := ChannelValues[0]*(100/65280);
                 8: lab.L := ChannelValues[0]*(100/65535);
               end;
               case IFD.RedBits of
                 16: lab.a := (ChannelValues[1]-32768)/256;
                 8: lab.a := (ChannelValues[1] shr 8)-128;
               end;
               case IFD.BlueBits of
                 16: lab.b := (ChannelValues[2]-32768)/256;
                 8: lab.b := (ChannelValues[2] shr 8)-128;
               end;
             end;
         end;
       end;
     //10: ITULAB: ITU L*a*b*
     //32844: LOGL: CIE Log2(L)
     //32845: LOGLUV: CIE Log2(L) (u',v')
    else
      TiffError('PhotometricInterpretation='+IntToStr(IFD.PhotoMetricInterpretation)+' not supported');
    end;

    if AlphaChannel >= 0 then
      lab.alpha:= ChannelValues[AlphaChannel]/65535;
  end;

var
  FPColorValue: TFPColor;

  procedure GetPixelAsFPColor;
  var PaletteIndex: LongWord;
    GrayValue: Word;
    lab: TLabA;
    cmyk: TStdCMYK;
    ycbcr: TYCbCr;
  begin
    if IFD.PhotoMetricInterpretation >= 8 then
    begin
      GetPixelAsLab(lab);
      //FPColorValue.FromLabA(lab);
      FPColorValue :=lab.ToExpandedPixel.ToFPColor;
      exit;
    end;

    case IFD.PhotoMetricInterpretation of
    0,1: // 0:bilevel grayscale 0 is white; 1:0 is black
      begin
        GrayValue := ChannelValues[0];
        if IFD.PhotoMetricInterpretation=0 then
          GrayValue:=$ffff-GrayValue;
        FPColorValue.red  := GrayValue;
        FPColorValue.green:= GrayValue;
        FPColorValue.blue := GrayValue;
        FPColorValue.alpha := alphaOpaque;
      end;

    2: // RGB(A)
      begin
        FPColorValue.red  := ChannelValues[0];
        FPColorValue.green:= ChannelValues[1];
        FPColorValue.blue := ChannelValues[2];
        FPColorValue.alpha := alphaOpaque;
      end;

    3: //3 Palette/color map indexed
      begin
        PaletteIndex := ChannelValues[0] shr (16 - SampleBits[0]);
        FPColorValue.red  := PaletteValues[PaletteIndex];
        FPColorValue.green:= PaletteValues[PaletteIndex+PaletteStride];
        FPColorValue.blue := PaletteValues[PaletteIndex+2*PaletteStride];
        FPColorValue.alpha := alphaOpaque;
      end;

    //4 Mask/holdout mask (obsolete by TIFF 6.0 specification)

    5: // CMYK plus optional alpha
      begin
        cmyk :=TStdCMYK.New(ChannelValues[0]/$ffff, ChannelValues[1]/$ffff, ChannelValues[2]/$ffff, ChannelValues[3]/$ffff);
        FPColorValue :=cmyk.ToExpandedPixel.ToFPColor(true); //MaxM: in Future we can use GammaCompression
      end;

    6: // YCBCR: CCIR 601
      begin
        ycbcr :=TYCbCr.New(ChannelValues[0]/$ffff, ChannelValues[1]/$ffff, ChannelValues[2]/$ffff);

        if IFD.YCbCr_LumaRed<>0
        then FPColorValue :=ycbcr.ToLinearRGBA(IFD.YCbCr_LumaRed, IFD.YCbCr_LumaGreen, IFD.YCbCr_LumaBlue).ToExpandedPixel.ToFPColor(false)
        else FPColorValue :=ycbcr.ToLinearRGBA(YCbCr_601).ToExpandedPixel.ToFPColor(false);
      end;

    else
      TiffError('PhotometricInterpretation='+IntToStr(IFD.PhotoMetricInterpretation)+' not supported');
    end;

    if AlphaChannel >= 0 then
    begin
      FPColorValue.alpha:= ChannelValues[AlphaChannel];
      if PremultipliedAlpha and (FPColorValue.alpha <> alphaOpaque) and (FPColorValue.alpha <> 0) then
      begin
        FPColorValue.red := (FPColorValue.red * alphaOpaque + FPColorValue.alpha div 2) div FPColorValue.alpha;
        FPColorValue.green := (FPColorValue.green * alphaOpaque + FPColorValue.alpha div 2) div FPColorValue.alpha;
        FPColorValue.blue := (FPColorValue.blue * alphaOpaque + FPColorValue.alpha div 2) div FPColorValue.alpha;
      end;
    end;
  end;

var
  ChunkOffsets: Pointer;
  ChunkByteCounts: PDWord;
  Chunk: PByte;
  ChunkCount: DWord;
  ChunkIndex: DWord;
  CurCount: SizeUInt;
  CurOffset: SizeUInt;
  CurByteCnt: PtrInt;
  Run: PByte;
  BitPos: Byte;
  x, y, cx, cy, dx1,dy1, dx2,dy2, sx, sy: integer;
  SampleBitsPerPixel: DWord;
  CurFPImg: TFPCustomImage;
  aContinue, ConvertFromLab: Boolean;
  ExpectedChunkLength: PtrInt;
  ChunkType: TTiffChunkType;
  TilesAcross, TilesDown: DWord;
  ChunkLeft, ChunkTop, ChunkWidth, ChunkHeight: DWord;
  ChunkBytesPerLine: DWord;

  LabArray: array of TLabA;
  ConversionFromLab: TBridgedConversion;
  DestStride: PtrInt;
  PDest: PByte;
  CurPixelValue: TBGRAPixel;

  procedure ComputeDestStride;
  begin
    DestStride := dy1*TCustomUniversalBitmap(CurFPImg).RowSize;
    if TCustomUniversalBitmap(CurFPImg).LineOrder = riloBottomToTop then
      DestStride := -DestStride;
    inc(DestStride, dx1*PtrInt(TCustomUniversalBitmap(CurFPImg).Colorspace.GetSize));
  end;

  procedure ReadResolutionValues;
  begin
    {$IF FPC_FULLVERSION<30301}
    if (CurFPImg is TCustomUniversalBitmap) then
    with TCustomUniversalBitmap(CurFPImg) do
    {$ELSE}
    with CurFPImg do
    {$ENDIF}
    begin
        ResolutionUnit :=TifResolutionUnitToResolutionUnit(IFD.ResolutionUnit);
        if (IFD.XResolution.Denominator>0)
        then ResolutionX :=IFD.XResolution.Numerator/IFD.XResolution.Denominator
        else ResolutionX :=IFD.XResolution.Numerator;
        if (IFD.YResolution.Denominator>0)
        then ResolutionY :=IFD.YResolution.Numerator/IFD.YResolution.Denominator
        else ResolutionY :=IFD.YResolution.Numerator;
     end;
  end;

begin
  if (IFD.ImageWidth=0) or (IFD.ImageHeight=0) then
    exit;

  if IFD.PhotoMetricInterpretation=High(IFD.PhotoMetricInterpretation) then
    TiffError('missing PhotometricInterpretation');
  if IFD.BitsPerSample=0 then
    TiffError('missing BitsPerSample');
  if IFD.TileWidth>0 then begin
    ChunkType:=tctTile;
    if IFD.TileLength=0 then
      TiffError('missing TileLength');
    if IFD.TileOffsets=0 then
      TiffError('missing TileOffsets');
    if IFD.TileByteCounts=0 then
      TiffError('missing TileByteCounts');
  end else begin
    ChunkType:=tctStrip;
    if IFD.RowsPerStrip=0 then
      TiffError('missing RowsPerStrip');
    if IFD.StripOffsets=0 then
      TiffError('missing StripOffsets');
    if IFD.StripByteCounts=0 then
      TiffError('missing StripByteCounts');
  end;

  if IFD.PlanarConfiguration > 1 then
     TiffError('Planar configuration not handled');

  {$ifdef FPC_Debug_Image}
  if Debug then
    writeln('TBGRAReaderTiff.LoadImageFromStream reading ...');
  {$endif}

  ChunkOffsets:=nil;
  ChunkByteCounts:=nil;
  Chunk:=nil;
  SampleBits:=nil;
  try
    // read chunk starts and sizes
    if ChunkType=tctTile then begin
      TilesAcross:=(IFD.ImageWidth+IFD.TileWidth-1) div IFD.TileWidth;
      TilesDown:=(IFD.ImageHeight+IFD.TileLength-1) div IFD.TileLength;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TBGRAReaderTiff.LoadImageFromStream TilesAcross=',TilesAcross,' TilesDown=',TilesDown);
      {$endif}
      ChunkCount := TilesAcross * TilesDown;
      ReadShortOrLongValues(IFD.TileOffsets,ChunkOffsets,CurCount);
      if CurCount<ChunkCount then
        TiffError('number of TileCounts is wrong');
      ReadShortOrLongValues(IFD.TileByteCounts,ChunkByteCounts,CurCount);
      if CurCount<ChunkCount then
        TiffError('number of TileByteCounts is wrong');
    end else begin //strip
      ChunkCount:=((IFD.ImageHeight-1) div IFD.RowsPerStrip)+1;
      ReadShortOrLongValues(IFD.StripOffsets,ChunkOffsets,CurCount);
      if CurCount<ChunkCount then
        TiffError('number of StripOffsets is wrong');
      ReadShortOrLongValues(IFD.StripByteCounts,ChunkByteCounts,CurCount);
      if CurCount<ChunkCount then
        TiffError('number of StripByteCounts is wrong');
    end;

    // read image sample structure
    ReadImageSampleProperties(IFD, AlphaChannel, PremultipliedAlpha,
      SampleCnt, SampleBits, SampleBitsPerPixel,
      PaletteCnt, PaletteValues);

    PaletteStride := PaletteCnt div 3;
    CheckBitCount;

    // create FPimage
    DoCreateImage(IFD);
    if IFD.Img=nil then
    begin
      IFD.Img := BGRABitmapFactory.Create;
      IFD.FreeImg := true;
    end;
    CurFPImg:=IFD.Img;
    if CurFPImg=nil then exit;

    //Resolution
    ReadResolutionValues;

    SetFPImgExtras(CurFPImg, IFD);

    case IFD.Orientation of
    0,1..4: CurFPImg.SetSize(IFD.ImageWidth,IFD.ImageHeight);
    5..8: CurFPImg.SetSize(IFD.ImageHeight,IFD.ImageWidth);
    end;

    {$ifdef FPC_Debug_Image}
    if Debug then
      writeln('TBGRAReaderTiff.LoadImageFromStream SampleBitsPerPixel=',SampleBitsPerPixel);
    {$endif}

    LabArray := nil;
    if (IFD.PhotoMetricInterpretation >= 8) and
       (CurFPImg is TCustomUniversalBitmap) then
    begin
      ConvertFromLab := true;
      ConversionFromLab := TLabAColorspace.GetBridgedConversion(TCustomUniversalBitmap(CurFPImg).Colorspace)
    end else
      ConvertFromLab := false;

    // read chunks
    for ChunkIndex:=0 to ChunkCount-1 do begin
      if BigTiff
      then CurOffset:=PSizeUInt(ChunkOffsets)[ChunkIndex]
      else CurOffset:=PDWord(ChunkOffsets)[ChunkIndex];

      CurByteCnt:=ChunkByteCounts[ChunkIndex];

      //writeln('TBGRAReaderTiff.LoadImageFromStream CurOffset=',CurOffset,' CurByteCnt=',CurByteCnt);
      if CurByteCnt<=0 then continue;
      ReAllocMem(Chunk,CurByteCnt);
      SetStreamPos(CurOffset);
      ReadBuffer(Chunk^,CurByteCnt);

      // decompress
      if ChunkType=tctTile then
        ExpectedChunkLength:=(SampleBitsPerPixel*IFD.TileWidth+7) div 8*IFD.TileLength
      else
        ExpectedChunkLength:=((SampleBitsPerPixel*IFD.ImageWidth+7) div 8)*IFD.RowsPerStrip;
      case IFD.Compression of
      TiffCompressionNone: ;
      TiffCompressionPackBits: DecodePackBits(Chunk,CurByteCnt);
      TiffCompressionLZW: DecodeLZW(Chunk,CurByteCnt);
      TiffCompressionDeflateAdobe,
      TiffCompressionDeflateZLib: DecodeDeflate(Chunk,CurByteCnt,ExpectedChunkLength);
      else
        TiffError('compression '+TiffCompressionName(IFD.Compression)+' not supported yet');
      end;
      if CurByteCnt<=0 then continue;

      // compute current chunk area
      if ChunkType=tctTile then begin
        ChunkLeft:=(ChunkIndex mod TilesAcross)*IFD.TileWidth;
        ChunkTop:=(ChunkIndex div TilesAcross)*IFD.TileLength;
        ChunkWidth:=Min(IFD.TileWidth,IFD.ImageWidth-ChunkLeft);
        ChunkHeight:=Min(IFD.TileLength,IFD.ImageHeight-ChunkTop);
        ChunkBytesPerLine:=(SampleBitsPerPixel*ChunkWidth+7) div 8;
        ExpectedChunkLength:=ChunkBytesPerLine*ChunkHeight;
        if CurByteCnt<ExpectedChunkLength then begin
          //writeln('TBGRAReaderTiff.LoadImageFromStream SampleBitsPerPixel=',SampleBitsPerPixel,' IFD.ImageWidth=',IFD.ImageWidth,' IFD.ImageHeight=',IFD.ImageHeight,' y=',y,' IFD.TileWidth=',IFD.TileWidth,' IFD.TileLength=',IFD.TileLength,' ExpectedChunkLength=',ExpectedChunkLength,' CurByteCnt=',CurByteCnt);
          TiffError('TBGRAReaderTiff.LoadImageFromStream Tile too short ByteCnt='+IntToStr(CurByteCnt)+' ChunkWidth='+IntToStr(ChunkWidth)+' ChunkHeight='+IntToStr(ChunkHeight)+' expected='+IntToStr(ExpectedChunkLength));
        end else if CurByteCnt>ExpectedChunkLength then begin
          // boundary tiles have padding
          ChunkBytesPerLine:=(SampleBitsPerPixel*IFD.TileWidth+7) div 8;
        end;
      end else begin //tctStrip
        ChunkLeft:=0;
        ChunkTop:=IFD.RowsPerStrip*ChunkIndex;
        ChunkWidth:=IFD.ImageWidth;
        ChunkHeight:=Min(IFD.RowsPerStrip,IFD.ImageHeight-ChunkTop);
        ChunkBytesPerLine:=(SampleBitsPerPixel*ChunkWidth+7) div 8;
        ExpectedChunkLength:=ChunkBytesPerLine*ChunkHeight;
        //writeln('TBGRAReaderTiff.LoadImageFromStream SampleBitsPerPixel=',SampleBitsPerPixel,' IFD.ImageWidth=',IFD.ImageWidth,' IFD.ImageHeight=',IFD.ImageHeight,' y=',y,' IFD.RowsPerStrip=',IFD.RowsPerStrip,' ExpectedChunkLength=',ExpectedChunkLength,' CurByteCnt=',CurByteCnt);
        if CurByteCnt<ExpectedChunkLength then
          TiffError('TBGRAReaderTiff.LoadImageFromStream Strip too short ByteCnt='+IntToStr(CurByteCnt)+' ChunkWidth='+IntToStr(ChunkWidth)+' ChunkHeight='+IntToStr(ChunkHeight)+' expected='+IntToStr(ExpectedChunkLength));
      end;

      // progress
      aContinue:=true;
      Progress(psRunning, 0, false, Rect(0,0,IFD.ImageWidth,ChunkTop), '', aContinue);
      if not aContinue then break;

      // Orientation
      if IFD.Orientation in [1..4] then begin
        sx:=ChunkLeft; sy:=ChunkTop;
        dy1 := 0; dx2 := 0;
        case IFD.Orientation of
        1: begin dx1:=1; dy2:=1; end;// 0,0 is left, top
        2: begin sx:=IFD.ImageWidth-sx-1; dx1:=-1; dy2:=1; end;// 0,0 is right, top
        3: begin sx:=IFD.ImageWidth-sx-1; dx1:=-1; sy:=IFD.ImageHeight-sy-1; dy2:=-1; end;// 0,0 is right, bottom
        4: begin dx1:=1; sy:=IFD.ImageHeight-sy-1; dy2:=-1; end;// 0,0 is left, bottom
        end;
      end else begin
        // rotated
        sx:=ChunkTop; sy:=ChunkLeft;
        dx1 := 0; dy2 := 0;
        case IFD.Orientation of
        5: begin dy1:=1; dx2:=1; end;// 0,0 is top, left (rotated)
        6: begin dy1:=1; sx:=IFD.ImageWidth-sx-1; dx2:=-1; end;// 0,0 is top, right (rotated)
        7: begin sy:=IFD.ImageHeight-sy-1; dy1:=-1; sx:=IFD.ImageHeight-sx-1; dx2:=-1; end;// 0,0 is bottom, right (rotated)
        8: begin sy:=IFD.ImageHeight-sy-1; dy1:=-1; dx2:=1; end;// 0,0 is bottom, left (rotated)
        end;
      end;

      //writeln('TBGRAReaderTiff.LoadImageFromStream Chunk ',ChunkIndex,' ChunkLeft=',ChunkLeft,' ChunkTop=',ChunkTop,' IFD.ImageWidth=',IFD.ImageWidth,' IFD.ImageHeight=',IFD.ImageHeight,' ChunkWidth=',ChunkWidth,' ChunkHeight=',ChunkHeight,' PaddingRight=',PaddingRight);
      for cy:=0 to ChunkHeight-1 do begin
        Run:=Chunk+ChunkBytesPerLine*cy;
        BitPos := 0;
        InitColor;
        //writeln('TBGRAReaderTiff.LoadImageFromStream (x,y)=(',sx,',',sy,')');

        if ConvertFromLab then
        begin
          if length(LabArray)<ChunkWidth then setlength(LabArray, ChunkWidth);

          if All16Bit then
          begin
            for cx:=0 to ChunkWidth-1 do begin
              ReadNext16BitData(Run);
              GetPixelAsLab(LabArray[cx]);
            end;
          end else
          if All8Bit then
          begin
            for cx:=0 to ChunkWidth-1 do begin
              ReadNext8BitData(Run);
              GetPixelAsLab(LabArray[cx]);
            end;
          end else
          begin
            for cx:=0 to ChunkWidth-1 do begin
              ReadNextPixelData(Run,BitPos);
              GetPixelAsLab(LabArray[cx]);
            end;
          end;

          ComputeDestStride;
          ConversionFromLab.Convert(@LabArray[0], TCustomUniversalBitmap(CurFPImg).GetPixelAddress(sx,sy),
                                    ChunkWidth, sizeof(TLabA), DestStride, nil);
        end else
        begin
          x:= sx;
          y:= sy;
          if All16Bit then
          begin
            if (CurFPImg is TBGRACustomBitmap) and (IFD.Predictor <> 2) then
            begin
              ComputeDestStride;
              PDest := TBGRACustomBitmap(CurFPImg).GetPixelAddress(sx,sy);
              if (IFD.PhotoMetricInterpretation = 0) and (SampleCnt = 1) then
              begin
                for cx:=0 to ChunkWidth-1 do begin
                  PBGRAPixel(PDest)^.red:= FastRoundDiv257(not (PWord(Run)^));
                  PBGRAPixel(PDest)^.green:= PBGRAPixel(PDest)^.red;
                  PBGRAPixel(PDest)^.blue:= PBGRAPixel(PDest)^.red;
                  PBGRAPixel(PDest)^.alpha:= 255;
                  inc(Run, 2);
                  inc(PDest, DestStride);
                end;
              end else
              if (IFD.PhotoMetricInterpretation = 1) and (SampleCnt = 1) then
              begin
                for cx:=0 to ChunkWidth-1 do begin
                  PBGRAPixel(PDest)^.red:= FastRoundDiv257(PWord(Run)^);
                  PBGRAPixel(PDest)^.green:= PBGRAPixel(PDest)^.red;
                  PBGRAPixel(PDest)^.blue:= PBGRAPixel(PDest)^.red;
                  PBGRAPixel(PDest)^.alpha:= 255;
                  inc(Run, 2);
                  inc(PDest, DestStride);
                end;
              end else
              if (IFD.PhotoMetricInterpretation = 2) and (SampleCnt = 3) then
              begin
                for cx:=0 to ChunkWidth-1 do begin
                  PBGRAPixel(PDest)^.red:= FastRoundDiv257(PWord(Run)^);
                  PBGRAPixel(PDest)^.green:= FastRoundDiv257(PWord(Run+2)^);
                  PBGRAPixel(PDest)^.blue:= FastRoundDiv257(PWord(Run+4)^);
                  PBGRAPixel(PDest)^.alpha:= 255;
                  inc(Run, 6);
                  inc(PDest, DestStride);
                end;
              end else
              for cx:=0 to ChunkWidth-1 do begin
                 ReadNext16BitData(Run);
                 GetPixelAsFPColor;
                 PBGRAPixel(PDest)^.red:= FastRoundDiv257(FPColorValue.red);
                 PBGRAPixel(PDest)^.green:= FastRoundDiv257(FPColorValue.green);
                 PBGRAPixel(PDest)^.blue:= FastRoundDiv257(FPColorValue.blue);
                 PBGRAPixel(PDest)^.alpha:= FastRoundDiv257(FPColorValue.alpha);
                 inc(PDest, DestStride);
               end;
            end else
              for cx:=0 to ChunkWidth-1 do begin
                ReadNext16BitData(Run);
                GetPixelAsFPColor;
                CurFPImg.Colors[x,y]:= FPColorValue;
                // next column
                inc(x,dx1);
                inc(y,dy1);
              end;
          end else
          if All8Bit then
          begin
            if CurFPImg is TBGRACustomBitmap then
            begin
              ComputeDestStride;
              PDest := TBGRACustomBitmap(CurFPImg).GetPixelAddress(sx,sy);
              if (IFD.PhotoMetricInterpretation = 0) and (SampleCnt = 1) then
              begin
                if IFD.Predictor = 2 then
                begin
                  CurPixelValue := BGRAPixelTransparent;
                  for cx:=0 to ChunkWidth-1 do begin
                    {$PUSH}{$R-}inc(CurPixelValue.green, run^);{$POP}
                    PBGRAPixel(PDest)^.red:= not CurPixelValue.green;
                    PBGRAPixel(PDest)^.green:= not CurPixelValue.green;
                    PBGRAPixel(PDest)^.blue:= not CurPixelValue.green;
                    PBGRAPixel(PDest)^.alpha:= 255;
                    inc(Run);
                    inc(PDest, DestStride);
                  end;
                end else
                for cx:=0 to ChunkWidth-1 do begin
                  PBGRAPixel(PDest)^.red:= not (Run^);
                  PBGRAPixel(PDest)^.green:= not (Run^);
                  PBGRAPixel(PDest)^.blue:= not (Run^);
                  PBGRAPixel(PDest)^.alpha:= 255;
                  inc(Run);
                  inc(PDest, DestStride);
                end;
              end else
              if (IFD.PhotoMetricInterpretation = 1) and (SampleCnt = 1) then
              begin
                if IFD.Predictor = 2 then
                begin
                  CurPixelValue := BGRAPixelTransparent;
                  for cx:=0 to ChunkWidth-1 do begin
                    {$PUSH}{$R-}inc(CurPixelValue.green, run^);{$POP}
                    PBGRAPixel(PDest)^.red:= CurPixelValue.green;
                    PBGRAPixel(PDest)^.green:= CurPixelValue.green;
                    PBGRAPixel(PDest)^.blue:= CurPixelValue.green;
                    PBGRAPixel(PDest)^.alpha:= 255;
                    inc(Run);
                    inc(PDest, DestStride);
                  end;
                end else
                for cx:=0 to ChunkWidth-1 do begin
                  PBGRAPixel(PDest)^.red:= Run^;
                  PBGRAPixel(PDest)^.green:= Run^;
                  PBGRAPixel(PDest)^.blue:= Run^;
                  PBGRAPixel(PDest)^.alpha:= 255;
                  inc(Run);
                  inc(PDest, DestStride);
                end;
              end else
              if (IFD.PhotoMetricInterpretation = 2) and (SampleCnt = 3) then
              begin
                if IFD.Predictor = 2 then
                begin
                  CurPixelValue := BGRAPixelTransparent;
                  for cx:=0 to ChunkWidth-1 do begin
                    {$PUSH}{$R-}inc(CurPixelValue.red, run^);{$POP}
                    {$PUSH}{$R-}inc(CurPixelValue.green, (run+1)^);{$POP}
                    {$PUSH}{$R-}inc(CurPixelValue.blue, (run+2)^);{$POP}
                    PBGRAPixel(PDest)^.red:= CurPixelValue.red;
                    PBGRAPixel(PDest)^.green:= CurPixelValue.green;
                    PBGRAPixel(PDest)^.blue:= CurPixelValue.blue;
                    PBGRAPixel(PDest)^.alpha:= 255;
                    inc(Run,3);
                    inc(PDest, DestStride);
                  end;
                end else
                for cx:=0 to ChunkWidth-1 do begin
                  PBGRAPixel(PDest)^.red:= Run^;
                  PBGRAPixel(PDest)^.green:= (Run+1)^;
                  PBGRAPixel(PDest)^.blue:= (Run+2)^;
                  PBGRAPixel(PDest)^.alpha:= 255;
                  inc(Run,3);
                  inc(PDest, DestStride);
                end;
              end else
              for cx:=0 to ChunkWidth-1 do begin
                 ReadNext8BitData(Run);
                 GetPixelAsFPColor;
                 PBGRAPixel(PDest)^.red:= FPColorValue.red shr 8;
                 PBGRAPixel(PDest)^.green:= FPColorValue.green shr 8;
                 PBGRAPixel(PDest)^.blue:= FPColorValue.blue shr 8;
                 PBGRAPixel(PDest)^.alpha:= FPColorValue.alpha shr 8;
                 inc(PDest, DestStride);
               end;
            end else
              for cx:=0 to ChunkWidth-1 do begin
                ReadNext8BitData(Run);
                GetPixelAsFPColor;
                CurFPImg.Colors[x,y]:= FPColorValue;
                // next column
                inc(x,dx1);
                inc(y,dy1);
              end;
          end else
          begin
            for cx:=0 to ChunkWidth-1 do begin
              ReadNextPixelData(Run,BitPos);
              GetPixelAsFPColor;
              CurFPImg.Colors[x,y]:= FPColorValue;
              // next column
              inc(x,dx1);
              inc(y,dy1);
            end;
          end;
        end;

        // next line
        inc(sx,dx2);
        inc(sy,dy2);
      end;
      // next chunk
    end;
  finally
    ReAllocMem(SampleBits,0);
    ReAllocMem(ChunkOffsets,0);
    ReAllocMem(ChunkByteCounts,0);
    ReAllocMem(Chunk,0);
    ReAllocMem(PaletteValues,0);
  end;
end;

{$ENDIF}

initialization
  DefaultBGRAImageReader[ifTiff] := TBGRAReaderTiff;

end.

