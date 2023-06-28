// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
    The original file is part of the Free Pascal run time library.
    Copyright (c) 2012 by the Free Pascal development team

    Tiff writer for fpImage modified by circular.

 **********************************************************************

 Working:
   Grayscale 8,16bit (optional alpha),
   RGB 8,16bit (optional alpha),
   Orientation,
   multiple images, pages
   thumbnail
   Compression: deflate

 ToDo:
   Compression: LZW, packbits, jpeg, ...
   Planar
   ColorMap
   separate mask
   fillorder - not needed by baseline tiff reader
   bigtiff 64bit offsets
   endian - currently using system endianess
   orientation with rotation
}
{*****************************************************************************}
{
  2023-06  - Massimo Magnano
           - added Resolution support
}
{*****************************************************************************}
unit BGRAWriteTiff;

{$mode objfpc}{$H+}

interface

uses
  Math, SysUtils, BGRAClasses, BGRABitmapTypes, BGRAReadTiff, zbase, zdeflate,
  FPimage, FPTiffCmn;

type

  { TTiffWriterEntry }

  TTiffWriterEntry = class
  public
    Tag: Word;
    EntryType: Word;
    Count: LongWord;
    Data: Pointer;
    DataPos: LongWord;
    Bytes: LongWord;
    destructor Destroy; override;
  end;

  TTiffWriterChunk = record
    Data: Pointer;
    Bytes: LongWord;
  end;
  PTiffWriterChunk = ^TTiffWriterChunk;

  { TTiffWriterChunkOffsets }

  TTiffWriterChunkOffsets = class(TTiffWriterEntry)
  public
    Chunks: PTiffWriterChunk;
    ChunkByteCounts: TTiffWriterEntry;
    constructor Create(ChunkType: TTiffChunkType);
    destructor Destroy; override;
    procedure SetCount(NewCount: LongWord);
  end;

  { TBGRAWriterTiff }

  TBGRAWriterTiff = class(TFPCustomImageWriter)
  private
    FPremultiplyRGB: boolean;
    FSaveCMYKAsRGB: boolean;
    fStartPos: Int64;
    FEntries: TFPList; // list of TFPList of TTiffWriterEntry
    fStream: TStream;
    fPosition: LongWord;
    procedure ClearEntries;
    procedure WriteTiff;
    procedure WriteHeader;
    procedure WriteIFDs;
    procedure WriteEntry(Entry: TTiffWriterEntry);
    procedure WriteData;
    procedure WriteEntryData(Entry: TTiffWriterEntry);
    procedure WriteBuf(var Buf; Count: LongWord);
    procedure WriteWord(w: Word);
    procedure WriteDWord(d: LongWord);
  protected
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
    procedure AddEntryString(Tag: word; const s: string);
    procedure AddEntryShort(Tag: word; Value: Word);
    procedure AddEntryLong(Tag: word; Value: LongWord);
    procedure AddEntryShortOrLong(Tag: word; Value: LongWord);
    procedure AddEntryRational(Tag: word; const Value: TTiffRational);
    procedure AddEntry(Tag: Word; EntryType: Word; EntryCount: LongWord;
                       Data: Pointer; Bytes: LongWord;
                       CopyData: boolean = true);
    procedure AddEntry(Entry: TTiffWriterEntry);
    procedure TiffError(Msg: string);
    procedure EncodeDeflate(var Buffer: Pointer; var Count: LongWord);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    procedure AddImage(Img: TFPCustomImage);
    procedure SaveToStream(Stream: TStream);
    property SaveCMYKAsRGB: boolean read FSaveCMYKAsRGB write FSaveCMYKAsRGB;
    property PremultiplyRGB: boolean read FPremultiplyRGB write FPremultiplyRGB;
  end;

function CompareTiffWriteEntries(Entry1, Entry2: Pointer): integer;

function CompressDeflate(InputData: PByte; InputCount: LongWord;
  out Compressed: PByte; var CompressedCount: LongWord;
  ErrorMsg: PAnsiString = nil): boolean;

implementation

function CompareTiffWriteEntries(Entry1, Entry2: Pointer): integer;
begin
  Result:=integer(TTiffWriterEntry(Entry1).Tag)-integer(TTiffWriterEntry(Entry2).Tag);
end;

function CompressDeflate(InputData: PByte; InputCount: LongWord; out
  Compressed: PByte; var CompressedCount: LongWord; ErrorMsg: PAnsiString
  ): boolean;
var
  stream : z_stream;
  err : integer;
begin
  Result:=false;
  //writeln('CompressDeflate START');
  Compressed:=nil;
  if InputCount=0 then begin
    CompressedCount:=0;
    exit(true);
  end;

  err := deflateInit(stream{%H-}, Z_DEFAULT_COMPRESSION);
  if err <> Z_OK then begin
    if ErrorMsg<>nil then
      ErrorMsg^:='deflateInit failed';
    exit;
  end;

  // set input = InputData data
  stream.avail_in := InputCount;
  stream.next_in  := InputData;

  // set output = compressed data
  if CompressedCount=0 then
    CompressedCount:=InputCount;
  GetMem(Compressed,CompressedCount);
  stream.avail_out := CompressedCount;
  stream.next_out := Compressed;

  err := deflate(stream, Z_NO_FLUSH);
  if err<>Z_OK then begin
    if ErrorMsg<>nil then
      ErrorMsg^:='deflate failed';
    exit;
  end;

  while TRUE do begin
    //writeln('run: total_in=',stream.total_in,' avail_in=',stream.avail_in,' total_out=',stream.total_out,' avail_out=',stream.avail_out);
    if (stream.avail_out=0) then begin
      // need more space
      if CompressedCount<128 then
        CompressedCount:=CompressedCount+128
      else if CompressedCount>High(CompressedCount)-1024 then begin
        if ErrorMsg<>nil then
          ErrorMsg^:='deflate compression failed, because not enough space';
        exit;
      end else
        CompressedCount:=CompressedCount+1024;
      ReAllocMem(Compressed,CompressedCount);
      stream.next_out:=Compressed+stream.total_out;
      stream.avail_out:=CompressedCount-stream.total_out;
    end;
    err := deflate(stream, Z_FINISH);
    if err = Z_STREAM_END then
      break;
    if err<>Z_OK then begin
      if ErrorMsg<>nil then
        ErrorMsg^:='deflate finish failed';
      exit;
    end;
  end;

  //writeln('compressed: total_in=',stream.total_in,' total_out=',stream.total_out);
  CompressedCount:=stream.total_out;
  ReAllocMem(Compressed,CompressedCount);

  err := deflateEnd(stream);
  if err<>Z_OK then begin
    if ErrorMsg<>nil then
      ErrorMsg^:='deflateEnd failed';
    exit;
  end;
  Result:=true;
end;

{ TBGRAWriterTiff }

procedure TBGRAWriterTiff.WriteWord(w: Word);
begin
  if fStream<>nil then
    fStream.WriteWord(w);
  inc(fPosition,2);
end;

procedure TBGRAWriterTiff.WriteDWord(d: LongWord);
begin
  if fStream<>nil then
    fStream.WriteDWord(d);
  inc(fPosition,4);
end;

procedure TBGRAWriterTiff.ClearEntries;
var
  i: Integer;
  List: TFPList;
  j: Integer;
begin
  for i:=FEntries.Count-1 downto 0 do begin
    List:=TFPList(FEntries[i]);
    for j:=List.Count-1 downto 0 do
      TObject(List[j]).Free;
    List.Free;
  end;
  FEntries.Clear;
end;

procedure TBGRAWriterTiff.WriteTiff;
begin
  {$IFDEF FPC_Debug_Image}
  writeln('TBGRAWriterTiff.WriteTiff fStream=',fStream<>nil);
  {$ENDIF}
  fPosition:=0;
  WriteHeader;
  WriteIFDs;
  WriteData;
end;

procedure TBGRAWriterTiff.WriteHeader;
var
  EndianMark: String;
begin
  EndianMark:={$IFDEF FPC_BIG_ENDIAN}'MM'{$ELSE}'II'{$ENDIF};
  WriteBuf(EndianMark[1],2);
  WriteWord(42);
  WriteDWord(8);
end;

procedure TBGRAWriterTiff.WriteIFDs;
var
  i: Integer;
  List: TFPList;
  j: Integer;
  Entry: TTiffWriterEntry;
  NextIFDPos: LongWord;
begin
  for i:=0 to FEntries.Count-1 do begin
    List:=TFPList(FEntries[i]);
    // write count
    {$IFDEF FPC_Debug_Image}
    writeln('TBGRAWriterTiff.WriteIFDs List=',i,' Count=',List.Count);
    {$ENDIF}
    WriteWord(List.Count);
    // write array of entries
    for j:=0 to List.Count-1 do begin
      Entry:=TTiffWriterEntry(List[j]);
      WriteEntry(Entry);
    end;
    // write position of next IFD
    if i<FEntries.Count-1 then
      NextIFDPos:=fPosition+4
    else
      NextIFDPos:=0;
    WriteDWord(NextIFDPos);
  end;
end;

procedure TBGRAWriterTiff.WriteEntry(Entry: TTiffWriterEntry);
var
  PadBytes: LongWord;
begin
  {$IFDEF FPC_Debug_Image}
  //writeln('TBGRAWriterTiff.WriteEntry Tag=',Entry.Tag,' Type=',Entry.EntryType,' Count=',Entry.Count,' Bytes=',Entry.Bytes);
  {$ENDIF}
  WriteWord(Entry.Tag);
  WriteWord(Entry.EntryType);
  WriteDWord(Entry.Count);
  if Entry.Bytes<=4 then begin
    if Entry.Bytes>0 then
      WriteBuf(Entry.Data^,Entry.Bytes);
    PadBytes:=0;
    WriteBuf(PadBytes,4-Entry.Bytes);
  end else begin
    WriteDWord(Entry.DataPos);
  end;
end;

procedure TBGRAWriterTiff.WriteData;
var
  i: Integer;
  List: TFPList;
  j: Integer;
  Entry: TTiffWriterEntry;
  Chunks: TTiffWriterChunkOffsets;
  k: Integer;
  Bytes: LongWord;
begin
  for i:=0 to FEntries.Count-1 do begin
    List:=TFPList(FEntries[i]);
    // write entry data
    for j:=0 to List.Count-1 do begin
      Entry:=TTiffWriterEntry(List[j]);
      WriteEntryData(Entry);
    end;
    // write Chunks
    for j:=0 to List.Count-1 do begin
      Entry:=TTiffWriterEntry(List[j]);
      if Entry is TTiffWriterChunkOffsets then begin
        Chunks:=TTiffWriterChunkOffsets(Entry);
        // write Chunks
        for k:=0 to Chunks.Count-1 do begin
          PLongWord(Chunks.Data)[k]:=fPosition;
          Bytes:=Chunks.Chunks[k].Bytes;
          PLongWord(Chunks.ChunkByteCounts.Data)[k]:=Bytes;
          {$IFDEF FPC_Debug_Image}
          //writeln('TBGRAWriterTiff.WriteData Chunk fPosition=',fPosition,' Bytes=',Bytes);
          {$ENDIF}
          if Bytes>0 then
            WriteBuf(Chunks.Chunks[k].Data^,Bytes);
        end;
      end;
    end;
  end;
end;

procedure TBGRAWriterTiff.WriteEntryData(Entry: TTiffWriterEntry);
begin
  if Entry.Bytes>4 then begin
    Entry.DataPos:=fPosition;
    WriteBuf(Entry.Data^,Entry.Bytes);
  end;
end;

procedure TBGRAWriterTiff.WriteBuf(var Buf; Count: LongWord);
begin
  if Count=0 then exit;
  if (fStream<>nil) then
    fStream.Write(Buf,Count);
  inc(fPosition,Count);
end;

procedure TBGRAWriterTiff.AddImage(Img: TFPCustomImage);

  function ClampInt(Value, Min,Max: integer): integer;
  begin
    If Value<Min then result := Min
    else if Value>Max then result := Max
    else result := Value;
  end;

  procedure WriteValue(var Run: PByte; Value: Word; BitDepth: byte);
  begin
    if BitDepth=8 then begin
      Run^:= (Value+128) div 257;
      inc(Run);
    end else if BitDepth=16 then begin
      PWord(Run)^:= Value;
      inc(Run,2);
    end;
  end;

  procedure WriteValueUnscaled(var Run: PByte; Value: LongWord; BitDepth: byte);
  begin
    if BitDepth=8 then begin
      Run^:= Value;
      inc(Run);
    end else if BitDepth=16 then begin
      PWord(Run)^:= Value;
      inc(Run,2);
    end;
  end;

var
  IFD: TTiffIFD;
  GrayBits, RedBits, GreenBits, BlueBits, AlphaBits: Word;
  ImgWidth, ImgHeight: LongWord;
  Compression: Word;
  BitsPerSample: array[0..3] of Word;
  SamplesPerPixel: Integer;
  ExtraSample, defaultColorBits: Word;
  BitsPerPixel: LongWord;
  i: Integer;
  OrientedWidth, OrientedHeight: LongWord;
  BytesPerLine: LongWord;
  ChunkType: TTiffChunkType;
  ChunkCount: LongWord;
  ChunkOffsets: TTiffWriterChunkOffsets;
  ChunkIndex: LongWord;
  ChunkBytes: LongWord;
  Chunk: PByte;
  ChunkLeft, ChunkTop, ChunkWidth, ChunkHeight: LongWord;
  TilesAcross, TilesDown: LongWord;
  Run: PByte;
  Value: Integer;
  CurEntries: TFPList;
  Shorts: array[0..3] of Word;
  NewSubFileType: LongWord;
  cx,cy,x,y,sx,sy: LongWord;
  dx1,dy1,dx2,dy2: integer;
  ChunkBytesPerLine: LongWord;
  labArray: array of TLabA;
  ConvertToLab: Boolean;
  ConversionToLab: TBridgedConversion;
  sourceStride: PtrInt;

  function GetDefaultAlphaBits: Word;
  var
    alphaIndex: Integer;
  begin
    if Img is TCustomUniversalBitmap then
    begin
      if not TCustomUniversalBitmap(Img).HasTransparentPixels then
        result := 0
      else
      begin
        alphaIndex := TCustomUniversalBitmap(Img).Colorspace.IndexOfAlphaChannel;
        if (alphaIndex <> -1) and (TCustomUniversalBitmap(Img).Colorspace.GetChannelBitDepth(alphaIndex)>8) then
          result := 16
        else
          result := 8;
      end;
    end else
      result := 8;
  end;

  function GetDefaultColorBits: Word;
  var
    i, alphaIndex: Integer;
  begin
    if Img is TCustomUniversalBitmap then
    begin
      result := 0;
      alphaIndex := TCustomUniversalBitmap(Img).Colorspace.IndexOfAlphaChannel;
      for i := 0 to TCustomUniversalBitmap(Img).Colorspace.GetChannelCount-1 do
        if i <> alphaIndex then
          result := max(result, TCustomUniversalBitmap(Img).Colorspace.GetChannelBitDepth(i));

      if result > 8 then
        result := 16
      else
        result := 8;
    end else
      result := 8;
  end;

  procedure WriteNextLabPixel(const AColor: TLabA);
  begin
    case IFD.PhotoMetricInterpretation of
    8:
      begin
        WriteValue(Run, ClampInt(round(AColor.L*(65535/100)),0,65535), GreenBits);
        case RedBits of
        8: WriteValueUnscaled(Run, ClampInt(round(AColor.a),-128,127), RedBits);
        16: WriteValueUnscaled(Run, ClampInt(round(AColor.a*256),-32768,32767), RedBits);
        end;
        case BlueBits of
        8: WriteValueUnscaled(Run, ClampInt(round(AColor.b),-128,127), BlueBits);
        16: WriteValueUnscaled(Run, ClampInt(round(AColor.b*256),-32768,32767), BlueBits);
        end;
        WriteValue(Run, ClampInt(round(AColor.alpha*65535),0,65535), AlphaBits);
      end;
    9:
      begin
        case GreenBits of
        8: WriteValueUnscaled(Run, ClampInt(round(AColor.L*(255/100)),0,255), GreenBits);
        16: WriteValueUnscaled(Run, ClampInt(round(AColor.L*(65280/100)),0,65280), GreenBits);
        end;
        case RedBits of
        8: WriteValueUnscaled(Run, ClampInt(round(AColor.a)+128,0,255), RedBits);
        16: WriteValueUnscaled(Run, ClampInt(round(AColor.a*256)+32768,0,65535), RedBits);
        end;
        case BlueBits of
        8: WriteValueUnscaled(Run, ClampInt(round(AColor.b)+128,0,255), BlueBits);
        16: WriteValueUnscaled(Run, ClampInt(round(AColor.b*256)+32768,0,65535), BlueBits);
        end;
        WriteValue(Run, ClampInt(round(AColor.alpha*65535),0,65535), AlphaBits);
      end;
    else raise exception.Create('Photometric interpretation not handled');
    end;
  end;

  procedure WriteNextPixel(const AColor: TFPColor);
  begin
    if IFD.PhotoMetricInterpretation >= 8 then
    begin
      WriteNextLabPixel(AColor);
      exit;
    end;

    case IFD.PhotoMetricInterpretation of
    0,1: // grayscale
      begin
        Value:=(LongWord(AColor.red)+AColor.green+AColor.blue) div 3;
        if ExtraSample=1 then Value := Value*AColor.alpha div 65535;
        if IFD.PhotoMetricInterpretation=0 then Value:=$ffff-Value;// 0 is white
        WriteValue(Run, Value, GrayBits);
        WriteValue(Run, AColor.alpha, AlphaBits);
      end;
    2:  // RGB
      begin
        Value := AColor.red;
        if ExtraSample=1 then Value := Value*AColor.alpha div 65535;
        WriteValue(Run, Value, RedBits);
        Value := AColor.green;
        if ExtraSample=1 then Value := Value*AColor.alpha div 65535;
        WriteValue(Run, Value, GreenBits);
        Value := AColor.blue;
        if ExtraSample=1 then Value := Value*AColor.alpha div 65535;
        WriteValue(Run, Value, BlueBits);
        WriteValue(Run, AColor.alpha, AlphaBits);
      end;
    else raise exception.Create('Photometric interpretation not handled');
    end;
  end;

  procedure WriteResolutionValues;
  begin
    if (Img is TCustomUniversalBitmap) then
    with TCustomUniversalBitmap(Img) do
    begin
        IFD.ResolutionUnit :=ResolutionUnitToTifResolutionUnit(ResolutionUnit);
        IFD.XResolution.Numerator :=Trunc(ResolutionX*1000);
        IFD.XResolution.Denominator :=1000;
        IFD.YResolution.Numerator :=Trunc(ResolutionY*1000);
        IFD.YResolution.Denominator :=1000;

        Img.Extra[TiffResolutionUnit]:=IntToStr(IFD.ResolutionUnit);
        Img.Extra[TiffXResolution]:=TiffRationalToStr(IFD.XResolution);
        Img.Extra[TiffYResolution]:=TiffRationalToStr(IFD.YResolution);
     end;
  end;


begin
  ChunkOffsets:=nil;
  Chunk:=nil;
  IFD:=TTiffIFD.Create;
  try
    // add new list of entries
    CurEntries:=TFPList.Create;
    FEntries.Add(CurEntries);

    IFD.ReadFPImgExtras(Img);
    if SaveCMYKAsRGB and (IFD.PhotoMetricInterpretation=5) then
      IFD.PhotoMetricInterpretation:=2;
    if (Img.Extra[TiffPhotoMetric]='') and (Img is TCustomUniversalBitmap) then
    begin
      if cfHasImaginaryColors in TCustomUniversalBitmap(Img).Colorspace.GetFlags then
        IFD.PhotoMetricInterpretation := 8;
    end;

    if Img.Extra[TiffCompression]='' then
      IFD.Compression:= TiffCompressionDeflateZLib;

    if not (IFD.PhotoMetricInterpretation in [0,1,2,8,9]) then
      TiffError('PhotoMetricInterpretation="'+Img.Extra[TiffPhotoMetric]+'" not supported');

    //Resolution
    WriteResolutionValues;

    GrayBits:=0;
    RedBits:=0;
    GreenBits:=0;
    BlueBits:=0;
    AlphaBits:=0;
    ExtraSample:=0;
    defaultColorBits := GetDefaultColorBits;
    case IFD.PhotoMetricInterpretation of
    0,1:
      begin
        GrayBits:=StrToIntDef(Img.Extra[TiffGrayBits], defaultColorBits);
        BitsPerSample[0]:=GrayBits;
        SamplesPerPixel:=1;
      end;
    2:
      begin
        RedBits:=StrToIntDef(Img.Extra[TiffRedBits], defaultColorBits);
        GreenBits:=StrToIntDef(Img.Extra[TiffGreenBits], defaultColorBits);
        BlueBits:=StrToIntDef(Img.Extra[TiffBlueBits], defaultColorBits);
        BitsPerSample[0]:=RedBits;
        BitsPerSample[1]:=GreenBits;
        BitsPerSample[2]:=BlueBits;
        SamplesPerPixel:=3;
      end;
    8,9:
      begin
        RedBits:=StrToIntDef(Img.Extra[TiffRedBits], defaultColorBits);
        GreenBits:=StrToIntDef(Img.Extra[TiffGreenBits], defaultColorBits);
        BlueBits:=StrToIntDef(Img.Extra[TiffBlueBits], defaultColorBits);
        BitsPerSample[0]:=GreenBits;
        if (RedBits=0) and (BlueBits=0) then SamplesPerPixel := 1
        else
        begin
          SamplesPerPixel:= 3;
          if RedBits=0 then RedBits := BlueBits else
          if BlueBits=0 then BlueBits := RedBits;
          BitsPerSample[1]:= RedBits;
          BitsPerSample[2]:= BlueBits;
        end;
      end;
    end;
    AlphaBits:= StrToIntDef(Img.Extra[TiffAlphaBits],GetDefaultAlphaBits);
    if AlphaBits>0 then begin
      BitsPerSample[SamplesPerPixel]:=AlphaBits;
      inc(SamplesPerPixel);
      if PremultiplyRGB and (IFD.PhotoMetricInterpretation<=2) then
        ExtraSample := 1
      else
        ExtraSample := 2;
    end;

    ImgWidth:=Img.Width;
    ImgHeight:=Img.Height;
    Compression:=IFD.Compression;
    case Compression of
    TiffCompressionNone,
    TiffCompressionDeflateZLib: ;
    else
      {$ifdef FPC_DEBUG_IMAGE}
      writeln('TBGRAWriterTiff.AddImage unsupported compression '+TiffCompressionName(Compression)+', using deflate instead.');
      {$endif}
      Compression:=TiffCompressionDeflateZLib;
    end;

    if IFD.Orientation in [1..4] then begin
      OrientedWidth:=ImgWidth;
      OrientedHeight:=ImgHeight;
    end else begin
      // rotated
      OrientedWidth:=ImgHeight;
      OrientedHeight:=ImgWidth;
    end;

    {$IFDEF FPC_Debug_Image}
    writeln('TBGRAWriterTiff.AddImage PhotoMetricInterpretation=',IFD.PhotoMetricInterpretation);
    writeln('TBGRAWriterTiff.AddImage ImageWidth=',ImgWidth,' ImageHeight=',ImgHeight);
    writeln('TBGRAWriterTiff.AddImage Orientation=',IFD.Orientation);
    writeln('TBGRAWriterTiff.AddImage ResolutionUnit=',IFD.ResolutionUnit);
    writeln('TBGRAWriterTiff.AddImage XResolution=',TiffRationalToStr(IFD.XResolution));
    writeln('TBGRAWriterTiff.AddImage YResolution=',TiffRationalToStr(IFD.YResolution));
    writeln('TBGRAWriterTiff.AddImage GrayBits=',GrayBits,' RedBits=',RedBits,' GreenBits=',GreenBits,' BlueBits=',BlueBits,' AlphaBits=',AlphaBits);
    writeln('TBGRAWriterTiff.AddImage Compression=',TiffCompressionName(Compression));
    writeln('TBGRAWriterTiff.AddImage Page=',IFD.PageNumber,'/',IFD.PageCount);
    {$ENDIF}

    // required meta entries
    AddEntryShortOrLong(256,ImgWidth);
    AddEntryShortOrLong(257,ImgHeight);
    AddEntryShort(259,Compression);
    AddEntryShort(262,IFD.PhotoMetricInterpretation);
    AddEntryShort(274,IFD.Orientation);
    AddEntryShort(296,IFD.ResolutionUnit);
    AddEntryRational(282,IFD.XResolution);
    AddEntryRational(283,IFD.YResolution);
    // BitsPerSample (required)
    AddEntry(258,3,SamplesPerPixel,@BitsPerSample[0],SamplesPerPixel*2);
    AddEntryShort(277,SamplesPerPixel);
    if ExtraSample<>0 then AddEntryShort(338, ExtraSample);

    // BitsPerPixel, BytesPerLine
    BitsPerPixel:=0;
    for i:=0 to SamplesPerPixel-1 do
      inc(BitsPerPixel,BitsPerSample[i]);
    BytesPerLine:=(BitsPerPixel*OrientedWidth+7) div 8;

    // optional entries
    NewSubFileType:=0;
    if IFD.ImageIsThumbNail then inc(NewSubFileType,1);
    if IFD.ImageIsPage then inc(NewSubFileType,2);
    if IFD.ImageIsMask then inc(NewSubFileType,4);
    if NewSubFileType>0 then
      AddEntryLong(254,NewSubFileType);
    if IFD.DocumentName<>'' then
      AddEntryString(269,IFD.DocumentName);
    if IFD.ImageDescription<>'' then
      AddEntryString(270,IFD.ImageDescription);
    if IFD.Make_ScannerManufacturer<>'' then
      AddEntryString(271,IFD.Make_ScannerManufacturer);
    if IFD.Model_Scanner<>'' then
      AddEntryString(272,IFD.Model_Scanner);
    if IFD.Software<>'' then
      AddEntryString(305,IFD.Software);
    if IFD.DateAndTime<>'' then
      AddEntryString(306,IFD.DateAndTime);
    if IFD.Artist<>'' then
      AddEntryString(315,IFD.Artist);
    if IFD.HostComputer<>'' then
      AddEntryString(316,IFD.HostComputer);
    if IFD.PageCount>0 then begin
      Shorts[0]:=IFD.PageNumber;
      Shorts[1]:=IFD.PageCount;
      AddEntry(297,3,2,@Shorts[0],2*SizeOf(Word));
    end;
    if IFD.PageName<>'' then
      AddEntryString(285,IFD.PageName);
    if IFD.Copyright<>'' then
      AddEntryString(33432,IFD.Copyright);

    // chunks
    ChunkType:=tctStrip;
    if IFD.TileWidth>0 then begin
      AddEntryShortOrLong(322,IFD.TileWidth);
      AddEntryShortOrLong(323,IFD.TileLength);
      ChunkType:=tctTile;
    end else begin
      // RowsPerStrip (required)
      if OrientedWidth=0 then
        IFD.RowsPerStrip:=8
      else
        IFD.RowsPerStrip:=8192 div BytesPerLine;
      if IFD.RowsPerStrip<1 then
        IFD.RowsPerStrip:=1;
      {$IFDEF FPC_Debug_Image}
      writeln('TBGRAWriterTiff.AddImage BitsPerPixel=',BitsPerPixel,' OrientedWidth=',OrientedWidth,' BytesPerLine=',BytesPerLine,' RowsPerStrip=',IFD.RowsPerStrip);
      {$ENDIF}
      AddEntryShortOrLong(278,IFD.RowsPerStrip);
    end;

    // tags for Offsets and ByteCounts
    ChunkOffsets:=TTiffWriterChunkOffsets.Create(ChunkType);
    AddEntry(ChunkOffsets);
    AddEntry(ChunkOffsets.ChunkByteCounts);

    labArray := nil;
    if (Img is TCustomUniversalBitmap) and
       (IFD.PhotoMetricInterpretation >= 8) then
    begin
      ConvertToLab := true;
      ConversionToLab := TCustomUniversalBitmap(Img).Colorspace.GetBridgedConversion(TLabAColorspace);
    end else
      ConvertToLab := false;

    if (OrientedHeight>0) and (OrientedWidth>0) then begin
      if ChunkType=tctTile then begin
        TilesAcross:=(OrientedWidth+IFD.TileWidth{%H-}-1) div IFD.TileWidth;
        TilesDown:=(OrientedHeight+IFD.TileLength{%H-}-1) div IFD.TileLength;
        ChunkCount:=TilesAcross*TilesDown;
        {$IFDEF FPC_Debug_Image}
        writeln('TBGRAWriterTiff.AddImage BitsPerPixel=',BitsPerPixel,' OrientedWidth=',OrientedWidth,' OrientedHeight=',OrientedHeight,' TileWidth=',IFD.TileWidth,' TileLength=',IFD.TileLength,' TilesAcross=',TilesAcross,' TilesDown=',TilesDown,' ChunkCount=',ChunkCount);
        {$ENDIF}
      end else begin
        ChunkCount:=(OrientedHeight+IFD.RowsPerStrip{%H-}-1) div IFD.RowsPerStrip;
      end;
      ChunkOffsets.SetCount(ChunkCount);
      // create chunks
      for ChunkIndex:=0 to ChunkCount-1 do begin
        if ChunkType=tctTile then begin
          ChunkLeft:=(ChunkIndex mod TilesAcross)*IFD.TileWidth;
          ChunkTop:=(ChunkIndex div TilesAcross)*IFD.TileLength;
          ChunkWidth:=Min(IFD.TileWidth,OrientedWidth-ChunkLeft);
          ChunkHeight:=Min(IFD.TileLength,OrientedHeight-ChunkTop);
          // boundary tiles are padded to a full tile
          // the padding is filled with 0 and compression will get rid of it
          ChunkBytesPerLine:=(BitsPerPixel*IFD.TileWidth+7) div 8;
          ChunkBytes:=ChunkBytesPerLine*IFD.TileLength;
        end else begin
          ChunkLeft:=0;
          ChunkTop:=IFD.RowsPerStrip*ChunkIndex;
          ChunkWidth:=OrientedWidth;
          ChunkHeight:=Min(IFD.RowsPerStrip,OrientedHeight-ChunkTop);
          ChunkBytesPerLine:=BytesPerLine;
          ChunkBytes:=ChunkBytesPerLine*ChunkHeight;
        end;
        GetMem(Chunk,ChunkBytes);
        FillByte(Chunk^,ChunkBytes,0); // fill unused bytes with 0 to help compression

        // Orientation
        if IFD.Orientation in [1..4] then begin
          sx:=ChunkLeft; sy:=ChunkTop;
          dy1 := 0; dx2 := 0;
          case IFD.Orientation of
          1: begin dx1:=1; dy2:=1; end;// 0,0 is left, top
          2: begin sx:=OrientedWidth-sx-1; dx1:=-1; dy2:=1; end;// 0,0 is right, top
          3: begin sx:=OrientedWidth-sx-1; dx1:=-1; sy:=OrientedHeight-sy-1; dy2:=-1; end;// 0,0 is right, bottom
          4: begin dx1:=1; sy:=OrientedHeight-sy-1; dy2:=-1; end;// 0,0 is left, bottom
          end;
        end else begin
          // rotated
          sx:=ChunkTop; sy:=ChunkLeft;
          dx1 := 0; dy2 := 0;
          case IFD.Orientation of
          5: begin dx2:=1; dy1:=1; end;// 0,0 is top, left (rotated)
          6: begin dx2:=1; sy:=OrientedWidth-sy-1; dy1:=-1; end;// 0,0 is top, right (rotated)
          7: begin sx:=OrientedHeight-sx-1; dx2:=-1; sy:=OrientedWidth-sy-1; dy1:=-1; end;// 0,0 is bottom, right (rotated)
          8: begin sx:=OrientedHeight-sx-1; dx2:=-1; dy1:=1; end;// 0,0 is bottom, left (rotated)
          end;
        end;
        //writeln('TBGRAWriterTiff.AddImage Chunk=',ChunkIndex,'/',ChunkCount,' ChunkBytes=',ChunkBytes,' ChunkRect=',ChunkLeft,',',ChunkTop,',',ChunkWidth,'x',ChunkHeight,' x=',x,' y=',y,' dx=',dx,' dy=',dy);
        for cy:=0 to ChunkHeight-1 do begin
          Run:=Chunk+cy*ChunkBytesPerLine;

          if ConvertToLab then
          begin
            if ChunkWidth > Length(labArray) then setlength(labArray, ChunkWidth);
            sourceStride := TCustomUniversalBitmap(Img).RowSize*dy1;
            if TCustomUniversalBitmap(Img).LineOrder = riloBottomToTop then
              sourceStride := -sourceStride;
            inc(sourceStride, dx1*PtrInt(TCustomUniversalBitmap(Img).Colorspace.GetSize) );
            ConversionToLab.Convert( TCustomUniversalBitmap(Img).GetPixelAddress(sx,sy),
                                     @labArray[0], ChunkWidth, sourceStride, sizeof(TLabA), nil);
            for cx := 0 to ChunkWidth-1 do
              WriteNextLabPixel(labArray[cx]);
          end else
          begin
            x := sx;
            y := sy;
            for cx:=0 to ChunkWidth-1 do begin
              WriteNextPixel(Img.Colors[x,y]);
              inc(x,dx1);
              inc(y,dy1);
            end;
          end;

          inc(sx,dx2);
          inc(sy,dy2);
        end;

        // compress
        case Compression of
        TiffCompressionDeflateZLib: EncodeDeflate(Chunk,ChunkBytes);
        end;

        ChunkOffsets.Chunks[ChunkIndex].Data:=Chunk;
        ChunkOffsets.Chunks[ChunkIndex].Bytes:=ChunkBytes;
        // next chunk
      end;
      // created chunks
    end;

    CurEntries.Sort(@CompareTiffWriteEntries);
  finally
    IFD.Free;
  end;
end;

procedure TBGRAWriterTiff.SaveToStream(Stream: TStream);
begin
  fStartPos:=Stream.Position;
  // simulate write to compute offsets
  fStream:=nil;
  WriteTiff;
  // write to stream
  fStream:=Stream;
  WriteTiff;
  fStream:=nil;
end;

procedure TBGRAWriterTiff.InternalWrite(Stream: TStream; Img: TFPCustomImage);
begin
  AddImage(Img);
  SaveToStream(Stream);
end;

procedure TBGRAWriterTiff.AddEntryString(Tag: word; const s: string);
begin
  if s<>'' then
    AddEntry(Tag,2,length(s)+1,@s[1],length(s)+1)
  else
    AddEntry(Tag,2,0,nil,0);
end;

procedure TBGRAWriterTiff.AddEntryShort(Tag: word; Value: Word);
begin
  AddEntry(Tag,3,1,@Value,2);
end;

procedure TBGRAWriterTiff.AddEntryLong(Tag: word; Value: LongWord);
begin
  AddEntry(Tag,4,1,@Value,4);
end;

procedure TBGRAWriterTiff.AddEntryShortOrLong(Tag: word; Value: LongWord);
begin
  if Value<=High(Word) then
    AddEntryShort(Tag,Value)
  else
    AddEntryLong(Tag,Value);
end;

procedure TBGRAWriterTiff.AddEntryRational(Tag: word; const Value: TTiffRational
  );
begin
  AddEntry(Tag,5,1,@Value,8);
end;

procedure TBGRAWriterTiff.AddEntry(Tag: Word; EntryType: Word; EntryCount: LongWord;
  Data: Pointer; Bytes: LongWord; CopyData: boolean);
var
  Entry: TTiffWriterEntry;
begin
  Entry:=TTiffWriterEntry.Create;
  Entry.Tag:=Tag;
  Entry.EntryType:=EntryType;
  Entry.Count:=EntryCount;
  if CopyData then begin
    if Bytes>0 then begin
      GetMem(Entry.Data,Bytes);
      System.Move(Data^,Entry.Data^,Bytes);
    end else begin
      Entry.Data:=nil;
    end;
  end else
    Entry.Data:=Data;
  Entry.Bytes:=Bytes;
  AddEntry(Entry);
end;

procedure TBGRAWriterTiff.AddEntry(Entry: TTiffWriterEntry);
var
  List: TFPList;
begin
  List:=TFPList(FEntries[FEntries.Count-1]);
  List.Add(Entry);
end;

procedure TBGRAWriterTiff.TiffError(Msg: string);
begin
  raise Exception.Create('TBGRAWriterTiff.TiffError: '+Msg);
end;

procedure TBGRAWriterTiff.EncodeDeflate(var Buffer: Pointer; var Count: LongWord);
var
  NewBuffer: PByte;
  NewCount: LongWord;
  ErrorMsg: String;
begin
  ErrorMsg:='';
  NewBuffer:=nil;
  try
    NewCount:=Count;
    if not CompressDeflate(Buffer,Count,NewBuffer,NewCount,@ErrorMsg) then
      TiffError(ErrorMsg);
    FreeMem(Buffer);
    Buffer:=NewBuffer;
    Count:=NewCount;
    NewBuffer:=nil;
  finally
    ReAllocMem(NewBuffer,0);
  end;
end;

constructor TBGRAWriterTiff.Create;
begin
  inherited Create;
  FEntries:=TFPList.Create;
  FSaveCMYKAsRGB:= true;
  FPremultiplyRGB:= false;
end;

destructor TBGRAWriterTiff.Destroy;
begin
  Clear;
  FreeAndNil(FEntries);
  inherited Destroy;
end;

procedure TBGRAWriterTiff.Clear;
begin
  ClearEntries;
end;

{ TTiffWriterEntry }

destructor TTiffWriterEntry.Destroy;
begin
  ReAllocMem(Data,0);
  inherited Destroy;
end;

{ TTiffWriterChunkOffsets }

constructor TTiffWriterChunkOffsets.Create(ChunkType: TTiffChunkType);
begin
  EntryType:=4; // long
  ChunkByteCounts:=TTiffWriterEntry.Create;
  ChunkByteCounts.EntryType:=4; // long
  if ChunkType=tctTile then begin
    Tag:=324; // TileOffsets
    ChunkByteCounts.Tag:=325; // TileByteCounts
  end else begin
    Tag:=273; // StripOffsets
    ChunkByteCounts.Tag:=279; // StripByteCounts
  end;
end;

destructor TTiffWriterChunkOffsets.Destroy;
var
  i: Integer;
begin
  if Chunks<>nil then begin
    for i:=0 to Count-1 do
      ReAllocMem(Chunks[i].Data,0);
    ReAllocMem(Chunks,0);
  end;
  inherited Destroy;
end;

procedure TTiffWriterChunkOffsets.SetCount(NewCount: LongWord);
var
  Size: LongWord;
begin
  {$IFDEF FPC_Debug_Image}
  writeln('TTiffWriteStripOffsets.SetCount OldCount=',Count,' NewCount=',NewCount);
  {$ENDIF}
  Count:=NewCount;
  Size:=Count*SizeOf(TTiffWriterChunk);
  ReAllocMem(Chunks,Size);
  if Size>0 then FillByte(Chunks^,Size,0);
  Size:=Count*SizeOf(LongWord);
  // Offsets
  ReAllocMem(Data,Size);
  if Size>0 then FillByte(Data^,Size,0);
  Bytes:=Size;
  // ByteCounts
  ReAllocMem(ChunkByteCounts.Data,Size);
  if Size>0 then FillByte(ChunkByteCounts.Data^,Size,0);
  ChunkByteCounts.Count:=Count;
  ChunkByteCounts.Bytes:=Size;
end;

initialization
  if ImageHandlers.ImageWriter[TiffHandlerName]=nil then
    ImageHandlers.RegisterImageWriter (TiffHandlerName, 'tif;tiff', TBGRAWriterTiff);
  DefaultBGRAImageWriter[ifTiff] := TBGRAWriterTiff;

end.
