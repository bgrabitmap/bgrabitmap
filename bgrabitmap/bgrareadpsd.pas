// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
    The original file was part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team

    Psd reader for fpImage modified by circular.

 **********************************************************************

    03/2014 changes by circular :
    - added MinifyHeight,WantedHeight and OutputHeight (useful for thumbnails)

    2023-06  - Massimo Magnano
             - added Read of Image Resources Section
             - added Resolution support

}
unit BGRAReadPSD;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, BGRABitmapTypes, SysUtils, FPimage, FPReadPSD;

const
  { Image Resource IDs  }
  PSD_ResourceSectionSignature ='8BIM';

  PSD_PS2_IMAGE_INFO = $03e8;  { Obsolete - ps 2.0 image info  }
  PSD_MAC_PRINT_INFO = $03e9;  { Optional - Mac print manager print info record  }
  PSD_PS2_COLOR_TAB = $03eb;  { Obsolete - ps 2.0 indexed color table  }
  PSD_RESN_INFO = $03ed;  { ResolutionInfo structure  }
  PSD_ALPHA_NAMES = $03ee;  { Alpha channel names  }
  PSD_DISPLAY_INFO = $03ef;  { Superceded by PSD_DISPLAY_INFO_NEW for ps CS3 and higher - DisplayInfo structure  }
  PSD_CAPTION = $03f0;  { Optional - Caption string  }
  PSD_BORDER_INFO = $03f1;  { Border info  }
  PSD_BACKGROUND_COL = $03f2;  { Background color  }
  PSD_PRINT_FLAGS = $03f3;  { Print flags  }
  PSD_GREY_HALFTONE = $03f4;  { Greyscale and multichannel halftoning info  }
  PSD_COLOR_HALFTONE = $03f5;  { Color halftoning info  }
  PSD_DUOTONE_HALFTONE = $03f6;  { Duotone halftoning info  }
  PSD_GREY_XFER = $03f7;  { Greyscale and multichannel transfer functions  }
  PSD_COLOR_XFER = $03f8;  { Color transfer functions  }
  PSD_DUOTONE_XFER = $03f9;  { Duotone transfer functions  }
  PSD_DUOTONE_INFO = $03fa;  { Duotone image information  }
  PSD_EFFECTIVE_BW = $03fb;  { Effective black & white values for dot range  }
  PSD_OBSOLETE_01 = $03fc;  { Obsolete  }
  PSD_EPS_OPT = $03fd;  { EPS options  }
  PSD_QUICK_MASK = $03fe;  { Quick mask info  }
  PSD_OBSOLETE_02 = $03ff;  { Obsolete  }
  PSD_LAYER_STATE = $0400;  { Layer state info  }
  PSD_WORKING_PATH = $0401;  { Working path (not saved)  }
  PSD_LAYER_GROUP = $0402;  { Layers group info  }
  PSD_OBSOLETE_03 = $0403;  { Obsolete  }
  PSD_IPTC_NAA_DATA = $0404;  { IPTC-NAA record (IMV4.pdf)  }
  PSD_IMAGE_MODE_RAW = $0405;  { Image mode for raw format files  }
  PSD_JPEG_QUAL = $0406;  { JPEG quality  }
  PSD_GRID_GUIDE = $0408;  { Grid & guide info  }
  PSD_THUMB_RES = $0409;  { Thumbnail resource  }
  PSD_COPYRIGHT_FLG = $040a;  { Copyright flag  }
  PSD_URL = $040b;  { URL string  }
  PSD_THUMB_RES2 = $040c;  { Thumbnail resource  }
  PSD_GLOBAL_ANGLE = $040d;  { Superceded by PSD_NEW_COLOR_SAMPLER for ps CS3 and higher - Global angle  }
  PSD_COLOR_SAMPLER = $040e;  { Superceded by PSD_NEW_COLOR_SAMPLER for ps CS3 and higher - Color samplers resource  }
  PSD_ICC_PROFILE = $040f;  { ICC Profile  }
  PSD_WATERMARK = $0410;  { Watermark  }
  PSD_ICC_UNTAGGED = $0411;  { Do not use ICC profile flag  }
  PSD_EFFECTS_VISIBLE = $0412;  { Show / hide all effects layers  }
  PSD_SPOT_HALFTONE = $0413;  { Spot halftone  }
  PSD_DOC_IDS = $0414;  { Document specific IDs  }
  PSD_ALPHA_NAMES_UNI = $0415;  { Unicode alpha names  }
  PSD_IDX_COL_TAB_CNT = $0416;  { Indexed color table count  }
  PSD_IDX_TRANSPARENT = $0417;  { Index of transparent color (if any)  }
  PSD_GLOBAL_ALT = $0419;  { Global altitude  }
  PSD_SLICES = $041a;  { Slices  }
  PSD_WORKFLOW_URL_UNI = $041b;  { Workflow URL - Unicode string  }
  PSD_JUMP_TO_XPEP = $041c;  { Jump to XPEP (?)  }
  PSD_ALPHA_ID = $041d;  { Alpha IDs  }
  PSD_URL_LIST_UNI = $041e;  { URL list - unicode  }
  PSD_VERSION_INFO = $0421;  { Version info  }
  PSD_EXIF_DATA = $0422;  { Exif data block 1  }
  PSD_EXIF_DATA_3 = $0423;  { Exif data block 3 (?)  }
  PSD_XMP_DATA = $0424;  { XMP data block  }
  PSD_CAPTION_DIGEST = $0425;  { Caption digest  }
  PSD_PRINT_SCALE = $0426;  { Print scale  }
  PSD_PIXEL_AR = $0428;  { Pixel aspect ratio  }
  PSD_LAYER_COMPS = $0429;  { Layer comps  }
  PSD_ALT_DUOTONE_COLOR = $042A;  { Alternative Duotone colors  }
  PSD_ALT_SPOT_COLOR = $042B;  { Alternative Spot colors  }
  PSD_LAYER_SELECT_ID = $042D;  { Layer selection ID  }
  PSD_HDR_TONING_INFO = $042E;  { HDR toning information  }
  PSD_PRINT_INFO_SCALE = $042F;  { Print scale  }
  PSD_LAYER_GROUP_E_ID = $0430;  { Layer group(s) enabled ID  }
  PSD_COLOR_SAMPLER_NEW = $0431;  { Color sampler resource for ps CS3 and higher PSD files  }
  PSD_MEASURE_SCALE = $0432;  { Measurement scale  }
  PSD_TIMELINE_INFO = $0433;  { Timeline information  }
  PSD_SHEET_DISCLOSE = $0434;  { Sheet discloser  }
  PSD_DISPLAY_INFO_NEW = $0435;  { DisplayInfo structure for ps CS3 and higher PSD files  }
  PSD_ONION_SKINS = $0436;  { Onion skins  }
  PSD_COUNT_INFO = $0438;  { Count information }
  PSD_PRINT_INFO = $043A;  { Print information added in ps CS5 }
  PSD_PRINT_STYLE = $043B;  { Print style  }
  PSD_MAC_NSPRINTINFO = $043C;  { Mac NSPrintInfo }
  PSD_WIN_DEVMODE = $043D;  { Windows DEVMODE  }
  PSD_AUTO_SAVE_PATH = $043E;  { Auto save file path  }
  PSD_AUTO_SAVE_FORMAT = $043F;  { Auto save format  }
  PSD_PATH_INFO_FIRST = $07d0;  { First path info block  }
  PSD_PATH_INFO_LAST = $0bb6;  { Last path info block  }
  PSD_CLIPPING_PATH = $0bb7;  { Name of clipping path  }
  PSD_PLUGIN_R_FIRST = $0FA0;  { First plugin resource  }
  PSD_PLUGIN_R_LAST = $1387;  { Last plugin resource  }
  PSD_IMAGEREADY_VARS = $1B58;  { Imageready variables  }
  PSD_IMAGEREADY_DATA = $1B59;  { Imageready data sets  }
  PSD_LIGHTROOM_WORK = $1F40;  { Lightroom workflow  }
  PSD_PRINT_FLAGS_2 = $2710;  { Print flags  }

  { Display resolution units  }
  PSD_RES_INCH = 1; { Pixels / inch  }
  PSD_RES_CM = 2;   { Pixels / cm  }

  { Width and height units  }
  PSD_UNIT_INCH = 1;  { inches  }
  PSD_UNIT_CM = 2;    { cm  }
  PSD_UNIT_POINT = 3; { points  (72 points =   1 inch)  }
  PSD_UNIT_PICA = 4;  { pica    ( 6 pica   =   1 inch)  }
  PSD_UNIT_COLUMN = 5;{ columns ( column defined in ps prefs, default = 2.5 inches)  }



type
  { TBGRAReaderPSD }

  TPSDResourceBlock = packed record
    Types : array[0..3] of Char;   // Always "8BIM"
    ID:word;                       // see previous Image Resource IDs constants
    NameLen:Byte;                  // Pascal-format string, 2 bytes or longer
    Name:Char;
  end;
  PPSDResourceBlock =^TPSDResourceBlock;

  TPSDResourceBlockData = packed record
    Size:LongWord;
    Data:Byte;
  end;
  PPSDResourceBlockData =^TPSDResourceBlockData;

  TBGRAReaderPSD = class(TFPReaderPSD)
  private
    FCompressed: boolean;
  protected
    FScanLines      : array of PByte;
    FInputLine     : array of record
        StreamOffset: Int64;
        Size: PtrInt;
      end;
    FImage:TFPCustomImage;
    FOutputHeight: integer;
    function ReadPalette(Stream: TStream): boolean;
    procedure AnalyzeHeader;
    procedure ReadResourceBlockData(blockID:Word; blockName:ShortString; Size:LongWord; Data:Pointer); virtual;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function ReadScanLine(Stream: TStream; AInputSize: PtrInt; AChannel: integer): boolean; overload;
    procedure WriteScanLine(Img: TFPCustomImage; Row: integer); overload;
    function  InternalCheck(Stream: TStream) : boolean; override;
  public
    MinifyHeight,WantedHeight: integer;
    constructor Create; override;
    property Compressed: Boolean read FCompressed;
    property OutputHeight: integer read FOutputHeight;
  end;

function PSDResolutionUnitToResolutionUnit(APSDResolutionUnit: Word): TResolutionUnit;
function ResolutionUnitToPSdResolutionUnit(AResolutionUnit: TResolutionUnit): Word;

implementation

function clamp(AValue, AMax: integer): integer;
begin
  if AValue < 0 then result := 0 else
  if AValue > AMax then result := AMax else
   result := AValue;;
end;

function CMYKtoRGB ( C : TFPColor):  TFPColor;
var r,g,b: integer;
begin
  r := $ffff - c.red + c.green div 10 + c.blue div 10 - c.alpha;
  g := $ffff + c.red div 10 - c.green + c.blue div 10 - c.alpha;
  b := $ffff + c.red div 10 + c.green div 10 - c.blue - c.alpha;
  result.red := clamp(r, 65535);
  result.green := clamp(g, 65535);
  result.blue := clamp(b, 65535);
  Result.alpha:=alphaOpaque;
end;

function fInv(t: single): single;
begin
  if t > 6/29 then result := t*t*t else
   result := 3*(6/29)*(6/29)*(t-4/29);
end;

function Csrgb(linear: single): single;
begin
  if linear <= 0.0031308 then
      result := 12.92*linear else
  result := (1+0.055)*exp(ln(linear)*(1/2.4)) - 0.055;
end;

function LabToRGB(L,a,b: single):TFPColor; overload;
var r,g,blue: single;
begin
  if a < 0 then
    r := L + a + 0.25*b
  else
    r := L + 0.75*a + 0.25*b;
  g := L - 0.25*a;
  blue := L - b;
  Result.red:= clamp(round((r)*65535),65535);
  Result.green:= clamp(round((g)*65535),65535);
  Result.blue:= clamp(round((blue)*65535),65535);
  result.alpha := 65535;
end;

function LabToRGB(const Lab:TLab):TFPColor; overload;
var L: single;
begin
  L := 1/255*Lab.L;
  result := LabToRGB(L,(Lab.a-128)/127,(Lab.b-128)/127);
end;

function PSDResolutionUnitToResolutionUnit(APSDResolutionUnit: Word): TResolutionUnit;
begin
  Case APSDResolutionUnit of
  PSD_RES_INCH: Result :=ruPixelsPerInch;
  PSD_RES_CM: Result :=ruPixelsPerCentimeter;
  else Result :=ruNone;
  end;
end;

function ResolutionUnitToPSdResolutionUnit(AResolutionUnit: TResolutionUnit): Word;
begin
  Case AResolutionUnit of
  ruPixelsPerInch: Result :=PSD_RES_INCH;
  ruPixelsPerCentimeter: Result :=PSD_RES_CM;
  else Result :=0;
  end;
end;

{ TBGRAReaderPSD }

function TBGRAReaderPSD.ReadPalette(Stream: TStream): boolean;
Var
  I : Integer;
  c : TFPColor;
  OldPos: Integer;
  BufSize:Longint;
  {%H-}PalBuf: array[0..767] of Byte;
  ContProgress: Boolean;
begin
  Result:=false;
  ThePalette.count := 0;
  OldPos := Stream.Position;
  BufSize:=0;
  Stream.Read(BufSize, SizeOf(BufSize));
  BufSize:=BEtoN(BufSize);
  Stream.Read({%H-}PalBuf, BufSize);
  ContProgress:=true;
  Progress(FPimage.psRunning, 0, False, Rect(0,0,0,0), '', ContProgress);
  if not ContProgress then exit;
  For I:=0 To BufSize div 3 Do
  Begin
    With c do
    begin
      Red:=PalBuf[I] shl 8;
      Green:=PalBuf[I+(BufSize div 3)] shl 8;
      Blue:=PalBuf[I+(BufSize div 3)* 2] shl 8;
      Alpha:=alphaOpaque;
    end;
    ThePalette.Add(C);
  End;
  Stream.Position := OldPos;
  Result:=true;
end;

procedure TBGRAReaderPSD.AnalyzeHeader;
var channel: integer;
begin
  With FHeader do
  begin
    Depth:=BEtoN(Depth);
    if (Signature <> '8BPS') then
      Raise Exception.Create('Unknown/Unsupported PSD image type');
    Channels:=BEtoN(Channels);
    if Channels > 4 then
      FBytesPerPixel:=Depth*4
    else
      FBytesPerPixel:=Depth*Channels;
    Mode:=BEtoN(Mode);
    FWidth:=BEtoN(Columns);
    FHeight:=BEtoN(Rows);
    FChannelCount:=Channels;
    FLineSize:=(PtrInt(FWidth)*Depth+7) div 8;
    setlength(FScanLines, FChannelCount);
    for channel := 0 to FChannelCount-1 do
      GetMem(FScanLines[channel],FLineSize);
  end;
end;

//MaxM: in the near future we could make a list organized by ids (the blockname is always null) to hold the data of the blocks
procedure TBGRAReaderPSD.ReadResourceBlockData(blockID:Word; blockName:ShortString; Size: LongWord; Data: Pointer);
var
  ResolutionInfo:TResolutionInfo;
  ResDWord: DWord;

begin
  case blockID of
  PSD_RESN_INFO:begin
          if (FImage is TCustomUniversalBitmap) then
          with TCustomUniversalBitmap(FImage) do
          begin
            ResolutionInfo :=TResolutionInfo(Data^);
            //MaxM: Do NOT Remove the Casts after BEToN
            ResolutionUnit :=PSDResolutionUnitToResolutionUnit(BEToN(Word(ResolutionInfo.hResUnit)));

            //MaxM: Resolution always recorded in a fixed point implied decimal int32
            //      with 16 bits before point and 16 after (cast as DWord and divide resolution by 2^16
            ResDWord :=BEToN(DWord(ResolutionInfo.hRes));
            ResolutionX :=ResDWord/65536;
            ResDWord :=BEToN(DWord(ResolutionInfo.vRes));
            ResolutionY :=ResDWord/65536;

            if (ResolutionUnit<>ruNone) and
               (ResolutionInfo.vResUnit<>ResolutionInfo.hResUnit)
            then Case BEToN(Word(ResolutionInfo.vResUnit)) of
                 PSD_RES_INCH: ResolutionY :=ResolutionY/2.54; //Vertical Resolution is in Inch convert to Cm
                 PSD_RES_CM: ResolutionY :=ResolutionY*2.54; //Vertical Resolution is in Cm convert to Inch
                 end;
          end;
        end;
  end;
end;

procedure TBGRAReaderPSD.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  H,HOutput,InputLineIndex,LenOfLineIndex,channel: Integer;
  LenOfLineFactor: PtrInt;
  BufSize:LongWord;
  Encoding:word;
  ContProgress: Boolean;
  CurOffset: int64;
  PrevOutputRow, OutputRow, OutputRowAdd, OutputRowAcc, OutputRowAccAdd, OutputRowMod: integer;

  procedure ReadResourceBlocks;
  var
     TotalBlockSize,
     pPosition:LongWord;
     blockData,
     curBlock :PPSDResourceBlock;
     curBlockData :PPSDResourceBlockData;
     signature:String[4];
     blockName:ShortString;
     blockID:Word;
     dataSize:LongWord;

  begin
    //MaxM: Do NOT Remove the Casts after BEToN
    Stream.Read(TotalBlockSize, 4);
    TotalBlockSize :=BEtoN(DWord(TotalBlockSize));
    GetMem(blockData, TotalBlockSize);
    try
       Stream.Read(blockData^, TotalBlockSize);

       pPosition :=0;
       curBlock :=blockData;

       repeat
         signature :=curBlock^.Types;

         if (signature=PSD_ResourceSectionSignature) then
         begin
           blockID :=BEtoN(Word(curBlock^.ID));
           blockName :=curBlock^.Name;
           setLength(blockName, curBlock^.NameLen);
           curBlockData :=PPSDResourceBlockData(curBlock);

           Inc(Pointer(curBlockData), sizeof(TPSDResourceBlock));

           if (curBlock^.NameLen>0) then //Maybe tested, in all my tests is always 0
           begin
             Inc(Pointer(curBlockData), curBlock^.NameLen);
             if not(Odd(curBlock^.NameLen))
             then Inc(Pointer(curBlockData), 1);
           end;

           dataSize :=BEtoN(DWord(curBlockData^.Size));
           Inc(Pointer(curBlockData), 4);
           ReadResourceBlockData(blockID, blockName, dataSize, curBlockData);
           Inc(Pointer(curBlockData), dataSize);
         end
         else Inc(Pointer(curBlockData), 1); //skip padding or something went wrong, search for next '8BIM'

         curBlock :=PPSDResourceBlock(curBlockData);
         pPosition :=Pointer(curBlockData)-Pointer(blockData);
       until (pPosition >= TotalBlockSize);

    finally
      FreeMem(blockData, TotalBlockSize);
    end;
  end;

begin
  FImage :=Img;
  FScanLines:=nil;
  FPalette:=nil;
  try
    Stream.Position:=0;
    ContProgress:=true;
    Progress(FPimage.psStarting, 0, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;
    // read header
    Stream.Read(FHeader, SizeOf(FHeader));
    Progress(FPimage.psRunning, 0, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;
    AnalyzeHeader;
    Case FHeader.Mode of
        0:begin  // Bitmap (monochrome)
            FPalette := TFPPalette.Create(0);
            CreateBWPalette;
          end;
        1, 8:begin // Gray-scale
            FPalette := TFPPalette.Create(0);
            CreateGrayPalette;
          end;
        2:begin // Indexed color (palette color)
            FPalette := TFPPalette.Create(0);
            if not ReadPalette(stream) then exit;
          end;
    end;

    if Assigned(OnCreateImage) then
      OnCreateImage(Self,Img);

    if (MinifyHeight > 0) and (FHeight > MinifyHeight) then
      FOutputHeight:= MinifyHeight
    else
      if WantedHeight > 0 then
        FOutputHeight:= WantedHeight
      else
        FOutputHeight:= FHeight;
    Img.SetSize(FWidth,FOutputHeight);

//MaxM: the original code is wrong as it should read the size of the palette (BufSize) anyway
    //  color palette
    BufSize:=0;
    Stream.Read(BufSize, SizeOf(BufSize));
    BufSize:=BEtoN(BufSize);
    Stream.Seek(BufSize, soCurrent);

    // Image Resources Section
    ReadResourceBlocks;

    //  mask
    Stream.Read(BufSize, SizeOf(BufSize));
    BufSize:=BEtoN(BufSize);
    Stream.Seek(BufSize, soCurrent);
    //  compression type
    Encoding:=0;
    Stream.Read(Encoding, SizeOf(Encoding));
    FCompressed:=BEtoN(Encoding) = 1;
    if BEtoN(Encoding)>1 then
      Raise Exception.Create('Unknown compression type');
    If FCompressed then
    begin
      SetLength(FLengthOfLine, FHeight * FChannelCount);
      Stream.ReadBuffer(FLengthOfLine[0], 2 * Length(FLengthOfLine));
      Progress(FPimage.psRunning, 0, False, Rect(0,0,0,0), '', ContProgress);
      if not ContProgress then exit;
      if not (FHeader.Mode in [0, 2]) then
        LenOfLineFactor := FHeader.Depth div 8
      else
        LenOfLineFactor := 1;
    end else
    begin
      FLengthOfLine := nil;
    end;

    setlength(FInputLine, FHeight * FChannelCount);
    CurOffset := Stream.Position;
    H := 0;
    channel := 0;
    InputLineIndex:= 0;
    for LenOfLineIndex := 0 to FHeight * FChannelCount-1 do
    begin
      FInputLine[InputLineIndex].StreamOffset := CurOffset;
      if FLengthOfLine <> nil then
        FInputLine[InputLineIndex].Size := BEtoN(FLengthOfLine[LenOfLineIndex])*LenOfLineFactor else
        FInputLine[InputLineIndex].Size := FLineSize;
      inc(CurOffset, FInputLine[InputLineIndex].Size);
      inc(H);
      Inc(InputLineIndex, FChannelCount);
      if H = FHeight then
      begin
        H := 0;
        Inc(channel);
        InputLineIndex:= channel;
      end;
    end;

    InputLineIndex := 0;
    PrevOutputRow := -1;
    OutputRow := 0;
    OutputRowAdd := FOutputHeight div FHeight;
    OutputRowMod:= FHeight;
    OutputRowAccAdd := FOutputHeight mod FHeight;
    OutputRowAcc:= 0;

    For H := 0 to FHeight - 1 do
    begin
      if OutputRow > PrevOutputRow then
      begin
        for channel := 0 to FChannelCount-1 do
        begin
            Stream.Position := FInputLine[InputLineIndex].StreamOffset;
            ReadScanLine(Stream, FInputLine[InputLineIndex].Size, channel);
            Inc(InputLineIndex);
        end;
        For HOutput:= PrevOutputRow+1 to OutputRow do WriteScanLine(Img, HOutput);
        Progress(FPimage.psRunning, round((H+1)*99.0 / (FHeight * FChannelCount)), False, Rect(0,0,0,0), '', ContProgress);
        if not ContProgress then exit;
      end else inc(InputLineIndex, FChannelCount);

      PrevOutputRow:= OutputRow;
      Inc(OutputRow, OutputRowAdd);
      Inc(OutputRowAcc, OutputRowAccAdd);
      if OutputRowAcc> OutputRowMod then
      begin
        dec(OutputRowAcc, OutputRowMod);
        inc(OutputRow);
      end;
    end;
    Progress(FPimage.psRunning, 100, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;

   {$ifdef FPC_Debug_Image}
    WriteLn('TBGRAReaderPSD.InternalRead AAA1 ',Stream.position,' ',Stream.size);
    {$endif}
  finally
    FreeAndNil(FPalette);
    for channel := 0 to FChannelCount-1 do
      ReAllocMem(FScanLines[channel],0);
  end;
  Progress(FPimage.psEnding, 100, false, Rect(0,0,FWidth,FHeight), '', ContProgress);
end;

function TBGRAReaderPSD.ReadScanLine(Stream: TStream; AInputSize: PtrInt;
  AChannel: integer): boolean;
Var
  P : PByte;
  B : Byte;
  I, left : PtrInt;
  N : Shortint;
  Count:integer;
  buf, PBuf: PByte;
begin
  Result:=false;
  If Not Compressed then
    Stream.ReadBuffer(FScanLines[AChannel]^,FLineSize)
  else
    begin
      getmem(buf, AInputSize);
      if stream.Read(buf^,AInputSize) <> AInputSize then
      begin
        freemem(buf);
        result := false;
        exit;
      end;
      P:=FScanLines[AChannel];
      left := FLineSize;
      i:=AInputSize;
      PBuf := buf;
      repeat
        Count:=0;
        N:= PShortInt(PBuf)^;
        inc(PBuf);
        dec(i);
        If N = -128 then
        else
        if N < 0 then
        begin
           Count:=-N+1;
           if Count > left then Count := left;
           dec(left,Count);
           B:= PBuf^;
           Inc(PBuf);
           dec(i);
           fillchar(p^,count,B);
           inc(p,count);
        end
        else
        begin
           Count:=N+1;
           if Count > left then Count := left;
           dec(left,Count);
           Move(PBuf^, P^, Count);
           Inc(PBuf, Count);
           inc(p, count);
           dec(i, count);
        end;
      until (i <= 0) or (left <= 0);
      freemem(buf);
    end;
  Result:=true;
end;

function Value32To16(p: PLongWord; gamma: single): Word;
var v: single;
begin
  v := (BEtoN(P^) - 1024000000)/40960000;
  if v <= 0 then result := 0 else
    if v >= 1 then result := 65535 else
      result := round(exp(ln(v)*gamma)*65535);
end;

procedure TBGRAReaderPSD.WriteScanLine(Img: TFPCustomImage; Row: integer);
Var
  Col : Integer;
  C   : TFPColor;
  P, P1, P2, P3   : PByte;
  Lab : TLab;
begin
  C.Alpha:=AlphaOpaque;
  P:=FScanLines[0];
  begin
    case FBytesPerPixel of
      1 : begin
             for Col:=0 to Img.Width-1 do
               if (P[col div 8] and (128 shr (col mod 8))) <> 0 then
                 Img.Colors[Col,Row]:=ThePalette[0]
  	       else
                 Img.Colors[Col,Row]:=ThePalette[1];
           end;
      8 : begin
             for Col:=0 to Img.Width-1 do
             begin
               Img.Colors[Col,Row]:=ThePalette[P[0]];
               inc(p);
             end;
          end;
      16 : if (FHeader.Mode = 1) or (FHeader.Mode = 8) then
           begin
             if FChannelCount = 1 then
               for Col:=0 to Img.Width-1 do
               begin
                 C.Red:=BEtoN(PWord(P)^);
                 C.green:=C.Red;
                 C.blue:=C.Red;
                 C.alpha:=65535;
                 Img[col, row] := C;
                 Inc(P,2);
               end else
             if FChannelCount = 2 then
             begin
               P1:=FScanLines[1];
               for Col:=0 to Img.Width-1 do
               begin
                 C.Red:=P^ shl 8 + P^;
                 C.green:=C.Red;
                 C.blue:=C.Red;
                 C.alpha:=p1^ shl 8 + P1^;
                 Img[col, row] := C;
                 Inc(P);
                 Inc(P1);
               end;
             end;
           end else
           begin
             for Col:=0 to Img.Width-1 do
             begin
               Img.Colors[Col,Row]:=ThePalette[BEtoN(PWord(P)^)];
               inc(p,2);
             end;
          end;
      24 : if FChannelCount>=3 then
           begin
           P1:=FScanLines[1];
           P2:=FScanLines[2];
           for Col:=0 to Img.Width-1 do
           begin
             if (FHeader.Mode =9) then
             begin
               Lab.L:=P[0];
               Lab.a:=P1[0];
               Lab.b:=P2[0];
               C:=LabToRGB(Lab);
             end
             else
              With C do
              begin
                Red:=P[0] or (P[0] shl 8);
                green:=P1[0] or (P1[0] shl 8);
                blue:=P2[0] or (P2[0] shl 8);
                alpha:=alphaOpaque;
              end;
              Inc(P);
              Inc(P1);
              Inc(P2);
              Img[col, row] := C;
           end;
          end;
      32 : if (FHeader.Mode = 1) or (FHeader.Mode = 8) then
           begin
             if FChannelCount = 1 then
               for Col:=0 to Img.Width-1 do
               begin
                 C.Red:=Value32To16(PLongWord(P),1.3);
                 C.green:=C.Red;
                 C.blue:=C.Red;
                 C.alpha:=65535;
                 Img[col, row] := C;
                 Inc(P,4);
               end else
             if FChannelCount = 2 then
             begin
               P1:=FScanLines[1];
               for Col:=0 to Img.Width-1 do
               begin
                 C.Red:=BEtoN(PWord(P)^);
                 C.green:=C.Red;
                 C.blue:=C.Red;
                 C.alpha:=BEtoN(PWord(p1)^);
                 Img[col, row] := C;
                 Inc(P,2);
                 Inc(P1,2);
               end;
             end;
           end else
           if FChannelCount >= 4 then
           begin
           P1:=FScanLines[1];
           P2:=FScanLines[2];
           P3:=FScanLines[3];
           for Col:=0 to Img.Width-1 do
           begin
             if (FHeader.Mode =4) then
             begin
                 P^ := 255 - P^;
                 P1^ := 255 - P1^;
                 P2^ := 255 - P2^;
                 P3^ := 255 - P3^;
             end;
             C.Red:=P[0] or (P[0] shl 8);
             C.green:=P1[0] or (P1[0] shl 8);
             C.blue:=P2[0] or (P2[0] shl 8);
             C.alpha:=P3[0] or (P3[0] shl 8);
             if (FHeader.Mode =4) then  C:=CMYKtoRGB(C); // CMYK to RGB
             Img[col, row] := C;
             Inc(P);
             Inc(P1);
             Inc(P2);
             Inc(P3);
           end;
          end;
      48 :if FChannelCount = 3 then
          begin
           P1:=FScanLines[1];
           P2:=FScanLines[2];
           C.alpha:=alphaOpaque;
           for Col:=0 to Img.Width-1 do
           begin
              if (FHeader.Mode =9) then
                C := LabToRGB(BEtoN(PWord(P)^)/65535, (BEtoN(PWord(P1)^)-32768)/32767, (BEtoN(PWord(P2)^)-32768)/32767)
              else
              With C do
              begin
                Red:=BEtoN(PWord(P)^);
                green:=BEtoN(PWord(P1)^);
                blue:=BEtoN(PWord(P2)^);
              end;
              Inc(P,2);
              Inc(P1,2);
              Inc(P2,2);
              Img[col, row] := C;
           end;
          end;
      64 : if FChannelCount = 4 then
           begin
           P1:=FScanLines[1];
           P2:=FScanLines[2];
           P3:=FScanLines[3];
           for Col:=0 to Img.Width-1 do
           begin
             C.Red:=BEtoN(PWord(P)^);
             C.green:=BEtoN(PWord(P1)^);
             C.blue:=BEtoN(PWord(P2)^);
             C.alpha:=BEtoN(PWord(P3)^);
             if (FHeader.Mode =4) then
             begin
                 C.red:=$ffff-C.red;
                 C.green:=$ffff-C.green;
                 C.blue:=$ffff-C.blue;
                 C.alpha:=$ffff-C.alpha;
             end;
             if (FHeader.Mode =4) then  C:=CMYKtoRGB(C); // CMYK to RGB
             Img[col, row] := C;
             Inc(P,2);
             Inc(P1,2);
             Inc(P2,2);
             Inc(P3,2);
           end;
          end;
      96 :if FChannelCount = 3 then
          begin
           P1:=FScanLines[1];
           P2:=FScanLines[2];
           C.alpha:=alphaOpaque;
           for Col:=0 to Img.Width-1 do
           begin
              With C do
              begin
                Red:=Value32To16(PLongWord(P),2.7);
                green:=Value32To16(PLongWord(P1),2.7);
                blue:=Value32To16(PLongWord(P2),2.7);
              end;
              Inc(P,4);
              Inc(P1,4);
              Inc(P2,4);
              Img[col, row] := C;
           end;
          end;
    end;
  end;
end;

function TBGRAReaderPSD.InternalCheck(Stream: TStream): boolean;
var
  OldPos: Int64;
begin
  try
    OldPos:=Stream.Position;
    Stream.Read(FHeader,SizeOf(FHeader));
    Result:=(FHeader.Signature = '8BPS');
    Stream.Position:=OldPos;
  except
    Result:=False;
  end;
end;

constructor TBGRAReaderPSD.Create;
begin
  inherited Create;
end;

initialization

  DefaultBGRAImageReader[ifPsd] := TBGRAReaderPSD;

end.
