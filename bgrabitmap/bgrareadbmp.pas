// SPDX-License-Identifier: LGPL-3.0-linking-exception
{*****************************************************************************}
{
    This original file was part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    BMP reader implementation modified by circular.
}
{*****************************************************************************}
{ 08/2005 by Giulio Bernardi:
   - Added support for 16 and 15 bpp bitmaps.
   - If we have bpp <= 8 make an indexed image instead of converting it to RGB
   - Support for RLE4 and RLE8 decoding
   - Support for top-down bitmaps

  03/2014 by circular:
   - RLE optimisation using a read buffer
   - direct access to pixels with TBGRABitmap
   - vertical shrink option with MinifyHeight,WantedHeight,OutputHeight (useful for thumbnails)
  01/2017 by circular:
   - support for OS/2 1.x format
   - support for headerless files
}

{$mode objfpc}
{$h+}

unit BGRAReadBMP;

interface

uses FPImage, BGRAClasses, SysUtils, BMPcomn, BGRABitmapTypes;

type
  TBMPTransparencyOption = (toAuto, toTransparent, toOpaque);
  TBitMapInfoHeader = BMPcomn.TBitMapInfoHeader;
  TBitMapFileHeader = BMPcomn.TBitMapFileHeader;
  TOS2BitmapHeader = packed record
    bcSize: LongWord;
    bcWidth: Word;
    bcHeight: Word;
    bcPlanes: Word;
    bcBitCount: Word;
  end;
  TMinimumBitmapHeader = packed record
    Size:longint;
    Width:longint;
    Height:longint;
    Planes:word;
    BitCount:word;
  end;
  TBitmapSubFormat = (bsfWithFileHeader, bsfHeaderless, bsfHeaderlessWithMask);
  TReadScanlineProc = procedure(Row : Integer; Stream : TStream) of object;
  TWriteScanlineProc = procedure(Row : Integer; Img : TFPCustomImage) of object;
  TProgressProc = procedure(Percent: integer; var ShouldContinue: boolean) of object;


  { TBGRAReaderBMP }

  TBGRAReaderBMP = class (TBGRAImageReader)
    Private
      DeltaX, DeltaY : integer; // Used for the never-used delta option in RLE
      TopDown : boolean;        // If set, bitmap is stored top down instead of bottom up
      Procedure FreeBufs;       // Free (and nil) buffers.
    protected
      ReadSize : Integer;       // Size (in bytes) of 1 scanline.
      BFH: TBitMapFileHeader;    // The file header
      BFI: TBitMapInfoHeader;  // The header as read from the stream.
      FPaletteEntrySize: integer;  // 4 for Windows, 3 for OS/2 1.x
      FPalette : PFPcolor;      // Buffer with Palette entries. (useless now)
      FBGRAPalette : PBGRAPixel;
      LineBuf : PByte;          // Buffer for 1 scanline. Can be Byte, Word, TColorRGB or TColorRGBA
      RedMask, GreenMask, BlueMask : LongWord; //Used if Compression=bi_bitfields
      RedShift, GreenShift, BlueShift : shortint;
      FOutputHeight: integer;
      FOriginalHeight: Integer;
      FTransparencyOption: TBMPTransparencyOption;
      FBuffer: packed array of byte;
      FBufferPos, FBufferSize: integer;
      FBufferStream: TStream;
      FHasAlphaValues: boolean;
      FMaskData: PByte;
      FMaskDataSize: integer;
      // SetupRead will allocate the needed buffers, and read the colormap if needed.
      procedure SetupRead(nPalette, nRowBits: Integer; Stream : TStream); virtual;
      function CountBits(Value : byte) : shortint;
      function ShiftCount(Mask : LongWord) : shortint;
      function ExpandColor(value : LongWord) : TFPColor;
      function ExpandColorBGRA(value : LongWord) : TBGRAPixel;
      procedure ExpandRLE8ScanLine(Row : Integer; Stream : TStream);
      procedure ExpandRLE4ScanLine(Row : Integer; Stream : TStream);
      procedure ReadScanLine(Row : Integer; Stream : TStream); virtual;
      procedure SkipScanLine(Row : Integer; Stream : TStream); virtual;
      procedure WriteScanLine(Row : Integer; Img : TFPCustomImage); virtual;
      procedure WriteScanLineBGRA(Row : Integer; Img : TFPCustomImage); virtual;
      procedure ReadMaskLine({%H-}Row : Integer; Stream : TStream); virtual;
      procedure SkipMaskLine({%H-}Row : Integer; Stream : TStream); virtual;
      procedure WriteMaskLine(Row : Integer; Img : TFPCustomImage); virtual;
      // required by TFPCustomImageReader
      procedure InternalRead  (Stream:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Stream:TStream) : boolean; override;
      procedure InitReadBuffer(AStream: TStream; ASize: integer);
      procedure CloseReadBuffer;
      function GetNextBufferByte: byte;
      procedure MakeOpaque(Img: TFPCustomImage);
      procedure LoadMask(Stream:TStream; Img:TFPCustomImage; var ShouldContinue: boolean);
      procedure MainProgressProc(Percent: integer; var ShouldContinue: boolean);
      procedure ImageVerticalLoop(Stream:TStream; Img:TFPCustomImage;
        ReadProc, SkipProc: TReadScanlineProc; WriteProc: TWriteScanlineProc;
        ProgressProc: TProgressProc; var ShouldContinue: boolean);
    public
      MinifyHeight,WantedHeight: integer;
      Hotspot: TPoint;
      Subformat: TBitmapSubFormat;
      constructor Create; override;
      destructor Destroy; override;
      property OriginalHeight: integer read FOriginalHeight;
      property OutputHeight: integer read FOutputHeight;
      property TransparencyOption: TBMPTransparencyOption read FTransparencyOption write FTransparencyOption;
      function GetQuickInfo(AStream: TStream): TQuickImageInfo; override;
      function GetBitmapDraft(AStream: TStream; {%H-}AMaxWidth, AMaxHeight: integer; out AOriginalWidth,AOriginalHeight: integer): TBGRACustomBitmap; override;
  end;

function MakeBitmapFileHeader(AData: TStream): TBitMapFileHeader;

implementation

uses math;

function MakeBitmapFileHeader(AData: TStream): TBitMapFileHeader;
var header: PBitMapInfoHeader;
  headerSize: integer;
  extraSize: integer;
  os2header: TOS2BitmapHeader;
begin
  AData.Position := 0;
  headerSize := LEtoN(AData.ReadDWord);
  if headerSize = sizeof(TOS2BitmapHeader) then //OS2 1.x
  begin
    AData.ReadBuffer({%H-}os2header,sizeof(os2header));
    if LEtoN(os2header.bcBitCount) in [1,2,4,8] then
    begin
      extraSize := 3*(1 shl LEtoN(os2header.bcBitCount));
    end else
      extraSize := 0;
    result.bfType:= Word('BM');
    result.bfSize := NtoLE(Integer(sizeof(TBitMapFileHeader) + AData.Size));
    result.bfReserved:= 0;
    result.bfOffset := NtoLE(Integer(sizeof(TBitMapFileHeader) + headerSize + extraSize));
  end else
  begin
    if (headerSize < 16) or (headerSize > AData.Size) or (headerSize > 1024) then
      raise exception.Create('Invalid header size');
    getmem(header, headerSize);
    try
      fillchar(header^, headerSize,0);
      header^.Size := NtoLE(headerSize);
      AData.ReadBuffer((PByte(header)+4)^, headerSize-4);
      if LEtoN(header^.Compression) = BI_BITFIELDS then
        extraSize := 4*3
      else if LEtoN(header^.BitCount) in [1,2,4,8] then
      begin
        if header^.ClrUsed > 0 then
          extraSize := 4*header^.ClrUsed
        else
          extraSize := 4*(1 shl header^.BitCount);
      end else
        extraSize := 0;
      result.bfType:= Word('BM');
      result.bfSize := NtoLE(Integer(sizeof(TBitMapFileHeader) + AData.Size));
      result.bfReserved:= 0;
      result.bfOffset := NtoLE(Integer(sizeof(TBitMapFileHeader) + headerSize + extraSize));
    finally
      freemem(header);
    end;
  end;
end;

function RGBAToFPColor(Const RGBA: TColorRGBA) : TFPcolor;
begin
  with Result, RGBA do
    begin
    Red   :=(R shl 8) or R;
    Green :=(G shl 8) or G;
    Blue  :=(B shl 8) or B;
    Alpha :=(A shl 8) or A
    end;
end;

Function RGBToFPColor(Const RGB : TColorRGB) : TFPColor;

begin
  with Result,RGB do
    begin  {Use only the high byte to convert the color}
    Red   := (R shl 8) + R;
    Green := (G shl 8) + G;
    Blue  := (B shl 8) + B;
    Alpha := AlphaOpaque;
    end;
end;

constructor TBGRAReaderBMP.Create;

begin
  inherited create;
  FTransparencyOption := toTransparent;
  Subformat:= bsfWithFileHeader;
end;

destructor TBGRAReaderBMP.Destroy;

begin
  FreeBufs;
  inherited destroy;
end;

function TBGRAReaderBMP.GetQuickInfo(AStream: TStream): TQuickImageInfo;
var headerSize: LongWord;
  os2header: TOS2BitmapHeader;
  minHeader: TMinimumBitmapHeader;
  totalDepth: integer;
  headerPos: int64;
begin
  {$PUSH}{$HINTS OFF}fillchar({%H-}result, sizeof({%H-}result), 0);{$POP}
  headerPos := AStream.Position;
  if AStream.Read({%H-}headerSize, sizeof(headerSize)) <> sizeof(headerSize) then exit;
  headerSize := LEtoN(headerSize);

  //check presence of file header
  if (headerSize and $ffff) = BMmagic then
  begin
    inc(headerPos, sizeof(TBitMapFileHeader));
    AStream.Position := headerPos;
    if AStream.Read(headerSize, sizeof(headerSize)) <> sizeof(headerSize) then exit;
    headerSize := LEtoN(headerSize);
  end;

  AStream.Position := headerPos;

  if headerSize = sizeof(TOS2BitmapHeader) then //OS2 1.x
  begin
    if AStream.Read({%H-}os2header, sizeof(os2header)) <> sizeof(os2header) then exit;
    result.width := LEtoN(os2header.bcWidth);
    result.height := LEtoN(os2header.bcHeight);
    result.colorDepth := LEtoN(os2header.bcBitCount);
    result.alphaDepth := 0;
  end
  else
  if headerSize >= sizeof(minHeader) then
  begin
    if AStream.Read({%H-}minHeader, sizeof(minHeader)) <> sizeof(minHeader) then exit;
    result.width := LEtoN(minHeader.Width);
    result.height := LEtoN(minHeader.Height);
    totalDepth := LEtoN(minHeader.BitCount);
    if totalDepth > 24 then
    begin
      result.colorDepth:= 24;
      result.alphaDepth:= 8;
    end else
    begin
      result.colorDepth := totalDepth;
      result.alphaDepth:= 0;
    end;
  end else
  begin
    result.width := 0;
    result.height:= 0;
    result.colorDepth:= 0;
    result.alphaDepth:= 0;
  end;
end;

function TBGRAReaderBMP.GetBitmapDraft(AStream: TStream; AMaxWidth,
  AMaxHeight: integer; out AOriginalWidth, AOriginalHeight: integer): TBGRACustomBitmap;
var
  bmpFormat: TBGRAReaderBMP;
  prevStreamPos: Int64;
begin
  bmpFormat:= TBGRAReaderBMP.Create;
  bmpFormat.Subformat:= Subformat;
  bmpFormat.MinifyHeight := AMaxHeight*2;
  result := BGRABitmapFactory.Create;
  prevStreamPos := AStream.Position;
  try
    result.LoadFromStream(AStream, bmpFormat);
    AOriginalWidth:= result.Width;
    AOriginalHeight:= bmpFormat.OriginalHeight;
  finally
    bmpFormat.Free;
    AStream.Position := prevStreamPos;
  end;
end;

procedure TBGRAReaderBMP.FreeBufs;
begin
  If (LineBuf<>Nil) then
    begin
    FreeMem(LineBuf);
    LineBuf:=Nil;
    end;
  If (FPalette<>Nil) then
    begin
    FreeMem(FPalette);
    FPalette:=Nil;
    end;
  If (FBGRAPalette<>Nil) then
    begin
    FreeMem(FBGRAPalette);
    FBGRAPalette:=Nil;
    end;
end;

{ Counts how many bits are set }
function TBGRAReaderBMP.CountBits(Value : byte) : shortint;
var i,bits : shortint;
begin
  bits:=0;
  for i:=0 to 7 do
  begin
    if (value mod 2)<>0 then inc(bits);
    value:=value shr 1;
  end;
  Result:=bits;
end;

{ If compression is bi_bitfields, there could be arbitrary masks for colors.
  Although this is not compatible with windows9x it's better to know how to read these bitmaps
  We must determine how to switch the value once masked
  Example: 0000 0111 1110 0000, if we shr 5 we have 00XX XXXX for the color, but these bits must be the
  highest in the color, so we must shr (5-(8-6))=3, and we have XXXX XX00.
  A negative value means "shift left"  }
function TBGRAReaderBMP.ShiftCount(Mask : LongWord) : shortint;
var tmp : shortint;
begin
  tmp:=0;
  if Mask=0 then
  begin
    Result:=0;
    exit;
  end;

  while (Mask mod 2)=0 do { rightmost bit is 0 }
  begin
    inc(tmp);
    Mask:= Mask shr 1;
  end;
  tmp:=tmp-(8-CountBits(Mask and $FF));
  Result:=tmp;
end;

function TBGRAReaderBMP.ExpandColor(value : LongWord) : TFPColor;
var tmpr, tmpg, tmpb : LongWord;
    col : TColorRGB;
begin
  {$IFDEF ENDIAN_BIG}
  value:=swap(value);
  {$ENDIF}
  tmpr:=value and RedMask;
  tmpg:=value and GreenMask;
  tmpb:=value and BlueMask;
  if RedShift < 0 then col.R:=byte(tmpr shl (-RedShift))
  else col.R:=byte(tmpr shr RedShift);
  if GreenShift < 0 then col.G:=byte(tmpg shl (-GreenShift))
  else col.G:=byte(tmpg shr GreenShift);
  if BlueShift < 0 then col.B:=byte(tmpb shl (-BlueShift))
  else col.B:=byte(tmpb shr BlueShift);
  Result:=RGBToFPColor(col);
end;

function TBGRAReaderBMP.ExpandColorBGRA(value: LongWord): TBGRAPixel;
var tmpr, tmpg, tmpb : LongWord;
begin
  {$IFDEF ENDIAN_BIG}
  value:=swap(value);
  {$ENDIF}
  tmpr:=value and RedMask;
  tmpg:=value and GreenMask;
  tmpb:=value and BlueMask;
  if RedShift < 0 then result.red:=byte(tmpr shl (-RedShift))
  else result.red:=byte(tmpr shr RedShift);
  if GreenShift < 0 then result.green:=byte(tmpg shl (-GreenShift))
  else result.green:=byte(tmpg shr GreenShift);
  if BlueShift < 0 then result.blue:=byte(tmpb shl (-BlueShift))
  else result.blue:=byte(tmpb shr BlueShift);
  result.alpha:= 255;
end;

procedure TBGRAReaderBMP.SetupRead(nPalette, nRowBits: Integer; Stream : TStream);

var
  ColInfo: ARRAY OF TColorRGBA;
  ColInfo3: packed array of TColorRGB;
  i,colorPresent: Integer;

begin
  if ((BFI.Compression=BI_RGB) and (BFI.BitCount=16)) then { 5 bits per channel, fixed mask }
  begin
    RedMask:=$7C00; RedShift:=7;
    GreenMask:=$03E0; GreenShift:=2;
    BlueMask:=$001F; BlueShift:=-3;
  end
  else if ((BFI.Compression=BI_BITFIELDS) and (BFI.BitCount in [16,32])) then { arbitrary mask }
  begin
    Stream.Read(RedMask,4);
    Stream.Read(GreenMask,4);
    Stream.Read(BlueMask,4);
    {$IFDEF ENDIAN_BIG}
    RedMask:=swap(RedMask);
    GreenMask:=swap(GreenMask);
    BlueMask:=swap(BlueMask);
    {$ENDIF}
    RedShift:=ShiftCount(RedMask);
    GreenShift:=ShiftCount(GreenMask);
    BlueShift:=ShiftCount(BlueMask);
  end
  else if nPalette>0 then
  begin
    GetMem(FPalette, nPalette*SizeOf(TFPColor));
    GetMem(FBGRAPalette, nPalette*SizeOf(TBGRAPixel));
    SetLength(ColInfo, nPalette);
    if BFI.ClrUsed>0 then
      colorPresent:= min(BFI.ClrUsed,nPalette)
    else
      colorPresent:= nPalette;
    if FPaletteEntrySize = 3 then
    begin
      setlength(ColInfo3, nPalette);
      Stream.Read(ColInfo3[0],colorPresent*SizeOf(TColorRGB));
      for i := 0 to colorPresent-1 do
        ColInfo[i].RGB := ColInfo3[i];
    end
    else
    begin
      Stream.Read(ColInfo[0],colorPresent*SizeOf(TColorRGBA));
    end;
    for i := 0 to High(ColInfo) do
    begin
      FPalette[i] := RGBToFPColor(ColInfo[i].RGB);
      FBGRAPalette[i]:= FPColorToBGRA(FPalette[i]);
    end
  end
  else if BFI.ClrUsed>0 then { Skip palette }
    {$PUSH}{$HINTS OFF}
    Stream.Position := Stream.Position + BFI.ClrUsed*SizeOf(TColorRGBA);
    {$POP}
  ReadSize:=((nRowBits + 31) div 32) shl 2;
  GetMem(LineBuf,ReadSize);
end;

procedure TBGRAReaderBMP.InternalRead(Stream:TStream; Img:TFPCustomImage);

Var
  i, pallen : Integer;
  BadCompression : boolean;
  WriteScanlineProc: TWriteScanlineProc;
  headerSize: LongWord;
  os2header: TOS2BitmapHeader;
  shouldContinue: boolean;

begin
  shouldContinue:=true;
  Progress(psStarting,0,false,EmptyRect,'',shouldContinue);
  if not shouldContinue then exit;

  headerSize := LEtoN(Stream.ReadDWord);
  fillchar({%H-}BFI,SizeOf(BFI),0);
  if headerSize = sizeof(TOS2BitmapHeader) then
  begin
    fillchar({%H-}os2header,SizeOf(os2header),0);
    Stream.Read(os2header.bcWidth,min(SizeOf(os2header),headerSize)-sizeof(LongWord));
    BFI.Size := 16;
    BFI.Width := LEtoN(os2header.bcWidth);
    BFI.Height := LEtoN(os2header.bcHeight);
    BFI.Planes := LEtoN(os2header.bcPlanes);
    BFI.BitCount := LEtoN(os2header.bcBitCount);
    FPaletteEntrySize:= 3;
  end else
  begin
    Stream.Read(BFI.Width,min(SizeOf(BFI),headerSize)-sizeof(LongWord));
    {$IFDEF ENDIAN_BIG}
    SwapBMPInfoHeader(BFI);
    {$ENDIF}
    BFI.Size := headerSize;
    FPaletteEntrySize:= 4;
  end;
  { This will move past any junk after the BFI header }
  Stream.Position:=Stream.Position-SizeOf(BFI)+BFI.Size;
  with BFI do
  begin
    BadCompression:=false;
    if ((Compression=BI_RLE4) and (BitCount<>4)) then BadCompression:=true;
    if ((Compression=BI_RLE8) and (BitCount<>8)) then BadCompression:=true;
    if ((Compression=BI_BITFIELDS) and (not (BitCount in [16,32]))) then BadCompression:=true;
    if not (Compression in [BI_RGB..BI_BITFIELDS]) then BadCompression:=true;
    if BadCompression then
      raise FPImageException.Create('Bad BMP compression mode');
    TopDown:=(Height<0);
    Height:=abs(Height);
    FOriginalHeight := Height;
    if (TopDown and (not (Compression in [BI_RGB,BI_BITFIELDS]))) then
      raise FPImageException.Create('Top-down bitmaps cannot be compressed');
    Img.SetSize(0,0);
    if BitCount<=8 then
    begin
      Img.UsePalette:=true;
      Img.Palette.Clear;
    end
    else Img.UsePalette:=false;
    Case BFI.BitCount of
      1 : { Monochrome }
        SetupRead(2,Width,Stream);
      4 :
        SetupRead(16,Width*4,Stream);
      8 :
        SetupRead(256,Width*8,Stream);
      16 :
        SetupRead(0,Width*8*2,Stream);
      24:
        SetupRead(0,Width*8*3,Stream);
      32:
        SetupRead(0,Width*8*4,Stream);
    else raise exception.Create('Invalid bit depth ('+inttostr(BFI.BitCount)+')');
    end;
  end;
  if Subformat = bsfHeaderlessWithMask then BFI.Height := BFI.Height div 2;
  Try
    { Note: it would be better to Fill the image palette in setupread instead of creating FPalette.
      FPalette is indeed useless but we cannot remove it since it's not private :\ }
    pallen:=0;
    if BFI.BitCount<=8 then
      if BFI.ClrUsed>0 then pallen:=BFI.ClrUsed
      else pallen:=(1 shl BFI.BitCount);
    if pallen>0 then
    begin
      if FPalette = nil then raise exception.Create('Internal error: palette object not initialized');
      Img.Palette.Count:=pallen;
      for i:=0 to pallen-1 do
        Img.Palette.Color[i]:=FPalette[i];
    end;
    if (MinifyHeight > 0) and (MinifyHeight < BFI.Height) then FOutputHeight:= MinifyHeight else
    if WantedHeight > 0 then FOutputHeight:= WantedHeight else
      FOutputHeight:= BFI.Height;

    if (BFI.Compression=BI_RLE8) or(BFI.Compression=BI_RLE4) then InitReadBuffer(Stream,2048);
    FHasAlphaValues:= false;

    Img.SetSize(BFI.Width,FOutputHeight);

    if Img is TBGRACustomBitmap then
      WriteScanlineProc := @WriteScanLineBGRA else
        WriteScanlineProc := @WriteScanLine;

    ImageVerticalLoop(Stream, Img, @ReadScanLine, @SkipScanLine, WriteScanlineProc,
                      @MainProgressProc, shouldContinue);

    if shouldContinue then
    begin
      if not FHasAlphaValues and (TransparencyOption = toAuto) and (BFI.BitCount = 32) then
        MakeOpaque(Img);
      if (BFI.Compression=BI_RLE8) or(BFI.Compression=BI_RLE4) then CloseReadBuffer;

      if Subformat = bsfHeaderlessWithMask then LoadMask(Stream,Img, shouldContinue);

      Progress(psEnding,100,false,EmptyRect,'',shouldContinue);
    end;

  finally
    FreeBufs;
  end;
end;

procedure TBGRAReaderBMP.ExpandRLE8ScanLine(Row : Integer; Stream : TStream);
var i,j,k : integer;
    b0, b1 : byte;
begin
  i:=0;
  while true do
  begin
    { let's see if we must skip pixels because of delta... }
    if DeltaY<>-1 then
    begin
      if Row=DeltaY then j:=DeltaX { If we are on the same line, skip till DeltaX }
      else j:=ReadSize;            { else skip up to the end of this line }
      while (i<j) do
        begin
          LineBuf[i]:=0;
          inc(i);
        end;

      if Row=DeltaY then { we don't need delta anymore }
        DeltaY:=-1
      else break; { skipping must continue on the next line, we are finished here }
    end;

    b0 := GetNextBufferByte; b1 := GetNextBufferByte;
    if b0<>0 then { number of repetitions }
    begin
      if b0+i>ReadSize then
        raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
      j:=i+b0;
      while (i<j) do
      begin
        LineBuf[i]:=b1;
        inc(i);
      end;
    end
    else
      case b1 of
        0: break; { end of line }
        1: break; { end of file }
        2: begin  { Next pixel position. Skipped pixels should be left untouched, but we set them to zero }
             b0 := GetNextBufferByte; b1 := GetNextBufferByte;
             DeltaX:=i+b0; DeltaY:=Row+b1;
           end
        else begin { absolute mode }
               if b1+i>ReadSize then
                 raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
               for k := b1-1 downto 0 do
               Begin
                 LineBuf[i] := GetNextBufferByte;
                 Inc(i);
               end;
               { aligned on 2 bytes boundary: every group starts on a 2 bytes boundary, but absolute group
                 could end on odd address if there is a odd number of elements, so we pad it  }
               if (b1 mod 2)<>0 then GetNextBufferByte;
             end;
      end;
  end;
end;

procedure TBGRAReaderBMP.ExpandRLE4ScanLine(Row : Integer; Stream : TStream);
var i,j,tmpsize : integer;
    b0, b1 : byte;
    nibline : pbyte; { temporary array of nibbles }
    even : boolean;
begin
  tmpsize:=ReadSize*2; { ReadSize is in bytes, while nibline is made of nibbles, so it's 2*readsize long }
  getmem(nibline,tmpsize);
  if nibline=nil then
    raise FPImageException.Create('Out of memory');
  try
    i:=0;
    while true do
    begin
      { let's see if we must skip pixels because of delta... }
      if DeltaY<>-1 then
      begin
        if Row=DeltaY then j:=DeltaX { If we are on the same line, skip till DeltaX }
        else j:=tmpsize;            { else skip up to the end of this line }
        while (i<j) do
          begin
            NibLine[i]:=0;
            inc(i);
          end;

        if Row=DeltaY then { we don't need delta anymore }
          DeltaY:=-1
        else break; { skipping must continue on the next line, we are finished here }
      end;

      b0 := GetNextBufferByte; b1:= GetNextBufferByte;
      if b0<>0 then { number of repetitions }
      begin
        if b0+i>tmpsize then
          raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
        even:=true;
        j:=i+b0;
        while (i<j) do
        begin
          if even then NibLine[i]:=(b1 and $F0) shr 4
          else NibLine[i]:=b1 and $0F;
          inc(i);
          even:=not even;
        end;
      end
      else
        case b1 of
          0: break; { end of line }
          1: break; { end of file }
          2: begin  { Next pixel position. Skipped pixels should be left untouched, but we set them to zero }
               b0 := GetNextBufferByte; b1:= GetNextBufferByte;
               DeltaX:=i+b0; DeltaY:=Row+b1;
             end
          else begin { absolute mode }
                 if b1+i>tmpsize then
                   raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
                 j:=i+b1;
                 even:=true;
                 while (i<j) do
                 begin
                   if even then
                   begin
                     b0 := GetNextBufferByte;
                     NibLine[i]:=(b0 and $F0) shr 4;
                   end
                   else NibLine[i]:=b0 and $0F;
                   inc(i);
                   even:=not even;
                 end;
               { aligned on 2 bytes boundary: see rle8 for details  }
                 b1:=b1+(b1 mod 2);
                 if (b1 mod 4)<>0 then GetNextBufferByte;
               end;
        end;
    end;
    { pack the nibline into the linebuf }
    for i:=0 to ReadSize-1 do
      LineBuf[i]:=(NibLine[i*2] shl 4) or NibLine[i*2+1];
  finally
    FreeMem(nibline)
  end;
end;

procedure TBGRAReaderBMP.ReadScanLine(Row : Integer; Stream : TStream);
begin
  if BFI.Compression=BI_RLE8 then ExpandRLE8ScanLine(Row,Stream)
  else if BFI.Compression=BI_RLE4 then ExpandRLE4ScanLine(Row,Stream)
  else Stream.Read(LineBuf[0],ReadSize);
end;

procedure TBGRAReaderBMP.SkipScanLine(Row: Integer; Stream: TStream);
begin
  if (BFI.Compression=BI_RLE8) or(BFI.Compression=BI_RLE4) then ReadScanLine(Row,Stream)
  else Stream.Position := Stream.Position+ReadSize;
end;

procedure TBGRAReaderBMP.WriteScanLine(Row : Integer; Img : TFPCustomImage);

Var
  Column : Integer;
  c: TFPColor;
begin
  Case BFI.BitCount of
   1 :
     for Column:=0 to Img.Width-1 do
       if ((LineBuf[Column div 8] shr (7-(Column and 7)) ) and 1) <> 0 then
         img.Pixels[Column,Row]:=1
       else
         img.Pixels[Column,Row]:=0;
   4 :
      for Column:=0 to img.Width-1 do
        img.Pixels[Column,Row]:=(LineBuf[Column div 2] shr (((Column+1) and 1)*4)) and $0f;
   8 :
      for Column:=0 to img.Width-1 do
        img.Pixels[Column,Row]:=LineBuf[Column];
   16 :
      for Column:=0 to img.Width-1 do
        img.colors[Column,Row]:=ExpandColor(PWord(LineBuf)[Column]);
   24 :
      for Column:=0 to img.Width-1 do
        img.colors[Column,Row]:=RGBToFPColor(PColorRGB(LineBuf)[Column]);
   32 :
      for Column:=0 to img.Width-1 do
        if BFI.Compression=BI_BITFIELDS then
          img.colors[Column,Row]:=ExpandColor(PLongWord(LineBuf)[Column])
        else
        begin
          if FTransparencyOption = toOpaque then
            img.colors[Column,Row]:=RGBToFPColor(PColorRGB(PColorRGBA(LineBuf)+Column)^)
          else
          begin
            c := RGBAToFPColor(PColorRGBA(LineBuf)[Column]);
            if c.alpha <> 0 then FHasAlphaValues:= true;
            img.colors[Column,Row]:= c;
          end;
        end;
    end;
end;

procedure TBGRAReaderBMP.WriteScanLineBGRA(Row: Integer; Img: TFPCustomImage);

Var
  Column : Integer;
  PDest: PBGRAPixel;
  PSrc: PByte;
begin
  PDest := TBGRACustomBitmap(Img).ScanLine[Row];
  Case BFI.BitCount of
   1 :
     for Column:=0 to Img.Width-1 do
     begin
       if ((LineBuf[Column div 8] shr (7-(Column and 7)) ) and 1) <> 0 then
         PDest^ := FBGRAPalette[1]
       else
         PDest^ := FBGRAPalette[0];
       inc(PDest);
     end;
   4 :
      for Column:=0 to img.Width-1 do
      begin
        PDest^ := FBGRAPalette[(LineBuf[Column div 2] shr (((Column+1) and 1)*4)) and $0f];
        inc(PDest);
      end;
   8 :
      for Column:=0 to img.Width-1 do
      begin
        PDest^ := FBGRAPalette[LineBuf[Column]];
        inc(PDest);
      end;
   16 :
      for Column:=0 to img.Width-1 do
      begin
        PDest^ :=ExpandColorBGRA(PWord(LineBuf)[Column]);
        inc(PDest);
      end;
   24 : begin
      PSrc := LineBuf;
      for Column:=0 to img.Width-1 do
      begin
        PDest^ := BGRA((Psrc+2)^,(Psrc+1)^,(Psrc)^);
        inc(PDest);
        inc(PSrc,3);
      end;
   end;
   32 :
     if BFI.Compression=BI_BITFIELDS then
     begin
      for Column:=0 to img.Width-1 do
      begin
        PDest^:=ExpandColorBGRA(PLongWord(LineBuf)[Column]);
        inc(PDest);
      end;
     end else
     if FTransparencyOption = toOpaque then
     begin
       {$PUSH}{$WARNINGS OFF}
       if TBGRAPixel_RGBAOrder then
       begin
        PSrc := LineBuf;
        for Column:=0 to img.Width-1 do
        begin
          PDest^:= BGRA((PSrc)^,(PSrc+1)^,(PSrc+2)^);
          inc(PDest);
          Inc(PSrc,4);
        end;
       end
       else
       begin
        PSrc := LineBuf;
        for Column:=0 to img.Width-1 do
        begin
          PDest^:= BGRA((PSrc+2)^,(PSrc+1)^,(PSrc+1)^);
          inc(PDest);
          Inc(PSrc,4);
        end;
       end;
       {$POP}
     end else
     begin
       {$PUSH}{$WARNINGS OFF}
       if TBGRAPixel_RGBAOrder then
       begin
        PSrc := LineBuf;
        for Column:=0 to img.Width-1 do
        begin
          PDest^:= BGRA((PSrc+2)^,(PSrc+1)^,(PSrc)^,(PSrc+3)^);
          if PDest^.alpha <> 0 then FHasAlphaValues:= true;
          inc(PDest);
          Inc(PSrc,4);
        end;
       end
       else
       begin
         PSrc := LineBuf;
         for Column:=0 to img.Width-1 do
         begin
           PDest^ := PBGRAPixel(PSrc)^;
           if PDest^.alpha <> 0 then FHasAlphaValues:= true;
           inc(PDest);
           Inc(PSrc,4);
         end;
       end;
       {$POP}
     end;
    end;
end;

procedure TBGRAReaderBMP.ReadMaskLine(Row: Integer; Stream: TStream);
begin
  FillChar(FMaskData^, FMaskDataSize, 0);
  Stream.Read(FMaskData^, FMaskDataSize);
end;

procedure TBGRAReaderBMP.SkipMaskLine(Row: Integer; Stream: TStream);
begin
  Stream.Position := Stream.Position+FMaskDataSize;
end;

procedure TBGRAReaderBMP.WriteMaskLine(Row: Integer; Img: TFPCustomImage);
var x, maskPos: integer;
  bit: byte;
  bmp: TBGRACustomBitmap;
  pimg: PBGRAPixel;
begin
  if Img is TBGRACustomBitmap then
    bmp := TBGRACustomBitmap(Img)
  else
    exit;

  maskPos := 0;
  bit := $80;
  pimg := bmp.ScanLine[Row];
  for x := 0 to bmp.Width-1 do
  begin
    if (FMaskData[maskPos] and bit) <> 0 then //if AND mask is non zero, value is kept
    begin
      if pimg^.alpha = 255 then
      begin
        pimg^.alpha := 0;
        if LongWord(pimg^) <> 0 then
        begin
         bmp.NeedXorMask;
         bmp.XorMask.SetPixel(x,Row,pimg^);
        end;
      end;
    end;
    inc(pimg);
    bit := bit shr 1;
    if bit = 0 then
    begin
      bit := $80;
      inc(maskPos);
    end;
  end;
end;

function  TBGRAReaderBMP.InternalCheck (Stream:TStream) : boolean;
begin
  fillchar(BFH, sizeof(BFH), 0);
  if Subformat in [bsfHeaderless,bsfHeaderlessWithMask] then
  begin
   result := true;
   Hotspot := Point(0,0);
  end else
  begin
    if stream.Read(BFH,SizeOf(BFH)) <> sizeof(BFH) then
    begin
      result := false;
      exit;
    end;
    Hotspot := Point(LEtoN(PWord(@BFH.bfReserved)^),LEtoN((PWord(@BFH.bfReserved)+1)^));
    {$IFDEF ENDIAN_BIG}
    SwapBMPFileHeader(BFH);
    {$ENDIF}
    With BFH do
      Result:=(bfType=BMmagic); // Just check magic number
  end;
end;

procedure TBGRAReaderBMP.InitReadBuffer(AStream: TStream; ASize: integer);
begin
  setLength(FBuffer,ASize);
  FBufferSize := AStream.Read(FBuffer[0],ASize);
  FBufferPos := 0;
  FBufferStream := AStream;
end;

procedure TBGRAReaderBMP.CloseReadBuffer;
begin
  FBufferStream.Position:= FBufferStream.Position-FBufferSize+FBufferPos;
end;

function TBGRAReaderBMP.GetNextBufferByte: byte;
begin
  if FBufferPos < FBufferSize then
  begin
    result := FBuffer[FBufferPos];
    inc(FBufferPos);
  end else
  if FBufferSize = 0 then
    result := 0
  else
  begin
    FBufferSize := FBufferStream.Read(FBuffer[0],length(FBuffer));
    FBufferPos := 0;
    if FBufferPos < FBufferSize then
    begin
      result := FBuffer[FBufferPos];
      inc(FBufferPos);
    end else
      result := 0;
  end;
end;

procedure TBGRAReaderBMP.MakeOpaque(Img: TFPCustomImage);
var c: TFPColor;
  x,y: Int32or64;
begin
  if Img is TBGRACustomBitmap then
    TBGRACustomBitmap(Img).AlphaFill(255)
  else
    for y := 0 to Img.Height-1 do
      for x := 0 to Img.Width-1 do
      begin
        c := Img.Colors[x,y];
        c.alpha := alphaOpaque;
        Img.Colors[x,y] := c;
      end;
end;

procedure TBGRAReaderBMP.LoadMask(Stream: TStream; Img: TFPCustomImage; var ShouldContinue: boolean);
begin
  if Img is TBGRACustomBitmap then TBGRACustomBitmap(Img).DiscardXorMask;
  FMaskDataSize := ((Img.Width+31) div 32)*4; //padded to LongWord
  getmem(FMaskData, FMaskDataSize);
  try
    ImageVerticalLoop(Stream,Img, @ReadMaskLine, @SkipMaskLine, @WriteMaskLine, nil, ShouldContinue);
  finally
    freemem(FMaskData);
    FMaskData := nil;
    FMaskDataSize := 0;
  end;
end;

procedure TBGRAReaderBMP.MainProgressProc(Percent: integer;
  var ShouldContinue: boolean);
begin
  Progress(psRunning,Percent,false,EmptyRect,'',ShouldContinue);
end;

procedure TBGRAReaderBMP.ImageVerticalLoop(Stream: TStream;
  Img: TFPCustomImage; ReadProc, SkipProc: TReadScanlineProc;
  WriteProc: TWriteScanlineProc; ProgressProc: TProgressProc;
  var ShouldContinue: boolean);
var
  prevPercent, percent, percentAdd : byte;
  percentMod : LongWord;
  percentAcc, percentAccAdd : LongWord;
  PrevSourceRow,SourceRow, SourceRowDelta, SourceLastRow: integer;
  SourceRowAdd: integer;
  SourceRowAcc,SourceRowMod: integer;
  SourceRowAccAdd: integer;
  OutputLastRow, OutputRow, OutputRowDelta: integer;
begin
  if OutputHeight <= 0 then exit;

  percent:=0;
  percentMod:= OutputHeight;
  percentAdd := 100 div percentMod;
  percentAcc:= percentMod div 2;
  percentAccAdd := 100 mod percentMod;

  DeltaX:=-1; DeltaY:=-1;
  if TopDown then
  begin
    SourceRowDelta := 1;
    SourceRow := 0;
    SourceLastRow := BFI.Height-1;
  end else
  begin
    SourceRowDelta := -1;
    SourceRow := BFI.Height-1;
    SourceLastRow := 0;
  end;
  OutputRowDelta:= SourceRowDelta;

  SourceRowMod := OutputHeight;
  SourceRowAdd := (BFI.Height div SourceRowMod)*SourceRowDelta;
  SourceRowAcc := SourceRowMod div 2;
  SourceRowAccAdd := BFI.Height mod SourceRowMod;
  If TopDown then
  begin
    OutputRow := 0;
    OutputLastRow := OutputHeight-1;
  end
  else
  begin
    OutputRow := OutputHeight-1;
    OutputLastRow := 0;
  end;

  PrevSourceRow := SourceRow-SourceRowDelta;

  while ShouldContinue and (SourceRow <> SourceLastRow+SourceRowDelta) do
  begin
    while PrevSourceRow <> SourceRow do
    begin
      inc(PrevSourceRow, SourceRowDelta);
      if PrevSourceRow = SourceRow then
        ReadProc(PrevSourceRow,Stream)
      else
        SkipProc(PrevSourceRow,Stream);
    end;
    WriteProc(OutputRow,Img);
    if OutputRow = OutputLastRow then break;

    inc(OutputRow,OutputRowDelta);
    inc(SourceRow,SourceRowAdd);
    inc(SourceRowAcc,SourceRowAccAdd);
    if SourceRowAcc >= SourceRowMod then
    begin
     dec(SourceRowAcc,SourceRowMod);
     Inc(SourceRow,SourceRowDelta);
    end;

    prevPercent := percent;
    inc(percent,percentAdd);
    inc(percentAcc,percentAccAdd);
    if percentAcc>=percentMod then
    begin
      dec(percentAcc, percentMod);
      inc(percent);
    end;
    if (percent<>prevPercent) and Assigned(ProgressProc) then ProgressProc(percent, ShouldContinue);
  end;
end;

initialization

  DefaultBGRAImageReader[ifBmp] := TBGRAReaderBMP;

end.
