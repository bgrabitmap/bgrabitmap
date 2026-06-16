// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ @abstract(Provides reader for GIF format.)

  This unit implements some optimisations of TFPReaderGif:
  decompression algorithm and direct pixel access of TBGRABitmap.

  Note: to read an animation use TBGRAAnimatedGif instead. }

unit BGRAReadGif;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPimage, FPReadGif;

type
  PGifRGB = ^TGifRGB;

  { @abstract(Reader for GIF still image format.)

    For animations, use TBGRAAnimatedGif class. }
  TBGRAReaderGif = class(TFPReaderGif)
  protected
    procedure ReadPaletteAtOnce(Stream: TStream; Size: integer);
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    class function InternalSize(Str: TStream): TPoint; override;
    class function ReadHeader(Str: TStream; out AHeader: TGIFHeader;
      out AColorTableSize: integer): boolean;
    class function ReadDescriptor(Str: TStream; out ADescriptor: TGifImageDescriptor;
      out AColorTableSize: integer): boolean;
    class function IgnoreExtensionBlock(Str: TStream): byte;
    function ReadScanLine(Stream: TStream): boolean; override;
    function WriteScanLineBGRA(Img: TFPCustomImage): Boolean; virtual;
  end;

implementation

uses BGRABitmapTypes;

{ TBGRAReaderGif }

procedure TBGRAReaderGif.ReadPaletteAtOnce(Stream: TStream; Size: integer);
Var
  RGBEntries, RGBEntry : PGifRGB;
  I : Integer;
  c : TFPColor;
begin
  FPalette.count := 0;
  getmem(RGBEntries, sizeof(TGifRGB)*Size);
  Stream.Read(RGBEntries^, sizeof(TGifRGB)*Size);
  For I:=0 To Size-1 Do
  Begin
    RGBEntry := RGBEntries+I;
    With c do
    begin
      Red:=RGBEntry^.Red or (RGBEntry^.Red shl 8);
      Green:=RGBEntry^.Green or (RGBEntry^.Green shl 8);
      Blue:=RGBEntry^.Blue or (RGBEntry^.Blue shl 8);
      Alpha:=alphaOpaque;
    end;
    FPalette.Add(C);
  End;
  FreeMem(RGBEntries);
end;

procedure TBGRAReaderGif.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  Introducer:byte;
  ColorTableSize :Integer;
  ContProgress: Boolean;
begin
  FPalette:=nil;
  FScanLine:=nil;
  try
    ContProgress:=true;
    Progress(psStarting, 0, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;

    FPalette := TFPPalette.Create(0);

    Stream.Position:=0;
    if not ReadHeader(Stream, FHeader, ColorTableSize) then
      raise Exception.Create('Unknown/Unsupported GIF image type');
    Progress(psRunning, 0, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;

    // global palette
    if ColorTableSize > 0 then
      ReadPaletteAtOnce(stream, ColorTableSize);

    // skip extensions
    Repeat
      Introducer:=SkipBlock(Stream);
    until (Introducer = $2C) or (Introducer = $3B);

    if not ReadDescriptor(Stream, FDescriptor, ColorTableSize) then exit;
    // local palette
    if ColorTableSize > 0 then
      ReadPaletteAtOnce(stream, ColorTableSize);

    // parse header
    if not AnalyzeHeader then exit;

    // create image
    if Assigned(OnCreateImage) then
      OnCreateImage(Self,Img);
    Img.SetSize(FWidth,FHeight);

    // read pixels
    if not ReadScanLine(Stream) then exit;
    if Img is TBGRACustomBitmap then
    begin
      if not WriteScanLineBGRA(Img) then exit;
    end else
      if not WriteScanLine(Img) then exit;

    // ToDo: read further images
  finally
    FreeAndNil(FPalette);
    ReAllocMem(FScanLine,0);
  end;
  Progress(FPimage.psEnding, 100, false, Rect(0,0,FWidth,FHeight), '', ContProgress);
end;

class function TBGRAReaderGif.ReadHeader(Str: TStream; out AHeader: TGIFHeader;
  out AColorTableSize: integer): boolean;
begin
  fillchar({%H-}AHeader, sizeof(AHeader), 0);
  result :=
    (Str.Read(AHeader, SizeOf(AHeader)) = SizeOf(AHeader))
    and (AHeader.Signature = 'GIF')
    and ((AHeader.Version = '87a') or (AHeader.Version = '89a'));

  {$IFDEF ENDIAN_BIG}
  with AHeader do
  begin
    ScreenWidth := LEtoN(ScreenWidth);
    ScreenHeight := LEtoN(ScreenHeight);
  end;
  {$ENDIF}

  if (AHeader.Packedbit and $80) <> 0 then
    AColorTableSize := 1 shl (AHeader.Packedbit and 7 + 1)
  else
    AColorTableSize := 0;
end;

class function TBGRAReaderGif.ReadDescriptor(Str: TStream; out
  ADescriptor: TGifImageDescriptor; out AColorTableSize: integer): boolean;
begin
  fillchar({%H-}ADescriptor, sizeof(ADescriptor), 0);
  // descriptor
  result := Str.Read(ADescriptor, SizeOf(ADescriptor)) = SizeOf(ADescriptor);
  {$IFDEF ENDIAN_BIG}
  with ADescriptor do
  begin
    Left := LEtoN(Left);
    Top := LEtoN(Top);
    Width := LEtoN(Width);
    Height := LEtoN(Height);
  end;
  {$ENDIF}
  // local palette
  if (ADescriptor.Packedbit and $80) <> 0 then
    AColorTableSize := 1 shl (ADescriptor.Packedbit and 7 + 1)
  else
    AColorTableSize := 0;
end;

class function TBGRAReaderGif.IgnoreExtensionBlock(Str: TStream): byte;
var
  Introducer,
  Labels,
  SkipByte : byte;
begin
  Introducer := 0;
  Str.read(Introducer,1);
  if Introducer = $21 then
  begin
     Labels := 0;
     Str.read(Labels,1);
     Case Labels of
       $FE, $FF :     // Comment Extension block or Application Extension block
            while true do
            begin
              SkipByte := 0;
              Str.Read(SkipByte, 1);
              if SkipByte = 0 then Break;
              Str.Seek(SkipByte, soFromCurrent);
            end;
       $F9 :         // Graphics Control Extension block
            begin
              Str.Seek(SizeOf(TGifGraphicsControlExtension), soFromCurrent);
            end;
       $01 :        // Plain Text Extension block
            begin
              SkipByte := 0;
              Str.Read(SkipByte, 1);
              Str.Seek(SkipByte, soFromCurrent);
              while true do
              begin
                SkipByte := 0;
                Str.Read(SkipByte, 1);
                if SkipByte = 0 then Break;
                Str.Seek(SkipByte, soFromCurrent);
              end;
            end;
      end;
  end;
  Result:=Introducer;
end;


class function TBGRAReaderGif.InternalSize(Str: TStream): TPoint;
var h: TGIFHeader;
  colorTableSize: integer;
  d: TGifImageDescriptor;
  Introducer: Byte;
begin
  result := Point(0, 0);
  if not ReadHeader(Str, h, colorTableSize) then exit;
  // skip palette
  if colorTableSize > 0 then
     Str.Seek(sizeof(TGifRGB)*colorTableSize, soCurrent);
  // skip extensions
  Repeat
    Introducer:= IgnoreExtensionBlock(Str);
  until (Introducer = $2C) or (Introducer = $3B);
  if not ReadDescriptor(Str, d, colorTableSize) then exit;
  result := Point(d.Width, d.Height);
end;

function TBGRAReaderGif.ReadScanLine(Stream: TStream): boolean;
var
  OldPos,
  UnpackedSize,
  PackedSize:longint;
  I: Integer;
  Data,
  Bits,
  Code: LongWord;
  SourcePtr: PByte;
  InCode: LongWord;

  CodeSize: LongWord;
  CodeMask: LongWord;
  FreeCode: LongWord;
  OldCode: LongWord;
  Prefix: array[0..4095] of LongWord;
  Suffix,
  Stack: array [0..4095] of Byte;
  StackPointer, StackTop: PByte;
  StackSize: integer;
  DataComp,
  Target: PByte;
  {%H-}B,
  {%H-}FInitialCodeSize,
  FirstChar: Byte;
  ClearCode,
  EOICode: Word;
  ContProgress: Boolean;

begin
  DataComp:=nil;
  ContProgress:=true;
  try
    // read dictionary size
    Stream.read({%H-}FInitialCodeSize, 1);

    // search end of compressor table
    OldPos:=Stream.Position;
    PackedSize := 0;
    Repeat
      Stream.read({%H-}B, 1);
      if B > 0 then
      begin
        inc(PackedSize, B);
        Stream.Seek(B, soFromCurrent);
      end;
    until B = 0;

    Progress(psRunning, trunc(100.0 * (Stream.position / Stream.size)),
             False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit(false);

    Getmem(DataComp, PackedSize);
    // read compressor table
    SourcePtr:=DataComp;
    Stream.Position:=OldPos;
    Repeat
      Stream.read(B, 1);
      if B > 0 then
      begin
         Stream.ReadBuffer(SourcePtr^, B);
         Inc(SourcePtr,B);
      end;
    until B = 0;

    Progress(psRunning, trunc(100.0 * (Stream.position / Stream.size)),
             False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit(false);

    SourcePtr:=DataComp;
    Target := FScanLine;
    CodeSize := FInitialCodeSize + 1;
    ClearCode := 1 shl FInitialCodeSize;
    EOICode := ClearCode + 1;
    FreeCode := ClearCode + 2;
    OldCode := 4096;
    CodeMask := (1 shl CodeSize) - 1;
    UnpackedSize:=FWidth * FHeight;
    for I := 0 to ClearCode - 1 do
    begin
      Prefix[I] := 4096;
      Suffix[I] := I;
    end;
    StackTop := @Stack[high(Stack)];
    StackPointer := StackTop;
    FirstChar := 0;
    Data := 0;
    Bits := 0;
    // LZW decompression gif
    while (UnpackedSize > 0) and (PackedSize > 0) do
    begin
      Inc(Data, SourcePtr^ shl Bits);
      Inc(Bits, 8);
      while Bits >= CodeSize do
      begin
        Code := Data and CodeMask;
        Data := Data shr CodeSize;
        Dec(Bits, CodeSize);
        if Code = EOICode then Break;
        if Code = ClearCode then
        begin
          CodeSize := FInitialCodeSize + 1;
          CodeMask := (1 shl CodeSize) - 1;
          FreeCode := ClearCode + 2;
          OldCode := 4096;
          Continue;
        end;
        if Code > FreeCode then Break;
        if OldCode = 4096 then
        begin
          FirstChar := Suffix[Code];
          Target^ := FirstChar;
          Inc(Target);
          Dec(UnpackedSize);
          OldCode := Code;
          Continue;
        end;
        InCode := Code;
        if Code = FreeCode then
        begin
          StackPointer^ := FirstChar;
          dec(StackPointer);
          Code := OldCode;
        end;
        while Code > ClearCode do
        begin
          StackPointer^ := Suffix[Code];
          dec(StackPointer);
          Code := Prefix[Code];
        end;
        FirstChar := Suffix[Code];
        StackPointer^ := FirstChar;
        dec(StackPointer);
        Prefix[FreeCode] := OldCode;
        Suffix[FreeCode] := FirstChar;
        if (FreeCode = CodeMask) and
           (CodeSize < 12) then
        begin
          Inc(CodeSize);
          CodeMask := (1 shl CodeSize) - 1;
        end;
        if FreeCode < 4095 then Inc(FreeCode);
        OldCode := InCode;
        StackSize := StackTop-StackPointer;
        if StackSize > 0 then
        begin
          Move((StackPointer+1)^, Target^, StackSize);
          inc(Target, StackSize);
          StackPointer:= StackTop;
          dec(UnpackedSize, StackSize);
        end;
      end;
      Inc(SourcePtr);
      Dec(PackedSize);
    end;
    Progress(psRunning, trunc(100.0 * (Stream.position / Stream.size)),
             False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit(false);
  finally
    if DataComp<>nil then
      FreeMem(DataComp);
  end;
  Result:=true;
end;

function TBGRAReaderGif.WriteScanLineBGRA(Img: TFPCustomImage): Boolean;
Var
  Row, Col,i : Integer;
  Pass, Every : byte;
  P : PByte;
  PBGRAPalette: PBGRAPixel;
  PDest: PBGRAPixel;
  function IsMultiple(NumberA, NumberB: Integer): Boolean;
  begin
    Result := (NumberA >= NumberB) and
              (NumberB > 0) and
              (NumberA mod NumberB = 0);
  end;
begin
  Result:=false;
  P:=FScanLine;
  getmem(PBGRAPalette, (FPalette.Count)*sizeof(TBGRAPixel));
  for i := 0 to FPalette.Count-1 do PBGRAPalette[i] := FPColorToBGRA(FPalette.Color[i]);
  If FInterlace then
  begin
    For Pass := 1 to 4 do
    begin
      Case Pass of
         1 : begin
               Row := 0;
               Every := 8;
             end;
         2 : begin
               Row := 4;
               Every := 8;
             end;
         3 : begin
               Row := 2;
               Every := 4;
             end;
         else{4}
             begin
               Row := 1;
               Every := 2;
             end;
        end;
      Repeat
        PDest := TBGRACustomBitmap(Img).ScanLine[Row];
        for Col:=Img.Width-1 downto 0 do
        begin
          PDest^ := PBGRAPalette[P^];
          Inc(P);
          Inc(PDest);
        end;
        Inc(Row, Every);
      until Row >= Img.Height;
    end;
  end
  else
  begin
    for Row:=0 to Img.Height-1 do
    begin
      PDest := TBGRACustomBitmap(Img).ScanLine[Row];
      for Col:=Img.Width-1 downto 0 do
      begin
        PDest^ := PBGRAPalette[P^];
        Inc(P);
        Inc(PDest);
      end;
    end;
  end;
  FreeMem(PBGRAPalette);
  Result:=true;
end;


initialization
  BGRARegisterImageReader(ifGif, TBGRAReaderGif, True, 'GIF Graphics', 'gif');

end.
