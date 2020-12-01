// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAUTF8;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

uses
  BGRAClasses, SysUtils, math, BGRAUnicode{$IFDEF BGRABITMAP_USE_LCL}, lazutf8classes{$ENDIF};

const
  UTF8_ARABIC_ALEPH = 'ا';
  UTF8_ARABIC_ALEPH_HAMZA_BELOW = 'إ';
  UTF8_ARABIC_ALEPH_HAMZA_ABOVE = 'أ';
  UTF8_ARABIC_ALEPH_MADDA_ABOVE = 'آ';
  UTF8_ARABIC_LAM = 'ل';
  UTF8_NO_BREAK_SPACE = ' ';
  UTF8_ZERO_WIDTH_NON_JOINER = '‌';
  UTF8_ZERO_WIDTH_JOINER = '‍';
  UTF8_LINE_SEPARATOR = #$E2#$80#$A8;       //equivalent of <br>
  UTF8_PARAGRAPH_SEPARATOR = #$E2#$80#$A9;  //equivalent of </p>
  UTF8_NEXT_LINE = #$C2#$85;                //equivalent of CRLF

{$IFDEF BGRABITMAP_USE_LCL}
type
  TFileStreamUTF8 = lazutf8classes.TFileStreamUTF8;
  TStringListUTF8 = lazutf8classes.TStringListUTF8;
{$ELSE}
type
  TFileStreamUTF8 = class(THandleStream)
  private
    FFileName: utf8string;
  public
    constructor Create(const AFileName: utf8string; Mode: Word); overload;
    constructor Create(const AFileName: utf8string; Mode: Word; Rights: LongWord); overload;
    destructor Destroy; override;
    property FileName: utf8string Read FFilename;
  end;

  TStringListUTF8 = class(TStringList)
  protected
    function DoCompareText(const s1,s2 : string) : PtrInt; override;
  public
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
  end;
{$ENDIF}

procedure LoadStringsFromFileUTF8(List: TStrings; const FileName: string);
procedure SaveStringsToFileUTF8(List: TStrings; const FileName: string);

function UTF8ToSys(const s: string): string;
function SysToUTF8(const s: string): string;

function UTF8LowerCase(const s: string): string;
function UTF8UpperCase(const s: string): string;

function UTF8CompareStr(const S1, S2: string): Integer;
function UTF8CompareText(const S1, S2: string): Integer;

function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar;

function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
function FileCreateUTF8(Const FileName : string) : THandle; overload;
function FileCreateUTF8(Const FileName : string; Rights: LongWord) : THandle; overload;
function FileExistsUTF8(Const FileName : string): boolean;
function DeleteFileUTF8(Const FileName : string): boolean;
function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
function FindNextUTF8(var Rslt: TSearchRec): Longint;
procedure FindCloseUTF8(var F: TSearchrec);

type
  string4 = string[4];
  TUnicodeArray = packed array of LongWord;
  TIntegerArray = array of integer;

function UTF8CharacterLength(p: PChar): integer;
function UTF8Length(const s: string): PtrInt; overload;
function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt; overload;
function UnicodeCharToUTF8(u: LongWord): string4;
function UTF8ReverseString(const s: string): string;
function UTF8CodepointToUnicode(p: PChar; ACodePointLen: integer): LongWord;
function UTF8ToUTF16(const S: AnsiString): UnicodeString;
function UTF16ToUTF8(const S: UnicodeString): AnsiString;
procedure UTF8ToUnicodeArray(const sUTF8: string; out u: TUnicodeArray; out ofs: TIntegerArray);

type
  TBidiUTF8Info = packed record
    Offset: Integer;
    BidiInfo: TUnicodeBidiInfo;
  end;
  TBidiUTF8Array = packed array of TBidiUTF8Info;
  TUnicodeDisplayOrder = BGRAUnicode.TUnicodeDisplayOrder;
  TUnicodeBidiInfo = BGRAUnicode.TUnicodeBidiInfo;

function GetBidiClassUTF8(P: PChar): TUnicodeBidiClass;
function GetFirstStrongBidiClassUTF8(const sUTF8: string): TUnicodeBidiClass;
function GetLastStrongBidiClassUTF8(const sUTF8: string): TUnicodeBidiClass;
function IsRightToLeftUTF8(const sUTF8: string): boolean;
function IsZeroWidthUTF8(const sUTF8: string): boolean;
function AddParagraphBidiUTF8(s: string; ARightToLeft: boolean): string;
function AnalyzeBidiUTF8(const sUTF8: string): TBidiUTF8Array; overload;
function AnalyzeBidiUTF8(const sUTF8: string; ABidiMode: TFontBidiMode): TBidiUTF8Array; overload;
function AnalyzeBidiUTF8(const sUTF8: string; ARightToLeft: boolean): TBidiUTF8Array; overload;
function GetUTF8DisplayOrder(const ABidi: TBidiUTF8Array): TUnicodeDisplayOrder;
function ContainsBidiIsolateOrFormattingUTF8(const sUTF8: string): boolean;

function UTF8OverrideDirection(const sUTF8: string; ARightToLeft: boolean): string;
function UTF8EmbedDirection(const sUTF8: string; ARightToLeft: boolean): string;
function UTF8Ligature(const sUTF8: string; ARightToLeft: boolean; ALigatureLeft, ALigatureRight: boolean): string;

type

  { TGlyphUtf8 }

  TGlyphUtf8 = record
  private
    function GetEmpty: boolean;
  public
    GlyphUtf8, MirroredGlyphUtf8: string;
    RightToLeft, Mirrored, Merged: boolean;
    ByteOffset, ByteSize: integer;
    property Empty: boolean read GetEmpty;
  end;

  { TGlyphCursorUtf8 }

  TGlyphCursorUtf8 = record
  private
    sUTF8: string;
    currentChar: string;
    currentOffset: integer;
    currentBidiInfo: TUnicodeBidiInfo;
    bidiArray: TBidiUTF8Array;
    displayOrder: TUnicodeDisplayOrder;
    displayIndex: Integer;
    procedure NextMultichar;
    procedure PeekMultichar;
  public
    class function New(const textUTF8: string; ABidiMode: TFontBidiMode): TGlyphCursorUtf8; static;
    function GetNextGlyph: TGlyphUtf8;
    procedure Rewind;
    function EndOfString: boolean;
  end;

//little endian stream functions
function LEReadInt64(Stream: TStream): int64;
procedure LEWriteInt64(Stream: TStream; AValue: int64);
function LEReadLongint(Stream: TStream): longint;
procedure LEWriteLongint(Stream: TStream; AValue: LongInt);
function LEReadByte(Stream: TStream): byte;
procedure LEWriteByte(Stream: TStream; AValue: Byte);
function LEReadSingle(Stream: TStream): single;
procedure LEWriteSingle(Stream: TStream; AValue: single);

implementation

{$IFDEF BGRABITMAP_USE_LCL}
uses LazFileUtils, LazUtf8;

procedure LoadStringsFromFileUTF8(List: TStrings; const FileName: string);
begin
  lazutf8classes.LoadStringsFromFileUTF8(List,FileName);
end;

procedure SaveStringsToFileUTF8(List: TStrings; const FileName: string);
begin
  lazutf8classes.SaveStringsToFileUTF8(List,FileName);
end;

function UTF8ToSys(const s: string): string;
begin
  result := LazUtf8.UTF8ToSys(s);
end;

function SysToUTF8(const s: string): string;
begin
  result := LazUtf8.SysToUTF8(s);
end;

function UTF8LowerCase(const s: string): string;
begin
  result := LazUtf8.UTF8LowerCase(s);
end;

function UTF8UpperCase(const s: string): string;
begin
  result := LazUtf8.UTF8UpperCase(s);
end;

function UTF8CompareStr(const S1, S2: string): Integer;
begin
  result := LazUtf8.UTF8CompareStr(S1,S2);
end;

function UTF8CompareText(const S1, S2: string): Integer;
begin
  result := LazUtf8.UTF8CompareText(S1,S2);
end;

function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
begin
  result := LazFileUtils.FileOpenUTF8(FileName, Mode);
end;

function FileCreateUTF8(Const FileName : string) : THandle; overload;
begin
  result := LazFileUtils.FileCreateUTF8(FileName);
end;

function FileCreateUTF8(Const FileName : string; Rights: LongWord) : THandle; overload;
begin
  result := LazFileUtils.FileCreateUTF8(FileName, Rights);
end;

function FileExistsUTF8(Const FileName : string): boolean;
begin
  result := LazFileUtils.FileExistsUTF8(FileName);
end;

function DeleteFileUTF8(const FileName: string): boolean;
begin
  result := LazFileUtils.DeleteFileUTF8(FileName);
end;

function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec
  ): Longint;
begin
  result := LazFileUtils.FindFirstUTF8(Path,Attr,Rslt);
end;

function FindNextUTF8(var Rslt: TSearchRec): Longint;
begin
  result := LazFileUtils.FindNextUTF8(Rslt);
end;

procedure FindCloseUTF8(var F: TSearchrec);
begin
  LazFileUtils.FindCloseUTF8(F);
end;

function UTF8CharacterLength(p: PChar): integer;
begin
  result := LazUtf8.UTF8CharacterLength(p);
end;

function UTF8Length(const s: string): PtrInt;
begin
  result := LazUtf8.UTF8Length(s);
end;

function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
begin
  result := LazUtf8.UTF8Length(p, ByteCount);
end;

function UnicodeCharToUTF8(u: LongWord): string4;
begin
  result := LazUtf8.UnicodeToUTF8(u);
end;

function UTF8ToUTF16(const S: AnsiString): UnicodeString;
begin
  result := LazUTf8.UTF8ToUTF16(s);
end;

function UTF16ToUTF8(const S: UnicodeString): AnsiString;
begin
  result := LazUTf8.UTF16ToUTF8(s);
end;

{$ELSE}

procedure LoadStringsFromFileUTF8(List: TStrings; const FileName: string);
var
  uList: TStringListUTF8;
begin
  if List is TStringListUTF8 then
  begin
    List.LoadFromFile(FileName);
    exit;
  end;
  uList:=TStringListUTF8.Create;
  try
    uList.LoadFromFile(FileName);
    List.Assign(uList);
  finally
    uList.Free;
  end;
end;

procedure SaveStringsToFileUTF8(List: TStrings; const FileName: string);
var
  uList: TStringListUTF8;
begin
  if List is TStringListUTF8 then
  begin
    List.SaveToFile(FileName);
    exit;
  end;
  uList:=TStringListUTF8.Create;
  try
    uList.Assign(List);
    uList.SaveToFile(FileName);
  finally
    uList.Free;
  end;
end;

function UTF8LowerCase(const s: string): string;
begin
  result := UTF8Encode(UnicodeLowerCase(UTF8Decode(s)));
end;

function UTF8UpperCase(const s: string): string;
begin
  result := UTF8Encode(UnicodeUpperCase(UTF8Decode(s)));
end;

function UTF8CompareStr(const S1, S2: string): Integer;
begin
  Result := SysUtils.CompareStr(S1, S2);
end;

function UTF8CompareText(const S1, S2: string): Integer;
begin
  Result := UnicodeCompareText(UTF8Decode(S1), UTF8Decode(S2));
end;

function FileOpenUTF8(const FileName: string; Mode: Integer): THandle;
begin
  result := FileOpen(UTF8ToSys(FileName),Mode);
end;

function FileCreateUTF8(const FileName: string): THandle;
begin
  result := FileCreate(UTF8ToSys(FileName));
end;

function FileCreateUTF8(const FileName: string; Rights: LongWord): THandle;
begin
  result := FileCreate(UTF8ToSys(FileName),Rights);
end;

function FileExistsUTF8(const FileName: string): boolean;
begin
  result := FileExists(UTF8ToSys(FileName));
end;

function DeleteFileUTF8(const FileName: string): boolean;
begin
  result := DeleteFile(UTF8ToSys(FileName));
end;

function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec
  ): Longint;
begin
  result := FindFirst(UTF8ToSys(Path),Attr,Rslt);
  Rslt.Name := SysToUTF8(Rslt.Name);
end;

function FindNextUTF8(var Rslt: TSearchRec): Longint;
begin
  result := FindNext(Rslt);
  if result = 0 then
    Rslt.Name := SysToUTF8(Rslt.Name);
end;

procedure FindCloseUTF8(var F: TSearchrec);
begin
  FindClose(F);
end;

function UTF8ToSys(const s: string): string;
begin
  result := Utf8ToAnsi(s);
end;

function SysToUTF8(const s: string): string;
begin
  result := AnsiToUtf8(s);
end;

function UTF8CharacterLength(p: PChar): integer;
begin
  if p<>nil then begin
    if ord(p^)<%11000000 then begin
      // regular single byte character (#0 is a character, this is pascal ;)
      Result:=1;
    end
    else begin
      // multi byte
      if ((ord(p^) and %11100000) = %11000000) then begin
        // could be 2 byte character
        if (ord(p[1]) and %11000000) = %10000000 then
          Result:=2
        else
          Result:=1;
      end
      else if ((ord(p^) and %11110000) = %11100000) then begin
        // could be 3 byte character
        if ((ord(p[1]) and %11000000) = %10000000)
        and ((ord(p[2]) and %11000000) = %10000000) then
          Result:=3
        else
          Result:=1;
      end
      else if ((ord(p^) and %11111000) = %11110000) then begin
        // could be 4 byte character
        if ((ord(p[1]) and %11000000) = %10000000)
        and ((ord(p[2]) and %11000000) = %10000000)
        and ((ord(p[3]) and %11000000) = %10000000) then
          Result:=4
        else
          Result:=1;
      end
      else
        Result:=1;
    end;
  end else
    Result:=0;
end;

function UTF8Length(const s: string): PtrInt;
begin
  Result:=UTF8Length(PChar(s),length(s));
end;

function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
var
  CharLen: LongInt;
begin
  Result:=0;
  while (ByteCount>0) do begin
    inc(Result);
    CharLen:=UTF8CharacterLength(p);
    inc(p,CharLen);
    dec(ByteCount,CharLen);
  end;
end;

function UnicodeToUTF8Inline(CodePoint: LongWord; Buf: PChar): integer;
begin
  case CodePoint of
    0..$7f:
      begin
        Result:=1;
        Buf[0]:=char(byte(CodePoint));
      end;
    $80..$7ff:
      begin
        Result:=2;
        Buf[0]:=char(byte($c0 or (CodePoint shr 6)));
        Buf[1]:=char(byte($80 or (CodePoint and $3f)));
      end;
    $800..$ffff:
      begin
        Result:=3;
        Buf[0]:=char(byte($e0 or (CodePoint shr 12)));
        Buf[1]:=char(byte((CodePoint shr 6) and $3f) or $80);
        Buf[2]:=char(byte(CodePoint and $3f) or $80);
      end;
    $10000..$10ffff:
      begin
        Result:=4;
        Buf[0]:=char(byte($f0 or (CodePoint shr 18)));
        Buf[1]:=char(byte((CodePoint shr 12) and $3f) or $80);
        Buf[2]:=char(byte((CodePoint shr 6) and $3f) or $80);
        Buf[3]:=char(byte(CodePoint and $3f) or $80);
      end;
  else
    Result:=0;
  end;
end;

function UnicodeCharToUTF8(u: LongWord): string4;
begin
  result[0] := chr(UnicodeToUTF8Inline(u,@result[1]));
end;

constructor TFileStreamUTF8.Create(const AFileName: utf8string; Mode: Word);
var
  lHandle: THandle;
begin
  FFileName:= AFileName;
  if Mode = fmcreate then
    lHandle:= FileCreateUTF8(AFileName)
  else
    lHandle:= FileOpenUTF8(AFileName, Mode);

  If (THandle(lHandle)=feInvalidHandle) then
  begin
    if Mode = fmCreate then
      raise EFCreateError.createfmt({SFCreateError}'Unable to create file "%s"', [AFileName])
    else
      raise EFOpenError.Createfmt({SFOpenError}'Unable to open file "%s"', [AFilename]);
  end
  else
    inherited Create(lHandle);
end;

constructor TFileStreamUTF8.Create(const AFileName: utf8string; Mode: Word; Rights: LongWord);
var
  lHandle: THandle;
begin
  FFileName:=AFileName;
  if Mode=fmcreate then
    lHandle:=FileCreateUTF8(AFileName,Rights)
  else
    lHandle:=FileOpenUTF8(AFileName,Mode);

  if (THandle(lHandle)=feInvalidHandle) then
  begin
    if Mode=fmcreate then
      raise EFCreateError.createfmt({SFCreateError}'Unable to create file "%s"',[AFileName])
    else
      raise EFOpenError.Createfmt({SFOpenError}'Unable to open file "%s"',[AFilename]);
  end
  else
    inherited Create(lHandle);
end;

destructor TFileStreamUTF8.Destroy;
begin
  FileClose(Handle);
end;

function TStringListUTF8.DoCompareText(const s1, s2: string): PtrInt;
begin
  if CaseSensitive then
    Result:= UTF8CompareStr(s1,s2)
  else
    Result:= UTF8CompareText(s1,s2);
end;

procedure TStringListUTF8.LoadFromFile(const FileName: string);
var
  TheStream: TFileStreamUTF8;
begin
  TheStream:= TFileStreamUTF8.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

procedure TStringListUTF8.SaveToFile(const FileName: string);
var
  TheStream: TFileStreamUTF8;
begin
  TheStream:=TFileStreamUTF8.Create(FileName,fmCreate);
  try
    SaveToStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

{copied from LazUTF8
 ------------------------------------------------------------------------------}
type
  TConvertResult = (trNoError, trNullSrc, trNullDest, trDestExhausted,
    trInvalidChar, trUnfinishedChar);

  TConvertOption = (toInvalidCharError, toInvalidCharToSymbol,
    toUnfinishedCharError, toUnfinishedCharToSymbol);
  TConvertOptions = set of TConvertOption;

{ ------------------------------------------------------------------------------
  Name:    ConvertUTF8ToUTF16
  Params:  Dest                - Pointer to destination string
           DestWideCharCount   - Wide char count allocated in destination string
           Src                 - Pointer to source string
           SrcCharCount        - Char count allocated in source string
           Options             - Conversion options, if none is set, both
             invalid and unfinished source chars are skipped

             toInvalidCharError       - Stop on invalid source char and report
                                      error
             toInvalidCharToSymbol    - Replace invalid source chars with '?'
             toUnfinishedCharError    - Stop on unfinished source char and
                                      report error
             toUnfinishedCharToSymbol - Replace unfinished source char with '?'

           ActualWideCharCount - Actual wide char count converted from source
                               string to destination string
  Returns:
    trNoError        - The string was successfully converted without
                     any error
    trNullSrc        - Pointer to source string is nil
    trNullDest       - Pointer to destination string is nil
    trDestExhausted  - Destination buffer size is not big enough to hold
                     converted string
    trInvalidChar    - Invalid source char has occurred
    trUnfinishedChar - Unfinished source char has occurred

  Converts the specified UTF-8 encoded string to UTF-16 encoded (system endian)
 ------------------------------------------------------------------------------}
function ConvertUTF8ToUTF16(Dest: PWideChar; DestWideCharCount: SizeUInt;
  Src: PChar; SrcCharCount: SizeUInt; Options: TConvertOptions;
  out ActualWideCharCount: SizeUInt): TConvertResult;
var
  DestI, SrcI: SizeUInt;
  B1, B2, B3, B4: Byte;
  W: Word;
  C: LongWord;

  function UnfinishedCharError: Boolean;
  begin
    if toUnfinishedCharToSymbol in Options then
    begin
      Dest[DestI] := System.WideChar('?');
      Inc(DestI);
      Result := False;
    end
    else
      if toUnfinishedCharError in Options then
      begin
        ConvertUTF8ToUTF16 := trUnfinishedChar;
        Result := True;
      end
      else Result := False;
  end;

  function InvalidCharError(Count: SizeUInt): Boolean; inline;
  begin
    if not (toInvalidCharError in Options) then
    begin
      if toInvalidCharToSymbol in Options then
      begin
        Dest[DestI] := System.WideChar('?');
        Inc(DestI);
      end;

      Dec(SrcI, Count);

      // skip trailing UTF-8 char bytes
      while (Count > 0) do
      begin
        if (Byte(Src[SrcI]) and %11000000) <> %10000000 then Break;
        Inc(SrcI);
        Dec(Count);
      end;

      Result := False;
    end
    else
      if toInvalidCharError in Options then
      begin
        ConvertUTF8ToUTF16 := trUnfinishedChar;
        Result := True;
      end;
  end;

begin
  ActualWideCharCount := 0;

  if not Assigned(Src) then
  begin
    Result := trNullSrc;
    Exit;
  end;

  if not Assigned(Dest) then
  begin
    Result := trNullDest;
    Exit;
  end;
  SrcI := 0;
  DestI := 0;

  while (DestI < DestWideCharCount) and (SrcI < SrcCharCount) do
  begin
    B1 := Byte(Src[SrcI]);
    Inc(SrcI);

    if B1 < 128 then // single byte UTF-8 char
    begin
      Dest[DestI] := System.WideChar(B1);
      Inc(DestI);
    end
    else
    begin
      if SrcI >= SrcCharCount then
        if UnfinishedCharError then Exit(trInvalidChar)
        else Break;

      B2 := Byte(Src[SrcI]);
      Inc(SrcI);

      if (B1 and %11100000) = %11000000 then // double byte UTF-8 char
      begin
        if (B2 and %11000000) = %10000000 then
        begin
          Dest[DestI] := System.WideChar(((B1 and %00011111) shl 6) or (B2 and %00111111));
          Inc(DestI);
        end
        else // invalid character, assume single byte UTF-8 char
          if InvalidCharError(1) then Exit(trInvalidChar);
      end
      else
      begin
        if SrcI >= SrcCharCount then
          if UnfinishedCharError then Exit(trInvalidChar)
          else Break;

        B3 := Byte(Src[SrcI]);
        Inc(SrcI);

        if (B1 and %11110000) = %11100000 then // triple byte UTF-8 char
        begin
          if ((B2 and %11000000) = %10000000) and ((B3 and %11000000) = %10000000) then
          begin
            W := ((B1 and %00011111) shl 12) or ((B2 and %00111111) shl 6) or (B3 and %00111111);
            if (W < $D800) or (W > $DFFF) then // to single wide char UTF-16 char
            begin
              Dest[DestI] := System.WideChar(W);
              Inc(DestI);
            end
            else // invalid UTF-16 character, assume double byte UTF-8 char
              if InvalidCharError(2) then Exit(trInvalidChar);
          end
          else // invalid character, assume double byte UTF-8 char
            if InvalidCharError(2) then Exit(trInvalidChar);
        end
        else
        begin
          if SrcI >= SrcCharCount then
            if UnfinishedCharError then Exit(trInvalidChar)
            else Break;

          B4 := Byte(Src[SrcI]);
          Inc(SrcI);

          if ((B1 and %11111000) = %11110000) and ((B2 and %11000000) = %10000000)
            and ((B3 and %11000000) = %10000000) and ((B4 and %11000000) = %10000000) then
          begin // 4 byte UTF-8 char
            C := ((B1 and %00011111) shl 18) or ((B2 and %00111111) shl 12)
              or ((B3 and %00111111) shl 6)  or (B4 and %00111111);
            // to double wide char UTF-16 char
            Dest[DestI] := System.WideChar($D800 or ((C - $10000) shr 10));
            Inc(DestI);
            if DestI >= DestWideCharCount then Break;
            Dest[DestI] := System.WideChar($DC00 or ((C - $10000) and %0000001111111111));
            Inc(DestI);
          end
          else // invalid character, assume triple byte UTF-8 char
            if InvalidCharError(3) then Exit(trInvalidChar);
        end;
      end;
    end;
  end;

  if DestI >= DestWideCharCount then
  begin
    DestI := DestWideCharCount - 1;
    Result := trDestExhausted;
  end
  else
    Result := trNoError;

  Dest[DestI] := #0;
  ActualWideCharCount := DestI + 1;
end;

function UTF8ToUTF16(const P: PChar; ByteCnt: SizeUInt): UnicodeString;
var
  L: SizeUInt;
begin
  if ByteCnt=0 then
    exit('');
  SetLength(Result, ByteCnt);
  // wide chars of UTF-16 <= bytes of UTF-8 string
  if ConvertUTF8ToUTF16(PWideChar(Result), Length(Result) + 1, P, ByteCnt,
    [toInvalidCharToSymbol], L) = trNoError
  then SetLength(Result, L - 1)
  else Result := '';
end;

{------------------------------------------------------------------------------
  Name:    UTF8ToUTF16
  Params:  S - Source UTF-8 string
  Returns: UTF-16 encoded string

  Converts the specified UTF-8 encoded string to UTF-16 encoded (system endian)
  Avoid copying the result string since on windows a widestring requires a full
  copy
 ------------------------------------------------------------------------------}
function UTF8ToUTF16(const S: AnsiString): UnicodeString;
begin
  Result:=UTF8ToUTF16(PChar(S),length(S));
end;

{------------------------------------------------------------------------------
  Name:    ConvertUTF16ToUTF8
  Params:  Dest             - Pointer to destination string
           DestCharCount    - Char count allocated in destination string
           Src              - Pointer to source string
           SrcWideCharCount - Wide char count allocated in source string
           Options          - Conversion options, if none is set, both
             invalid and unfinished source chars are skipped.
             See ConvertUTF8ToUTF16 for details.

           ActualCharCount  - Actual char count converted from source
                            string to destination string
  Returns: See ConvertUTF8ToUTF16

  Converts the specified UTF-16 encoded string (system endian) to UTF-8 encoded
 ------------------------------------------------------------------------------}
function ConvertUTF16ToUTF8(Dest: PChar; DestCharCount: SizeUInt;
  Src: PWideChar; SrcWideCharCount: SizeUInt; Options: TConvertOptions;
  out ActualCharCount: SizeUInt): TConvertResult;
var
  DestI, SrcI: SizeUInt;
  W1, W2: Word;
  C: LongWord;

  function UnfinishedCharError: Boolean;
  begin
    if toUnfinishedCharToSymbol in Options then
    begin
      Dest[DestI] := Char('?');
      Inc(DestI);
      Result := False;
    end
    else
      if toUnfinishedCharError in Options then
      begin
        ConvertUTF16ToUTF8 := trUnfinishedChar;
        Result := True;
      end
      else Result := False;
  end;

  function InvalidCharError(Count: SizeUInt): Boolean; inline;
  begin
    if not (toInvalidCharError in Options) then
    begin
      if toInvalidCharToSymbol in Options then
      begin
        Dest[DestI] := Char('?');
        Inc(DestI);
      end;

      Dec(SrcI, Count);
      // skip trailing UTF-16 wide char
      if (Word(Src[SrcI]) and $FC00) = $DC00 then Inc(SrcI);

      Result := False;
    end
    else
      if toInvalidCharError in Options then
      begin
        ConvertUTF16ToUTF8 := trUnfinishedChar;
        Result := True;
      end;
  end;

begin
  ActualCharCount := 0;

  if not Assigned(Src) then
  begin
    Result := trNullSrc;
    Exit;
  end;

  if not Assigned(Dest) then
  begin
    Result := trNullDest;
    Exit;
  end;
  SrcI := 0;
  DestI := 0;

  while (DestI < DestCharCount) and (SrcI < SrcWideCharCount) do
  begin
    W1 := Word(Src[SrcI]);
    Inc(SrcI);

    if (W1 < $D800) or (W1 > $DFFF) then // single wide char UTF-16 char
    begin
      if W1 < $0080 then // to single byte UTF-8 char
      begin
        Dest[DestI] := Char(W1);
        Inc(DestI);
      end
      else
        if W1 < $0800 then // to double byte UTF-8 char
        begin
          Dest[DestI] := Char(%11000000 or ((W1 and %11111000000) shr 6));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or (W1 and %111111));
          Inc(DestI);
        end
        else
        begin // to triple byte UTF-8 char
          Dest[DestI] := Char(%11100000 or ((W1 and %1111000000000000) shr 12));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or ((W1 and %111111000000) shr 6));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or (W1 and %111111));
          Inc(DestI);
        end;
    end
    else
    begin
      if SrcI >= SrcWideCharCount then
        if UnfinishedCharError then Exit(trInvalidChar)
        else Break;

      W2 := Word(Src[SrcI]);
      Inc(SrcI);

      if (W1 and $F800) = $D800 then // double wide char UTF-16 char
      begin
        if (W2 and $FC00) = $DC00 then
        begin
          C := (W1 - $D800) shl 10 + (W2 - $DC00) + $10000;

          // to 4 byte UTF-8 char
          Dest[DestI] := Char(%11110000 or (C shr 18));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or ((C and $3F000) shr 12));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or ((C and %111111000000) shr 6));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or (C and %111111));
          Inc(DestI);
        end
        else // invalid character, assume single wide char UTF-16 char
          if InvalidCharError(1) then Exit(trInvalidChar);
      end
      else // invalid character, assume single wide char UTF-16 char
        if InvalidCharError(1) then Exit(trInvalidChar);
    end;
  end;

  if DestI >= DestCharCount then
  begin
    DestI := DestCharCount - 1;
    Result := trDestExhausted;
  end
  else
    Result := trNoError;

  Dest[DestI] := #0;
  ActualCharCount := DestI + 1;
end;

function UTF16ToUTF8(const P: PWideChar; WideCnt: SizeUInt): AnsiString;
var
  L: SizeUInt;
begin
  if WideCnt=0 then
    exit('');

  SetLength(Result, WideCnt * 3);
  // bytes of UTF-8 <= 3 * wide chars of UTF-16 string
  // e.g. %11100000 10100000 10000000 (UTF-8) is $0800 (UTF-16)
  if ConvertUTF16ToUTF8(PChar(Result), Length(Result) + 1, P, WideCnt,
    [toInvalidCharToSymbol], L) = trNoError then
  begin
    SetLength(Result, L - 1);
  end else
    Result := '';
end;

{------------------------------------------------------------------------------
  Name:    UTF16ToUTF8
  Params:  S - Source UTF-16 string (system endian)
  Returns: UTF-8 encoded string

  Converts the specified UTF-16 encoded string (system endian) to UTF-8 encoded
 ------------------------------------------------------------------------------}
function UTF16ToUTF8(const S: UnicodeString): AnsiString;
begin
  Result := UTF16ToUTF8(PWideChar(S),length(S));
end;

{end of copy from LazUTF8
 ------------------------------------------------------------------------------}

{$ENDIF}

function UTF8ReverseString(const s: string): string;
var
  pSrc,pDest,pEnd: PChar;
  charLen: Integer;
begin
  if s = '' then
  begin
    result := '';
    exit;
  end;
  setlength(result, length(s));
  pDest := @result[1] + length(result);
  pSrc := @s[1];
  pEnd := pSrc+length(s);
  while pSrc < pEnd do
  begin
    charLen := UTF8CharacterLength(pSrc);
    if (charLen = 0) or (pSrc+charLen > pEnd) then break;
    dec(pDest, charLen);
    move(pSrc^, pDest^, charLen);
    inc(pSrc, charLen);
  end;
end;

function UTF8CodepointToUnicode(p: PChar; ACodePointLen: integer): LongWord;
begin
  case ACodePointLen of
    0: result := 0;
    1: result := ord(p^);
    2: result := ((ord(p^) and %00011111) shl 6) or (ord(p[1]) and %00111111);
    3: result := ((ord(p^) and %00011111) shl 12) or ((ord(p[1]) and %00111111) shl 6)
                or (ord(p[2]) and %00111111);
    4: result := ((ord(p^) and %00001111) shl 18) or ((ord(p[1]) and %00111111) shl 12)
                or ((ord(p[2]) and %00111111) shl 6) or (ord(p[3]) and %00111111);
    else
      raise exception.Create('Invalid code point length');
  end;
end;

function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar;
var
  CharLen: LongInt;
begin
  Result:=UTF8Str;
  if Result<>nil then begin
    while (CharIndex>0) and (Len>0) do begin
      CharLen:=UTF8CharacterLength(Result);
      dec(Len,CharLen);
      dec(CharIndex);
      inc(Result,CharLen);
    end;
    if (CharIndex<>0) or (Len<0) then
      Result:=nil;
  end;
end;

function GetBidiClassUTF8(P: PChar): TUnicodeBidiClass;
begin
  result := GetUnicodeBidiClass(UTF8CodepointToUnicode(P, UTF8CharacterLength(p)));
end;

function GetFirstStrongBidiClassUTF8(const sUTF8: string): TUnicodeBidiClass;
var
  p,pEnd: PChar;
  charLen: Integer;
  u: LongWord;
  curBidi: TUnicodeBidiClass;
  isolateNesting: integer;
begin
  if sUTF8 = '' then exit(ubcUnknown);
  p := @sUTF8[1];
  pEnd := p + length(sUTF8);
  isolateNesting:= 0;
  while p < pEnd do
  begin
    charLen := UTF8CharacterLength(p);
    if (charLen = 0) or (p+charLen > pEnd) then break;
    u := UTF8CodepointToUnicode(p, charLen);
    case u of
      UNICODE_POP_DIRECTIONAL_ISOLATE: if isolateNesting > 0 then dec(isolateNesting);
      UNICODE_LEFT_TO_RIGHT_OVERRIDE: exit(ubcLeftToRight);
      UNICODE_RIGHT_TO_LEFT_OVERRIDE: exit(ubcRightToLeft);
    end;
    curBidi := GetUnicodeBidiClass(u);
    if isolateNesting = 0 then
    begin
      if curBidi in[ubcLeftToRight,ubcRightToLeft,ubcArabicLetter] then
        exit(curBidi);
    end;
    case u of
      UNICODE_FIRST_STRONG_ISOLATE, UNICODE_LEFT_TO_RIGHT_ISOLATE, UNICODE_RIGHT_TO_LEFT_ISOLATE: inc(isolateNesting);
    end;
    if curBidi = ubcParagraphSeparator then isolateNesting:= 0;
    inc(p,charLen);
  end;
  exit(ubcUnknown);
end;

function GetLastStrongBidiClassUTF8(const sUTF8: string): TUnicodeBidiClass;
var
  p,pEnd: PChar;
  charLen: Integer;
  u: LongWord;
  curBidi: TUnicodeBidiClass;
  isolateNesting: integer;
begin
  if sUTF8 = '' then exit(ubcUnknown);
  p := @sUTF8[1];
  pEnd := p + length(sUTF8);
  isolateNesting:= 0;
  result := ubcUnknown;
  while p < pEnd do
  begin
    charLen := UTF8CharacterLength(p);
    if (charLen = 0) or (p+charLen > pEnd) then break;
    u := UTF8CodepointToUnicode(p, charLen);
    case u of
      UNICODE_POP_DIRECTIONAL_ISOLATE: if isolateNesting > 0 then dec(isolateNesting);
    end;
    curBidi := GetUnicodeBidiClass(u);
    if isolateNesting = 0 then
    begin
      if curBidi in[ubcLeftToRight,ubcRightToLeft,ubcArabicLetter] then
        result := curBidi;
    end;
    case u of
      UNICODE_FIRST_STRONG_ISOLATE, UNICODE_LEFT_TO_RIGHT_ISOLATE, UNICODE_RIGHT_TO_LEFT_ISOLATE: inc(isolateNesting);
    end;
    if curBidi = ubcParagraphSeparator then isolateNesting:= 0;
    inc(p,charLen);
  end;
end;

function IsRightToLeftUTF8(const sUTF8: string): boolean;
begin
  result := GetFirstStrongBidiClassUTF8(sUTF8) in[ubcRightToLeft,ubcArabicLetter];
end;

function IsZeroWidthUTF8(const sUTF8: string): boolean;
var
  p,pEnd: PChar;
  charLen: Integer;
  u: LongWord;
begin
  if sUTF8 = '' then exit(true);
  p := @sUTF8[1];
  pEnd := p + length(sUTF8);
  while p < pEnd do
  begin
    charLen := UTF8CharacterLength(p);
    if (charLen = 0) or (p+charLen > pEnd) then break;
    u := UTF8CodepointToUnicode(p, charLen);
    if not IsZeroWidthUnicode(u) then exit(false);
    inc(p,charLen);
  end;
  exit(true);
end;

function AddParagraphBidiUTF8(s: string; ARightToLeft: boolean): string;
var
  i,curParaStart: Integer;

  procedure CheckParagraph;
  var
    para,newPara: string;
    paraRTL: boolean;
  begin
    if i > curParaStart then
    begin
      para := copy(s,curParaStart,i-curParaStart);
      paraRTL := GetFirstStrongBidiClassUTF8(para) in[ubcRightToLeft,ubcArabicLetter];
      //detected paragraph does not match overall RTL option
      if paraRTL <> ARightToLeft then
      begin
        if not paraRTL then
          newPara := UnicodeCharToUTF8(UNICODE_LEFT_TO_RIGHT_MARK)+para+UnicodeCharToUTF8(UNICODE_LEFT_TO_RIGHT_MARK)
        else
          newPara := UnicodeCharToUTF8(UNICODE_RIGHT_TO_LEFT_MARK)+para+UnicodeCharToUTF8(UNICODE_RIGHT_TO_LEFT_MARK);
        inc(i, length(newPara)-length(para));
        delete(s, curParaStart, length(para));
        insert(newPara, s, curParaStart);
      end;
    end;
  end;

var
  charLen: integer;
  u: LongWord;

begin
  i := 1;
  curParaStart := 1;
  while i <= length(s) do
  begin
    charLen := UTF8CharacterLength(@s[i]);
    u := UTF8CodepointToUnicode(@s[i], charLen);
    if IsUnicodeParagraphSeparator(u) then
    begin
      CheckParagraph;
      //skip end of line
      inc(i);
      //skip second CRLF
      if ((u = 10) or (u = 13)) and (i <= length(s)) and (s[i] in[#13,#10]) and (s[i]<>s[i-1]) then inc(i);
      curParaStart := i;
    end else
      inc(i);
  end;
  CheckParagraph;
  result := s;
end;

procedure UTF8ToUnicodeArray(const sUTF8: string; out u: TUnicodeArray; out ofs: TIntegerArray);
var
  index,len,charLen: integer;
  p,pStart,pEnd: PChar;
begin
  if sUTF8 = '' then
  begin
    u := nil;
    ofs := nil;
  end
  else
  begin
    pStart := @sUTF8[1];
    pEnd := pStart + length(sUTF8);
    p := pStart;
    len := 0;
    while p < pEnd do
    begin
      charLen := UTF8CharacterLength(p);
      inc(len);
      inc(p,charLen);
    end;

    setlength(u, len);
    setlength(ofs, len);
    p := pStart;
    index := 0;
    while p < pEnd do
    begin
      charLen := UTF8CharacterLength(p);
      u[index] := UTF8CodepointToUnicode(p, charLen);
      ofs[index] := p - pStart;
      inc(index);
      inc(p,charLen);
    end;
  end;
end;

function AnalyzeBidiUTF8(const sUTF8: string; ABidiMode: TFontBidiMode): TBidiUTF8Array;
var
  u: TUnicodeArray;
  ofs: TIntegerArray;
  a: TUnicodeBidiArray;
  i: Integer;
begin
  if sUTF8 = '' then
    result := nil
  else
  begin
    UTF8ToUnicodeArray(sUTF8, u, ofs);
    a := AnalyzeBidiUnicode(@u[0], length(u), ABidiMode);
    setlength(result, length(u));
    for i := 0 to high(result) do
    begin
      result[i].Offset:= ofs[i];
      result[i].BidiInfo := a[i];
    end;
  end;
end;

function AnalyzeBidiUTF8(const sUTF8: string; ARightToLeft: boolean): TBidiUTF8Array;
begin
  if ARightToLeft then
    result := AnalyzeBidiUTF8(sUTF8, fbmRightToLeft)
    else result := AnalyzeBidiUTF8(sUTF8, fbmLeftToRight);
end;

function AnalyzeBidiUTF8(const sUTF8: string): TBidiUTF8Array;
begin
  result := AnalyzeBidiUTF8(sUTF8, fbmAuto)
end;

function GetUTF8DisplayOrder(const ABidi: TBidiUTF8Array): TUnicodeDisplayOrder;
begin
  if length(ABidi) = 0 then
    result := nil
  else
    result := GetUnicodeDisplayOrder(@ABidi[0].BidiInfo, sizeof(TBidiUTF8Info), length(ABidi));
end;

function ContainsBidiIsolateOrFormattingUTF8(const sUTF8: string): boolean;
var
  p,pEnd: PChar;
  charLen: Integer;
  u: LongWord;
begin
  if sUTF8 = '' then exit(false);
  p := @sUTF8[1];
  pEnd := p + length(sUTF8);
  while p < pEnd do
  begin
    charLen := UTF8CharacterLength(p);
    if (charLen = 0) or (p+charLen > pEnd) then break;
    u := UTF8CodepointToUnicode(p, charLen);
    case u of
      UNICODE_LEFT_TO_RIGHT_ISOLATE, UNICODE_RIGHT_TO_LEFT_ISOLATE, UNICODE_FIRST_STRONG_ISOLATE,
      UNICODE_LEFT_TO_RIGHT_EMBEDDING, UNICODE_RIGHT_TO_LEFT_EMBEDDING,
      UNICODE_LEFT_TO_RIGHT_OVERRIDE, UNICODE_RIGHT_TO_LEFT_OVERRIDE: exit(true);
    end;
    inc(p,charLen);
  end;
  exit(false);
end;

function UTF8OverrideDirection(const sUTF8: string; ARightToLeft: boolean): string;
begin
  if ARightToLeft then
    result := UnicodeCharToUTF8(UNICODE_RIGHT_TO_LEFT_OVERRIDE) + sUTF8 + UnicodeCharToUTF8(UNICODE_POP_DIRECTIONAL_FORMATTING)
  else
    result := UnicodeCharToUTF8(UNICODE_LEFT_TO_RIGHT_OVERRIDE) + sUTF8 + UnicodeCharToUTF8(UNICODE_POP_DIRECTIONAL_FORMATTING);
end;

function UTF8EmbedDirection(const sUTF8: string; ARightToLeft: boolean): string;
begin
  if ARightToLeft then
    result := UnicodeCharToUTF8(UNICODE_RIGHT_TO_LEFT_EMBEDDING) + sUTF8 + UnicodeCharToUTF8(UNICODE_POP_DIRECTIONAL_FORMATTING)
  else
    result := UnicodeCharToUTF8(UNICODE_LEFT_TO_RIGHT_EMBEDDING) + sUTF8 + UnicodeCharToUTF8(UNICODE_POP_DIRECTIONAL_FORMATTING);
end;

function UTF8Ligature(const sUTF8: string; ARightToLeft: boolean;
  ALigatureLeft, ALigatureRight: boolean): string;
begin
  result := sUTF8;
  if (ALigatureRight and ARightToLeft) or
     (ALigatureLeft and not ARightToLeft) then
     result := UTF8_ZERO_WIDTH_JOINER + result;
  if (ALigatureLeft and ARightToLeft) or
     (ALigatureRight and not ARightToLeft) then
     result := result + UTF8_ZERO_WIDTH_JOINER;
end;

//little endian stream functions
function LEReadInt64(Stream: TStream): int64;
begin
  Result := 0;
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

procedure LEWriteInt64(Stream: TStream; AValue: int64);
begin
  AValue := NtoLE(AValue);
  stream.Write(AValue, sizeof(AValue));
end;

function LEReadLongint(Stream: TStream): longint;
begin
  Result := 0;
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

procedure LEWriteLongint(Stream: TStream; AValue: LongInt);
begin
  AValue := NtoLE(AValue);
  stream.Write(AValue, sizeof(AValue));
end;

function LEReadByte(Stream: TStream): byte;
begin
  Result := 0;
  stream.Read(Result, sizeof(Result));
end;

procedure LEWriteByte(Stream: TStream; AValue: Byte);
begin
  stream.Write(AValue, sizeof(AValue));
end;

function LEReadSingle(Stream: TStream): single;
var
  ResultAsDWord : LongWord absolute result;
begin
  ResultAsDWord := 0;
  stream.Read(ResultAsDWord, sizeof(Result));
  ResultAsDWord := LEtoN(ResultAsDWord);
end;

procedure LEWriteSingle(Stream: TStream; AValue: single);
var
  ValueAsDWord : LongWord absolute AValue;
begin
  ValueAsDWord := NtoLE(ValueAsDWord);
  stream.Write(ValueAsDWord, sizeof(AValue));
end;

{ TGlyphUtf8 }

function TGlyphUtf8.GetEmpty: boolean;
begin
  result := GlyphUtf8 = '';
end;

{ TGlyphCursorUtf8 }

class function TGlyphCursorUtf8.New(const textUTF8: string; ABidiMode: TFontBidiMode): TGlyphCursorUtf8;
begin
  result.sUTF8 := textUTF8;
  result.bidiArray := AnalyzeBidiUTF8(result.sUTF8, ABidiMode);
  result.displayOrder := GetUTF8DisplayOrder(result.bidiArray);
  result.Rewind;
end;

function TGlyphCursorUtf8.GetNextGlyph: TGlyphUtf8;
var
  rtlScript, ligatureLeft, ligatureRight: Boolean;
  u: LongWord;
  bracketInfo: TUnicodeBracketInfo;
begin
  if EndOfString then
  begin
    result.GlyphUtf8:= '';
    result.RightToLeft:= false;
    result.Mirrored:= false;
    result.MirroredGlyphUtf8:= '';
    exit;
  end;
  PeekMultichar;
  NextMultichar;
  result.GlyphUtf8 := currentChar;
  result.RightToLeft := currentBidiInfo.IsRightToLeft;
  result.Mirrored := currentBidiInfo.IsMirrored;
  result.MirroredGlyphUtf8:= '';
  result.ByteOffset := currentOffset;
  result.ByteSize := length(currentChar);
  result.Merged:= false;
  if result.Mirrored then
  begin
    u := UTF8CodepointToUnicode(pchar(currentChar),
      min(UTF8CharacterLength(pchar(currentChar)), length(currentChar)));
    bracketInfo := GetUnicodeBracketInfo(u);
    if bracketInfo.OpeningBracket = u then
      result.MirroredGlyphUtf8 := UnicodeCharToUTF8(bracketInfo.ClosingBracket)
    else if bracketInfo.ClosingBracket = u then
      result.MirroredGlyphUtf8 := UnicodeCharToUTF8(bracketInfo.OpeningBracket);
  end else
  begin
    rtlScript := currentBidiInfo.IsRightToLeftScript;
    ligatureRight := currentBidiInfo.HasLigatureRight;
    ligatureLeft := currentBidiInfo.HasLigatureLeft;
    if (currentChar.StartsWith(UTF8_ARABIC_ALEPH) or
       currentChar.StartsWith(UTF8_ARABIC_ALEPH_HAMZA_BELOW) or
       currentChar.StartsWith(UTF8_ARABIC_ALEPH_HAMZA_ABOVE) or
       currentChar.StartsWith(UTF8_ARABIC_ALEPH_MADDA_ABOVE)) and
      not EndOfString then
    begin
      PeekMultichar;
      if currentChar.StartsWith(UTF8_ARABIC_LAM) then
      begin
        result.GlyphUtf8 := currentChar + result.GlyphUtf8;
        result.ByteOffset:= Min(result.ByteOffset, currentOffset);
        inc(result.ByteSize, length(currentChar));
        result.Merged := true;
        ligatureRight := currentBidiInfo.HasLigatureRight;
        NextMultichar;
      end;
    end;
    result.GlyphUtf8 := UTF8Ligature(result.GlyphUtf8, rtlScript, ligatureLeft, ligatureRight);
  end;
end;

procedure TGlyphCursorUtf8.Rewind;
begin
  displayIndex := 0;
  while (displayIndex < length(displayOrder))
    and not bidiArray[displayOrder[displayIndex]].BidiInfo.IsMulticharStart do
      inc(displayIndex);
end;

procedure TGlyphCursorUtf8.NextMultichar;
begin
  inc(displayIndex);
  while (displayIndex < length(displayOrder))
    and not bidiArray[displayOrder[displayIndex]].BidiInfo.IsMulticharStart do
      inc(displayIndex);
end;

procedure TGlyphCursorUtf8.PeekMultichar;
var
  startIndex, nextIndex, charLen, startOffset: Integer;
begin
  startIndex := displayOrder[displayIndex];
  startOffset := bidiArray[startIndex].Offset;
  currentBidiInfo := bidiArray[startIndex].BidiInfo;
  nextIndex := startIndex+1;
  while (nextIndex < length(bidiArray))
    and not bidiArray[nextIndex].BidiInfo.IsMulticharStart do
      inc(nextIndex);
  if nextIndex >= length(bidiArray) then
    charLen := length(sUTF8) - startOffset
  else
    charLen := bidiArray[nextIndex].Offset - startOffset;
  setlength(currentChar, charLen);
  if charLen > 0 then move(sUTF8[startOffset+1], currentChar[1], charLen);
  currentOffset := startOffset;
end;

function TGlyphCursorUtf8.EndOfString: boolean;
begin
  result := displayIndex >= length(displayOrder);
end;

end.

