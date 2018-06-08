unit BGRAUTF8;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

uses
  Classes, SysUtils{$IFDEF BGRABITMAP_USE_LCL}, lazutf8classes{$ENDIF};

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
    constructor Create(const AFileName: utf8string; Mode: Word);
    constructor Create(const AFileName: utf8string; Mode: Word; Rights: Cardinal);
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
function FileCreateUTF8(Const FileName : string; Rights: Cardinal) : THandle; overload;
function FileExistsUTF8(Const FileName : string): boolean;
function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
function FindNextUTF8(var Rslt: TSearchRec): Longint;
procedure FindCloseUTF8(var F: TSearchrec);

type
  string4 = string[4];

function UTF8CharacterLength(p: PChar): integer;
function UTF8Length(const s: string): PtrInt;
function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
function UnicodeCharToUTF8(u: cardinal): string4;

type
  TUnicodeBidiClass = (ubcBoundaryNeutral, ubcNeutral, ubcCommonSeparator, ubcNonSpacingMark,
                      ubcLeftToRight, ubcEuropeanNumber, ubcEuropeanNumberSeparator, ubcEuropeanNumberTerminator,
                      ubcRightToLeft, ubcArabicLetter, ubcArabicNumber, ubcUnknown);
  TUnicodeAnalysisElement = record
    u, paragraph, lineNumber: Cardinal;
    bidiClass: TUnicodeBidiClass;
    bidiLevel: byte;
    removed: boolean;       //RLE, LRE, RLO, LRO, PDF and BN are supposed to be removed
    prevInIsolate, nextInIsolate: integer; //next index in current isolate
  end;

  TUnicodeAnalysisArray = array of TUnicodeAnalysisElement;

const
  //maximum nesting level of isolates and bidi-formatting blocks
  UNICODE_MAX_BIDI_DEPTH = 125;

  UNICODE_LINE_SEPARATOR = $2028;      //equivalent of <br>
  UNICODE_PARAGRAPH_SEPARATOR = $2029; //equivalent of </p>
  UNICODE_NEXT_LINE = $0085;           //equivalent of CRLF

  //characters that split lines into top-level bidi blocks
  UNICODE_LEFT_TO_RIGHT_ISOLATE = $2066;
  UNICODE_RIGHT_TO_LEFT_ISOLATE = $2067;
  UNICODE_FIRST_STRONG_ISOLATE = $2068;
  UNICODE_POP_DIRECTIONAL_ISOLATE = $2069;

  //characters that split into bidi sub-blocks (called "formatting")
  UNICODE_LEFT_TO_RIGHT_EMBEDDING = $202A;
  UNICODE_RIGHT_TO_LEFT_EMBEDDING = $202B;
  UNICODE_LEFT_TO_RIGHT_OVERRIDE = $202D;
  UNICODE_RIGHT_TO_LEFT_OVERRIDE = $202E;
  UNICODE_POP_DIRECTIONAL_FORMATTING = $202C;

  //characters that mark direction without splitting the bidi block
  UNICODE_LEFT_TO_RIGHT_MARK = $200E;
  UNICODE_RIGHT_TO_LEFT_MARK = $200F;
  UNICODE_ARABIC_LETTER_MARK = $061C;

type
  TUnicodeBracketInfo = record
    IsBracket: boolean;
    OpeningBracket,ClosingBracket: cardinal;
  end;

function UTF8ReverseString(const s: string): string;
function UTF8CodepointToUnicode(p: PChar; ACodePointLen: integer): cardinal;
function GetUnicodeBidiClass(P: PChar): TUnicodeBidiClass;
function GetUnicodeBidiClass(u: cardinal): TUnicodeBidiClass;
function GetFirstStrongBidiClass(const sUTF8: string): TUnicodeBidiClass;
function GetLastStrongBidiClass(const sUTF8: string): TUnicodeBidiClass;
function GetUnicodeBracketInfo(u: cardinal): TUnicodeBracketInfo;
function IsZeroWidthString(const sUTF8: string): boolean;
function IsZeroWidthUnicode(u: cardinal): boolean;
function AddParagraphBidi(s: string; ARightToLeft: boolean): string;
function AnalyzeUnicode(const sUTF8: string; ARightToLeft: boolean): TUnicodeAnalysisArray; overload;
function AnalyzeUnicode(const sUTF8: string): TUnicodeAnalysisArray; overload;

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

function FileCreateUTF8(Const FileName : string; Rights: Cardinal) : THandle; overload;
begin
  result := LazFileUtils.FileCreateUTF8(FileName, Rights);
end;

function FileExistsUTF8(Const FileName : string): boolean;
begin
  result := LazFileUtils.FileExistsUTF8(FileName);
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

function UnicodeCharToUTF8(u: cardinal): string4;
begin
  result := LazUtf8.UnicodeToUTF8(u);
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

function FileCreateUTF8(const FileName: string; Rights: Cardinal): THandle;
begin
  result := FileCreate(UTF8ToSys(FileName),Rights);
end;

function FileExistsUTF8(const FileName: string): boolean;
begin
  result := FileExists(UTF8ToSys(FileName));
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

function UnicodeToUTF8Inline(CodePoint: cardinal; Buf: PChar): integer;
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

function UnicodeCharToUTF8(u: cardinal): string4;
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

constructor TFileStreamUTF8.Create(const AFileName: utf8string; Mode: Word; Rights: Cardinal);
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

function UTF8CodepointToUnicode(p: PChar; ACodePointLen: integer): cardinal;
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

function GetUnicodeBidiClass(P: PChar): TUnicodeBidiClass;
begin
  result := GetUnicodeBidiClass(UTF8CodepointToUnicode(P, UTF8CharacterLength(p)));
end;

function GetUnicodeBidiClass(u: cardinal): TUnicodeBidiClass;
begin //generated 06/06/2018
  case u of
  $00..$08, $0E..$1B, $7F..$84, $86..$9F, $AD, $180E, $200B..$200D, $2060..$2064,
  $206A..$206F, $FEFF, $1BCA0..$1BCA3, $1D173..$1D17A, $E0001, $E0020..$E007F: result := ubcBoundaryNeutral;
  $41..$5A, $61..$7A, $AA, $B5, $BA, $C0..$D6, $D8..$F6, $F8..$2B8, $2BB..$2C1,
  $2D0, $2D1, $2E0..$2E4, $2EE, $370..$373, $376, $377, $37A..$37D, $37F, $386,
  $388..$38A, $38C, $38E..$3A1, $3A3..$3F5, $3F7..$482, $48A..$52F, $531..$556,
  $559..$589: result := ubcLeftToRight;
  $5BE, $5C0, $5C3, $5C6, $5D0..$5EA, $5EF..$5F4: result := ubcRightToLeft;
  $608, $60B, $60D, $61B, $61C, $61E..$64A, $66D..$66F, $671..$6D5, $6E5, $6E6,
  $6EE, $6EF, $6FA..$70D, $70F, $710, $712..$72F, $74D..$7A5, $7B1: result := ubcArabicLetter;
  $7C0..$7EA, $7F4, $7F5, $7FA, $7FE..$815, $81A, $824, $828, $830..$83E, $840..$858,
  $85E: result := ubcRightToLeft;
  $860..$86A, $8A0..$8B4, $8B6..$8BD: result := ubcArabicLetter;
  $903..$939, $93B, $93D..$940, $949..$94C, $94E..$950, $958..$961, $964..$980,
  $982, $983, $985..$98C, $98F, $990, $993..$9A8, $9AA..$9B0, $9B2, $9B6..$9B9,
  $9BD..$9C0, $9C7, $9C8, $9CB, $9CC, $9CE, $9D7, $9DC, $9DD, $9DF..$9E1, $9E6..$9F1,
  $9F4..$9FA, $9FC, $9FD, $A03, $A05..$A0A, $A0F, $A10, $A13..$A28, $A2A..$A30,
  $A32, $A33, $A35, $A36, $A38, $A39, $A3E..$A40, $A59..$A5C, $A5E, $A66..$A6F,
  $A72..$A74, $A76, $A83, $A85..$A8D, $A8F..$A91, $A93..$AA8, $AAA..$AB0, $AB2,
  $AB3, $AB5..$AB9, $ABD..$AC0, $AC9, $ACB, $ACC, $AD0, $AE0, $AE1, $AE6..$AF0,
  $AF9, $B02, $B03, $B05..$B0C, $B0F, $B10, $B13..$B28, $B2A..$B30, $B32, $B33,
  $B35..$B39, $B3D, $B3E, $B40, $B47, $B48, $B4B, $B4C, $B57, $B5C, $B5D, $B5F..$B61,
  $B66..$B77, $B83, $B85..$B8A, $B8E..$B90, $B92..$B95, $B99, $B9A, $B9C, $B9E,
  $B9F, $BA3, $BA4, $BA8..$BAA, $BAE..$BB9, $BBE, $BBF, $BC1, $BC2, $BC6..$BC8,
  $BCA..$BCC, $BD0, $BD7, $BE6..$BF2, $C01..$C03, $C05..$C0C, $C0E..$C10, $C12..$C28,
  $C2A..$C39, $C3D, $C41..$C44, $C58..$C5A, $C60, $C61, $C66..$C6F, $C7F, $C80,
  $C82..$C8C, $C8E..$C90, $C92..$CA8, $CAA..$CB3, $CB5..$CB9, $CBD..$CC4, $CC6..$CC8,
  $CCA, $CCB, $CD5, $CD6, $CDE, $CE0, $CE1, $CE6..$CEF, $CF1, $CF2, $D02, $D03,
  $D05..$D0C, $D0E..$D10, $D12..$D3A, $D3D..$D40, $D46..$D48, $D4A..$D4C, $D4E,
  $D4F, $D54..$D61, $D66..$D7F, $D82, $D83, $D85..$D96, $D9A..$DB1, $DB3..$DBB,
  $DBD, $DC0..$DC6, $DCF..$DD1, $DD8..$DDF, $DE6..$DEF, $DF2..$DF4, $E01..$E30,
  $E32, $E33, $E40..$E46, $E4F..$E5B, $E81, $E82, $E84, $E87, $E88, $E8A, $E8D,
  $E94..$E97, $E99..$E9F, $EA1..$EA3, $EA5, $EA7, $EAA, $EAB, $EAD..$EB0, $EB2,
  $EB3, $EBD, $EC0..$EC4, $EC6, $ED0..$ED9, $EDC..$EDF, $F00..$F17, $F1A..$F34,
  $F36, $F38, $F3E..$F47, $F49..$F6C, $F7F, $F85, $F88..$F8C, $FBE..$FC5, $FC7..$FCC,
  $FCE..$FDA, $1000..$102C, $1031, $1038, $103B, $103C, $103F..$1057, $105A..$105D,
  $1061..$1070, $1075..$1081, $1083, $1084, $1087..$108C, $108E..$109C, $109E..$10C5,
  $10C7, $10CD, $10D0..$1248, $124A..$124D, $1250..$1256, $1258, $125A..$125D,
  $1260..$1288, $128A..$128D, $1290..$12B0, $12B2..$12B5, $12B8..$12BE, $12C0,
  $12C2..$12C5, $12C8..$12D6, $12D8..$1310, $1312..$1315, $1318..$135A, $1360..$137C,
  $1380..$138F, $13A0..$13F5, $13F8..$13FD, $1401..$167F, $1681..$169A, $16A0..$16F8,
  $1700..$170C, $170E..$1711, $1720..$1731, $1735, $1736, $1740..$1751, $1760..$176C,
  $176E..$1770, $1780..$17B3, $17B6, $17BE..$17C5, $17C7, $17C8, $17D4..$17DA,
  $17DC, $17E0..$17E9, $1810..$1819, $1820..$1878, $1880..$1884, $1887..$18A8,
  $18AA, $18B0..$18F5, $1900..$191E, $1923..$1926, $1929..$192B, $1930, $1931,
  $1933..$1938, $1946..$196D, $1970..$1974, $1980..$19AB, $19B0..$19C9, $19D0..$19DA,
  $1A00..$1A16, $1A19, $1A1A, $1A1E..$1A55, $1A57, $1A61, $1A63, $1A64, $1A6D..$1A72,
  $1A80..$1A89, $1A90..$1A99, $1AA0..$1AAD, $1B04..$1B33, $1B35, $1B3B, $1B3D..$1B41,
  $1B43..$1B4B, $1B50..$1B6A, $1B74..$1B7C, $1B82..$1BA1, $1BA6, $1BA7, $1BAA,
  $1BAE..$1BE5, $1BE7, $1BEA..$1BEC, $1BEE, $1BF2, $1BF3, $1BFC..$1C2B, $1C34,
  $1C35, $1C3B..$1C49, $1C4D..$1C88, $1C90..$1CBA, $1CBD..$1CC7, $1CD3, $1CE1,
  $1CE9..$1CEC, $1CEE..$1CF3, $1CF5..$1CF7, $1D00..$1DBF, $1E00..$1F15, $1F18..$1F1D,
  $1F20..$1F45, $1F48..$1F4D, $1F50..$1F57, $1F59, $1F5B, $1F5D, $1F5F..$1F7D,
  $1F80..$1FB4, $1FB6..$1FBC, $1FBE, $1FC2..$1FC4, $1FC6..$1FCC, $1FD0..$1FD3,
  $1FD6..$1FDB, $1FE0..$1FEC, $1FF2..$1FF4, $1FF6..$1FFC, $200E: result := ubcLeftToRight;
  $200F: result := ubcRightToLeft;
  $2071, $207F, $2090..$209C, $2102, $2107, $210A..$2113, $2115, $2119..$211D,
  $2124, $2126, $2128, $212A..$212D, $212F..$2139, $213C..$213F, $2145..$2149,
  $214E, $214F, $2160..$2188, $2336..$237A, $2395, $249C..$24E9, $26AC, $2800..$28FF,
  $2C00..$2C2E, $2C30..$2C5E, $2C60..$2CE4, $2CEB..$2CEE, $2CF2, $2CF3, $2D00..$2D25,
  $2D27, $2D2D, $2D30..$2D67, $2D6F, $2D70, $2D80..$2D96, $2DA0..$2DA6, $2DA8..$2DAE,
  $2DB0..$2DB6, $2DB8..$2DBE, $2DC0..$2DC6, $2DC8..$2DCE, $2DD0..$2DD6, $2DD8..$2DDE,
  $3005..$3007, $3021..$3029, $302E, $302F, $3031..$3035, $3038..$303C, $3041..$3096,
  $309D..$309F, $30A1..$30FA, $30FC..$30FF, $3105..$312F, $3131..$318E, $3190..$31BA,
  $31F0..$321C, $3220..$324F, $3260..$327B, $327F..$32B0, $32C0..$32CB, $32D0..$32FE,
  $3300..$3376, $337B..$33DD, $33E0..$33FE, $3400, $4DB5, $4E00, $9FEF, $A000..$A48C,
  $A4D0..$A60C, $A610..$A62B, $A640..$A66E, $A680..$A69D, $A6A0..$A6EF, $A6F2..$A6F7,
  $A722..$A787, $A789..$A7B9, $A7F7..$A801, $A803..$A805, $A807..$A80A, $A80C..$A824,
  $A827, $A830..$A837, $A840..$A873, $A880..$A8C3, $A8CE..$A8D9, $A8F2..$A8FE,
  $A900..$A925, $A92E..$A946, $A952, $A953, $A95F..$A97C, $A983..$A9B2, $A9B4,
  $A9B5, $A9BA, $A9BB, $A9BD..$A9CD, $A9CF..$A9D9, $A9DE..$A9E4, $A9E6..$A9FE,
  $AA00..$AA28, $AA2F, $AA30, $AA33, $AA34, $AA40..$AA42, $AA44..$AA4B, $AA4D,
  $AA50..$AA59, $AA5C..$AA7B, $AA7D..$AAAF, $AAB1, $AAB5, $AAB6, $AAB9..$AABD,
  $AAC0, $AAC2, $AADB..$AAEB, $AAEE..$AAF5, $AB01..$AB06, $AB09..$AB0E, $AB11..$AB16,
  $AB20..$AB26, $AB28..$AB2E, $AB30..$AB65, $AB70..$ABE4, $ABE6, $ABE7, $ABE9..$ABEC,
  $ABF0..$ABF9, $AC00, $D7A3, $D7B0..$D7C6, $D7CB..$D7FB, $D800, $DB7F, $DB80,
  $DBFF, $DC00, $DFFF, $E000, $F8FF..$FA6D, $FA70..$FAD9, $FB00..$FB06, $FB13..$FB17: result := ubcLeftToRight;
  $FB1D, $FB1F..$FB28, $FB2A..$FB36, $FB38..$FB3C, $FB3E, $FB40, $FB41, $FB43,
  $FB44, $FB46..$FB4F: result := ubcRightToLeft;
  $FB50..$FBC1, $FBD3..$FD3D, $FD50..$FD8F, $FD92..$FDC7, $FDF0..$FDFC, $FE70..$FE74,
  $FE76..$FEFC: result := ubcArabicLetter;
  $FF21..$FF3A, $FF41..$FF5A, $FF66..$FFBE, $FFC2..$FFC7, $FFCA..$FFCF, $FFD2..$FFD7,
  $FFDA..$FFDC, $10000..$1000B, $1000D..$10026, $10028..$1003A, $1003C, $1003D,
  $1003F..$1004D, $10050..$1005D, $10080..$100FA, $10100, $10102, $10107..$10133,
  $10137..$1013F, $1018D, $1018E, $101D0..$101FC, $10280..$1029C, $102A0..$102D0,
  $10300..$10323, $1032D..$1034A, $10350..$10375, $10380..$1039D, $1039F..$103C3,
  $103C8..$103D5, $10400..$1049D, $104A0..$104A9, $104B0..$104D3, $104D8..$104FB,
  $10500..$10527, $10530..$10563, $1056F, $10600..$10736, $10740..$10755, $10760..$10767: result := ubcLeftToRight;
  $10800..$10805, $10808, $1080A..$10835, $10837, $10838, $1083C, $1083F..$10855,
  $10857..$1089E, $108A7..$108AF, $108E0..$108F2, $108F4, $108F5, $108FB..$1091B,
  $10920..$10939, $1093F, $10980..$109B7, $109BC..$109CF, $109D2..$10A00, $10A10..$10A13,
  $10A15..$10A17, $10A19..$10A35, $10A40..$10A48, $10A50..$10A58, $10A60..$10A9F,
  $10AC0..$10AE4, $10AEB..$10AF6, $10B00..$10B35, $10B40..$10B55, $10B58..$10B72,
  $10B78..$10B91, $10B99..$10B9C, $10BA9..$10BAF, $10C00..$10C48, $10C80..$10CB2,
  $10CC0..$10CF2, $10CFA..$10CFF: result := ubcRightToLeft;
  $10D00..$10D23: result := ubcArabicLetter;
  $10F00..$10F27: result := ubcRightToLeft;
  $10F30..$10F45, $10F51..$10F59: result := ubcArabicLetter;
  $11000, $11002..$11037, $11047..$1104D, $11066..$1106F, $11082..$110B2, $110B7,
  $110B8, $110BB..$110C1, $110CD, $110D0..$110E8, $110F0..$110F9, $11103..$11126,
  $1112C, $11136..$11146, $11150..$11172, $11174..$11176, $11182..$111B5, $111BF..$111C8,
  $111CD, $111D0..$111DF, $111E1..$111F4, $11200..$11211, $11213..$1122E, $11232,
  $11233, $11235, $11238..$1123D, $11280..$11286, $11288, $1128A..$1128D, $1128F..$1129D,
  $1129F..$112A9, $112B0..$112DE, $112E0..$112E2, $112F0..$112F9, $11302, $11303,
  $11305..$1130C, $1130F, $11310, $11313..$11328, $1132A..$11330, $11332, $11333,
  $11335..$11339, $1133D..$1133F, $11341..$11344, $11347, $11348, $1134B..$1134D,
  $11350, $11357, $1135D..$11363, $11400..$11437, $11440, $11441, $11445, $11447..$11459,
  $1145B, $1145D, $11480..$114B2, $114B9, $114BB..$114BE, $114C1, $114C4..$114C7,
  $114D0..$114D9, $11580..$115B1, $115B8..$115BB, $115BE, $115C1..$115DB, $11600..$11632,
  $1163B, $1163C, $1163E, $11641..$11644, $11650..$11659, $11680..$116AA, $116AC,
  $116AE, $116AF, $116B6, $116C0..$116C9, $11700..$1171A, $11720, $11721, $11726,
  $11730..$1173F, $11800..$1182E, $11838, $1183B, $118A0..$118F2, $118FF, $11A00,
  $11A07, $11A08, $11A0B..$11A32, $11A39, $11A3A, $11A3F..$11A46, $11A50, $11A57,
  $11A58, $11A5C..$11A83, $11A86..$11A89, $11A97, $11A9A..$11AA2, $11AC0..$11AF8,
  $11C00..$11C08, $11C0A..$11C2F, $11C3E..$11C45, $11C50..$11C6C, $11C70..$11C8F,
  $11CA9, $11CB1, $11CB4, $11D00..$11D06, $11D08, $11D09, $11D0B..$11D30, $11D46,
  $11D50..$11D59, $11D60..$11D65, $11D67, $11D68, $11D6A..$11D8E, $11D93, $11D94,
  $11D96, $11D98, $11DA0..$11DA9, $11EE0..$11EF2, $11EF5..$11EF8, $12000..$12399,
  $12400..$1246E, $12470..$12474, $12480..$12543, $13000..$1342E, $14400..$14646,
  $16800..$16A38, $16A40..$16A5E, $16A60..$16A69, $16A6E, $16A6F, $16AD0..$16AED,
  $16AF5, $16B00..$16B2F, $16B37..$16B45, $16B50..$16B59, $16B5B..$16B61, $16B63..$16B77,
  $16B7D..$16B8F, $16E40..$16E9A, $16F00..$16F44, $16F50..$16F7E, $16F93..$16F9F,
  $16FE0, $16FE1, $17000, $187F1, $18800..$18AF2, $1B000..$1B11E, $1B170..$1B2FB,
  $1BC00..$1BC6A, $1BC70..$1BC7C, $1BC80..$1BC88, $1BC90..$1BC99, $1BC9C, $1BC9F,
  $1D000..$1D0F5, $1D100..$1D126, $1D129..$1D166, $1D16A..$1D172, $1D183, $1D184,
  $1D18C..$1D1A9, $1D1AE..$1D1E8, $1D2E0..$1D2F3, $1D360..$1D378, $1D400..$1D454,
  $1D456..$1D49C, $1D49E, $1D49F, $1D4A2, $1D4A5, $1D4A6, $1D4A9..$1D4AC, $1D4AE..$1D4B9,
  $1D4BB, $1D4BD..$1D4C3, $1D4C5..$1D505, $1D507..$1D50A, $1D50D..$1D514, $1D516..$1D51C,
  $1D51E..$1D539, $1D53B..$1D53E, $1D540..$1D544, $1D546, $1D54A..$1D550, $1D552..$1D6A5,
  $1D6A8..$1D6DA, $1D6DC..$1D714, $1D716..$1D74E, $1D750..$1D788, $1D78A..$1D7C2,
  $1D7C4..$1D7CB, $1D800..$1D9FF, $1DA37..$1DA3A, $1DA6D..$1DA74, $1DA76..$1DA83,
  $1DA85..$1DA8B: result := ubcLeftToRight;
  $1E800..$1E8C4, $1E8C7..$1E8CF, $1E900..$1E943, $1E950..$1E959, $1E95E, $1E95F: result := ubcRightToLeft;
  $1EC71..$1ECB4, $1EE00..$1EE03, $1EE05..$1EE1F, $1EE21, $1EE22, $1EE24, $1EE27,
  $1EE29..$1EE32, $1EE34..$1EE37, $1EE39, $1EE3B, $1EE42, $1EE47, $1EE49, $1EE4B,
  $1EE4D..$1EE4F, $1EE51, $1EE52, $1EE54, $1EE57, $1EE59, $1EE5B, $1EE5D, $1EE5F,
  $1EE61, $1EE62, $1EE64, $1EE67..$1EE6A, $1EE6C..$1EE72, $1EE74..$1EE77, $1EE79..$1EE7C,
  $1EE7E, $1EE80..$1EE89, $1EE8B..$1EE9B, $1EEA1..$1EEA3, $1EEA5..$1EEA9, $1EEAB..$1EEBB: result := ubcArabicLetter;
  $1F110..$1F12E, $1F130..$1F169, $1F170..$1F1AC, $1F1E6..$1F202, $1F210..$1F23B,
  $1F240..$1F248, $1F250, $1F251, $20000, $2A6D6, $2A700, $2B734, $2B740, $2B81D,
  $2B820, $2CEA1, $2CEB0, $2EBE0, $2F800..$2FA1D, $F0000, $FFFFD, $100000, $10FFFD: result := ubcLeftToRight;
  $30..$39, $B2, $B3, $B9, $6F0..$6F9, $2070, $2074..$2079, $2080..$2089, $2488..$249B,
  $FF10..$FF19, $102E1..$102FB, $1D7CE..$1D7FF, $1F100..$1F10A: result := ubcEuropeanNumber;
  $2B, $2D, $207A, $207B, $208A, $208B, $2212, $FB29, $FE62, $FE63, $FF0B, $FF0D: result := ubcEuropeanNumberSeparator;
  $23..$25, $A2..$A5, $B0, $B1, $58F, $609, $60A, $66A, $9F2, $9F3, $9FB, $AF1,
  $BF9, $E3F, $17DB, $2030..$2034, $20A0..$20BF, $212E, $2213, $A838, $A839,
  $FE5F, $FE69, $FE6A, $FF03..$FF05, $FFE0, $FFE1, $FFE5, $FFE6: result := ubcEuropeanNumberTerminator;
  $600..$605, $660..$669, $66B, $66C, $6DD, $8E2, $10D30..$10D39, $10E60..$10E7E: result := ubcArabicNumber;
  $2C, $2E, $2F, $3A, $A0: result := ubcCommonSeparator;
  $300..$36F, $483..$489, $591..$5BD, $5BF, $5C1, $5C2, $5C4, $5C5, $5C7: result := ubcNonSpacingMark;
  $60C: result := ubcCommonSeparator;
  $610..$61A, $64B..$65F, $670, $6D6..$6DC, $6DF..$6E4, $6E7, $6E8, $6EA..$6ED,
  $711, $730..$74A, $7A6..$7B0, $7EB..$7F3, $7FD, $816..$819, $81B..$823, $825..$827,
  $829..$82D, $859..$85B, $8D3..$8E1, $8E3..$902, $93A, $93C, $941..$948, $94D,
  $951..$957, $962, $963, $981, $9BC, $9C1..$9C4, $9CD, $9E2, $9E3, $9FE, $A01,
  $A02, $A3C, $A41, $A42, $A47, $A48, $A4B..$A4D, $A51, $A70, $A71, $A75, $A81,
  $A82, $ABC, $AC1..$AC5, $AC7, $AC8, $ACD, $AE2, $AE3, $AFA..$AFF, $B01, $B3C,
  $B3F, $B41..$B44, $B4D, $B56, $B62, $B63, $B82, $BC0, $BCD, $C00, $C04, $C3E..$C40,
  $C46..$C48, $C4A..$C4D, $C55, $C56, $C62, $C63, $C81, $CBC, $CCC, $CCD, $CE2,
  $CE3, $D00, $D01, $D3B, $D3C, $D41..$D44, $D4D, $D62, $D63, $DCA, $DD2..$DD4,
  $DD6, $E31, $E34..$E3A, $E47..$E4E, $EB1, $EB4..$EB9, $EBB, $EBC, $EC8..$ECD,
  $F18, $F19, $F35, $F37, $F39, $F71..$F7E, $F80..$F84, $F86, $F87, $F8D..$F97,
  $F99..$FBC, $FC6, $102D..$1030, $1032..$1037, $1039, $103A, $103D, $103E,
  $1058, $1059, $105E..$1060, $1071..$1074, $1082, $1085, $1086, $108D, $109D,
  $135D..$135F, $1712..$1714, $1732..$1734, $1752, $1753, $1772, $1773, $17B4,
  $17B5, $17B7..$17BD, $17C6, $17C9..$17D3, $17DD, $180B..$180D, $1885, $1886,
  $18A9, $1920..$1922, $1927, $1928, $1932, $1939..$193B, $1A17, $1A18, $1A1B,
  $1A56, $1A58..$1A5E, $1A60, $1A62, $1A65..$1A6C, $1A73..$1A7C, $1A7F, $1AB0..$1ABE,
  $1B00..$1B03, $1B34, $1B36..$1B3A, $1B3C, $1B42, $1B6B..$1B73, $1B80, $1B81,
  $1BA2..$1BA5, $1BA8, $1BA9, $1BAB..$1BAD, $1BE6, $1BE8, $1BE9, $1BED, $1BEF..$1BF1,
  $1C2C..$1C33, $1C36, $1C37, $1CD0..$1CD2, $1CD4..$1CE0, $1CE2..$1CE8, $1CED,
  $1CF4, $1CF8, $1CF9, $1DC0..$1DF9, $1DFB..$1DFF: result := ubcNonSpacingMark;
  $202F, $2044: result := ubcCommonSeparator;
  $20D0..$20F0, $2CEF..$2CF1, $2D7F, $2DE0..$2DFF, $302A..$302D, $3099, $309A,
  $A66F..$A672, $A674..$A67D, $A69E, $A69F, $A6F0, $A6F1, $A802, $A806, $A80B,
  $A825, $A826, $A8C4, $A8C5, $A8E0..$A8F1, $A8FF, $A926..$A92D, $A947..$A951,
  $A980..$A982, $A9B3, $A9B6..$A9B9, $A9BC, $A9E5, $AA29..$AA2E, $AA31, $AA32,
  $AA35, $AA36, $AA43, $AA4C, $AA7C, $AAB0, $AAB2..$AAB4, $AAB7, $AAB8, $AABE,
  $AABF, $AAC1, $AAEC, $AAED, $AAF6, $ABE5, $ABE8, $ABED, $FB1E, $FE00..$FE0F,
  $FE20..$FE2F: result := ubcNonSpacingMark;
  $FE50, $FE52, $FE55, $FF0C, $FF0E, $FF0F, $FF1A: result := ubcCommonSeparator;
  $101FD, $102E0, $10376..$1037A, $10A01..$10A03, $10A05, $10A06, $10A0C..$10A0F,
  $10A38..$10A3A, $10A3F, $10AE5, $10AE6, $10D24..$10D27, $10F46..$10F50, $11001,
  $11038..$11046, $1107F..$11081, $110B3..$110B6, $110B9, $110BA, $11100..$11102,
  $11127..$1112B, $1112D..$11134, $11173, $11180, $11181, $111B6..$111BE, $111C9..$111CC,
  $1122F..$11231, $11234, $11236, $11237, $1123E, $112DF, $112E3..$112EA, $11300,
  $11301, $1133B, $1133C, $11340, $11366..$1136C, $11370..$11374, $11438..$1143F,
  $11442..$11444, $11446, $1145E, $114B3..$114B8, $114BA, $114BF, $114C0, $114C2,
  $114C3, $115B2..$115B5, $115BC, $115BD, $115BF, $115C0, $115DC, $115DD, $11633..$1163A,
  $1163D, $1163F, $11640, $116AB, $116AD, $116B0..$116B5, $116B7, $1171D..$1171F,
  $11722..$11725, $11727..$1172B, $1182F..$11837, $11839, $1183A, $11A01..$11A06,
  $11A09, $11A0A, $11A33..$11A38, $11A3B..$11A3E, $11A47, $11A51..$11A56, $11A59..$11A5B,
  $11A8A..$11A96, $11A98, $11A99, $11C30..$11C36, $11C38..$11C3D, $11C92..$11CA7,
  $11CAA..$11CB0, $11CB2, $11CB3, $11CB5, $11CB6, $11D31..$11D36, $11D3A, $11D3C,
  $11D3D, $11D3F..$11D45, $11D47, $11D90, $11D91, $11D95, $11D97, $11EF3, $11EF4,
  $16AF0..$16AF4, $16B30..$16B36, $16F8F..$16F92, $1BC9D, $1BC9E, $1D167..$1D169,
  $1D17B..$1D182, $1D185..$1D18B, $1D1AA..$1D1AD, $1D242..$1D244, $1DA00..$1DA36,
  $1DA3B..$1DA6C, $1DA75, $1DA84, $1DA9B..$1DA9F, $1DAA1..$1DAAF, $1E000..$1E006,
  $1E008..$1E018, $1E01B..$1E021, $1E023, $1E024, $1E026..$1E02A, $1E8D0..$1E8D6,
  $1E944..$1E94A, $E0100..$E01EF: result := ubcNonSpacingMark;
  $09..$0D, $1C..$22, $26..$2A, $3B..$40, $5B..$60, $7B..$7E, $85, $A1, $A6..$A9,
  $AB, $AC, $AE, $AF, $B4, $B6..$B8, $BB..$BF, $D7, $F7, $2B9, $2BA, $2C2..$2CF,
  $2D2..$2DF, $2E5..$2ED, $2EF..$2FF, $374, $375, $37E, $384, $385, $387, $3F6,
  $58A, $58D, $58E, $606, $607, $60E, $60F, $6DE, $6E9, $7F6..$7F9, $BF3..$BF8,
  $BFA, $C78..$C7E, $F3A..$F3D, $1390..$1399, $1400, $1680, $169B, $169C, $17F0..$17F9,
  $1800..$180A, $1940, $1944, $1945, $19DE..$19FF, $1FBD, $1FBF..$1FC1, $1FCD..$1FCF,
  $1FDD..$1FDF, $1FED..$1FEF, $1FFD, $1FFE, $2000..$200A, $2010..$2029, $2035..$2043,
  $2045..$205F, $207C..$207E, $208C..$208E, $2100, $2101, $2103..$2106, $2108,
  $2109, $2114, $2116..$2118, $211E..$2123, $2125, $2127, $2129, $213A, $213B,
  $2140..$2144, $214A..$214D, $2150..$215F, $2189..$218B, $2190..$2211, $2214..$2335,
  $237B..$2394, $2396..$2426, $2440..$244A, $2460..$2487, $24EA..$26AB, $26AD..$27FF,
  $2900..$2B73, $2B76..$2B95, $2B98..$2BC8, $2BCA..$2BFE, $2CE5..$2CEA, $2CF9..$2CFF,
  $2E00..$2E4E, $2E80..$2E99, $2E9B..$2EF3, $2F00..$2FD5, $2FF0..$2FFB, $3000..$3004,
  $3008..$3020, $3030, $3036, $3037, $303D..$303F, $309B, $309C, $30A0, $30FB,
  $31C0..$31E3, $321D, $321E, $3250..$325F, $327C..$327E, $32B1..$32BF, $32CC..$32CF,
  $3377..$337A, $33DE, $33DF, $33FF, $4DC0..$4DFF, $A490..$A4C6, $A60D..$A60F,
  $A673, $A67E, $A67F, $A700..$A721, $A788, $A828..$A82B, $A874..$A877, $FD3E,
  $FD3F, $FDFD, $FE10..$FE19, $FE30..$FE4F, $FE51, $FE54, $FE56..$FE5E, $FE60,
  $FE61, $FE64..$FE66, $FE68, $FE6B, $FF01, $FF02, $FF06..$FF0A, $FF1B..$FF20,
  $FF3B..$FF40, $FF5B..$FF65, $FFE2..$FFE4, $FFE8..$FFEE, $FFF9..$FFFD, $10101,
  $10140..$1018C, $10190..$1019B, $101A0, $1091F, $10B39..$10B3F, $11052..$11065,
  $11660..$1166C, $1D200..$1D241, $1D245, $1D300..$1D356, $1D6DB, $1D715, $1D74F,
  $1D789, $1D7C3, $1EEF0, $1EEF1, $1F000..$1F02B, $1F030..$1F093, $1F0A0..$1F0AE,
  $1F0B1..$1F0BF, $1F0C1..$1F0CF, $1F0D1..$1F0F5, $1F10B, $1F10C, $1F12F, $1F16A,
  $1F16B, $1F260..$1F265, $1F300..$1F6D4, $1F6E0..$1F6EC, $1F6F0..$1F6F9, $1F700..$1F773,
  $1F780..$1F7D8, $1F800..$1F80B, $1F810..$1F847, $1F850..$1F859, $1F860..$1F887,
  $1F890..$1F8AD, $1F900..$1F90B, $1F910..$1F93E, $1F940..$1F970, $1F973..$1F976,
  $1F97A, $1F97C..$1F9A2, $1F9B0..$1F9B9, $1F9C0..$1F9C2, $1F9D0..$1F9FF, $1FA60..$1FA6D: result := ubcNeutral;
  else result := ubcUnknown;
  end;
end;

function GetUnicodeBracketInfo(u: cardinal): TUnicodeBracketInfo;
  procedure Bracket(AOpening,AClosing: cardinal);
  begin
    result.IsBracket := true;
    result.OpeningBracket := AOpening;
    result.ClosingBracket := AClosing;
  end;
begin
  case u of
  $0028, $0029: Bracket($0028, $0029);
  $005B, $005D: Bracket($005B, $005D);
  $007B, $007D: Bracket($007B, $007D);
  $0F3A, $0F3B: Bracket($0F3A, $0F3B);
  $0F3C, $0F3D: Bracket($0F3C, $0F3D);
  $169B, $169C: Bracket($169B, $169C);
  $2045, $2046: Bracket($2045, $2046);
  $207D, $207E: Bracket($207D, $207E);
  $208D, $208E: Bracket($208D, $208E);
  $2308, $2309: Bracket($2308, $2309);
  $230A, $230B: Bracket($230A, $230B);
  $2329, $232A: Bracket($2329, $232A);
  $2768, $2769: Bracket($2768, $2769);
  $276A, $276B: Bracket($276A, $276B);
  $276C, $276D: Bracket($276C, $276D);
  $276E, $276F: Bracket($276E, $276F);
  $2770, $2771: Bracket($2770, $2771);
  $2772, $2773: Bracket($2772, $2773);
  $2774, $2775: Bracket($2774, $2775);
  $27C5, $27C6: Bracket($27C5, $27C6);
  $27E6, $27E7: Bracket($27E6, $27E7);
  $27E8, $27E9: Bracket($27E8, $27E9);
  $27EA, $27EB: Bracket($27EA, $27EB);
  $27EC, $27ED: Bracket($27EC, $27ED);
  $27EE, $27EF: Bracket($27EE, $27EF);
  $2983, $2984: Bracket($2983, $2984);
  $2985, $2986: Bracket($2985, $2986);
  $2987, $2988: Bracket($2987, $2988);
  $2989, $298A: Bracket($2989, $298A);
  $298B, $298C: Bracket($298B, $298C);
  $298D, $2990: Bracket($298D, $2990);
  $298F, $298E: Bracket($298F, $298E);
  $2991, $2992: Bracket($2991, $2992);
  $2993, $2994: Bracket($2993, $2994);
  $2995, $2996: Bracket($2995, $2996);
  $2997, $2998: Bracket($2997, $2998);
  $29D8, $29D9: Bracket($29D8, $29D9);
  $29DA, $29DB: Bracket($29DA, $29DB);
  $29FC, $29FD: Bracket($29FC, $29FD);
  $2E22, $2E23: Bracket($2E22, $2E23);
  $2E24, $2E25: Bracket($2E24, $2E25);
  $2E26, $2E27: Bracket($2E26, $2E27);
  $2E28, $2E29: Bracket($2E28, $2E29);
  $3008, $3009: Bracket($3008, $3009);
  $300A, $300B: Bracket($300A, $300B);
  $300C, $300D: Bracket($300C, $300D);
  $300E, $300F: Bracket($300E, $300F);
  $3010, $3011: Bracket($3010, $3011);
  $3014, $3015: Bracket($3014, $3015);
  $3016, $3017: Bracket($3016, $3017);
  $3018, $3019: Bracket($3018, $3019);
  $301A, $301B: Bracket($301A, $301B);
  $FE59, $FE5A: Bracket($FE59, $FE5A);
  $FE5B, $FE5C: Bracket($FE5B, $FE5C);
  $FE5D, $FE5E: Bracket($FE5D, $FE5E);
  $FF08, $FF09: Bracket($FF08, $FF09);
  $FF3B, $FF3D: Bracket($FF3B, $FF3D);
  $FF5B, $FF5D: Bracket($FF5B, $FF5D);
  $FF5F, $FF60: Bracket($FF5F, $FF60);
  $FF62, $FF63: Bracket($FF62, $FF63);
  else
    begin
      result.IsBracket := false;
      result.OpeningBracket := 0;
      result.ClosingBracket := 0;
    end;
  end;
end;

function GetFirstStrongBidiClass(const sUTF8: string): TUnicodeBidiClass;
var
  p,pEnd: PChar;
  charLen: Integer;
  u: Cardinal;
  curBidi: TUnicodeBidiClass;
begin
  if sUTF8 = '' then exit(ubcUnknown);
  p := @sUTF8[1];
  pEnd := p + length(sUTF8);
  while p < pEnd do
  begin
    charLen := UTF8CharacterLength(p);
    if (charLen = 0) or (p+charLen > pEnd) then break;
    u := UTF8CodepointToUnicode(p, charLen);
    curBidi := GetUnicodeBidiClass(u);
    if curBidi in[ubcLeftToRight,ubcRightToLeft,ubcArabicLetter] then
      exit(curBidi);
    inc(p,charLen);
  end;
  exit(ubcUnknown);
end;

function GetLastStrongBidiClass(const sUTF8: string): TUnicodeBidiClass;
var
  pStart,p : PChar;
  curBidi: TUnicodeBidiClass;
begin
  if sUTF8 = '' then exit(ubcUnknown);
  pStart := @sUTF8[1];
  p := pStart + length(sUTF8);
  while p > pStart do
  begin
    dec(p);
    while (p > pStart) and (ord(p^) and $C0 = $80) do dec(p);
    curBidi := GetUnicodeBidiClass(p);
    if curBidi in[ubcLeftToRight,ubcRightToLeft,ubcArabicLetter] then
      exit(curBidi);
  end;
  exit(ubcUnknown);
end;

function IsZeroWidthString(const sUTF8: string): boolean;
var
  p,pEnd: PChar;
  charLen: Integer;
  u: Cardinal;
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

function IsZeroWidthUnicode(u: cardinal): boolean;
begin
  case u of
  $200B,$200C,$200D,$FEFF,UNICODE_LEFT_TO_RIGHT_MARK,UNICODE_RIGHT_TO_LEFT_MARK,$061C: result := true;
  else result := false;
  end;
end;

function AddParagraphBidi(s: string; ARightToLeft: boolean): string;
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
      paraRTL := GetFirstStrongBidiClass(para) in[ubcRightToLeft,ubcArabicLetter];
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

begin
  i := 1;
  curParaStart := 1;
  while i <= length(s) do
  begin
    if s[i] in[#13,#10] then
    begin
      CheckParagraph;
      //skip end of line
      inc(i);
      if (i <= length(s)) and (s[i] in[#13,#10]) and (s[i]<>s[i-1]) then inc(i);
      curParaStart := i;
    end else
      inc(i);
  end;
  CheckParagraph;
  result := s;
end;

function AnalyzeUnicode(const sUTF8: string; baseDirection: cardinal): TUnicodeAnalysisArray;
var
  a: TUnicodeAnalysisArray;

  procedure ResolveWeakTypes(startIndex, charCount: integer; startOfSequence, {%H-}endOfSequence: TUnicodeBidiClass);
  var
    curIndex,backIndex,endIndex: Integer;
    latestStrongClass, prevClass: TUnicodeBidiClass;
  begin
    endIndex := startIndex+charCount;
    //rules W1 and W2
    prevClass := startOfSequence;
    latestStrongClass:= prevClass;
    curIndex := startIndex;
    while curIndex < endIndex do
    begin
      if not a[curIndex].removed then
      begin
        case a[curIndex].bidiClass of
          ubcNonSpacingMark: a[curIndex].bidiClass:= prevClass;
          ubcEuropeanNumber: if latestStrongClass = ubcArabicLetter then a[curIndex].bidiClass:= ubcArabicNumber;
        end;
        case a[curIndex].u of
        UNICODE_LEFT_TO_RIGHT_ISOLATE,
        UNICODE_RIGHT_TO_LEFT_ISOLATE,
        UNICODE_FIRST_STRONG_ISOLATE,
        UNICODE_POP_DIRECTIONAL_ISOLATE: prevClass := ubcNeutral;
        else prevClass := a[curIndex].bidiClass;
        end;
        if prevClass in [ubcLeftToRight,ubcRightToLeft,ubcArabicLetter] then latestStrongClass:= prevClass;
      end;
      curIndex += 1;
    end;

    // rule W4 and W5
    prevClass := startOfSequence;
    curIndex := startIndex;
    while curIndex < endIndex do
    begin
      if not a[curIndex].removed then
      begin
        case a[curIndex].bidiClass of
          ubcArabicLetter: a[curIndex].bidiClass := ubcRightToLeft;
          ubcEuropeanNumber:
            begin
              backIndex := curIndex;
              while backIndex > startIndex do
              begin
                backIndex -= 1;
                if a[backIndex].removed then continue;
                if a[backIndex].bidiClass = ubcEuropeanNumberTerminator then
                  a[backIndex].bidiClass := ubcEuropeanNumber
                else break;
              end;
            end;
          ubcEuropeanNumberSeparator:
            if (prevClass = ubcEuropeanNumber) and (curIndex+1 < endIndex) and
              (a[curIndex+1].bidiClass = ubcEuropeanNumber) then
                a[curIndex].bidiClass:= ubcEuropeanNumber;
          ubcCommonSeparator:
            if (prevClass in[ubcEuropeanNumber,ubcArabicNumber]) and (curIndex+1 < endIndex) and
              (a[curIndex+1].bidiClass = prevClass) then
                a[curIndex].bidiClass:= prevClass;
          ubcEuropeanNumberTerminator:
            if prevClass = ubcEuropeanNumber then
              a[curIndex].bidiClass:= ubcEuropeanNumber;
        end;
        prevClass := a[curIndex].bidiClass;
      end;

      curIndex += 1;
    end;

    // rule W6 and W7
    curIndex := startIndex;
    latestStrongClass := startOfSequence;
    while curIndex < endIndex do
    begin
      if not a[curIndex].removed then
      begin
        case a[curIndex].bidiClass of
          ubcEuropeanNumberSeparator,ubcEuropeanNumberTerminator,ubcCommonSeparator: a[curIndex].bidiClass := ubcNeutral;
          ubcLeftToRight,ubcRightToLeft,ubcArabicLetter: latestStrongClass:= a[curIndex].bidiClass;
          ubcEuropeanNumber: if latestStrongClass = ubcLeftToRight then a[curIndex].bidiClass := ubcLeftToRight;
        end;
      end;
      curIndex += 1;
    end;
  end;

  procedure ResolveNeutrals(startIndex, charCount: integer; startOfSequence, endOfSequence: TUnicodeBidiClass);
  var
    endIndex,curIndex,previewIndex: Integer;
    curRTL, include, rightToLeft: Boolean;
    bidiClass: TUnicodeBidiClass;
  begin
    rightToLeft := startOfSequence in [ubcRightToLeft,ubcArabicLetter];
    endIndex := startIndex+charCount;
    curIndex := startIndex;
    curRTL := rightToLeft;
    while curIndex < endIndex do
    begin
      case a[curIndex].bidiClass of
        ubcLeftToRight: curRTL := false;
        ubcRightToLeft,ubcArabicLetter,ubcArabicNumber,ubcEuropeanNumber: curRTL := true;
      else
        if curRTL <> rightToLeft then
        begin
          //determine whether following neutral chars are included in reverse direction
          previewIndex := curIndex+1;
          include := false;
          while previewIndex <= endIndex do //uses endOfSequence for overflow
          begin
            if previewIndex = endIndex then
              bidiClass:= endOfSequence
            else
              bidiClass:= a[previewIndex].bidiClass;
            case bidiClass of
              ubcLeftToRight:
                begin
                  include := not curRTL;
                  break;
                end;
              ubcRightToLeft,ubcArabicLetter,ubcArabicNumber,ubcEuropeanNumber:
                begin
                  include := curRTL;
                  break;
                end;
            end;
            previewIndex += 1;
          end;
          if previewIndex = endIndex then previewIndex -= 1;
          if include then
          begin
            while curIndex <> previewIndex do
            begin
              if a[curIndex].bidiClass = ubcBoundaryNeutral then
                a[curIndex].removed := true; //supposed to be removed for rendering

              if a[curIndex].bidiClass in [ubcNeutral,ubcBoundaryNeutral,ubcUnknown] then
              begin
                if curRTL then a[curIndex].bidiClass := ubcRightToLeft
                else a[curIndex].bidiClass := ubcLeftToRight;
              end;

              curIndex += 1;
            end;
          end else
            curRTL := rightToLeft;
        end;
      end;

      if a[curIndex].bidiClass = ubcBoundaryNeutral then
        a[curIndex].removed := true; //supposed to be removed for rendering

      if a[curIndex].bidiClass in [ubcNeutral,ubcBoundaryNeutral,ubcUnknown] then
      begin
        if curRTL then a[curIndex].bidiClass := ubcRightToLeft
        else a[curIndex].bidiClass := ubcLeftToRight;
      end;

      curIndex += 1;
    end;
  end;

  procedure ResolveImplicitLevels(startIndex: integer); // rule I1 and I2
  var
    curIndex: Integer;
  begin
    curIndex := startIndex;
    while curIndex <> -1 do
    begin
      case a[curIndex].bidiClass of
      ubcRightToLeft,ubcArabicLetter:
        if not Odd(a[curIndex].bidiLevel) then a[curIndex].bidiLevel += 1;
      ubcEuropeanNumber,ubcArabicNumber:
        if Odd(a[curIndex].bidiLevel) then a[curIndex].bidiLevel += 1
        else a[curIndex].bidiLevel += 2;
      ubcLeftToRight: if Odd(a[curIndex].bidiLevel) then a[curIndex].bidiLevel += 1;
      end;
      curIndex := a[curIndex].nextInIsolate;
    end;
  end;

  //analyse bidi formatting of an embedding or an override block
  procedure AnalyzeFormattingBlocks(startIndex, lastIndex: integer; minBidiLevel: byte; formattingCode: cardinal);
  var curIndex, nextIndex, levelIncrease: integer;
    subFormatBeforeStart, subFormatStart, formatNesting: integer;
    subFormatCode: cardinal;
  begin
    case formattingCode of
    UNICODE_LEFT_TO_RIGHT_OVERRIDE,UNICODE_LEFT_TO_RIGHT_EMBEDDING:
      if odd(minBidiLevel) then minBidiLevel += 1;
    UNICODE_RIGHT_TO_LEFT_OVERRIDE,UNICODE_RIGHT_TO_LEFT_EMBEDDING:
      if not odd(minBidiLevel) then minBidiLevel += 1;
    end;
    nextIndex := startIndex;
    repeat
      Assert(nextIndex >= 0, 'Expecting valid index');
      curIndex := nextIndex;
      nextIndex := a[curIndex].nextInIsolate;
      a[curIndex].bidiLevel := minBidiLevel;

      //apply override
      if formattingCode = UNICODE_LEFT_TO_RIGHT_OVERRIDE then a[curIndex].bidiClass := ubcLeftToRight
      else if formattingCode = UNICODE_RIGHT_TO_LEFT_OVERRIDE then a[curIndex].bidiClass := ubcRightToLeft;

      case a[curIndex].u of
      UNICODE_LEFT_TO_RIGHT_EMBEDDING, UNICODE_RIGHT_TO_LEFT_EMBEDDING,
      UNICODE_LEFT_TO_RIGHT_OVERRIDE, UNICODE_RIGHT_TO_LEFT_OVERRIDE:
        begin
          a[curIndex].removed := true;
          case a[curIndex].u of
            UNICODE_LEFT_TO_RIGHT_OVERRIDE,UNICODE_LEFT_TO_RIGHT_EMBEDDING:
              if odd(minBidiLevel) then levelIncrease := 1
              else levelIncrease := 2;
            UNICODE_RIGHT_TO_LEFT_OVERRIDE,UNICODE_RIGHT_TO_LEFT_EMBEDDING:
              if odd(minBidiLevel) then levelIncrease := 2
              else levelIncrease := 1;
          end;
          if minBidiLevel <= UNICODE_MAX_BIDI_DEPTH-levelIncrease-1 then
          begin
            subFormatCode:= a[curIndex].u;
            subFormatBeforeStart := curIndex;
            subFormatStart := nextIndex;
            formatNesting:= 1;
            while formatNesting > 0 do
            begin
              //sub-format ends because no more chars
              if curIndex = lastIndex then
              begin
                if curIndex <> subFormatBeforeStart then
                  AnalyzeFormattingBlocks(subFormatStart, curIndex, minBidiLevel+levelIncrease, subFormatCode);
                break;
              end;

              Assert(nextIndex >= 0, 'Expecting valid index');
              case a[nextIndex].u of
              UNICODE_LEFT_TO_RIGHT_EMBEDDING, UNICODE_RIGHT_TO_LEFT_EMBEDDING,
              UNICODE_LEFT_TO_RIGHT_OVERRIDE, UNICODE_RIGHT_TO_LEFT_OVERRIDE: inc(formatNesting);
              UNICODE_POP_DIRECTIONAL_FORMATTING:
                begin
                  dec(formatNesting);
                  if formatNesting = 0 then
                  begin
                    //sub-format ends because enough matching pop chars found
                    if curIndex <> subFormatBeforeStart then
                      AnalyzeFormattingBlocks(subFormatStart, curIndex, minBidiLevel+levelIncrease, subFormatCode);

                    curIndex := nextIndex;
                    nextIndex := a[curIndex].nextInIsolate;
                    a[curIndex].removed := true;
                    break;
                  end;
                end;
              end;

              curIndex := nextIndex;
              nextIndex := a[curIndex].nextInIsolate;
            end;
          end;
        end;
      UNICODE_POP_DIRECTIONAL_FORMATTING: //ignored when no matching formatting code
        begin
          a[curIndex].removed := true;
        end;
      end;
    until curIndex = lastIndex;
  end;

  procedure ResolveBrackets(startIndex, charCount: integer; startOfSequence, {%H-}endOfSequence: TUnicodeBidiClass);
  type TBracketPair = record
                  openIndex,closeIndex: integer;
                end;
  var
    bracketPairs: array of TBracketPair;
    bracketPairCount: integer;
    endIndex: integer;
    rightToLeft: boolean;

    procedure SortBracketPairs;
    var
      i,j,k: Integer;
      temp: TBracketPair;
    begin
      for i := 1 to bracketPairCount-1 do
      begin
        for j := 0 to i-1 do
          if bracketPairs[j].openIndex > bracketPairs[i].openIndex then
          begin
            temp := bracketPairs[i];
            for k := i downto j+1 do
              bracketPairs[k] := bracketPairs[k-1];
            bracketPairs[j] := temp;
          end;
      end;
    end;

    procedure FindBrackets; // rule BD16
    const MAX_BRACKET_STACK = 63;
    var
      bracketStack: array[0..MAX_BRACKET_STACK-1] of record
          bracketCharInfo: TUnicodeBracketInfo;
          index: integer;
        end;
      bracketStackPos,peekPos: integer;
      curIndex: integer;
      curBracket: TUnicodeBracketInfo;
      prevLevel: byte;
    begin
      bracketPairCount := 0;
      bracketStackPos := 0;
      curIndex := startIndex;
      prevLevel := a[startIndex].bidiLevel;
      while curIndex < endIndex do
      begin
        if a[curIndex].bidiLevel <> prevLevel then bracketStackPos:= 0;
        curBracket := GetUnicodeBracketInfo(a[curIndex].u);
        if curBracket.IsBracket then
        begin
          // found opening bracket
          if curBracket.OpeningBracket = a[curIndex].u then
          begin
            if bracketStackPos <= high(bracketStack) then
            begin
              bracketStack[bracketStackPos].bracketCharInfo := curBracket;
              bracketStack[bracketStackPos].index := curIndex;
              bracketStackPos += 1;
            end else
              break;
          end else
          begin
            for peekPos := bracketStackPos-1 downto 0 do
              if bracketStack[peekPos].bracketCharInfo.ClosingBracket = a[curIndex].u then
              begin
                bracketStackPos := peekPos;
                if bracketPairCount >= length(bracketPairs) then
                  setlength(bracketPairs, bracketPairCount*2 + 8);
                bracketPairs[bracketPairCount].openIndex := bracketStack[peekPos].index;
                bracketPairs[bracketPairCount].closeIndex := curIndex;
                inc(bracketPairCount);
                break;
              end;
          end;
        end;
        curIndex += 1;
      end;
    end;

    procedure SetCharClass(index: integer; newClass: TUnicodeBidiClass);
    begin
      a[index].bidiClass:= newClass;
      while (index+1 < endIndex) and (GetUnicodeBidiClass(a[index+1].u) = ubcNonSpacingMark) do
      begin
        inc(index);
        a[index].bidiClass := newClass;
      end;
    end;

    procedure ResolveBrackets; // rule N0
    var
      i, curIndex: Integer;
      sameDirection, oppositeDirection, oppositeContext: boolean;
    begin
      for i := 0 to bracketPairCount-1 do
      begin
        curIndex := bracketPairs[i].openIndex+1;
        sameDirection:= false;
        oppositeDirection:= false;
        while curIndex <> bracketPairs[i].closeIndex do
        begin
          Assert((curIndex >= startIndex) and (curIndex < endIndex), 'Expecting valid index');
          case a[curIndex].bidiClass of
          ubcLeftToRight:
            if not rightToLeft then
            begin
              sameDirection := true;
              break;
            end else oppositeDirection:= true;
          ubcRightToLeft,ubcArabicLetter,ubcEuropeanNumber,ubcArabicNumber:
            if rightToLeft then
            begin
              sameDirection := true;
              break;
            end else oppositeDirection:= true;
          end;
          curIndex += 1;
        end;
        if sameDirection then
        begin
          if rightToLeft then
          begin
            SetCharClass(bracketPairs[i].openIndex, ubcRightToLeft);
            SetCharClass(bracketPairs[i].closeIndex, ubcRightToLeft);
          end else
          begin
            SetCharClass(bracketPairs[i].openIndex, ubcLeftToRight);
            SetCharClass(bracketPairs[i].closeIndex, ubcLeftToRight);
          end;
        end else
        if oppositeDirection then
        begin
          curIndex := bracketPairs[i].openIndex-1;
          oppositeContext := false;
          while curIndex >= startIndex do
          begin
            case a[curIndex].bidiClass of
            ubcRightToLeft,ubcArabicLetter,ubcEuropeanNumber,ubcArabicNumber:
              begin
                oppositeContext := not rightToLeft;
                break;
              end;
            ubcLeftToRight:
              begin
                oppositeContext := rightToLeft;
                break;
              end;
            end;
            curIndex -= 1;
          end;
          if rightToLeft xor oppositeContext then
          begin
            SetCharClass(bracketPairs[i].openIndex, ubcRightToLeft);
            SetCharClass(bracketPairs[i].closeIndex, ubcRightToLeft);
          end else
          begin
            SetCharClass(bracketPairs[i].openIndex, ubcLeftToRight);
            SetCharClass(bracketPairs[i].closeIndex, ubcLeftToRight);
          end;
        end;
      end;
    end;

  begin
    rightToLeft:= startOfSequence in[ubcRightToLeft,ubcArabicLetter];
    endIndex := startIndex+charCount;
    FindBrackets;
    SortBracketPairs;
    ResolveBrackets;
  end;

  procedure AnalyzeSequence(startIndex, charCount: integer; sos, eos: TUnicodeBidiClass);
  begin
    if charCount = 0 then exit;
    ResolveWeakTypes(startIndex, charCount, sos, eos);
    ResolveBrackets(startIndex, charCount, sos, eos);
    ResolveNeutrals(startIndex, charCount, sos, eos);
  end;

  procedure SameLevelRuns(startIndex: integer);
  var
    curBidiLevel: byte;
    latestIndex,curIndex, curStartIndex: Integer;
    curSos,eos: TUnicodeBidiClass;
  begin
    curIndex := startIndex;
    while (curIndex<>-1) and a[curIndex].removed do
      curIndex := a[curIndex].nextInIsolate;
    if curIndex = -1 then exit;

    curStartIndex:= curIndex;
    curBidiLevel := a[curIndex].bidiLevel;
    if odd(curBidiLevel) then curSos := ubcRightToLeft else curSos := ubcLeftToRight;
    latestIndex := -1;
    while curIndex <> -1 do
    begin
      if not a[curIndex].removed then
      begin
        if (latestIndex <> -1) and (a[curIndex].bidiLevel <> curBidiLevel) then
        begin
          if a[curIndex].bidiLevel > curBidiLevel then
          begin
            if odd(a[curIndex].bidiLevel) then eos := ubcRightToLeft else eos := ubcLeftToRight;
          end else
          begin
            if odd(curBidiLevel) then eos := ubcRightToLeft else eos := ubcLeftToRight;
          end;

          AnalyzeSequence(curStartIndex, latestIndex-curStartIndex+1, curSos, eos);

          curSos := eos;
          curBidiLevel:= a[curIndex].bidiLevel;
          curStartIndex:= curIndex;
        end;
        latestIndex := curIndex;
      end;

      if (a[curIndex].nextInIsolate <> curIndex+1) and (latestIndex<>-1) then
      begin
        if odd(a[latestIndex].bidiLevel) then eos := ubcRightToLeft else eos := ubcLeftToRight;
        AnalyzeSequence(curStartIndex, curIndex-curStartIndex+1, curSos, eos);

        curIndex := a[curIndex].nextInIsolate;
        while (curIndex<>-1) and a[curIndex].removed do
          curIndex := a[curIndex].nextInIsolate;
        if curIndex = -1 then break;

        curSos := eos;
        curBidiLevel := a[curIndex].bidiLevel;
        curStartIndex := curIndex;
        latestIndex:= -1;
        continue;
      end;

      curIndex := a[curIndex].nextInIsolate;
    end;
  end;

  function DetermineIsolateDirectionFromFirstStrongClass(startIndex: integer): cardinal;
  var
    curIndex: Integer;
    firstStrongClass: TUnicodeBidiClass;
  begin
    curIndex := startIndex;
    firstStrongClass := ubcUnknown;
    while curIndex <> -1 do
    begin
      Assert(curIndex >= 0, 'Expecting valid index');
      if firstStrongClass = ubcUnknown then
      begin
        if a[curIndex].bidiClass in [ubcLeftToRight,ubcRightToLeft,ubcArabicLetter] then
        begin
          firstStrongClass := a[curIndex].bidiClass;
          break;
        end;
      end;
      curIndex := a[curIndex].nextInIsolate;
    end;

    if firstStrongClass in[ubcRightToLeft,ubcArabicLetter] then
      result := UNICODE_RIGHT_TO_LEFT_ISOLATE
    else
      result := UNICODE_LEFT_TO_RIGHT_ISOLATE;
  end;

  procedure LinkCharsInIsolate(startIndex: integer; charCount: integer; out endIndex : integer);
  var
    curIndex,isolateStackPos,
    prevIndex: Integer;
  begin
    a[startIndex].prevInIsolate := -1;
    prevIndex := -1;
    curIndex := startIndex;
    isolateStackPos:= 0;
    while curIndex < startIndex+charCount do
    begin
      if a[curIndex].u = UNICODE_POP_DIRECTIONAL_ISOLATE then
        if isolateStackPos > 0 then dec(isolateStackPos);

      if isolateStackPos = 0 then
      begin
        if prevIndex<>-1 then a[prevIndex].nextInIsolate := curIndex;
        a[curIndex].prevInIsolate := prevIndex;

        prevIndex := curIndex;
      end;

      case a[curIndex].u of
      UNICODE_LEFT_TO_RIGHT_ISOLATE, UNICODE_RIGHT_TO_LEFT_ISOLATE, UNICODE_FIRST_STRONG_ISOLATE: inc(isolateStackPos);
      end;
      inc(curIndex);
    end;
    a[prevIndex].nextInIsolate := -1;
    endIndex := prevIndex;
  end;

  //split isolates in order to format them independently
  procedure AnalyzeIsolates(startIndex: integer; charCount: integer; isolateDirection: cardinal; minBidiLevel: byte = 0);
  var curIndex, endIndex: integer;
    nextIndex: integer;
    subBidiLevel, levelIncrease: byte;
    subIsolateStart: integer;
    subIsolateDirection: cardinal;
  begin
    if charCount = 0 then exit;
    Assert(startIndex>=0, 'Invalid start index');

    LinkCharsInIsolate(startIndex, charCount, endIndex);

    if isolateDirection = UNICODE_FIRST_STRONG_ISOLATE then
      isolateDirection := DetermineIsolateDirectionFromFirstStrongClass(startIndex);

    if (isolateDirection = UNICODE_LEFT_TO_RIGHT_ISOLATE) and Odd(minBidiLevel) then minBidiLevel += 1;
    if (isolateDirection = UNICODE_RIGHT_TO_LEFT_ISOLATE) and not Odd(minBidiLevel) then minBidiLevel += 1;

    case isolateDirection of
    UNICODE_LEFT_TO_RIGHT_ISOLATE: AnalyzeFormattingBlocks(startIndex, endIndex, minBidiLevel, UNICODE_LEFT_TO_RIGHT_EMBEDDING);
    UNICODE_RIGHT_TO_LEFT_ISOLATE: AnalyzeFormattingBlocks(startIndex, endIndex, minBidiLevel, UNICODE_RIGHT_TO_LEFT_EMBEDDING);
    end;

    SameLevelRuns(startIndex);
    ResolveImplicitLevels(startIndex);

    //analyse sub-isolates
    curIndex := startIndex;
    while curIndex <> -1 do
    begin
      Assert(curIndex >= 0, 'Expecting valid index');
      case a[curIndex].u of
      UNICODE_LEFT_TO_RIGHT_ISOLATE, UNICODE_RIGHT_TO_LEFT_ISOLATE, UNICODE_FIRST_STRONG_ISOLATE:
        begin
          subBidiLevel := a[curIndex].bidiLevel;
          nextIndex := a[curIndex].nextInIsolate;
          if nextIndex <> -1 then
          begin
            if a[nextIndex].bidiLevel > subBidiLevel then
              subBidiLevel:= a[nextIndex].bidiLevel;
          end;
          if ((isolateDirection = UNICODE_LEFT_TO_RIGHT_ISOLATE) and
             (a[curIndex].u = UNICODE_RIGHT_TO_LEFT_ISOLATE)) or
             ((isolateDirection = UNICODE_LEFT_TO_RIGHT_ISOLATE) and
             (a[curIndex].u = UNICODE_RIGHT_TO_LEFT_ISOLATE)) then
            levelIncrease := 1
          else
            levelIncrease:= 2;
          if subBidiLevel+levelIncrease <= UNICODE_MAX_BIDI_DEPTH-1 then
          begin
            subIsolateDirection := a[curIndex].u;
            subIsolateStart:= curIndex+1;
            curIndex := nextIndex;

            //sub-isolates ends because no more chars
            if curIndex = -1 then
            begin
              AnalyzeIsolates(subIsolateStart, startIndex+charCount-subIsolateStart, subIsolateDirection, subBidiLevel+1);
              break;
            end else
            begin
              AnalyzeIsolates(subIsolateStart, curIndex-subIsolateStart, subIsolateDirection, subBidiLevel+1);
              continue;
            end;
          end;
        end;
      end;
      curIndex := a[curIndex].nextInIsolate;
    end;
  end;

  //split UTF8 string into paragraphs and lines
  procedure SplitParagraphs(startIndex: integer; startChar: PChar; charCount: integer);
  var
    u: cardinal;
    charLen: integer;
    paragraph, lineNumber: cardinal;

    lineStartIndex, curIndex, endIndex: integer;
    curChar: PChar;
  begin
    curChar := startChar;
    curIndex := startIndex;
    endIndex := startIndex+charCount;
    paragraph := 0;
    lineNumber := 0;
    lineStartIndex := curIndex;
    while curIndex < endIndex do
    begin
      charLen := UTF8CharacterLength(curChar);
      u := UTF8CodepointToUnicode(curChar, charLen);

      a[curIndex].u := u;
      a[curIndex].bidiClass := GetUnicodeBidiClass(u);
      a[curIndex].lineNumber:= lineNumber;
      a[curIndex].paragraph := paragraph;
      case u of
      UNICODE_LINE_SEPARATOR:      //line separator within paragraph
        begin
          AnalyzeIsolates(lineStartIndex, curIndex+1-lineStartIndex, baseDirection);
          inc(lineNumber);
          lineStartIndex := curIndex+1;
        end;
      UNICODE_PARAGRAPH_SEPARATOR: //separator between paragraphs
        begin
          AnalyzeIsolates(lineStartIndex, curIndex+1-lineStartIndex, baseDirection);
          inc(paragraph);
          lineNumber := 0;
          lineStartIndex := curIndex+1;
        end;
      13,10,UNICODE_NEXT_LINE:     //system line ending
        begin
          //skip second CRLF char
          if (u in[13,10]) and (curIndex+1 < endIndex) and ((curChar+1)^ in [#13,#10]) and (curChar^ <> (curChar+1)^) then
          begin
            curChar += charLen;
            inc(curIndex);

            charLen := 1;
            u := ord(curChar^);

            a[curIndex].u := u;
            a[curIndex].lineNumber:= lineNumber;
            a[curIndex].paragraph := paragraph;
          end;
          AnalyzeIsolates(lineStartIndex, curIndex+1-lineStartIndex, baseDirection);

          //consider system line ending to be a paragraph separator
          inc(paragraph);
          lineNumber := 0;
          lineStartIndex := curIndex+1;
        end;
      end;

      curChar += charLen;
      inc(curIndex);
    end;
    if curIndex > lineStartIndex then
      AnalyzeIsolates(lineStartIndex, curIndex-lineStartIndex, baseDirection);
  end;

  function DetermineUTF8Length: integer;
  var pos,charLen: integer;
  begin
    result := 0;
    pos := 1;
    while pos <= length(sUTF8) do
    begin
      charLen := UTF8CharacterLength(@sUTF8[pos]);
      inc(pos,charLen);
      inc(result);
    end;
  end;

var len: integer;
begin
  len := DetermineUTF8Length;
  setlength(a, len);
  if len > 0 then
    SplitParagraphs(0, @sUTF8[1], len);

  result := a;
end;

function AnalyzeUnicode(const sUTF8: string; ARightToLeft: boolean): TUnicodeAnalysisArray;
begin
  if ARightToLeft then
    result := AnalyzeUnicode(sUTF8, UNICODE_RIGHT_TO_LEFT_ISOLATE)
  else
    result := AnalyzeUnicode(sUTF8, UNICODE_LEFT_TO_RIGHT_ISOLATE);
end;

function AnalyzeUnicode(const sUTF8: string): TUnicodeAnalysisArray;
begin
   result := AnalyzeUnicode(sUTF8, UNICODE_FIRST_STRONG_ISOLATE);
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
  ResultAsDWord : longword absolute result;
begin
  ResultAsDWord := 0;
  stream.Read(ResultAsDWord, sizeof(Result));
  ResultAsDWord := LEtoN(ResultAsDWord);
end;

procedure LEWriteSingle(Stream: TStream; AValue: single);
var
  ValueAsDWord : longword absolute AValue;
begin
  ValueAsDWord := NtoLE(ValueAsDWord);
  stream.Write(ValueAsDWord, sizeof(AValue));
end;

end.

