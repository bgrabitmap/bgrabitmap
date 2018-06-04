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
  TUnicodeBidiClass = (ubcLeftToRight, ubcRightToLeft, ubcArabicLetter, ubcUnknown);

function UTF8ReverseString(const s: string): string;
function UTF8CodepointToUnicode(p: PChar; ACodePointLen: integer): cardinal;
function GetUnicodeBidiClass(P: PChar): TUnicodeBidiClass;
function GetUnicodeBidiClass(u: cardinal): TUnicodeBidiClass;
function GetFirstStrongBidiClass(const sUTF8: string): TUnicodeBidiClass;
function GetLastStrongBidiClass(const sUTF8: string): TUnicodeBidiClass;
function IsZeroWidthString(const sUTF8: string): boolean;
function IsZeroWidthUnicode(u: cardinal): boolean;

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
begin //generated 16/05/2018
  case u of
  $41..$5A, $61..$7A, $AA, $B5, $BA, $C0..$D6, $D8..$F6, $F8..$2B8, $2BB..$2C1,
  $2D0, $2D1, $2E0..$2E4, $2EE, $370..$373, $376, $377, $37A..$37D, $37F, $386,
  $388..$38A, $38C, $38E..$3A1, $3A3..$3F5, $3F7..$482, $48A..$52F, $531..$556,
  $559..$55F, $561..$587, $589: result := ubcLeftToRight;
  $5BE, $5C0, $5C3, $5C6, $5D0..$5EA, $5F0..$5F4: result := ubcRightToLeft;
  $608, $60B, $60D, $61B, $61C, $61E..$64A, $66D..$66F, $671..$6D5, $6E5, $6E6,
  $6EE, $6EF, $6FA..$70D, $70F, $710, $712..$72F, $74D..$7A5, $7B1: result := ubcArabicLetter;
  $7C0..$7EA, $7F4, $7F5, $7FA, $800..$815, $81A, $824, $828, $830..$83E, $840..$858,
  $85E: result := ubcRightToLeft;
  $860..$86A, $8A0..$8B4, $8B6..$8BD: result := ubcArabicLetter;
  $903..$939, $93B, $93D..$940, $949..$94C, $94E..$950, $958..$961, $964..$980,
  $982, $983, $985..$98C, $98F, $990, $993..$9A8, $9AA..$9B0, $9B2, $9B6..$9B9,
  $9BD..$9C0, $9C7, $9C8, $9CB, $9CC, $9CE, $9D7, $9DC, $9DD, $9DF..$9E1, $9E6..$9F1,
  $9F4..$9FA, $9FC, $9FD, $A03, $A05..$A0A, $A0F, $A10, $A13..$A28, $A2A..$A30,
  $A32, $A33, $A35, $A36, $A38, $A39, $A3E..$A40, $A59..$A5C, $A5E, $A66..$A6F,
  $A72..$A74, $A83, $A85..$A8D, $A8F..$A91, $A93..$AA8, $AAA..$AB0, $AB2, $AB3,
  $AB5..$AB9, $ABD..$AC0, $AC9, $ACB, $ACC, $AD0, $AE0, $AE1, $AE6..$AF0, $AF9,
  $B02, $B03, $B05..$B0C, $B0F, $B10, $B13..$B28, $B2A..$B30, $B32, $B33, $B35..$B39,
  $B3D, $B3E, $B40, $B47, $B48, $B4B, $B4C, $B57, $B5C, $B5D, $B5F..$B61, $B66..$B77,
  $B83, $B85..$B8A, $B8E..$B90, $B92..$B95, $B99, $B9A, $B9C, $B9E, $B9F, $BA3,
  $BA4, $BA8..$BAA, $BAE..$BB9, $BBE, $BBF, $BC1, $BC2, $BC6..$BC8, $BCA..$BCC,
  $BD0, $BD7, $BE6..$BF2, $C01..$C03, $C05..$C0C, $C0E..$C10, $C12..$C28, $C2A..$C39,
  $C3D, $C41..$C44, $C58..$C5A, $C60, $C61, $C66..$C6F, $C7F, $C80, $C82, $C83,
  $C85..$C8C, $C8E..$C90, $C92..$CA8, $CAA..$CB3, $CB5..$CB9, $CBD..$CC4, $CC6..$CC8,
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
  $17DC, $17E0..$17E9, $1810..$1819, $1820..$1877, $1880..$1884, $1887..$18A8,
  $18AA, $18B0..$18F5, $1900..$191E, $1923..$1926, $1929..$192B, $1930, $1931,
  $1933..$1938, $1946..$196D, $1970..$1974, $1980..$19AB, $19B0..$19C9, $19D0..$19DA,
  $1A00..$1A16, $1A19, $1A1A, $1A1E..$1A55, $1A57, $1A61, $1A63, $1A64, $1A6D..$1A72,
  $1A80..$1A89, $1A90..$1A99, $1AA0..$1AAD, $1B04..$1B33, $1B35, $1B3B, $1B3D..$1B41,
  $1B43..$1B4B, $1B50..$1B6A, $1B74..$1B7C, $1B82..$1BA1, $1BA6, $1BA7, $1BAA,
  $1BAE..$1BE5, $1BE7, $1BEA..$1BEC, $1BEE, $1BF2, $1BF3, $1BFC..$1C2B, $1C34,
  $1C35, $1C3B..$1C49, $1C4D..$1C88, $1CC0..$1CC7, $1CD3, $1CE1, $1CE9..$1CEC,
  $1CEE..$1CF3, $1CF5..$1CF7, $1D00..$1DBF, $1E00..$1F15, $1F18..$1F1D, $1F20..$1F45,
  $1F48..$1F4D, $1F50..$1F57, $1F59, $1F5B, $1F5D, $1F5F..$1F7D, $1F80..$1FB4,
  $1FB6..$1FBC, $1FBE, $1FC2..$1FC4, $1FC6..$1FCC, $1FD0..$1FD3, $1FD6..$1FDB,
  $1FE0..$1FEC, $1FF2..$1FF4, $1FF6..$1FFC, $200E: result := ubcLeftToRight;
  $200F: result := ubcRightToLeft;
  $2071, $207F, $2090..$209C, $2102, $2107, $210A..$2113, $2115, $2119..$211D,
  $2124, $2126, $2128, $212A..$212D, $212F..$2139, $213C..$213F, $2145..$2149,
  $214E, $214F, $2160..$2188, $2336..$237A, $2395, $249C..$24E9, $26AC, $2800..$28FF,
  $2C00..$2C2E, $2C30..$2C5E, $2C60..$2CE4, $2CEB..$2CEE, $2CF2, $2CF3, $2D00..$2D25,
  $2D27, $2D2D, $2D30..$2D67, $2D6F, $2D70, $2D80..$2D96, $2DA0..$2DA6, $2DA8..$2DAE,
  $2DB0..$2DB6, $2DB8..$2DBE, $2DC0..$2DC6, $2DC8..$2DCE, $2DD0..$2DD6, $2DD8..$2DDE,
  $3005..$3007, $3021..$3029, $302E, $302F, $3031..$3035, $3038..$303C, $3041..$3096,
  $309D..$309F, $30A1..$30FA, $30FC..$30FF, $3105..$312E, $3131..$318E, $3190..$31BA,
  $31F0..$321C, $3220..$324F, $3260..$327B, $327F..$32B0, $32C0..$32CB, $32D0..$32FE,
  $3300..$3376, $337B..$33DD, $33E0..$33FE, $3400, $4DB5, $4E00, $9FEA, $A000..$A48C,
  $A4D0..$A60C, $A610..$A62B, $A640..$A66E, $A680..$A69D, $A6A0..$A6EF, $A6F2..$A6F7,
  $A722..$A787, $A789..$A7AE, $A7B0..$A7B7, $A7F7..$A801, $A803..$A805, $A807..$A80A,
  $A80C..$A824, $A827, $A830..$A837, $A840..$A873, $A880..$A8C3, $A8CE..$A8D9,
  $A8F2..$A8FD, $A900..$A925, $A92E..$A946, $A952, $A953, $A95F..$A97C, $A983..$A9B2,
  $A9B4, $A9B5, $A9BA, $A9BB, $A9BD..$A9CD, $A9CF..$A9D9, $A9DE..$A9E4, $A9E6..$A9FE,
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
  $10A15..$10A17, $10A19..$10A33, $10A40..$10A47, $10A50..$10A58, $10A60..$10A9F,
  $10AC0..$10AE4, $10AEB..$10AF6, $10B00..$10B35, $10B40..$10B55, $10B58..$10B72,
  $10B78..$10B91, $10B99..$10B9C, $10BA9..$10BAF, $10C00..$10C48, $10C80..$10CB2,
  $10CC0..$10CF2, $10CFA..$10CFF: result := ubcRightToLeft;
  $11000, $11002..$11037, $11047..$1104D, $11066..$1106F, $11082..$110B2, $110B7,
  $110B8, $110BB..$110C1, $110D0..$110E8, $110F0..$110F9, $11103..$11126, $1112C,
  $11136..$11143, $11150..$11172, $11174..$11176, $11182..$111B5, $111BF..$111C9,
  $111CD, $111D0..$111DF, $111E1..$111F4, $11200..$11211, $11213..$1122E, $11232,
  $11233, $11235, $11238..$1123D, $11280..$11286, $11288, $1128A..$1128D, $1128F..$1129D,
  $1129F..$112A9, $112B0..$112DE, $112E0..$112E2, $112F0..$112F9, $11302, $11303,
  $11305..$1130C, $1130F, $11310, $11313..$11328, $1132A..$11330, $11332, $11333,
  $11335..$11339, $1133D..$1133F, $11341..$11344, $11347, $11348, $1134B..$1134D,
  $11350, $11357, $1135D..$11363, $11400..$11437, $11440, $11441, $11445, $11447..$11459,
  $1145B, $1145D, $11480..$114B2, $114B9, $114BB..$114BE, $114C1, $114C4..$114C7,
  $114D0..$114D9, $11580..$115B1, $115B8..$115BB, $115BE, $115C1..$115DB, $11600..$11632,
  $1163B, $1163C, $1163E, $11641..$11644, $11650..$11659, $11680..$116AA, $116AC,
  $116AE, $116AF, $116B6, $116C0..$116C9, $11700..$11719, $11720, $11721, $11726,
  $11730..$1173F, $118A0..$118F2, $118FF, $11A00, $11A07, $11A08, $11A0B..$11A32,
  $11A39, $11A3A, $11A3F..$11A46, $11A50, $11A57, $11A58, $11A5C..$11A83, $11A86..$11A89,
  $11A97, $11A9A..$11A9C, $11A9E..$11AA2, $11AC0..$11AF8, $11C00..$11C08, $11C0A..$11C2F,
  $11C3E..$11C45, $11C50..$11C6C, $11C70..$11C8F, $11CA9, $11CB1, $11CB4, $11D00..$11D06,
  $11D08, $11D09, $11D0B..$11D30, $11D46, $11D50..$11D59, $12000..$12399, $12400..$1246E,
  $12470..$12474, $12480..$12543, $13000..$1342E, $14400..$14646, $16800..$16A38,
  $16A40..$16A5E, $16A60..$16A69, $16A6E, $16A6F, $16AD0..$16AED, $16AF5, $16B00..$16B2F,
  $16B37..$16B45, $16B50..$16B59, $16B5B..$16B61, $16B63..$16B77, $16B7D..$16B8F,
  $16F00..$16F44, $16F50..$16F7E, $16F93..$16F9F, $16FE0, $16FE1, $17000, $187EC,
  $18800..$18AF2, $1B000..$1B11E, $1B170..$1B2FB, $1BC00..$1BC6A, $1BC70..$1BC7C,
  $1BC80..$1BC88, $1BC90..$1BC99, $1BC9C, $1BC9F, $1D000..$1D0F5, $1D100..$1D126,
  $1D129..$1D166, $1D16A..$1D172, $1D183, $1D184, $1D18C..$1D1A9, $1D1AE..$1D1E8,
  $1D360..$1D371, $1D400..$1D454, $1D456..$1D49C, $1D49E, $1D49F, $1D4A2, $1D4A5,
  $1D4A6, $1D4A9..$1D4AC, $1D4AE..$1D4B9, $1D4BB, $1D4BD..$1D4C3, $1D4C5..$1D505,
  $1D507..$1D50A, $1D50D..$1D514, $1D516..$1D51C, $1D51E..$1D539, $1D53B..$1D53E,
  $1D540..$1D544, $1D546, $1D54A..$1D550, $1D552..$1D6A5, $1D6A8..$1D6DA, $1D6DC..$1D714,
  $1D716..$1D74E, $1D750..$1D788, $1D78A..$1D7C2, $1D7C4..$1D7CB, $1D800..$1D9FF,
  $1DA37..$1DA3A, $1DA6D..$1DA74, $1DA76..$1DA83, $1DA85..$1DA8B: result := ubcLeftToRight;
  $1E800..$1E8C4, $1E8C7..$1E8CF, $1E900..$1E943, $1E950..$1E959, $1E95E, $1E95F: result := ubcRightToLeft;
  $1EE00..$1EE03, $1EE05..$1EE1F, $1EE21, $1EE22, $1EE24, $1EE27, $1EE29..$1EE32,
  $1EE34..$1EE37, $1EE39, $1EE3B, $1EE42, $1EE47, $1EE49, $1EE4B, $1EE4D..$1EE4F,
  $1EE51, $1EE52, $1EE54, $1EE57, $1EE59, $1EE5B, $1EE5D, $1EE5F, $1EE61, $1EE62,
  $1EE64, $1EE67..$1EE6A, $1EE6C..$1EE72, $1EE74..$1EE77, $1EE79..$1EE7C, $1EE7E,
  $1EE80..$1EE89, $1EE8B..$1EE9B, $1EEA1..$1EEA3, $1EEA5..$1EEA9, $1EEAB..$1EEBB: result := ubcArabicLetter;
  $1F110..$1F12E, $1F130..$1F169, $1F170..$1F1AC, $1F1E6..$1F202, $1F210..$1F23B,
  $1F240..$1F248, $1F250, $1F251, $20000, $2A6D6, $2A700, $2B734, $2B740, $2B81D,
  $2B820, $2CEA1, $2CEB0, $2EBE0, $2F800..$2FA1D, $F0000, $FFFFD, $100000, $10FFFD: result := ubcLeftToRight;
  else result := ubcUnknown;
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
  $200B,$200C,$200D,$FEFF,$200E,$200F: result := true;
  else result := false;
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

