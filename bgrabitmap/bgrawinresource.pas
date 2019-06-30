unit BGRAWinResource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAMultiFileType, BGRABitmapTypes, BGRAReadBMP;

const
  RT_CURSOR = 1;
  RT_BITMAP = 2;
  RT_ICON = 3;

  RT_MENU = 4;
  RT_DIALOG = 5;
  RT_STRING = 6;
  RT_FONTDIR = 7;
  RT_FONT = 8;
  RT_ACCELERATOR = 9;
  RT_RCDATA = 10;
  RT_MESSAGETABLE = 11;

  RT_GROUP = 11;
  RT_GROUP_CURSOR = RT_GROUP + RT_CURSOR;
  RT_GROUP_ICON = RT_GROUP + RT_ICON;

  RT_VERSION = 16;
  RT_ANICURSOR = 21;
  RT_ANIICON = 22;
  RT_HTML = 23;
  RT_MANIFEST = 24;

  ICON_OR_CURSOR_FILE_ICON_TYPE = 1;
  ICON_OR_CURSOR_FILE_CURSOR_TYPE = 2;

type
  TNameOrId = record
    Id: integer;
    Name: utf8string;
  end;

  { TResourceInfo }

  TResourceInfo = object
    DataVersion: DWord;
    MemoryFlags: Word;
    LanguageId: Word;
    Version: DWord;
    Characteristics: DWord;
    procedure SwapIfNecessary;
  end;

  TWinResourceContainer = class;

  { TCustomResourceEntry }

  TCustomResourceEntry = class(TMultiFileEntry)
  private
    class function GetNextEntry(AContainer: TMultiFileContainer; AStream: TStream): TCustomResourceEntry; static;
    procedure Serialize(ADestination: TStream);
  protected
    FTypeNameOrId: TNameOrId;
    FEntryNameOrId: TNameOrId;
    FResourceInfo: TResourceInfo;
    FReferenceCount: integer;
    function GetName: utf8string; override;
    procedure SetName(AValue: utf8string); override;
    function GetId: integer;
    procedure SetId(AValue: integer);
    function GetTypeId: integer;
    function GetTypeName: utf8string;
    procedure IncrementReferences; virtual;
    procedure DecrementReferences; virtual;
    procedure SerializeHeader(ADestination: TStream); virtual;
    procedure SerializeData(ADestination: TStream); virtual; abstract;
    function GetDataSize: integer; virtual; abstract;
    function GetLanguageId: integer;
    procedure SetLanguageId(AValue: integer);
  public
    constructor Create(AContainer: TMultiFileContainer; ATypeNameOrId: TNameOrId; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo);
    property Id: integer read GetId write SetId;
    property TypeName: utf8string read GetTypeName;
    property TypeId: integer read GetTypeId;
    property LanguageId: integer read GetLanguageId write SetLanguageId;
  end;

  { TUnformattedResourceEntry }

  TUnformattedResourceEntry = class(TCustomResourceEntry)
  protected
    FDataStream: TStream;
    function GetFileSize: int64; override;
    function GetDataSize: integer; override;
    procedure SerializeData(ADestination: TStream); override;
    function GetExtension: utf8string; override;
  public
    constructor Create(AContainer: TMultiFileContainer; ATypeNameOrId: TNameOrId; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo; ADataStream: TStream);
    destructor Destroy; override;
    function CopyTo(ADestination: TStream): int64; override;
  end;

  { TBitmapResourceEntry }

  TBitmapResourceEntry = class(TUnformattedResourceEntry)
  protected
    function GetFileSize: int64; override;
    function GetExtension: utf8string; override;
  public
    constructor Create(AContainer: TMultiFileContainer; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo; ADataStream: TStream);
    function CopyTo(ADestination: TStream): int64; override;
    procedure CopyFrom(ASource: TStream);
  end;

  { TGroupIconHeader }

  TGroupIconHeader = object
    Reserved, ResourceType, ImageCount: Word;
    procedure SwapIfNecessary;
  end;
  TGroupIconDirEntry = packed record
    Width, Height, Colors, Reserved: byte;
    //stored in little endian
    case byte of
    0: (Variable: DWord; ImageSize: DWord; ImageId: Word);
    1: (Planes, BitsPerPixel: Word);
    2: (HotSpotX, HotSpotY: Word);
  end;
  TIconFileDirEntry = packed record
    Width, Height, Colors, Reserved: byte;
    //stored in little endian
    case byte of
    0: (Variable: DWord; ImageSize: DWord; ImageOffset: DWord);
    1: (Planes, BitsPerPixel: Word);
    2: (HotSpotX, HotSpotY: Word);
  end;

  { TGroupIconOrCursorEntry }

  TGroupIconOrCursorEntry = class(TCustomResourceEntry)
  private
    function GetNbIcons: integer;
  protected
    FGroupIconHeader: TGroupIconHeader;
    FDirectory: packed array of TGroupIconDirEntry;
    function GetFileSize: int64; override;
    function GetDataSize: integer; override;
    procedure SerializeData(ADestination: TStream); override;
    procedure IncrementReferences; override;
    procedure DecrementReferences; override;
    function ExpectedResourceType: word; virtual; abstract;
  public
    constructor Create(AContainer: TMultiFileContainer; ATypeNameOrId: TNameOrId; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo; ADataStream: TStream);
    constructor Create(AContainer: TMultiFileContainer; ATypeNameOrId: TNameOrId; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo);
    procedure Clear;
    function CopyTo(ADestination: TStream): int64; override;
    procedure CopyFrom(ASource: TStream);
    property NbIcons: integer read GetNbIcons;
  end;

  { TGroupIconEntry }

  TGroupIconEntry = class(TGroupIconOrCursorEntry)
  protected
    function GetExtension: utf8string; override;
    function ExpectedResourceType: word; override;
  public
    constructor Create(AContainer: TMultiFileContainer; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo; ADataStream: TStream);
    constructor Create(AContainer: TMultiFileContainer; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo);
  end;

  { TGroupCursorEntry }

  TGroupCursorEntry = class(TGroupIconOrCursorEntry)
  protected
    function GetExtension: utf8string; override;
    function ExpectedResourceType: word; override;
  public
    constructor Create(AContainer: TMultiFileContainer; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo; ADataStream: TStream);
    constructor Create(AContainer: TMultiFileContainer; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo);
  end;

  { TWinResourceContainer }

  TWinResourceContainer = class(TMultiFileContainer)
  private
    function InternalFind(const AEntry: TNameOrId; const AType: TNameOrId; ALanguageId: integer = 0): TCustomResourceEntry;
    procedure AddHidden(AEntry: TCustomResourceEntry);
    function GetMaxId(AType: TNameOrId): integer;
    procedure IncrementReferenceOf(ANameId, ATypeId: integer);
    procedure DecrementReferenceOf(ANameId, ATypeId: integer);
  protected
    FHiddenEntries: TMultiFileEntryList;
    procedure Init; override;
    procedure ClearHiddenEntries;
    procedure RemoveHidden(AEntry: TCustomResourceEntry);
    function CreateEntry(AName: utf8string; AExtension: utf8string; AContent: TStream; ALanguageId: integer): TMultiFileEntry; overload;
    function CreateEntry(AName: utf8string; AExtension: utf8string; AContent: TStream): TMultiFileEntry; override;
  public
    procedure Clear; override;
    destructor Destroy; override;
    procedure Delete(AIndex: integer); override;
    procedure LoadFromStream(AStream: TStream); override;
    function IndexOf(AName: utf8string; AExtenstion: utf8string; ACaseSensitive: boolean = True): integer; override;
    function IndexOf(AName: utf8string; AExtenstion: utf8string; ALanguageId: integer; ACaseSensitive: boolean = True): integer; overload;
    procedure SaveToStream(ADestination: TStream); override;
  end;

implementation

uses Math, BGRAUTF8;

operator =(const ANameOrId1, ANameOrId2: TNameOrId): boolean;
begin
  if (ANameOrId1.Id < 0) then
    result := (ANameOrId2.Id < 0) and (ANameOrId2.Name = ANameOrId1.Name)
  else
    result := ANameOrId2.Id = ANameOrId1.Id;
end;

function NameOrId(AName: string): TNameOrId; overload;
begin
  result.Id := -1;
  result.Name := AName;
end;

function NameOrId(AId: integer): TNameOrId; overload;
begin
  result.Id := AId;
  result.Name := IntToStr(AId);
end;

{ TGroupCursorEntry }

function TGroupCursorEntry.GetExtension: utf8string;
begin
  Result:= 'cur';
end;

function TGroupCursorEntry.ExpectedResourceType: word;
begin
  result := ICON_OR_CURSOR_FILE_CURSOR_TYPE;
end;

constructor TGroupCursorEntry.Create(AContainer: TMultiFileContainer;
  AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo;
  ADataStream: TStream);
begin
  inherited Create(AContainer,NameOrId(RT_GROUP_CURSOR),AEntryNameOrId,AResourceInfo,ADataStream);
end;

constructor TGroupCursorEntry.Create(AContainer: TMultiFileContainer;
  AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo);
begin
  inherited Create(AContainer,NameOrId(RT_GROUP_CURSOR),AEntryNameOrId,AResourceInfo);
end;

{ TGroupIconEntry }

function TGroupIconEntry.GetExtension: utf8string;
begin
  Result:= 'ico';
end;

function TGroupIconEntry.ExpectedResourceType: word;
begin
  result := ICON_OR_CURSOR_FILE_ICON_TYPE;
end;

constructor TGroupIconEntry.Create(AContainer: TMultiFileContainer;
  AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo;
  ADataStream: TStream);
begin
  inherited Create(AContainer,NameOrId(RT_GROUP_ICON),AEntryNameOrId,AResourceInfo,ADataStream);
end;

constructor TGroupIconEntry.Create(AContainer: TMultiFileContainer;
  AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo);
begin
  inherited Create(AContainer,NameOrId(RT_GROUP_ICON),AEntryNameOrId,AResourceInfo);
end;

{ TGroupIconHeader }

procedure TGroupIconHeader.SwapIfNecessary;
begin
  Reserved := LEtoN(Reserved);
  ResourceType := LEtoN(ResourceType);
  ImageCount := LEtoN(ImageCount);
end;

{ TGroupIconOrCursorEntry }

function TGroupIconOrCursorEntry.GetNbIcons: integer;
begin
  result := FGroupIconHeader.ImageCount;
end;

function TGroupIconOrCursorEntry.GetFileSize: int64;
var
  i: Integer;
begin
  Result:= sizeof(FGroupIconHeader) + sizeof(TIconFileDirEntry)*NbIcons;
  for i := 0 to NbIcons-1 do
    inc(Result, LEtoN(FDirectory[i].ImageSize) );
end;

function TGroupIconOrCursorEntry.GetDataSize: integer;
begin
  result := sizeof(FGroupIconHeader) + sizeof(TGroupIconDirEntry)*NbIcons;
end;

procedure TGroupIconOrCursorEntry.SerializeData(ADestination: TStream);
begin
  FGroupIconHeader.SwapIfNecessary;
  try
    ADestination.WriteBuffer(FGroupIconHeader, sizeof(FGroupIconHeader));
  finally
    FGroupIconHeader.SwapIfNecessary;
  end;
  ADestination.WriteBuffer(FDirectory[0], sizeof(TGroupIconDirEntry)*NbIcons);
end;

procedure TGroupIconOrCursorEntry.IncrementReferences;
var
  i: Integer;
begin
  for i := 0 to NbIcons-1 do
    TWinResourceContainer(Container).IncrementReferenceOf(LEtoN(FDirectory[i].ImageId), TypeId - RT_GROUP);
end;

procedure TGroupIconOrCursorEntry.DecrementReferences;
var
  i: Integer;
begin
  for i := 0 to NbIcons-1 do
    TWinResourceContainer(Container).DecrementReferenceOf(LEtoN(FDirectory[i].ImageId), TypeId - RT_GROUP);
end;

constructor TGroupIconOrCursorEntry.Create(AContainer: TMultiFileContainer;
  ATypeNameOrId: TNameOrId; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo;
  ADataStream: TStream);
begin
  inherited Create(AContainer,ATypeNameOrId,AEntryNameOrId,AResourceInfo);

  ADataStream.ReadBuffer(FGroupIconHeader, sizeof(FGroupIconHeader));
  FGroupIconHeader.SwapIfNecessary;
  if FGroupIconHeader.ResourceType <> ExpectedResourceType then
    raise exception.Create('Unexpected group type');

  if ADataStream.Position + NbIcons*sizeof(TGroupIconDirEntry) > ADataStream.Size then
    raise exception.Create('Directory dimension mismatch');
  setlength(FDirectory, NbIcons);
  ADataStream.ReadBuffer(FDirectory[0], NbIcons*sizeof(TGroupIconDirEntry));
  ADataStream.Free;
end;

constructor TGroupIconOrCursorEntry.Create(AContainer: TMultiFileContainer;
  ATypeNameOrId: TNameOrId; AEntryNameOrId: TNameOrId;
  const AResourceInfo: TResourceInfo);
begin
  inherited Create(AContainer,ATypeNameOrId,AEntryNameOrId,AResourceInfo);

  FGroupIconHeader.Reserved := 0;
  FGroupIconHeader.ResourceType := ExpectedResourceType;
  FGroupIconHeader.ImageCount := 0;
end;

procedure TGroupIconOrCursorEntry.Clear;
begin
  DecrementReferences;
  FDirectory := nil;
  FGroupIconHeader.ImageCount := 0;
end;

function TGroupIconOrCursorEntry.CopyTo(ADestination: TStream): int64;
var
  fileDir: packed array of TIconFileDirEntry;
  offset, written, i: integer;
  iconEntry: TCustomResourceEntry;
  iconEntrySize: DWord;
  iconData: TMemoryStream;
  copyCount: Int64;
  subType: TNameOrId;

  procedure FillZero(ACount: integer);
  var
    Zero: packed array[0..255] of byte;
  begin
    if ACount <= 0 then exit;
    FillChar({%H-}Zero, Sizeof(Zero), 0);
    while ACount > 0 do
    begin
      ADestination.WriteBuffer(Zero, Min(ACount, sizeof(Zero)));
      Dec(ACount, Min(ACount, sizeof(Zero)));
    end;
  end;

begin
  result:= 0;
  FGroupIconHeader.SwapIfNecessary;
  try
    ADestination.WriteBuffer(FGroupIconHeader, sizeof(FGroupIconHeader));
  finally
    FGroupIconHeader.SwapIfNecessary;
  end;
  Inc(result, sizeof(FGroupIconHeader));

  offset := result+sizeof(TIconFileDirEntry)*NbIcons;
  setlength(fileDir, NbIcons);
  for i := 0 to NbIcons-1 do
  begin
    move(FDirectory[i], fileDir[i], 12);
    fileDir[i].ImageOffset := NtoLE(offset);
    inc(offset, fileDir[i].ImageSize);
  end;

  ADestination.WriteBuffer(fileDir[0], sizeof(TIconFileDirEntry)*NbIcons);
  inc(result, sizeof(TIconFileDirEntry)*NbIcons);

  subType := NameOrId(TypeId - RT_GROUP);
  for i := 0 to NbIcons-1 do
  begin
    iconEntry := (Container as TWinResourceContainer).InternalFind(NameOrId(LEtoN(FDirectory[i].ImageId)),subType); //no language for icons
    iconEntrySize := LEtoN(FDirectory[i].ImageSize);
    if iconEntry = nil then
      FillZero(iconEntrySize) else
    begin
      iconData := TMemoryStream.Create;
      try
        iconEntry.CopyTo(IconData);
        iconData.Position:= 0;
        copyCount := Min(IconData.Size, iconEntrySize);
        if copyCount > 0 then written := ADestination.CopyFrom(IconData, copyCount)
        else written := 0;
        FillZero(iconEntrySize-written);
      finally
        IconData.Free;
      end;
    end;
    inc(result, iconEntrySize);
  end;
end;

procedure TGroupIconOrCursorEntry.CopyFrom(ASource: TStream);
var
  tempGroup: TGroupIconHeader;
  fileDir: packed array of TIconFileDirEntry;
  iconStream: array of TMemoryStream;
  startPos: int64;
  maxId, i: integer;
  iconEntry: TUnformattedResourceEntry;
  resourceInfo: TResourceInfo;
  subType: TNameOrId;
begin
  startPos := ASource.Position;
  ASource.ReadBuffer({%H-}tempGroup, sizeof(tempGroup));
  tempGroup.SwapIfNecessary;
  if tempGroup.ResourceType <> ExpectedResourceType then
    raise exception.Create('Unexpected resource type');

  if ASource.Position + sizeof(TIconFileDirEntry)*tempGroup.ImageCount > ASource.Size then
    raise exception.Create('Directory dimension mismatch');

  setlength(fileDir, tempGroup.ImageCount);
  ASource.ReadBuffer(fileDir[0], sizeof(TIconFileDirEntry)*tempGroup.ImageCount);

  try
    setlength(iconStream, tempGroup.ImageCount);
    for i := 0 to tempGroup.ImageCount-1 do
    begin
      ASource.Position:= startPos + LEtoN(fileDir[i].ImageOffset);
      iconStream[i] := TMemoryStream.Create;
      iconStream[i].CopyFrom(ASource, LEtoN(fileDir[i].ImageSize));
    end;

    subType := NameOrId(self.TypeId - RT_GROUP);
    maxId := TWinResourceContainer(Container).GetMaxId(subType);

    Clear;
    FGroupIconHeader.ImageCount := tempGroup.ImageCount;
    setlength(FDirectory, tempGroup.ImageCount);
    fillchar({%H-}resourceInfo,sizeof(resourceInfo),0);
    for i := 0 to tempGroup.ImageCount-1 do
    begin
      move(fileDir[i], FDirectory[i], 12);
      inc(maxId);
      FDirectory[i].ImageId := maxId;
      iconEntry := TUnformattedResourceEntry.Create(Container, subType, NameOrId(maxId), resourceInfo, iconStream[i]);
      iconStream[i] := nil;
      TWinResourceContainer(Container).AddHidden(iconEntry);
    end;

  finally
    for i := 0 to high(iconStream) do
      iconStream[i].Free;
    iconStream := nil;
  end;
end;

{ TBitmapResourceEntry }

function TBitmapResourceEntry.GetFileSize: int64;
begin
  result := sizeof(TBitMapFileHeader)+FDataStream.Size;
end;

function TBitmapResourceEntry.GetExtension: utf8string;
begin
  Result:= 'bmp';
end;

constructor TBitmapResourceEntry.Create(AContainer: TMultiFileContainer;
  AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo;
  ADataStream: TStream);
begin
  inherited Create(AContainer, NameOrId(RT_BITMAP), AEntryNameOrId, AResourceInfo, ADataStream);
end;

function TBitmapResourceEntry.CopyTo(ADestination: TStream): int64;
var fileHeader: TBitMapFileHeader;
begin
  result := 0;
  FDataStream.Position := 0;
  fileHeader := MakeBitmapFileHeader(FDataStream);
  ADestination.WriteBuffer(fileHeader, sizeof(fileHeader));
  inc(result, sizeof(fileHeader) );
  FDataStream.Position := 0;
  inc(result, ADestination.CopyFrom(FDataStream, FDataStream.Size) );
end;

procedure TBitmapResourceEntry.CopyFrom(ASource: TStream);
var
  fileHeader: TBitMapFileHeader;
  dataSize: integer;
begin
  ASource.ReadBuffer({%H-}fileHeader, sizeof(fileHeader));
  if fileHeader.bfType <> Word('BM') then
    raise exception.Create('Invalid file header');
  dataSize := LEtoN(fileHeader.bfSize) - sizeof(fileHeader);
  if ASource.Position + dataSize > ASource.Size then
    raise exception.Create('Invalid file size');

  FDataStream.Free;
  FDataStream := TMemoryStream.Create;
  FDataStream.CopyFrom(ASource, dataSize);
end;

{ TUnformattedResourceEntry }

function TUnformattedResourceEntry.GetFileSize: int64;
begin
  Result:= FDataStream.Size;
end;

function TUnformattedResourceEntry.GetDataSize: integer;
begin
  result := FDataStream.Size;
end;

procedure TUnformattedResourceEntry.SerializeData(ADestination: TStream);
begin
  if FDataStream.Size > 0 then
  begin
    FDataStream.Position := 0;
    ADestination.CopyFrom(FDataStream, FDataStream.Size);
  end;
end;

function TUnformattedResourceEntry.GetExtension: utf8string;
var format: TBGRAImageFormat;
begin
  case TypeId of
  RT_MANIFEST: result := 'manifest';
  RT_HTML: result := 'html';
  RT_RCDATA:
  begin
    FDataStream.Position:= 0;
    format := DetectFileFormat(FDataStream);
    if format = ifUnknown then
      result := 'dat'
    else
      result := SuggestImageExtension(format);
  end;
  RT_ANICURSOR: result := 'ani';
  else
    if TypeName = 'ANICURSOR' then
      result := 'ani'
    else
      result := '';
  end;
end;

constructor TUnformattedResourceEntry.Create(AContainer: TMultiFileContainer;
  ATypeNameOrId: TNameOrId; AEntryNameOrId: TNameOrId;
  const AResourceInfo: TResourceInfo; ADataStream: TStream);
begin
  inherited Create(AContainer,ATypeNameOrId,AEntryNameOrId,AResourceInfo);
  FDataStream := ADataStream;
end;

destructor TUnformattedResourceEntry.Destroy;
begin
  FreeAndNil(FDataStream);
  inherited Destroy;
end;

function TUnformattedResourceEntry.CopyTo(ADestination: TStream): int64;
begin
  if FDataStream.Size > 0 then
  begin
    FDataStream.Position := 0;
    result := ADestination.CopyFrom(FDataStream, FDataStream.Size)
  end
  else
    result := 0;
end;

{ TResourceInfo }

procedure TResourceInfo.SwapIfNecessary;
begin
  DataVersion := LEtoN(DataVersion);
  MemoryFlags := LEtoN(MemoryFlags);
  LanguageId := LEtoN(LanguageId);
  Version := LEtoN(Version);
  Characteristics := LEtoN(Characteristics);
end;

{ TCustomResourceEntry }

function TCustomResourceEntry.GetId: integer;
begin
  result := FEntryNameOrId.Id;
end;

function TCustomResourceEntry.GetTypeId: integer;
begin
  result := FTypeNameOrId.Id;
end;

function GetDWord(var ASource: PByte; var ARemainingBytes: Integer): DWord;
begin
  if ARemainingBytes >= 4 then
  begin
    result := LEtoN(PDWord(ASource)^);
    inc(ASource, 4);
    dec(ARemainingBytes, 4);
  end else
  begin
    result := 0;
    inc(ASource, ARemainingBytes);
    ARemainingBytes:= 0;
  end;
end;

function GetWord(var ASource: PByte; var ARemainingBytes: Integer): Word;
begin
  if ARemainingBytes >= 2 then
  begin
    result := LEtoN(PWord(ASource)^);
    inc(ASource, 2);
    dec(ARemainingBytes, 2);
  end else
  begin
    result := 0;
    inc(ASource, ARemainingBytes);
    ARemainingBytes:= 0;
  end;
end;

function GetNameOrId(var ASource: PByte; var ARemainingBytes: Integer): TNameOrId;
var curChar: Word;
  pstart: PByte;
begin
  pstart := ASource;
  curChar := GetWord(ASource,ARemainingBytes);
  if curChar = $ffff then
  begin
    result.Id := GetWord(ASource,ARemainingBytes);
    result.Name := IntToStr(result.Id);
  end else
  begin
    while curChar <> 0 do
      curChar := GetWord(ASource,ARemainingBytes);
    result.Id := -1;
    result.Name := UTF8Encode(WideCharLenToString(PWideChar(pstart), (ASource-pstart) div 2 -1));
  end;
end;

function TCustomResourceEntry.GetLanguageId: integer;
begin
  result := FResourceInfo.LanguageId;
end;

class function TCustomResourceEntry.GetNextEntry(AContainer: TMultiFileContainer; AStream: TStream): TCustomResourceEntry;
var
  entrySize, headerSize, remaining, padding: Integer;
  headerData: Pointer;
  pHeaderData: PByte;
  typeNameOrId: TNameOrId;
  entryNameOrId: TNameOrId;
  info: TResourceInfo;
  dataStream: TMemoryStream;
  dummy: DWord;
begin
  result := nil;
  if AStream.Position + 16 < AStream.Size then
  begin
    entrySize := LEtoN(AStream.ReadDWord);
    headerSize := LEtoN(AStream.ReadDWord);
    if headerSize < 16 then
      raise exception.Create('Header too small');
    remaining := ((headerSize-8) + 3) and not 3;
    if AStream.Position + remaining + entrySize > AStream.Size then
      raise exception.Create('Data would be outside of stream');

    GetMem(headerData, remaining);
    try
      AStream.ReadBuffer(headerData^, remaining);
      pHeaderData := PByte(headerData);
      typeNameOrId := GetNameOrId(pHeaderData, remaining);
      entryNameOrId := GetNameOrId(pHeaderData, remaining);
      padding := (4 - (PtrUInt(pHeaderData-PByte(headerData)) and 3)) and 3;
      inc(pHeaderData, padding);
      dec(remaining, padding);

      FillChar({%H-}info, SizeOf(info), 0);
      Move(pHeaderData^, info, Min(Sizeof(info), remaining));
      info.SwapIfNecessary;

      dataStream := TMemoryStream.Create;
      if entrySize > 0 then dataStream.CopyFrom(AStream, entrySize);
      padding := ((entrySize+3) and not 3) - entrySize;
      if padding > 0 then AStream.Read({%H-}dummy, padding);
    finally
      FreeMem(headerData);
    end;

    dataStream.Position := 0;
    case typeNameOrId.Id of
    RT_BITMAP: result := TBitmapResourceEntry.Create(AContainer,entryNameOrId,info,dataStream);
    RT_GROUP_ICON: result := TGroupIconEntry.Create(AContainer,entryNameOrId,info,dataStream);
    RT_GROUP_CURSOR: result := TGroupCursorEntry.Create(AContainer,entryNameOrId,info,dataStream);
    else
      result := TUnformattedResourceEntry.Create(AContainer,typeNameOrId,entryNameOrId,info,dataStream);
    end;
  end;
end;

procedure WriteNameOrId(ADestination: TStream; ANameOrId: TNameOrId);
var buffer: PUnicodeChar;
  maxLen,actualLen: integer;
begin
  if ANameOrId.Id < 0 then
  begin
    maxLen := length(ANameOrId.Name)*2 + 1;
    getmem(buffer, maxLen*sizeof(UnicodeChar));
    try
      fillchar(buffer^, maxLen*sizeof(UnicodeChar), 0);
      actualLen := Utf8ToUnicode(buffer, maxLen, @ANameOrId.Name[1], length(ANameOrId.Name));
      ADestination.WriteBuffer(buffer^, actualLen*sizeof(UnicodeChar));
    finally
      freemem(buffer);
    end;
  end else
  begin
    ADestination.WriteWord($ffff);
    ADestination.WriteWord(NtoLE(Word(ANameOrId.Id)));
  end;
end;

procedure TCustomResourceEntry.Serialize(ADestination: TStream);
var zero: DWord;
  padding: integer;
begin
  SerializeHeader(ADestination);
  SerializeData(ADestination);
  padding := (4-(GetDataSize and 3)) and 3;
  if padding > 0 then
  begin
    zero := 0;
    ADestination.WriteBuffer(zero, padding);
  end;
end;

procedure TCustomResourceEntry.SetLanguageId(AValue: integer);
begin
  if (AValue >= 0) and (AValue <= 65535) then
  begin
    if AValue = LanguageId then exit;
    if FTypeNameOrId.Id >= 0 then
    begin
      if TWinResourceContainer(Container).InternalFind(FEntryNameOrId, FTypeNameOrId, AValue) <> nil then
        raise exception.Create('Language id already used for this resource');
    end else
      raise exception.Create('Language id cannot be specified for custom types');
    FEntryNameOrId.Id := AValue;
    FEntryNameOrId.Name := IntToStr(AValue);
  end
  else
    raise ERangeError.Create('Id out of bounds');
end;

procedure TCustomResourceEntry.SerializeHeader(ADestination: TStream);
var
  entryHeader: record
    EntrySize: integer;
    HeaderSize: integer;
  end;
  headerStream: TMemoryStream;
begin
  entryHeader.EntrySize := LEtoN(GetDataSize);
  headerStream := TMemoryStream.Create;
  try
    WriteNameOrId(headerStream,FTypeNameOrId);
    WriteNameOrId(headerStream,FEntryNameOrId);
    if headerStream.Position and 3 = 2 then headerStream.WriteWord(0);
    FResourceInfo.SwapIfNecessary;
    try
      headerStream.WriteBuffer(FResourceInfo, sizeof(FResourceInfo));
    finally
      FResourceInfo.SwapIfNecessary;
    end;
    entryHeader.HeaderSize := LEtoN(integer(headerStream.Size+8));
    headerStream.Position:= 0;
    ADestination.WriteBuffer(entryHeader, sizeof(entryHeader));
    ADestination.CopyFrom(headerStream, headerStream.Size);
    if headerStream.Size and 3 = 2 then ADestination.WriteWord(0);
  finally
    headerStream.Free;
  end;
end;

constructor TCustomResourceEntry.Create(AContainer: TMultiFileContainer;
  ATypeNameOrId: TNameOrId; AEntryNameOrId: TNameOrId;
  const AResourceInfo: TResourceInfo);
begin
  inherited Create(AContainer);
  FTypeNameOrId := ATypeNameOrId;
  FEntryNameOrId := AEntryNameOrId;
  FResourceInfo := AResourceInfo;
end;

procedure TCustomResourceEntry.SetId(AValue: integer);
begin
  if (AValue >= 0) and (AValue <= 65535) then
  begin
    if AValue = FEntryNameOrId.Id then exit;
    if TWinResourceContainer(Container).InternalFind(NameOrId(AValue), FTypeNameOrId, LanguageId) <> nil then
      raise exception.Create('Id already used for this resource type');
    FEntryNameOrId.Id := AValue;
    FEntryNameOrId.Name := IntToStr(AValue);
  end
  else
    raise ERangeError.Create('Id out of bounds');
end;

function TCustomResourceEntry.GetName: utf8string;
begin
  Result:= FEntryNameOrId.Name;
end;

procedure TCustomResourceEntry.SetName(AValue: utf8string);
begin
  if FEntryNameOrId = NameOrId(AValue) then exit;
  if TWinResourceContainer(Container).InternalFind(NameOrId(AValue), FTypeNameOrId, LanguageId) <> nil then
      raise exception.Create('Name already used for this resource type');
  FEntryNameOrId.Name := AValue;
  FEntryNameOrId.Id := -1;
end;

function TCustomResourceEntry.GetTypeName: utf8string;
begin
  result := FTypeNameOrId.Name;
end;

procedure TCustomResourceEntry.IncrementReferences;
begin
  //nothing
end;

procedure TCustomResourceEntry.DecrementReferences;
begin
  //nothing
end;

{ TWinResourceContainer }

procedure TWinResourceContainer.LoadFromStream(AStream: TStream);
var curEntry: TCustomResourceEntry;
  i: Integer;
begin
  Clear;
  repeat
    curEntry := TCustomResourceEntry.GetNextEntry(self, AStream);
    if curEntry <> nil then
    begin
      if curEntry.TypeId in [RT_ICON,RT_CURSOR] then
        FHiddenEntries.Add(curEntry)
      else
        AddEntry(curEntry);
    end;
  until curEntry = nil;
  for i := 0 to Count-1 do
    TCustomResourceEntry(Entry[i]).IncrementReferences;
end;

function TWinResourceContainer.IndexOf(AName: utf8string; AExtenstion: utf8string; ACaseSensitive: boolean): integer;
begin
  result := IndexOf(AName, AExtenstion, 0, ACaseSensitive);
end;

function TWinResourceContainer.IndexOf(AName: utf8string; AExtenstion: utf8string;
  ALanguageId: integer; ACaseSensitive: boolean): integer;
var
  i: Integer;
  entryId, errPos: integer;
begin
  if AExtenstion = '' then
  begin
    result := -1;
    exit;
  end;
  if ACaseSensitive then
  begin
    for i := 0 to Count-1 do
      if (TCustomResourceEntry(Entry[i]).FEntryNameOrId.Id < 0) and
         (TCustomResourceEntry(Entry[i]).FEntryNameOrId.Name = AName) and
         (UTF8CompareText(Entry[i].Extension,AExtenstion) = 0) and
         (TCustomResourceEntry(Entry[i]).LanguageId = ALanguageId) then
      begin
        result := i;
        exit;
      end;
  end else
    for i := 0 to Count-1 do
      if (TCustomResourceEntry(Entry[i]).FEntryNameOrId.Id < 0) and
         (UTF8CompareText(TCustomResourceEntry(Entry[i]).FEntryNameOrId.Name,AName) = 0) and
         (UTF8CompareText(Entry[i].Extension,AExtenstion) = 0) and
         (TCustomResourceEntry(Entry[i]).LanguageId = ALanguageId) then
      begin
        result := i;
        exit;
      end;
  val(AName, entryId, errPos);
  if (errPos = 0) and (entryId >= 0) then
  begin
    for i := 0 to Count-1 do
      if (TCustomResourceEntry(Entry[i]).FEntryNameOrId.Id = entryId) and
         (UTF8CompareText(Entry[i].Extension,AExtenstion) = 0) and
         (TCustomResourceEntry(Entry[i]).LanguageId = ALanguageId) then
      begin
        result := i;
        exit;
      end;
  end;
  result := -1;
end;

procedure TWinResourceContainer.Init;
begin
  inherited Init;
  FHiddenEntries := TMultiFileEntryList.Create;
end;

procedure TWinResourceContainer.ClearHiddenEntries;
var i: integer;
begin
  if Assigned(FHiddenEntries) then
  begin
    for i := 0 to FHiddenEntries.Count-1 do
      FHiddenEntries[i].Free;
    FHiddenEntries.Clear;
  end;
end;

procedure TWinResourceContainer.RemoveHidden(AEntry: TCustomResourceEntry);
var
  index: LongInt;
begin
  if Assigned(FHiddenEntries) then
  begin
    index := FHiddenEntries.IndexOf(AEntry);
    if index <> -1 then
    begin
      AEntry.Free;
      FHiddenEntries.Delete(index);
    end;
  end;
end;

function TWinResourceContainer.CreateEntry(AName: utf8string; AExtension: utf8string;
  AContent: TStream; ALanguageId: integer): TMultiFileEntry;
var
  resourceInfo: TResourceInfo;
  entryName: TNameOrId;
  errPos: integer;
begin
  FillChar({%H-}resourceInfo, sizeof(resourceInfo), 0);
  resourceInfo.LanguageId := ALanguageId;
  val(AName, entryName.Id, errPos);
  if (errPos = 0) and (entryName.Id >= 0) then
    entryName.Name := IntToStr(entryName.Id)
  else
  begin
    entryName.Id := -1;
    entryName.Name := AName;
  end;

  case UTF8LowerCase(AExtension) of
  'ico': begin
           result := TGroupIconEntry.Create(self, entryName, resourceInfo);
           AContent.Position:= 0;
           TGroupIconEntry(result).CopyFrom(AContent);
           AContent.Free;
         end;
  'cur': begin
           result := TGroupCursorEntry.Create(self, entryName, resourceInfo);
           AContent.Position:= 0;
           TGroupCursorEntry(result).CopyFrom(AContent);
           AContent.Free;
         end;
  'bmp': begin
           result := TBitmapResourceEntry.Create(self, entryName, resourceInfo, AContent);
           AContent.Position:= 0;
           TBitmapResourceEntry(result).CopyFrom(AContent);
           AContent.Free;
         end;
  'dat': result := TUnformattedResourceEntry.Create(self, NameOrId(RT_RCDATA), entryName, resourceInfo, AContent);
  'html','htm': result := TUnformattedResourceEntry.Create(self, NameOrId(RT_HTML), entryName, resourceInfo, AContent);
  'manifest': result := TUnformattedResourceEntry.Create(self, NameOrId(RT_MANIFEST), entryName, resourceInfo, AContent);
  'ani': result := TUnformattedResourceEntry.Create(self, NameOrId(RT_ANICURSOR), entryName, resourceInfo, AContent);
  else
    case SuggestImageFormat('.'+AExtension) of
    ifUnknown: raise exception.Create('Unhandled file extension');
    else
      result := TUnformattedResourceEntry.Create(self, NameOrId(RT_RCDATA), entryName, resourceInfo, AContent);
    end;
  end;
end;

function TWinResourceContainer.CreateEntry(AName: utf8string; AExtension: utf8string;
  AContent: TStream): TMultiFileEntry;
begin
  result := CreateEntry(AName, AExtension, AContent, 0);
end;

procedure TWinResourceContainer.Clear;
begin
  ClearHiddenEntries;
  inherited Clear;
end;

destructor TWinResourceContainer.Destroy;
begin
  ClearHiddenEntries;
  FreeAndNil(FHiddenEntries);
  inherited Destroy;
end;

procedure TWinResourceContainer.Delete(AIndex: integer);
begin
  if (AIndex >= 0) and (AIndex < Count) then
    TCustomResourceEntry(Entry[AIndex]).DecrementReferences;
  inherited Delete(AIndex);
end;

procedure TWinResourceContainer.SaveToStream(ADestination: TStream);
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    TCustomResourceEntry(Entry[i]).Serialize(ADestination);
  for i := 0 to FHiddenEntries.Count-1 do
    TCustomResourceEntry(FHiddenEntries.Items[i]).Serialize(ADestination);
end;

function TWinResourceContainer.InternalFind(const AEntry: TNameOrId;
  const AType: TNameOrId; ALanguageId: integer): TCustomResourceEntry;
var i: integer;
begin
  if Assigned(FHiddenEntries) and (ALanguageId = 0) and (AType.Id >= 0) then
  begin
    for i := 0 to FHiddenEntries.Count-1 do
      if (TCustomResourceEntry(FHiddenEntries.Items[i]).FEntryNameOrId = AEntry) and
         (TCustomResourceEntry(FHiddenEntries.Items[i]).FTypeNameOrId = AType) then
      begin
        result := TCustomResourceEntry(FHiddenEntries.Items[i]);
        exit;
      end;
  end;
  for i := 0 to Count-1 do
    if (TCustomResourceEntry(Entry[i]).FEntryNameOrId = AEntry) and
       (TCustomResourceEntry(Entry[i]).FTypeNameOrId = AType) and
       (TCustomResourceEntry(Entry[i]).LanguageId = ALanguageId) then
    begin
      result := TCustomResourceEntry(Entry[i]);
      exit;
    end;
  result := nil;
end;

procedure TWinResourceContainer.AddHidden(AEntry: TCustomResourceEntry);
begin
  FHiddenEntries.Add(AEntry);
end;

function TWinResourceContainer.GetMaxId(AType: TNameOrId): integer;
var i: integer;
begin
  result := 0;
  if Assigned(FHiddenEntries) and (AType.Id >= 0) then
  begin
    for i := 0 to FHiddenEntries.Count-1 do
      if (TCustomResourceEntry(FHiddenEntries.Items[i]).FTypeNameOrId = AType) then
      begin
        if TCustomResourceEntry(FHiddenEntries.Items[i]).Id > result then result := TCustomResourceEntry(FHiddenEntries.Items[i]).Id;
      end;
  end;
  for i := 0 to Count-1 do
    if (TCustomResourceEntry(Entry[i]).FTypeNameOrId = AType) then
    begin
      if TCustomResourceEntry(Entry[i]).Id > result then result := TCustomResourceEntry(Entry[i]).Id;
    end;
end;

procedure TWinResourceContainer.IncrementReferenceOf(ANameId, ATypeId: integer);
var
  item: TCustomResourceEntry;
begin
  item := InternalFind(NameOrId(ANameId), NameOrId(ATypeId));
  if Assigned(item) then inc(item.FReferenceCount);
end;

procedure TWinResourceContainer.DecrementReferenceOf(ANameId, ATypeId: integer);
var
  item: TCustomResourceEntry;
begin
  item := InternalFind(NameOrId(ANameId), NameOrId(ATypeId));
  if Assigned(item) then
  begin
    if item.FReferenceCount > 1 then
      dec(item.FReferenceCount)
    else
      RemoveHidden(item);
  end;
end;

end.

