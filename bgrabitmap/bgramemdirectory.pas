unit BGRAMemDirectory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAMultiFileType, fgl;

const
  MemDirectoryFileHeader = 'TMemDirectory'#26#0#0;
  MemDirectoryEntry_FlagDirectory = 1;   //entry is a directory
  MemDirectoryEntry_FlagCompressed = 2;  //the stream is compressed
  MemDirectoryEntry_FlagSmallEntryPacked = $8000; //name and size <= 255

type
  TMemDirectory = class;
  TEntryFilename = BGRAMultiFileType.TEntryFilename;

type
  TMemDirectoryPath = specialize TFPGList<TEntryFilename>;

  { TMemDirectoryEntry }

  TMemDirectoryEntry = class(TMultiFileEntry)
  private
    FStream: TStream;
    function GetIsCompressed: boolean;
    function GetCompressedSize: int64;
    function GetIsDirectory: boolean;
    procedure SetIsCompressed(AValue: boolean);
    procedure LoadExtraFromEmbeddedStream(ADataStream: TStream; AStartPos: int64);
    procedure SaveToEmbeddedStream(AEntryStream, ADataStream: TStream; AStartPos: int64; out uncompressedSize: int64);
  protected
    FFlags: Word;
    FName,FExtension: utf8String;
    FUncompressedSize: int64;
    FEmbeddedStreamPos: int64;
    FMemDirectory: TMemDirectory;
    function GetName: utf8string; override;
    procedure SetName(AValue: utf8string); override;
    function GetFileSize: int64; override;
    function GetExtension: utf8string; override;
    function InternalCopyTo({%H-}ADestination: TStream): int64;
  public
    function CopyTo({%H-}ADestination: TStream): int64; override;
    constructor Create(AContainer: TMultiFileContainer; AFilename: TEntryFilename; AUncompressedStream: TStream; AOwnStream: boolean); overload;
    constructor CreateDirectory(AContainer: TMultiFileContainer; AFilename: TEntryFilename);
    constructor CreateFromData(AContainer: TMultiFileContainer; AFilename: TEntryFilename; AStream: TStream; AOwnStream: boolean; AUncompressedSize: int64; AFlags: Word);
    destructor Destroy; override;
    property EmbeddedStreamPos: int64 read FEmbeddedStreamPos write FEmbeddedStreamPos;
    property IsCompressed: boolean read GetIsCompressed write SetIsCompressed;
    property IsDirectory: boolean read GetIsDirectory;
    property CompressedSize: int64 read GetCompressedSize;
    property Flags: Word read FFlags;
    property MemDirectory: TMemDirectory read FMemDirectory;
  end;

  TMemDirectory = class(TMultiFileContainer)
  private
    FParentDirectory: TMemDirectory;
    function GetEntryCompressed(AIndex: integer): boolean;
    function GetIsDirectory(AIndex: integer): boolean;
    function GetDirectory(AIndex: integer): TMemDirectory;
    procedure SetEntryCompressed(AIndex: integer; AValue: boolean);
  protected
    function CreateEntry(AName: utf8string; AExtension: utf8string; AContent: TStream): TMultiFileEntry; override;
    function SplitPath(APath: utf8string): TMemDirectoryPath;
  public
    constructor Create(AParentDirectory: TMemDirectory = nil);
    function Equals(Obj: TObject): boolean; override;
    procedure LoadFromStream(AStream: TStream); override;
    class function CheckHeader(AStream: TStream): boolean; static;
    procedure LoadFromEmbeddedStream(ARootStream, ADataStream: TStream; AStartPos: int64);
    procedure SaveToStream(ADestination: TStream); override;
    procedure SaveToEmbeddedStream(ARootDest, ADataDest: TStream; AStartPos: int64);
    function AddDirectory(AName: utf8string; AExtension: utf8string= ''; ACaseSensitive: boolean= true): integer;
    function Rename(AName: utf8string; AExtension: utf8string; ANewName: utf8string; ACaseSensitive: boolean= true): boolean;
    function FindPath(APath: utf8String; ACaseSensitive: boolean = true): TMemDirectory;
    function FindEntry(APath: utf8String; ACaseSensitive: boolean = true): TMemDirectoryEntry;
    procedure CopyTo(ADest: TMemDirectory; ARecursive: boolean);
    property IsEntryCompressed[AIndex: integer]: boolean read GetEntryCompressed write SetEntryCompressed;
    property Directory[AIndex: integer]: TMemDirectory read GetDirectory;
    property IsDirectory[AIndex: integer]: boolean read GetIsDirectory;
    property ParentDirectory: TMemDirectory read FParentDirectory;
  end;

implementation

uses zstream, BGRAUTF8, strutils;

type
  TDirEntryRecord = packed record
    Flags: Word;
    FilenameSize: Word;
    Offset: int64;
  end;

{ TMemDirectory }

function TMemDirectory.GetEntryCompressed(AIndex: integer): boolean;
begin
  result := (Entry[AIndex] as TMemDirectoryEntry).IsCompressed;
end;

function TMemDirectory.GetIsDirectory(AIndex: integer): boolean;
begin
  result := (Entry[AIndex] as TMemDirectoryEntry).IsDirectory;
end;

function TMemDirectory.GetDirectory(AIndex: integer): TMemDirectory;
begin
  result := (Entry[AIndex] as TMemDirectoryEntry).MemDirectory;
end;

procedure TMemDirectory.SetEntryCompressed(AIndex: integer; AValue: boolean);
begin
  (Entry[AIndex] as TMemDirectoryEntry).IsCompressed := AValue;
end;

function TMemDirectory.CreateEntry(AName: utf8string; AExtension: utf8string;
  AContent: TStream): TMultiFileEntry;
begin
  result := TMemDirectoryEntry.Create(self, EntryFilename(AName, AExtension), AContent, true);
end;

procedure TMemDirectory.LoadFromStream(AStream: TStream);
var rootPos, rootSize: integer;
  header: string;
  rootStream: TStream;
  startPos: Int64;
begin
  startPos := AStream.Position;
  setlength(header, length(MemDirectoryFileHeader));
  AStream.ReadBuffer(header[1], length(header));
  if header<>MemDirectoryFileHeader then
    raise exception.Create('Invalid header');
  rootPos := LEReadInt64(AStream);
  if rootPos = 0 then
    raise exception.Create('Invalid root offset');
  rootSize := LEReadInt64(AStream);
  if rootSize <= 4 then
    raise exception.Create('Invalid root size');
  AStream.Position:= rootPos + startPos;
  rootStream:= TMemoryStream.Create;
  try
    rootStream.CopyFrom(AStream, rootSize);
    LoadFromEmbeddedStream(rootStream, AStream, startPos);
  finally
    rootStream.Free;
  end;
end;

class function TMemDirectory.CheckHeader(AStream: TStream): boolean;
var
  startPos: Int64;
  header: string;
begin
  startPos := AStream.Position;
  setlength(header, length(MemDirectoryFileHeader));
  AStream.Read(header[1], length(header));
  result := (header=MemDirectoryFileHeader);
  AStream.Position:= startPos;
end;

procedure TMemDirectory.LoadFromEmbeddedStream(ARootStream, ADataStream: TStream;
  AStartPos: int64);
var
  nbEntries,i: LongInt;
  entryRec: TDirEntryRecord;
  filename: string;
  entryData: TStream;
  newEntry: TMemDirectoryEntry;
  compressedSize, uncompressedSize: Int64;

begin
  Clear;
  ARootStream.Position := 0;
  nbEntries := LEReadLongint(ARootStream);
  for i := 1 to nbEntries do
  begin
    ARootStream.ReadBuffer({%H-}entryRec, sizeof(entryRec));
    entryRec.Offset:= LEtoN(entryRec.Offset);
    entryRec.Flags:= LEtoN(entryRec.Flags);
    entryRec.FilenameSize:= LEtoN(entryRec.FilenameSize);

    if (entryRec.Flags and MemDirectoryEntry_FlagSmallEntryPacked) <> 0 then
    begin
      entryRec.Flags := entryRec.Flags xor MemDirectoryEntry_FlagSmallEntryPacked;
      compressedSize := entryRec.FilenameSize shr 8;
      uncompressedSize := compressedSize;
      entryRec.FilenameSize := entryRec.FilenameSize and 255;
    end else
    begin
      compressedSize := LEReadInt64(ARootStream);
      uncompressedSize := LEReadInt64(ARootStream);
    end;

    setlength(filename, entryRec.FilenameSize);
    if length(filename)> 0 then
      ARootStream.ReadBuffer(filename[1], entryRec.FilenameSize);

    ADataStream.Position:= entryRec.Offset + AStartPos;
    entryData := TMemoryStream.Create;
    try
      if compressedSize <> 0 then
        entryData.CopyFrom(ADataStream, compressedSize);
      newEntry := TMemDirectoryEntry.CreateFromData(self, EntryFilename(filename), entryData, true,
                  uncompressedSize, entryRec.Flags);
      newEntry.LoadExtraFromEmbeddedStream(ADataStream, AStartPos);
      AddEntry(newEntry);
      entryData := nil;
    finally
      entryData.Free;
    end;
  end;
end;

procedure TMemDirectory.SaveToStream(ADestination: TStream);
var rootPos,rootSize: integer;
  header: string;
  rootRecPos, startPos, endPos: int64;
  rootStream: TStream;
begin
  startPos := ADestination.Position;
  header := MemDirectoryFileHeader;
  ADestination.WriteBuffer(header[1], length(header));

  rootRecPos := ADestination.Position;
  LEWriteInt64(ADestination,0); //root pos
  LEWriteInt64(ADestination,0); //root size

  rootStream := TMemoryStream.Create;
  try
    SaveToEmbeddedStream(rootStream, ADestination, startPos);
    rootStream.Position := 0;
    rootPos := ADestination.Position - startPos;
    rootSize := rootStream.Size;
    ADestination.CopyFrom(rootStream, rootStream.Size);
    FreeAndNil(rootStream);
    endPos := ADestination.Position;
    ADestination.Position := rootRecPos;
    LEWriteInt64(ADestination, rootPos);
    LEWriteInt64(ADestination, rootSize);
    ADestination.Position := endPos;
  finally
    rootStream.Free;
  end;
end;

procedure TMemDirectory.SaveToEmbeddedStream(ARootDest, ADataDest: TStream;
  AStartPos: int64);
var
  entryRec: TDirEntryRecord;
  entryStream: TMemoryStream;
  curEntry: TMemDirectoryEntry;
  filename: string;
  i: Integer;
  uncompressedSize: int64;
begin
  LEWriteLongint(ARootDest, Count);
  entryStream := TMemoryStream.Create;
  try
    for i := 0 to Count-1 do
    begin
      curEntry := Entry[i] as TMemDirectoryEntry;
      entryStream.Clear;
      curEntry.SaveToEmbeddedStream(entryStream, ADataDest, AStartPos, uncompressedSize);

      entryRec.Offset:= ADataDest.Position - AStartPos;
      entryRec.Offset:= NtoLE(entryRec.Offset);
      if curEntry.Extension <> '' then
        filename := curEntry.Name+'.'+curEntry.Extension
      else
        filename := curEntry.Name;

      if ((curEntry.Flags and MemDirectoryEntry_FlagCompressed)=0) and
         (Length(filename)<=255) and (entryStream.Size<=255) then
      begin
        entryRec.Flags:= curEntry.Flags or MemDirectoryEntry_FlagSmallEntryPacked;
        entryRec.Flags:= NtoLE(entryRec.Flags);
        entryRec.FilenameSize:= length(filename) + (entryStream.Size shl 8);
        entryRec.FilenameSize := NtoLE(entryRec.FilenameSize);
        ARootDest.WriteBuffer(entryRec, sizeof(entryRec));
      end else
      begin
        entryRec.Flags:= curEntry.Flags;
        entryRec.Flags:= NtoLE(entryRec.Flags);
        entryRec.FilenameSize:= length(filename);
        entryRec.FilenameSize := NtoLE(entryRec.FilenameSize);
        ARootDest.WriteBuffer(entryRec, sizeof(entryRec));
        LEWriteInt64(ARootDest, entryStream.Size);
        LEWriteInt64(ARootDest, uncompressedSize);
      end;

      if filename <> '' then
        ARootDest.WriteBuffer(filename[1], length(filename));

      entryStream.Position:= 0;
      ADataDest.CopyFrom(entryStream, entryStream.Size);
    end;
  finally
    entryStream.Free;
  end;
end;

function TMemDirectory.AddDirectory(AName: utf8string; AExtension: utf8string;
  ACaseSensitive: boolean): integer;
var
  newEntry: TMemDirectoryEntry;
begin
  result := IndexOf(AName,AExtension,ACaseSensitive);
  if result <> -1 then
  begin
    if not IsDirectory[result] then
      raise exception.Create('There is already a file with this name and extension');
    exit;
  end;
  newEntry := TMemDirectoryEntry.CreateDirectory(self, EntryFilename(AName, AExtension));
  result := AddEntry(newEntry);
end;

function TMemDirectory.Rename(AName: utf8string; AExtension: utf8string;
  ANewName: utf8string; ACaseSensitive: boolean): boolean;
var
  idx, i: Integer;
begin
  idx := IndexOf(AName, AExtension, ACaseSensitive);
  if idx = -1 then exit(false);
  for i := 0 to Count-1 do
  if i <> idx then
  begin
    if Entry[i].CompareNameAndExtension(ANewName,AExtension,ACaseSensitive) = 0 then
      raise exception.Create('Name with extension already in use');
  end;
  Entry[idx].Name := ANewName;
  exit(true);
end;

function TMemDirectory.FindPath(APath: utf8String; ACaseSensitive: boolean): TMemDirectory;
var
  path: TMemDirectoryPath;
  idxPath: integer;
  idxSub: LongInt;
begin
  path := SplitPath(APath);
  result := self;
  if path.Items[0].IsEmpty then
  begin
    idxPath := 1;
    while Assigned(result.ParentDirectory) do result := result.ParentDirectory;
  end
  else
    idxPath := 0;

  while idxPath < path.Count do
  begin
    idxSub := result.IndexOf(path[idxPath], ACaseSensitive);
    if idxSub= -1 then
    begin
      result := nil;
      break;
    end;
    result := result.Directory[idxSub];
    inc(idxPath);
  end;

  path.Free;
end;

function TMemDirectory.FindEntry(APath: utf8String; ACaseSensitive: boolean): TMemDirectoryEntry;
var
  path: TMemDirectoryPath;
  idxPath: integer;
  idxSub, idxEntry: LongInt;
  curDir: TMemDirectory;
begin
  path := SplitPath(APath);
  curDir := self;
  if path.Items[0].IsEmpty then
  begin
    idxPath := 1;
    while Assigned(curDir.ParentDirectory) do curDir := curDir.ParentDirectory;
  end
  else
    idxPath := 0;

  while idxPath < path.Count-1 do
  begin
    idxSub := curDir.IndexOf(path[idxPath], ACaseSensitive);
    if idxSub= -1 then
    begin
      curDir := nil;
      break;
    end;
    curDir := curDir.Directory[idxSub];
    inc(idxPath);
  end;

  if Assigned(curDir) and (idxPath < path.Count) then
  begin
    idxEntry := curDir.IndexOf(path[idxPath], ACaseSensitive);
    if idxEntry = -1 then
      result := nil
    else
      result := curDir.Entry[idxEntry] as TMemDirectoryEntry;
  end
  else
    result := nil;

  path.Free;
end;

procedure TMemDirectory.CopyTo(ADest: TMemDirectory; ARecursive: boolean);
var
  i, idxDir: Integer;
  entryContent: TMemoryStream;
begin
  for i := 0 to Count-1 do
    if IsDirectory[i] and ARecursive then
    begin
      idxDir := ADest.AddDirectory(Entry[i].Name,Entry[i].Extension);
      Directory[i].CopyTo(ADest.Directory[idxDir], true);
    end else
    begin
      entryContent := TMemoryStream.Create;
      Entry[i].CopyTo(entryContent);
      ADest.Add(Entry[i].Name,Entry[i].Extension,entryContent,false,true);
    end;
end;

function TMemDirectory.SplitPath(APath: utf8string): TMemDirectoryPath;
var idx,idxSlash: integer;
begin
  result := TMemDirectoryPath.Create;
  idx := 1;
  repeat
    idxSlash := PosEx('/',APath,idx);
    if idxSlash = 0 then
    begin
      result.Add(EntryFilename(copy(APath, idx, length(APath)-idx+1)));
      break;
    end else
    begin
      result.Add(EntryFilename(copy(APath, idx, idxSlash-idx)));
      idx := idxSlash+1;
    end;
  until false;
end;

constructor TMemDirectory.Create(AParentDirectory: TMemDirectory);
begin
  inherited Create;
  FParentDirectory := AParentDirectory;
end;

function TMemDirectory.Equals(Obj: TObject): boolean;
var
  other: TMemDirectory;
  i, j: Integer;
  data,otherData: TMemoryStream;
  different: Boolean;
begin
  if Obj = self then exit(true);
  if not (Obj is TMemDirectory) then exit(false);
  other := TMemDirectory(Obj);
  if other.Count <> Count then exit(false);
  for i := 0 to Count-1 do
  begin
    j := other.IndexOf(Entry[i].Name,Entry[i].Extension,true);
    if j = -1 then exit(false);
    if IsDirectory[i] then
    begin
      if not other.IsDirectory[j] then exit(false);
      if not other.Directory[j].Equals(Directory[i]) then exit(false);
    end else
    if Entry[i].FileSize <> other.Entry[j].FileSize then exit(false)
    else
    begin
      data := TMemoryStream.Create;
      otherData := TMemoryStream.Create;
      Entry[i].CopyTo(data);
      other.Entry[j].CopyTo(otherData);
      different := not CompareMem(data.Memory, otherData.Memory, data.Size);
      data.Free;
      otherData.Free;
      if different then exit(false);
    end;
  end;
  result := true;
end;

{ TMemDirectoryEntry }

function TMemDirectoryEntry.GetIsCompressed: boolean;
begin
  result := (FFlags and MemDirectoryEntry_FlagCompressed) <> 0;
end;

function TMemDirectoryEntry.GetCompressedSize: int64;
begin
  if not IsDirectory and Assigned(FStream) then
    result := FStream.Size
  else
    result := 0;
end;

function TMemDirectoryEntry.GetIsDirectory: boolean;
begin
  result := (FFlags and MemDirectoryEntry_FlagDirectory) <> 0;
end;

procedure TMemDirectoryEntry.SetIsCompressed(AValue: boolean);
var compressedStream,decompressed: TMemoryStream;
  compression: Tcompressionstream;
begin
  if AValue = IsCompressed then exit;

  if Assigned(FStream) then
  begin
    if AValue then //compress
    begin
      compressedStream := TMemoryStream.Create;
      compression := nil;
      try
        compression := Tcompressionstream.create(cldefault, compressedStream, true);
        FStream.Position := 0;
        compression.CopyFrom(FStream,FStream.Size);
        FStream.Free;
        FStream := compressedStream;
        compressedStream := nil;
        FFlags := FFlags xor MemDirectoryEntry_FlagCompressed;
      finally
        compression.Free;
        compressedStream.Free;
      end;
    end else
    begin //decompress
      decompressed := TMemoryStream.Create;
      try
        InternalCopyTo(decompressed);
        FStream.Free;
        FStream := decompressed;
        decompressed := nil;
        FFlags := FFlags xor MemDirectoryEntry_FlagCompressed;
      finally
        decompressed.Free;
      end;
    end;
  end else
    FFlags := FFlags xor MemDirectoryEntry_FlagCompressed;
end;

function TMemDirectoryEntry.GetName: utf8string;
begin
  result := FName;
end;

procedure TMemDirectoryEntry.SetName(AValue: utf8string);
begin
  while AValue[length(AValue)] = '.' do delete(AValue, length(AValue), 1);
  FName := AValue;
end;

function TMemDirectoryEntry.GetFileSize: int64;
begin
  if IsDirectory then
    result := 0
  else
    Result:= FUncompressedSize;
end;

function TMemDirectoryEntry.GetExtension: utf8string;
begin
  Result:= FExtension;
end;

function TMemDirectoryEntry.InternalCopyTo(ADestination: TStream): int64;
var
  decomp: Tdecompressionstream;
begin
  if not Assigned(FStream) then exit(0);
  if IsCompressed then
  begin
    FStream.Position := 0;
    decomp := Tdecompressionstream.Create(FStream,true);
    try
      result := ADestination.CopyFrom(decomp,FUncompressedSize);
    finally
      decomp.Free;
    end;
  end else
  begin
    FStream.Position := 0;
    result := ADestination.CopyFrom(FStream, FStream.Size);
  end;
end;

function TMemDirectoryEntry.CopyTo(ADestination: TStream): int64;
begin
  if IsDirectory then exit(0);
  result := InternalCopyTo(ADestination);
end;

constructor TMemDirectoryEntry.Create(AContainer: TMultiFileContainer; AFilename: TEntryFilename;
  AUncompressedStream: TStream; AOwnStream: boolean);
begin
  CreateFromData(AContainer, AFilename, AUncompressedStream, AOwnStream, AUncompressedStream.Size, 0);
end;

constructor TMemDirectoryEntry.CreateFromData(AContainer: TMultiFileContainer; AFilename: TEntryFilename;
  AStream: TStream; AOwnStream: boolean;
  AUncompressedSize: int64; AFlags: Word);
begin
  inherited Create(AContainer);
  Name := AFilename.Name;
  FExtension:= AFilename.Extension;
  if AOwnStream then
    FStream := AStream
  else
  begin
    FStream := TMemoryStream.Create;
    AStream.Position:= 0;
    FStream.CopyFrom(AStream, AStream.Size);
  end;
  FUncompressedSize:= AUncompressedSize;
  FFlags:= AFlags;
  FMemDirectory := nil;
end;

procedure TMemDirectoryEntry.SaveToEmbeddedStream(AEntryStream, ADataStream: TStream;
  AStartPos: int64; out uncompressedSize: int64);
var
  entryStartPos: Int64;
begin
  if IsDirectory then
  begin
    if not Assigned(FMemDirectory) then
      raise exception.Create('Directory not allocated');
    FreeAndNil(FStream);
    IsCompressed:= false;
    entryStartPos := AEntryStream.Position;
    FMemDirectory.SaveToEmbeddedStream(AEntryStream, ADataStream, AStartPos);
    uncompressedSize:= AEntryStream.Position - entryStartPos;
  end else
  if Assigned(FStream) then
  begin
    FStream.Position:= 0;
    AEntryStream.CopyFrom(FStream, FStream.Size);
    uncompressedSize:= FUncompressedSize;
  end;
end;

procedure TMemDirectoryEntry.LoadExtraFromEmbeddedStream(ADataStream: TStream;
  AStartPos: int64);
begin
  if IsDirectory and Assigned(FStream) then
  begin
    IsCompressed:= false;
    if not Assigned(FMemDirectory) then
      FMemDirectory := TMemDirectory.Create(Container as TMemDirectory);
    FMemDirectory.LoadFromEmbeddedStream(FStream, ADataStream, AStartPos);
    FreeAndNil(FStream);
  end;
end;

constructor TMemDirectoryEntry.CreateDirectory(AContainer: TMultiFileContainer;
  AFilename: TEntryFilename);
begin
  Name := AFilename.Name;
  FExtension:= AFilename.Extension;
  FStream := nil;
  FUncompressedSize:= 0;
  FFlags := MemDirectoryEntry_FlagDirectory;
  FContainer := AContainer;
  FMemDirectory := TMemDirectory.Create(Container as TMemDirectory);
end;

destructor TMemDirectoryEntry.Destroy;
begin
  FStream.Free;
  FMemDirectory.Free;
  inherited Destroy;
end;

end.

