unit BGRAMultiFileType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TMultiFileContainer = class;

  { TMultiFileEntry }

  TMultiFileEntry = class
  protected
    FContainer: TMultiFileContainer;
    function GetName: utf8string; virtual; abstract;
    procedure SetName(AValue: utf8string); virtual; abstract;
    function GetFileSize: int64; virtual;
    function GetExtension: utf8string; virtual;
  public
    constructor Create(AContainer: TMultiFileContainer);
    function CopyTo({%H-}ADestination: TStream): integer; virtual;
    property Name: utf8string read GetName write SetName;
    property Extension: utf8string read GetExtension;
    property FileSize: int64 read GetFileSize;
    property Container: TMultiFileContainer read FContainer;
  end;

  TMultiFileEntryList = specialize TFPGList<TMultiFileEntry>;

  { TMultiFileContainer }

  TMultiFileContainer = class
  private
    FEntries: TMultiFileEntryList;
  protected
    procedure Init; virtual;
    function AddEntry(AEntry: TMultiFileEntry): integer;
    function GetCount: integer;
    function GetEntry(AIndex: integer): TMultiFileEntry;
    function CreateEntry(AName: utf8string; AExtension: utf8string; AContent: TStream): TMultiFileEntry; virtual; abstract;
  public
    constructor Create;
    constructor Create(AFilename: utf8string);
    constructor Create(AStream: TStream);
    constructor Create(AStream: TStream; AStartPos: Int64);
    function Add(AName: utf8string; AExtension: utf8string; AContent: TStream; AOverwrite: boolean = false; AOwnStream: boolean = true): integer;
    function Add(AName: utf8string; AExtension: utf8string; AContent: utf8String; AOverwrite: boolean = false): integer;
    procedure Clear; virtual;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename: utf8string);
    procedure LoadFromStream(AStream: TStream); virtual; abstract;
    procedure SaveToFile(AFilename: utf8string);
    procedure SaveToStream(ADestination: TStream); virtual; abstract;
    procedure Remove(AEntry: TMultiFileEntry); virtual;
    procedure Delete(AIndex: integer); virtual; overload;
    function Delete(AName: utf8string; AExtension: utf8string;ACaseSensitive: boolean = True): boolean; overload;
    function IndexOf(AEntry: TMultiFileEntry): integer;
    function IndexOf(AName: utf8string; AExtenstion: utf8string; ACaseSensitive: boolean = True): integer; virtual;
    property Count: integer read GetCount;
    property Entry[AIndex: integer]: TMultiFileEntry read GetEntry;
  end;

implementation

uses BGRAUTF8;

{ TMultiFileEntry }

function TMultiFileEntry.GetFileSize: int64;
begin
  result := 0;
end;

function TMultiFileEntry.GetExtension: utf8string;
begin
  result := '';
end;

constructor TMultiFileEntry.Create(AContainer: TMultiFileContainer);
begin
  FContainer := AContainer;
end;

function TMultiFileEntry.CopyTo(ADestination: TStream): integer;
begin
  result := 0;
end;

{ TMultiFileContainer }

function TMultiFileContainer.GetCount: integer;
begin
  result := FEntries.Count;
end;

function TMultiFileContainer.GetEntry(AIndex: integer): TMultiFileEntry;
begin
  result := FEntries[AIndex];
end;

procedure TMultiFileContainer.Init;
begin
  FEntries := TMultiFileEntryList.Create;
end;

function TMultiFileContainer.AddEntry(AEntry: TMultiFileEntry): integer;
begin
  result := FEntries.Add(AEntry);
end;

constructor TMultiFileContainer.Create;
begin
  Init;
end;

constructor TMultiFileContainer.Create(AFilename: utf8string);
begin
  Init;
  LoadFromFile(AFilename);
end;

constructor TMultiFileContainer.Create(AStream: TStream);
begin
  Init;
  LoadFromStream(AStream);
end;

constructor TMultiFileContainer.Create(AStream: TStream; AStartPos: Int64);
begin
  Init;
  AStream.Position := AStartPos;
  LoadFromStream(AStream);
end;

function TMultiFileContainer.Add(AName: utf8string; AExtension: utf8string;
  AContent: TStream; AOverwrite: boolean; AOwnStream: boolean): integer;
var
  index: Integer;
  newEntry: TMultiFileEntry;
  contentCopy: TMemoryStream;
begin
  index := IndexOf(AName,AExtension);
  if index <> -1 then
  begin
    if AOverwrite then
      Delete(index)
    else
      raise Exception.Create('Duplicate entry');
  end;
  if not AOwnStream then
  begin
    AContent.Position:= 0;
    contentCopy := TMemoryStream.Create;
    contentCopy.CopyFrom(AContent, AContent.Size);
    newEntry := CreateEntry(AName, AExtension, contentCopy);
  end else
    newEntry := CreateEntry(AName, AExtension, AContent);
  if Assigned(newEntry) then
    result := AddEntry(newEntry)
  else
    raise exception.Create('Unable to create entry');
end;

function TMultiFileContainer.Add(AName: utf8string; AExtension: utf8string;
  AContent: utf8String; AOverwrite: boolean): integer;
var stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  stream.Write(AContent[1],length(AContent));
  result := Add(AName,AExtension,stream,AOverwrite);
end;

destructor TMultiFileContainer.Destroy;
begin
  Clear;
  FreeAndNil(FEntries);
  inherited Destroy;
end;

procedure TMultiFileContainer.LoadFromFile(AFilename: utf8string);
var stream: TFileStream;
begin
  stream := TFileStream.Create(Utf8ToAnsi(AFilename), fmOpenRead);
  LoadFromStream(stream);
  stream.Free;
end;

procedure TMultiFileContainer.SaveToFile(AFilename: utf8string);
var stream: TFileStream;
begin
  stream := TFileStream.Create(Utf8ToAnsi(AFilename), fmCreate);
  SaveToStream(stream);
  stream.Free;
end;

procedure TMultiFileContainer.Remove(AEntry: TMultiFileEntry);
var
  index: Integer;
begin
  index := IndexOf(AEntry);
  if index = -1 then
    raise exception.Create('Entry not found');
  Delete(index);
end;

procedure TMultiFileContainer.Delete(AIndex: integer);
begin
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    Entry[AIndex].Free;
    FEntries.Delete(AIndex);
  end else
    raise ERangeError.Create('Index out of bounds');
end;

function TMultiFileContainer.Delete(AName: utf8string; AExtension: utf8string;
  ACaseSensitive: boolean): boolean;
var
  index: Integer;
begin
  index := IndexOf(AName, AExtension, ACaseSensitive);
  if index = -1 then
    result := false
  else
  begin
    Delete(index);
    result := true;
  end;
end;

function TMultiFileContainer.IndexOf(AEntry: TMultiFileEntry): integer;
begin
  result := FEntries.IndexOf(AEntry);
end;

function TMultiFileContainer.IndexOf(AName: utf8string; AExtenstion: utf8string; ACaseSensitive: boolean): integer;
var
  i: Integer;
begin
  if ACaseSensitive then
  begin
    for i := 0 to Count-1 do
      if (Entry[i].Name = AName) and (UTF8CompareText(Entry[i].Extension,AExtenstion) = 0) then
      begin
        result := i;
        exit;
      end;
  end else
    for i := 0 to Count-1 do
      if (UTF8CompareText(Entry[i].Name,AName) = 0) and (UTF8CompareText(Entry[i].Extension,AExtenstion) = 0) then
      begin
        result := i;
        exit;
      end;
  result := -1;
end;

procedure TMultiFileContainer.Clear;
var
  i: Integer;
begin
  for i := 0 to FEntries.Count-1 do
    FEntries.Items[i].Free;
  FEntries.Clear;
end;

end.

