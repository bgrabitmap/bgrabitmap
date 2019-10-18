unit BGRAMultiFileType;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, fgl;

type

  { TEntryFilename }

  TEntryFilename = record
  private
    FExtension: utf8string;
    FName: utf8string;
    function GetFilename: utf8string;
    function GetIsEmpty: boolean;
    procedure SetExtension(AValue: utf8string);
    procedure SetFilename(AValue: utf8string);
    procedure SetName(AValue: utf8string);
  public
    class operator =(const AValue1,AValue2: TEntryFilename): boolean;
    property Filename: utf8string read GetFilename write SetFilename;
    property Name: utf8string read FName write SetName;
    property Extension: utf8string read FExtension write SetExtension;
    property IsEmpty: boolean read GetIsEmpty;
  end;

function EntryFilename(AName,AExtension: string): TEntryFilename; overload;
function EntryFilename(AFilename: string): TEntryFilename; overload;

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
    function CopyTo({%H-}ADestination: TStream): int64; virtual;
    function CompareNameAndExtension(AName: utf8string; AExtension: utf8string; ACaseSensitive: boolean = true): integer;
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
    function AddEntry(AEntry: TMultiFileEntry; AIndex: integer = -1): integer;
    function GetCount: integer;
    function GetEntry(AIndex: integer): TMultiFileEntry;
    function CreateEntry(AName: utf8string; AExtension: utf8string; AContent: TStream): TMultiFileEntry; virtual; abstract;
    function GetRawString(AIndex: integer): RawByteString;
    function GetRawStringByFilename(AFilename: string): RawByteString;
    procedure SetRawString(AIndex: integer; AValue: RawByteString);
    procedure SetRawStringByFilename(AFilename: string; AValue: RawByteString);
  public
    constructor Create; overload;
    constructor Create(AFilename: utf8string); overload;
    constructor Create(AStream: TStream); overload;
    constructor Create(AStream: TStream; AStartPos: Int64); overload;
    function Add(AName: utf8string; AExtension: utf8string; AContent: TStream; AOverwrite: boolean = false; AOwnStream: boolean = true): integer; overload;
    function Add(AName: utf8string; AExtension: utf8string; AContent: RawByteString; AOverwrite: boolean = false): integer; overload;
    function Add(AFilename: TEntryFilename; AContent: TStream; AOverwrite: boolean = false; AOwnStream: boolean = true): integer; overload;
    function Add(AFilename: TEntryFilename; AContent: RawByteString; AOverwrite: boolean = false): integer; overload;
    procedure Clear; virtual;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename: utf8string);
    procedure LoadFromStream(AStream: TStream); virtual; abstract;
    procedure LoadFromResource(AFilename: string); virtual;
    procedure SaveToFile(AFilename: utf8string);
    procedure SaveToStream(ADestination: TStream); virtual; abstract;
    procedure Remove(AEntry: TMultiFileEntry); virtual;
    procedure Delete(AIndex: integer); overload; virtual;
    function Delete(AName: utf8string; AExtension: utf8string; ACaseSensitive: boolean = True): boolean; overload;
    function Delete(AFilename: TEntryFilename; ACaseSensitive: boolean = True): boolean; overload;
    function IndexOf(AEntry: TMultiFileEntry): integer; overload;
    function IndexOf(AName: utf8string; AExtenstion: utf8string; ACaseSensitive: boolean = True): integer; overload; virtual;
    function IndexOf(AFilename: TEntryFilename; ACaseSensitive: boolean = True): integer; overload;
    property Count: integer read GetCount;
    property Entry[AIndex: integer]: TMultiFileEntry read GetEntry;
    property RawString[AIndex: integer]: RawByteString read GetRawString write SetRawString;
    property RawStringByFilename[AFilename: string]: RawByteString read GetRawStringByFilename write SetRawStringByFilename;
  end;

implementation

uses BGRAUTF8, strutils, BGRABitmapTypes;

{ TEntryFilename }

function TEntryFilename.GetFilename: utf8string;
begin
  if Extension = '' then
    result := Name
  else
    result := Name+'.'+Extension;
end;

function TEntryFilename.GetIsEmpty: boolean;
begin
  result := (FName='') and (FExtension = '');
end;

procedure TEntryFilename.SetExtension(AValue: utf8string);
var
  i: Integer;
begin
  if FExtension=AValue then Exit;
  for i := 1 to length(AValue) do
    if AValue[i] in ['.','/'] then
      raise Exception.Create('Invalid extension');
  FExtension:=AValue;
end;

procedure TEntryFilename.SetFilename(AValue: utf8string);
var
  idxDot: SizeInt;
begin
  idxDot := RPos('.',AValue);
  if idxDot = 0 then
  begin
    Name := AValue;
    Extension := '';
  end
  else
  begin
    Name := copy(AValue,1,idxDot-1);
    Extension := copy(AValue,idxDot+1,length(AValue)-idxDot);
  end;
end;

procedure TEntryFilename.SetName(AValue: utf8string);
var
  i: Integer;
begin
  if FName=AValue then Exit;
  for i := 1 to length(AValue) do
    if AValue[i] = '/' then
      raise Exception.Create('Invalid name');
  FName:=AValue;
end;

function EntryFilename(AName, AExtension: string): TEntryFilename;
begin
  result.Name := AName;
  result.Extension:= AExtension;
end;

function EntryFilename(AFilename: string): TEntryFilename;
begin
  result.Filename:= AFilename;
end;

class operator TEntryFilename.=(const AValue1, AValue2: TEntryFilename): boolean;
begin
  result := (AValue1.Name = AValue2.Name) and (AValue1.Extension = AValue2.Extension);
end;

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

function TMultiFileEntry.CopyTo(ADestination: TStream): int64;
begin
  result := 0;
end;

function TMultiFileEntry.CompareNameAndExtension(AName: utf8string;
  AExtension: utf8string; ACaseSensitive: boolean): integer;
begin
  if ACaseSensitive then
    result := CompareStr(AName, Name)
  else
    result := UTF8CompareText(AName, Name);

  if result = 0 then
    result := UTF8CompareText(AExtension, Extension);
end;

{ TMultiFileContainer }

function TMultiFileContainer.GetCount: integer;
begin
  if Assigned(FEntries) then
    result := FEntries.Count
  else
    result := 0;
end;

function TMultiFileContainer.GetEntry(AIndex: integer): TMultiFileEntry;
begin
  result := FEntries[AIndex];
end;

function TMultiFileContainer.GetRawString(AIndex: integer): RawByteString;
var s: TMemoryStream;
begin
  s := TMemoryStream.Create;
  try
    Entry[AIndex].CopyTo(s);
    setlength(result, s.Size);
    if length(result)>0 then
      move(s.Memory^, result[1], length(result));
  finally
    s.Free;
  end;
end;

function TMultiFileContainer.GetRawStringByFilename(AFilename: string
  ): RawByteString;
var
  idx: Integer;
begin
  idx := IndexOf(EntryFilename(AFilename));
  if idx = -1 then
    result := ''
  else
    result := GetRawString(idx);
end;

procedure TMultiFileContainer.SetRawString(AIndex: integer;
  AValue: RawByteString);
begin
  with Entry[AIndex] do
    Add(Name, Extension, AValue, true);
end;

procedure TMultiFileContainer.SetRawStringByFilename(AFilename: string;
  AValue: RawByteString);
var
  f: TEntryFilename;
begin
  f := EntryFilename(AFilename);
  Add(f.Name,f.Extension,AValue,true);
end;

procedure TMultiFileContainer.Init;
begin
  FEntries := TMultiFileEntryList.Create;
end;

function TMultiFileContainer.AddEntry(AEntry: TMultiFileEntry; AIndex: integer): integer;
begin
  if not Assigned(FEntries) then
    raise exception.Create('Entry list not created');
  if (AIndex >= 0) and (AIndex < FEntries.Count) then
  begin
    FEntries.Insert(AIndex, AEntry);
    result := AIndex;
  end
  else
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
    result := AddEntry(newEntry, index)
  else
    raise exception.Create('Unable to create entry');
end;

function TMultiFileContainer.Add(AName: utf8string; AExtension: utf8string;
  AContent: RawByteString; AOverwrite: boolean): integer;
var stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  if length(AContent) > 0 then stream.Write(AContent[1],length(AContent));
  result := Add(AName,AExtension,stream,AOverwrite);
end;

function TMultiFileContainer.Add(AFilename: TEntryFilename; AContent: TStream;
  AOverwrite: boolean; AOwnStream: boolean): integer;
begin
  result := Add(AFilename.Name,AFilename.Extension, AContent, AOverwrite, AOwnStream);
end;

function TMultiFileContainer.Add(AFilename: TEntryFilename;
  AContent: RawByteString; AOverwrite: boolean): integer;
begin
  result := Add(AFilename.Name,AFilename.Extension, AContent, AOverwrite);
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

procedure TMultiFileContainer.LoadFromResource(AFilename: string);
var
  stream: TStream;
begin
  stream := BGRAResource.GetResourceStream(AFilename);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
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

function TMultiFileContainer.Delete(AFilename: TEntryFilename;
  ACaseSensitive: boolean): boolean;
begin
  result := Delete(AFilename.Name,AFilename.Extension,ACaseSensitive);
end;

function TMultiFileContainer.IndexOf(AEntry: TMultiFileEntry): integer;
begin
  result := FEntries.IndexOf(AEntry);
end;

function TMultiFileContainer.IndexOf(AName: utf8string; AExtenstion: utf8string; ACaseSensitive: boolean): integer;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    if Entry[i].CompareNameAndExtension(AName, AExtenstion, ACaseSensitive) = 0 then
      exit(i);
  result := -1;
end;

function TMultiFileContainer.IndexOf(AFilename: TEntryFilename;
  ACaseSensitive: boolean): integer;
begin
  result := IndexOf(AFilename.Name,AFilename.Extension,ACaseSensitive);
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

