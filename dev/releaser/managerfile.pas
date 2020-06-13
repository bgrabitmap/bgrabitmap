// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
unit ManagerFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ReleaserTypes,
  fpjson, jsonparser, ArchiveUrl, PackageFile;

type

  { TManagerFile }

  TManagerFile = class(TReleaserObject)
  private
    FArchive: TArchiveUrl;
    FFilename: string;
    FPackages: TPackageFileList;
    FRoot: TJSONObject;
    FNew, FChanged: boolean;
    procedure SetArchive(AValue: TArchiveUrl);
    function GetPackageUpdateList: TJSONArray;
    function GetDataNode: TJSONObject;
    function GetDownloadUrl: string;
  public
    constructor Create(AParameters: TStringList; ALogicDir: string); override;
    destructor Destroy; override;
    class function IsUnique: boolean; override;
    procedure LinkWith(AOtherObject: TReleaserObject); override;
    property Archive: TArchiveUrl read FArchive write SetArchive;
    procedure GetVersions({%H-}AVersionList: TStringList); override;
    procedure CheckVersion(AVersion: TVersion); override;
    procedure UpdateVersion(AVersion: TVersion); override;
    procedure Save; override;
  end;

implementation

{ TManagerFile }

procedure TManagerFile.SetArchive(AValue: TArchiveUrl);
begin
  if FArchive=AValue then Exit;
  FArchive:=AValue;
end;

function TManagerFile.GetPackageUpdateList: TJSONArray;
const packagePath1 = 'UpdateLazPackages';
      packagePath2 = 'UpdatePackageFiles';
var
  node: TJSONData;
begin
  node := FRoot.FindPath(packagePath1);
  if node = nil then node := FRoot.FindPath(packagePath2);
  if node <> nil then result := node as TJSONArray
  else
  begin
    result := TJSONArray.Create;
    FRoot.Add(packagePath1, result);
  end;
end;

function TManagerFile.GetDataNode: TJSONObject;
const dataPath = 'UpdatePackageData';
var
  node: TJSONData;
begin
  node := FRoot.FindPath(dataPath);
  if node <> nil then result := node as TJSONObject
  else
  begin
    result := TJSONObject.Create;
    FRoot.Add(dataPath, result);
  end;
end;

function TManagerFile.GetDownloadUrl: string;
var
  url: string;
  data: TJSONObject;
begin
  if Archive <> nil then
  begin
    data := GetDataNode;
    url := '';
    url := data.Get('DownloadZipURL', url);
    result := url;
  end
  else result := '';
end;

constructor TManagerFile.Create(AParameters: TStringList; ALogicDir: string);
var
  stream: TFileStream;
begin
  inherited Create(AParameters, ALogicDir);
  ExpectParamCount(1);
  FFilename:= ExpandFileName(ReplaceVariables(Param[0]));
  if FileExists(FFilename) then
  begin
    FNew := false;
    stream := TFileStream.Create(FFilename, fmOpenRead);
    try
      FRoot := GetJSON(stream) as TJSONObject;
    finally
      stream.Free;
    end;
  end else
  begin
    FNew := true;
    FRoot := TJSONObject.Create;
  end;
  FPackages := TPackageFileList.Create;
end;

destructor TManagerFile.Destroy;
begin
  FPackages.Free;
  FRoot.Free;
  inherited Destroy;
end;

class function TManagerFile.IsUnique: boolean;
begin
  Result:= true;
end;

procedure TManagerFile.LinkWith(AOtherObject: TReleaserObject);
var
  updateList: TJSONArray;
  packageEntry: TJSONObject;
  name: String;
  i: Integer;
  updateName: string;
  package: TPackageFile;
  updateVer: TVersion;
begin
  inherited LinkWith(AOtherObject);
  if AOtherObject is TArchiveUrl then
    Archive := TArchiveUrl(AOtherObject)
  else if AOtherObject is TPackageFile then
  begin
    package := TPackageFile(AOtherObject);
    name := ExtractFileName(package.Filename);
    updateList := GetPackageUpdateList;
    if FNew then
    begin
      packageEntry := TJSONObject.Create;
      packageEntry.Booleans['ForceNotify'] := false;
      packageEntry.Integers['InternalVersion'] := 1;
      packageEntry.Strings['Name'] := name;
      packageEntry.Strings['Version'] := VersionToStr(package.GetVersion, true);
      updateList.Add(packageEntry);
      FPackages.Add(package);
    end else
      for i := 0 to updateList.Count-1 do
      begin
        packageEntry := updateList.Items[i] as TJSONObject;
        updateName := packageEntry.Strings['Name'];
        updateVer := StrToVersion(packageEntry.Strings['Version']);
        if updateName = name then
        begin
          FPackages.Add(package);
          writeln('Package ', package.name,' is used in manager');
          if updateVer <> package.GetVersion then
            raise exception.Create('Package version specified in manager is inconsistent');
        end;
      end;
  end;
end;

procedure TManagerFile.GetVersions(AVersionList: TStringList);
begin
  //version will be provided by packages
end;

procedure TManagerFile.CheckVersion(AVersion: TVersion);
var
  url: string;
begin
  inherited CheckVersion(AVersion);
  if Archive <> nil then
  begin
    url := GetDownloadUrl;
    if (url <> '') and (url <> Archive.GetUrlForVersion(AVersion)) then
      raise exception.Create('Archive version is not consistent (DownloadZipURL field of JSON)');
  end;
end;

procedure TManagerFile.UpdateVersion(AVersion: TVersion);
var
  name, updateName, url: String;
  i, j: Integer;
  updateList: TJSONArray;
  packageEntry, data: TJSONObject;
begin
  for i := 0 to FPackages.Count-1 do
  begin
    name := ExtractFileName(FPackages[i].Filename);
    updateList := GetPackageUpdateList;
    for j := 0 to updateList.Count-1 do
    begin
      packageEntry := updateList.Items[j] as TJSONObject;
      updateName := packageEntry.Strings['Name'];
      if updateName = name then
      begin
        packageEntry.Strings['Version'] := VersionToStr(AVersion, true);
        FChanged := true;
      end;
    end;
  end;

  data := GetDataNode;
  url := Archive.GetUrlForVersion(AVersion);
  data.Strings['DownloadZipURL'] := url;
end;

procedure TManagerFile.Save;
var t: textfile;
  data: TJSONObject;
  url: String;
begin
  if (FPackages.Count = 0) then raise exception.Create('Manager does not have an associated package');

  data := GetDataNode;
  if FNew then
  begin
    data.Booleans['DisableInOPM'] := false;
    data.Strings['Name'] := FPackages[0].Name;
  end;
  if Assigned(Archive) then
  begin
    if GetDownloadUrl = '' then
    begin
      url := Archive.GetUrlForVersion(FPackages[0].GetVersion);
      data.Strings['DownloadZipURL'] := url;
      FChanged := true;
    end;
  end;

  if FNew or FChanged then
  begin
    if FNew then
      writeln('Creating manager file...')
    else
      writeln('Updating manager file...');
    assignfile(t, FFilename);
    rewrite(t);
    write(t, FRoot.FormatJSON);
    closefile(t);
  end else
    writeln('Manager file unchanged');
end;

end.

