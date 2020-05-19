// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
unit PackageFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ReleaserTypes, fgl, laz2_XMLRead, laz2_XMLWrite, Laz2_DOM;

type

  { TPackageFile }

  TPackageFile = class(TReleaserObject)
  private
    FFilename: string;
    FXml: TXmlDocument;
    FChanged: boolean;
    function GetName: string;
  public
    constructor Create(AParameters: TStringList; ALogicDir: string); override;
    destructor Destroy; override;
    property Filename: string read FFilename;
    function GetVersion: TVersion;
    procedure GetVersions(AVersionList: TStringList); override;
    procedure CheckVersion(AVersion: TVersion); override;
    property Name: string read GetName;
    procedure Save; override;
    procedure UpdateVersion(AVersion: TVersion); override;
  end;

  TPackageFileList = specialize TFPGList<TPackageFile>;

implementation

{ TPackageFile }

function TPackageFile.GetName: string;
var
  config, packageNode, nameNode: TDOMNode;
begin
  config := FXml.FindNode('CONFIG');
  if Assigned(config) then
  begin
    packageNode := config.FindNode('Package');
    if Assigned(packageNode) then
    begin
      nameNode := packageNode.FindNode('Name');
      if Assigned(nameNode) then
      begin
        with (nameNode as TDOMElement) do
        begin
          result := GetAttribute('Value');
          exit;
        end;
      end;
    end;
  end;
  result := ChangeFileExt(ExtractFileName(FFilename),'');
end;

constructor TPackageFile.Create(AParameters: TStringList; ALogicDir: string);
var
  stream: TFileStream;
begin
  inherited Create(AParameters, ALogicDir);
  ExpectParamCount(1);
  FFilename:= ExpandFileName(ReplaceVariables(Param[0]));
  stream := TFileStream.Create(FFilename, fmOpenRead);
  try
    ReadXMLFile(FXml,stream);
  finally
    stream.Free;
  end;
  writeln('Package ',Name,' version ',VersionToStr(GetVersion));
end;

destructor TPackageFile.Destroy;
begin
  FXml.Free;
  inherited Destroy;
end;

function TPackageFile.GetVersion: TVersion;
var
  config, packageNode, versionNode: TDOMNode;
begin
  config := FXml.FindNode('CONFIG');
  if Assigned(config) then
  begin
    packageNode := config.FindNode('Package');
    if Assigned(packageNode) then
    begin
      versionNode := packageNode.FindNode('Version');
      if Assigned(versionNode) then
      begin
        with (versionNode as TDOMElement) do
        begin
          result.Major:= StrToIntDef(GetAttribute('Major'),0);
          result.Minor:= StrToIntDef(GetAttribute('Minor'),0);
          result.Release:= StrToIntDef(GetAttribute('Release'),0);
          result.Build:= StrToIntDef(GetAttribute('Build'),0);
          exit;
        end;
      end;
    end;
  end;
  raise exception.Create('Version node not found');
end;

procedure TPackageFile.GetVersions(AVersionList: TStringList);
var
  ver: TVersion;
  verStr: String;
begin
  ver := GetVersion;
  verStr := VersionToStr(ver);
  if AVersionList.IndexOf(verStr)=-1 then
    AVersionList.Add(verStr);
end;

procedure TPackageFile.CheckVersion(AVersion: TVersion);
begin
  inherited CheckVersion(AVersion);
  if AVersion<>GetVersion then raise exception.Create('Inconsistent version of package '+Name);
end;

procedure TPackageFile.Save;
begin
  if FChanged then
  begin
    writeln('Updating package ', Name,'...');
    WriteXMLFile(FXml, FFilename);
  end;
end;

procedure TPackageFile.UpdateVersion(AVersion: TVersion);
var
  config, packageNode, versionNode: TDOMNode;
begin
  config := FXml.FindNode('CONFIG');
  if Assigned(config) then
  begin
    packageNode := config.FindNode('Package');
    if Assigned(packageNode) then
    begin
      versionNode := packageNode.FindNode('Version');
      if Assigned(versionNode) then
      begin
        with (versionNode as TDOMElement) do
        begin
          FChanged := true;
          SetAttribute('Major', inttostr(AVersion.Major));
          if AVersion.Minor <> 0 then SetAttribute('Minor', inttostr(AVersion.Minor)) else RemoveAttribute('Minor');
          if AVersion.Release <> 0 then SetAttribute('Release', inttostr(AVersion.Release)) else RemoveAttribute('Release');
          if AVersion.Build <> 0 then SetAttribute('Build', inttostr(AVersion.Build)) else RemoveAttribute('Build');
          exit;
        end;
      end;
    end;
  end;
  raise exception.Create('Version node not found');
end;

end.

