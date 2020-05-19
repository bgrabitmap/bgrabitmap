// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
unit ProjectFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ReleaserTypes, laz2_XMLRead, laz2_XMLWrite, Laz2_DOM;

type

  { TProjectFile }

  TProjectFile = class(TReleaserObject)
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

implementation

{ TProjectFile }

function TProjectFile.GetName: string;
var
  config, projectNode, titleNode, generalNode: TDOMNode;
begin
  config := FXml.FindNode('CONFIG');
  if Assigned(config) then
  begin
    projectNode := config.FindNode('ProjectOptions');
    if Assigned(projectNode) then
    begin
      generalNode := projectNode.FindNode('General');
      if Assigned(generalNode) then
      begin
        titleNode := generalNode.FindNode('Title');
        if Assigned(titleNode) then
        begin
          with (titleNode as TDOMElement) do
          begin
            result := GetAttribute('Value');
            exit;
          end;
        end;
      end;
    end;
  end;
  result := ChangeFileExt(ExtractFileName(FFilename),'');
end;

constructor TProjectFile.Create(AParameters: TStringList; ALogicDir: string);
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
  writeln('Project ',Name,' version ',VersionToStr(GetVersion));
end;

destructor TProjectFile.Destroy;
begin
  FXml.Free;
  inherited Destroy;
end;

function TProjectFile.GetVersion: TVersion;
var
  config, projectNode, versionNode: TDOMNode;

  function GetSubNode(ATag: string): integer;
  var
    subNode: TDOMNode;
  begin
    subNode := versionNode.FindNode(ATag);
    if Assigned(subNode) then with (subNode as TDOMElement) do
      result:= StrToIntDef(GetAttribute('Value'),0)
    else
      result:= 0;
  end;

begin
  config := FXml.FindNode('CONFIG');
  if Assigned(config) then
  begin
    projectNode := config.FindNode('ProjectOptions');
    if Assigned(projectNode) then
    begin
      versionNode := projectNode.FindNode('VersionInfo');
      if Assigned(versionNode) then
      begin
        result.Major:= GetSubNode('MajorVersionNr');
        result.Minor:= GetSubNode('MinorVersionNr');
        result.Release:= GetSubNode('RevisionNr');
        result.Build:= GetSubNode('BuildNr');
        exit;
      end;
    end;
  end;
  raise exception.Create('Version node not found');
end;

procedure TProjectFile.GetVersions(AVersionList: TStringList);
var
  ver: TVersion;
  verStr: String;
begin
  ver := GetVersion;
  verStr := VersionToStr(ver);
  if AVersionList.IndexOf(verStr)=-1 then
    AVersionList.Add(verStr);
end;

procedure TProjectFile.CheckVersion(AVersion: TVersion);
begin
  inherited CheckVersion(AVersion);
  if AVersion<>GetVersion then raise exception.Create('Inconsistent version of project '+Name);
end;

procedure TProjectFile.Save;
begin
  if FChanged then
  begin
    writeln('Updating project ', Name,'...');
    WriteXMLFile(FXml, FFilename);
  end else
    writeln('Project file unchanged');
end;

procedure TProjectFile.UpdateVersion(AVersion: TVersion);
var
  config, projectNode, versionNode: TDOMNode;

  procedure UpdateSubNode(ATag: string; AValue: integer);
  var
    subNode: TDOMElement;
  begin
    subNode := versionNode.FindNode(ATag) as TDOMElement;
    if AValue<>0 then
    begin
      if subNode=nil then
      begin
        subNode := FXml.CreateElement(ATag);
        versionNode.AppendChild(subNode);
      end;
      subNode.SetAttribute('Value', inttostr(AValue));
    end else
    begin
      if Assigned(subNode) then
      begin
        versionNode.RemoveChild(subNode);
        subNode.Free;
      end;
    end;
  end;

begin
  config := FXml.FindNode('CONFIG');
  if Assigned(config) then
  begin
    projectNode := config.FindNode('ProjectOptions');
    if Assigned(projectNode) then
    begin
      versionNode := projectNode.FindNode('VersionInfo');
      if Assigned(versionNode) then
      begin
        FChanged := true;
        UpdateSubNode('MajorVersionNr', AVersion.Major);
        UpdateSubNode('MinorVersionNr', AVersion.Minor);
        UpdateSubNode('RevisionNr', AVersion.Release);
        UpdateSubNode('BuildNr', AVersion.Build);
        exit;
      end;
    end;
  end;
  raise exception.Create('Version node not found');
end;

end.

