// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
unit MacBundle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ReleaserTypes, laz2_XMLRead, laz2_XMLWrite, Laz2_DOM;

type

  { TMacBundle }

  TMacBundle = class(TReleaserObject)
  private
    FBundlepath: string;
    FFilename: string;
    FXml: TXmlDocument;
    FDict: TStringList;
    FChanged: boolean;
    function GetName: string;
    function GetPListFilename: string;
    function ReadDict(ARoot: TDOMNode): TStringList;
    procedure UpdateDict(ARoot: TDOMNode; ADict: TStringList);
    function GetPList: TDOMNode;
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

{ TMacBundle }

function TMacBundle.GetName: string;
begin
  result := '';
  if Assigned(FDict) then
    result := FDict.Values['CFBundleName'];
  if result = '' then
    result := ChangeFileExt(ExtractFileName(FBundlepath),'');
end;

function TMacBundle.GetPListFilename: string;
begin
  result := FBundlepath+PathDelim+'Contents'+PathDelim+'Info.plist';
end;

function TMacBundle.ReadDict(ARoot: TDOMNode): TStringList;
var
  dict, entry: TDOMNode;
  key: string;
begin
  result := TStringList.Create;
  if Assigned(ARoot) then
  begin
    dict := ARoot.FindNode('dict');
    if Assigned(dict) then
    begin
      entry := dict.FirstChild;
      while entry <> nil do
      begin
        if entry.NodeName = 'key' then
        begin
          key := entry.TextContent;
          entry := entry.NextSibling;
          if (entry <> nil) and (entry.NodeName = 'string') then
          begin
            result.Values[key] := entry.TextContent;
            entry := entry.NextSibling;
          end
          else if (entry <> nil) and (entry.NodeName <> 'key') then
            entry := entry.NextSibling;
        end else
          entry := entry.NextSibling;
      end;
    end else
      raise exception.Create('"dict" node not found');
  end;
end;

procedure TMacBundle.UpdateDict(ARoot: TDOMNode; ADict: TStringList);
var
  dict, entry: TDOMNode;
  key: string;
begin
  if Assigned(ARoot) then
  begin
    dict := ARoot.FindNode('dict');
    if Assigned(dict) then
    begin
      entry := dict.FirstChild;
      while entry <> nil do
      begin
        if entry.NodeName = 'key' then
        begin
          key := entry.TextContent;
          entry := entry.NextSibling;
          if (entry <> nil) and (entry.NodeName = 'string') then
          begin
            entry.TextContent := ADict.Values[key];
            entry := entry.NextSibling;
          end
          else if (entry <> nil) and (entry.NodeName <> 'key') then
            entry := entry.NextSibling;
        end else
          entry := entry.NextSibling;
      end;
    end else
      raise exception.Create('"dict" node not found');
  end;
end;

function TMacBundle.GetPList: TDOMNode;
begin
  result := FXml.FirstChild;
  while (result <> nil) and not (result is TDOMElement) do result := result.NextSibling;
  if (result = nil) or (result.NodeName <> 'plist') then raise exception.Create('"plist" node not found');
end;

constructor TMacBundle.Create(AParameters: TStringList; ALogicDir: string);
var
  stream: TFileStream;
begin
  inherited Create(AParameters, ALogicDir);
  ExpectParamCount(1);
  FBundlepath:= ExpandFileName(ReplaceVariables(Param[0]));
  stream := TFileStream.Create(GetPListFilename, fmOpenRead);
  try
    ReadXMLFile(FXml,stream);
  finally
    stream.Free;
  end;
  FDict := ReadDict(GetPList);
  writeln('Bundle ',Name,' version ',VersionToStr(GetVersion));
end;

destructor TMacBundle.Destroy;
begin
  FXml.Free;
  inherited Destroy;
end;

function TMacBundle.GetVersion: TVersion;
begin
  if Assigned(FDict) then
    result := StrToVersion(FDict.Values['CFBundleVersion'])
  else raise exception.Create('Version node not found');
end;

procedure TMacBundle.GetVersions(AVersionList: TStringList);
var
  ver: TVersion;
  verStr: String;
begin
  ver := GetVersion;
  verStr := VersionToStr(ver);
  if AVersionList.IndexOf(verStr)=-1 then
    AVersionList.Add(verStr);
end;

procedure TMacBundle.CheckVersion(AVersion: TVersion);
begin
  inherited CheckVersion(AVersion);
  if AVersion<>GetVersion then raise exception.Create('Inconsistent version of bundle '+Name);
end;

procedure TMacBundle.Save;
begin
  if FChanged then
  begin
    writeln('Updating bundle ', Name,'...');
    WriteXMLFile(FXml, GetPListFilename);
  end else
    writeln('Bundle unchanged');
end;

procedure TMacBundle.UpdateVersion(AVersion: TVersion);
var
  versionStr: String;
begin
  if Assigned(FDict) then
  begin
    versionStr := VersionToStr(AVersion);
    if FDict.Values['CFBundleVersion'] <> versionStr then
    begin
      FDict.Values['CFBundleVersion'] := versionStr;
      FChanged := true;
    end;
    if FDict.Values['CFBundleShortVersionString'] <> versionStr then
    begin
      FDict.Values['CFBundleShortVersionString'] := versionStr;
      FChanged := true;
    end;
    UpdateDict(GetPList, FDict);
  end;
end;

end.

