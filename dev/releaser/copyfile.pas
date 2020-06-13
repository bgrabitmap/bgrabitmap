// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
unit CopyFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ReleaserTypes;

type

  { TCopyFile }

  TCopyFile = class(TReleaserObject)
  private
    FSourceFilename, FDestFilename: String;
    FVersion: TVersion;
    FVersionDefined: boolean;
  public
    constructor Create(AParameters: TStringList; ALogicDir: string); override;
    procedure Save; override;
    procedure GetVersions({%H-}AVersionList: TStringList); override;
    procedure UpdateVersion(AVersion: TVersion); override;
  end;

implementation

{ TCopyFile }

constructor TCopyFile.Create(AParameters: TStringList; ALogicDir: string);
begin
  inherited Create(AParameters, ALogicDir);
  ExpectParamCount(2);
  FSourceFilename := ExpandFileName(Param[0]);
  FDestFilename := ExpandFileName(Param[1]);
end;

procedure TCopyFile.Save;
var
  dest: String;
  streamIn,streamOut: TStream;
  buf: array of byte;
  bufCount: LongInt;
begin
  if not FVersionDefined then exit;
  streamIn := TFileStream.Create(ReplaceVariables(FSourceFilename), fmOpenRead);
  streamOut := nil;
  buf := nil;
  try
    dest := ReplaceVariables(FDestFilename);
    if FileExists(dest) then
      writeln('Replacing file "',ExtractFilename(dest),'"');
    streamOut := TFileStream.Create(dest, fmCreate);
    setlength(buf, 4096);
    repeat
      bufCount:= streamIn.Read(buf[0], length(buf));
      streamOut.WriteBuffer(buf[0], bufCount);
    until bufCount = 0;
  finally
    buf := nil;
    streamOut.Free;
    streamIn.Free;
  end;
end;

procedure TCopyFile.GetVersions(AVersionList: TStringList);
begin
  //nothing
end;

procedure TCopyFile.UpdateVersion(AVersion: TVersion);
begin
  if not FileExists(ReplaceVariables(FSourceFilename)) then
    raise exception.Create('Source file not found: '+FSourceFilename);
  if not DirectoryExists(ExtractFilePath(ReplaceVariables(FDestFilename))) then
    raise exception.Create('Target directory not found: '+ExtractFilePath(ReplaceVariables(FDestFilename)));
  FVersion := AVersion;
  FVersionDefined := true;
end;

end.

