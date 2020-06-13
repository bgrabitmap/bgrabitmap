// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
unit TextLine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ReleaserTypes;

type

  { TTextLine }

  TTextLine = class(TReleaserObject)
  private
    FFilename, FTextLine: String;
    FTextLineStart, FTextLineEnd: integer;
    FVersion: TVersion;
    FText: string;
    FChanged: boolean;
  public
    constructor Create(AParameters: TStringList; ALogicDir: string); override;
    destructor Destroy; override;
    procedure Save; override;
    function GetLineForVersion(AVersion: TVersion): string;
    procedure GetVersions({%H-}AVersionList: TStringList); override;
    procedure CheckVersion(AVersion: TVersion); override;
    procedure UpdateVersion(AVersion: TVersion); override;
  end;

implementation

{ TTextLine }

constructor TTextLine.Create(AParameters: TStringList; ALogicDir: string);
var
  str: TStringStream;
  stream: TFileStream;
begin
  inherited Create(AParameters, ALogicDir);
  ExpectParamCount(2);
  FFilename := ExpandFileName(ReplaceVariables(Param[0]));
  FTextLine := Param[1];
  FTextLineStart:= 0;
  FTextLineEnd:= 0;
  stream := nil;
  str := nil;
  try
    stream := TFileStream.Create(FFilename, fmOpenRead);
    str := TStringStream.Create('');
    if str.CopyFrom(stream, stream.Size)<>stream.Size then
      raise exception.Create('Unable to read file');
    FText := str.DataString;
  finally
    str.Free;
    stream.Free;
  end;
end;

destructor TTextLine.Destroy;
begin
  inherited Destroy;
end;

procedure TTextLine.Save;
var
  stream: TFileStream;
begin
  if FChanged then
  begin
    writeln('Updating text file "',ExtractFileName(FFilename),'"');
    stream := TFileStream.Create(FFilename, fmCreate);
    try
      if FText <> '' then
        stream.WriteBuffer(FText[1], length(FText));
    finally
      stream.Free;
    end;
  end;
end;

function TTextLine.GetLineForVersion(AVersion: TVersion): string;
begin
  result := ReplaceVariables(FTextLine, AVersion);
end;

procedure TTextLine.GetVersions(AVersionList: TStringList);
begin
  //nothing
end;

procedure TTextLine.CheckVersion(AVersion: TVersion);
var
  i, start: Integer;
  expectLine: string;

  procedure TryLine(AEnd: integer);
  var
    curLine: String;
  begin
    if AEnd > start then
    begin
      curLine := copy(FText, start, AEnd-start);
      if curLine = expectLine then
      begin
        FTextLineStart:= start;
        FTextLineEnd:= AEnd;
        FVersion := AVersion;
      end;
    end;
    start := AEnd+1;
  end;

begin
  inherited CheckVersion(AVersion);
  if FTextLineEnd > FTextLineStart then exit;
  expectLine := GetLineForVersion(AVersion);
  i := 1;
  start := 1;
  while (i < length(FText)) and (FTextLineEnd <= FTextLineStart) do
  begin
    if FText[i] in[#13,#10] then TryLine(i);
    inc(i);
  end;
  if (FTextLineEnd <= FTextLineStart) then TryLine(i);
  if FTextLineEnd > FTextLineStart then
    writeln('Text file "',ExtractFileName(FFilename),'" line found')
  else
    writeln('Text file "',ExtractFileName(FFilename),'" line not found');
end;

procedure TTextLine.UpdateVersion(AVersion: TVersion);
var
  newLine: String;
begin
  if AVersion = FVersion then exit;
  newLine := GetLineForVersion(AVersion);
  if FTextLineEnd > FTextLineStart then
  begin
    delete(FText, FTextLineStart, FTextLineEnd-FTextLineStart);
    insert(newLine, FText, FTextLineStart);
    FTextLineEnd:= FTextLineStart+length(newLine);
    FChanged:= true;
  end else
    writeln('Please add manually a line "',newLine,'" in "',ExtractFileName(FFilename),'"');
end;

end.

