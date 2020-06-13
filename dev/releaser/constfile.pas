// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
unit ConstFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ReleaserTypes;

type

  { TConstFile }

  TConstFile = class(TReleaserObject)
  private
    FFilename, FConstname: String;
    FSourceCode: string;
    FChanged: boolean;
    procedure AnalyzeVersionLine(ALine: string; out AValueStart,
      AValueLength: integer);
  public
    constructor Create(AParameters: TStringList; ALogicDir: string); override;
    destructor Destroy; override;
    procedure Save; override;
    function TryVersion(out AValue: TVersion): boolean;
    procedure GetVersions(AVersionList: TStringList); override;
    procedure UpdateVersion(AVersion: TVersion); override;
  end;

implementation

{ TConstFile }

procedure TConstFile.AnalyzeVersionLine(ALine: string; out AValueStart, AValueLength: integer);
var
  s: String;
  p: integer;
begin
  AValueStart := 0;
  AValueLength:= 0;

  s := ALine;
  p := pos(FConstname+' ',s);
  if (p<> 0) and ((p=1) or (s[p-1] in[#0..#32])) then
  begin
    inc(p, length(FConstName));
    while (p <= length(s)) and (s[p] in[#0..#32]) do inc(p);
    if (p <= length(s)) and (s[p] = '=') then
    begin
      inc(p);
      while (p <= length(s)) and (s[p] in[#0..#32]) do inc(p);
      AValueStart := p;
      while (p <= length(s)) and (s[p] in['0'..'9']) do inc(p);
      AValueLength:= p-AValueStart;
      exit;
    end;
  end;
end;

constructor TConstFile.Create(AParameters: TStringList; ALogicDir: string);
var
  ver: TVersion;
  str: TStringStream;
  stream: TFileStream;
begin
  inherited Create(AParameters, ALogicDir);
  ExpectParamCount(2);
  FFilename := ExpandFileName(ReplaceVariables(Param[0]));
  FConstname := Param[1];
  stream := nil;
  str := nil;
  try
    stream := TFileStream.Create(FFilename, fmOpenRead);
    str := TStringStream.Create('');
    if str.CopyFrom(stream, stream.Size)<>stream.Size then
      raise exception.Create('Unable to read file');
    FSourceCode := str.DataString;
  finally
    str.Free;
    stream.Free;
  end;
  if TryVersion(ver) then
    writeln('Code file "',ExtractFileName(FFilename),'" version ',VersionToStr(ver))
  else
    writeln('Code file "',ExtractFileName(FFilename),'" undefined version');
end;

destructor TConstFile.Destroy;
begin
  inherited Destroy;
end;

procedure TConstFile.Save;
var
  stream: TFileStream;
begin
  if FChanged then
  begin
    writeln('Updating code file "',ExtractFileName(FFilename),'"');
    stream := TFileStream.Create(FFilename, fmCreate);
    try
      if FSourceCode <> '' then
        stream.WriteBuffer(FSourceCode[1], length(FSourceCode));
    finally
      stream.Free;
    end;
  end;
end;

function TConstFile.TryVersion(out AValue: TVersion): boolean;
var
  valueStart,valueLen,errPos: integer;
  verValue: LongWord;
begin
  AValue.Major:= 0;
  AValue.Minor:= 0;
  AValue.Release:= 0;
  AValue.Build:= 0;
  AnalyzeVersionLine(FSourceCode, valueStart, valueLen);
  if valueStart > 0 then
  begin
    val(copy(FSourceCode, valueStart, valueLen), verValue, errPos);
    if errPos = 0 then
    begin
      AValue.Major:= verValue div 1000000;
      AValue.Minor := (verValue div 10000) mod 100;
      AValue.Release := (verValue div 100) mod 100;
      AValue.Build := verValue mod 100;
      exit(true);
    end;
  end;
  result := false;
end;

procedure TConstFile.GetVersions(AVersionList: TStringList);
var
  ver: TVersion;
  verStr: String;
begin
  if TryVersion(ver) then
  begin
    verStr := VersionToStr(ver);
    if AVersionList.IndexOf(verStr)=-1 then AVersionList.Add(verStr);
  end;
end;

procedure TConstFile.UpdateVersion(AVersion: TVersion);
var
  ver: TVersion;
  newValue, valueStart, valueLength: Integer;
  s: String;
begin
  newValue := AVersion.Major*1000000 + AVersion.Minor*10000 + AVersion.Release*100 + AVersion.Build;
  if TryVersion(ver) then
  begin
    if AVersion<>ver then
    begin
      AnalyzeVersionLine(FSourceCode, valueStart,valueLength);
      if valueStart <> 0 then
      begin
        s := FSourceCode;
        delete(s, valueStart,valueLength);
        insert(IntToStr(newValue), s,valueStart);
        FSourceCode := s;
        FChanged:= true;
      end;
    end;
  end else
    writeln('Please add manually a constant ',FConstname,' = ',newValue,' in "',ExtractFileName(FFilename),'"');
end;

end.

