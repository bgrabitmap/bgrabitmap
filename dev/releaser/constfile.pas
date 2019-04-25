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
    FSourceCode: TStringList;
    FChanged: boolean;
    procedure FindVersionLine(out ALine: integer);
    procedure AnalyzeVersionLine(ALine: integer; out AValueStart,
      AValueLength: integer);
  public
    constructor Create(AParameters: TStringList); override;
    destructor Destroy; override;
    procedure Save; override;
    function TryVersion(out AValue: TVersion): boolean;
    procedure GetVersions(AVersionList: TStringList); override;
    procedure UpdateVersion(AVersion: TVersion); override;
  end;

implementation

{ TConstFile }

procedure TConstFile.FindVersionLine(out ALine: integer);
var
  s: String;
  p, i: integer;
begin
  for i := 0 to FSourceCode.Count-1 do
  begin
    s := FSourceCode[i];
    p := pos(FConstname+' ', FSourceCode[i]);
    if (p>0) and ((p=1) or (s[p-1] in[#0..#32])) then
    begin
      ALine := i;
      exit;
    end;
  end;
  ALine := -1;
end;

procedure TConstFile.AnalyzeVersionLine(ALine: integer; out AValueStart, AValueLength: integer);
var
  s: String;
  p: integer;
begin
  AValueStart := 0;
  AValueLength:= 0;

  s := FSourceCode[ALine];
  p := pos(FConstname+' ',s);
  if p<> 0 then
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

constructor TConstFile.Create(AParameters: TStringList);
var
  ver: TVersion;
begin
  inherited Create(AParameters);
  ExpectParamCount(2);
  FFilename := ExpandFileName(Param[0]);
  FConstname := Param[1];
  FSourceCode := TStringList.Create;
  FSourceCode.LoadFromFile(FFilename);
  if TryVersion(ver) then
    writeln('Code file "',ExtractFileName(FFilename),'" version ',VersionToStr(ver))
  else
    writeln('Code file "',ExtractFileName(FFilename),'" undefined version');
end;

destructor TConstFile.Destroy;
begin
  FSourceCode.Free;
  inherited Destroy;
end;

procedure TConstFile.Save;
begin
  if FChanged then
  begin
    writeln('Updating code file "',ExtractFileName(FFilename),'"');
    FSourceCode.SaveToFile(FFilename);
  end;
end;

function TConstFile.TryVersion(out AValue: TVersion): boolean;
var
  s: String;
  line, valueStart,valueLen,p, errPos: integer;
  verValue: cardinal;
begin
  AValue.Major:= 0;
  AValue.Minor:= 0;
  AValue.Release:= 0;
  AValue.Build:= 0;
  FindVersionLine(line);
  if line <> -1 then
  begin
    AnalyzeVersionLine(line, valueStart, valueLen);
    if valueStart > 0 then
    begin
      val(copy(FSourceCode[line], valueStart, valueLen), verValue, errPos);
      if errPos = 0 then
      begin
        AValue.Major:= verValue div 1000000;
        AValue.Minor := (verValue div 10000) mod 100;
        AValue.Release := (verValue div 100) mod 100;
        AValue.Build := verValue mod 100;
        exit(true);
      end;
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
  newValue, valueStart, valueLength, line: Integer;
  s: String;
begin
  newValue := AVersion.Major*1000000 + AVersion.Minor*10000 + AVersion.Release*100 + AVersion.Build;
  if TryVersion(ver) then
  begin
    if AVersion<>ver then
    begin
      FindVersionLine(line);
      if line <> -1 then
      begin
        AnalyzeVersionLine(line, valueStart,valueLength);
        if valueStart <> 0 then
        begin
          s := FSourceCode[line];
          delete(s, valueStart,valueLength);
          insert(IntToStr(newValue), s,valueStart);
          FSourceCode[line] := s;
          FChanged:= true;
        end;
      end;
    end;
  end else
    writeln('Please add manually a constant ',FConstname,' = ',newValue,' in "',ExtractFileName(FFilename),'"');
end;

end.

