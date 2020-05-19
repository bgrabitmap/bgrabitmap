// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
unit ReleaserTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TVersion = record
    case boolean of
    false: (Value: array[1..4] of integer);
    true: (Major, Minor, Release, Build: integer);
  end;

  operator =(AVersion1,AVersion2: TVersion): boolean;
  function VersionToStr(AVersion: TVersion; AAlwaysIncludeZero: boolean = false): string;
  function StrToVersion(AStr: string): TVersion;

type
  { TReleaserObject }

  TReleaserObject = class
  private
    function GetParam(AIndex: integer): string;
    function GetParamCount: integer;
  protected
    FLogicDir: string;
    function ReplaceVariables(AText: string; AVersion: TVersion): string; overload;
    function ReplaceVariables(AText: string): string; overload;
  public
    FParameters: TStringList;
    constructor Create(AParameters: TStringList; ALogicDir: string); virtual;
    procedure ExpectParamCount(ACount: integer);
    destructor Destroy; override;
    property ParamCount: integer read GetParamCount;
    property Param[AIndex: integer]: string read GetParam;
    class function IsUnique: boolean; virtual;
    procedure LinkWith({%H-}AOtherObject: TReleaserObject); virtual;
    procedure GetVersions(AVersionList: TStringList); virtual; abstract;
    procedure CheckVersion({%H-}AVersion: TVersion); virtual;
    procedure UpdateVersion({%H-}AVersion: TVersion); virtual;
    procedure Save; virtual; abstract;
  end;

  TReleaserObjectFactory = class of TReleaserObject;

  TReleaserObjectList = specialize TFPGObjectList<TReleaserObject>;

function AdaptPathDelim(APath: string): string;

implementation

operator=(AVersion1, AVersion2: TVersion): boolean;
begin
  result := (AVersion1.Major = AVersion2.Major) and
    (AVersion1.Minor = AVersion2.Minor) and
    (AVersion1.Release = AVersion2.Release) and
    (AVersion1.Build = AVersion2.Build);
end;

function VersionToStr(AVersion: TVersion; AAlwaysIncludeZero: boolean): string;
begin
  result := IntToStr(AVersion.Major);
  if AAlwaysIncludeZero or (AVersion.Minor<>0) or (AVersion.Release<>0) or (AVersion.Build<>0) then
  begin
    result += '.' + IntToStr(AVersion.Minor);
    if AAlwaysIncludeZero or (AVersion.Release<>0) or (AVersion.Build<>0) then
    begin
      result += '.' + IntToStr(AVersion.Release);
      if AAlwaysIncludeZero or (AVersion.Build<>0) then
      begin
        result += '.' + IntToStr(AVersion.Build);
      end;
    end;
  end;
end;

function StrToVersion(AStr: string): TVersion;
var
  lst: TStringList;
  i: Integer;
begin
  lst := TStringList.Create;
  lst.Delimiter:= '.';
  lst.DelimitedText := AStr;
  if lst.Count > 4 then
  begin
    lst.Free;
    raise exception.Create('Invalid version string');
  end;
  for i := 1 to 4 do result.Value[i] := 0;
  for i := 1 to lst.Count do
    result.Value[i] := StrToInt(lst[i-1]);
  lst.Free;
end;

function AdaptPathDelim(APath: string): string;
begin
  if PathDelim <> '\' then
    result := StringReplace(APath, '\', PathDelim, [rfReplaceAll]);
  if PathDelim <> '/' then
    result := StringReplace(APath, '/', PathDelim, [rfReplaceAll]);
end;

{ TReleaserObject }

function TReleaserObject.GetParam(AIndex: integer): string;
begin
  result := FParameters[AIndex];
end;

function TReleaserObject.GetParamCount: integer;
begin
  result := FParameters.Count;
end;

function TReleaserObject.ReplaceVariables(AText: string; AVersion: TVersion): string;
begin
  result := AText;
  result := StringReplace(result, '$(Version)', VersionToStr(AVersion), [rfIgnoreCase,rfReplaceAll]);
  result := StringReplace(result, '$(LogicDir)', FLogicDir, [rfIgnoreCase,rfReplaceAll]);
end;

function TReleaserObject.ReplaceVariables(AText: string): string;
begin
  result := AText;
  result := StringReplace(result, '$(Version)', '?', [rfIgnoreCase,rfReplaceAll]);
  result := StringReplace(result, '$(LogicDir)', FLogicDir, [rfIgnoreCase,rfReplaceAll]);
end;

constructor TReleaserObject.Create(AParameters: TStringList; ALogicDir: string);
begin
  FParameters := TStringList.Create;
  FParameters.AddStrings(AParameters);
  FLogicDir := ALogicDir;
end;

procedure TReleaserObject.ExpectParamCount(ACount: integer);
begin
  if ACount <> ParamCount then
    raise exception.Create('Invalid number of parameters. Found '+inttostr(ParamCount)+' but expected '+inttostr(ACount));
end;

destructor TReleaserObject.Destroy;
begin
  FParameters.Free;
  inherited Destroy;
end;

class function TReleaserObject.IsUnique: boolean;
begin
  result := false;
end;

procedure TReleaserObject.LinkWith(AOtherObject: TReleaserObject);
begin
  //nothing
end;

procedure TReleaserObject.CheckVersion(AVersion: TVersion);
begin
  //nothing
end;

procedure TReleaserObject.UpdateVersion(AVersion: TVersion);
begin
  //nothing
end;

end.

