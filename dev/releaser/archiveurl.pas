// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
unit ArchiveUrl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ReleaserTypes;

type

  { TArchiveUrl }

  TArchiveUrl = class(TReleaserObject)
  private
    FUrl: string;
  public
    constructor Create(AParameters: TStringList; ALogicDir: string); override;
    class function IsUnique: boolean; override;
    function GetUrlForVersion(AVersion: TVersion): string;
    property Url: string read FUrl;
    procedure GetVersions({%H-}AVersionList: TStringList); override;
    procedure Save; override;
  end;

implementation

{ TArchiveUrl }

constructor TArchiveUrl.Create(AParameters: TStringList; ALogicDir: string);
begin
  inherited Create(AParameters, ALogicDir);
  ExpectParamCount(1);
  FUrl := Param[0];
end;

class function TArchiveUrl.IsUnique: boolean;
begin
  Result:= true;
end;

function TArchiveUrl.GetUrlForVersion(AVersion: TVersion): string;
begin
  result := ReplaceVariables(FUrl, AVersion);
end;

procedure TArchiveUrl.GetVersions(AVersionList: TStringList);
begin
  //nothing
end;

procedure TArchiveUrl.Save;
begin
  //nothing
end;

end.

