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
    constructor Create(AParameters: TStringList); override;
    class function IsUnique: boolean; override;
    function GetUrlForVersion(AVersion: TVersion): string;
    property Url: string read FUrl;
    procedure GetVersions({%H-}AVersionList: TStringList); override;
    procedure Save; override;
  end;

implementation

{ TArchiveUrl }

constructor TArchiveUrl.Create(AParameters: TStringList);
begin
  inherited Create(AParameters);
  ExpectParamCount(1);
  FUrl := Param[0];
end;

class function TArchiveUrl.IsUnique: boolean;
begin
  Result:= true;
end;

function TArchiveUrl.GetUrlForVersion(AVersion: TVersion): string;
begin
  result := StringReplace(FUrl, '$(Version)', VersionToStr(AVersion), [rfIgnoreCase,rfReplaceAll]);
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

