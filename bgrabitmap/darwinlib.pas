unit darwinlib;

{$mode objfpc}{$H+}

{ This unit allows to find the latest implementation of a library.
  Note that between different versions, there may be incompatibilities
  (in the signature of the functions or the record types). So make sure
  the functions you are calling are stable or check the version of the
  library once its loaded using one of its functions.
}

interface

uses
  Classes, SysUtils;

function FindDarwinLibrary(AName: string; AMinimumVersion: integer = 0): string;

implementation

function FindDarwinLibrary(AName: string; AMinimumVersion: integer): string;
const
  BinPaths: array[0..2] of string = ('~/lib', '/usr/local/lib', '/usr/lib');
var i, version, errPos: integer;
  searchRec: TRawByteSearchRec;
  path, withoutDyLib, versionStr: RawByteString;
begin
  if ExtractFileExt(AName)='.dylib' then AName := ChangeFileExt(AName,'');
  versionStr := copy(ExtractFileExt(AName), 2);
  val(versionStr, version, errPos);
  if errPos = 0 then
  begin
    if AMinimumVersion = 0 then AMinimumVersion := version;
    AName := ChangeFileExt(AName,'');
  end;
  result := '';
  for i := 0 to high(BinPaths) do
  begin
    path := BinPaths[i]+PathDelim;
    if FindFirst(path+AName+'.*.dylib', faAnyFile, searchRec)=0 then
    repeat
      withoutDyLib := ChangeFileExt(searchRec.Name, '');
      versionStr := copy(ExtractFileExt(withoutDyLib), 2, 99);
      val(versionStr, version, errPos);
      if (errPos = 0) and ((version > AMinimumVersion) or ((version = AMinimumVersion) and (result = ''))) then
      begin
        result := path+searchRec.Name;
        AMinimumVersion := version;
      end;
    until FindNext(searchRec)<>0;
  end;
end;

end.

