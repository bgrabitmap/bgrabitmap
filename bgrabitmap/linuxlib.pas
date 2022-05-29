// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit linuxlib;

{$mode objfpc}{$H+}

{ This unit allows to find the implementation of a library from its
  "linker name" whatever its version. Note that between different versions,
  there may be incompatibilities (in the signature of the functions or the
  record types). So make sure the functions you are calling are stable or
  check the version of the library once its loaded using one of its functions.


  Linker name
  -----------
  The linker name normally can only be used when compiling a program.
  It ends up with .so and does not have any version number. There isn't
  necessarily a file with this name though it may be provided in the
  development package (ending with -dev).
  - libwebp.so
  - libportaudio.so
  - libtiff.so
  - libpython.so


  Soname (qualified with a version number)
  ----------------------------------------
  The soname can be supplied to the LoadLibray function to load at runtime,
  without specifying any path. It is the same as the linker name, but with
  a version number. The file exists most of the time and it is generally
  a symbolic link to the implementation (or "real name").
  - libwebp.so.6
  - libportaudio.so.2
  - libtiff.so.5
  - libpython2.7.so


  Implementation or real name (with minor number)
  -----------------------------------------------
  The real name contains the implementation. It has a minor number and
  an optional release number. Most of the time, you don't need to know this
  name to use the library.
  - libwebp.so.6.0.2
  - libportaudio.so.2.0.0
  - libtiff.so.5.3.0
  - libpython2.7.so.1.0


  See: http://tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html }

interface

uses
  BGRAClasses, SysUtils;

function FindLinuxLibrary(ALinkerName: string; AMinimumVersion: integer = 0): string;

implementation

uses process;

function FindBinPath(AFilename: string): string;
const
  BinPaths: array[0..5] of string =
  ('/usr/local/sbin','/usr/local/bin','/usr/sbin','/usr/bin','/sbin','/bin');
var i: integer;
begin
  for i := 0 to high(BinPaths) do
    If FileExists(BinPaths[i] + '/' + AFilename) then
      exit(BinPaths[i] + '/' + AFilename);
  exit(AFilename);
end;

function FindLinuxLibrary(ALinkerName: string; AMinimumVersion: integer): string;
const
  OpenBracket = ' (';
  Arrow = ') => ';
var
  dataText, s, fileName, flags, path, versionStr: string;
  dataList, flagList: TStringList;
  openBracketPos, arrowPos, posDot: SizeInt;
  versionInt, errPos, i: integer;
  maxVersionInt: integer;
begin
  versionStr := copy(ExtractFileExt(ALinkerName), 2);
  val(versionStr, versionInt, errPos);
  if errPos = 0 then
  begin
    if AMinimumVersion = 0 then AMinimumVersion := versionInt;
    ALinkerName := ChangeFileExt(ALinkerName,'');
  end;
  result := '';
  maxVersionInt := AMinimumVersion-1;
  RunCommand(FindBinPath('ldconfig'), ['-p'], dataText, []);
  dataList := TStringList.Create;
  dataList.Text := dataText;
  flagList := TStringList.Create;
  for i := 0 to dataList.Count-1 do
  begin
    s := dataList[i];
    openBracketPos := pos(OpenBracket, s);
    arrowPos := pos(Arrow,s);
    if (openBracketPos <> 0) and (arrowPos <> 0) then
    begin
      fileName := trim(copy(s,1,openBracketPos-1));
      if fileName.StartsWith(ALinkerName+'.') then
      begin
        versionStr := copy(fileName, length(ALinkerName)+2, length(fileName)-length(ALinkerName)-1);
        posDot := pos('.', versionStr);
        if posDot > 0 then versionStr := copy(versionStr, posDot-1);
        val(versionStr, versionInt, errPos);
        if errPos = 0 then
        begin
          flags := copy(s, openBracketPos+length(OpenBracket), arrowPos-openBracketPos-length(OpenBracket));
          flagList.CommaText := flags;
          if {$IFNDEF CPU64}not{$ENDIF} (flagList.IndexOf('x86-64') <> -1) then
          begin
            path := copy(s, arrowPos+length(Arrow), length(s)-arrowPos-length(Arrow)+1);
            if versionInt > maxVersionInt then
            begin
              maxVersionInt := versionInt;
              result := path;
            end;
          end;
        end;
      end;
    end;
  end;
  flagList.Free;
  dataList.Free;
end;

end.

