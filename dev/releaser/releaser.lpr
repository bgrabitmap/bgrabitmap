// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
program Releaser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, ReleaserTypes, ManagerFile, ArchiveUrl,
  PackageFile, ProjectFile, ConstFile, TextLine, CopyFile, MacBundle
  { you can add units after this };

type

  { TReleaserApp }

  TReleaserApp = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure ProcessFile(AFilename: string; AOptions: TStringList);
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  TIfState = (isNone, isSkipTrue, isDoTrue, isSkipFalse, isDoFalse);

{ TReleaserApp }

procedure TReleaserApp.DoRun;
var
  ErrorMsg, dir: String;
  opts, logicFiles: TStringList;
  i: Integer;
  findRec: TRawByteSearchRec;
begin
  // quick check parameters
  opts := TStringList.Create;
  logicFiles := TStringList.Create;
  ErrorMsg:=CheckOptions('hv', ['help','version:'], opts, logicFiles);
  if ErrorMsg<>'' then begin
    writeln(ErrorMsg);
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if logicFiles.Count = 0 then
  begin
    if FindFirst('*.logic', faAnyFile, findRec)=0 then
      repeat
        if (findRec.Attr and faDirectory)=0 then logicFiles.Add(ExpandFileName(findRec.Name));
      until FindNext(findRec)<>0;
    FindClose(findRec);
  end;

  dir := GetCurrentDir;
  for i := 0 to logicFiles.Count-1 do
  begin
    SetCurrentDir(dir);
    ProcessFile(logicFiles[i], opts);
  end;

  opts.Free;
  logicFiles.Free;

  // stop program loop
  Terminate;
end;

constructor TReleaserApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

procedure TReleaserApp.ProcessFile(AFilename: string;
  AOptions: TStringList);
var
  objs: TReleaserObjectList;

  function GetVersion: TVersion;
  var versions: TStringList;
    i: integer;
  begin
    versions := TStringList.Create;
    try
      for i := 0 to objs.Count-1 do
        objs[i].GetVersions(versions);
      if versions.Count = 0 then raise exception.Create('Version not found')
      else if versions.Count > 1 then writeln('Multiple versions found!');
      result := StrToVersion(versions[0]);
    finally
      versions.Free;
    end;
  end;

var
  t: textfile;
  s, cmd: string;
  ver, newVer: TVersion;
  line: TStringList;
  factory: TReleaserObjectFactory;
  i, lineNumber, j: Integer;
  newVerStr, logicDir, newDir: string;
  ifStack: array of TIfState;
  skipCommand: boolean;

  function PeekIfStack: TIfState;
  begin
    if ifStack = nil then
      result := isNone
      else result := ifStack[high(ifStack)];
  end;

  procedure PokeIfStack(AValue: TIfState);
  begin
    if ifStack <> nil then
      ifStack[high(ifStack)] := AValue;
  end;

  procedure PopIfStack;
  begin
    if ifStack <> nil then
      setlength(ifStack, length(ifStack)-1);
  end;

  procedure PushIfStack(AValue: TIfState);
  begin
    setlength(ifStack, length(ifStack)+1);
    ifStack[high(ifStack)] := AValue;
  end;

begin
  AFilename := ExpandFileName(AFilename);
  writeln('Logic file "', AFilename,'"');
  assignfile(t, AFilename);
  reset(t);
  line := TStringList.Create;
  objs := TReleaserObjectList.Create;
  lineNumber := 0;
  ifStack := nil;
  try
    while not eof(t) do
    begin
      inc(lineNumber);
      readln(t, s);
      line.CommaText:= trim(s);
      if line.Count > 0 then
      begin
        cmd := line[0];
        line.Delete(0);

        skipCommand := false;
        if (cmd = 'end') and (PeekIfStack <> isNone) then
        begin
          PopIfStack;
          skipCommand:= true;
        end;
        if (cmd = 'else') and (PeekIfStack in[isDoTrue,isSkipTrue]) then
        begin
          if PeekIfStack = isDoTrue then
            PokeIfStack(isSkipFalse)
            else PokeIfStack(isDoFalse);
          skipCommand:= true;
        end;

        if not skipCommand and (PeekIfStack in[isSkipTrue,isSkipFalse]) then
          skipCommand:= true;
        factory := nil;
        if not skipCommand then
        case LowerCase(cmd) of
        'cd': begin
            if line.Count <> 1 then raise exception.Create('Expecting directory');
            logicDir := ExtractFilePath(AFilename);
            delete(logicDir, length(logicDir), 1);
            newDir := StringReplace(AdaptPathDelim(line[0]),'($LogicDir)',logicDir,[rfReplaceAll]);
            SetCurrentDir(newDir);
          end;
        'manager': factory := TManagerFile;
        'archive': factory := TArchiveUrl;
        'package': factory := TPackageFile;
        'project': factory := TProjectFile;
        'bundle': factory := TMacBundle;
        'const': factory := TConstFile;
        'echo': for i := 0 to line.Count-1 do writeln(line[i]);
        'text': factory := TTextLine;
        'copy': factory := TCopyFile;
        'if': begin
                if line.Count = 0 then
                  raise exception.Create('Expecting condition');
                if line[0] = 'exists' then
                begin
                  if line.Count <> 2 then
                    raise exception.Create('Expecting 1 parameter');
                  if FileExists(line[1]) or DirectoryExists(line[1]) then
                    PushIfStack(isDoTrue)
                    else PushIfStack(isSkipTrue);
                end else
                  raise exception.Create('Unknown condition "'+line[0]);

            end;
        'else', 'end': raise exception.Create('Unexpected branching "'+cmd+'"');
        else
          raise exception.Create('Unknown command "'+cmd+'"');
        end;
        if Assigned(factory) then
        begin
          if factory.IsUnique then
          begin
            for i := 0 to objs.Count-1 do
              if objs[i] is factory then
                raise exception.Create('Unicity constraint not satisfied for '+factory.ClassName);
          end;
          objs.Add(factory.Create(line, logicDir));
        end;
      end;
    end;
    lineNumber := 0;
    for i := 0 to objs.Count-1 do
      for j := 0 to objs.Count-1 do
        objs[i].LinkWith(objs[j]);

    ver := GetVersion;
    for i := 0 to objs.Count-1 do
      objs[i].CheckVersion(ver);

    writeln('Current version: ',VersionToStr(ver));
    newVerStr := '';
    for i := 0 to AOptions.Count-1 do
      if AOptions[i].StartsWith('version=') then
      begin
        newVerStr := copy(AOptions[i], length('version=')+1, length(AOptions[i])-length('version='));
        break;
      end;
    if newVerStr = '' then
    begin
      write('New version (press Enter to keep the same): ');
      readln(newVerStr);
    end else
      writeln('New version: ', newVerStr);

    if Trim(newVerStr)='' then newVer := ver
    else newVer := StrToVersion(newVerStr);

    if newVer <> ver then
    begin
      for i := 0 to objs.Count-1 do
        objs[i].UpdateVersion(newVer);
    end else
    begin
      for i := 0 to objs.Count-1 do
        if (objs[i] is TConstFile) or (objs[i] is TCopyFile) then  //constants and copied files are loosely checked
          objs[i].UpdateVersion(newVer);
    end;

    for i := 0 to objs.Count-1 do
      objs[i].Save;
    writeln('Done.');
  except
    on ex:exception do
    begin
      write('Error');
      if lineNumber <> 0 then write(' on line ',lineNumber);
      writeln(': ', ex.Message);
    end;
  end;
  objs.Free;
  line.Free;
  closefile(t);
end;

destructor TReleaserApp.Destroy;
begin
  inherited Destroy;
end;

procedure TReleaserApp.WriteHelp;
begin
  { add your help code here }
  writeln('Update version number and check its consistence.');
  writeln;
  writeln('Usage: ', ExeName, ' [logicfile1 logicfile2...] [--version=versionNb] [--help]');
  writeln;
  writeln('  Parameter  Description');
  writeln('  ---------  ----------------------------------------------------------------');
  writeln('  versionNb  New version number to assign to manager and packages');
  writeln;
  writeln('  logicfile  File containing the location of the version number. If it is not');
  writeln('             specified, all logic files in current directory are processed.');
  writeln;
  writeln('             Sample file: mylib.logic');
  writeln('             ----------------------------------------------------------------');
  writeln('             cd /mylib');
  writeln('             manager update_mylib.json');
  writeln('             archive https://github.com/mylib/mylib/archive/v$(Version).zip');
  writeln('             package mylib/mylibpack1.lpk');
  writeln('             package mylib/mylibpack2.lpk');
  writeln('             const mylib/mylibtypes.pas MyLibVersion');
  writeln;
  writeln('             Sample file: myprog.logic');
  writeln('             ----------------------------------------------------------------');
  writeln('             cd ($LogicDir)');
  writeln('             project myproject.lpi');
  writeln('             const myconsts.pas MyProjectVersion');
  writeln;
end;

var
  Application: TReleaserApp;
begin
  Application:=TReleaserApp.Create(nil);
  Application.Title:='Releaser';
  Application.Run;
  Application.Free;
end.

