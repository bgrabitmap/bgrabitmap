program Make;
{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  StrUtils,
  FileUtil,
  Zipper,
  fphttpclient,
  RegExpr,
  openssl,
  opensslsockets,
  Process;

const
  Target: string = 'test';
  Dependencies: array of string = ('epiktimer', 'BGRAControls');

type
  TLog = (audit, info, error);
  Output = record
    Success: boolean;
    Output: string;
  end;

  procedure OutLog(Knd: TLog; Msg: string);
  begin
    case Knd of
        error: Writeln(stderr, #27'[91m', Msg, #27'[0m');
        info:  Writeln(stderr, #27'[32m', Msg, #27'[0m');
        audit: Writeln(stderr, #27'[33m', Msg, #27'[0m');
    end;
  end;

  function CheckModules: Output;
  var Line: string;
  begin
    if FileExists('.gitmodules') then
      if RunCommand('git', ['submodule', 'update', '--init', '--recursive',
        '--force', '--remote'], Result.Output) then
      begin
        for Line in SplitString(Result.Output, LineEnding) do
          if Line <> '' then
            OutLog(info, Line);
      end;
  end;

  function AddPackage(Path: string): Output;
  begin
    with TRegExpr.Create do
    begin
      Expression :=
        {$IFDEF MSWINDOWS}
          '(cocoa|x11|_template)'
        {$ELSE}
          '(cocoa|gdi|_template)'
        {$ENDIF}
      ;
      if not Exec(Path) and RunCommand('lazbuild', ['--add-package-link', Path],
        Result.Output) then
        OutLog(info, 'added ' + Path);
      Free;
    end;
  end;

  function BuildProject(Path: string): Output;
  var
    Line: string;
  begin
    OutLog(audit, 'build from ' + Path);
    try
      Result.Success := RunCommand('lazbuild', ['--build-all', '--recursive',
        '--no-write-project', Path], Result.Output);
      if Result.Success then
        for Line in SplitString(Result.Output, LineEnding) do
        begin
          if ContainsStr(Line, 'Linking') then
          begin
            Result.Output := SplitString(Line, ' ')[2];
            OutLog(info, ' to ' + Result.Output);
            break;
          end;
        end
      else
      begin
        ExitCode += 1;
        with TRegExpr.Create do
        begin
          Expression := '(Fatal|Error|/ld(\.[a-z]+)?):';
          for Line in SplitString(Result.Output, LineEnding) do
          begin
            if Exec(Line) then
              OutLog(error, Line);
          end;
          Free;
        end;
      end;
    except
      on E: Exception do
        OutLog(error, E.ClassName + LineEnding + E.Message);
    end;
  end;

  function RunTest(Path: string): Output;
  var
    Temp: string;
  begin
    Result := BuildProject(Path);
    Temp:= Result.Output;
    if Result.Success then
        try
          if not RunCommand(Temp, ['--all', '--format=plain', '--progress'], Result.Output) then
          begin
            ExitCode += 1;
            OutLog(error, Result.Output);
          end;
        except
          on E: Exception do
            OutLog(error, E.ClassName + LineEnding + E.Message);
        end;
  end;

  function InstallOPM(Each: string): string;
  var
    OutFile, Uri: string;
    Zip: TStream;
  begin
    Result :=
      {$IFDEF MSWINDOWS}
      GetEnvironmentVariable('APPDATA') + '\.lazarus\onlinepackagemanager\packages\'
      {$ELSE}
      GetEnvironmentVariable('HOME') + '/.lazarus/onlinepackagemanager/packages/'
      {$ENDIF}
      + Each;
    OutFile := GetTempFileName;
    Uri := 'https://packages.lazarus-ide.org/' + Each + '.zip';
    if not DirectoryExists(Result) then
    begin
      Zip := TFileStream.Create(OutFile, fmCreate or fmOpenWrite);
      with TFPHttpClient.Create(nil) do
      begin
        try
          AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
          AllowRedirect := True;
          Get(Uri, Zip);
          OutLog(audit, 'Download from ' + Uri + ' to ' + OutFile);
        finally
          Free;
        end;
      end;
      Zip.Free;
      CreateDir(Result);
      with TUnZipper.Create do
      begin
        try
          FileName := OutFile;
          OutputPath := Result;
          Examine;
          UnZipAllFiles;
          OutLog(audit, 'Unzip from ' + OutFile + ' to ' + Result);
        finally
          Free;
        end;
      end;
      DeleteFile(OutFile);
    end;
  end;

  procedure BuildAll;
  var
    Each, Item: string;
    List: TStringList;
  begin
    CheckModules;
    InitSSLInterface;
    for Item in Dependencies do
    begin
      List := FindAllFiles(InstallOPM(Item), '*.lpk', True);
      try
        for Each in List do
          AddPackage(Each);
      finally
        List.Free;
      end;
    end;
    List := FindAllFiles(GetCurrentDir, '*.lpk', True);
    try
      for Each in List do
        AddPackage(Each);
    finally
      List.Free;
    end;
    List := FindAllFiles(Target, '*.lpi', True);
    try
      for Each in List do
        if not ContainsStr(Each, 'zengl') then
          if ContainsStr(ReadFileToString(ReplaceStr(Each, '.lpi', '.lpr')),
            'consoletestrunner') then
            RunTest(Each)
          else
            BuildProject(Each);
    finally
      List.Free;
    end;
    OutLog(audit,   '------------');
    if ExitCode <> 0 then
      OutLog(error, 'Errors: ' + IntToStr(ExitCode))
    else
      OutLog(info,  'No Errors 😊');
    OutLog(audit,   '------------');  
  end;

begin
  BuildAll;
end.
