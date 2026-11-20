program Regression;
{$mode objfpc}{$H+}

uses Classes, SysUtils, StrUtils, Process;

var
  Root, BuildDir, LazBuild, FPC, PCP, OnlyPrefix: string;
  TestTimeout: QWord = 120000;
  PortableOnly: boolean = false;
  Passed, Failed, Skipped, Selected: integer;

function Option(const AName, ADefault: string): string;
var i: integer; prefix: string;
begin
  result := ADefault;
  prefix := '--'+AName+'=';
  for i := 1 to ParamCount do
    if AnsiStartsStr(prefix,ParamStr(i)) then
      result := Copy(ParamStr(i),Length(prefix)+1,MaxInt);
end;

function NativePath(const S: string): string;
begin
  result := StringReplace(S,'/',PathDelim,[rfReplaceAll]);
end;

{ Drain the pipe while the child runs, so a verbose compiler cannot block.
  A failed launch, nonzero exit, signal or timeout must fail the CI step. }
function Execute(const AExecutable: string; AArgs: TStrings;
  const ADirectory, ALog: string; ATimeout: QWord): boolean;
var
  child: TProcess;
  output: TStringStream;
  buffer: array[0..4095] of char;
  count: integer;
  started: QWord;
  timedOut: boolean;

  procedure ReadOutput;
  begin
    while child.Output.NumBytesAvailable > 0 do
    begin
      count := child.Output.Read(buffer,SizeOf(buffer));
      if count <= 0 then break;
      output.WriteBuffer(buffer,count);
    end;
  end;

begin
  result := false;
  child := TProcess.Create(nil);
  output := TStringStream.Create('');
  try
    child.Executable := AExecutable;
    child.Parameters.Assign(AArgs);
    child.CurrentDirectory := ADirectory;
    child.Options := [poUsePipes,poStderrToOutPut];
    started := GetTickCount64;
    timedOut := false;
    try
      child.Execute;
      while child.Running do
      begin
        ReadOutput;
        if GetTickCount64-started >= ATimeout then
        begin
          timedOut := true;
          child.Terminate(124);
          child.WaitOnExit(5000);
          break;
        end;
        Sleep(10);
      end;
      ReadOutput;
      result := not timedOut and (child.ExitStatus = 0);
      if timedOut then
        WriteLn('FAIL: timed out after ',ATimeout div 1000,' seconds')
      else if not result then
        WriteLn('FAIL: exit code ',child.ExitCode);
    except
      on E: Exception do
        output.WriteString(E.ClassName+': '+E.Message+LineEnding);
    end;
    output.SaveToFile(ALog);
    if not result then
      WriteLn(output.DataString);
  finally
    output.Free;
    child.Free;
  end;
end;

procedure CopyRuntime(const ASource, ADest: string);
var source, dest: TFileStream;
begin
  source := TFileStream.Create(ASource,fmOpenRead or fmShareDenyWrite);
  try
    dest := TFileStream.Create(ADest,fmCreate);
    try dest.CopyFrom(source,0);
    finally dest.Free end;
  finally source.Free end;
end;

procedure RunTest(const ASource, AMode: string);
var
  source, project, executable, name, outputDir, unitsDir: string;
  args: TStringList;
  success: boolean;
begin
  if (AMode <> 'core') and (AMode <> 'nogui') and (AMode <> 'cocoa') and (AMode <> 'webp') and (AMode <> 'win32') then
    raise Exception.Create('Unknown regression mode: '+AMode);
  source := ExpandFileName(Root+PathDelim+'test'+PathDelim+'regression'+PathDelim+NativePath(ASource));
  if not FileExists(source) then
    raise Exception.Create('Missing test source: '+source);
  Inc(Selected);
  if (AMode = 'cocoa') and (PortableOnly {$IFNDEF DARWIN}or true{$ENDIF}) then
  begin
    WriteLn('SKIP: ',ASource,' requires macOS with Cocoa');
    Inc(Skipped);
    exit;
  end;
  {$IFNDEF WINDOWS}
  if AMode = 'win32' then
  begin
    WriteLn('SKIP: ',ASource,' requires Windows with the Win32 widgetset');
    Inc(Skipped);
    exit;
  end;
  {$ENDIF}
  name := ChangeFileExt(ExtractFileName(source),'');
  outputDir := BuildDir+PathDelim+AMode+PathDelim+name;
  if not ForceDirectories(outputDir) then
    raise Exception.Create('Cannot create '+outputDir);
  {$IFDEF WINDOWS}
  if AMode = 'webp' then
    CopyRuntime(Root+PathDelim+'libwebp'+PathDelim+
      {$IFDEF CPU64}'libwebp64.dll'{$ELSE}'libwebp32.dll'{$ENDIF},
      outputDir+PathDelim+{$IFDEF CPU64}'libwebp64.dll'{$ELSE}'libwebp32.dll'{$ENDIF});
  {$ENDIF}
  args := TStringList.Create;
  try
    WriteLn('BUILD [',AMode,']: ',ASource);
    if (AMode = 'core') or (AMode = 'webp') then
    begin
      unitsDir := BuildDir+PathDelim+'core'+PathDelim+'units';
      if not ForceDirectories(unitsDir) then
        raise Exception.Create('Cannot create '+unitsDir);
      executable := outputDir+PathDelim+name+{$IFDEF WINDOWS}'.exe'{$ELSE}''{$ENDIF};
      args.Add('-dBGRABITMAP_CORE');
      args.Add('-Fu'+Root+PathDelim+'bgrabitmap');
      args.Add('-Fi'+Root+PathDelim+'bgrabitmap');
      args.Add('-FU'+unitsDir);
      args.Add('-o'+executable);
      args.Add(source);
      success := Execute(FPC,args,Root,outputDir+PathDelim+'build.log',600000);
    end else
    begin
      project := ChangeFileExt(source,'.lpi');
      if not FileExists(project) then raise Exception.Create('Missing project: '+project);
      { LCL regression projects use the source basename as their target,
        beside the project. This avoids parsing localized compiler output. }
      executable := ChangeFileExt(source,{$IFDEF WINDOWS}'.exe'{$ELSE}''{$ENDIF});
      args.Add('--no-write-project');
      args.Add('--ws='+AMode);
      if PCP <> '' then args.Add('--pcp='+PCP);
      args.Add(project);
      success := Execute(LazBuild,args,Root,outputDir+PathDelim+'build.log',600000);
    end;
    if success then
    begin
      WriteLn('RUN: ',ASource);
      args.Clear;
      if (AMode <> 'core') and (AMode <> 'webp') then args.Add(outputDir);
      success := Execute(executable,args,outputDir,outputDir+PathDelim+'run.log',TestTimeout);
    end;
    if success then
    begin
      Inc(Passed);
      WriteLn('PASS: ',ASource);
    end else
    begin
      Inc(Failed);
      WriteLn('FAIL: ',ASource,' (logs: ',outputDir,')');
    end;
  finally
    args.Free;
  end;
end;

var manifest, fields: TStringList; fileName, line: string; i: integer;
begin
  try
    Root := ExpandFileName(Option('root',GetCurrentDir));
    BuildDir := ExpandFileName(Option('build-dir',Root+PathDelim+'.regression-build'));
    LazBuild := Option('lazbuild','lazbuild');
    FPC := Option('fpc','fpc');
    PCP := Option('pcp','');
    OnlyPrefix := Option('only','');
    for i := 1 to ParamCount do
      if ParamStr(i) = '--portable' then PortableOnly := true;
    TestTimeout := StrToQWord(Option('timeout','120'))*1000;
    if TestTimeout = 0 then raise Exception.Create('Timeout must be positive');
    fileName := Option('manifest',Root+PathDelim+'test'+PathDelim+'regression'+PathDelim+'tests.txt');
    manifest := TStringList.Create;
    fields := TStringList.Create;
    try
      manifest.LoadFromFile(fileName);
      fields.StrictDelimiter := true;
      fields.Delimiter := '|';
      for i := 0 to manifest.Count-1 do
      begin
        line := Trim(manifest[i]);
        if (line = '') or (line[1] = '#') then continue;
        fields.DelimitedText := line;
        if fields.Count <> 2 then raise Exception.Create('Invalid manifest line: '+line);
        if (OnlyPrefix <> '') and not AnsiStartsStr(OnlyPrefix,fields[0]) then continue;
        RunTest(Trim(fields[0]),Trim(fields[1]));
      end;
      if Selected = 0 then raise Exception.Create('No regression test selected');
    finally
      fields.Free;
      manifest.Free;
    end;
    WriteLn('Regression results: ',Passed,' passed, ',Failed,' failed, ',Skipped,' skipped');
    if Failed <> 0 then Halt(1);
  except
    on E: Exception do
    begin
      WriteLn(StdErr,'FAIL: ',E.ClassName,': ',E.Message);
      Halt(1);
    end;
  end;
end.
