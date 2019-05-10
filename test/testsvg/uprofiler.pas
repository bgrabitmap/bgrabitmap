unit UProfiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
 TProfiler = class
  private
   freq,
   start: Int64;

   function GetCounter: Int64;

  public
   constructor Create;

   procedure BeginMeasure;
   function EndMeasure: Double;

   function FormatTime(prefix: String; v: Double): String;

 end;

implementation

uses
 LCLIntf
 {$ifdef WINDOWS}
 , Windows
 {$endif};

constructor TProfiler.Create;
begin
 inherited;

 start:= 0;
 {$ifdef WINDOWS}
 if Windows.QueryPerformanceFrequency(freq) then
  Exit;
 {$endif}
 freq:= 1000;
end;

function TProfiler.GetCounter: Int64;
begin
 {$ifdef WINDOWS}
  if Windows.QueryPerformanceCounter(Result) then
   Exit;
 {$endif}
 Result:= LCLIntf.GetTickCount64;
end;

procedure TProfiler.BeginMeasure;
begin
 start:= GetCounter;
end;

function TProfiler.EndMeasure: Double;
begin
 Result:= (GetCounter - start) / freq;
end;

function TProfiler.FormatTime(prefix: String; v: Double): String;
begin
 Result:= FormatFloat(prefix+'0.######s',v);
end;

end.

