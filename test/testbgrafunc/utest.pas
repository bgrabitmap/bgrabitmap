unit utest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TTest = class
  public
    Name: string;
    OpaqueDraw: boolean;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); virtual; abstract;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); virtual; abstract;
  end;

var
  ResourceDir: string;

implementation

end.

