// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Reader for XPM format }
unit BGRAReadXPM;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPReadXPM, FPimage;

type
  { Reader for XPM image format }
  TBGRAReaderXPM = class(TFPReaderXPM)
    protected
      procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
      function InternalCheck(Str: TStream): boolean; override;
      class function InternalSize(Str: TStream): TPoint; override;
    public
      class procedure ConvertToXPM3(ASource: TStream; ADestination: TStream); static;
  end;

implementation

uses BGRABitmapTypes;

{ TBGRAReaderXPM }

procedure TBGRAReaderXPM.InternalRead(Str: TStream; Img: TFPCustomImage);
var tempStream: TMemoryStream;
begin
  tempStream := TMemoryStream.Create;
  try
    ConvertToXPM3(Str, tempStream);
    tempStream.Position:= 0;
    try
      img.UsePalette := true;
      inherited InternalRead(tempStream, Img);
    finally
    end;
  finally
    tempStream.free;
  end;
end;

function TBGRAReaderXPM.InternalCheck(Str: TStream): boolean;
var {%H-}magic : array[0..5] of char;
    l : integer;
    prevPos: int64;
begin
  try
    prevPos := str.Position;
    l := str.Read ({%H-}magic[0],sizeof(magic));
    str.Position:= prevPos;
    result := (l = sizeof(magic)) and (magic = '! XPM2');
    if not result then result := inherited InternalCheck(Str)
  except
    result := false;
  end;
end;

class function TBGRAReaderXPM.InternalSize(Str: TStream): TPoint;
var
  lst: TStringList;
  valuesLine: string;
  startQuote, endQuote, w, h, i, errPos: Integer;
begin
  result := Point(0, 0);
  valuesLine := '';
  lst := TStringList.Create;
  try
    lst.LoadFromStream(Str);
    if (lst[0] = '! XPM2') and (lst.Count > 1) then
      valuesLine := lst[1]
    else if (lst[0] = '/* XPM */') then
    begin
      for i := 1 to lst.Count-1 do
      begin
        startQuote := lst[i].IndexOf('"');
        endQuote := lst[i].LastIndexOf('"');
        if endQuote > startQuote then
        begin
          valuesLine := lst[i].Substring(startQuote + 1, endQuote-startQuote - 1);
          break;
        end;
      end;
    end;
    if valuesLine <> '' then
    begin
      lst.Clear;
      lst.Delimiter := ' ';
      lst.DelimitedText := valuesLine;
      if lst.Count >= 2 then
      begin
        val(lst[0], w, errPos);
        if errPos = 0 then
        begin
          val(lst[1], h, errPos);
          if errPos = 0 then
            result := Point(w, h);
        end;
      end;
    end;
  finally
    lst.Free;
  end;
end;

class procedure TBGRAReaderXPM.ConvertToXPM3(ASource: TStream;
  ADestination: TStream);
var
  lst: TStringList;
  i : integer;
begin
  lst := TStringList.Create;
  try
    lst.LoadFromStream(ASource);
    if (lst[0] = '! XPM2') and (lst.count > 1) then
    begin
      lst[0] := '/* XPM */';
      lst.Insert(1, 'static char * data[] = {');
      for i := 2 to lst.Count-2 do
        lst[i] := '"' + lst[i] + '",';
      lst[lst.count-1] := '"' + lst[lst.count-1] + '"';
      lst.Add('}');
    end;
    lst.SaveToStream(ADestination);
  finally
    lst.free;
  end;
end;

initialization
  BGRARegisterImageReader(ifXPixMap, TBGRAReaderXPM, True, 'XPM Format', 'xpm');

end.

