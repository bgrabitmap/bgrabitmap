unit BGRAReadPNM;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPReadPNM, FPimage, BGRABitmapTypes;

type
  { Reader for P?M image format }

  { TBGRAReaderPNM }

  TBGRAReaderPNM = class(TFPReaderPNM)
    protected
      class function InternalSize(Str: TStream): TPoint; override;
  end;

implementation

{ TBGRAReaderPNM }

class function TBGRAReaderPNM.InternalSize(Str: TStream): TPoint;
var lst: TStringList;
  valuesLine: String;
  w, h, errPos, valuesIndex: integer;
begin
  result := Point(0, 0);
  lst := TStringList.Create;
  try
    lst.LoadFromStream(Str);
    if (lst.Count > 0) and (length(lst[0]) = 2) and lst[0].StartsWith('P')
      and (lst[0][2] in ['1'..'7']) then
    begin
      valuesIndex := 1;
      while (valuesIndex < lst.Count) and (lst[valuesIndex].StartsWith('#')) do
       inc(valuesIndex);
      if valuesIndex < lst.Count then
      begin
        valuesLine := lst[valuesIndex];
        lst.Clear;
        lst.Delimiter:= ' ';
        lst.DelimitedText:= valuesLine;
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
    end;
  finally
    lst.Free;
  end;
end;

initialization

  BGRARegisterImageReader(ifPortableAnyMap, TBGRAReaderPNM, False, 'Netpbm Portable aNyMap', 'pnm;pbm;pgm;ppm');

end.

