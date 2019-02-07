program parseunicodeclasses;

uses Classes, sysutils;

  procedure GenerateUnicodeFunctions;
  const Indent = '      ';
  var
    tIn,tOut: TextFile;

    procedure IncludeClasses(AClasses: TStrings; AMinCode, AMaxCode: integer);
    var
      line,curBidi,newBidi: string;
      codes: array of integer;
      codeCount: integer;
      cells: TStringList;
      curCode: LongInt;

      procedure FlushCase;
      var i: integer;
        buf: string;
        bufLines: TStringList;
      begin
        if codeCount = 0 then exit;

        bufLines := TStringList.Create;
        i := 0;
        buf := Indent+'  ';
        while i < codeCount do
        begin
          if i > 0 then buf += ', ';

          if length(buf) > 95 then
          begin
            bufLines.Add(buf);
            buf := Indent+'  ';
          end;

          if (i+2 < codeCount) and (codes[i]+1 = codes[i+1]) and (codes[i+1]+1 = codes[i+2]) then
          begin
            buf += '$'+IntToHex(codes[i],2) + '..';
            while (i+1 < codeCount) and (codes[i]+1 = codes[i+1]) do inc(i);
            buf += '$'+IntToHex(codes[i],2);
          end else
            buf += '$'+IntToHex(codes[i],2);

          inc(i);
        end;

        if trim(buf) <> '' then bufLines.Add(buf);

        buf := '';
        for i := 0 to bufLines.Count-1 do
        begin
          if i > 0 then buf += LineEnding;
          buf += bufLines[i];
        end;

        bufLines.Free;

        case curBidi of
        'CS': WriteLn(tOut,buf+': result := ubcCommonSeparator;');
        'L': WriteLn(tOut,buf+': result := ubcLeftToRight;');
        'EN': WriteLn(tOut,buf+': result := ubcEuropeanNumber;');
        'ES': WriteLn(tOut,buf+': result := ubcEuropeanNumberSeparator;');
        'ET': WriteLn(tOut,buf+': result := ubcEuropeanNumberTerminator;');
        'R': WriteLn(tOut,buf+': result := ubcRightToLeft;');
        'AL': WriteLn(tOut,buf+': result := ubcArabicLetter;');
        'AN': WriteLn(tOut,buf+': result := ubcArabicNumber;');
        'NSM': WriteLn(tOut,buf+': result := ubcNonSpacingMark;');
        'BN': WriteLn(tOut,buf+': result := ubcBoundaryNeutral;');
        'B': WriteLn(tOut,buf+': result := ubcParagraphSeparator;');
        'S': WriteLn(tOut,buf+': result := ubcSegmentSeparator;');
        'WS': WriteLn(tOut,buf+': result := ubcWhiteSpace;');
        'ON': WriteLn(tOut,buf+': result := ubcOtherNeutrals;');
        end;
        codeCount:= 0;
      end;

    begin
      AssignFile(tIn, 'UnicodeData.txt');
      Reset(tIn);

      cells := TStringList.Create;
      codeCount := 0;
      curBidi := '?';
      while not eof(tIn) do
      begin
        ReadLn(tIn,line);
        cells.Delimiter := ';';
        cells.QuoteChar := '"';
        cells.StrictDelimiter := true;
        cells.DelimitedText := line;
        if cells.Count >= 5 then
        begin
          newBidi := cells[4];
          if AClasses.IndexOf(newBidi)<>-1 then
          begin
            if newBidi <> curBidi then
            begin
              FlushCase;
              curBidi := newBidi;
            end;
            curCode := StrToInt('$'+cells[0]);
            if (curCode >= AMinCode) and (curCode <= AMaxCode) then
            begin
              if codeCount >= length(codes) then
                setlength(codes, codeCount*2 + 8);
              codes[codeCount] := curCode;
              inc(codeCount);
            end;
          end;
        end;
      end;
      FlushCase;
      cells.Free;

      CloseFile(tIn);
    end;

    procedure ParseUnicodeBidiClasses;
    var c: TStringList;

      procedure Include(AMinCode,AMaxCode: integer);
      begin
        Writeln(tOut,Indent+'case u of');
        c.CommaText := 'BN';
        IncludeClasses(c, AMinCode,AMaxCode);
        c.CommaText := 'S';
        IncludeClasses(c, AMinCode,AMaxCode);
        c.CommaText := 'B';
        IncludeClasses(c, AMinCode,AMaxCode);
        c.CommaText := 'WS';
        IncludeClasses(c, AMinCode,AMaxCode);
        c.CommaText := 'L,R,AL';
        IncludeClasses(c, AMinCode,AMaxCode);
        c.CommaText := 'EN';
        IncludeClasses(c, AMinCode,AMaxCode);
        c.CommaText := 'ES';
        IncludeClasses(c, AMinCode,AMaxCode);
        c.CommaText := 'ET';
        IncludeClasses(c, AMinCode,AMaxCode);
        c.CommaText := 'AN';
        IncludeClasses(c, AMinCode,AMaxCode);
        c.CommaText := 'CS,NSM';
        IncludeClasses(c, AMinCode,AMaxCode);
        c.CommaText := 'ON';
        IncludeClasses(c, AMinCode,AMaxCode);
        writeln(tout,Indent+'else result := ubcUnknown;');
        writeln(tout,Indent+'end;');
      end;

    begin
      Writeln(tOut,'function GetUnicodeBidiClass(u: cardinal): TUnicodeBidiClass;');
      FormatSettings.ShortDateFormat := 'yyyy/mm/dd';
      Writeln(tOut,'begin //generated '+DateToStr(Date));
      c := TStringList.Create;
      writeln(tOut,'  case u of');
      writeln(tOut,'  $00000..$07FFF:');
      writeln(tOut,'    case u of');
      writeln(tOut,'    $00000..$003FF:');
      Include($00000, $003FF);
      writeln(tOut,'    $00400..$007FF:');
      Include($00400, $007FF);
      writeln(tOut,'    $00800..$00FFF:');
      Include($00800, $00FFF);
      writeln(tOut,'    $01000..$01FFF:');
      Include($01000, $01FFF);
      writeln(tOut,'    else');
      Include($02000, $07FFF);
      writeln(tOut,'    end;');
      writeln(tOut,'  $08000..$0FFFF:');
      Include($08000, $0FFFF);
      writeln(tOut,'  else');
      writeln(tOut,'    case u of');
      writeln(tOut,'    $10000..$10FFF:');
      Include($10000, $10FFF);
      writeln(tOut,'    $11000..$117FF:');
      Include($11000, $117FF);
      writeln(tOut,'    $11800..$17FFF:');
      Include($11800, $17FFF);
      writeln(tOut,'    $18000..$FFFFF:');
      Include($18000, $FFFFF);
      writeln(tOut,'    else result := ubcUnknown;');
      writeln(tOut,'    end');
      writeln(tOut,'  end');


      c.Free;

      writeln(tout,'end;');
      writeln(tout);
    end;

    procedure ParseBidiBrackets;
    var elem: TStringList;
      line: string;
    begin
      Writeln(tOut,'type');
      writeln(tout,'  TUnicodeBracketInfo = record');
      writeln(tout,'    IsBracket: boolean;');
      writeln(tout,'    OpeningBracket,ClosingBracket: cardinal;');
      writeln(tout,'  end;');
      Writeln(tOut,'function GetUnicodeBracketInfo(u: cardinal): TUnicodeBracketInfo;');
      Writeln(tOut,'  procedure Bracket(AOpening,AClosing: cardinal);');
      Writeln(tOut,'  begin');
      Writeln(tOut,'    result.IsBracket := true;');
      Writeln(tOut,'    result.OpeningBracket := AOpening;');
      Writeln(tOut,'    result.ClosingBracket := AClosing;');
      Writeln(tOut,'  end;');
      Writeln(tOut,'begin');
      Writeln(tOut,'  case u of');

      assignfile(tIn, 'BidiBrackets.txt');
      reset(tin);
      elem := TStringList.Create;
      elem.Delimiter := ';';
      elem.StrictDelimiter:= true;
      while not eof(tin) do
      begin
        readln(tin, line);
        elem.DelimitedText:= line;
        if elem.Count >= 3 then
        begin
          if copy(trim(elem[2]),1,1) = 'o' then
            writeln(tOut,'  $'+trim(elem[0])+', $'+trim(elem[1])+': Bracket($'+trim(elem[0])+', $'+trim(elem[1])+');');
        end;
      end;
      elem.Free;
      closefile(tin);

      writeln(tout,'  else');
      writeln(tout,'    begin');
      writeln(tout,'      result.IsBracket := false;');
      writeln(tout,'      result.OpeningBracket := 0;');
      writeln(tout,'      result.ClosingBracket := 0;');
      writeln(tout,'    end;');
      Writeln(tOut,'  end;');
      Writeln(tOut,'end;');
      Writeln(tOut);
    end;

  begin
    AssignFile(tOut, 'UnicodeFunctions.generated.pas');
    Rewrite(tOut);

    ParseUnicodeBidiClasses;
    ParseBidiBrackets;

    CloseFile(tOut);
  end;

begin
  GenerateUnicodeFunctions;
end.

