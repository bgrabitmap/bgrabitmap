program parseunicodeclasses;

uses Classes, sysutils, BGRAUTF8, BGRAUnicode, fgl;

type
  TIntegerList = specialize TFPGList<Integer>;

var
  UnicodeData: array of record
    Code: LongInt;
    Name, Category, BidiCategory, Decomposition: string;
    Mirrored: boolean;
    OldName: string;
  end;
  UnicodeCount: integer;

  procedure LoadUnicodeData;
  var
    lines, cells: TStringList;
    i: Integer;
  begin
    lines := TStringList.Create;
    lines.LoadFromFile('UnicodeData.txt');
    setlength(UnicodeData, lines.Count);
    UnicodeCount := 0;
    cells := TStringList.Create;
    cells.Delimiter := ';';
    cells.QuoteChar := '"';
    cells.StrictDelimiter := true;
    for i := 0 to lines.Count-1 do
    begin
      cells.DelimitedText := lines[i];
      if cells.Count >= 11 then
      with UnicodeData[UnicodeCount] do
      begin
        Code := StrToInt('$'+cells[0]);
        Name := cells[1];
        Category := cells[2];
        BidiCategory := cells[4];
        Decomposition:= cells[5];
        Mirrored := (cells[9] = 'Y');
        OldName := cells[10];
        inc(UnicodeCount);
      end;
    end;
    SetLength(UnicodeData, unicodeCount);
    lines.Free;
  end;

  function ArrayOfCodeToCase(ACodes: array of integer; AIndent: string): string;
  var
    codeCount, i: Integer;
    bufLines: TStringList;
    buf: String;
  begin
    codeCount := length(ACodes);
    if codeCount = 0 then exit('');

    bufLines := TStringList.Create;
    i := 0;
    buf := AIndent+'  ';
    while i < codeCount do
    begin
      if i > 0 then buf += ', ';

      if length(buf) > 95 then
      begin
        bufLines.Add(buf);
        buf := AIndent+'  ';
      end;

      if (i+2 < codeCount) and (ACodes[i]+1 = ACodes[i+1]) and (ACodes[i+1]+1 = ACodes[i+2]) then
      begin
        buf += '$'+IntToHex(ACodes[i],2) + '..';
        while (i+1 < codeCount) and (ACodes[i]+1 = ACodes[i+1]) do inc(i);
        buf += '$'+IntToHex(ACodes[i],2);
      end else
        buf += '$'+IntToHex(ACodes[i],2);

      inc(i);
    end;

    if trim(buf) <> '' then bufLines.Add(buf);

    result := '';
    for i := 0 to bufLines.Count-1 do
    begin
      if i > 0 then result += LineEnding;
      result += bufLines[i];
    end;

    bufLines.Free;
    result += ': ';
  end;

  procedure GenerateUnicodeFunctions;
  const Indent = '      ';
  var
    tIn, tOut: TextFile;

    procedure ParseUnicodeData;

      procedure IncludeClasses(AClasses: TStrings; AMinCode, AMaxCode: integer);
      var
        curBidi: string;
        codes: array of integer;
        codeCount: integer;

        procedure FlushCase;
        var
          caseStr: string;
        begin
          if codeCount = 0 then exit;

          caseStr := ArrayOfCodeToCase(slice(codes, codeCount), Indent);

          case curBidi of
          'CS': WriteLn(tOut,caseStr+'result := ubcCommonSeparator;');
          'L': WriteLn(tOut,caseStr+'result := ubcLeftToRight;');
          'EN': WriteLn(tOut,caseStr+'result := ubcEuropeanNumber;');
          'ES': WriteLn(tOut,caseStr+'result := ubcEuropeanNumberSeparator;');
          'ET': WriteLn(tOut,caseStr+'result := ubcEuropeanNumberTerminator;');
          'R': WriteLn(tOut,caseStr+'result := ubcRightToLeft;');
          'AL': WriteLn(tOut,caseStr+'result := ubcArabicLetter;');
          'AN': WriteLn(tOut,caseStr+'result := ubcArabicNumber;');
          'NSM': WriteLn(tOut,caseStr+'result := ubcNonSpacingMark;');
          'BN': WriteLn(tOut,caseStr+'result := ubcBoundaryNeutral;');
          'B': WriteLn(tOut,caseStr+'result := ubcParagraphSeparator;');
          'S': WriteLn(tOut,caseStr+'result := ubcSegmentSeparator;');
          'WS': WriteLn(tOut,caseStr+'result := ubcWhiteSpace;');
          'ON': WriteLn(tOut,caseStr+'result := ubcOtherNeutrals;');
          end;
          codeCount:= 0;
        end;

      var
        newBidi: string;
        curCode: LongInt;
        i: integer;

      begin
        writeln('Parsing unicode data for classes ', AClasses.DelimitedText,'...');
        codes := nil;
        codeCount := 0;
        curBidi := '?';
        for i := 0 to UnicodeCount-1 do
        begin
          newBidi := UnicodeData[i].BidiCategory;
          if AClasses.IndexOf(newBidi)<>-1 then
          begin
            if newBidi <> curBidi then
            begin
              FlushCase;
              curBidi := newBidi;
            end;
            curCode := UnicodeData[i].Code;
            if (curCode >= AMinCode) and (curCode <= AMaxCode) then
            begin
              if codeCount >= length(codes) then
                setlength(codes, codeCount*2 + 8);
              codes[codeCount] := curCode;
              inc(codeCount);
            end;
          end;
        end;
        FlushCase;
      end;

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

      procedure ParseUnicodeMirrored;
      var
        codes: array of LongInt;
        codeCount, i: integer;
        curCode: integer;
      begin
        writeln('Parsing unicode data for mirorred characters...');
        codes := nil;
        codeCount := 0;
        for i := 0 to UnicodeCount-1 do
        begin
          if UnicodeData[i].Mirrored then
          begin
            curCode := UnicodeData[i].Code;
            if codeCount >= length(codes) then
              setlength(codes, codeCount*2 + 8);
            codes[codeCount] := curCode;
            inc(codeCount);
          end;
        end;

        Writeln(tOut,'function IsUnicodeMirrored(u: LongWord): boolean;');
        writeln(tout,'begin');
        writeln(tout,'  case u of');
        writeln(tout, ArrayOfCodeToCase(Slice(codes, codeCount), '  '), 'result:= true;');
        writeln(tout,'  else result := false;');
        writeln(tout,'  end;');
        writeln(tout,'end;');
        writeln(tout);
      end;

    begin
      Writeln(tOut,'function GetUnicodeBidiClass(u: LongWord): TUnicodeBidiClass;');
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
      writeln(tOut,'    $00800..$00BFF:');
      Include($00800, $00BFF);
      writeln(tOut,'    $00C00..$00FFF:');
      Include($00C00, $00FFF);
      writeln(tOut,'    $01000..$017FF:');
      Include($01000, $017FF);
      writeln(tOut,'    $01800..$01FFF:');
      Include($01800, $01FFF);
      writeln(tOut,'    $02000..$02FFF:');
      Include($02000, $02FFF);
      writeln(tOut,'    else');
      Include($03000, $07FFF);
      writeln(tOut,'    end;');
      writeln(tOut,'  $08000..$0BFFF:');
      Include($08000, $0BFFF);
      writeln(tOut,'  $0C000..$0FFFF:');
      Include($0C000, $0FFFF);
      writeln(tOut,'  else');
      writeln(tOut,'    case u of');
      writeln(tOut,'    $10000..$107FF:');
      Include($10000, $107FF);
      writeln(tOut,'    $10800..$10FFF:');
      Include($10800, $10FFF);
      writeln(tOut,'    $11000..$117FF:');
      Include($11000, $117FF);
      writeln(tOut,'    $11800..$17FFF:');
      Include($11800, $17FFF);
      writeln(tOut,'    $18000..$1DFFF:');
      Include($18000, $1DFFF);
      writeln(tOut,'    $1E000..$FFFFF:');
      Include($1E000, $FFFFF);
      writeln(tOut,'    else result := ubcUnknown;');
      writeln(tOut,'    end');
      writeln(tOut,'  end');
      c.Free;

      writeln(tout,'end;');
      writeln(tout);

      ParseUnicodeMirrored;
    end;

    procedure ParseBidiBrackets;
    var elem: TStringList;
      line: string;
      tIn: TextFile;
    begin
      Writeln(tOut,'type');
      writeln(tout,'  TUnicodeBracketInfo = record');
      writeln(tout,'    IsBracket: boolean;');
      writeln(tout,'    OpeningBracket,ClosingBracket: LongWord;');
      writeln(tout,'  end;');
      Writeln(tOut,'function GetUnicodeBracketInfo(u: LongWord): TUnicodeBracketInfo;');
      Writeln(tOut,'  procedure Bracket(AOpening,AClosing: LongWord);');
      Writeln(tOut,'  begin');
      Writeln(tOut,'    result.IsBracket := true;');
      Writeln(tOut,'    result.OpeningBracket := AOpening;');
      Writeln(tOut,'    result.ClosingBracket := AClosing;');
      Writeln(tOut,'  end;');
      Writeln(tOut,'begin');
      Writeln(tOut,'  case u of');

      writeln('Parsing bracket data...');
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

    procedure ParseArabicLigature;
    var
      line: string;
      cells: TStringList;
      chars: TStringList;
      u: LongWord;
      j: Integer;

      procedure AddJoiningType(joinType: string; joinTypeEnum: string; AIndent: string; AMinIndex,AMaxIndex: integer);
      var
        i,nb: Integer;
        charsList: array of integer;
      begin
        nb := 0;
        for i := AMinIndex to AMaxIndex do
          if chars.ValueFromIndex[i]=joinType then inc(nb);
        if nb = 0 then exit;
        setlength(charsList, nb);
        nb := 0;
        for i := AMinIndex to AMaxIndex do
          if chars.ValueFromIndex[i]=joinType then
          begin
            charsList[nb] := StrToInt('$'+chars.Names[i]);
            inc(nb);
          end;
        writeln(tOut,ArrayOfCodeToCase(charsList, AIndent)+'result := '+joinTypeEnum+';');
      end;

      procedure AddJoiningTypeRange(AMinIndex,AMaxIndex: integer; AIndent: string; AForceCase: boolean = false);
      const MaxGaps = 45;
      var
        mid, i, gaps, halfGaps: Integer;
      begin
        gaps := 0;
        for i := AMinIndex+1 to AMaxIndex do
          if (StrToInt('$'+chars.Names[i])-StrToInt('$'+chars.Names[i-1]) > 1) or
            (chars.ValueFromIndex[i] <> chars.ValueFromIndex[i-1]) then inc(gaps);
        if (gaps > MaxGaps) and not AForceCase then
        begin
          halfGaps := 0;
          mid := (AMinIndex+AMaxIndex) div 2;
          for i := AMinIndex+1 to AMaxIndex do
            if (StrToInt('$'+chars.Names[i])-StrToInt('$'+chars.Names[i-1]) > 1) or
              (chars.ValueFromIndex[i] <> chars.ValueFromIndex[i-1]) then
            begin
              inc(halfGaps);
              if halfGaps >= gaps shr 1 then
              begin
                mid := i;
                break;
              end;
            end;
          if gaps <= MaxGaps*2.5 then
          begin
            writeln(tOut,AIndent, 'if u <= $', chars.Names[mid],' then');
            AddJoiningTypeRange(AMinIndex, mid, AIndent+'  ', true);
            writeln(tOut,AIndent, 'else');
            AddJoiningTypeRange(mid+1, AMaxIndex, AIndent+'  ', true);
          end else
          begin
            writeln(tOut,AIndent, 'if u <= $', chars.Names[mid],' then begin');
            AddJoiningTypeRange(AMinIndex, mid, AIndent+'  ');
            writeln(tOut,AIndent, 'end else begin');
            AddJoiningTypeRange(mid+1, AMaxIndex, AIndent+'  ');
            writeln(tOut,AIndent, 'end');
          end;
        end else
        begin
          writeln(tOut,AIndent, 'case u of');
          AddJoiningType('T', 'ujtTransparent', AIndent, AMinIndex, AMaxIndex);
          AddJoiningType('R', 'ujtRightJoining', AIndent, AMinIndex, AMaxIndex);
          AddJoiningType('L', 'ujtLeftJoining', AIndent, AMinIndex, AMaxIndex);
          AddJoiningType('D', 'ujtDualJoining', AIndent, AMinIndex, AMaxIndex);
          AddJoiningType('C', 'ujtJoinCausing', AIndent, AMinIndex, AMaxIndex);
          writeln(tOut,AIndent, 'end');
        end;
      end;

    begin
      writeln('Parsing arabic ligature data...');
      chars := TStringList.Create;
      for j := 0 to UnicodeCount-1 do
      begin
        if (UnicodeData[j].Category = 'Mn') or (UnicodeData[j].Category = 'Me')
          or (UnicodeData[j].Category = 'Cf') then
            chars.Values[IntToHex(UnicodeData[j].Code,6)] := 'T';
      end;
      assignfile(tIn, 'ArabicShaping.txt');
      reset(tIn);
      cells := TStringList.Create;
      cells.Delimiter := ';';
      cells.QuoteChar := '"';
      cells.StrictDelimiter := true;
      while not eof(tIn) do
      begin
        readln(tIn, line);
        if (line = '') or (line[1]='#') then continue;
        cells.DelimitedText:= line;
        if cells.Count >= 4 then
        begin
          u := StrToInt('$'+cells[0]);
          if trim(cells[2]) = 'U' then
          begin
            j := chars.IndexOfName(IntToHex(u,6));
            if j <> -1 then
              chars.Delete(j);
          end
          else
            chars.Values[IntToHex(u,6)] := trim(cells[2]);
        end;
      end;
      closefile(tIn);
      cells.Free;
      chars.Sort;
      Writeln(tOut,'function GetUnicodeJoiningType(u: LongWord): TUnicodeJoiningType;');
      Writeln(tOut,'begin');
      writeln(tOut,'  result := ujtNonJoining;');
      AddJoiningTypeRange(0, chars.Count-1, '  ');
      chars.Free;
      Writeln(tOut,'end;');
      Writeln(tOut);
    end;

  begin
    AssignFile(tOut, 'UnicodeFunctions.generated.pas');
    Rewrite(tOut);

    ParseUnicodeData;
    ParseBidiBrackets;
    ParseArabicLigature;

    CloseFile(tOut);
  end;

  function ListCompareBinary(List: TStringList; Index1, Index2: Integer): Integer;
  begin
    result := CompareStr(List[Index1], List[Index2]);
  end;

  procedure UTF8RecompositionFunction;
  const
    CombineLeftOnly = '093F,094E,' + {DEVANAGARI}
      '09BF,09C7,09C8,' + {BENGALI}
      '0A3F,' + {GURMUKHI}
      '0ABF,' + {GUJARATI}
      '0B47,0B48,0B4B,0B4C,' + {ORIYA}
      '0BC6,0BC7,0BC8,' + {TAMIL}
      '0D46,0D47,0D48,' + {MALAYALAM}
      '0DD9,0DDA,0DDB,0DDC,0DDD,0DDE,' + {SINHALA}
      '1031,103C,1084,' + {MYANMAR}
      '17BE,17C1,17C2,17C3,' + {KHMER}
      '1A19,' + {BUGINESE}
      '1B3E,1B3F,' + {BALINESE}
      '302E,302F,' + {HANGUL}
      'A9BA,A9BB,A9BF,' + {JAVANESE}
      'AA2F,AA30,AA34,'; {CHAM}
    CombineLeftAndRight = '09CB,09CC,' + {BENGALI}
      '0BCA,0BCB,0BCC,' + {TAMIL}
      '0D4A,0D4B,0D4C,' + {MALAYALAM}
      '17BF,17C0,17C4,17C5,' + {KHMER}
      '1B3D,1B40,1B41,'; {BALINESE}

  type TDecompositionKind = string;
  const dMultichar = 'arNone';
        dInitial = 'arInitial';
        dMedial = 'arMedial';
        dFinal = 'arFinal';
        dIsolated = 'arIsolated';
  var tOut: TextFile;
    decomposed, kind, decomposedUTF8: string;
    thousands: integer;
    mergedU,nextU: longword;
    posClose, posSpace: SizeInt;
    hasNSM, isLa: Boolean;
    correspList: TStringList;
    i, decomposedLen, j: Integer;
    typedKind: TDecompositionKind;
    combineLeftList, combineRightList, combineLeftAndRightList: TIntegerList;
    combineThousands: TIntegerList;

    function RemoveUptoTab(AText: string): string;
    var
      idxTab: SizeInt;
    begin
      idxTab := pos(#9, AText);
      result := copy(AText, idxTab+1, length(AText)-idxTab);
    end;

    procedure AddCaseUnicodeCombineLayout(AIndent: string; AThousand: longword);

      function MakeCodeList(AList: TIntegerList): boolean;
      var
        s: String;
        i: integer;
        prevCode,curCode: LongWord;
        inRange: boolean;
      begin
        result := false;
        s := AIndent;
        prevCode := 0;
        inRange := false;
        for i := 0 to AList.Count-1 do
        begin
          curCode := AList[i];
          if curCode shr 12 <> AThousand then continue;
          if s <> AIndent then
          begin
            if not inRange and (curCode = prevCode+1) then
            begin
              AppendStr(s, '..');
              inRange := true;
            end else
            if inRange then
            begin
              if curCode <> prevCode+1 then
              begin
                AppendStr(s, '$' + IntToHex(prevCode,4));
                inRange := false;
              end;
            end;
          end;
          if not inRange then
          begin
            if s <> AIndent then
              AppendStr(s, ', ');
            if length(s) >= 72 then
            begin
              writeln(tOut, s);
              result := true;
              s := AIndent;
            end;
            AppendStr(s, '$' + IntToHex(curCode,4));
          end;
          prevCode := curCode;
        end;
        if inRange then AppendStr(s, '$' + IntToHex(prevCode, 4));
        if s <> AIndent then
        begin
          write(tOut, s);
          result := true;
        end;
      end;

    begin
      writeln(tOut, AIndent, 'case u of');
      if MakeCodeList(combineLeftList) then
        writeln(tOut, ': exit(uclLeft);');
      if MakeCodeList(combineRightList) then
        writeln(tOut, ': exit(uclRight);');
      if MakeCodeList(combineLeftAndRightList) then
        writeln(tOut, ': exit(uclLeftAndRight);');
      writeln(tOut, AIndent, 'end;');
    end;

  begin
    writeln('Parsing decomposition data...');
    correspList := TStringList.Create;
    combineLeftList := TIntegerList.Create;
    combineRightList := TIntegerList.Create;
    combineLeftAndRightList := TIntegerList.Create;
    combineThousands := TIntegerList.Create;

    for j := 0 to UnicodeCount-1 do
    begin
      mergedU := UnicodeData[j].Code;
      if GetUnicodeBidiClass(mergedU) = ubcNonSpacingMark then continue;
      if UnicodeData[j].Category = 'Mc' then
      begin
        thousands := mergedU shr 12;
        if combineThousands.IndexOf(thousands) = -1 then combineThousands.Add(thousands);
        if pos(IntToHex(mergedU,4)+',', CombineLeftOnly) <> 0 then combineLeftList.Add(mergedU)
        else if pos(IntToHex(mergedU,4)+',', CombineLeftAndRight) <> 0 then combineLeftAndRightList.Add(mergedU)
        else combineRightList.Add(mergedU);
      end;
      decomposed := UnicodeData[j].Decomposition;
      if decomposed = '' then continue;
      typedKind := dMultichar;
      if decomposed[1] = '<' then
      begin
        posClose := pos('>', decomposed);
        if posClose = 0 then continue;
        kind := copy(decomposed,1,posClose);
        delete(decomposed, 1, posClose);
        if kind = '<initial>' then typedKind := dInitial else
        if kind = '<medial>' then typedKind := dMedial else
        if kind = '<final>' then typedKind := dFinal else
        if kind = '<isolated>' then typedKind := dIsolated else
        if (kind = '<compat>') and (mergedU >= $FB00) and (mergedU <= $FB04) then
          typedKind := dMultichar
        else
          continue;
        decomposed := trim(decomposed);
      end;
      decomposedUTF8 := '';
      decomposedLen := 0;
      hasNSM := false;
      while decomposed <> '' do
      begin
        posSpace := pos(' ',decomposed);
        if posSpace = 0 then posSpace := length(decomposed)+1;
        nextU := strToInt('$'+copy(decomposed,1,posSpace-1));
        if GetUnicodeBidiClass(nextU) = ubcNonSpacingMark then hasNSM := true;
        AppendStr(decomposedUTF8, UnicodeCharToUTF8(nextU));
        delete(decomposed, 1, posSpace);
        inc(decomposedLen);
      end;
      isLa := (decomposedUTF8 = UTF8_ARABIC_LAM+UTF8_ARABIC_ALEPH) or
              (decomposedUTF8 = UTF8_ARABIC_LAM+UTF8_ARABIC_ALEPH_HAMZA_BELOW) or
              (decomposedUTF8 = UTF8_ARABIC_LAM+UTF8_ARABIC_ALEPH_HAMZA_ABOVE) or
              (decomposedUTF8 = UTF8_ARABIC_LAM+UTF8_ARABIC_ALEPH_MADDA_ABOVE);
      if ((typedKind = dMultichar) and (decomposedLen > 1)
           and (hasNSM or (copy(decomposedUTF8,1,1) = 'f'))) or
         ((typedKind <> dMultichar) and ((decomposedLen = 1) or isLa)) then
        correspList.Add(decomposedUTF8+#9+'('+
           'de:''' + decomposedUTF8 + '''; ' +
           're:''' + UnicodeCharToUTF8(mergedU) + '''; ' +
           'join:' + typedKind +
           ')');
    end;

    correspList.CustomSort(@ListCompareBinary);

    AssignFile(tOut, 'utf8decomposition.inc');
    Rewrite(tOut);
    writeln(tOut, 'type');
    writeln(tOut, '  TArabicJoin = (arNone, arInitial, arMedial, arFinal, arIsolated);');
    writeln(tOut, '  TUTF8Decomposition = record');
    writeln(tOut, '    de, re: string; //decomposed, recomposed UTF8');
    writeln(tOut, '    join: TArabicJoin;');
    writeln(tOut, '  end;');
    writeln(tOut, 'const');
    writeln(tOut, '  UTF8Decomposition : array[0..', correspList.Count-1, '] of TUTF8Decomposition = (');
    for i := 0 to correspList.Count-1 do
      if i <> correspList.Count-1 then
        writeln(tOut, '  ', RemoveUptoTab(correspList[i]), ',')
      else
        writeln(tOut, '  ', RemoveUptoTab(correspList[i]));
    writeln(tOut, '  );');
    writeln(tout);
    CloseFile(tOut);

    AssignFile(tOut, 'UnicodeFunctions.generated.pas');
    Append(tOut);
    writeln(tOut, 'function GetUnicodeCombiningLayout(u: LongWord): TUnicodeCombiningLayout;');
    writeln(tOut, 'begin');
    writeln(tOut, '  case u shr 24 of');
    for i := 0 to combineThousands.Count-1 do
    begin
      writeln(tOut, '  $',IntToHex(combineThousands[i],2),':');
      AddCaseUnicodeCombineLayout('    ', combineThousands[i]);
    end;
    writeln(tOut, '  end;');
    writeln(tOut, '  result := uclNone;');
    writeln(tOut, 'end;');
    Writeln(tOut);
    correspList.Free;
    CloseFile(tOut);
  end;

begin
  LoadUnicodeData;
  GenerateUnicodeFunctions;
  UTF8RecompositionFunction;
  writeln('Done.');
end.

