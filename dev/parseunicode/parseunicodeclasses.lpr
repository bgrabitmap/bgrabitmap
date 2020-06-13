// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
program parseunicodeclasses;

uses Classes, sysutils, fgl, LazUTF8;

type
  TIntegerList = specialize TFPGList<Integer>;

var
  UnicodeData: array of record
    Code: LongInt;
    Name, Category: string;
    CombiningClass: byte;
    BidiClass, Decomposition: string;
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
        CombiningClass:= StrToInt(cells[3]);
        BidiClass := cells[4];
        Decomposition:= cells[5];
        Mirrored := (cells[9] = 'Y');
        OldName := cells[10];
        inc(UnicodeCount);
      end;
    end;
    SetLength(UnicodeData, unicodeCount);
    lines.Free;
  end;

  function IndexOfUnicode(u: LongInt): integer;
  var
    low, high, mid: Integer;
  begin
    low := 0;
    high := UnicodeCount-1;
    while low < high do
    begin
      mid := (low+high) div 2;
      if u > UnicodeData[mid].Code then
        low := mid+1
      else
        high := mid;
    end;
    if UnicodeData[low].Code = u then
      result := low
    else
      result := -1;
  end;

  function GetUnicodeBidiClass(u: LongInt): string;
  var
    idx: Integer;
  begin
    idx := IndexOfUnicode(u);
    if idx = -1 then
      result := ''
      else result := UnicodeData[idx].BidiClass;
  end;

  function GetUnicodeCombiningClass(u: LongInt): byte;
  var
    idx: Integer;
  begin
    idx := IndexOfUnicode(u);
    if idx = -1 then
      result := 0
      else result := UnicodeData[idx].CombiningClass;
  end;

  function UnicodeCharToUTF8(u: LongInt): string;
  begin
    if u >= 0 then
      result := UnicodeToUTF8(cardinal(u))
    else
      result := '';
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

  function ArrayOfCodeToCase(ACodes: TIntegerList; AIndent: string): string;
  var a: array of integer;
    i: Integer;
  begin
    setlengtH(a, ACodes.Count);
    for i := 0 to high(a) do
      a[i] := ACodes[i];
    result := ArrayOfCodeToCase(a, AIndent);
  end;

  procedure GenerateUnicodeFunctions;
  const Indent = '      ';
  var
    tIn, tOut: TextFile;

    procedure ParseBidiClasses;
    type
      TUnicodeBidiClass = (ubcBoundaryNeutral, ubcSegmentSeparator, ubcParagraphSeparator, ubcWhiteSpace, ubcOtherNeutrals,
                          ubcCommonSeparator, ubcNonSpacingMark,
                          ubcLeftToRight, ubcEuropeanNumber, ubcEuropeanNumberSeparator, ubcEuropeanNumberTerminator,
                          ubcRightToLeft, ubcArabicLetter, ubcArabicNumber,
                          ubcUnknown,
                          ubcCombiningLeftToRight,   //ubcLeftToRight in Mc category
                          ubcMirroredNeutral);       //ubcOtherNeutrals with Mirrored property

      procedure IncludeClasses(AClasses: TStrings; AMinCode, AMaxCode: integer);
      const
        MaxGapsPerClass = 20;
      var
        codes: array[TUnicodeBidiClass] of TIntegerList;
        gaps: array[TUnicodeBidiClass] of integer;

        procedure FlushCase(curBidi: TUnicodeBidiClass);
        var
          caseStr: string;
        begin
          if codes[curBidi].Count = 0 then exit;
          caseStr := ArrayOfCodeToCase(codes[curBidi], Indent);

          case curBidi of
          ubcCommonSeparator: WriteLn(tOut,caseStr+'result := ubcCommonSeparator;');
          ubcLeftToRight: WriteLn(tOut,caseStr+'result := ubcLeftToRight;');
          ubcCombiningLeftToRight: WriteLn(tOut,caseStr+'result := ubcCombiningLeftToRight;');
          ubcEuropeanNumber: WriteLn(tOut,caseStr+'result := ubcEuropeanNumber;');
          ubcEuropeanNumberSeparator: WriteLn(tOut,caseStr+'result := ubcEuropeanNumberSeparator;');
          ubcEuropeanNumberTerminator: WriteLn(tOut,caseStr+'result := ubcEuropeanNumberTerminator;');
          ubcRightToLeft: WriteLn(tOut,caseStr+'result := ubcRightToLeft;');
          ubcArabicLetter: WriteLn(tOut,caseStr+'result := ubcArabicLetter;');
          ubcArabicNumber: WriteLn(tOut,caseStr+'result := ubcArabicNumber;');
          ubcNonSpacingMark: WriteLn(tOut,caseStr+'result := ubcNonSpacingMark;');
          ubcBoundaryNeutral: WriteLn(tOut,caseStr+'result := ubcBoundaryNeutral;');
          ubcParagraphSeparator: WriteLn(tOut,caseStr+'result := ubcParagraphSeparator;');
          ubcSegmentSeparator: WriteLn(tOut,caseStr+'result := ubcSegmentSeparator;');
          ubcWhiteSpace: WriteLn(tOut,caseStr+'result := ubcWhiteSpace;');
          ubcMirroredNeutral: WriteLn(tOut,caseStr+'result := ubcMirroredNeutral;');
          ubcOtherNeutrals: WriteLn(tOut,caseStr+'result := ubcOtherNeutrals;');
          else raise exception.Create('Unknown bidi class');
          end;
          codes[curBidi].Clear;
          gaps[curBidi] := 0;
        end;

      var
        newBidi: TUnicodeBidiClass;
        curCode: LongInt;
        i: integer;

      begin
        write(' ', AClasses.DelimitedText);
        for newBidi := low(TUnicodeBidiClass) to high(TUnicodeBidiClass) do
        begin
          codes[newBidi] := TIntegerList.Create;
          gaps[newBidi] := 0;
        end;
        for i := 0 to UnicodeCount-1 do
        begin
          case UnicodeData[i].BidiClass of
          'CS': newBidi := ubcCommonSeparator;
          'L': newBidi := ubcLeftToRight;
          'EN': newBidi := ubcEuropeanNumber;
          'ES': newBidi := ubcEuropeanNumberSeparator;
          'ET': newBidi := ubcEuropeanNumberTerminator;
          'R': newBidi := ubcRightToLeft;
          'AL': newBidi := ubcArabicLetter;
          'AN': newBidi := ubcArabicNumber;
          'NSM': newBidi := ubcNonSpacingMark;
          'BN': newBidi := ubcBoundaryNeutral;
          'B': newBidi := ubcParagraphSeparator;
          'S': newBidi := ubcSegmentSeparator;
          'WS': newBidi := ubcWhiteSpace;
          'ON': newBidi := ubcOtherNeutrals;
          else continue;
          end;
          if (newBidi = ubcLeftToRight) and (UnicodeData[i].Category = 'Mc') then newBidi := ubcCombiningLeftToRight
          else if (newBidi = ubcOtherNeutrals) and UnicodeData[i].Mirrored then newBidi := ubcMirroredNeutral;
          if AClasses.IndexOf(UnicodeData[i].BidiClass)<>-1 then
          begin
            curCode := UnicodeData[i].Code;
            if (curCode >= AMinCode) and (curCode <= AMaxCode) then
            begin
              if (codes[newBidi].Count > 0) and (codes[newBidi].Last+1 <> curCode) then
                inc(gaps[newBidi]);
              codes[newBidi].Add(curCode);
              if gaps[newBidi] > MaxGapsPerClass then
                FlushCase(newBidi);
            end;
          end;
        end;
        for newBidi := low(TUnicodeBidiClass) to high(TUnicodeBidiClass) do
        begin
          FlushCase(newBidi);
          codes[newBidi].Free;
        end;
      end;

    var c: TStringList;

      procedure Include(AMinCode,AMaxCode: integer);
      begin
        write('Classes from ',IntToHex(AMinCode,2),' to ',IntToHex(AMaxCode,2),':');
        Writeln(tOut,Indent+'case u of');
        c.CommaText := 'BN';
        IncludeClasses(c, AMinCode,AMaxCode);
        c.CommaText := 'S';
        IncludeClasses(c, AMinCode,AMaxCode);
        c.CommaText := 'B';
        IncludeClasses(c, AMinCode,AMaxCode);
        c.CommaText := 'WS';
        IncludeClasses(c, AMinCode,AMaxCode);
        c.CommaText := 'L,CL,R,AL';
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
        c.CommaText := 'ON,MN';
        IncludeClasses(c, AMinCode,AMaxCode);
        writeln(tout,Indent+'else result := ubcUnknown;');
        writeln(tout,Indent+'end;');
        writeln;
      end;

    begin
      Writeln(tOut,'function GetUnicodeBidiClassEx(u: LongWord): TUnicodeBidiClass;');
      Writeln(tOut,'begin');
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
    end;

    procedure ParseBidiBrackets;
    var elem: TStringList;
      line: string;
      tIn: TextFile;
    begin
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
      u: LongInt;
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

    procedure ParseCombiningClasses;
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

    var
      i: Integer;
      infos: TStringList;
      u: LongInt;
      c: byte;
      s: String;

      procedure FlushLine;
      begin
        writeln(tOut, s);
        s := '   ';
      end;

    begin
      infos := TStringList.Create;
      for i := 0 to UnicodeCount-1 do
      begin
        u := UnicodeData[i].Code;
        if (UnicodeData[i].BidiClass = 'NSM') or
           (UnicodeData[i].Category = 'Mc') then
        begin
          c := UnicodeData[i].CombiningClass;
          if (c = 0) and (UnicodeData[i].Category = 'Mc') then
          begin
            if pos(IntToHex(u,4)+',', CombineLeftOnly) <> 0 then c := 208
            else if pos(IntToHex(u,4)+',', CombineLeftAndRight) <> 0 then c := 0
            else c := 210;
          end;
          infos.Add('(u:$'+IntToHex(u,2)+'; c:'+IntToStr(c)+')');
        end;
      end;
      writeln(tOut,'type');
      writeln(tOut,'  TUnicodeCombiningInfo = record');
      writeln(tOut,'    u: LongWord;');
      writeln(tOut,'    c: Byte;');
      writeln(tOut,'  end;');
      writeln(tOut,'const');
      writeln(tOut,'  UnicodeCombiningInfos: array[0..',infos.count-1,'] of TUnicodeCombiningInfo =');
      s := '  (';
      for i := 0 to infos.Count-1 do
      begin
        if length(s) + length(infos[i]) + 2 > 80 then FlushLine;
        AppendStr(s, ' ' + infos[i]);
        if i < infos.Count-1 then AppendStr(s, ',');
      end;
      if s <> '   ' then FlushLine;
      writeln(tOut,'  );');
      writeln(tOut);
      infos.Free;
    end;

  begin
    AssignFile(tOut, 'generatedunicode.inc');
    Rewrite(tOut);
    writeln(tOut,'{ This file is generated by dev/parseunicode/parseunicodeclasses program }');
    Writeln(tOut);
    ParseBidiClasses;
    ParseBidiBrackets;
    ParseArabicLigature;
    ParseCombiningClasses;
    CloseFile(tOut);
  end;

  function ListCompareBinary(List: TStringList; Index1, Index2: Integer): Integer;
  begin
    result := CompareStr(List[Index1], List[Index2]);
  end;

  procedure ParseUTF8Decomposition;
  type TDecompositionKind = string;
  const dMultichar = 'arNone';
        dInitial = 'arInitial';
        dMedial = 'arMedial';
        dFinal = 'arFinal';
        dIsolated = 'arIsolated';
  const UTF8_ARABIC_ALEPH = 'ا';
        UTF8_ARABIC_ALEPH_HAMZA_BELOW = 'إ';
        UTF8_ARABIC_ALEPH_HAMZA_ABOVE = 'أ';
        UTF8_ARABIC_ALEPH_MADDA_ABOVE = 'آ';
        UTF8_ARABIC_LAM = 'ل';
  var tOut: TextFile;
    decomposed, kind, decomposedUTF8, s: string;
    decomposedFirstChar: LongInt;
    mergedU,nextU, fallbackU: LongInt;
    posClose, posSpace: SizeInt;
    hasNSM, isLa: Boolean;
    correspList: TStringList;
    kerningFallback: TStringList;
    i, decomposedLen, j: Integer;
    typedKind: TDecompositionKind;
    hasMarkLeft, hasMarkRight: boolean;

    function RemoveUptoTab(AText: string): string;
    var
      idxTab: SizeInt;
    begin
      idxTab := pos(#9, AText);
      result := copy(AText, idxTab+1, length(AText)-idxTab);
    end;

  begin
    writeln('Parsing decomposition data...');
    correspList := TStringList.Create;
    kerningFallback := TStringList.Create;
    for j := 0 to UnicodeCount-1 do
    begin
      mergedU := UnicodeData[j].Code;
      if UnicodeData[j].BidiClass = 'NSM' then continue;
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
      decomposedFirstChar:= 0;
      hasMarkLeft := false;
      hasMarkRight := false;
      hasNSM := false;
      while decomposed <> '' do
      begin
        posSpace := pos(' ',decomposed);
        if posSpace = 0 then posSpace := length(decomposed)+1;
        nextU := strToInt('$'+copy(decomposed,1,posSpace-1));
        if GetUnicodeBidiClass(nextU) = 'NSM' then hasNSM := true;
        case GetUnicodeCombiningClass(nextU) of
        200,208,212,218,224,228: hasMarkLeft := true;
        204,210,216,222,226,232: hasMarkRight := true;
        end;
        if decomposedLen = 0 then decomposedFirstChar:= nextU;
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
      if (typedKind = dMultichar) and (decomposedUTF8 <> '') and not hasMarkLeft and not hasMarkRight and
         ((decomposedUTF8[1] in ['A'..'Z']) or (copy(decomposedUTF8,1,length('Æ')) = 'Æ') or
         (copy(decomposedUTF8,1,length('Ç')) = 'Ç') or
         (copy(decomposedUTF8,1,length('Г')) = 'Г') or
         (copy(decomposedUTF8,1,length('Ѵ')) = 'Ѵ') or
         (copy(decomposedUTF8,1,length('Ω')) = 'Ω') or
         (copy(decomposedUTF8,1,length('Ө')) = 'Ө')) then
      begin
        fallbackU := decomposedFirstChar;
        if fallbackU <> 32 then
          kerningFallback.Add('(u:$' + inttohex(mergedU,2)+'; fb:$'+ inttohex(fallbackU,2)+')');
      end;
    end;

    AssignFile(tOut, 'generatedutf8.inc');
    Rewrite(tOut);
    writeln(tOut,'{ This file is generated by dev/parseunicode/parseunicodeclasses program }');
    writeln(tOut, 'type');
    writeln(tOut, '  TArabicJoin = (arNone, arInitial, arMedial, arFinal, arIsolated);');
    writeln(tOut, '  TUTF8Decomposition = record');
    writeln(tOut, '    de, re: string; //decomposed, recomposed UTF8');
    writeln(tOut, '    join: TArabicJoin;');
    writeln(tOut, '  end;');
    writeln(tOut, 'const');
    writeln(tOut, '  UTF8Decomposition : array[0..', correspList.Count-1, '] of TUTF8Decomposition = (');
    correspList.CustomSort(@ListCompareBinary);
    for i := 0 to correspList.Count-1 do
      if i <> correspList.Count-1 then
        writeln(tOut, '  ', RemoveUptoTab(correspList[i]), ',')
      else
        writeln(tOut, '  ', RemoveUptoTab(correspList[i]));
    correspList.Free;
    writeln(tOut, '  );');
    writeln(tout);
    CloseFile(tOut);

    AssignFile(tOut, 'generatedkerningfallback.inc');
    Rewrite(tOut);
    writeln(tOut,'{ This file is generated by dev/parseunicode/parseunicodeclasses program }');
    writeln(tOut, 'type');
    writeln(tOut, '  TKerningFallbackInfo = record');
    writeln(tOut, '    u: integer;      //composed charcode');
    writeln(tOut, '    fb: integer;     //fallback code');
    writeln(tOut, '  end;');
    writeln(tOut, 'const');
    writeln(tOut, '  KerningFallbackInfo : array[0..', kerningFallback.Count-1, '] of TKerningFallbackInfo = (');
    s := '';
    for i := 0 to kerningFallback.Count-1 do
    begin
      if i <> kerningFallback.Count-1 then
        AppendStr(s, kerningFallback[i] + ', ')
      else
        AppendStr(s, kerningFallback[i]);
      if length(s) > 70 then
      begin
        writeln(tOut, '  ', s);
        s := '';
      end;
    end;
    if s <> '' then
      writeln(tOut, '  ', s);
    writeln(tOut, '  );');
    writeln(tout);
    kerningFallback.Free;
    CloseFile(tOut);
  end;

begin
  LoadUnicodeData;
  GenerateUnicodeFunctions;
  ParseUTF8Decomposition;
  writeln('Done.');
end.

