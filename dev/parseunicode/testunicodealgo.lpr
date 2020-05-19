// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
program testunicodealgo;

uses BGRAUTF8, Classes, sysUtils;

var
  tIn,tOut: TextFile;
  testDef: string;
  testElem, unicodeChars: TStringList;
  testText: string;
  posHash, i: integer;
  a: TBidiUTF8Array;
  levelStr, orderStr: string;
  failCount, successCount: integer;
  o: TUnicodeDisplayOrder;

begin
  assignfile(tIn, 'BidiCharacterTest.txt');
  reset(tIn);

  assignfile(tOut, 'BidiCharacterTest.result');
  rewrite(tOut);
  testElem := TStringList.Create;
  testElem.Delimiter := ';';
  testElem.StrictDelimiter:= true;
  unicodeChars := TStringList.Create;
  unicodeChars.Delimiter := ' ';
  unicodeChars.StrictDelimiter := true;
  while not eof(tIn) do
  begin
    readln(tIn, testDef);
    posHash := pos('#',testDef);
    if posHash <> 0 then testDef := copy(testDef,1,posHash-1);
    testElem.DelimitedText := testDef;
    if testElem.Count = 5 then
    begin
      unicodeChars.DelimitedText := testElem[0];
      testText := '';
      for i := 0 to unicodeChars.Count-1 do
        testText += UnicodeCharToUTF8(StrToInt('$'+unicodeChars[i]));

      if testText = '' then continue;

      //writeln(testDef);
      case testElem[1] of
      '0': a := AnalyzeBidiUTF8(testText,false);
      '1': a := AnalyzeBidiUTF8(testText,true);
      '2': a := AnalyzeBidiUTF8(testText);
      end;
      levelStr := '';
      for i := 0 to high(a) do
      begin
        if i > 0 then levelStr += ' ';
        if a[i].BidiInfo.IsRemoved then
          levelStr += 'x'
        else
          levelStr += inttostr(a[i].BidiInfo.BidiLevel);
      end;

      o := GetUTF8DisplayOrder(a);
      orderStr := '';
      for i := 0 to high(o) do
      begin
        if i > 0 then orderStr += ' ';
        orderStr += inttostr(o[i]);
      end;

      if (levelStr <> testElem[3]) or (orderStr <> testElem[4]) then
      begin
        if levelStr = testElem[3] then
          write(tOut,'Success;') else write(tOut,'Fail;');

        writeln(tOut, testDef, ';', levelStr, ';', orderStr+';', testText);
        inc(failCount);
      end else
        inc(successCount);
    end;
  end;
  writeln(tOut, failCount, ' fail');
  writeln(tOut, successCount, ' success');
  closefile(tOut);

  closefile(tIn);
end.

