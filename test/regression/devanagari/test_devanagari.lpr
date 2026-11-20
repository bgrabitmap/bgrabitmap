program TestDevanagari;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  SysUtils, BGRAUTF8, BGRATypewriter, BGRAUnicode;

type
  { Typewriter that has a glyph for any text and records the glyphs it browses }
  TRecordingTypeWriter = class(TBGRACustomTypeWriter)
  protected
    function GetGlyph(AIdentifier: string): TBGRAGlyph; override;
  public
    Browsed: array of string;
    procedure OnGlyph(ATextUTF8: string; AGlyph: TBGRAGlyph;
      AFlags: TBrowseGlyphCallbackFlags; AData: Pointer; out AContinue: boolean);
  end;

function TRecordingTypeWriter.GetGlyph(AIdentifier: string): TBGRAGlyph;
begin
  result := inherited GetGlyph(AIdentifier);
  if result = nil then
  begin
    result := TBGRAGlyph.Create(AIdentifier);
    result.Width := 1;
    result.Height := 1;
    SetGlyph(AIdentifier, result);
  end;
end;

procedure TRecordingTypeWriter.OnGlyph(ATextUTF8: string; AGlyph: TBGRAGlyph;
  AFlags: TBrowseGlyphCallbackFlags; AData: Pointer; out AContinue: boolean);
begin
  SetLength(Browsed, Length(Browsed)+1);
  Browsed[High(Browsed)] := AGlyph.Identifier;
  AContinue := true;
end;

procedure Fail(const AMsg: string);
begin
  raise Exception.Create(AMsg);
end;

const
  KA = #$E0#$A4#$95;   // U+0915
  CA = #$E0#$A4#$9A;   // U+091A
  MA = #$E0#$A4#$AE;   // U+092E
  TA = #$E0#$A4#$A4;   // U+0924
  RA = #$E0#$A4#$B0;   // U+0930
  AA = #$E0#$A4#$BE;   // U+093E vowel sign aa
  V = #$E0#$A5#$8D;        // U+094D virama

procedure CheckTypeWriter(const AText: string; const AExpected: array of string;
  const AContext: string);
var
  tw: TRecordingTypeWriter;
  i: integer;
begin
  tw := TRecordingTypeWriter.Create;
  try
    tw.BrowseGlyphs(AText, @tw.OnGlyph, nil, false);
    if Length(tw.Browsed) <> Length(AExpected) then
      Fail(AContext + ': typewriter expected ' + IntToStr(Length(AExpected)) +
        ' glyphs but got ' + IntToStr(Length(tw.Browsed)));
    for i := 0 to High(AExpected) do
      if tw.Browsed[i] <> AExpected[i] then
        Fail(AContext + ': typewriter glyph ' + IntToStr(i) + ' differs');
  finally
    tw.Free;
  end;
end;

procedure CheckCursor(const AText: string; const AExpected: array of string;
  const AContext: string);
var
  cursor: TGlyphCursorUtf8;
  glyph: TGlyphUtf8;
  i, byteTotal: integer;
begin
  cursor := TGlyphCursorUtf8.New(AText, fbmAuto);
  i := 0;
  byteTotal := 0;
  while not cursor.EndOfString do
  begin
    glyph := cursor.GetNextGlyph;
    if i > High(AExpected) then Fail(AContext + ': cursor returns too many glyphs');
    if glyph.GlyphUtf8 <> AExpected[i] then
      Fail(AContext + ': cursor glyph ' + IntToStr(i) + ' differs');
    inc(byteTotal, glyph.ByteSize);
    inc(i);
  end;
  if i <> Length(AExpected) then
    Fail(AContext + ': cursor expected ' + IntToStr(Length(AExpected)) + ' glyphs but got ' + IntToStr(i));
  if byteTotal <> Length(AText) then
    Fail(AContext + ': cursor byte sizes do not add up to the text length');
end;

procedure CheckBoth(const AText: string; const AExpected: array of string;
  const AContext: string);
begin
  CheckTypeWriter(AText, AExpected, AContext);
  CheckCursor(AText, AExpected, AContext);
end;

begin
  // ma + virama + ca form one conjunct
  CheckBoth(MA + V + CA, [MA + V + CA], 'two consonants');
  // conjunct followed by a vowel sign: the vowel sign stays a separate unit
  CheckBoth(MA + V + CA + AA, [MA + V + CA + AA], 'conjunct with its vowel sign (kept in the same unit by the bidi analysis)');
  // up to four consonants combine, the fifth starts a new group
  CheckBoth(KA + V + TA + V + RA + V + MA + V + CA,
    [KA + V + TA + V + RA + V + MA + V, CA], 'five consonants (maximum is four)');
  CheckBoth(KA + V + TA + V + RA + V + MA,
    [KA + V + TA + V + RA + V + MA], 'four consonants');
  // without virama consonants stay separate
  CheckBoth(MA + CA, [MA, CA], 'no virama');
  // a trailing virama has nothing to combine with
  CheckBoth(MA + V, [MA + V], 'trailing virama');
  // virama followed by something that is not a consonant
  CheckBoth(MA + V + ' ' + CA, [MA + V, ' ', CA], 'virama then space');
  // Latin ligatures are not affected
  CheckTypeWriter('fi', ['fi'], 'latin ligature');

  WriteLn('PASS: Devanagari conjuncts are grouped into one glyph');
end.
