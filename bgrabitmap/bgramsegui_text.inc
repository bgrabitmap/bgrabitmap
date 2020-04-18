{copied from LazUTF8
 ------------------------------------------------------------------------------}
type
  TConvertResult = (trNoError, trNullSrc, trNullDest, trDestExhausted,
    trInvalidChar, trUnfinishedChar);

  TConvertOption = (toInvalidCharError, toInvalidCharToSymbol,
    toUnfinishedCharError, toUnfinishedCharToSymbol);
  TConvertOptions = set of TConvertOption;   

{ ------------------------------------------------------------------------------
  Name:    ConvertUTF8ToUTF16
  Params:  Dest                - Pointer to destination string
           DestWideCharCount   - Wide char count allocated in destination string
           Src                 - Pointer to source string
           SrcCharCount        - Char count allocated in source string
           Options             - Conversion options, if none is set, both
             invalid and unfinished source chars are skipped

             toInvalidCharError       - Stop on invalid source char and report
                                      error
             toInvalidCharToSymbol    - Replace invalid source chars with '?'
             toUnfinishedCharError    - Stop on unfinished source char and
                                      report error
             toUnfinishedCharToSymbol - Replace unfinished source char with '?'

           ActualWideCharCount - Actual wide char count converted from source
                               string to destination string
  Returns:
    trNoError        - The string was successfully converted without
                     any error
    trNullSrc        - Pointer to source string is nil
    trNullDest       - Pointer to destination string is nil
    trDestExhausted  - Destination buffer size is not big enough to hold
                     converted string
    trInvalidChar    - Invalid source char has occured
    trUnfinishedChar - Unfinished source char has occured

  Converts the specified UTF-8 encoded string to UTF-16 encoded (system endian)
 ------------------------------------------------------------------------------}
function ConvertUTF8ToUTF16(Dest: PWideChar; DestWideCharCount: SizeUInt;
  Src: PChar; SrcCharCount: SizeUInt; Options: TConvertOptions;
  out ActualWideCharCount: SizeUInt): TConvertResult;
var
  DestI, SrcI: SizeUInt;
  B1, B2, B3, B4: Byte;
  W: Word;
  C: Cardinal;

  function UnfinishedCharError: Boolean;
  begin
    if toUnfinishedCharToSymbol in Options then
    begin
      Dest[DestI] := System.WideChar('?');
      Inc(DestI);
      Result := False;
    end
    else
      if toUnfinishedCharError in Options then
      begin
        ConvertUTF8ToUTF16 := trUnfinishedChar;
        Result := True;
      end
      else Result := False;
  end;

  function InvalidCharError(Count: SizeUInt): Boolean; inline;
  begin
    if not (toInvalidCharError in Options) then
    begin
      if toInvalidCharToSymbol in Options then
      begin
        Dest[DestI] := System.WideChar('?');
        Inc(DestI);
      end;

      Dec(SrcI, Count);

      // skip trailing UTF-8 char bytes
      while (Count > 0) do
      begin
        if (Byte(Src[SrcI]) and %11000000) <> %10000000 then Break;
        Inc(SrcI);
        Dec(Count);
      end;

      Result := False;
    end
    else
      if toInvalidCharError in Options then
      begin
        ConvertUTF8ToUTF16 := trUnfinishedChar;
        Result := True;
      end;
  end;

begin
  ActualWideCharCount := 0;

  if not Assigned(Src) then
  begin
    Result := trNullSrc;
    Exit;
  end;

  if not Assigned(Dest) then
  begin
    Result := trNullDest;
    Exit;
  end;
  SrcI := 0;
  DestI := 0;

  while (DestI < DestWideCharCount) and (SrcI < SrcCharCount) do
  begin
    B1 := Byte(Src[SrcI]);
    Inc(SrcI);

    if B1 < 128 then // single byte UTF-8 char
    begin
      Dest[DestI] := System.WideChar(B1);
      Inc(DestI);
    end
    else
    begin
      if SrcI >= SrcCharCount then
        if UnfinishedCharError then Exit(trInvalidChar)
        else Break;

      B2 := Byte(Src[SrcI]);
      Inc(SrcI);

      if (B1 and %11100000) = %11000000 then // double byte UTF-8 char
      begin
        if (B2 and %11000000) = %10000000 then
        begin
          Dest[DestI] := System.WideChar(((B1 and %00011111) shl 6) or (B2 and %00111111));
          Inc(DestI);
        end
        else // invalid character, assume single byte UTF-8 char
          if InvalidCharError(1) then Exit(trInvalidChar);
      end
      else
      begin
        if SrcI >= SrcCharCount then
          if UnfinishedCharError then Exit(trInvalidChar)
          else Break;

        B3 := Byte(Src[SrcI]);
        Inc(SrcI);

        if (B1 and %11110000) = %11100000 then // triple byte UTF-8 char
        begin
          if ((B2 and %11000000) = %10000000) and ((B3 and %11000000) = %10000000) then
          begin
            W := ((B1 and %00011111) shl 12) or ((B2 and %00111111) shl 6) or (B3 and %00111111);
            if (W < $D800) or (W > $DFFF) then // to single wide char UTF-16 char
            begin
              Dest[DestI] := System.WideChar(W);
              Inc(DestI);
            end
            else // invalid UTF-16 character, assume double byte UTF-8 char
              if InvalidCharError(2) then Exit(trInvalidChar);
          end
          else // invalid character, assume double byte UTF-8 char
            if InvalidCharError(2) then Exit(trInvalidChar);
        end
        else
        begin
          if SrcI >= SrcCharCount then
            if UnfinishedCharError then Exit(trInvalidChar)
            else Break;

          B4 := Byte(Src[SrcI]);
          Inc(SrcI);

          if ((B1 and %11111000) = %11110000) and ((B2 and %11000000) = %10000000)
            and ((B3 and %11000000) = %10000000) and ((B4 and %11000000) = %10000000) then
          begin // 4 byte UTF-8 char
            C := ((B1 and %00011111) shl 18) or ((B2 and %00111111) shl 12)
              or ((B3 and %00111111) shl 6)  or (B4 and %00111111);
            // to double wide char UTF-16 char
            Dest[DestI] := System.WideChar($D800 or ((C - $10000) shr 10));
            Inc(DestI);
            if DestI >= DestWideCharCount then Break;
            Dest[DestI] := System.WideChar($DC00 or ((C - $10000) and %0000001111111111));
            Inc(DestI);
          end
          else // invalid character, assume triple byte UTF-8 char
            if InvalidCharError(3) then Exit(trInvalidChar);
        end;
      end;
    end;
  end;

  if DestI >= DestWideCharCount then
  begin
    DestI := DestWideCharCount - 1;
    Result := trDestExhausted;
  end
  else
    Result := trNoError;

  Dest[DestI] := #0;
  ActualWideCharCount := DestI + 1;
end;

function UTF8ToUTF16(const P: PChar; ByteCnt: SizeUInt): UnicodeString;
var
  L: SizeUInt;
begin
  if ByteCnt=0 then
    exit('');
  SetLength(Result, ByteCnt);
  // wide chars of UTF-16 <= bytes of UTF-8 string
  if ConvertUTF8ToUTF16(PWideChar(Result), Length(Result) + 1, P, ByteCnt,
    [toInvalidCharToSymbol], L) = trNoError
  then SetLength(Result, L - 1)
  else Result := '';
end;   

{------------------------------------------------------------------------------
  Name:    UTF8ToUTF16
  Params:  S - Source UTF-8 string
  Returns: UTF-16 encoded string

  Converts the specified UTF-8 encoded string to UTF-16 encoded (system endian)
  Avoid copying the result string since on windows a widestring requires a full
  copy
 ------------------------------------------------------------------------------}
function UTF8ToUTF16(const S: AnsiString): UnicodeString;
begin
  Result:=UTF8ToUTF16(PChar(S),length(S));
end;

{end of copy from LazUTF8
 ------------------------------------------------------------------------------}
 
procedure BitmapTextOut(ABitmap: TBitmap; ACoord: TPoint; AText: string);
var sUTF16: UnicodeString;
begin 
  sUTF16 := UTF8ToUTF16(AText);
  inc(ACoord.Y, ABitmap.Canvas.Font.GlyphHeight-1);
  ABitmap.Canvas.DrawString(sUTF16, ACoord);
end;

procedure BitmapTextRect(ABitmap: TBitmap; ARect: TRect; ACoord: TPoint; 
  AText: string; const AStyle: TTextStyle);
var sUTF16: UnicodeString;
begin 
  sUTF16 := UTF8ToUTF16(AText);
  case AStyle.Alignment of
  taRightJustify: ACoord.X := ARect.Right - ABitmap.Canvas.GetStringWidth(sUTF16);
  taCenter: ACoord.X := ARect.Left + (ARect.Width - ABitmap.Canvas.GetStringWidth(sUTF16)) div 2;
  end;
  case AStyle.Layout of
  tlBottom: ACoord.Y := ARect.Bottom - ABitmap.Canvas.Font.Height;
  tlCenter: ACoord.Y := ARect.Top + (ARect.Height - ABitmap.Canvas.Font.Height) div 2;
  end;
  inc(ACoord.Y, ABitmap.Canvas.Font.GlyphHeight-1);
  ABitmap.Canvas.DrawString(sUTF16, ACoord);
end;

function BitmapTextExtent(ABitmap: TBitmap; AText: string): TSize;
var sUTF16: UnicodeString;
begin
  sUTF16 := UTF8ToUTF16(AText);
  result := Size(ABitmap.Canvas.GetStringWidth(sUTF16), ABitmap.Canvas.Font.Height);
end;

function BitmapTextFitInfo(ABitmap: TBitmap; AText: string; AMaxWidth: integer): integer;
var 
  bytePos, charLen: integer;  
  sUTF16: UnicodeString; 
begin
  result := 0;
  bytePos := 1;
  while bytePos <= length(AText) do
  begin
    charLen := UTF8CharacterLength(@AText[bytePos]);
    sUTF16 := UTF8ToUTF16(copy(AText, 1, bytePos+charLen-1));
    if ABitmap.Canvas.GetStringWidth(sUTF16) > AMaxWidth then break;
    inc(result);
    inc(bytePos,charLen);
  end;
end;

procedure BitmapFillRect(ABitmap: TBitmap; ARect: TRect; AColor: TColor);
begin
  ABitmap.Canvas.FillRect(ARect, AColor);
end;
