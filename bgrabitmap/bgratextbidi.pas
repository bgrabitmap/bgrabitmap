unit BGRATextBidi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, BGRAUTF8, BGRAUnicode, BGRATransform;

type
  TBidiCaretPos = record
    PartIndex: integer;

    Top, Bottom: TPointF;
    RightToLeft: boolean;

    PreviousTop, PreviousBottom: TPointF;
    PreviousRightToLeft: boolean;
  end;

  { TBidiTextLayout }

  TBidiTextLayout = class
  private
    FAvailableHeight: single;
    FAvailableWidth: single;
    FTopLeft: TPointF;
    function GetBrokenLineAffineBox(AIndex: integer): TAffineBox;
    function GetBrokenLineCount: integer;
    function GetBrokenLineEndCaret(AIndex: integer): TBidiCaretPos;
    function GetBrokenLineEndIndex(AIndex: integer): integer;
    function GetBrokenLineRightToLeft(AIndex: integer): boolean;
    function GetBrokenLineStartCaret(AIndex: integer): TBidiCaretPos;
    function GetBrokenLineStartIndex(AIndex: integer): integer;
    function GetPartAffineBox(AIndex: integer): TAffineBox;
    function GetPartBrokenLineIndex(AIndex: integer): integer;
    function GetPartCount: integer;
    function GetPartEndIndex(AIndex: integer): integer;
    function GetPartRightToLeft(AIndex: integer): boolean;
    function GetPartStartIndex(AIndex: integer): integer;
    function GetUnicodeChar(APosition0: integer): cardinal;
    function GetUTF8Char(APosition0: integer): string4;
    procedure SetAvailableHeight(AValue: single);
    procedure SetAvailableWidth(AValue: single);
    procedure SetTopLeft(AValue: TPointF);
  protected
    FText: string;
    FBidi: TBidiUTF8Array;
    FCharCount: integer;
    FRenderer: TBGRACustomFontRenderer;
    FUnbrokenLineStart: array of integer;
    FUnbrokenLineCount: integer;
    FPart: array of record
             startIndex, endIndex: integer;
             bidiLevel: byte;
             affineBox: TAffineBox;
             posCorrection: TPointF;
             sUTF8: string;
             brokenLineIndex: integer;
           end;
    FPartCount: integer;
    FBrokenLine: array of record
                   startIndex, endIndex: integer;
                   bidiLevel: byte;
                   affineBox: TAffineBox;
                 end;
    FBrokenLineCount: integer;
    FStartCaret: TBidiCaretPos;
    FLayoutComputed: boolean;

    function TextSizeBidiOverride(sUTF8: string; ARightToLeft: boolean): TPointF;
    function TextSizeBidiOverrideSplit(AStartIndex, AEndIndex: integer; ARightToLeft: boolean; ASplitIndex: integer): TPointF;
    function TextFitInfoBidiOverride(sUTF8: string; AWidth: single; ARightToLeft: boolean): integer;
    function GetFontFullHeight: single;
    function GetFontBaseline: single;
    function GetFontOrientation: single;
    procedure TextOutBidiOverride(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; ARightToLeft: boolean);

    procedure AddPart(AStartIndex, AEndIndex: integer; ABidiLevel: byte; AAffineBox: TAffineBox; APosCorrection: TPointF; ASUTF8: string; ABrokenLineIndex: integer);
    function GetPartStartCaret(APartIndex: integer): TBidiCaretPos;
    function GetPartEndCaret(APartIndex: integer): TBidiCaretPos;

    procedure AnalyzeLineStart;
    function GetSameLevelString(startIndex,endIndex: integer): string;
    procedure LevelSize(AMaxWidth: single; startIndex, endIndex: integer; bidiLevel: byte; out ASplitIndex: integer; out AWidth, AHeight: single);
    procedure ComputeLevelLayout(AMatrix: TAffineMatrix; APos: TPointF; startIndex,
      endIndex: integer; bidiLevel: byte; fullHeight, baseLine: single; brokenLineIndex: integer;
      out AWidth: single);
    procedure Init;
    procedure ComputeLayout;
    procedure NeedLayout;
  public
    constructor Create(AFontRenderer: TBGRACustomFontRenderer; sUTF8: string);
    constructor Create(AFontRenderer: TBGRACustomFontRenderer; sUTF8: string; ARightToLeft: boolean);
    procedure SetLayout(ARect: TRectF);
    procedure InvalidateLayout;

    procedure DrawText(ADest: TBGRACustomBitmap);
    procedure DrawCaret(ADest: TBGRACustomBitmap; ACharIndex: integer; AMainColor, ASecondaryColor: TBGRAPixel);
    procedure DrawSelection(ADest: TBGRACustomBitmap; AStartIndex, AEndIndex: integer; AColor: TBGRAPixel);

    function GetCaret(ACharIndex: integer): TBidiCaretPos;
    function GetCharIndexAt(APosition: TPointF): integer;
    function GetTextEnveloppe(AStartIndex, AEndIndex: integer): ArrayOfTPointF;

    property CharCount: integer read FCharCount;
    property UTF8Char[APosition0: integer]: string4 read GetUTF8Char;
    property UnicodeChar[APosition0: integer]: cardinal read GetUnicodeChar;

    property BrokenLineCount: integer read GetBrokenLineCount;
    property BrokenLineStartIndex[AIndex: integer]: integer read GetBrokenLineStartIndex;
    property BrokenLineEndIndex[AIndex: integer]: integer read GetBrokenLineEndIndex;
    property BrokenLineAffineBox[AIndex: integer]: TAffineBox read GetBrokenLineAffineBox;
    property BrokenLineRightToLeft[AIndex: integer]: boolean read GetBrokenLineRightToLeft;
    property BrokenLineStartCaret[AIndex: integer]: TBidiCaretPos read GetBrokenLineStartCaret;
    property BrokenLineEndCaret[AIndex: integer]: TBidiCaretPos read GetBrokenLineEndCaret;

    property PartCount: integer read GetPartCount;
    property PartStartIndex[AIndex: integer]: integer read GetPartStartIndex;
    property PartEndIndex[AIndex: integer]: integer read GetPartEndIndex;
    property PartBrokenLineIndex[AIndex: integer]: integer read GetPartBrokenLineIndex;
    property PartStartCaret[AIndex: integer]: TBidiCaretPos read GetPartStartCaret;
    property PartEndCaret[AIndex: integer]: TBidiCaretPos read GetPartEndCaret;
    property PartAffineBox[AIndex: integer]: TAffineBox read GetPartAffineBox;
    property PartRightToLeft[AIndex: integer]: boolean read GetPartRightToLeft;

    property TopLeft: TPointF read FTopLeft write SetTopLeft;
    property AvailableWidth: single read FAvailableWidth write SetAvailableWidth;
    property AvailableHeight: single read FAvailableHeight write SetAvailableHeight;
  end;

implementation

{ TBidiTextLayout }

function TBidiTextLayout.GetBrokenLineAffineBox(AIndex: integer): TAffineBox;
begin
  NeedLayout;
  if (AIndex < 0) or (AIndex >= FBrokenLineCount) then
    raise ERangeError.Create('Invalid index');
  result := FBrokenLine[AIndex].affineBox;
end;

function TBidiTextLayout.GetBrokenLineCount: integer;
begin
  NeedLayout;
  result := FBrokenLineCount;
end;

function TBidiTextLayout.GetBrokenLineEndCaret(AIndex: integer): TBidiCaretPos;
begin
  NeedLayout;
  with BrokenLineAffineBox[AIndex] do
  begin
    if BrokenLineRightToLeft[AIndex] then
      result.Top := TopLeft
    else
      result.Top := TopRight;
    result.Bottom := result.Top + (BottomLeft-TopLeft);
    result.RightToLeft := odd(FBrokenLine[AIndex].bidiLevel);
    result.PartIndex := -1;
    result.PreviousTop := EmptyPointF;
    result.PreviousBottom := EmptyPointF;
    result.PreviousRightToLeft := result.RightToLeft;
  end;
end;

function TBidiTextLayout.GetBrokenLineEndIndex(AIndex: integer): integer;
begin
  NeedLayout;
  if (AIndex < 0) or (AIndex >= FBrokenLineCount) then
    raise ERangeError.Create('Invalid index');
  result := FBrokenLine[AIndex].endIndex;
end;

function TBidiTextLayout.GetBrokenLineRightToLeft(AIndex: integer): boolean;
begin
  NeedLayout;
  if (AIndex < 0) or (AIndex >= FBrokenLineCount) then
    raise ERangeError.Create('Invalid index');
  result := odd(FBrokenLine[AIndex].bidiLevel);
end;

function TBidiTextLayout.GetBrokenLineStartCaret(AIndex: integer): TBidiCaretPos;
begin
  NeedLayout;
  with BrokenLineAffineBox[AIndex] do
  begin
    if BrokenLineRightToLeft[AIndex] then
      result.Top := TopRight
    else
      result.Top := TopLeft;
    result.Bottom := result.Top + (BottomLeft-TopLeft);
    result.RightToLeft := odd(FBrokenLine[AIndex].bidiLevel);
    result.PartIndex:= -1;
    result.PreviousTop := EmptyPointF;
    result.PreviousBottom := EmptyPointF;
    result.PreviousRightToLeft := result.RightToLeft;
  end;
end;

function TBidiTextLayout.GetBrokenLineStartIndex(AIndex: integer): integer;
begin
  NeedLayout;
  if (AIndex < 0) or (AIndex >= FBrokenLineCount) then
    raise ERangeError.Create('Invalid index');
  result := FBrokenLine[AIndex].startIndex;
end;

function TBidiTextLayout.GetPartAffineBox(AIndex: integer): TAffineBox;
begin
  NeedLayout;
  if (AIndex < 0) or (AIndex >= FPartCount) then
    raise ERangeError.Create('Invalid index');
  result := FPart[AIndex].affineBox;
end;

function TBidiTextLayout.GetPartBrokenLineIndex(AIndex: integer): integer;
begin
  NeedLayout;
  if (AIndex < 0) or (AIndex >= FPartCount) then
    raise ERangeError.Create('Invalid index');
  result := FPart[AIndex].brokenLineIndex;
end;

function TBidiTextLayout.GetPartCount: integer;
begin
  NeedLayout;
  result := FPartCount;
end;

function TBidiTextLayout.GetPartEndIndex(AIndex: integer): integer;
begin
  NeedLayout;
  if (AIndex < 0) or (AIndex >= FPartCount) then
    raise ERangeError.Create('Invalid index');
  result := FPart[AIndex].endIndex;
end;

function TBidiTextLayout.GetPartRightToLeft(AIndex: integer): boolean;
begin
  NeedLayout;
  if (AIndex < 0) or (AIndex >= FPartCount) then
    raise ERangeError.Create('Invalid index');
  result := odd(FPart[AIndex].bidiLevel);
end;

function TBidiTextLayout.GetPartStartIndex(AIndex: integer): integer;
begin
  NeedLayout;
  if (AIndex < 0) or (AIndex >= FPartCount) then
    raise ERangeError.Create('Invalid index');
  result := FPart[AIndex].startIndex;
end;

function TBidiTextLayout.GetUnicodeChar(APosition0: integer): cardinal;
var p : PChar;
  charLen: Integer;
begin
  if (APosition0 < 0) or (APosition0 >= CharCount) then
    raise ERangeError.Create('Invalid position');
  p := @FText[FBidi[APosition0].Offset+1];
  charLen := UTF8CharacterLength(p);
  result := UTF8CodepointToUnicode(p, charLen);
end;

function TBidiTextLayout.GetUTF8Char(APosition0: integer): string4;
begin
  if (APosition0 < 0) or (APosition0 >= CharCount) then
    raise ERangeError.Create('Invalid position');

  result := copy(FText, FBidi[APosition0].Offset+1, FBidi[APosition0+1].Offset-FBidi[APosition0].Offset);
end;

procedure TBidiTextLayout.SetAvailableHeight(AValue: single);
begin
  if FAvailableHeight=AValue then Exit;
  FAvailableHeight:=AValue;
  FLayoutComputed:= false;
end;

procedure TBidiTextLayout.SetAvailableWidth(AValue: single);
begin
  if FAvailableWidth=AValue then Exit;
  FAvailableWidth:=AValue;
  FLayoutComputed:= false;
end;

procedure TBidiTextLayout.SetTopLeft(AValue: TPointF);
begin
  if FTopLeft=AValue then Exit;
  FTopLeft:=AValue;
  FLayoutComputed:= false;
end;

function TBidiTextLayout.TextSizeBidiOverride(sUTF8: string;
  ARightToLeft: boolean): TPointF;
var
  tabPos: integer;
  beforeTab, afterTab: string;
begin
  tabPos := pos(#9, sUTF8);
  if tabPos <> 0 then
  begin
    beforeTab := copy(sUTF8, 1, tabPos-1);
    afterTab := copy(sUTF8, tabPos+1, length(sUTF8)-tabPos);

  end else
  begin
    if ARightToLeft then
      sUTF8 := UnicodeCharToUTF8(UNICODE_RIGHT_TO_LEFT_OVERRIDE)+ sUTF8
    else
      sUTF8 := UnicodeCharToUTF8(UNICODE_LEFT_TO_RIGHT_OVERRIDE)+ sUTF8;

    with FRenderer.TextSizeAngle(CleanTextOutString(sUTF8), FRenderer.FontOrientation) do
      result := PointF(Width, Height);
  end;
end;

function TBidiTextLayout.TextSizeBidiOverrideSplit(AStartIndex, AEndIndex: integer;
  ARightToLeft: boolean; ASplitIndex: integer): TPointF;
var nextIndex, prevIndex: integer;
  s: String;
  extraS: string4;
  extraW, combW: Single;
  charClass: TUnicodeBidiClass;
begin
  s := copy(FText, FBidi[AStartIndex].Offset+1, FBidi[ASplitIndex].Offset-FBidi[AStartIndex].Offset);
  result := TextSizeBidiOverride(s, ARightToLeft);

  nextIndex := ASplitIndex;
  //check if there might be a ligature
  if (nextIndex < AEndIndex) and (GetUnicodeBidiClass(GetUnicodeChar(nextIndex)) in [ubcRightToLeft,ubcArabicLetter,ubcLeftToRight,ubcArabicNumber,ubcEuropeanNumber]) then
  begin
    inc(nextIndex);
    //find previous letter
    prevIndex := ASplitIndex-1;
    while (prevIndex > AStartIndex) and (GetUnicodeBidiClass(GetUnicodeChar(prevIndex)) = ubcNonSpacingMark) do dec(prevIndex);
    charClass := GetUnicodeBidiClass(GetUnicodeChar(prevIndex));
    //arabic ligatures are asymmetric in size so use the tatweel to measure the actual size
    if charClass = ubcArabicLetter then
    begin
      //measure tatweel size
      extraS := UnicodeCharToUTF8(UNICODE_ARABIC_TATWEEL);
      extraW := TextSizeBidiOverride(extraS, ARightToLeft).x;
      combW := TextSizeBidiOverride(s+extraS, ARightToLeft).x;
      result.x := combW - extraW;  //subtract the size of the tatweel (which itself is not included in the ligature)
    end else
    // otherwise, assume that the ligature is symmetric so subtract half of the ligature size
    begin
      //measure the next char on its own
      while (nextIndex < AEndIndex) and (GetUnicodeBidiClass(GetUnicodeChar(nextIndex)) = ubcNonSpacingMark) do inc(nextIndex);
      extraS := copy(FText, FBidi[ASplitIndex].Offset+1, FBidi[nextIndex].Offset-FBidi[ASplitIndex].Offset);
      extraW := TextSizeBidiOverride(extraS, ARightToLeft).x;

      combW := TextSizeBidiOverride(s+extraS, ARightToLeft).x;
      if combW < result.x then result.x := combW
      else result.x -= (result.x+extraW - combW) * 0.5;
    end;
  end;
end;

function TBidiTextLayout.TextFitInfoBidiOverride(sUTF8: string; AWidth: single;
  ARightToLeft: boolean): integer;
begin
  if ARightToLeft then
    sUTF8 := UnicodeCharToUTF8(UNICODE_RIGHT_TO_LEFT_OVERRIDE)+ sUTF8
  else
    sUTF8 := UnicodeCharToUTF8(UNICODE_LEFT_TO_RIGHT_OVERRIDE)+ sUTF8;

  result := FRenderer.TextFitInfo(sUTF8, round(AWidth))-1;
end;

function TBidiTextLayout.GetFontFullHeight: single;
begin
  result := FRenderer.TextSizeAngle('Hg', FRenderer.FontOrientation).Height;
end;

function TBidiTextLayout.GetFontBaseline: single;
begin
  result := FRenderer.GetFontPixelMetric.Baseline;
end;

function TBidiTextLayout.GetFontOrientation: single;
begin
  result := FRenderer.FontOrientation*0.1;
end;

procedure TBidiTextLayout.TextOutBidiOverride(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string;  ARightToLeft: boolean);
begin
  if ARightToLeft then
    sUTF8 := UnicodeCharToUTF8(UNICODE_RIGHT_TO_LEFT_OVERRIDE)+ CleanTextOutString(sUTF8)
  else
    sUTF8 := UnicodeCharToUTF8(UNICODE_LEFT_TO_RIGHT_OVERRIDE)+ CleanTextOutString(sUTF8);

  FRenderer.TextOut(ADest, x,y, sUTF8, BGRABlack, taLeftJustify, ARightToLeft);
end;

procedure TBidiTextLayout.AddPart(AStartIndex, AEndIndex: integer;
  ABidiLevel: byte; AAffineBox: TAffineBox; APosCorrection: TPointF;
  ASUTF8: string; ABrokenLineIndex: integer);
begin
  if FPartCount >= length(FPart) then
    setlength(FPart, length(FPart)*2+8);

  with FPart[FPartCount] do
  begin
    startIndex:= AStartIndex;
    endIndex:= AEndIndex;
    bidiLevel := ABidiLevel;
    affineBox := AAffineBox;
    posCorrection := APosCorrection;
    sUTF8:= ASUTF8;
    brokenLineIndex:= ABrokenLineIndex;
  end;
  inc(FPartCount)
end;

procedure TBidiTextLayout.AnalyzeLineStart;
var
  lineIndex, i: Integer;
begin
  FUnbrokenLineCount := 1;
  for i := 0 to high(FBidi)-1 do
    if FBidi[i].BidiInfo.IsEndOfLine or FBidi[i].BidiInfo.IsEndOfParagraph then
      FUnbrokenLineCount += 1;

  setlength(FUnbrokenLineStart, FUnbrokenLineCount+1);
  lineIndex := 0;
  FUnbrokenLineStart[lineIndex] := 0;
  inc(lineIndex);
  for i := 0 to high(FBidi)-1 do
  begin
    if FBidi[i].BidiInfo.IsEndOfLine or FBidi[i].BidiInfo.IsEndOfParagraph then
    begin
      FUnbrokenLineStart[lineIndex] := i+1;
      inc(lineIndex);
    end;
  end;
  FUnbrokenLineStart[lineIndex] := length(FBidi);

  setlength(FBidi, length(FBidi)+1);
  FBidi[High(FBidi)].Offset := length(FText);
end;

function TBidiTextLayout.GetSameLevelString(startIndex, endIndex: integer): string;
var i, len, charLen: integer;
begin
  len := 0;
  for i := startIndex to endIndex-1 do
    if not FBidi[i].BidiInfo.IsRemoved then
      inc(len, FBidi[i+1].Offset - FBidi[i].Offset);

  setlength(result, len);
  len := 0;
  for i := startIndex to endIndex-1 do
    if not FBidi[i].BidiInfo.IsRemoved then
    begin
      charLen := FBidi[i+1].Offset - FBidi[i].Offset;
      move(FText[FBidi[i].Offset+1], result[len+1], charLen);
      inc(len, charLen);
    end;
end;

procedure TBidiTextLayout.LevelSize(AMaxWidth: single; startIndex,
  endIndex: integer; bidiLevel: byte; out ASplitIndex: integer; out AWidth,
  AHeight: single);
var
  i: Integer;
  subLevel: byte;
  subStart, subSplit, fitInfo: integer;
  subStr: string;
  w,h: single;
  splitting: boolean;
  subSize: TPointF;
begin
  AWidth := 0;
  AHeight := 0;
  ASplitIndex:= endIndex;

  while (startIndex < endIndex) and FBidi[startIndex].BidiInfo.IsRemoved do inc(startIndex);
  while (startIndex < endIndex) and FBidi[endIndex-1].BidiInfo.IsRemoved do dec(endIndex);
  if endIndex = startIndex then exit;

  i := startIndex;
  while i < endIndex do
  begin
    if not FBidi[i].BidiInfo.IsRemoved then
    begin
      if FBidi[i].BidiInfo.BidiLevel > bidiLevel then
      begin
        subStart := i;
        subLevel := FBidi[i].BidiInfo.BidiLevel;
        inc(i);
        while (i < endIndex) and (FBidi[i].BidiInfo.BidiLevel > bidiLevel) do
        begin
          if FBidi[i].BidiInfo.BidiLevel < subLevel then
            subLevel := FBidi[i].BidiInfo.BidiLevel;
          inc(i);
        end;

        if AMaxWidth <> EmptySingle then
          LevelSize(AMaxWidth - AWidth, subStart, i, subLevel, subSplit, w, h)
        else
          LevelSize(AMaxWidth, subStart, i, subLevel, subSplit, w, h);
        AWidth += w;
        if h > AHeight then AHeight := h;

        if subSplit < i then
        begin
          ASplitIndex := subSplit;
          exit;
        end;
      end else
      begin
        subStart:= i;
        inc(i);
        while (i < endIndex) and (FBidi[i].BidiInfo.BidiLevel = bidiLevel) do inc(i);

        subStr := GetSameLevelString(subStart,i);
        if AMaxWidth <> EmptySingle then
        begin
          fitInfo := TextFitInfoBidiOverride(subStr, AMaxWidth - AWidth, odd(bidiLevel));
          if fitInfo < i-subStart then
          begin
            ASplitIndex:= subStart+fitInfo;
            subStr := GetSameLevelString(subStart,ASplitIndex);
            splitting := true;
          end else
            splitting := false;
        end else
          splitting := false;

        subSize := TextSizeBidiOverride(subStr, odd(bidiLevel));
        w := subSize.x;
        h := subSize.y;
        AWidth += w;
        if h > AHeight then AHeight:= h;

        if splitting then exit;
      end;

    end else
      inc(i);
  end;
end;


constructor TBidiTextLayout.Create(AFontRenderer: TBGRACustomFontRenderer; sUTF8: string);
begin
  Init;
  FRenderer := AFontRenderer;
  FText:= sUTF8;
  FBidi:= AnalyzeBidiUTF8(sUTF8);
  FCharCount := length(FBidi);
  AnalyzeLineStart;
end;

constructor TBidiTextLayout.Create(AFontRenderer: TBGRACustomFontRenderer; sUTF8: string; ARightToLeft: boolean);
begin
  Init;
  FRenderer := AFontRenderer;
  FText:= sUTF8;
  FBidi:= AnalyzeBidiUTF8(sUTF8, ARightToLeft);
  FCharCount := length(FBidi);
  AnalyzeLineStart;
end;

procedure TBidiTextLayout.SetLayout(ARect: TRectF);
begin
  TopLeft := ARect.TopLeft;
  AvailableWidth:= ARect.Width;
  AvailableHeight:= ARect.Height;
end;

procedure TBidiTextLayout.InvalidateLayout;
begin
  FLayoutComputed:= false;
end;

procedure TBidiTextLayout.ComputeLayout;
var w,h, lineHeight, fullHeight, baseLine: single;
  i, splitIndex, nextStart: Integer;
  lineStart, subStart, lineEnd: integer;
  paraSpacing, correctedBaseLine: single;
  isEndOfPara: boolean;
  partStr, remainStr: string;
  pos: TPointF;
  m: TAffineMatrix;
begin
  fullHeight:= GetFontFullHeight;
  baseLine := GetFontBaseline;
  FPartCount := 0;
  m := AffineMatrixTranslation(FTopLeft.x, FTopLeft.y)*AffineMatrixRotationDeg(-GetFontOrientation);
  FStartCaret.Top := FTopLeft;
  FStartCaret.Bottom := m*PointF(0,fullHeight);
  FStartCaret.RightToLeft := false;
  FStartCaret.PreviousTop := EmptyPointF;
  FStartCaret.PreviousBottom := EmptyPointF;
  FStartCaret.PreviousRightToLeft := false;
  FStartCaret.PartIndex := 0;
  FBrokenLineCount := 0;

  FLayoutComputed:= true;

  paraSpacing := 0;
  pos := PointF(0,0);

  for i := 0 to FUnbrokenLineCount-1 do
  begin
    lineStart := FUnbrokenLineStart[i];
    lineEnd := FUnbrokenLineStart[i+1];
    isEndOfPara:= (lineEnd>lineStart) and FBidi[lineEnd-1].BidiInfo.IsEndOfParagraph;

    subStart := lineStart;
    //avoid warnings
    splitIndex := subStart;
    h := 0;

    while subStart < lineEnd do
    begin
      LevelSize(FAvailableWidth, subStart, lineEnd, FBidi[lineStart].BidiInfo.ParagraphBidiLevel, splitIndex, w,h);

      if splitIndex < lineEnd then
      begin
        partStr := copy(FText, FBidi[subStart].Offset+1, FBidi[splitIndex].Offset - FBidi[subStart].Offset);
        remainStr := copy(FText, FBidi[splitIndex].Offset+1, FBidi[lineEnd].Offset - FBidi[splitIndex].Offset);
        BGRADefaultWordBreakHandler(partStr, remainStr);

        splitIndex:= subStart + UTF8Length(partStr);
        nextStart := splitIndex;
        while (nextStart < lineEnd) and (FText[FBidi[nextStart].Offset+1] in [' ',#9]) do inc(nextStart);
      end else
        nextStart := splitIndex;

      if h > fullHeight then
        lineHeight := h
      else
        lineHeight := fullHeight;

      if fullHeight <> 0 then
        correctedBaseLine := baseLine*lineHeight/fullHeight
      else
        correctedBaseLine:= 0;

      if Odd(FBidi[lineStart].BidiInfo.ParagraphBidiLevel) and (FAvailableWidth <> EmptySingle) then
        ComputeLevelLayout(m, pos + PointF(FAvailableWidth,0), subStart, splitIndex, FBidi[lineStart].BidiInfo.ParagraphBidiLevel, lineHeight, correctedBaseLine, FBrokenLineCount, w)
      else
        ComputeLevelLayout(m, pos, subStart, splitIndex, FBidi[lineStart].BidiInfo.ParagraphBidiLevel, lineHeight, correctedBaseLine, FBrokenLineCount, w);

      if FBrokenLineCount >= length(FBrokenLine) then
        setlength(FBrokenLine, length(FBrokenLine)*2+4);
      FBrokenLine[FBrokenLineCount].startIndex:= subStart;
      FBrokenLine[FBrokenLineCount].endIndex:= splitIndex;
      FBrokenLine[FBrokenLineCount].bidiLevel := FBidi[lineStart].BidiInfo.ParagraphBidiLevel;
      if FAvailableWidth <> EmptySingle then
        FBrokenLine[FBrokenLineCount].affineBox := TAffineBox.AffineBox(m*pos, m*(pos+PointF(FAvailableWidth,0)), m*(pos+PointF(0,lineHeight)))
      else
      begin
        if Odd(FBidi[lineStart].BidiInfo.ParagraphBidiLevel) then
          FBrokenLine[FBrokenLineCount].affineBox := TAffineBox.AffineBox(m*(pos+PointF(-w,0)), m*pos, m*(pos+PointF(-w,lineHeight)))
        else
          FBrokenLine[FBrokenLineCount].affineBox := TAffineBox.AffineBox(m*pos, m*(pos+PointF(w,0)), m*(pos+PointF(0,lineHeight)))
      end;

      FBrokenLineCount += 1;
      subStart := nextStart;
      pos.y += lineHeight;
      if (FAvailableHeight <> EmptySingle) and (pos.y >= FAvailableHeight) then exit;
    end;

    if isEndOfPara then pos.y += paraSpacing;
  end;
end;

procedure TBidiTextLayout.NeedLayout;
begin
  if not FLayoutComputed then ComputeLayout;
end;

procedure TBidiTextLayout.DrawText(ADest: TBGRACustomBitmap);
var
  i: Integer;
begin
  NeedLayout;
  for i := 0 to FPartCount-1 do
    with (FPart[i].affineBox.TopLeft + FPart[i].posCorrection) do
      TextOutBidiOverride(ADest, x,y, FPart[i].sUTF8, odd(FPart[i].bidiLevel));
end;

procedure TBidiTextLayout.DrawCaret(ADest: TBGRACustomBitmap;
  ACharIndex: integer; AMainColor, ASecondaryColor: TBGRAPixel);

  procedure DrawSingleCaret(ATop,ABottom: TPointF; ARightToLeft, AShowDir: boolean; AColor: TBGRAPixel);
  var u,v: TPointF;
    triSize,len: single;
  begin
    //hinting depending on orientation
    if abs(ATop.x - ABottom.x) < abs(ATop.y - ABottom.y) then
    begin
      ATop.x := round(ATop.x);
      ABottom.x := round(ABottom.x);
    end
    else
    begin
      ATop.y := round(ATop.y);
      ABottom.y := round(ABottom.y);
    end;
    u := ABottom-ATop;
    len := VectLen(u);
    if len > 0 then
    begin
      u := (1/len)*u;
      v := PointF(u.y,-u.x);
      if AShowDir then
      begin
        triSize := len*0.2;
        if ARightToLeft then
          ADest.FillPolyAntialias(PointsF([ABottom + 0.5*v, ATop + 0.5*v - 0.5*u, ATop - (triSize+0.5)*v - 0.5*u, ATop - 0.5*v + triSize*u, ABottom - 0.5*v]), AColor)
        else
          ADest.FillPolyAntialias(PointsF([ABottom - 0.5*v, ATop - 0.5*v - 0.5*u, ATop + (triSize+0.5)*v - 0.5*u, ATop + triSize*u + 0.5*v, ABottom + 0.5*v]), AColor)
      end
      else
      begin
        if len > 10 then
          ADest.FillPolyAntialias(PointsF([ABottom - 0.5*v, ATop - 0.5*v, ATop + 1.5*v, ABottom + 1.5*v]), AColor)
        else
          ADest.FillPolyAntialias(PointsF([ABottom - 0.5*v, ATop - 0.5*v, ATop + 0.5*v, ABottom + 0.5*v]), AColor)
      end;
    end else
      ADest.DrawPixel(round(ATop.x),round(ATop.y), AColor);
  end;

var
  caret: TBidiCaretPos;
  showDir: Boolean;
begin
  NeedLayout;

  caret := GetCaret(ACharIndex);
  showDir := not isEmptyPointF(caret.PreviousTop) and (caret.RightToLeft <> caret.PreviousRightToLeft);
  if not isEmptyPointF(caret.Top) then DrawSingleCaret(caret.Top, caret.Bottom, caret.RightToLeft, showDir, AMainColor);
  if not isEmptyPointF(caret.PreviousTop) then DrawSingleCaret(caret.PreviousTop, caret.PreviousBottom, caret.PreviousRightToLeft, showDir, ASecondaryColor);
end;

procedure TBidiTextLayout.DrawSelection(ADest: TBGRACustomBitmap; AStartIndex,
  AEndIndex: integer; AColor: TBGRAPixel);
var
  env: ArrayOfTPointF;
begin
  NeedLayout;

  if AStartIndex = AEndIndex then exit;
  env := GetTextEnveloppe(AStartIndex,AEndIndex);
  ADest.FillPolyAntialias(env, AColor);
end;

function TBidiTextLayout.GetCaret(ACharIndex: integer): TBidiCaretPos;
var
  i: Integer;
  w: Single;
  u: TPointF;
begin
  NeedLayout;

  if (ACharIndex < 0) or (ACharIndex > CharCount) then
    raise ERangeError.Create('Invalid index');
  result.PartIndex := -1;
  result.Top := EmptyPointF;
  result.Bottom := EmptyPointF;
  result.RightToLeft := false;
  result.PreviousTop := EmptyPointF;
  result.PreviousBottom := EmptyPointF;
  result.PreviousRightToLeft := false;

  for i := 0 to FPartCount-1 do
    if ACharIndex <= FPart[i].startIndex then
    begin
      result := GetPartStartCaret(i);
      exit;
    end else
    if (ACharIndex > FPart[i].startIndex) and (ACharIndex <= FPart[i].endIndex) then
    begin
      if (i < FPartCount-1) and (ACharIndex = FPart[i+1].startIndex) then
      begin
        result := GetPartStartCaret(i+1);
        exit;
      end else
      begin
        if i = FPart[i].endIndex then
        begin
          result := GetPartEndCaret(i);
          exit;
        end else
        begin
          w := TextSizeBidiOverrideSplit(FPart[i].startIndex, FPart[i].endIndex, odd(FPart[i].bidiLevel), ACharIndex).x;

          u := FPart[i].affineBox.TopRight - FPart[i].affineBox.TopLeft;
          if VectLen(u) > 0 then u := (1/VectLen(u))*u;
          if Odd(FPart[i].bidiLevel) then
            result.Top := FPart[i].affineBox.TopRight - w*u
          else
            result.Top := FPart[i].affineBox.TopLeft + w*u;
          result.Bottom := result.Top+(FPart[i].affineBox.BottomLeft-FPart[i].affineBox.TopLeft);
          result.RightToLeft := odd(FPart[i].bidiLevel);
          result.PreviousRightToLeft := result.RightToLeft;
          result.PartIndex := i;
        end;
        exit;
      end;
    end;

  if (PartCount > 0) and (ACharIndex >= FPart[PartCount-1].endIndex) then
    result := GetPartEndCaret(PartCount-1)
  else
  if ACharIndex = 0 then
    result := FStartCaret;
end;

function TBidiTextLayout.GetCharIndexAt(APosition: TPointF): integer;
var
  i,j, fit: Integer;
  u,u2: cardinal;
  axis, origin: TPointF;
  len, w, curW, newW: Single;
  str: String;
  curIndex, newIndex: integer;
begin
  NeedLayout;

  for i := 0 to BrokenLineCount-1 do
    if BrokenLineAffineBox[i].Contains(APosition) then
    begin
      for j := 0 to PartCount-1 do
        if (PartBrokenLineIndex[j] = i) and PartAffineBox[j].Contains(APosition) then
        begin
          with PartAffineBox[j] do
          begin
            if PartRightToLeft[j] then
            begin
              axis := TopLeft-TopRight;
              origin := TopRight;
            end else
            begin
              axis := TopRight-TopLeft;
              origin := TopLeft;
            end;
            len := VectLen(axis);
            if len > 0 then
            begin
              w := ((APosition-origin)*axis)/len;
              str := copy(FText, FBidi[PartStartIndex[j]].Offset+1, FBidi[PartEndIndex[j]].Offset - FBidi[PartStartIndex[j]].Offset);
              fit := TextFitInfoBidiOverride(str, w, PartRightToLeft[j]);
              curIndex := PartStartIndex[j]+fit;
              curW := TextSizeBidiOverrideSplit(PartStartIndex[j], PartEndIndex[j], PartRightToLeft[j], curIndex).x;
              while (curW < w) and (curIndex < PartEndIndex[j]) do
              begin
                newIndex := curIndex+1;
                while (newIndex < PartEndIndex[j]) and (GetUnicodeBidiClass(GetUnicodeChar(newIndex)) = ubcNonSpacingMark) do inc(newIndex);
                newW := TextSizeBidiOverrideSplit(PartStartIndex[j], PartEndIndex[j], PartRightToLeft[j], newIndex).x;
                if newW >= w then
                begin
                  if (curW+newW)*0.5 + 1 < w then curIndex := newIndex;
                  break;
                end;
                curIndex := newIndex;
              end;
              exit(curIndex);
            end;
          end;
          exit(PartStartIndex[j]);
        end;
      result := BrokenLineEndIndex[i];
      if result > BrokenLineStartIndex[i] then
      begin
        u := GetUnicodeChar(result-1);
        if IsUnicodeParagraphSeparator(u) or (u = UNICODE_LINE_SEPARATOR) then
        begin
          dec(result);
          if (result > BrokenLineStartIndex[i]) and (u = 13) or (u = 10) then
          begin
            u2 := GetUnicodeChar(result-1);
            if (u2 <> u) and ((u2 = 13) or (u2 = 10)) then dec(result);
          end;
        end;
      end;
      exit;
    end;
  exit(CharCount);
end;

function TBidiTextLayout.GetTextEnveloppe(AStartIndex, AEndIndex: integer): ArrayOfTPointF;
var
  temp, i: Integer;
  caret, caretEnd, caretStartPart, caretEndPart: TBidiCaretPos;
  brokenLineIndex: integer;
begin
  NeedLayout;

  result := nil;

  if AStartIndex > AEndIndex then
  begin
    temp := AStartIndex;
    AStartIndex:= AEndIndex;
    AEndIndex:= temp;
  end;
  caret := GetCaret(AStartIndex);
  caretEnd := GetCaret(AEndIndex);
  if not isEmptyPointF(caretEnd.PreviousTop) then
  begin
    caretEnd.Top := caretEnd.PreviousTop;        caretEnd.PreviousTop := EmptyPointF;
    caretEnd.Bottom := caretEnd.PreviousBottom;  caretEnd.PreviousBottom := EmptyPointF;
    caretEnd.RightToLeft := caretEnd.PreviousRightToLeft;
    if caretEnd.PartIndex <> -1 then caretEnd.PartIndex -= 1;
  end;

  if caret.PartIndex = caretEnd.PartIndex then
  begin
    if not isEmptyPointF(caret.Top) and not isEmptyPointF(caretEnd.Top) then
      result := PointsF([caret.Top-PointF(0.5,0.5),caret.Bottom-PointF(0.5,0.5),caretEnd.Bottom-PointF(0.5,0.5),caretEnd.Top-PointF(0.5,0.5)]);
  end else
  begin
    result := nil;
    for i := caret.PartIndex to caretEnd.PartIndex do
    begin
      if i > caret.PartIndex then caretStartPart := PartStartCaret[i]
      else caretStartPart := caret;

      if i < caretEnd.PartIndex then caretEndPart := PartEndCaret[i]
      else caretEndPart := caretEnd;

      brokenLineIndex := PartBrokenLineIndex[i];
      if (i > caret.PartIndex) and (PartBrokenLineIndex[i-1] <> brokenLineIndex) then
      begin
        if BrokenLineRightToLeft[brokenLineIndex] = PartRightToLeft[i] then
          caretStartPart := BrokenLineStartCaret[brokenLineIndex]
        else
          caretEndPart := BrokenLineStartCaret[brokenLineIndex];
      end;

      if (i < caretEnd.PartIndex) and (PartBrokenLineIndex[i+1] <> PartBrokenLineIndex[i]) then
      begin
        if BrokenLineRightToLeft[brokenLineIndex] = PartRightToLeft[i] then
          caretEndPart := BrokenLineEndCaret[brokenLineIndex]
        else
          caretStartPart := BrokenLineEndCaret[brokenLineIndex];
      end;

      result := ConcatPointsF([result,
                            PointsF([caretStartPart.Top-PointF(0.5,0.5),caretStartPart.Bottom-PointF(0.5,0.5),caretEndPart.Bottom-PointF(0.5,0.5),caretEndPart.Top-PointF(0.5,0.5), EmptyPointF])
                           ]);
    end;
    if result <> nil then setlength(result, length(result)-1);
  end;
end;

function TBidiTextLayout.GetPartStartCaret(APartIndex: integer): TBidiCaretPos;
begin
  if (APartIndex < 0) or (APartIndex > PartCount) then
    raise ERangeError.Create('Invalid index');

  result.PartIndex := APartIndex;

  if Odd(FPart[APartIndex].bidiLevel) then
    result.Top := FPart[APartIndex].affineBox.TopRight
  else
    result.Top := FPart[APartIndex].affineBox.TopLeft;
  result.Bottom := result.Top+(FPart[APartIndex].affineBox.BottomLeft-FPart[APartIndex].affineBox.TopLeft);
  result.RightToLeft := odd(FPart[APartIndex].bidiLevel);

  if (APartIndex > 0) and (FPart[APartIndex-1].endIndex = FPart[APartIndex].startIndex) then
  begin
    if Odd(FPart[APartIndex-1].bidiLevel) then
      result.PreviousTop := FPart[APartIndex-1].affineBox.TopLeft
    else
      result.PreviousTop := FPart[APartIndex-1].affineBox.TopRight;
    result.PreviousBottom := result.PreviousTop+(FPart[APartIndex-1].affineBox.BottomLeft-FPart[APartIndex-1].affineBox.TopLeft);
    result.PreviousRightToLeft := odd(FPart[APartIndex-1].bidiLevel);
  end else
  begin
    result.PreviousTop := EmptyPointF;
    result.PreviousBottom := EmptyPointF;
    result.PreviousRightToLeft := result.RightToLeft;
  end;
end;

function TBidiTextLayout.GetPartEndCaret(APartIndex: integer): TBidiCaretPos;
begin
  if (APartIndex < 0) or (APartIndex > PartCount) then
    raise ERangeError.Create('Invalid index');

  result.PartIndex := APartIndex;

  if Odd(FPart[APartIndex].bidiLevel) then
    result.Top := FPart[APartIndex].affineBox.TopLeft
  else
    result.Top := FPart[APartIndex].affineBox.TopRight;
  result.Bottom := result.Top+(FPart[APartIndex].affineBox.BottomLeft-FPart[APartIndex].affineBox.TopLeft);
  result.RightToLeft := odd(FPart[APartIndex].bidiLevel);

  result.PreviousTop := EmptyPointF;
  result.PreviousBottom := EmptyPointF;
  result.PreviousRightToLeft := result.RightToLeft;
end;

procedure TBidiTextLayout.ComputeLevelLayout(AMatrix: TAffineMatrix; APos: TPointF; startIndex,
  endIndex: integer; bidiLevel: byte; fullHeight, baseLine: single; brokenLineIndex: integer;
  out AWidth: single);
var
  i: Integer;
  subLevel: byte;
  subStart, subSplit: integer;
  subStr: string;
  w,w2,h,dy: single;
  subSize: TPointF;
begin
  AWidth := 0;

  while (startIndex < endIndex) and FBidi[startIndex].BidiInfo.IsRemoved do inc(startIndex);
  while (startIndex < endIndex) and FBidi[endIndex-1].BidiInfo.IsRemoved do dec(endIndex);
  if endIndex = startIndex then exit;

  i := startIndex;
  while i < endIndex do
  begin
    if not FBidi[i].BidiInfo.IsRemoved then
    begin
      if FBidi[i].BidiInfo.BidiLevel > bidiLevel then
      begin
        subStart := i;
        subLevel := FBidi[i].BidiInfo.BidiLevel;
        inc(i);
        while (i < endIndex) and (FBidi[i].BidiInfo.BidiLevel > bidiLevel) do
        begin
          if FBidi[i].BidiInfo.BidiLevel < subLevel then
            subLevel := FBidi[i].BidiInfo.BidiLevel;
          inc(i);
        end;

        if odd(bidiLevel) then
        begin
          if odd(subLevel) then
          begin
            ComputeLevelLayout(AMatrix, APos, subStart, i, subLevel, fullHeight, baseLine, brokenLineIndex, w);
            APos.x -= w;
          end else
          begin
            LevelSize(EmptySingle, subStart, i, subLevel, subSplit, w,h);
            APos.x -= w;
            ComputeLevelLayout(AMatrix, APos, subStart, subSplit, subLevel, fullHeight, baseLine, brokenLineIndex, w2);
          end;
        end else
        begin
          if odd(subLevel) then
          begin
            LevelSize(EmptySingle, subStart, i, subLevel, subSplit, w,h);
            APos.x += w;
            ComputeLevelLayout(AMatrix, APos, subStart, subSplit, subLevel, fullHeight, baseLine, brokenLineIndex, w2);
          end else
          begin
            ComputeLevelLayout(AMatrix, APos, subStart, i, subLevel, fullHeight, baseLine, brokenLineIndex, w);
            APos.x += w;
          end;
        end;
        AWidth += w;
      end else
      begin
        subStart:= i;
        inc(i);
        while (i < endIndex) and (FBidi[i].BidiInfo.BidiLevel = bidiLevel) do inc(i);

        subStr := GetSameLevelString(subStart,i);

        subSize := TextSizeBidiOverride(subStr, odd(bidiLevel));
        w := subSize.x;
        if (subSize.y <> fullHeight) and (fullHeight <> 0) then
        begin
          dy := baseLine * (1 - subSize.y/fullHeight);
        end else
          dy := 0;
        if odd(bidiLevel) then
        begin
          APos.x -= w;
          AddPart(subStart, i, bidiLevel,
                  TAffineBox.AffineBox(AMatrix*APos, AMatrix * (APos + PointF(w,0)), AMatrix * (APos + PointF(0,fullHeight)) ),
                  AMatrix * (APos + PointF(0,dy)) - AMatrix*APos, subStr, brokenLineIndex);
        end else
        begin
          AddPart(subStart, i, bidiLevel,
                  TAffineBox.AffineBox(AMatrix*APos, AMatrix * (APos + PointF(w,0)), AMatrix * (APos + PointF(0,fullHeight)) ),
                  AMatrix * (APos + PointF(0,dy)) - AMatrix*APos, subStr, brokenLineIndex);
          APos.x += w;
        end;
        AWidth += w;
      end;

    end else
      inc(i);
  end;
end;

procedure TBidiTextLayout.Init;
begin
  FPartCount:= 0;
  FBrokenLineCount:= 0;
  FTopLeft := PointF(0,0);
  FAvailableWidth:= EmptySingle;
  FAvailableHeight:= EmptySingle;
  FLayoutComputed:= false;
end;

end.

