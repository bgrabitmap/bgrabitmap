// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAUnicode;
{ Implementation of Unicode bidi algorithm }
{ Author: circular }

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  BGRAClasses, SysUtils;

type
  TUnicodeBidiClass = (ubcBoundaryNeutral, ubcSegmentSeparator, ubcParagraphSeparator, ubcWhiteSpace, ubcOtherNeutrals,
                      ubcCommonSeparator, ubcNonSpacingMark,
                      ubcLeftToRight, ubcEuropeanNumber, ubcEuropeanNumberSeparator, ubcEuropeanNumberTerminator,
                      ubcRightToLeft, ubcArabicLetter, ubcArabicNumber,
                      ubcUnknown,
                      ubcCombiningLeftToRight,   //ubcLeftToRight in Mc category
                      ubcMirroredNeutral);       //ubcOtherNeutrals with Mirrored property
  TUnicodeJoiningType = (ujtNonJoining{U}, ujtTransparent{T}, ujtRightJoining{R}, ujtLeftJoining{L},
                         ujtDualJoining{D}, ujtJoinCausing{C});
  TFontBidiMode = (fbmAuto, fbmLeftToRight, fbmRightToLeft);

const
  ubcNeutral = [ubcSegmentSeparator, ubcParagraphSeparator, ubcWhiteSpace, ubcOtherNeutrals];

  BIDI_FLAG_REMOVED = 1;                   //RLE, LRE, RLO, LRO, PDF and BN are supposed to be removed
  BIDI_FLAG_IMPLICIT_END_OF_PARAGRAPH = 2; //implicit end of paragraph (paragraph spacing below due to end of text)
  BIDI_FLAG_EXPLICIT_END_OF_PARAGRAPH = 4; //explicit end of paragraph (paragraph spacing below due to paragraph split)
  BIDI_FLAG_END_OF_LINE = 8;               //line break <br>
  BIDI_FLAG_LIGATURE_RIGHT = 16;           //joins to the letter on the right (possible for joining type R and D)
  BIDI_FLAG_LIGATURE_LEFT = 32;            //joins to the letter on the left (possible for joining type L and D)
  BIDI_FLAG_LIGATURE_BOUNDARY = 64;        //zero-width joiner or non-joiner
  BIDI_FLAG_LIGATURE_TRANSPARENT = 128;    //does not affect ligature
  BIDI_FLAG_RTL_SCRIPT = 256;              //script is written from right to left (arabic, N'Ko...)
  BIDI_FLAG_NON_SPACING_MARK = 512;        //it is a non-spacing mark
  BIDI_FLAG_COMBINING_LEFT = 1024;         //this letter is to be combined to the left of previous letter
  BIDI_FLAG_COMBINING_RIGHT = 2048;        //this letter is to be combined to the right of previous letter
  BIDI_FLAG_MULTICHAR_START = 4096;        //start of a multichar (letter + non spacing marks, non spacing marks)
  BIDI_FLAG_MIRRORED = 8192;               //the glyph is mirrored when in RTL text

type
  PUnicodeBidiInfo = ^TUnicodeBidiInfo;

  { TUnicodeBidiInfo }

  TUnicodeBidiInfo = packed record
  private
    function GetDiscardable: boolean;
    function GetEndOfLine: boolean;
    function GetEndOfParagraph: boolean;
    function GetExplicitEndOfParagraph: boolean;
    function GetHasLigatureLeft: boolean;
    function GetHasLigatureRight: boolean;
    function GetImplicitEndOfParagraph: boolean;
    function GetIsCombiningLeft: boolean;
    function GetIsCombiningRight: boolean;
    function GetIsMirrored: boolean;
    function GetLigatureBoundary: boolean;
    function GetLigatureTransparent: boolean;
    function GetMulticharStart: boolean;
    function GetNonSpacingMark: boolean;
    function GetRemoved: boolean;
    function GetRightToLeft: boolean;
    function GetParagraphRightToLeft: boolean;
    function GetRightToLeftScript: boolean;
  public
    ParagraphBidiLevel, BidiLevel: byte;
    Flags: Word;
    class operator =(const AInfo1, AInfo2: TUnicodeBidiInfo): boolean;
    property IsRemoved: boolean read GetRemoved;
    property IsRightToLeft: boolean read GetRightToLeft;
    property IsParagraphRightToLeft: boolean read GetParagraphRightToLeft;
    property IsEndOfLine: boolean read GetEndOfLine;
    property IsEndOfParagraph: boolean read GetEndOfParagraph;
    property IsExplicitEndOfParagraph: boolean read GetExplicitEndOfParagraph;
    property IsImplicitEndOfParagraph: boolean read GetImplicitEndOfParagraph;
    property HasLigatureRight: boolean read GetHasLigatureRight;
    property HasLigatureLeft: boolean read GetHasLigatureLeft;
    property IsLigatureBoundary: boolean read GetLigatureBoundary;
    property IsLigatureTransparent: boolean read GetLigatureTransparent;
    property IsDiscardable: boolean read GetDiscardable;
    property IsRightToLeftScript: boolean read GetRightToLeftScript;
    property IsNonSpacingMark: boolean read GetNonSpacingMark;
    property IsCombiningLeft: boolean read GetIsCombiningLeft;
    property IsCombiningRight: boolean read GetIsCombiningRight;
    property IsMulticharStart: boolean read GetMulticharStart;
    property IsMirrored: boolean read GetIsMirrored;
  end;

  TUnicodeBidiArray = packed array of TUnicodeBidiInfo;
  TUnicodeDisplayOrder = array of integer;

const
  //maximum nesting level of isolates and bidi-formatting blocks (char bidi level can actually be higher due to char properties)
  UNICODE_MAX_BIDI_DEPTH = 125;

  UNICODE_NO_BREAK_SPACE = $A0;
  UNICODE_LINE_SEPARATOR = $2028;      //equivalent of <br>
  UNICODE_PARAGRAPH_SEPARATOR = $2029; //equivalent of </p>
  UNICODE_NEXT_LINE = $0085;           //equivalent of CRLF

  //characters that split lines into top-level bidi blocks
  UNICODE_LEFT_TO_RIGHT_ISOLATE = $2066;
  UNICODE_RIGHT_TO_LEFT_ISOLATE = $2067;
  UNICODE_FIRST_STRONG_ISOLATE = $2068;
  UNICODE_POP_DIRECTIONAL_ISOLATE = $2069;

  //characters that split into bidi sub-blocks (called "formatting")
  UNICODE_LEFT_TO_RIGHT_EMBEDDING = $202A;
  UNICODE_RIGHT_TO_LEFT_EMBEDDING = $202B;
  UNICODE_LEFT_TO_RIGHT_OVERRIDE = $202D;
  UNICODE_RIGHT_TO_LEFT_OVERRIDE = $202E;
  UNICODE_POP_DIRECTIONAL_FORMATTING = $202C;

  //characters that mark direction without splitting the bidi block
  UNICODE_LEFT_TO_RIGHT_MARK = $200E;
  UNICODE_RIGHT_TO_LEFT_MARK = $200F;
  UNICODE_ARABIC_LETTER_MARK = $061C;

  //data separators
  UNICODE_INFORMATION_SEPARATOR_FOUR = $001C;   //end-of-file
  UNICODE_INFORMATION_SEPARATOR_THREE = $001D;  //section separator
  UNICODE_INFORMATION_SEPARATOR_TWO = $001E;    //record separator, kind of equivalent to paragraph separator
  UNICODE_INFORMATION_SEPARATOR_ONE = $001F;    //field separator, kind of equivalent to Tab

  //zero-width
  UNICODE_ZERO_WIDTH_SPACE = $200B;
  UNICODE_ZERO_WIDTH_NON_JOINER = $200C;
  UNICODE_ZERO_WIDTH_NO_BREAK_SPACE = $FEFF;   //byte order mark
  UNICODE_ZERO_WIDTH_JOINER = $200D;
  UNICODE_COMBINING_GRAPHEME_JOINER = $034F;

  //arabic letters
  UNICODE_ARABIC_TATWEEL = $0640;    //horizontal line that makes a ligature with most letters

  //ideographic punctuation
  UNICODE_IDEOGRAPHIC_COMMA = $3001;
  UNICODE_IDEOGRAPHIC_FULL_STOP = $3002;
  UNICODE_FULLWIDTH_COMMA = $FF0C;
  UNICODE_HORIZONTAL_ELLIPSIS = $2026;

  //bracket equivalence
  UNICODE_RIGHT_POINTING_ANGLE_BRACKET = $232A;
  UNICODE_RIGHT_ANGLE_BRACKET = $3009;

type //bracket matching
  TUnicodeBracketInfo = record
    IsBracket: boolean;
    OpeningBracket,ClosingBracket: LongWord;
  end;

{ Returns the Bidi class as defined by Unicode used to determine text direction }
function GetUnicodeBidiClass(u: LongWord): TUnicodeBidiClass;
{ Same as above but returns additional classes: ubcCombiningLeftToRight and ubcMirroredNeutral }
function GetUnicodeBidiClassEx(u: LongWord): TUnicodeBidiClass;
function GetUnicodeBracketInfo(u: LongWord): TUnicodeBracketInfo;
{ Returns how the letter can be joined to the surrounding letters (for example in arabic) }
function GetUnicodeJoiningType(u: LongWord): TUnicodeJoiningType;
{ Returns the Combining class defined by unicode for non-spacing marks and combining marks
  or 255 if the character is not to be combined }
function GetUnicodeCombiningClass(u: LongWord): byte;
function IsZeroWidthUnicode(u: LongWord): boolean;
{ Returns if the symbol can be mirrored horizontally for right-to-left text }
function IsUnicodeMirrored(u: LongWord): boolean;
function IsUnicodeParagraphSeparator(u: LongWord): boolean;
function IsUnicodeCrLf(u: LongWord): boolean;
function IsUnicodeSpace(u: LongWord): boolean;
function IsUnicodeIsolateOrFormatting(u: LongWord): boolean;
function IsModifierCombiningMark(u: LongWord): boolean;

{ Analyze unicode and return bidi levels for each character.
  baseDirection can be either UNICODE_LEFT_TO_RIGHT_ISOLATE, UNICODE_RIGHT_TO_LEFT_ISOLATE or UNICODE_FIRST_STRONG_ISOLATE }
function AnalyzeBidiUnicode(u: PLongWord; ALength: integer; baseDirection: LongWord): TUnicodeBidiArray;
function AnalyzeBidiUnicode(u: PLongWord; ALength: integer; ABidiMode: TFontBidiMode): TUnicodeBidiArray;

{ Determine diplay order, provided the display surface is horizontally infinite }
function GetUnicodeDisplayOrder(const AInfo: TUnicodeBidiArray): TUnicodeDisplayOrder; overload;
function GetUnicodeDisplayOrder(ALevels: PByte; ACount: integer): TUnicodeDisplayOrder; overload;
function GetUnicodeDisplayOrder(ABidiInfo: PUnicodeBidiInfo; AStride, ACount: integer): TUnicodeDisplayOrder; overload;

implementation

{$i generatedunicode.inc}

function GetUnicodeCombiningClass(u: LongWord): byte;
var
  minIndex, maxIndex, midIndex: Integer;
  compU: LongWord;
begin
  minIndex := 0;
  maxIndex := high(UnicodeCombiningInfos);
  repeat
    midIndex := (minIndex+maxIndex) shr 1;
    compU := UnicodeCombiningInfos[midIndex].u;
    if u = compU then exit(UnicodeCombiningInfos[midIndex].c) else
    if u < compU then maxIndex := midIndex-1
    else minIndex := midIndex+1;
  until maxIndex < minIndex;
  result := 255; //not combining
end;

function GetUnicodeBidiClass(u: LongWord): TUnicodeBidiClass;
begin
  result := GetUnicodeBidiClassEx(u);
  if result = ubcMirroredNeutral then result := ubcOtherNeutrals
  else if result = ubcCombiningLeftToRight then result := ubcLeftToRight;
end;

function IsUnicodeMirrored(u: LongWord): boolean;
begin
  result := GetUnicodeBidiClassEx(u) = ubcMirroredNeutral;
end;

function IsZeroWidthUnicode(u: LongWord): boolean;
begin
  case u of
  UNICODE_ZERO_WIDTH_SPACE, UNICODE_ZERO_WIDTH_NON_JOINER,
  UNICODE_ZERO_WIDTH_JOINER, UNICODE_ZERO_WIDTH_NO_BREAK_SPACE,
  UNICODE_LEFT_TO_RIGHT_MARK,UNICODE_RIGHT_TO_LEFT_MARK,
  UNICODE_ARABIC_LETTER_MARK: result := true;
  else result := false;
  end;
end;

function IsUnicodeParagraphSeparator(u: LongWord): boolean;
begin
  case u of
  $0A, $0D, UNICODE_NEXT_LINE, UNICODE_PARAGRAPH_SEPARATOR,
  UNICODE_INFORMATION_SEPARATOR_FOUR, UNICODE_INFORMATION_SEPARATOR_THREE, UNICODE_INFORMATION_SEPARATOR_TWO: result := true;
  else result := false;
  end;
end;

function IsUnicodeCrLf(u: LongWord): boolean;
begin
  result := (u=10) or (u=13);
end;

function IsUnicodeSpace(u: LongWord): boolean;
begin
  result := GetUnicodeBidiClass(u) = ubcWhiteSpace;
end;

function IsUnicodeIsolateOrFormatting(u: LongWord): boolean;
begin
  case u of
  UNICODE_LEFT_TO_RIGHT_ISOLATE, UNICODE_RIGHT_TO_LEFT_ISOLATE, UNICODE_FIRST_STRONG_ISOLATE,
  UNICODE_LEFT_TO_RIGHT_EMBEDDING, UNICODE_RIGHT_TO_LEFT_EMBEDDING,
  UNICODE_LEFT_TO_RIGHT_OVERRIDE, UNICODE_RIGHT_TO_LEFT_OVERRIDE: exit(true)
  else exit(false);
  end;
end;

function IsModifierCombiningMark(u: LongWord): boolean;
begin
  case u of
  $0654,$0655,$0658,$06DC,$06E3,$06E7,$06E8,$08D3,$08F3: exit(true);
  else exit(false);
  end;
end;

{ TUnicodeBidiInfo }

function TUnicodeBidiInfo.GetDiscardable: boolean;
begin
  result := IsRemoved and not IsLigatureBoundary;
end;

function TUnicodeBidiInfo.GetEndOfLine: boolean;
begin
  result := (Flags and BIDI_FLAG_END_OF_LINE) <> 0;
end;

function TUnicodeBidiInfo.GetEndOfParagraph: boolean;
begin
  result := (Flags and (BIDI_FLAG_EXPLICIT_END_OF_PARAGRAPH or BIDI_FLAG_IMPLICIT_END_OF_PARAGRAPH)) <> 0;
end;

function TUnicodeBidiInfo.GetExplicitEndOfParagraph: boolean;
begin
  result := (Flags and BIDI_FLAG_EXPLICIT_END_OF_PARAGRAPH) <> 0;
end;

function TUnicodeBidiInfo.GetHasLigatureLeft: boolean;
begin
  result := (Flags and BIDI_FLAG_LIGATURE_LEFT) <> 0;
end;

function TUnicodeBidiInfo.GetHasLigatureRight: boolean;
begin
  result := (Flags and BIDI_FLAG_LIGATURE_RIGHT) <> 0;
end;

function TUnicodeBidiInfo.GetImplicitEndOfParagraph: boolean;
begin
  result := (Flags and BIDI_FLAG_IMPLICIT_END_OF_PARAGRAPH) <> 0;
end;

function TUnicodeBidiInfo.GetIsCombiningLeft: boolean;
begin
  result := (Flags and BIDI_FLAG_COMBINING_LEFT) <> 0;
end;

function TUnicodeBidiInfo.GetIsCombiningRight: boolean;
begin
  result := (Flags and BIDI_FLAG_COMBINING_RIGHT) <> 0;
end;

function TUnicodeBidiInfo.GetIsMirrored: boolean;
begin
  result := (Flags and BIDI_FLAG_MIRRORED) <> 0;
end;

function TUnicodeBidiInfo.GetLigatureBoundary: boolean;
begin
  result := (Flags and BIDI_FLAG_LIGATURE_BOUNDARY) <> 0;
end;

function TUnicodeBidiInfo.GetLigatureTransparent: boolean;
begin
  result := (Flags and BIDI_FLAG_LIGATURE_TRANSPARENT) <> 0;
end;

function TUnicodeBidiInfo.GetMulticharStart: boolean;
begin
  result := (Flags and BIDI_FLAG_MULTICHAR_START) <> 0;
end;

function TUnicodeBidiInfo.GetNonSpacingMark: boolean;
begin
  result := (Flags and BIDI_FLAG_NON_SPACING_MARK) <> 0;
end;

function TUnicodeBidiInfo.GetRemoved: boolean;
begin
  result := (Flags and BIDI_FLAG_REMOVED) <> 0;
end;

function TUnicodeBidiInfo.GetRightToLeft: boolean;
begin
  result := Odd(BidiLevel);
end;

function TUnicodeBidiInfo.GetParagraphRightToLeft: boolean;
begin
  result := Odd(ParagraphBidiLevel);
end;

function TUnicodeBidiInfo.GetRightToLeftScript: boolean;
begin
  result := (Flags and BIDI_FLAG_RTL_SCRIPT) <> 0;
end;

class operator TUnicodeBidiInfo.=(const AInfo1, AInfo2: TUnicodeBidiInfo
  ): boolean;
begin
  result := (AInfo1.BidiLevel = AInfo2.BidiLevel) and
    (AInfo1.Flags = AInfo2.Flags) and
    (AInfo1.ParagraphBidiLevel = AInfo2.ParagraphBidiLevel);
end;

function AnalyzeBidiUnicode(u: PLongWord; ALength: integer; baseDirection: LongWord): TUnicodeBidiArray;
type
  TUnicodeAnalysisElement = record
    bidiClass: TUnicodeBidiClass;
    prevInIsolate, nextInIsolate: integer; //next index in current isolate
  end;
  TUnicodeAnalysisArray = array of TUnicodeAnalysisElement;

var
  a: TUnicodeAnalysisArray;

  procedure ResolveWeakTypes(startIndex, afterEndIndex: integer; startOfSequence, {%H-}endOfSequence: TUnicodeBidiClass);
  var
    curIndex,backIndex: Integer;
    latestStrongClass, prevClass: TUnicodeBidiClass;
  begin
    //rules W1 and W2
    prevClass := startOfSequence;
    latestStrongClass:= prevClass;
    curIndex := startIndex;
    while curIndex <> afterEndIndex do
    begin
      if not result[curIndex].IsRemoved then
      begin
        case a[curIndex].bidiClass of
          ubcNonSpacingMark: a[curIndex].bidiClass:= prevClass;
          ubcEuropeanNumber: if latestStrongClass = ubcArabicLetter then a[curIndex].bidiClass:= ubcArabicNumber;
        end;
        case u[curIndex] of
        UNICODE_LEFT_TO_RIGHT_ISOLATE,
        UNICODE_RIGHT_TO_LEFT_ISOLATE,
        UNICODE_FIRST_STRONG_ISOLATE,
        UNICODE_POP_DIRECTIONAL_ISOLATE: prevClass := ubcOtherNeutrals;
        else prevClass := a[curIndex].bidiClass;
        end;
        if prevClass in [ubcLeftToRight,ubcRightToLeft,ubcArabicLetter] then latestStrongClass:= prevClass;
      end;
      curIndex := a[curIndex].nextInIsolate;
    end;

    // rule W4 and W5
    prevClass := startOfSequence;
    curIndex := startIndex;
    while curIndex <> afterEndIndex do
    begin
      if not result[curIndex].IsRemoved then
      begin
        case a[curIndex].bidiClass of
          ubcArabicLetter: a[curIndex].bidiClass := ubcRightToLeft;
          ubcEuropeanNumber:
            begin
              backIndex := curIndex;
              while backIndex > startIndex do
              begin
                dec(backIndex);
                if result[backIndex].IsRemoved then continue;
                if a[backIndex].bidiClass = ubcEuropeanNumberTerminator then
                  a[backIndex].bidiClass := ubcEuropeanNumber
                else break;
              end;
            end;
          ubcEuropeanNumberSeparator:
            if (prevClass = ubcEuropeanNumber) and (a[curIndex].nextInIsolate <> afterEndIndex) and
              (a[a[curIndex].nextInIsolate].bidiClass = ubcEuropeanNumber) then
                a[curIndex].bidiClass:= ubcEuropeanNumber;
          ubcCommonSeparator:
            if (prevClass in[ubcEuropeanNumber,ubcArabicNumber]) and (a[curIndex].nextInIsolate <> afterEndIndex) and
              (a[a[curIndex].nextInIsolate].bidiClass = prevClass) then
                a[curIndex].bidiClass:= prevClass;
          ubcEuropeanNumberTerminator:
            if prevClass = ubcEuropeanNumber then
              a[curIndex].bidiClass:= ubcEuropeanNumber;
        end;
        prevClass := a[curIndex].bidiClass;
      end;

      curIndex := a[curIndex].nextInIsolate;
    end;

    // rule W6 and W7
    curIndex := startIndex;
    latestStrongClass := startOfSequence;
    while curIndex <> afterEndIndex do
    begin
      if not result[curIndex].IsRemoved then
      begin
        case a[curIndex].bidiClass of
          ubcEuropeanNumberSeparator,ubcEuropeanNumberTerminator,ubcCommonSeparator: a[curIndex].bidiClass := ubcOtherNeutrals;
          ubcLeftToRight,ubcRightToLeft,ubcArabicLetter: latestStrongClass:= a[curIndex].bidiClass;
          ubcEuropeanNumber: if latestStrongClass = ubcLeftToRight then a[curIndex].bidiClass := ubcLeftToRight;
        end;
      end;
      curIndex := a[curIndex].nextInIsolate;
    end;
  end;

  procedure ResolveNeutrals(startIndex, afterEndIndex: integer; startOfSequence, endOfSequence: TUnicodeBidiClass);
  var
    curIndex,prevIndex,previewIndex: Integer;
    curRTL, include, rightToLeftEmbedding: Boolean;
    bidiClass: TUnicodeBidiClass;
  begin
    rightToLeftEmbedding := odd(result[startIndex].BidiLevel);
    curIndex := startIndex;
    curRTL := startOfSequence in [ubcRightToLeft,ubcArabicLetter];
    while curIndex <> afterEndIndex do
    begin
      case a[curIndex].bidiClass of
        ubcLeftToRight: curRTL := false;
        ubcRightToLeft,ubcArabicLetter,ubcArabicNumber,ubcEuropeanNumber: curRTL := true;
      else
        if curRTL <> rightToLeftEmbedding then
        begin
          //determine whether following neutral chars are included in reverse direction
          prevIndex := curIndex;
          previewIndex := a[curIndex].nextInIsolate;
          include := false;
          while previewIndex <> afterEndIndex do //uses endOfSequence for overflow
          begin
            if previewIndex = afterEndIndex then
              bidiClass:= endOfSequence
            else
              bidiClass:= a[previewIndex].bidiClass;
            case bidiClass of
              ubcLeftToRight:
                begin
                  include := not curRTL;
                  break;
                end;
              ubcRightToLeft,ubcArabicLetter,ubcArabicNumber,ubcEuropeanNumber:
                begin
                  include := curRTL;
                  break;
                end;
            end;
            prevIndex := previewIndex;
            previewIndex := a[previewIndex].nextInIsolate;
          end;
          if previewIndex = afterEndIndex then previewIndex := prevIndex;
          if include then
          begin
            while curIndex <> previewIndex do
            begin
              if a[curIndex].bidiClass = ubcBoundaryNeutral then
                result[curIndex].Flags := result[curIndex].Flags OR BIDI_FLAG_REMOVED; //supposed to be removed for rendering

              if a[curIndex].bidiClass in (ubcNeutral+[ubcBoundaryNeutral,ubcUnknown]) then
              begin
                if curRTL then a[curIndex].bidiClass := ubcRightToLeft
                else a[curIndex].bidiClass := ubcLeftToRight;
              end;

              curIndex := a[curIndex].nextInIsolate;
            end;
          end else
            curRTL := rightToLeftEmbedding;
        end;
      end;

      if a[curIndex].bidiClass = ubcBoundaryNeutral then
        result[curIndex].Flags := result[curIndex].Flags OR BIDI_FLAG_REMOVED; //supposed to be removed for rendering

      if a[curIndex].bidiClass in (ubcNeutral+[ubcBoundaryNeutral,ubcUnknown]) then
      begin
        if curRTL then a[curIndex].bidiClass := ubcRightToLeft
        else a[curIndex].bidiClass := ubcLeftToRight;
      end;

      curIndex := a[curIndex].nextInIsolate;
    end;
  end;

  procedure ResolveBrackets(startIndex, afterEndIndex: integer; startOfSequence, {%H-}endOfSequence: TUnicodeBidiClass);
  type TBracketPair = record
                  openIndex,closeIndex: integer;
                end;
  var
    bracketPairs: array of TBracketPair;
    bracketPairCount: integer;
    rightToLeft: boolean;

    procedure SortBracketPairs;
    var
      i,j,k: Integer;
      temp: TBracketPair;
    begin
      for i := 1 to bracketPairCount-1 do
      begin
        for j := 0 to i-1 do
          if bracketPairs[j].openIndex > bracketPairs[i].openIndex then
          begin
            temp := bracketPairs[i];
            for k := i downto j+1 do
              bracketPairs[k] := bracketPairs[k-1];
            bracketPairs[j] := temp;
          end;
      end;
    end;

    procedure FindBrackets; // rule BD16
    const MAX_BRACKET_STACK = 63;
    var
      bracketStack: array[0..MAX_BRACKET_STACK-1] of record
          bracketCharInfo: TUnicodeBracketInfo;
          index: integer;
        end;
      bracketStackPos,peekPos: integer;
      curIndex: integer;
      curBracket: TUnicodeBracketInfo;
    begin
      bracketPairCount := 0;
      bracketStackPos := 0;
      bracketStack[0].index := -1; //avoid warning
      curIndex := startIndex;
      while curIndex <> afterEndIndex do
      begin
        if a[curIndex].bidiClass = ubcOtherNeutrals then
        begin
          curBracket := GetUnicodeBracketInfo(u[curIndex]);
          if curBracket.IsBracket then
          begin
            // found opening bracket
            if curBracket.OpeningBracket = u[curIndex] then
            begin
              if bracketStackPos <= high(bracketStack) then
              begin
                bracketStack[bracketStackPos].bracketCharInfo := curBracket;
                bracketStack[bracketStackPos].index := curIndex;
                inc(bracketStackPos);
              end else
                break;
            end else
            begin
              for peekPos := bracketStackPos-1 downto 0 do
                if (bracketStack[peekPos].bracketCharInfo.ClosingBracket = u[curIndex]) or
                  ((bracketStack[peekPos].bracketCharInfo.ClosingBracket = UNICODE_RIGHT_ANGLE_BRACKET) and (u[curIndex] = UNICODE_RIGHT_POINTING_ANGLE_BRACKET)) or
                  ((bracketStack[peekPos].bracketCharInfo.ClosingBracket = UNICODE_RIGHT_POINTING_ANGLE_BRACKET) and (u[curIndex] = UNICODE_RIGHT_ANGLE_BRACKET)) then
                begin
                  bracketStackPos := peekPos;
                  if bracketPairCount >= length(bracketPairs) then
                    setlength(bracketPairs, bracketPairCount*2 + 8);
                  bracketPairs[bracketPairCount].openIndex := bracketStack[peekPos].index;
                  bracketPairs[bracketPairCount].closeIndex := curIndex;
                  inc(bracketPairCount);
                  break;
                end;
            end;
          end;
        end;
        curIndex := a[curIndex].nextInIsolate;
      end;
    end;

    procedure SetCharClass(index: integer; newClass: TUnicodeBidiClass);
    begin
      a[index].bidiClass:= newClass;
      index := a[index].nextInIsolate;
      while (index <> afterEndIndex) and (GetUnicodeBidiClass(u[index]) = ubcNonSpacingMark) do
      begin
        a[index].bidiClass := newClass;
        index := a[index].nextInIsolate;
      end;
    end;

    procedure ResolveBrackets; // rule N0
    var
      i, curIndex: Integer;
      sameDirection, oppositeDirection, oppositeContext: boolean;
    begin
      for i := 0 to bracketPairCount-1 do
      begin
        curIndex := bracketPairs[i].openIndex+1;
        sameDirection:= false;
        oppositeDirection:= false;
        while curIndex <> bracketPairs[i].closeIndex do
        begin
          Assert((curIndex >= startIndex) and (curIndex < length(a)), 'Expecting valid index');
          case a[curIndex].bidiClass of
          ubcLeftToRight:
            if not rightToLeft then
            begin
              sameDirection := true;
              break;
            end else oppositeDirection:= true;
          ubcRightToLeft,ubcArabicLetter,ubcEuropeanNumber,ubcArabicNumber:
            if rightToLeft then
            begin
              sameDirection := true;
              break;
            end else oppositeDirection:= true;
          end;
          curIndex := a[curIndex].nextInIsolate;
        end;
        if sameDirection then
        begin
          if rightToLeft then
          begin
            SetCharClass(bracketPairs[i].openIndex, ubcRightToLeft);
            SetCharClass(bracketPairs[i].closeIndex, ubcRightToLeft);
          end else
          begin
            SetCharClass(bracketPairs[i].openIndex, ubcLeftToRight);
            SetCharClass(bracketPairs[i].closeIndex, ubcLeftToRight);
          end;
        end else
        if oppositeDirection then
        begin
          curIndex := a[bracketPairs[i].openIndex].prevInIsolate;
          oppositeContext := false;
          while curIndex >= startIndex do
          begin
            case a[curIndex].bidiClass of
            ubcRightToLeft,ubcArabicLetter,ubcEuropeanNumber,ubcArabicNumber:
              begin
                oppositeContext := not rightToLeft;
                break;
              end;
            ubcLeftToRight:
              begin
                oppositeContext := rightToLeft;
                break;
              end;
            end;
            curIndex := a[curIndex].prevInIsolate;
          end;
          if rightToLeft xor oppositeContext then
          begin
            SetCharClass(bracketPairs[i].openIndex, ubcRightToLeft);
            SetCharClass(bracketPairs[i].closeIndex, ubcRightToLeft);
          end else
          begin
            SetCharClass(bracketPairs[i].openIndex, ubcLeftToRight);
            SetCharClass(bracketPairs[i].closeIndex, ubcLeftToRight);
          end;
        end;
      end;
    end;

  begin
    rightToLeft:= startOfSequence in[ubcRightToLeft,ubcArabicLetter];
    FindBrackets;
    SortBracketPairs;
    ResolveBrackets;
  end;

  procedure ResolveLigature(startIndex: integer);
  var
    prevJoiningType, joiningType: TUnicodeJoiningType;
    prevJoiningTypeBidilevel: byte;
    prevJoiningTypeIndex: integer;
    curIndex: Integer;
  begin
    prevJoiningType := ujtNonJoining;
    prevJoiningTypeIndex := -1;
    prevJoiningTypeBidilevel:= 0;
    curIndex := startIndex;
    while curIndex <> -1 do
    begin
      if prevJoiningTypeBidilevel <> result[curIndex].BidiLevel then
        prevJoiningType := ujtNonJoining;
      if result[curIndex].IsNonSpacingMark then
        joiningType := ujtTransparent //NSM are always joining-transparent
        else joiningType := GetUnicodeJoiningType(u[curIndex]);
      if joiningType = ujtTransparent then
        result[curIndex].Flags:= result[curIndex].Flags or BIDI_FLAG_LIGATURE_TRANSPARENT;
      if result[curIndex].IsRightToLeft then
      begin
        if (joiningType in[ujtRightJoining,ujtDualJoining])
          and (prevJoiningType in[ujtLeftJoining,ujtDualJoining,ujtJoinCausing]) then
          result[curIndex].Flags:= result[curIndex].Flags or BIDI_FLAG_LIGATURE_RIGHT;
        if (prevJoiningType in[ujtLeftJoining,ujtDualJoining]) and (prevJoiningTypeIndex <> -1) and
          (joiningType in[ujtRightJoining,ujtDualJoining,ujtJoinCausing]) then
          result[prevJoiningTypeIndex].Flags:= result[prevJoiningTypeIndex].Flags or BIDI_FLAG_LIGATURE_LEFT;
      end else
      begin
        if (joiningType in[ujtLeftJoining,ujtDualJoining])
          and (prevJoiningType in[ujtRightJoining,ujtDualJoining,ujtJoinCausing]) then
          result[curIndex].Flags:= result[curIndex].Flags or BIDI_FLAG_LIGATURE_LEFT;
        if (prevJoiningType in[ujtRightJoining,ujtDualJoining]) and (prevJoiningTypeIndex <> -1) and
          (joiningType in[ujtLeftJoining,ujtDualJoining,ujtJoinCausing]) then
          result[prevJoiningTypeIndex].Flags:= result[prevJoiningTypeIndex].Flags or BIDI_FLAG_LIGATURE_RIGHT;
      end;
      if joiningType <> ujtTransparent then
      begin
        prevJoiningType := joiningType;
        prevJoiningTypeIndex:= curIndex;
        prevJoiningTypeBidilevel:= result[curIndex].BidiLevel;
      end;
      curIndex := a[curIndex].nextInIsolate;
    end;
  end;

  procedure AnalyzeSequence(startIndex, afterEndIndex: integer; sos, eos: TUnicodeBidiClass);
  begin
    if afterEndIndex = startIndex then exit;
    ResolveWeakTypes(startIndex, afterEndIndex, sos, eos);
    ResolveBrackets(startIndex, afterEndIndex, sos, eos);
    ResolveNeutrals(startIndex, afterEndIndex, sos, eos);
  end;

  procedure SameLevelRuns(startIndex: integer);
  var
    curBidiLevel: byte;
    latestIndex,curIndex, curStartIndex: Integer;
    curSos,eos: TUnicodeBidiClass;
  begin
    curIndex := startIndex;
    while (curIndex<>-1) and result[curIndex].IsRemoved do
      curIndex := a[curIndex].nextInIsolate;
    if curIndex = -1 then exit;

    curStartIndex:= curIndex;
    curBidiLevel := result[curIndex].bidiLevel;
    if odd(curBidiLevel) then curSos := ubcRightToLeft else curSos := ubcLeftToRight;
    latestIndex := -1;
    while curIndex <> -1 do
    begin
      if not result[curIndex].IsRemoved then
      begin
        if (latestIndex <> -1) and (result[curIndex].bidiLevel <> curBidiLevel) then
        begin
          if result[curIndex].bidiLevel > curBidiLevel then
          begin
            if odd(result[curIndex].bidiLevel) then eos := ubcRightToLeft else eos := ubcLeftToRight;
          end else
          begin
            if odd(curBidiLevel) then eos := ubcRightToLeft else eos := ubcLeftToRight;
          end;

          AnalyzeSequence(curStartIndex, a[latestIndex].nextInIsolate, curSos, eos);

          curSos := eos;
          curBidiLevel:= result[curIndex].bidiLevel;
          curStartIndex:= curIndex;
        end;
        latestIndex := curIndex;
      end;

      if (a[curIndex].nextInIsolate = -1) and (latestIndex<>-1) then
      begin
        if odd(result[latestIndex].bidiLevel) then eos := ubcRightToLeft else eos := ubcLeftToRight;
        AnalyzeSequence(curStartIndex, a[latestIndex].nextInIsolate, curSos, eos);
        break;
      end;

      curIndex := a[curIndex].nextInIsolate;
    end;
  end;

  //analyse bidi formatting of an embedding or an override block
  procedure AnalyzeFormattingBlocks(startIndex, lastIndex: integer; minBidiLevel: byte; formattingCode: LongWord);
  var curIndex, nextIndex, levelIncrease: integer;
    subFormatBeforeStart, subFormatStart, formatNesting: integer;
    subFormatCode: LongWord;
  begin
    case formattingCode of
    UNICODE_LEFT_TO_RIGHT_OVERRIDE,UNICODE_LEFT_TO_RIGHT_EMBEDDING:
      if odd(minBidiLevel) then inc(minBidiLevel);
    UNICODE_RIGHT_TO_LEFT_OVERRIDE,UNICODE_RIGHT_TO_LEFT_EMBEDDING:
      if not odd(minBidiLevel) then inc(minBidiLevel);
    end;
    nextIndex := startIndex;
    repeat
      Assert(nextIndex >= 0, 'Expecting valid index');
      curIndex := nextIndex;
      nextIndex := a[curIndex].nextInIsolate;
      result[curIndex].bidiLevel := minBidiLevel;

      //apply override
      if formattingCode = UNICODE_LEFT_TO_RIGHT_OVERRIDE then a[curIndex].bidiClass := ubcLeftToRight
      else if formattingCode = UNICODE_RIGHT_TO_LEFT_OVERRIDE then a[curIndex].bidiClass := ubcRightToLeft;

      case u[curIndex] of
      UNICODE_LEFT_TO_RIGHT_EMBEDDING, UNICODE_RIGHT_TO_LEFT_EMBEDDING,
      UNICODE_LEFT_TO_RIGHT_OVERRIDE, UNICODE_RIGHT_TO_LEFT_OVERRIDE:
        begin
          result[curIndex].Flags := result[curIndex].Flags OR BIDI_FLAG_REMOVED;
          case u[curIndex] of
            UNICODE_LEFT_TO_RIGHT_OVERRIDE,UNICODE_LEFT_TO_RIGHT_EMBEDDING:
              if odd(minBidiLevel) then levelIncrease := 1
              else levelIncrease := 2;
            UNICODE_RIGHT_TO_LEFT_OVERRIDE,UNICODE_RIGHT_TO_LEFT_EMBEDDING:
              if odd(minBidiLevel) then levelIncrease := 2
              else levelIncrease := 1;
          else levelIncrease:= 2;
          end;
          if minBidiLevel <= UNICODE_MAX_BIDI_DEPTH-levelIncrease-1 then
          begin
            subFormatCode:= u[curIndex];
            subFormatBeforeStart := curIndex;
            subFormatStart := nextIndex;
            formatNesting:= 1;
            while formatNesting > 0 do
            begin
              //sub-format ends because no more chars
              if curIndex = lastIndex then
              begin
                if curIndex <> subFormatBeforeStart then
                  AnalyzeFormattingBlocks(subFormatStart, curIndex, minBidiLevel+levelIncrease, subFormatCode);
                break;
              end;

              Assert(nextIndex >= 0, 'Expecting valid index');
              case u[nextIndex] of
              UNICODE_LEFT_TO_RIGHT_EMBEDDING, UNICODE_RIGHT_TO_LEFT_EMBEDDING,
              UNICODE_LEFT_TO_RIGHT_OVERRIDE, UNICODE_RIGHT_TO_LEFT_OVERRIDE: inc(formatNesting);
              UNICODE_POP_DIRECTIONAL_FORMATTING:
                begin
                  dec(formatNesting);
                  if formatNesting = 0 then
                  begin
                    //sub-format ends because enough matching pop chars found
                    if curIndex <> subFormatBeforeStart then
                      AnalyzeFormattingBlocks(subFormatStart, curIndex, minBidiLevel+levelIncrease, subFormatCode);

                    curIndex := nextIndex;
                    nextIndex := a[curIndex].nextInIsolate;
                    result[curIndex].Flags := result[curIndex].Flags OR BIDI_FLAG_REMOVED;
                    break;
                  end;
                end;
              end;

              curIndex := nextIndex;
              nextIndex := a[curIndex].nextInIsolate;
            end;
          end;
        end;
      UNICODE_POP_DIRECTIONAL_FORMATTING: //ignored when no matching formatting code
        begin
          result[curIndex].Flags := result[curIndex].Flags OR BIDI_FLAG_REMOVED;
        end;
      end;
    until curIndex = lastIndex;
  end;

  procedure ResolveImplicitLevels(startIndex: integer); // rule I1 and I2
  var
    curIndex: Integer;
  begin
    curIndex := startIndex;
    while curIndex <> -1 do
    begin
      case a[curIndex].bidiClass of
      ubcRightToLeft,ubcArabicLetter:
        if not Odd(result[curIndex].bidiLevel) then inc(result[curIndex].bidiLevel);
      ubcEuropeanNumber,ubcArabicNumber:
        if Odd(result[curIndex].bidiLevel) then inc(result[curIndex].bidiLevel)
        else inc(result[curIndex].bidiLevel, 2);
      ubcLeftToRight: if Odd(result[curIndex].bidiLevel) then inc(result[curIndex].bidiLevel);
      end;
      curIndex := a[curIndex].nextInIsolate;
    end;
  end;

  procedure ResetEndOfParagraphLevels(startIndex: integer);  // rule L1
  var
    prevIndex,curIndex: Integer;

    procedure TweakWhiteSpaceBefore(index: integer);
    var
      isWhiteSpaceOrIsolate: boolean;
    begin
      while index <> -1 do
      begin
        case u[index] of
        UNICODE_FIRST_STRONG_ISOLATE, UNICODE_POP_DIRECTIONAL_ISOLATE,
        UNICODE_LEFT_TO_RIGHT_ISOLATE, UNICODE_RIGHT_TO_LEFT_ISOLATE:
          isWhiteSpaceOrIsolate:= true;
        else
          isWhiteSpaceOrIsolate:= GetUnicodeBidiClass(u[index]) = ubcWhiteSpace;
        end;
        if isWhiteSpaceOrIsolate then
          result[index].bidiLevel := result[index].ParagraphBidiLevel
        else
          break;
        index := a[index].prevInIsolate;
      end;
    end;

  begin
    prevIndex := -1;
    curIndex := startIndex;
    while curIndex <> -1 do
    begin
      case GetUnicodeBidiClass(u[curIndex]) of
        ubcSegmentSeparator, ubcParagraphSeparator:
        begin
          result[curIndex].bidiLevel := result[curIndex].ParagraphBidiLevel;
          TweakWhiteSpaceBefore(prevIndex);
        end;
      end;
      prevIndex := curIndex;
      curIndex := a[curIndex].nextInIsolate;
    end;
    TweakWhiteSpaceBefore(prevIndex);
  end;

  function DetermineIsolateDirectionFromFirstStrongClass(startIndex: integer): LongWord;
  var
    curIndex: Integer;
  begin
    curIndex := startIndex;
    while curIndex <> -1 do
    begin
      Assert(curIndex >= 0, 'Expecting valid index');
      case a[curIndex].bidiClass of
        ubcLeftToRight: exit(UNICODE_LEFT_TO_RIGHT_ISOLATE);
        ubcRightToLeft,ubcArabicLetter: exit(UNICODE_RIGHT_TO_LEFT_ISOLATE);
      end;
      case u[curIndex] of
        UNICODE_LEFT_TO_RIGHT_OVERRIDE: exit(UNICODE_LEFT_TO_RIGHT_ISOLATE);
        UNICODE_RIGHT_TO_LEFT_OVERRIDE: exit(UNICODE_RIGHT_TO_LEFT_ISOLATE);
      end;
      curIndex := a[curIndex].nextInIsolate;
    end;
    result := UNICODE_LEFT_TO_RIGHT_ISOLATE;
  end;

  procedure LinkCharsInIsolate(startIndex: integer; charCount: integer; out endIndex : integer);
  var
    curIndex,isolateStackPos,
    prevIndex: Integer;
  begin
    a[startIndex].prevInIsolate := -1;
    prevIndex := -1;
    curIndex := startIndex;
    isolateStackPos:= 0;
    while curIndex < startIndex+charCount do
    begin
      if u[curIndex] = UNICODE_POP_DIRECTIONAL_ISOLATE then
        if isolateStackPos > 0 then dec(isolateStackPos);

      if isolateStackPos = 0 then
      begin
        if prevIndex<>-1 then a[prevIndex].nextInIsolate := curIndex;
        a[curIndex].prevInIsolate := prevIndex;

        prevIndex := curIndex;
      end;

      case u[curIndex] of
      UNICODE_LEFT_TO_RIGHT_ISOLATE, UNICODE_RIGHT_TO_LEFT_ISOLATE, UNICODE_FIRST_STRONG_ISOLATE: inc(isolateStackPos);
      end;
      inc(curIndex);
    end;
    a[prevIndex].nextInIsolate := -1;
    endIndex := prevIndex;
  end;

  //split isolates in order to format them independently
  procedure AnalyzeIsolates(startIndex: integer; charCount: integer; isolateDirection: LongWord; minBidiLevel: byte = 0;
                            isParagraph: boolean = false);
  var curIndex, endIndex: integer;
    nextIndex: integer;
    subBidiLevel, levelIncrease: byte;
    subIsolateStart: integer;
    subIsolateDirection: LongWord;
  begin
    if charCount = 0 then exit;
    Assert(startIndex>=0, 'Invalid start index');

    LinkCharsInIsolate(startIndex, charCount, endIndex);

    if isolateDirection = UNICODE_FIRST_STRONG_ISOLATE then
      isolateDirection := DetermineIsolateDirectionFromFirstStrongClass(startIndex);

    case isolateDirection of
    UNICODE_LEFT_TO_RIGHT_ISOLATE: if Odd(minBidiLevel) then inc(minBidiLevel);
    UNICODE_RIGHT_TO_LEFT_ISOLATE: if not Odd(minBidiLevel) then inc(minBidiLevel);
    else
      raise EInvalidOperation.Create('Unknown isolate direction');
    end;

    if isParagraph then
    begin
      curIndex := startIndex;
      while curIndex <> -1 do
      begin
        result[curIndex].ParagraphBidiLevel := minBidiLevel;
        curIndex := a[curIndex].nextInIsolate;
      end;
    end;

    case isolateDirection of
    UNICODE_LEFT_TO_RIGHT_ISOLATE: AnalyzeFormattingBlocks(startIndex, endIndex, minBidiLevel, UNICODE_LEFT_TO_RIGHT_EMBEDDING);
    UNICODE_RIGHT_TO_LEFT_ISOLATE: AnalyzeFormattingBlocks(startIndex, endIndex, minBidiLevel, UNICODE_RIGHT_TO_LEFT_EMBEDDING);
    end;

    SameLevelRuns(startIndex);
    ResolveImplicitLevels(startIndex);
    ResolveLigature(startIndex);

    if isParagraph then
      ResetEndOfParagraphLevels(startIndex);

    //analyse sub-isolates
    curIndex := startIndex;
    while curIndex <> -1 do
    begin
      Assert(curIndex >= 0, 'Expecting valid index');
      case u[curIndex] of
      UNICODE_LEFT_TO_RIGHT_ISOLATE, UNICODE_RIGHT_TO_LEFT_ISOLATE, UNICODE_FIRST_STRONG_ISOLATE:
        begin
          subBidiLevel := result[curIndex].bidiLevel;
          nextIndex := a[curIndex].nextInIsolate;
          if nextIndex <> -1 then
          begin
            if result[nextIndex].bidiLevel > subBidiLevel then
              subBidiLevel:= result[nextIndex].bidiLevel;
          end;
          if ((isolateDirection = UNICODE_LEFT_TO_RIGHT_ISOLATE) and
             (u[curIndex] = UNICODE_RIGHT_TO_LEFT_ISOLATE)) or
             ((isolateDirection = UNICODE_LEFT_TO_RIGHT_ISOLATE) and
             (u[curIndex] = UNICODE_RIGHT_TO_LEFT_ISOLATE)) then
            levelIncrease := 1
          else
            levelIncrease:= 2;
          if subBidiLevel+levelIncrease <= UNICODE_MAX_BIDI_DEPTH-1 then
          begin
            subIsolateDirection := u[curIndex];
            subIsolateStart:= curIndex+1;
            curIndex := nextIndex;

            //sub-isolates ends because no more chars
            if curIndex = -1 then
            begin
              AnalyzeIsolates(subIsolateStart, startIndex+charCount-subIsolateStart, subIsolateDirection, subBidiLevel+1);
              break;
            end else
            begin
              AnalyzeIsolates(subIsolateStart, curIndex-subIsolateStart, subIsolateDirection, subBidiLevel+1);
              continue;
            end;
          end;
        end;
      end;
      curIndex := a[curIndex].nextInIsolate;
    end;
  end;

  //split UTF8 string into paragraphs
  procedure SplitParagraphs;
  var
    lineStartIndex, curIndex: integer;
  begin
    curIndex := 0;
    lineStartIndex := curIndex;
    while curIndex < ALength do
    begin
      if a[curIndex].bidiClass = ubcParagraphSeparator then
      begin
        //skip second CRLF char
        if IsUnicodeCrLf(u[curIndex]) and (curIndex+1 < ALength) and
           IsUnicodeCrLf(u[curIndex+1]) and (u[curIndex+1] <> u[curIndex]) then
        begin
          inc(curIndex);
          result[curIndex].Flags := result[curIndex].Flags and not BIDI_FLAG_MULTICHAR_START;
        end;

        result[curIndex].Flags := result[curIndex].Flags or BIDI_FLAG_EXPLICIT_END_OF_PARAGRAPH;

        AnalyzeIsolates(lineStartIndex, curIndex+1-lineStartIndex, baseDirection, 0, true);
        lineStartIndex := curIndex+1;
      end;
      inc(curIndex);
    end;
    if curIndex > lineStartIndex then
    begin
      result[curIndex-1].Flags := result[curIndex-1].Flags or BIDI_FLAG_IMPLICIT_END_OF_PARAGRAPH;
      AnalyzeIsolates(lineStartIndex, curIndex-lineStartIndex, baseDirection, 0, true);
    end;
  end;

var i: integer;
  classEx: TUnicodeBidiClass;
begin
  setlength(a, ALength);
  setlength(result, ALength);
  if ALength > 0 then
  begin
    for i := 0 to high(a) do
    begin
      classEx := GetUnicodeBidiClassEx(u[i]);
      case classEx of
      ubcMirroredNeutral:
        begin
          result[i].Flags := result[i].Flags or BIDI_FLAG_MIRRORED;
          a[i].bidiClass := ubcOtherNeutrals;
        end;
      ubcCombiningLeftToRight:
        begin
          case GetUnicodeCombiningClass(u[i]) of
          208, 224: result[i].Flags := result[i].Flags OR BIDI_FLAG_COMBINING_LEFT;
          210, 226, 9: result[i].Flags := result[i].Flags OR BIDI_FLAG_COMBINING_RIGHT;
          0: result[i].Flags := result[i].Flags OR BIDI_FLAG_COMBINING_LEFT OR BIDI_FLAG_COMBINING_RIGHT;
          end;
          a[i].bidiClass := ubcLeftToRight;
        end;
      otherwise
        a[i].bidiClass := classEx;
      end;
      case u[i] of
      UNICODE_LINE_SEPARATOR: //line separator within paragraph
        result[i].Flags := result[i].Flags or BIDI_FLAG_END_OF_LINE;
      UNICODE_ZERO_WIDTH_JOINER, UNICODE_ZERO_WIDTH_NON_JOINER:
        result[i].Flags := result[i].Flags OR BIDI_FLAG_LIGATURE_BOUNDARY;
      end;
      case a[i].bidiClass of
      ubcArabicLetter,ubcArabicNumber,ubcRightToLeft:
        result[i].Flags := result[i].Flags OR BIDI_FLAG_RTL_SCRIPT;
      ubcNonSpacingMark: result[i].Flags := result[i].Flags OR BIDI_FLAG_NON_SPACING_MARK;
      end;
      if (result[i].Flags and (BIDI_FLAG_NON_SPACING_MARK or BIDI_FLAG_COMBINING_LEFT
                               or BIDI_FLAG_COMBINING_RIGHT) = 0) or
        (i = 0) or (a[i-1].bidiClass in [ubcSegmentSeparator, ubcParagraphSeparator]) then
        result[i].Flags := result[i].Flags OR BIDI_FLAG_MULTICHAR_START;
    end;
    SplitParagraphs;
  end;
end;

function AnalyzeBidiUnicode(u: PLongWord; ALength: integer;
  ABidiMode: TFontBidiMode): TUnicodeBidiArray;
begin
  case ABidiMode of
    fbmLeftToRight: result := AnalyzeBidiUnicode(u, ALength, UNICODE_LEFT_TO_RIGHT_ISOLATE);
    fbmRightToLeft: result := AnalyzeBidiUnicode(u, ALength, UNICODE_RIGHT_TO_LEFT_ISOLATE);
  else
    {fbmAuto} result := AnalyzeBidiUnicode(u, ALength, UNICODE_FIRST_STRONG_ISOLATE);
  end;
end;

function GetUnicodeDisplayOrder(const AInfo: TUnicodeBidiArray): TUnicodeDisplayOrder;
begin
  if length(AInfo)=0 then
    result := nil
  else
    result := GetUnicodeDisplayOrder(@AInfo[0], sizeof(TUnicodeBidiInfo), length(AInfo));
end;

function GetUnicodeDisplayOrder(ALevels: PByte; ACount: integer): TUnicodeDisplayOrder;

  procedure DetermineDisplayOrderRec(AOffset: integer; AStartIndex, ABlockCount: integer; AEmbeddingLevel: byte);
  var minLevel: byte;
    blockIndex,subStartIndex,subCount, subOffset: integer;
  begin
    //writeln('DetermineDisplayOrderRec('+inttostr(AOffset)+'/'+inttostr(ACount)+',' + inttostr(AStartIndex) +',*' +inttostr(ABlockCount)+','+inttostr(AEmbeddingLevel)+')');
    blockIndex := 0;
    subStartIndex := 0; //avoid warning
    while blockIndex < ABlockCount do
    begin
      Assert(AOffset < ACount, 'Offset out of bounds');
      if ALevels[AOffset] = AEmbeddingLevel then
      begin
        if odd(AEmbeddingLevel) then
          result[AStartIndex+ABlockCount-1-blockIndex] := AOffset
        else
          result[AStartIndex+blockIndex] := AOffset;
        inc(AOffset);
        inc(blockIndex);
      end else
      begin
        if not odd(AEmbeddingLevel) then
          subStartIndex := AStartIndex+blockIndex;
        subOffset := AOffset;
        minLevel := ALevels[AOffset];
        inc(AOffset);
        inc(blockIndex);
        subCount := 1;
        while true do
        begin
          if (blockIndex < ABlockCount) and (ALevels[AOffset] > AEmbeddingLevel) then
          begin
            Assert(AOffset < ACount, 'Offset out of bounds');
            if ALevels[AOffset] < minLevel then
              minLevel:= ALevels[AOffset];
            inc(AOffset);
            inc(blockIndex);
            inc(subCount);
          end else
          begin
            if odd(AEmbeddingLevel) then
              subStartIndex := AStartIndex+ABlockCount-1-(blockIndex-1);
            DetermineDisplayOrderRec(subOffset, subStartIndex, subCount, minLevel);
            break;
          end;
        end;
      end;
    end;
  end;

begin
  setlength(result, ACount);
  DetermineDisplayOrderRec(0, 0, ACount, 0);
end;

function GetUnicodeDisplayOrder(ABidiInfo: PUnicodeBidiInfo; AStride, ACount: integer): TUnicodeDisplayOrder;
var
  levels: packed array of byte;
  originalIndices: array of integer;
  index,len, i: integer;
  p: PByte;
begin
  len := 0;
  p := PByte(ABidiInfo);
  for i := 0 to ACount-1 do
  begin
    if not PUnicodeBidiInfo(p)^.IsRemoved then inc(len);
    inc(p, AStride);
  end;
  if len = 0 then
    result := nil
  else
  begin
    setlength(levels, len);
    setlength(originalIndices, len);
    p := PByte(ABidiInfo);
    index := 0;
    for i := 0 to ACount-1 do
    begin
      if not PUnicodeBidiInfo(p)^.IsRemoved then
      begin
        levels[index] := PUnicodeBidiInfo(p)^.BidiLevel;
        originalIndices[index] := i;
        inc(index);
      end;
      inc(p, AStride);
    end;
    result := GetUnicodeDisplayOrder(@levels[0], len);
    for i := 0 to len-1 do
      result[i] := originalIndices[result[i]];
  end;
end;

end.

