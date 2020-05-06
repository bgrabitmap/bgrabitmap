unit BGRAFreeType;

{$mode objfpc}{$H+}

{
  Font rendering units : BGRAText, BGRATextFX, BGRAVectorize, BGRAFreeType

  This units provide a font renderer with FreeType fonts, using the integrated FreeType font engine in Lazarus.
  The simplest way to render effects is to use TBGRAFreeTypeFontRenderer class.
  To do this, create an instance of this class and assign it to a TBGRABitmap.FontRenderer property. Now functions
  to draw text like TBGRABitmap.TextOut will use the chosen renderer.

  >> Note that you need to define the default FreeType font collection
  >> using EasyLazFreeType unit.

  To set the effects, keep a variable containing
  the TBGRAFreeTypeFontRenderer class and modify ShadowVisible and other effects parameters. The FontHinted property
  allows you to choose if the font is snapped to pixels to make it more readable.

  TBGRAFreeTypeDrawer class is the class that provides basic FreeType drawing
  by deriving the TFreeTypeDrawer type. You can use it directly, but it is not
  recommended, because there are less text layout parameters. However, it is
  necessary if you want to create TBGRATextEffect objects using FreeType fonts.
}

interface

{$i bgrabitmap.inc}

uses
  BGRAClasses, SysUtils, BGRAGraphics, BGRABitmapTypes, EasyLazFreeType, FPimage,
  BGRACustomTextFX, BGRAPhongTypes, BGRATypewriter;

type
  TBGRAFreeTypeDrawer = class;

  //this is the class to assign to FontRenderer property of TBGRABitmap
  { TBGRAFreeTypeFontRenderer }

  TBGRAFreeTypeFontRenderer = class(TBGRACustomFontRenderer)
  private
    FDrawer: TBGRAFreeTypeDrawer;
    FFont: TFreeTypeFont;
    FLastFontSize: single;
    function GetCollection: TCustomFreeTypeFontCollection;
    function GetDrawer(ASurface: TBGRACustomBitmap): TBGRAFreeTypeDrawer;
    function GetShaderLightPosition: TPoint;
    function GetShaderLightPositionF: TPointF;
    procedure SetShaderLightPosition(const AValue: TPoint);
    procedure SetShaderLightPositionF(const AValue: TPointF);
  protected
    FShaderOwner: boolean;
    FShader: TCustomPhongShading;
    FTypeWriter: TBGRACustomTypeWriter;
    function GetTypeWriter: TBGRACustomTypeWriter;
    procedure UpdateFont(ADisableClearType: boolean = false);
    procedure Init;
    procedure TextOutAnglePatch(ADest: TBGRACustomBitmap; x, y: single; orientation: integer; s: string;
              c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment);
    procedure InternalTextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; c: TBGRAPixel; align: TAlignment);
    property TypeWriter: TBGRACustomTypeWriter read GetTypeWriter;
  public
    FontHinted: boolean;

    ShaderActive: boolean;

    ShadowVisible: boolean;
    ShadowColor: TBGRAPixel;
    ShadowRadius: integer;
    ShadowOffset: TPoint;
    ShadowQuality: TRadialBlurType;

    OutlineColor: TBGRAPixel;
    OutlineVisible,OuterOutlineOnly: boolean;
    OutlineTexture: IBGRAScanner;

    constructor Create; overload;
    constructor Create(AShader: TCustomPhongShading; AShaderOwner: boolean); overload;
    function GetFontPixelMetric: TFontPixelMetric; override;
    function GetFontPixelMetricF: TFontPixelMetricF; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientation: integer; s: string; c: TBGRAPixel; align: TAlignment); overload; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientation: integer; s: string; texture: IBGRAScanner; align: TAlignment); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; texture: IBGRAScanner; align: TAlignment); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; c: TBGRAPixel; align: TAlignment); overload; override;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; s: string; style: TTextStyle; c: TBGRAPixel); overload; override;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; s: string; style: TTextStyle; texture: IBGRAScanner); overload; override;
    function TextSize(sUTF8: string): TSize; overload; override;
    function TextSizeF(sUTF8: string): TPointF; overload; override;
    function TextSize(sUTF8: string; AMaxWidth: integer; {%H-}ARightToLeft: boolean): TSize; overload; override;
    function TextSizeF(sUTF8: string; AMaxWidthF: single; {%H-}ARightToLeft: boolean): TPointF; overload; override;
    function TextFitInfo(sUTF8: string; AMaxWidth: integer): integer; override;
    function TextFitInfoF(sUTF8: string; AMaxWidthF: single): integer; override;
    destructor Destroy; override;
    property Collection: TCustomFreeTypeFontCollection read GetCollection;
    property ShaderLightPosition: TPoint read GetShaderLightPosition write SetShaderLightPosition;
    property ShaderLightPositionF: TPointF read GetShaderLightPositionF write SetShaderLightPositionF;
  end;

  { TBGRAFreeTypeDrawer }

  TBGRAFreeTypeDrawer = class(TFreeTypeDrawer)
  private
    FMask: TBGRACustomBitmap;
    FColor: TBGRAPixel;
    FInCreateTextEffect: boolean;
    procedure RenderDirectly(x, y, tx: integer; data: pointer);
    procedure RenderDirectlyClearType(x, y, tx: integer; data: pointer);
    function ShadowActuallyVisible :boolean;
    function OutlineActuallyVisible: boolean;
    function ShaderActuallyActive : boolean;
  public
    Destination: TBGRACustomBitmap;
    ClearTypeRGBOrder: boolean;
    Texture: IBGRAScanner;

    Shader: TCustomPhongShading;
    ShaderActive: boolean;

    ShadowVisible: boolean;
    ShadowColor: TBGRAPixel;
    ShadowRadius: integer;
    ShadowOffset: TPoint;
    ShadowQuality: TRadialBlurType;

    OutlineColor: TBGRAPixel;
    OutlineVisible,OuterOutlineOnly: boolean;
    OutlineTexture: IBGRAScanner;

    constructor Create(ADestination: TBGRACustomBitmap);
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TFPColor); overload; override;
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TBGRAPixel); overload;
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TBGRAPixel; AAlign: TFreeTypeAlignments); overload;
    { If this code does not compile, you probably have an older version of Lazarus. To fix the problem,
      go into "bgrabitmap.inc" and comment the compiler directives }
    {$IFDEF BGRABITMAP_USE_LCL12}
    procedure DrawTextWordBreak(AText: string; AFont: TFreeTypeRenderableFont; x, y, AMaxWidth: Single; AColor: TBGRAPixel; AAlign: TFreeTypeAlignments); overload;
    procedure DrawTextRect(AText: string; AFont: TFreeTypeRenderableFont; X1,Y1,X2,Y2: Single; AColor: TBGRAPixel; AAlign: TFreeTypeAlignments); overload;
    {$ENDIF}
    {$IFDEF BGRABITMAP_USE_LCL15}
    procedure DrawGlyph(AGlyph: integer; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TFPColor); overload; override;
    procedure DrawGlyph(AGlyph: integer; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TBGRAPixel); overload;
    procedure DrawGlyph(AGlyph: integer; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TBGRAPixel; AAlign: TFreeTypeAlignments); overload;
    {$ENDIF}
    function CreateTextEffect(AText: string; AFont: TFreeTypeRenderableFont): TBGRACustomTextEffect;
    destructor Destroy; override;
  end;


implementation

uses BGRABlend, Math, BGRATransform, BGRAUnicode, BGRAUTF8;

{$i generatedutf8.inc}

const
  ArabicMarkAbove: array[0..10] of LongWord =
    ($0618, $0619, $064B, $064C, $064E, $064F,
     $0651, $0652, $0670, $08F0, $08F1);

  ArabicMarkBelow: array[0..3] of LongWord =
    ($061A, $064D, $0650, $08F2);

type
  TMarkFallback = record
    NonSpacing: LongWord;
    Spacing: LongWord;
    Moved: boolean;
  end;

const
  MarkFallback: array[0..36] of TMarkFallback = (
  (NonSpacing: $300; Spacing: $2CA; Moved: false),
  (NonSpacing: $301; Spacing: $B4; Moved: false),
  (NonSpacing: $302; Spacing: $5E; Moved: false),
  (NonSpacing: $303; Spacing: $2DC; Moved: false),
  (NonSpacing: $304; Spacing: $AF; Moved: false),
  (NonSpacing: $305; Spacing: $203E; Moved: false),
  (NonSpacing: $306; Spacing: $2D8; Moved: false),
  (NonSpacing: $307; Spacing: $2D9; Moved: false),
  (NonSpacing: $308; Spacing: $A8; Moved: false),
  (NonSpacing: $30A; Spacing: $2DA; Moved: false),
  (NonSpacing: $30B; Spacing: $2DD; Moved: false),
  (NonSpacing: $30E; Spacing: $22; Moved: false),
  (NonSpacing: $313; Spacing: $1FBD; Moved: false),
  (NonSpacing: $314; Spacing: $1FFE; Moved: false),
  (NonSpacing: $316; Spacing: $2CA; Moved: true),
  (NonSpacing: $317; Spacing: $B4; Moved: true),
  (NonSpacing: $320; Spacing: $AF; Moved: true),
  (NonSpacing: $324; Spacing: $A8; Moved: true),
  (NonSpacing: $325; Spacing: $2DA; Moved: true),
  (NonSpacing: $327; Spacing: $B8; Moved: false),
  (NonSpacing: $328; Spacing: $2DB; Moved: false),
  (NonSpacing: $32D; Spacing: $5E; Moved: true),
  (NonSpacing: $32E; Spacing: $2D8; Moved: true),
  (NonSpacing: $330; Spacing: $2DC; Moved: true),
  (NonSpacing: $331; Spacing: $AF; Moved: true),
  (NonSpacing: $332; Spacing: $203E; Moved: true),
  (NonSpacing: $333; Spacing: $2017; Moved: false),
  (NonSpacing: $33F; Spacing: $2017; Moved: true),
  (NonSpacing: $342; Spacing: $1FC0; Moved: false),
  (NonSpacing: $340; Spacing: $2CA; Moved: false),
  (NonSpacing: $341; Spacing: $B4; Moved: false),
  (NonSpacing: $342; Spacing: $2DC; Moved: false),
  (NonSpacing: $343; Spacing: $1FBD; Moved: false),
  (NonSpacing: $345; Spacing: $37A; Moved: false),
  (NonSpacing: $348; Spacing: $22; Moved: true),
  (NonSpacing: $3099; Spacing: $309B; Moved: false),
  (NonSpacing: $309A; Spacing: $309C; Moved: false));

function IsArabicMarkAbove(u: LongWord): boolean;
var
  i: Integer;
begin
  for i := 0 to high(ArabicMarkAbove) do
    if ArabicMarkAbove[i] = u then exit(true);
  result := false;
end;

function IsArabicMarkBelow(u: LongWord): boolean;
var
  i: Integer;
begin
  for i := 0 to high(ArabicMarkBelow) do
    if ArabicMarkBelow[i] = u then exit(true);
  result := false;
end;

procedure RecomposeUTF8(AFont: TFreeTypeFont; ADecomposed: string; out ARecomposed: string; out AMarks, AInnerMarks: string);
var
  joinBefore, joinAfter: boolean;
  lookFor: string;

  function FindChars(AText: string): boolean;
  var
    p, charLen: Integer;
    u: LongWord;
  begin
    if AFont = nil then exit(true);

    p := 1;
    while p <= length(AText) do
    begin
      charLen := UTF8CharacterLength(@AText[p]);
      u := UTF8CodepointToUnicode(@AText[p], charLen);
      if AFont.CharIndex[u] = 0 then exit(false);
      inc(p, charLen);
    end;
    result := true;
  end;

  function RecomposeRec(AMin,AMax: integer): boolean;

    procedure TryExactMatch;
    var
      i, extra: Integer;
      newExtra: String;
    begin
      for i := AMin to AMax do
        if UTF8Decomposition[i].de = ARecomposed then
        begin
          if UTF8Decomposition[i].join <> arNone then
            if (joinBefore xor (UTF8Decomposition[i].join in[arMedial,arFinal])) or
               (joinAfter xor (UTF8Decomposition[i].join in[arInitial,arMedial])) then continue;
          if not FindChars(UTF8Decomposition[i].re) then continue;
          ARecomposed := UTF8Decomposition[i].re;
          result := true;
          exit;
        end;
      for i := AMin to AMax do
        if ARecomposed.StartsWith(UTF8Decomposition[i].de, true) then
        begin
          extra := length(ARecomposed) - length(UTF8Decomposition[i].de);
          if UTF8Decomposition[i].join <> arNone then
            if (joinBefore xor (UTF8Decomposition[i].join in[arMedial,arFinal])) or
               (joinAfter xor (UTF8Decomposition[i].join in[arInitial,arMedial])) then continue;
          if not FindChars(UTF8Decomposition[i].re) then continue;
          newExtra := copy(ARecomposed, length(ARecomposed)+1-extra, extra);
          if GetFirstStrongBidiClassUTF8(newExtra) <> ubcUnknown then continue;
          AMarks := newExtra + AMarks;
          ARecomposed := UTF8Decomposition[i].re;
          result := true;
          exit;
        end;
      result := false;
    end;

  var i,j: integer;
  begin
    if AMax <= AMin+9 then
    begin
      TryExactMatch;
    end else
    begin
      i := (AMin+AMax) div 2;
      if UTF8Decomposition[i].de.StartsWith(lookFor, true) then
      begin
        j := i;
        while (j > AMin) and UTF8Decomposition[j-1].de.StartsWith(lookFor, true) do dec(j);
        AMin := j;
        j := i;
        while (j < AMax) and UTF8Decomposition[j+1].de.StartsWith(lookFor, true) do inc(j);
        AMax := j;
        TryExactMatch;
      end else
      if CompareStr(lookFor, UTF8Decomposition[i].de) > 0 then
        result := RecomposeRec(i+1, AMax)
      else
        result := RecomposeRec(AMin, i-1);
    end;
  end;

  procedure ExtractInnerMarks;
  var
    p, charLen, pStart: Integer;
    u: LongWord;
  begin
    if ARecomposed.StartsWith(UTF8_ARABIC_LAM, true) then
    begin
      pStart := length(UTF8_ARABIC_LAM)+1;
      p := pStart;
      while p <= length(ARecomposed) do
      begin
        charLen := UTF8CharacterLength(@ARecomposed[p]);
        u := UTF8CodepointToUnicode(@ARecomposed[p], charLen);
        if GetUnicodeBidiClass(u) = ubcNonSpacingMark then
          inc(p, charLen)
          else break;
      end;
      if p>pStart then
      begin
        AppendStr(AInnerMarks, copy(ARecomposed, pStart, p-pStart));
        delete(ARecomposed, pStart, p-pStart);
      end;
    end;
  end;

  procedure ExtractFinalMarks;
  var
    p,pStart,pPrev: Integer;
  begin
    pStart := length(ARecomposed)+1;
    p := pStart;
    while p > 1 do
    begin
      pPrev := p;
      dec(p);
      while (p > 1) and (ARecomposed[p] in[#$80..#$BF]) do dec(p);
      if (p = 1) or (ARecomposed[p] in [#$80..#$BF]) or
        (GetUnicodeBidiClass(UTF8CodepointToUnicode(@ARecomposed[p], pPrev-p)) <> ubcNonSpacingMark) then
      begin
        p := pPrev;
        break;
      end;
    end;
    if p < pStart then
    begin
      AMarks := copy(ARecomposed, p, pStart-p) + AMarks;
      delete(ARecomposed, p, pStart-p);
    end;
  end;

begin
  joinBefore := ADecomposed.StartsWith(UTF8_ZERO_WIDTH_JOINER, true);
  joinAfter := ADecomposed.EndsWith(UTF8_ZERO_WIDTH_JOINER, true);
  if joinBefore and joinAfter then
    ADecomposed := copy(ADecomposed, length(UTF8_ZERO_WIDTH_JOINER)+1,
                   length(ADecomposed) - (length(UTF8_ZERO_WIDTH_JOINER) shl 1)) else
  if joinBefore then Delete(ADecomposed, 1, length(UTF8_ZERO_WIDTH_JOINER)) else
  if joinAfter then Delete(ADecomposed, length(ADecomposed) - length(UTF8_ZERO_WIDTH_JOINER) + 1, length(UTF8_ZERO_WIDTH_JOINER));

  ARecomposed := ADecomposed;
  AMarks := '';
  AInnerMarks := '';
  ExtractInnerMarks;
  repeat
    if length(ADecomposed)<=1 then break;
    lookFor := copy(ARecomposed, 1, UTF8CharacterLength(@ARecomposed[1]));
  until not RecomposeRec(0, high(UTF8Decomposition));
  ExtractFinalMarks;
end;

type
  { TBGRAFreeTypeGlyph }

  TBGRAFreeTypeGlyph = class(TBGRAGlyph)
  public
    Font: TFreeTypeFont;
    Recomposed: string;
    Marks, InnerMarks: TUnicodeArray;
    Bounds: TRect;
    constructor Create(AFont: TFreeTypeFont; AIdentifier: string);
    constructor Create(AIdentifier: string); override;
  end;

  { TFreeTypeTypeWriter }

  TFreeTypeTypeWriter = class(TBGRACustomTypeWriter)
  protected
    FFont: TFreeTypeFont;
    function GetGlyph(AIdentifier: string): TBGRAGlyph; override;
  public
    constructor Create(AFont: TFreeTypeFont);
    procedure DrawText(ADrawer: TBGRAFreeTypeDrawer; ATextUTF8: string; X,Y: Single;
              AColor: TBGRAPixel; AAlign: TBGRATypeWriterAlignment = twaTopLeft); overload;
  end;

{ TFreeTypeGlyph }

constructor TBGRAFreeTypeGlyph.Create(AFont: TFreeTypeFont; AIdentifier: string);
var
  marksStr, innerMarksStr: string;
  ofs: TIntegerArray;

  procedure SortMarks(A: TUnicodeArray);
    procedure MoveBefore(AFrom, ATo: integer);
    var k: integer;
      backU: LongWord;
    begin
      if ATo >= AFrom then exit;
      backU := A[AFrom];
      for k := AFrom downto ATo+1 do
        A[k] := A[k-1];
      A[ATo] := backU;
    end;

    procedure SortByCombiningClass;
    var
      start, i, j: Integer;
      newCC: Byte;
    begin
      start := 0;
      i := start+1;
      while i <= high(a) do
      begin
        //sequence is split
        if A[i] = UNICODE_COMBINING_GRAPHEME_JOINER then
        begin
          start := i+1;
          i := start+1;
          continue;
        end else
        begin
          newCC := GetUnicodeCombiningClass(A[i]);
          j := i;
          while (j > start) and (newCC < GetUnicodeCombiningClass(A[j-1])) do dec(j);
          MoveBefore(i, j);
          inc(i);
        end;
      end;
    end;

    procedure PutShaddaFirst;
    var
      i, j: Integer;
    begin
      j := 0;
      for i := 0 to high(A) do
        if A[i] = UNICODE_COMBINING_GRAPHEME_JOINER then
          j := i+1 else
        if GetUnicodeCombiningClass(A[i]) = 33 then
        begin
          MoveBefore(i, j);
          inc(j);
        end;
    end;

    procedure PutLeadingMCMFirst(ACombiningClass: byte);
    var
      i, j: Integer;
    begin
      j := 0;
      i := 0;
      while i <= high(A) do
      begin
        if A[i] = UNICODE_COMBINING_GRAPHEME_JOINER then
        begin
          j := i+1;
          inc(i);
        end else
        if GetUnicodeCombiningClass(A[i]) = ACombiningClass then
        begin
          //put leading MCM first
          while IsModifierCombiningMark(A[i]) do
          begin
            MoveBefore(i, j);
            inc(j);
            inc(i);
            if (i >= length(A)) or
              not (GetUnicodeCombiningClass(A[i]) = ACombiningClass) then
                break;
          end;
          //skip rest of combining class
          while (i <= high(A)) and (GetUnicodeCombiningClass(A[i]) = ACombiningClass) do
            inc(i);
        end else
          inc(i);
      end;
    end;

  begin
    if A = nil then exit;
    SortByCombiningClass;
    PutShaddaFirst;
    PutLeadingMCMFirst(230);
    PutLeadingMCMFirst(220);
  end;

begin
  inherited Create(AIdentifier);
  Font := AFont;
  RecomposeUTF8(Font, AIdentifier, Recomposed, marksStr, innerMarksStr);
  UTF8ToUnicodeArray(marksStr, Marks, ofs);
  SortMarks(Marks);
  UTF8ToUnicodeArray(innerMarksStr, InnerMarks, ofs);
  SortMarks(InnerMarks);
end;

constructor TBGRAFreeTypeGlyph.Create(AIdentifier: string);
begin
  Create(nil, AIdentifier);
end;

{ TFreeTypeTypeWriter }

function TFreeTypeTypeWriter.GetGlyph(AIdentifier: string): TBGRAGlyph;
var
  g: TBGRAFreeTypeGlyph;
  u: LongWord;
  glyphIndex: LongInt;
  ftGlyph: TFreeTypeGlyph;
begin
  Result:= inherited GetGlyph(AIdentifier);
  if result = nil then
  begin
    g := TBGRAFreeTypeGlyph.Create(FFont, AIdentifier);
    g.Width := FFont.TextWidth(g.Recomposed);
    g.Height := FFont.LineFullHeight;
    SetGlyph(AIdentifier, g);
    g.Bounds := EmptyRect;
    if length(g.Recomposed) <> 0 then
    begin
      u := UTF8CodepointToUnicode(@g.Recomposed[1], UTF8CharacterLength(@g.Recomposed[1]));
      glyphIndex := FFont.CharIndex[u];
      if glyphIndex <> 0 then
      begin
        ftGlyph := FFont.Glyph[glyphIndex];
        g.Bounds := ftGlyph.Bounds;
      end;
    end;
    result := g;
  end;
end;

constructor TFreeTypeTypeWriter.Create(AFont: TFreeTypeFont);
begin
  inherited Create;
  FFont := AFont;
  SubstituteBidiBracket:= true;
end;

procedure TFreeTypeTypeWriter.DrawText(ADrawer: TBGRAFreeTypeDrawer;
  ATextUTF8: string; X, Y: Single; AColor: TBGRAPixel; AAlign: TBGRATypeWriterAlignment);
const GlyphBoxFixed = true;
var
  i,j : Integer;
  ptGlyph: TPointF;
  di: TBGRATextDisplayInfo;
  markGlyphIndex: LongInt;
  markFreetypeGlyph: TFreeTypeGlyph;
  markGlyphBounds: TRect;
  markCombining: byte;
  aboveOfs, belowOfs, xRef, xRefBelow, xAfter: Single;
  justBelow, justAbove: boolean;

  function RetrieveMarkGlyph(AMark: LongWord): boolean;
  var k: integer;
  begin
    markGlyphIndex := FFont.CharIndex[AMark];
    if markGlyphIndex = 0 then
    begin
      for k := 0 to high(MarkFallback) do
        if (MarkFallback[k].NonSpacing = AMark) and
           (not MarkFallback[k].Moved or GlyphBoxFixed) then
        begin
          markGlyphIndex := FFont.CharIndex[MarkFallback[k].Spacing];
          break;
        end;
    end;
    if markGlyphIndex <> 0 then
    begin
      markFreetypeGlyph := FFont.Glyph[markGlyphIndex];
      markGlyphBounds := markFreetypeGlyph.Bounds;
      markCombining := GetUnicodeCombiningClass(AMark);
      if markCombining in[27..35] then
      begin
        if IsArabicMarkAbove(AMark) then markCombining := 230 else
        if IsArabicMarkBelow(AMark) then markCombining := 220;
      end;
      result := true;
    end
    else result := false;
  end;

  procedure DoJustAbove(const ALetterBounds: TRect);
  begin
    if justAbove then
    begin
      if GlyphBoxFixed then
      begin
        DecF(aboveOfs, ALetterBounds.Top - markGlyphBounds.Bottom);
        incF(aboveOfs, FFont.SizeInPixels/12);
      end else
        DecF(aboveOfs, ALetterBounds.Top + FFont.Ascent/3);
      justAbove := false;
    end;
  end;

  procedure DoJustBelow(const ALetterBounds: TRect);
  begin
    if justBelow then
    begin
      if GlyphBoxFixed then
      begin
        incF(belowOfs, ALetterBounds.Bottom - markGlyphBounds.Top);
        incF(belowOfs, FFont.SizeInPixels/12);
      end else
        incF(belowOfs, ALetterBounds.Bottom);
      justBelow := false;
    end;
  end;

  function GetMarkOffsetY(AMark: LongWord): single;
  begin
    if (AMark = $304) or (AMark= $305)  or (AMark= $33F) or
       (AMark = $320) or (AMark = $331) or (AMark = $332) or (AMark = $333) then
    begin
      result := FFont.SizeInPixels/8;
    end else
    begin
      if GlyphBoxFixed then
        result := markGlyphBounds.Height + FFont.SizeInPixels/20
      else
        result := FFont.SizeInPixels/4;
    end;
  end;

  procedure DrawMark(AMark: LongWord; const ALetterBounds: TRect);
  var
    ofsX, ofsY: Single;
  begin
    if RetrieveMarkGlyph(AMark) then
    begin
      if markCombining in[228,230,232] then
      begin
        DoJustAbove(ALetterBounds);
        ofsX := -(markGlyphBounds.Left + markGlyphBounds.Right)/2;
        ofsY := -aboveOfs;
        IncF(aboveOfs, GetMarkOffsetY(AMark));
        ADrawer.DrawGlyph(markGlyphIndex, FFont,
            xRef + ofsX, ptGlyph.y + ofsY, BGRAToFPColor(AColor), [ftaTop,ftaLeft]);
      end else
      if markCombining in[218,220,222,240] then
      begin
        if justBelow then incF(ofsX, xRefBelow - xRef);
        DoJustBelow(ALetterBounds);
        ofsX := -(markGlyphBounds.Left + markGlyphBounds.Right)/2;
        ofsY := belowOfs;
        IncF(belowOfs, GetMarkOffsetY(AMark));
        ADrawer.DrawGlyph(markGlyphIndex, FFont,
            xRefBelow + ofsX, ptGlyph.y + ofsY, BGRAToFPColor(AColor), [ftaTop,ftaLeft]);
      end else
        ADrawer.DrawGlyph(markGlyphIndex, FFont,
            xAfter, ptGlyph.y, BGRAToFPColor(AColor), [ftaTop,ftaLeft]);
    end;
  end;

begin
  di := GetDisplayInfo(ATextUTF8, x, y, AAlign);
  for i := 0 to high(di) do
  begin
    if di[i].Mirrored then
      ptGlyph := di[i].Matrix * PointF(di[i].Glyph.Width, 0)
    else
      ptGlyph := di[i].Matrix * PointF(0, 0);
    with TBGRAFreeTypeGlyph(di[i].Glyph) do
    begin
      ADrawer.DrawText(Recomposed, FFont,
          ptGlyph.x, ptGlyph.y, BGRAToFPColor(AColor), [ftaTop,ftaLeft]);

      if di[i].RTL then xAfter := ptGlyph.x else xAfter := ptGlyph.x+Width;

      if Marks <> nil then
      begin
        justAbove := false;
        if (Recomposed = 'ﻁ') or (Recomposed = 'ﻂ') or (Recomposed = 'ﻃ') or (Recomposed = 'ﻄ') or
           (Recomposed = 'ﻅ') or (Recomposed = 'ﻆ') or (Recomposed = 'ﻇ') or (Recomposed = 'ﻈ') then
        begin
          aboveOfs := 0;
          xRef := ptGlyph.X + Width*3/4;
          xRefBelow := xRef;
        end else
        if (Recomposed = 'ﻝ') or (Recomposed = 'ﻞ') or (Recomposed = 'ﻚ') or (Recomposed = 'ﻙ') then
        begin
          aboveOfs := 0;
          xRef := ptGlyph.X + Width/2;
          xRefBelow := xRef;
        end else
        if (Recomposed = 'ﻵ') or (Recomposed = 'ﻶ') or (Recomposed = 'ﻷ') or (Recomposed = 'ﻸ') or
           (Recomposed = 'ﻹ') or (Recomposed = 'ﻺ') or (Recomposed = 'ﻻ') or (Recomposed = 'ﻼ') then
        begin
          justAbove := true;
          aboveOfs := - FFont.SizeInPixels/10;
          xRef := ptGlyph.X + Width/6;
          xRefBelow := ptGlyph.X + Width/4;
        end else
        begin
          justAbove := true;
          aboveOfs := 0;
          xRef := ptGlyph.X + Width/2;
          xRefBelow := xRef;
        end;
        if (Recomposed = 'ﻅ') or (Recomposed = 'ﻆ') or (Recomposed = 'ﻇ') or (Recomposed = 'ﻈ') or
         (Recomposed = 'ﻚ') or (Recomposed = 'ﻙ') then
        begin
          IncF(aboveOfs, FFont.SizeInPixels/12);
        end;
        if (Recomposed = 'ﺏ') or (Recomposed = 'ﺐ') or (Recomposed = 'ﭒ') or (Recomposed = 'ﭓ') or
           (Recomposed = 'ﭖ') or (Recomposed = 'ﭗ') or (Recomposed = 'ﭚ') or (Recomposed = 'ﭛ') or
           (Recomposed = 'ٮ') then
        begin
          DecF(aboveOfs, FFont.SizeInPixels/16);
        end;

        belowOfs := 0;
        justBelow := true;
        for j := 0 to high(Marks) do
          DrawMark(Marks[j], Bounds);
      end;
      if InnerMarks <> nil then
      begin
        xRef := ptGlyph.X + Width*3/4;
        xRefBelow := xRef;
        aboveOfs := 0;
        justAbove := true;
        belowOfs := 0;
        justBelow := true;
        for j := 0 to high(InnerMarks) do
          DrawMark(InnerMarks[j], Bounds);
      end;
    end;
  end;
end;

{ TBGRAFreeTypeFontRenderer }

function TBGRAFreeTypeFontRenderer.GetCollection: TCustomFreeTypeFontCollection;
begin
  result := EasyLazFreeType.FontCollection;
end;

function TBGRAFreeTypeFontRenderer.GetDrawer(ASurface: TBGRACustomBitmap): TBGRAFreeTypeDrawer;
begin
  result := FDrawer;
  result.ShadowColor := ShadowColor;
  result.ShadowOffset := ShadowOffset;
  result.ShadowRadius := ShadowRadius;
  result.ShadowVisible := ShadowVisible;
  result.ShadowQuality := ShadowQuality;
  result.ClearTypeRGBOrder := FontQuality <> fqFineClearTypeBGR;
  result.Destination := ASurface;
  result.OutlineColor := OutlineColor;
  result.OutlineVisible := OutlineVisible;
  result.OuterOutlineOnly := OuterOutlineOnly;
  result.OutlineTexture := OutlineTexture;
  if ShaderActive then result.Shader := FShader
   else result.Shader := nil;
end;

function TBGRAFreeTypeFontRenderer.GetShaderLightPosition: TPoint;
begin
  if FShader = nil then
    result := point(0,0)
  else
    result := FShader.LightPosition;
end;

function TBGRAFreeTypeFontRenderer.GetShaderLightPositionF: TPointF;
begin
  if FShader = nil then
    result := pointF(0,0)
  else
    result := FShader.LightPositionF;
end;

procedure TBGRAFreeTypeFontRenderer.SetShaderLightPosition(const AValue: TPoint);
begin
  if FShader <> nil then
    FShader.LightPosition := AValue;
end;

procedure TBGRAFreeTypeFontRenderer.SetShaderLightPositionF(
  const AValue: TPointF);
begin
  if FShader <> nil then
    FShader.LightPositionF := AValue;
end;

function TBGRAFreeTypeFontRenderer.GetTypeWriter: TBGRACustomTypeWriter;
begin
  if FTypeWriter = nil then
    FTypeWriter := TFreeTypeTypeWriter.Create(FFont);
  result := FTypeWriter;
end;

procedure TBGRAFreeTypeFontRenderer.UpdateFont(ADisableClearType: boolean);
var fts: TFreeTypeStyles;
  filename: string;
  twChange, newClearType: boolean;
  newSize: Single;
begin
  twChange := false;
  fts := [];
  if fsBold in FontStyle then include(fts, ftsBold);
  if fsItalic in FontStyle then include(fts, ftsItalic);
  try
    filename := FontName;
    if (filename <> FFont.Name) or (fts <> FFont.Style) then
    begin
      twChange := true;
      {$IFDEF BGRABITMAP_USE_LCL12}
      FFont.SetNameAndStyle(filename,fts);
      {$ELSE}
      FFont.Name := filename;
      FFont.Style := fts;
      {$ENDIF}
    end;
  except
    on ex: exception do
    begin
    end;
  end;
  newSize := FontEmHeightF;
  if newSize <> FLastFontSize then
  begin
    twChange := true;
    if FontEmHeightF >= 0 then
      FFont.SizeInPixels := FontEmHeightF
    else
      FFont.LineFullHeight := -FontEmHeightF;
  end;
  case FontQuality of
    fqSystem:
    begin
      FFont.Quality := grqMonochrome;
      newClearType := false;
    end;
    fqSystemClearType:
    begin
      FFont.Quality:= grqLowQuality;
      newClearType:= true;
    end;
    fqFineAntialiasing:
    begin
      FFont.Quality:= grqHighQuality;
      newClearType:= false;
    end;
    fqFineClearTypeRGB,fqFineClearTypeBGR:
    begin
      FFont.Quality:= grqHighQuality;
      newClearType:= true;
    end;
  end;
  if ADisableClearType then newClearType:= false;
  if newClearType <> FFont.ClearType then
  begin
    twChange := true;
    FFont.ClearType:= newClearType;
  end;
  if FFont.Hinted <> FontHinted then
  begin
    twChange := true;
    FFont.Hinted := FontHinted;
  end;
  {$IFDEF BGRABITMAP_USE_LCL12}
    FFont.StrikeOutDecoration := fsStrikeOut in FontStyle;
    FFont.UnderlineDecoration := fsUnderline in FontStyle;
  {$ENDIF}
  if twChange then FreeAndNil(FTypeWriter);
end;

procedure TBGRAFreeTypeFontRenderer.Init;
begin
  ShaderActive := true;

  FDrawer := TBGRAFreeTypeDrawer.Create(nil);
  FFont := TFreeTypeFont.Create;
  FLastFontSize:= EmptySingle;
  FontHinted:= True;

  ShadowColor := BGRABlack;
  ShadowVisible := false;
  ShadowOffset := Point(5,5);
  ShadowRadius := 5;
  ShadowQuality:= rbFast;
end;

procedure TBGRAFreeTypeFontRenderer.TextOutAnglePatch(ADest: TBGRACustomBitmap;
  x, y: single; orientation: integer; s: string; c: TBGRAPixel;
  tex: IBGRAScanner; align: TAlignment);
const orientationToDeg = -0.1;
var
  temp: TBGRACustomBitmap;
  coord: TPointF;
  angleDeg: single;
  OldOrientation: integer;
  filter: TResampleFilter;
begin
  OldOrientation := FontOrientation;
  FontOrientation:= 0;
  UpdateFont(true);

  temp := BGRABitmapFactory.Create;
  with TypeWriter.GetTextSizeBeforeTransform(s) do
    temp.SetSize(ceil(x),ceil(y));
  temp.FillTransparent;
  if tex<>nil then
  begin
    FDrawer.Texture := tex;
    InternalTextOut(temp,0,0, s, BGRAWhite, taLeftJustify);
    FDrawer.Texture := nil;
  end
  else
    InternalTextOut(temp,0,0, s, c, taLeftJustify);

  orientation:= orientation mod 3600;
  if orientation < 0 then inc(orientation, 3600);

  angleDeg := orientation * orientationToDeg;
  coord := PointF(x,y);
  case align of
  taRightJustify: coord.Offset( AffineMatrixRotationDeg(angleDeg)*PointF(-temp.Width,0) );
  taCenter: coord.Offset( AffineMatrixRotationDeg(angleDeg)*PointF(-0.5*temp.Width,0) );
  end;
  case orientation of
  0,900,1800,2700: filter := rfBox;
  else filter := rfCosine;
  end;
  ADest.PutImageAngle(coord.x,coord.y, temp, angleDeg, filter);
  temp.Free;

  FontOrientation:= OldOrientation;
end;

procedure TBGRAFreeTypeFontRenderer.InternalTextOut(ADest: TBGRACustomBitmap;
  x, y: single; s: string; c: TBGRAPixel; align: TAlignment);
var
  twAlign: TBGRATypeWriterAlignment;
begin
  case align of
    taCenter: twAlign:= twaTop;
    taRightJustify: twAlign := twaTopRight
  else
    twAlign := twaTopLeft;
  end;
  TFreeTypeTypeWriter(TypeWriter).DrawText(GetDrawer(ADest), s, x,y, c, twAlign);
end;

constructor TBGRAFreeTypeFontRenderer.Create;
begin
  Init;
end;

constructor TBGRAFreeTypeFontRenderer.Create(AShader: TCustomPhongShading;
  AShaderOwner: boolean);
begin
  Init;
  FShader := AShader;
  FShaderOwner := AShaderOwner;
end;

function TBGRAFreeTypeFontRenderer.GetFontPixelMetric: TFontPixelMetric;
begin
  UpdateFont;
  result.Baseline := round(FFont.Ascent);
  result.CapLine:= round(FFont.Ascent*0.2);
  result.DescentLine:= round(FFont.Ascent+FFont.Descent);
  result.Lineheight := round(FFont.LineFullHeight);
  result.xLine := round(FFont.Ascent*0.45);
  result.Defined := True;
end;

function TBGRAFreeTypeFontRenderer.GetFontPixelMetricF: TFontPixelMetricF;
begin
  UpdateFont;
  result.Baseline := FFont.Ascent;
  result.CapLine:= FFont.Ascent*0.2;
  result.DescentLine:= FFont.Ascent+FFont.Descent;
  result.Lineheight := FFont.LineFullHeight;
  result.xLine := FFont.Ascent*0.45;
  result.Defined := True;
end;

procedure TBGRAFreeTypeFontRenderer.TextOutAngle(ADest: TBGRACustomBitmap; x,
  y: single; orientation: integer; s: string; c: TBGRAPixel; align: TAlignment);
begin
  TextOutAnglePatch(ADest, x,y, orientation, s, c, nil, align);
end;

procedure TBGRAFreeTypeFontRenderer.TextOutAngle(ADest: TBGRACustomBitmap; x,
  y: single; orientation: integer; s: string; texture: IBGRAScanner;
  align: TAlignment);
begin
  TextOutAnglePatch(ADest, x,y, orientation, s, BGRAPixelTransparent, texture, align);
end;

procedure TBGRAFreeTypeFontRenderer.TextOut(ADest: TBGRACustomBitmap; x,
  y: single; s: string; texture: IBGRAScanner; align: TAlignment);
begin
  if FontOrientation mod 3600 <> 0 then
    TextOutAngle(ADest, x,y, FontOrientation, s, texture, align)
  else
  begin
    FDrawer.Texture := texture;
    TextOut(ADest,x,y,s,BGRAWhite,align);
    FDrawer.Texture := nil;
  end;
end;

procedure TBGRAFreeTypeFontRenderer.TextOut(ADest: TBGRACustomBitmap; x,
  y: single; s: string; c: TBGRAPixel; align: TAlignment);
begin
  if FontOrientation mod 3600 <> 0 then
    TextOutAngle(ADest, x,y, FontOrientation, s, c, align)
  else
  begin
    UpdateFont;
    InternalTextOut(ADest, x,y, s, c, align);
  end;
end;

procedure TBGRAFreeTypeFontRenderer.TextRect(ADest: TBGRACustomBitmap;
  ARect: TRect; x, y: integer; s: string; style: TTextStyle; c: TBGRAPixel);
var align: TFreeTypeAlignments;
    intersectedClip,previousClip: TRect;
begin
  previousClip := ADest.ClipRect;
  if style.Clipping then
  begin
    intersectedClip := TRect.Intersect(previousClip, ARect);
    if intersectedClip.IsEmpty then exit;
    ADest.ClipRect := intersectedClip;
  end;
  UpdateFont;
  align := [];
  case style.Alignment of
  taCenter: begin ARect.Left := x; include(align, ftaCenter); end;
  taRightJustify: begin ARect.Left := x; include(align, ftaRight); end;
  else
    include(align, ftaLeft);
  end;
  case style.Layout of
  {$IFDEF BGRABITMAP_USE_LCL12}
    tlCenter: begin ARect.Top := y; include(align, ftaVerticalCenter); end;
  {$ENDIF}
  tlBottom: begin ARect.top := y; include(align, ftaBottom); end;
  else include(align, ftaTop);
  end;
  try
    {$IFDEF BGRABITMAP_USE_LCL12}
      if style.Wordbreak then
        GetDrawer(ADest).DrawTextRect(s, FFont, ARect.Left,ARect.Top,ARect.Right,ARect.Bottom,BGRAToFPColor(c),align)
      else
    {$ENDIF}
    begin
      case style.Layout of
      tlCenter: y := (ARect.Top+ARect.Bottom) div 2;
      tlBottom: y := ARect.Bottom;
      else
        y := ARect.Top;
      end;
      case style.Alignment of
      taLeftJustify: GetDrawer(ADest).DrawText(s,FFont,ARect.Left,y,BGRAToFPColor(c),align);
      taCenter: GetDrawer(ADest).DrawText(s,FFont,(ARect.Left+ARect.Right-1) div 2,y,BGRAToFPColor(c),align);
      taRightJustify: GetDrawer(ADest).DrawText(s,FFont,ARect.Right,y,BGRAToFPColor(c),align);
      end;
    end;
  finally
    if style.Clipping then
      ADest.ClipRect := previousClip;
  end;
end;

procedure TBGRAFreeTypeFontRenderer.TextRect(ADest: TBGRACustomBitmap;
  ARect: TRect; x, y: integer; s: string; style: TTextStyle;
  texture: IBGRAScanner);
begin
  FDrawer.Texture := texture;
  TextRect(ADest,ARect,x,y,s,style,BGRAWhite);
  FDrawer.Texture := nil;
end;

function TBGRAFreeTypeFontRenderer.TextSize(sUTF8: string): TSize;
begin
  with TextSizeF(sUTF8) do
    result := Size(System.Round(x),System.Round(y));
end;

function TBGRAFreeTypeFontRenderer.TextSizeF(sUTF8: string): TPointF;
begin
  UpdateFont;
  result := TypeWriter.GetTextSizeBeforeTransform(sUTF8);
end;

function TBGRAFreeTypeFontRenderer.TextSize(sUTF8: string; AMaxWidth: integer;
  ARightToLeft: boolean): TSize;
begin
  with TextSizeF(sUTF8, AMaxWidth, ARightToLeft) do
    result := Size(System.Round(x),System.Round(y));
end;

function TBGRAFreeTypeFontRenderer.TextSizeF(sUTF8: string; AMaxWidthF: single;
  ARightToLeft: boolean): TPointF;
var
  w,h: single;
  charCount, byteCount: integer;
begin
  UpdateFont;
  result.x := 0;
  result.y := 0;
  h := FFont.LineFullHeight;
  repeat
    TypeWriter.TextFitInfoBeforeTransform(sUTF8, AMaxWidthF, charCount, byteCount, w);
    if w>result.x then result.x := w;
    IncF(result.y, h);
    sUTF8 := copy(sUTF8, byteCount+1, length(sUTF8)-byteCount);
  until sUTF8 = '';
end;

function TBGRAFreeTypeFontRenderer.TextFitInfo(sUTF8: string; AMaxWidth: integer): integer;
begin
  result := TextFitInfoF(sUTF8, AMaxWidth);
end;

function TBGRAFreeTypeFontRenderer.TextFitInfoF(sUTF8: string;
  AMaxWidthF: single): integer;
var
  byteCount: integer;
  usedWidth: single;
begin
  UpdateFont;
  TypeWriter.TextFitInfoBeforeTransform(sUTF8, AMaxWidthF, result, byteCount, usedWidth);
end;

destructor TBGRAFreeTypeFontRenderer.Destroy;
begin
  FTypeWriter.Free;
  FDrawer.Free;
  FFont.Free;
  if FShaderOwner then FShader.Free;
  inherited Destroy;
end;

{ TBGRAFreeTypeDrawer }

procedure TBGRAFreeTypeDrawer.RenderDirectly( x,y,tx: integer;
                          data: pointer );
var psrc: pbyte;
    pdest: PBGRAPixel;
    c: TBGRAPixel;
begin
  if Destination <> nil then
  begin
    //ensure rendering in bounds
    if (y < 0) or (y >= Destination.height) or (x < 0) or (x > Destination.width-tx) then exit;

    psrc := pbyte(data);
    pdest := Destination.ScanLine[y]+x;
    if Texture = nil then
    begin
      c := FColor;
      while tx > 0 do
      begin
        DrawPixelInlineWithAlphaCheck(pdest,c,psrc^);
        inc(psrc);
        inc(pdest);
        dec(tx);
      end;
    end else
    begin
      Texture.ScanMoveTo(x,y);
      while tx > 0 do
      begin
        DrawPixelInlineWithAlphaCheck(pdest,Texture.ScanNextPixel,psrc^);
        inc(psrc);
        inc(pdest);
        dec(tx);
      end;
    end;
  end;
end;

procedure TBGRAFreeTypeDrawer.RenderDirectlyClearType(x, y, tx: integer; data: pointer);
var xb: integer;
    psrc: pbyte;
    pdest: PBGRAPixel;
begin
  if Destination <> nil then
  begin
    tx := tx div 3;
    if tx=0 then exit;
    if (FMask <> nil) and (FMask.Width <> tx) then
      FMask.SetSize(tx,1)
    else if FMask = nil then FMask := BGRABitmapFactory.create(tx,1);

    pdest := FMask.Data;
    psrc := pbyte(data);
    pdest^.red := (psrc^ + psrc^ + (psrc+1)^) div 3;
    pdest^.green := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
    if tx > 1 then
      pdest^.blue := ((psrc+1)^ + (psrc+2)^ + (psrc+3)^) div 3
    else
      pdest^.blue := ((psrc+1)^ + (psrc+2)^ + (psrc+2)^) div 3;
    inc(pdest);
    inc(psrc,3);
    for xb := 1 to tx-2 do
    begin
      pdest^.red := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
      pdest^.green := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
      pdest^.blue := ((psrc+1)^ + (psrc+2)^ + (psrc+3)^) div 3;
      inc(pdest);
      inc(psrc,3);
    end;
    if tx > 1 then
    begin
      pdest^.red := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
      pdest^.green := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
      pdest^.blue := ((psrc+1)^ + (psrc+2)^ + (psrc+2)^) div 3;
    end;
    BGRAFillClearTypeRGBMask(Destination,x div 3,y,FMask,FColor,Texture,ClearTypeRGBOrder);
  end;
end;

function TBGRAFreeTypeDrawer.ShadowActuallyVisible: boolean;
begin
  result := ShadowVisible and (ShadowColor.alpha <> 0);
end;

function TBGRAFreeTypeDrawer.OutlineActuallyVisible: boolean;
begin
  result := ((OutlineTexture <> nil) or (OutlineColor.alpha <> 0)) and OutlineVisible;
end;

function TBGRAFreeTypeDrawer.ShaderActuallyActive: boolean;
begin
  result := (Shader <> nil) and ShaderActive;
end;

constructor TBGRAFreeTypeDrawer.Create(ADestination: TBGRACustomBitmap);
begin
  Destination := ADestination;
  ClearTypeRGBOrder:= true;
  ShaderActive := true;
  ShadowQuality:= rbFast;
end;

procedure TBGRAFreeTypeDrawer.DrawText(AText: string;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TFPColor);
var fx: TBGRACustomTextEffect;
  procedure DoOutline;
  begin
    if OutlineActuallyVisible then
    begin
      if OutlineTexture <> nil then
        fx.DrawOutline(Destination,round(x),round(y), OutlineTexture)
      else
        fx.DrawOutline(Destination,round(x),round(y), OutlineColor);
    end;
  end;
begin
  if not FInCreateTextEffect and (ShadowActuallyVisible or OutlineActuallyVisible or ShaderActuallyActive) then
  begin
    fx := CreateTextEffect(AText, AFont);
    fx.ShadowQuality := ShadowQuality;
    DecF(y, AFont.Ascent);
    if ShadowActuallyVisible then fx.DrawShadow(Destination, round(x+ShadowOffset.X),round(y+ShadowOffset.Y), ShadowRadius, ShadowColor);
    if OuterOutlineOnly then DoOutline;

    if texture <> nil then
    begin
      if ShaderActuallyActive then
        fx.DrawShaded(Destination,floor(x),floor(y), Shader, round(fx.TextSize.cy*0.05), texture)
      else
        fx.Draw(Destination,round(x),round(y), texture);
    end else
    begin
      if ShaderActuallyActive then
        fx.DrawShaded(Destination,floor(x),floor(y), Shader, round(fx.TextSize.cy*0.05), FPColorToBGRA(AColor))
      else
        fx.Draw(Destination,round(x),round(y), FPColorToBGRA(AColor));
    end;
    if not OuterOutlineOnly then DoOutline;
    fx.Free;
  end else
  begin
    FColor := FPColorToBGRA(AColor);
    if AFont.ClearType then
      AFont.RenderText(AText, x, y, Destination.ClipRect, @RenderDirectlyClearType)
    else
      AFont.RenderText(AText, x, y, Destination.ClipRect, @RenderDirectly);
  end;
end;

procedure TBGRAFreeTypeDrawer.DrawText(AText: string;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TBGRAPixel);
begin
  DrawText(AText, AFont, x,y, BGRAToFPColor(AColor));
end;

procedure TBGRAFreeTypeDrawer.DrawText(AText: string;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TBGRAPixel;
  AAlign: TFreeTypeAlignments);
begin
  DrawText(AText, AFont, x,y, BGRAToFPColor(AColor), AAlign);
end;

{$IFDEF BGRABITMAP_USE_LCL12}
procedure TBGRAFreeTypeDrawer.DrawTextWordBreak(AText: string;
  AFont: TFreeTypeRenderableFont; x, y, AMaxWidth: Single; AColor: TBGRAPixel;
  AAlign: TFreeTypeAlignments);
begin
  DrawTextWordBreak(AText,AFont,x,y,AMaxWidth,BGRAToFPColor(AColor),AAlign);
end;

procedure TBGRAFreeTypeDrawer.DrawTextRect(AText: string;
  AFont: TFreeTypeRenderableFont; X1, Y1, X2, Y2: Single; AColor: TBGRAPixel;
  AAlign: TFreeTypeAlignments);
begin
  DrawTextRect(AText,AFont,X1,Y1,X2,Y2,BGRAToFPColor(AColor),AAlign);
end;
{$ENDIF}

{$IFDEF BGRABITMAP_USE_LCL15}
procedure TBGRAFreeTypeDrawer.DrawGlyph(AGlyph: integer;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TFPColor);
var f: TFreeTypeFont;
begin
  if not (AFont is TFreeTypeFont) then exit;
  f := TFreeTypeFont(Afont);
  FColor := FPColorToBGRA(AColor);
  if AFont.ClearType then
    f.RenderGlyph(AGlyph, x, y, Destination.ClipRect, @RenderDirectlyClearType)
  else
    f.RenderGlyph(AGlyph, x, y, Destination.ClipRect, @RenderDirectly);
end;

procedure TBGRAFreeTypeDrawer.DrawGlyph(AGlyph: integer;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TBGRAPixel);
begin
  DrawGlyph(AGlyph, AFont, x,y, BGRAToFPColor(AColor));
end;

procedure TBGRAFreeTypeDrawer.DrawGlyph(AGlyph: integer;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TBGRAPixel;
  AAlign: TFreeTypeAlignments);
begin
  DrawGlyph(AGlyph, AFont, x,y, BGRAToFPColor(AColor), AAlign);
end;
{$ENDIF}

function TBGRAFreeTypeDrawer.CreateTextEffect(AText: string;
  AFont: TFreeTypeRenderableFont): TBGRACustomTextEffect;
var
  mask: TBGRACustomBitmap;
  tx,ty,marginHoriz,marginVert: integer;
  tempDest: TBGRACustomBitmap;
  tempTex: IBGRAScanner;
  tempClearType: boolean;
begin
  FInCreateTextEffect:= True;
  try
    tx := ceil(AFont.TextWidth(AText));
    ty := ceil(AFont.TextHeight(AText));
    marginHoriz := ty div 2;
    marginVert := 1;
    mask := BGRABitmapFactory.Create(tx+2*marginHoriz,ty+2*marginVert,BGRABlack);
    tempDest := Destination;
    tempTex := Texture;
    tempClearType:= AFont.ClearType;
    Destination := mask;
    Texture := nil;
    AFont.ClearType := false;
    DrawText(AText,AFont,marginHoriz,marginVert,BGRAWhite,[ftaTop,ftaLeft]);
    Destination := tempDest;
    Texture := tempTex;
    AFont.ClearType := tempClearType;
    mask.ConvertToLinearRGB;
    result := TBGRACustomTextEffect.Create(mask, true,tx,ty,point(-marginHoriz,-marginVert));
  finally
    FInCreateTextEffect:= false;
  end;
end;

destructor TBGRAFreeTypeDrawer.Destroy;
begin
  FMask.Free;
  inherited Destroy;
end;

end.
