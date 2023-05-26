// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRATextBidi;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, BGRAUTF8, BGRAUnicode, BGRATransform,
  BGRAUnicodeText;

type
  TBrokenLinesChangedEvent = procedure(ASender: TObject; AParagraphIndex: integer;
    ASubBrokenStart, ASubBrokenChangedCountBefore, ASubBrokenChangedCountAfter: integer;
    ASubBrokenTotalCountBefore, ASubBrokenTotalCountAfter: integer) of object;
  TParagraphLayoutSplitEvent = procedure(ASender: TObject; AParagraphIndex: integer;
      ASubBrokenIndex, ACharIndex: integer) of object;

  { TBidiCaretPos }

  TBidiCaretPos = record
    PartIndex: integer;

    Top, Bottom: TPointF;
    RightToLeft: boolean;

    PreviousTop, PreviousBottom: TPointF;
    PreviousRightToLeft: boolean;

    procedure Transform(AMatrix: TAffineMatrix);
  end;

  PPartInfo = ^TPartInfo;

  { TPartInfo }

  TPartInfo = record
         brokenLineIndex: integer;
         startIndex, endIndex: integer;
         bidiLevel: byte;
         modified: bytebool;
         rectF: TRectF;
         posCorrection: TPointF;
         function IsRightToLeft: boolean;
       end;

  PBrokenLineInfo = ^TBrokenLineInfo;

  { TBrokenLineInfo }

  TBrokenLineInfo = record
                unbrokenLineIndex: integer;
                startIndex, endIndex: integer;
                bidiLevel: byte;
                rectF: TRectF;
                usedWidth: single;
                firstPartIndex: integer;
                parts: array of TPartInfo;
                partCount: integer;
                function IsRightToLeft: boolean;
              end;

  PParagraphInfo = ^TParagraphInfo;
  TParagraphInfo = record
    alignment: TBidiTextAlignment;
    layoutComputed, overflow: boolean;
    rectF: TRectF;
    firstBrokenLineIndex: integer;
    firstPartIndex: integer;
    brokenLines: array of TBrokenLineInfo;
    brokenLineCount: integer;
  end;

  TBidiTextLayout = class;

  { TPartEnumerator }

  TPartEnumerator = record
  private
    FJustCreated: boolean;
    FParagraphIndex: integer;
    FBrokenLineIndex: integer;
    FPartIndex: integer;
    FLayout: TBidiTextLayout;
    FSubBrokenIndex: integer;
    FSubBrokenCount: integer;
    FCurBroken: PBrokenLineInfo;
    FSubPartIndex: integer;
    FSubPartCount: integer;
    FEndPartIndex: integer;
    function GetPartInfo: PPartInfo;
    procedure Update;
  public
    class function New(ALayout: TBidiTextLayout; AParagraphIndex: integer;
      ASubBrokenIndex: integer; ASubPartIndex: integer; AEndPartIndex: integer): TPartEnumerator; static;
    function GetNext: boolean;
    property Layout: TBidiTextLayout read FLayout;
    property ParagraphIndex: integer read FParagraphIndex;
    property BrokenLineIndex: integer read FBrokenLineIndex;
    property PartIndex: integer read FPartIndex;
    property PartInfo: PPartInfo read GetPartInfo;
    property BrokenLineInfo: PBrokenLineInfo read FCurBroken;
  end;

  { TBidiTextLayout }

  TBidiTextLayout = class
  private
    FAvailableHeight: single;
    FAvailableWidth: single;
    FClipMargin: integer;
    FOnBrokenLinesChanged: TBrokenLinesChangedEvent;
    FOnParagraphChanged: TParagraphEvent;
    FOnParagraphDeleted: TParagraphEvent;
    FOnParagraphMergedWithNext: TParagraphEvent;
    FOnParagraphSplit: TParagraphLayoutSplitEvent;
    FOnParagraphVerticalTrimChanged: TParagraphEvent;
    FParagraphSpacingAbove: single;
    FParagraphSpacingBelow: single;
    FTopLeft: TPointF;
    FMatrix, FMatrixInverse: TAffineMatrix;
    FTabSize: Single;
    FWordBreakHandler: TWordBreakHandler;
    function GetBrokenLineAffineBox(AIndex: integer): TAffineBox;
    function GetBrokenLineCount: integer;
    function GetBrokenLineEndCaret(AIndex: integer): TBidiCaretPos;
    function GetBrokenLineEndPart(AIndex: integer): integer;
    function GetBrokenLineStartPart(AIndex: integer): integer;
    function GetBrokenLineUntransformedEndCaret(AIndex: integer): TBidiCaretPos;
    function GetBrokenLineEndIndex(AIndex: integer): integer;
    function GetBrokenLineParagraphIndex(AIndex: integer): integer;
    function GetBrokenLineUnbrokenIndex(AIndex: integer): integer;
    function GetBrokenLineInfo(AIndex: integer): PBrokenLineInfo;
    function GetBrokenLineRectF(AIndex: integer): TRectF;
    function GetBrokenLineRightToLeft(AIndex: integer): boolean;
    function GetBrokenLineStartCaret(AIndex: integer): TBidiCaretPos;
    function GetBrokenLineUntransformedStartCaret(AIndex: integer): TBidiCaretPos;
    function GetBrokenLineStartIndex(AIndex: integer): integer;
    function GetBrokenLineUsedWidth(AIndex: integer): single;
    function GetCharCount: integer;
    function GetFontBidiMode: TFontBidiMode;
    function GetLayoutComputed: boolean;
    function GetLineHeight: single;
    function GetMatrix: TAffineMatrix;
    function GetMatrixInverse: TAffineMatrix;
    function GetParagraphAffineBox(AIndex: integer): TAffineBox;
    function GetParagraphAlignment(AIndex: integer): TBidiTextAlignment;
    function GetParagraphCount: integer;
    function GetParagraphEndBrokenLine(AIndex: integer): integer;
    function GetParagraphEndIndex(AIndex: integer): integer;
    function GetParagraphEndIndexBeforeParagraphSeparator(AIndex: integer): integer;
    function GetParagraphEndPart(AIndex: integer): integer;
    function GetParagraphInfo(AIndex: integer): PParagraphInfo;
    function GetParagraphRectF(AIndex: integer): TRectF;
    function GetParagraphRightToLeft(AIndex: integer): boolean;
    function GetParagraphStartBrokenLine(AIndex: integer): integer;
    function GetParagraphStartIndex(AIndex: integer): integer;
    function GetParagraphStartPart(AIndex: integer): integer;
    function GetPartAffineBox(AIndex: integer): TAffineBox;
    function GetPartBrokenLineIndex(AIndex: integer): integer;
    function GetPartCount: integer;
    function GetPartEnumerator(AFirstPart: integer): TPartEnumerator;
    function GetPartEnumerator(AFirstPart, ALastPartPlus1: integer): TPartEnumerator;
    function GetPartInfo(AIndex: integer): PPartInfo;
    function GetPartEndIndex(AIndex: integer): integer;
    function GetPartRectF(AIndex: integer): TRectF;
    function GetPartRightToLeft(AIndex: integer): boolean;
    function GetPartStartIndex(AIndex: integer): integer;
    function GetText: string;
    function GetTotalTextHeight: single;
    function GetUnicodeChar(APosition0: integer): LongWord;
    function GetUsedWidth: single;
    function GetUTF8Char(APosition0: integer): string4;
    procedure SetAvailableHeight(AValue: single);
    procedure SetAvailableWidth(AValue: single);
    procedure SetFontBidiMode(AValue: TFontBidiMode);
    procedure SetFontRenderer(AValue: TBGRACustomFontRenderer);
    procedure SetParagraphAlignment(AIndex: integer; AValue: TBidiTextAlignment);
    procedure SetParagraphSpacingAbove(AValue: single);
    procedure SetParagraphSpacingBelow(AValue: single);
    procedure SetTabSize(AValue: single);
    procedure SetTopLeft(AValue: TPointF);
    procedure ComputeMatrix;
  protected
    FAnalysis: TUnicodeAnalysis;
    FRenderer: TBGRACustomFontRenderer;
    FLineHeight: single;

    FParagraph: array of TParagraphInfo;
    FComputedBrokenLineCount: integer;
    FComputedPartCount: integer;

    FColor: TBGRAPixel;
    FTexture: IBGRAScanner;

    function TextSizeBidiOverride(sUTF8: string; ARightToLeft: boolean): TPointF;
    function TextSizeBidiOverrideSplit(AStartIndex, AEndIndex: integer; ARightToLeft: boolean; ASplitIndex: integer): TPointF;
    function TextFitInfoBidiOverride(sUTF8: string; AWidth: single; ARightToLeft: boolean): integer;
    function GetFontFullHeight: single;
    function GetFontBaseline: single;
    function GetFontOrientation: single;
    procedure TextOutBidiOverride(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; ARightToLeft: boolean);
    procedure TextPathBidiOverride(ADest: IBGRAPath; x, y: single; sUTF8: string; ARightToLeft: boolean);

    procedure AddPart(AStartIndex, AEndIndex: integer; ABidiLevel: byte; ARectF: TRectF; APosCorrection: TPointF; ABrokenLineIndex: integer; ABrokenLine: PBrokenLineInfo);
    function GetPartStartCaret(APartIndex: integer): TBidiCaretPos;
    function GetPartEndCaret(APartIndex: integer): TBidiCaretPos;
    function GetUntransformedPartStartCaret(APartIndex: integer): TBidiCaretPos;
    function GetUntransformedPartStartCaret(APartIndex: integer; APrevPart, APart: PPartInfo): TBidiCaretPos;
    function GetUntransformedPartEndCaret(APartIndex: integer): TBidiCaretPos;
    function GetUntransformedPartEndCaret(APartIndex: integer; APart: PPartInfo): TBidiCaretPos;
    function GetUntransformedParagraphAt(APosition: TPointF): integer; overload;

    function GetSameLevelString(startIndex,endIndex: integer): string; overload;
    function GetSameLevelString(startIndex,endIndex: integer; out nonDiscardedCount: integer): string; overload;
    function ComputeBidiTree(AMaxWidth: single; startIndex, endIndex: integer; bidiLevel: byte): TBidiTree;
    procedure AddPartsFromTree(APos: TPointF; ATree: TBidiTree; fullHeight, baseLine: single; ABrokenLineIndex: integer; ABrokenLine: PBrokenLineInfo);
    procedure Init(ATextUTF8: string; ABidiMode: TFontBidiMode); virtual;
    procedure ComputeLayout; virtual;
    procedure CheckTextLayout;
    procedure NeedLayout;
    procedure InvalidateParagraphLayout(AParagraphIndex: integer);
    procedure InternalInvalidateParagraphLayout(AParagraphIndex: integer);
    procedure OffsetParagraph(AParagraphIndex: integer; ADeltaY: single; ADeltaBroken, ADeltaPart: integer);
    procedure OffsetParagraphCharIndex(AParagraphIndex: integer; ADeltaChar: integer);
    procedure TrimParagraphLayoutVertically(AParagraphIndex: integer);
    procedure InternalDrawText(ADest: TBGRACustomBitmap);
    procedure InternalPathText(ADest: IBGRAPath); overload;
    procedure InternalPathText(ADest: IBGRAPath; AClipRect: TRect); overload;
    procedure InternalDrawTextParts(ADest: TBGRACustomBitmap; AFirstPart, ALastPartPlus1: integer);
    procedure InternalPathTextParts(ADest: IBGRAPath; AFirstPart, ALastPartPlus1: integer); overload;
    procedure InternalPathTextParts(ADest: IBGRAPath; AClipRect: TRect; AFirstPart, ALastPartPlus1: integer); overload;
    procedure InternalRangeError;

    //unicode analysis events
    procedure BidiModeChanged({%H-}ASender: TObject);
    procedure CharDeleted({%H-}ASender: TObject; AParagraphIndex: integer; {%H-}ACharStart, {%H-}ACharCount: integer);
    procedure CharInserted({%H-}ASender: TObject; AParagraphIndex: integer; {%H-}ACharStart, {%H-}ACharCount: integer);
    procedure AnalysisChanged({%H-}ASender: TObject; AParagraphIndex: integer; {%H-}ACharStart, {%H-}ACharCount: integer);
    procedure ParagraphDeleted({%H-}ASender: TObject; AParagraphIndex: integer);
    procedure ParagraphMergedWithNext({%H-}ASender: TObject; AParagraphIndex: integer);
    procedure ParagraphSplit({%H-}ASender: TObject; AParagraphIndex: integer; {%H-}ACharIndex: integer);
    procedure InternalParagraphDeleted(AParagraphIndex: integer);
    property LayoutComputed: boolean read GetLayoutComputed;
  public
    constructor Create(AFontRenderer: TBGRACustomFontRenderer; sUTF8: string); overload;
    constructor Create(AFontRenderer: TBGRACustomFontRenderer; sUTF8: string; ARightToLeft: boolean); overload;
    constructor Create(AFontRenderer: TBGRACustomFontRenderer; sUTF8: string; AFontBidiMode: TFontBidiMode); overload;
    destructor Destroy; override;
    procedure SetLayout(ARect: TRectF);
    procedure InvalidateLayout;
    procedure ComputeLayoutIfNeeded;
    function AddOverrideIfNecessary(var sUTF8: string; ARightToLeft: boolean): boolean;
    function GetTextPart(APartIndex: integer; AAddOverrideIfNecessary: boolean): string;

    procedure DrawText(ADest: TBGRACustomBitmap); overload;
    procedure DrawText(ADest: TBGRACustomBitmap; AColor: TBGRAPixel); overload;
    procedure DrawText(ADest: TBGRACustomBitmap; ATexture: IBGRAScanner); overload;
    procedure PathText(ADest: IBGRAPath);
    procedure PathText(ADest: IBGRAPath; AClipRect: TRect);
    procedure DrawTextParts(ADest: TBGRACustomBitmap; AFirstPart, ALastPartPlus1: integer); overload;
    procedure DrawTextParts(ADest: TBGRACustomBitmap; AColor: TBGRAPixel; AFirstPart, ALastPartPlus1: integer); overload;
    procedure DrawTextParts(ADest: TBGRACustomBitmap; ATexture: IBGRAScanner; AFirstPart, ALastPartPlus1: integer); overload;
    procedure PathTextParts(ADest: IBGRAPath; AFirstPart, ALastPartPlus1: integer); overload;
    procedure PathTextParts(ADest: IBGRAPath; AClipRect: TRect; AFirstPart, ALastPartPlus1: integer); overload;
    procedure DrawParagraphs(ADest: TBGRACustomBitmap; AFirstPara, ALastParaPlus1: integer); overload;
    procedure DrawParagraphs(ADest: TBGRACustomBitmap; AColor: TBGRAPixel; AFirstPara, ALastParaPlus1: integer); overload;
    procedure DrawParagraphs(ADest: TBGRACustomBitmap; ATexture: IBGRAScanner; AFirstPara, ALastParaPlus1: integer); overload;
    procedure PathParagraphs(ADest: IBGRAPath; AFirstPara, ALastParaPlus1: integer); overload;
    procedure PathParagraphs(ADest: IBGRAPath; AClipRect: TRect; AFirstPara, ALastParaPlus1: integer); overload;
    procedure DrawBrokenLines(ADest: TBGRACustomBitmap; AFirstBroken, ALastBrokenPlus1: integer); overload;
    procedure DrawBrokenLines(ADest: TBGRACustomBitmap; AColor: TBGRAPixel; AFirstBroken, ALastBrokenPlus1: integer); overload;
    procedure DrawBrokenLines(ADest: TBGRACustomBitmap; ATexture: IBGRAScanner; AFirstBroken, ALastBrokenPlus1: integer); overload;
    procedure PathBrokenLines(ADest: IBGRAPath; AFirstBroken, ALastBrokenPlus1: integer); overload;
    procedure PathBrokenLines(ADest: IBGRAPath; AClipRect: TRect; AFirstBroken, ALastBrokenPlus1: integer); overload;

    procedure DrawCaret(ADest: TBGRACustomBitmap; ACharIndex: integer; AMainColor, ASecondaryColor: TBGRAPixel);
    procedure DrawSelection(ADest: TBGRACustomBitmap; AStartIndex, AEndIndex: integer;
                            AFillColor: TBGRAPixel; ABorderColor: TBGRAPixel; APenWidth: single); overload;
    procedure DrawSelection(ADest: TBGRACustomBitmap; AStartIndex, AEndIndex: integer;
                            AFillColor: TBGRAPixel); overload;

    function GetCaret(ACharIndex: integer): TBidiCaretPos;
    function GetUntransformedCaret(ACharIndex: integer): TBidiCaretPos;
    function GetCharIndexAt(APosition: TPointF; ABetweenGlyphs: boolean = true): integer;
    function GetTextEnveloppe(AStartIndex, AEndIndex: integer; APixelCenteredCoordinates: boolean = true; AMergeBoxes: boolean = true; AVerticalClip: boolean = false): ArrayOfTPointF;
    function GetUntransformedTextEnveloppe(AStartIndex, AEndIndex: integer; APixelCenteredCoordinates: boolean = true; AMergeBoxes: boolean = true; AVerticalClip: boolean = false): ArrayOfTPointF;
    function GetParagraphAt(ACharIndex: Integer): integer; overload;
    function GetParagraphAt(APosition: TPointF): integer; overload;
    function GetBrokenLineAt(ACharIndex: integer): integer;

    function InsertText(ATextUTF8: string; APosition: integer): integer;
    function InsertLineSeparator(APosition: integer): integer;
    function DeleteText(APosition, ACount: integer): integer;
    function DeleteTextBefore(APosition, ACount: integer): integer;
    function CopyText(APosition, ACount: integer): string;
    function CopyTextBefore(APosition, ACount: integer): string;
    function IncludeNonSpacingChars(APosition, ACount: integer; AIncludeCombiningMarks: boolean = true): integer;
    function IncludeNonSpacingCharsBefore(APosition, ACount: integer; AIncludeCombiningMarks: boolean = true): integer;
    function FindTextAbove(AFromPosition: integer): integer;
    function FindTextBelow(AFromPosition: integer): integer;

    property CharCount: integer read GetCharCount;
    property UTF8Char[APosition0: integer]: string4 read GetUTF8Char;
    property UnicodeChar[APosition0: integer]: LongWord read GetUnicodeChar;

    property BrokenLineCount: integer read GetBrokenLineCount;
    property BrokenLineParagraphIndex[AIndex: integer]: integer read GetBrokenLineParagraphIndex;
    property BrokenLineUnbrokenIndex[AIndex: integer]: integer read GetBrokenLineUnbrokenIndex;
    property BrokenLineStartIndex[AIndex: integer]: integer read GetBrokenLineStartIndex;
    property BrokenLineEndIndex[AIndex: integer]: integer read GetBrokenLineEndIndex;
    property BrokenLineStartPart[AIndex: integer]: integer read GetBrokenLineStartPart;
    property BrokenLineEndPart[AIndex: integer]: integer read GetBrokenLineEndPart;
    property BrokenLineRectF[AIndex: integer]: TRectF read GetBrokenLineRectF;
    property BrokenLineUsedWidth[AIndex: integer]: single read GetBrokenLineUsedWidth;
    property BrokenLineAffineBox[AIndex: integer]: TAffineBox read GetBrokenLineAffineBox;
    property BrokenLineRightToLeft[AIndex: integer]: boolean read GetBrokenLineRightToLeft;
    property BrokenLineStartCaret[AIndex: integer]: TBidiCaretPos read GetBrokenLineStartCaret;
    property BrokenLineEndCaret[AIndex: integer]: TBidiCaretPos read GetBrokenLineEndCaret;
    property OnBrokenLinesChanged: TBrokenLinesChangedEvent read FOnBrokenLinesChanged write FOnBrokenLinesChanged;

    property PartCount: integer read GetPartCount;
    property PartStartIndex[AIndex: integer]: integer read GetPartStartIndex;
    property PartEndIndex[AIndex: integer]: integer read GetPartEndIndex;
    property PartBrokenLineIndex[AIndex: integer]: integer read GetPartBrokenLineIndex;
    property PartStartCaret[AIndex: integer]: TBidiCaretPos read GetPartStartCaret;
    property PartEndCaret[AIndex: integer]: TBidiCaretPos read GetPartEndCaret;
    property PartRectF[AIndex: integer]: TRectF read GetPartRectF;
    property PartAffineBox[AIndex: integer]: TAffineBox read GetPartAffineBox;
    property PartRightToLeft[AIndex: integer]: boolean read GetPartRightToLeft;

    property TopLeft: TPointF read FTopLeft write SetTopLeft;
    property AvailableWidth: single read FAvailableWidth write SetAvailableWidth;
    property AvailableHeight: single read FAvailableHeight write SetAvailableHeight;
    property TabSize: single read FTabSize write SetTabSize;
    property ParagraphSpacingAbove: single read FParagraphSpacingAbove write SetParagraphSpacingAbove;
    property ParagraphSpacingBelow: single read FParagraphSpacingBelow write SetParagraphSpacingBelow;
    property ParagraphRectF[AIndex: integer]: TRectF read GetParagraphRectF;
    property ParagraphAffineBox[AIndex: integer]: TAffineBox read GetParagraphAffineBox;
    property ParagraphAlignment[AIndex: integer]: TBidiTextAlignment read GetParagraphAlignment write SetParagraphAlignment;
    property ParagraphStartIndex[AIndex: integer]: integer read GetParagraphStartIndex;
    property ParagraphEndIndex[AIndex: integer]: integer read GetParagraphEndIndex;
    property ParagraphEndIndexBeforeParagraphSeparator[AIndex: integer]: integer read GetParagraphEndIndexBeforeParagraphSeparator;
    property ParagraphRightToLeft[AIndex: integer]: boolean read GetParagraphRightToLeft;
    property ParagraphStartPart[AIndex: integer]: integer read GetParagraphStartPart;
    property ParagraphEndPart[AIndex: integer]: integer read GetParagraphEndPart;
    property ParagraphStartBrokenLine[AIndex: integer]: integer read GetParagraphStartBrokenLine;
    property ParagraphEndBrokenLine[AIndex: integer]: integer read GetParagraphEndBrokenLine;
    property ParagraphCount: integer read GetParagraphCount;
    property OnParagraphDeleted : TParagraphEvent read FOnParagraphDeleted write FOnParagraphDeleted;
    property OnParagraphMergedWithNext: TParagraphEvent read FOnParagraphMergedWithNext write FOnParagraphMergedWithNext;
    property OnParagraphSplit: TParagraphLayoutSplitEvent read FOnParagraphSplit write FOnParagraphSplit;
    property OnParagraphChanged: TParagraphEvent read FOnParagraphChanged write FOnParagraphChanged;
    property OnParagraphVerticalTrimChanged: TParagraphEvent read FOnParagraphVerticalTrimChanged write FOnParagraphVerticalTrimChanged;

    property UsedWidth: single read GetUsedWidth;
    property TotalTextHeight: single read GetTotalTextHeight;
    property LineHeight: single read GetLineHeight;

    property Matrix: TAffineMatrix read GetMatrix;
    property MatrixInverse: TAffineMatrix read GetMatrixInverse;
    property TextUTF8: string read GetText;
    property WordBreakHandler: TWordBreakHandler read FWordBreakHandler write FWordBreakHandler;
    property ClipMargin: integer read FClipMargin write FClipMargin; // how many pixels can the text go outside of its box

    property FontRenderer: TBGRACustomFontRenderer read FRenderer write SetFontRenderer;
    property FontBidiMode: TFontBidiMode read GetFontBidiMode write SetFontBidiMode;
  end;

  { TBidiLayoutTree }

  TBidiLayoutTree = class(TBidiTree)
  private
    FBidiPos: single;
    FSize: TPointF;
    FTextUTF8: string;
    FNonDiscardedCount: integer;
    FLayout: TBidiTextLayout;
    FMaxWidth: single;
    function GetCumulatedBidiPos: single;
    function GetHeight: single;
    function GetLayout: TBidiTextLayout;
    function GetMaxWidth: single;
    function GetWidth: single;
    procedure UpdateBranchSize;
  public
    constructor Create(AData: pointer; AStartIndex, AEndIndex: integer; ABidiLevel: byte; AIsLeaf: boolean); override;
    procedure AddBranch(ABranch: TBidiTree); override;
    procedure Shorten(AEndIndex: integer); override;
    procedure AfterFinish; override;
    function TrySplit: boolean; override;
    property Layout: TBidiTextLayout read GetLayout;
    property MaxWidth: single read GetMaxWidth;
    property BidiPos: single read FBidiPos;
    property CumulatedBidiPos: single read GetCumulatedBidiPos;
    property Width: single read GetWidth;
    property Height: single read GetHeight;
  end;

  TBidiLayoutTreeData = record
    Layout: TBidiTextLayout;
    MaxWidth: single;
  end;

implementation

uses math;

{ TPartEnumerator }

function TPartEnumerator.GetPartInfo: PPartInfo;
begin
  if FSubPartIndex < FSubPartCount then
    result := @FCurBroken^.parts[FSubPartIndex]
  else
    result := nil;
end;

procedure TPartEnumerator.Update;
begin
  with FLayout.FParagraph[FParagraphIndex] do
  begin
    FSubBrokenCount:= brokenLineCount;
    FBrokenLineIndex:= FSubBrokenIndex + firstBrokenLineIndex;
    if FSubBrokenIndex < brokenLineCount then
    begin
      FCurBroken := @brokenLines[FSubBrokenIndex];
      with FCurBroken^ do
      begin
        FSubPartCount:= partCount;
        FPartIndex:= FSubPartIndex + firstPartIndex;
      end;
    end else
    begin
      FCurBroken := nil;
      FSubPartCount:= 0;
      if brokenLineCount > 0 then
        FPartIndex := brokenLines[brokenLineCount-1].firstPartIndex +
                      brokenLines[brokenLineCount-1].partCount
      else
        FPartIndex := firstPartIndex;
    end;
  end;
end;

class function TPartEnumerator.New(ALayout: TBidiTextLayout;
  AParagraphIndex: integer; ASubBrokenIndex: integer; ASubPartIndex: integer;
  AEndPartIndex: integer): TPartEnumerator;
begin
  result.FLayout := ALayout;
  result.FParagraphIndex:= AParagraphIndex;
  result.FSubBrokenIndex:= ASubBrokenIndex;
  result.FSubPartIndex:= ASubPartIndex;
  result.Update;
  result.FJustCreated:= true;
  result.FEndPartIndex:= AEndPartIndex;
end;

function TPartEnumerator.GetNext: boolean;
begin
  if FPartIndex >= FEndPartIndex then exit(false);
  if FJustCreated then
  begin
    FJustCreated := false;
    result := (FSubPartIndex < FSubPartCount) and (FPartIndex < FEndPartIndex);
    exit;
  end;
  if FSubPartIndex + 1 >= FSubPartCount then
  begin
    if FSubBrokenIndex + 1 >= FSubBrokenCount then
    begin
      if (FParagraphIndex + 1 >= FLayout.ParagraphCount) or
         (FLayout.FParagraph[FParagraphIndex + 1].brokenLineCount = 0) then
        exit(false) else
      begin
        inc(FPartIndex);
        inc(FParagraphIndex);
        FSubBrokenIndex:= 0;
        FSubPartIndex:= 0;
        Update;
      end;
    end else
    begin
      inc(FPartIndex);
      inc(FSubBrokenIndex);
      inc(FBrokenLineIndex);
      FSubPartIndex:= 0;
      with FLayout.FParagraph[FParagraphIndex] do
      begin
        FCurBroken := @brokenLines[FSubBrokenIndex];
        with FCurBroken^ do
        begin
          FSubPartCount:= partCount;
          FPartIndex:= FSubPartIndex + firstPartIndex;
        end;
      end;
    end;
  end else
  begin
    inc(FPartIndex);
    inc(FSubPartIndex);
  end;
  result := FPartIndex < FEndPartIndex;
end;

{ TPartInfo }

function TPartInfo.IsRightToLeft: boolean;
begin
  result := odd(bidiLevel);
end;

{ TBrokenLineInfo }

function TBrokenLineInfo.IsRightToLeft: boolean;
begin
  result := odd(bidiLevel);
end;

{ TBidiLayoutTree }

function TBidiLayoutTree.GetLayout: TBidiTextLayout;
begin
  result := FLayout;
end;

function TBidiLayoutTree.GetHeight: single;
begin
  result := FSize.y;
end;

function TBidiLayoutTree.GetCumulatedBidiPos: single;
begin
  result := BidiPos;
  if Assigned(Parent) then
    IncF(result, TBidiLayoutTree(Parent).CumulatedBidiPos);
end;

function TBidiLayoutTree.GetMaxWidth: single;
begin
  result := FMaxWidth;
end;

function TBidiLayoutTree.GetWidth: single;
begin
  result := FSize.x;
end;

procedure TBidiLayoutTree.UpdateBranchSize;
var
  i: Integer;
  last: TBidiLayoutTree;
begin
  if not IsLeaf then
  begin
    //write('Update branch size from ', round(FSize.x));
    if Count = 0 then
    begin
      FSize := PointF(0,0)
    end
    else
    begin
      last := TBidiLayoutTree(Branch[Count-1]);
      FSize.x := last.BidiPos + last.Width;
      FSize.y := 0;
      for i := 0 to Count-1 do
        FSize.y := max(FSize.y, TBidiLayoutTree(Branch[i]).Height);
    end;
    //writeln(' to ', round(FSize.x), ' (',inttostr(Count),')');
  end;
end;

constructor TBidiLayoutTree.Create(AData: pointer; AStartIndex,
  AEndIndex: integer; ABidiLevel: byte; AIsLeaf: boolean);
begin
  inherited Create(AData, AStartIndex, AEndIndex, ABidiLevel, AIsLeaf);
  FLayout := TBidiLayoutTreeData(AData^).Layout;
  FMaxWidth := TBidiLayoutTreeData(AData^).MaxWidth;
  if IsLeaf then
  begin
    FTextUTF8:= Layout.GetSameLevelString(StartIndex,EndIndex, FNonDiscardedCount);
    FSize := Layout.TextSizeBidiOverride(FTextUTF8, IsRightToLeft);
    //writeln('Created leaf ', round(FSize.x), ' of level ',BidiLevel);
  end
  else
  begin
    //writeln('Created branch of level ',BidiLevel);
    FTextUTF8:= '';
    FNonDiscardedCount:= 0;
    FSize := PointF(0,0);
  end;
end;

procedure TBidiLayoutTree.AddBranch(ABranch: TBidiTree);
var
  prev: TBidiLayoutTree;
begin
  inherited AddBranch(ABranch);
  if Count > 1 then
  begin
    prev := TBidiLayoutTree(Branch[Count-2]);
    TBidiLayoutTree(ABranch).FBidiPos:= prev.BidiPos + prev.Width;
  end;
  if (TBidiLayoutTree(ABranch).Width <> 0) or
     (TBidiLayoutTree(ABranch).Height <> 0) then
    UpdateBranchSize;
end;

procedure TBidiLayoutTree.Shorten(AEndIndex: integer);
begin
  inherited Shorten(AEndIndex);
  if IsLeaf then
  begin
    FTextUTF8:= Layout.GetSameLevelString(StartIndex,EndIndex, FNonDiscardedCount);
    FSize := Layout.TextSizeBidiOverride(FTextUTF8, IsRightToLeft);
    //writeln('Shortened leaf ', round(FSize.x));
  end else
    UpdateBranchSize;
end;

procedure TBidiLayoutTree.AfterFinish;
begin
  if Assigned(Parent) then
    TBidiLayoutTree(Parent).UpdateBranchSize;
end;

function TBidiLayoutTree.TrySplit: boolean;
var
  fitInfo, splitIndex: Integer;
  a: TUnicodeAnalysis;
  remain: Single;
begin
  if not IsLeaf then exit(false);
  if MaxWidth = EmptySingle then exit(false);
  remain := MaxWidth - CumulatedBidiPos;
  if Width > remain then
  begin
    fitInfo := Layout.TextFitInfoBidiOverride(FTextUTF8, remain, IsRightToLeft);
    if fitInfo < FNonDiscardedCount then
    begin
      //writeln('Splitting leaf ',round(Width), ' (max ',round(remain),')');
      splitIndex:= StartIndex;
      a:= Layout.FAnalysis;
      while fitInfo > 0 do
      begin
        while (splitIndex < EndIndex) and a.BidiInfo[splitIndex].IsDiscardable do
          Inc(splitIndex);
        if splitIndex < EndIndex then inc(splitIndex);
        dec(fitInfo);
      end;
      Shorten(splitIndex);
      TBidiLayoutTree(Parent).UpdateBranchSize;
      exit(true);
    end;
  end;
  exit(false);
end;

{ TBidiCaretPos }

procedure TBidiCaretPos.Transform(AMatrix: TAffineMatrix);
begin
  Top := AMatrix*Top;
  Bottom := AMatrix*Bottom;
  PreviousTop := AMatrix*PreviousTop;
  PreviousBottom := AMatrix*PreviousBottom;
end;

{ TBidiTextLayout }

function TBidiTextLayout.GetBrokenLineAffineBox(AIndex: integer): TAffineBox;
begin
  result := Matrix*TAffineBox.AffineBox(BrokenLineRectF[AIndex]);
end;

function TBidiTextLayout.GetBrokenLineCount: integer;
begin
  NeedLayout;
  result := FComputedBrokenLineCount;
end;

function TBidiTextLayout.GetBrokenLineEndCaret(AIndex: integer): TBidiCaretPos;
begin
  result := GetBrokenLineUntransformedEndCaret(AIndex);
  result.Transform(Matrix);
end;

function TBidiTextLayout.GetBrokenLineEndPart(AIndex: integer): integer;
begin
  with GetBrokenLineInfo(AIndex)^ do
    result := firstPartIndex + partCount;
end;

function TBidiTextLayout.GetBrokenLineStartPart(AIndex: integer): integer;
begin
  result := GetBrokenLineInfo(AIndex)^.firstPartIndex;
end;

function TBidiTextLayout.GetBrokenLineUntransformedEndCaret(AIndex: integer): TBidiCaretPos;
begin
  with GetBrokenLineInfo(AIndex)^ do
  begin
    result.Top.y := rectF.Top;
    if BrokenLineRightToLeft[AIndex] then
      result.Top.x := rectF.Left
    else
      result.Top.x := rectF.Right;
    result.Bottom.y := rectF.Bottom;
    result.Bottom.x := result.Top.x;
    result.RightToLeft := IsRightToLeft;
    result.PartIndex:= -1;
    result.PreviousTop := EmptyPointF;
    result.PreviousBottom := EmptyPointF;
    result.PreviousRightToLeft := result.RightToLeft;
  end;
end;

function TBidiTextLayout.GetBrokenLineEndIndex(AIndex: integer): integer;
begin
  result := GetBrokenLineInfo(AIndex)^.endIndex;
end;

function TBidiTextLayout.GetBrokenLineParagraphIndex(AIndex: integer): integer;
var
  ub: Integer;
begin
  ub := GetBrokenLineInfo(AIndex)^.unbrokenLineIndex;
  result := FAnalysis.UnbrokenLineParagraphIndex[ub];
end;

function TBidiTextLayout.GetBrokenLineUnbrokenIndex(AIndex: integer): integer;
begin
  result := GetBrokenLineInfo(AIndex)^.unbrokenLineIndex;
end;

function TBidiTextLayout.GetBrokenLineInfo(AIndex: integer): PBrokenLineInfo;
var
  minParaIndex, maxParaIndex, midParaIndex: integer;
begin
  NeedLayout;
  if (AIndex < 0) or (AIndex >= FComputedBrokenLineCount) then
    raise ERangeError.Create('Invalid index');
  minParaIndex := 0;
  maxParaIndex := ParagraphCount-1;
  result := nil;
  repeat
    if minParaIndex > maxParaIndex then
      InternalRangeError else
    if minParaIndex = maxParaIndex then
    begin
      result := @FParagraph[minParaIndex].brokenLines
                  [AIndex - FParagraph[minParaIndex].firstBrokenLineIndex];
      exit;
    end else
    begin
      midParaIndex := (minParaIndex + maxParaIndex + 1) shr 1;
      if AIndex < FParagraph[midParaIndex].firstBrokenLineIndex then
        maxParaIndex := midParaIndex-1
      else
        minParaIndex := midParaIndex;
    end;
  until false;
end;

function TBidiTextLayout.GetBrokenLineRectF(AIndex: integer): TRectF;
begin
  result := GetBrokenLineInfo(AIndex)^.rectF;
end;

function TBidiTextLayout.GetBrokenLineRightToLeft(AIndex: integer): boolean;
begin
  result := GetBrokenLineInfo(AIndex)^.IsRightToLeft;
end;

function TBidiTextLayout.GetBrokenLineStartCaret(AIndex: integer): TBidiCaretPos;
begin
  result := GetBrokenLineUntransformedStartCaret(AIndex);
  result.Transform(Matrix);
end;

function TBidiTextLayout.GetBrokenLineUntransformedStartCaret(AIndex: integer): TBidiCaretPos;
begin
  NeedLayout;
  with GetBrokenLineInfo(AIndex)^ do
  begin
    result.Top.y := rectF.Top;
    if BrokenLineRightToLeft[AIndex] then
      result.Top.x := rectF.Right
    else
      result.Top.x := rectF.Left;
    result.Bottom.y := rectF.Bottom;
    result.Bottom.x := result.Top.x;
    result.RightToLeft := IsRightToLeft;
    result.PartIndex:= -1;
    result.PreviousTop := EmptyPointF;
    result.PreviousBottom := EmptyPointF;
    result.PreviousRightToLeft := result.RightToLeft;
  end;
end;

function TBidiTextLayout.GetBrokenLineStartIndex(AIndex: integer): integer;
begin
  result := GetBrokenLineInfo(AIndex)^.startIndex;
end;

function TBidiTextLayout.GetBrokenLineUsedWidth(AIndex: integer): single;
begin
  result := GetBrokenLineInfo(AIndex)^.usedWidth;
end;

function TBidiTextLayout.GetCharCount: integer;
begin
  result := FAnalysis.CharCount;
end;

function TBidiTextLayout.GetFontBidiMode: TFontBidiMode;
begin
  result := FAnalysis.BidiMode;
end;

function TBidiTextLayout.GetLayoutComputed: boolean;
var
  i: Integer;
begin
  for i := 0 to ParagraphCount-1 do
    if not FParagraph[i].layoutComputed then exit(false);
  result := true;
end;

function TBidiTextLayout.GetLineHeight: single;
begin
  NeedLayout;
  result := FLineHeight;
end;

function TBidiTextLayout.GetMatrix: TAffineMatrix;
begin
  NeedLayout;
  result := FMatrix;
end;

function TBidiTextLayout.GetMatrixInverse: TAffineMatrix;
begin
  NeedLayout;
  result := FMatrixInverse;
end;

function TBidiTextLayout.GetParagraphAffineBox(AIndex: integer): TAffineBox;
begin
  result := Matrix*TAffineBox.AffineBox(ParagraphRectF[AIndex]);
end;

function TBidiTextLayout.GetParagraphAlignment(AIndex: integer): TBidiTextAlignment;
begin
  //layout not needed
  if (AIndex < 0) or (AIndex >= ParagraphCount) then
    raise ERangeError.Create('Invalid index');
  result := FParagraph[AIndex].alignment;
end;

function TBidiTextLayout.GetParagraphCount: integer;
begin
  result := FAnalysis.ParagraphCount;
end;

function TBidiTextLayout.GetParagraphEndBrokenLine(AIndex: integer): integer;
begin
  if AIndex = ParagraphCount-1 then
    result := BrokenLineCount
  else
    result := GetParagraphInfo(AIndex+1)^.firstBrokenLineIndex;
end;

function TBidiTextLayout.GetParagraphEndIndex(AIndex: integer): integer;
begin
  result := FAnalysis.ParagraphEndIndex[AIndex];
end;

function TBidiTextLayout.GetParagraphEndIndexBeforeParagraphSeparator(AIndex: integer): integer;
begin
  result := FAnalysis.ParagraphEndIndexBeforeParagraphSeparator[AIndex];
end;

function TBidiTextLayout.GetParagraphEndPart(AIndex: integer): integer;
begin
  if AIndex = ParagraphCount-1 then
    result := PartCount
  else
    result := GetParagraphInfo(AIndex+1)^.firstPartIndex;
end;

function TBidiTextLayout.GetParagraphInfo(AIndex: integer): PParagraphInfo;
begin
  NeedLayout;
  if (AIndex < 0) or (AIndex >= ParagraphCount) then
    raise ERangeError.Create('Paragraph index out of bounds');
  result := @FParagraph[AIndex];
end;

function TBidiTextLayout.GetParagraphRectF(AIndex: integer): TRectF;
begin
  result := GetParagraphInfo(AIndex)^.rectF;
end;

function TBidiTextLayout.GetParagraphRightToLeft(AIndex: integer): boolean;
begin
  result := FAnalysis.ParagraphRightToLeft[AIndex];
end;

function TBidiTextLayout.GetParagraphStartBrokenLine(AIndex: integer): integer;
begin
  result := GetParagraphInfo(AIndex)^.firstBrokenLineIndex;
end;

function TBidiTextLayout.GetParagraphStartIndex(AIndex: integer): integer;
begin
  result := FAnalysis.ParagraphStartIndex[AIndex];
end;

function TBidiTextLayout.GetParagraphStartPart(AIndex: integer): integer;
begin
  result := GetParagraphInfo(AIndex)^.firstPartIndex;
end;

function TBidiTextLayout.GetPartAffineBox(AIndex: integer): TAffineBox;
begin
  result := Matrix*TAffineBox.AffineBox(PartRectF[AIndex]);
end;

function TBidiTextLayout.GetPartBrokenLineIndex(AIndex: integer): integer;
begin
  result := GetPartInfo(AIndex)^.brokenLineIndex;
end;

function TBidiTextLayout.GetPartCount: integer;
begin
  NeedLayout;
  result := FComputedPartCount;
end;

function TBidiTextLayout.GetPartEnumerator(AFirstPart: integer): TPartEnumerator;
begin
  result := GetPartEnumerator(AFirstPart, FComputedPartCount);
end;

function TBidiTextLayout.GetPartEnumerator(AFirstPart, ALastPartPlus1: integer): TPartEnumerator;
var
  minParaIndex,maxParaIndex,midParaIndex: integer;
  minBrokenIndex, maxBrokenIndex, midBrokenIndex: Integer;
begin
  if (AFirstPart < 0) or (AFirstPart > FComputedPartCount) or
     (ALastPartPlus1 < 0) or (ALastPartPlus1 > FComputedPartCount)  then
    raise ERangeError.Create('Invalid start index');
  minParaIndex:= 0;
  maxParaIndex:= ParagraphCount - 1;
  repeat
    if minParaIndex > maxParaIndex then
      InternalRangeError else
    if minParaIndex = maxParaIndex then
    with FParagraph[minParaIndex] do
    begin
      if brokenLineCount = 0 then
      begin
        result := TPartEnumerator.New(self, minParaIndex, 0, 0, ALastPartPlus1);
        exit;
      end;
      minBrokenIndex := 0;
      maxBrokenIndex := brokenLineCount-1;
      repeat
        if minBrokenIndex > maxBrokenIndex then
          InternalRangeError else
        if minBrokenIndex = maxBrokenIndex then
        begin
          result := TPartEnumerator.New(self, minParaIndex, minBrokenIndex,
                      AFirstPart - brokenLines[minBrokenIndex].firstPartIndex,
                      ALastPartPlus1);
          exit;
        end else
        begin
          midBrokenIndex := (minBrokenIndex + maxBrokenIndex + 1) shr 1;
          if AFirstPart < brokenLines[midBrokenIndex].firstPartIndex then
            maxBrokenIndex := midBrokenIndex-1
          else
            minBrokenIndex := midBrokenIndex;
        end;
      until false;
    end else
    begin
      midParaIndex := (minParaIndex + maxParaIndex + 1) shr 1;
      if AFirstPart < FParagraph[midParaIndex].firstPartIndex then
        maxParaIndex := midParaIndex-1
      else
        minParaIndex := midParaIndex;
    end;
  until false;
end;


function TBidiTextLayout.GetPartInfo(AIndex: integer): PPartInfo;
var
  partEnum: TPartEnumerator;
begin
  partEnum := GetPartEnumerator(AIndex);
  if not partEnum.GetNext then raise ERangeError.Create('Invalid index');
  result := partEnum.PartInfo;
end;

function TBidiTextLayout.GetPartEndIndex(AIndex: integer): integer;
begin
  result := GetPartInfo(AIndex)^.endIndex;
end;

function TBidiTextLayout.GetPartRectF(AIndex: integer): TRectF;
begin
  result := GetPartInfo(AIndex)^.rectF;
end;

function TBidiTextLayout.GetPartRightToLeft(AIndex: integer): boolean;
begin
  result := GetPartInfo(AIndex)^.IsRightToLeft;
end;

function TBidiTextLayout.GetPartStartIndex(AIndex: integer): integer;
begin
  result := GetPartInfo(AIndex)^.startIndex;
end;

function TBidiTextLayout.GetText: string;
begin
  result := FAnalysis.TextUTF8;
end;

function TBidiTextLayout.GetTotalTextHeight: single;
begin
  NeedLayout;
  result := FParagraph[ParagraphCount-1].rectF.Bottom - FParagraph[0].rectF.Top;
end;

function TBidiTextLayout.GetUnicodeChar(APosition0: integer): LongWord;
begin
  result := FAnalysis.UnicodeChar[APosition0];
end;

function TBidiTextLayout.GetUsedWidth: single;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to BrokenLineCount-1 do
    result := max(result, BrokenLineUsedWidth[i]);
end;

function TBidiTextLayout.GetUTF8Char(APosition0: integer): string4;
begin
  result := FAnalysis.UTF8Char[APosition0];
end;

procedure TBidiTextLayout.SetAvailableHeight(AValue: single);
var
  i: Integer;
begin
  if FAvailableHeight=AValue then Exit;
  FAvailableHeight:=AValue;
  for i := 0 to ParagraphCount-1 do
    TrimParagraphLayoutVertically(i);
end;

procedure TBidiTextLayout.SetAvailableWidth(AValue: single);
begin
  if FAvailableWidth=AValue then Exit;
  FAvailableWidth:=AValue;
  InvalidateLayout;
end;

procedure TBidiTextLayout.SetFontBidiMode(AValue: TFontBidiMode);
begin
  FAnalysis.BidiMode := AValue;
end;

procedure TBidiTextLayout.SetFontRenderer(AValue: TBGRACustomFontRenderer);
begin
  if FRenderer=AValue then Exit;
  FRenderer:=AValue;
  InvalidateLayout;
end;

procedure TBidiTextLayout.SetParagraphAlignment(AIndex: integer;
  AValue: TBidiTextAlignment);
var
  brokenCount: Integer;
begin
  if (AIndex < 0) or (AIndex >= ParagraphCount) then
    raise ERangeError.Create('Paragraph index out of bounds');
  FParagraph[AIndex].alignment := AValue;
  InvalidateParagraphLayout(AIndex);
  if Assigned(FOnBrokenLinesChanged) then
  begin
    brokenCount := FParagraph[AIndex].brokenLineCount;
    FOnBrokenLinesChanged(self, AIndex, 0, brokenCount, brokenCount,
      brokenCount, brokenCount);
  end;
end;

procedure TBidiTextLayout.SetParagraphSpacingAbove(AValue: single);
begin
  if FParagraphSpacingAbove=AValue then Exit;
  FParagraphSpacingAbove:=AValue;
  InvalidateLayout;
end;

procedure TBidiTextLayout.SetParagraphSpacingBelow(AValue: single);
begin
  if FParagraphSpacingBelow=AValue then Exit;
  FParagraphSpacingBelow:=AValue;
  InvalidateLayout;
end;

procedure TBidiTextLayout.SetTabSize(AValue: single);
begin
  if FTabSize=AValue then Exit;
  FTabSize:=AValue;
  InvalidateLayout;
end;

procedure TBidiTextLayout.SetTopLeft(AValue: TPointF);
begin
  if FTopLeft=AValue then Exit;
  FTopLeft:=AValue;
  if LayoutComputed then ComputeMatrix;
end;

procedure TBidiTextLayout.ComputeMatrix;
begin
  FMatrix := AffineMatrixTranslation(FTopLeft.x, FTopLeft.y)*AffineMatrixRotationDeg(-GetFontOrientation);
  FMatrixInverse := AffineMatrixInverse(FMatrix);
end;

procedure TBidiTextLayout.BidiModeChanged(ASender: TObject);
begin
  InvalidateLayout;
end;

procedure TBidiTextLayout.CharDeleted(ASender: TObject;
  AParagraphIndex: integer; ACharStart, ACharCount: integer);
var
  i, charEnd, j, partIndex: Integer;
  curPart: PPartInfo;
begin
  InvalidateParagraphLayout(AParagraphIndex);
  charEnd := ACharStart + ACharCount;
  with FParagraph[AParagraphIndex] do
  begin
    for j := 0 to brokenLineCount-1 do
    with brokenLines[j] do
    begin
      // is broken line affected ?
      if (startIndex < charEnd) and (endIndex > ACharStart) then
      begin
        for partIndex := 0 to partCount-1 do
        begin
          curPart := @parts[partIndex];
          // is part affected ?
          if (curPart^.startIndex < charEnd)
            or (curPart^.endIndex > ACharStart) then
          begin
            curPart^.modified := true;
            // is part completely deleted ?
            if (curPart^.startIndex >= ACharStart) and
               (curPart^.endIndex <= charEnd) then
            begin
              curPart^.startIndex := ACharStart;
              curPart^.endIndex := ACharStart;
            end else
            begin
              // part partially deleted
              if curPart^.startIndex < ACharStart then
                curPart^.endIndex := ACharStart
              else if curPart^.endIndex > charEnd then
              begin
                curPart^.startIndex := charEnd - ACharCount;
                dec(curPart^.endIndex, ACharCount);
              end;
            end;
          end else
          if curPart^.startIndex >= charEnd then // part located after deletion
          begin
            dec(curPart^.startIndex, ACharCount);
            dec(curPart^.endIndex, ACharCount);
          end;
        end;
        dec(endIndex, ACharCount);
      end else
      if startIndex >= charEnd then // broken line located after deletion
      begin
        dec(startIndex, ACharCount);
        dec(endIndex, ACharCount);
        for partIndex := 0 to partCount-1 do
        begin
          curPart := @parts[partIndex];
          dec(curPart^.startIndex, ACharCount);
          dec(curPart^.endIndex, ACharCount);
        end;
      end;
    end;
  end;
  for i := AParagraphIndex + 1 to high(FParagraph) do
    OffsetParagraphCharIndex(i, -ACharCount);
end;

function TBidiTextLayout.TextSizeBidiOverride(sUTF8: string;
  ARightToLeft: boolean): TPointF;
begin
  AddOverrideIfNecessary(sUTF8, ARightToLeft);
  result := FRenderer.TextSizeAngleF(sUTF8, FRenderer.FontOrientation);
end;

procedure TBidiTextLayout.ParagraphSplit(ASender: TObject;
  AParagraphIndex: integer; ACharIndex: integer);
var
  i, j, subBrokenIndex, brokenMoveCount: Integer;
  curPara, nextPara: PParagraphInfo;
begin
  if (AParagraphIndex < 0) or (AParagraphIndex > high(FParagraph)) then
    raise exception.Create('Paragrah index out of bounds (0 <= '+inttostr(AParagraphIndex)+' <= '+inttostr(high(FParagraph))+')');

  setlength(FParagraph, length(FParagraph)+1);
  for i := high(FParagraph) downto AParagraphIndex+2 do
    FParagraph[i] := FParagraph[i-1];

  curPara := @FParagraph[AParagraphIndex];
  nextPara := @FParagraph[AParagraphIndex+1];

  subBrokenIndex := curPara^.brokenLineCount;
  for j := 0 to curPara^.brokenLineCount-1 do
    if (curPara^.brokenLines[j].startIndex <= ACharIndex) and
       (curPara^.brokenLines[j].endIndex > ACharIndex) then
    begin
      subBrokenIndex := j;
      break;
    end else
    if (curPara^.brokenLines[j].startIndex > ACharIndex) then
    begin
      subBrokenIndex := max(j-1, 0);
      break;
    end;
  brokenMoveCount := curPara^.brokenLineCount - (subBrokenIndex + 1);
  if brokenMoveCount < 0 then brokenMoveCount := 0;

  nextPara^.alignment := curPara^.alignment;
  nextPara^.layoutComputed := false;
  nextPara^.overflow := curPara^.overflow;
  nextPara^.rectF := EmptyRectF;
  nextPara^.firstBrokenLineIndex:= curPara^.firstBrokenLineIndex + curPara^.brokenLineCount - brokenMoveCount;
  if brokenMoveCount > 0 then
    nextPara^.firstPartIndex := curPara^.brokenLines[curPara^.brokenLineCount - brokenMoveCount].firstPartIndex
  else
  begin
    if curPara^.brokenLineCount > 0 then
      with curPara^.brokenLines[curPara^.brokenLineCount - 1] do
        nextPara^.firstPartIndex := firstPartIndex + partCount
        else nextPara^.firstPartIndex := curPara^.firstPartIndex;
  end;
  nextPara^.brokenLineCount:= brokenMoveCount;
  setlength(nextPara^.brokenLines, brokenMoveCount);
  for j := 0 to brokenMoveCount - 1 do
    nextPara^.brokenLines[j] := curPara^.brokenLines[curPara^.brokenLineCount - brokenMoveCount + j];
  dec(curPara^.brokenLineCount, brokenMoveCount);
  InternalInvalidateParagraphLayout(AParagraphIndex);
  InternalInvalidateParagraphLayout(AParagraphIndex+1);
  if Assigned(FOnParagraphSplit) then
    FOnParagraphSplit(self, AParagraphIndex, subBrokenIndex, ACharIndex);
end;

procedure TBidiTextLayout.InternalParagraphDeleted(AParagraphIndex: integer);
var
  i, deltaBroken, deltaPart: Integer;
  deltaY: Single;
begin
  if (AParagraphIndex < 0) or (AParagraphIndex >= ParagraphCount) then exit;
  deltaY := -FParagraph[AParagraphIndex].rectF.Height;
  deltaBroken := -FParagraph[AParagraphIndex].brokenLineCount;
  deltaPart := 0;
  for i := 0 to FParagraph[AParagraphIndex].brokenLineCount-1 do
    dec(deltaPart, FParagraph[AParagraphIndex].brokenLines[i].partCount);

  for i := AParagraphIndex to high(FParagraph)-1 do
  begin
    FParagraph[i] := FParagraph[i+1];
    OffsetParagraph(i, deltaY, deltaBroken, deltaPart);
  end;
  setlength(FParagraph, length(FParagraph)-1);
  inc(FComputedBrokenLineCount, deltaBroken);
  inc(FComputedPartCount, deltaPart);
end;

procedure TBidiTextLayout.CharInserted(ASender: TObject;
  AParagraphIndex: integer; ACharStart, ACharCount: integer);
var
  i, j, partIndex: Integer;
begin
  InvalidateParagraphLayout(AParagraphIndex);
  with FParagraph[AParagraphIndex] do
  begin
    for j := 0 to brokenLineCount-1 do
    with brokenLines[j] do
    begin
      // is broken line affected
      if (ACharStart >= startIndex) and (ACharStart < endIndex) then
      begin
        for partIndex := 0 to partCount-1 do
          with parts[partIndex] do
          begin
            // is part affected
            if (ACharStart > startIndex) and (ACharStart < endIndex) then
            begin
              modified := true;
              inc(endIndex, ACharCount);
            end else
            if (ACharStart <= startIndex) then // part located after insertion
            begin
              inc(startIndex, ACharCount);
              inc(endIndex, ACharCount);
            end;
          end;
        inc(endIndex, ACharCount);
      end else
      if (ACharStart <= StartIndex) then // broken line located after insertion
      begin
        inc(startIndex, ACharCount);
        inc(endIndex, ACharCount);
        for partIndex := 0 to partCount-1 do
          with parts[partIndex] do
          begin
            inc(startIndex, ACharCount);
            inc(endIndex, ACharCount);
          end;
      end;
    end;
  end;
  for i := AParagraphIndex + 1 to high(FParagraph) do
    OffsetParagraphCharIndex(i, ACharCount);
end;

procedure TBidiTextLayout.AnalysisChanged(ASender: TObject;
  AParagraphIndex: integer; ACharStart, ACharCount: integer);
begin
  InvalidateParagraphLayout(AParagraphIndex);
end;

procedure TBidiTextLayout.ParagraphMergedWithNext(ASender: TObject;
  AParagraphIndex: integer);
var
  i, mergedBrokenLineCount, prevBrokenLineCount: Integer;
  curPara: PParagraphInfo;
begin
  if (AParagraphIndex < 0) or (AParagraphIndex >= high(FParagraph)) then
    InternalRangeError;
  curPara := @FParagraph[AParagraphIndex];
  prevBrokenLineCount := curPara^.brokenLineCount;
  mergedBrokenLineCount := prevBrokenLineCount + FParagraph[AParagraphIndex+1].brokenLineCount;
  if length(curPara^.brokenLines) < mergedBrokenLineCount then
    setlength(curPara^.brokenLines, mergedBrokenLineCount);
  for i := 0 to FParagraph[AParagraphIndex+1].brokenLineCount-1 do
    curPara^.brokenLines[prevBrokenLineCount + i] :=
      FParagraph[AParagraphIndex+1].brokenLines[i];
  curPara^.brokenLineCount := mergedBrokenLineCount;
  curPara^.rectF.Bottom:= FParagraph[AParagraphIndex+1].rectF.Bottom;
  curPara^.overflow := curPara^.overflow or FParagraph[AParagraphIndex+1].overflow;
  for i := AParagraphIndex + 1 to high(FParagraph)-1 do
    FParagraph[i] := FParagraph[i+1];
  setlength(FParagraph, length(FParagraph) - 1);
  InternalInvalidateParagraphLayout(AParagraphIndex);
  if Assigned(FOnParagraphMergedWithNext) then
    FOnParagraphMergedWithNext(self, AParagraphIndex);
end;

procedure TBidiTextLayout.ParagraphDeleted(ASender: TObject;
  AParagraphIndex: integer);
begin
  InternalParagraphDeleted(AParagraphIndex);
  If Assigned(FOnParagraphDeleted) then
    FOnParagraphDeleted(self, AParagraphIndex);
end;

function TBidiTextLayout.TextSizeBidiOverrideSplit(AStartIndex, AEndIndex: integer;
  ARightToLeft: boolean; ASplitIndex: integer): TPointF;
var checkIndex: integer;
  s: String;
begin
  if ASplitIndex <= AStartIndex then
  begin
    s := FAnalysis.CopyTextUTF8(AStartIndex, AEndIndex-AStartIndex);
    result := TextSizeBidiOverride(s, ARightToLeft);
    result.x := 0;
    exit;
  end;

  s := FAnalysis.CopyTextUTF8(AStartIndex, ASplitIndex-AStartIndex);
  checkIndex := ASplitIndex-1;
  while (checkIndex > AStartIndex) and
    FAnalysis.BidiInfo[checkIndex].IsLigatureTransparent do dec(checkIndex);
  if (ARightToLeft and FAnalysis.BidiInfo[checkIndex].HasLigatureLeft) or
     (not ARightToLeft and FAnalysis.BidiInfo[checkIndex].HasLigatureRight) then
    s := s+UnicodeCharToUTF8(UNICODE_ZERO_WIDTH_JOINER);
  result := TextSizeBidiOverride(s, ARightToLeft);
end;

function TBidiTextLayout.TextFitInfoBidiOverride(sUTF8: string; AWidth: single;
  ARightToLeft: boolean): integer;
var
  over: Boolean;
  i: Integer;
  p, pStart, pEnd: PChar;
  u: LongWord;
begin
  if sUTF8 = '' then exit(0);
  over := AddOverrideIfNecessary(sUTF8, ARightToLeft);

  result := FRenderer.TextFitInfoF(sUTF8, AWidth);
  if over then dec(result);

  //check that position is not a combining mark
  pEnd := @sUTF8[length(sUTF8)];
  pStart := @sUTF8[1];
  if over then inc(pStart, UTF8CharacterLength(pStart));
  p := @sUTF8[1];
  for i := 1 to result do
  begin
    inc(p, UTF8CharacterLength(p));
    if p > pEnd then break;
  end;
  if p <= pEnd then
  begin
    while (result > 0) and (p > pStart) do
    begin
      u := UTF8CodepointToUnicode(p, UTF8CharacterLength(p));
      if GetUnicodeBidiClassEx(u) = ubcCombiningLeftToRight then
      begin
        dec(p);
        while (p >= pStart) and (p^ in[#$80..#$BF]) do dec(p);
        dec(result);
      end else
        break;
    end;
  end;
end;

function TBidiTextLayout.GetFontFullHeight: single;
begin
  result := FRenderer.TextSizeAngleF('Hg', FRenderer.FontOrientation).y;
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
  if sUTF8 = #9 then exit;
  AddOverrideIfNecessary(sUTF8, ARightToLeft);

  if FTexture <> nil then
    FRenderer.TextOut(ADest, x,y, sUTF8, FTexture, taLeftJustify, ARightToLeft)
  else
    FRenderer.TextOut(ADest, x,y, sUTF8, FColor, taLeftJustify, ARightToLeft);
end;

procedure TBidiTextLayout.TextPathBidiOverride(ADest: IBGRAPath; x,
  y: single; sUTF8: string; ARightToLeft: boolean);
begin
  if sUTF8 = #9 then exit;
  AddOverrideIfNecessary(sUTF8, ARightToLeft);

  FRenderer.CopyTextPathTo(ADest, x,y, sUTF8, taLeftJustify, ARightToLeft)
end;

function TBidiTextLayout.AddOverrideIfNecessary(var sUTF8: string;
  ARightToLeft: boolean): boolean;
var
  p: PChar;
  pEnd: Pointer;
  add, hasStrong: boolean;
  charLen: Integer;
  u: LongWord;
  curBidi: TUnicodeBidiClass;
  isSpacing: boolean;
begin
  if sUTF8 = '' then exit(false);
  isSpacing:= true;
  p := @sUTF8[1];
  pEnd := p + length(sUTF8);
  add := false;
  hasStrong := false;
  while p < pEnd do
  begin
    charLen := UTF8CharacterLength(p);
    if (charLen = 0) or (p+charLen > pEnd) then break;
    u := UTF8CodepointToUnicode(p, charLen);
    curBidi := GetUnicodeBidiClass(u);
    if curBidi <> ubcWhiteSpace then isSpacing:= false;
    if curBidi in[ubcLeftToRight,ubcRightToLeft,ubcArabicLetter] then
      hasStrong := true;
    if (curBidi = ubcLeftToRight) and ARightToLeft then
    begin
      add := true;
      break;
    end else
    if (curBidi in[ubcRightToLeft,ubcArabicLetter]) and not ARightToLeft then
    begin
      add := true;
      break;
    end;
    inc(p,charLen);
  end;
  if not hasStrong and ARightToLeft and not isSpacing then add := true;
  if add then
  begin
    if ARightToLeft then
      sUTF8 := UnicodeCharToUTF8(UNICODE_RIGHT_TO_LEFT_OVERRIDE)+ sUTF8
    else
      sUTF8 := UnicodeCharToUTF8(UNICODE_LEFT_TO_RIGHT_OVERRIDE)+ sUTF8;
    exit(true);
  end
  else exit(false);
end;

function TBidiTextLayout.GetTextPart(APartIndex: integer;
  AAddOverrideIfNecessary: boolean): string;
begin
  result := FAnalysis.CopyTextUTF8(PartStartIndex[APartIndex],
    PartEndIndex[APartIndex] - PartStartIndex[APartIndex]);
  if AAddOverrideIfNecessary then
    AddOverrideIfNecessary(result, PartRightToLeft[APartIndex]);
end;

procedure TBidiTextLayout.AddPart(AStartIndex, AEndIndex: integer;
  ABidiLevel: byte; ARectF: TRectF; APosCorrection: TPointF;
  ABrokenLineIndex: integer; ABrokenLine: PBrokenLineInfo);
begin
  if ABrokenLine^.partCount >= length(ABrokenLine^.parts) then
    setlength(ABrokenLine^.parts, length(ABrokenLine^.parts)*2+8);

  with ABrokenLine^.parts[ABrokenLine^.partCount] do
  begin
    startIndex:= AStartIndex;
    endIndex:= AEndIndex;
    bidiLevel := ABidiLevel;
    rectF := ARectF;
    posCorrection := APosCorrection;
    brokenLineIndex:= ABrokenLineIndex;
    modified := false;
  end;
  inc(ABrokenLine^.partCount)
end;

function TBidiTextLayout.GetSameLevelString(startIndex, endIndex: integer): string;
var
  nonDiscardedCount: integer;
begin
  result := GetSameLevelString(startIndex,endIndex,nonDiscardedCount);
end;

function TBidiTextLayout.GetSameLevelString(startIndex, endIndex: integer; out nonDiscardedCount: integer): string;
begin
  result := FAnalysis.CopyTextUTF8DiscardChars(startIndex, endIndex, nonDiscardedCount);
end;

function TBidiTextLayout.ComputeBidiTree(AMaxWidth: single; startIndex,
  endIndex: integer; bidiLevel: byte): TBidiTree;
var
  data: TBidiLayoutTreeData;
begin
  data.MaxWidth := AMaxWidth;
  data.Layout := self;
  result := FAnalysis.CreateBidiTree(TBidiLayoutTree, @data, startIndex, endIndex, bidiLevel);
end;

constructor TBidiTextLayout.Create(AFontRenderer: TBGRACustomFontRenderer; sUTF8: string);
begin
  Init(sUTF8, fbmAuto);
  FRenderer := AFontRenderer;
end;

constructor TBidiTextLayout.Create(AFontRenderer: TBGRACustomFontRenderer; sUTF8: string; ARightToLeft: boolean);
begin
  if ARightToLeft then
    Init(sUTF8, fbmRightToLeft)
  else
    Init(sUTF8, fbmLeftToRight);
  FRenderer := AFontRenderer;
end;

constructor TBidiTextLayout.Create(AFontRenderer: TBGRACustomFontRenderer;
  sUTF8: string; AFontBidiMode: TFontBidiMode);
begin
  Init(sUTF8, AFontBidiMode);
  FRenderer := AFontRenderer;
end;

destructor TBidiTextLayout.Destroy;
begin
  FAnalysis.Free;
  inherited Destroy;
end;

procedure TBidiTextLayout.SetLayout(ARect: TRectF);
begin
  TopLeft := ARect.TopLeft;
  AvailableWidth:= ARect.Width;
  AvailableHeight:= ARect.Height;
end;

procedure TBidiTextLayout.InvalidateLayout;
var
  i: Integer;
begin
  for i := 0 to ParagraphCount-1 do
    InvalidateParagraphLayout(i);
end;

procedure TBidiTextLayout.ComputeLayoutIfNeeded;
begin
  if not LayoutComputed then ComputeLayout;
end;

procedure TBidiTextLayout.InvalidateParagraphLayout(AParagraphIndex: integer);
begin
  InternalInvalidateParagraphLayout(AParagraphIndex);
  if Assigned(FOnParagraphChanged) then
    FOnParagraphChanged(self, AParagraphIndex);
end;

procedure TBidiTextLayout.InternalInvalidateParagraphLayout(
  AParagraphIndex: integer);
begin
  if (AParagraphIndex >= 0) and (AParagraphIndex <= high(FParagraph)) then
    FParagraph[AParagraphIndex].layoutComputed := false;
end;

procedure TBidiTextLayout.OffsetParagraph(AParagraphIndex: integer;
  ADeltaY: single; ADeltaBroken, ADeltaPart: integer);
var subBrokenIndex: integer;
  curBroken: PBrokenLineInfo;
  partIndex: integer;
begin
  if (ADeltaY = 0) and (ADeltaBroken = 0) and (ADeltaPart = 0) then exit;
  with FParagraph[AParagraphIndex] do
  begin
    rectF.Offset(0, ADeltaY);
    inc(firstBrokenLineIndex, ADeltaBroken);
    inc(firstPartIndex, ADeltaPart);
    for subBrokenIndex := 0 to brokenLineCount-1 do
    begin
      curBroken := @brokenLines[subBrokenIndex];
      curBroken^.rectF.Offset(0, ADeltaY);
      inc(curBroken^.firstPartIndex, ADeltaPart);
      for partIndex := 0 to curBroken^.partCount-1 do
      with curBroken^.parts[partIndex] do
      begin
        inc(brokenLineIndex, ADeltaBroken);
        rectF.Offset(0, ADeltaY);
      end;
    end;
    if ADeltaY <> 0 then
      TrimParagraphLayoutVertically(AParagraphIndex);
  end;
end;

procedure TBidiTextLayout.OffsetParagraphCharIndex(AParagraphIndex: integer;
  ADeltaChar: integer);
var
  j, k: Integer;
begin
  with FParagraph[AParagraphIndex] do
  begin
    for j := 0 to brokenLineCount-1 do
    with brokenLines[j] do
    begin
      inc(startIndex, ADeltaChar);
      inc(endIndex, ADeltaChar);
      for k := 0 to partCount-1 do
      begin
        inc(parts[k].startIndex, ADeltaChar);
        inc(parts[k].endIndex, ADeltaChar);
      end;
    end;
  end;
end;

procedure TBidiTextLayout.TrimParagraphLayoutVertically(AParagraphIndex: integer);
var
  subBrokenIndex, nextDeltaBroken, nextDeltaPart, i: Integer;
begin
  if (AvailableHeight = EmptySingle) or (AParagraphIndex < 0) or
    (AParagraphIndex > high(FParagraph)) then exit;
  with FParagraph[AParagraphIndex] do
  begin
    if not layoutComputed then exit;
    if overflow and (rectF.Bottom < AvailableHeight) then
    begin
      layoutComputed:= false;
      if Assigned(FOnParagraphVerticalTrimChanged) then
        FOnParagraphVerticalTrimChanged(self, AParagraphIndex);
    end else
    if (rectF.Bottom > AvailableHeight) then
    begin
      for subBrokenIndex := 0 to brokenLineCount-1 do
      begin
        //there must be at least one broken line in the text
        if (AParagraphIndex = 0) and (subBrokenIndex = 0) then continue;
        if brokenLines[subBrokenIndex].rectF.Top >= AvailableHeight then
        begin
          nextDeltaBroken := 0;
          nextDeltaPart := 0;
          for i := subBrokenIndex to brokenLineCount-1 do
          begin
            dec(nextDeltaBroken);
            dec(nextDeltaPart, brokenLines[i].partCount);
          end;
          brokenLineCount := subBrokenIndex;
          for i := AParagraphIndex+1 to ParagraphCount-1 do
            OffsetParagraph(i, 0, nextDeltaBroken, nextDeltaPart);
          inc(FComputedPartCount, nextDeltaPart);
          inc(FComputedBrokenLineCount, nextDeltaBroken);
          overflow := true;
          if subBrokenIndex > 0 then
            rectF.Bottom := brokenLines[subBrokenIndex-1].rectF.Bottom + ParagraphSpacingBelow*FLineHeight
            else rectF.Bottom := rectF.Top + ParagraphSpacingAbove*FLineHeight;
          if Assigned(FOnParagraphVerticalTrimChanged) then
            FOnParagraphVerticalTrimChanged(self, AParagraphIndex);
          break;
        end;
      end;
    end;
  end;
end;

procedure TBidiTextLayout.DrawText(ADest: TBGRACustomBitmap);
begin
  DrawText(ADest, BGRABlack);
end;

procedure TBidiTextLayout.DrawText(ADest: TBGRACustomBitmap; AColor: TBGRAPixel);
begin
  FColor := AColor;
  InternalDrawText(ADest);
end;

procedure TBidiTextLayout.DrawText(ADest: TBGRACustomBitmap;
  ATexture: IBGRAScanner);
begin
  FColor := BGRAWhite;
  FTexture := ATexture;
  InternalDrawText(ADest);
  FTexture := nil;
end;

procedure TBidiTextLayout.PathText(ADest: IBGRAPath);
begin
  InternalPathText(ADest);
end;

procedure TBidiTextLayout.PathText(ADest: IBGRAPath; AClipRect: TRect);
begin
  InternalPathText(ADest, AClipRect);
end;

procedure TBidiTextLayout.DrawTextParts(ADest: TBGRACustomBitmap; AFirstPart,
  ALastPartPlus1: integer);
begin
  DrawTextParts(ADest, BGRABlack, AFirstPart, ALastPartPlus1);
end;

procedure TBidiTextLayout.DrawTextParts(ADest: TBGRACustomBitmap;
  AColor: TBGRAPixel; AFirstPart, ALastPartPlus1: integer);
begin
  FColor := AColor;
  InternalDrawTextParts(ADest, AFirstPart, ALastPartPlus1);
end;

procedure TBidiTextLayout.DrawTextParts(ADest: TBGRACustomBitmap;
  ATexture: IBGRAScanner; AFirstPart, ALastPartPlus1: integer);
begin
  FColor := BGRAWhite;
  FTexture := ATexture;
  InternalDrawTextParts(ADest, AFirstPart, ALastPartPlus1);
  FTexture := nil;
end;

procedure TBidiTextLayout.PathTextParts(ADest: IBGRAPath; AFirstPart,
  ALastPartPlus1: integer);
begin
  InternalPathTextParts(ADest, AFirstPart, ALastPartPlus1);
end;

procedure TBidiTextLayout.PathTextParts(ADest: IBGRAPath; AClipRect: TRect;
  AFirstPart, ALastPartPlus1: integer);
begin
  InternalPathTextParts(ADest, AClipRect, AFirstPart, ALastPartPlus1);
end;

procedure TBidiTextLayout.DrawParagraphs(ADest: TBGRACustomBitmap;
  AFirstPara, ALastParaPlus1: integer);
begin
  if ALastParaPlus1 <= AFirstPara then exit;
  DrawTextParts(ADest, ParagraphStartPart[AFirstPara], ParagraphEndPart[ALastParaPlus1-1]);
end;

procedure TBidiTextLayout.DrawParagraphs(ADest: TBGRACustomBitmap;
  AColor: TBGRAPixel; AFirstPara, ALastParaPlus1: integer);
begin
  if ALastParaPlus1 <= AFirstPara then exit;
  DrawTextParts(ADest, AColor, ParagraphStartPart[AFirstPara], ParagraphEndPart[ALastParaPlus1-1]);
end;

procedure TBidiTextLayout.DrawParagraphs(ADest: TBGRACustomBitmap;
  ATexture: IBGRAScanner; AFirstPara, ALastParaPlus1: integer);
begin
  if ALastParaPlus1 <= AFirstPara then exit;
  DrawTextParts(ADest, ATexture, ParagraphStartPart[AFirstPara], ParagraphEndPart[ALastParaPlus1-1]);
end;

procedure TBidiTextLayout.PathParagraphs(ADest: IBGRAPath; AFirstPara,
  ALastParaPlus1: integer);
begin
  if ALastParaPlus1 <= AFirstPara then exit;
  PathTextParts(ADest, ParagraphStartPart[AFirstPara], ParagraphEndPart[ALastParaPlus1-1]);
end;

procedure TBidiTextLayout.PathParagraphs(ADest: IBGRAPath; AClipRect: TRect;
  AFirstPara, ALastParaPlus1: integer);
begin
  if ALastParaPlus1 <= AFirstPara then exit;
  PathTextParts(ADest, AClipRect, ParagraphStartPart[AFirstPara], ParagraphEndPart[ALastParaPlus1-1]);
end;

procedure TBidiTextLayout.DrawBrokenLines(ADest: TBGRACustomBitmap;
  AFirstBroken, ALastBrokenPlus1: integer);
begin
  if ALastBrokenPlus1 <= AFirstBroken then exit;
  DrawTextParts(ADest, BrokenLineStartPart[AFirstBroken], BrokenLineEndPart[ALastBrokenPlus1-1]);
end;

procedure TBidiTextLayout.DrawBrokenLines(ADest: TBGRACustomBitmap;
  AColor: TBGRAPixel; AFirstBroken, ALastBrokenPlus1: integer);
begin
  if ALastBrokenPlus1 <= AFirstBroken then exit;
  DrawTextParts(ADest, AColor, BrokenLineStartPart[AFirstBroken], BrokenLineEndPart[ALastBrokenPlus1-1]);

end;

procedure TBidiTextLayout.DrawBrokenLines(ADest: TBGRACustomBitmap;
  ATexture: IBGRAScanner; AFirstBroken, ALastBrokenPlus1: integer);
begin
  if ALastBrokenPlus1 <= AFirstBroken then exit;
  DrawTextParts(ADest, ATexture, BrokenLineStartPart[AFirstBroken], BrokenLineEndPart[ALastBrokenPlus1-1]);
end;

procedure TBidiTextLayout.PathBrokenLines(ADest: IBGRAPath; AFirstBroken,
  ALastBrokenPlus1: integer);
begin
  if ALastBrokenPlus1 <= AFirstBroken then exit;
  PathTextParts(ADest, BrokenLineStartPart[AFirstBroken], BrokenLineEndPart[ALastBrokenPlus1-1]);
end;

procedure TBidiTextLayout.PathBrokenLines(ADest: IBGRAPath; AClipRect: TRect;
  AFirstBroken, ALastBrokenPlus1: integer);
begin
  if ALastBrokenPlus1 <= AFirstBroken then exit;
  PathTextParts(ADest, AClipRect, BrokenLineStartPart[AFirstBroken], BrokenLineEndPart[ALastBrokenPlus1-1]);
end;

procedure TBidiTextLayout.ComputeLayout;
var curLineHeight, baseLine, tabPixelSize: single;
  paraIndex, ubIndex, i,j, nextTabIndex, splitIndex: Integer;
  curPara: PParagraphInfo;
  brokenIndex, partIndex: integer;
  curBroken: PBrokenLineInfo;
  lineStart, subStart, lineEnd: integer;
  paraSpacingAbove, paraSpacingBelow, correctedBaseLine: single;
  paraRTL, needNewLine: boolean;
  partStr, remainStr: string;
  pos: TPointF;
  curBidiPos,endBidiPos,nextTabBidiPos, availWidth0, remainWidth: single;
  tabSectionStart, tabSectionCount: integer;
  tabSection: array of record
                startIndex, endIndex: integer;
                bidiPos: single;
                tree: TBidiLayoutTree;
              end;
  alignment: TAlignment;
  paraBidiLevel: Byte;
  r: TRectF;
  u: LongWord;
  nextTree: TBidiLayoutTree;
  oldBrokenLines: array of TBrokenLineInfo;
  oldBrokenLineCount: integer;

  procedure AddTabSection(startIndex,endIndex: integer; tree: TBidiLayoutTree);
  begin
    if tabSectionCount >= length(tabSection) then setlength(tabSection, length(tabSection)*2+4);
    tabSection[tabSectionCount].startIndex:= startIndex;
    tabSection[tabSectionCount].endIndex:= endIndex;
    tabSection[tabSectionCount].bidiPos:= curBidiPos;
    tabSection[tabSectionCount].tree := tree;
    inc(tabSectionCount);
  end;

  procedure StartBrokenLine(ACharStart: integer; ACharEnd: integer; ABidiLevel: byte; AWidth, AHeight: single);
  begin
    if curPara^.brokenLineCount >= length(curPara^.brokenLines) then
      setlength(curPara^.brokenLines, length(curPara^.brokenLines)*2+4);

    curBroken := @curPara^.brokenLines[curPara^.brokenLineCount];
    curBroken^.unbrokenLineIndex := ubIndex;
    curBroken^.startIndex:= ACharStart;
    curBroken^.endIndex:= ACharEnd;
    curBroken^.bidiLevel := ABidiLevel;
    curBroken^.firstPartIndex:= partIndex+1;
    curBroken^.usedWidth:= AWidth;

    if FAvailableWidth <> EmptySingle then
      curBroken^.rectF := RectF(0,pos.y,FAvailableWidth,pos.y+AHeight)
    else
    begin
      case alignment of
      taRightJustify: curBroken^.rectF := RectF(-AWidth,pos.y,0,pos.y+AHeight);
      taCenter: curBroken^.rectF := RectF(-AWidth*0.5,pos.y,AWidth*0.5,pos.y+AHeight);
      else {taLeftJustify}
        curBroken^.rectF := RectF(0,pos.y,AWidth,pos.y+AHeight);
      end;
    end;

    if FAvailableWidth = EmptySingle then
    begin
      if FParagraph[paraIndex].rectF.Left = EmptySingle then
      begin
        FParagraph[paraIndex].rectF.Left := curBroken^.rectF.left;
        FParagraph[paraIndex].rectF.Right := curBroken^.rectF.Right;
      end else
      begin
        if FParagraph[paraIndex].rectF.Left < curBroken^.rectF.left then
          FParagraph[paraIndex].rectF.Left := curBroken^.rectF.left;
        if FParagraph[paraIndex].rectF.Right > curBroken^.rectF.Right then
          FParagraph[paraIndex].rectF.Right := curBroken^.rectF.Right;
      end;
    end;

    inc(curPara^.brokenLineCount);
    inc(brokenIndex);
  end;

  procedure DoneBrokenLine;
  begin
    inc(partIndex, curBroken^.partCount);
    IncF(pos.y, curBroken^.rectF.Height);
  end;

  procedure ClearTabSections;
  var
    i: Integer;
  begin
    tabSectionCount := 0;
    for i := 0 to high(tabSection) do
      FreeAndNil(tabSection[i].tree);
  end;

  procedure UpdateQuickSearch;
  begin
    FComputedPartCount:= partIndex + 1;
    FComputedBrokenLineCount:= brokenIndex + 1;
  end;

  procedure Finished;
  begin
    ClearTabSections;
    UpdateQuickSearch;
    CheckTextLayout;
  end;

  procedure StartParagraph(AParagraphIndex: integer);
  begin
    curPara := @FParagraph[AParagraphIndex];
    curPara^.overflow := false;
    curPara^.firstBrokenLineIndex:= brokenIndex + 1;
    curPara^.firstPartIndex := partIndex + 1;

    oldBrokenLineCount:= curPara^.brokenLineCount;
    oldBrokenLines:= curPara^.brokenLines;
    curPara^.brokenLines := nil;
    curPara^.brokenLineCount:= 0;

    curPara^.rectF.Top := pos.y;
    curPara^.rectF.Bottom := pos.y;
    if FAvailableWidth <> EmptySingle then
    begin
      curPara^.rectF.Left := 0;
      curPara^.rectF.Right := FAvailableWidth;
    end else
    begin
      curPara^.rectF.Left := EmptySingle;
      curPara^.rectF.Right := EmptySingle;
    end;
    paraRTL := ParagraphRightToLeft[AParagraphIndex];
    if FAvailableWidth <> EmptySingle then
      alignment := BidiTextAlignmentToAlignment(curPara^.alignment, paraRTL)
    else
      alignment := taLeftJustify;
  end;

  procedure DoneParagraph(AParagraphIndex: integer);
  var
    firstBrokenIndex, lastBrokenIndexFromEnd: Integer;
    newBroken, oldBroken: PBrokenLineInfo;

    function BrokenDifferent: boolean;
    var
      i: integer;
      oldPart, newPart: PPartInfo;
    begin
      if (oldBroken^.startIndex <> newBroken^.startIndex) or
         (oldBroken^.endIndex <> newBroken^.endIndex) or
         (oldBroken^.bidiLevel <> newBroken^.bidiLevel) or
         (oldBroken^.partCount <> newBroken^.partCount) then exit(true);
      for i := 0 to oldBroken^.partCount-1 do
      begin
        oldPart := @oldBroken^.parts[i];
        newPart := @newBroken^.parts[i];
        if oldPart^.modified then exit(true);
        if (oldPart^.startIndex <> newPart^.startIndex) or
           (oldPart^.endIndex <> newPart^.endIndex) or
           (oldPart^.bidiLevel <> newPart^.bidiLevel) then exit(true);
      end;
      result := false;
    end;
  begin
    curPara^.layoutComputed := true;
    if Assigned(FOnBrokenLinesChanged) then
    begin
      firstBrokenIndex := 0;
      while (firstBrokenIndex < oldBrokenLineCount) and
        (firstBrokenIndex < curPara^.brokenLineCount) do
      begin
        oldBroken := @oldBrokenLines[firstBrokenIndex];
        newBroken := @curPara^.brokenLines[firstBrokenIndex];
        if BrokenDifferent then break;
        inc(firstBrokenIndex);
      end;
      lastBrokenIndexFromEnd := 0;
      while (oldBrokenLineCount - lastBrokenIndexFromEnd - 1 > firstBrokenIndex) and
            (curPara^.brokenLineCount - lastBrokenIndexFromEnd - 1 > firstBrokenIndex) do
      begin
        oldBroken := @oldBrokenLines[oldBrokenLineCount - lastBrokenIndexFromEnd - 1];
        newBroken := @curPara^.brokenLines[curPara^.brokenLineCount - lastBrokenIndexFromEnd - 1];
        if BrokenDifferent then break;
        inc(lastBrokenIndexFromEnd);
      end;
      if Assigned(FOnBrokenLinesChanged) and
        ((curPara^.brokenLineCount <> oldBrokenLineCount) or
        (firstBrokenIndex < oldBrokenLineCount)) then
      begin
        FOnBrokenLinesChanged(self, AParagraphIndex, firstBrokenIndex,
          oldBrokenLineCount - lastBrokenIndexFromEnd - firstBrokenIndex,
          curPara^.brokenLineCount - lastBrokenIndexFromEnd - firstBrokenIndex,
          oldBrokenLineCount, curPara^.brokenLineCount);
      end;
    end;
  end;

begin
  FLineHeight:= GetFontFullHeight;
  baseLine := GetFontBaseline;
  ComputeMatrix;

  paraSpacingAbove := ParagraphSpacingAbove * FLineHeight;
  paraSpacingBelow := ParagraphSpacingBelow * FLineHeight;
  if FAvailableWidth <> EmptySingle then
    availWidth0 := FAvailableWidth
  else
    availWidth0:= 0;

  tabPixelSize := TabSize*TextSizeBidiOverride(' ',False).x;
  tabSection := nil;
  pos := PointF(0,0);
  brokenIndex := -1;
  curBroken:= nil;
  partIndex := -1;

  for paraIndex := 0 to ParagraphCount-1 do
  begin
    curPara := @FParagraph[paraIndex];
    if curPara^.layoutComputed then
    begin
      OffsetParagraph(paraIndex, pos.y - curPara^.rectF.Top,
                          brokenIndex+1 - curPara^.firstBrokenLineIndex,
                          partIndex+1 - curPara^.firstPartIndex);
      if curPara^.layoutComputed then
      begin
        pos.y := FParagraph[paraIndex].rectF.Bottom;
        inc(brokenIndex, curPara^.brokenLineCount);
        for i := 0 to curPara^.brokenLineCount-1 do
          inc(partIndex, curPara^.brokenLines[i].partCount);
        continue;
      end;
    end;

    StartParagraph(paraIndex);
    IncF(pos.y, paraSpacingAbove);
    curPara^.rectF.Bottom:= pos.y;

    for ubIndex := FAnalysis.ParagraphFirstUnbrokenLine[paraIndex] to FAnalysis.ParagraphLastUnbrokenLinePlusOne[paraIndex]-1 do
    begin
      if (FAvailableHeight <> EmptySingle) and (pos.y >= FAvailableHeight) and
         (ubIndex <> 0) {there must be at least one broken line} then
      begin
        curPara^.overflow:= true;
        curPara^.rectF.Bottom := pos.y;
        DoneParagraph(paraIndex);
        for i := paraIndex+1 to high(FParagraph) do
        begin
          StartParagraph(i);
          curPara^.overflow:= true;
          DoneParagraph(i);
        end;
        Finished;
        exit;
      end;

      lineStart := FAnalysis.UnbrokenLineStartIndex[ubIndex];
      lineEnd := FAnalysis.UnbrokenLineEndIndex[ubIndex];
      if lineStart < lineEnd then
        paraBidiLevel := FAnalysis.BidiInfo[lineStart].ParagraphBidiLevel
      else
        paraBidiLevel := 0;

      if lineEnd > lineStart then
      begin
        u := UnicodeChar[lineEnd-1];
        case u of
        UNICODE_LINE_SEPARATOR, UNICODE_PARAGRAPH_SEPARATOR, UNICODE_NEXT_LINE: dec(lineEnd);
        13,10:
          begin
            dec(lineEnd);
            if lineEnd > lineStart then
            begin
              u := UnicodeChar[lineEnd-1];
              if (u = 13) or (u = 10) then dec(lineEnd);
            end;
          end;
        end;
      end;

      subStart := lineStart;

      //empty paragraph
      if subStart = lineEnd then
      begin
        StartBrokenLine(subStart, lineEnd, paraBidiLevel, 0, FLineHeight);

        case alignment of
        taRightJustify: pos.x := availWidth0;
        taCenter: pos.x := availWidth0*0.5;
        else {taLeftJustify}
          pos.x := 0;
        end;
        AddPart(subStart, lineEnd, paraBidiLevel,
                RectF(pos.x, curBroken^.rectF.Top,
                      pos.x, curBroken^.rectF.Bottom),
                PointF(0,0), brokenIndex, curBroken);

        DoneBrokenLine;
      end else
      //break lines
      while subStart < lineEnd do
      begin
        //split into sections according to tabs
        ClearTabSections;
        curBidiPos := 0;
        tabSectionStart := subStart;
        tabSectionCount := 0;
        curLineHeight := FLineHeight;

        while tabSectionStart < lineEnd do
        begin
          needNewLine := false;
          while (tabSectionStart < lineEnd) and (FAnalysis.UnicodeChar[tabSectionStart] = 9) do
          begin
            if tabPixelSize = 0 then inc(tabSectionStart)
            else
            begin
              nextTabBidiPos := tabPixelSize* (floor(curBidiPos / tabPixelSize + 1e-6)+1);
              if (FAvailableWidth = EmptySingle) or (nextTabBidiPos <= FAvailableWidth) or (tabSectionStart = subStart) then
              begin
                AddTabSection(tabSectionStart, tabSectionStart+1, nil);
                inc(tabSectionStart);
                curBidiPos := nextTabBidiPos;
              end else
              begin
                //if tab is last char then go to the end of the line
                if tabSectionStart = lineEnd-1 then
                begin
                  AddTabSection(tabSectionStart, lineEnd, nil);
                  inc(tabSectionStart);
                  curBidiPos := FAvailableWidth;
                  needNewLine := true;
                  break;
                end
                else //otherwise a new line is needed before the tab
                begin
                  needNewLine := true;
                  break;
                end;
              end;
            end;
          end;
          if needNewLine then
          begin
            splitIndex:= tabSectionStart;
            break;
          end;

          nextTabIndex := tabSectionStart;
          while (nextTabIndex < lineEnd) and (FAnalysis.UnicodeChar[nextTabIndex] <> 9) do inc(nextTabIndex);
          if FAvailableWidth = EmptySingle then
            remainWidth := EmptySingle
          else
            remainWidth := FAvailableWidth - curBidiPos;
          nextTree := TBidiLayoutTree(ComputeBidiTree(remainWidth, tabSectionStart, nextTabIndex, paraBidiLevel));
          splitIndex := nextTree.EndIndex;

          AddTabSection(tabSectionStart, splitIndex, nextTree);

          if splitIndex < nextTabIndex then
          begin
            if (tabSectionCount = 1) and (splitIndex = tabSectionStart) then
            begin
              inc(splitIndex);
              while (splitIndex < nextTabIndex) and not FAnalysis.BidiInfo[splitIndex].IsMulticharStart do inc(splitIndex);
            end;
            partStr := FAnalysis.CopyTextUTF8(tabSectionStart, splitIndex-tabSectionStart);
            remainStr := FAnalysis.CopyTextUTF8(splitIndex, nextTabIndex-splitIndex);
            if tabSectionCount > 1 then partStr := ' '+partStr;
            if Assigned(FWordBreakHandler) then
              FWordBreakHandler(partStr, remainStr)
            else
              BGRADefaultWordBreakHandler(partStr, remainStr);
            if tabSectionCount > 1 then delete(partStr,1,1);

            splitIndex:= tabSectionStart + UTF8Length(partStr);

            //section is deleted
            if splitIndex = tabSectionStart then
            begin
              dec(tabSectionCount);
              //tabSectionStart stay the same
            end
            else
            begin
              //section is extended
              if splitIndex > nextTree.EndIndex then
              begin
                nextTree := TBidiLayoutTree(ComputeBidiTree(EmptySingle, tabSectionStart, splitIndex, paraBidiLevel));
                tabSection[tabSectionCount-1].tree.Free;
                tabSection[tabSectionCount-1].tree := nextTree;
              end
              else
              begin //otherwise the section is split
                nextTree.Shorten(splitIndex);
                tabSection[tabSectionCount-1].endIndex:= splitIndex;
              end;

              IncF(curBidiPos, nextTree.Width);
              if nextTree.Height > curLineHeight then curLineHeight := nextTree.Height;

              tabSectionStart := splitIndex;
              while (tabSectionStart < nextTabIndex) and IsUnicodeSpace(FAnalysis.UnicodeChar[tabSectionStart]) do inc(tabSectionStart);
            end;
            break;
          end else
          begin
            IncF(curBidiPos, nextTree.Width);
            if nextTree.Height > curLineHeight then curLineHeight := nextTree.Height;
            tabSectionStart := splitIndex;
          end;
        end;

        // add broken line info
        StartBrokenLine(subStart, splitIndex, paraBidiLevel, curBidiPos, curLineHeight);

        subStart := tabSectionStart;

        case alignment of
        taRightJustify:
          if paraRTL then
            pos.x := availWidth0
          else
            pos.x := availWidth0 - curBidiPos;
        taCenter:
          if paraRTL then
            pos.x := (availWidth0 + curBidiPos)*0.5
          else
            pos.x := (availWidth0 - curBidiPos)*0.5;
        else {taLeftJustify}
          if paraRTL then
            pos.x := curBidiPos
          else
            pos.x := 0;
        end;

        if FLineHeight <> 0 then
          correctedBaseLine := baseLine*curLineHeight/FLineHeight
        else
          correctedBaseLine:= 0;

        for j := 0 to tabSectionCount-1 do
        begin
          if not Assigned(tabSection[j].tree) then
          begin
            if j = tabSectionCount-1 then
              endBidiPos:= curBidiPos
            else
              endBidiPos:= tabSection[j+1].bidiPos;

            if paraRTL then
              r := RectF(pos.x-endBidiPos, pos.y, pos.x-tabSection[j].bidiPos, pos.y+curLineHeight)
            else
              r := RectF(pos.x+tabSection[j].bidiPos, pos.y, pos.x+endBidiPos, pos.y+curLineHeight);

            AddPart(tabSection[j].startIndex, tabSection[j].endIndex, paraBidiLevel, r, PointF(0,0), brokenIndex, curBroken);
          end
          else
          begin
            if paraRTL then
              AddPartsFromTree(pos - PointF(tabSection[j].bidiPos,0), tabSection[j].tree, curLineHeight, correctedBaseLine, brokenIndex, curBroken)
            else
              AddPartsFromTree(pos + PointF(tabSection[j].bidiPos,0), tabSection[j].tree, curLineHeight, correctedBaseLine, brokenIndex, curBroken)
          end;
        end;

        DoneBrokenLine;
        if (FAvailableHeight <> EmptySingle) and (pos.y >= FAvailableHeight) then
        begin
          curPara^.overflow := true;
          break;
        end;
      end;
    end;
    IncF(pos.y, paraSpacingBelow);
    curPara^.rectF.Bottom := pos.y;
    DoneParagraph(paraIndex);
  end;
  Finished;
end;

procedure TBidiTextLayout.CheckTextLayout;
var
  i, charIndex, partIndex, j, k, curBrokenLineEndIndex: Integer;
begin
  charIndex := 0;
  partIndex := 0;
  if length(FParagraph) <> ParagraphCount then
    raise exception.Create('Number of paragraph mismatch ' + inttostr(length(FParagraph)) +
    '/' + inttostr(ParagraphCount));
  for i := 0 to ParagraphCount-1 do
  begin
    if not FParagraph[i].layoutComputed then
      raise exception.Create('Layout not computed for paragraph ' + inttostr(i));
    if i > 0 then
    begin
      if FParagraph[i].firstBrokenLineIndex < FParagraph[i-1].firstBrokenLineIndex then
        raise exception.Create('Broken index is not ascending');
      if FParagraph[i].firstBrokenLineIndex <> FParagraph[i-1].firstBrokenLineIndex
         + FParagraph[i-1].brokenLineCount then
        raise exception.Create('Expecting at least one broken line');
      if FParagraph[i].firstPartIndex < FParagraph[i-1].firstPartIndex then
        raise exception.Create('Part index is not ascending');
      if FParagraph[i].firstPartIndex <> partIndex then
        raise exception.Create('Part index is not consistent between paragraphs');
    end else
    begin
      if FParagraph[i].firstPartIndex <> 0 then
        raise exception.Create('First part index is expected to be 0');
      if FParagraph[i].firstBrokenLineIndex <> 0 then
        raise exception.Create('First broken index is expected to be 0');
      if (FParagraph[i].brokenLineCount > 0) and
         (FParagraph[i].brokenLines[0].firstPartIndex <> FParagraph[i].firstPartIndex) then
        raise exception.Create('Inconsistent part index between paragraph and first broken line');
    end;
    with FParagraph[i] do
      for j := 0 to brokenLineCount-1 do
      begin
        if brokenLines[j].firstPartIndex <> partIndex then
          raise exception.Create('Inconsistent first part index of broken line (' +
           inttostr(brokenLines[j].firstPartIndex) + ' expecting ' +
           inttostr(partIndex) + ' for broken line ' +
           inttostr(firstBrokenLineIndex + j) + ')');
        inc(partIndex, brokenLines[j].partCount);
        if brokenLines[j].startIndex < charIndex then
          raise exception.Create('Inconsistent first char index of broken line (' +
           inttostr(brokenLines[j].startIndex) + ' expecting at least ' +
           inttostr(charIndex) + ' for broken line ' +
           inttostr(firstBrokenLineIndex + j) + ' and paragraph ' + inttostr(i) + ')');
        with brokenLines[j] do
        begin
          curBrokenLineEndIndex := endIndex;
          for k := 0 to partCount-1 do
          with parts[k] do
          begin
            if startIndex < charIndex then
              raise exception.Create('Inconsistent first char index of part');
            if endIndex > curBrokenLineEndIndex then
              raise exception.Create('Last char index of part out of broken line range');
            charIndex := endIndex;
          end;
        end;
        charIndex := brokenLines[j].endIndex;
      end;
  end;
  if charIndex > CharCount then
    raise exception.Create('Last char index of broken line out of bounds (' +
      inttostr(charIndex)+' > '+inttostr(CharCount) + ')');
end;

procedure TBidiTextLayout.NeedLayout;
begin
  if not LayoutComputed then ComputeLayout;
end;

procedure TBidiTextLayout.InternalDrawText(ADest: TBGRACustomBitmap);
begin
  InternalDrawTextParts(ADest, 0, PartCount);
end;

procedure TBidiTextLayout.InternalPathText(ADest: IBGRAPath);
begin
  InternalPathTextParts(ADest, 0, PartCount);
end;

procedure TBidiTextLayout.InternalPathText(ADest: IBGRAPath; AClipRect: TRect);
begin
  InternalPathTextParts(ADest, AClipRect, 0, PartCount);
end;

procedure TBidiTextLayout.InternalDrawTextParts(ADest: TBGRACustomBitmap;
  AFirstPart, ALastPartPlus1: integer);
var
  part: PPartInfo;
  enumPart: TPartEnumerator;
  r: TRectF;
  b: TRect;
  pos: TPointF;
begin
  NeedLayout;
  enumPart := GetPartEnumerator(AFirstPart, ALastPartPlus1);
  while enumPart.GetNext do begin
    part := enumPart.PartInfo;
    r := part^.rectF;
    DecF(r.Left, LineHeight/2 + FClipMargin);
    DecF(r.Top, FClipMargin);
    IncF(r.Right, LineHeight/2 + FClipMargin);
    IncF(r.Bottom, FClipMargin);
    b := (Matrix*TAffineBox.AffineBox(r)).RectBounds;
    if not b.IntersectsWith(ADest.ClipRect) then continue;
    pos := Matrix*(part^.rectF.TopLeft + part^.posCorrection);
    TextOutBidiOverride(ADest, pos.x, pos.y,
      FAnalysis.CopyTextUTF8(part^.startIndex, part^.endIndex - part^.startIndex),
      part^.IsRightToLeft);
  end;
end;

procedure TBidiTextLayout.InternalPathTextParts(ADest: IBGRAPath; AFirstPart,
  ALastPartPlus1: integer);
var
  part: PPartInfo;
  pos: TPointF;
  enumPart: TPartEnumerator;
begin
  NeedLayout;
  enumPart := GetPartEnumerator(AFirstPart, ALastPartPlus1);
  while enumPart.GetNext do begin
    part := enumPart.PartInfo;
    pos := Matrix*(part^.rectF.TopLeft + part^.posCorrection);
    TextPathBidiOverride(ADest, pos.x, pos.y,
    FAnalysis.CopyTextUTF8(part^.startIndex, part^.endIndex - part^.startIndex),
    part^.IsRightToLeft);
  end;
end;

procedure TBidiTextLayout.InternalPathTextParts(ADest: IBGRAPath;
  AClipRect: TRect; AFirstPart, ALastPartPlus1: integer);
var
  part: PPartInfo;
  enumPart: TPartEnumerator;
  r: TRectF;
  b: TRect;
  pos: TPointF;
begin
  NeedLayout;
  enumPart := GetPartEnumerator(AFirstPart, ALastPartPlus1);
  while enumPart.GetNext do begin
    part := enumPart.PartInfo;
    r := part^.rectF;
    DecF(r.Left, LineHeight/2 + FClipMargin);
    DecF(r.Top, FClipMargin);
    IncF(r.Right, LineHeight/2 + FClipMargin);
    IncF(r.Bottom, FClipMargin);
    b := (Matrix*TAffineBox.AffineBox(r)).RectBounds;
    if not b.IntersectsWith(AClipRect) then continue;
    pos := Matrix*(part^.rectF.TopLeft + part^.posCorrection);
    TextPathBidiOverride(ADest, pos.x, pos.y,
    FAnalysis.CopyTextUTF8(part^.startIndex, part^.endIndex - part^.startIndex),
    part^.IsRightToLeft);
  end;
end;

procedure TBidiTextLayout.InternalRangeError;
begin
  raise ERangeError.Create('Internal error');
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
          ADest.FillPolyAntialias(PointsF([ABottom, ATop, ATop - triSize*v, ATop - v + triSize*u, ABottom - v]), AColor, false)
        else
          ADest.FillPolyAntialias(PointsF([ABottom, ATop, ATop + triSize*v, ATop + triSize*u + v, ABottom + v]), AColor, False)
      end
      else
      begin
        if len > 10 then
        begin
          if ARightToLeft then
            ADest.FillPolyAntialias(PointsF([ABottom, ATop, ATop - 2*v, ABottom - 2*v]), AColor, False)
          else
            ADest.FillPolyAntialias(PointsF([ABottom, ATop, ATop + 2*v, ABottom + 2*v]), AColor, False)
        end
        else
        begin
          if ARightToLeft then
            ADest.FillPolyAntialias(PointsF([ABottom, ATop, ATop - v, ABottom - v]), AColor, False)
          else
            ADest.FillPolyAntialias(PointsF([ABottom, ATop, ATop + v, ABottom + v]), AColor, False)
        end;
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
  AEndIndex: integer; AFillColor: TBGRAPixel; ABorderColor: TBGRAPixel; APenWidth: single);
var
  env: ArrayOfTPointF;
begin
  NeedLayout;

  if AStartIndex = AEndIndex then exit;
  env := GetTextEnveloppe(AStartIndex,AEndIndex, False, True);
  ADest.FillPolyAntialias(env, AFillColor, False);
  if (ABorderColor.alpha <> 0) and (APenWidth > 0) then
    ADest.DrawPolygonAntialias(env, ABorderColor, APenWidth);
end;

procedure TBidiTextLayout.DrawSelection(ADest: TBGRACustomBitmap; AStartIndex,
  AEndIndex: integer; AFillColor: TBGRAPixel);
begin
  DrawSelection(ADest, AStartIndex,AEndIndex, AFillColor, BGRAPixelTransparent, 0);
end;

function TBidiTextLayout.GetCaret(ACharIndex: integer): TBidiCaretPos;
begin
  result := GetUntransformedCaret(ACharIndex);
  result.Transform(Matrix);
end;

function TBidiTextLayout.GetUntransformedCaret(ACharIndex: integer): TBidiCaretPos;
var
  i, blIndex, lastPartIndex: Integer;
  w: Single;
  bl: PBrokenLineInfo;
  part: PPartInfo;
begin
  NeedLayout;

  if (ACharIndex < 0) or (ACharIndex > CharCount) then
    raise ERangeError.Create('Invalid index');

  if (PartCount > 0) and (ACharIndex >= PartEndIndex[PartCount-1]) then
  begin
    result := GetUntransformedPartEndCaret(PartCount-1);
    exit;
  end;

  result.PartIndex := -1;
  result.Top := EmptyPointF;
  result.Bottom := EmptyPointF;
  result.RightToLeft := false;
  result.PreviousTop := EmptyPointF;
  result.PreviousBottom := EmptyPointF;
  result.PreviousRightToLeft := false;

  blIndex := GetBrokenLineAt(ACharIndex);
  bl := GetBrokenLineInfo(blIndex);
  if blIndex <> -1 then
  begin
    lastPartIndex := bl^.firstPartIndex + bl^.partCount - 1;
    for i := bl^.firstPartIndex to lastPartIndex do
    begin
      part := @bl^.parts[i - bl^.firstPartIndex];
      if ACharIndex <= part^.startIndex then
      begin
        result := GetUntransformedPartStartCaret(i);
        exit;
      end else
      if (ACharIndex > part^.startIndex) and (ACharIndex <= part^.endIndex) then
      begin
        if (i < FComputedPartCount-1) and (ACharIndex = part^.startIndex) then
        begin
          result := GetUntransformedPartStartCaret(i+1);
          exit;
        end else
        begin
          if ACharIndex = part^.endIndex then
          begin
            result := GetUntransformedPartEndCaret(i);
            exit;
          end else
          begin
            w := TextSizeBidiOverrideSplit(part^.startIndex, part^.endIndex, part^.IsRightToLeft, ACharIndex).x;

            if part^.IsRightToLeft then
              result.Top := PointF(part^.rectF.Right - w, part^.rectF.Top)
            else result.Top := PointF(part^.rectF.Left + w, part^.rectF.Top);
            result.Bottom := result.Top + PointF(0, part^.rectF.Height);

            result.RightToLeft := part^.IsRightToLeft;
            result.PreviousRightToLeft := result.RightToLeft;
            result.PartIndex := i;
          end;
          exit;
        end;
      end else
      if i = lastPartIndex then
      begin
        result := GetUntransformedPartEndCaret(i);
        exit;
      end;
    end;
  end;

  if ACharIndex = 0 then
  begin
    result.Top := PointF(0,0);
    result.Bottom := PointF(0,FLineHeight);
    result.RightToLeft := false;
    result.PreviousTop := EmptyPointF;
    result.PreviousBottom := EmptyPointF;
    result.PreviousRightToLeft := false;
    result.PartIndex := 0;
  end;
end;

function TBidiTextLayout.GetCharIndexAt(APosition: TPointF; ABetweenGlyphs: boolean): integer;
var
  brokenIndex,j, fit: Integer;
  u,u2: LongWord;
  axis, origin: TPointF;
  len, w, curW, newW: Single;
  str: String;
  curIndex, newIndex, paraIndex: integer;
  untransformedPos: TPointF;
  para: PParagraphInfo;
  curBroken: PBrokenLineInfo;
  part: PPartInfo;
  ab: TAffineBox;
begin
  NeedLayout;
  untransformedPos := FMatrixInverse*APosition;
  paraIndex := GetUntransformedParagraphAt(untransformedPos);
  para := @FParagraph[paraIndex];

  if untransformedPos.Y < para^.rectF.Top then
    exit(ParagraphStartIndex[paraIndex]);

  if untransformedPos.Y >= para^.rectF.Bottom then
    exit(ParagraphEndIndex[paraIndex]);

  for brokenIndex := 0 to para^.brokenLineCount-1 do
  begin
    curBroken := @para^.brokenLines[brokenIndex];
    if untransformedPos.Y < curBroken^.rectF.Bottom then
    begin
      if untransformedPos.Y < curBroken^.rectF.Top then
        exit(curBroken^.startIndex);

      //before part
      if curBroken^.partCount > 0 then
      begin
        if (curBroken^.IsRightToLeft and (untransformedPos.x >= curBroken^.parts[0].rectF.Right)) or
           (not curBroken^.IsRightToLeft and (untransformedPos.x < curBroken^.parts[0].rectF.Left)) then
          exit(curBroken^.startIndex)
      end;

      for j := 0 to curBroken^.partCount-1 do
      begin
        part := @curBroken^.parts[j];
        ab := Matrix*TAffineBox.AffineBox(part^.rectF);
        if ab.Contains(APosition) then
        begin
          if part^.IsRightToLeft then
          begin
            axis := ab.TopLeft - ab.TopRight;
            origin := ab.TopRight;
          end else
          begin
            axis := ab.TopRight - ab.TopLeft;
            origin := ab.TopLeft;
          end;
          len := VectLen(axis);
          if len > 0 then
          begin
            w := ((APosition-origin)**axis)/len;
            //if there is just one char, it is the whole part
            if part^.endIndex = part^.startIndex + 1 then
            begin
              if ABetweenGlyphs then
              begin
                if w > 0.5*len then
                  exit(part^.endIndex)
                else
                  exit(part^.startIndex);
              end else
                exit(part^.startIndex);
            end;

            str := FAnalysis.CopyTextUTF8(part^.startIndex, part^.endIndex - part^.startIndex);
            fit := TextFitInfoBidiOverride(str, w, part^.IsRightToLeft);
            curIndex := part^.startIndex+fit;
            if curIndex > part^.endIndex then curIndex:= part^.endIndex;
            if curIndex = 0 then curW := 0
            else curW := TextSizeBidiOverrideSplit(part^.startIndex, part^.endIndex, part^.IsRightToLeft, curIndex).x;
            while (curW < w) and (curIndex < part^.endIndex) do
            begin
              newIndex := curIndex+1;
              while (newIndex < part^.endIndex) and not FAnalysis.BidiInfo[newIndex].IsMulticharStart do inc(newIndex);
              newW := TextSizeBidiOverrideSplit(part^.startIndex, part^.endIndex, part^.IsRightToLeft, newIndex).x;
              if newW >= w then
              begin
                if ABetweenGlyphs and ((curW+newW)*0.5 + 1 < w) then curIndex := newIndex;
                break;
              end else
              begin
                curW := newW;
                curIndex := newIndex;
              end;
            end;
            exit(curIndex);
          end;
          exit(part^.startIndex);
        end;
      end;

      //after part
      result := curBroken^.endIndex;
      if result > curBroken^.startIndex then
      begin
        u := GetUnicodeChar(result-1);
        if IsUnicodeParagraphSeparator(u) or (u = UNICODE_LINE_SEPARATOR) then
        begin
          dec(result);
          if (result > curBroken^.startIndex) and (u = 13) or (u = 10) then
          begin
            u2 := GetUnicodeChar(result-1);
            if (u2 <> u) and ((u2 = 13) or (u2 = 10)) then dec(result);
          end;
        end;
      end;
      exit;
    end;
  end;

  exit(ParagraphEndIndexBeforeParagraphSeparator[paraIndex]);
end;

function TBidiTextLayout.GetTextEnveloppe(AStartIndex, AEndIndex: integer; APixelCenteredCoordinates: boolean; AMergeBoxes: boolean; AVerticalClip: boolean): ArrayOfTPointF;
var
  i: Integer;
  m: TAffineMatrix;
begin
  result := GetUntransformedTextEnveloppe(AStartIndex,AEndIndex,false,AMergeBoxes,AVerticalClip);
  if APixelCenteredCoordinates then m := AffineMatrixTranslation(-0.5,0.5)*Matrix else m := Matrix;
  for i := 0 to high(result) do
    result[i] := m*result[i];
end;

function TBidiTextLayout.GetUntransformedTextEnveloppe(AStartIndex,
  AEndIndex: integer; APixelCenteredCoordinates: boolean; AMergeBoxes: boolean; AVerticalClip: boolean): ArrayOfTPointF;
var
  startCaret, endCaret: TBidiCaretPos;
  vertResult: array of record
                box: TAffineBox;
                joinPrevious: boolean;
              end;

  procedure AppendVertResult(ABox: TAffineBox; ARightToLeft: boolean);
  begin
    if AVerticalClip and (AvailableHeight <> EmptySingle) then
    begin
      if (ABox.TopLeft.y >= AvailableHeight) or (ABox.TopRight.y >= AvailableHeight) then exit;
      if ABox.BottomLeft.y > AvailableHeight then ABox.BottomLeft.y := AvailableHeight;
    end;

    if ARightToLeft then
      ABox := TAffineBox.AffineBox(ABox.TopRight,ABox.TopLeft,ABox.BottomRight);

    if AMergeBoxes and (vertResult <> nil) and (ABox.TopLeft = vertResult[high(vertResult)].box.BottomLeft) and
       (ABox.TopRight = vertResult[high(vertResult)].box.BottomRight) then
       vertResult[high(vertResult)].box :=
         TAffineBox.AffineBox(vertResult[high(vertResult)].box.TopLeft, vertResult[high(vertResult)].box.TopRight, ABox.BottomLeft)
    else
    begin
      setlength(vertResult, length(vertResult)+1);
      vertResult[high(vertResult)].box := ABox;
      if high(vertResult)>0 then
        vertResult[high(vertResult)].joinPrevious:= AMergeBoxes and ((VectLen(ABox.TopLeft-vertResult[high(vertResult)-1].box.BottomLeft)<1e-3) or
                                                    (VectLen(ABox.TopRight-vertResult[high(vertResult)-1].box.BottomRight)<1e-3))
      else
        vertResult[high(vertResult)].joinPrevious:= false;
    end;
  end;

  procedure AppendComplexSelection;
  var
    horizResult: array of TAffineBox;

    procedure AppendHorizResult(AStartTop, AEndTop, AEndBottom, AStartBottom: TPointF; ARightToLeft: boolean);
    var
      temp: TPointF;

      procedure TryMergeBefore;
      begin
        while length(horizResult)>=2 do
        begin
          if (horizResult[high(horizResult)].TopRight = horizResult[high(horizResult)-1].TopLeft) and
             (horizResult[high(horizResult)].BottomRight = horizResult[high(horizResult)-1].BottomLeft) then
          begin
            horizResult[high(horizResult)-1] := TAffineBox.AffineBox(horizResult[high(horizResult)].TopLeft,
                                                                     horizResult[high(horizResult)-1].TopRight,
                                                                     horizResult[high(horizResult)].BottomLeft);
            setlength(horizResult, length(horizResult)-1);
          end else
          if (horizResult[high(horizResult)].TopLeft = horizResult[high(horizResult)-1].TopRight) and
             (horizResult[high(horizResult)].BottomLeft = horizResult[high(horizResult)-1].BottomRight) then
          begin
            horizResult[high(horizResult)-1] := TAffineBox.AffineBox(horizResult[high(horizResult)-1].TopLeft,
                                                                     horizResult[high(horizResult)].TopRight,
                                                                     horizResult[high(horizResult)-1].BottomLeft);
            setlength(horizResult, length(horizResult)-1);
          end else
            break;
        end;
      end;

    begin
      if ARightToLeft then
      begin
        temp := AStartTop;
        AStartTop := AEndTop;
        AEndTop := temp;

        temp := AStartBottom;
        AStartBottom := AEndBottom;
        AEndBottom := temp;
      end;

      if AMergeBoxes and (horizResult <> nil) and (AStartTop = horizResult[high(horizResult)].TopRight)
         and (AStartBottom = horizResult[high(horizResult)].BottomRight) then
      begin
        horizResult[high(horizResult)] := TAffineBox.AffineBox(horizResult[high(horizResult)].TopLeft,AEndTop,horizResult[high(horizResult)].BottomLeft);
        TryMergeBefore;
      end
      else
      if AMergeBoxes and (horizResult <> nil) and (AEndTop = horizResult[high(horizResult)].TopLeft)
         and (AEndBottom = horizResult[high(horizResult)].BottomLeft) then
      begin
        horizResult[high(horizResult)] := TAffineBox.AffineBox(AStartTop,horizResult[high(horizResult)].TopRight,AStartBottom);
        TryMergeBefore;
      end
      else
      begin
        setlength(horizResult, length(horizResult)+1);
        horizResult[high(horizResult)] := TAffineBox.AffineBox(AStartTop, AEndTop, AStartBottom);
      end;
    end;

    procedure FlushHorizResult;
    var
      idx, j: Integer;
    begin
      if horizResult <> nil then
      begin
        AppendVertResult(horizResult[0], false);
        if length(horizResult)>1 then //additional boxes are added without vertical join
        begin
          idx := length(vertResult);
          setlength(vertResult, length(vertResult)+length(horizResult)-1);
          for j := 1 to high(horizResult) do
          begin
            vertResult[idx+j-1].box := horizResult[j];
            vertResult[idx+j-1].joinPrevious := false;
          end;
        end;
        horizResult := nil;
      end;
    end;

  var
    curPartStartCaret, curPartEndCaret,
    lineStartCaret, lineEndCaret, curPartCaret: TBidiCaretPos;
    curBrokenIndex, curParaIndex, prevParaIndex, j,
      brokenLineLastPartIndexPlus1, curPartIndex: integer;
    r: TRectF;
    partEnum: TPartEnumerator;
    prevPart, curPart: PPartInfo;
    curBroken: PBrokenLineInfo;

  begin
    horizResult := nil;
    partEnum := GetPartEnumerator(startCaret.PartIndex, endCaret.PartIndex + 1);
    curPart := nil;
    curParaIndex := -1;

    if partEnum.GetNext then
    while true do
    begin
      prevParaIndex := curParaIndex;
      prevPart := curPart;
      curParaIndex := partEnum.ParagraphIndex;
      curPart := partEnum.PartInfo;
      curPartIndex := partEnum.PartIndex;
      curBroken := partEnum.BrokenLineInfo;
      curBrokenIndex := partEnum.BrokenLineIndex;

      //space between paragraph
      if (curPartIndex > startCaret.PartIndex) and (ParagraphSpacingAbove+ParagraphSpacingBelow <> 0) then
      begin
        if (curParaIndex > 0) and (prevParaIndex = curParaIndex-1) then
        begin
          FlushHorizResult;

          r := RectF(ParagraphRectF[curParaIndex-1].Left, ParagraphRectF[curParaIndex-1].Bottom - ParagraphSpacingBelow*FLineHeight,
                       ParagraphRectF[curParaIndex-1].Right, ParagraphRectF[curParaIndex-1].Bottom);
          AppendVertResult(TAffineBox.AffineBox(r), False);

          r := RectF(ParagraphRectF[curParaIndex].Left, ParagraphRectF[curParaIndex].Top,
                       ParagraphRectF[curParaIndex].Right, ParagraphRectF[curParaIndex].Top + ParagraphSpacingAbove*FLineHeight);
          AppendVertResult(TAffineBox.AffineBox(r), False);
        end;
      end;

      //whole broken line selected
      brokenLineLastPartIndexPlus1 := curBroken^.firstPartIndex + curBroken^.partCount;
      if (curPartIndex = curBroken^.firstPartIndex) and
         ((curPartIndex > startCaret.PartIndex) or (AStartIndex = curPart^.startIndex)) and
         (endCaret.PartIndex >= brokenLineLastPartIndexPlus1) then
      begin
        FlushHorizResult;

        lineStartCaret := GetBrokenLineUntransformedStartCaret(curBrokenIndex);
        lineEndCaret := GetBrokenLineUntransformedEndCaret(curBrokenIndex);
        AppendVertResult(TAffineBox.AffineBox(lineStartCaret.Top,lineEndCaret.Top,lineStartCaret.Bottom), BrokenLineRightToLeft[curBrokenIndex]);

        //skip broken line
        for j := curPartIndex to brokenLineLastPartIndexPlus1-2 do
          partEnum.GetNext;
        if not partEnum.GetNext then break;
      end else
      begin
        if curPartIndex > startCaret.PartIndex then
          curPartStartCaret := GetUntransformedPartStartCaret(curPartIndex, prevPart, curPart)
          else curPartStartCaret := startCaret;

        if curPartIndex < endCaret.PartIndex then
          curPartEndCaret := GetUntransformedPartEndCaret(curPartIndex, curPart)
          else curPartEndCaret := endCaret;

        //start of lines
        if (curPartIndex > startCaret.PartIndex) and (prevPart^.brokenLineIndex <> curBrokenIndex) then
        begin
          FlushHorizResult;

          lineStartCaret := GetBrokenLineUntransformedStartCaret(curBrokenIndex);
          if curBroken^.IsRightToLeft = curPart^.IsRightToLeft then
            AppendHorizResult(lineStartCaret.Top, curPartStartCaret.Top,
                          curPartStartCaret.Bottom, lineStartCaret.Bottom,
                          BrokenLineRightToLeft[curBrokenIndex])
          else
            AppendHorizResult(lineStartCaret.Top, curPartEndCaret.Top,
                          curPartEndCaret.Bottom, lineStartCaret.Bottom,
                          curBroken^.IsRightToLeft);
        end;

        //text parts
        AppendHorizResult(curPartStartCaret.Top, curPartEndCaret.Top,
                          curPartEndCaret.Bottom, curPartStartCaret.Bottom,
                          curPart^.IsRightToLeft);

        //end of lines
        if not partEnum.GetNext then break;

        if (partEnum.BrokenLineIndex <> curBrokenIndex) then
        begin
          lineEndCaret := GetBrokenLineUntransformedEndCaret(curBrokenIndex);
          if curBroken^.IsRightToLeft = curPart^.IsRightToLeft then
            curPartCaret := GetUntransformedPartEndCaret(curPartIndex)
          else
            curPartCaret := GetUntransformedPartStartCaret(curPartIndex);
          AppendHorizResult(curPartCaret.Top, lineEndCaret.Top,
            lineEndCaret.Bottom, curPartCaret.Bottom, curBroken^.IsRightToLeft)
        end;
      end;

    end;

    FlushHorizResult;
  end;

var
  temp: integer;
  i,j, idxOut, k: integer;

begin
  NeedLayout;

  vertResult := nil;

  if AStartIndex > AEndIndex then
  begin
    temp := AStartIndex;
    AStartIndex:= AEndIndex;
    AEndIndex:= temp;
  end;
  startCaret := GetUntransformedCaret(AStartIndex);
  endCaret := GetUntransformedCaret(AEndIndex);
  if not isEmptyPointF(endCaret.PreviousTop) then
  begin
    endCaret.Top := endCaret.PreviousTop;        endCaret.PreviousTop := EmptyPointF;
    endCaret.Bottom := endCaret.PreviousBottom;  endCaret.PreviousBottom := EmptyPointF;
    endCaret.RightToLeft := endCaret.PreviousRightToLeft;
    if endCaret.PartIndex <> -1 then dec(endCaret.PartIndex);
  end;

  if startCaret.PartIndex = endCaret.PartIndex then
  begin
    if not isEmptyPointF(startCaret.Top) and not isEmptyPointF(endCaret.Top) then
      AppendVertResult(TAffineBox.AffineBox(startCaret.Top,endCaret.Top,startCaret.Bottom), startCaret.RightToLeft);
  end else
    AppendComplexSelection;

  if APixelCenteredCoordinates then
    for i := 0 to high(vertResult) do
      vertResult[i].box.Offset(-0.5, -0.5);

  if vertResult <> nil then
  begin
    setlength(result, length(vertResult)*5 - 1); //maximum point count
    idxOut := 0;
    i := 0;
    while i <= high(vertResult) do
    begin
      if i > 0 then
      begin
        result[idxOut] := EmptyPointF;
        inc(idxOut);
      end;
      result[idxOut] := vertResult[i].box.TopLeft; inc(idxOut);
      result[idxOut] := vertResult[i].box.TopRight; inc(idxOut);
      result[idxOut] := vertResult[i].box.BottomRight; inc(idxOut);
      j := i;
      while (j<high(vertResult)) and vertResult[j+1].joinPrevious do
      begin
        inc(j);
        result[idxOut] := vertResult[j].box.TopRight; inc(idxOut);
        result[idxOut] := vertResult[j].box.BottomRight; inc(idxOut);
      end;
      for k := j downto i+1 do
      begin
        result[idxOut] := vertResult[k].box.BottomLeft; inc(idxOut);
        result[idxOut] := vertResult[k].box.TopLeft; inc(idxOut);
      end;
      result[idxOut] := vertResult[i].box.BottomLeft; inc(idxOut);
      i := j+1;
    end;
    setlength(result, idxOut);
  end else
    result := nil;
end;

function TBidiTextLayout.GetParagraphAt(ACharIndex: Integer): integer;
begin
  result := FAnalysis.GetParagraphAt(ACharIndex);
end;

function TBidiTextLayout.GetParagraphAt(APosition: TPointF): integer;
begin
  result := GetParagraphAt(FMatrixInverse*APosition);
end;

function TBidiTextLayout.GetBrokenLineAt(ACharIndex: integer): integer;
var
  paraMinIndex, paraMaxIndex, paraMidIndex, brokenMinIndex,
    brokenMaxIndex, brokenMidIndex: Integer;
begin
  if (ACharIndex < 0) or (ACharIndex > CharCount) then raise exception.Create('Position out of bounds');
  paraMinIndex := 0;
  paraMaxIndex := ParagraphCount-1;
  repeat
    if paraMinIndex > paraMaxIndex then
      InternalRangeError else
    if paraMinIndex = paraMaxIndex then
    with FParagraph[paraMinIndex] do
    begin
      brokenMinIndex := 0;
      brokenMaxIndex := brokenLineCount-1;
      repeat
        if brokenMinIndex > brokenMaxIndex then InternalRangeError else
        if brokenMinIndex = brokenMaxIndex then
        begin
          result := brokenMinIndex + firstBrokenLineIndex;
          exit;
        end else
        begin
          brokenMidIndex := (brokenMinIndex + brokenMaxIndex + 1) shr 1;
          if ACharIndex < brokenLines[brokenMidIndex].startIndex then
            brokenMaxIndex := brokenMidIndex-1
            else brokenMinIndex := brokenMidIndex;
        end;
      until false;
    end else
    begin
      paraMidIndex := (paraMinIndex + paraMaxIndex + 1) shr 1;
      if ACharIndex < ParagraphStartIndex[paraMidIndex] then
        paraMaxIndex := paraMidIndex-1
        else paraMinIndex := paraMidIndex;
    end;
  until false;
end;

function TBidiTextLayout.InsertText(ATextUTF8: string; APosition: integer): integer;
begin
  result := FAnalysis.InsertText(ATextUTF8,APosition);
end;

function TBidiTextLayout.InsertLineSeparator(APosition: integer): integer;
begin
  result := InsertText(UnicodeCharToUTF8(UNICODE_LINE_SEPARATOR), APosition);
end;

function TBidiTextLayout.DeleteText(APosition, ACount: integer): integer;
begin
  result := FAnalysis.DeleteText(APosition, ACount);
end;

function TBidiTextLayout.DeleteTextBefore(APosition, ACount: integer): integer;
begin
  result := FAnalysis.DeleteTextBefore(APosition, ACount);
end;

function TBidiTextLayout.CopyText(APosition, ACount: integer): string;
begin
  ACount := IncludeNonSpacingChars(APosition, ACount);
  result := FAnalysis.CopyTextUTF8(APosition, ACount);
end;

function TBidiTextLayout.CopyTextBefore(APosition, ACount: integer): string;
begin
  ACount := IncludeNonSpacingCharsBefore(APosition, ACount);
  result := FAnalysis.CopyTextUTF8(APosition-ACount, ACount);
end;

function TBidiTextLayout.IncludeNonSpacingChars(APosition, ACount: integer; AIncludeCombiningMarks: boolean): integer;
begin
  result := FAnalysis.IncludeNonSpacingChars(APosition,ACount,AIncludeCombiningMarks);
end;

function TBidiTextLayout.IncludeNonSpacingCharsBefore(APosition, ACount: integer; AIncludeCombiningMarks: boolean): integer;
begin
  result := FAnalysis.IncludeNonSpacingCharsBefore(APosition,ACount,AIncludeCombiningMarks);
end;

function TBidiTextLayout.FindTextAbove(AFromPosition: integer): integer;
var
  curPos: TBidiCaretPos;
  bIndex: LongInt;
  pt: TPointF;
begin
  curPos := GetUntransformedCaret(AFromPosition);
  bIndex := PartBrokenLineIndex[curPos.PartIndex];
  if (bIndex > 0) and not isEmptyPointF(curPos.Top) then
  begin
    dec(bIndex);
    pt := PointF(curPos.Top.x, (BrokenLineRectF[bIndex].Top+BrokenLineRectF[bIndex].Bottom)*0.5);
    result := GetCharIndexAt(Matrix*pt);
  end else
    exit(-1);
end;

function TBidiTextLayout.FindTextBelow(AFromPosition: integer): integer;
var
  curPos: TBidiCaretPos;
  bIndex: LongInt;
  pt: TPointF;
begin
  curPos := GetUntransformedCaret(AFromPosition);
  bIndex := PartBrokenLineIndex[curPos.PartIndex];
  if (bIndex < BrokenLineCount-1) and not isEmptyPointF(curPos.Top) then
  begin
    inc(bIndex);
    pt := PointF(curPos.Top.x, (BrokenLineRectF[bIndex].Top+BrokenLineRectF[bIndex].Bottom)*0.5);
    result := GetCharIndexAt(Matrix*pt);
  end else
    exit(-1);
end;

function TBidiTextLayout.GetPartStartCaret(APartIndex: integer): TBidiCaretPos;
begin
  result := GetUntransformedPartStartCaret(APartIndex);
  result.Transform(Matrix)
end;

function TBidiTextLayout.GetPartEndCaret(APartIndex: integer): TBidiCaretPos;
begin
  result := GetUntransformedPartEndCaret(APartIndex);
  result.Transform(Matrix);
end;

function TBidiTextLayout.GetUntransformedPartStartCaret(APartIndex: integer): TBidiCaretPos;
var
  prevPart, part: PPartInfo;
  partEnum: TPartEnumerator;
begin
  if (APartIndex < 0) or (APartIndex > PartCount) then
    raise ERangeError.Create('Invalid index');

  if APartIndex > 0 then
  begin
    partEnum := GetPartEnumerator(APartIndex - 1);
    if not partEnum.GetNext then InternalRangeError;
    prevPart := partEnum.PartInfo;
    if not partEnum.GetNext then InternalRangeError;
    part := partEnum.PartInfo;
  end else
  begin
    prevPart := nil;
    part := GetPartInfo(APartIndex);
  end;

  result := GetUntransformedPartStartCaret(APartIndex, prevPart, part);
end;

function TBidiTextLayout.GetUntransformedPartStartCaret(APartIndex: integer;
  APrevPart, APart: PPartInfo): TBidiCaretPos;
begin
  result.PartIndex := APartIndex;

  if APart^.IsRightToLeft then
    result.Top := PointF(APart^.rectF.Right, APart^.rectF.Top)
  else
    result.Top := PointF(APart^.rectF.Left, APart^.rectF.Top);
  result.Bottom := result.Top + PointF(0, APart^.rectF.Height);

  result.RightToLeft := APart^.IsRightToLeft;

  if (APartIndex > 0) and (APrevPart^.endIndex = APart^.startIndex) and
    (BrokenLineUnbrokenIndex[APrevPart^.brokenLineIndex] =
     BrokenLineUnbrokenIndex[APart^.brokenLineIndex]) then
  begin
    if APrevPart^.IsRightToLeft then
      result.PreviousTop := PointF(APrevPart^.rectF.Left, APrevPart^.rectF.Top)
    else
      result.PreviousTop := PointF(APrevPart^.rectF.Right, APrevPart^.rectF.Top);
    result.PreviousBottom := result.PreviousTop + PointF(0, APrevPart^.rectF.Height);
    result.PreviousRightToLeft := APrevPart^.IsRightToLeft;
  end else
  begin
    result.PreviousTop := EmptyPointF;
    result.PreviousBottom := EmptyPointF;
    result.PreviousRightToLeft := result.RightToLeft;
  end;
end;

function TBidiTextLayout.GetUntransformedPartEndCaret(APartIndex: integer): TBidiCaretPos;
var
  part: PPartInfo;
begin
  part := GetPartInfo(APartIndex);
  result := GetUntransformedPartEndCaret(APartIndex, part);
end;

function TBidiTextLayout.GetUntransformedPartEndCaret(APartIndex: integer;
  APart: PPartInfo): TBidiCaretPos;
begin
  result.PartIndex := APartIndex;

  if APart^.IsRightToLeft then
    result.Top := PointF(APart^.rectF.Left, APart^.rectF.Top)
  else
    result.Top := PointF(APart^.rectF.Right, APart^.rectF.Top);
  result.Bottom := result.Top + PointF(0, APart^.rectF.Height);
  result.RightToLeft := APart^.IsRightToLeft;

  result.PreviousTop := EmptyPointF;
  result.PreviousBottom := EmptyPointF;
  result.PreviousRightToLeft := result.RightToLeft;
end;

function TBidiTextLayout.GetUntransformedParagraphAt(APosition: TPointF): integer;

  procedure FindRec(AFirstParaIndex, ALastParaIndex: integer);
  var
    midIndex: Integer;
  begin
    midIndex := (AFirstParaIndex + ALastParaIndex) shr 1;
    if APosition.y < FParagraph[midIndex].rectF.Top then
    begin
      if midIndex <= AFirstParaIndex then
      begin
        result := AFirstParaIndex;
        exit;
      end;
      FindRec(AFirstParaIndex, midIndex-1);
    end
    else if APosition.y >= FParagraph[midIndex].rectF.Bottom then
    begin
      if midIndex >= ALastParaIndex then
      begin
        result := ALastParaIndex;
        exit;
      end;
      FindRec(midIndex+1, ALastParaIndex);
    end
    else
    begin
      result := midIndex;
      exit;
    end;
  end;

begin
  NeedLayout;
  result := 0;
  FindRec(0, ParagraphCount-1);
end;

procedure TBidiTextLayout.AddPartsFromTree(APos: TPointF; ATree: TBidiTree;
  fullHeight, baseLine: single; ABrokenLineIndex: integer; ABrokenLine: PBrokenLineInfo);
var
  i: Integer;
  root, branch: TBidiLayoutTree;
  dy: Single;
begin
  root := TBidiLayoutTree(ATree);
  if root.IsLeaf then
  begin
    if (root.Height <> fullHeight) and (fullHeight <> 0) then
    begin
      dy := baseLine * (1 - root.Height/fullHeight);
    end else
      dy := 0;
    if odd(root.BidiLevel) then
    begin
      DecF(APos.x, root.Width);
      AddPart(root.StartIndex, root.EndIndex, root.BidiLevel,
              RectF(APos.x, APos.y, APos.x+root.Width, APos.y+fullHeight), PointF(0,dy), ABrokenLineIndex, ABrokenLine);
    end else
    begin
      AddPart(root.StartIndex, root.EndIndex, root.BidiLevel,
              RectF(APos.x, APos.y, APos.x+root.Width, APos.y+fullHeight), PointF(0,dy), ABrokenLineIndex, ABrokenLine);
      IncF(APos.x, root.Width);
    end;
  end else
  begin
    for i := 0 to root.Count-1 do
    begin
      branch := TBidiLayoutTree(root.Branch[i]);
      if odd(root.BidiLevel) then
      begin
        if odd(branch.BidiLevel) then
        begin
          AddPartsFromTree(APos, branch, fullHeight, baseLine, ABrokenLineIndex, ABrokenLine);
          DecF(APos.x, branch.Width);
        end else
        begin
          DecF(APos.x, branch.Width);
          AddPartsFromTree(APos, branch, fullHeight, baseLine, ABrokenLineIndex, ABrokenLine);
        end;
      end else
      begin
        if odd(branch.BidiLevel) then
        begin
          IncF(APos.x, branch.Width);
          AddPartsFromTree(APos, branch, fullHeight, baseLine, ABrokenLineIndex, ABrokenLine);
        end else
        begin
          AddPartsFromTree(APos, branch, fullHeight, baseLine, ABrokenLineIndex, ABrokenLine);
          IncF(APos.x, branch.Width);
        end;
      end;
    end;
  end;
end;

procedure TBidiTextLayout.Init(ATextUTF8: string; ABidiMode: TFontBidiMode);
var
  i: Integer;
begin
  FComputedBrokenLineCount:= 0;
  FComputedPartCount:= 0;
  FTopLeft := PointF(0,0);
  FAvailableWidth:= EmptySingle;
  FAvailableHeight:= EmptySingle;
  FTabSize := 8;
  FParagraphSpacingAbove:= 0;
  FParagraphSpacingBelow:= 0;
  FMatrix := AffineMatrixIdentity;
  FClipMargin := 0;
  FColor := BGRABlack;
  FTexture := nil;
  FWordBreakHandler:= nil;
  FAnalysis := TUnicodeAnalysis.Create(ATextUTF8, ABidiMode);
  FAnalysis.OnBidiModeChanged:= @BidiModeChanged;
  FAnalysis.OnCharDeleted:= @CharDeleted;
  FAnalysis.OnParagraphDeleted:=@ParagraphDeleted;
  FAnalysis.OnParagraphMergedWithNext:=@ParagraphMergedWithNext;
  FAnalysis.OnCharInserted:=@CharInserted;
  FAnalysis.OnParagraphSplit:=@ParagraphSplit;
  FAnalysis.OnAnalysisChanged:= @AnalysisChanged;
  SetLength(FParagraph, FAnalysis.ParagraphCount);
  for i := 0 to high(FParagraph) do
  begin
    FParagraph[i].rectF := EmptyRectF;
    FParagraph[i].alignment:= btaNatural;
    FParagraph[i].layoutComputed := false;
  end;
end;

end.

