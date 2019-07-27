unit BGRATextBidi;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, BGRAUTF8, BGRAUnicode, BGRATransform,
  BGRAUnicodeText;

type

  { TBidiCaretPos }

  TBidiCaretPos = record
    PartIndex: integer;

    Top, Bottom: TPointF;
    RightToLeft: boolean;

    PreviousTop, PreviousBottom: TPointF;
    PreviousRightToLeft: boolean;

    procedure Transform(AMatrix: TAffineMatrix);
  end;

  { TBidiTextLayout }

  TBidiTextLayout = class
  private
    FAvailableHeight: single;
    FAvailableWidth: single;
    FParagraphSpacingAbove: single;
    FParagraphSpacingBelow: single;
    FTopLeft: TPointF;
    FMatrix, FMatrixInverse: TAffineMatrix;
    FTabSize: Single;
    FWordBreakHandler: TWordBreakHandler;
    function GetBrokenLineAffineBox(AIndex: integer): TAffineBox;
    function GetBrokenLineCount: integer;
    function GetBrokenLineEndCaret(AIndex: integer): TBidiCaretPos;
    function GetBrokenLineUntransformedEndCaret(AIndex: integer): TBidiCaretPos;
    function GetBrokenLineEndIndex(AIndex: integer): integer;
    function GetBrokenLineParagraphIndex(AIndex: integer): integer;
    function GetBrokenLineRectF(AIndex: integer): TRectF;
    function GetBrokenLineRightToLeft(AIndex: integer): boolean;
    function GetBrokenLineStartCaret(AIndex: integer): TBidiCaretPos;
    function GetBrokenLineUntransformedStartCaret(AIndex: integer): TBidiCaretPos;
    function GetBrokenLineStartIndex(AIndex: integer): integer;
    function GetCharCount: integer;
    function GetFontBidiMode: TFontBidiMode;
    function GetMatrix: TAffineMatrix;
    function GetMatrixInverse: TAffineMatrix;
    function GetParagraphAffineBox(AIndex: integer): TAffineBox;
    function GetParagraphAlignment(AIndex: integer): TBidiTextAlignment;
    function GetParagraphCount: integer;
    function GetParagraphEndIndex(AIndex: integer): integer;
    function GetParagraphEndIndexBeforeParagraphSeparator(AIndex: integer): integer;
    function GetParagraphRectF(AIndex: integer): TRectF;
    function GetParagraphRightToLeft(AIndex: integer): boolean;
    function GetParagraphStartIndex(AIndex: integer): integer;
    function GetPartAffineBox(AIndex: integer): TAffineBox;
    function GetPartBrokenLineIndex(AIndex: integer): integer;
    function GetPartCount: integer;
    function GetPartEndIndex(AIndex: integer): integer;
    function GetPartRectF(AIndex: integer): TRectF;
    function GetPartRightToLeft(AIndex: integer): boolean;
    function GetPartStartIndex(AIndex: integer): integer;
    function GetText: string;
    function GetTotalTextHeight: single;
    function GetUnicodeChar(APosition0: integer): cardinal;
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

    FParagraph: array of record
      rectF: TRectF;
      alignment: TBidiTextAlignment;
      firstBrokenLineIndex: integer;
    end;

    FBrokenLine: array of record
                   unbrokenLineIndex: integer;
                   startIndex, endIndex: integer;
                   bidiLevel: byte;
                   rectF: TRectF;
                   firstPartIndex, lastPartIndexPlusOne: integer;
                 end;
    FBrokenLineCount: integer;

    FPart: array of record
             startIndex, endIndex: integer;
             bidiLevel: byte;
             rectF: TRectF;
             posCorrection: TPointF;
             sUTF8: string;
             brokenLineIndex: integer;
           end;
    FPartCount: integer;
    FLayoutComputed: boolean;
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
    function AddOverrideIfNecessary(var sUTF8: string; ARightToLeft: boolean): boolean;

    procedure AddPart(AStartIndex, AEndIndex: integer; ABidiLevel: byte; ARectF: TRectF; APosCorrection: TPointF; ASUTF8: string; ABrokenLineIndex: integer);
    function GetPartStartCaret(APartIndex: integer): TBidiCaretPos;
    function GetPartEndCaret(APartIndex: integer): TBidiCaretPos;
    function GetUntransformedPartStartCaret(APartIndex: integer): TBidiCaretPos;
    function GetUntransformedPartEndCaret(APartIndex: integer): TBidiCaretPos;
    function GetUntransformedParagraphAt(APosition: TPointF): integer; overload;

    function GetSameLevelString(startIndex,endIndex: integer): string; overload;
    function GetSameLevelString(startIndex,endIndex: integer; out nonDiscardedCount: integer): string; overload;
    function ComputeBidiTree(AMaxWidth: single; startIndex, endIndex: integer; bidiLevel: byte): TBidiTree;
    procedure AddPartsFromTree(APos: TPointF; ATree: TBidiTree; fullHeight, baseLine: single; brokenLineIndex: integer);
    procedure Init(ATextUTF8: string; ABidiMode: TFontBidiMode); virtual;
    procedure ComputeLayout; virtual;
    procedure NeedLayout;
    procedure InvalidateParagraphLayout({%H-}AParagraphIndex: integer);
    procedure InternalDrawText(ADest: TBGRACustomBitmap);
    procedure InternalPathText(ADest: IBGRAPath);

    //unicode analysis events
    procedure BidiModeChanged({%H-}ASender: TObject);
    procedure CharDeleted({%H-}ASender: TObject; AParagraphIndex: integer; {%H-}ACharStart, {%H-}ACharCount: integer);
    procedure CharInserted({%H-}ASender: TObject; AParagraphIndex: integer; {%H-}ACharStart, {%H-}ACharCount: integer);
    procedure ParagraphDeleted({%H-}ASender: TObject; AParagraphIndex: integer);
    procedure ParagraphMergedWithNext({%H-}ASender: TObject; AParagraphIndex: integer);
    procedure ParagraphSplit({%H-}ASender: TObject; AParagraphIndex: integer; {%H-}ACharIndex: integer);
  public
    constructor Create(AFontRenderer: TBGRACustomFontRenderer; sUTF8: string); overload;
    constructor Create(AFontRenderer: TBGRACustomFontRenderer; sUTF8: string; ARightToLeft: boolean); overload;
    constructor Create(AFontRenderer: TBGRACustomFontRenderer; sUTF8: string; AFontBidiMode: TFontBidiMode); overload;
    destructor Destroy; override;
    procedure SetLayout(ARect: TRectF);
    procedure InvalidateLayout;

    procedure DrawText(ADest: TBGRACustomBitmap); overload;
    procedure DrawText(ADest: TBGRACustomBitmap; AColor: TBGRAPixel); overload;
    procedure DrawText(ADest: TBGRACustomBitmap; ATexture: IBGRAScanner); overload;
    procedure PathText(ADest: IBGRAPath);
    procedure DrawCaret(ADest: TBGRACustomBitmap; ACharIndex: integer; AMainColor, ASecondaryColor: TBGRAPixel);
    procedure DrawSelection(ADest: TBGRACustomBitmap; AStartIndex, AEndIndex: integer;
                            AFillColor: TBGRAPixel; ABorderColor: TBGRAPixel; APenWidth: single); overload;
    procedure DrawSelection(ADest: TBGRACustomBitmap; AStartIndex, AEndIndex: integer;
                            AFillColor: TBGRAPixel); overload;

    function GetCaret(ACharIndex: integer): TBidiCaretPos;
    function GetUntransformedCaret(ACharIndex: integer): TBidiCaretPos;
    function GetCharIndexAt(APosition: TPointF): integer;
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
    function IncludeNonSpacingChars(APosition, ACount: integer): integer;
    function IncludeNonSpacingCharsBefore(APosition, ACount: integer): integer;
    function FindTextAbove(AFromPosition: integer): integer;
    function FindTextBelow(AFromPosition: integer): integer;

    property CharCount: integer read GetCharCount;
    property UTF8Char[APosition0: integer]: string4 read GetUTF8Char;
    property UnicodeChar[APosition0: integer]: cardinal read GetUnicodeChar;

    property BrokenLineCount: integer read GetBrokenLineCount;
    property BrokenLineParagraphIndex[AIndex: integer]: integer read GetBrokenLineParagraphIndex;
    property BrokenLineStartIndex[AIndex: integer]: integer read GetBrokenLineStartIndex;
    property BrokenLineEndIndex[AIndex: integer]: integer read GetBrokenLineEndIndex;
    property BrokenLineRectF[AIndex: integer]: TRectF read GetBrokenLineRectF;
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
    property ParagraphCount: integer read GetParagraphCount;

    property TotalTextHeight: single read GetTotalTextHeight;

    property Matrix: TAffineMatrix read GetMatrix;
    property MatrixInverse: TAffineMatrix read GetMatrixInverse;
    property TextUTF8: string read GetText;
    property WordBreakHandler: TWordBreakHandler read FWordBreakHandler write FWordBreakHandler;

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
  if Assigned(Parent) then result += TBidiLayoutTree(Parent).CumulatedBidiPos;
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
    FSize := Layout.TextSizeBidiOverride(FTextUTF8, odd(BidiLevel));
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
    FSize := Layout.TextSizeBidiOverride(FTextUTF8, odd(BidiLevel));
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
    fitInfo := Layout.TextFitInfoBidiOverride(FTextUTF8, remain, odd(BidiLevel));
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
  result := FBrokenLineCount;
end;

function TBidiTextLayout.GetBrokenLineEndCaret(AIndex: integer): TBidiCaretPos;
begin
  result := GetBrokenLineUntransformedEndCaret(AIndex);
  result.Transform(Matrix);
end;

function TBidiTextLayout.GetBrokenLineUntransformedEndCaret(AIndex: integer): TBidiCaretPos;
begin
  NeedLayout;
  with BrokenLineRectF[AIndex] do
  begin
    result.Top.y := Top;
    if BrokenLineRightToLeft[AIndex] then
      result.Top.x := Left
    else
      result.Top.x := Right;
    result.Bottom.y := Bottom;
    result.Bottom.x := result.Top.x;
    result.RightToLeft := odd(FBrokenLine[AIndex].bidiLevel);
    result.PartIndex:= -1;
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

function TBidiTextLayout.GetBrokenLineParagraphIndex(AIndex: integer): integer;
begin
  NeedLayout;
  if (AIndex < 0) or (AIndex >= FBrokenLineCount) then
    raise ERangeError.Create('Invalid index');
  result := FAnalysis.UnbrokenLineParagraphIndex[FBrokenLine[AIndex].unbrokenLineIndex];
end;

function TBidiTextLayout.GetBrokenLineRectF(AIndex: integer): TRectF;
begin
  NeedLayout;
  if (AIndex < 0) or (AIndex >= FBrokenLineCount) then
    raise ERangeError.Create('Invalid index');
  result := FBrokenLine[AIndex].rectF;
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
  result := GetBrokenLineUntransformedStartCaret(AIndex);
  result.Transform(Matrix);
end;

function TBidiTextLayout.GetBrokenLineUntransformedStartCaret(AIndex: integer): TBidiCaretPos;
begin
  NeedLayout;
  with BrokenLineRectF[AIndex] do
  begin
    result.Top.y := Top;
    if BrokenLineRightToLeft[AIndex] then
      result.Top.x := Right
    else
      result.Top.x := Left;
    result.Bottom.y := Bottom;
    result.Bottom.x := result.Top.x;
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

function TBidiTextLayout.GetCharCount: integer;
begin
  result := FAnalysis.CharCount;
end;

function TBidiTextLayout.GetFontBidiMode: TFontBidiMode;
begin
  result := FAnalysis.BidiMode;
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
  if (AIndex < 0) or (AIndex >= ParagraphCount) then
    raise ERangeError.Create('Invalid index');
  result := FParagraph[AIndex].alignment;
end;

function TBidiTextLayout.GetParagraphCount: integer;
begin
  result := FAnalysis.ParagraphCount;
end;

function TBidiTextLayout.GetParagraphEndIndex(AIndex: integer): integer;
begin
  result := FAnalysis.ParagraphEndIndex[AIndex];
end;

function TBidiTextLayout.GetParagraphEndIndexBeforeParagraphSeparator(AIndex: integer): integer;
begin
  result := FAnalysis.ParagraphEndIndexBeforeParagraphSeparator[AIndex];
end;

function TBidiTextLayout.GetParagraphRectF(AIndex: integer): TRectF;
begin
  NeedLayout;
  if (AIndex < 0) or (AIndex >= ParagraphCount) then
    raise ERangeError.Create('Paragraph index out of bounds');
  result := FParagraph[AIndex].rectF;
end;

function TBidiTextLayout.GetParagraphRightToLeft(AIndex: integer): boolean;
var
  firstUnbroken, startIndex: Integer;
begin
  if (AIndex < 0) or (AIndex >= ParagraphCount) then
    raise ERangeError.Create('Paragraph index out of bounds');

  firstUnbroken := FAnalysis.ParagraphFirstUnbrokenLine[AIndex];
  startIndex := FAnalysis.UnbrokenLineStartIndex[firstUnbroken];
  if startIndex < CharCount then
    result := odd(FAnalysis.BidiInfo[startIndex].ParagraphBidiLevel)
  else
    result := FontBidiMode = fbmRightToLeft;
end;

function TBidiTextLayout.GetParagraphStartIndex(AIndex: integer): integer;
begin
  result := FAnalysis.ParagraphStartIndex[AIndex];
end;

function TBidiTextLayout.GetPartAffineBox(AIndex: integer): TAffineBox;
begin
  result := Matrix*TAffineBox.AffineBox(PartRectF[AIndex]);
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

function TBidiTextLayout.GetPartRectF(AIndex: integer): TRectF;
begin
  NeedLayout;
  if (AIndex < 0) or (AIndex >= FPartCount) then
    raise ERangeError.Create('Invalid index');
  result := FPart[AIndex].rectF;
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

function TBidiTextLayout.GetText: string;
begin
  result := FAnalysis.TextUTF8;
end;

function TBidiTextLayout.GetTotalTextHeight: single;
begin
  NeedLayout;
  result := FParagraph[ParagraphCount-1].rectF.Bottom - FParagraph[0].rectF.Top;
end;

function TBidiTextLayout.GetUnicodeChar(APosition0: integer): cardinal;
begin
  result := FAnalysis.UnicodeChar[APosition0];
end;

function TBidiTextLayout.GetUTF8Char(APosition0: integer): string4;
begin
  result := FAnalysis.UTF8Char[APosition0];
end;

procedure TBidiTextLayout.SetAvailableHeight(AValue: single);
begin
  if FAvailableHeight=AValue then Exit;
  FAvailableHeight:=AValue;
  InvalidateLayout;
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
begin
  if (AIndex < 0) or (AIndex >= ParagraphCount) then
    raise ERangeError.Create('Paragraph index out of bounds');
  FParagraph[AIndex].alignment := AValue;
  InvalidateParagraphLayout(AIndex);
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
  if FLayoutComputed then ComputeMatrix;
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
begin
  InvalidateParagraphLayout(AParagraphIndex);
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
  i: Integer;
begin
  if (AParagraphIndex < 0) or (AParagraphIndex > high(FParagraph)) then
    raise exception.Create('Paragrah index out of bounds (0 <= '+inttostr(AParagraphIndex)+' <= '+inttostr(high(FParagraph))+')');
  setlength(FParagraph, length(FParagraph)+1);
  for i := high(FParagraph) downto AParagraphIndex+1 do
    FParagraph[i] := FParagraph[i-1];
  FParagraph[AParagraphIndex+1].rectF := EmptyRectF;

  InvalidateLayout;
end;

procedure TBidiTextLayout.CharInserted(ASender: TObject;
  AParagraphIndex: integer; ACharStart, ACharCount: integer);
begin
  InvalidateParagraphLayout(AParagraphIndex);
end;

procedure TBidiTextLayout.ParagraphMergedWithNext(ASender: TObject;
  AParagraphIndex: integer);
var
  i: Integer;
begin
  for i := AParagraphIndex+1 to high(FParagraph)-1 do
    FParagraph[i] := FParagraph[i+1];
  setlength(FParagraph, length(FParagraph)-1);

  InvalidateParagraphLayout(AParagraphIndex);
  InvalidateParagraphLayout(AParagraphIndex+1);
end;

procedure TBidiTextLayout.ParagraphDeleted(ASender: TObject;
  AParagraphIndex: integer);
var
  i: Integer;
begin
  for i := AParagraphIndex to high(FParagraph)-1 do
    FParagraph[i] := FParagraph[i+1];
  setlength(FParagraph, length(FParagraph)-1);

  InvalidateLayout;
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
    (GetUnicodeJoiningType(GetUnicodeChar(checkIndex))=ujtTransparent) do dec(checkIndex);
  if (ARightToLeft and FAnalysis.BidiInfo[checkIndex].HasLigatureLeft) or
     (not ARightToLeft and FAnalysis.BidiInfo[checkIndex].HasLigatureRight) then
    s := s+UnicodeCharToUTF8(UNICODE_ZERO_WIDTH_JOINER);
  result := TextSizeBidiOverride(s, ARightToLeft);
end;

function TBidiTextLayout.TextFitInfoBidiOverride(sUTF8: string; AWidth: single;
  ARightToLeft: boolean): integer;
var
  over: Boolean;
begin
  over := AddOverrideIfNecessary(sUTF8, ARightToLeft);

  result := FRenderer.TextFitInfoF(sUTF8, AWidth);
  if over then dec(result);
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
  u: Cardinal;
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

procedure TBidiTextLayout.AddPart(AStartIndex, AEndIndex: integer;
  ABidiLevel: byte; ARectF: TRectF; APosCorrection: TPointF; ASUTF8: string;
  ABrokenLineIndex: integer);
begin
  if FPartCount >= length(FPart) then
    setlength(FPart, length(FPart)*2+8);

  with FPart[FPartCount] do
  begin
    startIndex:= AStartIndex;
    endIndex:= AEndIndex;
    bidiLevel := ABidiLevel;
    rectF := ARectF;
    posCorrection := APosCorrection;
    sUTF8:= ASUTF8;
    brokenLineIndex:= ABrokenLineIndex;
  end;
  inc(FPartCount)
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
begin
  FLayoutComputed:= false;
end;

procedure TBidiTextLayout.InvalidateParagraphLayout(AParagraphIndex: integer);
begin
  InvalidateLayout;
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

procedure TBidiTextLayout.ComputeLayout;
var lineHeight, baseLine, tabPixelSize: single;
  paraIndex, ubIndex, i,j, nextTabIndex, splitIndex: Integer;
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

  procedure AddTabSection(startIndex,endIndex: integer; tree: TBidiLayoutTree);
  begin
    if tabSectionCount >= length(tabSection) then setlength(tabSection, length(tabSection)*2+4);
    tabSection[tabSectionCount].startIndex:= startIndex;
    tabSection[tabSectionCount].endIndex:= endIndex;
    tabSection[tabSectionCount].bidiPos:= curBidiPos;
    tabSection[tabSectionCount].tree := tree;
    inc(tabSectionCount);
  end;

  procedure AddBrokenLine(ACharStart: integer; ACharEnd: integer; ABidiLevel: byte; AWidth, AHeight: single);
  begin
    if FBrokenLineCount >= length(FBrokenLine) then
      setlength(FBrokenLine, length(FBrokenLine)*2+4);
    FBrokenLine[FBrokenLineCount].unbrokenLineIndex := ubIndex;
    FBrokenLine[FBrokenLineCount].startIndex:= ACharStart;
    FBrokenLine[FBrokenLineCount].endIndex:= ACharEnd;
    FBrokenLine[FBrokenLineCount].bidiLevel := ABidiLevel;
    FBrokenLine[FBrokenLineCount].firstPartIndex:= FPartCount;
    if FAvailableWidth <> EmptySingle then
      FBrokenLine[FBrokenLineCount].rectF := RectF(0,pos.y,FAvailableWidth,pos.y+AHeight)
    else
    begin
      case alignment of
      taRightJustify: FBrokenLine[FBrokenLineCount].rectF := RectF(-AWidth,pos.y,0,pos.y+AHeight);
      taCenter: FBrokenLine[FBrokenLineCount].rectF := RectF(-AWidth*0.5,pos.y,AWidth*0.5,pos.y+AHeight);
      else {taLeftJustify}
        FBrokenLine[FBrokenLineCount].rectF := RectF(0,pos.y,AWidth,pos.y+AHeight);
      end;
    end;
    FBrokenLineCount += 1;

    if FAvailableWidth = EmptySingle then
    begin
      if FParagraph[paraIndex].rectF.Left = EmptySingle then
      begin
        FParagraph[paraIndex].rectF.Left := FBrokenLine[FBrokenLineCount-1].rectF.left;
        FParagraph[paraIndex].rectF.Right := FBrokenLine[FBrokenLineCount-1].rectF.Right;
      end else
      begin
        if FParagraph[paraIndex].rectF.Left < FBrokenLine[FBrokenLineCount-1].rectF.left then
          FParagraph[paraIndex].rectF.Left := FBrokenLine[FBrokenLineCount-1].rectF.left;
        if FParagraph[paraIndex].rectF.Right > FBrokenLine[FBrokenLineCount-1].rectF.Right then
          FParagraph[paraIndex].rectF.Right := FBrokenLine[FBrokenLineCount-1].rectF.Right;
      end;
    end;
  end;

  procedure ClearTabSections;
  var
    i: Integer;
  begin
    tabSectionCount := 0;
    for i := 0 to high(tabSection) do
      FreeAndNil(tabSection[i].tree);
  end;

begin
  FLineHeight:= GetFontFullHeight;
  baseLine := GetFontBaseline;
  ComputeMatrix;
  FPartCount := 0;
  FBrokenLineCount := 0;

  FLayoutComputed:= true;

  paraSpacingAbove := ParagraphSpacingAbove*FLineHeight;
  paraSpacingBelow := ParagraphSpacingBelow*FLineHeight;
  pos := PointF(0,0);
  if FAvailableWidth <> EmptySingle then
    availWidth0 := FAvailableWidth
  else
    availWidth0:= 0;

  tabPixelSize := TabSize*TextSizeBidiOverride(' ',False).x;
  tabSection := nil;

  for paraIndex := 0 to ParagraphCount-1 do
  begin
    FParagraph[paraIndex].rectF.Top := pos.y;
    FParagraph[paraIndex].rectF.Bottom := pos.y;
    if FAvailableWidth <> EmptySingle then
    begin
      FParagraph[paraIndex].rectF.Left := 0;
      FParagraph[paraIndex].rectF.Right := FAvailableWidth;
    end else
    begin
      FParagraph[paraIndex].rectF.Left := EmptySingle;
      FParagraph[paraIndex].rectF.Right := EmptySingle;
    end;
    pos.y += paraSpacingAbove;
    paraRTL := ParagraphRightToLeft[paraIndex];

    if FAvailableWidth <> EmptySingle then
      alignment := BidiTextAlignmentToAlignment(FParagraph[paraIndex].alignment, paraRTL)
    else
      alignment := taLeftJustify;

    FParagraph[paraIndex].firstBrokenLineIndex:= FBrokenLineCount;

    for ubIndex := FAnalysis.ParagraphFirstUnbrokenLine[paraIndex] to FAnalysis.ParagraphLastUnbrokenLinePlusOne[paraIndex]-1 do
    begin
      if (FAvailableHeight <> EmptySingle) and (pos.y >= FAvailableHeight) then
      begin
        FParagraph[paraIndex].rectF.Bottom := pos.y;
        ClearTabSections;
        for i := paraIndex+1 to high(FParagraph) do
        begin
          FParagraph[i].rectF.Top := pos.y;
          FParagraph[i].rectF.Bottom := pos.y;
          if FAvailableWidth <> EmptySingle then
          begin
            FParagraph[i].rectF.Left := 0;
            FParagraph[i].rectF.Right := FAvailableWidth;
          end else
          begin
            FParagraph[i].rectF.Left := EmptySingle;
            FParagraph[i].rectF.Right := EmptySingle;
          end;
          FParagraph[i].firstBrokenLineIndex:= FBrokenLineCount;
        end;
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
        AddBrokenLine(subStart, lineEnd, paraBidiLevel, 0, FLineHeight);

        case alignment of
        taRightJustify: pos.x := availWidth0;
        taCenter: pos.x := availWidth0*0.5;
        else {taLeftJustify}
          pos.x := 0;
        end;
        AddPart(subStart, lineEnd, paraBidiLevel,
                RectF(pos.x,FBrokenLine[FBrokenLineCount-1].rectF.Top,
                      pos.x,FBrokenLine[FBrokenLineCount-1].rectF.Bottom),
                PointF(0,0), '', FBrokenLineCount-1);

        FBrokenLine[FBrokenLineCount-1].lastPartIndexPlusOne:= FPartCount;
        pos.y += FLineHeight;
      end else
      //break lines
      while subStart < lineEnd do
      begin
        //split into sections according to tabs
        ClearTabSections;
        curBidiPos := 0;
        tabSectionStart := subStart;
        tabSectionCount := 0;
        lineHeight := FLineHeight;

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

              curBidiPos += nextTree.Width;
              if nextTree.Height > lineHeight then lineHeight := nextTree.Height;

              tabSectionStart := splitIndex;
              while (tabSectionStart < nextTabIndex) and IsUnicodeSpace(FAnalysis.UnicodeChar[tabSectionStart]) do inc(tabSectionStart);
            end;
            break;
          end else
          begin
            curBidiPos += nextTree.Width;
            if nextTree.Height > lineHeight then lineHeight := nextTree.Height;
            tabSectionStart := splitIndex;
          end;
        end;

        // add broken line info
        AddBrokenLine(subStart, splitIndex, paraBidiLevel, curBidiPos, lineHeight);

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
          correctedBaseLine := baseLine*lineHeight/FLineHeight
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
              r := RectF(pos.x-endBidiPos, pos.y, pos.x-tabSection[j].bidiPos, pos.y+lineHeight)
            else
              r := RectF(pos.x+tabSection[j].bidiPos, pos.y, pos.x+endBidiPos, pos.y+lineHeight);

            AddPart(tabSection[j].startIndex, tabSection[j].endIndex, paraBidiLevel, r, PointF(0,0), #9, FBrokenLineCount-1);

          end
          else
          begin
            if paraRTL then
              AddPartsFromTree(pos - PointF(tabSection[j].bidiPos,0), tabSection[j].tree, lineHeight, correctedBaseLine, FBrokenLineCount-1)
            else
              AddPartsFromTree(pos + PointF(tabSection[j].bidiPos,0), tabSection[j].tree, lineHeight, correctedBaseLine, FBrokenLineCount-1)
          end;
        end;
        FBrokenLine[FBrokenLineCount-1].lastPartIndexPlusOne:= FPartCount;

        pos.y += lineHeight;
      end;
    end;
    pos.y += paraSpacingBelow;
    FParagraph[paraIndex].rectF.Bottom := pos.y;
  end;
  ClearTabSections;
end;

procedure TBidiTextLayout.NeedLayout;
begin
  if not FLayoutComputed then ComputeLayout;
end;

procedure TBidiTextLayout.InternalDrawText(ADest: TBGRACustomBitmap);
var
  i: Integer;
  b: TRect;
begin
  NeedLayout;
  for i := 0 to FPartCount-1 do
  begin
    b := PartAffineBox[i].RectBounds;
    if not b.IntersectsWith(ADest.ClipRect) then continue;
    with (Matrix*(FPart[i].rectF.TopLeft + FPart[i].posCorrection)) do
      TextOutBidiOverride(ADest, x,y, FPart[i].sUTF8, odd(FPart[i].bidiLevel));
  end;
end;

procedure TBidiTextLayout.InternalPathText(ADest: IBGRAPath);
var
  i: Integer;
begin
  NeedLayout;
  for i := 0 to FPartCount-1 do
  begin
    with (Matrix*(FPart[i].rectF.TopLeft + FPart[i].posCorrection)) do
      TextPathBidiOverride(ADest, x,y, FPart[i].sUTF8, odd(FPart[i].bidiLevel));
  end;
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
  i, blIndex: Integer;
  w: Single;
begin
  NeedLayout;

  if (ACharIndex < 0) or (ACharIndex > CharCount) then
    raise ERangeError.Create('Invalid index');

  if (PartCount > 0) and (ACharIndex >= FPart[PartCount-1].endIndex) then
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
  if blIndex <> -1 then
  for i := FBrokenLine[blIndex].firstPartIndex to FBrokenLine[blIndex].lastPartIndexPlusOne-1 do
    if ACharIndex <= FPart[i].startIndex then
    begin
      result := GetUntransformedPartStartCaret(i);
      exit;
    end else
    if (ACharIndex > FPart[i].startIndex) and (ACharIndex <= FPart[i].endIndex) then
    begin
      if (i < FPartCount-1) and (ACharIndex = FPart[i+1].startIndex) then
      begin
        result := GetUntransformedPartStartCaret(i+1);
        exit;
      end else
      begin
        if ACharIndex = FPart[i].endIndex then
        begin
          result := GetUntransformedPartEndCaret(i);
          exit;
        end else
        begin
          w := TextSizeBidiOverrideSplit(FPart[i].startIndex, FPart[i].endIndex, odd(FPart[i].bidiLevel), ACharIndex).x;

          if Odd(FPart[i].bidiLevel) then
            result.Top := PointF(FPart[i].rectF.Right - w, FPart[i].rectF.Top)
          else result.Top := PointF(FPart[i].rectF.Left + w, FPart[i].rectF.Top);
          result.Bottom := result.Top + PointF(0,FPart[i].rectF.Height);

          result.RightToLeft := odd(FPart[i].bidiLevel);
          result.PreviousRightToLeft := result.RightToLeft;
          result.PartIndex := i;
        end;
        exit;
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

function TBidiTextLayout.GetCharIndexAt(APosition: TPointF): integer;
var
  brokenLineIndex,j, fit: Integer;
  u,u2: cardinal;
  axis, origin: TPointF;
  len, w, curW, newW: Single;
  str: String;
  curIndex, newIndex, paraIndex, brokenStart, brokenEnd: integer;
  untransformedPos: TPointF;
begin
  NeedLayout;
  untransformedPos := FMatrixInverse*APosition;
  paraIndex := GetUntransformedParagraphAt(untransformedPos);

  if untransformedPos.Y < FParagraph[paraIndex].rectF.Top then
    exit(ParagraphStartIndex[paraIndex]);

  if untransformedPos.Y >= FParagraph[paraIndex].rectF.Bottom then
    exit(ParagraphEndIndex[paraIndex]);

  brokenStart := FParagraph[paraIndex].firstBrokenLineIndex;
  if paraIndex = ParagraphCount-1 then
    brokenEnd := BrokenLineCount
  else
    brokenEnd := FParagraph[paraIndex+1].firstBrokenLineIndex;

  for brokenLineIndex := brokenStart to brokenEnd-1 do
    if untransformedPos.Y < FBrokenLine[brokenLineIndex].rectF.Bottom then
    begin
      if untransformedPos.Y < FBrokenLine[brokenLineIndex].rectF.Top then
        exit(FBrokenLine[brokenLineIndex].startIndex);

      j := FBrokenLine[brokenLineIndex].firstPartIndex;
      if j < FBrokenLine[brokenLineIndex].lastPartIndexPlusOne then
      begin
        if (BrokenLineRightToLeft[brokenLineIndex] and (untransformedPos.x >= PartRectF[j].Right)) or
           (not BrokenLineRightToLeft[brokenLineIndex] and (untransformedPos.x < PartRectF[j].Left)) then
          exit(FBrokenLine[brokenLineIndex].startIndex)
      end;

      for j := FBrokenLine[brokenLineIndex].firstPartIndex to FBrokenLine[brokenLineIndex].lastPartIndexPlusOne-1 do
        if (PartBrokenLineIndex[j] = brokenLineIndex) and PartAffineBox[j].Contains(APosition) then
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
              //if there is just one char, it is the whole part
              if PartEndIndex[j] = PartStartIndex[j]+1 then
              begin
                if w > 0.5*len then
                  exit(PartEndIndex[j])
                else
                  exit(PartStartIndex[j]);
              end;

              str := FAnalysis.CopyTextUTF8(PartStartIndex[j], PartEndIndex[j]-PartStartIndex[j]);
              fit := TextFitInfoBidiOverride(str, w, PartRightToLeft[j]);
              curIndex := PartStartIndex[j]+fit;
              if curIndex > PartEndIndex[j] then curIndex:= PartEndIndex[j];
              if curIndex = 0 then curW := 0
              else curW := TextSizeBidiOverrideSplit(PartStartIndex[j], PartEndIndex[j], PartRightToLeft[j], curIndex).x;
              while (curW < w) and (curIndex < PartEndIndex[j]) do
              begin
                newIndex := curIndex+1;
                while (newIndex < PartEndIndex[j]) and not FAnalysis.BidiInfo[newIndex].IsMulticharStart do inc(newIndex);
                newW := TextSizeBidiOverrideSplit(PartStartIndex[j], PartEndIndex[j], PartRightToLeft[j], newIndex).x;
                if newW >= w then
                begin
                  if (curW+newW)*0.5 + 1 < w then curIndex := newIndex;
                  break;
                end else
                begin
                  curW := newW;
                  curIndex := newIndex;
                end;
              end;
              exit(curIndex);
            end;
          end;
          exit(PartStartIndex[j]);
        end;
      result := BrokenLineEndIndex[brokenLineIndex];
      if result > BrokenLineStartIndex[brokenLineIndex] then
      begin
        u := GetUnicodeChar(result-1);
        if IsUnicodeParagraphSeparator(u) or (u = UNICODE_LINE_SEPARATOR) then
        begin
          dec(result);
          if (result > BrokenLineStartIndex[brokenLineIndex]) and (u = 13) or (u = 10) then
          begin
            u2 := GetUnicodeChar(result-1);
            if (u2 <> u) and ((u2 = 13) or (u2 = 10)) then dec(result);
          end;
        end;
      end;
      exit;
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
    i: Integer;
    curPartStartCaret, curPartEndCaret,
    lineStartCaret, lineEndCaret, curPartCaret: TBidiCaretPos;
    brokenLineIndex, paraIndex: integer;
    r: TRectF;

  begin
    horizResult := nil;

    i := startCaret.PartIndex;
    while i <= endCaret.PartIndex do
    begin
      //space between paragraph
      if (i > startCaret.PartIndex) and (ParagraphSpacingAbove+ParagraphSpacingBelow <> 0) then
      begin
        paraIndex := BrokenLineParagraphIndex[PartBrokenLineIndex[i]];
        if (paraIndex > 0) and (BrokenLineParagraphIndex[PartBrokenLineIndex[i-1]] = paraIndex-1) then
        begin
          FlushHorizResult;

          r := RectF(ParagraphRectF[paraIndex-1].Left, ParagraphRectF[paraIndex-1].Bottom - ParagraphSpacingBelow*FLineHeight,
                       ParagraphRectF[paraIndex-1].Right, ParagraphRectF[paraIndex-1].Bottom);
          AppendVertResult(TAffineBox.AffineBox(r), False);

          r := RectF(ParagraphRectF[paraIndex].Left, ParagraphRectF[paraIndex].Top,
                       ParagraphRectF[paraIndex].Right, ParagraphRectF[paraIndex].Top + ParagraphSpacingAbove*FLineHeight);
          AppendVertResult(TAffineBox.AffineBox(r), False);
        end;
      end;

      brokenLineIndex := PartBrokenLineIndex[i];
      //whole broken line selected
      if (i = FBrokenLine[brokenLineIndex].firstPartIndex) and
         ((i > startCaret.PartIndex) or (AStartIndex = PartStartIndex[i])) and
         (endCaret.PartIndex >= FBrokenLine[brokenLineIndex].lastPartIndexPlusOne) then
      begin
        FlushHorizResult;

        lineStartCaret := GetBrokenLineUntransformedStartCaret(brokenLineIndex);
        lineEndCaret := GetBrokenLineUntransformedEndCaret(brokenLineIndex);
        AppendVertResult(TAffineBox.AffineBox(lineStartCaret.Top,lineEndCaret.Top,lineStartCaret.Bottom), BrokenLineRightToLeft[brokenLineIndex]);

        i := FBrokenLine[brokenLineIndex].lastPartIndexPlusOne;
      end else
      begin
        //start of lines
        if (i > startCaret.PartIndex) and (PartBrokenLineIndex[i-1] <> brokenLineIndex) then
        begin
          FlushHorizResult;

          lineStartCaret := GetBrokenLineUntransformedStartCaret(brokenLineIndex);
          if BrokenLineRightToLeft[brokenLineIndex] = PartRightToLeft[i] then
            AppendHorizResult(lineStartCaret.Top,GetUntransformedPartStartCaret(i).Top,GetUntransformedPartStartCaret(i).Bottom,lineStartCaret.Bottom, BrokenLineRightToLeft[brokenLineIndex])
          else
            AppendHorizResult(lineStartCaret.Top,GetUntransformedPartEndCaret(i).Top,GetUntransformedPartEndCaret(i).Bottom,lineStartCaret.Bottom, BrokenLineRightToLeft[brokenLineIndex]);
        end;

        //text parts
        if i > startCaret.PartIndex then curPartStartCaret := GetUntransformedPartStartCaret(i)
        else curPartStartCaret := startCaret;

        if i < endCaret.PartIndex then curPartEndCaret := GetUntransformedPartEndCaret(i)
        else curPartEndCaret := endCaret;

        AppendHorizResult(curPartStartCaret.Top,curPartEndCaret.Top,curPartEndCaret.Bottom,curPartStartCaret.Bottom, PartRightToLeft[i]);

        //end of lines
        if (i < endCaret.PartIndex) and (PartBrokenLineIndex[i+1] <> PartBrokenLineIndex[i]) then
        begin
          lineEndCaret := GetBrokenLineUntransformedEndCaret(brokenLineIndex);
          if BrokenLineRightToLeft[brokenLineIndex] = PartRightToLeft[i] then
            curPartCaret := GetUntransformedPartEndCaret(i)
          else
            curPartCaret := GetUntransformedPartStartCaret(i);
          AppendHorizResult(curPartCaret.Top,lineEndCaret.Top,lineEndCaret.Bottom,curPartCaret.Bottom, BrokenLineRightToLeft[brokenLineIndex])
        end;

        inc(i);
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
  procedure FindRec(AFirstBrokenLineIndex, ALastBrokenLineIndex: integer);
  var
    midIndex: Integer;
  begin
    if ALastBrokenLineIndex<AFirstBrokenLineIndex then
    begin
      result := -1;
      exit;
    end;
    midIndex := (AFirstBrokenLineIndex+ALastBrokenLineIndex) shr 1;
    if (ACharIndex < FBrokenLine[midIndex].startIndex) then
      FindRec(AFirstBrokenLineIndex, midIndex-1)
    else if (midIndex < FBrokenLineCount-1) and (ACharIndex >= FBrokenLine[midIndex+1].startIndex) then
      FindRec(midIndex+1, ALastBrokenLineIndex)
    else
    begin
      result := midIndex;
      exit;
    end;
  end;

begin
  if (ACharIndex < 0) or (ACharIndex > CharCount) then raise exception.Create('Position out of bounds');
  if (BrokenLineCount > 0) and (ACharIndex = FBrokenLine[BrokenLineCount-1].endIndex) then
    result := BrokenLineCount-1
  else
    FindRec(0, BrokenLineCount-1);
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

function TBidiTextLayout.IncludeNonSpacingChars(APosition, ACount: integer): integer;
begin
  result := FAnalysis.IncludeNonSpacingChars(APosition,ACount);
end;

function TBidiTextLayout.IncludeNonSpacingCharsBefore(APosition, ACount: integer): integer;
begin
  result := FAnalysis.IncludeNonSpacingCharsBefore(APosition,ACount);
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
begin
  if (APartIndex < 0) or (APartIndex > PartCount) then
    raise ERangeError.Create('Invalid index');

  result.PartIndex := APartIndex;

  if Odd(FPart[APartIndex].bidiLevel) then
    result.Top := PointF(FPart[APartIndex].rectF.Right, FPart[APartIndex].rectF.Top)
  else
    result.Top := PointF(FPart[APartIndex].rectF.Left, FPart[APartIndex].rectF.Top);
  result.Bottom := result.Top + PointF(0, FPart[APartIndex].rectF.Height);

  result.RightToLeft := odd(FPart[APartIndex].bidiLevel);

  if (APartIndex > 0) and (FPart[APartIndex-1].endIndex = FPart[APartIndex].startIndex) and
    (FBrokenLine[FPart[APartIndex-1].brokenLineIndex].unbrokenLineIndex =
     FBrokenLine[FPart[APartIndex].brokenLineIndex].unbrokenLineIndex) then
  begin
    if Odd(FPart[APartIndex-1].bidiLevel) then
      result.PreviousTop := PointF(FPart[APartIndex-1].rectF.Left, FPart[APartIndex-1].rectF.Top)
    else
      result.PreviousTop := PointF(FPart[APartIndex-1].rectF.Right, FPart[APartIndex-1].rectF.Top);
    result.PreviousBottom := result.PreviousTop + PointF(0, FPart[APartIndex-1].rectF.Height);
    result.PreviousRightToLeft := odd(FPart[APartIndex-1].bidiLevel);
  end else
  begin
    result.PreviousTop := EmptyPointF;
    result.PreviousBottom := EmptyPointF;
    result.PreviousRightToLeft := result.RightToLeft;
  end;
end;

function TBidiTextLayout.GetUntransformedPartEndCaret(APartIndex: integer): TBidiCaretPos;
begin
  if (APartIndex < 0) or (APartIndex > PartCount) then
    raise ERangeError.Create('Invalid index');

  result.PartIndex := APartIndex;

  if Odd(FPart[APartIndex].bidiLevel) then
    result.Top := PointF(FPart[APartIndex].rectF.Left, FPart[APartIndex].rectF.Top)
  else
    result.Top := PointF(FPart[APartIndex].rectF.Right, FPart[APartIndex].rectF.Top);
  result.Bottom := result.Top + PointF(0, FPart[APartIndex].rectF.Height);
  result.RightToLeft := odd(FPart[APartIndex].bidiLevel);

  result.PreviousTop := EmptyPointF;
  result.PreviousBottom := EmptyPointF;
  result.PreviousRightToLeft := result.RightToLeft;
end;

function TBidiTextLayout.GetUntransformedParagraphAt(APosition: TPointF): integer;

  procedure FindRec(AFirstParaIndex, ALastParaIndex: integer);
  var
    midIndex: Integer;
  begin
    midIndex := (AFirstParaIndex+ALastParaIndex) shr 1;
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
  FindRec(0, ParagraphCount-1);
end;

procedure TBidiTextLayout.AddPartsFromTree(APos: TPointF; ATree: TBidiTree;
  fullHeight, baseLine: single; brokenLineIndex: integer);
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
      APos.x -= root.Width;
      AddPart(root.StartIndex, root.EndIndex, root.BidiLevel,
              RectF(APos.x, APos.y, APos.x+root.Width, APos.y+fullHeight), PointF(0,dy), root.FTextUTF8, brokenLineIndex);
    end else
    begin
      AddPart(root.StartIndex, root.EndIndex, root.BidiLevel,
              RectF(APos.x, APos.y, APos.x+root.Width, APos.y+fullHeight), PointF(0,dy), root.FTextUTF8, brokenLineIndex);
      APos.x += root.Width;
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
          AddPartsFromTree(APos, branch, fullHeight, baseLine, brokenLineIndex);
          APos.x -= branch.Width;
        end else
        begin
          APos.x -= branch.Width;
          AddPartsFromTree(APos, branch, fullHeight, baseLine, brokenLineIndex);
        end;
      end else
      begin
        if odd(branch.BidiLevel) then
        begin
          APos.x += branch.Width;
          AddPartsFromTree(APos, branch, fullHeight, baseLine, brokenLineIndex);
        end else
        begin
          AddPartsFromTree(APos, branch, fullHeight, baseLine, brokenLineIndex);
          APos.x += branch.Width;
        end;
      end;
    end;
  end;
end;

procedure TBidiTextLayout.Init(ATextUTF8: string; ABidiMode: TFontBidiMode);
var
  i: Integer;
begin
  FBrokenLineCount:= 0;
  FPartCount:= 0;
  FTopLeft := PointF(0,0);
  FAvailableWidth:= EmptySingle;
  FAvailableHeight:= EmptySingle;
  FTabSize := 8;
  FParagraphSpacingAbove:= 0;
  FParagraphSpacingBelow:= 0;
  FMatrix := AffineMatrixIdentity;
  FLayoutComputed:= false;
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
  SetLength(FParagraph, FAnalysis.ParagraphCount);
  for i := 0 to high(FParagraph) do
  begin
    FParagraph[i].rectF := EmptyRectF;
    FParagraph[i].alignment:= btaNatural;
  end;
end;

end.

