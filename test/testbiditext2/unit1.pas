unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, ComCtrls, BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes,
  BGRATextBidi, BGRAFreeType, EasyLazFreeType, LazFreeTypeFontCollection,
  fgl, Types;

const
  CaretBlinkTimeMs = 500;
  ssShortcut = {$IFDEF DARWIN}ssMeta{$ELSE}ssCtrl{$ENDIF};

type
  TRenderedBrokenLineList = specialize TFPGObjectList<TBGRABitmap>;

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    CheckBox_ClearType: TCheckBox;
    CheckBox_FreeType: TCheckBox;
    ImageList1: TImageList;
    Label2: TLabel;
    Panel1: TPanel;
    ScrollBar1: TScrollBar;
    SpinEdit_FontSize: TSpinEdit;
    TimerBlinkCaret: TTimer;
    ToolBar1: TToolBar;
    ToolButtonLeftAlign: TToolButton;
    ToolButtonCenterAlign: TToolButton;
    ToolButtonRightAlign: TToolButton;
    procedure BGRAVirtualScreen1MouseDown(Sender: TObject;
      Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BGRAVirtualScreen1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure CheckBox_ClearTypeChange(Sender: TObject);
    procedure CheckBox_FreeTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure ScrollBar1Change(Sender: TObject);
    procedure SpinEdit_FontSizeChange(Sender: TObject);
    procedure TimerBlinkCaretTimer(Sender: TObject);
    procedure ToolButtonCenterAlignClick(Sender: TObject);
    procedure ToolButtonLeftAlignClick(Sender: TObject);
    procedure ToolButtonRightAlignClick(Sender: TObject);
  private
    FFontRenderer: TBGRACustomFontRenderer;
    FTextLayout: TBidiTextLayout;
    FRenderedParagraphs: array of TRenderedBrokenLineList;
    FBlinkCaretTime: TDateTime;
    FBlinkCaretState: boolean;
    FSelStart,FSelLength: integer;
    FSelFirstClick,FSelLastClick: integer;
    FCurFirstParagraph,FCurLastParagraph: integer;
    FTestText: string;
    FInUnicode: boolean;
    FUnicodeValue: LongWord;
    function GetLayoutReady: boolean;
    function GetSelLastClick: integer;
    procedure LayoutBrokenLinesChanged({%H}ASender: TObject;
      AParagraphIndex: integer; ASubBrokenStart, ASubBrokenChangedCountBefore,
      ASubBrokenChangedCountAfter: integer; ASubBrokenTotalCountBefore,
      {%H}ASubBrokenTotalCountAfter: integer);
    procedure LayoutParagraphDeleted({%H}ASender: TObject; AParagraphIndex: integer);
    procedure LayoutParagraphMergedWithNext(ASender: TObject;
      AParagraphIndex: integer);
    procedure LayoutParagraphSplit({%H}ASender: TObject; AParagraphIndex: integer;
      ASubBrokenIndex, {%H-}ACharIndex: integer);
    procedure SetSelLastClick(AValue: integer);
    procedure SetSelLength(AValue: integer);
    procedure SetSelStart(AValue: integer);
    procedure FlushUnicode;
    procedure DiscardRenderedBrokenLines;
    procedure LayoutCompletelyChanged;
  public
    procedure UpdateCurrentParagraph;
    procedure UpdateSelectionFromFirstLastClick;
    procedure SetCurrentParagraphAlign(AAlign: TAlignment);
    procedure DeleteSelection;
    procedure InsertText(AText: string);
    procedure ShowCaret;
    property SelStart: integer read FSelStart write SetSelStart;
    property SelLength: integer read FSelLength write SetSelLength;
    property SelLastClick: integer read GetSelLastClick write SetSelLastClick;
    property LayoutReady: boolean read GetLayoutReady;
  end;

var
  Form1: TForm1;

implementation

uses BGRAText, LCLType, BGRAUTF8, Clipbrd, LCLIntf, math;

{$R *.lfm}

procedure SetClipboardAsText(Value: string);
var
  strStream: TStringStream;
begin
  strStream := TStringStream.Create(Value);
  Clipboard.SetFormat(PredefinedClipboardFormat(pcfText), strStream);
  strStream.Free;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTextLayout := nil;
  FFontRenderer := nil;
  FSelStart:= 0;
  FSelLength:= 0;
  FSelFirstClick := -1;
  FSelLastClick:= -1;
  TimerBlinkCaret.Interval := CaretBlinkTimeMs;
  FCurFirstParagraph:= -1;
  FCurLastParagraph:= -1;

  BGRAVirtualScreen1.OnKeyDown:= @FormKeyDown;
  BGRAVirtualScreen1.OnKeyUp:= @FormKeyUp;
  BGRAVirtualScreen1.OnKeyPress:= @FormKeyPress;
  BGRAVirtualScreen1.Cursor := crIBeam;
  BGRAVirtualScreen1.BitmapAutoScale:= false;

  FTestText := 'تحتوي العربية على 28 حرفاً مكتوباً. ويرى بعض اللغويين أنه يجب إضافة حرف الهمزة إلى حروف العربية، ليصبح عدد الحروف 29. تُكتب العربية من اليمين إلى اليسار - ومثلها اللغة الفارسية والعبرية على عكس كثير من اللغات العالمية - ومن أعلى الصفحة إلى أسفلها.'+LineEnding+
             'Arabic reversed "' + UTF8OverrideDirection('صباح الخير',false)+'". Arabic marks: "لاٍُ لٍُإ بًٍّ  ةُِ ںْ رُ ٮَ  بٔ".'+ LineEnding +
             #9'Le français est une langue indo-européenne de la famille des langues romanes. Le français s''est formé en France (variété de la « langue d''oïl », qui est la langue de la partie septentrionale du pays).'+LineEnding+
             'Glorious finds itself reversed as '+ UTF8OverrideDirection('"glorious"',True) + '. ' +
               '"Hello!" is '+ UTF8EmbedDirection('"مرحبا!"',True) + ' in arabic.' + LineEnding +
             'देवनागरी एक भारतीय लिपि है जिसमें अनेक भारतीय भाषाएँ तथा कई विदेशी भाषाएँ लिखी जाती हैं। यह बायें से दायें लिखी जाती है।' + LineEnding +
             '对于汉语的分支语言，学界主要有两种观点，一种观点将汉语定义为语言，并将官话、贛語、闽语、粤语、客家语、吴语、湘语七大语言定义为一级方言.'+LineEnding+
             'עִבְרִית היא שפה שמית, ממשפחת השפות האפרו-אסיאתיות, הידועה כשפתם של היהודים ושל השומרונים, אשר ניב מודרני שלה (עברית ישראלית) משמש כשפה הרשמית והעיקרית של מדינת ישראל.';

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DiscardRenderedBrokenLines;
  FTextLayout.Free;
  FFontRenderer.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  procedure MoveTo(ANewPos: integer);
  begin
    if ssShift in Shift then
    begin
      if ANewPos <> -1 then
        SelLastClick := ANewPos;
    end else
    begin
      if ANewPos <> -1 then
        SelStart := ANewPos;
      SelLength:= 0;
    end;
  end;

var
  idxPara, newPos: Integer;
begin
  if not LayoutReady then exit;

  if (Key = VK_U) and ([ssCtrl,ssShift] <= Shift) then
  begin
    FlushUnicode;
    FInUnicode := true;
    FUnicodeValue:= 0;
    Key := 0;
  end else
  if FInUnicode then
  begin
    case Key of
      VK_DELETE: begin
          FUnicodeValue := FUnicodeValue shr 4;
        end;
      VK_0..VK_9: begin
          FUnicodeValue := (FUnicodeValue shl 4) + (Key - VK_0);
        end;
      VK_NUMPAD0..VK_NUMPAD9: begin
          FUnicodeValue := (FUnicodeValue shl 4) + (Key - VK_NUMPAD0);
        end;
      VK_A..VK_F: begin
          FUnicodeValue := (FUnicodeValue shl 4) + (Key - VK_A + 10);
        end;
    else
      FlushUnicode;
    end;
    if (FUnicodeValue >= $10FFF0) or
       (FUnicodeValue >= $11000) then
      FlushUnicode;
    Key := 0;
  end else
  if KEY = VK_DELETE then
  begin
    if SelLength > 0 then DeleteSelection
    else
    begin
      FTextLayout.DeleteText(SelStart, 1);
      SelStart := SelStart + FTextLayout.IncludeNonSpacingChars(SelStart, 0);
      ShowCaret;
    end;
    Key := 0;
  end else
  if (Key = VK_LEFT) or (Key = VK_RIGHT) then
  begin
    if (Key = VK_LEFT) xor FTextLayout.ParagraphRightToLeft[FTextLayout.GetParagraphAt(SelLastClick)] then
    begin
      if SelLastClick > 0 then
        newPos := SelLastClick - FTextLayout.IncludeNonSpacingCharsBefore(SelLastClick,1)
      else newPos := -1;

      MoveTo(newPos);
    end else
    begin
      if SelLastClick < FTextLayout.CharCount then
        newPos := SelLastClick + FTextLayout.IncludeNonSpacingChars(SelLastClick,1)
      else newPos := -1;

      MoveTo(newPos);
    end;
    Key := 0;
  end else
  if (Key = VK_UP) or (Key = VK_DOWN) then
  begin
    if Key = VK_UP then
      newPos := FTextLayout.FindTextAbove(SelLastClick)
    else
      newPos := FTextLayout.FindTextBelow(SelLastClick);

    MoveTo(newPos);
    Key := 0;
  end else
  if Key = VK_HOME then
  begin
    idxPara := FTextLayout.GetParagraphAt(SelLastClick);
    if ssCtrl in Shift then newPos := 0 else
      newPos := FTextLayout.ParagraphStartIndex[idxPara];
    MoveTo(newPos);
    Key := 0;
  end else
  if Key = VK_END then
  begin
    idxPara := FTextLayout.GetParagraphAt(SelLastClick);
    if ssCtrl in Shift then newPos := FTextLayout.CharCount else
      newPos := FTextLayout.ParagraphEndIndexBeforeParagraphSeparator[idxPara];
    MoveTo(newPos);
    Key := 0;
  end else
  if Key = VK_RETURN then
  begin
    if SelLength > 0 then DeleteSelection;
    if ssShift in Shift then
    begin
      SelStart := SelStart + FTextLayout.InsertLineSeparator(SelStart);
    end else
      InsertText(LineEnding);
    Key := 0;
  end else
  if Key = VK_TAB then
  begin
    if SelLength > 0 then DeleteSelection;
    InsertText(#9);
    Key := 0;
  end else
  If (Key = VK_C) and (ssShortcut in Shift) then
  begin
    if SelLength> 0 then
      SetClipboardAsText(FTextLayout.CopyText(SelStart, SelLength));
    Key := 0;
  end else
  If (Key = VK_X) and (ssShortcut in Shift) then
  begin
    if SelLength > 0 then
    begin
      SetClipboardAsText(FTextLayout.CopyText(SelStart, SelLength));
      DeleteSelection;
    end;
    Key := 0;
  end else
  If (Key = VK_V) and (ssShortcut in Shift) then
  begin
    InsertText(Clipboard.AsText);
    Key := 0;
  end else
  If (Key = VK_A) and (ssShortcut in Shift) then
  begin
    SelStart:= 0;
    SelLength:= FTextLayout.CharCount;
    Key := 0;
  end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FInUnicode then
  begin
    if (Key = VK_CONTROL) or (Key = VK_SHIFT) then
      FlushUnicode;
  end;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
var
  delCount: Integer;
begin
  if not LayoutReady then exit;

  if Key = #8 then
  begin
    if SelLength > 0 then DeleteSelection
    else
    begin
      if SelStart > 0 then
      begin
        delCount := FTextLayout.DeleteTextBefore(SelStart, 1);
        SelStart := SelStart - delCount;
        SelStart := SelStart + FTextLayout.IncludeNonSpacingChars(SelStart, 0);
      end;
    end;
  end
  else
  if Key = #13 then
    InsertText(LineEnding)
  else
    InsertText(Key);
  Key := #0
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
  BGRAVirtualScreen1.DiscardBitmap;
end;

procedure TForm1.SpinEdit_FontSizeChange(Sender: TObject);
begin
  LayoutCompletelyChanged;
end;

procedure TForm1.TimerBlinkCaretTimer(Sender: TObject);
begin
  BGRAVirtualScreen1.DiscardBitmap;
end;

procedure TForm1.ToolButtonCenterAlignClick(Sender: TObject);
begin
  SetCurrentParagraphAlign(taCenter);
end;

procedure TForm1.ToolButtonLeftAlignClick(Sender: TObject);
begin
  SetCurrentParagraphAlign(taLeftJustify);
end;

procedure TForm1.ToolButtonRightAlignClick(Sender: TObject);
begin
  SetCurrentParagraphAlign(taRightJustify);
end;

function TForm1.GetLayoutReady: boolean;
begin
  result := Assigned(FTextLayout) and Assigned(FFontRenderer);
end;

function TForm1.GetSelLastClick: integer;
begin
  if FSelLastClick = -1 then
    result := FSelStart + FSelLength
  else
    result := FSelLastClick;
end;

procedure TForm1.LayoutBrokenLinesChanged(ASender: TObject;
  AParagraphIndex: integer; ASubBrokenStart, ASubBrokenChangedCountBefore,
  ASubBrokenChangedCountAfter: integer; ASubBrokenTotalCountBefore,
  ASubBrokenTotalCountAfter: integer);
var
  i: Integer;
begin
  if (AParagraphIndex < 0) or (AParagraphIndex > high(FRenderedParagraphs)) or
    (FRenderedParagraphs[AParagraphIndex] = nil) then exit;
  if ASubBrokenTotalCountBefore <> FRenderedParagraphs[AParagraphIndex].Count then
    FreeAndNil(FRenderedParagraphs[AParagraphIndex])
  else
  begin
    for i := 0 to ASubBrokenChangedCountBefore-1 do
      FRenderedParagraphs[AParagraphIndex].Delete(ASubBrokenStart);
    for i := 0 to ASubBrokenChangedCountAfter-1 do
      FRenderedParagraphs[AParagraphIndex].Insert(ASubBrokenStart, nil);
  end;
end;

procedure TForm1.DiscardRenderedBrokenLines;
var
  i: Integer;
begin
  for i := 0 to high(FRenderedParagraphs) do
    FreeAndNil(FRenderedParagraphs[i]);
end;

procedure TForm1.LayoutCompletelyChanged;
begin
  if Assigned(FTextLayout) then FTextLayout.InvalidateLayout;
  DiscardRenderedBrokenLines;
  BGRAVirtualScreen1.DiscardBitmap;
end;

procedure TForm1.LayoutParagraphDeleted(ASender: TObject;
  AParagraphIndex: integer);
var
  i: Integer;
begin
  if (AParagraphIndex >= 0) and (AParagraphIndex <= high(FRenderedParagraphs)) then
  begin
    FreeAndNil(FRenderedParagraphs[AParagraphIndex]);
    for i := AParagraphIndex to high(FRenderedParagraphs)-1 do
      FRenderedParagraphs[i] := FRenderedParagraphs[i+1];
    setlength(FRenderedParagraphs, length(FRenderedParagraphs)-1);
  end;
end;

procedure TForm1.LayoutParagraphMergedWithNext(ASender: TObject;
  AParagraphIndex: integer);
var
  i, insertIndex: Integer;
  renderedBrokenLine: TBGRABitmap;
begin
  insertIndex := FRenderedParagraphs[AParagraphIndex].Count;
  for i := FRenderedParagraphs[AParagraphIndex+1].Count-1 downto 0 do
  begin
    renderedBrokenLine := FRenderedParagraphs[AParagraphIndex+1].Items[i];
    FRenderedParagraphs[AParagraphIndex].Insert(insertIndex, renderedBrokenLine);
    FRenderedParagraphs[AParagraphIndex+1].Extract(renderedBrokenLine);
  end;
  LayoutParagraphDeleted(ASender, AParagraphIndex+1);
end;

procedure TForm1.LayoutParagraphSplit(ASender: TObject;
  AParagraphIndex: integer; ASubBrokenIndex, ACharIndex: integer);
var
  i, j: Integer;
  renderedBrokenLine: TBGRABitmap;
begin
  if (AParagraphIndex >= 0) and (AParagraphIndex <= high(FRenderedParagraphs)) then
  begin
    setlength(FRenderedParagraphs, length(FRenderedParagraphs)+1);
    for i := high(FRenderedParagraphs) downto AParagraphIndex+2 do
      FRenderedParagraphs[i] := FRenderedParagraphs[i-1];
    FRenderedParagraphs[AParagraphIndex+1] := TRenderedBrokenLineList.Create;
    for j := FRenderedParagraphs[AParagraphIndex].Count-1 downto ASubBrokenIndex+1 do
    begin
      renderedBrokenLine := FRenderedParagraphs[AParagraphIndex].Items[j];
      FRenderedParagraphs[AParagraphIndex+1].Insert(0, renderedBrokenLine);
      FRenderedParagraphs[AParagraphIndex].Extract(renderedBrokenLine);
    end;
  end;
end;

procedure TForm1.SetSelLastClick(AValue: integer);
begin
  if FSelFirstClick = -1 then
    FSelFirstClick := FSelStart;
  FSelLastClick:= AValue;
  UpdateSelectionFromFirstLastClick;
end;

procedure TForm1.SetSelLength(AValue: integer);
begin
  if FSelLength=AValue then Exit;
  FSelLength:=AValue;
  FSelFirstClick:=-1;
  FSelLastClick:=-1;
  ShowCaret;
end;

procedure TForm1.SetSelStart(AValue: integer);
begin
  if FSelStart=AValue then Exit;
  FSelStart:=AValue;
  if FSelStart + FSelLength > FTextLayout.CharCount then
    FSelLength := FTextLayout.CharCount - FSelStart;
  FSelFirstClick:=-1;
  FSelLastClick:=-1;
  ShowCaret;
end;

procedure TForm1.FlushUnicode;
begin
  if not FInUnicode then exit;
  FInUnicode := false;
  InsertText(UnicodeCharToUTF8(FUnicodeValue));
end;

procedure TForm1.UpdateCurrentParagraph;
var curAlign: TAlignment;
begin
  if not LayoutReady then exit;

  FCurFirstParagraph:= FTextLayout.GetParagraphAt(SelStart);
  FCurLastParagraph:= FTextLayout.GetParagraphAt(SelStart+SelLength);
  case FTextLayout.ParagraphAlignment[FCurFirstParagraph] of
  btaCenter: curAlign := taCenter;
  btaLeftJustify: curAlign := taLeftJustify;
  btaRightJustify: curAlign:= taRightJustify;
  btaOpposite: if FTextLayout.ParagraphRightToLeft[FCurFirstParagraph] then
                 curAlign:= taLeftJustify else curAlign:= taRightJustify;
  else
    if FTextLayout.ParagraphRightToLeft[FCurFirstParagraph] then
               curAlign:= taRightJustify else curAlign:= taLeftJustify;
  end;
  ToolButtonLeftAlign.Down := curAlign = taLeftJustify;
  ToolButtonCenterAlign.Down := curAlign = taCenter;
  ToolButtonRightAlign.Down := curAlign = taRightJustify;
end;

procedure TForm1.UpdateSelectionFromFirstLastClick;
begin
  if FSelLastClick < FSelFirstClick then
  begin
    FSelStart := FSelLastClick;
    FSelLength:= FSelFirstClick-FSelLastClick;
  end else
  begin
    FSelStart:= FSelFirstClick;
    FSelLength:= FSelLastClick-FSelFirstClick;
  end;
  ShowCaret;
end;

procedure TForm1.SetCurrentParagraphAlign(AAlign: TAlignment);
var
  i: Integer;
  newAlign: TBidiTextAlignment;
begin
  if LayoutReady and (FCurFirstParagraph <> -1) then
  begin
    for i := FCurFirstParagraph to FCurLastParagraph do
    begin
      case AALign of
        taLeftJustify: if FTextLayout.ParagraphRightToLeft[i] then
                         newAlign := btaOpposite
                         else newAlign := btaNatural;
        taRightJustify: if FTextLayout.ParagraphRightToLeft[i] then
                         newAlign := btaNatural
                         else newAlign := btaOpposite;
        else {taCenter:} newAlign := btaCenter;
      end;
      FTextLayout.ParagraphAlignment[i] := newAlign;
    end;

    BGRAVirtualScreen1.DiscardBitmap;
  end;
end;

procedure TForm1.DeleteSelection;
begin
  if SelLength <> 0 then
  begin
    FTextLayout.DeleteText(SelStart, SelLength);
    SelStart := SelStart + FTextLayout.IncludeNonSpacingChars(SelStart, 0);
    SelLength:= 0;
    ShowCaret;
  end;
end;

procedure TForm1.InsertText(AText: string);
var
  insertCount: Integer;
begin
  if not LayoutReady then exit;
  DeleteSelection;
  insertCount := FTextLayout.InsertText(AText, SelStart);
  SelStart := SelStart + insertCount;
  SelStart := SelStart + FTextLayout.IncludeNonSpacingChars(SelStart, 0);
  ShowCaret;
end;

procedure TForm1.ShowCaret;
begin
  FBlinkCaretState := true;
  FBlinkCaretTime:= Now;
  BGRAVirtualScreen1.DiscardBitmap;
  TimerBlinkCaret.Enabled := false;
  TimerBlinkCaret.Enabled := true;
end;

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  zoom, prevAvailWidth: single;
  caretColor, selectionColor: TBGRAPixel;
  newTime: TDateTime;
  oldTopLeft: TPointF;
  i: Integer;
  startBroken, endBroken, j, countBroken: LongInt;
  renderedBroken: TBGRABitmap;
  renderRect: TRect;
begin
  zoom := BGRAVirtualScreen1.BitmapScale * Screen.PixelsPerInch / 96;
  if FFontRenderer = nil then
  begin
    if CheckBox_FreeType.Checked then
    begin
      FFontRenderer := TBGRAFreeTypeFontRenderer.Create;
      FFontRenderer.FontName := 'Liberation Serif';
    end else
    begin
      FFontRenderer := TLCLFontRenderer.Create;
      FFontRenderer.FontName := {$IFDEF LINUX}'Liberation Serif'{$ELSE}'serif'{$ENDIF};
    End;
  end;
  if CheckBox_ClearType.Checked then
  begin
    //force ClearType to RGB if disabled on the system
    if fqFineClearType() = fqFineAntialiasing then
      FFontRenderer.FontQuality:= fqFineClearTypeRGB
    else
      FFontRenderer.FontQuality:= fqSystemClearType;
  end
  else
  begin
    if CheckBox_FreeType.Checked then
      FFontRenderer.FontQuality:= fqFineAntialiasing
    else
      FFontRenderer.FontQuality:= fqSystem;
  end;
  FFontRenderer.FontEmHeightF:= SpinEdit_FontSize.Value * zoom;

  if FTextLayout = nil then
  begin
    FTextLayout:= TBidiTextLayout.Create(FFontRenderer, FTestText);
    FTextLayout.ParagraphSpacingBelow:= 0.25;
    FTextLayout.ParagraphSpacingAbove:= 0.25;
    FTextLayout.OnParagraphDeleted:=@LayoutParagraphDeleted;
    FTextLayout.OnParagraphMergedWithNext:=@LayoutParagraphMergedWithNext;
    FTextLayout.OnParagraphSplit:=@LayoutParagraphSplit;
    FTextLayout.OnBrokenLinesChanged:=@LayoutBrokenLinesChanged;
  end else
    FTextLayout.FontRenderer := FFontRenderer;

  prevAvailWidth := FTextLayout.AvailableWidth;
  FTextLayout.AvailableWidth := Bitmap.Width - 8*zoom;
  FTextLayout.TopLeft := PointF(4*zoom,4*zoom);
  if prevAvailWidth <> FTextLayout.AvailableWidth then
    DiscardRenderedBrokenLines;
  FTextLayout.ComputeLayoutIfNeeded;

  oldTopLeft := FTextLayout.TopLeft;
  ScrollBar1.Min:= 0;
  ScrollBar1.Max:= round(FTextLayout.TotalTextHeight + 8*zoom);
  ScrollBar1.PageSize:= Bitmap.Height;
  ScrollBar1.LargeChange:= Bitmap.Height*2 div 3;
  ScrollBar1.SmallChange:= round(FTextLayout.LineHeight);
  if ScrollBar1.Position > max(0, ScrollBar1.Max - ScrollBar1.PageSize) then
    ScrollBar1.Position := max(0, ScrollBar1.Max - ScrollBar1.PageSize);

  caretColor := BGRA(0,0,255);
  selectionColor := BGRA(0,0,255,128);

  newTime := Now;
  if newTime > FBlinkCaretTime + (CaretBlinkTimeMs/1000/24/60/60) then
  begin
    FBlinkCaretTime:= newTime;
    FBlinkCaretState:= not FBlinkCaretState;
  end;

  FTextLayout.TopLeft := oldTopLeft + PointF(0, -ScrollBar1.Position);
  if FBlinkCaretState and (SelLength = 0) and BGRAVirtualScreen1.Focused then
    FTextLayout.DrawCaret(Bitmap, SelStart, BGRA(caretColor.red,caretColor.green,caretColor.blue,140), BGRA(caretColor.red,caretColor.green,caretColor.blue,100));

  for i := FTextLayout.ParagraphCount to high(FRenderedParagraphs) do
    FreeAndNil(FRenderedParagraphs[i]);
  setlength(FRenderedParagraphs, FTextLayout.ParagraphCount);

  for i := 0 to FTextLayout.ParagraphCount-1 do
  begin
    if FRenderedParagraphs[i] = nil then
      FRenderedParagraphs[i] := TRenderedBrokenLineList.Create;
    startBroken := FTextLayout.ParagraphStartBrokenLine[i];
    endBroken := FTextLayout.ParagraphEndBrokenLine[i];
    for j := startBroken to endBroken - 1 do
    begin
      if j - startBroken >= FRenderedParagraphs[i].Count then
        FRenderedParagraphs[i].Add(nil);
      if j - startBroken < FRenderedParagraphs[i].Count then
      begin
        renderRect := RectWithSize(0, round(oldTopLeft.y + FTextLayout.BrokenLineRectF[j].Top) - ScrollBar1.Position,
                                   Bitmap.Width, ceil(FTextLayout.BrokenLineRectF[j].Height));
        if renderRect.IntersectsWith(Bitmap.ClipRect) then
        begin
          if FRenderedParagraphs[i].Items[j - startBroken] = nil then
          begin
            renderedBroken := TBGRABitmap.Create(Bitmap.Width,
            ceil(FTextLayout.BrokenLineRectF[j].Height), BGRAVirtualScreen1.Color);
            FTextLayout.TopLeft := PointF(oldTopLeft.x, -FTextLayout.BrokenLineRectF[j].Top);
            FTextLayout.DrawBrokenLines(renderedBroken, j, j+1);
            FRenderedParagraphs[i].Items[j - startBroken] := renderedBroken;
          end;
          Bitmap.PutImage(renderRect.Left, renderRect.Top,
            FRenderedParagraphs[i].Items[j - startBroken], dmSet);
        end else
          FRenderedParagraphs[i].Items[j - startBroken] := nil;
      end;
    end;
    countBroken := endBroken - startBroken;
    while FRenderedParagraphs[i].Count > countBroken do
      FRenderedParagraphs[i].Delete(countBroken);
  end;
  FTextLayout.TopLeft := oldTopLeft + PointF(0, -ScrollBar1.Position);

  FTextLayout.DrawSelection(Bitmap, SelStart, SelStart+SelLength, selectionColor, BGRA(0,0,192),1);

  if FBlinkCaretState and (SelLength = 0) then
    FTextLayout.DrawCaret(Bitmap, SelStart, BGRA(caretColor.red,caretColor.green,caretColor.blue,140), BGRA(caretColor.red,caretColor.green,caretColor.blue,100));

  UpdateCurrentParagraph;
  FTextLayout.TopLeft := oldTopLeft;
  //let some time for events
  TimerBlinkCaret.Enabled := false;
  TimerBlinkCaret.Enabled := true;
end;

procedure TForm1.CheckBox_ClearTypeChange(Sender: TObject);
begin
  LayoutCompletelyChanged;
end;

procedure TForm1.CheckBox_FreeTypeChange(Sender: TObject);
begin
  LayoutCompletelyChanged;
  FreeAndNil(FFontRenderer);
end;

procedure TForm1.BGRAVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  index: Integer;
begin
  BGRAVirtualScreen1.SetFocus;
  if Button = mbLeft then
  begin
    index := FTextLayout.GetCharIndexAt(PointF(X, Y) * BGRAVirtualScreen1.BitmapScale
               + PointF(0,ScrollBar1.Position));
    FSelFirstClick:= index;
    FSelLastClick:= index;
    UpdateSelectionFromFirstLastClick;
  end;
end;

procedure TForm1.BGRAVirtualScreen1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  index: Integer;
begin
  if (FSelFirstClick <> -1) and (ssLeft in Shift) then
  begin
    index := FTextLayout.GetCharIndexAt(PointF(X,Y) * BGRAVirtualScreen1.BitmapScale
               + PointF(0,ScrollBar1.Position));
    FSelLastClick:= index;
    UpdateSelectionFromFirstLastClick;
  end;
end;

procedure TForm1.BGRAVirtualScreen1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  ScrollBar1.Position := ScrollBar1.Position - (WheelDelta * ScrollBar1.SmallChange div 120);
end;

initialization

  EasyLazFreeType.FontCollection := TFreeTypeFontCollection.Create;
  EasyLazFreeType.FontCollection.AddFolder(ExtractFilePath(Application.ExeName)
     {$IFDEF DARWIN} + '../../../' {$ENDIF});

finalization

  FreeAndNil(EasyLazFreeType.FontCollection);

end.

