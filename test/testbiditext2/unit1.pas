unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, ComCtrls, BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes,
  BGRATextBidi;

const
  CaretBlinkTimeMs = 500;

type
  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    SpinEdit_Angle: TSpinEdit;
    SpinEdit_FontSize: TSpinEdit;
    TimerBlinkCaret: TTimer;
    ToolBar1: TToolBar;
    ToolButtonLeftAlign: TToolButton;
    ToolButtonCenterAlign: TToolButton;
    ToolButtonRightAlign: TToolButton;
    procedure BGRAVirtualScreen1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure SpinEdit_AngleChange(Sender: TObject);
    procedure SpinEdit_FontSizeChange(Sender: TObject);
    procedure TimerBlinkCaretTimer(Sender: TObject);
    procedure ToolButtonCenterAlignClick(Sender: TObject);
    procedure ToolButtonLeftAlignClick(Sender: TObject);
    procedure ToolButtonRightAlignClick(Sender: TObject);
  private
    function GetSelLastClick: integer;
    procedure SetSelLastClick(AValue: integer);
    procedure SetSelLength(AValue: integer);
    procedure SetSelStart(AValue: integer);
  public
    FFontRenderer: TBGRACustomFontRenderer;
    FTextLayout: TBidiTextLayout;
    FBlinkCaretTime: TDateTime;
    FBlinkCaretState: boolean;
    FSelStart,FSelLength: integer;
    FSelFirstClick,FSelLastClick: integer;
    FCurFirstParagraph,FCurLastParagraph: integer;
    FTestText: string;
    procedure UpdateCurrentParagraph;
    procedure UpdateSelectionFromFirstLastClick;
    procedure SetCurrentParagraphAlign(AAlign: TAlignment);
    procedure DeleteSelection;
    procedure InsertText(AText: string);
    procedure ShowCaret;
    property SelStart: integer read FSelStart write SetSelStart;
    property SelLength: integer read FSelLength write SetSelLength;
    property SelLastClick: integer read GetSelLastClick write SetSelLastClick;
  end;

var
  Form1: TForm1;

implementation

uses BGRAText, LCLType, BGRAUTF8, Clipbrd, LCLIntf;

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

  FTestText := 'تحتوي العربية على 28 حرفاً مكتوباً. ويرى بعض اللغويين أنه يجب إضافة حرف الهمزة إلى حروف العربية، ليصبح عدد الحروف 29. تُكتب العربية من اليمين إلى اليسار - ومثلها اللغة الفارسية والعبرية على عكس كثير من اللغات العالمية - ومن أعلى الصفحة إلى أسفلها.'+LineEnding+
             #9'The French language (French: français, pronounced "Fronce-eh") is a Romance language that was first spoken in France.'+LineEnding+
             'Glorious is reversed as '+ UTF8OverrideDirection('"glorious"',True) + '. ' +
               '"Hello!" is '+ UTF8EmbedDirection('"مرحبا!"',True) + ' in arabic.' + LineEnding +
             '对于汉语的分支语言，学界主要有两种观点，一种观点将汉语定义为语言，并将官话、贛語、闽语、粤语、客家语、吴语、湘语七大语言定义为一级方言.'+LineEnding+
             'עִבְרִית היא שפה שמית, ממשפחת השפות האפרו-אסיאתיות, הידועה כשפתם של היהודים ושל השומרונים, אשר ניב מודרני שלה (עברית ישראלית) משמש כשפה הרשמית והעיקרית של מדינת ישראל.';

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FTextLayout.Free;
  FFontRenderer.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  idxPara: Integer;
begin
  if FTextLayout = nil then exit;

  if KEY = VK_DELETE then
  begin
    if SelLength > 0 then DeleteSelection
    else
    begin
      FTextLayout.DeleteText(SelStart, 1);
      ShowCaret;
    end;
    Key := 0;
  end else
  if (Key = VK_LEFT) or (Key = VK_RIGHT) then
  begin
    if (Key = VK_LEFT) xor FTextLayout.ParagraphRightToLeft[FTextLayout.GetParagraphAt(SelStart)] then
    begin
      if ssShift in Shift then
      begin
        if SelLastClick > 0 then
          SelLastClick := SelLastClick - FTextLayout.IncludeNonSpacingCharsBefore(SelStart,1);
      end else
      begin
        if SelLastClick > 0 then
          SelStart := SelLastClick - FTextLayout.IncludeNonSpacingCharsBefore(SelStart,1);
        SelLength:= 0;
      end;
    end else
    begin
      if ssShift in Shift then
      begin
        if SelLastClick < FTextLayout.CharCount then
          SelLastClick := SelLastClick + FTextLayout.IncludeNonSpacingChars(SelStart,1);
      end else
      begin
        if SelLastClick < FTextLayout.CharCount then
          SelStart := SelLastClick + FTextLayout.IncludeNonSpacingChars(SelStart,1);
        SelLength:= 0;
      end;
    end;
    Key := 0;
  end else
  if Key = VK_HOME then
  begin
    idxPara := FTextLayout.GetParagraphAt(SelStart);

    if ssShift in Shift then
    begin
      SelLastClick := FTextLayout.ParagraphStartIndex[idxPara];
    end else
    begin
      SelStart := FTextLayout.ParagraphStartIndex[idxPara];
      SelLength:= 0;
    end;
    Key := 0;
  end else
  if Key = VK_END then
  begin
    idxPara := FTextLayout.GetParagraphAt(SelStart+SelLength);

    if ssShift in Shift then
    begin
      SelLastClick := FTextLayout.ParagraphEndIndexBeforeParagraphSeparator[idxPara];
    end else
    begin
      SelStart := FTextLayout.ParagraphEndIndexBeforeParagraphSeparator[idxPara];
      SelLength:= 0;
    end;
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
  If (Key = VK_C) and (ssCtrl in Shift) then
  begin
    if SelLength> 0 then
      SetClipboardAsText(FTextLayout.CopyText(SelStart, SelLength));
    Key := 0;
  end else
  If (Key = VK_X) and (ssCtrl in Shift) then
  begin
    if SelLength > 0 then
    begin
      SetClipboardAsText(FTextLayout.CopyText(SelStart, SelLength));
      DeleteSelection;
    end;
    Key := 0;
  end else
  If (Key = VK_V) and (ssCtrl in Shift) then
  begin
    InsertText(Clipboard.AsText);
    Key := 0;
  end else
  If (Key = VK_A) and (ssCtrl in Shift) then
  begin
    SelStart:= 0;
    SelLength:= FTextLayout.CharCount;
    Key := 0;
  end;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
var
  delCount: Integer;
begin
  if FTextLayout = nil then exit;

  if Key = #8 then
  begin
    if SelLength > 0 then DeleteSelection
    else
    begin
      if SelStart > 0 then
      begin
        delCount := FTextLayout.DeleteTextBefore(SelStart, 1);
        SelStart := SelStart - delCount;
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

procedure TForm1.SpinEdit_AngleChange(Sender: TObject);
begin
  if Assigned(FTextLayout) then FTextLayout.InvalidateLayout;
  BGRAVirtualScreen1.DiscardBitmap;
end;

procedure TForm1.SpinEdit_FontSizeChange(Sender: TObject);
begin
  if Assigned(FTextLayout) then FTextLayout.InvalidateLayout;
  BGRAVirtualScreen1.DiscardBitmap;
end;

procedure TForm1.TimerBlinkCaretTimer(Sender: TObject);
begin
  TimerBlinkCaret.Enabled := false;
  BGRAVirtualScreen1.DiscardBitmap;
  TimerBlinkCaret.Enabled := true;
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

function TForm1.GetSelLastClick: integer;
begin
  if FSelLastClick = -1 then
    result := FSelStart + FSelLength
  else
    result := FSelLastClick;
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

procedure TForm1.UpdateCurrentParagraph;
var curAlign: TAlignment;
begin
  if FTextLayout = nil then exit;

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
begin
  if (FTextLayout <> nil) and (FCurFirstParagraph <> -1) then
  begin
    for i := FCurFirstParagraph to FCurLastParagraph do
      case AALign of
        taCenter: FTextLayout.ParagraphAlignment[i] := btaCenter;
        taLeftJustify: if FTextLayout.ParagraphRightToLeft[i] then
                         FTextLayout.ParagraphAlignment[i] := btaOpposite
                         else FTextLayout.ParagraphAlignment[i] := btaNatural;
        taRightJustify: if FTextLayout.ParagraphRightToLeft[i] then
                         FTextLayout.ParagraphAlignment[i] := btaNatural
                         else FTextLayout.ParagraphAlignment[i] := btaOpposite;
      end;

    BGRAVirtualScreen1.DiscardBitmap;
  end;
end;

procedure TForm1.DeleteSelection;
begin
  if SelLength <> 0 then
  begin
    FTextLayout.DeleteText(SelStart, SelLength);
    SelLength:= 0;
    ShowCaret;
  end;
end;

procedure TForm1.InsertText(AText: string);
var
  insertCount: Integer;
begin
  if FTextLayout = nil then exit;
  DeleteSelection;
  insertCount := FTextLayout.InsertText(AText, SelStart);
  SelStart := SelStart + insertCount;
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
  caretColor, selectionColor: TBGRAPixel;
  newTime: TDateTime;
begin
  if FFontRenderer = nil then
  begin
    FFontRenderer := TLCLFontRenderer.Create;
    FFontRenderer.FontName := 'default';
    FFontRenderer.FontQuality:= fqSystemClearType;
  end;
  FFontRenderer.FontEmHeight:= SpinEdit_FontSize.Value;
  FFontRenderer.FontOrientation := SpinEdit_Angle.Value*10;

  if FTextLayout = nil then
  begin
    FTextLayout:= TBidiTextLayout.Create(FFontRenderer, FTestText);
    FTextLayout.ParagraphSpacingBelow:= 0.25;
    FTextLayout.ParagraphSpacingAbove:= 0.25;
  end;
  FTextLayout.SetLayout(rectF(4,4,Bitmap.Width-4,Bitmap.Height-4));

  caretColor := BGRA(0,0,255);
  selectionColor := BGRA(0,0,255,128);

  newTime := Now;
  if newTime > FBlinkCaretTime + (CaretBlinkTimeMs/1000/24/60/60) then
  begin
    FBlinkCaretTime:= newTime;
    FBlinkCaretState:= not FBlinkCaretState;
  end;

  if FBlinkCaretState and (SelLength = 0) then
    FTextLayout.DrawCaret(Bitmap, SelStart, BGRA(caretColor.red,caretColor.green,caretColor.blue,140), BGRA(caretColor.red,caretColor.green,caretColor.blue,100));

  FTextLayout.DrawSelection(Bitmap, SelStart, SelStart+SelLength, selectionColor);

  FTextLayout.DrawText(Bitmap);

  if FBlinkCaretState and (SelLength = 0) then
    FTextLayout.DrawCaret(Bitmap, SelStart, BGRA(caretColor.red,caretColor.green,caretColor.blue,140), BGRA(caretColor.red,caretColor.green,caretColor.blue,100));

  UpdateCurrentParagraph;
end;

procedure TForm1.BGRAVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  index: Integer;
begin
  if Button = mbLeft then
  begin
    index := FTextLayout.GetCharIndexAt(PointF(X,Y));
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
    index := FTextLayout.GetCharIndexAt(PointF(X,Y));
    FSelLastClick:= index;
    UpdateSelectionFromFirstLastClick;
  end;
end;

end.

