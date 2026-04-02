unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, FileCtrl, Menus, ComCtrls, Types,

  BGRABitmapTypes, BGRABitmap, BGRASVG, BGRASVGType, BGRASVGShapes, BGRAUnits,

  UProfiler;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox_ReplaceStrokeAndFill: TCheckBox;
    FileListBox1: TFileListBox;
    Image1: TImage;
    Image2: TImage;
    MainMenu1: TMainMenu;
    MCopy1: TMenuItem;
    MCut1: TMenuItem;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    N1: TMenuItem;
    MPaste1: TMenuItem;
    MSelectAll: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanProf: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    TabSheet1: TTabSheet;
    procedure CheckBox_ReplaceStrokeAndFillChange(Sender: TObject);
    procedure FileListBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MCopy1Click(Sender: TObject);
    procedure MCut1Click(Sender: TObject);
    procedure Memo1DblClick(Sender: TObject);
    procedure Memo1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure MPaste1Click(Sender: TObject);
    procedure MSelectAllClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    svg: TBGRASVG;
    last_image: TImage;
    prof: TProfiler;

    procedure ChangeFill(AElement: TSVGElement; AData: pointer;
      var ARecursive: boolean);
    procedure ReloadFile;
    procedure Test(var ms: TMemoryStream; kzoom: Single = 1; AFillCode: boolean = false);
    procedure Test(path: String; kzoom: Single = 1; AFillCode: boolean = false);

  public

  end;

var
  Form1: TForm1;

implementation

uses
 Clipbrd, BGRAVectorize;

{$R *.lfm}

const
 s_file_overwrite = 'Existing file: do you want to overwrite it?';

procedure TForm1.Test(var ms: TMemoryStream; kzoom: Single = 1; AFillCode: boolean = false);
Var
 v: Double;
 s: String;
 bmp: TBGRABitmap;
begin
 bmp:= TBGRABitmap.Create;
 try
  if Assigned(svg) then
   svg.Free;

  prof.BeginMeasure;
  svg:= TBGRASVG.Create(ms);
  v:= prof.EndMeasure;
  s:= prof.FormatTime('create: ',v);

  prof.BeginMeasure;
  svg.ContainerWidthAsPixel := 480;
  svg.ContainerHeightAsPixel := 360;
  bmp.FontRenderer := TBGRAVectorizedFontRenderer.Create;
  bmp.SetSize(Round(svg.WidthAsPixel*kzoom),Round(svg.HeightAsPixel*kzoom));
  bmp.Fill(BGRAWhite);
  v:= prof.EndMeasure;
  s:= s + prof.FormatTime(' | bmp: ',v);

  prof.BeginMeasure;
  if CheckBox_ReplaceStrokeAndFill.Checked then
     svg.IterateElements(@ChangeFill, nil, true);
  svg.StretchDraw(bmp.Canvas2D, 0,0,bmp.Width,bmp.Height);
  v:= prof.EndMeasure;
  s:= s + prof.FormatTime(' | draw: ',v);

  Image1.Picture.Bitmap.Assign(bmp);
  PanProf.Caption:= s;

  if AFillCode then
    Memo1.Text:= svg.AsUTF8String;
 finally
  bmp.Free;
 end;
end;

procedure TForm1.ChangeFill(AElement: TSVGElement; AData: pointer;
  var ARecursive: boolean);
begin
  if AElement is TSVGDefine then
    ARecursive:= false
  else
  begin
   if not AElement.isFillNone then AElement.fillColor := CSSSkyBlue;
   if not AElement.isStrokeNone then AElement.strokeColor := CSSOrange;
  end;
end;

procedure TForm1.ReloadFile;
Var
 png_find: Boolean;
 path: String;
begin
 if FileListBox1.ItemIndex <> -1 then
 begin
  path:= FileListBox1.FileName;
  Test(path, Screen.PixelsPerInch / 96 * GetCanvasScaleFactor, true);
  //(view correct result)
  path:= ChangeFileExt(path,'.png');
  png_find:= False;
  if FileExists(path) then
   try
    with Image2.Picture do
    begin
     LoadFromFile(path);
     Image2.Hint:= '(anteprime: png '+
                   IntToStr(Width)+'x'+IntToStr(Height)+')';
    end;
    png_find:= True;
   except
   end;
  if not png_find then
  begin
   Image2.Hint:= '';
   Image2.Picture:= nil;
  end;
 end;
end;

procedure TForm1.Test(path: String; kzoom: Single = 1;  AFillCode: boolean = false);
Var
 ms: TMemoryStream;
begin
 ms:= TMemoryStream.Create;
 try
  ms.LoadFromFile(path);
  ms.Position:= 0;

  path:= ExtractFileName(path);
  Test(ms, kzoom, AFillCode);
 finally
  ms.Free;
 end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var baseDir: string;
begin
 svg:= nil;
 last_image:= nil;

 prof:= TProfiler.Create;

 with FileListBox1 do
 begin
  baseDir := ExtractFilePath(Application.ExeName);
  {$IFDEF DARWIN}
  if not DirectoryExists(baseDir+'svg') then
    baseDir := baseDir + '../../../';
  {$ENDIF}
  Directory := baseDir+'svg'+PathDelim;
  Mask:= '*.svg';
 end;

 PanProf.DoubleBuffered:= True;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
 if FileListBox1.Count <> 0 then
 begin
  FileListBox1.ItemIndex:= 0;
  FileListBox1.Click;
 end;
 PageControl1.ActivePageIndex:= 0;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 svg.Free;
 prof.Free;
end;

procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Sender is TImage then
 begin
  last_image:= Sender as TImage;
  if (Button = mbRight) and (last_image.Picture <> nil) then
   PopupMenu2.PopUp(Mouse.CursorPos.x,Mouse.CursorPos.y);
 end;
end;

procedure TForm1.MCut1Click(Sender: TObject);
begin
 if Memo1.SelLength <> 0 then
  Memo1.CutToClipboard;
end;

procedure TForm1.MCopy1Click(Sender: TObject);
begin
 if Memo1.SelLength <> 0 then
  Memo1.CopyToClipboard;
end;

procedure TForm1.MPaste1Click(Sender: TObject);
begin
 Memo1.PasteFromClipboard;
end;

procedure TForm1.MSelectAllClick(Sender: TObject);
begin
 Memo1.SelectAll;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
Var
 b: Boolean;
begin
 b:= Memo1.SelLength <> 0;

 MCut1.Enabled:= b;
 MCopy1.Enabled:= b;
 MPaste1.Enabled:= Clipboard.HasFormat(CF_Text);
 MSelectAll.Enabled:= Length(Memo1.Lines.Text) <> 0;
end;

procedure TForm1.Memo1DblClick(Sender: TObject);
begin
 Memo1.ReadOnly:= not Memo1.ReadOnly;
end;

procedure TForm1.Memo1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if (Button = mbRight) and (not Memo1.ReadOnly) then
  PopupMenu1.PopUp(Mouse.CursorPos.x,Mouse.CursorPos.y);
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
Var
 s: String;
 scale: single;
 ms: TMemoryStream;
begin
 if (Sender is TMenuItem) then
  scale:= (Sender as TMenuItem).Tag / 100
 else
  scale:= 1;

 ms:= TMemoryStream.Create;
 try
  s:= Memo1.Text;
  ms.WriteBuffer(s[1],Length(s));
  ms.Position:= 0;
  Test(ms,scale);
 finally
  ms.Free;
 end;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
Var
 path: String;
begin
 SaveDialog1.Filter:= 'png|*.png';
 if SaveDialog1.Execute then
  try
   path:= SaveDialog1.FileName;
   if FileExists(path) then
    if MessageDlg('', s_file_overwrite, mtConfirmation,
       [mbYes, mbNo, mbIgnore],0) <> mrYes then
     Exit;
   last_image.Picture.SaveToFile(path);
  except
  end;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
Var
 path: String;
begin
 SaveDialog1.Filter:= 'svg|*.svg';
 if SaveDialog1.Execute then
  try
   path:= SaveDialog1.FileName;
   if FileExists(path) then
    if MessageDlg('', s_file_overwrite, mtConfirmation,
       [mbYes, mbNo, mbIgnore],0) <> mrYes then
     Exit;
   Memo1.Lines.SaveToFile(path);
  except
  end;
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
 MenuItem1.Click;
end;

procedure TForm1.MenuItem9Click(Sender: TObject);
begin
 Close;
end;

procedure TForm1.FileListBox1Change(Sender: TObject);
begin
  ReloadFile;
end;

procedure TForm1.CheckBox_ReplaceStrokeAndFillChange(Sender: TObject);
begin
  ReloadFile;
end;

end.

