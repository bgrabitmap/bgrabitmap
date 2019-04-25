unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, FileCtrl, Menus, ComCtrls,

  BGRABitmapTypes, BGRABitmap, BGRASVG, BGRAUnits, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    FileListBox1: TFileListBox;
    Image1: TImage;
    Image2: TImage;
    MCopy1: TMenuItem;
    MCut1: TMenuItem;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MPaste1: TMenuItem;
    MSelectAll: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
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
    procedure MPaste1Click(Sender: TObject);
    procedure MSelectAllClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    svg,
    svg_sample: TBGRASVG;
    sample_id: Integer;
    last_image: TImage;

    procedure Test(var ms: TMemoryStream; kzoom: Single = 1);
    procedure Test(path: String);

    procedure UpdateInternalState;
    function DialogGetID(title: String; const can_nil: Boolean = False): Integer;

  public

  end;

var
  Form1: TForm1;

implementation

uses
 Clipbrd;

 {$R *.lfm}

(*
var
 c: TBGRAPixel;
begin
 c:= ColorToBGRA(clBtnFace);
*)

const
 s_file_overwrite = 'Existing file: do you want to overwrite it?';
 s_operation_ok = 'Operation completed!';
 s_element = 'Element';
 s_parent = 'Parent';

procedure TForm1.Test(var ms: TMemoryStream; kzoom: Single = 1);
Var
 bmp: TBGRABitmap;
begin
 bmp:= TBGRABitmap.Create;
 try
  if Assigned(svg) then
   svg.Free;
  svg:= TBGRASVG.Create(ms);

  bmp.SetSize(Round(svg.WidthAsPixel*kzoom),Round(svg.HeightAsPixel*kzoom));
  bmp.Fill(BGRAWhite);
  svg.StretchDraw(bmp.Canvas2D, 0,0,bmp.Width,bmp.Height);
  //svg.Draw(bmp.Canvas2D, 0,0, cuPixel);
  Image1.Picture.Bitmap.Assign(bmp);

  UpdateInternalState;
 finally
  bmp.Free;
 end;
end;

procedure TForm1.Test(path: String);
Var
 ms: TMemoryStream;
begin
 ms:= TMemoryStream.Create;
 try
  ms.LoadFromFile(path);
  ms.Position:= 0;

  path:= ExtractFileName(path);
  {if (path = 'test_4.svg') or
     (path = 'test_pow.svg') or
     (path = 'test_wand.svg') or
     (path = 'test_scarecrow.svg') or
     (path = 'test_shadow.svg') or
     (path = 'test_spartan.svg') then
   Test(ms, 0.5)
  else if (path = 'test_.svg') or
          (path = 'v1.svg') or
          (path = 'v2.svg') or
          (path = 'v3.svg') or
          (path = 'v4.svg') or
          (path = 'v5.svg') or
          (path = 'v6.svg') or
          (path = 'v7.svg') then
   Test(ms, 10)
  else    }
   Test(ms);
 finally
  ms.Free;
 end;
end;

procedure TForm1.UpdateInternalState;
begin
 if Assigned(svg) then
  with svg.DataLink.GetInternalState do
  begin
   Memo3.Clear;
   Memo3.Lines.Add( 'Element: '+IntToStr(svg.DataLink.ElementCount) );
   Memo3.Lines.Add( 'RootElement: '+IntToStr(svg.DataLink.RootElementCount) );
   Memo3.Lines.Add( '--------------------' );
   Memo3.Lines.Add( Text );
   Free;
  end;
end;

function TForm1.DialogGetID(title: String; const can_nil: Boolean = False): Integer;
Var
 nc,id: Integer;
 s,s_min: String;
begin
 Result:= -2;
 if Assigned(svg) then
 begin
  nc:= svg.DataLink.ElementCount;
  if (nc = 0) and (not can_nil) then
  begin
   ShowMessage('Elements list is empty!');
   Exit;
  end;
  s:= Trim( InputBox('ID Request',title,'') );
  if s = '' then
   Exit;
  try
   id:= StrToInt(s);
  except
   ShowMessage('Input invalid!');
   Exit;
  end;
  if ((id >= 0) and (id < nc)) or
     ((id = -1) and can_nil) then
   Result:= id
  else
  begin
   if can_nil then
    s_min:= '[-1(nil)..'
   else
    s_min:= '[0..';
   ShowMessage('Range invalid! '+s_min+IntToStr(nc-1)+']');
  end;
 end;
end;

procedure TForm1.FormCreate(Sender: TObject);
Var
 sl: TStringList;
 ms: TMemoryStream;
begin
 svg:= nil;
 last_image:= nil;

 sl:= TStringList.Create;
 ms:= TMemoryStream.Create;
 try
  with sl do
  begin
   Add('<svg height="100" width="100">');
   Add('<g id="sample" gstroke="black" stroke-width="3">');
   Add(' <circle id = "sample" cx="50" cy="50" r="40" fill="red" />');
   Add('</g>');
   Add('</svg>');
  end;
  sl.SaveToStream(ms);
  ms.Position:= 0;
  svg_sample:= TBGRASVG.Create(ms);
  sample_id:= 1;//(for this example: 0 circle; 1 group)
 finally
  ms.Free;
  sl.Free;
 end;

 with FileListBox1 do
 begin
  Directory:= 'svg'+PathDelim;
  Mask:= '*.svg';
 end;
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
 if Assigned(svg) then
  svg.Free;
 svg_sample.Free;
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

procedure TForm1.FileListBox1Change(Sender: TObject);
Var
 png_find: Boolean;
 path: String;
begin
 if FileListBox1.ItemIndex <> -1 then
 begin
  path:= FileListBox1.FileName;
  Test(path);
  //(svg source)
  try
   Memo1.Lines.LoadFromFile(path);
   Memo2.Text:= Memo1.Text;
  except
   Memo1.Clear;
  end;
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

procedure TForm1.Button4Click(Sender: TObject);
begin
 UpdateInternalState;
end;

procedure TForm1.Button1Click(Sender: TObject);
Var
 ide,idp: Integer;
begin
 ide:= DialogGetID(s_element+' (sample = -1)',True);
 if ide = -2 then
  Exit;
 idp:= DialogGetID(s_parent+' (nil = -1)',True);

 if ide = -1 then
  //"clear" link to svg_sample for test with a indipendent element
  with svg_sample.DataLink.Elements[sample_id] do
  begin
   DataParent:= nil;
   DataChildList.Clear;
  end;

 if (ide <> -2) and (idp <> -2) then
  with svg.DataLink do
  begin
   if (ide = -1) and (idp = -1) then
    Link(svg_sample.DataLink.Elements[sample_id],nil)
   else if ide = -1 then
    Link(svg_sample.DataLink.Elements[sample_id],Elements[idp])
   else if idp = -1 then
    Link(Elements[ide],nil)
   else
    Link(Elements[ide],Elements[idp]);
   ShowMessage(s_operation_ok);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
Var
 id: Integer;
begin
 id:= DialogGetID(s_element);
 if id <> -2 then
  with svg.DataLink do
  begin
   UnLink(Elements[id]);
   ShowMessage(s_operation_ok);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
Var
 ide,idp: Integer;
begin
 ide:= DialogGetID(s_element);
 if ide = -2 then
  Exit;
 idp:= DialogGetID(s_parent+' (nil = -1)',True);
 if (ide <> -2) and (idp <> -2) then
  with svg.DataLink do
  begin
   if idp = -1 then
    ReLink(Elements[ide],nil)
   else
    ReLink(Elements[ide],Elements[idp]);
   ShowMessage(s_operation_ok);
  end;
end;

end.

