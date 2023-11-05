unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRABitmap, BGRAAnimatedGif, LCLType, StdCtrls;

type

  { TAnimationPanel }

  TAnimationPanel = class(TPanel)
  private
    procedure SetImage(AValue: TBGRAAnimatedGif);
  protected
    FImage: TBGRAAnimatedGif;
    FFirstPaint, FAnimatePaint: boolean;
    procedure Paint; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EraseBackground(DC: HDC); override;
    procedure Animate;
    property Image: TBGRAAnimatedGif read FImage write SetImage;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    image: TBGRAAnimatedGif;
    filename, error: string;
    animationPanel: TAnimationPanel;
    procedure LoadImage(AFilename: string);
    procedure FillMemo;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TAnimationPanel }

constructor TAnimationPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FImage := nil;
  FFirstPaint:= true;
  FAnimatePaint:= false;
end;

procedure TAnimationPanel.EraseBackground(DC: HDC);
begin
  // keep background
end;

procedure TAnimationPanel.Animate;
begin
  FAnimatePaint := true;
  Invalidate;
end;

procedure TAnimationPanel.SetImage(AValue: TBGRAAnimatedGif);
begin
  if FImage=AValue then Exit;
  FImage:=AValue;
  FFirstPaint:= true;
  Invalidate;
end;

procedure TAnimationPanel.Paint;
begin
  inherited Paint;

  if FFirstPaint or not FAnimatePaint then
  begin
    Canvas.Brush.Assign(Brush);
    Canvas.FillRect(ClientRect);
    if Assigned(FImage) and (FImage.Count > 0) then
      FImage.Show(Canvas, ClientRect);
  end else
  begin
    if Assigned(FImage) and (FImage.Count > 0) then
      FImage.Update(Canvas, ClientRect);
  end;
  FAnimatePaint := false;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  image := TBGRAAnimatedGif.Create;
  animationPanel := TAnimationPanel.Create(self);
  animationPanel.Left := 4;
  animationPanel.Top := 4;
  animationPanel.Color:= self.Color;
  animationPanel.Image := image;
  InsertControl(animationPanel, 0);
  LoadImage(ConcatPaths([ExtractFilePath(Application.ExeName), 'waterdrops.gif']));
  FillMemo;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  animationPanel.Refresh;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  try
    if SaveDialog1.Execute then
      image.SaveToFile(SaveDialog1.FileName);
  except
    on ex: exception do
    begin
      error := ex.Message;
      FillMemo;
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  image.Free;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  LoadImage(FileNames[0]);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if Visible then
  begin
    animationPanel.Animate;
    Label1.Caption := 'Frame: ' + inttostr(image.CurrentImage);
  end;
end;

procedure TForm1.LoadImage(AFilename: string);
begin
  Timer1.Enabled := false;
  filename := AFilename;
  try
    if not FileExists(filename) then
    begin
      image.Clear;
      error := 'Not found';
    end else
    begin
      image.LoadFromFile(filename);
      error := 'Ok';
    end;
  except
    on ex:Exception do
      error := ex.Message;
  end;
  animationPanel.Width := image.Width;
  animationPanel.Height := image.Height;
  FillMemo;
  Timer1.Enabled := true;
end;

procedure TForm1.FillMemo;
var s: string;
  i: Integer;
begin
  s := error + LineEnding +
       ExtractFileName(filename) + LineEnding +
       inttostr(image.Width)+'x'+inttostr(image.height) + LineEnding +
       inttostr(image.Count)+' frames' + LineEnding;
  if image.LoopCount = 0 then
    s += 'infinite loop' + LineEnding
  else
    s += inttostr(image.LoopCount)+' loops' + LineEnding;
  for i := 0 to image.Count-1 do
    s += '#' + inttostr(i)+': '+inttostr(image.FrameDelayMs[i])+' ms, '+
         inttostr(image.FrameImage[i].Width)+'x'+inttostr(image.FrameImage[i].Height) + LineEnding;
  Memo1.Text := s;
end;

end.

