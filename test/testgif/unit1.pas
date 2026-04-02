unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRABitmap, BGRAAnimatedGif, LCLType, StdCtrls;

type

  { TGifAnimationPanel }

  TGifAnimationPanel = class(TPanel)
  private
    FOnRenderFrame: TNotifyEvent;
    procedure SetImage(AValue: TBGRAAnimatedGif);
    procedure SetOnRenderFrame(AValue: TNotifyEvent);
  protected
    FImage: TBGRAAnimatedGif;
    FFirstPaint, FAnimatePaint: boolean;
    FTimer: TTimer;
    procedure Paint; override;
    procedure EraseBackground(DC: HDC); override;
    procedure ImageChanged(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Image: TBGRAAnimatedGif read FImage;
    property OnRenderFrame: TNotifyEvent read FOnRenderFrame write SetOnRenderFrame;
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
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
  protected
    { private declarations }
    procedure GifAnimationPanelRenderFrame(Sender: TObject);
  public
    { public declarations }
    filename, error: string;
    animationPanel: TGifAnimationPanel;
    procedure LoadImage(AFilename: string);
    procedure FillMemo;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TGifAnimationPanel }

constructor TGifAnimationPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FFirstPaint:= true;
  FAnimatePaint:= false;
  BevelInner:= bvNone;
  BevelOuter:= bvNone;
  FImage := TBGRAAnimatedGif.Create;
  FImage.OnChange := @ImageChanged;
  FTimer := TTimer.Create(self);
  FTimer.Enabled := false;
  FTimer.OnTimer := @TimerTimer;
end;

destructor TGifAnimationPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TGifAnimationPanel.SetImage(AValue: TBGRAAnimatedGif);
begin
  FImage.Free;
end;

procedure TGifAnimationPanel.SetOnRenderFrame(AValue: TNotifyEvent);
begin
  if FOnRenderFrame=AValue then Exit;
  FOnRenderFrame:=AValue;
end;

procedure TGifAnimationPanel.Paint;
var
  waitTime: Integer;
begin
  inherited Paint;

  if FFirstPaint or not FAnimatePaint then
  begin
    Canvas.Brush.Assign(Brush);
    Canvas.FillRect(ClientRect);
    image.BackgroundMode := gbmEraseBackground;
    image.EraseColor:= Color;
    if Assigned(FImage) and (FImage.Count > 0) then
      FImage.Show(Canvas, ClientRect);
  end else
  begin
    if Assigned(FImage) and (FImage.Count > 0) then
      FImage.Update(Canvas, ClientRect);
  end;
  FAnimatePaint := false;
  FFirstPaint := false;
  if Assigned(FOnRenderFrame) then
    FOnRenderFrame(self);
  if Assigned(FImage) and (FImage.Count > 1) then
  begin
    waitTime := Image.TimeUntilNextImageMs;
    if waitTime < 15 then waitTime := 15;
    FTimer.Interval := waitTime;
    FTimer.Enabled := true;
  end;
end;

procedure TGifAnimationPanel.EraseBackground(DC: HDC);
begin
  // don't erase background
end;

procedure TGifAnimationPanel.ImageChanged(Sender: TObject);
begin
  FTimer.Enabled:= false;
  Invalidate;
end;

procedure TGifAnimationPanel.TimerTimer(Sender: TObject);
begin
  FTimer.Enabled := false;
  FAnimatePaint := true;
  Refresh;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  animationPanel := TGifAnimationPanel.Create(self);
  animationPanel.OnRenderFrame := @GifAnimationPanelRenderFrame;
  animationPanel.Left := 4;
  animationPanel.Top := 4;
  animationPanel.Color:= self.Color;
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
      animationPanel.Image.SaveToFile(SaveDialog1.FileName);
  except
    on ex: exception do
    begin
      error := ex.Message;
      FillMemo;
    end;
  end;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  LoadImage(FileNames[0]);
end;

procedure TForm1.GifAnimationPanelRenderFrame(Sender: TObject);
begin
  Label1.Caption := 'Frame: ' + inttostr(animationPanel.image.CurrentImage) +
                    ', Wait: ' + inttostr(animationPanel.Image.TimeUntilNextImageMs);
end;

procedure TForm1.LoadImage(AFilename: string);
begin
  filename := AFilename;
  try
    if not FileExists(filename) then
    begin
      animationPanel.Image.Clear;
      error := 'Not found';
    end else
    begin
      animationPanel.Image.LoadFromFile(filename);
      error := 'Ok';
    end;
  except
    on ex:Exception do
      error := ex.Message;
  end;
  animationPanel.Width := animationPanel.Image.Width;
  animationPanel.Height := animationPanel.Image.Height;
  FillMemo;
end;

procedure TForm1.FillMemo;
var s: string;
  i: Integer;
begin
  with animationPanel do
  begin
    s := error + LineEnding +
         ExtractFileName(filename) + LineEnding +
         inttostr(image.Width)+'x'+inttostr(image.height) + LineEnding +
         inttostr(image.Count)+' frames' + LineEnding;
    if image.LoopCount = 0 then
      s := s + 'infinite loop' + LineEnding
    else
      s := s + inttostr(image.LoopCount)+' loops' + LineEnding;
    for i := 0 to image.Count-1 do
      s := s + '#' + inttostr(i)+': '+inttostr(image.FrameDelayMs[i])+' ms, '+
           inttostr(image.FrameImage[i].Width)+'x'+inttostr(image.FrameImage[i].Height) + LineEnding;
  end;
  Memo1.Text := s;
end;

end.

