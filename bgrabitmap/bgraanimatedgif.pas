// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Classes to read and write animated GIF and animated PNG files. }
unit BGRAAnimatedGif;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

uses
  BGRAClasses, SysUtils, BGRAGraphics, FPImage, BGRABitmap, BGRABitmapTypes,
  BGRAPalette, BGRAGifFormat{$IFDEF BGRABITMAP_USE_LCL}, ExtCtrls{$ENDIF};

type
  TDisposeMode = BGRAGifFormat.TDisposeMode;
  TGifSubImage = BGRAGifFormat.TGifSubImage;
  TGifSubImageArray = BGRAGifFormat.TGifSubImageArray;

  {* How to deal with the background under the GIF animation }
  TGifBackgroundMode = (
    gbmSimplePaint,                   // frames are rendered without clearing the backgroud
    gbmEraseBackground,               // pixels in the GIF that become transparent are filled with EraseColor
    gbmSaveBackgroundOnce,            // background is saved once before drawing the first frame
    gbmUpdateBackgroundContinuously); // background is updated continuously to handle overlapping animations

  {** String constants for TGifBackgroundMode }
  const GifBackgroundModeStr: array[TGifBackgroundMode] of string =
    ('gbmSimplePaint', 'gbmEraseBackground', 'gbmSaveBackgroundOnce',
    'gbmUpdateBackgroundContinuously');

type
  {* Class to read/write animated GIF, supports animated PNG as well when specified }
  TBGRAAnimatedGif = class(TGraphic)
  private
    FAspectRatio: single;
    FWidth, FHeight:  integer;
    FBackgroundColor: TColor;

    FPrevDate: TDateTime;
    FPaused:   boolean;
    FTimeAccumulator: double;
    FCurrentImage, FWantedImage: integer;
    FTotalAnimationTime: int64;
    FPreviousDisposeMode: TDisposeMode;

    FBackgroundImage, FPreviousVirtualScreen, FStretchedVirtualScreen,
    FInternalVirtualScreen, FRestoreImage: TBGRABitmap;
    FImageChanged: boolean;

    {$IFDEF BGRABITMAP_USE_LCL}
    FTimer: TTimer;
    {$ENDIF}

    procedure CheckFrameIndex(AIndex: integer);
    function GetAverageDelayMs: integer;
    function GetCount: integer;
    function GetFrameDelayMs(AIndex: integer): integer;
    function GetFrameDisposeMode(AIndex: integer): TDisposeMode;
    function GetFrameDrawMode(AIndex: integer): TDrawMode;
    function GetFrameHasLocalPalette(AIndex: integer): boolean;
    function GetFrameImage(AIndex: integer): TBGRABitmap;
    function GetFrameImagePos(AIndex: integer): TPoint;
    function GetTimeUntilNextImage: integer;
    {$IFDEF BGRABITMAP_USE_LCL}
    procedure OnTimer(Sender: TObject);
    {$ENDIF}
    procedure Render(StretchWidth, StretchHeight: integer);
    procedure SetAspectRatio(AValue: single);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetFrameDelayMs(AIndex: integer; AValue: integer);
    procedure SetFrameDisposeMode(AIndex: integer; AValue: TDisposeMode);
    procedure SetFrameDrawMode(AIndex: integer; AValue: TDrawMode);
    procedure SetFrameHasLocalPalette(AIndex: integer; AValue: boolean);
    procedure SetFrameImage(AIndex: integer; AValue: TBGRABitmap);
    procedure SetFrameImagePos(AIndex: integer; AValue: TPoint);
    procedure UpdateSimple(Canvas: TCanvas; ARect: TRect;
      DrawOnlyIfChanged: boolean = True);
    procedure UpdateEraseBackground(Canvas: TCanvas; ARect: TRect;
      DrawOnlyIfChanged: boolean = True);
    procedure Init;
    function GetBitmap: TBitmap;
    function GetMemBitmap: TBGRABitmap;
    procedure SaveBackgroundOnce(Canvas: TCanvas; ARect: TRect);
    procedure SetCurrentImage(Index: integer);

  protected
    FImages: TGifSubImageArray;
    FDestroying: boolean;

    {TGraphic}
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: boolean; override;
    function GetHeight: integer; override;
    function GetTransparent: boolean; override;
    function GetWidth: integer; override;
    procedure SetHeight({%H-}Value: integer); override;
    procedure SetTransparent({%H-}Value: boolean); override;
    procedure SetWidth({%H-}Value: integer); override;
    procedure ClearViewer; virtual;
    procedure Changed(Sender: TObject); override;
    procedure EnsureNextFrameRec(AIndex: integer);
    procedure AssignTo(Dest: TPersistent); override;
    procedure AssignImage(AImage: TFPCustomImage; AOwned: boolean);
    procedure LoadFromStreamAsGif(Stream: TStream; AMaxImageCount: integer);
    procedure LoadFromStreamAsPng(Stream: TStream; AMaxImageCount: integer);
    procedure LoadFromStreamAsStatic(Stream: TStream);

    procedure CheckSavable(AFormat: TBGRAImageFormat);
    procedure CheckAnyFrame;
    procedure SaveToStreamAsPng(Stream: TStream);
    procedure SaveToStreamAsPng(Stream: TStream;
      AQuantizer: TBGRAColorQuantizerAny;
      ADitheringAlgorithm: TDitheringAlgorithm); overload; virtual;
    procedure SaveToStreamAsGif(Stream: TStream;
      AQuantizer: TBGRAColorQuantizerAny;
      ADitheringAlgorithm: TDitheringAlgorithm); overload; virtual;

  public
    EraseColor:     TColor;
    BackgroundMode: TGifBackgroundMode;
    LoopCount:      Word;
    LoopDone:       Integer;

    constructor Create(filenameUTF8: string); overload;
    constructor Create(stream: TStream); overload;
    constructor Create(stream: TStream; AMaxImageCount: integer); overload;
    constructor Create; overload; override;
    function Duplicate: TBGRAAnimatedGif;
    procedure Assign(ASource: TPersistent); override;
    function AddFrame(AImage: TFPCustomImage; X,Y: integer; ADelayMs: integer;
      ADisposeMode: TDisposeMode = dmErase; AHasLocalPalette: boolean = false;
      ADrawMode: TDrawMode = dmSetExceptTransparent; AOwned: boolean = false) : integer;
    procedure InsertFrame(AIndex: integer; AImage: TFPCustomImage; X,Y: integer; ADelayMs: integer;
      ADisposeMode: TDisposeMode = dmErase; AHasLocalPalette: boolean = false;
      ADrawMode: TDrawMode = dmSetExceptTransparent; AOwned: boolean = false);
    procedure DeleteFrame(AIndex: integer; AEnsureNextFrameDoesNotChange: boolean);

    {** Add a frame that replaces completely the previous one }
    function AddFullFrame(AImage: TFPCustomImage; ADelayMs: integer;
                          AHasLocalPalette: boolean = true;
                          ADrawMode: TDrawMode = dmSetExceptTransparent; AOwned: boolean = false): integer;
    {** Insert at the specified _AIndex_ a frame that replaces completely the previous one }
    procedure InsertFullFrame(AIndex: integer;
                              AImage: TFPCustomImage; ADelayMs: integer;
                              AHasLocalPalette: boolean = true;
                              ADrawMode: TDrawMode = dmSetExceptTransparent; AOwned: boolean = false);
    procedure ReplaceFullFrame(AIndex: integer;
                              AImage: TFPCustomImage; ADelayMs: integer;
                              AHasLocalPalette: boolean = true;
                              ADrawMode: TDrawMode = dmSetExceptTransparent; AOwned: boolean = false);
    procedure OptimizeFrames;

    {TGraphic}
    procedure LoadFromStream(Stream: TStream); overload; override;
    procedure LoadFromStream(Stream: TStream; AMaxImageCount: integer); overload;
    procedure LoadFromResource(AFilename: string);
    {** Save to a stream using GIF format }
    procedure SaveToStream(Stream: TStream); override; overload;
    {** There are some differences in the dispose modes and draw modes so some files
        cannot be directly saved from one format to the other:
        - dispose mode dmErase is only in GIF and dispose mode dmEraseArea is only in PNG,
        - draw mode in GIF is only dmSetExceptTransparent and draw mode in PNG is dmSet or dmDrawWithTransparency.

        PNG format is not limited to 256 colors, so there is no need for quantization even if it possible.
        When PNG has a palette, it applies to all frames, whereas for GIF, there can be a palette for each frame. }
    procedure SaveToStream(Stream: TStream; AFormat: TBGRAImageFormat); overload;
    procedure LoadFromFile(const AFilenameUTF8: string); override;
    procedure SaveToFile(const AFilenameUTF8: string); override;
    class function GetFileExtensions: string; override;

    procedure SetSize(AWidth,AHeight: integer); virtual;
    procedure SaveToStream(Stream: TStream; AQuantizer: TBGRAColorQuantizerAny;
      ADitheringAlgorithm: TDitheringAlgorithm; AFormat: TBGRAImageFormat = ifGif); overload; virtual;
    procedure Clear; override;
    destructor Destroy; override;
    procedure Pause;
    procedure Resume;

    procedure Show(Canvas: TCanvas; ARect: TRect); overload;
    procedure Update(Canvas: TCanvas; ARect: TRect); overload;
    procedure Hide(Canvas: TCanvas; ARect: TRect); overload;
    function MakeBitmapCopy(ABackground: TColor = clNone): TBitmap;

    property BackgroundColor: TColor Read FBackgroundColor write SetBackgroundColor;
    property Count: integer Read GetCount;
    property Width: integer Read FWidth;
    property Height: integer Read FHeight;
    property Paused: boolean Read FPaused;
    property Bitmap: TBitmap Read GetBitmap;
    property MemBitmap: TBGRABitmap Read GetMemBitmap;
    property CurrentImage: integer Read FCurrentImage Write SetCurrentImage;
    property TimeUntilNextImageMs: integer read GetTimeUntilNextImage;
    property FrameImage[AIndex: integer]: TBGRABitmap read GetFrameImage write SetFrameImage;
    property FrameHasLocalPalette[AIndex: integer]: boolean read GetFrameHasLocalPalette write SetFrameHasLocalPalette;
    property FrameImagePos[AIndex: integer]: TPoint read GetFrameImagePos write SetFrameImagePos;
    property FrameDelayMs[AIndex: integer]: integer read GetFrameDelayMs write SetFrameDelayMs;
    property FrameDisposeMode[AIndex: integer]: TDisposeMode read GetFrameDisposeMode write SetFrameDisposeMode;
    property FrameDrawMode[AIndex: integer]: TDrawMode read GetFrameDrawMode write SetFrameDrawMode; // linear blend only in PNG
    property AspectRatio: single read FAspectRatio write SetAspectRatio;
    property TotalAnimationTimeMs: Int64 read FTotalAnimationTime;
    property AverageDelayMs: integer read GetAverageDelayMs;
  end;

  {* @abstract(Class to read/write animated PNG, supports animated GIF as well when specified.)

     This class only changes default format used, everything is implemented in TBGRAAnimatedGif }
  TBGRAAnimatedPng = class(TBGRAAnimatedGif)
    {** Save to a stream using PNG format }
    procedure SaveToStream(Stream: TStream); override; overload;
    class function GetFileExtensions: string; override;
  end;

  {* Static GIF reader }
  TBGRAReaderGIF = class(TFPCustomImageReader)
  protected
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
  end;

  {* Static GIF writer }
  TBGRAWriterGIF = class(TFPCustomImageWriter)
  protected
    procedure InternalWrite(Str: TStream; Img: TFPCustomImage); override;
  end;

implementation

uses BGRABlend, BGRAUTF8,
  BGRAReadPng, BGRAWritePng, BGRAPNGComn
  {$IFDEF BGRABITMAP_USE_LCL}, Graphics{$ENDIF};

const
  {$IFDEF ENDIAN_LITTLE}
  AlphaMask = $FF000000;
  {$ELSE}
  AlphaMask = $000000FF;
  {$ENDIF}


{ TBGRAAnimatedGif }

class function TBGRAAnimatedGif.GetFileExtensions: string;
begin
  Result := 'gif';
end;

procedure TBGRAAnimatedGif.SetSize(AWidth, AHeight: integer);
begin
  ClearViewer;
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TBGRAAnimatedGif.SaveToStream(Stream: TStream;
      AQuantizer: TBGRAColorQuantizerAny;
      ADitheringAlgorithm: TDitheringAlgorithm;
      AFormat: TBGRAImageFormat);
begin
  case AFormat of
    ifGif: SaveToStreamAsGif(Stream, AQuantizer, ADitheringAlgorithm);
    ifPng: SaveToStreamAsPng(Stream, AQuantizer, ADitheringAlgorithm);
    else
      raise Exception.Create('Unhandled image format (' + SuggestImageExtension(AFormat) + ')');
  end;
end;

procedure TBGRAAnimatedGif.Render(StretchWidth, StretchHeight: integer);
var
  curDate: TDateTime;
  previousImage, nextImage: integer;

begin
  if FInternalVirtualScreen = nil then
  begin
    FInternalVirtualScreen := TBGRABitmap.Create(FWidth, FHeight);
    if (Count = 0) and (BackgroundColor <> clNone) then
      FInternalVirtualScreen.Fill(BackgroundColor)
    else
      FInternalVirtualScreen.Fill(BGRAPixelTransparent);
    FImageChanged := True;
  end;

  if Count = 0 then
    exit;

  previousImage := FCurrentImage;

  curDate := Now;
  if FWantedImage <> -1 then
  begin
    nextImage    := FWantedImage;
    FTimeAccumulator := 0;
    FWantedImage := -1;
  end
  else
  if FCurrentImage = -1 then
  begin
    nextImage := 0;
    FTimeAccumulator := 0;
    FPreviousDisposeMode := dmNone;
  end
  else
  begin
    if not FPaused then
      IncF(FTimeAccumulator, (curDate - FPrevDate) * 24 * 60 * 60 * 1000);
    if FTotalAnimationTime > 0 then FTimeAccumulator:= frac(FTimeAccumulator/FTotalAnimationTime)*FTotalAnimationTime;
    nextImage := FCurrentImage;
    while FTimeAccumulator > FImages[nextImage].DelayMs do
    begin
      DecF(FTimeAccumulator, FImages[nextImage].DelayMs);
      Inc(nextImage);
      if nextImage >= Count then
      begin
        if (LoopCount > 0) and (LoopDone >= LoopCount-1) then
        begin
          LoopDone := LoopCount;
          dec(nextImage);
          break;
        end else
        begin
          nextImage := 0;
          inc(LoopDone);
        end;
      end;

      if nextImage = previousImage then
      begin
        if not ((LoopCount > 0) and (LoopDone >= LoopCount-1)) then
        begin
          Inc(nextImage);
          if nextImage >= Count then
            nextImage := 0;
        end;
        break;
      end;
    end;
  end;
  FPrevDate := curDate;

  while FCurrentImage <> nextImage do
  begin
    case FPreviousDisposeMode of
      dmEraseArea:
        with FImages[FCurrentImage] do
          FInternalVirtualScreen.EraseRect(
            RectWithSize(Position.X, Position.Y, Image.Width, Image.Height), 255);
    end;

    Inc(FCurrentImage);
    if FCurrentImage >= Count then
    begin
      FCurrentImage := 0;
      FPreviousDisposeMode := dmErase;
    end;

    case FPreviousDisposeMode of
      dmErase: FInternalVirtualScreen.Fill(BGRAPixelTransparent);
      dmRestore: if FRestoreImage <> nil then
          FInternalVirtualScreen.PutImage(0, 0, FRestoreImage, dmSet);
    end;

    with FImages[FCurrentImage] do
    begin
      if disposeMode = dmRestore then
      begin
        if FRestoreImage = nil then
          FRestoreImage := TBGRABitmap.Create(FWidth, FHeight);
        FRestoreImage.PutImage(0, 0, FInternalVirtualScreen, dmSet);
      end;

      if Image <> nil then
        FInternalVirtualScreen.PutImage(Position.X, Position.Y, Image,
          DrawMode);
      FPreviousDisposeMode := DisposeMode;
    end;

    FImageChanged := True;
    previousImage := FCurrentImage;
    FInternalVirtualScreen.InvalidateBitmap;
  end;

  if FStretchedVirtualScreen <> nil then
    FStretchedVirtualScreen.FreeReference;
  if (FInternalVirtualScreen.Width = StretchWidth) and
    (FInternalVirtualScreen.Height = StretchHeight) then
    FStretchedVirtualScreen := TBGRABitmap(FInternalVirtualScreen.NewReference)
  else
    FStretchedVirtualScreen :=
      TBGRABitmap(FInternalVirtualScreen.Resample(StretchWidth, StretchHeight));
end;

procedure TBGRAAnimatedGif.SetAspectRatio(AValue: single);
begin
  if AValue < 0.25 then AValue := 0.25;
  if AValue > 4 then AValue := 4;
  if FAspectRatio=AValue then Exit;
  FAspectRatio:=AValue;
end;

procedure TBGRAAnimatedGif.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor=AValue then Exit;
  FBackgroundColor:=AValue;
end;

procedure TBGRAAnimatedGif.SetFrameDelayMs(AIndex: integer; AValue: integer);
begin
  CheckFrameIndex(AIndex);
  if AValue < 0 then AValue := 0;
  FTotalAnimationTime := FTotalAnimationTime + AValue - FImages[AIndex].DelayMs;
  FImages[AIndex].DelayMs := AValue;
end;

procedure TBGRAAnimatedGif.SetFrameDisposeMode(AIndex: integer;
  AValue: TDisposeMode);
begin
  CheckFrameIndex(AIndex);
  FImages[AIndex].DisposeMode := AValue;
end;

procedure TBGRAAnimatedGif.SetFrameDrawMode(AIndex: integer; AValue: TDrawMode);
begin
  CheckFrameIndex(AIndex);
  if not (AValue in[dmSet, dmSetExceptTransparent, dmLinearBlend]) then
    raise Exception.Create('Unhandled draw mode');
  FImages[AIndex].DrawMode := AValue;
end;

procedure TBGRAAnimatedGif.SetFrameHasLocalPalette(AIndex: integer;
  AValue: boolean);
begin
  CheckFrameIndex(AIndex);
  FImages[AIndex].HasLocalPalette := AValue;
end;

procedure TBGRAAnimatedGif.SetFrameImage(AIndex: integer; AValue: TBGRABitmap);
var ACopy: TBGRABitmap;
begin
  CheckFrameIndex(AIndex);
  ACopy := AValue.Duplicate;
  FImages[AIndex].Image.FreeReference;
  FImages[AIndex].Image := ACopy;
end;

procedure TBGRAAnimatedGif.SetFrameImagePos(AIndex: integer; AValue: TPoint);
begin
  CheckFrameIndex(AIndex);
  FImages[AIndex].Position := AValue;
end;

procedure TBGRAAnimatedGif.UpdateSimple(Canvas: TCanvas; ARect: TRect;
  DrawOnlyIfChanged: boolean = True);
begin
  if FPreviousVirtualScreen <> nil then
  begin
    FPreviousVirtualScreen.FreeReference;
    FPreviousVirtualScreen := nil;
  end;

  Render(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
  if FImageChanged then
  begin
    FStretchedVirtualScreen.Draw(Canvas, ARect.Left, ARect.Top, False);
    FImageChanged := False;
  end
  else
  if not DrawOnlyIfChanged then
    FStretchedVirtualScreen.Draw(Canvas, ARect.Left, ARect.Top, False);

  FPreviousVirtualScreen := TBGRABitmap(FStretchedVirtualScreen.NewReference);
end;

procedure TBGRAAnimatedGif.CheckFrameIndex(AIndex: integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then Raise ERangeError.Create('Index out of bounds');
end;

function TBGRAAnimatedGif.GetAverageDelayMs: integer;
var sum: int64;
  i: Integer;
begin
  if Count > 0 then
  begin
    sum := 0;
    for i := 0 to Count-1 do
      inc(sum, FrameDelayMs[i]);
    result := sum div Count;
  end else
    result := 100; //default
end;

function TBGRAAnimatedGif.GetCount: integer;
begin
  Result := length(FImages);
end;

function TBGRAAnimatedGif.GetFrameDelayMs(AIndex: integer): integer;
begin
  CheckFrameIndex(AIndex);
  result := FImages[AIndex].DelayMs;
end;

function TBGRAAnimatedGif.GetFrameDisposeMode(AIndex: integer): TDisposeMode;
begin
  CheckFrameIndex(AIndex);
  result := FImages[AIndex].DisposeMode;
end;

function TBGRAAnimatedGif.GetFrameDrawMode(AIndex: integer): TDrawMode;
begin
  CheckFrameIndex(AIndex);
  result := FImages[AIndex].DrawMode;
end;

function TBGRAAnimatedGif.GetFrameHasLocalPalette(AIndex: integer): boolean;
begin
  CheckFrameIndex(AIndex);
  result := FImages[AIndex].HasLocalPalette;
end;

function TBGRAAnimatedGif.GetFrameImage(AIndex: integer): TBGRABitmap;
begin
  CheckFrameIndex(AIndex);
  result := FImages[AIndex].Image;
end;

function TBGRAAnimatedGif.GetFrameImagePos(AIndex: integer): TPoint;
begin
  CheckFrameIndex(AIndex);
  result := FImages[AIndex].Position;
end;

function TBGRAAnimatedGif.GetTimeUntilNextImage: integer;
var
  acc: double;
begin
  if Count <= 1 then result := 60*1000 else
  if (FWantedImage <> -1) or (FCurrentImage = -1) then
    result := 0
  else
  begin
    acc := FTimeAccumulator;
    if not FPaused then IncF(acc, (Now- FPrevDate) * 24 * 60 * 60 * 1000);
    if acc >= FImages[FCurrentImage].DelayMs then
      result := 0
    else
      result := round(FImages[FCurrentImage].DelayMs-FTimeAccumulator);
  end;
end;

{$IFDEF BGRABITMAP_USE_LCL}
procedure TBGRAAnimatedGif.OnTimer(Sender: TObject);
var
  waitMs: Integer;
begin
  waitMs := TimeUntilNextImageMs;
  if waitMs <= 0 then
  begin
    Changed(self);
  end else
  begin
    FTimer.Enabled := false;
    FTimer.Interval:= waitMs+5;
    FTimer.Enabled := true;
  end;
end;
{$ENDIF}

constructor TBGRAAnimatedGif.Create(filenameUTF8: string);
begin
  inherited Create;
  Init;
  LoadFromFile(filenameUTF8);
end;

constructor TBGRAAnimatedGif.Create(stream: TStream);
begin
  inherited Create;
  Init;
  LoadFromStream(stream);
end;

constructor TBGRAAnimatedGif.Create(stream: TStream; AMaxImageCount: integer);
begin
  inherited Create;
  Init;
  LoadFromStream(stream, AMaxImageCount);
end;

constructor TBGRAAnimatedGif.Create;
begin
  inherited Create;
  Init;
  LoadFromStream(nil);
end;

function TBGRAAnimatedGif.Duplicate: TBGRAAnimatedGif;
var
  i: integer;
begin
  Result := TBGRAAnimatedGif.Create;
  setlength(Result.FImages, length(FImages));
  for i := 0 to high(FImages) do
  begin
    Result.FImages[i] := FImages[i];
    FImages[i].Image.NewReference;
  end;
  Result.FWidth  := FWidth;
  Result.FHeight := FHeight;
  Result.FBackgroundColor := FBackgroundColor;
end;

procedure TBGRAAnimatedGif.Assign(ASource: TPersistent);
var
  i: integer;
  src: TBGRAAnimatedGif;
begin
  if ASource is TBGRAAnimatedGif then
  begin
    src := TBGRAAnimatedGif(ASource);
    Clear;
    FWidth  := src.Width;
    FHeight := src.Height;
    FBackgroundColor := src.BackgroundColor;
    FAspectRatio:= src.AspectRatio;
    LoopDone := 0;
    LoopCount := src.LoopCount;

    SetLength(FImages, src.Count);
    FTotalAnimationTime:= 0;
    for i := 0 to src.Count-1 do
    begin
      FImages[i] := src.FImages[i];
      FImages[i].Image := FImages[i].Image.Duplicate;
      inc(FTotalAnimationTime, FImages[i].DelayMs);
    end;
    Changed(self);
  end else
  if ASource is TFPCustomImage then
    AssignImage(TFPCustomImage(ASource), false)
  else
    inherited Assign(ASource);
end;

function TBGRAAnimatedGif.AddFrame(AImage: TFPCustomImage; X, Y: integer;
  ADelayMs: integer; ADisposeMode: TDisposeMode; AHasLocalPalette: boolean;
  ADrawMode: TDrawMode; AOwned: boolean): integer;
begin
  result := length(FImages);
  InsertFrame(result, AImage, X, Y, ADelayMs, ADisposeMode, AHasLocalPalette,
    ADrawMode, AOwned);
end;

procedure TBGRAAnimatedGif.InsertFrame(AIndex: integer; AImage: TFPCustomImage; X,
  Y: integer; ADelayMs: integer; ADisposeMode: TDisposeMode;
  AHasLocalPalette: boolean; ADrawMode: TDrawMode; AOwned: boolean);
var i: integer;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise ERangeError.Create('Index out of bounds');
  setlength(FImages, length(FImages)+1);
  if ADelayMs < 0 then ADelayMs:= 0;
  for i := high(FImages) downto AIndex+1 do
    FImages[i] := FImages[i-1];
  with FImages[AIndex] do
  begin
    if AOwned then
    begin
      if AImage is TBGRABitmap then
        Image := TBGRABitmap(AImage)
      else
      begin
        Image := TBGRABitmap.Create(AImage);
        AImage.Free;
      end;
    end else
      Image := TBGRABitmap.Create(AImage);
    Position := Point(x,y);
    DelayMs := ADelayMs;
    HasLocalPalette := AHasLocalPalette;
    DisposeMode := ADisposeMode;
    DrawMode := ADrawMode;
  end;
  inc(FTotalAnimationTime, ADelayMs);
  if AIndex <= FCurrentImage then inc(FCurrentImage);
end;

function TBGRAAnimatedGif.AddFullFrame(AImage: TFPCustomImage;
  ADelayMs: integer; AHasLocalPalette: boolean;
  ADrawMode: TDrawMode; AOwned: boolean): integer;
begin
  if (AImage.Width <> Width) or (AImage.Height <> Height) then
    raise exception.Create('Size mismatch');
  if Count > 0 then
    FrameDisposeMode[Count-1] := dmErase;
  result := AddFrame(AImage, 0,0, ADelayMs, dmErase, AHasLocalPalette,
    ADrawMode, AOwned);
end;

procedure TBGRAAnimatedGif.InsertFullFrame(AIndex: integer;
  AImage: TFPCustomImage; ADelayMs: integer; AHasLocalPalette: boolean;
  ADrawMode: TDrawMode; AOwned: boolean);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise ERangeError.Create('Index out of bounds');

  if AIndex = Count then
    AddFullFrame(AImage, ADelayMs, AHasLocalPalette, ADrawMode, AOwned)
  else
  begin
    //if previous image did not clear up, ensure that
    //next image will stay the same
    if AIndex > 0 then
      EnsureNextFrameRec(AIndex-1);

    InsertFrame(AIndex, AImage, 0,0, ADelayMs, dmErase, AHasLocalPalette,
      ADrawMode, AOwned);
  end;
end;

procedure TBGRAAnimatedGif.ReplaceFullFrame(AIndex: integer;
  AImage: TFPCustomImage; ADelayMs: integer; AHasLocalPalette: boolean;
  ADrawMode: TDrawMode; AOwned: boolean);
begin
  DeleteFrame(AIndex, True);
  if AIndex > 0 then FrameDisposeMode[AIndex-1] := dmErase;
  InsertFrame(AIndex, AImage, 0,0, ADelayMs, dmErase, AHasLocalPalette,
    ADrawMode, AOwned);
end;

procedure TBGRAAnimatedGif.OptimizeFrames;
var
  prevCurImage, i, y, x: Integer;
  prevFrame, curFrame, changeFrame: TBGRABitmap;
  scanPrev, scanNext: PBGRAPixel;
  transparentAppear: Boolean;
  rChange: TRect;
begin
  if Count <= 1 then exit;
  prevCurImage := CurrentImage;
  CurrentImage := 0;
  prevFrame := MemBitmap.Duplicate;
  for i := 1 to Count-1 do
  begin
    CurrentImage := i;
    curFrame := MemBitmap.Duplicate;
    //necessary only if transparent pixels appear
    if FrameDisposeMode[i-1] = dmErase then
    begin
      transparentAppear := false;
      for y := 0 to Height-1 do
      begin
        scanPrev := prevFrame.ScanLine[y];
        scanNext := curFrame.ScanLine[y];
        for x := 0 to Width-1 do
        begin
          if (scanNext^.alpha < 255) and (scanPrev^ <> scanNext^) then
          begin
            transparentAppear:= true;
            break;
          end;
          inc(scanPrev);
          inc(scanNext);
        end;
      end;
      if not transparentAppear then
        FrameDisposeMode[i-1] := dmKeep;
    end;

    if FrameDisposeMode[i-1] = dmKeep then
    begin
      changeFrame := curFrame.Duplicate;
      for y := 0 to Height-1 do
      begin
        scanPrev := prevFrame.ScanLine[y];
        scanNext := changeFrame.ScanLine[y];
        for x := 0 to Width-1 do
        begin
          if scanPrev^ = scanNext^ then
            scanNext^ := BGRAPixelTransparent;
          inc(scanPrev);
          inc(scanNext);
        end;
      end;
      rChange := changeFrame.GetImageBounds;
      FImages[i].Image.FreeReference;
      if rChange.IsEmpty then
        FImages[i].Image := TBGRABitmap.Create
      else
        FImages[i].Image := changeFrame.GetPart(rChange);
      FImages[i].Position := rChange.TopLeft;
      changeFrame.Free;
    end else
    if FrameDisposeMode[i-1] = dmErase then
    begin
      rChange := curFrame.GetImageBounds;
      if rChange <> RectWithSize(FImages[i].Position.x, FImages[i].Position.y,
         FImages[i].Image.Width, FImages[i].Image.Height) then
      begin
        FImages[i].Image.FreeReference;
        if rChange.IsEmpty then
          FImages[i].Image := TBGRABitmap.Create
        else
          FImages[i].Image := curFrame.GetPart(rChange);
        FImages[i].Position := rChange.TopLeft;
      end;
    end;

    prevFrame.Free;
    prevFrame := curFrame;
    curFrame := nil;
  end;
  prevFrame.Free;
  CurrentImage := prevCurImage;
end;

procedure TBGRAAnimatedGif.DeleteFrame(AIndex: integer;
  AEnsureNextFrameDoesNotChange: boolean);
var
  i: Integer;
begin
  CheckFrameIndex(AIndex);

  //if this frame did not clear up, ensure that
  //next image will stay the same
  if AEnsureNextFrameDoesNotChange then
    EnsureNextFrameRec(AIndex);

  dec(FTotalAnimationTime, FImages[AIndex].DelayMs);

  FImages[AIndex].Image.FreeReference;
  for i := AIndex to Count-2 do
    FImages[i] := FImages[i+1];
  SetLength(FImages, Count-1);

  if AIndex < CurrentImage then
    CurrentImage := CurrentImage-1
  else
  if (CurrentImage >= Count) then
  begin
    CurrentImage := 0;
    Changed(self);
  end;
end;

procedure TBGRAAnimatedGif.LoadFromStream(Stream: TStream);
begin
  LoadFromStream(Stream, maxLongint);
end;

procedure TBGRAAnimatedGif.LoadFromStream(Stream: TStream;
  AMaxImageCount: integer);
begin
  if Stream = nil then
  begin
    Clear;
    FWidth  := 0;
    FHeight := 0;
    exit;
  end;
  case DetectFileFormat(Stream) of
    ifGif: LoadFromStreamAsGif(Stream, AMaxImageCount);
    ifPng: LoadFromStreamAsPng(Stream, AMaxImageCount);
    ifUnknown: raise Exception.Create('Unknown image format');
  else
    LoadFromStreamAsStatic(Stream);
  end;
end;

procedure TBGRAAnimatedGif.LoadFromResource(AFilename: string);
var
  stream: TStream;
begin
  stream := BGRAResource.GetResourceStream(AFilename);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TBGRAAnimatedGif.SaveToStream(Stream: TStream);
begin
  SaveToStream(Stream, ifGif);
end;

procedure TBGRAAnimatedGif.SaveToStream(Stream: TStream; AFormat: TBGRAImageFormat);
var temp: TMemoryStream; // needed because stream position is set to zero
begin
  case AFormat of
    ifGif: SaveToStream(Stream, BGRAColorQuantizerFactory, daFloydSteinberg, AFormat);
    ifPng: SaveToStreamAsPng(Stream);
  else
    begin
      temp := TMemoryStream.Create;
      try
        MemBitmap.SaveToStreamAs(temp, AFormat);
        temp.Position := 0;
        Stream.CopyFrom(temp, temp.Size);
      finally
        temp.Free;
      end;
    end;
  end;
end;

procedure TBGRAAnimatedGif.LoadFromFile(const AFilenameUTF8: string);
var stream: TFileStreamUTF8;
begin
  stream := TFileStreamUTF8.Create(AFilenameUTF8,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(stream);
  finally
    Stream.Free;
  end;
end;

procedure TBGRAAnimatedGif.SaveToFile(const AFilenameUTF8: string);
var
  Stream: TFileStreamUTF8;
  imageFormat: TBGRAImageFormat;
begin
  imageFormat := SuggestImageFormat(AFilenameUTF8);
  if imageFormat = ifUnknown then imageFormat := ifGif;
  CheckSavable(imageFormat);
  Stream := TFileStreamUTF8.Create(AFilenameUTF8, fmCreate);
  try
    SaveToStream(Stream, imageFormat);
  finally
    Stream.Free;
  end;
end;

procedure TBGRAAnimatedGif.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  if FBackgroundImage <> nil then
    FreeAndNil(FBackgroundImage);
  SaveBackgroundOnce(ACanvas, Rect);

  if FPreviousVirtualScreen <> nil then
  begin
    FPreviousVirtualScreen.FreeReference;
    FPreviousVirtualScreen := nil;
  end;

  Render(Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
  FStretchedVirtualScreen.Draw(ACanvas, Rect.Left, Rect.Top, false);
  FImageChanged := False;

  FPreviousVirtualScreen := TBGRABitmap(FStretchedVirtualScreen.Duplicate);

  {$IFDEF BGRABITMAP_USE_LCL}
  FTimer.Enabled := false;
  if Count > 1 then
  begin
    FTimer.Interval := TimeUntilNextImageMs + 5;
    FTimer.Enabled := true;
  end;
  {$ENDIF}
end;

function TBGRAAnimatedGif.GetEmpty: boolean;
begin
  Result := (length(FImages) = 0);
end;

function TBGRAAnimatedGif.GetHeight: integer;
begin
  Result := FHeight;
end;

function TBGRAAnimatedGif.GetTransparent: boolean;
begin
  Result := True;
end;

function TBGRAAnimatedGif.GetWidth: integer;
begin
  Result := FWidth;
end;

procedure TBGRAAnimatedGif.SetHeight(Value: integer);
begin
  //not implemented
end;

procedure TBGRAAnimatedGif.SetTransparent(Value: boolean);
begin
  //not implemented
end;

procedure TBGRAAnimatedGif.SetWidth(Value: integer);
begin
  //not implemented
end;

procedure TBGRAAnimatedGif.ClearViewer;
begin
  FCurrentImage    := -1;
  FWantedImage     := -1;
  FTimeAccumulator := 0;

  if FStretchedVirtualScreen <> nil then
    FStretchedVirtualScreen.FreeReference;
  if FPreviousVirtualScreen <> nil then
    FPreviousVirtualScreen.FreeReference;
  FInternalVirtualScreen.Free;
  FRestoreImage.Free;
  FBackgroundImage.Free;

  FInternalVirtualScreen := nil;
  FStretchedVirtualScreen := nil;
  FRestoreImage    := nil;
  FBackgroundImage := nil;
  FPreviousVirtualScreen := nil;

  FPreviousDisposeMode := dmNone;
end;

procedure TBGRAAnimatedGif.Changed(Sender: TObject);
begin
  {$IFDEF BGRABITMAP_USE_LCL}
  if Assigned(FTimer) then FTimer.Enabled := false;
  {$ENDIF}
  inherited Changed(Sender);
end;

procedure TBGRAAnimatedGif.EnsureNextFrameRec(AIndex: integer);
var
  nextImage: TBGRABitmap;
  prevCurrentImage: integer;
begin
  if (AIndex < Count-1) and (FrameDisposeMode[AIndex] <> dmErase) then
  begin
    prevCurrentImage := CurrentImage;
    CurrentImage := AIndex+1;
    nextImage := MemBitmap.Duplicate;
    FrameImagePos[AIndex+1] := Point(0,0);
    FrameImage[AIndex+1] := nextImage;
    FrameHasLocalPalette[AIndex+1] := true;
    FreeAndNil(nextImage);
    EnsureNextFrameRec(AIndex+1);
    FrameDisposeMode[AIndex] := dmErase;
    CurrentImage := prevCurrentImage;
  end;
end;

procedure TBGRAAnimatedGif.AssignTo(Dest: TPersistent);

  procedure AssignToBitmap;
  {$IFDEF WINDOWS}
  begin
    MemBitmap.AssignToBitmap(TBitmap(Dest));
  end;
  {$ELSE}
  var
    copy: TBitmap;
  begin
    copy := MemBitmap.MakeBitmapCopy(CSSSilver, true);
    try
      TBitmap(Dest).Assign(copy);
    finally
      copy.Free;
    end;
  end;
  {$ENDIF}

  procedure AssignToFPImage;
  var
    img: TFPCustomImage;
    p: PBGRAPixel;
    yb, xb: Integer;
    bgra: TBGRABitmap;
  begin
    bgra := MemBitmap;
    img := TFPCustomImage(Dest);
    img.SetSize(bgra.Width, bgra.Height);
    for yb := 0 to bgra.Height-1 do
    begin
      p := bgra.ScanLine[yb];
      for xb := 0 to bgra.Width-1 do
      begin
        img.Colors[xb,yb] := p^.ToFPColor;
        inc(p);
      end;
    end;
  end;

begin
  if Dest is TBitmap then
    AssignToBitmap
  else if Dest is TBGRACustomBitmap then
    Dest.Assign(MemBitmap)
  else if Dest is TFPCustomImage then
    AssignToFPImage
  else
    inherited AssignTo(Dest);
end;

procedure TBGRAAnimatedGif.AssignImage(AImage: TFPCustomImage; AOwned: boolean);
begin
  Clear;
  SetSize(AImage.Width, AImage.Height);
  AddFrame(AImage, 0, 0, 100, dmKeep, False, dmSet, AOwned);
  Changed(self);
end;

procedure TBGRAAnimatedGif.LoadFromStreamAsGif(Stream: TStream;
  AMaxImageCount: integer);
var data: TGIFData;
  i: integer;
begin
  data := GIFLoadFromStream(Stream, AMaxImageCount);

  Clear;
  FWidth  := data.Width;
  FHeight := data.Height;
  FBackgroundColor := data.BackgroundColor;
  FAspectRatio:= data.AspectRatio;
  LoopCount := data.LoopCount;

  SetLength(FImages, length(data.Images));
  FTotalAnimationTime:= 0;
  for i := 0 to high(FImages) do
  begin
    FImages[i] := data.Images[i];
    inc(FTotalAnimationTime, FImages[i].DelayMs);
  end;

  Changed(self);
end;

procedure TBGRAAnimatedGif.LoadFromStreamAsPng(Stream: TStream;
  AMaxImageCount: integer);
var
  reader: TBGRAReaderPNG;
  mainBitmap, frameBitmap: TBGRABitmap;
  frameControl: TFrameControlChunk;
  i: Integer;
  disposeMode: TDisposeMode;
  drawMode: TDrawMode;
begin
  reader := TBGRAReaderPNG.Create;
  mainBitmap := nil;
  try
    mainBitmap := TBGRABitmap.Create;
    mainBitmap.CanvasDrawModeFP := dmSet;
    reader.ImageRead(Stream, mainBitmap);
    // if it is actually an animation
    if reader.FrameCount > 0 then
    begin
      Clear;
      LoopCount := reader.LoopCount;
      if mainBitmap.ResolutionY <> 0 then
        AspectRatio := mainBitmap.ResolutionX / mainBitmap.ResolutionY;
      SetSize(mainBitmap.Width, mainBitmap.Height);
      for i := 0 to reader.FrameCount-1 do
      begin
        frameControl := reader.FrameControl[i];
        case frameControl.DisposeOp of
          APNG_DISPOSE_OP_NONE: disposeMode := dmKeep;
          APNG_DISPOSE_OP_PREVIOUS:
          begin
            if i = 0 then
              disposeMode := dmErase
              else disposeMode:= dmRestore;
          end
          else {APNG_DISPOSE_OP_BACKGROUND}
          begin
            if (frameControl.OffsetX = 0) and (frameControl.OffsetY = 0)
              and (frameControl.Width = Width) and (frameControl.Height = Height) then
              disposeMode := dmErase
            else
              disposeMode:= dmEraseArea;
          end;
        end;
        if frameControl.BlendOp = APNG_BLEND_OP_OVER then
          drawMode := dmLinearBlend
        else
          drawMode := dmSet;
        if (i = reader.MainImageFrameIndex) and Assigned(mainBitmap) then
        begin
          frameBitmap := mainBitmap;
          mainBitmap := nil;
        end else
        begin
          frameBitmap := TBGRABitmap.Create;
          frameBitmap.CanvasDrawModeFP := dmSet;
          reader.LoadFrame(i, frameBitmap);
        end;
        AddFrame(frameBitmap, frameControl.OffsetX, frameControl.OffsetY,
          round(frameControl.DelayNum / frameControl.DelayDenom * 1000),
          disposeMode, false, drawMode, true);
      end;
    end else
    begin
      AssignImage(mainBitmap, true);
      mainBitmap := nil;
    end;
  finally
    mainBitmap.Free;
    reader.Free;
  end;
end;

procedure TBGRAAnimatedGif.LoadFromStreamAsStatic(Stream: TStream);
var
  image: TBGRABitmap;
begin
  image := TBGRABitmap.Create(Stream);
  AssignImage(image, true);
end;

procedure TBGRAAnimatedGif.CheckSavable(AFormat: TBGRAImageFormat);
var
  drawMode: TDrawMode;
  disposeMode: TDisposeMode;
  framePos: TPoint;
  i: integer;
begin
  CheckAnyFrame;
  case AFormat of
    ifGif: begin
      for i := 0 to Count-1 do
      begin
        drawMode := FrameDrawMode[i];
        if (drawMode <> dmSetExceptTransparent) and
         not ((drawMode in[dmSet, dmLinearBlend]) and (i = 0)) and
         not ((drawMode = dmLinearBlend) and FrameImage[i].HasSemiTransparentPixels) then
        begin
          raise Exception.Create('Draw mode not supported by GIF');
        end;
        disposeMode := FrameDisposeMode[i];
        framePos := FrameImagePos[i];
        if (disposeMode = dmEraseArea) and
          ((framePos.X <> 0) or (framePos.Y <> 0) or
           (FrameImage[i].Width <> Width) or
           (FrameImage[i].Height <> Height)) then
          raise Exception.Create('Dispose mode not supported by GIF');
      end;
    end;
  end;
end;

procedure TBGRAAnimatedGif.CheckAnyFrame;
begin
  if Count = 0 then
    raise Exception.Create('No frame defined');
  if (Width = 0) or (Height = 0) then
    raise Exception.Create('Image of zero size');
end;

procedure TBGRAAnimatedGif.SaveToStreamAsPng(Stream: TStream);
var
  writer: TBGRAWriterPNG;
  curImage, mainImageWithMargin: TBGRABitmap;
  framesToWrite: TPNGArrayOfFrameToWrite;
  fc: TFrameControlChunk;
  i: integer;
  temp: TMemoryStream; // needed because stream position is set to zero
begin
  CheckSavable(ifPng);

  writer := TBGRAWriterPNG.Create;
  mainImageWithMargin := nil;
  temp := TMemoryStream.Create;
  try
    // check if transparency will be used
    writer.UseAlpha:= false;
    for i := 0 to Count-1 do
      if FrameImage[i].HasTransparentPixels then
        writer.UseAlpha:= true;

    // define frame array to write
    SetLength(framesToWrite, Count);
    for i := 0 to Count-1 do
    begin
      curImage := FrameImage[i];
      if i = 0 then
      begin
        fc.Width := Width;
        fc.Height := Height;
        fc.OffsetX := 0;
        fc.OffsetY := 0;
        if (curImage.Width <> Width) or
          (curImage.Height <> Height) or
          (FrameImagePos[i].X <> 0) or
          (FrameImagePos[i].Y <> 0) then
        begin
          // add margin to main image
          mainImageWithMargin := TBGRABitmap.Create(Width, Height);
          mainImageWithMargin.PutImage(FrameImagePos[i].X,
            FrameImagePos[i].Y, curImage, dmSet);
          curImage.CopyPropertiesTo(mainImageWithMargin);
          curImage := mainImageWithMargin;
          mainImageWithMargin := nil;
        end;
      end else
      begin
        fc.Width := curImage.Width;
        fc.Height := curImage.Height;
        fc.OffsetX:= FrameImagePos[i].X;
        fc.OffsetY:= FrameImagePos[i].Y;
      end;
      fc.DelayNum := FrameDelayMs[i];
      fc.DelayDenom := 1000;
      case FrameDisposeMode[i] of
        dmErase, dmEraseArea: fc.DisposeOp := APNG_DISPOSE_OP_BACKGROUND;
        dmRestore: fc.DisposeOp:= APNG_DISPOSE_OP_PREVIOUS;
        else fc.DisposeOp := APNG_DISPOSE_OP_NONE;
      end;
      case FrameDrawMode[i] of
        dmLinearBlend, dmDrawWithTransparency: fc.BlendOp:= APNG_BLEND_OP_OVER;
        else fc.BlendOp:= APNG_BLEND_OP_SOURCE;
      end;
      framesToWrite[i].FrameControl := fc;
      framesToWrite[i].Image := curImage;
    end;

    writer.AnimationWrite(temp, framesToWrite[0].Image, framesToWrite);
    temp.Position := 0;
    Stream.CopyFrom(temp, temp.Size);
  finally
    temp.Free;
    mainImageWithMargin.Free;
    writer.Free;
  end;
end;

procedure TBGRAAnimatedGif.SaveToStreamAsPng(Stream: TStream;
  AQuantizer: TBGRAColorQuantizerAny; ADitheringAlgorithm: TDitheringAlgorithm);
var
  weightedPalette: TBGRAWeightedPalette;
  reducedPalette: TFPPalette;
  bmp: TBGRABitmap;
  i: Integer;
  pData: PBGRAPixel;
  quantizer: TBGRACustomColorQuantizer;
  writer: TBGRAWriterPNG;
  framesToWrite: TPNGArrayOfFrameToWrite;
  fc: TFrameControlChunk;
  curImage: TBGRABitmap;
  mainImageWithMargin: TBGRABitmap;
begin
  CheckSavable(ifPng);
  weightedPalette := nil;
  reducedPalette := nil;
  quantizer := nil;
  mainImageWithMargin := nil;
  writer := TBGRAWriterPNG.Create;
  try
    // check if transparency will be used
    writer.UseAlpha:= false;
    for i := 0 to Count-1 do
      if FrameImage[i].HasTransparentPixels then
        writer.UseAlpha:= true;

    // make global palette for all frames
    weightedPalette := TBGRAWeightedPalette.Create;
    for i := 0 to Count-1 do
      weightedPalette.IncColors(FrameImage[i]);
    quantizer := AQuantizer.Create(weightedPalette, false, 256);
    FreeAndNil(weightedPalette);
    reducedPalette := TFPPalette.Create(0);
    quantizer.ReducedPalette.AssignTo(reducedPalette);
    writer.CustomPalette := reducedPalette;

    // define frame array to write
    SetLength(framesToWrite, Count);
    for i := 0 to Count-1 do
    begin
      curImage := FrameImage[i];
      if i = 0 then
      begin
        fc.Width := Width;
        fc.Height := Height;
        fc.OffsetX := 0;
        fc.OffsetY := 0;
        if (curImage.Width <> Width) or
          (curImage.Height <> Height) or
          (FrameImagePos[i].X <> 0) or
          (FrameImagePos[i].Y <> 0) then
        begin
          // add margin to main image
          mainImageWithMargin := TBGRABitmap.Create(Width, Height);
          mainImageWithMargin.PutImage(FrameImagePos[i].X,
            FrameImagePos[i].Y, curImage, dmSet);
          curImage.CopyPropertiesTo(mainImageWithMargin);
          quantizer.ApplyDitheringInplace(ADitheringAlgorithm, mainImageWithMargin);
          curImage := mainImageWithMargin;
          mainImageWithMargin := nil;
        end;
      end else
      begin
        fc.Width := curImage.Width;
        fc.Height := curImage.Height;
        fc.OffsetX:= FrameImagePos[i].X;
        fc.OffsetY:= FrameImagePos[i].Y;
        curImage := curImage.Duplicate(true);
        quantizer.ApplyDitheringInplace(ADitheringAlgorithm, curImage);
      end;
      fc.DelayNum := FrameDelayMs[i];
      fc.DelayDenom := 1000;
      case FrameDisposeMode[i] of
        dmErase, dmEraseArea: fc.DisposeOp := APNG_DISPOSE_OP_BACKGROUND;
        dmRestore: fc.DisposeOp:= APNG_DISPOSE_OP_PREVIOUS;
        else fc.DisposeOp := APNG_DISPOSE_OP_NONE;
      end;
      case FrameDrawMode[i] of
        dmLinearBlend, dmDrawWithTransparency: fc.BlendOp:= APNG_BLEND_OP_OVER;
        else fc.BlendOp:= APNG_BLEND_OP_SOURCE;
      end;
      framesToWrite[i].FrameControl := fc;
      framesToWrite[i].Image := curImage;
    end;

    writer.AnimationWrite(Stream, framesToWrite[0].Image, framesToWrite);
  finally
    for i := 0 to high(framesToWrite) do
      framesToWrite[i].Image.Free;
    mainImageWithMargin.Free;
    quantizer.Free;
    weightedPalette.Free;
    reducedPalette.Free;
    writer.Free;
  end;
end;

procedure TBGRAAnimatedGif.SaveToStreamAsGif(Stream: TStream;
  AQuantizer: TBGRAColorQuantizerAny; ADitheringAlgorithm: TDitheringAlgorithm);
var data: TGIFData;
begin
  CheckSavable(ifGif);
  data.Height:= Height;
  data.Width := Width;
  data.AspectRatio := AspectRatio;
  data.BackgroundColor := BackgroundColor;
  data.Images := FImages;
  data.LoopCount := LoopCount;
  GIFSaveToStream(data, Stream, AQuantizer, ADitheringAlgorithm);
end;

procedure TBGRAAnimatedGif.SaveBackgroundOnce(Canvas: TCanvas; ARect: TRect);
begin
  if (FBackgroundImage <> nil) and
    ((FBackgroundImage.Width <> ARect.Right - ARect.Left) or
    (FBackgroundImage.Height <> ARect.Bottom - ARect.Top)) then
    FreeAndNil(FBackgroundImage);

  if (BackgroundMode in [gbmSaveBackgroundOnce, gbmUpdateBackgroundContinuously]) and
    (FBackgroundImage = nil) then
  begin
    FBackgroundImage := TBGRABitmap.Create(ARect.Right - ARect.Left,
      ARect.Bottom - ARect.Top);
    FBackgroundImage.GetImageFromCanvas(Canvas, ARect.Left, ARect.Top);
  end;
end;

procedure TBGRAAnimatedGif.SetCurrentImage(Index: integer);
begin
  if (Index >= 0) and (Index < Length(FImages)) then
    FWantedImage := Index;
end;

procedure TBGRAAnimatedGif.Clear;
var
  i, prevCount: integer;
begin
  inherited Clear;

  prevCount := Count;

  for i := 0 to Count - 1 do
    FImages[i].Image.FreeReference;
  FImages := nil;
  LoopDone := 0;
  LoopCount := 0;
  AspectRatio := 1;
  BackgroundColor:= clNone;
  ClearViewer;

  if not FDestroying and (prevCount <> 0) then
    Changed(self);
end;

destructor TBGRAAnimatedGif.Destroy;
begin
  FDestroying := true;
  {$IFDEF BGRABITMAP_USE_LCL}
  FTimer.Enabled := false;
  FreeAndNil(FTimer);
  {$ENDIF}
  Clear;

  if FStretchedVirtualScreen <> nil then
    FStretchedVirtualScreen.FreeReference;
  if FPreviousVirtualScreen <> nil then
    FPreviousVirtualScreen.FreeReference;
  FInternalVirtualScreen.Free;
  FRestoreImage.Free;
  FBackgroundImage.Free;
  inherited Destroy;
end;

procedure TBGRAAnimatedGif.Pause;
begin
  FPaused := True;
end;

procedure TBGRAAnimatedGif.Resume;
begin
  FPaused := False;
end;

procedure TBGRAAnimatedGif.Show(Canvas: TCanvas; ARect: TRect);
begin
  Canvas.StretchDraw(ARect, self);
end;

procedure TBGRAAnimatedGif.Update(Canvas: TCanvas; ARect: TRect);
var
  n: integer;
  PChangePix, PNewPix, PBackground, PNewBackground: PLongWord;
  oldpix, newpix, newbackpix: LongWord;
  NewBackgroundImage: TBGRABitmap;
begin
  if (BackgroundMode = gbmUpdateBackgroundContinuously) and
    (FBackgroundImage = nil) then
    BackgroundMode := gbmSaveBackgroundOnce;

  SaveBackgroundOnce(Canvas, ARect);

  case BackgroundMode of
    gbmSimplePaint:
    begin
      UpdateSimple(Canvas, ARect);
      exit;
    end;
    gbmEraseBackground:
    begin
      UpdateEraseBackground(Canvas, ARect);
      exit;
    end;
    gbmSaveBackgroundOnce, gbmUpdateBackgroundContinuously:
    begin
      if FPreviousVirtualScreen <> nil then
      begin
        if (FPreviousVirtualScreen.Width <> ARect.Right - ARect.Left) or
          (FPreviousVirtualScreen.Height <> ARect.Bottom - ARect.Top) then
        begin
          FPreviousVirtualScreen.FreeReference;
          FPreviousVirtualScreen := nil;
        end
        else
          FPreviousVirtualScreen := TBGRABitmap(FPreviousVirtualScreen.GetUnique);
      end;

      Render(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);

      if FImageChanged then
      begin
        if BackgroundMode = gbmUpdateBackgroundContinuously then
        begin
          NewBackgroundImage :=
            TBGRABitmap.Create(FStretchedVirtualScreen.Width,
            FStretchedVirtualScreen.Height);
          NewBackgroundImage.GetImageFromCanvas(Canvas, ARect.Left, ARect.Top);

          if FPreviousVirtualScreen = nil then
          begin
            FPreviousVirtualScreen := TBGRABitmap.Create(FWidth, FHeight);
            FPreviousVirtualScreen.Fill(BGRAPixelTransparent);
          end;

          PChangePix  := PLongWord(FPreviousVirtualScreen.Data);
          PNewPix     := PLongWord(FStretchedVirtualScreen.Data);
          PBackground := PLongWord(FBackgroundImage.Data);
          PNewBackground := PLongWord(NewBackgroundImage.Data);
          for n := FStretchedVirtualScreen.NbPixels - 1 downto 0 do
          begin
            oldpix := PChangePix^;

            if (oldpix and AlphaMask = AlphaMask) then //pixel opaque précédent
            begin
              newbackpix := PNewBackground^;
              if (newbackpix <> oldpix) then //stocke nouveau fond
                PBackground^ := newbackpix;
            end;

            newpix := PNewPix^;

            if newpix and AlphaMask = AlphaMask then
              PChangePix^ := newpix //pixel opaque
            else if newpix and AlphaMask > 0 then
            begin
              PChangePix^ := PBackground^;
              DrawPixelInlineNoAlphaCheck(PBGRAPixel(PChangePix), PBGRAPixel(@newpix)^);
            end
            else if PChangePix^ and AlphaMask <> 0 then
              PChangePix^ := PBackground^; //efface précédent

{               if newpix and AlphaMask > AlphaLimit then PChangePix^ := newpix or AlphaMask //pixel opaque
               else if PChangePix^ and AlphaMask <> 0 then PChangePix^ := PBackground^; //efface précédent}

            Inc(PNewPix);
            Inc(PChangePix);
            Inc(PBackground);
            Inc(PNewBackground);
          end;
          NewBackgroundImage.Free;
          FPreviousVirtualScreen.InvalidateBitmap;
          FPreviousVirtualScreen.Draw(Canvas, ARect.Left, ARect.Top, false);
          FPreviousVirtualScreen.PutImage(0, 0, FStretchedVirtualScreen, dmSet);
        end
        else
        begin
          if FPreviousVirtualScreen = nil then
          begin
            FStretchedVirtualScreen.Draw(Canvas, ARect.Left, ARect.Top, false);
            FPreviousVirtualScreen :=
              TBGRABitmap(FStretchedVirtualScreen.NewReference);
          end
          else
          begin
            PChangePix  := PLongWord(FPreviousVirtualScreen.Data);
            PNewPix     := PLongWord(FStretchedVirtualScreen.Data);
            PBackground := PLongWord(FBackgroundImage.Data);
            for n := FStretchedVirtualScreen.NbPixels - 1 downto 0 do
            begin
              newpix := PNewPix^;

              if newpix and AlphaMask = AlphaMask then
                PChangePix^ := newpix //pixel opaque
              else if newpix and AlphaMask > 0 then
              begin
                PChangePix^ := PBackground^;
                DrawPixelInlineNoAlphaCheck(PBGRAPixel(PChangePix), PBGRAPixel(@newpix)^);
              end
              else if PChangePix^ and AlphaMask <> 0 then
                PChangePix^ := PBackground^; //efface précédent

{                 if newpix and AlphaMask > AlphaLimit then PChangePix^ := newpix or AlphaMask //pixel opaque
                 else if PChangePix^ and AlphaMask <> 0 then PChangePix^ := PBackground^; //efface précédent}

              Inc(PNewPix);
              Inc(PChangePix);
              Inc(PBackground);
            end;
            FPreviousVirtualScreen.InvalidateBitmap;
            FPreviousVirtualScreen.Draw(Canvas, ARect.Left, ARect.Top, false);
            FPreviousVirtualScreen.PutImage(0, 0, FStretchedVirtualScreen, dmSet);
          end;
        end;
        FImageChanged := False;
      end;
    end;
  end;
end;

procedure TBGRAAnimatedGif.Hide(Canvas: TCanvas; ARect: TRect);
var
  shape: TBGRABitmap;
  p, pback: PBGRAPixel;
  MemEraseColor: TBGRAPixel;
  n: integer;
begin
  MemEraseColor := ColorToBGRA(EraseColor);
  if FPreviousVirtualScreen <> nil then
  begin
    if (FPreviousVirtualScreen.Width <> ARect.Right - ARect.Left) or
      (FPreviousVirtualScreen.Height <> ARect.Bottom - ARect.Top) then
    begin
      FPreviousVirtualScreen.FreeReference;
      FPreviousVirtualScreen := nil;
    end;
  end;

  case BackgroundMode of
    gbmEraseBackground, gbmSimplePaint:
    begin
      if FPreviousVirtualScreen <> nil then
      begin
        shape := TBGRABitmap(FPreviousVirtualScreen.Duplicate);
        p     := shape.Data;
        for n := shape.NbPixels - 1 downto 0 do
        begin
          if p^.alpha <> 0 then
            p^ := MemEraseColor
          else
            p^ := BGRAPixelTransparent;
          Inc(p);
        end;
        shape.Draw(Canvas, ARect.Left, ARect.Top, false);
        shape.FreeReference;
      end;
    end;
    gbmSaveBackgroundOnce, gbmUpdateBackgroundContinuously:
    begin
      if (FPreviousVirtualScreen <> nil) and (FBackgroundImage <> nil) then
      begin
        shape := TBGRABitmap(FPreviousVirtualScreen.Duplicate);
        p     := shape.Data;
        pback := FBackgroundImage.Data;
        for n := shape.NbPixels - 1 downto 0 do
        begin
          if p^.alpha <> 0 then
            p^ := pback^
          else
            p^ := BGRAPixelTransparent;
          Inc(p);
          Inc(pback);
        end;
        shape.Draw(Canvas, ARect.Left, ARect.Top, false);
        shape.FreeReference;
      end;
    end;
  end;
end;

function TBGRAAnimatedGif.MakeBitmapCopy(ABackground: TColor): TBitmap;
begin
  result := MemBitmap.MakeBitmapCopy(ABackground);
end;

procedure TBGRAAnimatedGif.UpdateEraseBackground(Canvas: TCanvas;
  ARect: TRect; DrawOnlyIfChanged: boolean);
var
  n:      integer;
  PChangePix, PNewPix: PLongWord;
  newpix: LongWord;
  MemPixEraseColor: LongWord;
begin
  if EraseColor = clNone then
  begin
    UpdateSimple(Canvas, ARect, DrawOnlyIfChanged);
    exit;
  end;

  if FPreviousVirtualScreen <> nil then
  begin
    if (FPreviousVirtualScreen.Width <> ARect.Right - ARect.Left) or
      (FPreviousVirtualScreen.Height <> ARect.Bottom - ARect.Top) then
    begin
      FPreviousVirtualScreen.FreeReference;
      FPreviousVirtualScreen := nil;
    end
    else
      FPreviousVirtualScreen := TBGRABitmap(FPreviousVirtualScreen.GetUnique);
  end;

  Render(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
  if FImageChanged then
  begin
    PBGRAPixel(@MemPixEraseColor)^ := ColorToBGRA(EraseColor);
    if FPreviousVirtualScreen = nil then
    begin
      FStretchedVirtualScreen.Draw(Canvas, ARect.Left, ARect.Top, false);
      FPreviousVirtualScreen := TBGRABitmap(FStretchedVirtualScreen.NewReference);
    end
    else
    begin
      PChangePix := PLongWord(FPreviousVirtualScreen.Data);
      PNewPix    := PLongWord(FStretchedVirtualScreen.Data);
      for n := FStretchedVirtualScreen.NbPixels - 1 downto 0 do
      begin
        newpix := PNewPix^;

        if newpix and AlphaMask = AlphaMask then
          PChangePix^ := newpix //pixel opaque
        else if newpix and AlphaMask > 0 then
        begin
          PChangePix^ := MemPixEraseColor;
          DrawPixelInlineNoAlphaCheck(PBGRAPixel(PChangePix), PBGRAPixel(@newpix)^);
        end
        else if PChangePix^ and AlphaMask <> 0 then
          PChangePix^ := MemPixEraseColor; //efface précédent
{           if newpix and AlphaMask > AlphaLimit then PChangePix^ := newpix or AlphaMask //pixel opaque
           else if PChangePix^ and AlphaMask <> 0 then PChangePix^ := MemPixEraseColor; //efface précédent}

        Inc(PNewPix);
        Inc(PChangePix);
      end;
      FPreviousVirtualScreen.InvalidateBitmap;
      FPreviousVirtualScreen.Draw(Canvas, ARect.Left, ARect.Top, false);
      FPreviousVirtualScreen.PutImage(0, 0, FStretchedVirtualScreen, dmSet);
    end;

    FImageChanged := False;
  end;
end;

procedure TBGRAAnimatedGif.Init;
begin
  FDestroying := false;
  BackgroundMode := gbmSaveBackgroundOnce;
  BackgroundColor:= clNone;
  LoopCount := 0;
  LoopDone := 0;
  AspectRatio:= 1;
  {$IFDEF BGRABITMAP_USE_LCL}
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.OnTimer:=@OnTimer;
  {$ENDIF}
end;

function TBGRAAnimatedGif.GetBitmap: TBitmap;
begin
  Render(FWidth, FHeight);
  Result := FStretchedVirtualScreen.Bitmap;
end;

function TBGRAAnimatedGif.GetMemBitmap: TBGRABitmap;
begin
  Render(FWidth, FHeight);
  Result := FStretchedVirtualScreen;
end;

{ TBGRAAnimatedPng }

procedure TBGRAAnimatedPng.SaveToStream(Stream: TStream);
begin
  SaveToStream(Stream, ifPng);
end;

class function TBGRAAnimatedPng.GetFileExtensions: string;
begin
  Result:= 'apng';
end;

{ TBGRAReaderGIF }

procedure TBGRAReaderGIF.InternalRead(Str: TStream; Img: TFPCustomImage);
var
  gif:  TBGRAAnimatedGif;
  x, y: integer;
  Mem:  TBGRABitmap;
begin
  gif := TBGRAAnimatedGif.Create(Str, 1);
  Mem := gif.MemBitmap;
  if Img is TBGRABitmap then
  begin
    TBGRABitmap(Img).Assign(Mem);
  end
  else
  begin
    Img.SetSize(gif.Width, gif.Height);
    for y := 0 to gif.Height - 1 do
      for x := 0 to gif.Width - 1 do
        with Mem.GetPixel(x, y) do
          Img.Colors[x, y] := FPColor(red * $101, green * $101, blue *
            $101, alpha * $101);
  end;
  gif.Free;
end;

function TBGRAReaderGIF.InternalCheck(Str: TStream): boolean;
var
  GIFSignature: TGIFSignature;
  savepos:      int64;
begin
  savepos := str.Position;
  try
    fillchar({%H-}GIFSignature, sizeof(GIFSignature), 0);
    str.Read(GIFSignature, sizeof(GIFSignature));
    if (GIFSignature[1] = 'G') and (GIFSignature[2] = 'I') and
      (GIFSignature[3] = 'F') then
    begin
      Result := True;
    end
    else
      Result := False;
  except
    on ex: Exception do
      Result := False;
  end;
  str.Position := savepos;
end;

{ TBGRAWriterGIF }

procedure TBGRAWriterGIF.InternalWrite(Str: TStream; Img: TFPCustomImage);
var
  gif: TBGRAAnimatedGif;
begin
  gif := TBGRAAnimatedGif.Create;
  try
    gif.SetSize(Img.Width,Img.Height);
    gif.AddFrame(Img, 0,0,0);
    gif.SaveToStream(Str, BGRAColorQuantizerFactory, daFloydSteinberg);
  except
    on ex: EColorQuantizerMissing do
    begin
      FreeAndNil(gif);
      raise EColorQuantizerMissing.Create('Please define the color quantizer factory. You can do that with the following statements: Uses BGRAPalette, BGRAColorQuantization; BGRAColorQuantizerFactory:= TBGRAColorQuantizer;');
    end;
    on ex: Exception do
    begin
      FreeAndNil(gif);
      raise ex;
    end;
  end;
  FreeAndNil(gif);
end;

initialization

  DefaultBGRAImageReader[ifGif] := TBGRAReaderGIF;
  DefaultBGRAImageWriter[ifGif] := TBGRAWriterGIF;

  {$IFDEF BGRABITMAP_USE_LCL}
  //Lazarus Picture
  TPicture.RegisterFileFormat('gif', 'Animated GIF', TBGRAAnimatedGif);
  TPicture.RegisterFileFormat('apng', 'Animated PNG', TBGRAAnimatedPng);
  {$ENDIF}
end.

