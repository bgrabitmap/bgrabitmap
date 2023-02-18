// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAText;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

{$IFDEF LINUX}
  {$DEFINE SYSTEM_RENDERER_IS_FINE}
  {$DEFINE SYSTEM_CLEARTYPE_RENDERER_IS_FINE}
  {$DEFINE RENDER_TEXT_ON_TBITMAP}
{$ENDIF}
{$IFDEF FREEBSD}
  {$DEFINE SYSTEM_RENDERER_IS_FINE}
  {$DEFINE SYSTEM_CLEARTYPE_RENDERER_IS_FINE}
{$ENDIF}
{$IFDEF DARWIN}
  {$DEFINE SYSTEM_RENDERER_IS_FINE}
  {$DEFINE RENDER_TEXT_ON_TBITMAP}
{$ENDIF}
{$IFDEF WINDOWS}
  {$IFNDEF LEGACY_FONT_VERTICAL_OFFSET}
    {$DEFINE FIX_FONT_VERTICAL_OFFSET}
  {$ENDIF}
{$ENDIF}
{$IFDEF BGRABITMAP_USE_MSEGUI}
  {$DEFINE RENDER_TEXT_ON_TBITMAP}
{$ENDIF}

{
  Font rendering units : BGRAText, BGRATextFX, BGRAVectorize, BGRAFreeType

  This unit provides basic text rendering functions using system renderer 
  (LCL or MSEgui).

  Text functions use a temporary bitmap where the operating system text drawing 
  is used. Then it is scaled down (if antialiasing is activated) and colored.

  These routines are rather slow, so you may use other font renderers
  like TBGRATextEffectFontRenderer in BGRATextFX if you want to use LCL fonts,
  or, if you have TrueType fonts files, you may use TBGRAFreeTypeFontRenderer
  in BGRAFreeType. }

uses
  BGRAClasses, SysUtils, BGRAGraphics, BGRABitmapTypes, BGRAPen, BGRAGrayscaleMask
  {$IFDEF LCL},InterfaceBase, LCLVersion{$ENDIF};

const
  RenderTextOnBitmap = {$IFDEF RENDER_TEXT_ON_TBITMAP}true{$ELSE}false{$ENDIF};

type
  TWordBreakHandler = BGRABitmapTypes.TWordBreakHandler;

  { TBGRASystemFontRenderer }

  TBGRASystemFontRenderer = class(TBGRACustomFontRenderer)
  protected
    FFont: TFont;             //font parameters
    FWordBreakHandler: TWordBreakHandler;
    FOwnUnderline: boolean;
    procedure UpdateFont; virtual;
    function InternalTextSize(sUTF8: string; AShowPrefix: boolean): TSize;
    function InternalTextSizeAngle(sUTF8: string; AShowPrefix: boolean; AOrientation: integer): TSize; virtual;
    procedure InternalTextWordBreak(ADest: TBGRACustomBitmap; ATextUTF8: string;
                                    x, y, AMaxWidth: integer; AColor: TBGRAPixel; ATexture: IBGRAScanner;
                                    AHorizAlign: TAlignment; AVertAlign: TTextLayout; ARightToLeft: boolean);
    procedure InternalTextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; c: TBGRAPixel; ATexture: IBGRAScanner);
    procedure InternalTextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; c: TBGRAPixel; texture: IBGRAScanner;
                              align: TAlignment; AShowPrefix: boolean = false; ARightToLeft: boolean = false); 
    procedure InternalTextOutAngle(ADest: TBGRACustomBitmap; x, y: single; AOrientation: integer; sUTF8: string; c: TBGRAPixel; texture: IBGRAScanner;
                              align: TAlignment; AShowPrefix: boolean = false; ARightToLeft: boolean = false); virtual;
    procedure InternalTextOutEllipse(ADest: TBGRACustomBitmap; x, y, availableWidth: single; sUTF8: string; c: TBGRAPixel; texture: IBGRAScanner;
                              align: TAlignment; AShowPrefix: boolean = false; ARightToLeft: boolean = false);
    procedure InternalSplitText(var ATextUTF8: string; AMaxWidth: integer; out ARemainsUTF8: string; out ALineEndingBreak: boolean;
                                AWordBreak: TWordBreakHandler); overload;
    procedure InternalSplitText(var ATextUTF8: string; AMaxWidth: integer; out ARemainsUTF8: string;
                                AWordBreak: TWordBreakHandler); overload;
    function InternalGetFontPixelMetric: TFontPixelMetric;
    procedure DefaultWorkBreakHandler(var ABeforeUTF8, AAfterUTF8: string);
  public
    OverrideUnderlineDecoration: boolean; // draw unerline according to computed font pixel metric instead of using system rendering of underline
    procedure SplitText(var ATextUTF8: string; AMaxWidth: integer; out ARemainsUTF8: string);
    function GetFontPixelMetric: TFontPixelMetric; override;
    function FontExists(AName: string): boolean; override;
    class function PatchSystemFontName(AName: string): string;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; c: TBGRAPixel; align: TAlignment); overload; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; c: TBGRAPixel; align: TAlignment; ARightToLeft: boolean); overload; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; texture: IBGRAScanner; align: TAlignment); overload; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; texture: IBGRAScanner; align: TAlignment; ARightToLeft: boolean); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; texture: IBGRAScanner; align: TAlignment); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; c: TBGRAPixel; align: TAlignment); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; texture: IBGRAScanner; align: TAlignment; ARightToLeft: boolean); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; c: TBGRAPixel; align: TAlignment; ARightToLeft: boolean); overload; override;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; c: TBGRAPixel); overload; override;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; texture: IBGRAScanner); overload; override;
    procedure TextWordBreak(ADest: TBGRACustomBitmap; AText: string; x, y, AMaxWidth: integer; AColor: TBGRAPixel; AHorizAlign: TAlignment; AVertAlign: TTextLayout; ARightToLeft: boolean = false); overload;
    procedure TextWordBreak(ADest: TBGRACustomBitmap; AText: string; x, y, AMaxWidth: integer; ATexture: IBGRAScanner; AHorizAlign: TAlignment; AVertAlign: TTextLayout; ARightToLeft: boolean = false); overload;
    function TextSize(sUTF8: string): TSize; overload; override;
    function TextSizeAngle(sUTF8: string; orientationTenthDegCCW: integer): TSize; override;
    function TextSize(sUTF8: string; AMaxWidth: integer; {%H-}ARightToLeft: boolean): TSize; overload; override;
    function TextFitInfo(sUTF8: string; AMaxWidth: integer): integer; override;
    constructor Create;
    destructor Destroy; override;
    property OnWordBreak: TWordBreakHandler read FWordBreakHandler write FWordBreakHandler;
  end;

{$IFDEF BGRABITMAP_USE_MSEGUI}
  TMSEFontRenderer = class(TBGRASystemFontRenderer);
{$ELSE}
  TLCLFontRenderer = class(TBGRASystemFontRenderer);
{$ENDIF}

function CleanTextOutString(s: string): string; //this works with UTF8 strings as well
function RemoveLineEnding(var s: string; indexByte: integer): boolean; //this works with UTF8 strings however the index is the byte index
function RemoveLineEndingUTF8(var sUTF8: string; indexUTF8: integer): boolean;

procedure BGRATextOut(bmp: TBGRACustomBitmap; Font: TFont; Quality: TBGRAFontQuality; xf, yf: single; sUTF8: string;
  c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; CustomAntialiasingLevel: Integer = 0;
  ShowPrefix: boolean = false; RightToLeft: boolean = false);

procedure BGRATextOutAngle(bmp: TBGRACustomBitmap; Font: TFont; Quality: TBGRAFontQuality; xf, yf: single; orientationTenthDegCCW: integer;
  sUTF8: string; c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; CustomAntialiasingLevel: Integer = 0);

procedure BGRATextRect(bmp: TBGRACustomBitmap; Font: TFont; Quality: TBGRAFontQuality; ARect: TRect; xf, yf: single;
  sUTF8: string; style: TTextStyle; c: TBGRAPixel; tex: IBGRAScanner; CustomAntialiasingLevel: Integer = 0);

function BGRATextSize(Font: TFont; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer): TSize;
function BGRATextSizeAngle(Font: TFont; AOrientation: integer; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer): TSize;
function BGRATextFitInfo(Font: TFont; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer; AMaxWidth: integer): integer;
function BGRATextFitInfoAngle(Font: TFont; AOrientation: integer; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer; AMaxWidth: integer): integer;
function BGRAOriginalTextSize(Font: TFont; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: integer): TSize;
function BGRAOriginalTextSizeAngle(Font: TFont; AOrientation: integer; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer): TSize;
function BGRAOriginalTextSizeEx(Font: TFont; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer;
                                out actualAntialiasingLevel: integer; out extraVerticalMarginDueToRotation: integer): TSize;
function BGRAOriginalTextSizeExAngle(Font: TFont; AOrientation: integer; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer;
                                out actualAntialiasingLevel: integer; out extraVerticalMarginDueToRotation: integer): TSize;

function BGRATextUnderline(ATopLeft: TPointF; AWidth: Single; AMetrics: TFontPixelMetric): ArrayOfTPointF; overload;
function BGRATextUnderline(ATopLeft: TPointF; AWidth: Single; ABaseline, AEmHeight: single): ArrayOfTPointF; overload;
function BGRATextStrikeOut(ATopLeft: TPointF; AWidth: Single; AMetrics: TFontPixelMetric): ArrayOfTPointF; overload;
function BGRATextStrikeOut(ATopLeft: TPointF; AWidth: Single; ABaseline, AEmHeight, AXHeight: single): ArrayOfTPointF; overload;

function GetFontHeightSign: integer;
function FontEmHeightSign: integer;
function FontFullHeightSign: integer;
function SystemFontAvailable: boolean;
function GetFineClearTypeAuto: TBGRAFontQuality;
function FixSystemFontFullHeight({%H-}AFontName: string; AFontHeight: integer): integer;

{$IFDEF LCL}
function LCLFontAvailable: boolean;
function FixLCLFontFullHeight(AFontName: string; AFontHeight: integer): integer;
{$ENDIF}

procedure BGRAFillClearTypeGrayscaleMask(dest: TBGRACustomBitmap; x,y: integer; xThird: integer; mask: TGrayscaleMask; color: TBGRAPixel; texture: IBGRAScanner = nil; RGBOrder: boolean=true);
procedure BGRAFillClearTypeMask(dest: TBGRACustomBitmap; x,y: integer; xThird: integer; mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner = nil; RGBOrder: boolean=true);
procedure BGRAFillClearTypeRGBMask(dest: TBGRACustomBitmap; x,y: integer; mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner = nil; KeepRGBOrder: boolean=true);

const FontAntialiasingLevel = {$IFDEF SYSTEM_RENDERER_IS_FINE}3{$ELSE}6{$ENDIF};
const FontDefaultQuality = fqAntialiased;
const IsLclFontRendererFine = {$IFDEF SYSTEM_RENDERER_IS_FINE}true{$ELSE}false{$ENDIF};

function GetLCLFontPixelMetric(AFont: TFont): TFontPixelMetric;

var
  BGRATextOutImproveReadabilityProc : procedure (bmp: TBGRACustomBitmap; AFont: TFont; xf,yf: single; text: string; color: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; mode : TBGRATextOutImproveReadabilityMode);

procedure BitmapTextOut(ABitmap: TBitmap; ACoord: TPoint; AText: string);
procedure BitmapTextOutAngle(ABitmap: TBitmap; ACoord: TPoint; AText: string; AOrientation: integer);
procedure BitmapTextRect(ABitmap: TBitmap; ARect: TRect; ACoord: TPoint; 
  AText: string; const AStyle: TTextStyle);
function BitmapTextExtent(ABitmap: TBitmap; AText: string): TSize;
function BitmapTextExtentAngle(ABitmap: TBitmap; AText: string; AOrientation: integer): TSize;
function BitmapTextFitInfo(ABitmap: TBitmap; AText: string; AMaxWidth: integer): integer;
function BitmapTextFitInfoAngle(ABitmap: TBitmap; AText: string; AMaxWidth: integer; AOrientation: integer): integer;
procedure BitmapFillRect(ABitmap: TBitmap; ARect: TRect; AColor: TColor);

implementation

uses Math, BGRATransform, BGRABlend, BGRAUTF8, BGRAUnicode, BGRATextBidi
     {$IFDEF LCL}, Forms{$IF lcl_fullversion >= 1070000}, lclplatformdef{$ENDIF}{$ENDIF};

const MaxPixelMetricCount = 100;

var
  SystemFontDisabledValue: boolean;
  fqFineClearTypeComputed: boolean;
  fqFineClearTypeValue: TBGRAFontQuality;
  FontHeightSignComputed: boolean;
  FontHeightSignValue: integer;
  FontPixelMetricArray: array[0..MaxPixelMetricCount-1] of record
                          usage: integer;
                          name: string;
                          height: integer;
                          italic: boolean;
                          bold: boolean;
                          metric: TFontPixelMetric;
                        end;
  FontPixelMetricCount: integer;
  
{$IF defined(BGRABITMAP_USE_MSEGUI)}
{$i bgramsegui_text.inc}
{$ELSEIF defined(BGRABITMAP_USE_LCL)}
procedure BitmapTextOut(ABitmap: TBitmap; ACoord: TPoint; AText: string);
begin
  ABitmap.Canvas.Brush.Style := bsClear;
  ABitmap.Canvas.TextOut(ACoord.X, ACoord.Y, AText);
end;

procedure BitmapTextOutAngle(ABitmap: TBitmap; ACoord: TPoint; AText: string; AOrientation: integer);
begin
  ABitmap.Canvas.Font.Orientation := AOrientation;
  BitmapTextOut(ABitmap, ACoord, AText);
end;

procedure BitmapTextRect(ABitmap: TBitmap; ARect: TRect; ACoord: TPoint; 
  AText: string; const AStyle: TTextStyle);
begin
  ABitmap.Canvas.Brush.Style := bsClear;
  {$IFDEF DARWIN}
  if AStyle.RightToLeft then AText := UTF8EmbedDirection(AText, true);
  {$ENDIF}
  ABitmap.Canvas.TextRect(ARect, ACoord.X, ACoord.Y, AText, AStyle);
end;

function BitmapTextExtent(ABitmap: TBitmap; AText: string): TSize;
begin
  {$IFDEF DARWIN}
  AText := StringReplace(AText, ' ', UTF8_NO_BREAK_SPACE, [rfReplaceAll]);
  {$ENDIF}
  result := ABitmap.Canvas.TextExtent(AText);
end;

function BitmapTextExtentAngle(ABitmap: TBitmap; AText: string; AOrientation: integer): TSize;
begin
  ABitmap.Canvas.Font.Orientation := AOrientation;
  result := BitmapTextExtent(ABitmap, AText);
end;

function BitmapTextFitInfo(ABitmap: TBitmap; AText: string; AMaxWidth: integer): integer;
begin
  {$IFDEF DARWIN}
  AText := StringReplace(AText, ' ', UTF8_NO_BREAK_SPACE, [rfReplaceAll]);
  {$ENDIF}
  result := ABitmap.Canvas.TextFitInfo(AText, AMaxWidth);
end;

function BitmapTextFitInfoAngle(ABitmap: TBitmap; AText: string; AMaxWidth: integer; AOrientation: integer): integer;
begin
  ABitmap.Canvas.Font.Orientation := AOrientation;
  result := BitmapTextFitInfo(ABitmap, AText, AMaxWidth);
end;

procedure BitmapFillRect(ABitmap: TBitmap; ARect: TRect; AColor: TColor);
begin
  ABitmap.Canvas.Brush.Style := bsSolid;
  ABitmap.Canvas.Brush.Color := AColor;
  ABitmap.Canvas.Pen.Style := psClear;
  ABitmap.Canvas.FillRect(ARect);
end;
{$ELSE}
procedure BitmapTextOut(ABitmap: TBitmap; ACoord: TPoint; AText: string);
begin raise exception.Create('Not implemented') end;

procedure BitmapTextOutAngle(ABitmap: TBitmap; ACoord: TPoint; AText: string; AOrientation: integer);
begin raise exception.Create('Not implemented') end;

procedure BitmapTextRect(ABitmap: TBitmap; ARect: TRect; ACoord: TPoint;
  AText: string; const AStyle: TTextStyle);
begin raise exception.Create('Not implemented') end;

function BitmapTextExtent(ABitmap: TBitmap; AText: string): TSize;
begin raise exception.Create('Not implemented') end;

function BitmapTextExtentAngle(ABitmap: TBitmap; AText: string; AOrientation: integer): TSize;
begin raise exception.Create('Not implemented') end;

function BitmapTextFitInfo(ABitmap: TBitmap; AText: string; AMaxWidth: integer): integer;
begin raise exception.Create('Not implemented') end;

function BitmapTextFitInfoAngle(ABitmap: TBitmap; AText: string; AMaxWidth: integer; AOrientation: integer): integer;
begin raise exception.Create('Not implemented') end;

procedure BitmapFillRect(ABitmap: TBitmap; ARect: TRect; AColor: TColor);
begin raise exception.Create('Not implemented') end;
{$ENDIF}

procedure ComputeFontVerticalBounds(text: string; font: TFont; out top, bottom, totalHeight: integer);
var
  xb,yb: integer;
  pmask: PBGRAPixel;
  nbPix: array of integer;
  nbCur: integer;
  mean: integer;
  mask: TBGRACustomBitmap;
  size: TSize;
begin
  if not SystemFontAvailable then
  begin
    top := 0;
    bottom := 0;
    totalHeight := 0;
    exit;
  end;
  size := BGRAOriginalTextSize(font,fqSystem,text,FontAntialiasingLevel);
  mask := BGRABitmapFactory.Create(size.cx,size.cy,BGRABlack);
  mask.Canvas.Font := font;
  mask.Canvas.Font.Quality := fqAntialiased;
  mask.Canvas.Font.Color := clWhite;
  mask.Canvas.Font.Style := font.style * [fsBold,fsItalic];
  BitmapTextOut(mask.Bitmap, Point(0,0), text);
  top := -1;
  bottom := -1;
  totalHeight:= mask.Height;

  mean := 0;
  setlength(nbPix, mask.Height);
  for yb := 0 to mask.Height-1 do
  begin
    pmask := mask.scanline[yb];
    nbCur := 0;
    for xb := 0 to mask.Width-1 do
    begin
      if (pmask^.green > 0) then inc(nbCur);
      inc(pmask);
    end;
    nbPix[yb] := nbCur;
    inc(mean,nbCur);
  end;
  mean := (mean+ (mask.Height div 2)) div mask.Height;

  for yb := 0 to high(nbPix) do
  begin
    if nbPix[yb]> mean div 3 then
    begin
      if top = -1 then top := yb
      else bottom := yb+1;
    end;
  end;
  mask.Free;
end;

function ComputeFontPixelMetric(AFont: TFont): TFontPixelMetric;
begin
  ComputeFontVerticalBounds('acemu',AFont,result.xLine,result.Baseline,result.Lineheight);
  ComputeFontVerticalBounds('gDjSO',AFont,result.CapLine,result.DescentLine,result.Lineheight);
  if result.xLine = -1 then result.xLine := result.CapLine else
  if result.CapLine = -1 then result.CapLine := result.xLine;
  if result.DescentLine = -1 then result.DescentLine := result.Baseline else
  if result.Baseline = -1 then result.Baseline := result.DescentLine;
  result.Defined := (result.xLine <> -1) and (result.CapLine <> -1) and (result.Baseline <> -1) and (result.DescentLine <> -1) and
     (result.Lineheight <> -1);
end;

function ComparePixelMetric(index: integer; font: TFont): integer;
begin
  if (index < 0) or (index >= FontPixelMetricCount) then
    result := 0
  else
  begin
    with FontPixelMetricArray[index] do
      if (name = font.Name) and (height = font.Height) then
        result := 0 else
      if (height > font.Height) then
        result := 1 else
      if (height < font.Height) then
        result := -1 else
      if name > font.Name then
        result := 1 else
      if name < font.Name then
        result := -1
      else result := 0;
  end;
end;

procedure FindPixelMetricPos(AFont: TFont; out startPos,endPos: integer);
var middle,iStart,iEnd: integer;
begin
  if FontPixelMetricCount = 0 then
  begin
    startPos := 0;
    endPos := 0;
  end;
  iStart:= 0;
  iEnd:= FontPixelMetricCount;
  while iStart < iEnd do
  begin
    middle := (iStart+iEnd) div 2;
    if ComparePixelMetric(middle,AFont) >= 0 then
      iEnd := middle
    else
      iStart := middle+1;
  end;
  startPos := iStart;

  iStart:= startPos;
  iEnd:= FontPixelMetricCount;
  while iStart < iEnd do
  begin
    middle := (iStart+iEnd) div 2;
    if ComparePixelMetric(middle,AFont) <= 0 then
      iStart := middle+1
    else
      iEnd := middle;
  end;
  endPos := iEnd;
end;

procedure RemoveOldPixelMetric;
var sum,nb,i: integer;
begin
  if FontPixelMetricCount = 0 then exit;
  sum := 0;
  for i := 0 to FontPixelMetricCount-1 do
    inc(sum, FontPixelMetricArray[i].usage);
  sum := sum div FontPixelMetricCount;
  nb := 0;
  for i := 0 to FontPixelMetricCount-1 do
  begin
    if FontPixelMetricArray[i].usage > sum then
    begin
      FontPixelMetricArray[nb] := FontPixelMetricArray[i];
      inc(nb);
    end;
  end;
  FontPixelMetricCount := nb;
end;

function GetLCLFontPixelMetric(AFont: TFont): TFontPixelMetric;
var i,startPos,endPos: integer;
  prevHeight,fixHeight: integer;
begin
  if (AFont.Height < -200) or (AFont.Height > 150) then
  begin
    prevHeight := AFont.Height;
    if AFont.Height < 0 then
      fixHeight := -200
    else
      fixHeight := 150;
    AFont.Height := fixHeight;
    result := GetLCLFontPixelMetric(AFont);
    AFont.Height := prevHeight;

    result.Baseline := round(result.Baseline/fixHeight*prevHeight);
    result.CapLine := round(result.CapLine/fixHeight*prevHeight);
    result.DescentLine := round(result.DescentLine/fixHeight*prevHeight);
    result.Lineheight := round(result.Lineheight/fixHeight*prevHeight);
    result.xLine := round(result.xLine/fixHeight*prevHeight);
    exit;
  end;

  FindPixelMetricPos(AFont,startPos,endPos);
  for i := startPos to endPos-1 do
    if (FontPixelMetricArray[i].bold = AFont.bold) and
      (FontPixelMetricArray[i].italic = AFont.Italic) then
    begin
      result := FontPixelMetricArray[i].metric;
      inc(FontPixelMetricArray[i].usage);
      exit;
    end;
  if FontPixelMetricCount = MaxPixelMetricCount then RemoveOldPixelMetric;
  for i := FontPixelMetricCount downto endPos+1 do
    FontPixelMetricArray[i] := FontPixelMetricArray[i-1];
  inc(FontPixelMetricCount);
  with FontPixelMetricArray[endPos]do
  begin
    italic := AFont.Italic;
    bold := AFont.Bold;
    usage := 1;
    name := AFont.Name;
    height:= AFont.Height;
    metric := ComputeFontPixelMetric(AFont);
    result := metric;
  end;
end;

const DefaultFontHeightSign = -1;

function BGRATextUnderline(ATopLeft: TPointF;
  AWidth: Single; AMetrics: TFontPixelMetric): ArrayOfTPointF;
begin
  result := BGRATextUnderline(ATopLeft, AWidth, AMetrics.Baseline,AMetrics.Baseline-AMetrics.CapLine);
end;

function BGRATextUnderline(ATopLeft: TPointF;
  AWidth: Single; ABaseline, AEmHeight: single): ArrayOfTPointF;
var height,y: single;
begin
  height := AEmHeight*0.080;
  y := ATopLeft.y+ABaseline+1.6*height;
  result := ComputeWidePolylinePoints([PointF(ATopLeft.x,y),
                   PointF(ATopLeft.x+AWidth,y)],height,BGRABlack,pecFlat,pjsMiter,
                   SolidPenStyle, []);
end;

function BGRATextStrikeOut(ATopLeft: TPointF; AWidth: Single;
  AMetrics: TFontPixelMetric): ArrayOfTPointF;
begin
  result := BGRATextStrikeOut(ATopLeft, AWidth, AMetrics.Baseline,AMetrics.Baseline-AMetrics.CapLine,AMetrics.Baseline-AMetrics.xLine);
end;

function BGRATextStrikeOut(ATopLeft: TPointF; AWidth: Single; ABaseline,
  AEmHeight, AXHeight: single): ArrayOfTPointF;
var height,y: single;
begin
  height := AEmHeight*0.075;
  y := ATopLeft.y+ABaseline-AXHeight*0.5;
  result := ComputeWidePolylinePoints([PointF(ATopLeft.x,y),
                   PointF(ATopLeft.x+AWidth,y)],height,BGRABlack,pecFlat,pjsMiter,
                   SolidPenStyle, []);
end;

function GetFontHeightSign: integer;
var
  HeightP1, HeightM1: integer;
  tempBmp: TBitmap;
begin
  if SystemFontDisabledValue then
  begin
    result := DefaultFontHeightSign;
    exit;
  end;

  if FontHeightSignComputed then
  begin
    result := FontHeightSignValue;
    exit;
  end;

  if {$IFDEF LCL}WidgetSet.LCLPlatform = lpNoGUI{$ELSE}False{$ENDIF} then
  begin
    SystemFontDisabledValue:= True;
    result := -1;
    exit;
  end;

  tempBmp := nil;
  try
    tempBmp := TBitmap.Create;
    tempBmp.Width := 30;
    tempBmp.Height := 30;
    tempBmp.Canvas.Font.Name := 'Arial';
    tempBmp.Canvas.Font.Style := [];
    tempBmp.Canvas.Font.Height := 20;
    HeightP1  := BitmapTextExtent(tempBmp, 'Hg').cy;
    tempBmp.Canvas.Font.Height := -20;
    HeightM1  := BitmapTextExtent(tempBmp, 'Hg').cy;

    if HeightP1 > HeightM1 then
      FontHeightSignValue := 1
    else
      FontHeightSignValue := -1;

    FontHeightSignComputed := true;
    result := FontHeightSignValue;
  except
    on ex: Exception do
    begin
      SystemFontDisabledValue := True;
      result := -1;
    end;
  end;
  tempBmp.Free;
end;

function GetFineClearTypeAuto: TBGRAFontQuality;
var
  lclBmp: TBitmap;
  bgra: TBGRACustomBitmap;
  x,y: integer;
begin
  if fqFineClearTypeComputed then
  begin
    result:= fqFineClearTypeValue;
    exit;
  end;
  result := fqFineAntialiasing;
  if not SystemFontDisabledValue and not ({$IFDEF LCL}WidgetSet.LCLPlatform = lpNoGUI{$ELSE}False{$ENDIF}) then
  begin
    lclBmp := TBitmap.Create;
    lclBmp.Width := 1;
    lclBmp.Height := 1;
    lclBmp.Canvas.Font.Height := -50;
    lclBmp.Canvas.Font.Quality := fqCleartype;
    lclBmp.Canvas.Font.Color := clBlack;
    with BitmapTextExtent(lclBmp, '/') do
    begin
      lclBmp.Width := cx;
      lclBmp.Height := cy;
    end;
    BitmapFillRect(lclBmp, rect(0,0,lclBmp.Width,lclBmp.Height), clWhite);
    BitmapTextOut(lclBmp, Point(0,0), '/');
    bgra:= BGRABitmapFactory.Create(lclBmp);
    x:= bgra.Width div 2;
    for y := 0 to bgra.Height-1 do
      with bgra.GetPixel(x,y) do
        if (red<>blue) then
        begin
          if blue < red then
            result:= fqFineClearTypeRGB
          else
            result:= fqFineClearTypeBGR;
          break;
        end else
        if (green = 0) then break;
	bgra.Free;
    lclBmp.Free;
  end;
  fqFineClearTypeValue := result;
  fqFineClearTypeComputed:= true;
end;

{$IFNDEF WINDOWS}
var LCLFontFullHeightRatio : array of record
                          FontName: string;
                          Ratio: single;
                        end;
{$ENDIF}

function FixSystemFontFullHeight(AFontName: string; AFontHeight: integer): integer;
{$IFNDEF WINDOWS}
const TestHeight = 200;
var
  i: Integer;
  ratio : single;
  f: TFont;
  h: LongInt;
begin
  if (AFontHeight = 0) or
    (AFontHeight*FontEmHeightSign > 0) then
      result := AFontHeight
  else
  begin
    ratio := EmptySingle;
    for i := 0 to high(LCLFontFullHeightRatio) do
      if CompareText(AFontName, LCLFontFullHeightRatio[i].FontName)=0 then
      begin
        ratio := LCLFontFullHeightRatio[i].Ratio;
        break;
      end;
    if ratio = EmptySingle then
    begin
      f := TFont.Create;
      f.Quality := fqDefault;
      f.Name := AFontName;
      f.Height := FontFullHeightSign*TestHeight;
      h := BGRATextSize(f, fqSystem, 'Hg', 1).cy;
      f.Free;
      if h = 0 then ratio := 1
      else ratio := TestHeight/h;

      setlength(LCLFontFullHeightRatio, length(LCLFontFullHeightRatio)+1);
      LCLFontFullHeightRatio[high(LCLFontFullHeightRatio)].FontName:= AFontName;
      LCLFontFullHeightRatio[high(LCLFontFullHeightRatio)].Ratio:= ratio;
    end;
    result := round(AFontHeight*ratio);
  end;
end;
{$ELSE}
begin
  result := AFontHeight;
end;
{$ENDIF}

{$IFDEF LCL}
function LCLFontAvailable: boolean;
begin
  result := SystemFontAvailable;
end;

function FixLCLFontFullHeight(AFontName: string; AFontHeight: integer): integer;
begin
  result := FixSystemFontFullHeight(AFontName, AFontHeight);
end;
{$ENDIF}

function FontEmHeightSign: integer;
begin
  result := GetFontHeightSign;
end;

function FontFullHeightSign: integer;
begin
  result := -FontEmHeightSign;
end;

function SystemFontAvailable: boolean;
begin
  if not FontHeightSignComputed then GetFontHeightSign;
  result := not SystemFontDisabledValue;
end;

procedure BGRAFillClearTypeGrayscaleMask(dest: TBGRACustomBitmap; x,
  y: integer; xThird: integer; mask: TGrayscaleMask; color: TBGRAPixel;
  texture: IBGRAScanner; RGBOrder: boolean);
begin
  BGRAGrayscaleMask.BGRAFillClearTypeGrayscaleMask(dest,x,y,xThird,mask,color,texture,RGBOrder);
end;

procedure BGRAFillClearTypeMask(dest: TBGRACustomBitmap; x,y: integer; xThird: integer; mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner; RGBOrder: boolean);
begin
  BGRABlend.BGRAFillClearTypeMask(dest,x,y,xThird,mask,color,texture,RGBOrder);
end;

procedure BGRAFillClearTypeRGBMask(dest: TBGRACustomBitmap; x, y: integer;
  mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner;
  KeepRGBOrder: boolean);
begin
  BGRABlend.BGRAFillClearTypeRGBMask(dest,x,y,mask,color,texture,KeepRGBOrder);
end;

function BGRAOriginalTextSizeEx(Font: TFont; Quality: TBGRAFontQuality; 
  sUTF8: string; CustomAntialiasingLevel: Integer; 
  out actualAntialiasingLevel: integer; 
  out extraVerticalMarginDueToRotation: integer): TSize;
begin
  result := BGRAOriginalTextSizeExAngle(Font, Font.Orientation, Quality, sUTF8,
    CustomAntialiasingLevel, actualAntialiasingLevel, extraVerticalMarginDueToRotation);  
end;                               

function BGRAOriginalTextSizeExAngle(Font: TFont; AOrientation: integer; 
  Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer;
  out actualAntialiasingLevel: integer; 
  out extraVerticalMarginDueToRotation: integer): TSize;
var
  tempBmp: TBitmap;
begin
  actualAntialiasingLevel:= CustomAntialiasingLevel;
  extraVerticalMarginDueToRotation := 0;
  if not SystemFontAvailable then
    result := Size(0,0)
  else
  begin
    tempBmp := nil;
    try
      tempBmp := TBitmap.Create;
      {$IFDEF BGRABITMAP_USE_MSEGUI}
      tempBmp.Width := 1;
      tempBmp.Height := 1;
      {$ENDIF}
      tempBmp.Canvas.Font := Font;
      if Quality in[fqFineClearTypeBGR,fqFineClearTypeRGB,fqFineAntialiasing] then
      begin
        tempBmp.Canvas.Font.Height := Font.Height*CustomAntialiasingLevel;
      end else
      begin
        tempBmp.Canvas.Font.Height := Font.Height;
        actualAntialiasingLevel:= 1;
      end;
      result := BitmapTextExtentAngle(tempBmp, sUTF8, AOrientation);
      if Font.Orientation <> 0 then
      begin
        tempBmp.Canvas.Font.Orientation:= 0;
        extraVerticalMarginDueToRotation := result.cy - 
          BitmapTextExtentAngle(tempBmp, sUTF8, AOrientation).cy;
      end;
    except
      on ex: exception do
      begin
        result := Size(0,0);
        SystemFontDisabledValue := True;
      end;
    end;
    tempBmp.Free;
  end;
end;

function BGRATextFitInfo(Font: TFont; Quality: TBGRAFontQuality; sUTF8: string; 
  CustomAntialiasingLevel: Integer; AMaxWidth: integer): integer;
begin
  result := BGRATextFitInfoAngle(Font, Font.Orientation, Quality, sUTF8,
    CustomAntialiasingLevel, AMaxWidth);
end;

function BGRATextFitInfoAngle(Font: TFont; AOrientation: integer; Quality: TBGRAFontQuality; sUTF8: string;
  CustomAntialiasingLevel: Integer; AMaxWidth: integer): integer;
var
  actualAntialiasingLevel{$IFDEF LCL}{$IF lcl_fullversion < 1070000}, len1{$ENDIF}{$ENDIF}: Integer;
  tempBmp: TBitmap;
begin
  if (AMaxWidth = 0) or (length(sUTF8)=0) then exit(0);
  actualAntialiasingLevel:= CustomAntialiasingLevel;
  if not SystemFontAvailable then
    result := 0
  else
  begin
    tempBmp := nil;
    try
      tempBmp := TBitmap.Create;
      tempBmp.Canvas.Font := Font;
      if Quality in[fqFineClearTypeBGR,fqFineClearTypeRGB,fqFineAntialiasing] then
      begin
        tempBmp.Canvas.Font.Height := Font.Height*CustomAntialiasingLevel;
      end else
      begin
        tempBmp.Canvas.Font.Height := Font.Height;
        actualAntialiasingLevel:= 1;
      end;
      {$IFDEF LCL}{$IF lcl_fullversion < 1070000}
      len1 := BitmapTextExtentAngle(tempBmp, 
                copy(sUTF8,1,UTF8CharacterLength(@sUTF8[1])),
                AOrientation).cx;
      if len1 > AMaxWidth*actualAntialiasingLevel then exit(0);
      {$ENDIF}{$ENDIF}
      result := BitmapTextFitInfoAngle(tempBmp, sUTF8, 
        AMaxWidth*actualAntialiasingLevel, AOrientation);
    except
      on ex: exception do
      begin
        result := 0;
        SystemFontDisabledValue := True;
      end;
    end;
    tempBmp.Free;
  end;
end;

function BGRAOriginalTextSize(Font: TFont; Quality: TBGRAFontQuality; 
  sUTF8: string; CustomAntialiasingLevel: Integer): TSize;
begin
  result := BGRAOriginalTextSizeAngle(Font, Font.Orientation, Quality, sUTF8, 
                                      CustomAntialiasingLevel);
end;

function BGRAOriginalTextSizeAngle(Font: TFont; AOrientation: integer;
  Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer): TSize;
var actualAntialiasingLevel, extraMargin: integer;
begin
  result := BGRAOriginalTextSizeExAngle(Font, AOrientation, Quality, sUTF8, 
    CustomAntialiasingLevel, actualAntialiasingLevel, extraMargin);
  {$IFDEF FIX_FONT_VERTICAL_OFFSET}
  if extraMargin > 0 then dec(result.cy, extraMargin);
  {$ENDIF}
end;

function BGRATextSize(Font: TFont; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer): TSize;
begin
  result := BGRATextSizeAngle(Font, Font.Orientation, Quality, sUTF8, CustomAntialiasingLevel);
end;

function BGRATextSizeAngle(Font: TFont; AOrientation: integer; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer): TSize;
begin
  {$IFDEF SYSTEM_RENDERER_IS_FINE}
  if Quality = fqFineAntialiasing then Quality:= fqSystem;
  {$ENDIF}
  result := BGRAOriginalTextSizeAngle(Font, AOrientation, Quality, sUTF8, CustomAntialiasingLevel);
  if Quality in[fqFineClearTypeBGR,fqFineClearTypeRGB,fqFineAntialiasing] then
  begin
    result.cx := ceil(Result.cx/CustomAntialiasingLevel);
    result.cy := ceil(Result.cy/CustomAntialiasingLevel);
  end;
end;

function RemovePrefix(sUTF8: string): string;
var i,resLen: integer;
begin
  setlength(result, length(sUTF8));
  resLen := 0;
  i := 1;
  while i <= length(sUTF8) do
  begin
    if sUTF8[i] = '&' then
    begin // double ('&&') indicate single char '&'
      if (i < length(sUTF8)) and (sUTF8[i+1] = '&') then
      begin
        inc(resLen);
        result[resLen] := '&';
        inc(i,2);
      end else
        // single indicate underline
        inc(i);
    end else
    begin
      inc(resLen);
      result[resLen] := sUTF8[i];
      inc(i);
    end;
  end;
  setlength(result,resLen);
end;

procedure FilterOriginalText(Quality: TBGRAFontQuality; CustomAntialiasingLevel: Integer; var temp: TBGRACustomBitmap;
  out grayscaleMask: TGrayscaleMask);
var
  n: integer;
  maxAlpha: UInt32or64;
  pb: PByte;
  multiplyX: integer;
  resampled: TBGRACustomBitmap;
begin
  grayscaleMask := nil;
  case Quality of
  fqFineClearTypeBGR,fqFineClearTypeRGB,fqFineAntialiasing:
    begin
      if Quality in [fqFineClearTypeBGR,fqFineClearTypeRGB] then multiplyX:= 3 else multiplyX:= 1;
      if (temp.Height < CustomAntialiasingLevel*8) and (temp.Height >= CustomAntialiasingLevel*3) then
      begin
        temp.ResampleFilter := rfSpline;
        resampled := temp.Resample(round(temp.width/CustomAntialiasingLevel*multiplyX),round(temp.Height/CustomAntialiasingLevel),rmFineResample);
        grayscaleMask := TGrayscaleMask.Create(resampled,cGreen);
        FreeAndNil(resampled);
      end else
        grayscaleMask := TGrayscaleMask.CreateDownSample(temp, round(temp.width/CustomAntialiasingLevel*multiplyX),round(temp.Height/CustomAntialiasingLevel));
      FreeAndNil(temp);

      maxAlpha := 0;
      pb := grayscaleMask.Data;
      for n := grayscaleMask.NbPixels - 1 downto 0 do
      begin
        if Pb^ > maxAlpha then maxAlpha := Pb^;
        Inc(pb);
      end;
      if (maxAlpha <> 0) and (maxAlpha <> 255) then
      begin
        pb := grayscaleMask.Data;
        for n := grayscaleMask.NbPixels - 1 downto 0 do
        begin
          pb^:= pb^ * 255 div maxAlpha;
          Inc(pb);
        end;
      end;
    end;
  fqSystem:
    begin
      grayscaleMask := TGrayscaleMask.Create(temp, cGreen);
      FreeAndNil(temp);
      {$IFNDEF LINUX}
      pb := grayscaleMask.Data;
      for n := grayscaleMask.NbPixels - 1 downto 0 do
      begin
        pb^:= GammaExpansionTab[pb^] shr 8;
        Inc(pb);
      end;
      {$ENDIF}
    end;
  end;
end;

function CleanTextOutString(s: string): string;
begin
  result := BGRABitmapTypes.CleanTextOutString(s);
end;

function RemoveLineEnding(var s: string; indexByte: integer): boolean;
begin
  result := BGRABitmapTypes.RemoveLineEnding(s, indexByte);
end;

function RemoveLineEndingUTF8(var sUTF8: string; indexUTF8: integer): boolean;
begin
  result := BGRABitmapTypes.RemoveLineEndingUTF8(sUTF8,indexUTF8);
end;

procedure BGRAInternalRenderText(dest: TBGRACustomBitmap; Quality: TBGRAFontQuality; grayscale: TGrayscaleMask; temp: TBGRACustomBitmap;
  x,y,xThird: integer; c: TBGRAPixel; tex: IBGRAScanner);
begin
  if Quality in [fqFineClearTypeBGR,fqFineClearTypeRGB,fqSystemClearType] then
  begin
    if grayscale <> nil then
      BGRAFillClearTypeGrayscaleMask(dest,x,y,xThird, grayscale,c,tex,Quality=fqFineClearTypeRGB)
    else if temp <> nil then
      BGRAFillClearTypeRGBMask(dest,x,y, temp,c,tex);
  end
  else
  begin
    if grayscale <> nil then
    begin
      if tex <> nil then
        grayscale.DrawAsAlpha(dest, x, y, tex) else
        grayscale.DrawAsAlpha(dest, x, y, c);
    end
    else if temp <> nil then
      dest.PutImage(x, y, temp, dmDrawWithTransparency);
  end;
end;

procedure BGRATextOut(bmp: TBGRACustomBitmap; Font: TFont; Quality: TBGRAFontQuality; xf, yf: single; sUTF8: string;
  c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; CustomAntialiasingLevel: Integer = 0;
  ShowPrefix: boolean = false; RightToLeft: boolean = false);
var
  size: TSize;
  sizeFactor, extraVerticalMargin: integer;
  xMarginF: single;
  style: TTextStyle;
  noPrefix: string;
begin
  if not SystemFontAvailable then exit;

  if CustomAntialiasingLevel = 0 then
    CustomAntialiasingLevel:= FontAntialiasingLevel;

  if Font.Orientation mod 3600 <> 0 then
  begin
    BGRATextOutAngle(bmp,Font,Quality,xf,yf,Font.Orientation,sUTF8,c,tex,align);
    exit;
  end;

  {$IFDEF SYSTEM_RENDERER_IS_FINE}
  if (Quality in [fqFineAntialiasing, fqFineClearTypeRGB, fqFineClearTypeBGR]) and
     (BGRATextSize(Font, fqSystem, 'Hg', 1).cy >= 13) then
  begin
    if Quality = fqFineAntialiasing then Quality := fqSystem;
    {$IFDEF SYSTEM_CLEARTYPE_RENDERER_IS_FINE}
    if Quality = GetFineClearTypeAuto then Quality := fqSystemClearType;
    {$ENDIF}
  end;
  {$ENDIF}

  if ShowPrefix then
    noPrefix := RemovePrefix(sUTF8)
  else
    noPrefix := sUTF8;

  size := BGRAOriginalTextSizeEx(Font,Quality,noPrefix,CustomAntialiasingLevel,sizeFactor,extraVerticalMargin);
  if (size.cx = 0) or (size.cy = 0) then
    exit;

  if (size.cy >= 144) and (Quality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB]) and (CustomAntialiasingLevel > 4) then
  begin
    CustomAntialiasingLevel:= 4;
    size := BGRAOriginalTextSizeEx(Font,Quality,noPrefix,CustomAntialiasingLevel,sizeFactor,extraVerticalMargin);
  end;

  case align of
    taLeftJustify: ;
    taCenter: DecF(xf, size.cx/2/sizeFactor);
    taRightJustify: DecF(xf, size.cx/sizeFactor);
  end;

  xMarginF := size.cy/sizeFactor;
  fillchar({%H-}style,sizeof(style),0);
  style.SingleLine := true;
  style.Alignment := taLeftJustify;
  style.Layout := tlTop;
  style.RightToLeft := RightToLeft;
  style.ShowPrefix := ShowPrefix;
  BGRATextRect(bmp, Font, Quality,
        rect(floor(xf-xMarginF), floor(yf)-1, ceil(xf+size.cx/sizeFactor+xMarginF), ceil(yf+size.cy/sizeFactor)+1),
        xf,yf, sUTF8, style, c, tex, sizeFactor);
end;

procedure BGRATextOutAngle(bmp: TBGRACustomBitmap; Font: TFont; Quality: TBGRAFontQuality; xf, yf: single;
  orientationTenthDegCCW: integer;
  sUTF8: string; c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; CustomAntialiasingLevel: Integer = 0);
var
  posF: TPointF;
  x,y: integer;
  deltaX,deltaY: integer;
  size: TSize;
  temp: TBGRACustomBitmap;
  TopLeft,TopRight,BottomRight,BottomLeft: TPointF;
  Top,dy: Single;
  Left: Single;
  cosA,sinA: single;
  rotBounds: TRect;
  sizeFactor, extraVerticalMargin: integer;
  TempFont: TFont;
  oldOrientation: integer;
  grayscale:TGrayscaleMask;
  {$IFDEF RENDER_TEXT_ON_TBITMAP}
  tempLCL: TBitmap;
  {$ENDIF}

  procedure rotBoundsAdd(pt: TPointF);
  begin
    if pt.x < Left then Left := pt.x;
    if pt.y < Top then Top := pt.y;
    if floor(pt.X) < rotBounds.Left then rotBounds.Left := floor(pt.X/sizeFactor)*sizeFactor;
    if floor(pt.Y) < rotBounds.Top then rotBounds.Top := floor(pt.Y/sizeFactor)*sizeFactor;
    if ceil(pt.X) > rotBounds.Right then rotBounds.Right := ceil(pt.X/sizeFactor)*sizeFactor;
    if ceil(pt.Y) > rotBounds.Bottom then rotBounds.Bottom := ceil(pt.Y/sizeFactor)*sizeFactor;
  end;

begin
  if not SystemFontAvailable or ((c.alpha = 0) and (tex = nil)) then exit;

  if CustomAntialiasingLevel = 0 then
    CustomAntialiasingLevel:= FontAntialiasingLevel;

  if orientationTenthDegCCW mod 3600 = 0 then
  begin
    oldOrientation := Font.Orientation;
    Font.Orientation := 0;
    BGRATextOut(bmp,Font,Quality,xf,yf,sUTF8,c,tex,align);
    Font.Orientation := oldOrientation;
    exit;
  end;
  TempFont := TFont.Create;
  TempFont.Assign(Font);
  TempFont.Height := Font.Height;
  size := BGRAOriginalTextSizeExAngle(TempFont,orientationTenthDegCCW,Quality,sUTF8,CustomAntialiasingLevel,sizeFactor, extraVerticalMargin);
  if (size.cx = 0) or (size.cy = 0) then
  begin
    tempFont.Free;
    exit;
  end;
  {$IFDEF FIX_FONT_VERTICAL_OFFSET}
  if extraVerticalMargin > 0 then
    dy := -extraVerticalMargin*0.5 -1
  else
    dy := 0;
  {$ELSE}
  dy := 0;
  {$ENDIF}
  tempFont.Free;

  cosA := cos(orientationTenthDegCCW*Pi/1800);
  sinA := sin(orientationTenthDegCCW*Pi/1800);
  TopLeft := PointF(sinA*dy,cosA*dy);
  posF := PointF(xf,yf);
  posF.Offset( TopLeft * (1/sizeFactor) );
  TopRight := TopLeft + PointF(cosA*size.cx,-sinA*size.cx);
  BottomRight := TopRight + PointF(sinA*size.cy,cosA*size.cy);
  BottomLeft := TopLeft + PointF(sinA*size.cy,cosA*size.cy);
  rotBounds := rect(0,0,0,0);
  Top := 0;
  Left := 0;
  rotBoundsAdd(TopRight);
  rotBoundsAdd(BottomRight);
  rotBoundsAdd(BottomLeft);
  inc(rotBounds.Right);
  inc(rotBounds.Bottom);

  posF.Offset( Left/sizeFactor, Top/sizeFactor );
  case align of
    taLeftJustify: ;
    taCenter:
      posF.Offset( -TopRight*(1/(2*sizeFactor)) );
    taRightJustify:
      posF.Offset( -TopRight*(1/sizeFactor) );
  end;
  x := floor(posF.x);
  deltaX := round((posF.x - x)*sizeFactor);
  y := floor(posF.y);
  deltaY := round((posF.y - y)*sizeFactor);
  if deltaX <> 0 then inc(rotBounds.Right, sizeFactor);
  if deltaY <> 0 then inc(rotBounds.Bottom, sizeFactor);

  {$IFDEF RENDER_TEXT_ON_TBITMAP}
  tempLCL := TBitmap.Create;
  tempLCL.Width := rotBounds.Right-rotBounds.Left;
  tempLCL.Height := rotBounds.Bottom-rotBounds.Top;
  BitmapFillRect(tempLCL, Rect(0,0,tempLCL.Width,tempLCL.Height), clBlack);
  with tempLCL do begin
  {$ELSE}
  temp := BGRABitmapFactory.Create(rotBounds.Right-rotBounds.Left,rotBounds.Bottom-rotBounds.Top, BGRABlack);
  with temp do begin
  {$ENDIF}
    Canvas.Font := Font;
    Canvas.Font.Color := clWhite;
    Canvas.Font.Height := round(Font.Height*sizeFactor);
    BitmapTextOutAngle({$IFDEF RENDER_TEXT_ON_TBITMAP}tempLCL{$ELSE}temp.Bitmap{$ENDIF}, 
      Point(-rotBounds.Left+deltaX, -rotBounds.Top+deltaY), sUTF8,
      orientationTenthDegCCW);
  end;
  {$IFDEF RENDER_TEXT_ON_TBITMAP}
  temp := BGRABitmapFactory.create(tempLCL,False);
  tempLCL.Free;
  {$ENDIF}

  FilterOriginalText(Quality,CustomAntialiasingLevel,temp,grayscale);
  BGRAInternalRenderText(bmp, Quality, grayscale,temp, x,y,0, c,tex);
  temp.Free;
  grayscale.Free;
end;

procedure BGRATextRect(bmp: TBGRACustomBitmap; Font: TFont; Quality: TBGRAFontQuality; ARect: TRect; xf, yf: single;
  sUTF8: string; style: TTextStyle; c: TBGRAPixel; tex: IBGRAScanner; CustomAntialiasingLevel: Integer = 0);
var
  lim: TRect;
  tx, ty: integer;
  temp:   TBGRACustomBitmap;
  sizeFactor: integer;
  cr: TRect;
  grayscale:TGrayscaleMask;
  {$IFDEF RENDER_TEXT_ON_TBITMAP}
  tempLCL: TBitmap;
  {$ENDIF}
begin
  if not SystemFontAvailable or ((c.alpha = 0) and (tex = nil)) then exit;

  if CustomAntialiasingLevel = 0 then
    CustomAntialiasingLevel:= FontAntialiasingLevel;

  cr := bmp.ClipRect;
  if ARect.Left < cr.Left then
    lim.Left := cr.Left else lim.Left := ARect.Left;
  if ARect.Top < cr.Top then
    lim.Top := cr.Top else lim.Top := ARect.Top;
  if ARect.Right > cr.Right then
    lim.Right := cr.Right else lim.Right := ARect.Right;
  if ARect.Bottom > cr.Bottom then
    lim.Bottom := cr.Bottom else lim.Bottom := ARect.Bottom;

  tx := lim.Right - lim.Left;
  ty := lim.Bottom - lim.Top;
  if (tx <= 0) or (ty <= 0) then
    exit;

  {$IFDEF SYSTEM_RENDERER_IS_FINE}
  if (Quality in [fqFineAntialiasing, fqFineClearTypeRGB, fqFineClearTypeBGR]) and
     (BGRATextSize(Font, fqSystem, 'Hg', 1).cy >= 13) then
  begin
    if Quality = fqFineAntialiasing then Quality := fqSystem;
    {$IFDEF SYSTEM_CLEARTYPE_RENDERER_IS_FINE}
    if Quality = GetFineClearTypeAuto then Quality := fqSystemClearType;
    {$ENDIF}
  end;
  {$ENDIF}

  if Quality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB] then
    sizeFactor := CustomAntialiasingLevel
  else
    sizeFactor := 1;

  {$IFDEF RENDER_TEXT_ON_TBITMAP}
  tempLCL := TBitmap.Create;
  tempLCL.Width := tx*sizeFactor;
  tempLCL.Height := ty*sizeFactor;
  BitmapFillRect(tempLCL, Rect(0,0,tempLCL.Width,tempLCL.Height), clBlack);
  with tempLCL do begin
  {$ELSE}
  temp := BGRABitmapFactory.Create(tx*sizeFactor, ty*sizeFactor, BGRABlack);
  with temp do begin
  {$ENDIF}
    Canvas.Font := Font;
    Canvas.Font.Orientation := 0;
    if Quality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB] then Canvas.Font.Height := Font.Height*CustomAntialiasingLevel
       else Canvas.Font.Height := Font.Height;
    Canvas.Font.Color := clWhite;
    BitmapTextRect({$IFDEF RENDER_TEXT_ON_TBITMAP}tempLCL{$ELSE}temp.Bitmap{$ENDIF}, rect(lim.Left-ARect.Left, lim.Top-ARect.Top,
         (ARect.Right-ARect.Left)*sizeFactor, (ARect.Bottom-ARect.Top)*sizeFactor),
         Point(round((xf - lim.Left)*sizeFactor), round((yf - lim.Top)*sizeFactor)), 
         sUTF8, style);
  end;
  {$IFDEF RENDER_TEXT_ON_TBITMAP}
  temp := BGRABitmapFactory.create(tempLCL,False);
  tempLCL.Free;
  {$ENDIF}

  FilterOriginalText(Quality,CustomAntialiasingLevel,temp,grayscale);
  BGRAInternalRenderText(bmp, Quality, grayscale,temp, lim.left,lim.top,0, c,tex);
  temp.Free;
  grayscale.Free;
end;

{ TBGRASystemFontRenderer }

{ Update font properties to internal TFont object }
procedure TBGRASystemFontRenderer.UpdateFont;
var fixedHeight: integer;
  fs: TFontStyles;
  patchedName: String;
begin
  patchedName := PatchSystemFontName(FontName);
  if FFont.Name <> patchedName then
    FFont.Name := patchedName;
  fs := FontStyle;
  if (OverrideUnderlineDecoration or (CompareText(Trim(patchedName),'FreeSans')=0) or
     (CompareText(Trim(patchedName),'FreeMono')=0) or (CompareText(Trim(patchedName),'FreeSerif')=0))
     and (fsUnderline in fs) then
  begin
    Exclude(fs, fsUnderline);
    FOwnUnderline := true;
  end else
    FOwnUnderline := false;
  if FFont.Style <> fs then
    FFont.Style := fs;
  if FontEmHeight < 0 then
    fixedHeight := FixSystemFontFullHeight(patchedName, FontEmHeight * FontEmHeightSign)
  else
    fixedHeight := FontEmHeight * FontEmHeightSign;
  if FFont.Height <> fixedHeight then
    FFont.Height := fixedHeight;
  if FontQuality = fqSystemClearType then
    FFont.Quality := fqCleartype
  else
    FFont.Quality := FontDefaultQuality;
end;

function TBGRASystemFontRenderer.InternalTextSize(sUTF8: string; 
  AShowPrefix: boolean): TSize;
begin
  result := InternalTextSizeAngle(sUTF8, AShowPrefix, FontOrientation);
end;

function TBGRASystemFontRenderer.InternalTextSizeAngle(sUTF8: string; 
  AShowPrefix: boolean; AOrientation: integer): TSize;
begin
  if AShowPrefix then sUTF8 := RemovePrefix(sUTF8);
  result := BGRAText.BGRATextSizeAngle(FFont, AOrientation, FontQuality,
                                       sUTF8, FontAntialiasingLevel);
  if (result.cy >= 24) 
   and (FontQuality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB]) 
   and (FontAntialiasingLevel > 4) then
    result := BGRAText.BGRATextSizeAngle(FFont, AOrientation, FontQuality,
                                         sUTF8, 4);
end;

procedure TBGRASystemFontRenderer.SplitText(var ATextUTF8: string;
  AMaxWidth: integer; out ARemainsUTF8: string);
var WordBreakHandler: TWordBreakHandler;
begin
  UpdateFont;
  if Assigned(FWordBreakHandler) then
    WordBreakHandler := FWordBreakHandler
  else
    WordBreakHandler := @DefaultWorkBreakHandler;

  InternalSplitText(ATextUTF8, AMaxWidth, ARemainsUTF8, WordBreakHandler);
end;

function TBGRASystemFontRenderer.GetFontPixelMetric: TFontPixelMetric;
begin
  UpdateFont;
  result := InternalGetFontPixelMetric;
end;

function TBGRASystemFontRenderer.FontExists(AName: string): boolean;
{$IFDEF LCL}
var
  i: Integer;
{$ENDIF}
begin
  {$IFDEF LCL}
  for i := 0 to Screen.Fonts.Count-1 do
    if CompareText(Screen.Fonts[i], AName) = 0 then exit(true);
  result := false;
  {$ELSE}
  result := true;
  {$ENDIF}
end;

class function TBGRASystemFontRenderer.PatchSystemFontName(AName: string): string;
begin
  if AName = 'serif' then
    result := {$IFDEF DARWIN}'Times'{$ELSE}'serif'{$ENDIF}
  else if AName = 'monospace' then
    result := {$IFDEF DARWIN}'Courier'{$ELSE}{$IFDEF LINUX}'DejaVu Sans Mono'{$ELSE}'monospace'{$ENDIF}{$ENDIF}
  else result := AName;
end;

procedure TBGRASystemFontRenderer.TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer;
  sUTF8: string; c: TBGRAPixel; align: TAlignment);
begin
  UpdateFont;
  InternalTextOutAngle(ADest,x,y,orientationTenthDegCCW,sUTF8,c,nil,align,false,false);
end;

procedure TBGRASystemFontRenderer.TextOutAngle(ADest: TBGRACustomBitmap; x,
  y: single; orientationTenthDegCCW: integer; sUTF8: string; c: TBGRAPixel;
  align: TAlignment; ARightToLeft: boolean);
begin
  UpdateFont;
  InternalTextOutAngle(ADest,x,y,orientationTenthDegCCW,sUTF8,c,nil,align,false,ARightToLeft);
end;

procedure TBGRASystemFontRenderer.TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer;
  sUTF8: string; texture: IBGRAScanner; align: TAlignment);
begin
  UpdateFont;
  InternalTextOutAngle(ADest,x,y,orientationTenthDegCCW,sUTF8,BGRAPixelTransparent,texture,align,false,false);
end;

procedure TBGRASystemFontRenderer.TextOutAngle(ADest: TBGRACustomBitmap; x,
  y: single; orientationTenthDegCCW: integer; sUTF8: string;
  texture: IBGRAScanner; align: TAlignment; ARightToLeft: boolean);
begin
  UpdateFont;
  InternalTextOutAngle(ADest,x,y,orientationTenthDegCCW,sUTF8,BGRAPixelTransparent,texture,align,false,ARightToLeft);
end;

procedure TBGRASystemFontRenderer.TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string;
  texture: IBGRAScanner; align: TAlignment);
begin
  UpdateFont;
  InternalTextOut(ADest, x,y, sUTF8, BGRAPixelTransparent,texture, align);
end;

procedure TBGRASystemFontRenderer.TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; c: TBGRAPixel;
  align: TAlignment);
begin
  UpdateFont;
  InternalTextOut(ADest, x,y, sUTF8, c,nil, align);
end;

procedure TBGRASystemFontRenderer.TextOut(ADest: TBGRACustomBitmap; x,
  y: single; sUTF8: string; texture: IBGRAScanner; align: TAlignment;
  ARightToLeft: boolean);
begin
  UpdateFont;
  InternalTextOut(ADest, x,y, sUTF8, BGRAPixelTransparent,texture, align,
                False, ARightToLeft);
end;

procedure TBGRASystemFontRenderer.TextOut(ADest: TBGRACustomBitmap; x,
  y: single; sUTF8: string; c: TBGRAPixel; align: TAlignment;
  ARightToLeft: boolean);
begin
  UpdateFont;
  InternalTextOut(ADest, x,y, sUTF8, c,nil, align, false, ARightToLeft);
end;

procedure TBGRASystemFontRenderer.TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string;
  style: TTextStyle; c: TBGRAPixel);
begin
  UpdateFont;
  InternalTextRect(ADest,ARect,x,y,sUTF8,style,c,nil);
end;

procedure TBGRASystemFontRenderer.TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string;
  style: TTextStyle; texture: IBGRAScanner);
begin
  UpdateFont;
  InternalTextRect(ADest,ARect,x,y,sUTF8,style,BGRAPixelTransparent,texture);
end;

procedure TBGRASystemFontRenderer.TextWordBreak(ADest: TBGRACustomBitmap;
  AText: string; x, y, AMaxWidth: integer; AColor: TBGRAPixel;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; ARightToLeft: boolean);
begin
  UpdateFont;
  InternalTextWordBreak(ADest,AText,x,y,AMaxWidth,AColor,nil,AHorizAlign,AVertAlign,ARightToLeft);
end;

procedure TBGRASystemFontRenderer.TextWordBreak(ADest: TBGRACustomBitmap;
  AText: string; x, y, AMaxWidth: integer; ATexture: IBGRAScanner;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; ARightToLeft: boolean);
begin
  UpdateFont;
  InternalTextWordBreak(ADest,AText,x,y,AMaxWidth,BGRAPixelTransparent,ATexture,AHorizAlign,AVertAlign,ARightToLeft);
end;

procedure TBGRASystemFontRenderer.InternalTextWordBreak(
  ADest: TBGRACustomBitmap; ATextUTF8: string; x, y, AMaxWidth: integer;
  AColor: TBGRAPixel; ATexture: IBGRAScanner; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; ARightToLeft: boolean);
var remains, part, curText,nextText: string;
  stepX,stepY: integer;
  lines: TStringList;
  i: integer;
  lineShift: single;
  WordBreakHandler: TWordBreakHandler;
  lineEndingBreak: boolean;
  bidiLayout: TBidiTextLayout;
  bidiAlign: TBidiTextAlignment;
begin
  if (ATextUTF8 = '') or (AMaxWidth <= 0) then exit;

  if Assigned(FWordBreakHandler) then
    WordBreakHandler := FWordBreakHandler
  else
    WordBreakHandler := @DefaultWorkBreakHandler;

  if ContainsBidiIsolateOrFormattingUTF8(ATextUTF8) or
    (pos(UTF8_LINE_SEPARATOR, ATextUTF8) <> 0) then
  begin
    bidiLayout := TBidiTextLayout.Create(self, ATextUTF8, ARightToLeft);
    bidiLayout.WordBreakHandler:= WordBreakHandler;
    bidiLayout.AvailableWidth := AMaxWidth;
    case AHorizAlign of
      taLeftJustify: bidiAlign:= btaLeftJustify;
      taRightJustify: begin
        bidiAlign:= btaRightJustify;
        dec(x, AMaxWidth);
      end
      else
      begin
        bidiAlign:= btaCenter;
        dec(x, AMaxWidth div 2);
      end;
    end;
    for i := 0 to bidiLayout.ParagraphCount-1 do
      bidiLayout.ParagraphAlignment[i] := bidiAlign;
    case AVertAlign of
      tlBottom: bidiLayout.TopLeft := PointF(x, y - bidiLayout.TotalTextHeight);
      tlCenter: bidiLayout.TopLeft := PointF(x, y - bidiLayout.TotalTextHeight/2);
    end;
    if ATexture <> nil then bidiLayout.DrawText(ADest, ATexture)
    else bidiLayout.DrawText(ADest, AColor);
    bidiLayout.Free;
    exit;
  end;

  stepX := 0;
  stepY := TextSize('Hg').cy;

  lines := TStringList.Create;
  curText := ATextUTF8;
  repeat
    InternalSplitText(curText, AMaxWidth, remains, lineEndingBreak, WordBreakHandler);
    part := curText;
    if not lineEndingBreak then
      // append following direction to part
      case GetFirstStrongBidiClassUTF8(remains) of
        ubcLeftToRight: if ARightToLeft then AppendStr(part, UnicodeCharToUTF8(UNICODE_LEFT_TO_RIGHT_MARK));
        ubcRightToLeft,ubcArabicLetter: if not ARightToLeft then AppendStr(part, UnicodeCharToUTF8(UNICODE_RIGHT_TO_LEFT_MARK));
      end;
    lines.Add(part);
    // prefix next part with previous direction
    nextText := remains;
    if not lineEndingBreak then
      case GetLastStrongBidiClassUTF8(curText) of
        ubcLeftToRight: if ARightToLeft then nextText := UnicodeCharToUTF8(UNICODE_LEFT_TO_RIGHT_MARK) + nextText;
        ubcRightToLeft,ubcArabicLetter: if not ARightToLeft then nextText := UnicodeCharToUTF8(UNICODE_RIGHT_TO_LEFT_MARK) + nextText;
      end;
    curText := nextText;
  until remains = '';
  if AVertAlign = tlCenter then lineShift := lines.Count/2
  else if AVertAlign = tlBottom then lineShift := lines.Count
  else lineShift := 0;

  dec(X, round(stepX*lineShift));
  dec(Y, round(stepY*lineShift));
  for i := 0 to lines.Count-1 do
  begin
    InternalTextOut(ADest,x,y,lines[i],AColor,ATexture,AHorizAlign,false,ARightToLeft);
    inc(X, stepX);
    inc(Y, stepY);
  end;
  lines.Free;
end;

procedure TBGRASystemFontRenderer.InternalTextRect(ADest: TBGRACustomBitmap;
  ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; c: TBGRAPixel;
  ATexture: IBGRAScanner);
var
  oldOrientation: integer;
  previousClip, intersected: TRect;
  lines: TStringList;
  iStart,i,h: integer;
  availableWidth: integer;
begin
  if sUTF8='' then exit;
  previousClip := ADest.ClipRect;
  if style.Clipping then
  begin
    intersected := TRect.Intersect(previousClip, ARect);
    if intersected.IsEmpty then exit;
    ADest.ClipRect := intersected;
  end;
  if style.SystemFont then FFont.Name := 'default';

  if not (style.Alignment in[taCenter,taRightJustify]) then ARect.Left := x;
  if not (style.Layout in[tlCenter,tlBottom]) then ARect.top := y;
  if (ARect.Right <= ARect.Left) and style.Clipping then
  begin
    ADest.ClipRect := previousClip;
    exit;
  end;
  if style.Layout = tlCenter then Y := (ARect.Top+ARect.Bottom) div 2 else
  if style.Layout = tlBottom then Y := ARect.Bottom else
    Y := ARect.Top;
  if style.Alignment = taCenter then X := (ARect.Left+ARect.Right) div 2 else
  if style.Alignment = taRightJustify then X := ARect.Right else
    X := ARect.Left;
  oldOrientation := FontOrientation;
  FontOrientation := 0;
  if style.Wordbreak then
  begin
    if style.ShowPrefix then sUTF8 := RemovePrefix(sUTF8); //prefix not handled
    InternalTextWordBreak(ADest,sUTF8,X,Y,ARect.Right-ARect.Left,c,ATexture,
        style.Alignment,style.Layout,style.RightToLeft);
  end
  else
  begin
    lines := nil;
    iStart := 1;

    if not style.SingleLine then
    begin
      i := iStart;
      while i <= length(sUTF8) do
      begin
        if sUTF8[i] in[#13,#10] then
        begin
          if not assigned(lines) then lines := TStringList.Create;
          lines.add(copy(sUTF8,iStart,i-iStart));
          if (sUTF8[i]=#13) and (i < length(sUTF8)) and (sUTF8[i+1]=#10) then inc(i);
          iStart := i+1
        end;
        inc(i);
      end;
    end;

    if style.Alignment = taLeftJustify then
      availableWidth := ARect.Right-X
    else
      availableWidth := ARect.Right-ARect.Left;
    if availableWidth < 0 then availableWidth:= 0;

    if lines = nil then //only one line
    begin
      if style.Layout = tlCenter then dec(Y, InternalTextSize(sUTF8,style.ShowPrefix).cy div 2);
      if style.Layout = tlBottom then dec(Y, InternalTextSize(sUTF8,style.ShowPrefix).cy);
      if style.EndEllipsis then
        InternalTextOutEllipse(ADest,X,Y,availableWidth,sUTF8,c,ATexture,style.Alignment,
                        style.ShowPrefix,style.RightToLeft)
      else
        InternalTextOut(ADest,X,Y,sUTF8,c,ATexture,style.Alignment,
                        style.ShowPrefix,style.RightToLeft);
    end else
    begin    //multiple lines
      lines.add(copy(sUTF8, iStart, length(sUTF8)-iStart+1));
      h := InternalTextSize('Hg',False).cy;
      if style.Layout = tlCenter then dec(Y, h*lines.Count div 2);
      if style.Layout = tlBottom then dec(Y, h*lines.Count);
      for i := 0 to lines.Count-1 do
      begin
        if style.EndEllipsis then
          InternalTextOutEllipse(ADest,X,Y,availableWidth,lines[i],c,ATexture,style.Alignment,
                          style.ShowPrefix,style.RightToLeft)
        else
          InternalTextOut(ADest,X,Y,lines[i],c,ATexture,style.Alignment,
                          style.ShowPrefix,style.RightToLeft);
        inc(Y,h);
      end;
      lines.Free;
    end;

  end;

  FontOrientation := oldOrientation;
  if style.Clipping then
    ADest.ClipRect := previousClip;
end;

procedure TBGRASystemFontRenderer.InternalTextOut(ADest: TBGRACustomBitmap; x,
  y: single; sUTF8: string; c: TBGRAPixel; texture: IBGRAScanner;
  align: TAlignment; AShowPrefix: boolean = false; ARightToLeft: boolean = false);
begin
  InternalTextOutAngle(ADest, x,y, FontOrientation, sUTF8, c, texture,
    align, ASHowPrefix, ARightToLeft);
end;

procedure TBGRASystemFontRenderer.InternalTextOutAngle(ADest: TBGRACustomBitmap; x,
  y: single; AOrientation: integer; sUTF8: string; c: TBGRAPixel; texture: IBGRAScanner;
  align: TAlignment; AShowPrefix: boolean = false; ARightToLeft: boolean = false);
var mode : TBGRATextOutImproveReadabilityMode;
  s: TSize;
  pts: ArrayOfTPointF;
  m: TAffineMatrix;
  i: Integer;
begin
  if sUTF8='' then exit;
  {$IF defined(LINUX) or defined(DARWIN)}
  //help LCL detect the correct direction
  case GetFirstStrongBidiClassUTF8(sUTF8) of
    ubcRightToLeft, ubcArabicLetter: if not ARightToLeft then sUTF8 := UnicodeCharToUTF8(UNICODE_LEFT_TO_RIGHT_MARK) + sUTF8;
    else
      begin //suppose left-to-right
        if ARightToLeft then sUTF8 := UnicodeCharToUTF8(UNICODE_RIGHT_TO_LEFT_MARK) + sUTF8;
      end;
  end;
  {$ENDIF}
  if Assigned(BGRATextOutImproveReadabilityProc) and 
   (FontQuality in[{$IFNDEF SYSTEM_RENDERER_IS_FINE}fqFineAntialiasing,{$ENDIF}
                   fqFineClearTypeBGR,fqFineClearTypeRGB]) and 
   (AOrientation mod 3600 = 0) then
  begin
    case FontQuality of
      fqFineClearTypeBGR: mode := irClearTypeBGR;
      fqFineClearTypeRGB: mode := irClearTypeRGB;
    else
      mode := irNormal;
    end;
    if AShowPrefix then sUTF8 := RemovePrefix(sUTF8); //prefix not handled
    BGRATextOutImproveReadabilityProc(ADest,FFont,x,y,sUTF8,c,texture,align,mode);
  end else
  begin
    if AOrientation = 0 then
      BGRAText.BGRATextOut(ADest,FFont,FontQuality,x,y,sUTF8,c,texture,align,
        0,AShowPrefix,ARightToLeft)
    else
      BGRAText.BGRATextOutAngle(ADest,FFont,FontQuality,x,y,AOrientation,
        sUTF8,c,texture,align,0);
  end;
  if FOwnUnderline then
  begin
    s := InternalTextSizeAngle(sUTF8, AShowPrefix, AOrientation);
    pts := BGRATextUnderline(PointF(x,y),s.cx,InternalGetFontPixelMetric);
    if AOrientation mod 3600 <> 0 then
    begin
      m := AffineMatrixTranslation(x,y)*
           AffineMatrixRotationDeg(-AOrientation/10)*
           AffineMatrixTranslation(-x,-y);
      for i := 0 to high(pts) do
        pts[i] := m*pts[i];
    end;
    if texture<>nil then
      ADest.FillPolyAntialias(pts, texture, false)
    else
      ADest.FillPolyAntialias(pts, c, false);
  end;
end;

procedure TBGRASystemFontRenderer.InternalTextOutEllipse(
  ADest: TBGRACustomBitmap; x, y, availableWidth: single; sUTF8: string;
  c: TBGRAPixel; texture: IBGRAScanner; align: TAlignment;
  AShowPrefix: boolean; ARightToLeft: boolean);
var remain: string;
begin
  if sUTF8='' then exit;
  if InternalTextSize(sUTF8,AShowPrefix).cx > availableWidth then
  begin
    InternalSplitText(sUTF8, round(availableWidth - InternalTextSize('...',AShowPrefix).cx), remain, nil);
    AppendStr(sUTF8, '...');
  end;
  InternalTextOut(ADest,x,y,sUTF8,c,texture,align,AShowPrefix,ARightToLeft);
end;

procedure TBGRASystemFontRenderer.InternalSplitText(var ATextUTF8: string;
  AMaxWidth: integer; out ARemainsUTF8: string; out ALineEndingBreak: boolean; AWordBreak: TWordBreakHandler);
var p,skipCount, charLen: integer;
  zeroWidth: boolean;
  u: LongWord;
begin
  ALineEndingBreak:= false;
  if ATextUTF8= '' then
  begin
    ARemainsUTF8 := '';
    exit;
  end;
  if RemoveLineEnding(ATextUTF8,1) then
  begin
    ARemainsUTF8:= ATextUTF8;
    ATextUTF8 := '';
    ALineEndingBreak:= true;
    exit;
  end;
  if InternalTextSize(ATextUTF8, false).cx <= AMaxWidth then
  begin
    for p := 1 to length(ATextUTF8) do
    begin
      if RemoveLineEnding(ATextUTF8,p) then
      begin
        ARemainsUTF8:= copy(ATextUTF8,p,length(ATextUTF8)-p+1);
        ATextUTF8 := copy(ATextUTF8,1,p-1);
        ALineEndingBreak:= true;
        exit;
      end;
    end;
    ARemainsUTF8 := '';
    exit;
  end;

  if AMaxWidth <= 0 then
    skipCount := 0
  else
    skipCount := BGRATextFitInfo(FFont, FontQuality, ATextUTF8, FontAntialiasingLevel, AMaxWidth);

  if skipCount <= 0 then skipCount := 1;

  p := 1;
  zeroWidth := true;
  repeat
    charLen := UTF8CharacterLength(@ATextUTF8[p]);
    u := UTF8CodepointToUnicode(@ATextUTF8[p], charLen);
    if not IsZeroWidthUnicode(u) then
      zeroWidth:= false;
    inc(p, charLen); //UTF8 chars may be more than 1 byte long
    dec(skipCount);

    if RemoveLineEnding(ATextUTF8,p) then
    begin
      ARemainsUTF8:= copy(ATextUTF8,p,length(ATextUTF8)-p+1);
      ATextUTF8 := copy(ATextUTF8,1,p-1);
      ALineEndingBreak:= true;
      exit;
    end;
  until ((skipCount <= 0) and not zeroWidth) or (p >= length(ATextUTF8)+1);

  ARemainsUTF8:= copy(ATextUTF8,p,length(ATextUTF8)-p+1);
  ATextUTF8 := copy(ATextUTF8,1,p-1); //this includes the whole last UTF8 char
  if Assigned(AWordBreak) then AWordBreak(ATextUTF8,ARemainsUTF8);
end;

procedure TBGRASystemFontRenderer.InternalSplitText(var ATextUTF8: string;
  AMaxWidth: integer; out ARemainsUTF8: string; AWordBreak: TWordBreakHandler);
var lineEndingBreak: boolean;
begin
  InternalSplitText(ATextUTF8,AMaxWidth,ARemainsUTF8,lineEndingBreak,AWordBreak);
end;

function TBGRASystemFontRenderer.InternalGetFontPixelMetric: TFontPixelMetric;
var fxFont: TFont;
begin
  if FontQuality in[fqSystem,fqSystemClearType] then
    result := GetLCLFontPixelMetric(FFont)
  else
  begin
    FxFont := TFont.Create;
    FxFont.Assign(FFont);
    FxFont.Height := fxFont.Height*FontAntialiasingLevel;
    Result:= GetLCLFontPixelMetric(FxFont);
    if Result.Baseline <> -1 then Result.Baseline:= round((Result.Baseline-1)/FontAntialiasingLevel);
    if Result.CapLine <> -1 then Result.CapLine:= round(Result.CapLine/FontAntialiasingLevel);
    if Result.DescentLine <> -1 then Result.DescentLine:= round((Result.DescentLine-1)/FontAntialiasingLevel);
    if Result.Lineheight <> -1 then Result.Lineheight:= round(Result.Lineheight/FontAntialiasingLevel);
    if Result.xLine <> -1 then Result.xLine:= round(Result.xLine/FontAntialiasingLevel);
    FxFont.Free;
  end;
end;

procedure TBGRASystemFontRenderer.DefaultWorkBreakHandler(var ABeforeUTF8,
  AAfterUTF8: string);
begin
  BGRADefaultWordBreakHandler(ABeforeUTF8,AAfterUTF8);
end;

function TBGRASystemFontRenderer.TextSize(sUTF8: string): TSize;
var oldOrientation: integer;
begin
  oldOrientation:= FontOrientation;
  FontOrientation:= 0;
  UpdateFont;
  result := InternalTextSize(sUTF8,False);
  FontOrientation:= oldOrientation;
end;

function TBGRASystemFontRenderer.TextSizeAngle(sUTF8: string;
  orientationTenthDegCCW: integer): TSize;
var oldOrientation: integer;
begin
  oldOrientation:= FontOrientation;
  FontOrientation:= orientationTenthDegCCW;
  UpdateFont;
  result := InternalTextSize(sUTF8,False);
  FontOrientation:= oldOrientation;
end;

function TBGRASystemFontRenderer.TextSize(sUTF8: string;
  AMaxWidth: integer; ARightToLeft: boolean): TSize;
var
  remains: string;
  h, i, w: integer;
  WordBreakHandler: TWordBreakHandler;
  layout: TBidiTextLayout;
begin
  UpdateFont;

  if Assigned(FWordBreakHandler) then
    WordBreakHandler := FWordBreakHandler
  else
    WordBreakHandler := @DefaultWorkBreakHandler;

  if ContainsBidiIsolateOrFormattingUTF8(sUTF8) then
  begin
    layout := TBidiTextLayout.Create(self, sUTF8, ARightToLeft);
    layout.WordBreakHandler:= WordBreakHandler;
    layout.AvailableWidth := AMaxWidth;
    for i := 0 to layout.ParagraphCount-1 do
      layout.ParagraphAlignment[i] := btaLeftJustify;
    result.cx := 0;
    for i := 0 to layout.PartCount-1 do
    begin
      w := ceil(layout.PartRectF[i].Right);
      if w > result.cx then result.cx := w;
    end;
    result.cy := ceil(layout.TotalTextHeight);
    layout.Free;
  end else
  begin
    result.cx := 0;
    result.cy := 0;
    h := InternalTextSize('Hg',False).cy;
    repeat
      InternalSplitText(sUTF8, AMaxWidth, remains, WordBreakHandler);
      with InternalTextSize(sUTF8, false) do
        if cx > result.cx then result.cx := cx;
      inc(result.cy, h);
      sUTF8 := remains;
    until remains = '';
  end;
end;

function TBGRASystemFontRenderer.TextFitInfo(sUTF8: string; AMaxWidth: integer
  ): integer;
begin
  UpdateFont;
  result := BGRATextFitInfo(FFont, FontQuality, sUTF8, FontAntialiasingLevel, AMaxWidth);
end;

constructor TBGRASystemFontRenderer.Create;
begin
  FFont := TFont.Create;
end;

destructor TBGRASystemFontRenderer.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

initialization

  fqFineClearType := @GetFineClearTypeAuto;

end.

