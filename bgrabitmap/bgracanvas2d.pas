// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRACanvas2D;

{ To do :

  draw text with a different precision if the matrix is scaled
  drawImage(in image, in double sx, in double sy, in double sw, in double sh, in double dx, in double dy, in double dw, in double dh)
  -> using FillPoly with texture coordinates
  linear gradient any transformation
  clearPath clipping
  createRadialGradient
  globalCompositeOperation
  image data functions
}

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRAGraphics, BGRABitmapTypes, BGRATransform,
  BGRAGradientScanner, BGRAPath, BGRAPen, BGRAGrayscaleMask;

type
  ArrayOfString = array of string;

  IBGRACanvasTextureProvider2D = interface
    function getTexture: IBGRAScanner;
    property texture: IBGRAScanner read GetTexture;
  end;

  IBGRACanvasGradient2D = interface(IBGRACanvasTextureProvider2D)
    procedure addColorStop(APosition: single; AColor: TBGRAPixel);
    procedure addColorStop(APosition: single; AColor: TColor);
    procedure addColorStop(APosition: single; AColor: string);
    procedure setColors(ACustomGradient: TBGRACustomGradient);
    function GetGammaCorrection: boolean;
    procedure SetGammaCorrection(AValue: boolean);
    function GetRepetition: TBGRAGradientRepetition;
    procedure SetRepetition(AValue: TBGRAGradientRepetition);
    property gammaCorrection: boolean read GetGammaCorrection write SetGammaCorrection;
    property repetition: TBGRAGradientRepetition read GetRepetition write SetRepetition;
  end;

  { TBGRACanvasTextureProvider2D }

  TBGRACanvasTextureProvider2D = class(TInterfacedObject,IBGRACanvasTextureProvider2D)
    function getTexture: IBGRAScanner; virtual; abstract;
  end;

  { TBGRACanvasState2D }

  TBGRACanvasState2D = class
  private
    FClipMask: TGrayscaleMask;
    FClipMaskOwned: boolean;
    function GetClipMaskReadWrite: TGrayscaleMask;
  public
    strokeColor: TBGRAPixel;
    strokeTextureProvider: IBGRACanvasTextureProvider2D;
    fillColor: TBGRAPixel;
    fillMode: TFillMode;
    fillTextureProvider: IBGRACanvasTextureProvider2D;
    globalAlpha: byte;

    fontName: string;
    fontStyle: TFontStyles;
    fontEmHeight: single;
    textAlign: TAlignment;
    textBaseline: string;
    textDirection: TFontBidiMode;

    lineWidth: single;
    penStroker: TBGRAPenStroker;

    shadowOffsetX,shadowOffsetY,shadowBlur: single;
    shadowColor: TBGRAPixel;
    shadowFastest: boolean;

    matrix: TAffineMatrix;
    constructor Create(AMatrix: TAffineMatrix; AClipMask: TGrayscaleMask; AClipMaskOwned: boolean);
    function Duplicate: TBGRACanvasState2D;
    destructor Destroy; override;
    procedure transform(AMatrix: TAffineMatrix);
    procedure SetClipMask(AClipMask: TGrayscaleMask; AOwned: boolean);
    property clipMaskReadOnly: TGrayscaleMask read FClipMask;
    property clipMaskReadWrite: TGrayscaleMask read GetClipMaskReadWrite;
  end;

  TCanvas2dTextSize = record
    width,height: single;
  end;

  { TBGRACanvas2D }

  TBGRACanvas2D = class(IBGRAPath)
  private
    FSurface: TBGRACustomBitmap;
    StateStack: TList;
    currentState: TBGRACanvasState2D;
    FCanvasOffset: TPointF;
    FPixelCenteredCoordinates: boolean;
    FPathPoints: array of TPointF;
    FPathPointCount: integer;
    FTextPaths: array of record
        Text: string;
        FontName: string;
        FontMatrix: TAffineMatrix;
        FontAlign: TAlignment;
        FontAnchor: TFontVerticalAnchor;
        FontStyle: TFontStyles;
        TextDirection: TFontBidiMode;
      end;
    FFontRenderer: TBGRACustomFontRenderer;
    FLastCoord, FStartCoord: TPointF;
    function GetCurrentPathAsPoints: ArrayOfTPointF;
    function GetTextDirection: TFontBidiMode;
    function GetFontName: string;
    function GetFontRenderer: TBGRACustomFontRenderer;
    function GetFontEmHeight: single;
    function GetFontString: string;
    function GetFontStyle: TFontStyles;
    function GetGlobalAlpha: single;
    function GetHasShadow: boolean;
    function GetHeight: Integer;
    function GetLineCap: string;
    function GetLineCapLCL: TPenEndCap;
    function GetlineJoin: string;
    function GetlineJoinLCL: TPenJoinStyle;
    function GetLineWidth: single;
    function GetMatrix: TAffineMatrix;
    function GetMiterLimit: single;
    function GetPixelCenteredCoordinates: boolean;
    function GetShadowBlur: single;
    function GetShadowFastest: boolean;
    function GetShadowOffset: TPointF;
    function GetShadowOffsetX: single;
    function GetShadowOffsetY: single;
    function GetStrokeMatrix: TAffineMatrix;
    function GetTextAlign: string;
    function GetTextAlignLCL: TAlignment;
    function GetTextBaseline: string;
    function GetFillMode: TFillMode;
    function GetWidth: Integer;
    procedure SetTextDirection(AValue: TFontBidiMode);
    procedure SetFontName(AValue: string);
    procedure SetFontRenderer(AValue: TBGRACustomFontRenderer);
    procedure SetFontEmHeight(AValue: single);
    procedure SetFontString(AValue: string);
    procedure SetFontStyle(AValue: TFontStyles);
    procedure SetGlobalAlpha(const AValue: single);
    procedure SetLineCap(const AValue: string);
    procedure SetLineCapLCL(AValue: TPenEndCap);
    procedure SetLineJoin(const AValue: string);
    procedure FillPoly(const points: array of TPointF);
    procedure FillStrokePoly(const points: array of TPointF; fillOver: boolean);
    procedure FillTexts(AErase: boolean);
    procedure SetLineJoinLCL(AValue: TPenJoinStyle);
    procedure SetLineWidth(const AValue: single);
    procedure SetMatrix(AValue: TAffineMatrix);
    procedure SetMiterLimit(const AValue: single);
    procedure SetPixelCenteredCoordinates(const AValue: boolean);
    procedure SetShadowBlur(const AValue: single);
    procedure SetShadowFastest(AValue: boolean);
    procedure SetShadowOffset(const AValue: TPointF);
    procedure SetShadowOffsetX(const AValue: single);
    procedure SetShadowOffsetY(const AValue: single);
    procedure SetStrokeMatrix(AValue: TAffineMatrix);
    procedure SetTextAlign(AValue: string);
    procedure SetTextAlignLCL(AValue: TAlignment);
    procedure SetTextBaseline(AValue: string);
    procedure SetFillMode(mode: TFillMode);
    procedure StrokePoly(const points: array of TPointF);
    procedure DrawShadow(const points, points2: array of TPointF; AFillMode: TFillMode = fmWinding);
    procedure DrawShadowMask(X,Y: integer; AMask: TCustomUniversalBitmap; AMaskOwned: boolean);
    procedure ClearPoly(const points: array of TPointF);
    function ApplyTransform(const points: array of TPointF; matrix: TAffineMatrix): ArrayOfTPointF; overload;
    function ApplyTransform(const points: array of TPointF): ArrayOfTPointF; overload;
    function ApplyTransform(point: TPointF): TPointF; overload;
    function GetPenPos(defaultX, defaultY: single): TPointF;
    function GetPenPos(defaultPt: TPointF): TPointF;
    procedure AddPoint(point: TPointF);
    procedure AddPoints(const points: array of TPointF);
    procedure AddPointsRev(const points: array of TPointF);
    function ApplyGlobalAlpha(color: TBGRAPixel): TBGRAPixel;
    function GetDrawMode: TDrawMode;
    procedure copyTo({%H-}dest: IBGRAPath); //IBGRAPath
    function getPoints: ArrayOfTPointF; //IBGRAPath
    function getPoints(AMatrix: TAffineMatrix): ArrayOfTPointF; //IBGRAPath
    function getCursor: TBGRACustomPathCursor; //IBGRAPath
  public
    antialiasing, linearBlend, gradientGammaCorrection: boolean;
    constructor Create(ASurface: TBGRACustomBitmap);
    destructor Destroy; override;

    function toDataURL(mimeType: string = 'image/png'): string;

    procedure save;
    procedure restore;
    procedure scale(x,y: single); overload;
    procedure scale(factor: single); overload;
    procedure rotate(angleRadCW: single);
    procedure translate(x,y: single);
    procedure skewx(angleRadCW: single);
    procedure skewy(angleRadCW: single);
    procedure transform(m11,m21, m12,m22, m13,m23: single); overload;
    procedure transform(AMatrix: TAffineMatrix); overload;
    procedure setTransform(m11,m21, m12,m22, m13,m23: single);
    procedure resetTransform;

    procedure strokeScale(x,y: single);
    procedure strokeSkewx(angleRadCW: single);
    procedure strokeSkewy(angleRadCW: single);
    procedure strokeResetTransform;

    procedure strokeStyle(color: TBGRAPixel); overload;
    procedure strokeStyle(color: TColor); overload;
    procedure strokeStyle(color: string); overload;
    procedure strokeStyle(texture: IBGRAScanner); overload;
    procedure strokeStyle(provider: IBGRACanvasTextureProvider2D); overload;
    procedure fillStyle(color: TBGRAPixel); overload;
    procedure fillStyle(color: TColor); overload;
    procedure fillStyle(color: string); overload;
    procedure fillStyle(texture: IBGRAScanner); overload;
    procedure fillStyle(provider: IBGRACanvasTextureProvider2D); overload;
    procedure shadowColor(color: TBGRAPixel); overload;
    procedure shadowColor(color: TColor); overload;
    procedure shadowColor(color: string); overload;
    procedure shadowNone;
    function getShadowColor: TBGRAPixel;

    function createLinearGradient(x0,y0,x1,y1: single): IBGRACanvasGradient2D; overload;
    function createLinearGradient(p0,p1: TPointF): IBGRACanvasGradient2D; overload;
    function createLinearGradient(x0,y0,x1,y1: single; Colors: TBGRACustomGradient): IBGRACanvasGradient2D; overload;
    function createLinearGradient(p0,p1: TPointF; Colors: TBGRACustomGradient): IBGRACanvasGradient2D; overload;

    function createRadialGradient(x0,y0,r0,x1,y1,r1: single; flipGradient: boolean=false): IBGRACanvasGradient2D; overload;
    function createRadialGradient(p0: TPointF; r0: single; p1: TPointF; r1: single; flipGradient: boolean=false): IBGRACanvasGradient2D; overload;
    function createRadialGradient(x0,y0,r0,x1,y1,r1: single; Colors: TBGRACustomGradient; flipGradient: boolean=false): IBGRACanvasGradient2D; overload;
    function createRadialGradient(p0: TPointF; r0: single; p1: TPointF; r1: single; Colors: TBGRACustomGradient; flipGradient: boolean=false): IBGRACanvasGradient2D; overload;

    function createPattern(image: TBGRACustomBitmap; repetition: string): IBGRACanvasTextureProvider2D; overload;
    function createPattern(texture: IBGRAScanner): IBGRACanvasTextureProvider2D; overload;

    procedure fillRect(x,y,w,h: single);
    procedure strokeRect(x,y,w,h: single);
    procedure clearRect(x,y,w,h: single);

    procedure addPath(APath: IBGRAPath); overload;
    procedure addPath(ASvgPath: string); overload;
    procedure path(APath: IBGRAPath); overload;
    procedure path(ASvgPath: string); overload;
    procedure beginPath;
    procedure closePath;
    procedure toSpline(closed: boolean; style: TSplineStyle= ssOutside);
    procedure moveTo(x,y: single); overload;
    procedure lineTo(x,y: single); overload;
    procedure moveTo(constref pt: TPointF); overload;
    procedure lineTo(constref pt: TPointF); overload;
    procedure polylineTo(const pts: array of TPointF);
    procedure quadraticCurveTo(cpx,cpy,x,y: single); overload;
    procedure quadraticCurveTo(constref cp,pt: TPointF); overload;
    procedure bezierCurveTo(cp1x,cp1y,cp2x,cp2y,x,y: single); overload;
    procedure bezierCurveTo(constref cp1,cp2,pt: TPointF); overload;
    procedure rect(x,y,w,h: single);
    procedure roundRect(x,y,w,h,radius: single); overload;
    procedure roundRect(x,y,w,h,rx,ry: single); overload;
    procedure openedSpline(const pts: array of TPointF; style: TSplineStyle);
    procedure closedSpline(const pts: array of TPointF; style: TSplineStyle);
    procedure spline(const pts: array of TPointF; style: TSplineStyle= ssOutside);
    procedure splineTo(const pts: array of TPointF; style: TSplineStyle= ssOutside);
    procedure arc(x, y, radius, startAngleRadCW, endAngleRadCW: single; anticlockwise: boolean); overload;
    procedure arc(x, y, radius, startAngleRadCW, endAngleRadCW: single); overload;
    procedure arc(cx, cy, rx,ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single; anticlockwise: boolean); overload;
    procedure arc(cx, cy, rx,ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single); overload;
    procedure arc(constref arcDef: TArcDef); overload;
    procedure arcTo(x1, y1, x2, y2, radius: single); overload;
    procedure arcTo(p1,p2: TPointF; radius: single); overload;
    procedure arcTo(rx, ry, xAngleRadCW: single; largeArc,anticlockwise: boolean; x, y: single);
    procedure circle(x,y,r: single);
    procedure ellipse(x,y,rx,ry: single);
    procedure text(AText: string; x,y: single);
    procedure fillText(AText: string; x,y: single);
    procedure strokeText(AText: string; x,y: single);
    function measureText(AText: string): TCanvas2dTextSize;

    procedure fill; overload;
    procedure fill(AFillProc: TBGRAPathFillProc; AData: pointer); overload;
    procedure fill(AFillProc: TBGRAPathFillProc; const AMatrix: TAffineMatrix; AData: pointer); overload; //may not render curve nicely
    procedure stroke; overload;
    procedure stroke(ADrawProc: TBGRAPathDrawProc; AData: pointer); overload;
    procedure stroke(ADrawProc: TBGRAPathDrawProc; const AMatrix: TAffineMatrix; AData: pointer); overload; //may not render curve nicely
    procedure fillOverStroke;
    procedure strokeOverFill;
    procedure clearPath;
    procedure clip;
    procedure unclip;
    function isPointInPath(x,y: single): boolean; overload;
    function isPointInPath(pt: TPointF): boolean; overload;

    procedure drawImage(image: TBGRACustomBitmap; dx,dy: single; AFilter: TResampleFilter = rfLinear); overload;
    procedure drawImage(image: TBGRACustomBitmap; dx,dy,dw,dh: single; AFilter: TResampleFilter = rfLinear); overload;

    function getLineStyle: TBGRAPenStyle;
    procedure lineStyle(const AValue: array of single); overload;
    procedure lineStyle(AStyle: TPenStyle); overload;

    class function StrToFontNameList(AText: string): ArrayOfString;
    class function FontNameListToStr(AList: ArrayOfString): string;
    class function CSSFontNameToLCL(AName: string): string;

    property surface: TBGRACustomBitmap read FSurface;
    property width: Integer read GetWidth;
    property height: Integer read GetHeight;
    property pixelCenteredCoordinates: boolean read GetPixelCenteredCoordinates write SetPixelCenteredCoordinates;
    property globalAlpha: single read GetGlobalAlpha write SetGlobalAlpha;
    property matrix: TAffineMatrix read GetMatrix write SetMatrix;
    property strokeMatrix: TAffineMatrix read GetStrokeMatrix write SetStrokeMatrix;

    property lineWidth: single read GetLineWidth write SetLineWidth;
    property lineCap: string read GetLineCap write SetLineCap;
    property lineCapLCL: TPenEndCap read GetLineCapLCL write SetLineCapLCL;
    property lineJoin: string read GetlineJoin write SetLineJoin;
    property lineJoinLCL: TPenJoinStyle read GetlineJoinLCL write SetLineJoinLCL;
    property miterLimit: single read GetMiterLimit write SetMiterLimit;

    property shadowOffsetX: single read GetShadowOffsetX write SetShadowOffsetX;
    property shadowOffsetY: single read GetShadowOffsetY write SetShadowOffsetY;
    property shadowOffset: TPointF read GetShadowOffset write SetShadowOffset;
    property shadowBlur: single read GetShadowBlur write SetShadowBlur;
    property shadowFastest: boolean read GetShadowFastest write SetShadowFastest;
    property hasShadow: boolean read GetHasShadow;

    property fontName: string read GetFontName write SetFontName;
    property fontEmHeight: single read GetFontEmHeight write SetFontEmHeight;
    property fontStyle: TFontStyles read GetFontStyle write SetFontStyle;
    property font: string read GetFontString write SetFontString;
    property textAlignLCL: TAlignment read GetTextAlignLCL write SetTextAlignLCL;
    property textAlign: string read GetTextAlign write SetTextAlign;
    property textBaseline: string read GetTextBaseline write SetTextBaseline;
    property direction: TFontBidiMode read GetTextDirection write SetTextDirection;
    
    property fillMode: TFillMode read GetFillMode write SetFillMode;

    property currentPath: ArrayOfTPointF read GetCurrentPathAsPoints;
    property fontRenderer: TBGRACustomFontRenderer read GetFontRenderer write SetFontRenderer;

  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
  end;

implementation

uses Math, BGRAFillInfo, BGRAPolygon, BGRABlend, FPWriteJPEG, FPWriteBMP, base64;

type
  TColorStop = record
    position: single;
    color: TBGRAPixel;
  end;

  TGradientArrayOfColors = array of TBGRAPixel;
  TGradientArrayOfPositions = array of single;

  { TBGRACanvasGradient2D }

  TBGRACanvasGradient2D = class(TBGRACanvasTextureProvider2D, IBGRACanvasGradient2D)
  private
    colorStops: array of TColorStop;
    nbColorStops: integer;
    FCustomGradient: TBGRACustomGradient;
    FGammaCorrection: boolean;
    FRepetition: TBGRAGradientRepetition;
  protected
    scanner: TBGRAGradientScanner;
    procedure CreateScanner; virtual; abstract;
    function getColorArray: TGradientArrayOfColors;
    function getPositionArray: TGradientArrayOfPositions;
    procedure GetBGRAGradient(out ABGRAGradient: TBGRACustomGradient; out AOwned: boolean);
    function GetGammaCorrection: boolean;
    procedure SetGammaCorrection(AValue: boolean);
    function GetRepetition: TBGRAGradientRepetition;
    procedure SetRepetition(AValue: TBGRAGradientRepetition);
  public
    constructor Create;
    function getTexture: IBGRAScanner; override;
    destructor Destroy; override;
    procedure addColorStop(APosition: single; AColor: TBGRAPixel);
    procedure addColorStop(APosition: single; AColor: TColor);
    procedure addColorStop(APosition: single; AColor: string);
    procedure setColors(ACustomGradient: TBGRACustomGradient);
    property texture: IBGRAScanner read GetTexture;
    property colorStopCount: integer read nbColorStops;
    property gammaCorrection: boolean read GetGammaCorrection write SetGammaCorrection;
    property repetition: TBGRAGradientRepetition read GetRepetition write SetRepetition;
  end;

  { TBGRACanvasLinearGradient2D }

  TBGRACanvasLinearGradient2D = class(TBGRACanvasGradient2D)
  protected
    o1,o2: TPointF;
    FTransform: TAffineMatrix;
    procedure CreateScanner; override;
  public
    constructor Create(x0,y0,x1,y1: single; transform: TAffineMatrix);
    constructor Create(p0,p1: TPointF; transform: TAffineMatrix);
  end;

  { TBGRACanvasRadialGradient2D }

  TBGRACanvasRadialGradient2D = class(TBGRACanvasGradient2D)
  protected
    c0,c1: TPointF;
    cr0,cr1: single;
    FFlipGradient: boolean;
    FTransform: TAffineMatrix;
    procedure CreateScanner; override;
  public
    constructor Create(x0,y0,r0,x1,y1,r1: single; transform: TAffineMatrix; flipGradient: boolean=false);
    constructor Create(p0: TPointF; r0: single; p1: TPointF; r1: single; transform: TAffineMatrix; flipGradient: boolean=false);
  end;

  { TBGRACanvasPattern2D }

  TBGRACanvasPattern2D = class(TBGRACanvasTextureProvider2D)
  protected
    scanner: TBGRACustomScanner;
    foreignInterface: IBGRAScanner;
    ownScanner: boolean;
  public
    function getTexture: IBGRAScanner; override;
    constructor Create(source: TBGRACustomBitmap; repeatX,repeatY: boolean; Origin, HAxis, VAxis: TPointF);
    constructor Create(source: IBGRAScanner; transformation: TAffineMatrix);
    destructor Destroy; override;
  end;

{ TBGRACanvasPattern2D }

function TBGRACanvasPattern2D.GetTexture: IBGRAScanner;
begin
  if ownScanner then
    result := scanner
  else
    result := foreignInterface;
end;

constructor TBGRACanvasPattern2D.Create(source: TBGRACustomBitmap; repeatX,
  repeatY: boolean; Origin, HAxis, VAxis: TPointF);
var
  affine: TBGRAAffineBitmapTransform;
begin
  if (abs(Origin.X-round(Origin.X)) < 1e-6) and
     (abs(Origin.Y-round(Origin.Y)) < 1e-6) and
     (HAxis = Origin+PointF(1,0)) and
     (VAxis = Origin+PointF(0,1)) then
  begin
    if (round(Origin.X)=0) and (round(Origin.Y)=0) and repeatX and repeatY then
    begin
      foreignInterface := source;
      ownScanner:= false;
    end else
    begin
      scanner := TBGRABitmapScanner.Create(source,repeatX,repeatY,Point(round(Origin.X),round(Origin.Y)));
      ownScanner := true;
    end;
  end
  else
  begin
    affine := TBGRAAffineBitmapTransform.Create(source,repeatX,repeatY);
    affine.Fit(Origin,HAxis,VAxis);
    scanner := affine;
    ownScanner:= true;
  end;
end;

constructor TBGRACanvasPattern2D.Create(source: IBGRAScanner;
  transformation: TAffineMatrix);
var
  affine : TBGRAAffineScannerTransform;
begin
  if (abs(transformation[1,1]-1) < 1e-6) and
     (abs(transformation[2,2]-1) < 1e-6) and
     (abs(transformation[1,2]) < 1e-6) and
     (abs(transformation[2,1]) < 1e-6) and
     (abs(transformation[1,3]-round(transformation[1,3])) < 1e-6) and
     (abs(transformation[2,3]-round(transformation[2,3])) < 1e-6) then
  begin
    if (abs(transformation[1,3]) < 1e-6) and
      (abs(transformation[2,3]) < 1e-6) then
    begin
      foreignInterface := source;
      ownScanner := false;
    end else
    begin
     scanner := TBGRAScannerOffset.Create(source,Point(round(transformation[1,3]),round(transformation[2,3])));
     ownScanner := true;
    end;
  end else
  begin
    affine := TBGRAAffineScannerTransform.Create(source);
    affine.Matrix := transformation;
    affine.Invert;
    scanner := affine;
    ownScanner:= true;
  end;
end;

destructor TBGRACanvasPattern2D.Destroy;
begin
  fillchar(foreignInterface,sizeof(foreignInterface),0);
  if ownScanner then FreeAndNil(scanner);
  inherited Destroy;
end;

{ TBGRACanvasLinearGradient2D }

procedure TBGRACanvasLinearGradient2D.CreateScanner;
var GradientOwner: boolean;
    GradientColors: TBGRACustomGradient;
begin
  GetBGRAGradient(GradientColors,GradientOwner);
  scanner := TBGRAGradientScanner.Create(GradientColors,gtLinear,o1,o2,False,GradientOwner);
  scanner.Transform := FTransform;
end;

constructor TBGRACanvasLinearGradient2D.Create(x0, y0, x1, y1: single; transform: TAffineMatrix);
begin
  o1 := PointF(x0,y0);
  o2 := PointF(x1,y1);
  FTransform := transform;
end;

constructor TBGRACanvasLinearGradient2D.Create(p0, p1: TPointF; transform: TAffineMatrix);
begin
  o1 := p0;
  o2 := p1;
  FTransform := transform;
end;

{ TBGRACanvasRadialGradient2D }

procedure TBGRACanvasRadialGradient2D.CreateScanner;
var GradientOwner: boolean;
    GradientColors: TBGRACustomGradient;
begin
  GetBGRAGradient(GradientColors,GradientOwner);
  scanner := TBGRAGradientScanner.Create(GradientColors,c0,cr0,c1,cr1,GradientOwner);
  scanner.FlipGradient := not FFlipGradient;
  scanner.Transform := FTransform;
end;

constructor TBGRACanvasRadialGradient2D.Create(x0, y0, r0, x1, y1, r1: single;
  transform: TAffineMatrix; flipGradient: boolean);
begin
  self.c0 := PointF(x0,y0);
  self.cr0 := r0;
  self.c1 := PointF(x1,y1);
  self.cr1 := r1;
  FTransform := transform;
  FFlipGradient := flipGradient;
end;

constructor TBGRACanvasRadialGradient2D.Create(p0: TPointF; r0: single;
  p1: TPointF; r1: single; transform: TAffineMatrix; flipGradient: boolean);
begin
  self.c0 := p0;
  self.cr0 := r0;
  self.c1 := p1;
  self.cr1 := r1;
  FTransform := transform;
  FFlipGradient := flipGradient;
end;

{ TBGRACanvasGradient2D }

function TBGRACanvasGradient2D.getTexture: IBGRAScanner;
begin
  if scanner = nil then CreateScanner;
  result := scanner;
end;

function TBGRACanvasGradient2D.GetGammaCorrection: boolean;
begin
  result := FGammaCorrection;
end;

procedure TBGRACanvasGradient2D.SetGammaCorrection(AValue: boolean);
begin
  FGammaCorrection:= AValue;
  FreeAndNil(scanner);
end;

constructor TBGRACanvasGradient2D.Create;
begin
  inherited Create;
  scanner := nil;
  FGammaCorrection:= false;
end;

function TBGRACanvasGradient2D.GetRepetition: TBGRAGradientRepetition;
begin
  result := FRepetition;
end;

procedure TBGRACanvasGradient2D.SetRepetition(
  AValue: TBGRAGradientRepetition);
begin
  FRepetition := AValue;
  FreeAndNil(scanner);
end;

function TBGRACanvasGradient2D.getColorArray: TGradientArrayOfColors;
var
  i: Integer;
begin
  setlength(result, nbColorStops);
  for i := 0 to nbColorStops-1 do
    result[i] := colorStops[i].color;
end;

function TBGRACanvasGradient2D.getPositionArray: TGradientArrayOfPositions;
var
  i: Integer;
begin
  setlength(result, nbColorStops);
  for i := 0 to nbColorStops-1 do
    result[i] := colorStops[i].position;
end;

procedure TBGRACanvasGradient2D.GetBGRAGradient(out
  ABGRAGradient: TBGRACustomGradient; out AOwned: boolean);
begin
  if FCustomGradient = nil then
  begin
    if (colorStopCount = 2) and (colorStops[0].position = 0) and (colorStops[1].position = 1) then
    begin
      if FGammaCorrection then
        ABGRAGradient := TBGRASimpleGradientWithGammaCorrection.Create(colorStops[0].color, colorStops[1].color, FRepetition)
      else
        ABGRAGradient := TBGRASimpleGradientWithoutGammaCorrection.Create(colorStops[0].color, colorStops[1].color, FRepetition);
    end
    else
      ABGRAGradient := TBGRAMultiGradient.Create(getColorArray,getPositionArray,FGammaCorrection, FRepetition = grRepeat);
    AOwned := true;
  end else
  begin
    ABGRAGradient := FCustomGradient;
    AOwned := false;
  end;
end;

destructor TBGRACanvasGradient2D.Destroy;
begin
  FreeAndNil(scanner);
  inherited Destroy;
end;

procedure TBGRACanvasGradient2D.addColorStop(APosition: single;
  AColor: TBGRAPixel);
begin
  FreeAndNil(scanner);
  if nbColorStops = length(colorStops) then
    setlength(colorStops, (length(colorStops)+1)*2);

  with colorStops[nbColorStops] do
  begin
    position := APosition;
    color := AColor;
  end;
  inc(nbColorStops);
end;

procedure TBGRACanvasGradient2D.addColorStop(APosition: single; AColor: TColor
  );
begin
  addColorStop(APosition, ColorToBGRA(AColor));
end;

procedure TBGRACanvasGradient2D.addColorStop(APosition: single; AColor: string
  );
begin
  addColorStop(APosition, StrToBGRA(AColor));
end;

procedure TBGRACanvasGradient2D.setColors(ACustomGradient: TBGRACustomGradient
  );
begin
  FCustomGradient := ACustomGradient;
end;

{ TBGRACanvasState2D }

function TBGRACanvasState2D.GetClipMaskReadWrite: TGrayscaleMask;
begin
  if not FClipMaskOwned then
  begin
    if FClipMask <> nil then
      FClipMask := FClipMask.Duplicate as TGrayscaleMask;
    FClipMaskOwned := true;
  end;
  result := FClipMask;
end;

constructor TBGRACanvasState2D.Create(AMatrix: TAffineMatrix;
  AClipMask: TGrayscaleMask; AClipMaskOwned: boolean);
begin
  strokeColor := BGRABlack;
  fillColor := BGRABlack;
  globalAlpha := 255;

  fontName := 'sans-serif';
  fontEmHeight := 10;
  fontStyle := [];
  textDirection := fbmAuto;
  textAlign:= taLeftJustify;
  textBaseline := 'alphabetic';

  lineWidth := 1;
  penStroker := TBGRAPenStroker.Create;
  penStroker.LineCap := pecFlat;
  penStroker.JoinStyle := pjsMiter;
  penStroker.CustomPenStyle := DuplicatePenStyle(SolidPenStyle);
  penStroker.MiterLimit := 10;
  penStroker.StrokeMatrix := AffineMatrixIdentity;

  shadowOffsetX := 0;
  shadowOffsetY := 0;
  shadowBlur := 0;
  shadowColor := BGRAPixelTransparent;
  shadowFastest:= false;

  matrix := AMatrix;
  FClipMask := nil;
  FClipMaskOwned := true;
  SetClipMask(AClipMask,AClipMaskOwned);
end;

function TBGRACanvasState2D.Duplicate: TBGRACanvasState2D;
begin
  result := TBGRACanvasState2D.Create(matrix,clipMaskReadOnly,false);
  result.strokeColor := strokeColor;
  result.strokeTextureProvider := strokeTextureProvider;
  result.fillColor := fillColor;
  result.fillMode := fillMode;
  result.fillTextureProvider := fillTextureProvider;
  result.globalAlpha := globalAlpha;

  result.fontName:= fontName;
  result.fontEmHeight := fontEmHeight;
  result.fontStyle := fontStyle;
  result.textDirection:= textDirection;
  result.textBaseline:= textBaseline;

  result.lineWidth := lineWidth;
  result.penStroker.LineCap := penStroker.LineCap;
  result.penStroker.JoinStyle := penStroker.JoinStyle;
  result.penStroker.CustomPenStyle := DuplicatePenStyle(penStroker.CustomPenStyle);
  result.penStroker.MiterLimit := penStroker.MiterLimit;
  result.penStroker.StrokeMatrix := penStroker.StrokeMatrix;

  result.shadowOffsetX := shadowOffsetX;
  result.shadowOffsetY := shadowOffsetY;
  result.shadowBlur := shadowBlur;
  result.shadowColor := shadowColor;
  result.shadowFastest := shadowFastest;
end;

destructor TBGRACanvasState2D.Destroy;
begin
  if FClipMaskOwned and Assigned(FClipMask) then
    FClipMask.Free;
  penStroker.Free;
  inherited Destroy;
end;

procedure TBGRACanvasState2D.transform(AMatrix: TAffineMatrix);
begin
  matrix := matrix*AMatrix;
end;

procedure TBGRACanvasState2D.SetClipMask(AClipMask: TGrayscaleMask;
  AOwned: boolean);
begin
  if FClipMaskOwned and Assigned(FClipMask) then FreeAndNil(FClipMask);
  FClipMask := AClipMask;
  FClipMaskOwned := AOwned;
end;

{ TBGRACanvas2D }

function TBGRACanvas2D.GetHeight: Integer;
begin
  if Assigned(surface) then
    result := Surface.Height
  else
    result := 0;
end;

function TBGRACanvas2D.GetLineCap: string;
begin
  case currentState.penStroker.LineCap of
    pecRound: result := 'round';
    pecSquare: result := 'square';
    else result := 'butt';
  end;
end;

function TBGRACanvas2D.GetLineCapLCL: TPenEndCap;
begin
  result := currentState.penStroker.LineCap;
end;

function TBGRACanvas2D.GetlineJoin: string;
begin
  case currentState.penStroker.JoinStyle of
    pjsBevel: result := 'bevel';
    pjsRound: result := 'round';
    else result := 'miter';
  end;
end;

function TBGRACanvas2D.GetlineJoinLCL: TPenJoinStyle;
begin
  result := currentState.penStroker.JoinStyle;
end;

function TBGRACanvas2D.getLineStyle: TBGRAPenStyle;
begin
  result := DuplicatePenStyle(currentState.penStroker.CustomPenStyle);
end;

function TBGRACanvas2D.GetLineWidth: single;
begin
  result := currentState.lineWidth;
end;

function TBGRACanvas2D.GetMatrix: TAffineMatrix;
begin
  result := currentState.matrix;
end;

function TBGRACanvas2D.GetMiterLimit: single;
begin
  result := currentState.penStroker.MiterLimit;
end;

function TBGRACanvas2D.GetPixelCenteredCoordinates: boolean;
begin
  result := FPixelCenteredCoordinates;
end;

function TBGRACanvas2D.GetShadowBlur: single;
begin
  result := currentState.shadowBlur;
end;

function TBGRACanvas2D.GetShadowFastest: boolean;
begin
  result := currentState.shadowFastest;
end;

function TBGRACanvas2D.GetShadowOffset: TPointF;
begin
  result := PointF(shadowOffsetX,shadowOffsetY);
end;

function TBGRACanvas2D.GetShadowOffsetX: single;
begin
  result := currentState.shadowOffsetX;
end;

function TBGRACanvas2D.GetShadowOffsetY: single;
begin
  result := currentState.shadowOffsetY;
end;

function TBGRACanvas2D.GetStrokeMatrix: TAffineMatrix;
begin
  result := currentState.penStroker.StrokeMatrix;
end;

function TBGRACanvas2D.GetTextAlign: string;
begin
  case currentState.textAlign of
    taRightJustify: result := 'right';
    taCenter: result := 'center';
  else
    result := 'left';
  end;
end;

function TBGRACanvas2D.GetTextAlignLCL: TAlignment;
begin
  result := currentState.textAlign;
end;

function TBGRACanvas2D.GetTextBaseline: string;
begin
  result := currentState.textBaseline;
end;

function TBGRACanvas2D.GetGlobalAlpha: single;
begin
  result := currentState.globalAlpha/255;
end;

function TBGRACanvas2D.GetCurrentPathAsPoints: ArrayOfTPointF;
var i: integer;
begin
  setlength(result, FPathPointCount);
  for i := 0 to high(result) do
    result[i] := FPathPoints[i];
end;

function TBGRACanvas2D.GetTextDirection: TFontBidiMode;
begin
  result := currentState.textDirection;
end;

function TBGRACanvas2D.GetFontName: string;
begin
  result := currentState.fontName;
end;

function TBGRACanvas2D.GetFontRenderer: TBGRACustomFontRenderer;
var zoom1,zoom2,zoom: single;
begin
  if FFontRenderer = nil then
  begin
    if FSurface <> nil then
      result := FSurface.FontRenderer
    else
      result := nil;
  end else
    result := FFontRenderer;
  if Assigned(result) then
  begin
    result.FontName := CSSFontNameToLCL(currentState.fontName);
    result.FontStyle := currentState.fontStyle;
    if antialiasing then
      result.FontQuality:= fqFineAntialiasing
    else
      result.FontQuality := fqSystem;
    result.FontOrientation := 0;
    zoom1 := VectLen(currentState.matrix[1,1],currentState.matrix[2,1]);
    zoom2 := VectLen(currentState.matrix[1,2],currentState.matrix[2,2]);
    if zoom1>zoom2 then zoom := zoom1 else zoom := zoom2;
    result.FontEmHeight := round(currentState.fontEmHeight*zoom);
  end;
end;

function TBGRACanvas2D.GetFontEmHeight: single;
begin
  result := currentState.fontEmHeight;
end;

function TBGRACanvas2D.GetFontString: string;
var formats: TFormatSettings;
begin
  formats := DefaultFormatSettings;
  formats.DecimalSeparator := '.';

  result := '';
  if fsItalic in currentState.fontStyle then
    AppendStr(result, 'italic ');
  if fsBold in currentState.fontStyle then
    AppendStr(result, 'bold ');
  AppendStr(result, FloatToStrF(currentState.fontEmHeight,ffGeneral,6,0,formats)+'px ');
  AppendStr(result, currentState.fontName);
  result := trim(result);
end;

function TBGRACanvas2D.GetFontStyle: TFontStyles;
begin
  result := currentState.fontStyle;
end;

function TBGRACanvas2D.GetHasShadow: boolean;
begin
  result := (ApplyGlobalAlpha(currentState.shadowColor).alpha <> 0) and
    ( (currentState.shadowBlur <> 0) or (currentState.shadowOffsetX <> 0)
      or (currentState.shadowOffsetY <> 0) );
end;

function TBGRACanvas2D.GetWidth: Integer;
begin
  if Assigned(Surface) then
    result := Surface.Width
  else
    result := 0;
end;

procedure TBGRACanvas2D.SetTextDirection(AValue: TFontBidiMode);
begin
  currentState.textDirection := AValue;
end;

procedure TBGRACanvas2D.SetFontName(AValue: string);
var
  list: ArrayOfString;
  i: Integer;
begin
  list := StrToFontNameList(AValue);
  for i := 0 to high(list) do
  begin
    if (list[i] = 'serif') or (list[i] = 'sans-serif') or (list[i] = 'monospace')
    or (list[i] = 'cursive') or (list[i] = 'fantasy') then
    begin
      currentState.fontName:= list[i];
      exit;
    end else
    if Assigned(fontRenderer) and fontRenderer.FontExists(list[i]) then
    begin
      currentState.fontName := list[i];
      exit;
    end;
  end;
  currentState.fontName := 'sans-serif';
end;

procedure TBGRACanvas2D.SetFontRenderer(AValue: TBGRACustomFontRenderer);
begin
  if AValue = FFontRenderer then exit;
  FreeAndNil(FFontRenderer);
  FFontRenderer := AValue;
end;

procedure TBGRACanvas2D.SetFontEmHeight(AValue: single);
begin
  currentState.fontEmHeight := AValue;
end;

procedure TBGRACanvas2D.SetFontString(AValue: string);
var idxSpace,errPos: integer;
  attrib,u: string;
  value: single;
begin
  currentState.fontStyle := [];
  currentState.fontEmHeight := 10;
  currentState.fontName := 'sans-serif';
  AValue := trim(AValue);
  while AValue <> '' do
  begin
    while (AValue <> '') and (AValue[1]in [#0..#32]) do delete(AValue,1,1);
    idxSpace := pos(' ',AValue);
    if idxSpace = 0 then
      attrib := AValue
    else
      attrib := copy(AValue,1,idxSpace-1);
    attrib := lowerCase(attrib);
    if attrib = '' then break;
    if (attrib = 'normal') or (attrib = 'small-caps') or (attrib = 'lighter') then
    begin
      //nothing
    end else
    if (attrib = 'italic') or (attrib = 'oblique') then
    begin
      include(currentState.fontStyle, fsItalic);
    end else
    if (attrib = 'bold') or (attrib = 'bolder') then
    begin
      include(currentState.fontStyle, fsBold);
    end else
    if (attrib[1] in ['.','0'..'9']) then
    begin
      u := '';
      while (length(attrib)>0) and (attrib[length(attrib)] in['a'..'z']) do
      begin
        u := attrib[length(attrib)]+u;
        delete(attrib,length(attrib),1);
      end;
      val(attrib,value,errPos);
      if errPos = 0 then
      begin
        if u = '' then //weight
        begin
          if value >= 600 then include(currentState.fontStyle, fsBold);
        end else
        if u = 'px' then currentState.fontEmHeight := value else
        if u = 'pt' then currentState.fontEmHeight:= value/72*96 else
        if u = 'in' then currentState.fontEmHeight:= value*96 else
        if u = 'mm' then currentState.fontEmHeight:= value/25.4*96 else
        if u = 'cm' then currentState.fontEmHeight:= value/2.54*96;
      end;
    end else
      break;
    delete(AValue,1,length(attrib)+1);
  end;
  AValue := trim(AValue);
  if AValue <> '' then currentState.fontName := AValue;
end;

procedure TBGRACanvas2D.SetFontStyle(AValue: TFontStyles);
begin
  currentState.fontStyle:= AValue;
end;

procedure TBGRACanvas2D.SetGlobalAlpha(const AValue: single);
begin
  if AValue < 0 then currentState.globalAlpha:= 0 else
  if AValue > 1 then currentState.globalAlpha:= 255 else
    currentState.globalAlpha:= round(AValue*255);
end;

procedure TBGRACanvas2D.SetLineCap(const AValue: string);
begin
  if CompareText(AValue,'round')=0 then
    currentState.penStroker.LineCap := pecRound else
  if CompareText(AValue,'square')=0 then
    currentState.penStroker.LineCap := pecSquare
  else
    currentState.penStroker.LineCap := pecFlat;
end;

procedure TBGRACanvas2D.SetLineCapLCL(AValue: TPenEndCap);
begin
  currentState.penStroker.LineCap := AValue;
end;

procedure TBGRACanvas2D.SetLineJoin(const AValue: string);
begin
  if CompareText(AValue,'round')=0 then
    currentState.penStroker.JoinStyle := pjsRound else
  if CompareText(AValue,'bevel')=0 then
    currentState.penStroker.JoinStyle := pjsBevel
  else
    currentState.penStroker.JoinStyle := pjsMiter;
end;

procedure TBGRACanvas2D.FillPoly(const points: array of TPointF);
var
  bfill: boolean;
  tempScan: TBGRACustomScanner;
begin
  if (length(points) = 0) or (surface = nil) then exit;
  If hasShadow then DrawShadow(points,[],fillMode);
  bfill:= currentState.fillMode = fmWinding;
  if currentState.clipMaskReadOnly <> nil then
  begin
    if currentState.fillTextureProvider <> nil then
      tempScan := TBGRATextureMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),currentState.fillTextureProvider.texture,currentState.globalAlpha)
    else
      tempScan := TBGRASolidColorMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),ApplyGlobalAlpha(currentState.fillColor));
    if self.antialiasing then
      BGRAPolygon.FillPolyAntialiasWithTexture(surface, points, tempScan, bfill, linearBlend)
    else
      BGRAPolygon.FillPolyAliasedWithTexture(surface, points, tempScan, bfill, GetDrawMode);
    tempScan.free;
  end else
  begin
    if currentState.fillTextureProvider <> nil then
    begin
      if currentState.globalAlpha <> 255 then
      begin
        tempScan := TBGRAOpacityScanner.Create(currentState.fillTextureProvider.texture, currentState.globalAlpha);
        if self.antialiasing then
          BGRAPolygon.FillPolyAntialiasWithTexture(surface, points, tempScan, bfill, linearBlend)
        else
          BGRAPolygon.FillPolyAliasedWithTexture(surface, points, tempScan, bfill, GetDrawMode);
        tempScan.Free;
      end else
      begin
        if self.antialiasing then
          BGRAPolygon.FillPolyAntialiasWithTexture(surface, points, currentState.fillTextureProvider.texture, bfill, linearBlend)
        else
          BGRAPolygon.FillPolyAliasedWithTexture(surface, points, currentState.fillTextureProvider.texture, bfill, GetDrawMode);
      end
    end
    else
    begin
      if self.antialiasing then
        BGRAPolygon.FillPolyAntialias(surface, points, ApplyGlobalAlpha(currentState.fillColor), false, bfill, linearBlend)
      else
        BGRAPolygon.FillPolyAliased(surface, points, ApplyGlobalAlpha(currentState.fillColor), false, bfill, GetDrawMode)
    end
  end;
end;

procedure TBGRACanvas2D.FillStrokePoly(const points: array of TPointF;
  fillOver: boolean);
var
  tempScan,tempScan2: TBGRACustomScanner;
  multi: TBGRAMultishapeFiller;
  contour : array of TPointF;
  texture: IBGRAScanner;
  idxContour: Integer;
begin
  if (length(points) = 0) or (surface = nil) then exit;
  tempScan := nil;
  tempScan2 := nil;
  multi := TBGRAMultishapeFiller.Create;
  multi.FillMode := self.fillMode;
  if currentState.clipMaskReadOnly <> nil then
  begin
    if currentState.fillTextureProvider <> nil then
      tempScan := TBGRATextureMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),currentState.fillTextureProvider.texture,currentState.globalAlpha)
    else
      tempScan := TBGRASolidColorMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),ApplyGlobalAlpha(currentState.fillColor));
    multi.AddPolygon(points, tempScan);
  end else
  begin
    if currentState.fillTextureProvider <> nil then
    begin
      if currentState.globalAlpha <> 255 then
      begin
        tempScan := TBGRAOpacityScanner.Create(currentState.fillTextureProvider.texture, currentState.globalAlpha);
        multi.AddPolygon(points, tempScan);
      end else
        multi.AddPolygon(points, currentState.fillTextureProvider.texture)
    end
    else
      multi.AddPolygon(points, ApplyGlobalAlpha(currentState.fillColor));
  end;

  if currentState.lineWidth > 0 then
  begin
    contour := currentState.penStroker.ComputePolylineAutocycle(points,currentState.lineWidth);

    if currentState.clipMaskReadOnly <> nil then
    begin
      if currentState.strokeTextureProvider <> nil then
        tempScan2 := TBGRATextureMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),currentState.strokeTextureProvider.texture,currentState.globalAlpha)
      else
        tempScan2 := TBGRASolidColorMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),ApplyGlobalAlpha(currentState.strokeColor));
      idxContour := multi.AddPolygon(contour,tempScan);
    end else
    begin
      if currentState.strokeTextureProvider <> nil then
        texture := currentState.strokeTextureProvider.texture else
        texture := nil;
      if texture = nil then
        idxContour := multi.AddPolygon(contour,ApplyGlobalAlpha(currentState.strokeColor))
      else
        idxContour := multi.AddPolygon(contour,texture);
    end;
    multi.OverrideFillMode(idxContour, fmWinding);
    If hasShadow then DrawShadow(points,contour);
  end else
    If hasShadow then DrawShadow(points,[]);

  if fillOver then multi.PolygonOrder := poFirstOnTop else multi.PolygonOrder:= poLastOnTop;
  multi.Antialiasing := self.antialiasing;
  multi.Draw(surface);
  tempScan.free;
  tempScan2.free;
  multi.Free;
end;

procedure TBGRACanvas2D.FillTexts(AErase: boolean);
var
  i,j: Integer;
  hy,hx,h: single;
  bmp,bmpTransf: TBGRACustomBitmap;
  tempScan: TBGRACustomScanner;
  m: TAffineMatrix;
  s: TSize;
  sourceBounds, usedSourceBounds, surfaceBounds, shadowBounds: TRect;
  rf: TResampleFilter;
  pad: TSize;
  p: PBGRAPixel;
begin
  for i := 0 to High(FTextPaths) do
  with FTextPaths[i] do
  begin
    hx := VectLen(FontMatrix[1,1],FontMatrix[2,1]);
    hy := VectLen(FontMatrix[1,2],FontMatrix[2,2]);
    h := max(hx,hy);
    if self.antialiasing then h := round(h);
    if h<=0 then continue;
    m := FontMatrix*AffineMatrixScale(1/h, 1/h);
    if pixelCenteredCoordinates then m := AffineMatrixTranslation(0.5,0.5)*m;
    bmp := BGRABitmapFactory.Create;
    try
      bmp.FontName := CSSFontNameToLCL(FontName);
      bmp.FontStyle:= FontStyle;
      bmp.FontHeight:= round(h);
      bmp.FontBidiMode:= TextDirection;
      if self.antialiasing then
        bmp.FontQuality := fqFineAntialiasing
      else
        bmp.FontQuality:= fqSystem;

      bmp.FontVerticalAnchor:= FontAnchor;
      m := m*AffineMatrixTranslation(0,-bmp.FontVerticalAnchorOffset);
      bmp.FontVerticalAnchor:= fvaTop;

      s := bmp.TextSize(Text);
      case FontAlign of
        taCenter: m := m*AffineMatrixTranslation(-s.cx/2,0);
        taRightJustify: m := m*AffineMatrixTranslation(-s.cx,0);
      end;

      pad := Size(round(h/3), round(h/3));
      m := m*AffineMatrixTranslation(-pad.cx,-pad.cy);
      sourceBounds := BGRAClasses.Rect(0,0,s.cx+pad.cx*2,s.cy+pad.cy*2);
      surfaceBounds := surface.GetImageAffineBounds(m, sourceBounds);
      if hasShadow then
      begin
        shadowBounds := surfaceBounds;
        shadowBounds.Inflate(ceil(shadowBlur),ceil(shadowBlur));
        shadowBounds.Offset(round(shadowOffsetX),round(shadowOffsetY));
        shadowBounds.Intersect(surface.ClipRect);
        if not shadowBounds.IsEmpty then
        begin
          shadowBounds.Offset(-round(shadowOffsetX),-round(shadowOffsetY));
          surfaceBounds.Union(shadowBounds);
        end;
      end;
      if not surfaceBounds.IsEmpty and IsAffineMatrixInversible(m) then
      begin
        usedSourceBounds := (AffineMatrixInverse(m) *
                             TAffineBox.AffineBox(RectF(surfaceBounds))).RectBounds;
        usedSourceBounds.Inflate(1,1);
        sourceBounds.Intersect(usedSourceBounds);
        m := m * AffineMatrixTranslation(sourceBounds.Left, sourceBounds.Top);
        bmp.SetSize(sourceBounds.Width, sourceBounds.Height);
        bmp.Fill(BGRABlack);
        bmp.TextOut(pad.cx - sourceBounds.Left,pad.cy - sourceBounds.Top, Text, BGRAWhite);
        if self.antialiasing then bmp.ConvertToLinearRGB else
        begin
          p := bmp.Data;
          for j := bmp.NbPixels-1 downto 0 do
          begin
            if p^.green<128 then p^ := BGRABlack else p^ := BGRAWhite;
            inc(p);
          end;
        end;

        bmpTransf := BGRABitmapFactory.Create(surfaceBounds.Width,surfaceBounds.Height,BGRABlack);
        try
          m := AffineMatrixTranslation(-surfaceBounds.Left-0.5,-surfaceBounds.Top-0.5)*m;
          if self.antialiasing then rf:= rfCosine else rf := rfBox;
          bmpTransf.PutImageAffine(m, bmp, rf, GetDrawMode);
          FreeAndNil(bmp);

          if AErase then
            surface.EraseMask(surfaceBounds.Left,surfaceBounds.Top, bmpTransf) else
          begin
            if hasShadow then
              DrawShadowMask(surfaceBounds.Left+round(shadowOffsetX),surfaceBounds.Top+round(shadowOffsetY), bmpTransf, false);

            if currentState.clipMaskReadOnly <> nil then
            begin
              if currentState.fillTextureProvider <> nil then
                tempScan := TBGRATextureMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),currentState.fillTextureProvider.texture,currentState.globalAlpha)
              else
                tempScan := TBGRASolidColorMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),ApplyGlobalAlpha(currentState.fillColor));
              surface.FillMask(surfaceBounds.Left,surfaceBounds.Top, bmpTransf, tempScan, GetDrawMode);
              tempScan.free;
            end else
            begin
              if currentState.fillTextureProvider <> nil then
              begin
                if currentState.globalAlpha <> 255 then
                begin
                  tempScan := TBGRAOpacityScanner.Create(currentState.fillTextureProvider.texture, currentState.globalAlpha);
                  surface.FillMask(surfaceBounds.Left,surfaceBounds.Top, bmpTransf, tempScan, GetDrawMode);
                  tempScan.Free;
                end else
                  surface.FillMask(surfaceBounds.Left,surfaceBounds.Top, bmpTransf, currentState.fillTextureProvider.texture, GetDrawMode);
              end
              else
                surface.FillMask(surfaceBounds.Left,surfaceBounds.Top, bmpTransf, ApplyGlobalAlpha(currentState.fillColor), GetDrawMode);
            end;
          end;
        finally
          bmpTransf.Free;
        end;
      end;
    finally
      bmp.Free;
    end;
  end;
end;

procedure TBGRACanvas2D.SetLineJoinLCL(AValue: TPenJoinStyle);
begin
  currentState.penStroker.JoinStyle := AValue;
end;

procedure TBGRACanvas2D.lineStyle(const AValue: array of single);
var a: array of single;
  i: Integer;
  isClear: boolean;
begin
  isClear := (length(AValue) = 1) and (AValue[0] = 0);
  if odd(length(AValue)) and not isClear then
  begin
    setlength(a, length(AValue)*2);
    for i := 0 to high(AValue) do
    begin
      a[i] := AValue[i];
      a[i + length(AValue)] := AValue[i];
    end;
  end else
    a := DuplicatePenStyle(AValue);
  currentState.penStroker.CustomPenStyle := a;
end;

procedure TBGRACanvas2D.lineStyle(AStyle: TPenStyle);
begin
  if AStyle = psPattern then exit;
  lineStyle(PenStyleToBGRA(AStyle));
end;

class function TBGRACanvas2D.StrToFontNameList(AText: string): ArrayOfString;
var
  list: TStringList;
  inQuote: Char;
  nameStart, i: Integer;

  procedure SkipSpace;
  begin
    while (i < length(AText)) and (AText[i] in [#0..#32]) do inc(i);
  end;

  procedure SkipComma;
  begin
    SkipSpace;
    if (i < length(AText)) and (AText[i] = ',') then inc(i);
    SkipSpace;
  end;

begin
  list := TStringList.Create;
  inQuote := ' ';
  i := 1;
  SkipSpace;
  nameStart := -1;
  while i <= length(AText) do
  begin
    if inQuote <> ' ' then
    begin
      if AText[i] = inQuote then
      begin
        list.Add(copy(AText, nameStart, i-nameStart));
        inQuote := ' ';
        inc(i);
        SkipComma;
        nameStart := -1;
      end else
        inc(i);
    end else
    if nameStart = -1 then
    begin
      if AText[i] in ['''', '"'] then
      begin
        nameStart := i+1;
        inQuote := AText[i];
        inc(i);
      end else
      begin
        nameStart := i;
        inc(i);
      end;
    end else
    if AText[i] = ',' then
    begin
      list.Add(Trim(copy(AText, nameStart, i-nameStart)));
      inc(i);
      SkipComma;
      nameStart := -1;
    end else
      inc(i);
  end;
  if nameStart <> -1 then list.Add(copy(AText, nameStart, length(AText)-nameStart+1));
  setlength(result, list.Count);
  for i := 0 to list.Count-1 do
    result[i] := list[i];
  list.Free;
end;

class function TBGRACanvas2D.FontNameListToStr(AList: ArrayOfString): string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to high(AList) do
  begin
    if i > 0 then AppendStr(result, ', ');
    if pos(' ',AList[i]) <> -1 then
      AppendStr(result, '''' + StringReplace(AList[i], '''', '&#39;', [rfReplaceAll]) + '''')
      else AppendStr(result, AList[i]);
  end;
end;

class function TBGRACanvas2D.CSSFontNameToLCL(AName: string): string;
begin
  if AName = 'sans-serif' then result := 'sans'
  else if AName = 'cursive' then result := {$IFDEF WINDOWS}'Segoe Script'{$ELSE}
                                           {$IFDEF LINUX}'Z003'{$ELSE}
                                           {$IFDEF DARWIN}'Brush Script MT'{$ELSE}
                                           'sans'
                                           {$ENDIF}{$ENDIF}{$ENDIF}
  else if AName = 'fantasy' then result := {$IFDEF WINDOWS}'Comic Sans MS'{$ELSE}
                                           {$IFDEF DARWIN}'Papyrus'{$ELSE}
                                           'sans'
                                           {$ENDIF}{$ENDIF}
  else result := StringReplace(StringReplace(AName, '&apos;', '''', [rfReplaceAll]),
                                  '&#39;', '''', [rfReplaceAll]);
end;

function TBGRACanvas2D.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := longint(E_NOINTERFACE);
end;

{ There is no automatic reference counting, but it is compulsory to define these functions }
function TBGRACanvas2D._AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function TBGRACanvas2D._Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

procedure TBGRACanvas2D.SetLineWidth(const AValue: single);
begin
  currentState.lineWidth := AValue;
end;

procedure TBGRACanvas2D.SetMatrix(AValue: TAffineMatrix);
begin
  currentState.matrix := AValue;
end;

procedure TBGRACanvas2D.SetMiterLimit(const AValue: single);
begin
  currentState.penStroker.MiterLimit := AValue;
end;

procedure TBGRACanvas2D.SetPixelCenteredCoordinates(const AValue: boolean);
begin
  FPixelCenteredCoordinates:= AValue;
  if AValue then
    FCanvasOffset := PointF(0,0)
  else
    FCanvasOffset := PointF(-0.5,-0.5);
end;

procedure TBGRACanvas2D.SetShadowBlur(const AValue: single);
begin
  currentState.shadowBlur := AValue;
end;

procedure TBGRACanvas2D.SetShadowFastest(AValue: boolean);
begin
  currentState.shadowFastest := AValue;
end;

procedure TBGRACanvas2D.SetShadowOffset(const AValue: TPointF);
begin
  shadowOffsetX := AValue.X;
  shadowOffsetY := AValue.Y;
end;

procedure TBGRACanvas2D.SetShadowOffsetX(const AValue: single);
begin
  currentState.shadowOffsetX := AValue;
end;

procedure TBGRACanvas2D.SetShadowOffsetY(const AValue: single);
begin
  currentState.shadowOffsetY := AValue;
end;

procedure TBGRACanvas2D.SetStrokeMatrix(AValue: TAffineMatrix);
begin
  currentState.penStroker.strokeMatrix := AValue;
end;

procedure TBGRACanvas2D.SetTextAlign(AValue: string);
begin
  AValue := trim(LowerCase(AValue));
  if (AValue = 'left') or (AValue = 'start') then
    textAlignLCL := taLeftJustify else
  if (AValue = 'right') or (AValue = 'end') then
    textAlignLCL := taRightJustify else
  if AValue = 'center' then
    textAlignLCL := taCenter;
end;

procedure TBGRACanvas2D.SetTextAlignLCL(AValue: TAlignment);
begin
  currentState.textAlign := AValue;
end;

procedure TBGRACanvas2D.SetTextBaseline(AValue: string);
begin
  currentState.textBaseline := trim(lowercase(AValue));
end;

procedure TBGRACanvas2D.StrokePoly(const points: array of TPointF);
var
  texture: IBGRAScanner;
  tempScan: TBGRACustomScanner;
  contour: array of TPointF;
begin
  if (length(points)= 0) or (currentState.lineWidth = 0) or (surface = nil) then exit;
  contour := currentState.penStroker.ComputePolylineAutocycle(points,currentState.lineWidth);

  If hasShadow then DrawShadow(contour,[]);
  if currentState.clipMaskReadOnly <> nil then
  begin
    if currentState.strokeTextureProvider <> nil then
      tempScan := TBGRATextureMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),currentState.strokeTextureProvider.texture,currentState.globalAlpha)
    else
      tempScan := TBGRASolidColorMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),ApplyGlobalAlpha(currentState.strokeColor));
    if self.antialiasing then
      BGRAPolygon.FillPolyAntialiasWithTexture(Surface,contour,tempScan,True, linearBlend)
    else
      BGRAPolygon.FillPolyAliasedWithTexture(Surface,contour,tempScan,True,GetDrawMode);
    tempScan.free;
  end else
  begin
    if currentState.strokeTextureProvider <> nil then
      texture := currentState.strokeTextureProvider.texture else
      texture := nil;
    if texture = nil then
    begin
      if self.antialiasing then
        BGRAPolygon.FillPolyAntialias(Surface,contour,ApplyGlobalAlpha(currentState.strokeColor),false,True, linearBlend)
      else
        BGRAPolygon.FillPolyAliased(Surface,contour,ApplyGlobalAlpha(currentState.strokeColor),false,True,GetDrawMode)
    end
    else
    begin
      if self.antialiasing then
        BGRAPolygon.FillPolyAntialiasWithTexture(Surface,contour,texture,True, linearBlend)
      else
        BGRAPolygon.FillPolyAliasedWithTexture(Surface,contour,texture,True,GetDrawMode)
    end;
  end;
end;

procedure TBGRACanvas2D.DrawShadow(const points, points2: array of TPointF;
  AFillMode: TFillMode = fmWinding);
var ofsPts,ofsPts2: array of TPointF;
    offset: TPointF;
    i: Integer;
    tempBmp: TGrayscaleMask;
    maxRect: TRect;
    foundRect: TRect;
    firstFound: boolean;

    procedure AddPt(const coord: TPointF);
    var pixRect: TRect;
    begin
      if isEmptyPointF(coord) then exit;
      pixRect := BGRAClasses.Rect(round(floor(coord.x)),round(floor(coord.y)),round(ceil(coord.x+0.999))+1,round(ceil(coord.y+0.999))+1);
      if firstFound then
      begin
        foundRect := pixRect;
        firstFound := false
      end
      else
      begin
        if pixRect.left < foundRect.left then foundRect.left := pixRect.Left;
        if pixRect.top < foundRect.top then foundRect.top := pixRect.top;
        if pixRect.right > foundRect.right then foundRect.right := pixRect.right;
        if pixRect.bottom > foundRect.bottom then foundRect.bottom := pixRect.bottom;
      end;
    end;

begin
  if not hasShadow or (surface = nil) then exit;
  offset := PointF(shadowOffsetX,shadowOffsetY);
  setlength(ofsPts, length(points));
  for i := 0 to high(ofsPts) do
    ofsPts[i] := points[i]+offset;
  setlength(ofsPts2, length(points2));
  for i := 0 to high(ofsPts2) do
    ofsPts2[i] := points2[i]+offset;

  maxRect := BGRAClasses.Rect(0,0,width,height);
  if currentState.clipMaskReadOnly <> nil then
    foundRect := maxRect
  else
  begin
    firstFound := true;
    foundRect := EmptyRect;
    for i := 0 to high(ofsPts) do
      AddPt(ofsPts[i]);
    for i := 0 to high(ofsPts2) do
      AddPt(ofsPts2[i]);
    if firstFound then exit;
    foundRect.Inflate(ceil(shadowBlur), ceil(shadowBlur));
    foundRect.Intersect(maxRect);
    if foundRect.IsEmpty then exit;
    offset := PointF(-foundRect.Left,-foundRect.Top);
    for i := 0 to high(ofsPts) do
      ofsPts[i].Offset(offset);
    for i := 0 to high(ofsPts2) do
      ofsPts2[i].Offset(offset);
  end;

  tempBmp := TGrayscaleMask.Create(foundRect.Right-foundRect.Left,foundRect.Bottom-foundRect.Top,BGRABlack);
  tempBmp.FillMode := AFillMode;
  tempBmp.FillPolyAntialias(ofsPts, BGRAWhite);
  tempBmp.FillPolyAntialias(ofsPts2, BGRAWhite);
  DrawShadowMask(foundRect.Left,foundRect.Top, tempBmp, true);
end;

procedure TBGRACanvas2D.DrawShadowMask(X, Y: integer; AMask: TCustomUniversalBitmap; AMaskOwned: boolean);
const invSqrt2 = 1/sqrt(2);
var
  bmp: TCustomUniversalBitmap;
  gs: TGrayscaleMask;
begin
  if AMask.Colorspace <> TByteMaskColorspace then
  begin
    gs := TGrayscaleMask.Create(AMask as TBGRACustomBitmap, cGreen);
    if AMaskOwned then AMask.Free;
    AMask := gs;
    AMaskOwned:= true;
  end;
  bmp := AMask;
  if shadowBlur > 0 then
  begin
    if shadowFastest then
    begin
      if shadowBlur*invSqrt2 >= 0.5 then
        bmp := AMask.FilterBlurRadial(round(shadowBlur*invSqrt2),rbBox);
    end
    else
    begin
      if (shadowBlur < 5) and (abs(shadowBlur-round(shadowBlur)) > 1e-6) then
        bmp := AMask.FilterBlurRadial(round(shadowBlur*10),rbPrecise)
      else
        bmp := AMask.FilterBlurRadial(round(shadowBlur),rbFast);
    end;
  end;
  if currentState.clipMaskReadOnly <> nil then
  begin
    if (bmp = AMask) and not AMaskOwned then bmp := AMask.Duplicate;
    bmp.ApplyMask(currentState.clipMaskReadOnly);
  end;
  surface.FillMask(X,Y,bmp,ApplyGlobalAlpha(getShadowColor),GetDrawMode);
  if bmp <> AMask then bmp.Free;
  if AMaskOwned then AMask.Free;
end;

procedure TBGRACanvas2D.ClearPoly(const points: array of TPointF);
begin
  if surface = nil then exit;
  if self.antialiasing then
    BGRAPolygon.FillPolyAntialias(surface, points, BGRA(0,0,0,255), true, true, linearBlend)
  else
    BGRAPolygon.FillPolyAliased(surface, points, BGRA(0,0,0,255), true, true, dmSet);
end;

function TBGRACanvas2D.ApplyTransform(const points: array of TPointF;
  matrix: TAffineMatrix): ArrayOfTPointF;
var
  i: Integer;
begin
  setlength(result,length(points));
  for i := 0 to high(result) do
    if isEmptyPointF(points[i]) then
      result[i] := EmptyPointF
    else
      result[i] := matrix*points[i]+FCanvasOffset;
end;

function TBGRACanvas2D.ApplyTransform(const points: array of TPointF
  ): ArrayOfTPointF;
var
  i: Integer;
begin
  setlength(result,length(points));
  for i := 0 to high(result) do
    if isEmptyPointF(points[i]) then
      result[i] := EmptyPointF
    else
      result[i] := currentState.matrix*points[i]+FCanvasOffset;
end;

function TBGRACanvas2D.ApplyTransform(point: TPointF): TPointF;
begin
  result := currentState.matrix*point+FCanvasOffset;
end;

function TBGRACanvas2D.GetPenPos(defaultX,defaultY: single): TPointF;
begin
  if isEmptyPointF(FLastCoord) then
    result := PointF(defaultX,defaultY)
  else
    result := FLastCoord;
end;

function TBGRACanvas2D.GetPenPos(defaultPt: TPointF): TPointF;
begin
  result := GetPenPos(defaultPt.x,defaultPt.y);
end;

procedure TBGRACanvas2D.AddPoint(point: TPointF);
begin
  if FPathPointCount = length(FPathPoints) then
    setlength(FPathPoints, (length(FPathPoints)+1)*2);
  FPathPoints[FPathPointCount] := point;
  inc(FPathPointCount);
end;

procedure TBGRACanvas2D.AddPoints(const points: array of TPointF);
var i: integer;
begin
  if FPathPointCount+length(points) > length(FPathPoints) then
    setlength(FPathPoints, max( (length(FPathPoints)+1)*2, FPathPointCount+length(points) ) );
  for i := 0 to high(points) do
  begin
    FPathPoints[FPathPointCount] := points[i];
    inc(FPathPointCount);
  end;
end;

procedure TBGRACanvas2D.AddPointsRev(const points: array of TPointF);
var i: integer;
begin
  if FPathPointCount+length(points) > length(FPathPoints) then
    setlength(FPathPoints, max( (length(FPathPoints)+1)*2, FPathPointCount+length(points) ) );
  for i := high(points) downto 0 do
  begin
    FPathPoints[FPathPointCount] := points[i];
    inc(FPathPointCount);
  end;
end;

function TBGRACanvas2D.ApplyGlobalAlpha(color: TBGRAPixel): TBGRAPixel;
begin
  result := BGRA(color.red,color.green,color.blue,ApplyOpacity(color.alpha, currentState.globalAlpha));
end;

function TBGRACanvas2D.GetDrawMode: TDrawMode;
begin
  if linearBlend then result := dmLinearBlend else result := dmDrawWithTransparency;
end;

procedure TBGRACanvas2D.copyTo(dest: IBGRAPath);
begin
  //nothing
end;

function TBGRACanvas2D.getPoints: ArrayOfTPointF;
begin
  result := GetCurrentPathAsPoints;
end;

function TBGRACanvas2D.getPoints(AMatrix: TAffineMatrix): ArrayOfTPointF;
begin
  result := GetCurrentPathAsPoints;
  if not IsAffineMatrixIdentity(AMatrix) then
    result := AMatrix*result;
end;

function TBGRACanvas2D.getCursor: TBGRACustomPathCursor;
begin
  result := nil;
end;

constructor TBGRACanvas2D.Create(ASurface: TBGRACustomBitmap);
begin
  FSurface := ASurface;
  StateStack := TList.Create;
  FPathPointCount := 0;
  FLastCoord := EmptyPointF;
  FStartCoord := EmptyPointF;
  currentState := TBGRACanvasState2D.Create(AffineMatrixIdentity,nil,true);
  pixelCenteredCoordinates := false;
  antialiasing := true;
  gradientGammaCorrection := false;
end;

destructor TBGRACanvas2D.Destroy;
var
  i: Integer;
begin
  for i := 0 to StateStack.Count-1 do
    TObject(StateStack[i]).Free;
  StateStack.Free;
  currentState.Free;
  FreeAndNil(FFontRenderer);
  inherited Destroy;
end;

function TBGRACanvas2D.toDataURL(mimeType: string): string;
var
  stream: TMemoryStream;
  jpegWriter: TFPWriterJPEG;
  bmpWriter: TFPWriterBMP;
  output: TStringStream;
  encode64: TBase64EncodingStream;
begin
  if surface = nil then exit('');
  stream := TMemoryStream.Create;
  if mimeType='image/jpeg' then
  begin
    jpegWriter := TFPWriterJPEG.Create;
    Surface.SaveToStream(stream,jpegWriter);
    jpegWriter.Free;
  end else
  if mimeType='image/bmp' then
  begin
    bmpWriter := TFPWriterBMP.Create;
    Surface.SaveToStream(stream,bmpWriter);
    bmpWriter.Free;
  end else
  begin
    mimeType := 'image/png';
    Surface.SaveToStreamAsPng(stream);
  end;
  output := TStringStream.Create('data:'+mimeType+';base64,');
  output.Position := output.size;
  stream.Position := 0;
  encode64 := TBase64EncodingStream.Create(output);
  encode64.CopyFrom(stream,stream.size);
  encode64.free;
  stream.free;
  result := output.DataString;
  output.free;
end;

procedure TBGRACanvas2D.save;
begin
  StateStack.Add(currentState);
  currentState := currentState.Duplicate;
end;

procedure TBGRACanvas2D.restore;
begin
  if StateStack.Count > 0 then
  begin
    FreeAndNil(currentState);
    currentState := TBGRACanvasState2D(StateStack[StateStack.Count-1]);
    StateStack.Delete(StateStack.Count-1);
  end;
end;

procedure TBGRACanvas2D.scale(x, y: single);
begin
  currentState.transform(AffineMatrixScale(x,y));
end;

procedure TBGRACanvas2D.scale(factor: single);
begin
  currentState.transform( AffineMatrixScale(factor,factor) );
end;

procedure TBGRACanvas2D.rotate(angleRadCW: single);
begin
  currentState.transform( AffineMatrixRotationRad(-angleRadCW) );
end;

procedure TBGRACanvas2D.translate(x, y: single);
begin
  if (x = 0) and (y = 0) then exit;
  currentState.transform( AffineMatrixTranslation(x,y) );
end;

procedure TBGRACanvas2D.skewx(angleRadCW: single);
begin
  currentState.transform( AffineMatrixSkewXRad(-angleRadCW) );
end;

procedure TBGRACanvas2D.skewy(angleRadCW: single);
begin
  currentState.transform( AffineMatrixSkewYRad(-angleRadCW) );
end;

procedure TBGRACanvas2D.transform(m11,m21, m12,m22, m13,m23: single);
begin
  currentState.transform( AffineMatrix(m11,m12,m13,
                                       m21,m22,m23) );
end;

procedure TBGRACanvas2D.transform(AMatrix: TAffineMatrix);
begin
  currentState.transform( AMatrix );
end;

procedure TBGRACanvas2D.setTransform(m11,m21, m12,m22, m13,m23: single);
begin
  currentState.matrix := AffineMatrix(m11,m12,m13,
                                      m21,m22,m23);
end;

procedure TBGRACanvas2D.resetTransform;
begin
  currentState.matrix := AffineMatrixIdentity;
end;

procedure TBGRACanvas2D.strokeScale(x, y: single);
begin
  currentState.penStroker.strokeMatrix := currentState.penStroker.strokeMatrix * AffineMatrixScale(x,y);
end;

procedure TBGRACanvas2D.strokeSkewx(angleRadCW: single);
begin
  currentState.penStroker.strokeMatrix := currentState.penStroker.strokeMatrix * AffineMatrixSkewXRad(-angleRadCW);
end;

procedure TBGRACanvas2D.strokeSkewy(angleRadCW: single);
begin
  currentState.penStroker.strokeMatrix := currentState.penStroker.strokeMatrix * AffineMatrixSkewYRad(-angleRadCW);
end;

procedure TBGRACanvas2D.strokeResetTransform;
begin
  currentState.penStroker.strokeMatrix := AffineMatrixIdentity;
end;

procedure TBGRACanvas2D.strokeStyle(color: TBGRAPixel);
begin
  currentState.strokeColor := color;
  currentState.strokeTextureProvider := nil;
end;

procedure TBGRACanvas2D.strokeStyle(color: TColor);
begin
  currentState.strokeColor := ColorToBGRA(color);
  currentState.strokeTextureProvider := nil;
end;

procedure TBGRACanvas2D.strokeStyle(color: string);
begin
  currentState.strokeColor := StrToBGRA(color);
  currentState.strokeTextureProvider := nil;
end;

procedure TBGRACanvas2D.strokeStyle(texture: IBGRAScanner);
begin
  strokeStyle(createPattern(texture));
end;

procedure TBGRACanvas2D.strokeStyle(provider: IBGRACanvasTextureProvider2D);
begin
  currentState.strokeColor := BGRAPixelTransparent;
  currentState.strokeTextureProvider := provider;
end;

function TBGRACanvas2D.GetFillMode: TFillMode;
begin
  result := currentState.fillMode;
end;

procedure TBGRACanvas2D.SetFillMode(mode: TFillMode);
begin
  currentState.fillMode := mode;
end;     

procedure TBGRACanvas2D.fillStyle(color: TBGRAPixel);
begin
  currentState.fillColor := color;
  currentState.fillTextureProvider := nil;
end;

procedure TBGRACanvas2D.fillStyle(color: TColor);
begin
  currentState.fillColor := ColorToBGRA(color);
  currentState.fillTextureProvider := nil;
end;

procedure TBGRACanvas2D.fillStyle(color: string);
begin
  currentState.fillColor := StrToBGRA(color);
  currentState.fillTextureProvider := nil;
end;

procedure TBGRACanvas2D.fillStyle(texture: IBGRAScanner);
begin
  fillStyle(createPattern(texture));
end;

procedure TBGRACanvas2D.fillStyle(provider: IBGRACanvasTextureProvider2D);
begin
  currentState.fillColor := BGRAPixelTransparent;
  currentState.fillTextureProvider := provider;
end;

procedure TBGRACanvas2D.shadowColor(color: TBGRAPixel);
begin
  currentState.shadowColor := color;
end;

procedure TBGRACanvas2D.shadowColor(color: TColor);
begin
  shadowColor(ColorToBGRA(color));
end;

procedure TBGRACanvas2D.shadowColor(color: string);
begin
  shadowColor(StrToBGRA(color));
end;

procedure TBGRACanvas2D.shadowNone;
begin
  shadowColor(BGRAPixelTransparent);
end;

function TBGRACanvas2D.getShadowColor: TBGRAPixel;
begin
  result := currentState.shadowColor;
end;

function TBGRACanvas2D.createLinearGradient(x0, y0, x1, y1: single): IBGRACanvasGradient2D;
begin
  result := createLinearGradient(PointF(x0,y0), PointF(x1,y1));
end;

function TBGRACanvas2D.createLinearGradient(p0, p1: TPointF): IBGRACanvasGradient2D;
begin
  result := TBGRACanvasLinearGradient2D.Create(p0,p1,
            AffineMatrixTranslation(FCanvasOffset.x,FCanvasOffset.y)*currentState.matrix);
  result.gammaCorrection := gradientGammaCorrection;
end;

function TBGRACanvas2D.createLinearGradient(x0, y0, x1, y1: single;
  Colors: TBGRACustomGradient): IBGRACanvasGradient2D;
begin
  result := createLinearGradient(x0,y0,x1,y1);
  result.setColors(Colors);
end;

function TBGRACanvas2D.createLinearGradient(p0, p1: TPointF;
  Colors: TBGRACustomGradient): IBGRACanvasGradient2D;
begin
  result := createLinearGradient(p0,p1);
  result.setColors(Colors);
end;

function TBGRACanvas2D.createRadialGradient(x0, y0, r0, x1, y1, r1: single;
  flipGradient: boolean): IBGRACanvasGradient2D;
begin
  result := createRadialGradient(PointF(x0,y0), r0, PointF(x1,y1), r1, flipGradient);
end;

function TBGRACanvas2D.createRadialGradient(p0: TPointF; r0: single;
  p1: TPointF; r1: single; flipGradient: boolean): IBGRACanvasGradient2D;
begin
  result := TBGRACanvasRadialGradient2D.Create(p0,r0,p1,r1,
            AffineMatrixTranslation(FCanvasOffset.x,FCanvasOffset.y)*currentState.matrix,
            flipGradient);
  result.gammaCorrection := gradientGammaCorrection;
end;

function TBGRACanvas2D.createRadialGradient(x0, y0, r0, x1, y1, r1: single;
  Colors: TBGRACustomGradient; flipGradient: boolean): IBGRACanvasGradient2D;
begin
  result := createRadialGradient(x0,y0,r0,x1,y1,r1,flipGradient);
  result.setColors(Colors);
end;

function TBGRACanvas2D.createRadialGradient(p0: TPointF; r0: single;
  p1: TPointF; r1: single; Colors: TBGRACustomGradient; flipGradient: boolean): IBGRACanvasGradient2D;
begin
  result := createRadialGradient(p0,r0,p1,r1,flipGradient);
  result.setColors(Colors);
end;

function TBGRACanvas2D.createPattern(image: TBGRACustomBitmap; repetition: string
  ): IBGRACanvasTextureProvider2D;
var
  repeatX,repeatY: boolean;
  origin: TPointF;
begin
  repetition := lowercase(trim(repetition));
  repeatX := true;
  repeatY := true;
  if repetition = 'repeat-x' then repeatY := false else
  if repetition = 'repeat-y' then repeatX := false else
  if repetition = 'no-repeat' then
  begin
    repeatX := false;
    repeatY := false;
  end;
  origin := ApplyTransform(PointF(0,0));
  result := TBGRACanvasPattern2D.Create(image,repeatX,repeatY,
     origin, origin+PointF(currentState.matrix[1,1],currentState.matrix[2,1])*image.Width,
     origin+PointF(currentState.matrix[1,2],currentState.matrix[2,2])*image.Height);
end;

function TBGRACanvas2D.createPattern(texture: IBGRAScanner
  ): IBGRACanvasTextureProvider2D;
var
  tempTransform: TAffineMatrix;
begin
  tempTransform := AffineMatrixTranslation(FCanvasOffset.X+0.5,FCanvasOffset.Y+0.5)*currentState.matrix;
  result := TBGRACanvasPattern2D.Create(texture,tempTransform);
end;

procedure TBGRACanvas2D.fillRect(x, y, w, h: single);
begin
  if (w=0) or (h=0) then exit;
  FillPoly(ApplyTransform([PointF(x,y),PointF(x+w,y),PointF(x+w,y+h),PointF(x,y+h)]));
end;

procedure TBGRACanvas2D.strokeRect(x, y, w, h: single);
begin
  if (w=0) or (h=0) then exit;
  StrokePoly(ApplyTransform([PointF(x,y),PointF(x+w,y),PointF(x+w,y+h),PointF(x,y+h),PointF(x,y)]));
end;

procedure TBGRACanvas2D.clearRect(x, y, w, h: single);
begin
  if (w=0) or (h=0) then exit;
  ClearPoly(ApplyTransform([PointF(x,y),PointF(x+w,y),PointF(x+w,y+h),PointF(x,y+h)]));
end;

procedure TBGRACanvas2D.addPath(APath: IBGRAPath);
begin
  if (FPathPointCount <> 0) and not isEmptyPointF(FPathPoints[FPathPointCount-1]) then
  begin
    AddPoint(EmptyPointF);
    FLastCoord := EmptyPointF;
    FStartCoord := EmptyPointF;
  end;
  APath.copyTo(self);
end;

procedure TBGRACanvas2D.addPath(ASvgPath: string);
var p: TBGRAPath;
begin
  p := TBGRAPath.Create(ASvgPath);
  addPath(p);
  p.Free;
end;

procedure TBGRACanvas2D.path(APath: IBGRAPath);
begin
  beginPath;
  addPath(APath);
end;

procedure TBGRACanvas2D.path(ASvgPath: string);
begin
  beginPath;
  addPath(ASvgPath);
end;

procedure TBGRACanvas2D.beginPath;
begin
  FPathPointCount := 0;
  FLastCoord := EmptyPointF;
  FStartCoord := EmptyPointF;
  FTextPaths := nil;
end;

procedure TBGRACanvas2D.closePath;
var i: integer;
begin
  if FPathPointCount > 0 then
  begin
    i := FPathPointCount-1;
    while (i > 0) and not isEmptyPointF(FPathPoints[i-1]) do dec(i);
    AddPoint(FPathPoints[i]);
    FLastCoord := FStartCoord;
  end;
end;

procedure TBGRACanvas2D.toSpline(closed: boolean; style: TSplineStyle);
var i,j: integer;
  pts, splinePts: array of TPointF;
  nb: integer;
begin
  if FPathPointCount > 0 then
  begin
    i := FPathPointCount-1;
    while (i > 0) and not isEmptyPointF(FPathPoints[i-1]) do dec(i);
    nb := FPathPointCount - i;
    setlength(pts,nb);
    for j := 0 to nb-1 do
      pts[j] := FPathPoints[i+j];
    if closed then
      splinePts := BGRAPath.ComputeClosedSpline(pts,style)
    else
      splinePts := BGRAPath.ComputeOpenedSpline(pts,style);
    dec(FPathPointCount,nb);
    AddPoints(splinePts);
  end;
end;

procedure TBGRACanvas2D.moveTo(x, y: single);
begin
  moveTo(PointF(x,y));
end;

procedure TBGRACanvas2D.lineTo(x, y: single);
begin
  lineTo(PointF(x,y));
end;

procedure TBGRACanvas2D.moveTo(constref pt: TPointF);
begin
  if (FPathPointCount <> 0) and not isEmptyPointF(FPathPoints[FPathPointCount-1]) then
    AddPoint(EmptyPointF);
  AddPoint(ApplyTransform(pt));
  FStartCoord := pt;
  FLastCoord := pt;
end;

procedure TBGRACanvas2D.lineTo(constref pt: TPointF);
begin
  AddPoint(ApplyTransform(pt));
  FLastCoord := pt;
end;

procedure TBGRACanvas2D.polylineTo(const pts: array of TPointF);
begin
  if length(pts)> 0 then
  begin
    AddPoints(ApplyTransform(pts));
    FLastCoord := pts[high(pts)];
  end;
end;

procedure TBGRACanvas2D.quadraticCurveTo(cpx, cpy, x, y: single);
var
  curve : TQuadraticBezierCurve;
  pts : array of TPointF;
begin
  curve := BezierCurve(ApplyTransform(GetPenPos(cpx,cpy)),ApplyTransform(PointF(cpx,cpy)),ApplyTransform(PointF(x,y)));
  pts := BGRAPath.ComputeBezierCurve(curve);
  AddPoints(pts);
  FLastCoord := PointF(x,y);
end;

procedure TBGRACanvas2D.quadraticCurveTo(constref cp, pt: TPointF);
begin
  quadraticCurveTo(cp.x,cp.y,pt.x,pt.y);
end;

procedure TBGRACanvas2D.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y: single);
var
  curve : TCubicBezierCurve;
  pts : array of TPointF;
begin
  curve := BezierCurve(ApplyTransform(GetPenPos(cp1x,cp1y)),ApplyTransform(PointF(cp1x,cp1y)),
    ApplyTransform(PointF(cp2x,cp2y)),ApplyTransform(PointF(x,y)));
  pts := BGRAPath.ComputeBezierCurve(curve);
  AddPoints(pts);
  FLastCoord := PointF(x,y);
end;

procedure TBGRACanvas2D.bezierCurveTo(constref cp1, cp2, pt: TPointF);
begin
  bezierCurveTo(cp1.x,cp1.y,cp2.x,cp2.y,pt.x,pt.y);
end;

procedure TBGRACanvas2D.rect(x, y, w, h: single);
begin
  MoveTo(x,y);
  LineTo(x+w,y);
  LineTo(x+w,y+h);
  LineTo(x,y+h);
  closePath;
end;

procedure TBGRACanvas2D.roundRect(x, y, w, h, radius: single);
begin
  if radius <= 0 then
  begin
    rect(x,y,w,h);
    exit;
  end;
  if (w <= 0) or (h <= 0) then exit;
  if radius*2 > w then radius := w/2;
  if radius*2 > h then radius := h/2;
  moveTo(x+radius,y);
  arcTo(PointF(x+w,y),PointF(x+w,y+h), radius);
  arcTo(PointF(x+w,y+h),PointF(x,y+h), radius);
  arcTo(PointF(x,y+h),PointF(x,y), radius);
  arcTo(PointF(x,y),PointF(x+w,y), radius);
  closePath;
end;

procedure TBGRACanvas2D.roundRect(x, y, w, h, rx, ry: single);
begin
  if (w <= 0) or (h <= 0) then exit;
  if rx < 0 then rx := 0;
  if ry < 0 then ry := 0;
  if (rx = 0) and (ry = 0) then
  begin
    rect(x,y,w,h);
    exit;
  end;
  if rx*2 > w then rx := w/2;
  if ry*2 > h then ry := h/2;
  moveTo(x+rx,y);
  lineTo(x+w-rx,y);
  arcTo(rx,ry,0,false,false,x+w,y+ry);
  lineTo(x+w,y+h-ry);
  arcTo(rx,ry,0,false,false,x+w-rx,y+h);
  lineTo(x+rx,y+h);
  arcTo(rx,ry,0,false,false,x,y+h-ry);
  lineTo(x,y+ry);
  arcTo(rx,ry,0,false,false,x+rx,y);
  closePath;
end;

procedure TBGRACanvas2D.openedSpline(const pts: array of TPointF;
  style: TSplineStyle);
var transf: array of TPointF;
begin
  if length(pts)=0 then exit;
  transf := ApplyTransform(pts);
  transf := BGRAPath.ComputeOpenedSpline(transf,style);
  AddPoints(transf);
  FLastCoord := pts[high(pts)];
end;

procedure TBGRACanvas2D.closedSpline(const pts: array of TPointF;
  style: TSplineStyle);
var transf: array of TPointF;
begin
  if length(pts)=0 then exit;
  transf := ApplyTransform(pts);
  transf := BGRAPath.ComputeClosedSpline(slice(transf, length(transf)-1),style);
  AddPoints(transf);
  FLastCoord := pts[high(pts)];
end;

procedure TBGRACanvas2D.spline(const pts: array of TPointF; style: TSplineStyle);
var transf: array of TPointF;
begin
  if length(pts)=0 then exit;
  transf := ApplyTransform(pts);
  if (pts[0] = pts[high(pts)]) and (length(pts) > 1) then
    transf := BGRAPath.ComputeClosedSpline(slice(transf, length(transf)-1),style)
  else
    transf := BGRAPath.ComputeOpenedSpline(transf,style);
  AddPoints(transf);
  FLastCoord := pts[high(pts)];
end;

procedure TBGRACanvas2D.splineTo(const pts: array of TPointF;
  style: TSplineStyle);
var transf: array of TPointF;
  i: Integer;
begin
  if length(pts) = 0 then exit;
  transf := ApplyTransform(pts);
  if FPathPointCount <> 0 then
  begin
    setlength(transf,length(transf)+1);
    for i := high(transf) downto 1 do
      transf[i]:= transf[i-1];
    transf[0] := ApplyTransform(GetPenPos(pts[0].x,pts[0].y));
  end;
  transf := BGRAPath.ComputeOpenedSpline(transf,style);
  AddPoints(transf);
  FLastCoord := pts[high(pts)];
end;

procedure TBGRACanvas2D.arc(x, y, radius, startAngleRadCW, endAngleRadCW: single;
  anticlockwise: boolean);
var pts: array of TPointF;
  temp: single;
  pt: TPointF;
  rx,ry: single;
  len1,len2: single;
  unitAffine: TAffineMatrix;
  v1orig,v2orig,v1ortho,v2ortho: TPointF;
  startRadCCW,endRadCCW: single;
begin
  v1orig := PointF(currentState.matrix[1,1],currentState.matrix[2,1]);
  v2orig := PointF(currentState.matrix[1,2],currentState.matrix[2,2]);
  len1 := VectLen(v1orig);
  len2 := VectLen(v2orig);
  rx := len1*radius;
  ry := len2*radius;
  if len1 <> 0 then v1ortho := v1orig * (1/len1) else v1ortho := v1orig;
  if len2 <> 0 then v2ortho := v2orig * (1/len2) else v2ortho := v2orig;
  pt := currentState.matrix* PointF(x,y);
  unitAffine := AffineMatrix(v1ortho.x, v2ortho.x, pt.x,
                             v1ortho.y, v2ortho.y, pt.y);
  startRadCCW := -startAngleRadCW;
  endRadCCW := -endAngleRadCW;
  if not anticlockwise then
  begin
    temp := startRadCCW;
    startRadCCW := endRadCCW;
    endRadCCW:= temp;
    pts := BGRAPath.ComputeArcRad(0,0,rx,ry,startRadCCW,endRadCCW);
    pts := ApplyTransform(pts,unitAffine);
    AddPointsRev(pts);
  end else
  begin
    pts := BGRAPath.ComputeArcRad(0,0,rx,ry,startRadCCW,endRadCCW);
    pts := ApplyTransform(pts,unitAffine);
    AddPoints(pts);
  end;
  FLastCoord := ArcEndPoint(ArcDef(x,y,radius,radius,0,startAngleRadCW,endAngleRadCW,anticlockwise));
end;

procedure TBGRACanvas2D.arc(x, y, radius, startAngleRadCW, endAngleRadCW: single);
begin
  arc(x,y,radius,startAngleRadCW,endAngleRadCW,false);
end;

procedure TBGRACanvas2D.arc(cx, cy, rx, ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single;
  anticlockwise: boolean);
begin
  arc(ArcDef(cx,cy,rx,ry,xAngleRadCW,startAngleRadCW,endAngleRadCW,anticlockwise))
end;

procedure TBGRACanvas2D.arc(cx, cy, rx, ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single);
begin
  arc(ArcDef(cx,cy,rx,ry,xAngleRadCW,startAngleRadCW,endAngleRadCW,false))
end;

procedure TBGRACanvas2D.arc(constref arcDef: TArcDef);
var previousMatrix: TAffineMatrix;
begin
  if (arcDef.radius.x = 0) and (arcDef.radius.y = 0) then
    lineTo(arcDef.center) else
  begin
    previousMatrix := currentState.matrix;
    translate(arcDef.center.x,arcDef.center.y);
    rotate(arcDef.xAngleRadCW);
    scale(arcDef.radius.x,arcDef.radius.y);
    arc(0,0,1,arcDef.startAngleRadCW,arcDef.endAngleRadCW,arcDef.anticlockwise);
    currentState.matrix := previousMatrix;
    FLastCoord := ArcEndPoint(arcDef);
  end;
end;

procedure TBGRACanvas2D.arcTo(x1, y1, x2, y2, radius: single);
var p0: TPointF;
begin
  p0 := GetPenPos(x1,y1);
  arc(Html5ArcTo(p0,PointF(x1,y1),PointF(x2,y2),radius));
end;

procedure TBGRACanvas2D.arcTo(p1, p2: TPointF; radius: single);
begin
  arcTo(p1.x,p1.y,p2.x,p2.y,radius);
end;

procedure TBGRACanvas2D.arcTo(rx, ry, xAngleRadCW: single; largeArc,
  anticlockwise: boolean; x, y: single);
begin
  arc(SvgArcTo(GetPenPos(x,y), rx,ry, xAngleRadCW, largeArc, anticlockwise, PointF(x,y)));
  FLastCoord := PointF(x,y);
end;

procedure TBGRACanvas2D.circle(x, y, r: single);
begin
  arc(x,y,r,0,0);
end;

procedure TBGRACanvas2D.ellipse(x, y, rx, ry: single);
begin
  arc(x,y,rx,ry,0,0,0);
end;

procedure TBGRACanvas2D.text(AText: string; x, y: single);
var renderer : TBGRACustomFontRenderer;
  previousMatrix: TAffineMatrix;
  fva: TFontVerticalAnchor;
begin
  renderer := fontRenderer;
  if renderer = nil then exit;
  if renderer.FontEmHeight <= 0 then exit;

  case currentState.textBaseline of
    'bottom': fva := fvaBottom;
    'middle': fva := fvaCenter;
    'alphabetic': fva := fvaBaseline;
    else {'top','hanging'}
      fva := fvaTop;
  end;

  if renderer.HandlesTextPath then
  begin
    previousMatrix := currentState.matrix;
    translate(x,y);
    scale(currentState.fontEmHeight/renderer.FontEmHeight);
    if fva <> fvaTop then
    with renderer.GetFontPixelMetric do
    case fva of
      fvaBottom: translate(0,-Lineheight);
      fvaCenter: translate(0,-Lineheight/2);
      fvaBaseline: translate(0,-baseline);
    end;
    if direction=fbmAuto then
      renderer.CopyTextPathTo(self, 0,0, AText, textAlignLCL)
    else
      renderer.CopyTextPathTo(self, 0,0, AText, textAlignLCL, direction=fbmRightToLeft);
    currentState.matrix := previousMatrix;
  end else
  begin
    setlength(FTextPaths, length(FTextPaths)+1);
    FTextPaths[high(FTextPaths)].Text := AText;
    FTextPaths[high(FTextPaths)].FontName := fontName;
    FTextPaths[high(FTextPaths)].FontMatrix := currentState.matrix*AffineMatrixTranslation(x,y)*AffineMatrixScale(fontEmHeight,fontEmHeight);
    FTextPaths[high(FTextPaths)].FontStyle := fontStyle;
    FTextPaths[high(FTextPaths)].FontAlign := textAlignLCL;
    FTextPaths[high(FTextPaths)].FontAnchor := fva;
    FTextPaths[high(FTextPaths)].TextDirection := direction;
  end;

  FLastCoord := EmptyPointF;
  FStartCoord := EmptyPointF;
end;

procedure TBGRACanvas2D.fillText(AText: string; x, y: single);
begin
  beginPath;
  text(AText,x,y);
  fill;
  beginPath;
end;

procedure TBGRACanvas2D.strokeText(AText: string; x, y: single);
begin
  beginPath;
  text(AText,x,y);
  stroke;
  beginPath;
end;

function TBGRACanvas2D.measureText(AText: string): TCanvas2dTextSize;
var renderer: TBGRACustomFontRenderer;
  ratio: Single;
begin
  renderer := fontRenderer;
  if renderer <> nil then
  begin
    if renderer.FontEmHeight = 0 then
    begin
      result.width := 0;
      result.height:= 0;
    end else
    with renderer.TextSize(AText) do
    begin
      ratio := currentState.fontEmHeight/renderer.FontEmHeight;
      result.width := cx*ratio;
      result.height:= cy*ratio;
    end;
  end
  else
  begin
    result.width := 0;
    result.height := 0;
  end;
end;

procedure TBGRACanvas2D.fill;
begin
  if FPathPointCount > 0 then
    FillPoly(slice(FPathPoints,FPathPointCount));
  FillTexts(false);
end;

procedure TBGRACanvas2D.stroke(ADrawProc: TBGRAPathDrawProc; AData: pointer);
begin
  stroke(ADrawProc, AffineMatrixIdentity, AData);
end;

procedure TBGRACanvas2D.stroke(ADrawProc: TBGRAPathDrawProc;
  const AMatrix: TAffineMatrix; AData: pointer);
var
  startIndex: integer;

  procedure CallStrokeProc(AEndIndex: integer);
  var
    j: Integer;
    subPts: array of TPointF;
    closed: boolean;
  begin
    closed := false;
    while (AEndIndex>startIndex)
      and (FPathPoints[AEndIndex-1]=FPathPoints[startIndex]) do
    begin
      dec(AEndIndex);
      closed := true;
    end;
    if AEndIndex > startIndex then
    begin
      setlength(subPts, AEndIndex-startIndex);
      if IsAffineMatrixIdentity(AMatrix) then
      begin
        for j := 0 to high(subPts) do
          subPts[j] := FPathPoints[startIndex+j];
      end else
        for j := 0 to high(subPts) do
          subPts[j] := AMatrix*FPathPoints[startIndex+j];
      ADrawProc(subPts, closed, AData);
    end;
  end;

var i: integer;
begin
  startIndex := 0;
  for i := 0 to FPathPointCount-1 do
    if isEmptyPointF(FPathPoints[i]) then
    begin
      CallStrokeProc(i);
      startIndex := i+1;
    end;
  CallStrokeProc(FPathPointCount);
end;

procedure TBGRACanvas2D.stroke;
begin
  if FPathPointCount > 0 then
    StrokePoly(slice(FPathPoints,FPathPointCount));
end;

procedure TBGRACanvas2D.fill(AFillProc: TBGRAPathFillProc; AData: pointer);
begin
  fill(AFillProc, AffineMatrixIdentity, AData);
end;

procedure TBGRACanvas2D.fill(AFillProc: TBGRAPathFillProc;
  const AMatrix: TAffineMatrix; AData: pointer);
var
  startIndex: integer;

  procedure CallFillProc(AEndIndex: integer);
  var
    j: Integer;
    subPts: array of TPointF;
  begin
    if AEndIndex > startIndex then
    begin
      setlength(subPts, AEndIndex-startIndex);
      if IsAffineMatrixIdentity(AMatrix) then
      begin
        for j := 0 to high(subPts) do
          subPts[j] := FPathPoints[startIndex+j];
      end else
        for j := 0 to high(subPts) do
          subPts[j] := AMatrix*FPathPoints[startIndex+j];

      AFillProc(subPts, AData);
    end;
  end;

var i: integer;
begin
  startIndex := 0;
  for i := 0 to FPathPointCount-1 do
    if isEmptyPointF(FPathPoints[i]) then
    begin
      CallFillProc(i);
      startIndex := i+1;
    end;
  CallFillProc(FPathPointCount);
end;

procedure TBGRACanvas2D.fillOverStroke;
begin
  if FPathPointCount > 0 then
    FillStrokePoly(slice(FPathPoints,FPathPointCount),true);
  FillTexts(false);
end;

procedure TBGRACanvas2D.strokeOverFill;
begin
  FillTexts(false);
  if FPathPointCount > 0 then
    FillStrokePoly(slice(FPathPoints,FPathPointCount),false);
end;

procedure TBGRACanvas2D.clearPath;
begin
  if FPathPointCount > 0 then
    ClearPoly(slice(FPathPoints,FPathPointCount));
  FillTexts(true);
end;

procedure TBGRACanvas2D.clip;
var
  tempBmp: TGrayscaleMask;
begin
  if FPathPointCount = 0 then
  begin
    if currentState.clipMaskReadOnly <> nil then
      currentState.clipMaskReadWrite.Fill(BGRABlack);
    exit;
  end;
  if currentState.clipMaskReadOnly = nil then
    currentState.SetClipMask(TGrayscaleMask.Create(width,height,BGRAWhite),True);
  tempBmp := TGrayscaleMask.Create(width,height,BGRABlack);
  if antialiasing then
    tempBmp.FillPolyAntialias(slice(FPathPoints,FPathPointCount),BGRAWhite)
  else
    tempBmp.FillPoly(slice(FPathPoints,FPathPointCount),BGRAWhite);
  currentState.clipMaskReadWrite.ApplyMask(tempBmp);
  tempBmp.Free;
end;

procedure TBGRACanvas2D.unclip;
begin
  if FPathPointCount = 0 then exit;
  if currentState.clipMaskReadOnly = nil then exit;
  if antialiasing then
    currentState.clipMaskReadWrite.FillPolyAntialias(slice(FPathPoints,FPathPointCount),BGRAWhite)
  else
    currentState.clipMaskReadWrite.FillPoly(slice(FPathPoints,FPathPointCount),BGRAWhite,dmSet);
  if currentState.clipMaskReadOnly.Equals(BGRAWhite) then
    currentState.SetClipMask(nil,true);
end;

function TBGRACanvas2D.isPointInPath(x, y: single): boolean;
begin
  result := isPointInPath(PointF(x,y));
end;

function TBGRACanvas2D.isPointInPath(pt: TPointF): boolean;
begin
  if FPathPointCount <= 2 then
    result := false
  else
  begin
    setlength(FPathPoints,FPathPointCount);
    result := IsPointInPolygon(FPathPoints,pt+FCanvasOffset, fillMode = fmWinding);
  end;
end;

procedure TBGRACanvas2D.drawImage(image: TBGRACustomBitmap; dx, dy: single; AFilter: TResampleFilter);
var
  m: TAffineMatrix;
begin
  if (image.Width = 0) or (image.Height = 0) then exit;
  m := matrix*AffineMatrixTranslation(dx, dy);
  if pixelCenteredCoordinates then
    m := AffineMatrixTranslation(0.5, 0.5)*m;
  Surface.PutImageAffine(m, image, AFilter, GetDrawMode, currentState.globalAlpha, false);
end;

procedure TBGRACanvas2D.drawImage(image: TBGRACustomBitmap; dx, dy, dw, dh: single; AFilter: TResampleFilter);
var
  m: TAffineMatrix;
begin
  if (image.Width = 0) or (image.Height = 0) then exit;
  m := matrix*AffineMatrixTranslation(dx, dy)*AffineMatrixScale(dw/image.Width,dh/image.Height);
  if pixelCenteredCoordinates then
    m := AffineMatrixTranslation(0.5, 0.5)*m;
  Surface.PutImageAffine(m, image, AFilter, GetDrawMode, currentState.globalAlpha, false);
end;

end.

