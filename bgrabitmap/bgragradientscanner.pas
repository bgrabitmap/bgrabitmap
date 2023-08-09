// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAGradientScanner;

{$mode objfpc}{$H+}

interface

{ This unit contains scanners that generate gradients }

uses
  SysUtils, BGRABitmapTypes, BGRATransform;

type
  TBGRAColorInterpolation = (ciStdRGB, ciLinearRGB, ciLinearHSLPositive, ciLinearHSLNegative, ciGSBPositive, ciGSBNegative);
  TBGRAGradientRepetition = (grPad, grRepeat, grReflect, grSine);

  { TBGRASimpleGradient }

  TBGRASimpleGradient = class(TBGRACustomGradient)
  protected
    FColor1,FColor2: TBGRAPixel;
    ec1,ec2: TExpandedPixel;
    FRepetition: TBGRAGradientRepetition;
    function InterpolateToBGRA(position: word): TBGRAPixel; virtual; abstract;
    function InterpolateToExpanded(position: word): TExpandedPixel; virtual; abstract;
  public
    class function CreateAny(AInterpolation: TBGRAColorInterpolation; AColor1,AColor2: TBGRAPixel; ARepetition: TBGRAGradientRepetition): TBGRASimpleGradient; overload; static;
    class function CreateAny(AInterpolation: TBGRAColorInterpolation; AColor1,AColor2: TExpandedPixel; ARepetition: TBGRAGradientRepetition): TBGRASimpleGradient; overload; static;
    constructor Create(AColor1,AColor2: TBGRAPixel; ARepetition: TBGRAGradientRepetition); overload; //do not call directly
    constructor Create(AColor1,AColor2: TExpandedPixel; ARepetition: TBGRAGradientRepetition); overload; //do not call directly
    function GetColorAt(position: integer): TBGRAPixel; override;
    function GetColorAtF(position: single): TBGRAPixel; override;
    function GetExpandedColorAt(position: integer): TExpandedPixel; override;
    function GetExpandedColorAtF(position: single): TExpandedPixel; override;
    function GetAverageColor: TBGRAPixel; override;
    function GetAverageExpandedColor: TExpandedPixel; override;
    function GetMonochrome: boolean; override;
    property Repetition: TBGRAGradientRepetition read FRepetition write FRepetition;
  end;

  { TBGRASimpleGradientWithoutGammaCorrection }

  TBGRASimpleGradientWithoutGammaCorrection = class(TBGRASimpleGradient)
  protected
    function InterpolateToBGRA(position: word): TBGRAPixel; override;
    function InterpolateToExpanded(position: word): TExpandedPixel; override;
  public
    constructor Create(Color1,Color2: TBGRAPixel; ARepetition: TBGRAGradientRepetition = grPad); overload;
    constructor Create(Color1,Color2: TExpandedPixel; ARepetition: TBGRAGradientRepetition = grPad); overload;
  end;

  { TBGRASimpleGradientWithGammaCorrection }

  TBGRASimpleGradientWithGammaCorrection = class(TBGRASimpleGradient)
  protected
    function InterpolateToBGRA(position: word): TBGRAPixel; override;
    function InterpolateToExpanded(position: word): TExpandedPixel; override;
  public
    constructor Create(Color1,Color2: TBGRAPixel; ARepetition: TBGRAGradientRepetition = grPad); overload;
    constructor Create(Color1,Color2: TExpandedPixel; ARepetition: TBGRAGradientRepetition = grPad); overload;
  end;

  THueGradientOption = (hgoRepeat, hgoReflect,                       //repetition
                        hgoPositiveDirection, hgoNegativeDirection,  //hue orientation
                        hgoHueCorrection, hgoLightnessCorrection);   //color interpolation
  THueGradientOptions = set of THueGradientOption;

  { TBGRAHueGradient }

  TBGRAHueGradient = class(TBGRASimpleGradient)
  private
    hsla1,hsla2: THSLAPixel;
    hue1,hue2: LongWord;
    FOptions: THueGradientOptions;
    procedure Init(c1,c2: THSLAPixel; AOptions: THueGradientOptions);
    function InterpolateToHSLA(position: word): THSLAPixel;
  protected
    function InterpolateToBGRA(position: word): TBGRAPixel; override;
    function InterpolateToExpanded(position: word): TExpandedPixel; override;
  public
    constructor Create(Color1,Color2: TBGRAPixel; options: THueGradientOptions); overload;
    constructor Create(Color1,Color2: TExpandedPixel; options: THueGradientOptions); overload;
    constructor Create(Color1,Color2: THSLAPixel; options: THueGradientOptions); overload;
    constructor Create(AHue1,AHue2: Word; Saturation,Lightness: Word; options: THueGradientOptions); overload;
    function GetMonochrome: boolean; override;
  end;

  TGradientInterpolationFunction = function(t: single): single of object;

  { TBGRAMultiGradient }

  TBGRAMultiGradient = class(TBGRACustomGradient)
  private
    FColors: array of TBGRAPixel;
    FPositions: array of integer;
    FPositionsF: array of single;
    FEColors: array of TExpandedPixel;
    FCycle: Boolean;
    FInterpolationFunction: TGradientInterpolationFunction;
    procedure Init(Colors: array of TBGRAPixel; Positions0To1: array of single; AGammaCorrection, ACycle: boolean);
  public
    GammaCorrection: boolean;
    function CosineInterpolation(t: single): single;
    function HalfCosineInterpolation(t: single): single;
    constructor Create(Colors: array of TBGRAPixel; Positions0To1: array of single; AGammaCorrection: boolean; ACycle: boolean = false);
    function GetColorAt(position: integer): TBGRAPixel; override;
    function GetExpandedColorAt(position: integer): TExpandedPixel; override;
    function GetAverageColor: TBGRAPixel; override;
    function GetMonochrome: boolean; override;
    property InterpolationFunction: TGradientInterpolationFunction read FInterpolationFunction write FInterpolationFunction;
  end;

  { TBGRABufferedGradient }

  TBGRABufferedGradient = class(TBGRACustomGradient)
  protected
    FGradient: TBGRACustomGradient;
    FGradientOwned: boolean;
    FPadded: boolean;
    FAverageColorComputed: boolean;
    FAverageColorExpanded: TExpandedPixel;
    FMonochromeComputed: boolean;
    FMonochrome: boolean;
    FBufferSize, FBufferShift: integer;
    FColorTab: array of TBGRAPixel;
    FColorComputed: bitpacked array[0..65535] of boolean;
    FRepetition: TBGRAGradientRepetition;
  public
    constructor Create(AGradient: TBGRACustomGradient; AOwner: boolean; APadded: boolean;
      ABufferSize: integer);
    destructor Destroy; override;
    {** Returns the color at a given ''position''. The reference range is
        from 0 to 65535, however values beyond are possible as well }
    function GetColorAt(position: integer): TBGRAPixel; override;
    {** Returns the average color of the gradient }
    function GetAverageColor: TBGRAPixel; override;
    function GetAverageExpandedColor: TExpandedPixel; override;
    function GetMonochrome: boolean; override;
  end;

  TBGRAGradientScannerInternalScanNextFunc = function():single of object;
  TBGRAGradientScannerInternalScanAtFunc = function(const p: TPointF):single of object;

  { TBGRAGradientScanner }

  TBGRAGradientScanner = class(TBGRACustomScanner)
  protected
    FGradientType: TGradientType;
    FOrigin,FDir1,FDir2: TPointF;
    FRelativeFocal: TPointF;
    FRadius, FFocalRadius: single;
    FTransform, FHiddenTransform: TAffineMatrix;
    FSinus: Boolean;
    FGradient: TBGRACustomGradient;
    FGradientOwner: boolean;
    FFlipGradient: boolean;

    FMatrix: TAffineMatrix;
    FRepeatHoriz, FIsAverage: boolean;
    FAverageColor: TBGRAPixel;
    FAverageExpandedColor: TExpandedPixel;
    FScanNextFunc: TBGRAGradientScannerInternalScanNextFunc;
    FScanAtFunc: TBGRAGradientScannerInternalScanAtFunc;
    FGetGradientColor: TBGRAGradientGetColorAtFloatFunc;
    FGetGradientExpandedColor: TBGRAGradientGetExpandedColorAtFloatFunc;
    FFocalDistance: single;
    FFocalDirection, FFocalNormal: TPointF;
    FRadialDenominator, FRadialDeltaSign, maxW1, maxW2: single;

    FPosition: TPointF;
    FHorizColor: TBGRAPixel;
    FHorizExpandedColor: TExpandedPixel;

    procedure Init(AGradientType: TGradientType; AOrigin, d1: TPointF; ATransform: TAffineMatrix; Sinus: Boolean=False); overload;
    procedure Init(AGradientType: TGradientType; AOrigin, d1, d2: TPointF; ATransform: TAffineMatrix; Sinus: Boolean=False); overload;
    procedure Init(AOrigin: TPointF; ARadius: single; AFocal: TPointF; AFocalRadius: single; ATransform: TAffineMatrix; AHiddenTransform: TAffineMatrix); overload;

    procedure InitGradientType;
    procedure InitTransform;
    procedure InitGradient;

    function ComputeRadialFocal(const p: TPointF): single;

    function ScanNextLinear: single;
    function ScanNextReflected: single;
    function ScanNextDiamond: single;
    function ScanNextRadial: single;
    function ScanNextRadial2: single;
    function ScanNextRadialFocal: single;
    function ScanNextAngular: single;

    function ScanAtLinear(const p: TPointF): single;
    function ScanAtReflected(const p: TPointF): single;
    function ScanAtDiamond(const p: TPointF): single;
    function ScanAtRadial(const p: TPointF): single;
    function ScanAtRadial2(const p: TPointF): single;
    function ScanAtRadialFocal(const p: TPointF): single;
    function ScanAtAngular(const p: TPointF): single;

    function ScanNextInline: TBGRAPixel; inline;
    function ScanNextExpandedInline: TExpandedPixel; inline;
    procedure SetTransform(AValue: TAffineMatrix);
    procedure SetFlipGradient(AValue: boolean);
    procedure SetSinus(AValue: boolean);
    function GetGradientColor(a: single): TBGRAPixel;
    function GetGradientExpandedColor(a: single): TExpandedPixel;
    function GetGradientColorFlipped(a: single): TBGRAPixel;
    function GetGradientExpandedColorFlipped(a: single): TExpandedPixel;
    function GetGradientColorSinus(a: single): TBGRAPixel;
    function GetGradientExpandedColorSinus(a: single): TExpandedPixel;
    procedure UpdateGetGradientColorFunctions;
  public
    constructor Create(AGradientType: TGradientType; AOrigin, d1: TPointF); overload;
    constructor Create(AGradientType: TGradientType; AOrigin, d1, d2: TPointF); overload;
    constructor Create(AOrigin, d1, d2, AFocal: TPointF; ARadiusRatio: single = 1; AFocalRadiusRatio: single = 0); overload;
    constructor Create(AOrigin: TPointF; ARadius: single; AFocal: TPointF; AFocalRadius: single); overload;

    constructor Create(c1, c2: TBGRAPixel; AGradientType: TGradientType; AOrigin, d1: TPointF;
                       gammaColorCorrection: boolean = True; Sinus: Boolean=False); overload;
    constructor Create(c1, c2: TBGRAPixel; AGradientType: TGradientType; AOrigin, d1, d2: TPointF;
                       gammaColorCorrection: boolean = True; Sinus: Boolean=False); overload;

    constructor Create(gradient: TBGRACustomGradient; AGradientType: TGradientType; AOrigin, d1: TPointF;
                       Sinus: Boolean=False; AGradientOwner: Boolean=False); overload;
    constructor Create(gradient: TBGRACustomGradient; AGradientType: TGradientType; AOrigin, d1, d2: TPointF;
                       Sinus: Boolean=False; AGradientOwner: Boolean=False); overload;
    constructor Create(gradient: TBGRACustomGradient; AOrigin: TPointF; ARadius: single; AFocal: TPointF;
                       AFocalRadius: single; AGradientOwner: Boolean=False); overload;

    procedure SetGradient(c1,c2: TBGRAPixel; AGammaCorrection: boolean = true); overload;
    procedure SetGradient(AGradient: TBGRACustomGradient; AOwner: boolean); overload;
    destructor Destroy; override;
    procedure ScanMoveTo(X, Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanNextExpandedPixel: TExpandedPixel; override;
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    function ScanAtExpanded(X, Y: Single): TExpandedPixel; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    procedure ScanSkipPixels(ACount: integer); override;
    function IsScanPutPixelsDefined: boolean; override;
    property Transform: TAffineMatrix read FTransform write SetTransform;
    property Gradient: TBGRACustomGradient read FGradient;
    property FlipGradient: boolean read FFlipGradient write SetFlipGradient;
    property Sinus: boolean Read FSinus write SetSinus;
  end;

  { TBGRAConstantScanner }

  TBGRAConstantScanner = class(TBGRAGradientScanner)
    constructor Create(c: TBGRAPixel);
  end;

  { TBGRARandomScanner }

  TBGRARandomScanner = class(TBGRACustomScanner)
  private
    FOpacity: byte;
    FGrayscale: boolean;
    FRandomBuffer, FRandomBufferCount: integer;
  public
    constructor Create(AGrayscale: Boolean; AOpacity: byte);
    function ScanAtInteger({%H-}X, {%H-}Y: integer): TBGRAPixel; override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt({%H-}X, {%H-}Y: Single): TBGRAPixel; override;
  end;

  { TBGRAGradientTriangleScanner }

  TBGRAGradientTriangleScanner= class(TBGRACustomScanner)
  protected
    FMatrix: TAffineMatrix;
    FColor1,FDiff2,FDiff3,FStep: TColorF;
    FCurColor: TColorF;
  public
    constructor Create(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel);
    procedure ScanMoveTo(X,Y: Integer); override;
    procedure ScanMoveToF(X,Y: Single);
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanNextExpandedPixel: TExpandedPixel; override;
    procedure ScanSkipPixels(ACount: integer); override;
  end;

  { TBGRASolidColorMaskScanner }

  TBGRASolidColorMaskScanner = class(TBGRACustomScanner)
  private
    FOffset: TPoint;
    FMask: IBGRAScanner;
    FSolidColor: TBGRAPixel;
  public
    constructor Create(AMask: IBGRAScanner; AOffset: TPoint; ASolidColor: TBGRAPixel);
    destructor Destroy; override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    procedure ScanSkipPixels(ACount: integer); override;
    procedure ScanMoveTo(X,Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    property Color: TBGRAPixel read FSolidColor write FSolidColor;
  end;

  { TBGRATextureMaskScanner }

  TBGRATextureMaskScanner = class(TBGRACustomScanner)
  private
    FOffset: TPoint;
    FMask: IBGRAScanner;
    FTexture: IBGRAScanner;
    FTextureScanNext : TScanNextPixelFunction;
    FTextureScanAt : TScanAtFunction;
    FGlobalOpacity: Byte;
    FMemTex: packed array of TBGRAPixel;
  public
    constructor Create(AMask: IBGRAScanner; AOffset: TPoint; ATexture: IBGRAScanner; AGlobalOpacity: Byte = 255);
    destructor Destroy; override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    procedure ScanSkipPixels(ACount: integer); override;
    procedure ScanMoveTo(X,Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
  end;

  { TBGRAOpacityScanner }

  TBGRAOpacityScanner = class(TBGRACustomScanner)
  private
      FTexture: IBGRAScanner;
      FOwnedScanner: TBGRACustomScanner;
      FGlobalOpacity: Byte;
      FScanNext : TScanNextPixelFunction;
      FScanAt : TScanAtFunction;
      FMemTex: packed array of TBGRAPixel;
  public
    constructor Create(ATexture: IBGRAScanner; AGlobalOpacity: Byte = 255);
    constructor Create(ATexture: TBGRACustomScanner; AGlobalOpacity: Byte; AOwned: boolean);
    destructor Destroy; override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    procedure ScanSkipPixels(ACount: integer); override;
    procedure ScanMoveTo(X,Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
  end;

implementation

uses BGRABlend, Math;

{ TBGRABufferedGradient }

constructor TBGRABufferedGradient.Create(AGradient: TBGRACustomGradient;
  AOwner: boolean; APadded: boolean; ABufferSize: integer);
var
  bufferPowSize: integer;
begin
  FGradient := AGradient;
  FGradientOwned:= AOwner;
  FPadded := APadded;
  bufferPowSize := 0;
  while ABufferSize > 1 do
  begin
    ABufferSize := ABufferSize shr 1;
    inc(bufferPowSize);
  end;
  if bufferPowSize > 16 then bufferPowSize := 16;
  FBufferSize:= 1 shl bufferPowSize;
  setlength(FColorTab, FBufferSize);
  FBufferShift := 16-bufferPowSize;
end;

destructor TBGRABufferedGradient.Destroy;
begin
  if FGradientOwned then FGradient.Free;
  inherited Destroy;
end;

function TBGRABufferedGradient.GetColorAt(position: integer): TBGRAPixel;
var
  posBuf: Integer;
begin
  if FPadded then
  begin
    if position < 0 then
      position := 0 else
    if position >= 65536 then
      position := 65536;
    posBuf := position shr FBufferShift;
    if posBuf > FBufferSize shr 1 then dec(posBuf);
  end else
  begin
    position := position and 131071;
    posBuf := position shr (FBufferShift+1);
  end;

  if not FColorComputed[posBuf] then
  begin
    result := FGradient.GetColorAt(position);
    FColorTab[posBuf] := result;
    FColorComputed[posBuf] := true;
  end else
    result := FColorTab[posBuf];
end;

function TBGRABufferedGradient.GetAverageColor: TBGRAPixel;
begin
  result := GammaCompression(GetAverageExpandedColor);
end;

function TBGRABufferedGradient.GetAverageExpandedColor: TExpandedPixel;
begin
  if not FAverageColorComputed then
  begin
    FAverageColorExpanded := FGradient.GetAverageExpandedColor;
    FAverageColorComputed := true;
  end;
  result := FAverageColorExpanded;
end;

function TBGRABufferedGradient.GetMonochrome: boolean;
begin
  if not FMonochromeComputed then
  begin
    FMonochrome:= FGradient.Monochrome;
    FMonochromeComputed:= true;
  end;
  result := FMonochrome;
end;

{ TBGRASimpleGradient }

constructor TBGRASimpleGradient.Create(AColor1, AColor2: TBGRAPixel; ARepetition: TBGRAGradientRepetition);
begin
  FColor1 := AColor1;
  FColor2 := AColor2;
  ec1 := GammaExpansion(AColor1);
  ec2 := GammaExpansion(AColor2);
  FRepetition:= ARepetition;
end;

constructor TBGRASimpleGradient.Create(AColor1, AColor2: TExpandedPixel;
  ARepetition: TBGRAGradientRepetition);
begin
  FColor1 := GammaCompression(AColor1);
  FColor2 := GammaCompression(AColor2);
  ec1 := AColor1;
  ec2 := AColor2;
  FRepetition:= ARepetition;
end;

class function TBGRASimpleGradient.CreateAny(AInterpolation: TBGRAColorInterpolation;
  AColor1, AColor2: TBGRAPixel; ARepetition: TBGRAGradientRepetition): TBGRASimpleGradient;
begin
  case AInterpolation of
    ciStdRGB: result := TBGRASimpleGradientWithoutGammaCorrection.Create(AColor1,AColor2);
    ciLinearRGB: result := TBGRASimpleGradientWithGammaCorrection.Create(AColor1,AColor2);
    ciLinearHSLPositive: result := TBGRAHueGradient.Create(AColor1,AColor2,[hgoPositiveDirection]);
    ciLinearHSLNegative: result := TBGRAHueGradient.Create(AColor1,AColor2,[hgoNegativeDirection]);
    ciGSBPositive: result := TBGRAHueGradient.Create(AColor1,AColor2,[hgoPositiveDirection, hgoHueCorrection, hgoLightnessCorrection]);
    ciGSBNegative: result := TBGRAHueGradient.Create(AColor1,AColor2,[hgoNegativeDirection, hgoHueCorrection, hgoLightnessCorrection]);
    else
      raise Exception.Create('Unknown color interpolation');
  end;
  result.Repetition := ARepetition;
end;

class function TBGRASimpleGradient.CreateAny(AInterpolation: TBGRAColorInterpolation;
  AColor1, AColor2: TExpandedPixel; ARepetition: TBGRAGradientRepetition): TBGRASimpleGradient;
begin
  case AInterpolation of
    ciStdRGB: result := TBGRASimpleGradientWithoutGammaCorrection.Create(AColor1,AColor2);
    ciLinearRGB: result := TBGRASimpleGradientWithGammaCorrection.Create(AColor1,AColor2);
    ciLinearHSLPositive: result := TBGRAHueGradient.Create(AColor1,AColor2,[hgoPositiveDirection]);
    ciLinearHSLNegative: result := TBGRAHueGradient.Create(AColor1,AColor2,[hgoNegativeDirection]);
    ciGSBPositive: result := TBGRAHueGradient.Create(AColor1,AColor2,[hgoPositiveDirection, hgoHueCorrection, hgoLightnessCorrection]);
    ciGSBNegative: result := TBGRAHueGradient.Create(AColor1,AColor2,[hgoNegativeDirection, hgoHueCorrection, hgoLightnessCorrection]);
    else
      raise Exception.Create('Unknown color interpolation');
  end;
  result.Repetition := ARepetition;
end;

function TBGRASimpleGradient.GetAverageColor: TBGRAPixel;
begin
  result := InterpolateToBGRA(32768);
end;

function TBGRASimpleGradient.GetAverageExpandedColor: TExpandedPixel;
begin
  Result:= InterpolateToExpanded(32768);
end;

function TBGRASimpleGradient.GetColorAt(position: integer): TBGRAPixel;
begin
  case FRepetition of
  grSine: begin
            position := Sin65536(position and $ffff);
            if position = 65536 then
              result := FColor2
            else
              result := InterpolateToBGRA(position);
          end;
  grRepeat: result := InterpolateToBGRA(position and $ffff);
  grReflect:
    begin
      position := position and $1ffff;
      if position >= $10000 then
      begin
        if position = $10000 then
          result := FColor2
        else
          result := InterpolateToBGRA($20000 - position);
      end
      else
        result := InterpolateToBGRA(position);
    end;
  else
    begin
      if position <= 0 then
        result := FColor1 else
      if position >= 65536 then
        result := FColor2 else
        result := InterpolateToBGRA(position);
    end;
  end;
end;

function TBGRASimpleGradient.GetColorAtF(position: single): TBGRAPixel;
begin
  if position = EmptySingle then result := BGRAPixelTransparent else
  if FRepetition <> grPad then
    result := GetColorAt(round(frac(position*0.5)*131072)) else  //divided by 2 for reflected repetition
  begin
    if position <= 0 then
      result := FColor1 else
    if position >= 1 then
      result := FColor2 else
      result := GetColorAt(round(position*65536));
  end;
end;

function TBGRASimpleGradient.GetExpandedColorAt(position: integer
  ): TExpandedPixel;
begin
  case FRepetition of
  grSine: begin
            position := Sin65536(position and $ffff);
            if position = 65536 then
              result := ec2
            else
              result := InterpolateToExpanded(position);
          end;
  grRepeat: result := InterpolateToExpanded(position and $ffff);
  grReflect:
    begin
      position := position and $1ffff;
      if position >= $10000 then
      begin
        if position = $10000 then
          result := ec2
        else
          result := InterpolateToExpanded($20000 - position);
      end
      else
        result := InterpolateToExpanded(position);
    end;
  else
    begin
      if position <= 0 then
        result := ec1 else
      if position >= 65536 then
        result := ec2 else
        result := InterpolateToExpanded(position);
    end;
  end;
end;

function TBGRASimpleGradient.GetExpandedColorAtF(position: single
  ): TExpandedPixel;
begin
  if position = EmptySingle then result := BGRAPixelTransparent else
  if FRepetition <> grPad then
    result := GetExpandedColorAt(round(frac(position*0.5)*131072)) else  //divided by 2 for reflected repetition
  begin
    if position <= 0 then
      result := ec1 else
    if position >= 1 then
      result := ec2 else
      result := GetExpandedColorAt(round(position*65536));
  end;
end;

function TBGRASimpleGradient.GetMonochrome: boolean;
begin
  Result:= (FColor1 = FColor2);
end;

{ TBGRAConstantScanner }

constructor TBGRAConstantScanner.Create(c: TBGRAPixel);
begin
  inherited Create(c,c,gtLinear,PointF(0,0),PointF(0,0),false);
end;

{ TBGRARandomScanner }

constructor TBGRARandomScanner.Create(AGrayscale: Boolean; AOpacity: byte);
begin
  FGrayscale:= AGrayscale;
  FOpacity:= AOpacity;
  FRandomBufferCount := 0;
end;

function TBGRARandomScanner.ScanAtInteger(X, Y: integer): TBGRAPixel;
begin
  Result:=ScanNextPixel;
end;

function TBGRARandomScanner.ScanNextPixel: TBGRAPixel;
var rgb: integer;
begin
  if FGrayscale then
  begin
    if FRandomBufferCount = 0 then
    begin
      FRandomBuffer := random(256*256*256);
      FRandomBufferCount := 3;
    end;
    result.red := FRandomBuffer and 255;
    FRandomBuffer:= FRandomBuffer shr 8;
    dec(FRandomBufferCount);
    result.green := result.red;
    result.blue := result.red;
    result.alpha:= FOpacity;
  end else
  begin
    rgb := random(256*256*256);
    Result:= BGRA(rgb and 255,(rgb shr 8) and 255,(rgb shr 16) and 255,FOpacity);
  end;
end;

function TBGRARandomScanner.ScanAt(X, Y: Single): TBGRAPixel;
begin
  Result:=ScanNextPixel;
end;

{ TBGRAHueGradient }

procedure TBGRAHueGradient.Init(c1, c2: THSLAPixel; AOptions: THueGradientOptions);
begin
  FOptions:= AOptions;
  if (hgoLightnessCorrection in AOptions) then
  begin
    hsla1 := ExpandedToGSBA(ec1);
    hsla2 := ExpandedToGSBA(ec2);
  end else
  begin
    hsla1 := c1;
    hsla2 := c2;
  end;
  if not (hgoHueCorrection in AOptions) then
  begin
    hue1 := c1.hue;
    hue2 := c2.hue;
  end else
  begin
    hue1 := HtoG(c1.hue);
    hue2 := HtoG(c2.hue);
  end;
  if (hgoPositiveDirection in AOptions) and not (hgoNegativeDirection in AOptions) then
  begin
    if c2.hue <= c1.hue then inc(hue2, 65536);
  end else
  if not (hgoPositiveDirection in AOptions) and (hgoNegativeDirection in AOptions) then
  begin
    if c2.hue >= c1.hue then inc(hue1, 65536);
  end;
end;

function TBGRAHueGradient.InterpolateToHSLA(position: word): THSLAPixel;
var b,b2: LongWord;
begin
  b      := position shr 2;
  b2     := 16384-b;
  result.hue := ((hue1 * b2 + hue2 * b + 8191) shr 14) and $ffff;
  result.saturation := (hsla1.saturation * b2 + hsla2.saturation * b + 8191) shr 14;
  result.lightness := (hsla1.lightness * b2 + hsla2.lightness * b + 8191) shr 14;
  result.alpha := (hsla1.alpha * b2 + hsla2.alpha * b + 8191) shr 14;
  if hgoLightnessCorrection in FOptions then
  begin
    if not (hgoHueCorrection in FOptions) then
      result.hue := HtoG(result.hue);
  end else
  begin
    if hgoHueCorrection in FOptions then
      result.hue := GtoH(result.hue);
  end;
end;

function TBGRAHueGradient.InterpolateToBGRA(position: word): TBGRAPixel;
begin
  if hgoLightnessCorrection in FOptions then
    result := GSBAToBGRA(InterpolateToHSLA(position))
  else
    result := HSLAToBGRA(InterpolateToHSLA(position));
end;

function TBGRAHueGradient.InterpolateToExpanded(position: word): TExpandedPixel;
begin
  if hgoLightnessCorrection in FOptions then
    result := GSBAToExpanded(InterpolateToHSLA(position))
  else
    result := HSLAToExpanded(InterpolateToHSLA(position));
end;

constructor TBGRAHueGradient.Create(Color1, Color2: TBGRAPixel;options: THueGradientOptions);
begin
  if hgoReflect in options then
    inherited Create(Color1,Color2,grReflect)
  else if hgoRepeat in options then
    inherited Create(Color1,Color2,grRepeat)
  else
    inherited Create(Color1,Color2,grPad);

  Init(BGRAToHSLA(Color1),BGRAToHSLA(Color2),options);
end;

constructor TBGRAHueGradient.Create(Color1, Color2: TExpandedPixel;
  options: THueGradientOptions);
begin
  if hgoReflect in options then
    inherited Create(Color1,Color2,grReflect)
  else if hgoRepeat in options then
    inherited Create(Color1,Color2,grRepeat)
  else
    inherited Create(Color1,Color2,grPad);

  Init(ExpandedToHSLA(Color1),ExpandedToHSLA(Color2),options);
end;

constructor TBGRAHueGradient.Create(Color1, Color2: THSLAPixel; options: THueGradientOptions);
begin
  if hgoReflect in options then
    inherited Create(Color1.ToExpanded,Color2.ToExpanded,grReflect)
  else if hgoRepeat in options then
    inherited Create(Color1.ToExpanded,Color2.ToExpanded,grRepeat)
  else
    inherited Create(Color1.ToExpanded,Color2.ToExpanded,grPad);

  Init(Color1,Color2, options);
end;

constructor TBGRAHueGradient.Create(AHue1, AHue2: Word; Saturation,
  Lightness: Word; options: THueGradientOptions);
begin
  Create(HSLA(AHue1,saturation,lightness), HSLA(AHue2,saturation,lightness), options);
end;

function TBGRAHueGradient.GetMonochrome: boolean;
begin
  Result:= false;
end;

{ TBGRAMultiGradient }

procedure TBGRAMultiGradient.Init(Colors: array of TBGRAPixel;
  Positions0To1: array of single; AGammaCorrection, ACycle: boolean);
var
  i: Integer;
begin
  if length(Positions0To1) <> length(colors) then
    raise Exception.Create('Dimension mismatch');
  if length(Positions0To1) = 0 then
    raise Exception.Create('Empty gradient');
  setlength(FColors,length(Colors));
  setlength(FPositions,length(Positions0To1));
  setlength(FPositionsF,length(Positions0To1));
  setlength(FEColors,length(Colors));
  for i := 0 to high(colors) do
  begin
    FColors[i]:= colors[i];
    FPositions[i]:= round(Positions0To1[i]*65536);
    FPositionsF[i]:= Positions0To1[i];
    FEColors[i]:= GammaExpansion(colors[i]);
  end;
  GammaCorrection := AGammaCorrection;
  FCycle := ACycle;
  if FPositions[high(FPositions)] = FPositions[0] then FCycle := false;
end;

function TBGRAMultiGradient.CosineInterpolation(t: single): single;
begin
  result := (1-cos(t*Pi))*0.5;
end;

function TBGRAMultiGradient.HalfCosineInterpolation(t: single): single;
begin
  result := (1-cos(t*Pi))*0.25 + t*0.5;
end;

constructor TBGRAMultiGradient.Create(Colors: array of TBGRAPixel;
  Positions0To1: array of single; AGammaCorrection: boolean; ACycle: boolean);
begin
  Init(Colors,Positions0To1,AGammaCorrection, ACycle);
end;

function TBGRAMultiGradient.GetColorAt(position: integer): TBGRAPixel;
var i: Int32or64;
    ec: TExpandedPixel;
    curPos,posDiff: Int32or64;
begin
  if FCycle then
    position := (position-FPositions[0]) mod (FPositions[high(FPositions)] - FPositions[0]) + FPositions[0];
  if position <= FPositions[0] then
    result := FColors[0] else
  if position >= FPositions[high(FPositions)] then
    result := FColors[high(FColors)] else
  begin
    i := 0;
    while (i < high(FPositions)-1) and (position >= FPositions[i+1]) do
      inc(i);

    if Position = FPositions[i] then
      result := FColors[i]
    else
    begin
      curPos := position-FPositions[i];
      posDiff := FPositions[i+1]-FPositions[i];
      if FInterpolationFunction <> nil then
      begin
        curPos := round(FInterpolationFunction(curPos/posDiff)*65536);
        posDiff := 65536;
      end;
      if GammaCorrection then
      begin
        if FEColors[i+1].red < FEColors[i].red then
          ec.red := FEColors[i].red - UInt32or64(curPos)*UInt32or64(FEColors[i].red-FEColors[i+1].red) div UInt32or64(posDiff) else
          ec.red := FEColors[i].red + UInt32or64(curPos)*UInt32or64(FEColors[i+1].red-FEColors[i].red) div UInt32or64(posDiff);
        if FEColors[i+1].green < FEColors[i].green then
          ec.green := FEColors[i].green - UInt32or64(curPos)*UInt32or64(FEColors[i].green-FEColors[i+1].green) div UInt32or64(posDiff) else
          ec.green := FEColors[i].green + UInt32or64(curPos)*UInt32or64(FEColors[i+1].green-FEColors[i].green) div UInt32or64(posDiff);
        if FEColors[i+1].blue < FEColors[i].blue then
          ec.blue := FEColors[i].blue - UInt32or64(curPos)*UInt32or64(FEColors[i].blue-FEColors[i+1].blue) div UInt32or64(posDiff) else
          ec.blue := FEColors[i].blue + UInt32or64(curPos)*UInt32or64(FEColors[i+1].blue-FEColors[i].blue) div UInt32or64(posDiff);
        if FEColors[i+1].alpha < FEColors[i].alpha then
          ec.alpha := FEColors[i].alpha - UInt32or64(curPos)*UInt32or64(FEColors[i].alpha-FEColors[i+1].alpha) div UInt32or64(posDiff) else
          ec.alpha := FEColors[i].alpha + UInt32or64(curPos)*UInt32or64(FEColors[i+1].alpha-FEColors[i].alpha) div UInt32or64(posDiff);
        result := GammaCompression(ec);
      end else
      begin
        result.red := FColors[i].red + (curPos)*(FColors[i+1].red-FColors[i].red) div (posDiff);
        result.green := FColors[i].green + (curPos)*(FColors[i+1].green-FColors[i].green) div (posDiff);
        result.blue := FColors[i].blue + (curPos)*(FColors[i+1].blue-FColors[i].blue) div (posDiff);
        result.alpha := FColors[i].alpha + (curPos)*(FColors[i+1].alpha-FColors[i].alpha) div (posDiff);
      end;
    end;
  end;
end;

function TBGRAMultiGradient.GetExpandedColorAt(position: integer
  ): TExpandedPixel;
var i: Int32or64;
    curPos,posDiff: Int32or64;
    rw,gw,bw: UInt32or64;
begin
  if FCycle then
    position := (position-FPositions[0]) mod (FPositions[high(FPositions)] - FPositions[0]) + FPositions[0];
  if position <= FPositions[0] then
    result := FEColors[0] else
  if position >= FPositions[high(FPositions)] then
    result := FEColors[high(FColors)] else
  begin
    i := 0;
    while (i < high(FPositions)-1) and (position >= FPositions[i+1]) do
      inc(i);

    if Position = FPositions[i] then
      result := FEColors[i]
    else
    begin
      curPos := position-FPositions[i];
      posDiff := FPositions[i+1]-FPositions[i];
      if FInterpolationFunction <> nil then
      begin
        curPos := round(FInterpolationFunction(curPos/posDiff)*65536);
        posDiff := 65536;
      end;
      if GammaCorrection then
      begin
        if FEColors[i+1].red < FEColors[i].red then
          result.red := FEColors[i].red - UInt32or64(curPos)*UInt32or64(FEColors[i].red-FEColors[i+1].red) div UInt32or64(posDiff) else
          result.red := FEColors[i].red + UInt32or64(curPos)*UInt32or64(FEColors[i+1].red-FEColors[i].red) div UInt32or64(posDiff);
        if FEColors[i+1].green < FEColors[i].green then
          result.green := FEColors[i].green - UInt32or64(curPos)*UInt32or64(FEColors[i].green-FEColors[i+1].green) div UInt32or64(posDiff) else
          result.green := FEColors[i].green + UInt32or64(curPos)*UInt32or64(FEColors[i+1].green-FEColors[i].green) div UInt32or64(posDiff);
        if FEColors[i+1].blue < FEColors[i].blue then
          result.blue := FEColors[i].blue - UInt32or64(curPos)*UInt32or64(FEColors[i].blue-FEColors[i+1].blue) div UInt32or64(posDiff) else
          result.blue := FEColors[i].blue + UInt32or64(curPos)*UInt32or64(FEColors[i+1].blue-FEColors[i].blue) div UInt32or64(posDiff);
        if FEColors[i+1].alpha < FEColors[i].alpha then
          result.alpha := FEColors[i].alpha - UInt32or64(curPos)*UInt32or64(FEColors[i].alpha-FEColors[i+1].alpha) div UInt32or64(posDiff) else
          result.alpha := FEColors[i].alpha + UInt32or64(curPos)*UInt32or64(FEColors[i+1].alpha-FEColors[i].alpha) div UInt32or64(posDiff);
      end else
      begin
        rw := Int32or64(FColors[i].red shl 8) + (((curPos) shl 8)*(FColors[i+1].red-FColors[i].red)) div (posDiff);
        gw := Int32or64(FColors[i].green shl 8) + (((curPos) shl 8)*(FColors[i+1].green-FColors[i].green)) div (posDiff);
        bw := Int32or64(FColors[i].blue shl 8) + (((curPos) shl 8)*(FColors[i+1].blue-FColors[i].blue)) div (posDiff);

        if rw >= $ff00 then result.red := $ffff
        else result.red := (GammaExpansionTab[rw shr 8]*UInt32or64(255 - (rw and 255)) + GammaExpansionTab[(rw shr 8)+1]*UInt32or64(rw and 255)) shr 8;
        if gw >= $ff00 then result.green := $ffff
        else result.green := (GammaExpansionTab[gw shr 8]*UInt32or64(255 - (gw and 255)) + GammaExpansionTab[(gw shr 8)+1]*UInt32or64(gw and 255)) shr 8;
        if bw >= $ff00 then result.blue := $ffff
        else result.blue := (GammaExpansionTab[bw shr 8]*UInt32or64(255 - (bw and 255)) + GammaExpansionTab[(bw shr 8)+1]*UInt32or64(bw and 255)) shr 8;
        result.alpha := Int32or64(FColors[i].alpha shl 8) + (((curPos) shl 8)*(FColors[i+1].alpha-FColors[i].alpha)) div (posDiff);
        result.alpha := result.alpha + (result.alpha shr 8);
      end;
    end;
  end;
end;

function TBGRAMultiGradient.GetAverageColor: TBGRAPixel;
var sumR,sumG,sumB,sumA: integer;
  i: Integer;
begin
  sumR := 0;
  sumG := 0;
  sumB := 0;
  sumA := 0;
  for i := 0 to high(FColors) do
  begin
    inc(sumR, FColors[i].red);
    inc(sumG, FColors[i].green);
    inc(sumB, FColors[i].blue);
    inc(sumA, FColors[i].alpha);
  end;
  result := BGRA(sumR div length(FColors),sumG div length(FColors),
    sumB div length(FColors),sumA div length(FColors));
end;

function TBGRAMultiGradient.GetMonochrome: boolean;
var i: integer;
begin
  for i := 1 to high(FColors) do
    if FColors[i] <> FColors[0] then
    begin
      result := false;
      exit;
    end;
  Result:= true;
end;

{ TBGRASimpleGradientWithGammaCorrection }

function TBGRASimpleGradientWithGammaCorrection.InterpolateToBGRA(position: word
  ): TBGRAPixel;
var b,b2: LongWord;
    ec: TExpandedPixel;
begin
  b      := position;
  b2     := 65536-b;
  ec.red := (ec1.red * b2 + ec2.red * b + 32767) shr 16;
  ec.green := (ec1.green * b2 + ec2.green * b + 32767) shr 16;
  ec.blue := (ec1.blue * b2 + ec2.blue * b + 32767) shr 16;
  ec.alpha := (ec1.alpha * b2 + ec2.alpha * b + 32767) shr 16;
  result := GammaCompression(ec);
end;

function TBGRASimpleGradientWithGammaCorrection.InterpolateToExpanded(
  position: word): TExpandedPixel;
var b,b2: LongWord;
begin
  b      := position;
  b2     := 65536-b;
  result.red := (ec1.red * b2 + ec2.red * b + 32767) shr 16;
  result.green := (ec1.green * b2 + ec2.green * b + 32767) shr 16;
  result.blue := (ec1.blue * b2 + ec2.blue * b + 32767) shr 16;
  result.alpha := (ec1.alpha * b2 + ec2.alpha * b + 32767) shr 16;
end;

constructor TBGRASimpleGradientWithGammaCorrection.Create(Color1,
  Color2: TBGRAPixel; ARepetition: TBGRAGradientRepetition);
begin
  inherited Create(Color1,Color2,ARepetition);
end;

constructor TBGRASimpleGradientWithGammaCorrection.Create(Color1,
  Color2: TExpandedPixel; ARepetition: TBGRAGradientRepetition);
begin
  inherited Create(Color1,Color2,ARepetition);
end;

{ TBGRASimpleGradientWithoutGammaCorrection }

function TBGRASimpleGradientWithoutGammaCorrection.InterpolateToBGRA(
  position: word): TBGRAPixel;
var b,b2: LongWord;
begin
  b      := position shr 6;
  b2     := 1024-b;
  result.red  := (FColor1.red * b2 + FColor2.red * b + 511) shr 10;
  result.green := (FColor1.green * b2 + FColor2.green * b + 511) shr 10;
  result.blue := (FColor1.blue * b2 + FColor2.blue * b + 511) shr 10;
  result.alpha := (FColor1.alpha * b2 + FColor2.alpha * b + 511) shr 10;
end;

function TBGRASimpleGradientWithoutGammaCorrection.InterpolateToExpanded(
  position: word): TExpandedPixel;
var b,b2: LongWord;
    rw,gw,bw: word;
begin
  b      := position shr 6;
  b2     := 1024-b;
  rw  := (FColor1.red * b2 + FColor2.red * b + 511) shr 2;
  gw := (FColor1.green * b2 + FColor2.green * b + 511) shr 2;
  bw := (FColor1.blue * b2 + FColor2.blue * b + 511) shr 2;

  if rw >= $ff00 then
    result.red := 65535
  else
    result.red := (GammaExpansionTab[rw shr 8]*UInt32or64(255 - (rw and 255)) + GammaExpansionTab[(rw shr 8)+1]*UInt32or64(rw and 255)) shr 8;

  if gw >= $ff00 then
    result.green := 65535
  else
    result.green := (GammaExpansionTab[gw shr 8]*UInt32or64(255 - (gw and 255)) + GammaExpansionTab[(gw shr 8)+1]*UInt32or64(gw and 255)) shr 8;

  if bw >= $ff00 then
    result.blue := 65535
  else
    result.blue := (GammaExpansionTab[bw shr 8]*UInt32or64(255 - (bw and 255)) + GammaExpansionTab[(bw shr 8)+1]*UInt32or64(bw and 255)) shr 8;

  result.alpha := (FColor1.alpha * b2 + FColor2.alpha * b + 511) shr 2;
end;

constructor TBGRASimpleGradientWithoutGammaCorrection.Create(Color1,
  Color2: TBGRAPixel; ARepetition: TBGRAGradientRepetition);
begin
  inherited Create(Color1,Color2,ARepetition);
end;

constructor TBGRASimpleGradientWithoutGammaCorrection.Create(Color1,
  Color2: TExpandedPixel; ARepetition: TBGRAGradientRepetition);
begin
  inherited Create(Color1,Color2,ARepetition);
end;

{ TBGRAGradientTriangleScanner }

constructor TBGRAGradientTriangleScanner.Create(pt1, pt2, pt3: TPointF; c1, c2,
  c3: TBGRAPixel);
var ec1,ec2,ec3: TExpandedPixel;
begin
  FMatrix := AffineMatrix(pt2.X-pt1.X, pt3.X-pt1.X, 0,
                          pt2.Y-pt1.Y, pt3.Y-pt1.Y, 0);
  if not IsAffineMatrixInversible(FMatrix) then
    FMatrix := AffineMatrix(0,0,0,0,0,0)
  else
    FMatrix := AffineMatrixInverse(FMatrix) * AffineMatrixTranslation(-pt1.x,-pt1.y);

  ec1 := GammaExpansion(c1);
  ec2 := GammaExpansion(c2);
  ec3 := GammaExpansion(c3);
  FColor1[1] := ec1.red;
  FColor1[2] := ec1.green;
  FColor1[3] := ec1.blue;
  FColor1[4] := ec1.alpha;
  FDiff2[1] := ec2.red - ec1.red;
  FDiff2[2] := ec2.green - ec1.green;
  FDiff2[3] := ec2.blue - ec1.blue;
  FDiff2[4] := ec2.alpha - ec1.alpha;
  FDiff3[1] := ec3.red - ec1.red;
  FDiff3[2] := ec3.green - ec1.green;
  FDiff3[3] := ec3.blue - ec1.blue;
  FDiff3[4] := ec3.alpha - ec1.alpha;
  FStep := FDiff2*FMatrix[1,1]+FDiff3*FMatrix[2,1];
end;

procedure TBGRAGradientTriangleScanner.ScanMoveTo(X, Y: Integer);
begin
  ScanMoveToF(X, Y);
end;

procedure TBGRAGradientTriangleScanner.ScanMoveToF(X, Y: Single);
var
  Cur: TPointF;
begin
  Cur := FMatrix*PointF(X,Y);
  FCurColor := FColor1+FDiff2*Cur.X+FDiff3*Cur.Y;
end;

function TBGRAGradientTriangleScanner.ScanAt(X, Y: Single): TBGRAPixel;
begin
  ScanMoveToF(X,Y);
  result := ScanNextPixel;
end;

function TBGRAGradientTriangleScanner.ScanNextPixel: TBGRAPixel;
var r,g,b,a: int64;
begin
  r := round(FCurColor[1]);
  g := round(FCurColor[2]);
  b := round(FCurColor[3]);
  a := round(FCurColor[4]);
  if r > 65535 then r := 65535 else
  if r < 0 then r := 0;
  if g > 65535 then g := 65535 else
  if g < 0 then g := 0;
  if b > 65535 then b := 65535 else
  if b < 0 then b := 0;
  if a > 65535 then a := 65535 else
  if a < 0 then a := 0;
  result.red := GammaCompressionTab[r];
  result.green := GammaCompressionTab[g];
  result.blue := GammaCompressionTab[b];
  result.alpha := a shr 8;
  FCurColor := FCurColor + FStep;
end;

function TBGRAGradientTriangleScanner.ScanNextExpandedPixel: TExpandedPixel;
var r,g,b,a: int64;
begin
  r := round(FCurColor[1]);
  g := round(FCurColor[2]);
  b := round(FCurColor[3]);
  a := round(FCurColor[4]);
  if r > 65535 then r := 65535 else
  if r < 0 then r := 0;
  if g > 65535 then g := 65535 else
  if g < 0 then g := 0;
  if b > 65535 then b := 65535 else
  if b < 0 then b := 0;
  if a > 65535 then a := 65535 else
  if a < 0 then a := 0;
  result.red := r;
  result.green := g;
  result.blue := b;
  result.alpha := a;
  FCurColor := FCurColor + FStep;
end;

procedure TBGRAGradientTriangleScanner.ScanSkipPixels(ACount: integer);
begin
  FCurColor := FCurColor + FStep*ACount;
end;

{ TBGRAGradientScanner }

procedure TBGRAGradientScanner.SetTransform(AValue: TAffineMatrix);
begin
  if FTransform=AValue then Exit;
  FTransform:=AValue;
  InitTransform;
end;

constructor TBGRAGradientScanner.Create(AGradientType: TGradientType; AOrigin, d1: TPointF);
begin
  FGradient := nil;
  SetGradient(BGRABlack,BGRAWhite,False);
  Init(AGradientType,AOrigin,d1,AffineMatrixIdentity,False);
end;

constructor TBGRAGradientScanner.Create(AGradientType: TGradientType; AOrigin, d1,d2: TPointF);
begin
  FGradient := nil;
  SetGradient(BGRABlack,BGRAWhite,False);
  Init(AGradientType,AOrigin,d1,d2,AffineMatrixIdentity,False);
end;

constructor TBGRAGradientScanner.Create(AOrigin,
  d1, d2, AFocal: TPointF; ARadiusRatio: single; AFocalRadiusRatio: single);
var
  m, mInv: TAffineMatrix;
  focalInv: TPointF;
begin
  FGradient := nil;
  SetGradient(BGRABlack,BGRAWhite,False);

  m := AffineMatrix((d1-AOrigin).x, (d2-AOrigin).x, AOrigin.x,
                    (d1-AOrigin).y, (d2-AOrigin).y, AOrigin.y);
  if IsAffineMatrixInversible(m) then
  begin
    mInv := AffineMatrixInverse(m);
    focalInv := mInv*AFocal;
  end else
    focalInv := PointF(0,0);

  Init(PointF(0,0), ARadiusRatio, focalInv, AFocalRadiusRatio, AffineMatrixIdentity, m);
end;

constructor TBGRAGradientScanner.Create(AOrigin: TPointF; ARadius: single;
  AFocal: TPointF; AFocalRadius: single);
begin
  FGradient := nil;
  SetGradient(BGRABlack,BGRAWhite,False);

  Init(AOrigin, ARadius, AFocal, AFocalRadius, AffineMatrixIdentity, AffineMatrixIdentity);
end;

procedure TBGRAGradientScanner.SetFlipGradient(AValue: boolean);
begin
  if FFlipGradient=AValue then Exit;
  FFlipGradient:=AValue;
  UpdateGetGradientColorFunctions;
end;

function TBGRAGradientScanner.GetGradientColor(a: single): TBGRAPixel;
begin
  if a = EmptySingle then
    result := BGRAPixelTransparent
  else
  begin
    if FFlipGradient then a := 1-a;
    if FSinus then
    begin
      a := a*65536;
      if (a <= low(int64)) or (a >= high(int64)) then
        result := FAverageColor
      else
        result := FGradient.GetColorAt(Sin65536(round(a) and 65535));
    end else
      result := FGradient.GetColorAtF(a);
  end;
end;

function TBGRAGradientScanner.GetGradientExpandedColor(a: single): TExpandedPixel;
begin
  if a = EmptySingle then
    QWord(result) := 0
  else
  begin
    if FFlipGradient then a := 1-a;
    if FSinus then
    begin
      a := a * 65536;
      if (a <= low(int64)) or (a >= high(int64)) then
        result := FAverageExpandedColor
      else
        result := FGradient.GetExpandedColorAt(Sin65536(round(a) and 65535));
    end else
      result := FGradient.GetExpandedColorAtF(a);
  end;
end;

function TBGRAGradientScanner.GetGradientColorFlipped(a: single): TBGRAPixel;
begin
  result := FGradient.GetColorAtF(1 - a);
end;

function TBGRAGradientScanner.GetGradientExpandedColorFlipped(a: single): TExpandedPixel;
begin
  result := FGradient.GetExpandedColorAtF(1 - a);
end;

function TBGRAGradientScanner.GetGradientColorSinus(a: single): TBGRAPixel;
begin
  if FFlipGradient then a := 1-a;
  a := a * 65536;
  if (a <= low(int64)) or (a >= high(int64)) then
    result := FAverageColor
    else result := FGradient.GetColorAt(Sin65536(round(a) and 65535));
end;

function TBGRAGradientScanner.GetGradientExpandedColorSinus(a: single): TExpandedPixel;
begin
  if FFlipGradient then a := 1-a;
  a := a * 65536;
  if (a <= low(int64)) or (a >= high(int64)) then
    result := FAverageExpandedColor
    else result := FGradient.GetExpandedColorAt(Sin65536(round(a) and 65535));
end;

procedure TBGRAGradientScanner.UpdateGetGradientColorFunctions;
begin
  if FSinus then
  begin
    FGetGradientColor:= @GetGradientColorSinus;
    FGetGradientExpandedColor:= @GetGradientExpandedColorSinus;
  end else
  if FFlipGradient then
  begin
    FGetGradientColor:= @GetGradientColorFlipped;
    FGetGradientExpandedColor:= @GetGradientExpandedColorFlipped;
  end else
  begin
    FGetGradientColor:= @FGradient.GetColorAtF;
    FGetGradientExpandedColor:= @FGradient.GetExpandedColorAtF;
  end;
end;

procedure TBGRAGradientScanner.SetSinus(AValue: boolean);
begin
  if FSinus=AValue then Exit;
  FSinus:=AValue;
  UpdateGetGradientColorFunctions;
end;

procedure TBGRAGradientScanner.Init(AGradientType: TGradientType; AOrigin, d1: TPointF;
  ATransform: TAffineMatrix; Sinus: Boolean);
var d2: TPointF;
begin
  with (d1-AOrigin) do
    d2 := PointF(AOrigin.x+y,AOrigin.y-x);
  Init(AGradientType,AOrigin,d1,d2,ATransform,Sinus);
end;

procedure TBGRAGradientScanner.Init(AGradientType: TGradientType; AOrigin, d1, d2: TPointF;
  ATransform: TAffineMatrix; Sinus: Boolean);
begin
  FGradientType:= AGradientType;
  FFlipGradient:= false;
  FOrigin := AOrigin;
  FDir1 := d1;
  FDir2 := d2;
  FSinus := Sinus;
  FTransform := ATransform;
  FHiddenTransform := AffineMatrixIdentity;

  FRadius := 1;
  FRelativeFocal := PointF(0,0);
  FFocalRadius := 0;

  InitGradientType;
  InitTransform;
  UpdateGetGradientColorFunctions;
end;

procedure TBGRAGradientScanner.Init(AOrigin: TPointF; ARadius: single;
  AFocal: TPointF; AFocalRadius: single; ATransform: TAffineMatrix; AHiddenTransform: TAffineMatrix);
var maxRadius: single;
begin
  FGradientType:= gtRadial;
  FFlipGradient:= false;
  FOrigin := AOrigin;
  ARadius := abs(ARadius);
  AFocalRadius := abs(AFocalRadius);
  maxRadius := max(ARadius,AFocalRadius);
  FDir1 := AOrigin+PointF(maxRadius,0);
  FDir2 := AOrigin+PointF(0,maxRadius);
  FSinus := False;
  FTransform := ATransform;
  FHiddenTransform := AHiddenTransform;

  FRadius := ARadius/maxRadius;
  FRelativeFocal := (AFocal - AOrigin)*(1/maxRadius);
  FFocalRadius := AFocalRadius/maxRadius;

  InitGradientType;
  InitTransform;
  UpdateGetGradientColorFunctions;
end;

procedure TBGRAGradientScanner.InitGradientType;
begin
  case FGradientType of
    gtReflected: begin
      FScanNextFunc:= @ScanNextReflected;
      FScanAtFunc:= @ScanAtReflected;
    end;
    gtDiamond: begin
      FScanNextFunc:= @ScanNextDiamond;
      FScanAtFunc:= @ScanAtDiamond;
    end;
    gtRadial: if (FRelativeFocal.x = 0) and (FRelativeFocal.y = 0) then
    begin
      if (FFocalRadius = 0) and (FRadius = 1) then
      begin
        FScanNextFunc:= @ScanNextRadial;
        FScanAtFunc:= @ScanAtRadial;
      end else
      begin
        FScanNextFunc:= @ScanNextRadial2;
        FScanAtFunc:= @ScanAtRadial2;
      end;
    end else
    begin
      FScanNextFunc:= @ScanNextRadialFocal;
      FScanAtFunc:= @ScanAtRadialFocal;

      FFocalDirection := FRelativeFocal;
      FFocalDistance := VectLen(FFocalDirection);
      if FFocalDistance > 0 then FFocalDirection := FFocalDirection * (1/FFocalDistance);
      FFocalNormal := PointF(-FFocalDirection.y,FFocalDirection.x);
      FRadialDenominator := sqr(FRadius-FFocalRadius)-sqr(FFocalDistance);

      //case in which the second circle is bigger and the first circle is within the second
      if (FRadius < FFocalRadius) and (FFocalDistance <= FFocalRadius-FRadius) then
        FRadialDeltaSign := -1
      else
        FRadialDeltaSign := 1;

      //clipping afer the apex
      if (FFocalRadius < FRadius) and (FFocalDistance > FRadius-FFocalRadius) then
      begin
        maxW1 := FRadius/(FRadius-FFocalRadius)*FFocalDistance;
        maxW2 := MaxSingle;
      end else
      if (FRadius < FFocalRadius) and (FFocalDistance > FFocalRadius-FRadius) then
      begin
        maxW1 := MaxSingle;
        maxW2 := FFocalRadius/(FFocalRadius-FRadius)*FFocalDistance;
      end else
      begin
        maxW1 := MaxSingle;
        maxW2 := MaxSingle;
      end;
    end;
    gtAngular: begin
      FScanNextFunc:= @ScanNextAngular;
      FScanAtFunc:= @ScanAtAngular;
    end;
  else
    {gtLinear:} begin
      FScanNextFunc:= @ScanNextLinear;
      FScanAtFunc:= @ScanAtLinear;
    end;
  end;
end;

procedure TBGRAGradientScanner.SetGradient(c1, c2: TBGRAPixel;
  AGammaCorrection: boolean);
begin
  if Assigned(FGradient) and FGradientOwner then FreeAndNil(FGradient);

  //transparent pixels have no color so
  //take it from other color
  if c1.alpha = 0 then c1 := BGRA(c2.red,c2.green,c2.blue,0);
  if c2.alpha = 0 then c2 := BGRA(c1.red,c1.green,c1.blue,0);

  if AGammaCorrection then
    FGradient := TBGRASimpleGradientWithGammaCorrection.Create(c1,c2)
  else
    FGradient := TBGRASimpleGradientWithoutGammaCorrection.Create(c1,c2);
  FGradientOwner := true;
  InitGradient;
end;

procedure TBGRAGradientScanner.SetGradient(AGradient: TBGRACustomGradient;
  AOwner: boolean);
begin
  if Assigned(FGradient) and FGradientOwner then FreeAndNil(FGradient);
  FGradient := AGradient;
  FGradientOwner := AOwner;
  InitGradient;
end;

procedure TBGRAGradientScanner.InitTransform;
var u,v: TPointF;
begin
  u := FDir1-FOrigin;
  if FGradientType in[gtLinear,gtReflected] then
    v := PointF(u.y, -u.x)
  else
    v := FDir2-FOrigin;

  FMatrix := FTransform * FHiddenTransform * AffineMatrix(u.x, v.x, FOrigin.x,
                                                          u.y, v.y, FOrigin.y);
  if IsAffineMatrixInversible(FMatrix) then
  begin
    FMatrix := AffineMatrixInverse(FMatrix);
    FIsAverage:= false;
  end else
  begin
    FMatrix := AffineMatrixIdentity;
    FIsAverage:= true;
  end;

  case FGradientType of
    gtReflected: FRepeatHoriz := (FMatrix[1,1]=0);
    gtDiamond,gtAngular: FRepeatHoriz:= false;
    gtRadial: begin
      FRepeatHoriz:= false;
      if FFocalRadius = FRadius then FIsAverage:= true;
    end
  else
    {gtLinear:} FRepeatHoriz := (FMatrix[1,1]=0);
  end;

  if FGradient.Monochrome then
    FIsAverage:= true;

  if FIsAverage then
    FRepeatHoriz:= true;

  FPosition := PointF(0,0);
end;

procedure TBGRAGradientScanner.InitGradient;
begin
  FAverageColor := FGradient.GetAverageColor;
  FAverageExpandedColor := FGradient.GetAverageExpandedColor;
  UpdateGetGradientColorFunctions;
end;

function TBGRAGradientScanner.ComputeRadialFocal(const p: TPointF): single;
var
  w1,w2,num: single;
  h,d1,d2,delta: double;
begin
  w1 := p**FFocalDirection;
  w2 := FFocalDistance-w1;
  if (w1 < maxW1) and (w2 < maxW2) then
  begin
    //vertical position and distances
    h := sqr(p**FFocalNormal);
    d1 := sqr(w1)+h;
    d2 := sqr(w2)+h;
    //finding t
    delta := sqr(FFocalRadius)*d1 + 2*FRadius*FFocalRadius*(p**(FRelativeFocal-p))+
             sqr(FRadius)*d2 - sqr(VectDet(p,FRelativeFocal));
    if delta >= 0 then
    begin
      num := -FFocalRadius*(FRadius-FFocalRadius)-(FRelativeFocal**(FRelativeFocal-p));
      result := (num+FRadialDeltaSign*sqrt(delta))/FRadialDenominator;
    end else
      result := EmptySingle;
  end else
    result := EmptySingle;
end;

function TBGRAGradientScanner.ScanNextLinear: single;
begin
  result := FPosition.x;
end;

function TBGRAGradientScanner.ScanNextReflected: single;
begin
  result := abs(FPosition.x);
end;

function TBGRAGradientScanner.ScanNextDiamond: single;
begin
  result := max(abs(FPosition.x), abs(FPosition.y));
end;

function TBGRAGradientScanner.ScanNextRadial: single;
begin
  result := sqrt(sqr(FPosition.x) + sqr(FPosition.y));
end;

function TBGRAGradientScanner.ScanNextRadial2: single;
begin
  result := (sqrt(sqr(FPosition.x) + sqr(FPosition.y))-FFocalRadius)/(FRadius-FFocalRadius);
end;

function TBGRAGradientScanner.ScanNextRadialFocal: single;
begin
  result := ComputeRadialFocal(FPosition);
end;

function TBGRAGradientScanner.ScanNextAngular: single;
begin
  if FPosition.y >= 0 then
    result := arctan2(FPosition.y,FPosition.x)/(2*Pi)
  else
    result := 1-arctan2(-FPosition.y,FPosition.x)/(2*Pi)
end;

function TBGRAGradientScanner.ScanAtLinear(const p: TPointF): single;
begin
  with (FMatrix*p) do
    result := x;
end;

function TBGRAGradientScanner.ScanAtReflected(const p: TPointF): single;
begin
  with (FMatrix*p) do
    result := abs(x);
end;

function TBGRAGradientScanner.ScanAtDiamond(const p: TPointF): single;
begin
  with (FMatrix*p) do
    result := max(abs(x), abs(y));
end;

function TBGRAGradientScanner.ScanAtRadial(const p: TPointF): single;
begin
  with (FMatrix*p) do
    result := sqrt(sqr(x) + sqr(y));
end;

function TBGRAGradientScanner.ScanAtRadial2(const p: TPointF): single;
begin
  with (FMatrix*p) do
    result := (sqrt(sqr(x) + sqr(y))-FFocalRadius)/(FRadius-FFocalRadius);
end;

function TBGRAGradientScanner.ScanAtRadialFocal(const p: TPointF): single;
begin
  result := ComputeRadialFocal(FMatrix*p);
end;

function TBGRAGradientScanner.ScanAtAngular(const p: TPointF): single;
begin
  with (FMatrix*p) do
  begin
    if y >= 0 then
      result := arctan2(y,x)/(2*Pi)
    else
      result := 1-arctan2(-y,x)/(2*Pi)
  end;
end;

function TBGRAGradientScanner.ScanNextInline: TBGRAPixel;
begin
  result := FGetGradientColor(FScanNextFunc());
  FPosition.x := FPosition.x + FMatrix[1,1];
  FPosition.y := FPosition.y + FMatrix[2,1];
end;

function TBGRAGradientScanner.ScanNextExpandedInline: TExpandedPixel;
begin
  result := FGetGradientExpandedColor(FScanNextFunc());
  FPosition.x := FPosition.x + FMatrix[1,1];
  FPosition.y := FPosition.y + FMatrix[2,1];
end;

constructor TBGRAGradientScanner.Create(c1, c2: TBGRAPixel;
  AGradientType: TGradientType; AOrigin, d1: TPointF; gammaColorCorrection: boolean;
  Sinus: Boolean);
begin
  FGradient := nil;
  SetGradient(c1,c2,gammaColorCorrection);
  Init(AGradientType,AOrigin,d1,AffineMatrixIdentity,Sinus);
end;

constructor TBGRAGradientScanner.Create(c1, c2: TBGRAPixel;
  AGradientType: TGradientType; AOrigin, d1, d2: TPointF; gammaColorCorrection: boolean;
  Sinus: Boolean);
begin
  FGradient := nil;
  if AGradientType in[gtLinear,gtReflected] then raise EInvalidArgument.Create('Two directions are not required for linear and reflected gradients');
  SetGradient(c1,c2,gammaColorCorrection);
  Init(AGradientType,AOrigin,d1,d2,AffineMatrixIdentity,Sinus);
end;

constructor TBGRAGradientScanner.Create(gradient: TBGRACustomGradient;
  AGradientType: TGradientType; AOrigin, d1: TPointF; Sinus: Boolean; AGradientOwner: Boolean=False);
begin
  FGradient := gradient;
  FGradientOwner := AGradientOwner;
  InitGradient;
  Init(AGradientType,AOrigin,d1,AffineMatrixIdentity,Sinus);
end;

constructor TBGRAGradientScanner.Create(gradient: TBGRACustomGradient;
  AGradientType: TGradientType; AOrigin, d1, d2: TPointF; Sinus: Boolean;
  AGradientOwner: Boolean);
begin
  if AGradientType in[gtLinear,gtReflected] then raise EInvalidArgument.Create('Two directions are not required for linear and reflected gradients');
  FGradient := gradient;
  FGradientOwner := AGradientOwner;
  InitGradient;
  Init(AGradientType,AOrigin,d1,d2,AffineMatrixIdentity,Sinus);
end;

constructor TBGRAGradientScanner.Create(gradient: TBGRACustomGradient;
  AOrigin: TPointF; ARadius: single; AFocal: TPointF; AFocalRadius: single;
  AGradientOwner: Boolean);
begin
  FGradient := gradient;
  FGradientOwner := AGradientOwner;
  InitGradient;
  Init(AOrigin, ARadius, AFocal, AFocalRadius, AffineMatrixIdentity, AffineMatrixIdentity);
end;

destructor TBGRAGradientScanner.Destroy;
begin
  if FGradientOwner then
    FGradient.Free;
  inherited Destroy;
end;

procedure TBGRAGradientScanner.ScanMoveTo(X, Y: Integer);
begin
  FPosition := FMatrix*PointF(x,y);
  if FRepeatHoriz then
  begin
    if FIsAverage then
    begin
      FHorizColor := FAverageColor;
      FHorizExpandedColor := FAverageExpandedColor;
    end else
    begin
      FHorizColor := ScanNextInline;
      FHorizExpandedColor := ScanNextExpandedInline;
    end;
  end;
end;

function TBGRAGradientScanner.ScanNextPixel: TBGRAPixel;
begin
  if FRepeatHoriz then
    result := FHorizColor
  else
    result := ScanNextInline;
end;

function TBGRAGradientScanner.ScanNextExpandedPixel: TExpandedPixel;
begin
  if FRepeatHoriz then
    result := FHorizExpandedColor
  else
    result := ScanNextExpandedInline;
end;

function TBGRAGradientScanner.ScanAt(X, Y: Single): TBGRAPixel;
begin
  if FIsAverage then
    result := FAverageColor
  else
    result := GetGradientColor(FScanAtFunc(PointF(X,Y)));
end;

function TBGRAGradientScanner.ScanAtExpanded(X, Y: Single): TExpandedPixel;
begin
  if FIsAverage then
    result := FAverageExpandedColor
  else
    result := GetGradientExpandedColor(FScanAtFunc(PointF(X,Y)));
end;

procedure TBGRAGradientScanner.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
var c: TBGRAPixel;
begin
  if FRepeatHoriz then
  begin
    c := FHorizColor;
    case mode of
      dmDrawWithTransparency: DrawPixelsInline(pdest,c,count);
      dmLinearBlend: FastBlendPixelsInline(pdest,c,count);
      dmSet: FillDWord(pdest^,count,Longword(c));
      dmXor: XorInline(pdest,c,count);
      dmSetExceptTransparent: if c.alpha = 255 then FillDWord(pdest^,count,Longword(c));
    end;
    exit;
  end;

  case mode of
    dmDrawWithTransparency:
      while count > 0 do
      begin
        DrawPixelInlineWithAlphaCheck(pdest,ScanNextInline);
        inc(pdest);
        dec(count);
      end;
    dmLinearBlend:
      while count > 0 do
      begin
        FastBlendPixelInline(pdest,ScanNextInline);
        inc(pdest);
        dec(count);
      end;
    dmXor:
      while count > 0 do
      begin
        PLongWord(pdest)^ := PLongWord(pdest)^ xor LongWord(ScanNextInline);
        inc(pdest);
        dec(count);
      end;
    dmSet:
      while count > 0 do
      begin
        pdest^ := ScanNextInline;
        inc(pdest);
        dec(count);
      end;
    dmSetExceptTransparent:
      while count > 0 do
      begin
        c := ScanNextInline;
        if c.alpha = 255 then pdest^ := c;
        inc(pdest);
        dec(count);
      end;
  end;
end;

procedure TBGRAGradientScanner.ScanSkipPixels(ACount: integer);
begin
  if not FRepeatHoriz and not FIsAverage then
    FPosition.Offset(FMatrix[1,1]*ACount,FMatrix[2,1]*ACount);
end;

function TBGRAGradientScanner.IsScanPutPixelsDefined: boolean;
begin
  result := true;
end;

{ TBGRATextureMaskScanner }

constructor TBGRATextureMaskScanner.Create(AMask: IBGRAScanner;
  AOffset: TPoint; ATexture: IBGRAScanner; AGlobalOpacity: Byte);
begin
  FMask := AMask;
  FOffset := AOffset;
  FTexture := ATexture;
  FTextureScanNext := @FTexture.ScanNextPixel;
  FTextureScanAt := @FTexture.ScanAt;
  FGlobalOpacity:= AGlobalOpacity;
end;

destructor TBGRATextureMaskScanner.Destroy;
begin
  fillchar(FMask,sizeof(FMask),0); //avoids interface deref
  fillchar(FTexture,sizeof(FTexture),0);
  inherited Destroy;
end;

function TBGRATextureMaskScanner.IsScanPutPixelsDefined: boolean;
begin
  Result:= true;
end;

procedure TBGRATextureMaskScanner.ScanPutPixels(pdest: PBGRAPixel;
  count: integer; mode: TDrawMode);
var c: TBGRAPixel;
    ptex: pbgrapixel;
    pmask: PByteMask;
    stride, qty: integer;

  function GetNext: TBGRAPixel; inline;
  begin
    result := ptex^;
    inc(ptex);
    result.alpha := ApplyOpacity(result.alpha, pmask^.gray);
    inc(pmask, stride);
  end;

  function GetNextWithGlobal: TBGRAPixel; inline;
  begin
    result := ptex^;
    inc(ptex);
    result.alpha := ApplyOpacity( ApplyOpacity(result.alpha, pmask^.gray), FGlobalOpacity );
    inc(pmask, stride);
  end;

begin
  if count > length(FMemTex) then setlength(FMemTex, max(length(FMemTex)*2,count));
  ScannerPutPixels(FTexture,@FMemTex[0],count,dmSet);
  ptex := @FMemTex[0];
  while count > 0 do
  begin
    qty := count;
    FMask.ScanNextMaskChunk(qty, pMask, stride);
    dec(count, qty);
    if FGlobalOpacity <> 255 then
    begin
      case mode of
        dmDrawWithTransparency:
          while qty > 0 do
          begin
            DrawPixelInlineWithAlphaCheck(pdest,GetNextWithGlobal);
            inc(pdest);
            dec(qty);
          end;
        dmLinearBlend:
          while qty > 0 do
          begin
            FastBlendPixelInline(pdest,GetNextWithGlobal);
            inc(pdest);
            dec(qty);
          end;
        dmXor:
          while qty > 0 do
          begin
            PLongWord(pdest)^ := PLongWord(pdest)^ xor LongWord(GetNextWithGlobal);
            inc(pdest);
            dec(qty);
          end;
        dmSet:
          while qty > 0 do
          begin
            pdest^ := GetNextWithGlobal;
            inc(pdest);
            dec(qty);
          end;
        dmSetExceptTransparent:
          while qty > 0 do
          begin
            c := GetNextWithGlobal;
            if c.alpha = 255 then pdest^ := c;
            inc(pdest);
            dec(qty);
          end;
      end;
    end else
    begin
      case mode of
        dmDrawWithTransparency:
          while qty > 0 do
          begin
            DrawPixelInlineWithAlphaCheck(pdest,GetNext);
            inc(pdest);
            dec(qty);
          end;
        dmLinearBlend:
          while qty > 0 do
          begin
            FastBlendPixelInline(pdest,GetNext);
            inc(pdest);
            dec(qty);
          end;
        dmXor:
          while qty > 0 do
          begin
            PLongWord(pdest)^ := PLongWord(pdest)^ xor LongWord(GetNext);
            inc(pdest);
            dec(qty);
          end;
        dmSet:
          while qty > 0 do
          begin
            pdest^ := GetNext;
            inc(pdest);
            dec(qty);
          end;
        dmSetExceptTransparent:
          while qty > 0 do
          begin
            c := GetNext;
            if c.alpha = 255 then pdest^ := c;
            inc(pdest);
            dec(qty);
          end;
      end;
    end;
  end;
end;

procedure TBGRATextureMaskScanner.ScanSkipPixels(ACount: integer);
begin
  FMask.ScanSkipPixels(ACount);
  FTexture.ScanSkipPixels(ACount);
end;

procedure TBGRATextureMaskScanner.ScanMoveTo(X, Y: Integer);
begin
  FMask.ScanMoveTo(X+FOffset.X,Y+FOffset.Y);
  FTexture.ScanMoveTo(X,Y);
end;

function TBGRATextureMaskScanner.ScanNextPixel: TBGRAPixel;
var
  pMask: PByteMask;
  stride, qty: integer;
begin
  qty := 1;
  FMask.ScanNextMaskChunk(qty,pMask,stride);
  result := FTextureScanNext();
  result.alpha := ApplyOpacity( ApplyOpacity(result.alpha,pMask^.gray), FGlobalOpacity );
end;

function TBGRATextureMaskScanner.ScanAt(X, Y: Single): TBGRAPixel;
var alpha: byte;
begin
  alpha := FMask.ScanAtMask(X+FOffset.X,Y+FOffset.Y).gray;
  result := FTextureScanAt(X,Y);
  result.alpha := ApplyOpacity( ApplyOpacity(result.alpha,alpha), FGlobalOpacity );
end;

{ TBGRASolidColorMaskScanner }

constructor TBGRASolidColorMaskScanner.Create(AMask: IBGRAScanner;
  AOffset: TPoint; ASolidColor: TBGRAPixel);
begin
  FMask := AMask;
  FOffset := AOffset;
  FSolidColor := ASolidColor;
end;

destructor TBGRASolidColorMaskScanner.Destroy;
begin
  fillchar(FMask,sizeof(FMask),0); //avoids interface deref
  inherited Destroy;
end;

function TBGRASolidColorMaskScanner.IsScanPutPixelsDefined: boolean;
begin
  Result:= true;
end;

procedure TBGRASolidColorMaskScanner.ScanPutPixels(pdest: PBGRAPixel;
  count: integer; mode: TDrawMode);
var c: TBGRAPixel;
    pmask: PByteMask;
    stride, qty: integer;

  function GetNext: TBGRAPixel; inline;
  begin
    result := FSolidColor;
    result.alpha := ApplyOpacity(result.alpha,pmask^.gray);
    inc(pmask, stride);
  end;

begin
  while count > 0 do
  begin
    qty := count;
    FMask.ScanNextMaskChunk(qty, pMask, stride);
    dec(count, qty);
    case mode of
      dmDrawWithTransparency:
        while qty > 0 do
        begin
          DrawPixelInlineWithAlphaCheck(pdest,GetNext);
          inc(pdest);
          dec(qty);
        end;
      dmLinearBlend:
        while qty > 0 do
        begin
          FastBlendPixelInline(pdest,GetNext);
          inc(pdest);
          dec(qty);
        end;
      dmXor:
        while qty > 0 do
        begin
          PLongWord(pdest)^ := PLongWord(pdest)^ xor LongWord(GetNext);
          inc(pdest);
          dec(qty);
        end;
      dmSet:
        while qty > 0 do
        begin
          pdest^ := GetNext;
          inc(pdest);
          dec(qty);
        end;
      dmSetExceptTransparent:
        while qty > 0 do
        begin
          c := GetNext;
          if c.alpha = 255 then pdest^ := c;
          inc(pdest);
          dec(qty);
        end;
    end;
  end;
end;

procedure TBGRASolidColorMaskScanner.ScanSkipPixels(ACount: integer);
begin
  FMask.ScanSkipPixels(ACount);
end;

procedure TBGRASolidColorMaskScanner.ScanMoveTo(X, Y: Integer);
begin
  FMask.ScanMoveTo(X+FOffset.X,Y+FOffset.Y);
end;

function TBGRASolidColorMaskScanner.ScanNextPixel: TBGRAPixel;
var
  pMask: PByteMask;
  stride, qty: integer;
begin
  qty := 1;
  FMask.ScanNextMaskChunk(qty,pMask,stride);
  result := FSolidColor;
  result.alpha := ApplyOpacity(result.alpha,pMask^.gray);
end;

function TBGRASolidColorMaskScanner.ScanAt(X, Y: Single): TBGRAPixel;
var alpha: byte;
begin
  alpha := FMask.ScanAtMask(X,Y).gray;
  result := FSolidColor;
  result.alpha := ApplyOpacity(result.alpha,alpha);
end;

{ TBGRAOpacityScanner }

constructor TBGRAOpacityScanner.Create(ATexture: IBGRAScanner;
  AGlobalOpacity: Byte);
begin
  FTexture := ATexture;
  FScanNext := @FTexture.ScanNextPixel;
  FScanAt := @FTexture.ScanAt;
  FGlobalOpacity:= AGlobalOpacity;
  FOwnedScanner := nil;
end;

constructor TBGRAOpacityScanner.Create(ATexture: TBGRACustomScanner;
  AGlobalOpacity: Byte; AOwned: boolean);
begin
  FTexture := ATexture;
  FScanNext := @FTexture.ScanNextPixel;
  FScanAt := @FTexture.ScanAt;
  FGlobalOpacity:= AGlobalOpacity;
  if AOwned then
    FOwnedScanner := ATexture
  else
    FOwnedScanner := nil;
end;

destructor TBGRAOpacityScanner.Destroy;
begin
  fillchar(FTexture,sizeof(FTexture),0);
  FOwnedScanner.Free;
  inherited Destroy;
end;

function TBGRAOpacityScanner.IsScanPutPixelsDefined: boolean;
begin
  Result:= true;
end;

procedure TBGRAOpacityScanner.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
var c: TBGRAPixel;
    ptex: pbgrapixel;

  function GetNext: TBGRAPixel; inline;
  begin
    result := ptex^;
    inc(ptex);
    result.alpha := ApplyOpacity(result.alpha,FGlobalOpacity);
  end;

begin
  if count > length(FMemTex) then setlength(FMemTex, max(length(FMemTex)*2,count));
  ScannerPutPixels(FTexture,@FMemTex[0],count,dmSet);

  ptex := @FMemTex[0];

  case mode of
    dmDrawWithTransparency:
      while count > 0 do
      begin
        DrawPixelInlineWithAlphaCheck(pdest,GetNext);
        inc(pdest);
        dec(count);
      end;
    dmLinearBlend:
      while count > 0 do
      begin
        FastBlendPixelInline(pdest,GetNext);
        inc(pdest);
        dec(count);
      end;
    dmXor:
      while count > 0 do
      begin
        PLongWord(pdest)^ := PLongWord(pdest)^ xor LongWord(GetNext);
        inc(pdest);
        dec(count);
      end;
    dmSet:
      while count > 0 do
      begin
        pdest^ := GetNext;
        inc(pdest);
        dec(count);
      end;
    dmSetExceptTransparent:
      while count > 0 do
      begin
        c := GetNext;
        if c.alpha = 255 then pdest^ := c;
        inc(pdest);
        dec(count);
      end;
  end;
end;

procedure TBGRAOpacityScanner.ScanSkipPixels(ACount: integer);
begin
  FTexture.ScanSkipPixels(ACount);
end;

procedure TBGRAOpacityScanner.ScanMoveTo(X, Y: Integer);
begin
  FTexture.ScanMoveTo(X,Y);
end;

function TBGRAOpacityScanner.ScanNextPixel: TBGRAPixel;
begin
  result := FScanNext();
  result.alpha := ApplyOpacity(result.alpha, FGlobalOpacity );
end;

function TBGRAOpacityScanner.ScanAt(X, Y: Single): TBGRAPixel;
begin
  result := FScanAt(X,Y);
  result.alpha := ApplyOpacity(result.alpha, FGlobalOpacity );
end;

initialization

  Randomize;

end.

