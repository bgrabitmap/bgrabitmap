// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Basic types used with OpenGL }
unit BGRAOpenGLType;

{$mode objfpc}{$H+}

interface

uses
  BGRAGraphics, BGRABitmap, BGRABitmapTypes,
  FPimage, BGRAClasses, SysUtils, BGRATransform,
  BGRASSE, BGRAMatrix3D;

type
  TBGLTextureHandle = type Pointer;
  TOpenGLResampleFilter = (orfBox,orfLinear);
  TOpenGLRepeatMode = (ormRepeat, ormMirroredRepeat, ormClamp);
  TOpenGLBlendMode = (obmNormal, obmAdd, obmMultiply);
  TWaitForGPUOption = (wfgQueueAllCommands, wfgFinishAllCommands);
  TFaceCulling = BGRABitmapTypes.TFaceCulling;
  TOpenGLPrimitive = (opPoints,opLineStrip,opLineLoop,opLines,
                  opTriangleStrip,opTriangleFan,opTriangles);

const
  fcNone = BGRABitmapTypes.fcNone;
  fcKeepCW = BGRABitmapTypes.fcKeepCW;
  fcKeepCCW = BGRABitmapTypes.fcKeepCCW;

type

  { Interface for a font drawn on OpenGL canvas }
  IBGLFont = interface
    function GetClipped: boolean;
    function GetPadding: TRectF;
    function GetUseGradientColors: boolean;
    function GetHorizontalAlign: TAlignment;
    function GetJustify: boolean;
    function GetScale: single;
    function GetStepX: single;
    function GetVerticalAlign: TTextLayout;
    procedure SetClipped(AValue: boolean);
    procedure SetPadding(AValue: TRectF);
    procedure SetUseGradientColors(AValue: boolean);
    procedure SetHorizontalAlign(AValue: TAlignment);
    procedure SetJustify(AValue: boolean);
    procedure SetScale(AValue: single);
    procedure SetStepX(AValue: single);
    procedure SetVerticalAlign(AValue: TTextLayout);
    procedure TextOut(X, Y: Single; const Text : UTF8String); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AVertAlign: TTextLayout); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AVertAlign: TTextLayout); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    function TextWidth(const Text: UTF8String): single;
    function TextHeight(const Text: UTF8String): single; overload;
    function TextHeight(const Text: UTF8String; AWidth: single): single; overload;
    procedure SetGradientColors(ATopLeft, ATopRight, ABottomRight, ABottomLeft: TBGRAPixel);

    property Scale: single read GetScale write SetScale;
    property StepX: single read GetStepX write SetStepX;
    property Justify: boolean read GetJustify write SetJustify;
    property Clipped: boolean read GetClipped write SetClipped;
    property HorizontalAlign: TAlignment read GetHorizontalAlign write SetHorizontalAlign;
    property VerticalAlign: TTextLayout read GetVerticalAlign write SetVerticalAlign;
    property GradientColors: boolean read GetUseGradientColors write SetUseGradientColors;
    property Padding: TRectF read GetPadding write SetPadding;
  end;

  { Abstract class for a font drawn on OpenGL canvas }
  TBGLCustomFont = class(TInterfacedObject, IBGLFont)
  protected
    FScale, FStepX: single;
    FPadding: TRectF;
    FFlags: LongWord;
    FHorizontalAlign: TAlignment;
    FVerticalAlign: TTextLayout;
    FJustify: boolean;
    procedure Init; virtual;
    function LoadFromFile(AFilename: UTF8String): boolean; virtual; abstract;
    procedure FreeMemoryOnDestroy; virtual;

    function GetScale: single; virtual;
    function GetStepX: single; virtual;
    procedure SetScale(AValue: single); virtual;
    procedure SetStepX(AValue: single); virtual;
    function GetPadding: TRectF;
    procedure SetPadding(AValue: TRectF); virtual;

    function GetHorizontalAlign: TAlignment; virtual;
    function GetJustify: boolean; virtual;
    function GetVerticalAlign: TTextLayout; virtual;
    procedure SetHorizontalAlign(AValue: TAlignment); virtual;
    procedure SetJustify(AValue: boolean); virtual;
    procedure SetVerticalAlign(AValue: TTextLayout); virtual;

    function GetClipped: boolean; virtual; abstract;
    function GetUseGradientColors: boolean; virtual; abstract;
    procedure SetClipped(AValue: boolean); virtual; abstract;
    procedure SetUseGradientColors(AValue: boolean); virtual; abstract;

    procedure DoTextOut(X, Y: Single; const Text : UTF8String; AColor: TBGRAPixel); virtual; abstract;
    procedure DoTextRect(X, Y, Width, Height: Single; const Text : UTF8String; AColor: TBGRAPixel); virtual; abstract;

    function GetDefaultColor: TBGRAPixel; virtual;
    procedure SwapRectIfNeeded(var ARect: TRectF); overload;
    procedure SwapRectIfNeeded(var ARect: TRect); overload;
  public
    constructor Create(AFilename: UTF8String);
    procedure FreeMemory; virtual;
    destructor Destroy; override;
    procedure TextOut(X, Y: Single; const Text : UTF8String); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AVertAlign: TTextLayout); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRect; const Text : UTF8String); overload;
    procedure TextRect(ARect: TRect; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRect; const Text : UTF8String; AVertAlign: TTextLayout); overload;
    procedure TextRect(ARect: TRect; const Text : UTF8String; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRect; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextRect(ARect: TRect; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AVertAlign: TTextLayout); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    function TextWidth(const Text: UTF8String): single; virtual; abstract;
    function TextHeight(const Text: UTF8String): single; overload; virtual; abstract;
    function TextHeight(const Text: UTF8String; AWidth: single): single; overload; virtual; abstract;
    procedure SetGradientColors(ATopLeft, ATopRight, ABottomRight, ABottomLeft: TBGRAPixel); virtual; abstract;

    property Scale: single read GetScale write SetScale;
    property StepX: single read GetStepX write SetStepX;
    property Justify: boolean read GetJustify write SetJustify;
    property Clipped: boolean read GetClipped write SetClipped;
    property HorizontalAlign: TAlignment read GetHorizontalAlign write SetHorizontalAlign;
    property VerticalAlign: TTextLayout read GetVerticalAlign write SetVerticalAlign;
    property GradientColors: boolean read GetUseGradientColors write SetUseGradientColors;
    property Padding: TRectF read GetPadding write SetPadding;
  end;

  { Interface for a texture in OpenGL (stored in VRAM) }
  IBGLTexture = interface ['{BF2FF051-EBC6-4102-8268-37A9D0297B92}']
    function GetAllocatedHeight: integer;
    function GetAllocatedWidth: integer;
    function GetFlipX: IBGLTexture;
    function GetFlipY: IBGLTexture;
    function GetFrame(AIndex: integer): IBGLTexture;
    function GetFrameCount: integer;
    function GetFrameHeight: integer;
    function GetFrameWidth: integer;
    function GetHeight: integer;
    function GetImageCenter: TPointF;
    function GetMask: IBGLTexture;
    function GetOpenGLBlendMode: TOpenGLBlendMode;
    function GetOpenGLTexture: TBGLTextureHandle;
    function GetRepeatX: TOpenGLRepeatMode;
    function GetRepeatY: TOpenGLRepeatMode;
    function GetResampleFilter: TOpenGLResampleFilter;
    function GetUseGradientColors: boolean;
    function GetWidth: integer;

    procedure SetFrameSize(x,y: integer);
    procedure SetImageCenter(const AValue: TPointF);
    procedure SetOpenGLBlendMode(AValue: TOpenGLBlendMode);
    procedure SetRepetition(AValueX, AValueY: TOpenGLRepeatMode);
    procedure SetResampleFilter(AValue: TOpenGLResampleFilter);
    procedure SetGradientColors(ATopLeft, ATopRight, ABottomRight, ABottomLeft: TBGRAPixel);
    procedure SetUseGradientColors(AValue: boolean);
    procedure Update(ARGBAData: PLongWord; AllocatedWidth, AllocatedHeight, ActualWidth,ActualHeight: integer; RGBAOrder: boolean = true);
    procedure ToggleFlipX;
    procedure ToggleFlipY;
    procedure ToggleMask;
    function FilterBlurMotion(ARadius: single; ABlurType: TRadialBlurType; ADirection: TPointF): IBGLTexture;
    function FilterBlurRadial(ARadius: single; ABlurType: TRadialBlurType): IBGLTexture;
    procedure SetFrame(AIndex: integer);
    procedure FreeMemory;
    procedure Bind(ATextureNumber: integer);

    procedure Draw(x,y: single; AAlpha: byte = 255); overload;
    procedure Draw(x,y: single; AColor: TBGRAPixel); overload;
    procedure Draw(x,y: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure Draw(x,y: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure StretchDraw(x,y,w,h: single; AAlpha: byte = 255); overload;
    procedure StretchDraw(x,y,w,h: single; AColor: TBGRAPixel); overload;
    procedure StretchDraw(x,y,w,h: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure StretchDraw(x,y,w,h: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure DrawAffine(const Origin, HAxis, VAxis: TPointF; AAlpha: byte = 255); overload;
    procedure DrawAffine(const Origin, HAxis, VAxis: TPointF; AColor: TBGRAPixel); overload;
    procedure DrawAffine(x,y: single; const AMatrix: TAffineMatrix; AAlpha: byte = 255); overload;
    procedure DrawAffine(x,y: single; const AMatrix: TAffineMatrix; AColor: TBGRAPixel); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;

    property AllocatedWidth: integer read GetAllocatedWidth;
    property AllocatedHeight: integer read GetAllocatedHeight;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property FrameCount: integer read GetFrameCount;
    property Frame[AIndex: integer]: IBGLTexture read GetFrame;
    property FrameWidth: integer read GetFrameWidth;
    property FrameHeight: integer read GetFrameHeight;
    property FlipX: IBGLTexture read GetFlipX;
    property FlipY: IBGLTexture read GetFlipY;
    property Mask: IBGLTexture read GetMask;
    property Handle: TBGLTextureHandle read GetOpenGLTexture;
    property ImageCenter: TPointF read GetImageCenter write SetImageCenter;
    property RepeatX: TOpenGLRepeatMode read GetRepeatX;
    property RepeatY: TOpenGLRepeatMode read GetRepeatY;
    property ResampleFilter: TOpenGLResampleFilter read GetResampleFilter write SetResampleFilter;
    property BlendMode: TOpenGLBlendMode read GetOpenGLBlendMode write SetOpenGLBlendMode;
    property GradientColors: boolean read GetUseGradientColors write SetUseGradientColors;
  end;

  { Abstract RGBA bitmap that can be used with OpenGL by converting it into a texture }
  TBGLCustomBitmap = class(TBGRABitmap)
  protected
    FActualWidth,FActualHeight,
    FAllocatedWidth,FAllocatedHeight: integer;
    FTextureInvalidated: boolean;
    FActualRect: TRect;
    FTexture: IBGLTexture;
    procedure Init; override;
    function GetTexture: IBGLTexture; virtual;
    class function GetOpenGLMaxTexSize: integer; virtual; abstract;
    procedure NotifySizeTooBigForOpenGL; virtual;
    procedure NotifyOpenGLContextNotCreatedYet; virtual;
    function GetTextureGL: IUnknown; override;
    procedure SwapRedBlueWithoutInvalidate(ARect: TRect);
    procedure SetClipRect(const AValue: TRect); override;
  public
    procedure InvalidateBitmap; override;
    procedure Fill(const c: TBGRAPixel); override;
    procedure NoClip; override;
    destructor Destroy; override;
    procedure SwapRedBlue; overload; override;
    function Resample(newWidth, newHeight: integer; mode: TResampleMode=rmFineResample; ACopyProperties: Boolean=False): TBGLCustomBitmap; override;
    procedure ApplyGlobalOpacity(alpha: byte); overload; override;
    procedure ReplaceColor(before, after: TColor); overload; override;
    procedure ReplaceColor(const ABefore, AAfter: TBGRAPixel); overload; override;
    procedure ReplaceTransparent(const AAfter: TBGRAPixel); overload; override;
    procedure SetSize(AWidth, AHeight: integer); override;
    property Width: integer read FActualWidth;
    property Height: integer read FActualHeight;
    property AllocatedWidth: integer read FAllocatedWidth;
    property AllocatedHeight: integer read FAllocatedHeight;
    function MakeTextureAndFree: IBGLTexture;
    property Texture: IBGLTexture read GetTexture;
    property MaxTextureSize: integer read GetOpenGLMaxTexSize;
  end;

  { Abstract class for a texture in OpenGL (stored in VRAM) }
  TBGLCustomTexture = class(TInterfacedObject, IBGLTexture)
  private
    function GetAllocatedHeight: integer;
    function GetAllocatedWidth: integer;
    function GetFlipX: IBGLTexture;
    function GetFlipY: IBGLTexture;
    function GetFrame(AIndex: integer): IBGLTexture;
    function GetFrameCount: integer;
    function GetFrameHeight: integer;
    function GetFrameWidth: integer;
    function GetHeight: integer;
    function GetMask: IBGLTexture;
    function GetOpenGLBlendMode: TOpenGLBlendMode;
    function GetOpenGLTexture: TBGLTextureHandle;
    function GetWidth: integer;
    function GetImageCenter: TPointF;
    procedure SetImageCenter(const AValue: TPointF);
    function GetResampleFilter: TOpenGLResampleFilter;
    procedure SetOpenGLBlendMode(AValue: TOpenGLBlendMode);
    procedure SetResampleFilter(AValue: TOpenGLResampleFilter);
  protected
    FOpenGLTexture: TBGLTextureHandle;
    FOpenGLTextureOwned: boolean;
    FResampleFilter: TOpenGLResampleFilter;
    FWidth,FHeight: integer;
    FImageCenter: TPointF;
    FFrame: integer;
    FFrameWidth,FFrameHeight: integer;
    FIsMask: boolean;
    FGradTopLeft, FGradTopRight, FGradBottomRight, FGradBottomLeft: TBGRAPixel;
    FUseGradientColor: boolean;
    FBlendMode: TOpenGLBlendMode;

    class function GetOpenGLMaxTexSize: integer; virtual; abstract;
    class function GetNonPowerOfTwoSizeSupport: boolean; virtual;
    function CreateOpenGLTexture(ARGBAData: PLongWord; AAllocatedWidth, AAllocatedHeight, AActualWidth, AActualHeight: integer; RGBAOrder: boolean): TBGLTextureHandle; virtual; abstract;
    procedure UpdateOpenGLTexture(ATexture: TBGLTextureHandle; ARGBAData: PLongWord; AAllocatedWidth, AAllocatedHeight, AActualWidth,AActualHeight: integer; RGBAOrder: boolean); virtual; abstract;
    class function SupportsBGRAOrder: boolean; virtual;
    procedure SetOpenGLTextureSize(ATexture: TBGLTextureHandle; AAllocatedWidth, AAllocatedHeight, AActualWidth, AActualHeight: integer); virtual; abstract;
    function GetOpenGLAllocatedSize(ATexture: TBGLTextureHandle): TSize; virtual; abstract;
    procedure ComputeOpenGLFramesCoord(ATexture: TBGLTextureHandle; FramesX: Integer=1; FramesY: Integer=1); virtual; abstract;
    function GetOpenGLFrameCount(ATexture: TBGLTextureHandle): integer; virtual; abstract;
    function GetEmptyTexture: TBGLTextureHandle; virtual; abstract;
    procedure FreeOpenGLTexture(ATexture: TBGLTextureHandle); virtual; abstract;
    function GetRepeatX: TOpenGLRepeatMode; virtual; abstract;
    function GetRepeatY: TOpenGLRepeatMode; virtual; abstract;
    procedure UpdateGLResampleFilter(ATexture: TBGLTextureHandle; AFilter: TOpenGLResampleFilter); virtual; abstract;
    function GetUseGradientColors: boolean; virtual;
    procedure SetUseGradientColors(AValue: boolean); virtual;

    procedure DoDrawTriangleOrQuad(const {%H-}Points: array of TPointF;
      const {%H-}APointsZ: array of Single; const {%H-}APoints3D: array of TPoint3D_128;
      const {%H-}ANormals3D: array of TPoint3D_128; const {%H-}TexCoords: array of TPointF;
      const {%H-}AColors: array of TColorF); virtual;
    procedure DoStretchDraw(x,y,w,h: single; AColor: TBGRAPixel); virtual; abstract;
    procedure DoStretchDrawAngle(x,y,w,h,angleDeg: single; rotationCenter: TPointF; AColor: TBGRAPixel); virtual; abstract;
    procedure DoDrawAffine(Origin, HAxis, VAxis: TPointF; AColor: TBGRAPixel); virtual; abstract;
    function NewEmpty: TBGLCustomTexture; virtual; abstract;
    function NewFromTexture(ATexture: TBGLTextureHandle; AWidth,AHeight: integer): TBGLCustomTexture; virtual; abstract;
    procedure NotifyInvalidFrameSize; virtual;
    procedure NotifyErrorLoadingFile({%H-}AFilename: string); virtual;

    procedure Init(ATexture: TBGLTextureHandle; AWidth,AHeight: integer; AOwned: boolean); virtual;
    function Duplicate: TBGLCustomTexture; virtual;
    procedure FreeMemoryOnDestroy; virtual;

    procedure InitEmpty;
    procedure InitFromData(ARGBAData: PLongWord; AllocatedWidth,AllocatedHeight, ActualWidth,ActualHeight: integer; RGBAOrder: boolean);
    procedure InitFromStream(AStream: TStream);
  public
    destructor Destroy; override;
    constructor Create; overload;
    constructor Create(ATexture: TBGLTextureHandle; AWidth,AHeight: integer); overload;
    constructor Create(ARGBAData: PLongWord; AllocatedWidth,AllocatedHeight, ActualWidth,ActualHeight: integer; RGBAOrder: boolean = true); overload;
    constructor Create(AFPImage: TFPCustomImage); overload;
    constructor Create(ABitmap: TBitmap); overload;
    constructor Create(AWidth, AHeight: integer; Color: TColor); overload;
    constructor Create(AWidth, AHeight: integer; Color: TBGRAPixel); overload;
    constructor Create(AFilenameUTF8: string); overload;
    constructor Create(AFilenameUTF8: string; AWidth,AHeight: integer; AResampleFilter: TResampleFilter); overload;
    constructor Create(AStream: TStream); overload;
    procedure ToggleFlipX; virtual; abstract;
    procedure ToggleFlipY; virtual; abstract;
    procedure ToggleMask; virtual;
    function FilterBlurMotion({%H-}ARadius: single; {%H-}ABlurType: TRadialBlurType; {%H-}ADirection: TPointF): IBGLTexture; virtual;
    function FilterBlurRadial({%H-}ARadius: single; {%H-}ABlurType: TRadialBlurType): IBGLTexture; virtual;

    procedure SetFrameSize(x,y: integer);
    procedure SetRepetition(AValueX, AValueY: TOpenGLRepeatMode); virtual; abstract;
    procedure Update(ARGBAData: PLongWord; AllocatedWidth, AllocatedHeight, ActualWidth,ActualHeight: integer; RGBAOrder: boolean = true);
    procedure SetFrame(AIndex: integer);
    procedure SetGradientColors(ATopLeft, ATopRight, ABottomRight, ABottomLeft: TBGRAPixel);
    procedure FreeMemory;
    procedure Bind({%H-}ATextureNumber: integer); virtual;

    procedure Draw(x,y: single; AAlpha: byte = 255); overload;
    procedure Draw(x,y: single; AColor: TBGRAPixel); overload;
    procedure Draw(x,y: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure Draw(x,y: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure StretchDraw(x,y,w,h: single; AAlpha: byte = 255); overload;
    procedure StretchDraw(x,y,w,h: single; AColor: TBGRAPixel); overload;
    procedure StretchDraw(x,y,w,h: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure StretchDraw(x,y,w,h: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure DrawAffine(const Origin, HAxis, VAxis: TPointF; AAlpha: byte = 255); overload;
    procedure DrawAffine(const Origin, HAxis, VAxis: TPointF; AColor: TBGRAPixel); overload;
    procedure DrawAffine(x,y: single; const AMatrix: TAffineMatrix; AAlpha: byte = 255); overload;
    procedure DrawAffine(x,y: single; const AMatrix: TAffineMatrix; AColor: TBGRAPixel); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;

    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property AllocatedWidth: integer read GetAllocatedWidth;
    property AllocatedHeight: integer read GetAllocatedHeight;
    property FrameCount: integer read GetFrameCount;
    property Frame[AIndex: integer]: IBGLTexture read GetFrame;
    property FrameWidth: integer read GetFrameWidth;
    property FrameHeight: integer read GetFrameHeight;
    property FlipX: IBGLTexture read GetFlipX;
    property FlipY: IBGLTexture read GetFlipY;
    property Mask: IBGLTexture read GetMask;
    property Handle: TBGLTextureHandle read GetOpenGLTexture;
    property RepeatX: TOpenGLRepeatMode read GetRepeatX;
    property RepeatY: TOpenGLRepeatMode read GetRepeatY;
    property ResampleFilter: TOpenGLResampleFilter read GetResampleFilter write SetResampleFilter;
    property BlendMode: TOpenGLBlendMode read GetOpenGLBlendMode write SetOpenGLBlendMode;
    property GradientColors: boolean read GetUseGradientColors write SetUseGradientColors;
  end;

  { Abstract class for a frame buffer in OpenGL }
  TBGLCustomFrameBuffer = class
  protected
    FCanvas: pointer;
    function GetTexture: IBGLTexture; virtual; abstract;
    function GetHandle: pointer; virtual; abstract;
    function GetMatrix: TAffineMatrix; virtual; abstract;
    function GetHeight: integer; virtual; abstract;
    function GetProjectionMatrix: TMatrix4D; virtual; abstract;
    function GetWidth: integer; virtual; abstract;
    procedure SetMatrix(AValue: TAffineMatrix); virtual; abstract;
    procedure SetProjectionMatrix(AValue: TMatrix4D); virtual; abstract;

  public
    procedure UseOrthoProjection; overload; virtual;
    procedure UseOrthoProjection(AMinX,AMinY,AMaxX,AMaxY: single); overload; virtual;
    function MakeTextureAndFree: IBGLTexture; virtual;

    procedure SetCanvas(ACanvas: Pointer); //for internal use
    property Matrix: TAffineMatrix read GetMatrix write SetMatrix;
    property ProjectionMatrix: TMatrix4D read GetProjectionMatrix write SetProjectionMatrix;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property Handle: pointer read GetHandle;
    property Texture: IBGLTexture read GetTexture;
  end;

type
  TBGLBitmapAny = class of TBGLCustomBitmap;
  TBGLTextureAny = class of TBGLCustomTexture;

var
  BGLBitmapFactory : TBGLBitmapAny;
  BGLTextureFactory: TBGLTextureAny;

function OrthoProjectionToOpenGL(AMinX,AMinY,AMaxX,AMaxY: Single): TMatrix4D;
function GetPowerOfTwo( Value : Integer ) : Integer;

implementation

uses BGRAFilterScanner;

procedure TBGLCustomFrameBuffer.UseOrthoProjection;
begin
  ProjectionMatrix := OrthoProjectionToOpenGL(0,0,Width,Height);
end;

procedure TBGLCustomFrameBuffer.UseOrthoProjection(AMinX, AMinY, AMaxX, AMaxY: single);
begin
  ProjectionMatrix := OrthoProjectionToOpenGL(AMinX,AMinY,AMaxX,AMaxY);
end;

function TBGLCustomFrameBuffer.MakeTextureAndFree: IBGLTexture;
begin
  result := nil;
  raise exception.create('Not implemented');
end;

procedure TBGLCustomFrameBuffer.SetCanvas(ACanvas: Pointer);
begin
  FCanvas := ACanvas;
end;

function OrthoProjectionToOpenGL(AMinX, AMinY, AMaxX, AMaxY: Single): TMatrix4D;
var sx,sy: single;
begin
  sx := 2/(AMaxX-AMinX);
  sy := 2/(AMaxY-AMinY);
  result[1,1] := sx;   result[2,1] := 0;     result[3,1] := 0;   result[4,1] := -1 - AMinX*sx;
  result[1,2] := 0;    result[2,2] := -sy;   result[3,2] := 0;   result[4,2] := 1 + AMinY*sy;
  result[1,3] := 0;    result[2,3] := 0;     result[3,3] := -1;  result[4,3] := 0;
  result[1,4] := 0;    result[2,4] := 0;     result[3,4] := 0;   result[4,4] := 1;
end;

function GetPowerOfTwo( Value : Integer ) : Integer;
begin
  Result := Value - 1;
  Result := Result or ( Result shr 1 );
  Result := Result or ( Result shr 2 );
  Result := Result or ( Result shr 4 );
  Result := Result or ( Result shr 8 );
  Result := Result or ( Result shr 16 );
  Result := Result + 1;
end;

{ TBGLCustomTexture }

function TBGLCustomTexture.GetAllocatedHeight: integer;
begin
  result := GetOpenGLAllocatedSize(FOpenGLTexture).Height;
end;

function TBGLCustomTexture.GetAllocatedWidth: integer;
begin
  result := GetOpenGLAllocatedSize(FOpenGLTexture).Width;
end;

function TBGLCustomTexture.GetFlipX: IBGLTexture;
begin
  result := Duplicate;
  result.ToggleFlipX;
end;

function TBGLCustomTexture.GetFlipY: IBGLTexture;
begin
  result := Duplicate;
  result.ToggleFlipY;
end;

function TBGLCustomTexture.GetFrame(AIndex: integer): IBGLTexture;
var fc: integer;
begin
  fc := GetFrameCount;
  if fc <= 1 then
    result := self
  else
    begin
      if (AIndex < 1) or (AIndex > fc) then
        result := NewEmpty
      else
      begin
        result := Duplicate;
        result.SetFrame(AIndex);
      end;
    end;
end;

function TBGLCustomTexture.GetFrameCount: integer;
begin
  result := GetOpenGLFrameCount(FOpenGLTexture);
end;

function TBGLCustomTexture.GetFrameHeight: integer;
begin
  result := FFrameHeight;
end;

function TBGLCustomTexture.GetFrameWidth: integer;
begin
  result := FFrameWidth;
end;

function TBGLCustomTexture.GetHeight: integer;
begin
  result := FHeight;
end;

function TBGLCustomTexture.GetMask: IBGLTexture;
begin
  result := Duplicate;
  result.ToggleMask;
end;

function TBGLCustomTexture.GetOpenGLBlendMode: TOpenGLBlendMode;
begin
  result := FBlendMode;
end;

function TBGLCustomTexture.GetOpenGLTexture: TBGLTextureHandle;
begin
  result := FOpenGLTexture;
end;

function TBGLCustomTexture.GetUseGradientColors: boolean;
begin
  result := FUseGradientColor;
end;

function TBGLCustomTexture.GetWidth: integer;
begin
  result := FWidth;
end;

function TBGLCustomTexture.GetImageCenter: TPointF;
begin
  result := FImageCenter;
end;

procedure TBGLCustomTexture.SetImageCenter(const AValue: TPointF);
begin
  FImageCenter := AValue;
end;

function TBGLCustomTexture.GetResampleFilter: TOpenGLResampleFilter;
begin
  result := FResampleFilter;
end;

procedure TBGLCustomTexture.SetOpenGLBlendMode(AValue: TOpenGLBlendMode);
begin
  FBlendMode := AValue;
end;

procedure TBGLCustomTexture.SetResampleFilter(AValue: TOpenGLResampleFilter);
begin
  if AValue <> FResampleFilter then
  begin
    FResampleFilter:= AValue;
    UpdateGLResampleFilter(FOpenGLTexture, AValue);
  end;
end;

class function TBGLCustomTexture.GetNonPowerOfTwoSizeSupport: boolean;
begin
  result := false;
end;

class function TBGLCustomTexture.SupportsBGRAOrder: boolean;
begin
  result := false;
end;

procedure TBGLCustomTexture.SetUseGradientColors(AValue: boolean);
begin
  FUseGradientColor := AValue;
end;

procedure TBGLCustomTexture.DoDrawTriangleOrQuad(
  const Points: array of TPointF; const APointsZ: array of Single;
  const APoints3D: array of TPoint3D_128;
  const ANormals3D: array of TPoint3D_128; const TexCoords: array of TPointF;
  const AColors: array of TColorF);
begin
  raise Exception.Create('Not implemented');
end;

procedure TBGLCustomTexture.ToggleMask;
begin
  FIsMask := not FIsMask;
end;

function TBGLCustomTexture.FilterBlurMotion(ARadius: single; ABlurType: TRadialBlurType;
  ADirection: TPointF): IBGLTexture;
begin
  result := nil;
  raise exception.Create('Not implemented');
end;

function TBGLCustomTexture.FilterBlurRadial(ARadius: single; ABlurType: TRadialBlurType): IBGLTexture;
begin
  result := nil;
  raise exception.Create('Not implemented');
end;

procedure TBGLCustomTexture.Update(ARGBAData: PLongWord; AllocatedWidth,
  AllocatedHeight, ActualWidth, ActualHeight: integer; RGBAOrder: boolean);
begin
  UpdateOpenGLTexture(FOpenGLTexture, ARGBAData, AllocatedWidth, AllocatedHeight, ActualWidth,ActualHeight,RGBAOrder);
  ComputeOpenGLFramesCoord(FOpenGLTexture, round(FWidth/FFrameWidth),round(FWidth/FFrameHeight));
  FWidth := ActualWidth;
  FHeight := ActualHeight;
  FImageCenter := PointF(FWidth*0.5,FHeight*0.5);
end;

procedure TBGLCustomTexture.SetFrame(AIndex: integer);
begin
  if (AIndex >= 1) and (AIndex <= GetFrameCount) then
    begin
      FFrame := AIndex;
      FWidth := FFrameWidth;
      FHeight:= FFrameHeight;
      FImageCenter := PointF(FWidth*0.5,FHeight*0.5);
    end;
end;

procedure TBGLCustomTexture.SetGradientColors(ATopLeft, ATopRight,
  ABottomRight, ABottomLeft: TBGRAPixel);
begin
  FGradTopLeft := ATopLeft;
  FGradTopRight := ATopRight;
  FGradBottomLeft := ABottomLeft;
  FGradBottomRight := ABottomRight;
  GradientColors := true;
end;

procedure TBGLCustomTexture.FreeMemory;
begin
  if FOpenGLTextureOwned then
  begin
    FreeOpenGLTexture(FOpenGLTexture);
    FOpenGLTexture := GetEmptyTexture;
    FOpenGLTextureOwned := false;
  end;
end;

procedure TBGLCustomTexture.Bind(ATextureNumber: integer);
begin
  raise Exception.Create('Not implemented');
end;

procedure TBGLCustomTexture.NotifyInvalidFrameSize;
begin
  //
end;

procedure TBGLCustomTexture.NotifyErrorLoadingFile(AFilename: string);
begin
  //
end;

procedure TBGLCustomTexture.Init(ATexture: TBGLTextureHandle; AWidth,
  AHeight: integer; AOwned: boolean);
begin
  FOpenGLTexture:= ATexture;
  FWidth := AWidth;
  FHeight := AHeight;
  FImageCenter := PointF(FWidth*0.5,FHeight*0.5);
  FFrame:= 0;
  FFrameWidth := AWidth;
  FFrameHeight := AHeight;
  FIsMask:= false;
  FOpenGLTextureOwned := AOwned;
end;

function TBGLCustomTexture.Duplicate: TBGLCustomTexture;
begin
  result := NewFromTexture(FOpenGLTexture, FWidth, FHeight);
  result.FFrame := FFrame;
  result.FFrameWidth := FFrameWidth;
  result.FFrameHeight := FFrameHeight;
  result.FIsMask := FIsMask;
  result.FResampleFilter := FResampleFilter;
  result.FGradTopLeft := FGradTopLeft;
  result.FGradTopRight := FGradTopRight;
  result.FGradBottomRight := FGradBottomRight;
  result.FGradBottomLeft := FGradBottomLeft;
  result.FUseGradientColor := FUseGradientColor;
  result.FBlendMode := FBlendMode;
end;

procedure TBGLCustomTexture.FreeMemoryOnDestroy;
begin
  FreeMemory;
end;

procedure TBGLCustomTexture.InitEmpty;
begin
  Init(GetEmptyTexture,0,0,False);
end;

procedure TBGLCustomTexture.InitFromData(ARGBAData: PLongWord;
  AllocatedWidth, AllocatedHeight, ActualWidth, ActualHeight: integer;
  RGBAOrder: boolean);
var tex: TBGLTextureHandle;
    MaxTexSize: integer;
begin
  MaxTexSize := GetOpenGLMaxTexSize;
  if ( AllocatedWidth > MaxTexSize ) or ( AllocatedHeight > MaxTexSize ) or
    (AllocatedWidth <= 0) or (AllocatedHeight <= 0) then
    InitEmpty
  else
  begin
    tex := CreateOpenGLTexture(ARGBAData,AllocatedWidth,AllocatedHeight,ActualWidth,ActualHeight,RGBAOrder);
    FResampleFilter := orfLinear;
    ComputeOpenGLFramesCoord(tex);
    Init(tex,ActualWidth,ActualHeight,True);
  end;
end;

procedure TBGLCustomTexture.InitFromStream(AStream: TStream);
var bmp: TBGLCustomBitmap;
begin
  bmp := nil;
  try
    bmp := BGLBitmapFactory.Create(AStream);
    if not TBGRAPixel_RGBAOrder and not SupportsBGRAOrder then bmp.SwapRedBlue;
    InitFromData(PLongWord(bmp.Data), bmp.AllocatedWidth,bmp.AllocatedHeight, bmp.Width,bmp.Height,TBGRAPixel_RGBAOrder or not SupportsBGRAOrder);
  except
    InitEmpty;
  end;
  bmp.Free;
end;

destructor TBGLCustomTexture.Destroy;
begin
  FreeMemoryOnDestroy;
  inherited Destroy;
end;

constructor TBGLCustomTexture.Create;
begin
  InitEmpty;
end;

constructor TBGLCustomTexture.Create(ATexture: TBGLTextureHandle; AWidth,
  AHeight: integer);
begin
  Init(ATexture, AWidth,AHeight, False);
end;

constructor TBGLCustomTexture.Create(ARGBAData: PLongWord; AllocatedWidth,
  AllocatedHeight, ActualWidth, ActualHeight: integer; RGBAOrder: boolean);
begin
  InitFromData(ARGBAData,AllocatedWidth,AllocatedHeight,ActualWidth,ActualHeight,RGBAOrder);
end;

constructor TBGLCustomTexture.Create(AFPImage: TFPCustomImage);
var bmp: TBGLCustomBitmap;
begin
  if (AFPImage is TBGRACustomBitmap) and
    (
      (Assigned(BGLTextureFactory) and BGLTextureFactory.GetNonPowerOfTwoSizeSupport) or

      ((AFPImage.Width = GetPowerOfTwo(AFPImage.Width)) and
      (AFPImage.Height = GetPowerOfTwo(AFPImage.Height)))
    ) then
  begin
    with TBGRACustomBitmap(AFPImage) do
    begin
      if not TBGRAPixel_RGBAOrder and not SupportsBGRAOrder then SwapRedBlue;
      if LineOrder = riloBottomToTop then VerticalFlip;
      InitFromData(PLongWord(Data), Width,Height, Width,Height, TBGRAPixel_RGBAOrder or not SupportsBGRAOrder);
      if LineOrder = riloBottomToTop then VerticalFlip;
      if not TBGRAPixel_RGBAOrder and not SupportsBGRAOrder then SwapRedBlue;
    end;
  end else
  begin
    bmp := BGLBitmapFactory.Create(AFPImage);
    if not TBGRAPixel_RGBAOrder and not SupportsBGRAOrder then bmp.SwapRedBlue;
    InitFromData(PLongWord(bmp.Data), bmp.AllocatedWidth,bmp.AllocatedHeight, bmp.Width,bmp.Height, TBGRAPixel_RGBAOrder or not SupportsBGRAOrder);
    bmp.Free;
  end;
end;

constructor TBGLCustomTexture.Create(ABitmap: TBitmap);
var bmp: TBGLCustomBitmap;
begin
  bmp := BGLBitmapFactory.Create(ABitmap);
  if not TBGRAPixel_RGBAOrder and not SupportsBGRAOrder then bmp.SwapRedBlue;
  InitFromData(PLongWord(bmp.Data), bmp.AllocatedWidth,bmp.AllocatedHeight, bmp.Width,bmp.Height, TBGRAPixel_RGBAOrder or not SupportsBGRAOrder);
  bmp.Free;
end;

constructor TBGLCustomTexture.Create(AWidth, AHeight: integer; Color: TColor);
var bmp: TBGLCustomBitmap;
begin
  bmp := BGLBitmapFactory.Create(AWidth,AHeight,Color);
  if not TBGRAPixel_RGBAOrder and not SupportsBGRAOrder then bmp.SwapRedBlue;
  InitFromData(PLongWord(bmp.Data), bmp.AllocatedWidth,bmp.AllocatedHeight, bmp.Width,bmp.Height, TBGRAPixel_RGBAOrder or not SupportsBGRAOrder);
  bmp.Free;
end;

constructor TBGLCustomTexture.Create(AWidth, AHeight: integer;
  Color: TBGRAPixel);
var bmp: TBGLCustomBitmap;
begin
  bmp := BGLBitmapFactory.Create(AWidth,AHeight,Color);
  if not TBGRAPixel_RGBAOrder and not SupportsBGRAOrder then bmp.SwapRedBlue;
  InitFromData(PLongWord(bmp.Data), bmp.AllocatedWidth,bmp.AllocatedHeight, bmp.Width,bmp.Height, TBGRAPixel_RGBAOrder or not SupportsBGRAOrder);
  bmp.Free;
end;

constructor TBGLCustomTexture.Create(AFilenameUTF8: string);
var bmp: TBGLCustomBitmap;
begin
  bmp := nil;
  try
    bmp := BGLBitmapFactory.Create(AFilenameUTF8, True);
    if not TBGRAPixel_RGBAOrder and not SupportsBGRAOrder then bmp.SwapRedBlue;
    InitFromData(PLongWord(bmp.Data), bmp.AllocatedWidth,bmp.AllocatedHeight, bmp.Width,bmp.Height, TBGRAPixel_RGBAOrder or not SupportsBGRAOrder);
  except
    InitEmpty;
    NotifyErrorLoadingFile(AFilenameUTF8);
  end;
  bmp.Free;
end;

constructor TBGLCustomTexture.Create(AFilenameUTF8: string; AWidth,
  AHeight: integer; AResampleFilter: TResampleFilter);
var bmp, temp: TBGLCustomBitmap;
begin
  bmp := nil;
  try
    bmp := BGLBitmapFactory.Create(AFilenameUTF8, True);
    if (bmp.Width <> AWidth) or (bmp.Height <> AHeight) then
    begin
      if AResampleFilter = rfBox then
        temp := bmp.Resample(AWidth,AHeight,rmSimpleStretch) as TBGLCustomBitmap
      else
      begin
        bmp.ResampleFilter := AResampleFilter;
        temp := bmp.Resample(AWidth,AHeight) as TBGLCustomBitmap;
      end;
      bmp.Free;
      bmp := temp;
      temp := nil;
    end;
    if not TBGRAPixel_RGBAOrder and not SupportsBGRAOrder then bmp.SwapRedBlue;
    InitFromData(PLongWord(bmp.Data), bmp.AllocatedWidth,bmp.AllocatedHeight, bmp.Width,bmp.Height, TBGRAPixel_RGBAOrder);
  except
    InitEmpty;
    NotifyErrorLoadingFile(AFilenameUTF8);
  end;
  bmp.Free;
end;

constructor TBGLCustomTexture.Create(AStream: TStream);
begin
  InitFromStream(AStream);
end;

procedure TBGLCustomTexture.SetFrameSize(x, y: integer);
begin
  if (FWidth = 0) or (FHeight = 0) then exit;
  if (x <= 0) or (y <= 0) or (x > FWidth) or (y > FHeight) then
  begin
    NotifyInvalidFrameSize;
    exit;
  end;
  ComputeOpenGLFramesCoord(FOpenGLTexture, FWidth div x,FHeight div y);
  FFrameWidth:= x;
  FFrameHeight:= y;
end;

procedure TBGLCustomTexture.Draw(x, y: single; AAlpha: byte);
begin
  DoStretchDraw(x,y,FWidth,FHeight,BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.Draw(x, y: single; AColor: TBGRAPixel);
begin
  DoStretchDraw(x,y,FWidth,FHeight,AColor);
end;

procedure TBGLCustomTexture.Draw(x, y: single; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; AAlpha: byte);
begin
  Draw(x,y, AHorizAlign, AVertAlign, BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.Draw(x, y: single; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; AColor: TBGRAPixel);
begin
  StretchDraw(x,y, FWidth,FHeight, AHorizAlign,AVertAlign, AColor);
end;

procedure TBGLCustomTexture.StretchDraw(x, y, w, h: single; AAlpha: byte);
begin
  DoStretchDraw(x,y,w,h, BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.StretchDraw(x, y, w, h: single;
  AColor: TBGRAPixel);
begin
  DoStretchDraw(x,y,w,h,AColor);
end;

procedure TBGLCustomTexture.StretchDraw(x, y, w, h: single;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AAlpha: byte);
begin
  StretchDraw(x,y,w,h, AHorizAlign,AVertAlign, BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.StretchDraw(x, y, w, h: single;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel);
begin
  case AHorizAlign of
  taCenter: DecF(x, w*0.5);
  taRightJustify: DecF(x, w-1);
  end;
  case AVertAlign of
  tlCenter: DecF(y, h*0.5);
  tlBottom: DecF(y, h);
  end;
  DoStretchDraw(x,y,w,h,AColor);
end;

procedure TBGLCustomTexture.DrawAngle(x, y, angleDeg: single;
  const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte);
begin
  StretchDrawAngle(x,y,FWidth,FHeight,angleDeg,imageCenter,ARestoreOffsetAfterRotation,BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.DrawAngle(x, y, angleDeg: single;
  const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel);
begin
  StretchDrawAngle(x,y,FWidth,FHeight,angleDeg,imageCenter,ARestoreOffsetAfterRotation,AColor);
end;

procedure TBGLCustomTexture.DrawAngle(x, y, angleDeg: single; AAlpha: byte);
begin
  StretchDrawAngle(x,y, FWidth,FHeight, angleDeg, AAlpha);
end;

procedure TBGLCustomTexture.DrawAngle(x, y, angleDeg: single; AColor: TBGRAPixel);
begin
  StretchDrawAngle(x,y, FWidth,FHeight, angleDeg, AColor);
end;

procedure TBGLCustomTexture.DrawAngle(x, y, angleDeg: single;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AAlpha: byte);
begin
  StretchDrawAngle(x,y,FWidth,FHeight,angleDeg, AHorizAlign, AVertAlign, AAlpha);
end;

procedure TBGLCustomTexture.DrawAngle(x, y, angleDeg: single;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel);
begin
  StretchDrawAngle(x,y,FWidth,FHeight, angleDeg, AHorizAlign, AVertAlign, AColor);
end;

procedure TBGLCustomTexture.StretchDrawAngle(x, y,w,h, angleDeg: single;
  const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte);
begin
  StretchDrawAngle(x,y,w,h,angleDeg,imageCenter,ARestoreOffsetAfterRotation,BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.StretchDrawAngle(x, y,w,h, angleDeg: single;
  const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel);
var
  rotationCenter: TPointF;
begin
  if (FWidth=0) or (FHeight = 0) then exit;
  rotationCenter := PointF(imageCenter.x*w/FWidth, imageCenter.y*h/FHeight);
  if not ARestoreOffsetAfterRotation then
  begin
    DecF(x, rotationCenter.x);
    DecF(y, rotationCenter.y);
  end;
  DoStretchDrawAngle(x,y,w,h,angleDeg,rotationCenter+PointF(x,y),AColor);
end;

procedure TBGLCustomTexture.StretchDrawAngle(x, y,w,h, angleDeg: single; AAlpha: byte);
begin
  StretchDrawAngle(x, y, w,h, angleDeg, FImageCenter, True, BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.StretchDrawAngle(x, y,w,h, angleDeg: single;
  AColor: TBGRAPixel);
begin
  StretchDrawAngle(x, y, w,h, angleDeg, FImageCenter, True, AColor);
end;

procedure TBGLCustomTexture.StretchDrawAngle(x, y,w,h, angleDeg: single;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AAlpha: byte);
begin
  StretchDrawAngle(x,y,w,h,angleDeg, AHorizAlign, AVertAlign, BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.StretchDrawAngle(x, y,w,h, angleDeg: single;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel);
var imageCenter: TPointF;
begin
  case AHorizAlign of
  taCenter: imageCenter.x := FWidth*0.5;
  taRightJustify: imageCenter.x := FWidth;
  else imageCenter.x := 0;
  end;
  case AVertAlign of
  tlCenter: imageCenter.y := FHeight*0.5;
  tlBottom: imageCenter.y := FHeight;
  else imageCenter.y := 0;
  end;
  StretchDrawAngle(x,y,w,h, angleDeg, imageCenter, False, AColor);
end;

procedure TBGLCustomTexture.DrawAffine(const Origin, HAxis, VAxis: TPointF;
  AAlpha: byte);
begin
  {$PUSH}{$OPTIMIZATION OFF}
  DoDrawAffine(Origin,HAxis,VAxis, BGRA(255,255,255,AAlpha));
  {$POP}
end;

procedure TBGLCustomTexture.DrawAffine(const Origin, HAxis, VAxis: TPointF;
  AColor: TBGRAPixel);
begin
  {$PUSH}{$OPTIMIZATION OFF}
  DoDrawAffine(Origin,HAxis,VAxis, AColor);
  {$POP}
end;

procedure TBGLCustomTexture.DrawAffine(x, y: single;
  const AMatrix: TAffineMatrix; AAlpha: byte);
begin
  DoDrawAffine(AMatrix*PointF(0,0) + PointF(x,y), AMatrix*PointF(Width,0) + PointF(x,y),
     AMatrix*PointF(0,Height) + PointF(x,y), BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.DrawAffine(x, y: single;
  const AMatrix: TAffineMatrix; AColor: TBGRAPixel);
begin
  DoDrawAffine(AMatrix*PointF(0,0) + PointF(x,y), AMatrix*PointF(Width,0) + PointF(x,y),
     AMatrix*PointF(0,Height) + PointF(x,y), AColor);
end;

procedure TBGLCustomTexture.DrawTriangle(const APoints: array of TPointF;
  const ATexCoords: array of TPointF);
begin
  if (length(APoints) = 3) and (length(ATexCoords) = 3) then
    DoDrawTriangleOrQuad(APoints,[],[],[],ATexCoords,[]);
end;

procedure TBGLCustomTexture.DrawTriangle(const APoints: array of TPointF;
  const ATexCoords: array of TPointF; const AColors: array of TColorF);
begin
  if (length(APoints) = 3) and (length(ATexCoords) = 3)
     and (length(AColors) = 3) then
    DoDrawTriangleOrQuad(APoints,[],[],[],ATexCoords,AColors);
end;

procedure TBGLCustomTexture.DrawTriangle(const APoints: array of TPointF;
  const APointsZ: array of Single; const ATexCoords: array of TPointF);
begin
  if (length(APoints) = 3) and (length(ATexCoords) = 3)
     and (length(APointsZ) = 3) then
  DoDrawTriangleOrQuad(APoints,APointsZ,[],[],ATexCoords,[]);
end;

procedure TBGLCustomTexture.DrawTriangle(const APoints: array of TPointF;
  const APointsZ: array of Single; const ATexCoords: array of TPointF;
  const AColors: array of TColorF);
begin
  if (length(APoints) = 3) and (length(ATexCoords) = 3)
     and (length(APointsZ) = 3) and (length(AColors) = 3) then
  DoDrawTriangleOrQuad(APoints,APointsZ,[],[],ATexCoords,AColors);
end;

procedure TBGLCustomTexture.DrawTriangle(
  const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF);
begin
  if (length(APoints3D) = 3) and (length(ATexCoords) = 3) then
  DoDrawTriangleOrQuad([],[],APoints3D,[],ATexCoords,[]);
end;

procedure TBGLCustomTexture.DrawTriangle(
  const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF;
  const AColors: array of TColorF);
begin
  if (length(APoints3D) = 3) and (length(ATexCoords) = 3)
  and (length(AColors) = 3) then
  DoDrawTriangleOrQuad([],[],APoints3D,[],ATexCoords,AColors);
end;

procedure TBGLCustomTexture.DrawTriangle(const APoints3D: array of TPoint3D_128;
  const ANormals3D: array of TPoint3D_128;
  const ATexCoords: array of TPointF);
begin
  if (length(APoints3D) = 3) and (length(ATexCoords) = 3)
  and (length(ANormals3D) = 3) then
  DoDrawTriangleOrQuad([],[],APoints3D,ANormals3D,ATexCoords,[]);
end;

procedure TBGLCustomTexture.DrawTriangle(const APoints3D: array of TPoint3D_128;
  const ANormals3D: array of TPoint3D_128;
  const ATexCoords: array of TPointF; const AColors: array of TColorF);
begin
  if (length(APoints3D) = 3) and (length(ATexCoords) = 3)
  and (length(ANormals3D) = 3)
  and (length(AColors) = 3) then
  DoDrawTriangleOrQuad([],[],APoints3D,ANormals3D,ATexCoords,AColors);
end;

procedure TBGLCustomTexture.DrawQuad(const APoints: array of TPointF;
  const ATexCoords: array of TPointF);
begin
  if (length(APoints) = 4) and (length(ATexCoords) = 4) then
    DoDrawTriangleOrQuad(APoints,[],[],[],ATexCoords,[]);
end;

procedure TBGLCustomTexture.DrawQuad(const APoints: array of TPointF;
  const ATexCoords: array of TPointF; const AColors: array of TColorF);
begin
  if (length(APoints) = 4) and (length(ATexCoords) = 4)
    and (length(AColors) = 4) then
    DoDrawTriangleOrQuad(APoints,[],[],[],ATexCoords, AColors);
end;

procedure TBGLCustomTexture.DrawQuad(const APoints: array of TPointF;
  const APointsZ: array of Single; const ATexCoords: array of TPointF);
begin
  if (length(APoints) = 4) and (length(ATexCoords) = 4)
     and (length(APointsZ) = 4) then
    DoDrawTriangleOrQuad(APoints,APointsZ,[],[],ATexCoords,[]);
end;

procedure TBGLCustomTexture.DrawQuad(const APoints: array of TPointF;
  const APointsZ: array of Single; const ATexCoords: array of TPointF;
  const AColors: array of TColorF);
begin
  if (length(APoints) = 4) and (length(ATexCoords) = 4)
     and (length(APointsZ) = 4) and (length(AColors) = 4) then
    DoDrawTriangleOrQuad(APoints,APointsZ,[],[],ATexCoords,AColors);
end;

procedure TBGLCustomTexture.DrawQuad(const APoints3D: array of TPoint3D_128;
  const ATexCoords: array of TPointF);
begin
  if (length(APoints3D) = 4) and (length(ATexCoords) = 4) then
    DoDrawTriangleOrQuad([],[],APoints3D,[],ATexCoords,[]);
end;

procedure TBGLCustomTexture.DrawQuad(const APoints3D: array of TPoint3D_128;
  const ATexCoords: array of TPointF; const AColors: array of TColorF);
begin
  if (length(APoints3D) = 4) and (length(ATexCoords) = 4)
     and (length(AColors) = 4) then
    DoDrawTriangleOrQuad([],[],APoints3D,[],ATexCoords,AColors);
end;

procedure TBGLCustomTexture.DrawQuad(const APoints3D: array of TPoint3D_128;
  const ANormals3D: array of TPoint3D_128;
  const ATexCoords: array of TPointF);
begin
  if (length(APoints3D) = 4) and (length(ATexCoords) = 4)
     and (length(ANormals3D) = 4) then
    DoDrawTriangleOrQuad([],[],APoints3D,ANormals3D,ATexCoords,[]);
end;

procedure TBGLCustomTexture.DrawQuad(const APoints3D: array of TPoint3D_128;
  const ANormals3D: array of TPoint3D_128;
  const ATexCoords: array of TPointF; const AColors: array of TColorF);
begin
  if (length(APoints3D) = 4) and (length(ATexCoords) = 4)
     and (length(ANormals3D) = 4)
     and (length(AColors) = 4) then
    DoDrawTriangleOrQuad([],[],APoints3D,ANormals3D,ATexCoords,AColors);
end;

{ TBGLCustomFont }

function TBGLCustomFont.GetScale: single;
begin
  result := FScale;
end;

function TBGLCustomFont.GetStepX: single;
begin
  result := FStepX;
end;

procedure TBGLCustomFont.SetScale(AValue: single);
begin
  FScale:= AValue;
end;

procedure TBGLCustomFont.SetStepX(AValue: single);
begin
  FStepX:= AValue;
end;

function TBGLCustomFont.GetHorizontalAlign: TAlignment;
begin
  result := FHorizontalAlign;
end;

function TBGLCustomFont.GetJustify: boolean;
begin
  result := FJustify;
end;

function TBGLCustomFont.GetVerticalAlign: TTextLayout;
begin
  result := FVerticalAlign;
end;

procedure TBGLCustomFont.SetHorizontalAlign(AValue: TAlignment);
begin
  FHorizontalAlign:= AValue;
end;

procedure TBGLCustomFont.SetJustify(AValue: boolean);
begin
  FJustify:= AValue;
end;

procedure TBGLCustomFont.SetVerticalAlign(AValue: TTextLayout);
begin
  FVerticalAlign := AValue;
end;

function TBGLCustomFont.GetDefaultColor: TBGRAPixel;
begin
  result := BGRAWhite;
end;

procedure TBGLCustomFont.SwapRectIfNeeded(var ARect: TRectF);
var temp: single;
begin
  if ARect.Right < ARect.Left then
  begin
    temp := ARect.Left;
    ARect.Left := ARect.Right;
    ARect.Right := temp;
  end;
  if ARect.Bottom < ARect.Top then
  begin
    temp := ARect.Top;
    ARect.Top := ARect.Bottom;
    ARect.Bottom := temp;
  end;
end;

procedure TBGLCustomFont.SwapRectIfNeeded(var ARect: TRect);
var temp: integer;
begin
  if ARect.Right < ARect.Left then
  begin
    temp := ARect.Left;
    ARect.Left := ARect.Right;
    ARect.Right := temp;
  end;
  if ARect.Bottom < ARect.Top then
  begin
    temp := ARect.Top;
    ARect.Top := ARect.Bottom;
    ARect.Bottom := temp;
  end;
end;

procedure TBGLCustomFont.SetPadding(AValue: TRectF);
begin
  FPadding:=AValue;
end;

function TBGLCustomFont.GetPadding: TRectF;
begin
  result := FPadding;
end;

procedure TBGLCustomFont.Init;
begin
  FScale:= 1;
  FStepX:= 0;
  FHorizontalAlign:= taLeftJustify;
  FVerticalAlign:= tlTop;
  FJustify:= false;
  FPadding := RectF(1,1,1,1);
end;

procedure TBGLCustomFont.FreeMemoryOnDestroy;
begin
  FreeMemory;
end;

procedure TBGLCustomFont.FreeMemory;
begin

end;

constructor TBGLCustomFont.Create(AFilename: UTF8String);
begin
  Init;
  LoadFromFile(AFilename);
end;

destructor TBGLCustomFont.Destroy;
begin
  FreeMemoryOnDestroy;
  inherited Destroy;
end;

procedure TBGLCustomFont.TextOut(X, Y: Single; const Text: UTF8String);
begin
  DoTextOut(X,Y,Text,GetDefaultColor);
end;

procedure TBGLCustomFont.TextOut(X, Y: Single; const Text: UTF8String;
  AColor: TBGRAPixel);
begin
  DoTextOut(X,Y,Text,AColor);
end;

procedure TBGLCustomFont.TextOut(X, Y: Single; const Text: UTF8String;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout);
begin
  TextOut(X,Y,Text,AHorizAlign,AVertAlign,GetDefaultColor);
end;

procedure TBGLCustomFont.TextOut(X, Y: Single; const Text: UTF8String;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel);
var PrevHorizAlign: TAlignment;
    PrevVertAlign: TTextLayout;
begin
  PrevHorizAlign:= GetHorizontalAlign;
  PrevVertAlign:= GetVerticalAlign;
  SetHorizontalAlign(AHorizAlign);
  SetVerticalAlign(AVertAlign);
  DoTextOut(X,Y,Text,AColor);
  SetHorizontalAlign(PrevHorizAlign);
  SetVerticalAlign(PrevVertAlign);
end;

procedure TBGLCustomFont.TextRect(X, Y, Width, Height: Single;
  const Text: UTF8String);
begin
  DoTextRect(X+Padding.Left,Y+Padding.Top,Width-Padding.Left-Padding.Right,Height-Padding.Top-Padding.Bottom,Text,GetDefaultColor);
end;

procedure TBGLCustomFont.TextRect(X, Y, Width, Height: Single;
  const Text: UTF8String; AColor: TBGRAPixel);
begin
  DoTextRect(X+Padding.Left,Y+Padding.Top,Width-Padding.Left-Padding.Right,Height-Padding.Top-Padding.Bottom,Text,AColor);
end;

procedure TBGLCustomFont.TextRect(X, Y, Width, Height: Single;
  const Text: UTF8String; AVertAlign: TTextLayout);
begin
  TextRect(X+Padding.Left,Y+Padding.Top,Width-Padding.Left-Padding.Right,Height-Padding.Top-Padding.Bottom,Text,AVertAlign,GetDefaultColor);
end;

procedure TBGLCustomFont.TextRect(X, Y, Width, Height: Single;
  const Text: UTF8String; AVertAlign: TTextLayout; AColor: TBGRAPixel);
var PrevVertAlign: TTextLayout;
begin
  PrevVertAlign:= GetVerticalAlign;
  SetVerticalAlign(AVertAlign);
  DoTextRect(X+Padding.Left,Y+Padding.Top,Width-Padding.Left-Padding.Right,Height-Padding.Top-Padding.Bottom,Text,AColor);
  SetVerticalAlign(PrevVertAlign);
end;

procedure TBGLCustomFont.TextRect(X, Y, Width, Height: Single;
  const Text: UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout);
begin
  TextRect(X+Padding.Left,Y+Padding.Top,Width-Padding.Left-Padding.Right,Height-Padding.Top-Padding.Bottom,Text,AHorizAlign,AVertAlign,GetDefaultColor);
end;

procedure TBGLCustomFont.TextRect(X, Y, Width, Height: Single;
  const Text: UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout;
  AColor: TBGRAPixel);
var PrevHorizAlign: TAlignment;
    PrevVertAlign: TTextLayout;
    PrevJustify: boolean;
begin
  PrevHorizAlign:= GetHorizontalAlign;
  PrevVertAlign:= GetVerticalAlign;
  PrevJustify := GetJustify;
  SetHorizontalAlign(AHorizAlign);
  SetVerticalAlign(AVertAlign);
  SetJustify(False);
  DoTextRect(X+Padding.Left,Y+Padding.Top,Width-Padding.Left-Padding.Right,Height-Padding.Top-Padding.Bottom,Text,AColor);
  SetHorizontalAlign(PrevHorizAlign);
  SetVerticalAlign(PrevVertAlign);
  SetJustify(PrevJustify);
end;

procedure TBGLCustomFont.TextRect(ARect: TRect; const Text: UTF8String);
begin
  SwapRectIfNeeded(ARect);
  with ARect do TextRect(Left,Top,Right-Left,Bottom-Top,Text);
end;

procedure TBGLCustomFont.TextRect(ARect: TRect; const Text: UTF8String;
  AColor: TBGRAPixel);
begin
  SwapRectIfNeeded(ARect);
  with ARect do TextRect(Left,Top,Right-Left,Bottom-Top,Text,
    AColor);
end;

procedure TBGLCustomFont.TextRect(ARect: TRect; const Text: UTF8String;
  AVertAlign: TTextLayout);
begin
  SwapRectIfNeeded(ARect);
  with ARect do TextRect(Left,Top,Right-Left,Bottom-Top,Text,
    AVertAlign);
end;

procedure TBGLCustomFont.TextRect(ARect: TRect; const Text: UTF8String;
  AVertAlign: TTextLayout; AColor: TBGRAPixel);
begin
  SwapRectIfNeeded(ARect);
  with ARect do TextRect(Left,Top,Right-Left,Bottom-Top,Text,
    AVertAlign, AColor);
end;

procedure TBGLCustomFont.TextRect(ARect: TRect; const Text: UTF8String;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout);
begin
  SwapRectIfNeeded(ARect);
  with ARect do TextRect(Left,Top,Right-Left,Bottom-Top,Text,
    AHorizAlign, AVertAlign);
end;

procedure TBGLCustomFont.TextRect(ARect: TRect; const Text: UTF8String;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel);
begin
  SwapRectIfNeeded(ARect);
  with ARect do TextRect(Left,Top,Right-Left,Bottom-Top,Text,
    AHorizAlign, AVertAlign, AColor);
end;

procedure TBGLCustomFont.TextRect(ARect: TRectF; const Text: UTF8String);
begin
  SwapRectIfNeeded(ARect);
  with ARect do TextRect(Left,Top,Right-Left,Bottom-Top,Text);
end;

procedure TBGLCustomFont.TextRect(ARect: TRectF; const Text: UTF8String;
  AColor: TBGRAPixel);
begin
  SwapRectIfNeeded(ARect);
  with ARect do TextRect(Left,Top,Right-Left,Bottom-Top,Text,
    AColor);
end;

procedure TBGLCustomFont.TextRect(ARect: TRectF; const Text: UTF8String;
  AVertAlign: TTextLayout);
begin
  SwapRectIfNeeded(ARect);
  with ARect do TextRect(Left,Top,Right-Left,Bottom-Top,Text,
    AVertAlign);
end;

procedure TBGLCustomFont.TextRect(ARect: TRectF; const Text: UTF8String;
  AVertAlign: TTextLayout; AColor: TBGRAPixel);
begin
  SwapRectIfNeeded(ARect);
  with ARect do TextRect(Left,Top,Right-Left,Bottom-Top,Text,
    AVertAlign, AColor);
end;

procedure TBGLCustomFont.TextRect(ARect: TRectF; const Text: UTF8String;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout);
begin
  SwapRectIfNeeded(ARect);
  with ARect do TextRect(Left,Top,Right-Left,Bottom-Top,Text,
    AHorizAlign, AVertAlign);
end;

procedure TBGLCustomFont.TextRect(ARect: TRectF; const Text: UTF8String;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel);
begin
  SwapRectIfNeeded(ARect);
  with ARect do TextRect(Left,Top,Right-Left,Bottom-Top,Text,
    AHorizAlign, AVertAlign, AColor);
end;

{ TBGLCustomBitmap }

procedure TBGLCustomBitmap.Init;
begin
  inherited Init;
  FTextureInvalidated := true;
  FActualRect := rect(0,0,0,0);
  FScanWidth := 0;
  FScanHeight:= 0;
  FTexture := nil;
  FLineOrder := riloTopToBottom;
end;

function TBGLCustomBitmap.GetTexture: IBGLTexture;
begin
  if (Width = 0) or (Height = 0) then
    result := BGLTextureFactory.Create
  else
  begin
    if FTextureInvalidated then
    begin
      FTextureInvalidated := false;
      if not TBGRAPixel_RGBAOrder and not BGLTextureFactory.SupportsBGRAOrder then SwapRedBlueWithoutInvalidate(Rect(0,0,Width,Height));
      if FTexture = nil then
        FTexture := BGLTextureFactory.Create(PLongWord(self.Data), AllocatedWidth,AllocatedHeight, Width,Height, TBGRAPixel_RGBAOrder or not BGLTextureFactory.SupportsBGRAOrder)
      else
        FTexture.Update(PLongWord(self.Data), AllocatedWidth,AllocatedHeight, Width,Height, TBGRAPixel_RGBAOrder or not BGLTextureFactory.SupportsBGRAOrder);
      if not TBGRAPixel_RGBAOrder and not BGLTextureFactory.SupportsBGRAOrder then SwapRedBlueWithoutInvalidate(Rect(0,0,Width,Height));
    end;
    result := FTexture;
  end;
end;

procedure TBGLCustomBitmap.NotifySizeTooBigForOpenGL;
begin
  raise exception.Create('Size too big for OpenGL');
end;

procedure TBGLCustomBitmap.NotifyOpenGLContextNotCreatedYet;
begin
  raise exception.Create('OpenGL context has not been created yet');
end;

function TBGLCustomBitmap.GetTextureGL: IUnknown;
begin
  Result:=GetTexture;
end;

procedure TBGLCustomBitmap.SwapRedBlueWithoutInvalidate(ARect: TRect);
var y: Int32or64;
    p: PBGRAPixel;
begin
  if not CheckClippedRectBounds(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom) then exit;
  for y := ARect.Top to ARect.Bottom-1 do
  begin
    p := GetScanlineFast(y)+ARect.Left;
    TBGRAFilterScannerSwapRedBlue.ComputeFilterAt(p,p, ARect.Right-ARect.Left, False);
  end;
end;

procedure TBGLCustomBitmap.InvalidateBitmap;
begin
  inherited InvalidateBitmap;
  FTextureInvalidated := true;
end;

procedure TBGLCustomBitmap.Fill(const c: TBGRAPixel);
var oldClip: TRect;
begin
  oldClip := ClipRect;
  NoClip;
  FillRect(ClipRect, c, dmSet);
  ClipRect := oldClip;
end;

procedure TBGLCustomBitmap.NoClip;
begin
  ClipRect := FActualRect;
end;

destructor TBGLCustomBitmap.Destroy;
begin
  if FTexture <> nil then
  begin
    //always free the memory of the texture
    FTexture.FreeMemory;
    FTexture := nil;
  end;
  inherited Destroy;
end;

procedure TBGLCustomBitmap.SwapRedBlue;
var previousClip : TRect;
begin
  previousClip := ClipRect;
  NoClip;
  SwapRedBlue(rect(0,0,Width,Height));
  ClipRect := previousClip;
end;

function TBGLCustomBitmap.Resample(newWidth, newHeight: integer;
  mode: TResampleMode; ACopyProperties: Boolean=False): TBGLCustomBitmap;
var temp,resampled: TBGRACustomBitmap;
begin
  temp := TBGRABitmap.Create(FActualWidth,FActualHeight);
  temp.PutImage(-FActualRect.Left,-FActualRect.Top, self, dmSet);
  temp.ResampleFilter := ResampleFilter;
  resampled := temp.Resample(NewWidth,NewHeight,mode);
  temp.Free;
  Result:= NewBitmap(resampled) as TBGLCustomBitmap;
  resampled.Free;
  if ACopyProperties then CopyPropertiesTo(Result);
end;

procedure TBGLCustomBitmap.ApplyGlobalOpacity(alpha: byte);
var oldClip: TRect;
begin
  oldClip := ClipRect;
  NoClip;
  ApplyGlobalOpacity(FActualRect,alpha);
  ClipRect := oldClip;
end;

procedure TBGLCustomBitmap.ReplaceColor(before, after: TColor);
var oldClip: TRect;
begin
  oldClip := ClipRect;
  NoClip;
  ReplaceColor(FActualRect, before, after);
  ClipRect := oldClip;
end;

procedure TBGLCustomBitmap.ReplaceColor(const ABefore, AAfter: TBGRAPixel);
var oldClip: TRect;
begin
  oldClip := ClipRect;
  NoClip;
  ReplaceColor(FActualRect, ABefore, AAfter);
  ClipRect := oldClip;
end;

procedure TBGLCustomBitmap.ReplaceTransparent(const AAfter: TBGRAPixel);
var oldClip: TRect;
begin
  oldClip := ClipRect;
  NoClip;
  ReplaceTransparent(FActualRect,AAfter);
  ClipRect := oldClip;
end;

procedure TBGLCustomBitmap.SetClipRect(const AValue: TRect);
var r: TRect;
begin
  r := TRect.Intersect(AValue, FActualRect);
  inherited SetClipRect(r);
end;

procedure TBGLCustomBitmap.SetSize(AWidth, AHeight: integer);
var AllocatedWidthNeeded,AllocatedHeightNeeded,
    MaxTexSize: Integer;
begin
  if AWidth < 0 then AWidth := 0;
  if AHeight < 0 then AHeight := 0;
  if (AWidth = Width) and (AHeight = Height) then exit;
  if Assigned(BGLTextureFactory) and BGLTextureFactory.GetNonPowerOfTwoSizeSupport then
  begin
    AllocatedWidthNeeded := AWidth;
    AllocatedHeightNeeded := AHeight;
  end else
  begin
    AllocatedWidthNeeded := GetPowerOfTwo(AWidth);
    AllocatedHeightNeeded := GetPowerOfTwo(AHeight);
  end;
  MaxTexSize := GetOpenGLMaxTexSize;
  if (AllocatedWidthNeeded > MaxTexSize) or
     (AllocatedHeightNeeded > MaxTexSize) then
  begin
    if MaxTexSize = 0 then
      NotifyOpenGLContextNotCreatedYet
    else
      NotifySizeTooBigForOpenGL;
    if AllocatedWidthNeeded > MaxTexSize then
    begin
      AllocatedWidthNeeded := MaxTexSize;
      AWidth := MaxTexSize;
    end;
    if AllocatedHeightNeeded > MaxTexSize then
    begin
      AllocatedHeightNeeded := MaxTexSize;
      AHeight := MaxTexSize;
    end;
  end;
  FActualWidth := AWidth;
  FActualHeight := AHeight;
  FAllocatedWidth := AllocatedWidthNeeded;
  FAllocatedHeight := AllocatedHeightNeeded;
  FActualRect := rect(0,0,FActualWidth,FActualHeight);
  if (FAllocatedWidth <> inherited Width) or
     (FAllocatedHeight <> inherited Height) then
    inherited SetSize(FAllocatedWidth, FAllocatedHeight);
  inherited NoClip;
  inherited FillRect(Width,0,FAllocatedWidth,Height, BGRAPixelTransparent, dmSet);
  inherited FillRect(0,Height,FAllocatedWidth,FAllocatedHeight, BGRAPixelTransparent, dmSet);
  NoClip;
  FScanWidth := Width;
  FScanHeight:= Height;
  FTextureInvalidated:= true;
end;

function TBGLCustomBitmap.MakeTextureAndFree: IBGLTexture;
begin
  result := Texture;
  FTexture := nil;
  Free;
end;

end.

