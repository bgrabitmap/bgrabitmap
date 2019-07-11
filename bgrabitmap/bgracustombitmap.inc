{=== TBGRACustomBitmap and IBGRAScanner ===}
{$IFDEF INCLUDE_SCANNER_INTERFACE}
{$UNDEF INCLUDE_SCANNER_INTERFACE}
type
  TBGRACustomBitmap = class;

{==== IBGRAScanner ====}
  {* Interface for a scanner. A scanner is like an image, but its content
     has no limit and it can be calculated on the fly. It is like a
     infinite readonly image.
   *
   * Note: it must not implement reference counting even if it is an interface
   *
   * ''TBGRACustomBitmap'' implements this interface and the content is repeated
     horizontally and vertically. There are also various classes
     in ''BGRAGradientScanner'' unit that generate gradients on the fly and
     in ''BGRATransform'' unit that provide geometrical transformations of images }
  IBGRAScanner = interface
    {** Move to the position (''X'',''Y'') for the next call to ''ScanNextPixel'' }
    procedure ScanMoveTo(X,Y: Integer);
    {** Scan the pixel at the current location and increments ''X'' }
    function ScanNextPixel: TBGRAPixel;
    function ScanNextExpandedPixel: TExpandedPixel;
    {** Scan at any location using floating point coordinates }
    function ScanAt(X,Y: Single): TBGRAPixel;
    function ScanAtExpanded(X,Y: Single): TExpandedPixel;
    {** Scan at any location using integer coordinates }
    function ScanAtInteger(X,Y: integer): TBGRAPixel;
    function ScanAtIntegerExpanded(X,Y: integer): TExpandedPixel;
    {** Copy a row of pixels from ''X'' to ''X''+''count''-1 to a specified destination
        ''pdest''. ''mode'' indicates how to combine with existing data }
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode);
    procedure ScanSkipPixels(ACount: integer);
    {** Returns True if the function ''ScanPutPixels'' is available. Otherwise
        you need to call ''ScanNextPixel'' and combine pixels for example
        with ''SetPixel'' }
    function IsScanPutPixelsDefined: boolean;
    {** Returns the corresponding OpenGL texture. The value is ''nil'' if no texture is associated. **}
    function GetTextureGL: IUnknown;
    function GetImageBoundsWithin(const ARect: TRect; Channel: TChannel = cAlpha; ANothingValue: Byte = 0): TRect; overload;
    function GetImageBoundsWithin(const ARect: TRect; Channels: TChannels; ANothingValue: Byte = 0): TRect; overload;
    function ProvidesScanline(ARect: TRect): boolean;
    function GetScanlineAt(X,Y: integer): PBGRAPixel;
    function GetScanCustomColorspace: TColorspaceAny;
    procedure ScanNextCustomChunk(var ACount: integer; out APixels: Pointer);
    procedure ScanNextMaskChunk(var ACount: integer; out AMask: PByteMask; out AStride: integer);
    function ScanAtIntegerMask(X,Y: integer): TByteMask;
    function ScanAtMask(X,Y: Single): TByteMask;
  end;

  {** A type of function of a scanner that returns the content at floating point coordinates }
  TScanAtFunction = function (X,Y: Single): TBGRAPixel of object;
  {** A type of function of a scanner that returns the content at integer coordinates }
  TScanAtIntegerFunction = function (X,Y: Integer): TBGRAPixel of object;
  {** A type of function of a scanner that returns the next pixel }
  TScanNextPixelFunction = function: TBGRAPixel of object;

  { TBGRACustomScanner }
  {* Base class for implementing ''IBGRAScanner'' interface }
  TBGRACustomScanner = class(IBGRAScanner)
  private
    FCurX,FCurY: integer;
    FOwnCur: boolean;
    FChunk: packed array[0..3] of TExpandedPixel;
  public
    function ScanAtInteger(X,Y: integer): TBGRAPixel; virtual;
    function ScanAtIntegerExpanded(X,Y: integer): TExpandedPixel; virtual;
    procedure ScanMoveTo(X,Y: Integer); virtual;
    function ScanNextPixel: TBGRAPixel; virtual;
    function ScanNextExpandedPixel: TExpandedPixel; virtual;
    function ScanAt(X,Y: Single): TBGRAPixel; virtual; abstract;
    function ScanAtExpanded(X,Y: Single): TExpandedPixel; virtual;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); virtual;
    procedure ScanSkipPixels(ACount: integer); virtual;
    function IsScanPutPixelsDefined: boolean; virtual;
    function GetTextureGL: IUnknown; virtual;
    function GetImageBoundsWithin(const ARect: TRect; Channel: TChannel = cAlpha; ANothingValue: Byte = 0): TRect; overload; virtual;
    function GetImageBoundsWithin(const ARect: TRect; Channels: TChannels; ANothingValue: Byte = 0): TRect; overload; virtual;
    function ProvidesScanline({%H-}ARect: TRect): boolean; virtual;
    function GetScanlineAt({%H-}X,{%H-}Y: integer): PBGRAPixel; virtual;
    function GetScanCustomColorspace: TColorspaceAny; virtual;
    procedure ScanNextCustomChunk(var ACount: integer; out APixels: Pointer); virtual;
    procedure ScanNextMaskChunk(var ACount: integer; out AMask: PByteMask; out AStride: integer); virtual;
    function ScanAtIntegerMask(X,Y: integer): TByteMask; virtual;
    function ScanAtMask(X,Y: Single): TByteMask; virtual;
  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
  end;
{$ENDIF}

{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}
type
{==== TBGRACustomBitmap ====}

  { TBGRACustomBitmap }
  {* This is the base class for ''TBGRABitmap''. It is the direct parent of
     ''TBGRADefaultBitmap'' class, which is the parent of the diverse
     implementations. A bitmap can be used as a scanner using the interface
     ''IBGRAScanner'' }
  TBGRACustomBitmap = class(specialize TGenericUniversalBitmap<TBGRAPixel,TBGRAPixelColorspace>,IBGRAScanner)
  protected
    FXorMask: TBGRACustomBitmap;

    { accessors to properies }
     procedure SetXorMask(AValue: TBGRACustomBitmap);

     function GetAverageColor: TColor; virtual; abstract;
     function GetAveragePixel: TBGRAPixel; virtual; abstract;

     //FreePascal drawing routines
     function GetCanvasFP: TFPImageCanvas; virtual; abstract;
     function GetCanvasDrawModeFP: TDrawMode; virtual; abstract;
     procedure SetCanvasDrawModeFP(const AValue: TDrawMode); virtual; abstract;

     //GUI bitmap object
     function GetBitmap: TBitmap; virtual; abstract;
     function GetCanvas: TCanvas; virtual; abstract;
     function GetCanvasOpacity: byte; virtual; abstract;
     procedure SetCanvasOpacity(AValue: byte); virtual; abstract;
     function GetCanvasAlphaCorrection: boolean; virtual; abstract;
     procedure SetCanvasAlphaCorrection(const AValue: boolean); virtual; abstract;

     procedure Init; override;
     function InternalNew: TCustomUniversalBitmap; override;

     procedure InternalArc(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; AOptions: TArcOptions; ADrawChord: boolean = false; ATexture: IBGRAScanner = nil); overload;
     procedure InternalArc(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; AOptions: TArcOptions; ADrawChord: boolean = false; ATexture: IBGRAScanner = nil); overload; virtual; abstract;
     procedure InternalArcInRect(r: TRect; StartAngleRad,EndAngleRad: Single; ABorderColor : TBGRAPixel; w: single; AFillColor: TBGRAPixel; AOptions: TArcOptions; ADrawChord: boolean = false; ATexture: IBGRAScanner = nil);
     procedure InternalFillArcInRect(r: TRect; StartAngleRad,EndAngleRad: Single; AFillColor: TBGRAPixel; AOptions: TArcOptions; ATexture: IBGRAScanner = nil);

  public
     {** Resample filter is used when resizing the bitmap. See [[BGRABitmap Miscellaneous types#Images and resampling|resampling types]] }
     ResampleFilter : TResampleFilter;

     {** Scan interpolation filter is used when the bitmap is used
         as a scanner (interface ''IBGRAScanner'') }
     ScanInterpolationFilter: TResampleFilter;
     ScanMaskChannel: TChannel;

     {** Cursor position for mouse pointer }
     HotSpot: TPoint;

     { ** Free reference to xor mask }
     procedure DiscardXorMask; virtual;
     { ** Allocate xor mask }
     procedure NeedXorMask; virtual;
     {** Xor mask to be applied when image is drawn }
     property XorMask: TBGRACustomBitmap read FXorMask write SetXorMask;

     {** Average color of the image }
     property AverageColor: TColor Read GetAverageColor;
     {** Average color (including alpha) of the image }
     property AveragePixel: TBGRAPixel Read GetAveragePixel;

     {** Canvas compatible with FreePascal }
     property CanvasFP: TFPImageCanvas read GetCanvasFP;
     {** Draw mode to used when image is access using FreePascal functions
         and ''Colors'' property }
     property CanvasDrawModeFP: TDrawMode read GetCanvasDrawModeFP write SetCanvasDrawModeFP;

     {** Bitmap in a format compatible with the current GUI.
         Don't forget to call ''InvalidateBitmap'' before using it
         if you changed something with direct pixel access (''Scanline''
         and ''Data'') }
     property Bitmap: TBitmap Read GetBitmap;
     {** Canvas provided by the GUI }
     property Canvas: TCanvas Read GetCanvas;
     {** Opacity to apply to changes made using GUI functions, provided
         ''CanvasAlphaCorrection'' is set to ''True'' }
     property CanvasOpacity: byte Read GetCanvasOpacity Write SetCanvasOpacity;
     {** Specifies if the alpha values must be corrected after GUI access
         to the bitmap }
     property CanvasAlphaCorrection: boolean Read GetCanvasAlphaCorrection Write SetCanvasAlphaCorrection;

  protected {----------- pen style accessors ----------------}
     function GetPenJoinStyle: TPenJoinStyle; virtual; abstract;
     procedure SetPenJoinStyle(const AValue: TPenJoinStyle); virtual; abstract;
     function GetPenMiterLimit: single; virtual; abstract;
     procedure SetPenMiterLimit(const AValue: single); virtual; abstract;
     function GetPenStyle: TPenStyle; virtual; abstract;
     procedure SetPenStyle(const AValue: TPenStyle); virtual; abstract;
     function GetCustomPenStyle: TBGRAPenStyle; virtual; abstract;
     procedure SetCustomPenStyle(const AValue: TBGRAPenStyle); virtual; abstract;

     function GetArrowEndRepeat: integer; virtual; abstract;
     function GetArrowStartRepeat: integer; virtual; abstract;
     procedure SetArrowEndRepeat(AValue: integer); virtual; abstract;
     procedure SetArrowStartRepeat(AValue: integer); virtual; abstract;
     function GetArrowEndOffset: single; virtual; abstract;
     function GetArrowStartOffset: single; virtual; abstract;
     procedure SetArrowEndOffset(AValue: single); virtual; abstract;
     procedure SetArrowStartOffset(AValue: single); virtual; abstract;
     function GetArrowEndSize: TPointF; virtual; abstract;
     function GetArrowStartSize: TPointF; virtual; abstract;
     procedure SetArrowEndSize(AValue: TPointF); virtual; abstract;
     procedure SetArrowStartSize(AValue: TPointF); virtual; abstract;

  public {----------- pen style ----------------}
     {** How to join segments. See [[BGRABitmap Types imported from Graphics|BGRAGraphics]] }
     property JoinStyle: TPenJoinStyle read GetPenJoinStyle Write SetPenJoinStyle;
     {** Limit for the extension of the segments when joining them
         with ''pjsMiter'' join style, expressed in multiples of the width
         of the pen }
     property JoinMiterLimit: single read GetPenMiterLimit Write SetPenMiterLimit;
     {** Pen style. See [[BGRABitmap Types imported from Graphics|BGRAGraphics]] }
     property PenStyle: TPenStyle read GetPenStyle Write SetPenStyle;
     {** Custom pen style. See [[BGRABitmap Geometry types|geometric types]] }
     property CustomPenStyle: TBGRAPenStyle read GetCustomPenStyle write SetCustomPenStyle;

     {** Size of arrows at the start of the line }
     property ArrowStartSize: TPointF read GetArrowStartSize write SetArrowStartSize;
     {** Size of arrows at the end of the line }
     property ArrowEndSize: TPointF read GetArrowEndSize write SetArrowEndSize;
     {** Offset of the arrow from the start of the line }
     property ArrowStartOffset: single read GetArrowStartOffset write SetArrowStartOffset;
     {** Offset of the arrow from the end of the line }
     property ArrowEndOffset: single read GetArrowEndOffset write SetArrowEndOffset;
     {** Number of times to repeat the starting arrow }
     property ArrowStartRepeat: integer read GetArrowStartRepeat write SetArrowStartRepeat;
     {** Number of times to repeat the ending arrow }
     property ArrowEndRepeat: integer read GetArrowEndRepeat write SetArrowEndRepeat;

     procedure ArrowStartAsNone; virtual; abstract;
     procedure ArrowStartAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1); virtual; abstract;
     procedure ArrowStartAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5); virtual; abstract;
     procedure ArrowStartAsTail; virtual; abstract;

     procedure ArrowEndAsNone; virtual; abstract;
     procedure ArrowEndAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1); virtual; abstract;
     procedure ArrowEndAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5); virtual; abstract;
     procedure ArrowEndAsTail; virtual; abstract;

  protected {-------------------font style accessors------------------------}
     function GetFontAntialias: Boolean;
     procedure SetFontAntialias(const AValue: Boolean);
     function GetFontRenderer: TBGRACustomFontRenderer; virtual; abstract;
     procedure SetFontRenderer(AValue: TBGRACustomFontRenderer); virtual; abstract;
     function GetFontHeight: integer; virtual; abstract;
     procedure SetFontHeight(AHeight: integer); virtual; abstract;
     function GetFontFullHeight: integer; virtual; abstract;
     procedure SetFontFullHeight(AHeight: integer); virtual; abstract;
     function GetFontVerticalAnchorOffset: single; virtual; abstract;
     function GetFontPixelMetric: TFontPixelMetric; virtual; abstract;

     function GetFontRightToLeftFor(AText: string): boolean;

  public {-------------------font style------------------------}
     {** Specifies the font to use. Unless the font renderer accept otherwise,
         the name is in human readable form, like 'Arial', 'Times New Roman', ... }
     FontName: string;
     {** Specifies the set of styles to be applied to the font.
         These can be ''fsBold'', ''fsItalic'', ''fsStrikeOut'', ''fsUnderline''.
         So the value [''fsBold'',''fsItalic''] means that the font must be bold and italic.
         See [[BGRABitmap Miscellaneous types|miscellaneous types]] }
     FontStyle: TFontStyles;

     {** Specifies the quality of rendering. Default value is ''fqSystem''.
         See [[BGRABitmap Miscellaneous types|miscellaneous types]] }
     FontQuality : TBGRAFontQuality;

     {** Specifies the rotation of the text, for functions that support text rotation.
         It is expressed in tenth of degrees, positive values going counter-clockwise. }
     FontOrientation: integer;

     {** Specifies how the font is vertically aligned relative to the start coordinate.
         See [[BGRABitmap Miscellaneous types|miscellaneous types]]}
     FontVerticalAnchor: TFontVerticalAnchor;

     {** Specifies the base direction of the text (cf Unicode). By default, it is
         automatically determined by the first strongly oriented character.
         You can specify another base direction here however it is not taken
         into account by the LCL on Linux. }
     FontBidiMode: TFontBidiMode;

     {** Specifies the height of the font in pixels without taking into account
         additional line spacing. A negative value means that it is the
         full height instead (see below) }
     property FontHeight: integer Read GetFontHeight Write SetFontHeight;

     {** Specifies the height of the font in pixels, taking into account the
         additional line spacing defined for the font }
     property FontFullHeight: integer read GetFontFullHeight write SetFontFullHeight;

     {** Simplified property to specify the quality (see ''FontQuality'') }
     property FontAntialias: Boolean read GetFontAntialias write SetFontAntialias;

     property FontVerticalAnchorOffset: single read GetFontVerticalAnchorOffset;

     {** Returns measurement for the current font in pixels }
     property FontPixelMetric: TFontPixelMetric read GetFontPixelMetric;

     {** Specifies the font renderer. When working with the LCL,
         by default it is an instance of ''TLCLFontRenderer'' of
         unit ''BGRAText''. Other renderers are provided in ''BGRATextFX''
         unit and ''BGRAVectorize'' unit. Additionally, ''BGRAFreeType''
         provides a renderer independent from the LCL.
       *
       * Once you assign a renderer, it will automatically be freed when
         the bitmap is freed. The renderers may provide additional styling
         for the font, not accessible with the properties in this class
       *
       * See [[BGRABitmap tutorial Font rendering|font rendering]]}
     property FontRenderer: TBGRACustomFontRenderer read GetFontRenderer write SetFontRenderer;

  public
     constructor Create(AFPImage: TFPCustomImage); overload; virtual; abstract;
     constructor Create(ABitmap: TBitmap; AUseTransparent: boolean = true); overload; virtual; abstract;
     constructor Create(AFilename: string); overload; virtual; abstract;
     constructor Create(AFilename: string; AIsUtf8Filename: boolean); overload; virtual; abstract;
     constructor Create(AFilename: string; AIsUtf8Filename: boolean; AOptions: TBGRALoadingOptions); overload; virtual; abstract;
     constructor Create(AStream: TStream); overload; virtual; abstract;

     function NewBitmap: TBGRACustomBitmap; overload; virtual; abstract;
     function NewBitmap(AWidth, AHeight: integer): TBGRACustomBitmap; overload; virtual; abstract;
     function NewBitmap(AWidth, AHeight: integer; Color: TBGRAPixel): TBGRACustomBitmap; overload; virtual; abstract;
     function NewBitmap(Filename: string): TBGRACustomBitmap; overload; virtual; abstract;
     function NewBitmap(Filename: string; AIsUtf8: boolean): TBGRACustomBitmap; overload; virtual; abstract;
     function NewBitmap(Filename: string; AIsUtf8: boolean; AOptions: TBGRALoadingOptions): TBGRACustomBitmap; overload; virtual; abstract;
     function NewBitmap(AFPImage: TFPCustomImage): TBGRACustomBitmap; overload; virtual; abstract;

     procedure LoadFromStream(AStream: TStream; AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions); override;

     {==== Reference counting ====}

     {** Adds a reference (this reference count is not the same as
         the reference count of an interface, it changes only by
         explicit calls) }
     function NewReference: TBGRACustomBitmap;
     {** Returns an object with a reference count equal to 1. Duplicate
         this bitmap if necessary }
     function GetUnique: TBGRACustomBitmap;
     function Duplicate(DuplicateProperties: Boolean = False): TBGRACustomBitmap;
     function Duplicate(DuplicateProperties, DuplicateXorMask: Boolean) : TBGRACustomBitmap;
     procedure CopyPropertiesTo(ABitmap: TCustomUniversalBitmap); override;
     function GetPart(ARect: TRect): TBGRACustomBitmap;

     function CreateBrushTexture(ABrushStyle: TBrushStyle; APatternColor, ABackgroundColor: TBGRAPixel;
                 AWidth: integer = 8; AHeight: integer = 8; APenWidth: single = 1): TBGRACustomBitmap;

     {** Can only be called with an existing instance of ''TBGRACustomBitmap''.
         Sets the dimensions of an existing ''TBGRACustomBitmap'' instance. }
     procedure SetSize(AWidth, AHeight: integer); override;

     {==== Retrieve image from system ====}

     {** Gets the content of the specified device context }
     procedure LoadFromDevice(DC: HDC); overload; virtual; abstract;
     {** Gets the content from the specified rectangular area of a device context }
     procedure LoadFromDevice(DC: HDC; ARect: TRect); overload; virtual; abstract;
     {** Fills the content with a screenshot of the primary monitor }
     procedure TakeScreenshotOfPrimaryMonitor; virtual; abstract;
     {** Fills the content with a screenshot of the specified rectangular area of the desktop
         (it can be from any screen) }
     procedure TakeScreenshot(ARect: TRect); virtual; abstract;
     {** For more methods, see derived class [[TBGRABitmap class|TBGRABitmap]] }

     {==== Drawing functions ====}

     {Pixel functions}
     procedure SetPixel(x, y: int32or64; c: TColor); overload; virtual; abstract;
     procedure XorPixel(x, y: int32or64; const c: TBGRAPixel); overload; virtual; abstract;
     procedure DrawPixel(x, y: int32or64; const c: TBGRAPixel; ADrawMode: TDrawMode); overload; override;
     procedure FastBlendPixel(x, y: int32or64; const c: TBGRAPixel); virtual; abstract;
     function GetPixel256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TBGRAPixel; virtual; abstract;
     function GetPixel(x, y: single; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TBGRAPixel; overload; virtual; abstract;
     function GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; overload; virtual; abstract;
     function GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter; repeatX: boolean; repeatY: boolean): TBGRAPixel; overload; virtual; abstract;
     function GetPixelCycle256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; overload; virtual; abstract;
     function GetPixelCycle256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter; repeatX: boolean; repeatY: boolean): TBGRAPixel; overload; virtual; abstract;

     {Line primitives}
     procedure XorHorizLine(x, y, x2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure DrawHorizLine(x, y, x2: int32or64; ec: TExpandedPixel); overload; virtual; abstract;
     procedure DrawHorizLine(x, y, x2: int32or64; texture: IBGRAScanner); overload;
     procedure FastBlendHorizLine(x, y, x2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure DrawHorizLineDiff(x, y, x2: int32or64; c, compare: TBGRAPixel; maxDiff: byte); virtual; abstract;

     procedure XorVertLine(x, y, y2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure DrawVertLine(x, y, y2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure FastBlendVertLine(x, y, y2: int32or64; c: TBGRAPixel); virtual; abstract;

     {==== Rectangles, ellipses and path (floating point coordinates) ====}
     {* These functions use the current pen style/cap/join. The parameter ''w''
        specifies the width of the line and the base unit for dashes
      * The coordinates are pixel-centered, so that when filling a rectangle,
        if the supplied values are integers, the border will be half transparent.
        If you want the border to be completely filled, you can subtract/add
        0.5 to the coordinates to include the remaining thin border.
        See [[BGRABitmap tutorial 13|coordinate system]]. }

     {==== Multi-shape fill ====}

     {** Draws and fill a polyline using current pen style/cap/join in one go.
         The stroke is stricly over the fill even if partially transparent.
         ''fillcolor'' specifies a color to fill the polygon formed by the points }
     procedure DrawPolyLineAntialias(const points: array of TPointF; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel); overload; virtual; abstract;
     {** Draws a filled polygon using current pen style/cap/join in one go.
         The stroke is stricly over the fill even if partially transparent.
         The polygon is always closed. You don't need to set the last point
         to be the same as the first point. }
     procedure DrawPolygonAntialias(const points: array of TPointF; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel); overload; virtual; abstract;

     procedure EllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel; w: single; back: TBGRAPixel); overload; virtual; abstract;
     procedure EllipseAntialias(AOrigin, AXAxis, AYAxis: TPointF; c: TBGRAPixel; w: single; back: TBGRAPixel); overload; virtual; abstract;

     procedure DrawPath(APath: IBGRAPath; AStrokeColor: TBGRAPixel; AWidth: single; AFillColor: TBGRAPixel); overload; virtual; abstract;
     procedure DrawPath(APath: IBGRAPath; AStrokeTexture: IBGRAScanner; AWidth: single; AFillColor: TBGRAPixel); overload; virtual; abstract;
     procedure DrawPath(APath: IBGRAPath; AStrokeColor: TBGRAPixel; AWidth: single; AFillTexture: IBGRAScanner); overload; virtual; abstract;
     procedure DrawPath(APath: IBGRAPath; AStrokeTexture: IBGRAScanner; AWidth: single; AFillTexture: IBGRAScanner); overload; virtual; abstract;

     procedure DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix; AStrokeColor: TBGRAPixel; AWidth: single; AFillColor: TBGRAPixel); overload; virtual; abstract;
     procedure DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix; AStrokeTexture: IBGRAScanner; AWidth: single; AFillColor: TBGRAPixel); overload; virtual; abstract;
     procedure DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix; AStrokeColor: TBGRAPixel; AWidth: single; AFillTexture: IBGRAScanner); overload; virtual; abstract;
     procedure DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix; AStrokeTexture: IBGRAScanner; AWidth: single; AFillTexture: IBGRAScanner); overload; virtual; abstract;

     {==== Gradient/textured polygons ====}

     procedure FillTriangleLinearColor(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel); overload; virtual; abstract;
     procedure FillTriangleLinearColorAntialias(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel); overload; virtual; abstract;
     procedure FillTriangleLinearMapping(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; TextureInterpolation: Boolean= True); overload; virtual; abstract;
     procedure FillTriangleLinearMappingLightness(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; light1,light2,light3: word; TextureInterpolation: Boolean= True); overload; virtual; abstract;
     procedure FillTriangleLinearMappingAntialias(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF); overload; virtual; abstract;

     procedure FillQuadLinearColor(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel); overload; virtual; abstract;
     procedure FillQuadLinearColorAntialias(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel); overload; virtual; abstract;
     procedure FillQuadLinearMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; TextureInterpolation: Boolean= True; ACulling: TFaceCulling = fcNone; ACropToPolygon: boolean = true); overload; virtual; abstract;
     procedure FillQuadLinearMappingLightness(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; light1,light2,light3,light4: word; TextureInterpolation: Boolean= True); overload; virtual; abstract;
     procedure FillQuadLinearMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACulling: TFaceCulling = fcNone); overload; virtual; abstract;
     procedure FillQuadPerspectiveMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ADrawMode: TDrawMode = dmDrawWithTransparency); overload; virtual; abstract;
     procedure FillQuadPerspectiveMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACleanBorders: TRect; ADrawMode: TDrawMode = dmDrawWithTransparency); overload; virtual; abstract;
     procedure FillQuadPerspectiveMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF); overload; virtual; abstract;
     procedure FillQuadPerspectiveMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACleanBorders: TRect); overload; virtual; abstract;
     procedure FillQuadAffineMapping(Orig,HAxis,VAxis: TPointF; AImage: TBGRACustomBitmap; APixelCenteredCoordinates: boolean = true; ADrawMode: TDrawMode = dmDrawWithTransparency; AOpacity: byte = 255); virtual; abstract;
     procedure FillQuadAffineMappingAntialias(Orig,HAxis,VAxis: TPointF; AImage: TBGRACustomBitmap; APixelCenteredCoordinates: boolean = true; AOpacity: byte = 255); virtual; abstract;

     procedure FillEllipseLinearColorAntialias(x, y, rx, ry: single; outercolor, innercolor: TBGRAPixel); overload; virtual; abstract;
     procedure FillEllipseLinearColorAntialias(AOrigin, AXAxis, AYAxis: TPointF; outercolor, innercolor: TBGRAPixel); overload; virtual; abstract;

     procedure FillPolyLinearColor(const points: array of TPointF; AColors: array of TBGRAPixel);  overload; virtual; abstract;
     procedure FillPolyLinearMapping(const points: array of TPointF; texture: IBGRAScanner; texCoords: array of TPointF; TextureInterpolation: Boolean); overload; virtual; abstract;
     procedure FillPolyLinearMappingLightness(const points: array of TPointF; texture: IBGRAScanner; texCoords: array of TPointF; lightnesses: array of word; TextureInterpolation: Boolean); overload; virtual; abstract;
     procedure FillPolyPerspectiveMapping(const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner; texCoords: array of TPointF; TextureInterpolation: Boolean; zbuffer: psingle = nil); overload; virtual; abstract;
     procedure FillPolyPerspectiveMappingLightness(const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner; texCoords: array of TPointF; lightnesses: array of word; TextureInterpolation: Boolean; zbuffer: psingle = nil); overload; virtual; abstract;

     procedure Arc(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AColor: TBGRAPixel; w: single; ADrawChord: boolean; AFillColor: TBGRAPixel); overload;
     procedure Arc(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; w: single; ADrawChord: boolean; AFillColor: TBGRAPixel); overload;
     procedure FillChord(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AFillColor: TBGRAPixel); overload;
     procedure FillChord(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AFillColor: TBGRAPixel); overload;
     procedure FillChord(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; texture: IBGRAScanner); overload;
     procedure FillChord(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; texture: IBGRAScanner); overload;
     procedure FillChordInRect(const ARect: TRect; StartAngleRad,EndAngleRad: Single; AFillColor: TBGRAPixel); overload;
     procedure FillChordInRect(const ARect: TRect; StartAngleRad,EndAngleRad: Single; texture: IBGRAScanner); overload;

     procedure Pie(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel); overload;
     procedure Pie(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel); overload;
     procedure FillPie(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AFillColor: TBGRAPixel); overload;
     procedure FillPie(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AFillColor: TBGRAPixel); overload;
     procedure FillPie(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; texture: IBGRAScanner); overload;
     procedure FillPie(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; texture: IBGRAScanner); overload;
     procedure FillPieInRect(const ARect: TRect; StartAngleRad,EndAngleRad: Single; AFillColor: TBGRAPixel); overload;
     procedure FillPieInRect(const ARect: TRect; StartAngleRad,EndAngleRad: Single; texture: IBGRAScanner); overload;

     procedure RectangleAntialias(x, y, x2, y2: single; c: TBGRAPixel; w: single); overload; virtual;
     procedure RectangleAntialias(x, y, x2, y2: single; c: TBGRAPixel; w: single; back: TBGRAPixel); overload; virtual; abstract;
     procedure RectangleAntialias(x, y, x2, y2: single; texture: IBGRAScanner; w: single); overload; virtual; abstract;
     procedure RectangleWithin(x1,y1,x2,y2: single; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true); overload;
     procedure RectangleWithin(r: TRect; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel); overload;

     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; c: TBGRAPixel; w: single; options: TRoundRectangleOptions = []); overload; virtual; abstract;
     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; pencolor: TBGRAPixel; w: single; fillcolor: TBGRAPixel; options: TRoundRectangleOptions = []); overload; virtual; abstract;
     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; penTexture: IBGRAScanner; w: single; fillTexture: IBGRAScanner; options: TRoundRectangleOptions = []); overload; virtual; abstract;
     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; texture: IBGRAScanner; w: single; options: TRoundRectangleOptions = []); overload; virtual; abstract;
     procedure FillRoundRectAntialias(x,y,x2,y2,rx,ry: single; c: TBGRAPixel; options: TRoundRectangleOptions = []; pixelCenteredCoordinates: boolean = true); overload; virtual; abstract;
     procedure FillRoundRectAntialias(x,y,x2,y2,rx,ry: single; texture: IBGRAScanner; options: TRoundRectangleOptions = []; pixelCenteredCoordinates: boolean = true); overload; virtual; abstract;
     procedure EraseRoundRectAntialias(x,y,x2,y2,rx,ry: single; alpha: byte; options: TRoundRectangleOptions = []; pixelCenteredCoordinates: boolean = true); virtual; abstract;

     procedure FillRect(r: TRect; texture: IBGRAScanner; mode: TDrawMode; ditheringAlgorithm: TDitheringAlgorithm); overload; virtual;
     procedure FillRect(r: TRect; texture: IBGRAScanner; mode: TDrawMode; AScanOffset: TPoint; ditheringAlgorithm: TDitheringAlgorithm); overload; virtual;
     procedure FillRect(x, y, x2, y2: integer; texture: IBGRAScanner; mode: TDrawMode; ditheringAlgorithm: TDitheringAlgorithm); overload; virtual;
     procedure FillRect(x, y, x2, y2: integer; texture: IBGRAScanner; mode: TDrawMode; AScanOffset: TPoint; ditheringAlgorithm: TDitheringAlgorithm); overload; virtual; abstract;

     procedure TextOutCurved(ACursor: TBGRACustomPathCursor; sUTF8: string; AColor: TBGRAPixel; AAlign: TAlignment; ALetterSpacing: single); overload; virtual; abstract;
     procedure TextOutCurved(ACursor: TBGRACustomPathCursor; sUTF8: string; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single); overload; virtual; abstract;
     procedure TextOutCurved(APath: IBGRAPath; sUTF8: string; AColor: TBGRAPixel; AAlign: TAlignment; ALetterSpacing: single); overload; virtual;
     procedure TextOutCurved(APath: IBGRAPath; sUTF8: string; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single); overload; virtual;
     procedure TextRect(ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; c: TBGRAPixel); overload; virtual; abstract;
     procedure TextRect(ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; texture: IBGRAScanner); overload; virtual; abstract;
     procedure TextMultiline(x,y: single; sUTF8: string; c: TBGRAPixel; AAlign: TBidiTextAlignment = btaLeftJustify; AVertAlign: TTextLayout = tlTop; AParagraphSpacing: single = 0); overload;
     procedure TextMultiline(x,y: single; sUTF8: string; ATexture: IBGRAScanner; AAlign: TBidiTextAlignment = btaLeftJustify; AVertAlign: TTextLayout = tlTop; AParagraphSpacing: single = 0); overload;
     procedure TextMultiline(ALeft,ATop,AWidth: single; sUTF8: string; c: TBGRAPixel; AAlign: TBidiTextAlignment = btaNatural; AVertAlign: TTextLayout = tlTop; AParagraphSpacing: single = 0); overload; virtual; abstract;
     procedure TextMultiline(ALeft,ATop,AWidth: single; sUTF8: string; ATexture: IBGRAScanner; AAlign: TBidiTextAlignment = btaNatural; AVertAlign: TTextLayout = tlTop; AParagraphSpacing: single = 0); overload; virtual; abstract;
     function TextSize(sUTF8: string): TSize; overload; virtual; abstract;
     function TextAffineBox(sUTF8: string): TAffineBox; virtual; abstract;
     function TextSize(sUTF8: string; AMaxWidth: integer): TSize; overload; virtual; abstract;
     function TextSize(sUTF8: string; AMaxWidth: integer; ARightToLeft: boolean): TSize; overload; virtual; abstract;
     function TextFitInfo(sUTF8: string; AMaxWidth: integer): integer; virtual; abstract;

     { Draw the UTF8 encoded string, (x,y) being the top-left corner by default. The color c or texture is used to fill the text.
       The value of FontOrientation is taken into account, so that the text may be rotated. }
     procedure TextOut(x, y: single; sUTF8: string; c: TBGRAPixel; align: TAlignment); overload; virtual;
     procedure TextOut(x, y: single; sUTF8: string; texture: IBGRAScanner; align: TAlignment); overload; virtual;
     procedure TextOut(x, y: single; sUTF8: string; c: TBGRAPixel); overload; virtual;
     procedure TextOut(x, y: single; sUTF8: string; c: TBGRAPixel; ARightToLeft: boolean); overload; virtual;
     procedure TextOut(x, y: single; sUTF8: string; c: TColor); overload; virtual;
     procedure TextOut(x, y: single; sUTF8: string; c: TColor; ARightToLeft: boolean); overload; virtual;
     procedure TextOut(x, y: single; sUTF8: string; texture: IBGRAScanner); overload; virtual;
     procedure TextOut(x, y: single; sUTF8: string; texture: IBGRAScanner; ARightToLeft: boolean); overload; virtual;
     procedure TextOut(x, y: single; sUTF8: string; c: TBGRAPixel; align: TAlignment; ARightToLeft: boolean); overload; virtual; abstract;
     procedure TextOut(x, y: single; sUTF8: string; texture: IBGRAScanner; align: TAlignment; ARightToLeft: boolean); overload; virtual; abstract;

     { Overrides the font orientation with the parameter orientationTenthDegCCW }
     procedure TextOutAngle(x, y: single; orientationTenthDegCCW: integer; sUTF8: string; c: TBGRAPixel; align: TAlignment); overload; virtual;
     procedure TextOutAngle(x, y: single; orientationTenthDegCCW: integer; sUTF8: string; c: TBGRAPixel; align: TAlignment; ARightToLeft: boolean); overload; virtual; abstract;
     procedure TextOutAngle(x, y: single; orientationTenthDegCCW: integer; sUTF8: string; texture: IBGRAScanner; align: TAlignment); overload; virtual;
     procedure TextOutAngle(x, y: single; orientationTenthDegCCW: integer; sUTF8: string; texture: IBGRAScanner; align: TAlignment; ARightToLeft: boolean); overload; virtual; abstract;

     { Draw the UTF8 encoded string in the rectangle ARect. Text is wrapped if necessary.
       The position depends on the specified horizontal alignment halign and vertical alignement valign.
       The color c or texture is used to fill the text. No rotation is applied. }
     procedure TextRect(ARect: TRect; sUTF8: string; halign: TAlignment; valign: TTextLayout; c: TBGRAPixel); overload; virtual;
     procedure TextRect(ARect: TRect; sUTF8: string; halign: TAlignment; valign: TTextLayout; texture: IBGRAScanner); overload; virtual;

     //-------------------------- computing path ------------------------------------

     {Spline}
     function ComputeClosedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF; virtual; abstract;
     function ComputeOpenedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF; virtual; abstract;
     function ComputeBezierCurve(const curve: TCubicBezierCurve): ArrayOfTPointF; overload; virtual; abstract;
     function ComputeBezierCurve(const curve: TQuadraticBezierCurve): ArrayOfTPointF; overload; virtual; abstract;
     function ComputeBezierSpline(const spline: array of TCubicBezierCurve): ArrayOfTPointF; overload; virtual; abstract;
     function ComputeBezierSpline(const spline: array of TQuadraticBezierCurve): ArrayOfTPointF; overload; virtual; abstract;

     {can be accessed via Pen property}
     function ComputeWidePolyline(const points: array of TPointF; w: single): ArrayOfTPointF; overload; virtual; abstract;
     function ComputeWidePolyline(const points: array of TPointF; w: single; ClosedCap: boolean): ArrayOfTPointF; overload; virtual; abstract;
     function ComputeWidePolygon(const points: array of TPointF; w: single): ArrayOfTPointF; virtual; abstract;

     function ComputeEllipse(x,y,rx,ry: single): ArrayOfTPointF; overload; deprecated;
     function ComputeEllipse(x,y,rx,ry,w: single): ArrayOfTPointF; overload; deprecated;
     function ComputeEllipseContour(x,y,rx,ry: single; quality: single = 1): ArrayOfTPointF; overload; virtual; abstract;
     function ComputeEllipseContour(AOrigin, AXAxis, AYAxis: TPointF; quality: single = 1): ArrayOfTPointF; overload; virtual; abstract;
     function ComputeEllipseBorder(x,y,rx,ry,w: single; quality: single = 1): ArrayOfTPointF; overload; virtual; abstract;
     function ComputeEllipseBorder(AOrigin, AXAxis, AYAxis: TPointF; w: single; quality: single = 1): ArrayOfTPointF; overload; virtual; abstract;
     function ComputeArc65536(x,y,rx,ry: single; start65536,end65536: word; quality: single = 1): ArrayOfTPointF; virtual; abstract;
     function ComputeArcRad(x,y,rx,ry: single; startRad,endRad: single; quality: single = 1): ArrayOfTPointF; virtual; abstract;
     function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; quality: single = 1): ArrayOfTPointF; overload; virtual; abstract;
     function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; options: TRoundRectangleOptions; quality: single = 1): ArrayOfTPointF; overload; virtual; abstract;
     function ComputePie65536(x,y,rx,ry: single; start65536,end65536: word; quality: single = 1): ArrayOfTPointF; virtual; abstract;
     function ComputePieRad(x,y,rx,ry: single; startRad,endRad: single; quality: single = 1): ArrayOfTPointF; virtual; abstract;

     {Filling}

     // compatibility: take into account ClipRect
     procedure Fill(texture: IBGRAScanner); overload; virtual;
     procedure Fill(texture: IBGRAScanner; mode: TDrawMode); overload; override;

     procedure Fill(c: TBGRAPixel; start, Count: integer); overload; virtual; abstract;
     procedure DrawPixels(c: TBGRAPixel; start, Count: integer); virtual; abstract;
     procedure AlphaFill(alpha: byte; start, Count: integer); overload; virtual; abstract;
     procedure DrawCheckers(ARect: TRect; AColorEven,AColorOdd: TBGRAPixel); virtual; abstract;
     procedure ReplaceColor(before, after: TColor); overload; virtual; abstract;
     procedure ReplaceColor(ARect: TRect; before, after: TColor); overload; virtual; abstract;
     procedure FloodFill(X, Y: integer; Color: TBGRAPixel;
       mode: TFloodfillMode; Tolerance: byte = 0); virtual;
     procedure ParallelFloodFill(X, Y: integer; Dest: TBGRACustomBitmap; Color: TBGRAPixel;
       mode: TFloodfillMode; Tolerance: byte = 0); virtual; abstract;
     procedure GradientFill(x, y, x2, y2: integer; c1, c2: TBGRAPixel;
       gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
       gammaColorCorrection: boolean = True; Sinus: Boolean=False;
       ditherAlgo: TDitheringAlgorithm = daNearestNeighbor); overload; virtual; abstract;
     procedure GradientFill(x, y, x2, y2: integer; gradient: TBGRACustomGradient;
       gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
       Sinus: Boolean=False;
       ditherAlgo: TDitheringAlgorithm = daNearestNeighbor); overload; virtual; abstract;

     {Canvas drawing functions}
     procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
       AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); virtual; abstract;
     procedure DataDrawOpaque(ACanvas: TCanvas; ARect: TRect; AData: Pointer;
       ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); virtual; abstract;
     procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); virtual; abstract;
     procedure Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean = True); overload; virtual; abstract;
     procedure Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean = True); overload; virtual; abstract;
     procedure DrawPart(ARect: TRect; ACanvas: TCanvas; x, y: integer; Opaque: boolean); virtual;
     function GetPtrBitmap(Top,Bottom: Integer): TBGRACustomBitmap; virtual; abstract;
     function MakeBitmapCopy(BackgroundColor: TColor): TBitmap; virtual; abstract;

     {BGRA bitmap functions}
     procedure CrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadePosition: byte; mode: TDrawMode = dmDrawWithTransparency); overload; virtual; abstract;
     procedure CrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadeMask: IBGRAScanner; mode: TDrawMode = dmDrawWithTransparency); overload; virtual; abstract;
     procedure PutImage(x, y: integer; Source: TBitmap; mode: TDrawMode; AOpacity: byte = 255); overload;
     procedure StretchPutImage(ARect: TRect; Source: TBGRACustomBitmap; mode: TDrawMode; AOpacity: byte = 255); virtual; abstract;
     procedure StretchPutImageProportionally(ARect: TRect; AHorizAlign: TAlignment; AVertAlign: TTextLayout; Source: TBGRACustomBitmap; mode: TDrawMode; AOpacity: byte = 255);
     procedure PutImageSubpixel(x, y: single; Source: TBGRACustomBitmap; AOpacity: byte = 255);
     procedure PutImagePart(x,y: integer; Source: TBGRACustomBitmap; SourceRect: TRect; mode: TDrawMode; AOpacity: byte = 255);
     procedure PutImageAffine(Origin,HAxis,VAxis: TPointF; Source: TBGRACustomBitmap; AOpacity: Byte=255; ACorrectBlur: Boolean = false); overload;
     procedure PutImageAffine(Origin,HAxis,VAxis: TPointF; Source: TBGRACustomBitmap; AResampleFilter: TResampleFilter; AOpacity: Byte=255); overload;
     procedure PutImageAffine(Origin,HAxis,VAxis: TPointF; Source: TBGRACustomBitmap; AOutputBounds: TRect; AResampleFilter: TResampleFilter; AMode: TDrawMode; AOpacity: Byte=255); overload;
     procedure PutImageAffine(Origin,HAxis,VAxis: TPointF; Source: TBGRACustomBitmap; AOutputBounds: TRect; AOpacity: Byte=255; ACorrectBlur: Boolean = false); overload;
     procedure PutImageAffine(AMatrix: TAffineMatrix; Source: TBGRACustomBitmap; AOpacity: Byte=255; ACorrectBlur: Boolean = false; APixelCenteredCoords: boolean = true); overload;
     procedure PutImageAffine(AMatrix: TAffineMatrix; Source: TBGRACustomBitmap; AResampleFilter: TResampleFilter; AOpacity: Byte=255; APixelCenteredCoords: boolean = true); overload;
     procedure PutImageAffine(AMatrix: TAffineMatrix; Source: TBGRACustomBitmap; AResampleFilter: TResampleFilter; AMode: TDrawMode; AOpacity: Byte=255; APixelCenteredCoords: boolean = true); overload;
     procedure PutImageAffine(AMatrix: TAffineMatrix; Source: TBGRACustomBitmap; AOutputBounds: TRect; AResampleFilter: TResampleFilter; AMode: TDrawMode; AOpacity: Byte=255; APixelCenteredCoords: boolean = true); overload; virtual; abstract;
     procedure PutImageAffine(AMatrix: TAffineMatrix; Source: TBGRACustomBitmap; AOutputBounds: TRect; AOpacity: Byte=255; ACorrectBlur: Boolean = false; APixelCenteredCoords: boolean = true); overload;
     function GetImageAffineBounds(Origin,HAxis,VAxis: TPointF; Source: TBGRACustomBitmap): TRect; overload;
     function GetImageAffineBounds(Origin,HAxis,VAxis: TPointF; ASourceWidth, ASourceHeight: integer; const ASourceBounds: TRect; AClipOutput: boolean = true): TRect; overload;
     function GetImageAffineBounds(AMatrix: TAffineMatrix; Source: TBGRACustomBitmap; APixelCenteredCoords: boolean = true): TRect; overload;
     function GetImageAffineBounds(AMatrix: TAffineMatrix; ASourceBounds: TRect; AClipOutput: boolean = true; APixelCenteredCoords: boolean = true): TRect; overload; virtual; abstract;
     class function IsAffineRoughlyTranslation(AMatrix: TAffineMatrix; ASourceBounds: TRect): boolean; virtual; abstract;
     procedure PutImageAngle(x,y: single; Source: TBGRACustomBitmap; angle: single; AOutputBounds: TRect; imageCenterX: single = 0; imageCenterY: single = 0; AOpacity: Byte=255; ARestoreOffsetAfterRotation: boolean = false; ACorrectBlur: Boolean = false); overload;
     procedure PutImageAngle(x,y: single; Source: TBGRACustomBitmap; angle: single; imageCenterX: single = 0; imageCenterY: single = 0; AOpacity: Byte=255; ARestoreOffsetAfterRotation: boolean = false; ACorrectBlur: Boolean = false); overload;
     procedure PutImageAngle(x,y: single; Source: TBGRACustomBitmap; angle: single; AOutputBounds: TRect; AResampleFilter: TResampleFilter; imageCenterX: single = 0; imageCenterY: single = 0; AOpacity: Byte=255; ARestoreOffsetAfterRotation: boolean = false); overload;
     procedure PutImageAngle(x,y: single; Source: TBGRACustomBitmap; angle: single; AResampleFilter: TResampleFilter; imageCenterX: single = 0; imageCenterY: single = 0; AOpacity: Byte=255; ARestoreOffsetAfterRotation: boolean = false); overload;
     procedure ComputeImageAngleAxes(x,y,w,h,angle: single; imageCenterX,imageCenterY: single; ARestoreOffsetAfterRotation: boolean;
       out Origin,HAxis,VAxis: TPointF);
     function GetImageAngleBounds(x,y: single; Source: TBGRACustomBitmap; angle: single; imageCenterX: single = 0; imageCenterY: single = 0; ARestoreOffsetAfterRotation: boolean = false): TRect;
     procedure BlendImage(x, y: integer; Source: TBGRACustomBitmap; operation: TBlendOperation); virtual; abstract;
     procedure BlendImageOver(x, y: integer; Source: TBGRACustomBitmap; operation: TBlendOperation; AOpacity: byte = 255;
         ALinearBlend: boolean = false); virtual; abstract;
     function Resample(newWidth, newHeight: integer;
       mode: TResampleMode = rmFineResample): TBGRACustomBitmap; virtual; abstract;

     //masks
     procedure FillMask(x,y: integer; AMask: TCustomUniversalBitmap; color: TBGRAPixel); overload; virtual;
     procedure FillMask(x,y: integer; AMask: TCustomUniversalBitmap; texture: IBGRAScanner); overload; virtual;
     procedure FillMask(x,y: integer; AMask: TCustomUniversalBitmap; color: TBGRAPixel; ADrawMode: TDrawMode); overload; virtual; abstract;
     procedure FillMask(x,y: integer; AMask: TCustomUniversalBitmap; texture: IBGRAScanner; ADrawMode: TDrawMode; AOpacity: byte = 255); overload; virtual; abstract;
     procedure EraseMask(x,y: integer; AMask: TBGRACustomBitmap; alpha: byte=255); virtual; abstract;
     procedure FillClearTypeMask(x,y: integer; xThird: integer; AMask: TBGRACustomBitmap; color: TBGRAPixel; ARGBOrder: boolean = true); overload; virtual; abstract;
     procedure FillClearTypeMask(x,y: integer; xThird: integer; AMask: TBGRACustomBitmap; texture: IBGRAScanner; ARGBOrder: boolean = true); overload; virtual; abstract;
     function GetMaskFromAlpha: TBGRACustomBitmap; virtual; abstract;
     function GetImageBoundsWithin(const ARect: TRect; Channel: TChannel = cAlpha; ANothingValue: Byte = 0): TRect; overload; override;
     function GetImageBoundsWithin(const ARect: TRect; Channels: TChannels; ANothingValue: Byte = 0): TRect; overload; override;

     {inplace filters}
     procedure Negative; virtual; abstract;
     procedure NegativeRect(ABounds: TRect); virtual; abstract;
     procedure LinearNegative; virtual; abstract;
     procedure LinearNegativeRect(ABounds: TRect); virtual; abstract;
     procedure InplaceGrayscale(AGammaCorrection: boolean = true); overload; virtual; abstract;
     procedure InplaceGrayscale(ABounds: TRect; AGammaCorrection: boolean = true); overload; virtual; abstract;
     procedure InplaceNormalize(AEachChannel: boolean = True); overload; virtual; abstract;
     procedure InplaceNormalize(ABounds: TRect; AEachChannel: boolean = True); overload; virtual; abstract;
     procedure ConvertToLinearRGB; virtual; abstract;
     procedure ConvertFromLinearRGB; virtual; abstract;
     procedure SwapRedBlue; overload; virtual; abstract;
     procedure SwapRedBlue(ARect: TRect); overload; virtual; abstract;
     procedure GrayscaleToAlpha; virtual; abstract;
     procedure AlphaToGrayscale; virtual; abstract;
     procedure VerticalFlip(ARect: TRect); overload; override;
     procedure HorizontalFlip(ARect: TRect); overload; override;
     procedure RotateUDInplace(ARect: TRect); overload; override;

     {Filters}
     function RotateCW: TBGRACustomBitmap;
     function RotateCCW: TBGRACustomBitmap;
     function RotateUD: TBGRACustomBitmap;
     function FilterSmartZoom3(Option: TMedianOption): TBGRACustomBitmap; virtual; abstract;
     function FilterMedian(Option: TMedianOption): TBGRACustomBitmap; virtual; abstract;
     function FilterSmooth: TBGRACustomBitmap; virtual; abstract;
     function FilterSharpen(Amount: single = 1): TBGRACustomBitmap; overload; virtual; abstract;
     function FilterSharpen(ABounds: TRect; Amount: single = 1): TBGRACustomBitmap; overload; virtual; abstract;
     function FilterContour: TBGRACustomBitmap; virtual; abstract;
     function FilterPixelate(pixelSize: integer; useResample: boolean; filter: TResampleFilter = rfLinear): TBGRACustomBitmap; virtual; abstract;
     function FilterBlurRadial(radius: single; blurType: TRadialBlurType): TBGRACustomBitmap; overload;
     function FilterBlurRadial(ABounds: TRect; radius: single; blurType: TRadialBlurType): TBGRACustomBitmap; overload;
     function FilterBlurRadial(radiusX, radiusY: single; blurType: TRadialBlurType): TBGRACustomBitmap; overload;
     function FilterBlurRadial(ABounds: TRect; radiusX, radiusY: single; blurType: TRadialBlurType): TBGRACustomBitmap; overload;
     function FilterBlurMotion(distance: single; angle: single; oriented: boolean): TBGRACustomBitmap; overload;
     function FilterBlurMotion(ABounds: TRect; distance: single; angle: single; oriented: boolean): TBGRACustomBitmap; overload;
     function FilterCustomBlur(mask: TCustomUniversalBitmap): TBGRACustomBitmap; overload;
     function FilterCustomBlur(ABounds: TRect; mask: TCustomUniversalBitmap): TBGRACustomBitmap; overload;
     function FilterEmboss(angle: single; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGRACustomBitmap; overload; virtual; abstract;
     function FilterEmboss(angle: single; ABounds: TRect; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGRACustomBitmap; overload; virtual; abstract;
     function FilterEmbossHighlight(FillSelection: boolean): TBGRACustomBitmap; overload; virtual; abstract;
     function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel): TBGRACustomBitmap; overload; virtual; abstract;
     function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel; var Offset: TPoint): TBGRACustomBitmap; overload; virtual; abstract;
     function FilterGrayscale: TBGRACustomBitmap; overload; virtual; abstract;
     function FilterGrayscale(ABounds: TRect): TBGRACustomBitmap; overload; virtual; abstract;
     function FilterNormalize(eachChannel: boolean = True): TBGRACustomBitmap; overload; virtual; abstract;
     function FilterNormalize(ABounds: TRect; eachChannel: boolean = True): TBGRACustomBitmap; overload; virtual; abstract;
     function FilterRotate(origin: TPointF; angle: single; correctBlur: boolean = false): TBGRACustomBitmap; virtual; abstract;
     function FilterAffine(AMatrix: TAffineMatrix; correctBlur: boolean = false): TBGRACustomBitmap; virtual; abstract;
     function FilterSphere: TBGRACustomBitmap; virtual; abstract;
     function FilterTwirl(ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap; overload; virtual; abstract;
     function FilterTwirl(ABounds: TRect; ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap; overload; virtual; abstract;
     function FilterCylinder: TBGRACustomBitmap; virtual; abstract;
     function FilterPlane: TBGRACustomBitmap; virtual; abstract;

     //IBGRAScanner
     function ScanAtIntegerExpanded(X,Y: integer): TExpandedPixel; override;
     function ScanNextExpandedPixel: TExpandedPixel; override;
     function ScanAtExpanded(X,Y: Single): TExpandedPixel; override;
     function ProvidesScanline(ARect: TRect): boolean; override;
     function GetScanlineAt(X, Y: integer): PBGRAPixel; override;
     procedure ScanNextMaskChunk(var ACount: integer; out AMask: PByteMask; out AStride: integer); override;
     function ScanAtIntegerMask(X,Y: integer): TByteMask; override;
     function ScanAtMask(X,Y: Single): TByteMask; override;
  end;

type
  TBGRABitmapAny = class of TBGRACustomBitmap;  //used to create instances of the same type (see NewBitmap)

var
  BGRABitmapFactory : TBGRABitmapAny;
{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}
function InternalGetImageBoundsWithin(ASourceBitmap: TBGRACustomBitmap; ASourceTexture: IBGRAScanner;
  const ARect: TRect; Channels: TChannels; ANothingValue: Byte): TRect;
var
  minx, miny, maxx, maxy: integer;
  xb, xb2, yb: integer;
  p:      PDWord;
  colorMask, colorZeros: DWord;
  actualRect: TRect;
  pixelBuffer: TBGRAPixelBuffer;
begin
  pixelBuffer := nil;
  if ASourceBitmap <> nil then
  begin
    actualRect := EmptyRect;
    IntersectRect(actualRect,ARect,rect(0,0,ASourceBitmap.Width,ASourceBitmap.Height))
  end
  else if ASourceTexture <> nil then
  begin
    actualRect := ARect;
    AllocateBGRAPixelBuffer(pixelBuffer, ARect.Right-ARect.Left)
  end
  else
  begin
    result := EmptyRect;
    exit;
  end;
  maxx := actualRect.Left-1;
  maxy := actualRect.Top-1;
  minx := actualRect.Right;
  miny := actualRect.Bottom;
  colorMask := 0;
  colorZeros := 0;
  if cBlue in Channels then
  begin
    colorMask := colorMask or longword(BGRA(0,0,255,0));
    colorZeros:= colorZeros or longword(BGRA(0,0,ANothingValue,0));
  end;
  if cGreen in Channels then
  begin
    colorMask := colorMask or longword(BGRA(0,255,0,0));
    colorZeros:= colorZeros or longword(BGRA(0,ANothingValue,0,0));
  end;
  if cRed in Channels then
  begin
    colorMask := colorMask or longword(BGRA(255,0,0,0));
    colorZeros:= colorZeros or longword(BGRA(ANothingValue,0,0,0));
  end;
  if cAlpha in Channels then
  begin
    colorMask := colorMask or longword(BGRA(0,0,0,255));
    colorZeros:= colorZeros or longword(BGRA(0,0,0,ANothingValue));
  end;
  colorMask := NtoLE(colorMask);
  colorZeros := NtoLE(colorZeros);
  for yb := actualRect.Top to actualRect.Bottom-1 do
  begin
    if ASourceBitmap <> nil then
      p := PDWord(ASourceBitmap.ScanLine[yb])+actualRect.Left
    else
    begin
      p := @pixelBuffer[0];
      ASourceTexture.ScanMoveTo(actualRect.Left,actualRect.Top);
      ASourceTexture.ScanPutPixels(PBGRAPixel(p),ARect.Right-ARect.Left, dmSet);
    end;
    for xb := actualRect.Left to actualRect.Right - 1 do
    begin
      if (p^ and colorMask) <> colorZeros then
      begin
        if xb < minx then
          minx := xb;
        if yb < miny then
          miny := yb;
        if xb > maxx then
          maxx := xb;
        if yb > maxy then
          maxy := yb;

        inc(p, actualRect.Right-1-xb);
        for xb2 := actualRect.Right-1 downto xb+1 do
        begin
          if (p^ and colorMask) <> colorZeros then
          begin
            if xb2 > maxx then
              maxx := xb2;
            break;
          end;
          dec(p);
        end;
        break;
      end;
      Inc(p);
    end;
  end;
  if minx > maxx then
  begin
    Result.left   := 0;
    Result.top    := 0;
    Result.right  := 0;
    Result.bottom := 0;
  end
  else
  begin
    Result.left   := minx;
    Result.top    := miny;
    Result.right  := maxx + 1;
    Result.bottom := maxy + 1;
  end;
end;

{ TBGRACustomScanner }
{ The abstract class record the position so that a derived class
  need only to redefine ScanAt }

function TBGRACustomScanner.ScanAtInteger(X, Y: integer): TBGRAPixel;
begin
  result := ScanAt(X,Y);
end;

function TBGRACustomScanner.ScanAtIntegerExpanded(X, Y: integer
  ): TExpandedPixel;
begin
  result := GammaExpansion(ScanAtInteger(X,Y));
end;

procedure TBGRACustomScanner.ScanMoveTo(X, Y: Integer);
begin
  FCurX := X;
  FCurY := Y;
  FOwnCur := true;
end;

{ Call ScanAt to determine pixel value }
function TBGRACustomScanner.ScanNextPixel: TBGRAPixel;
begin
  result := ScanAt(FCurX,FCurY);
  Inc(FCurX);
end;

function TBGRACustomScanner.ScanNextExpandedPixel: TExpandedPixel;
begin
  result := GammaExpansion(ScanNextPixel);
end;

function TBGRACustomScanner.ScanAtExpanded(X, Y: Single): TExpandedPixel;
begin
  result := GammaExpansion(ScanAt(X,Y));
end;

{$hints off}
procedure TBGRACustomScanner.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
begin
  //do nothing
end;

procedure TBGRACustomScanner.ScanSkipPixels(ACount: integer);
begin
  if FOwnCur then inc(FCurX, ACount)
  else
  begin
    while ACount > 0 do
    begin
      ScanNextPixel;
      dec(ACount);
    end;
  end;
end;

{$hints on}

function TBGRACustomScanner.IsScanPutPixelsDefined: boolean;
begin
  result := false;
end;

function TBGRACustomScanner.GetTextureGL: IUnknown;
begin
  result := nil;
end;

function TBGRACustomScanner.GetImageBoundsWithin(const ARect: TRect;
  Channel: TChannel; ANothingValue: Byte): TRect;
begin
  result := InternalGetImageBoundsWithin(nil,self,ARect,[Channel],ANothingValue);
end;

function TBGRACustomScanner.GetImageBoundsWithin(const ARect: TRect;
  Channels: TChannels; ANothingValue: Byte): TRect;
begin
  result := InternalGetImageBoundsWithin(nil,self,ARect,Channels,ANothingValue);
end;

function TBGRACustomScanner.ProvidesScanline(ARect: TRect): boolean;
begin
  result := false;
end;

function TBGRACustomScanner.GetScanlineAt(X, Y: integer): PBGRAPixel;
begin
  result := nil;
end;

function TBGRACustomScanner.GetScanCustomColorspace: TColorspaceAny;
begin
  result := TExpandedPixelColorspace;
end;

procedure TBGRACustomScanner.ScanNextCustomChunk(var ACount: integer; out
  APixels: Pointer);
var
  i: Integer;
  p: PExpandedPixel;
begin
  if ACount > length(FChunk) then ACount := length(FChunk);
  p := @FChunk[low(FChunk)];
  APixels := p;
  for i := ACount-1 downto 0 do
  begin
    p^ := ScanNextExpandedPixel;
    inc(p);
  end;
end;

procedure TBGRACustomScanner.ScanNextMaskChunk(var ACount: integer; out AMask: PByteMask; out AStride: integer);
var
  cs: TColorspaceAny;
  pPixels: Pointer;
begin
  cs := GetScanCustomColorspace;
  if cs = TBGRAPixelColorspace then
  begin
    ScanNextCustomChunk(ACount, pPixels);
    AMask := @PBGRAPixel(pPixels)^.green;
    AStride := sizeof(TBGRAPixel);
  end else
  if cs = TByteMaskColorspace then
  begin
    ScanNextCustomChunk(ACount, pPixels);
    AMask := PByteMask(pPixels);
    AStride := sizeof(TByteMask);
  end else
    raise exception.Create('This scanner does not provide a mask.');
end;

function TBGRACustomScanner.ScanAtIntegerMask(X,Y: integer): TByteMask;
begin
  if GetScanCustomColorspace = TBGRAPixelColorspace then
    result := ScanAtInteger(X,Y).green
  else
    result := ScanAtMask(X,Y);
end;

function TBGRACustomScanner.ScanAtMask(X,Y: Single): TByteMask;
begin
  if GetScanCustomColorspace = TBGRAPixelColorspace then
    result := ScanAt(X,Y).green
  else
    raise exception.Create('This scanner does not provide a mask.');
end;

{ Interface gateway }
function TBGRACustomScanner.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := longint(E_NOINTERFACE);
end;

{ There is no automatic reference counting, but it is compulsory to define these functions }
function TBGRACustomScanner._AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function TBGRACustomScanner._Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

{********************** End of TBGRACustomScanner **************************}

{ TBGRACustomBitmap }

function TBGRACustomBitmap.GetFontAntialias: Boolean;
begin
  result := FontQuality <> fqSystem;
end;

procedure TBGRACustomBitmap.SetFontAntialias(const AValue: Boolean);
begin
  if AValue and not FontAntialias then
    FontQuality := fqFineAntialiasing
  else if not AValue and (FontQuality <> fqSystem) then
    FontQuality := fqSystem;
end;

procedure TBGRACustomBitmap.SetXorMask(AValue: TBGRACustomBitmap);
begin
  if FXorMask=AValue then Exit;
  if (AValue.Width <> Width) or (AValue.Height <> Height) then
    raise exception.Create('Dimension mismatch');
  DiscardXorMask;
  FXorMask:=AValue;
end;

procedure TBGRACustomBitmap.Init;
begin
  inherited Init;
  ScanMaskChannel:= cGreen;
end;

function TBGRACustomBitmap.GetFontRightToLeftFor(AText: string): boolean;
begin
  case FontBidiMode of
    fbmAuto: result := IsRightToLeftUTF8(AText);
    fbmRightToLeft: result := true;
  else
    {fbmLeftToRight}
    result := false;
  end;
end;

function TBGRACustomBitmap.InternalNew: TCustomUniversalBitmap;
begin
  Result:= BGRABitmapFactory.Create;
end;

procedure TBGRACustomBitmap.DiscardXorMask;
begin
  if Assigned(FXorMask) then
  begin
    if FXorMask is TBGRACustomBitmap then
    begin
      TBGRACustomBitmap(FXorMask).FreeReference;
      FXorMask := nil;
    end else
      FreeAndNil(FXorMask);
  end;
end;

procedure TBGRACustomBitmap.NeedXorMask;
begin
  if FXorMask = nil then
    FXorMask := BGRABitmapFactory.Create(Width,Height);
end;

function TBGRACustomBitmap.NewReference: TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(inherited NewReference);
end;

function TBGRACustomBitmap.GetUnique: TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(inherited GetUnique);
end;

function TBGRACustomBitmap.Duplicate(DuplicateProperties: Boolean): TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(inherited Duplicate(DuplicateProperties));
end;

function TBGRACustomBitmap.Duplicate(DuplicateProperties,
  DuplicateXorMask: Boolean): TBGRACustomBitmap;
begin
  result := Duplicate(DuplicateProperties);
  if DuplicateXorMask and Assigned(XorMask) then
    result.XorMask := FXorMask.Duplicate(True);
end;

procedure TBGRACustomBitmap.CopyPropertiesTo(ABitmap: TCustomUniversalBitmap);
var
  other: TBGRACustomBitmap;
begin
  inherited CopyPropertiesTo(ABitmap);
  if ABitmap is TBGRACustomBitmap then
  begin
    other := TBGRACustomBitmap(ABitmap);
    other.CanvasOpacity := CanvasOpacity;
    other.CanvasDrawModeFP := CanvasDrawModeFP;
    other.PenStyle := PenStyle;
    other.CustomPenStyle := CustomPenStyle;
    other.FontName := FontName;
    other.FontHeight := FontHeight;
    other.FontStyle := FontStyle;
    other.FontQuality := FontQuality;
    other.FontOrientation := FontOrientation;
    other.FontVerticalAnchor:= FontVerticalAnchor;
    other.FontBidiMode:= FontBidiMode;
    other.LineCap := LineCap;
    other.JoinStyle := JoinStyle;
    other.ResampleFilter := ResampleFilter;
    other.ScanInterpolationFilter:= ScanInterpolationFilter;
    other.HotSpot := HotSpot;
  end;
end;

function TBGRACustomBitmap.GetPart(ARect: TRect): TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(inherited GetPart(ARect));
end;

function TBGRACustomBitmap.CreateBrushTexture(ABrushStyle: TBrushStyle;
  APatternColor, ABackgroundColor: TBGRAPixel; AWidth: integer;
  AHeight: integer; APenWidth: single): TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(inherited CreateBrushTexture(ABrushStyle, APatternColor, ABackgroundColor, AWidth,AHeight,APenWidth));
end;

procedure TBGRACustomBitmap.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth <> Width) or (AHeight <> Height) then
  begin
    inherited SetSize(AWidth, AHeight);
    DiscardXorMask;
  end;
end;

procedure TBGRACustomBitmap.InternalArc(cx, cy, rx, ry: single;
  const StartPoint, EndPoint: TPointF; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; AOptions: TArcOptions;
  ADrawChord: boolean; ATexture: IBGRAScanner);
var angle1,angle2: single;
begin
  if (rx = 0) or (ry = 0) then exit;
  angle1 := arctan2(-(StartPoint.y-cy)/ry,(StartPoint.x-cx)/rx);
  angle2 := arctan2(-(EndPoint.y-cy)/ry,(EndPoint.x-cx)/rx);
  if angle1 = angle2 then angle2 := angle1+2*Pi;
  InternalArc(cx,cy,rx,ry, angle1,angle2,
              ABorderColor,w,AFillColor, AOptions, ADrawChord, ATexture);
end;

procedure TBGRACustomBitmap.InternalArcInRect(r: TRect; StartAngleRad,
  EndAngleRad: Single; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; AOptions: TArcOptions;
  ADrawChord: boolean; ATexture: IBGRAScanner);
var
  temp: LongInt;
begin
  if r.right = r.left then exit;
  if r.bottom = r.top then exit;
  if r.right < r.left then
  begin
    temp := r.left;
    r.left := r.right;
    r.right := temp;
  end;
  if r.Bottom < r.Top then
  begin
    temp := r.Top;
    r.Top := r.Bottom;
    r.Bottom := temp;
  end;
  InternalArc((r.left+r.right-1)/2,(r.top+r.bottom-1)/2,
             (r.right-r.left-1)/2,(r.bottom-r.top-1)/2,
             StartAngleRad,EndAngleRad,
             ABorderColor,w,AFillColor,
             AOptions, ADrawChord, ATexture);
end;

procedure TBGRACustomBitmap.InternalFillArcInRect(r: TRect; StartAngleRad,
  EndAngleRad: Single; AFillColor: TBGRAPixel; AOptions: TArcOptions;
  ATexture: IBGRAScanner);
var
  temp: LongInt;
begin
  if r.right = r.left then exit;
  if r.bottom = r.top then exit;
  if r.right < r.left then
  begin
    temp := r.left;
    r.left := r.right;
    r.right := temp;
  end;
  if r.Bottom < r.Top then
  begin
    temp := r.Top;
    r.Top := r.Bottom;
    r.Bottom := temp;
  end;
  InternalArc((r.left+r.right-1)/2,(r.top+r.bottom-1)/2,
             (r.right-r.left)/2,(r.bottom-r.top)/2,
             StartAngleRad,EndAngleRad,
             BGRAPixelTransparent,0,AFillColor,
             AOptions, False, ATexture);
end;

procedure TBGRACustomBitmap.DrawPixel(x, y: int32or64; const c: TBGRAPixel;
  ADrawMode: TDrawMode);
begin
  case ADrawMode of
  dmSet: SetPixel(x,y,c);
  dmSetExceptTransparent: if c.alpha = 255 then SetPixel(x,y,c);
  dmLinearBlend: FastBlendPixel(x,y,c);
  dmDrawWithTransparency: DrawPixel(x,y,c);
  dmXor: XorPixel(x,y,c);
  end;
end;

procedure TBGRACustomBitmap.LoadFromStream(AStream: TStream;
  AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions);
var
  OldDrawMode: TDrawMode;
begin
  { LoadFromStream uses TFPCustomImage routine, which uses
    Colors property to access pixels. That's why the
    FP drawing mode is temporarily changed to load
    bitmaps properly }
  OldDrawMode := CanvasDrawModeFP;
  CanvasDrawModeFP := dmSet;
  DiscardXorMask;
  try
    inherited LoadFromStream(AStream, AHandler, AOptions);
  finally
    CanvasDrawModeFP := OldDrawMode;
  end;
end;

{ Look for a pixel considering the bitmap is repeated in both directions }
procedure TBGRACustomBitmap.DrawHorizLine(x, y, x2: int32or64;
  texture: IBGRAScanner);
begin
  HorizLine(x,y,x2,texture,dmDrawWithTransparency);
end;

procedure TBGRACustomBitmap.Arc(cx, cy, rx, ry: single; const StartPoint,
  EndPoint: TPointF; AColor: TBGRAPixel; w: single; ADrawChord: boolean;
  AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,AColor,w,AFillColor,[aoFillPath],ADrawChord);
end;

procedure TBGRACustomBitmap.Arc(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; AColor: TBGRAPixel; w: single; ADrawChord: boolean;
  AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,AColor,w,AFillColor,[aoFillPath],ADrawChord);
end;

procedure TBGRACustomBitmap.FillChord(cx, cy, rx, ry: single; const StartPoint,
  EndPoint: TPointF; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,BGRAPixelTransparent,0,AFillColor,[aoFillPath]);
end;

procedure TBGRACustomBitmap.FillChord(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,BGRAPixelTransparent,0,AFillColor,[aoFillPath]);
end;

procedure TBGRACustomBitmap.FillChord(cx, cy, rx, ry: single; const StartPoint,
  EndPoint: TPointF; texture: IBGRAScanner);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,BGRAPixelTransparent,0,BGRAWhite,[aoFillPath],False,texture);
end;

procedure TBGRACustomBitmap.FillChord(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; texture: IBGRAScanner);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,BGRAPixelTransparent,0,BGRAWhite,[aoFillPath],False,texture);
end;

procedure TBGRACustomBitmap.FillChordInRect(const ARect: TRect; StartAngleRad,
  EndAngleRad: Single; AFillColor: TBGRAPixel);
begin
  InternalFillArcInRect(ARect,StartAngleRad,EndAngleRad,AFillColor,[aoFillPath]);
end;

procedure TBGRACustomBitmap.FillChordInRect(const ARect: TRect; StartAngleRad,
  EndAngleRad: Single; texture: IBGRAScanner);
begin
  InternalFillArcInRect(ARect,StartAngleRad,EndAngleRad,BGRAWhite,[aoFillPath],texture);
end;

procedure TBGRACustomBitmap.Pie(cx, cy, rx, ry: single; const StartPoint,
  EndPoint: TPointF; AColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,AColor,w,AFillColor,[aoFillPath,aoPie]);
end;

procedure TBGRACustomBitmap.Pie(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; AColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,AColor,w,AFillColor,[aoFillPath,aoPie]);
end;

procedure TBGRACustomBitmap.FillPie(cx, cy, rx, ry: single; const StartPoint,
  EndPoint: TPointF; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,BGRAPixelTransparent,0,AFillColor,[aoFillPath,aoPie]);
end;

procedure TBGRACustomBitmap.FillPie(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,BGRAPixelTransparent,0,AFillColor,[aoFillPath,aoPie]);
end;

procedure TBGRACustomBitmap.FillPie(cx, cy, rx, ry: single; const StartPoint,
  EndPoint: TPointF; texture: IBGRAScanner);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,BGRAPixelTransparent,0,BGRAWhite,[aoFillPath,aoPie],False,texture);
end;

procedure TBGRACustomBitmap.FillPie(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; texture: IBGRAScanner);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,BGRAPixelTransparent,0,BGRAWhite,[aoFillPath,aoPie],False,texture);
end;

procedure TBGRACustomBitmap.FillPieInRect(const ARect: TRect; StartAngleRad,
  EndAngleRad: Single; AFillColor: TBGRAPixel);
begin
  InternalFillArcInRect(ARect,StartAngleRad,EndAngleRad,AFillColor,[aoFillPath,aoPie]);
end;

procedure TBGRACustomBitmap.FillPieInRect(const ARect: TRect; StartAngleRad,
  EndAngleRad: Single; texture: IBGRAScanner);
begin
  InternalFillArcInRect(ARect,StartAngleRad,EndAngleRad,BGRAWhite,[aoFillPath,aoPie],texture);
end;

{ Following functions are defined for convenience }
procedure TBGRACustomBitmap.RectangleAntialias(x, y, x2, y2: single;
  c: TBGRAPixel; w: single);
begin
  RectangleAntialias(x, y, x2, y2, c, w, BGRAPixelTransparent);
end;

procedure TBGRACustomBitmap.RectangleWithin(x1, y1, x2, y2: single;
  ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel;
  APixelCenteredCoordinates: boolean);
begin
  if not APixelCenteredCoordinates then
  begin
    x1 -= 0.5;
    y1 -= 0.5;
    x2 -= 0.5;
    y2 -= 0.5;
  end;
  RectangleAntialias(x1+w*0.5,y1+w*0.5,x2-w*0.5,y2-w*0.5, ABorderColor, w, AFillColor);
end;

procedure TBGRACustomBitmap.RectangleWithin(r: TRect; ABorderColor: TBGRAPixel;
  w: single; AFillColor: TBGRAPixel);
begin
  RectangleWithin(r.left,r.top,r.right,r.bottom,ABorderColor,w,AFillColor,false);
end;

procedure TBGRACustomBitmap.FillRect(r: TRect; texture: IBGRAScanner;
  mode: TDrawMode; ditheringAlgorithm: TDitheringAlgorithm);
begin
  FillRect(r.Left,r.Top,r.Right,r.Bottom, texture, mode, ditheringAlgorithm);
end;

procedure TBGRACustomBitmap.FillRect(r: TRect; texture: IBGRAScanner;
  mode: TDrawMode; AScanOffset: TPoint; ditheringAlgorithm: TDitheringAlgorithm);
begin
  FillRect(r.Left,r.Top,r.Right,r.Bottom, texture, mode, AScanOffset, ditheringAlgorithm);
end;

procedure TBGRACustomBitmap.FillRect(x, y, x2, y2: integer;
  texture: IBGRAScanner; mode: TDrawMode;
  ditheringAlgorithm: TDitheringAlgorithm);
begin
  FillRect(x,y,x2,y2,texture,mode,Point(0,0),ditheringAlgorithm);
end;

procedure TBGRACustomBitmap.TextOutCurved(APath: IBGRAPath; sUTF8: string;
  AColor: TBGRAPixel; AAlign: TAlignment; ALetterSpacing: single);
var cursor: TBGRACustomPathCursor;
begin
  cursor := APath.getCursor;
  if cursor = nil then exit;
  case AAlign of
    taCenter: cursor.Position := cursor.PathLength*0.5;
    taRightJustify: cursor.Position:= cursor.PathLength;
  end;
  TextOutCurved(cursor, sUTF8, AColor, AAlign, ALetterSpacing);
  cursor.free;
end;

procedure TBGRACustomBitmap.TextOutCurved(APath: IBGRAPath; sUTF8: string;
  ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single);
var cursor: TBGRACustomPathCursor;
begin
  cursor := APath.getCursor;
  if cursor = nil then exit;
  case AAlign of
    taCenter: cursor.Position := cursor.PathLength*0.5;
    taRightJustify: cursor.Position:= cursor.PathLength;
  end;
  TextOutCurved(cursor, sUTF8, ATexture, AAlign, ALetterSpacing);
  cursor.free;
end;

procedure TBGRACustomBitmap.TextMultiline(x, y: single; sUTF8: string;
  c: TBGRAPixel; AAlign: TBidiTextAlignment; AVertAlign: TTextLayout; AParagraphSpacing: single);
begin
  TextMultiline(x, y, EmptySingle, sUTF8, c, AAlign, AVertAlign, AParagraphSpacing);
end;

procedure TBGRACustomBitmap.TextMultiline(x, y: single; sUTF8: string;
  ATexture: IBGRAScanner; AAlign: TBidiTextAlignment; AVertAlign: TTextLayout; AParagraphSpacing: single);
begin
  TextMultiline(x, y, EmptySingle, sUTF8, ATexture, AAlign, AVertAlign, AParagraphSpacing);
end;

procedure TBGRACustomBitmap.TextOut(x, y: single; sUTF8: string; c: TBGRAPixel;
  align: TAlignment);
begin
  TextOut(x,y,sUTF8,c,align, GetFontRightToLeftFor(sUTF8));
end;

procedure TBGRACustomBitmap.TextOut(x, y: single; sUTF8: string;
  texture: IBGRAScanner; align: TAlignment);
begin
  TextOut(x,y,sUTF8,texture,align, GetFontRightToLeftFor(sUTF8));
end;

{ Draw the UTF8 encoded string, (x,y) being the top-left corner. The color c is used to fill the text.
  The value of FontOrientation is taken into account, so that the text may be rotated. }
procedure TBGRACustomBitmap.TextOut(x, y: single; sUTF8: string; c: TBGRAPixel);
begin
  TextOut(x, y, sUTF8, c, taLeftJustify);
end;

procedure TBGRACustomBitmap.TextOut(x, y: single; sUTF8: string; c: TBGRAPixel;
  ARightToLeft: boolean);
begin
  TextOut(x, y, sUTF8, c, taLeftJustify, ARightToLeft);
end;

{ Draw the UTF8 encoded string, (x,y) being the top-left corner. The color c is used to fill the text.
  The value of FontOrientation is taken into account, so that the text may be rotated. }
procedure TBGRACustomBitmap.TextOut(x, y: single; sUTF8: string; c: TColor);
begin
  TextOut(x, y, sUTF8, ColorToBGRA(c));
end;

procedure TBGRACustomBitmap.TextOut(x, y: single; sUTF8: string; c: TColor;
  ARightToLeft: boolean);
begin
  TextOut(x, y, sUTF8, ColorToBGRA(c), ARightToLeft);
end;

{ Draw the UTF8 encoded string, (x,y) being the top-left corner. The texture is used to fill the text.
  The value of FontOrientation is taken into account, so that the text may be rotated. }
procedure TBGRACustomBitmap.TextOut(x, y: single; sUTF8: string;
  texture: IBGRAScanner);
begin
  TextOut(x, y, sUTF8, texture, taLeftJustify);
end;

procedure TBGRACustomBitmap.TextOut(x, y: single; sUTF8: string;
  texture: IBGRAScanner; ARightToLeft: boolean);
begin
  TextOut(x, y, sUTF8, texture, taLeftJustify, ARightToLeft);
end;

procedure TBGRACustomBitmap.TextOutAngle(x, y: single;
  orientationTenthDegCCW: integer; sUTF8: string; c: TBGRAPixel;
  align: TAlignment);
begin
  TextOutAngle(x,y, orientationTenthDegCCW, sUTF8,c,align, GetFontRightToLeftFor(sUTF8));
end;

procedure TBGRACustomBitmap.TextOutAngle(x, y: single;
  orientationTenthDegCCW: integer; sUTF8: string; texture: IBGRAScanner;
  align: TAlignment);
begin
  TextOutAngle(x,y, orientationTenthDegCCW, sUTF8,texture,align, GetFontRightToLeftFor(sUTF8));
end;

{ Draw the UTF8 encoded string in the rectangle ARect. Text is wrapped if necessary.
  The position depends on the specified horizontal alignment halign and vertical alignement valign.
  The color c is used to fill the text. No rotation is applied. }
procedure TBGRACustomBitmap.TextRect(ARect: TRect; sUTF8: string;
  halign: TAlignment; valign: TTextLayout; c: TBGRAPixel);
var
  style: TTextStyle;
begin
  {$hints off}
  FillChar(style,sizeof(style),0);
  {$hints on}
  style.Alignment := halign;
  style.Layout := valign;
  style.Wordbreak := true;
  style.ShowPrefix := false;
  style.Clipping := false;
  style.RightToLeft := GetFontRightToLeftFor(sUTF8);
  if FontBidiMode = fbmAuto then sUTF8 := AddParagraphBidiUTF8(sUTF8, style.RightToLeft);
  TextRect(ARect,ARect.Left,ARect.Top,sUTF8,style,c);
end;

{ Draw the UTF8 encoded string in the rectangle ARect. Text is wrapped if necessary.
  The position depends on the specified horizontal alignment halign and vertical alignement valign.
  The texture is used to fill the text. No rotation is applied. }
procedure TBGRACustomBitmap.TextRect(ARect: TRect; sUTF8: string;
  halign: TAlignment; valign: TTextLayout; texture: IBGRAScanner);
var
  style: TTextStyle;
begin
  {$hints off}
  FillChar(style,sizeof(style),0);
  {$hints on}
  style.Alignment := halign;
  style.Layout := valign;
  style.Wordbreak := true;
  style.ShowPrefix := false;
  style.Clipping := false;
  style.RightToLeft := GetFontRightToLeftFor(sUTF8);
  if FontBidiMode = fbmAuto then sUTF8 := AddParagraphBidiUTF8(sUTF8, style.RightToLeft);
  TextRect(ARect,ARect.Left,ARect.Top,sUTF8,style,texture);
end;

function TBGRACustomBitmap.ComputeEllipse(x, y, rx, ry: single): ArrayOfTPointF;
begin
  result := ComputeEllipseContour(x,y,rx,ry);
end;

function TBGRACustomBitmap.ComputeEllipse(x, y, rx, ry, w: single
  ): ArrayOfTPointF;
begin
  result := ComputeEllipseBorder(x,y,rx,ry,w);
end;

procedure TBGRACustomBitmap.Fill(texture: IBGRAScanner);
begin
  FillRect(ClipRect, texture, dmSet);
end;

procedure TBGRACustomBitmap.Fill(texture: IBGRAScanner; mode: TDrawMode);
begin
  FillRect(ClipRect, texture, mode);
end;

procedure TBGRACustomBitmap.FillMask(x, y: integer; AMask: TCustomUniversalBitmap;
  color: TBGRAPixel);
begin
  FillMask(x,y, AMask, color, dmDrawWithTransparency);
end;

procedure TBGRACustomBitmap.FillMask(x, y: integer; AMask: TCustomUniversalBitmap;
  texture: IBGRAScanner);
begin
  FillMask(x,y, AMask, texture, dmDrawWithTransparency);
end;

procedure TBGRACustomBitmap.FloodFill(X, Y: integer; Color: TBGRAPixel;
  mode: TFloodfillMode; Tolerance: byte);
begin
  ParallelFloodFill(X,Y,Self,Color,mode,Tolerance);
end;

procedure TBGRACustomBitmap.DrawPart(ARect: TRect; ACanvas: TCanvas; x,
  y: integer; Opaque: boolean);
var
  partial: TBGRACustomBitmap;
begin
  if (ARect.Left = 0) and (ARect.Top = 0) and (ARect.Right = Width) and (ARect.Bottom = Height) then
    Draw(ACanvas, x,y, Opaque)
  else
  begin
    partial := GetPart(ARect);
    if partial <> nil then
    begin
      partial.Draw(ACanvas, x, y, Opaque);
      partial.Free;
    end;
  end;
end;

procedure TBGRACustomBitmap.PutImage(x, y: integer; Source: TBitmap;
  mode: TDrawMode; AOpacity: byte);
var bgra: TBGRACustomBitmap;
begin
  bgra := BGRABitmapFactory.create(Source);
  PutImage(x,y, bgra, mode, AOpacity);
  bgra.free;
end;

procedure TBGRACustomBitmap.StretchPutImageProportionally(ARect: TRect;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; Source: TBGRACustomBitmap;
  mode: TDrawMode; AOpacity: byte);
var
  ratio: single;
  imgRect: TRect;
begin
  if (Source.Width = 0) or (Source.Height = 0) then exit;
  if (ARect.Width <= 0) or (ARect.Height <= 0) then exit;

  ratio := min(ARect.Width/Source.Width, ARect.Height/Source.Height);
  imgRect := RectWithSize(ARect.Left,ARect.Top, round(Source.Width*ratio), round(Source.Height*ratio));
  case AHorizAlign of
    taCenter: OffsetRect(imgRect, (ARect.Width-imgRect.Width) div 2, 0);
    taRightJustify: OffsetRect(imgRect, ARect.Width-imgRect.Width, 0);
  end;
  case AVertAlign of
    tlCenter: OffsetRect(imgRect, 0,(ARect.Height-imgRect.Height) div 2);
    tlBottom: OffsetRect(imgRect, 0,ARect.Height-imgRect.Height);
  end;
  StretchPutImage(imgRect, Source, mode, AOpacity);
end;

procedure TBGRACustomBitmap.PutImageSubpixel(x, y: single; Source: TBGRACustomBitmap; AOpacity: byte);
begin
  PutImageAngle(x,y,source,0,0,0,AOpacity);
end;

procedure TBGRACustomBitmap.PutImagePart(x, y: integer;
  Source: TBGRACustomBitmap; SourceRect: TRect; mode: TDrawMode; AOpacity: byte);
var w,h,sourcex,sourcey,nx,ny,xb,yb,destx,desty: integer;
    oldClip,newClip: TRect;
begin
  if (Source = nil) or (AOpacity = 0) then exit;
  w := SourceRect.Right-SourceRect.Left;
  h := SourceRect.Bottom-SourceRect.Top;
  if (w <= 0) or (h <= 0) or (Source.Width = 0) or (Source.Height = 0) then exit;
  sourcex := PositiveMod(SourceRect.Left, Source.Width);
  sourcey := PositiveMod(SourceRect.Top, Source.Height);
  nx := (sourceX+w + Source.Width-1) div Source.Width;
  ny := (sourceY+h + Source.Height-1) div Source.Height;

  oldClip := ClipRect;
  newClip := rect(x,y,x+w,y+h);
  if not IntersectRect(newClip,newClip,oldClip) then exit;

  ClipRect := newClip;

  desty := y-sourcey;
  for yb := 0 to ny-1 do
  begin
    destx := x-sourcex;
    for xb := 0 to nx-1 do
    begin
      self.PutImage(destx,desty,Source,mode,AOpacity);
      inc(destx,Source.Width);
    end;
    inc(desty,Source.Height);
  end;

  ClipRect := oldClip;
end;

procedure TBGRACustomBitmap.PutImageAffine(Origin, HAxis, VAxis: TPointF;
  Source: TBGRACustomBitmap; AOpacity: Byte; ACorrectBlur: Boolean);
begin
  if ACorrectBlur then
    PutImageAffine(Origin,HAxis,VAxis,Source,rfCosine,AOpacity)
  else
    PutImageAffine(Origin,HAxis,VAxis,Source,rfLinear,AOpacity);
end;

procedure TBGRACustomBitmap.PutImageAffine(Origin, HAxis, VAxis: TPointF;
  Source: TBGRACustomBitmap; AResampleFilter: TResampleFilter; AOpacity: Byte);
begin
  if (Source = nil) or (Source.Width = 0) or (Source.Height = 0) or (AOpacity = 0) then exit;
  PutImageAffine(Origin,HAxis,VAxis,Source,GetImageAffineBounds(Origin,HAxis,VAxis,Source),AResampleFilter,dmDrawWithTransparency,AOpacity);
end;

procedure TBGRACustomBitmap.PutImageAffine(Origin, HAxis, VAxis: TPointF;
  Source: TBGRACustomBitmap; AOutputBounds: TRect;
  AResampleFilter: TResampleFilter; AMode: TDrawMode; AOpacity: Byte);
var m: TAffineMatrix; w,h: integer;
begin
  if (Source = nil) or (Source.Width = 0) or (Source.Height = 0) or (AOpacity = 0) then exit;
  if Source.Width < 2 then w := 2 else w := Source.Width; //avoid actual size of zero
  if Source.Height < 2 then h := 2 else h := Source.Height;
  m[1,1] := (HAxis.x-Origin.x)/(w-1); m[1,2] := (VAxis.x-Origin.x)/(h-1); m[1,3] := Origin.x;
  m[2,1] := (HAxis.y-Origin.y)/(w-1); m[2,2] := (VAxis.y-Origin.y)/(h-1); m[2,3] := Origin.y;
  PutImageAffine(m,Source,AOutputBounds,AResampleFilter,AMode,AOpacity);
end;

procedure TBGRACustomBitmap.PutImageAffine(Origin, HAxis, VAxis: TPointF;
  Source: TBGRACustomBitmap; AOutputBounds: TRect; AOpacity: Byte;
  ACorrectBlur: Boolean);
begin
  if ACorrectBlur then
    PutImageAffine(Origin,HAxis,VAxis,Source,AOutputBounds,rfCosine,dmDrawWithTransparency,AOpacity)
  else
    PutImageAffine(Origin,HAxis,VAxis,Source,AOutputBounds,rfLinear,dmDrawWithTransparency,AOpacity);
end;

procedure TBGRACustomBitmap.PutImageAffine(AMatrix: TAffineMatrix;
  Source: TBGRACustomBitmap; AOpacity: Byte; ACorrectBlur: Boolean; APixelCenteredCoords: boolean);
begin
  if ACorrectBlur then
    PutImageAffine(AMatrix,Source,rfCosine,AOpacity,APixelCenteredCoords)
  else
    PutImageAffine(AMatrix,Source,rfLinear,AOpacity,APixelCenteredCoords);
end;

procedure TBGRACustomBitmap.PutImageAffine(AMatrix: TAffineMatrix;
  Source: TBGRACustomBitmap; AResampleFilter: TResampleFilter; AOpacity: Byte; APixelCenteredCoords: boolean);
begin
  PutImageAffine(AMatrix, Source, AResampleFilter, dmDrawWithTransparency, AOpacity, APixelCenteredCoords);
end;

procedure TBGRACustomBitmap.PutImageAffine(AMatrix: TAffineMatrix;
  Source: TBGRACustomBitmap; AResampleFilter: TResampleFilter;
  AMode: TDrawMode; AOpacity: Byte; APixelCenteredCoords: boolean);
begin
  if (Source = nil) or (Source.Width = 0) or (Source.Height = 0) or (AOpacity = 0) then exit;
  PutImageAffine(AMatrix, Source, GetImageAffineBounds(AMatrix,Source),
                 AResampleFilter,AMode,AOpacity,APixelCenteredCoords);
end;

procedure TBGRACustomBitmap.PutImageAffine(AMatrix: TAffineMatrix;
  Source: TBGRACustomBitmap; AOutputBounds: TRect; AOpacity: Byte;
  ACorrectBlur: Boolean; APixelCenteredCoords: boolean);
begin
  if ACorrectBlur then
    PutImageAffine(AMatrix,Source,AOutputBounds,rfCosine,dmDrawWithTransparency,AOpacity,APixelCenteredCoords)
  else
    PutImageAffine(AMatrix,Source,AOutputBounds,rfLinear,dmDrawWithTransparency,AOpacity,APixelCenteredCoords);
end;

{ Returns the area that contains the affine transformed image }
function TBGRACustomBitmap.GetImageAffineBounds(Origin, HAxis, VAxis: TPointF;
  Source: TBGRACustomBitmap): TRect;
begin
  if Source = nil then
    result := EmptyRect
  else
    result := GetImageAffineBounds(Origin,HAxis,VAxis,Source.Width,Source.Height,Source.GetImageBounds);
end;

function TBGRACustomBitmap.GetImageAffineBounds(Origin, HAxis, VAxis: TPointF;
  ASourceWidth, ASourceHeight: integer; const ASourceBounds: TRect; AClipOutput: boolean): TRect;
var m: TAffineMatrix;
begin
  if (ASourceWidth = 0) or (ASourceHeight = 0) then
    result := EmptyRect
  else
  begin
    if ASourceWidth < 2 then ASourceWidth := 2;   //avoid division by zero by supposing a pixel size of 2
    if ASourceHeight < 2 then ASourceHeight := 2; //i.e. an actual size of 1 (cf pixel centered coordinates)
    m[1,1] := (HAxis.x-Origin.x)/(ASourceWidth-1); m[1,2] := (VAxis.x-Origin.x)/(ASourceHeight-1); m[1,3] := Origin.x;
    m[2,1] := (HAxis.y-Origin.y)/(ASourceWidth-1); m[2,2] := (VAxis.y-Origin.y)/(ASourceHeight-1); m[2,3] := Origin.y;
    result := GetImageAffineBounds(m, ASourceBounds, AClipOutput);
  end;
end;

function TBGRACustomBitmap.GetImageAffineBounds(AMatrix: TAffineMatrix;
  Source: TBGRACustomBitmap; APixelCenteredCoords: boolean): TRect;
begin
  result := GetImageAffineBounds(AMatrix, Source.GetImageBounds, APixelCenteredCoords);
end;

procedure TBGRACustomBitmap.PutImageAngle(x, y: single;
  Source: TBGRACustomBitmap; angle: single; AOutputBounds: TRect;
  imageCenterX: single; imageCenterY: single; AOpacity: Byte;
  ARestoreOffsetAfterRotation: boolean; ACorrectBlur: Boolean);
begin
  if ACorrectBlur then
    PutImageAngle(x,y,Source,angle,AOutputBounds,rfCosine,imageCenterX,imageCenterY,AOpacity,ARestoreOffsetAfterRotation)
  else
    PutImageAngle(x,y,Source,angle,AOutputBounds,rfLinear,imageCenterX,imageCenterY,AOpacity,ARestoreOffsetAfterRotation);
end;

procedure TBGRACustomBitmap.PutImageAngle(x, y: single;
  Source: TBGRACustomBitmap; angle: single; imageCenterX: single;
  imageCenterY: single; AOpacity: Byte; ARestoreOffsetAfterRotation: boolean; ACorrectBlur: Boolean);
begin
  if ACorrectBlur then
    PutImageAngle(x,y,Source,angle,rfCosine,imageCenterX,imageCenterY,AOpacity,ARestoreOffsetAfterRotation)
  else
    PutImageAngle(x,y,Source,angle,rfLinear,imageCenterX,imageCenterY,AOpacity,ARestoreOffsetAfterRotation);
end;

procedure TBGRACustomBitmap.PutImageAngle(x, y: single;
  Source: TBGRACustomBitmap; angle: single; AOutputBounds: TRect;
  AResampleFilter: TResampleFilter; imageCenterX: single; imageCenterY: single; AOpacity: Byte;
  ARestoreOffsetAfterRotation: boolean);
var
  Origin,HAxis,VAxis: TPointF;
begin
  if (source = nil) or (AOpacity=0) then exit;
  ComputeImageAngleAxes(x,y,source.Width,source.Height,angle,imageCenterX,imageCenterY,ARestoreOffsetAfterRotation,
     Origin,HAxis,VAxis);
  PutImageAffine(Origin,HAxis,VAxis,source,AOutputBounds,AResampleFilter,dmDrawWithTransparency,AOpacity);
end;

procedure TBGRACustomBitmap.PutImageAngle(x, y: single;
  Source: TBGRACustomBitmap; angle: single; AResampleFilter: TResampleFilter;
  imageCenterX: single; imageCenterY: single; AOpacity: Byte;
  ARestoreOffsetAfterRotation: boolean);
var
  Origin,HAxis,VAxis: TPointF;
begin
  if (source = nil) or (AOpacity=0) then exit;
  ComputeImageAngleAxes(x,y,source.Width,source.Height,angle,imageCenterX,imageCenterY,ARestoreOffsetAfterRotation,
     Origin,HAxis,VAxis);
  PutImageAffine(Origin,HAxis,VAxis,source,AResampleFilter,AOpacity);
end;

procedure TBGRACustomBitmap.ComputeImageAngleAxes(x, y, w, h,
  angle: single; imageCenterX, imageCenterY: single;
  ARestoreOffsetAfterRotation: boolean; out Origin, HAxis, VAxis: TPointF);
var
  cosa,sina: single;

  { Compute rotated coordinates }
  function Coord(relX,relY: single): TPointF;
  begin
    relX -= imageCenterX;
    relY -= imageCenterY;
    result.x := relX*cosa-relY*sina+x;
    result.y := relY*cosa+relX*sina+y;
    if ARestoreOffsetAfterRotation then
      result.Offset(imageCenterX,imageCenterY);
  end;

begin
  cosa := cos(-angle*Pi/180);
  sina := -sin(-angle*Pi/180);
  Origin := Coord(0,0);
  if w < 2 then w := 2; //when pixel size is 1, actual size is zero, so avoid that
  if h < 2 then h := 2;
  HAxis := Coord(w-1,0);
  VAxis := Coord(0,h-1);
end;

function TBGRACustomBitmap.GetImageAngleBounds(x, y: single;
  Source: TBGRACustomBitmap; angle: single; imageCenterX: single;
  imageCenterY: single; ARestoreOffsetAfterRotation: boolean): TRect;
var
  cosa,sina: single;

  { Compute rotated coordinates }
  function Coord(relX,relY: single): TPointF;
  begin
    relX -= imageCenterX;
    relY -= imageCenterY;
    result.x := relX*cosa-relY*sina+x;
    result.y := relY*cosa+relX*sina+y;
    if ARestoreOffsetAfterRotation then
      result.Offset(imageCenterX,imageCenterY);
  end;

begin
  if (source = nil) then
  begin
    result := EmptyRect;
    exit;
  end;
  cosa := cos(-angle*Pi/180);
  sina := -sin(-angle*Pi/180);
  result := GetImageAffineBounds(Coord(0,0),Coord(source.Width,0),Coord(0,source.Height),source);
end;

procedure TBGRACustomBitmap.VerticalFlip(ARect: TRect);
begin
  inherited VerticalFlip(ARect);
  if Assigned(XorMask) then XorMask.VerticalFlip(ARect);
end;

procedure TBGRACustomBitmap.HorizontalFlip(ARect: TRect);
begin
  inherited HorizontalFlip(ARect);
  if Assigned(XorMask) then XorMask.HorizontalFlip(ARect);
end;

procedure TBGRACustomBitmap.RotateUDInplace(ARect: TRect);
begin
  inherited RotateUDInplace(ARect);
  if Assigned(XorMask) then XorMask.RotateUDInplace(ARect);
end;

function TBGRACustomBitmap.RotateCW: TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(Inherited RotateCW);
  if Assigned(XorMask) then result.FXorMask := self.XorMask.RotateCW;
end;

function TBGRACustomBitmap.RotateCCW: TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(Inherited RotateCCW);
  if Assigned(XorMask) then result.FXorMask := self.XorMask.RotateCCW;
end;

function TBGRACustomBitmap.RotateUD: TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(Inherited RotateUD);
  if Assigned(XorMask) then result.FXorMask := self.XorMask.RotateUD;
end;

function TBGRACustomBitmap.FilterBlurRadial(radius: single;
  blurType: TRadialBlurType): TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(inherited FilterBlurRadial(radius, blurType));
end;

function TBGRACustomBitmap.FilterBlurRadial(ABounds: TRect; radius: single;
  blurType: TRadialBlurType): TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(inherited FilterBlurRadial(ABounds, radius, blurType));
end;

function TBGRACustomBitmap.FilterBlurRadial(radiusX, radiusY: single;
  blurType: TRadialBlurType): TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(inherited FilterBlurRadial(radiusX,radiusY, blurType));
end;

function TBGRACustomBitmap.FilterBlurRadial(ABounds: TRect; radiusX,
  radiusY: single; blurType: TRadialBlurType): TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(inherited FilterBlurRadial(ABounds, radiusX,radiusY, blurType));
end;

function TBGRACustomBitmap.FilterBlurMotion(distance: single; angle: single;
  oriented: boolean): TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(inherited FilterBlurMotion(distance, angle, oriented));
end;

function TBGRACustomBitmap.FilterBlurMotion(ABounds: TRect; distance: single;
  angle: single; oriented: boolean): TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(inherited FilterBlurMotion(ABounds, distance, angle, oriented));
end;

function TBGRACustomBitmap.FilterCustomBlur(mask: TCustomUniversalBitmap
  ): TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(inherited FilterCustomBlur(mask));
end;

function TBGRACustomBitmap.FilterCustomBlur(ABounds: TRect;
  mask: TCustomUniversalBitmap): TBGRACustomBitmap;
begin
  result := TBGRACustomBitmap(inherited FilterCustomBlur(ABounds,mask));
end;

function TBGRACustomBitmap.GetImageBoundsWithin(const ARect: TRect;
  Channel: TChannel; ANothingValue: Byte): TRect;
begin
  result := InternalGetImageBoundsWithin(self,nil,ARect,[Channel],ANothingValue);
end;

function TBGRACustomBitmap.GetImageBoundsWithin(const ARect: TRect;
  Channels: TChannels; ANothingValue: Byte): TRect;
begin
  result := InternalGetImageBoundsWithin(self,nil,ARect,Channels,ANothingValue);
end;

function TBGRACustomBitmap.ScanAtIntegerExpanded(X, Y: integer): TExpandedPixel;
begin
  result := GammaExpansion(ScanAtInteger(X,Y));
end;

function TBGRACustomBitmap.ScanNextExpandedPixel: TExpandedPixel;
begin
  result := GammaExpansion(ScanNextPixel);
end;

function TBGRACustomBitmap.ScanAtExpanded(X, Y: Single): TExpandedPixel;
begin
  result := GammaExpansion(ScanAt(X,Y));
end;

function TBGRACustomBitmap.ProvidesScanline(ARect: TRect): boolean;
begin
  result := (ARect.Left+ScanOffset.x >= 0) and (ARect.Top+ScanOffset.y >= 0) and
      (ARect.Right+ScanOffset.x <= Width) and (ARect.Bottom+ScanOffset.y <= Height);
end;

function TBGRACustomBitmap.GetScanlineAt(X, Y: integer): PBGRAPixel;
begin
  result := ScanLine[y+ScanOffset.y]+x+ScanOffset.x;
end;

procedure TBGRACustomBitmap.ScanNextMaskChunk(var ACount: integer; out AMask: PByteMask; out AStride: integer);
var
  PPixels: Pointer;
begin
  ScanNextCustomChunk(ACount, PPixels);
  AMask := (PByteMask(PPixels)+TBGRAPixel_ChannelByteOffset[ScanMaskChannel]);
  AStride := sizeof(TBGRAPixel);
end;

function TBGRACustomBitmap.ScanAtIntegerMask(X,Y: integer): TByteMask;
var
  c: TBGRAPixel;
begin
  c := ScanAtInteger(X,Y);
  result := (PByte(@c)+TBGRAPixel_ChannelByteOffset[ScanMaskChannel])^;
end;

function TBGRACustomBitmap.ScanAtMask(X,Y: Single): TByteMask;
var
  c: TBGRAPixel;
begin
  c := ScanAt(X,Y);
  result := (PByte(@c)+TBGRAPixel_ChannelByteOffset[ScanMaskChannel])^;
end;

{$ENDIF}
