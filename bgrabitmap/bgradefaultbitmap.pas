// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
 /**************************************************************************\
                             bgradefaultbitmap.pas
                             ---------------------
                 This unit defines basic operations on bitmaps.
                 It should NOT be added to the 'uses' clause.
                 Some operations may be slow, so there are
                 accelerated versions for some routines.
}

unit BGRADefaultBitmap;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

{ This unit contains TBGRADefaultBitmap class. This class contains basic drawing routines,
  and call functions from other units to perform advanced drawing functions. }

uses
  SysUtils, BGRAClasses, FPImage, BGRAGraphics, BGRABitmapTypes,
  {$IFDEF BGRABITMAP_USE_FPCANVAS}FPImgCanv,{$ENDIF}
  BGRACanvas, BGRACanvas2D, BGRATransform, BGRATextBidi,
  UniversalDrawer, BGRAGrayscaleMask;

type
  TBGRAPtrBitmap = class;
  {=== TBGRABitmap reference ===}
  { TBGRADefaultBitmap }
  {* This class is the base for all ''TBGRABitmap'' classes. It implements most
     function to the exception from implementations specific to the
     widgetset }{ in the doc, it is presented as
  TBGRABitmap = class(TBGRACustomBitmap)
  }
  TBGRADefaultBitmap = class(TBGRACustomBitmap)
  private
    { Bounds checking which are shared by drawing functions. These functions check
      if the coordinates are visible and return true if it is the case, swap
      coordinates if necessary and make them fit into the clipping rectangle }
    function CheckRectBounds(var x,y,x2,y2: integer; minsize: integer): boolean; inline;
    function CheckAntialiasRectBounds(var x,y,x2,y2: single; w: single): boolean;
    function GetCanvasBGRA: TBGRACanvas;
    function GetCanvas2D: TBGRACanvas2D;
    procedure GradientFillDithered(x, y, x2, y2: integer; c1, c2: TBGRAPixel;
      gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
      gammaColorCorrection: boolean = True; Sinus: Boolean=False;
      ditherAlgo: TDitheringAlgorithm = daFloydSteinberg); overload;
    procedure GradientFillDithered(x, y, x2, y2: integer; gradient: TBGRACustomGradient;
      gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
      Sinus: Boolean=False;
      ditherAlgo: TDitheringAlgorithm = daFloydSteinberg); overload;

  protected
    //Pixel data
    FDataModified: boolean;              //if data image has changed so TBitmap should be updated

    //GUI bitmap object
    FBitmap:   TBitmap;
    FBitmapModified: boolean;         //if TBitmap has changed so pixel data should be updated
    FCanvasOpacity: byte;             //opacity used with standard canvas functions
    FAlphaCorrectionNeeded: boolean;  //the alpha channel is not correct because standard functions do not
                                      //take it into account

    //FreePascal drawing routines
    {$IFDEF BGRABITMAP_USE_FPCANVAS}FCanvasFP: TFPImageCanvas;{$ENDIF}
    FCanvasDrawModeFP: TDrawMode;
    FCanvasPixelProcFP: procedure(x, y: int32or64; const col: TBGRAPixel) of object;

    //canvas-like with antialiasing and texturing
    FCanvasBGRA: TBGRACanvas;
    FCanvas2D: TBGRACanvas2D;

    //drawing options
    FFontHeight: integer;
    FFontRenderer: TBGRACustomFontRenderer;

    //Pixel data
    function LoadFromRawImage(ARawImage: TRawImage; DefaultOpacity: byte;
      AlwaysReplaceAlpha: boolean = False; RaiseErrorOnInvalidPixelFormat: boolean = True): boolean; virtual; abstract;

    //FreePascal drawing routines
    {$IFDEF BGRABITMAP_USE_FPCANVAS}function GetCanvasFP: TFPImageCanvas; override;{$ENDIF}
    procedure SetCanvasDrawModeFP(const AValue: TDrawMode); override;
    function GetCanvasDrawModeFP: TDrawMode; override;

    //GUI bitmap object
    function GetBitmap: TBitmap; override;
    function GetCanvas: TCanvas; override;
    function GetCanvasOpacity: byte; override;
    procedure SetCanvasOpacity(AValue: byte); override;
    function GetCanvasAlphaCorrection: boolean; override;
    procedure SetCanvasAlphaCorrection(const AValue: boolean); override;
    procedure DoAlphaCorrection;
    procedure DiscardBitmapChange; inline;
    procedure DoLoadFromBitmap; virtual;

    function CreatePtrBitmap(AWidth,AHeight: integer; AData: PBGRAPixel): TBGRAPtrBitmap; virtual;

    procedure RebuildBitmap; virtual; abstract;
    procedure FreeBitmap; virtual;

    procedure Init; override;

    {TFPCustomImage}
    procedure SetInternalColor(x, y: integer; const Value: TFPColor); override;
    function GetInternalColor(x, y: integer): TFPColor; override;
    procedure SetInternalPixel(x, y: integer; Value: integer); override;
    function GetInternalPixel(x, y: integer): integer; override;

    {Image functions}
    function FineResample(NewWidth, NewHeight: integer): TBGRACustomBitmap;
    function SimpleStretch(NewWidth, NewHeight: integer): TBGRACustomBitmap;
    function CheckEmpty: boolean; override;
    function GetHasTransparentPixels: boolean; override;
    function GetHasSemiTransparentPixels: boolean; override;
    function GetAverageColor: TColor; override;
    function GetAveragePixel: TBGRAPixel; override;

  protected //pen style accesors
    function GetPenJoinStyle: TPenJoinStyle; override;
    procedure SetPenJoinStyle(const AValue: TPenJoinStyle); override;
    function GetPenMiterLimit: single; override;
    procedure SetPenMiterLimit(const AValue: single); override;
    function GetCustomPenStyle: TBGRAPenStyle; override;
    procedure SetCustomPenStyle(const AValue: TBGRAPenStyle); override;
    procedure SetPenStyle(const AValue: TPenStyle); override;
    function GetPenStyle: TPenStyle; override;

    function GetArrowEndSize: TPointF; override;
    function GetArrowStartSize: TPointF; override;
    procedure SetArrowEndSize(AValue: TPointF); override;
    procedure SetArrowStartSize(AValue: TPointF); override;
    function GetArrowEndOffset: single; override;
    function GetArrowStartOffset: single; override;
    procedure SetArrowEndOffset(AValue: single); override;
    procedure SetArrowStartOffset(AValue: single); override;
    function GetArrowEndRepeat: integer; override;
    function GetArrowStartRepeat: integer; override;
    procedure SetArrowEndRepeat(AValue: integer); override;
    procedure SetArrowStartRepeat(AValue: integer); override;

  protected //font accessors
    function GetFontHeight: integer; override;
    procedure SetFontHeight(AHeight: integer); override;
    function GetFontFullHeight: integer; override;
    procedure SetFontFullHeight(AHeight: integer); override;
    function GetFontPixelMetric: TFontPixelMetric; override;
    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; virtual; abstract;
    function GetFontVerticalAnchorOffset: single; override;
    function GetFontAnchorRotatedOffset: TPointF; overload;
    function GetFontAnchorRotatedOffset(ACustomOrientation: integer): TPointF; overload;
    function GetFontRenderer: TBGRACustomFontRenderer; override;
    procedure SetFontRenderer(AValue: TBGRACustomFontRenderer); override;

    function InternalGetPixelCycle256(ix,iy: int32or64; iFactX,iFactY: int32or64): TBGRAPixel;
    function InternalGetPixel256(ix,iy: int32or64; iFactX,iFactY: int32or64; smoothBorder: boolean): TBGRAPixel;
    procedure InternalTextOutCurved(ACursor: TBGRACustomPathCursor; sUTF8: string; AColor: TBGRAPixel; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single);
    procedure InternalTextOutLetterSpacing(x,y: single; sUTF8: string; AColor: TBGRAPixel; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single);
    procedure InternalCrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadePos: byte; AFadeMask: IBGRAScanner; mode: TDrawMode = dmDrawWithTransparency);

    procedure InternalArc(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; ABorderColor: TBGRAPixel; w: single;
      AFillColor: TBGRAPixel; AOptions: TArcOptions; ADrawChord: boolean = false; ATexture: IBGRAScanner = nil); override;
    function InternalNew: TBGRADefaultBitmap; override;

  public
    {** Provides a canvas with opacity and antialiasing }
    property CanvasBGRA: TBGRACanvas read GetCanvasBGRA;
    {** Provides a canvas with 2d transformation and similar to HTML5. }
    property Canvas2D: TBGRACanvas2D read GetCanvas2D;
    {** For more properties, see parent class [[TBGRACustomBitmap and IBGRAScanner#TBGRACustomBitmap|TBGRACustomBitmap]] }

    procedure SetSize(AWidth, AHeight: integer); override;

    {==== Constructors ====}

    {------------------------- Constructors from TBGRACustomBitmap-------------}

    {** Creates an image by copying the content of a ''TFPCustomImage'' }
    constructor Create(AFPImage: TFPCustomImage); overload; override;
    {** Creates an image by copying the content of a ''TBitmap'' }
    constructor Create(ABitmap: TBitmap; AUseTransparent: boolean = true); overload; override;

    {** Creates an image by loading its content from the file ''AFilename''.
        The encoding of the string is the default one for the operating system.
        It is recommended to use the next constructor and UTF8 encoding }
    constructor Create(AFilename: string); overload; override;

    {** Creates an image by loading its content from the file ''AFilename''.
        The boolean ''AIsUtf8Filename'' specifies if UTF8 encoding is assumed
        for the filename }
    constructor Create(AFilename: string; AIsUtf8: boolean); overload; override;
    constructor Create(AFilename: string; AIsUtf8: boolean; AOptions: TBGRALoadingOptions); overload; override;

    {** Creates an image by loading its content from the stream ''AStream'' }
    constructor Create(AStream: TStream); overload; override;
    {** Free the object and all its resources }
    destructor Destroy; override;

    {** Clear all channels of transparent pixels }
    procedure ClearTransparentPixels; override;

    {------------------------- Quasi-constructors -----------------------------}

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates a new instance with dimensions 0 x 0. }
    function NewBitmap: TBGRADefaultBitmap; overload; override;

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates a new instance with dimensions ''AWidth'' and ''AHeight'',
        containing transparent pixels. }
    function NewBitmap(AWidth, AHeight: integer): TBGRADefaultBitmap; overload; override;

    {* Example:
       <syntaxhighlight>
     * var bmp1, bmp2: TBGRABitmap;
     * begin
     *   bmp1 := TBGRABitmap.Create(100,100);
     *   bmp2 := bmp1.NewBitmap(100,100);
     *   ...
     * end;</syntaxhighlight>
       See tutorial 2 on [[BGRABitmap_tutorial_2|how to load and display an image]].
     * See reference on [[TBGRACustomBitmap_and_IBGRAScanner#Load_and_save_files|loading and saving files]] }

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates a new instance with dimensions ''AWidth'' and ''AHeight'',
        and fills it with Color }
    function NewBitmap(AWidth, AHeight: integer; const Color: TBGRAPixel): TBGRADefaultBitmap; overload; override;
    function NewBitmap(AWidth, AHeight: integer; AColor: Pointer): TBGRADefaultBitmap; overload; override;

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates a new instance with by loading its content
        from the file ''Filename''. The encoding of the string
        is the default one for the operating system }
    function NewBitmap(Filename: string): TBGRADefaultBitmap; overload; override;

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates a new instance with by loading its content
        from the file ''Filename'' }
    function NewBitmap(Filename: string; AIsUtf8: boolean): TBGRADefaultBitmap; overload; override;
    function NewBitmap(Filename: string; AIsUtf8: boolean; AOptions: TBGRALoadingOptions): TBGRADefaultBitmap; overload; override;

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates an image by copying the content of a ''TFPCustomImage'' }
    function NewBitmap(AFPImage: TFPCustomImage): TBGRADefaultBitmap; overload; override;

    {** Assign the content of the specified ''Source''. It can be a ''TBGRACustomBitmap'' or
        a ''TFPCustomImage'' }
    procedure Assign(Source: TPersistent); overload; override;
    procedure Assign(Source: TBitmap; AUseTransparent: boolean); overload;

    {** Stores the image in the stream without compression nor header }
    procedure Serialize(AStream: TStream); override;
    {** Reads the image in a stream that was previously serialized }
    procedure Deserialize(AStream: TStream); override;

    // universal brushes
    procedure SolidBrushIndirect(out ABrush: TUniversalBrush; AColor: Pointer; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    class procedure SolidBrush(out ABrush: TUniversalBrush; const AColor: TBGRAPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    class procedure ScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode = dmDrawWithTransparency;
                                 AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure MaskBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner;
                              AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure EraseBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
    class procedure AlphaBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;

    {==== Pixel functions ====}
    {** Sets the pixel by replacing the content at (''x'',''y'') with the specified color.
        Alpha value is set to 255 (opaque) }
    procedure SetPixel(x, y: int32or64; c: TColor); overload; override;
    {** Applies a logical '''xor''' to the content of the pixel with the specified value.
        This includes the alpha channel, so if you want to preserve the opacity, provide
        a color ''c'' with alpha channel equal to zero }
    procedure XorPixel(x, y: int32or64; const c: TBGRAPixel); override;
    {** Draws a pixel with gamma correction at (''x'',''y''). Pixel is supplied
        in sRGB colorspace }
    procedure DrawPixel(x, y: int32or64; const c: TBGRAPixel); overload; override;
    {** Draws a pixel without gamma correction at (''x'',''y''). Pixel is supplied
        in sRGB colorspace }
    procedure FastBlendPixel(x, y: int32or64; const c: TBGRAPixel); override;
    {** Erase the content of the pixel by reducing the value of the
        alpha channel. ''alpha'' specifies how much to decrease.
        If the resulting alpha reaches zero, the content
        is replaced by ''BGRAPixelTransparent'' }
    procedure ErasePixel(x, y: int32or64; alpha: byte); override;
    {** Sets the alpha value at (''x'',''y''). If ''alpha'' = 0, the
        pixel is replaced by ''BGRAPixelTransparent'' }
    procedure AlphaPixel(x, y: int32or64; alpha: byte); override;
    {** Computes the value of the pixel at a floating point coordiante
        by interpolating the values of the pixels around it.
      * There is a one pixel wide margin around the pixel where the pixels are
        still considered inside. If ''smoothBorder'' is set to true, pixel fade
        to transparent.
      * If it is more out of the bounds, the result is ''BGRAPixelTransparent''.
      * ''AResampleFilter'' specifies how pixels must be interpolated. Accepted
        values are ''rfBox'', ''rfLinear'', ''rfHalfCosine'' and ''rfCosine'' }
    function GetPixel(x, y: single; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TBGRAPixel; overload; override;
    {** Similar to previous ''GetPixel'' function, but the fractional part of
        the coordinate is supplied with a number from 0 to 255. The actual
        coordinate is (''x'' + ''fracX256''/256, ''y'' + ''fracY256''/256) }
    function GetPixel256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TBGRAPixel; override;
    {** Computes the value of the pixel at a floating point coordiante
        by interpolating the values of the pixels around it. If the pixel
        is out of bounds, the image is repeated.
      * ''AResampleFilter'' specifies how pixels must be interpolated. Accepted
        values are ''rfBox'', ''rfLinear'', ''rfHalfCosine'' and ''rfCosine'' }
    function GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; overload; override;
    {** Similar to previous ''GetPixel'' function, but the fractional part of
        the coordinate is supplied with a number from 0 to 255. The actual
        coordinate is (''x'' + ''fracX256''/256, ''y'' + ''fracY256''/256) }
    function GetPixelCycle256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; overload; override;
    {** Computes the value of the pixel at a floating point coordiante
        by interpolating the values of the pixels around it. ''repeatX'' and
        ''repeatY'' specifies if the image is to be repeated or not.
      * ''AResampleFilter'' specifies how pixels must be interpolated. Accepted
        values are ''rfBox'', ''rfLinear'', ''rfHalfCosine'' and ''rfCosine'' }
    function GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter; repeatX: boolean; repeatY: boolean): TBGRAPixel; overload; override;
    {** Similar to previous ''GetPixel'' function, but the fractional part of
        the coordinate is supplied with a number from 0 to 255. The actual
        coordinate is (''x'' + ''fracX256''/256, ''y'' + ''fracY256''/256) }
    function GetPixelCycle256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter; repeatX: boolean; repeatY: boolean): TBGRAPixel; overload; override;

    {==== Drawing lines and polylines (integer coordinates) ====}
    {* These functions do not take into account current pen style/cap/join.
       See [[BGRABitmap tutorial 13|coordinate system]]. }

    {** Applies xor to the pixels at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color.
        This includes the alpha channel, so if you want to preserve the
        opacity, provide a color ''c'' with alpha channel equal to zero }
    procedure XorHorizLine(x, y, x2: int32or64; c: TBGRAPixel); override;
    {** Draws an horizontal line with gamma correction at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color }
    procedure DrawHorizLine(x, y, x2: int32or64; ec: TExpandedPixel); override; overload;
    {** Draws an horizontal line without gamma correction at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color }
    procedure FastBlendHorizLine(x, y, x2: int32or64; c: TBGRAPixel); override;
    {** Replaces the alpha value of the pixels at line ''y'' and
        at columns ''x'' to ''x2'' included }
    procedure AlphaHorizLine(x, y, x2: int32or64; alpha: byte); override;
    {** Draws an horizontal line with gamma correction at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color,
        and with a transparency that increases with the color difference
        with ''compare''. If the difference is greater than ''maxDiff'',
        pixels are not changed }
    procedure DrawHorizLineDiff(x, y, x2: int32or64; c, compare: TBGRAPixel;
      maxDiff: byte); override;
    procedure HorizLineDiff(x, y, x2: int32or64; const ABrush: TUniversalBrush;
      ACompare: TBGRAPixel; AMaxDiffW: word); override;

    {** Xors a vertical line at column ''x'' and at row ''y'' to ''y2'' }
    procedure XorVertLine(x, y, y2: int32or64; c: TBGRAPixel); override;
    {** Draws a vertical line with gamma correction at column ''x'' and at row ''y'' to ''y2'' }
    procedure DrawVertLine(x, y, y2: int32or64; c: TBGRAPixel); override;
    {** Draws a vertical line without gamma correction at column ''x'' and at row ''y'' to ''y2'' }
    procedure FastBlendVertLine(x, y, y2: int32or64; c: TBGRAPixel); override;
    {** Replace alpha values in a vertical line at column ''x'' and at row ''y'' to ''y2'' }
    procedure AlphaVertLine(x, y, y2: int32or64; alpha: byte); override;

    {** Fills completely a rectangle, without any border, with the specified ''texture'' and
    with the specified ''mode'' }
    procedure FillRect(x, y, x2, y2: integer; texture: IBGRAScanner; mode: TDrawMode; AScanOffset: TPoint; ditheringAlgorithm: TDitheringAlgorithm); overload; override;

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
    procedure DrawPolyLineAntialias(const points: array of TPointF; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel); overload; override;
    {** Draws a filled polygon using current pen style/cap/join in one go.
        The stroke is stricly over the fill even if partially transparent.
        The polygon is always closed. You don't need to set the last point
        to be the same as the first point. }
    procedure DrawPolygonAntialias(const points: array of TPointF; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel); overload; override;

    {** Draws and fills an ellipse }
    procedure EllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel; w: single; back: TBGRAPixel); overload; override;
    procedure EllipseAntialias(AOrigin, AXAxis, AYAxis: TPointF; c: TBGRAPixel; w: single; back: TBGRAPixel); overload; override;

    {** Draws and fills a path }
    procedure DrawPath(APath: IBGRAPath; AStrokeColor: TBGRAPixel; AWidth: single; AFillColor: TBGRAPixel); overload; override;
    procedure DrawPath(APath: IBGRAPath; AStrokeTexture: IBGRAScanner; AWidth: single; AFillColor: TBGRAPixel); overload; override;
    procedure DrawPath(APath: IBGRAPath; AStrokeColor: TBGRAPixel; AWidth: single; AFillTexture: IBGRAScanner); overload; override;
    procedure DrawPath(APath: IBGRAPath; AStrokeTexture: IBGRAScanner; AWidth: single; AFillTexture: IBGRAScanner); overload; override;

    {** Draws and fills a path with a matrix transform }
    procedure DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix; AStrokeColor: TBGRAPixel; AWidth: single; AFillColor: TBGRAPixel); overload; override;
    procedure DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix; AStrokeTexture: IBGRAScanner; AWidth: single; AFillColor: TBGRAPixel); overload; override;
    procedure DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix; AStrokeColor: TBGRAPixel; AWidth: single; AFillTexture: IBGRAScanner); overload; override;
    procedure DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix; AStrokeTexture: IBGRAScanner; AWidth: single; AFillTexture: IBGRAScanner); overload; override;

    {** Draws a rectangle with antialiasing and fills it with color ''back''.
        Note that the pixel (x2,y2) is included contrary to integer coordinates }
    procedure RectangleAntialias(x, y, x2, y2: single; c: TBGRAPixel; w: single; back: TBGRAPixel); overload; override;

    {** Draws a rounded rectangle border with antialiasing. The corners have an
        elliptical radius of ''rx'' and ''ry''. ''options'' specifies how to
        draw the corners. See [[BGRABitmap Geometry types|geometry types]] }
    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; c: TBGRAPixel; w: single; options: TRoundRectangleOptions = []); overload; override;
    {** Draws a rounded rectangle border with the specified texture.
        The corners have an elliptical radius of ''rx'' and ''ry''.
        ''options'' specifies how to draw the corners.
        See [[BGRABitmap Geometry types|geometry types]] }
    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; texture: IBGRAScanner; w: single; options: TRoundRectangleOptions = []); overload; override;
    {** Draws and fills a round rectangle }
    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; pencolor: TBGRAPixel; w: single; fillcolor: TBGRAPixel; options: TRoundRectangleOptions = []); overload; override;
    {** Draws and fills a round rectangle with textures }
    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; penTexture: IBGRAScanner; w: single; fillTexture: IBGRAScanner; options: TRoundRectangleOptions = []); overload; override;

    {==== Gradient polygons ====}

    procedure FillTriangleLinearColor(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel); override;
    procedure FillTriangleLinearColorAntialias(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel); override;
    procedure FillTriangleLinearMapping(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; TextureInterpolation: Boolean= True); override;
    procedure FillTriangleLinearMappingLightness(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; light1,light2,light3: word; TextureInterpolation: Boolean= True); override;
    procedure FillTriangleLinearMappingAntialias(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF); override;

    procedure FillQuadLinearColor(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel); override;
    procedure FillQuadLinearColorAntialias(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel); override;
    procedure FillQuadLinearMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; TextureInterpolation: Boolean= True; ACulling: TFaceCulling = fcNone; ACropToPolygon: boolean = true); override;
    procedure FillQuadLinearMappingLightness(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; light1,light2,light3,light4: word; TextureInterpolation: Boolean= True); override;
    procedure FillQuadLinearMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACulling: TFaceCulling = fcNone); override;
    procedure FillQuadPerspectiveMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    procedure FillQuadPerspectiveMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACleanBorders: TRect; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    procedure FillQuadPerspectiveMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF); override;
    procedure FillQuadPerspectiveMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACleanBorders: TRect); override;
    procedure FillQuadAffineMapping(Orig,HAxis,VAxis: TPointF; AImage: TBGRACustomBitmap; APixelCenteredCoordinates: boolean = true; ADrawMode: TDrawMode = dmDrawWithTransparency; AOpacity: byte = 255); override;
    procedure FillQuadAffineMappingAntialias(Orig,HAxis,VAxis: TPointF; AImage: TBGRACustomBitmap; APixelCenteredCoordinates: boolean = true; AOpacity: byte = 255); override;

    {** Fills an ellipse with a gradient of color. ''outercolor'' specifies
        the end color of the gradient on the border of the ellipse and
        ''innercolor'' the end color of the gradient at the center of the ellipse }
    procedure FillEllipseLinearColorAntialias(x, y, rx, ry: single; outercolor, innercolor: TBGRAPixel); overload; override;
    procedure FillEllipseLinearColorAntialias(AOrigin, AXAxis, AYAxis: TPointF; outercolor, innercolor: TBGRAPixel); overload; override;

    procedure FillPolyLinearMapping(const points: array of TPointF; texture: IBGRAScanner; texCoords: array of TPointF; TextureInterpolation: Boolean); override;
    procedure FillPolyLinearMappingLightness(const points: array of TPointF; texture: IBGRAScanner; texCoords: array of TPointF; lightnesses: array of word; TextureInterpolation: Boolean); override;
    procedure FillPolyLinearColor(const points: array of TPointF; AColors: array of TBGRAPixel); override;
    procedure FillPolyPerspectiveMapping(const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner; texCoords: array of TPointF; TextureInterpolation: Boolean; zbuffer: psingle = nil); override;
    procedure FillPolyPerspectiveMappingLightness(const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner; texCoords: array of TPointF; lightnesses: array of word; TextureInterpolation: Boolean; zbuffer: psingle = nil); override;

    procedure ArrowStartAsNone; override;
    procedure ArrowStartAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1); override;
    procedure ArrowStartAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5); override;
    procedure ArrowStartAsTail; override;

    procedure ArrowEndAsNone; override;
    procedure ArrowEndAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1); override;
    procedure ArrowEndAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5); override;
    procedure ArrowEndAsTail; override;

    { Draws the UTF8 encoded string, with color c.
      If align is taLeftJustify, (x,y) is the top-left corner.
      If align is taCenter, (x,y) is at the top and middle of the text.
      If align is taRightJustify, (x,y) is the top-right corner.
      The value of FontOrientation is taken into account, so that the text may be rotated. }
    procedure TextOut(x, y: single; const sUTF8: string; c: TBGRAPixel; align: TAlignment; ARightToLeft: boolean); overload; override;

    { Same as above functions, except that the text is filled using texture.
      The value of FontOrientation is taken into account, so that the text may be rotated. }
    procedure TextOut(x, y: single; const sUTF8: string; texture: IBGRAScanner; align: TAlignment; ARightToLeft: boolean); overload; override;

    procedure TextOut(x, y: single; const sUTF8: string; AColor: TBGRAPixel; AAlign: TAlignment; ALetterSpacing: single); overload; override;
    procedure TextOut(x, y: single; const sUTF8: string; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single); overload; override;

    { Same as above, except that the orientation is specified, overriding the value of the property FontOrientation. }
    procedure TextOutAngle(x, y: single; orientationTenthDegCCW: integer; const sUTF8: string; c: TBGRAPixel; align: TAlignment; ARightToLeft: boolean); overload; override;
    procedure TextOutAngle(x, y: single; orientationTenthDegCCW: integer; const sUTF8: string; texture: IBGRAScanner; align: TAlignment; ARightToLeft: boolean); overload; override;

    procedure TextOutCurved(ACursor: TBGRACustomPathCursor; const sUTF8: string; AColor: TBGRAPixel; AAlign: TAlignment; ALetterSpacing: single); overload; override;
    procedure TextOutCurved(ACursor: TBGRACustomPathCursor; const sUTF8: string; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single); overload; override;

    procedure TextMultiline(ALeft,ATop,AWidth: single; const sUTF8: string; c: TBGRAPixel; AAlign: TBidiTextAlignment = btaNatural; AVertAlign: TTextLayout = tlTop; AParagraphSpacing: single = 0); overload; override;
    procedure TextMultiline(ALeft,ATop,AWidth: single; const sUTF8: string; ATexture: IBGRAScanner; AAlign: TBidiTextAlignment = btaNatural; AVertAlign: TTextLayout = tlTop; AParagraphSpacing: single = 0); overload; override;

    { Draw the UTF8 encoded string at the coordinate (x,y), clipped inside the rectangle ARect.
      Additional style information is provided by the style parameter.
      The color c or texture is used to fill the text. No rotation is applied. }
    procedure TextRect(ARect: TRect; x, y: integer; const sUTF8: string; style: TTextStyle; c: TBGRAPixel); overload; override;
    procedure TextRect(ARect: TRect; x, y: integer; const sUTF8: string; style: TTextStyle; texture: IBGRAScanner); overload; override;

    { Returns the total size of the string provided using the current font.
      Orientation is not taken into account, so that the width is along the text. End of lines are stripped from the string. }
    function TextSize(const sUTF8: string): TSize; override;
    function TextSizeMultiline(const sUTF8: string; AMaxWidth: single = EmptySingle; AParagraphSpacing: single = 0): TSize; override;

    { Returns the affine box of the string provided using the current font.
      Orientation is taken into account. End of lines are stripped from the string. }
    function TextAffineBox(const sUTF8: string): TAffineBox; override;

    { Returns the total size of a paragraph i.e. with word break }
    function TextSize(const sUTF8: string; AMaxWidth: integer): TSize; override;
    function TextSize(const sUTF8: string; AMaxWidth: integer; ARightToLeft: boolean): TSize; override;
    function TextFitInfo(const sUTF8: string; AMaxWidth: integer): integer; override;

    {Spline}
    function ComputeClosedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF; override;
    function ComputeOpenedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF; override;

    function ComputeBezierCurve(const ACurve: TCubicBezierCurve): ArrayOfTPointF; overload; override;
    function ComputeBezierCurve(const ACurve: TQuadraticBezierCurve): ArrayOfTPointF; overload; override;
    function ComputeBezierSpline(const ASpline: array of TCubicBezierCurve): ArrayOfTPointF; overload; override;
    function ComputeBezierSpline(const ASpline: array of TQuadraticBezierCurve): ArrayOfTPointF; overload; override;

    function ComputeWidePolyline(const points: array of TPointF; w: single): ArrayOfTPointF; overload; override;
    function ComputeWidePolyline(const points: array of TPointF; w: single; ClosedCap: boolean): ArrayOfTPointF; overload; override;
    function ComputeWidePolygon(const points: array of TPointF; w: single): ArrayOfTPointF; overload; override;

    function ComputeEllipseContour(x,y,rx,ry: single; quality: single = 1): ArrayOfTPointF;  overload; override;
    function ComputeEllipseContour(AOrigin, AXAxis, AYAxis: TPointF; quality: single = 1): ArrayOfTPointF;  overload; override;
    function ComputeEllipseBorder(x,y,rx,ry,w: single; quality: single = 1): ArrayOfTPointF; overload; override;
    function ComputeEllipseBorder(AOrigin, AXAxis, AYAxis: TPointF; w: single; quality: single = 1): ArrayOfTPointF; override; overload;
    function ComputeArc65536(x,y,rx,ry: single; start65536,end65536: word; quality: single = 1): ArrayOfTPointF; override;
    function ComputeArcRad(x,y,rx,ry: single; startRad,endRad: single; quality: single = 1): ArrayOfTPointF; override;
    function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; quality: single = 1): ArrayOfTPointF; overload; override;
    function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; options: TRoundRectangleOptions; quality: single = 1): ArrayOfTPointF; overload; override;
    function ComputePie65536(x,y,rx,ry: single; start65536,end65536: word; quality: single = 1): ArrayOfTPointF; override;
    function ComputePieRad(x,y,rx,ry: single; startRad,endRad: single; quality: single = 1): ArrayOfTPointF; override;

    {Filling}
    procedure Fill(c: TBGRAPixel; start, Count: integer); overload; override;
    procedure DrawPixels(c: TBGRAPixel; start, Count: integer); override;
    procedure AlphaFill(alpha: byte; start, Count: integer); overload; override;
    procedure FillMask(x,y: integer; AMask: TCustomUniversalBitmap; const AColor: TBGRAPixel; ADrawMode: TDrawMode); overload; override;
    procedure FillMask(x,y: integer; AMask: TCustomUniversalBitmap; ATexture: IBGRAScanner; ADrawMode: TDrawMode; AOpacity: byte); overload; override;
    procedure EraseMask(x,y: integer; AMask: TBGRACustomBitmap; alpha: byte=255); override;
    procedure FillClearTypeMask(x,y: integer; xThird: integer; AMask: TBGRACustomBitmap; color: TBGRAPixel; ARGBOrder: boolean = true); override;
    procedure FillClearTypeMask(x,y: integer; xThird: integer; AMask: TBGRACustomBitmap; texture: IBGRAScanner; ARGBOrder: boolean = true); override;
    procedure ReplaceColor(before, after: TColor); overload; override;
    procedure ReplaceColor(ABounds: TRect; before, after: TColor); overload; override;
    procedure ParallelFloodFill(X, Y: integer; Dest: TCustomUniversalBitmap; Color: TBGRAPixel;
      mode: TFloodfillMode; Tolerance: byte = 0; DestOfsX: integer = 0; DestOfsY: integer = 0); overload; override;
    procedure ParallelFloodFill(X, Y: integer; Dest: TCustomUniversalBitmap; const Brush: TUniversalBrush;
      Progressive: boolean; ToleranceW: Word = $00ff; DestOfsX: integer = 0; DestOfsY: integer = 0); overload; override;
    procedure GradientFill(x, y, x2, y2: integer; c1, c2: TBGRAPixel;
      gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
      gammaColorCorrection: boolean = True; Sinus: Boolean=False;
      ditherAlgo: TDitheringAlgorithm = daNearestNeighbor); override;
    procedure GradientFill(x, y, x2, y2: integer; gradient: TBGRACustomGradient;
      gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
      Sinus: Boolean=False; ditherAlgo: TDitheringAlgorithm = daNearestNeighbor); override;

    function ScanAtInteger(X,Y: integer): TBGRAPixel; override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;

    {Canvas drawing functions}
    procedure Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean = True); overload; override;
    procedure Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean = True); overload; override;
    procedure InvalidateBitmap; override;         //call if you modify with Scanline
    procedure LoadFromBitmapIfNeeded; override;   //call to ensure that bitmap data is up to date
    procedure NotifyBitmapChange; inline;

    {BGRA bitmap functions}
    procedure CrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadePosition: byte; mode: TDrawMode = dmDrawWithTransparency); overload; override;
    procedure CrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadeMask: IBGRAScanner; mode: TDrawMode = dmDrawWithTransparency); overload; override;
    procedure PutImage(X, Y: integer; ASource: TCustomUniversalBitmap; AMode: TDrawMode; AOpacity: byte); overload; override;
    procedure PutImageAffine(AMatrix: TAffineMatrix; Source: TBGRACustomBitmap; AOutputBounds: TRect; AResampleFilter: TResampleFilter; AMode: TDrawMode; AOpacity: Byte=255; APixelCenteredCoords: boolean = true); overload; override;
    function GetImageAffineBounds(AMatrix: TAffineMatrix; ASourceBounds: TRect; AClipOutput: boolean = true; APixelCenteredCoords: boolean = true): TRect; overload; override;
    class function IsAffineRoughlyTranslation(AMatrix: TAffineMatrix; ASourceBounds: TRect): boolean; override;

    procedure StretchPutImage(ARect: TRect; Source: TBGRACustomBitmap; mode: TDrawMode; AOpacity: byte = 255); override;
    procedure BlendRect(ADest: TRect; AColor: TBGRAPixel; AOperation: TBlendOperation; AExcludeChannels: TChannels); overload; override;
    procedure BlendRectOver(ADest: TRect; AColor: TBGRAPixel; AOperation: TBlendOperation; AOpacity: byte; ALinearBlend: boolean; AExcludeChannels: TChannels); overload; override;
    procedure BlendImage(x, y: integer; ASource: TBGRACustomBitmap; AOperation: TBlendOperation); overload; override;
    procedure BlendImage(ADest: TRect; ASource: IBGRAScanner; AOffsetX, AOffsetY: integer; AOperation: TBlendOperation); overload; override;
    procedure BlendImageOver(x, y: integer; ASource: TBGRACustomBitmap; AOperation: TBlendOperation; AOpacity: byte = 255; ALinearBlend: boolean = false); overload; override;
    procedure BlendImageOver(ADest: TRect; ASource: IBGRAScanner; AOffsetX, AOffsetY: integer; AOperation: TBlendOperation; AOpacity: byte = 255; ALinearBlend: boolean = false); overload; override;

    function GetPtrBitmap(Top,Bottom: Integer): TBGRACustomBitmap; override;
    function MakeBitmapCopy(BackgroundColor: TColor): TBitmap; override;

    function Resample(newWidth, newHeight: integer;
      mode: TResampleMode = rmFineResample): TBGRADefaultBitmap; override;
    procedure Negative; override;
    procedure NegativeRect(ABounds: TRect); override;
    procedure LinearNegative; override;
    procedure LinearNegativeRect(ABounds: TRect); override;
    procedure InplaceGrayscale(AGammaCorrection: boolean = true); overload; override;
    procedure InplaceGrayscale(ABounds: TRect; AGammaCorrection: boolean = true); overload; override;
    procedure InplaceNormalize(AEachChannel: boolean = True); overload; override;
    procedure InplaceNormalize(ABounds: TRect; AEachChannel: boolean = True); overload; override;
    procedure SwapRedBlue; override;
    procedure SwapRedBlue(ARect: TRect); override;
    procedure GrayscaleToAlpha; override;
    procedure AlphaToGrayscale; override;
    function GetMaskFromAlpha: TBGRADefaultBitmap; override;
    function GetGrayscaleMaskFromAlpha: TGrayscaleMask;
    procedure ConvertToLinearRGB; override;
    procedure ConvertFromLinearRGB; override;

    {Filters}
    function FilterSmartZoom3(Option: TMedianOption): TBGRADefaultBitmap; override;
    function FilterMedian(Option: TMedianOption): TBGRADefaultBitmap; override;
    function FilterSmooth: TBGRADefaultBitmap; override;
    function FilterSharpen(Amount: single = 1): TBGRADefaultBitmap; overload; override;
    function FilterSharpen(ABounds: TRect; Amount: single = 1): TBGRADefaultBitmap; overload; override;
    function FilterContour(AGammaCorrection: boolean = false): TBGRADefaultBitmap; override;
    function FilterPixelate(pixelSize: integer; useResample: boolean; filter: TResampleFilter = rfLinear): TBGRADefaultBitmap; override;
    function FilterEmboss(angle: single; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGRADefaultBitmap; overload; override;
    function FilterEmboss(angle: single; ABounds: TRect; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGRADefaultBitmap; overload; override;
    function FilterEmbossHighlight(FillSelection: boolean): TBGRADefaultBitmap; overload; override;
    function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel): TBGRADefaultBitmap; overload; override;
    function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel; var Offset: TPoint): TBGRADefaultBitmap; overload; override;
    function FilterGrayscale: TBGRADefaultBitmap; overload; override;
    function FilterGrayscale(ABounds: TRect): TBGRADefaultBitmap; overload; override;
    function FilterNormalize(eachChannel: boolean = True): TBGRADefaultBitmap; overload; override;
    function FilterNormalize(ABounds: TRect; eachChannel: boolean = True): TBGRADefaultBitmap; overload; override;
    function FilterRotate(origin: TPointF; angle: single; correctBlur: boolean = false): TBGRADefaultBitmap; override;
    function FilterAffine(AMatrix: TAffineMatrix; correctBlur: boolean = false): TBGRADefaultBitmap; override;
    function FilterSphere: TBGRADefaultBitmap; override;
    function FilterTwirl(ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRADefaultBitmap; overload; override;
    function FilterTwirl(ABounds: TRect; ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRADefaultBitmap; overload; override;
    function FilterCylinder: TBGRADefaultBitmap; override;
    function FilterPlane: TBGRADefaultBitmap; override;
  end;

  { TBGRAPtrBitmap }

  TBGRAPtrBitmap = class(TBGRADefaultBitmap)
  protected
    function GetLineOrder: TRawImageLineOrder; override;
    procedure SetLineOrder(AValue: TRawImageLineOrder); override;
    procedure ReallocData; override;
    procedure FreeData; override;
    procedure CannotResize;
    procedure NotImplemented;
    procedure RebuildBitmap; override;

    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; override; //to override
    function LoadFromRawImage({%H-}ARawImage: TRawImage; {%H-}DefaultOpacity: byte;
      {%H-}AlwaysReplaceAlpha: boolean=False; {%H-}RaiseErrorOnInvalidPixelFormat: boolean
      =True): boolean; override; //to override
  public
    constructor Create(AWidth, AHeight: integer; AData: Pointer); overload;
    procedure SetDataPtr(AData: Pointer);
    property LineOrder: TRawImageLineOrder Read GetLineOrder Write SetLineOrder;

    procedure DataDrawTransparent({%H-}ACanvas: TCanvas; {%H-}Rect: TRect; {%H-}AData: Pointer;
      {%H-}ALineOrder: TRawImageLineOrder; {%H-}AWidth, {%H-}AHeight: integer); override; //to override
    procedure DataDrawOpaque({%H-}ACanvas: TCanvas; {%H-}Rect: TRect; {%H-}AData: Pointer;
      {%H-}ALineOrder: TRawImageLineOrder; {%H-}AWidth, {%H-}AHeight: integer); override; //to override
    procedure GetImageFromCanvas({%H-}CanvasSource: TCanvas; {%H-}x, {%H-}y: integer); override; //to override

    procedure Assign({%H-}Source: TPersistent); override;
    procedure TakeScreenshot({%H-}ARect: TRect); override;
    procedure TakeScreenshotOfPrimaryMonitor; override;
    procedure LoadFromDevice({%H-}DC: HDC); override;
    procedure LoadFromDevice({%H-}DC: HDC; {%H-}ARect: TRect); override;
  end;

  { TBGRAMemoryStreamBitmap }

  TBGRAMemoryStreamBitmap = class(TBGRAPtrBitmap)
  private
    function GetOwnStream: boolean;
    procedure SetOwnStream(AValue: boolean);
  protected
    FStream: TMemoryStream;
    FStreamOffset: IntPtr;
    FOwnStream: boolean;
  public
    constructor Create(AWidth, AHeight: integer; AStream: TMemoryStream; AStreamOffset: IntPtr; AOwnStream: boolean);
    constructor Create(AWidth, AHeight: integer); override;
    constructor Create(AWidth, AHeight: integer; AColor: TBGRAPixel);
    destructor Destroy; override;
    property OwnStream: boolean read GetOwnStream write SetOwnStream;
    property Stream: TMemoryStream read FStream;
  end;

var
  DefaultTextStyle: TTextStyle;

procedure BGRAGradientFill(bmp: TBGRACustomBitmap; x, y, x2, y2: integer;
  c1, c2: TBGRAPixel; gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
  gammaColorCorrection: boolean = True; Sinus: Boolean=False);

implementation

uses Math, BGRAUTF8, BGRABlend, BGRAFilters, BGRAGradientScanner,
  BGRAResample, BGRAPolygon, BGRAPolygonAliased,
  BGRAPath, FPReadPcx, FPWritePcx, FPReadXPM, FPWriteXPM,
  BGRAReadBMP, BGRAReadJpeg,
  BGRADithering, BGRAFilterScanner;

{ TBGRAMemoryStreamBitmap }

function TBGRAMemoryStreamBitmap.GetOwnStream: boolean;
begin
  result := FOwnStream;
end;

procedure TBGRAMemoryStreamBitmap.SetOwnStream(AValue: boolean);
begin
  FOwnStream:= AValue;
end;

constructor TBGRAMemoryStreamBitmap.Create(AWidth, AHeight: integer;
  AStream: TMemoryStream; AStreamOffset: IntPtr; AOwnStream: boolean);
begin
  inherited Create(AWidth, AHeight, PByte(AStream.Memory) + AStreamOffset);
  FStream := AStream;
  FStreamOffset:= AStreamOffset;
  FOwnStream := AOwnStream;
end;

constructor TBGRAMemoryStreamBitmap.Create(AWidth, AHeight: integer);
begin
  Create(AWidth, AHeight, BGRAPixelTransparent);
end;

constructor TBGRAMemoryStreamBitmap.Create(AWidth, AHeight: integer;
  AColor: TBGRAPixel);
begin
  inherited Create(AWidth, AHeight);
  FStream := TMemoryStream.Create;
  FStreamOffset:= 0;
  FStream.Size := RowSize * Height;
  FOwnStream := true;
  SetDataPtr(PByte(FStream.Memory) + FStreamOffset);
  Fill(AColor, dmSet);
end;

destructor TBGRAMemoryStreamBitmap.Destroy;
begin
  if FOwnStream then FStream.Free;
  inherited Destroy;
end;

{ TBGRADefaultBitmap }

function TBGRADefaultBitmap.CheckEmpty: boolean;
const
  alphaMask = $ff shl TBGRAPixel_AlphaShift;
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Data;
  for i := (NbPixels shr 1) - 1 downto 0 do
  begin
    if PInt64(p)^ and (alphaMask or (alphaMask shl 32)) <> 0 then
    begin
      Result := False;
      exit;
    end;
    Inc(p,2);
  end;
  if Odd(NbPixels) and (p^.alpha <> 0) then
  begin
    Result := false;
    exit;
  end;
  Result := True;
end;

function TBGRADefaultBitmap.GetCanvasAlphaCorrection: boolean;
begin
  Result := (FCanvasOpacity <> 0);
end;

function TBGRADefaultBitmap.GetCustomPenStyle: TBGRAPenStyle;
begin
  result := GetInternalPen.CustomPenStyle;
end;

procedure TBGRADefaultBitmap.SetCanvasAlphaCorrection(const AValue: boolean);
begin
  if AValue then
  begin
    if FCanvasOpacity = 0 then
      FCanvasOpacity := 255;
  end
  else
    FCanvasOpacity := 0;
end;

procedure TBGRADefaultBitmap.DoLoadFromBitmap;
begin
  //nothing
end;

procedure TBGRADefaultBitmap.SetCustomPenStyle(const AValue: TBGRAPenStyle);
begin
  GetInternalPen.CustomPenStyle := AValue;
end;

procedure TBGRADefaultBitmap.SetPenStyle(const AValue: TPenStyle);
begin
  GetInternalPen.Style := AValue;
end;

function TBGRADefaultBitmap.GetPenStyle: TPenStyle;
begin
  Result:= GetInternalPen.Style;
end;

function TBGRADefaultBitmap.GetArrowEndSize: TPointF;
begin
  result := GetArrow.EndSize;
end;

function TBGRADefaultBitmap.GetArrowStartSize: TPointF;
begin
  result := GetArrow.StartSize;
end;

procedure TBGRADefaultBitmap.SetArrowEndSize(AValue: TPointF);
begin
  {$PUSH}{$OPTIMIZATION OFF}
  GetArrow.EndSize := AValue;
  {$POP}
end;

procedure TBGRADefaultBitmap.SetArrowStartSize(AValue: TPointF);
begin
  {$PUSH}{$OPTIMIZATION OFF}
  GetArrow.StartSize := AValue;
  {$POP}
end;

function TBGRADefaultBitmap.GetArrowEndOffset: single;
begin
  result := GetArrow.EndOffsetX;
end;

function TBGRADefaultBitmap.GetArrowStartOffset: single;
begin
  result := GetArrow.StartOffsetX;
end;

procedure TBGRADefaultBitmap.SetArrowEndOffset(AValue: single);
begin
  GetArrow.EndOffsetX := AValue;
end;

procedure TBGRADefaultBitmap.SetArrowStartOffset(AValue: single);
begin
  GetArrow.StartOffsetX := AValue;
end;

function TBGRADefaultBitmap.GetArrowEndRepeat: integer;
begin
  result := GetArrow.EndRepeatCount;
end;

function TBGRADefaultBitmap.GetArrowStartRepeat: integer;
begin
  result := GetArrow.StartRepeatCount;
end;

procedure TBGRADefaultBitmap.SetArrowEndRepeat(AValue: integer);
begin
  GetArrow.EndRepeatCount := AValue;
end;

procedure TBGRADefaultBitmap.SetArrowStartRepeat(AValue: integer);
begin
  GetArrow.StartRepeatCount := AValue;
end;

procedure TBGRADefaultBitmap.SetFontHeight(AHeight: integer);
begin
  FFontHeight := AHeight;
end;

function TBGRADefaultBitmap.GetFontFullHeight: integer;
begin
  if FontHeight < 0 then
    result := -FontHeight
  else
    result := TextSize('Hg').cy;
end;

procedure TBGRADefaultBitmap.SetFontFullHeight(AHeight: integer);
begin
  if AHeight > 0 then
    FontHeight := -AHeight
  else
    FontHeight := 1;
end;

function TBGRADefaultBitmap.GetFontPixelMetric: TFontPixelMetric;
begin
  result := FontRenderer.GetFontPixelMetric;
end;

function TBGRADefaultBitmap.GetFontRenderer: TBGRACustomFontRenderer;
begin
  if FFontRenderer = nil then FFontRenderer := CreateDefaultFontRenderer;
  if FFontRenderer = nil then raise exception.Create('No font renderer');
  result := FFontRenderer;
  result.FontName := FontName;
  result.FontStyle := FontStyle;
  result.FontQuality := FontQuality;
  result.FontOrientation := FontOrientation;
  result.FontEmHeight := FFontHeight;
end;

procedure TBGRADefaultBitmap.SetFontRenderer(AValue: TBGRACustomFontRenderer);
begin
  if AValue = FFontRenderer then exit;
  FFontRenderer.Free;
  FFontRenderer := AValue
end;

function TBGRADefaultBitmap.GetFontVerticalAnchorOffset: single;
begin
  case FontVerticalAnchor of
  fvaTop: result := 0;
  fvaCenter: result := FontFullHeight*0.5;
  fvaCapLine: result := FontPixelMetric.CapLine;
  fvaCapCenter: result := (FontPixelMetric.CapLine+FontPixelMetric.Baseline)*0.5;
  fvaXLine: result := FontPixelMetric.xLine;
  fvaXCenter: result := (FontPixelMetric.xLine+FontPixelMetric.Baseline)*0.5;
  fvaBaseline: result := FontPixelMetric.Baseline;
  fvaDescentLine: result := FontPixelMetric.DescentLine;
  fvaBottom: result := FontFullHeight;
  else
    result := 0;
  end;
end;

function TBGRADefaultBitmap.GetFontAnchorRotatedOffset: TPointF;
begin
  result := GetFontAnchorRotatedOffset(FontOrientation);
end;

function TBGRADefaultBitmap.GetFontAnchorRotatedOffset(
  ACustomOrientation: integer): TPointF;
begin
  result := PointF(0, GetFontVerticalAnchorOffset);
  if ACustomOrientation <> 0 then
    result := AffineMatrixRotationDeg(-ACustomOrientation*0.1)*result;
end;

{ Creates a new bitmap with dimensions 0 x 0 }
function TBGRADefaultBitmap.NewBitmap: TBGRADefaultBitmap;
begin
  Result := inherited NewBitmap as TBGRADefaultBitmap;
end;

{ Creates a new bitmap with dimensions AWidth and AHeight and filled with
  transparent pixels. Internally, it uses the same type so that if you
  use an optimized version, you get a new bitmap with the same optimizations }
function TBGRADefaultBitmap.NewBitmap(AWidth, AHeight: integer): TBGRADefaultBitmap;
begin
  result := inherited NewBitmap(AWidth, AHeight)  as TBGRADefaultBitmap;
end;

{ Can only be called from an existing instance of TBGRABitmap.
  Creates a new instance with dimensions AWidth and AHeight,
  and fills it with Color. }
function TBGRADefaultBitmap.NewBitmap(AWidth, AHeight: integer;
  const Color: TBGRAPixel): TBGRADefaultBitmap;
begin
  result := inherited NewBitmap(AWidth, AHeight, Color) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.NewBitmap(AWidth, AHeight: integer; AColor: Pointer): TBGRADefaultBitmap;
begin
  result := inherited NewBitmap(AWidth, AHeight, AColor) as TBGRADefaultBitmap;
end;

{ Creates a new bitmap and loads it contents from a file.
  The encoding of the string is the default one for the operating system.
  It is recommended to use the next function and UTF8 encoding }
function TBGRADefaultBitmap.NewBitmap(Filename: string): TBGRADefaultBitmap;
var
  BGRAClass: TBGRABitmapAny;
begin
  BGRAClass := TBGRABitmapAny(self.ClassType);
  Result    := BGRAClass.Create(Filename) as TBGRADefaultBitmap;
end;

{ Creates a new bitmap and loads it contents from a file.
  It is recommended to use UTF8 encoding }
function TBGRADefaultBitmap.NewBitmap(Filename: string; AIsUtf8: boolean): TBGRADefaultBitmap;
var
  BGRAClass: TBGRABitmapAny;
begin
  BGRAClass := TBGRABitmapAny(self.ClassType);
  Result    := BGRAClass.Create(Filename,AIsUtf8) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.NewBitmap(Filename: string; AIsUtf8: boolean;
  AOptions: TBGRALoadingOptions): TBGRADefaultBitmap;
var
  BGRAClass: TBGRABitmapAny;
begin
  BGRAClass := TBGRABitmapAny(self.ClassType);
  Result    := BGRAClass.Create(Filename,AIsUtf8,AOptions) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.NewBitmap(AFPImage: TFPCustomImage): TBGRADefaultBitmap;
var
  BGRAClass: TBGRABitmapAny;
begin
  BGRAClass := TBGRABitmapAny(self.ClassType);
  Result    := BGRAClass.Create(AFPImage) as TBGRADefaultBitmap;
end;

{----------------------- TFPCustomImage override ------------------------------}

{ Set the size of the current bitmap. All data is lost during the process }
procedure TBGRADefaultBitmap.SetSize(AWidth, AHeight: integer);
begin
  if (Width <> AWidth) or (Height <> AHeight) then
  begin
    inherited SetSize(AWidth, AHeight);
    FreeBitmap;
  end;
end;

{---------------------- Constructors ---------------------------------}

constructor TBGRADefaultBitmap.Create(AFPImage: TFPCustomImage);
begin
  inherited Create;
  Assign(AFPImage);
end;

{ Creates an image of dimensions AWidth and AHeight and filled with transparent pixels. }
constructor TBGRADefaultBitmap.Create(ABitmap: TBitmap; AUseTransparent: boolean);
begin
  inherited Create;
  Assign(ABitmap, AUseTransparent);
end;

{ Creates an image by loading its content from the file AFilename.
  The encoding of the string is the default one for the operating system.
  It is recommended to use the next constructor and UTF8 encoding. }
constructor TBGRADefaultBitmap.Create(AFilename: string);
begin
  inherited Create;
  LoadFromFile(Afilename);
end;

{ Free the object and all its resources }
destructor TBGRADefaultBitmap.Destroy;
begin
  DiscardXorMask;
  FFontRenderer.Free;
  {$IFDEF BGRABITMAP_USE_FPCANVAS}FCanvasFP.Free;{$ENDIF}
  FCanvasBGRA.Free;
  FCanvas2D.Free;
  FreeBitmap;
  inherited Destroy;
end;

{------------------------- Loading functions ----------------------------------}

{ Creates an image by loading its content from the file AFilename.
  The boolean AIsUtf8Filename specifies if UTF8 encoding is assumed for the filename. }
constructor TBGRADefaultBitmap.Create(AFilename: string; AIsUtf8: boolean);
begin
  inherited Create;
  if AIsUtf8 then
    LoadFromFileUTF8(Afilename)
  else
    LoadFromFile(Afilename);
end;

constructor TBGRADefaultBitmap.Create(AFilename: string; AIsUtf8: boolean;
  AOptions: TBGRALoadingOptions);
begin
  inherited Create;
  if AIsUtf8 then
    LoadFromFileUTF8(Afilename, AOptions)
  else
    LoadFromFile(Afilename, AOptions);
end;

{ Creates an image by loading its content from the stream AStream. }
constructor TBGRADefaultBitmap.Create(AStream: TStream);
begin
  inherited Create;
  LoadFromStream(AStream);
end;

procedure TBGRADefaultBitmap.Serialize(AStream: TStream);
begin
  If TBGRAPixel_RGBAOrder then
  begin
    LoadFromBitmapIfNeeded;
    TBGRAFilterScannerSwapRedBlue.ComputeFilterAt(Data,Data,FNbPixels,False);
  end;
  inherited Serialize(AStream);
  If TBGRAPixel_RGBAOrder then TBGRAFilterScannerSwapRedBlue.ComputeFilterAt(Data,Data,FNbPixels,False);
end;

procedure TBGRADefaultBitmap.Deserialize(AStream: TStream);
begin
  inherited Deserialize(AStream);
  If TBGRAPixel_RGBAOrder then TBGRAFilterScannerSwapRedBlue.ComputeFilterAt(Data,Data,FNbPixels,False);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.SolidBrushIndirect(out
  ABrush: TUniversalBrush; AColor: Pointer; ADrawMode: TDrawMode);
begin
  BGRASolidBrushIndirect(ABrush, AColor, ADrawMode);
end;

class procedure TBGRADefaultBitmap.SolidBrush(out ABrush: TUniversalBrush;
  const AColor: TBGRAPixel; ADrawMode: TDrawMode);
begin
  BGRASolidBrushIndirect(ABrush, @AColor, ADrawMode);
end;

class procedure TBGRADefaultBitmap.ScannerBrush(out ABrush: TUniversalBrush;
  AScanner: IBGRAScanner; ADrawMode: TDrawMode;
  AOffsetX: integer; AOffsetY: integer);
begin
  BGRAScannerBrush(ABrush, AScanner, ADrawMode, AOffsetX, AOffsetY);
end;

class procedure TBGRADefaultBitmap.MaskBrush(out ABrush: TUniversalBrush;
  AScanner: IBGRAScanner; AOffsetX: integer; AOffsetY: integer);
begin
  BGRAMaskBrush(ABrush, AScanner, AOffsetX, AOffsetY);
end;

class procedure TBGRADefaultBitmap.EraseBrush(out ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  BGRAEraseBrush(ABrush, AAlpha);
end;

class procedure TBGRADefaultBitmap.AlphaBrush(out ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  BGRAAlphaBrush(ABrush, AAlpha);
end;

procedure TBGRADefaultBitmap.Assign(Source: TPersistent);
var pdest: PBGRAPixel;
  x,y: Int32or64;
begin
  if Source is TBGRACustomBitmap then
  begin
    DiscardBitmapChange;
    SetSize(TBGRACustomBitmap(Source).Width, TBGRACustomBitmap(Source).Height);
    PutImage(0, 0, TBGRACustomBitmap(Source), dmSet);
    if Source is TBGRADefaultBitmap then
    begin
      HotSpot := TBGRADefaultBitmap(Source).HotSpot;
      if XorMask <> TBGRADefaultBitmap(Source).XorMask then
      begin
        DiscardXorMask;
        if TBGRADefaultBitmap(Source).XorMask is TBGRADefaultBitmap then
          FXorMask := TBGRADefaultBitmap(TBGRADefaultBitmap(Source).XorMask).NewReference as TBGRADefaultBitmap
        else
          FXorMask := TBGRADefaultBitmap(Source).XorMask.Duplicate;
      end;
    end;
  end else
  if Source is TFPCustomImage then
  begin
    DiscardBitmapChange;
    SetSize(TFPCustomImage(Source).Width, TFPCustomImage(Source).Height);
    for y := 0 to TFPCustomImage(Source).Height-1 do
    begin
      pdest := ScanLine[y];
      for x := 0 to TFPCustomImage(Source).Width-1 do
      begin
        pdest^ := FPColorToBGRA(TFPCustomImage(Source).Colors[x,y]);
        inc(pdest);
      end;
    end;
  end else
    inherited Assign(Source);
end;

procedure TBGRADefaultBitmap.Assign(Source: TBitmap; AUseTransparent: boolean);
var
  transpColor: TBGRAPixel;
begin
  Assign(Source);
  if AUseTransparent and TBitmap(Source).Transparent then
  begin
    if TBitmap(Source).TransparentMode = tmFixed then
      transpColor := ColorToBGRA(TBitmap(Source).TransparentColor)
    else
      transpColor := GetPixel(0,Height-1);
    ReplaceColor(transpColor, BGRAPixelTransparent);
  end;
end;

{------------------------- Clipping -------------------------------}

function TBGRADefaultBitmap.InternalGetPixelCycle256(ix, iy: int32or64; iFactX,
  iFactY: int32or64): TBGRAPixel;
var
  ixMod2: int32or64;
  pUpLeft, pUpRight, pDownLeft, pDownRight: PBGRAPixel;
  scan: PBGRAPixel;
begin
  scan := GetScanlineFast(iy);

  pUpLeft := (scan + ix);
  ixMod2 := ix+1;
  if ixMod2=Width then ixMod2 := 0;
  pUpRight := (scan + ixMod2);

  Inc(iy);
  if iy = Height then iy := 0;
  scan := GetScanlineFast(iy);
  pDownLeft := (scan + ix);
  pDownRight := (scan + ixMod2);

  InterpolateBilinear(pUpLeft, pUpRight, pDownLeft,
          pDownRight, iFactX, iFactY, @result);
end;

function TBGRADefaultBitmap.InternalGetPixel256(ix, iy: int32or64; iFactX,
  iFactY: int32or64; smoothBorder: boolean): TBGRAPixel;
var
  pUpLeft, pUpRight, pDownLeft, pDownRight: PBGRAPixel;
  scan: PBGRAPixel;
begin
  if (iy >= 0) and (iy < FHeight) then
  begin
    scan := GetScanlineFast(iy);

    if (ix >= 0) and (ix < FWidth) then
      pUpLeft := scan+ix
    else if smoothBorder then
      pUpLeft := @BGRAPixelTransparent
    else
      pUpLeft := nil;

    if (ix+1 >= 0) and (ix+1 < FWidth) then
      pUpRight := scan+(ix+1)
    else if smoothBorder then
      pUpRight := @BGRAPixelTransparent
    else
      pUpRight := nil;
  end else
  if smoothBorder then
  begin
    pUpLeft := @BGRAPixelTransparent;
    pUpRight := @BGRAPixelTransparent;
  end else
  begin
    pUpLeft := nil;
    pUpRight := nil;
  end;

  if (iy+1 >= 0) and (iy+1 < FHeight) then
  begin
    scan := GetScanlineFast(iy+1);

    if (ix >= 0) and (ix < FWidth) then
      pDownLeft := scan+ix
    else if smoothBorder then
      pDownLeft := @BGRAPixelTransparent
    else
      pDownLeft := nil;

    if (ix+1 >= 0) and (ix+1 < FWidth) then
      pDownRight := scan+(ix+1)
    else if smoothBorder then
      pDownRight := @BGRAPixelTransparent
    else
      pDownRight := nil;
  end else
  if smoothBorder then
  begin
    pDownLeft := @BGRAPixelTransparent;
    pDownRight := @BGRAPixelTransparent;
  end else
  begin
    pDownLeft := nil;
    pDownRight := nil;
  end;

  InterpolateBilinear(pUpLeft, pUpRight, pDownLeft,
          pDownRight, iFactX, iFactY, @result);
end;

{-------------------------- Pixel functions -----------------------------------}

procedure TBGRADefaultBitmap.XorPixel(x, y: int32or64; const c: TBGRAPixel);
var
  p : PLongWord;
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  p := PLongWord(GetScanlineFast(y) +x);
  p^ := p^ xor LongWord(c);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.SetPixel(x, y: int32or64; c: TColor);
var
  p: PBGRAPixel;
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  p  := GetScanlineFast(y) + x;
  RedGreenBlue(c, p^.red,p^.green,p^.blue);
  p^.alpha := 255;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.DrawPixel(x, y: int32or64; const c: TBGRAPixel);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  DrawPixelInlineWithAlphaCheck(GetScanlineFast(y) + x, c);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.FastBlendPixel(x, y: int32or64; const c: TBGRAPixel);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  FastBlendPixelInline(GetScanlineFast(y) + x, c);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.ErasePixel(x, y: int32or64; alpha: byte);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  ErasePixelInline(GetScanlineFast(y) + x, alpha);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.AlphaPixel(x, y: int32or64; alpha: byte);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  if alpha = 0 then
    (GetScanlineFast(y) +x)^ := BGRAPixelTransparent
  else
    (GetScanlineFast(y) +x)^.alpha := alpha;
  InvalidateBitmap;
end;

function TBGRADefaultBitmap.GetPixel256(x, y, fracX256, fracY256: int32or64;
  AResampleFilter: TResampleFilter; smoothBorder: boolean = true): TBGRAPixel;
begin
  if (fracX256 = 0) and (fracY256 = 0) then
    result := GetPixel(x,y)
  else if AResampleFilter = rfBox then
  begin
    if fracX256 >= 128 then inc(x);
    if fracY256 >= 128 then inc(y);
    result := GetPixel(x,y);
  end else
  begin
    LoadFromBitmapIfNeeded;
    result := InternalGetPixel256(x,y,FineInterpolation256(fracX256,AResampleFilter),FineInterpolation256(fracY256,AResampleFilter),smoothBorder);
  end;
end;

{$hints off}
{ This function compute an interpolated pixel at floating point coordinates }
function TBGRADefaultBitmap.GetPixel(x, y: single; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TBGRAPixel;
var
  ix, iy: Int32or64;
  iFactX,iFactY: Int32or64;
begin
  ix := round(x*256);
  if (ix<= -256) or (ix>=Width shl 8) then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  iy := round(y*256);
  if (iy<= -256) or (iy>=Height shl 8) then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;

  iFactX := ix and 255; //distance from integer coordinate
  iFactY := iy and 255;
  if ix<0 then ix := -1 else ix := ix shr 8;
  if iy<0 then iy := -1 else iy := iy shr 8;

  //if the coordinate is integer, then call standard GetPixel function
  if (iFactX = 0) and (iFactY = 0) then
  begin
    Result := (GetScanlineFast(iy)+ix)^;
    exit;
  end;

  LoadFromBitmapIfNeeded;
  result := InternalGetPixel256(ix,iy,FineInterpolation256(iFactX,AResampleFilter),FineInterpolation256(iFactY,AResampleFilter),smoothBorder);
end;

{ Same as GetPixel(single,single,TResampleFilter) but with coordinate cycle, supposing the image repeats itself in both directions }
function TBGRADefaultBitmap.GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel;
var
  ix, iy: Int32or64;
  iFactX,iFactY: Int32or64;
begin
  if FNbPixels = 0 then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  LoadFromBitmapIfNeeded;
  ix := round(x*256);
  iy := round(y*256);
  iFactX := ix and 255;
  iFactY := iy and 255;
  ix := PositiveMod(ix, FWidth shl 8) shr 8;
  iy := PositiveMod(iy, FHeight shl 8) shr 8;
  if (iFactX = 0) and (iFactY = 0) then
  begin
    result := (GetScanlineFast(iy)+ix)^;
    exit;
  end;
  if ScanInterpolationFilter <> rfLinear then
  begin
    iFactX := FineInterpolation256( iFactX, ScanInterpolationFilter );
    iFactY := FineInterpolation256( iFactY, ScanInterpolationFilter );
  end;
  result := InternalGetPixelCycle256(ix,iy, iFactX,iFactY);
end;

function TBGRADefaultBitmap.GetPixelCycle(x, y: single;
  AResampleFilter: TResampleFilter; repeatX: boolean; repeatY: boolean
  ): TBGRAPixel;
var
  ix, iy: Int32or64;
  iFactX,iFactY: Int32or64;
begin
  if FNbPixels = 0 then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  ix := round(x*256);
  iy := round(y*256);
  iFactX := ix and 255;
  iFactY := iy and 255;
  if ix < 0 then ix := -((iFactX-ix) shr 8)
  else ix := ix shr 8;
  if iy < 0 then iy := -((iFactY-iy) shr 8)
  else iy := iy shr 8;
  result := GetPixelCycle256(ix,iy,iFactX,iFactY,AResampleFilter,repeatX,repeatY);
end;

function TBGRADefaultBitmap.GetPixelCycle256(x, y, fracX256,
  fracY256: int32or64; AResampleFilter: TResampleFilter): TBGRAPixel;
begin
  if (fracX256 = 0) and (fracY256 = 0) then
    result := GetPixelCycle(x,y)
  else if AResampleFilter = rfBox then
  begin
    if fracX256 >= 128 then inc(x);
    if fracY256 >= 128 then inc(y);
    result := GetPixelCycle(x,y);
  end else
  begin
    LoadFromBitmapIfNeeded;
    result := InternalGetPixelCycle256(PositiveMod(x,FWidth),PositiveMod(y,FHeight),FineInterpolation256(fracX256,AResampleFilter),FineInterpolation256(fracY256,AResampleFilter));
  end;
end;

function TBGRADefaultBitmap.GetPixelCycle256(x, y, fracX256,
  fracY256: int32or64; AResampleFilter: TResampleFilter; repeatX: boolean;
  repeatY: boolean): TBGRAPixel;
begin
  if not repeatX and not repeatY then
    result := GetPixel256(x,y,fracX256,fracY256,AResampleFilter)
  else if repeatX and repeatY then
    result := GetPixelCycle256(x,y,fracX256,fracY256,AResampleFilter)
  else
  begin
    if not repeatX then
    begin
      if x < 0 then
      begin
        if x < -1 then
        begin
          result := BGRAPixelTransparent;
          exit;
        end;
        result := GetPixelCycle256(0,y,0,fracY256,AResampleFilter);
        result.alpha:= result.alpha*fracX256 shr 8;
        if result.alpha = 0 then
          result := BGRAPixelTransparent;
        exit;
      end;
      if x >= FWidth-1 then
      begin
        if x >= FWidth then
        begin
          result := BGRAPixelTransparent;
          exit;
        end;
        result := GetPixelCycle256(FWidth-1,y,0,fracY256,AResampleFilter);
        result.alpha:= result.alpha*(256-fracX256) shr 8;
        if result.alpha = 0 then
          result := BGRAPixelTransparent;
        exit;
      end;
    end else
    begin
      if y < 0 then
      begin
        if y < -1 then
        begin
          result := BGRAPixelTransparent;
          exit;
        end;
        result := GetPixelCycle256(x,0,fracX256,0,AResampleFilter);
        result.alpha:= result.alpha*fracY256 shr 8;
        if result.alpha = 0 then
          result := BGRAPixelTransparent;
        exit;
      end;
      if y >= FHeight-1 then
      begin
        if y >= FHeight then
        begin
          result := BGRAPixelTransparent;
          exit;
        end;
        result := GetPixelCycle256(x,FHeight-1,fracX256,0,AResampleFilter);
        result.alpha:= result.alpha*(256-fracY256) shr 8;
        if result.alpha = 0 then
          result := BGRAPixelTransparent;
        exit;
      end;
    end;
    result := GetPixelCycle256(x,y,fracX256,fracY256,AResampleFilter);
  end;
end;

{$hints on}

procedure TBGRADefaultBitmap.InvalidateBitmap;
begin
  FDataModified := True;
end;

function TBGRADefaultBitmap.GetBitmap: TBitmap;
begin
  if FAlphaCorrectionNeeded then
  begin
    if CanvasAlphaCorrection then
      LoadFromBitmapIfNeeded
    else
      FAlphaCorrectionNeeded := false;
  end;
  if FDataModified or (FBitmap = nil) then
  begin
    RebuildBitmap;
    FBitmapModified := false;
    FAlphaCorrectionNeeded:= false;
    FDataModified := False;
  end;
  Result := FBitmap;
end;

function TBGRADefaultBitmap.GetCanvas: TCanvas;
begin
  if FDataModified or (FBitmap = nil) then
  begin
    RebuildBitmap;
    FBitmapModified := false;
    FAlphaCorrectionNeeded:= false;
    FDataModified := False;
  end;
  Result := FBitmap.Canvas;
end;

{$IFDEF BGRABITMAP_USE_FPCANVAS}function TBGRADefaultBitmap.GetCanvasFP: TFPImageCanvas;
begin
  {$warnings off}
  if FCanvasFP = nil then
    FCanvasFP := TFPImageCanvas.Create(self);
  {$warnings on}
  result := FCanvasFP;
end;{$ENDIF}

procedure TBGRADefaultBitmap.SetCanvasDrawModeFP(const AValue: TDrawMode);
begin
  FCanvasDrawModeFP := AValue;
  Case AValue of
  dmLinearBlend: FCanvasPixelProcFP := @FastBlendPixel;
  dmDrawWithTransparency: FCanvasPixelProcFP := @DrawPixel;
  dmXor: FCanvasPixelProcFP:= @XorPixel;
  else FCanvasPixelProcFP := @SetPixel;
  end;
end;

function TBGRADefaultBitmap.GetCanvasDrawModeFP: TDrawMode;
begin
  Result:= FCanvasDrawModeFP;
end;

procedure TBGRADefaultBitmap.LoadFromBitmapIfNeeded;
begin
  if FBitmapModified then
  begin
    DoLoadFromBitmap;
    DiscardBitmapChange;
  end;
  if FAlphaCorrectionNeeded then
  begin
    DoAlphaCorrection;
  end;
end;

procedure TBGRADefaultBitmap.CrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadePosition: byte; mode: TDrawMode = dmDrawWithTransparency);
begin
  if AFadePosition = 0 then
    FillRect(ARect, Source1, mode) else
  if AFadePosition = 255 then
    FillRect(ARect, Source2, mode) else
    InternalCrossFade(ARect, Source1,Source2, AFadePosition,nil, mode);
end;

procedure TBGRADefaultBitmap.CrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadeMask: IBGRAScanner; mode: TDrawMode = dmDrawWithTransparency);
begin
  InternalCrossFade(ARect, Source1,Source2, 0,AFadeMask, mode);
end;

procedure TBGRADefaultBitmap.DiscardBitmapChange; inline;
begin
  FBitmapModified := False;
end;

procedure TBGRADefaultBitmap.NotifyBitmapChange;
begin
  FBitmapModified := True;
  FAlphaCorrectionNeeded := true;
end;

{ Initialize properties }
procedure TBGRADefaultBitmap.Init;
begin
  inherited Init;
  FBitmap    := nil;
  {$IFDEF BGRABITMAP_USE_FPCANVAS}FCanvasFP  := nil;{$ENDIF}
  FCanvasBGRA := nil; 
  CanvasDrawModeFP := dmDrawWithTransparency;
  FCanvasOpacity := 255;
  FAlphaCorrectionNeeded := False;

  FontName  := 'Arial';
  FontStyle := [];
  FontAntialias := False;
  FontVerticalAnchor:= fvaTop;
  FFontHeight := 20;

  ResampleFilter := rfHalfCosine;
  ScanInterpolationFilter := rfLinear;
end;

procedure TBGRADefaultBitmap.SetInternalColor(x, y: integer; const Value: TFPColor);
begin
  FCanvasPixelProcFP(x,y, FPColorToBGRA(Value));
end;

function TBGRADefaultBitmap.GetInternalColor(x, y: integer): TFPColor;
begin
  if (x < 0) or (y < 0) or (x >= Width) or (y >= Height) then
    result := colTransparent
  else
  begin
    LoadFromBitmapIfNeeded;
    result := BGRAToFPColor((Scanline[y] + x)^);
  end;
end;

procedure TBGRADefaultBitmap.SetInternalPixel(x, y: integer; Value: integer);
var
  c: TFPColor;
begin
  if not PtInClipRect(x,y) then exit;
  c  := Palette.Color[Value];
  (Scanline[y] + x)^ := FPColorToBGRA(c);
  InvalidateBitmap;
end;

function TBGRADefaultBitmap.GetInternalPixel(x, y: integer): integer;
var
  c: TFPColor;
begin
  if (x < 0) or (y < 0) or (x >= Width) or (y >= Height) then
    result := 0
  else
  begin
    LoadFromBitmapIfNeeded;
    c := BGRAToFPColor((Scanline[y] + x)^);
    Result := palette.IndexOf(c);
  end;
end;

procedure TBGRADefaultBitmap.Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean);
begin
  if (self = nil) or (Width = 0) or (Height = 0) then exit;
  if Opaque then
    DataDrawOpaque(ACanvas, Rect(X, Y, X + Width, Y + Height), Data,
      FLineOrder, FWidth, FHeight)
  else
  begin
    LoadFromBitmapIfNeeded;
    if Empty then
      exit;
    ACanvas.Draw(X, Y, Bitmap);
  end;
end;

procedure TBGRADefaultBitmap.Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean);
begin
  if (self = nil) or (Width = 0) or (Height = 0) then exit;
  if Opaque then
    DataDrawOpaque(ACanvas, Rect, Data, FLineOrder, FWidth, FHeight)
  else
  begin
    LoadFromBitmapIfNeeded;
    ACanvas.StretchDraw(Rect, Bitmap);
  end;
end;

{---------------------------- Line primitives ---------------------------------}

procedure TBGRADefaultBitmap.XorHorizLine(x, y, x2: int32or64; c: TBGRAPixel);
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  XorInline(scanline[y] + x, c, x2 - x + 1);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.DrawHorizLine(x, y, x2: int32or64; ec: TExpandedPixel
  );
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  DrawExpandedPixelsInline(scanline[y] + x, ec, x2 - x + 1);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.FastBlendHorizLine(x, y, x2: int32or64; c: TBGRAPixel);
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  FastBlendPixelsInline(scanline[y] + x, c, x2 - x + 1);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.AlphaHorizLine(x, y, x2: int32or64; alpha: byte);
begin
  if alpha = 0 then
  begin
    SetHorizLine(x, y, x2, BGRAPixelTransparent);
    exit;
  end;
  if not CheckHorizLineBounds(x,y,x2) then exit;
  AlphaFillInline(scanline[y] + x, alpha, x2 - x + 1);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.XorVertLine(x, y, y2: int32or64; c: TBGRAPixel);
var
  n, delta: int32or64;
  p: PBGRAPixel;
begin
  if not CheckVertLineBounds(x,y,y2) then exit;
  if LineOrder = riloTopToBottom then delta := Width else delta := -Width;
  p    := scanline[y] + x;
  for n := y2 - y downto 0 do
  begin
    PLongWord(p)^ := PLongWord(p)^ xor LongWord(c);
    Inc(p, delta);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.DrawVertLine(x, y, y2: int32or64; c: TBGRAPixel);
var
  n, delta: int32or64;
  p: PBGRAPixel;
begin
  if c.alpha = 255 then
  begin
    SetVertLine(x,y,y2,c);
    exit;
  end;
  if not CheckVertLineBounds(x,y,y2) or (c.alpha=0) then exit;
  p    := scanline[y] + x;
  if LineOrder = riloTopToBottom then delta := Width else delta := -Width;
  for n := y2 - y downto 0 do
  begin
    DrawPixelInlineNoAlphaCheck(p, c);
    Inc(p, delta);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.AlphaVertLine(x, y, y2: int32or64; alpha: byte);
var
  n, delta: int32or64;
  p: PBGRAPixel;
begin
  if alpha = 0 then
  begin
    SetVertLine(x, y, y2, BGRAPixelTransparent);
    exit;
  end;
  if not CheckVertLineBounds(x,y,y2) then exit;
  p    := scanline[y] + x;
  if LineOrder = riloTopToBottom then delta := Width else delta := -Width;
  for n := y2 - y downto 0 do
  begin
    p^.alpha := alpha;
    Inc(p, delta);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.FastBlendVertLine(x, y, y2: int32or64; c: TBGRAPixel);
var
  n, delta: int32or64;
  p: PBGRAPixel;
begin
  if not CheckVertLineBounds(x,y,y2) then exit;
  p    := scanline[y] + x;
  if LineOrder = riloTopToBottom then delta := Width else delta := -Width;
  for n := y2 - y downto 0 do
  begin
    FastBlendPixelInline(p, c);
    Inc(p, delta);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.DrawHorizLineDiff(x, y, x2: int32or64;
  c, compare: TBGRAPixel; maxDiff: byte);
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  DrawPixelsInlineDiff(scanline[y] + x, c, x2 - x + 1, compare, maxDiff);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.HorizLineDiff(x, y, x2: int32or64;
  const ABrush: TUniversalBrush; ACompare: TBGRAPixel; AMaxDiffW: word);
var
  pScan: PBGRAPixel;
  ctx: TUniBrushContext;
  sameCount, remain: Int32or64;
  startAlpha, nextAlpha: Word;
  compExpand: TExpandedPixel;
begin
  if ABrush.Colorspace <> Colorspace then RaiseInvalidBrushColorspace;
  if not CheckHorizLineBounds(x,y,x2) then exit;
  LoadFromBitmapIfNeeded;
  pScan := PBGRAPixel(GetPixelAddress(x,y));
  ABrush.MoveTo(@ctx, pScan,x,y);
  remain := x2-x+1;
  compExpand := ACompare.ToExpanded;
  if pScan^ = ACompare then nextAlpha := 65535
  else nextAlpha := (65535 * (AMaxDiffW + 1 - ExpandedDiff(GammaExpansion(pScan^), compExpand)) + (AMaxDiffW + 1) shr 1) div (AMaxDiffW + 1);
  inc(pScan);
  while remain > 0 do
  begin
    startAlpha := nextAlpha;
    sameCount := 1;
    dec(remain);
    while remain > 0 do
    begin
      if pScan^ = ACompare then nextAlpha := 65535
      else nextAlpha := (65535 * (AMaxDiffW + 1 - ExpandedDiff(GammaExpansion(pScan^), compExpand)) + (AMaxDiffW + 1) shr 1) div (AMaxDiffW + 1);
      inc(pScan);
      if nextAlpha = startAlpha then
      begin
        inc(sameCount);
        dec(remain);
      end else break;
    end;
    ABrush.PutNextPixels(@ctx, startAlpha, sameCount);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.InternalTextOutCurved(
  ACursor: TBGRACustomPathCursor; sUTF8: string; AColor: TBGRAPixel;
  ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single);
var
  glyphCursor: TGlyphCursorUtf8;
  currentGlyph: TGlyphUtf8;
  currentGlyphUtf8: string;
  currentGlyphWidth: single;
  angle, textLen: single;

  procedure NextGlyph;
  begin
    currentGlyph := glyphCursor.GetNextGlyph;
    if currentGlyph.MirroredGlyphUtf8 <> '' then
      currentGlyphUtf8:= currentGlyph.MirroredGlyphUtf8
      else currentGlyphUtf8 := currentGlyph.GlyphUtf8;
    currentGlyphWidth := TextSize(currentGlyphUtf8).cx;
  end;

begin
  if (ATexture = nil) and (AColor.alpha = 0) then exit;
  sUTF8 := CleanTextOutString(sUTF8);
  if sUTF8 = '' then exit;
  glyphCursor := TGlyphCursorUtf8.New(sUTF8, FontBidiMode);

  if AAlign<> taLeftJustify then
  begin
    textLen := -ALetterSpacing;
    while not glyphCursor.EndOfString do
    begin
      NextGlyph;
      IncF(textLen, ALetterSpacing + currentGlyphWidth);
    end;
    case AAlign of
      taCenter: ACursor.MoveBackward(textLen*0.5);
      taRightJustify: ACursor.MoveBackward(textLen);
    end;
    glyphCursor.Rewind;
  end;

  while not glyphCursor.EndOfString do
  begin
    NextGlyph;
    ACursor.MoveForward(currentGlyphWidth);
    ACursor.MoveBackward(currentGlyphWidth, false);
    ACursor.MoveForward(currentGlyphWidth*0.5);
    with ACursor.CurrentTangent do angle := arctan2(y,x);
    with ACursor.CurrentCoordinate do
    begin
      if ATexture = nil then
        TextOutAngle(x,y, system.round(-angle*1800/Pi), currentGlyphUtf8, AColor, taCenter)
      else
        TextOutAngle(x,y, system.round(-angle*1800/Pi), currentGlyphUtf8, ATexture, taCenter);
    end;
    ACursor.MoveForward(currentGlyphWidth*0.5 + ALetterSpacing);
  end;
end;

procedure TBGRADefaultBitmap.InternalTextOutLetterSpacing(x, y: single;
  sUTF8: string; AColor: TBGRAPixel; ATexture: IBGRAScanner;
  AAlign: TAlignment; ALetterSpacing: single);
var
  glyphCursor: TGlyphCursorUtf8;
  currentGlyph: TGlyphUtf8;
  currentGlyphUtf8: string;
  currentGlyphWidth: single;
  angle, textLen: single;
  m: TAffineMatrix;
  ofs: TPointF;

  procedure NextGlyph;
  begin
    currentGlyph := glyphCursor.GetNextGlyph;
    if currentGlyph.MirroredGlyphUtf8 <> '' then
      currentGlyphUtf8:= currentGlyph.MirroredGlyphUtf8
      else currentGlyphUtf8 := currentGlyph.GlyphUtf8;
    currentGlyphWidth := TextSize(currentGlyphUtf8).cx;
  end;

begin
  if (ATexture = nil) and (AColor.alpha = 0) then exit;
  sUTF8 := CleanTextOutString(sUTF8);
  if sUTF8 = '' then exit;
  glyphCursor := TGlyphCursorUtf8.New(sUTF8, FontBidiMode);

  ofs := PointF(0, 0);
  if AAlign<> taLeftJustify then
  begin
    textLen := -ALetterSpacing;
    while not glyphCursor.EndOfString do
    begin
      NextGlyph;
      IncF(textLen, ALetterSpacing + currentGlyphWidth);
    end;
    case AAlign of
      taCenter: DecF(ofs.x, 0.5*textLen);
      taRightJustify: DecF(ofs.x, textLen);
    end;
    glyphCursor.Rewind;
  end;
  m := AffineMatrixRotationDeg(-FontOrientation*0.1);
  ofs := m*ofs;
  incF(x, ofs.x);
  incF(y, ofs.y);

  while not glyphCursor.EndOfString do
  begin
    NextGlyph;
    if ATexture = nil then
      TextOut(x,y, currentGlyphUtf8, AColor, taLeftJustify, currentGlyph.RightToLeft)
    else
      TextOut(x,y, currentGlyphUtf8, ATexture, taLeftJustify, currentGlyph.RightToLeft);
    ofs := m*PointF(currentGlyphWidth + ALetterSpacing, 0);
    incF(x, ofs.x);
    incF(y, ofs.y);
  end;
end;

procedure TBGRADefaultBitmap.InternalCrossFade(ARect: TRect; Source1,
  Source2: IBGRAScanner; AFadePos: byte; AFadeMask: IBGRAScanner; mode: TDrawMode);
var xb,yb: Int32or64;
  pdest: PBGRAPixel;
  c: TBGRAPixel;
  buf1,buf2: ArrayOfTBGRAPixel;
begin
  ARect.Intersect(ClipRect);
  if ARect.IsEmpty then exit;
  setlength(buf1, ARect.Width);
  setlength(buf2, ARect.Width);
  for yb := ARect.top to ARect.Bottom-1 do
  begin
    pdest := GetScanlineFast(yb)+ARect.Left;
    Source1.ScanMoveTo(ARect.left, yb);
    Source1.ScanPutPixels(@buf1[0], length(buf1), dmSet);
    Source2.ScanMoveTo(ARect.left, yb);
    Source2.ScanPutPixels(@buf2[0], length(buf2), dmSet);
    if AFadeMask<>nil then AFadeMask.ScanMoveTo(ARect.left, yb);
    for xb := 0 to ARect.Right-ARect.left-1 do
    begin
      if AFadeMask<>nil then AFadePos := AFadeMask.ScanNextPixel.green;
      c := MergeBGRAWithGammaCorrection(buf1[xb],not AFadePos,buf2[xb],AFadePos);
      case mode of
      dmSet: pdest^ := c;
      dmDrawWithTransparency: DrawPixelInlineWithAlphaCheck(pdest, c);
      dmLinearBlend: FastBlendPixelInline(pdest,c);
      dmSetExceptTransparent: if c.alpha = 255 then pdest^ := c;
      end;
      inc(pdest);
    end;
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.InternalArc(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; AOptions: TArcOptions;
  ADrawChord: boolean; ATexture: IBGRAScanner);
var
  pts, ptsFill: array of TPointF;
  temp: single;
  multi: TBGRAMultishapeFiller;
begin
  if (rx = 0) or (ry = 0) then exit;
  if ADrawChord then AOptions := AOptions+[aoClosePath];
  if not (aoFillPath in AOptions) then
    AFillColor := BGRAPixelTransparent;

  if (ABorderColor.alpha = 0) and (AFillColor.alpha = 0) then exit;

  if (abs(StartAngleRad-EndAngleRad) >= 2*PI - 1e-6) or (StartAngleRad = EndAngleRad) then
  begin
    if (aoPie in AOptions) or ((PenStyle <> psSolid) and (PenStyle <> psClear)) then
      EndAngleRad:= StartAngleRad+2*PI
    else
    begin
      EllipseAntialias(cx,cy,rx,ry,ABorderColor,w,AFillColor);
      exit;
    end;
  end;

  if EndAngleRad < StartAngleRad then
  begin
    temp := StartAngleRad;
    StartAngleRad:= EndAngleRad;
    EndAngleRad:= temp;
  end;

  pts := ComputeArcRad(cx,cy,rx,ry,StartAngleRad,EndAngleRad);
  if aoPie in AOptions then pts := ConcatPointsF([PointsF([PointF(cx,cy)]),pts]);

  multi := TBGRAMultishapeFiller.Create;
  multi.FillMode := fmWinding;
  multi.PolygonOrder := poLastOnTop;
  if AFillColor.alpha <> 0 then
  begin
    if not (aoPie in AOptions) and (length(pts)>=2) then ptsFill := ConcatPointsF([PointsF([(pts[0]+pts[high(pts)])*0.5]),pts])
     else ptsFill := pts;
    if ATexture <> nil then
      multi.AddPolygon(ptsFill, ATexture)
    else
      multi.AddPolygon(ptsFill, AFillColor);
  end;
  if ABorderColor.alpha <> 0 then
  begin
    if [aoPie,aoClosePath]*AOptions <> [] then
      multi.AddPolygon(ComputeWidePolygon(pts,w), ABorderColor)
    else
      multi.AddPolygon(ComputeWidePolyline(pts,w), ABorderColor);
  end;
  multi.Antialiasing := true;
  multi.Draw(self);
  multi.Free;
end;

function TBGRADefaultBitmap.InternalNew: TBGRADefaultBitmap;
var
  BGRAClass: TBGRABitmapAny;
begin
  BGRAClass := TBGRABitmapAny(self.ClassType);
  if BGRAClass = TBGRAPtrBitmap then
    BGRAClass := TBGRADefaultBitmap;
  Result      := BGRAClass.Create(0, 0) as TBGRADefaultBitmap;
end;

class function TBGRADefaultBitmap.IsAffineRoughlyTranslation(AMatrix: TAffineMatrix; ASourceBounds: TRect): boolean;
const oneOver512 = 1/512;
var Orig,HAxis,VAxis: TPointF;
begin
  Orig := AMatrix*PointF(ASourceBounds.Left,ASourceBounds.Top);
  if (abs(Orig.x-round(Orig.x)) > oneOver512) or
     (abs(Orig.y-round(Orig.y)) > oneOver512) then
  begin
    result := false;
    exit;
  end;
  HAxis := AMatrix*PointF(ASourceBounds.Right-1,ASourceBounds.Top);
  if (abs(HAxis.x - (round(Orig.x)+ASourceBounds.Right-1 - ASourceBounds.Left)) > oneOver512) or
     (abs(HAxis.y - round(Orig.y)) > oneOver512) then
  begin
    result := false;
    exit;
  end;
  VAxis := AMatrix*PointF(ASourceBounds.Left,ASourceBounds.Bottom-1);
  if (abs(VAxis.y - (round(Orig.y)+ASourceBounds.Bottom-1 - ASourceBounds.Top)) > oneOver512) or
     (abs(VAxis.x - round(Orig.x)) > oneOver512) then
  begin
    result := false;
    exit;
  end;
  result := true;
end;

{---------------------------- Lines ---------------------------------}
{ Call appropriate functions }

procedure TBGRADefaultBitmap.DrawPolyLineAntialias(
  const points: array of TPointF; c: TBGRAPixel; w: single;
  fillcolor: TBGRAPixel);
var multi: TBGRAMultishapeFiller;
begin
  multi := TBGRAMultishapeFiller.Create;
  multi.PolygonOrder := poLastOnTop;
  multi.AddPolygon(points,fillcolor);
  multi.AddPolygon(ComputeWidePolyline(points,w),c);
  if LinearAntialiasing then
    multi.Draw(self,dmLinearBlend)
  else
    multi.Draw(self,dmDrawWithTransparency);
  multi.Free;
end;

procedure TBGRADefaultBitmap.DrawPolygonAntialias(
  const points: array of TPointF; c: TBGRAPixel; w: single;
  fillcolor: TBGRAPixel);
var multi: TBGRAMultishapeFiller;
begin
  multi := TBGRAMultishapeFiller.Create;
  multi.PolygonOrder := poLastOnTop;
  multi.AddPolygon(points,fillcolor);
  multi.AddPolygon(ComputeWidePolygon(points,w),c);
  if LinearAntialiasing then
    multi.Draw(self,dmLinearBlend)
  else
    multi.Draw(self,dmDrawWithTransparency);
  multi.Free;
end;

procedure TBGRADefaultBitmap.DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix;
  AStrokeColor: TBGRAPixel; AWidth: single; AFillColor: TBGRAPixel);
var tempPath: TBGRAPath;
  multi: TBGRAMultishapeFiller;
begin
  tempPath := TBGRAPath.Create(APath);
  multi := TBGRAMultishapeFiller.Create;
  multi.FillMode := FillMode;
  multi.PolygonOrder := poLastOnTop;
  multi.AddPathFill(tempPath,AMatrix,AFillColor);
  multi.AddPathStroke(tempPath,AMatrix,AStrokeColor,AWidth,GetInternalPen);
  multi.Draw(self);
  multi.Free;
  tempPath.Free;
end;

procedure TBGRADefaultBitmap.DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix;
  AStrokeTexture: IBGRAScanner; AWidth: single; AFillColor: TBGRAPixel);
var tempPath: TBGRAPath;
  multi: TBGRAMultishapeFiller;
begin
  tempPath := TBGRAPath.Create(APath);
  multi := TBGRAMultishapeFiller.Create;
  multi.FillMode := FillMode;
  multi.PolygonOrder := poLastOnTop;
  multi.AddPathFill(tempPath,AMatrix,AFillColor);
  multi.AddPathStroke(tempPath,AMatrix,AStrokeTexture,AWidth,GetInternalPen);
  multi.Draw(self);
  multi.Free;
  tempPath.Free;
end;

procedure TBGRADefaultBitmap.DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix;
  AStrokeColor: TBGRAPixel; AWidth: single; AFillTexture: IBGRAScanner);
var tempPath: TBGRAPath;
  multi: TBGRAMultishapeFiller;
begin
  tempPath := TBGRAPath.Create(APath);
  multi := TBGRAMultishapeFiller.Create;
  multi.FillMode := FillMode;
  multi.PolygonOrder := poLastOnTop;
  multi.AddPathFill(tempPath,AMatrix,AFillTexture);
  multi.AddPathStroke(tempPath,AMatrix,AStrokeColor,AWidth,GetInternalPen);
  multi.Draw(self);
  multi.Free;
  tempPath.Free;
end;

procedure TBGRADefaultBitmap.DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix;
  AStrokeTexture: IBGRAScanner; AWidth: single; AFillTexture: IBGRAScanner);
var
  tempPath: TBGRAPath;
  multi: TBGRAMultishapeFiller;
begin
  tempPath := TBGRAPath.Create(APath);
  multi := TBGRAMultishapeFiller.Create;
  multi.FillMode := FillMode;
  multi.PolygonOrder := poLastOnTop;
  multi.AddPathFill(tempPath,AMatrix,AFillTexture);
  multi.AddPathStroke(tempPath,AMatrix,AStrokeTexture,AWidth,GetInternalPen);
  multi.Draw(self);
  multi.Free;
  tempPath.Free;
end;

procedure TBGRADefaultBitmap.ArrowStartAsNone;
begin
  GetArrow.StartAsNone;
end;

procedure TBGRADefaultBitmap.ArrowStartAsClassic(AFlipped: boolean;
  ACut: boolean; ARelativePenWidth: single);
begin
  GetArrow.StartAsClassic(AFlipped,ACut,ARelativePenWidth);
end;

procedure TBGRADefaultBitmap.ArrowStartAsTriangle(ABackOffset: single;
  ARounded: boolean; AHollow: boolean; AHollowPenWidth: single);
begin
  GetArrow.StartAsTriangle(ABackOffset,ARounded,AHollow,AHollowPenWidth);
end;

procedure TBGRADefaultBitmap.ArrowStartAsTail;
begin
  GetArrow.StartAsTail;
end;

procedure TBGRADefaultBitmap.ArrowEndAsNone;
begin
  GetArrow.EndAsNone;
end;

procedure TBGRADefaultBitmap.ArrowEndAsClassic(AFlipped: boolean;
  ACut: boolean; ARelativePenWidth: single);
begin
  GetArrow.EndAsClassic(AFlipped,ACut,ARelativePenWidth);
end;

procedure TBGRADefaultBitmap.ArrowEndAsTriangle(ABackOffset: single;
  ARounded: boolean; AHollow: boolean; AHollowPenWidth: single);
begin
  GetArrow.EndAsTriangle(ABackOffset,ARounded,AHollow,AHollowPenWidth);
end;

procedure TBGRADefaultBitmap.ArrowEndAsTail;
begin
  GetArrow.EndAsTail;
end;

{------------------------ Shapes ----------------------------------------------}
{ Call appropriate functions }

procedure TBGRADefaultBitmap.FillTriangleLinearColor(pt1, pt2, pt3: TPointF;
  c1, c2, c3: TBGRAPixel);
begin
  FillPolyLinearColor([pt1,pt2,pt3],[c1,c2,c3]);
end;

procedure TBGRADefaultBitmap.FillTriangleLinearColorAntialias(pt1, pt2,
  pt3: TPointF; c1, c2, c3: TBGRAPixel);
var
  grad: TBGRAGradientTriangleScanner;
begin
  grad := TBGRAGradientTriangleScanner.Create(pt1,pt2,pt3, c1,c2,c3);
  FillPolyAntialias([pt1,pt2,pt3],grad);
  grad.Free;
end;

procedure TBGRADefaultBitmap.FillTriangleLinearMapping(pt1, pt2, pt3: TPointF;
  texture: IBGRAScanner; tex1, tex2, tex3: TPointF; TextureInterpolation: Boolean= True);
begin
  FillPolyLinearMapping([pt1,pt2,pt3],texture,[tex1,tex2,tex3],TextureInterpolation);
end;

procedure TBGRADefaultBitmap.FillTriangleLinearMappingLightness(pt1, pt2,
  pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; light1,
  light2, light3: word; TextureInterpolation: Boolean);
begin
  FillPolyLinearMappingLightness([pt1,pt2,pt3],texture,[tex1,tex2,tex3],[light1,light2,light3],TextureInterpolation);
end;

procedure TBGRADefaultBitmap.FillTriangleLinearMappingAntialias(pt1, pt2,
  pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF);
var
  mapping: TBGRATriangleLinearMapping;
begin
  mapping := TBGRATriangleLinearMapping.Create(texture, pt1,pt2,pt3, tex1, tex2, tex3);
  FillPolyAntialias([pt1,pt2,pt3],mapping);
  mapping.Free;
end;

procedure TBGRADefaultBitmap.FillQuadLinearColor(pt1, pt2, pt3, pt4: TPointF;
  c1, c2, c3, c4: TBGRAPixel);
var
  center: TPointF;
  centerColor: TBGRAPixel;
  multi: TBGRAMultishapeFiller;
begin
  if not IsConvex([pt1,pt2,pt3,pt4]) then //need to merge colors
  begin
    multi := TBGRAMultishapeFiller.Create;
    multi.AddQuadLinearColor(pt1,pt2,pt3,pt4,c1,c2,c3,c4);
    multi.Antialiasing:= false;
    multi.Draw(self);
    multi.Free;
    exit;
  end;
  center := (pt1+pt2+pt3+pt4)*(1/4);
  centerColor := GammaCompression( MergeBGRA(MergeBGRA(GammaExpansion(c1),GammaExpansion(c2)),
                    MergeBGRA(GammaExpansion(c3),GammaExpansion(c4))) );
  FillTriangleLinearColor(pt1,pt2,center, c1,c2,centerColor);
  FillTriangleLinearColor(pt2,pt3,center, c2,c3,centerColor);
  FillTriangleLinearColor(pt3,pt4,center, c3,c4,centerColor);
  FillTriangleLinearColor(pt4,pt1,center, c4,c1,centerColor);
end;

procedure TBGRADefaultBitmap.FillQuadLinearColorAntialias(pt1, pt2, pt3,
  pt4: TPointF; c1, c2, c3, c4: TBGRAPixel);
var multi : TBGRAMultishapeFiller;
begin
  multi := TBGRAMultishapeFiller.Create;
  multi.AddQuadLinearColor(pt1, pt2, pt3, pt4, c1, c2, c3, c4);
  multi.Draw(self);
  multi.free;
end;

procedure TBGRADefaultBitmap.FillQuadLinearMapping(pt1, pt2, pt3, pt4: TPointF;
  texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF;
  TextureInterpolation: Boolean; ACulling: TFaceCulling; ACropToPolygon: boolean);
var
  scan: TBGRAQuadLinearScanner;
  r: TRect;
begin
  if ((abs(pt1.y-pt2.y)<1e-6) and (abs(pt3.y-pt4.y)<1e-6)) or
     ((abs(pt3.y-pt2.y)<1e-6) and (abs(pt1.y-pt4.y)<1e-6)) then
     FillPolyLinearMapping([pt1,pt2,pt3,pt4], texture,
            [tex1,tex2,tex3,tex4], TextureInterpolation)
  else
  begin
    scan := TBGRAQuadLinearScanner.Create(texture,
         [tex1,tex2,tex3,tex4],
         [pt1,pt2,pt3,pt4],TextureInterpolation);
    scan.Culling := ACulling;
    if ACropToPolygon then
    begin
      scan.Padding := true;
      FillPoly([pt1,pt2,pt3,pt4],scan,dmDrawWithTransparency);
    end
    else
    begin
      r := RectWithSize(floor(pt1.x),floor(pt1.y),1,1);
      r.Union( RectWithSize(floor(pt2.x),floor(pt2.y),1,1) );
      r.Union( RectWithSize(floor(pt3.x),floor(pt3.y),1,1) );
      r.Union( RectWithSize(floor(pt4.x),floor(pt4.y),1,1) );
      FillRect(r,scan,dmDrawWithTransparency);
    end;
    scan.Free;
  end;
end;

procedure TBGRADefaultBitmap.FillQuadLinearMappingLightness(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; light1,
  light2, light3, light4: word; TextureInterpolation: Boolean);
var
  center: TPointF;
  centerTex: TPointF;
  centerLight: word;
begin
  center := (pt1+pt2+pt3+pt4)*(1/4);
  centerTex := (tex1+tex2+tex3+tex4)*(1/4);
  centerLight := (light1+light2+light3+light4) div 4;
  FillTriangleLinearMappingLightness(pt1,pt2,center, texture,tex1,tex2,centerTex, light1,light2,centerLight, TextureInterpolation);
  FillTriangleLinearMappingLightness(pt2,pt3,center, texture,tex2,tex3,centerTex, light2,light3,centerLight, TextureInterpolation);
  FillTriangleLinearMappingLightness(pt3,pt4,center, texture,tex3,tex4,centerTex, light3,light4,centerLight, TextureInterpolation);
  FillTriangleLinearMappingLightness(pt4,pt1,center, texture,tex4,tex1,centerTex, light4,light1,centerLight, TextureInterpolation);
end;

procedure TBGRADefaultBitmap.FillQuadLinearMappingAntialias(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF;
  ACulling: TFaceCulling);
var multi : TBGRAMultishapeFiller;
begin
  multi := TBGRAMultishapeFiller.Create;
  multi.AddQuadLinearMapping(pt1, pt2, pt3, pt4, texture, tex1,tex2,tex3,tex4, ACulling);
  multi.Draw(self);
  multi.free;
end;

procedure TBGRADefaultBitmap.FillQuadPerspectiveMapping(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF;
  ADrawMode: TDrawMode);
var
  persp: TBGRAPerspectiveScannerTransform;
begin
  persp := TBGRAPerspectiveScannerTransform.Create(texture,[tex1,tex2,tex3,tex4],[pt1,pt2,pt3,pt4]);
  FillPoly([pt1,pt2,pt3,pt4],persp,ADrawMode);
  persp.Free;
end;

procedure TBGRADefaultBitmap.FillQuadPerspectiveMapping(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF;
  ACleanBorders: TRect; ADrawMode: TDrawMode);
var
  persp: TBGRAPerspectiveScannerTransform;
  clean: TBGRAExtendedBorderScanner;
begin
  clean := TBGRAExtendedBorderScanner.Create(texture,ACleanBorders);
  persp := TBGRAPerspectiveScannerTransform.Create(clean,[tex1,tex2,tex3,tex4],[pt1,pt2,pt3,pt4]);
  FillPoly([pt1,pt2,pt3,pt4],persp,ADrawMode);
  persp.Free;
  clean.Free;
end;

procedure TBGRADefaultBitmap.FillQuadPerspectiveMappingAntialias(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF);
var
  persp: TBGRAPerspectiveScannerTransform;
begin
  persp := TBGRAPerspectiveScannerTransform.Create(texture,[tex1,tex2,tex3,tex4],[pt1,pt2,pt3,pt4]);
  FillPolyAntialias([pt1,pt2,pt3,pt4],persp);
  persp.Free;
end;

procedure TBGRADefaultBitmap.FillQuadPerspectiveMappingAntialias(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF;
  ACleanBorders: TRect);
var
  persp: TBGRAPerspectiveScannerTransform;
  clean: TBGRAExtendedBorderScanner;
begin
  clean := TBGRAExtendedBorderScanner.Create(texture,ACleanBorders);
  persp := TBGRAPerspectiveScannerTransform.Create(clean,[tex1,tex2,tex3,tex4],[pt1,pt2,pt3,pt4]);
  FillPolyAntialias([pt1,pt2,pt3,pt4],persp);
  persp.Free;
  clean.Free;
end;

procedure TBGRADefaultBitmap.FillQuadAffineMapping(Orig, HAxis, VAxis: TPointF;
  AImage: TBGRACustomBitmap; APixelCenteredCoordinates: boolean; ADrawMode: TDrawMode; AOpacity: byte);
var pts3: TPointF;
  affine: TBGRAAffineBitmapTransform;
begin
  if not APixelCenteredCoordinates then
  begin
    Orig.Offset(-0.5,-0.5);
    HAxis.Offset(-0.5,-0.5);
    VAxis.Offset(-0.5,-0.5);
  end;
  pts3 := HAxis+(VAxis-Orig);
  affine := TBGRAAffineBitmapTransform.Create(AImage,False,AImage.ScanInterpolationFilter,not APixelCenteredCoordinates);
  affine.GlobalOpacity:= AOpacity;
  affine.Fit(Orig,HAxis,VAxis);
  FillPoly([Orig,HAxis,pts3,VAxis],affine,ADrawMode);
  affine.Free;
end;

procedure TBGRADefaultBitmap.FillQuadAffineMappingAntialias(Orig, HAxis,
  VAxis: TPointF; AImage: TBGRACustomBitmap; APixelCenteredCoordinates: boolean; AOpacity: byte);
var pts3: TPointF;
  affine: TBGRAAffineBitmapTransform;
begin
  if not APixelCenteredCoordinates then
  begin
    Orig.Offset(-0.5,-0.5);
    HAxis.Offset(-0.5,-0.5);
    VAxis.Offset(-0.5,-0.5);
  end;
  pts3 := HAxis+(VAxis-Orig);
  affine := TBGRAAffineBitmapTransform.Create(AImage,False,AImage.ScanInterpolationFilter,not APixelCenteredCoordinates);
  affine.GlobalOpacity:= AOpacity;
  affine.Fit(Orig,HAxis,VAxis);
  FillPolyAntialias([Orig,HAxis,pts3,VAxis],affine);
  affine.Free;
end;

procedure TBGRADefaultBitmap.FillPolyLinearMapping(const points: array of TPointF;
  texture: IBGRAScanner; texCoords: array of TPointF;
  TextureInterpolation: Boolean);
begin
  PolygonLinearTextureMappingAliased(self,points,texture,texCoords,TextureInterpolation, FillMode = fmWinding);
end;

procedure TBGRADefaultBitmap.FillPolyLinearMappingLightness(
  const points: array of TPointF; texture: IBGRAScanner;
  texCoords: array of TPointF; lightnesses: array of word;
  TextureInterpolation: Boolean);
begin
  PolygonLinearTextureMappingAliasedWithLightness(self,points,texture,texCoords,TextureInterpolation,lightnesses,FillMode = fmWinding);
end;

procedure TBGRADefaultBitmap.FillPolyLinearColor(
  const points: array of TPointF; AColors: array of TBGRAPixel);
begin
  PolygonLinearColorGradientAliased(self,points,AColors, FillMode = fmWinding);
end;

procedure TBGRADefaultBitmap.FillPolyPerspectiveMapping(
  const points: array of TPointF; const pointsZ: array of single;
  texture: IBGRAScanner; texCoords: array of TPointF;
  TextureInterpolation: Boolean; zbuffer: psingle);
begin
  PolygonPerspectiveTextureMappingAliased(self,points,pointsZ,texture,texCoords,TextureInterpolation, FillMode = fmWinding, zbuffer);
end;

procedure TBGRADefaultBitmap.FillPolyPerspectiveMappingLightness(
  const points: array of TPointF; const pointsZ: array of single;
  texture: IBGRAScanner; texCoords: array of TPointF;
  lightnesses: array of word; TextureInterpolation: Boolean; zbuffer: psingle);
begin
  PolygonPerspectiveTextureMappingAliasedWithLightness(self,points,pointsZ,texture,texCoords,TextureInterpolation,lightnesses, FillMode = fmWinding, zbuffer);
end;

procedure TBGRADefaultBitmap.DrawPath(APath: IBGRAPath;
  AStrokeColor: TBGRAPixel; AWidth: single; AFillColor: TBGRAPixel);
begin
  DrawPath(APath,AffineMatrixIdentity,AStrokeColor,AWidth,AFillColor);
end;

procedure TBGRADefaultBitmap.DrawPath(APath: IBGRAPath;
  AStrokeTexture: IBGRAScanner; AWidth: single; AFillColor: TBGRAPixel);
begin
  DrawPath(APath,AffineMatrixIdentity,AStrokeTexture,AWidth,AFillColor);
end;

procedure TBGRADefaultBitmap.DrawPath(APath: IBGRAPath;
  AStrokeColor: TBGRAPixel; AWidth: single; AFillTexture: IBGRAScanner);
begin
  DrawPath(APath,AffineMatrixIdentity,AStrokeColor,AWidth,AFillTexture);
end;

procedure TBGRADefaultBitmap.DrawPath(APath: IBGRAPath;
  AStrokeTexture: IBGRAScanner; AWidth: single; AFillTexture: IBGRAScanner);
begin
  DrawPath(APath,AffineMatrixIdentity,AStrokeTexture,AWidth,AFillTexture);
end;

procedure TBGRADefaultBitmap.EllipseAntialias(x, y, rx, ry: single;
  c: TBGRAPixel; w: single; back: TBGRAPixel);
var multi: TBGRAMultishapeFiller;
    hw: single;
begin
  if (w=0) or (PenStyle = psClear) or (c.alpha = 0) then
  begin
    FillEllipseAntialias(x, y, rx, ry, back);
    exit;
  end;
  rx := abs(rx);
  ry := abs(ry);
  hw := w/2;
  if (rx <= hw) or (ry <= hw) then
  begin
    FillEllipseAntialias(x,y,rx+hw,ry+hw,c);
    exit;
  end;
  { use multishape filler for fine junction between polygons }
  multi := TBGRAMultishapeFiller.Create;
  if (PenStyle = psSolid) then
  begin
    if back.alpha <> 0 then multi.AddEllipse(x,y,rx-hw,ry-hw,back);
    multi.AddEllipseBorder(x,y,rx,ry,w,c)
  end
  else
  begin
    if back.alpha <> 0 then multi.AddEllipse(x,y,rx,ry,back);
    multi.AddPolygon(ComputeWidePolygon(ComputeEllipseContour(x,y,rx,ry),w),c);
  end;
  multi.PolygonOrder := poLastOnTop;
  multi.Draw(self);
  multi.Free;
end;

procedure TBGRADefaultBitmap.EllipseAntialias(AOrigin, AXAxis, AYAxis: TPointF;
  c: TBGRAPixel; w: single; back: TBGRAPixel);
var multi: TBGRAMultishapeFiller;
    pts: ArrayOfTPointF;
begin
  if (w=0) or (PenStyle = psClear) or (c.alpha = 0) then
  begin
    FillEllipseAntialias(AOrigin, AXAxis, AYAxis, back);
    exit;
  end;
  { use multishape filler for fine junction between polygons }
  multi := TBGRAMultishapeFiller.Create;
  pts := ComputeEllipseContour(AOrigin, AXAxis, AYAxis);
  if back.alpha <> 0 then multi.AddPolygon(pts, back);
  pts := ComputeWidePolygon(pts,w);
  multi.AddPolygon(pts,c);
  multi.PolygonOrder := poLastOnTop;
  multi.Draw(self);
  multi.Free;
end;

procedure TBGRADefaultBitmap.FillEllipseLinearColorAntialias(x, y, rx,
  ry: single; outercolor, innercolor: TBGRAPixel);
var
    grad: TBGRAGradientScanner;
    affine: TBGRAAffineScannerTransform;
begin
  if (rx=0) or (ry=0) then exit;
  if rx=ry then
  begin
    grad := TBGRAGradientScanner.Create(innercolor,outercolor,gtRadial,PointF(x,y),PointF(x+rx,y),True);
    FillEllipseAntialias(x,y,rx,ry,grad);
    grad.Free;
  end else
  begin
    grad := TBGRAGradientScanner.Create(innercolor,outercolor,gtRadial,PointF(0,0),PointF(1,0),True);
    affine := TBGRAAffineScannerTransform.Create(grad);
    affine.Scale(rx,ry);
    affine.Translate(x,y);
    FillEllipseAntialias(x,y,rx,ry,affine);
    affine.Free;
    grad.Free;
  end;
end;

procedure TBGRADefaultBitmap.FillEllipseLinearColorAntialias(AOrigin, AXAxis,
  AYAxis: TPointF; outercolor, innercolor: TBGRAPixel);
var
  grad: TBGRAGradientScanner;
  affine: TBGRAAffineScannerTransform;
begin
  grad := TBGRAGradientScanner.Create(innercolor,outercolor,gtRadial,PointF(0,0),PointF(1,0),True);
  affine := TBGRAAffineScannerTransform.Create(grad);
  affine.Fit(AOrigin,AXAxis,AYAxis);
  FillEllipseAntialias(AOrigin,AXAxis,AYAxis,affine);
  affine.Free;
  grad.Free;
end;

procedure TBGRADefaultBitmap.RectangleAntialias(x, y, x2, y2: single;
  c: TBGRAPixel; w: single; back: TBGRAPixel);
var
  bevel: single;
  multi: TBGRAMultishapeFiller;
  hw: single;
begin
  if (PenStyle = psClear) or (c.alpha=0) or (w=0) then
  begin
    if back <> BGRAPixelTransparent then
      FillRectAntialias(x,y,x2,y2,back);
    exit;
  end;

  hw := w/2;
  if not CheckAntialiasRectBounds(x,y,x2,y2,w) then
  begin
    if JoinStyle = pjsBevel then
    begin
      bevel := (2-sqrt(2))*hw;
      FillRoundRectAntialias(x - hw, y - hw, x2 + hw, y2 + hw, bevel,bevel, c, [rrTopLeftBevel, rrTopRightBevel, rrBottomLeftBevel, rrBottomRightBevel]);
    end else
    if JoinStyle = pjsRound then
     FillRoundRectAntialias(x - hw, y - hw, x2 + hw, y2 + hw, hw,hw, c)
    else
     FillRectAntialias(x - hw, y - hw, x2 + hw, y2 + hw, c);
    exit;
  end;

  { use multishape filler for fine junction between polygons }
  multi := TBGRAMultishapeFiller.Create;
  multi.FillMode := FillMode;
  if (JoinStyle = pjsMiter) and (PenStyle = psSolid) then
    multi.AddRectangleBorder(x,y,x2,y2,w,c)
  else
    multi.AddPolygon(ComputeWidePolygon([Pointf(x,y),Pointf(x2,y),Pointf(x2,y2),Pointf(x,y2)],w),c);

  if (frac(x + hw) = 0.5) and (frac(y + hw)=0.5) and (frac(x2 - hw)=0.5) and (frac(y2 - hw)=0.5) then
    FillRect(ceil(x + hw), ceil(y + hw), ceil(x2 - hw), ceil(y2 - hw), back, dmDrawWithTransparency)
  else
    multi.AddRectangle(x + hw, y + hw, x2 - hw, y2 - hw, back);
  multi.Draw(self);
  multi.Free;
end;

procedure TBGRADefaultBitmap.RoundRectAntialias(x, y, x2, y2, rx, ry: single;
   c: TBGRAPixel; w: single; options: TRoundRectangleOptions);
begin
  if (PenStyle = psClear) or (c.alpha = 0) then exit;
  if (PenStyle = psSolid) then
    BGRAPolygon.BorderRoundRectangleAntialias(self,x,y,x2,y2,rx,ry,w,options,c,False, LinearAntialiasing)
  else
    DrawPolygonAntialias(BGRAPath.ComputeRoundRect(x,y,x2,y2,rx,ry,options),c,w);
end;

procedure TBGRADefaultBitmap.RoundRectAntialias(x, y, x2, y2, rx, ry: single;
  pencolor: TBGRAPixel; w: single; fillcolor: TBGRAPixel;
  options: TRoundRectangleOptions);
var
  multi: TBGRAMultishapeFiller;
begin
  if (PenStyle = psClear) or (pencolor.alpha = 0) then
  begin
    FillRoundRectAntialias(x,y,x2,y2,rx,ry,fillColor,options);
    exit;
  end;
  if (PenStyle = psSolid) then
    BGRAPolygon.BorderAndFillRoundRectangleAntialias(self,x,y,x2,y2,rx,ry,w,options,pencolor,fillcolor,nil,nil,False)
  else
  begin
    multi := TBGRAMultishapeFiller.Create;
    multi.PolygonOrder := poLastOnTop;
    multi.AddRoundRectangle(x,y,x2,y2,rx,ry,fillColor,options);
    multi.AddPolygon(ComputeWidePolygon(BGRAPath.ComputeRoundRect(x,y,x2,y2,rx,ry,options),w),pencolor);
    multi.Draw(self);
    multi.Free;
  end;
end;

procedure TBGRADefaultBitmap.RoundRectAntialias(x, y, x2, y2, rx, ry: single;
  penTexture: IBGRAScanner; w: single; fillTexture: IBGRAScanner;
  options: TRoundRectangleOptions);
var
  multi: TBGRAMultishapeFiller;
begin
  if (PenStyle = psClear) then
  begin
    FillRoundRectAntialias(x,y,x2,y2,rx,ry,fillTexture,options);
    exit;
  end else
  if (PenStyle = psSolid) then
    BGRAPolygon.BorderAndFillRoundRectangleAntialias(self,x,y,x2,y2,rx,ry,w,options,BGRAPixelTransparent,BGRAPixelTransparent,pentexture,filltexture,False)
  else
  begin
    multi := TBGRAMultishapeFiller.Create;
    multi.PolygonOrder := poLastOnTop;
    multi.AddRoundRectangle(x,y,x2,y2,rx,ry,fillTexture,options);
    multi.AddPolygon(ComputeWidePolygon(ComputeRoundRect(x,y,x2,y2,rx,ry,options),w),penTexture);
    multi.Draw(self);
    multi.Free;
  end;
end;

procedure TBGRADefaultBitmap.RoundRectAntialias(x, y, x2, y2, rx, ry: single;
  texture: IBGRAScanner; w: single; options: TRoundRectangleOptions);
begin
  if (PenStyle = psClear) then exit;
  if (PenStyle = psSolid) then
    BGRAPolygon.BorderRoundRectangleAntialiasWithTexture(self,x,y,x2,y2,rx,ry,w,options,texture, LinearAntialiasing)
  else
    DrawPolygonAntialias(BGRAPath.ComputeRoundRect(x,y,x2,y2,rx,ry,options),texture,w);
end;

function TBGRADefaultBitmap.CheckRectBounds(var x, y, x2, y2: integer; minsize: integer): boolean; inline;
var
  temp: integer;
begin
  //swap coordinates if needed
  if (x > x2) then
  begin
    temp := x;
    x    := x2;
    x2   := temp;
  end;
  if (y > y2) then
  begin
    temp := y;
    y    := y2;
    y2   := temp;
  end;
  if (x2 - x <= minsize) or (y2 - y <= minsize) then
  begin
    result := false;
    exit;
  end else
    result := true;
end;

procedure TBGRADefaultBitmap.FillRect(x, y, x2, y2: integer;
  texture: IBGRAScanner; mode: TDrawMode; AScanOffset: TPoint; ditheringAlgorithm: TDitheringAlgorithm);
var dither: TDitheringTask;
begin
  if not CheckClippedRectBounds(x,y,x2,y2) then exit;
  dither := CreateDitheringTask(ditheringAlgorithm, texture, self, rect(x,y,x2,y2));
  dither.ScanOffset := AScanOffset;
  dither.DrawMode := mode;
  dither.Execute;
  dither.Free;
end;

{------------------------- Text functions ---------------------------------------}

procedure TBGRADefaultBitmap.TextOutAngle(x, y: single; orientationTenthDegCCW: integer;
  const sUTF8: string; c: TBGRAPixel; align: TAlignment; ARightToLeft: boolean);
begin
  with (PointF(x,y)-GetFontAnchorRotatedOffset(orientationTenthDegCCW)) do
    FontRenderer.TextOutAngle(self,x,y,orientationTenthDegCCW,CleanTextOutString(sUTF8),c,align,ARightToLeft);
end;

procedure TBGRADefaultBitmap.TextOutAngle(x, y: single; orientationTenthDegCCW: integer;
  const sUTF8: string; texture: IBGRAScanner; align: TAlignment; ARightToLeft: boolean);
begin
  with (PointF(x,y)-GetFontAnchorRotatedOffset(orientationTenthDegCCW)) do
    FontRenderer.TextOutAngle(self,x,y,orientationTenthDegCCW,CleanTextOutString(sUTF8),texture,align,ARightToLeft);
end;

procedure TBGRADefaultBitmap.TextOutCurved(ACursor: TBGRACustomPathCursor; const sUTF8: string; AColor: TBGRAPixel; AAlign: TAlignment; ALetterSpacing: single);
begin
  InternalTextOutCurved(ACursor, sUTF8, AColor, nil, AAlign, ALetterSpacing);
end;

procedure TBGRADefaultBitmap.TextOutCurved(ACursor: TBGRACustomPathCursor; const sUTF8: string; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single);
begin
  InternalTextOutCurved(ACursor, sUTF8, BGRAPixelTransparent, ATexture, AAlign, ALetterSpacing);
end;

procedure TBGRADefaultBitmap.TextMultiline(ALeft, ATop, AWidth: single; const sUTF8: string;
  c: TBGRAPixel; AAlign: TBidiTextAlignment; AVertAlign: TTextLayout; AParagraphSpacing: single);
var
  layout: TBidiTextLayout;
  i: Integer;
begin
  if FontBidiMode = fbmAuto then
    layout := TBidiTextLayout.Create(FontRenderer, sUTF8)
  else
    layout := TBidiTextLayout.Create(FontRenderer, sUTF8, GetFontRightToLeftFor(sUTF8));
  for i := 0 to layout.ParagraphCount-1 do
    layout.ParagraphAlignment[i] := AAlign;
  layout.ParagraphSpacingBelow:= AParagraphSpacing;
  layout.AvailableWidth := AWidth;
  case AVertAlign of
    tlBottom: layout.TopLeft := PointF(ALeft,ATop-layout.TotalTextHeight);
    tlCenter: layout.TopLeft := PointF(ALeft,ATop-layout.TotalTextHeight/2);
    else layout.TopLeft := PointF(ALeft,ATop);
  end;
  layout.DrawText(self, c);
  layout.Free;
end;

procedure TBGRADefaultBitmap.TextMultiline(ALeft, ATop, AWidth: single;
  const sUTF8: string; ATexture: IBGRAScanner; AAlign: TBidiTextAlignment;
  AVertAlign: TTextLayout; AParagraphSpacing: single);
var
  layout: TBidiTextLayout;
  i: Integer;
begin
  if FontBidiMode = fbmAuto then
    layout := TBidiTextLayout.Create(FontRenderer, sUTF8)
  else
    layout := TBidiTextLayout.Create(FontRenderer, sUTF8, GetFontRightToLeftFor(sUTF8));
  for i := 0 to layout.ParagraphCount-1 do
    layout.ParagraphAlignment[i] := AAlign;
  layout.ParagraphSpacingBelow:= AParagraphSpacing;
  layout.AvailableWidth := AWidth;
  case AVertAlign of
    tlBottom: layout.TopLeft := PointF(ALeft,ATop-layout.TotalTextHeight);
    tlCenter: layout.TopLeft := PointF(ALeft,ATop-layout.TotalTextHeight/2);
    else layout.TopLeft := PointF(ALeft,ATop);
  end;
  layout.DrawText(self, ATexture);
  layout.Free;
end;

procedure TBGRADefaultBitmap.TextOut(x, y: single; const sUTF8: string;
  texture: IBGRAScanner; align: TAlignment; ARightToLeft: boolean);
begin
  FontRenderer.TextOut(self,x,y,CleanTextOutString(sUTF8),texture,align, ARightToLeft);
end;

procedure TBGRADefaultBitmap.TextOut(x, y: single; const sUTF8: string;
  AColor: TBGRAPixel; AAlign: TAlignment; ALetterSpacing: single);
begin
  InternalTextOutLetterSpacing(x, y, sUTF8, AColor, nil, AAlign, ALetterSpacing);
end;

procedure TBGRADefaultBitmap.TextOut(x, y: single; const sUTF8: string;
  ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single);
begin
  InternalTextOutLetterSpacing(x, y, sUTF8, BGRAPixelTransparent, ATexture, AAlign, ALetterSpacing);
end;

procedure TBGRADefaultBitmap.TextOut(x, y: single; const sUTF8: string;
  c: TBGRAPixel; align: TAlignment; ARightToLeft: boolean);
begin
  with (PointF(x,y)-GetFontAnchorRotatedOffset) do
    FontRenderer.TextOut(self,x,y,CleanTextOutString(sUTF8),c,align, ARightToLeft);
end;

procedure TBGRADefaultBitmap.TextRect(ARect: TRect; x, y: integer;
  const sUTF8: string; style: TTextStyle; c: TBGRAPixel);
begin
  with (PointF(x,y)-GetFontAnchorRotatedOffset(0)) do
    FontRenderer.TextRect(self,ARect,system.round(x),system.round(y),sUTF8,style,c);
end;

procedure TBGRADefaultBitmap.TextRect(ARect: TRect; x, y: integer; const sUTF8: string;
  style: TTextStyle; texture: IBGRAScanner);
begin
  with (PointF(x,y)-GetFontAnchorRotatedOffset(0)) do
    FontRenderer.TextRect(self,ARect,system.round(x),system.round(y),sUTF8,style,texture);
end;

{ Returns the total size of the string provided using the current font.
  Orientation is not taken into account, so that the width is along the text.  }
function TBGRADefaultBitmap.TextSize(const sUTF8: string): TSize;
begin
  result := FontRenderer.TextSize(CleanTextOutString(sUTF8));
end;

function TBGRADefaultBitmap.TextSizeMultiline(const sUTF8: string; AMaxWidth: single;
  AParagraphSpacing: single): TSize;
var
  layout: TBidiTextLayout;
begin
  if FontBidiMode = fbmAuto then
    layout := TBidiTextLayout.Create(FontRenderer, sUTF8)
  else
    layout := TBidiTextLayout.Create(FontRenderer, sUTF8, GetFontRightToLeftFor(sUTF8));
  layout.ParagraphSpacingBelow:= AParagraphSpacing;
  layout.AvailableWidth := AMaxWidth;
  result := size(ceil(layout.UsedWidth), ceil(layout.TotalTextHeight));
  layout.Free;
end;

function TBGRADefaultBitmap.TextAffineBox(const sUTF8: string): TAffineBox;
var size: TSize;
  m: TAffineMatrix;
  dy: single;
begin
  dy := GetFontVerticalAnchorOffset;
  size := FontRenderer.TextSizeAngle(sUTF8, FontOrientation);
  m := AffineMatrixRotationDeg(-FontOrientation*0.1);
  result := TAffineBox.AffineBox(PointF(0,-dy), m*PointF(size.cx,-dy), m*PointF(0,size.cy-dy));
end;

function TBGRADefaultBitmap.TextSize(const sUTF8: string; AMaxWidth: integer): TSize;
begin
  result := FontRenderer.TextSize(sUTF8, AMaxWidth, GetFontRightToLeftFor(sUTF8));
end;

function TBGRADefaultBitmap.TextSize(const sUTF8: string; AMaxWidth: integer;
  ARightToLeft: boolean): TSize;
begin
  result := FontRenderer.TextSize(sUTF8, AMaxWidth, ARightToLeft);
end;

function TBGRADefaultBitmap.TextFitInfo(const sUTF8: string; AMaxWidth: integer
  ): integer;
begin
  result := FontRenderer.TextFitInfo(sUTF8, AMaxWidth);
end;

{---------------------------- Curves ----------------------------------------}

function TBGRADefaultBitmap.ComputeClosedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF;
begin
  result := BGRAPath.ComputeClosedSpline(APoints, AStyle);
end;

function TBGRADefaultBitmap.ComputeOpenedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF;
begin
  result := BGRAPath.ComputeOpenedSpline(APoints, AStyle);
end;

function TBGRADefaultBitmap.ComputeBezierCurve(const ACurve: TCubicBezierCurve
  ): ArrayOfTPointF;
begin
  Result:= BGRAPath.ComputeBezierCurve(ACurve);
end;

function TBGRADefaultBitmap.ComputeBezierCurve(
  const ACurve: TQuadraticBezierCurve): ArrayOfTPointF;
begin
  Result:= BGRAPath.ComputeBezierCurve(ACurve);
end;

function TBGRADefaultBitmap.ComputeBezierSpline(
  const ASpline: array of TCubicBezierCurve): ArrayOfTPointF;
begin
  Result:= BGRAPath.ComputeBezierSpline(ASpline);
end;

function TBGRADefaultBitmap.ComputeBezierSpline(
  const ASpline: array of TQuadraticBezierCurve): ArrayOfTPointF;
begin
  Result:= BGRAPath.ComputeBezierSpline(ASpline);
end;

function TBGRADefaultBitmap.ComputeWidePolyline(const points: array of TPointF;
  w: single): ArrayOfTPointF;
begin
  result := GetInternalPen.ComputePolyline(points,w);
end;

function TBGRADefaultBitmap.ComputeWidePolyline(const points: array of TPointF;
  w: single; ClosedCap: boolean): ArrayOfTPointF;
begin
  result := GetInternalPen.ComputePolyline(points,w,ClosedCap);
end;

function TBGRADefaultBitmap.ComputeWidePolygon(const points: array of TPointF;
  w: single): ArrayOfTPointF;
begin
  result := GetInternalPen.ComputePolygon(points,w);
end;

function TBGRADefaultBitmap.ComputeEllipseContour(x, y, rx, ry: single; quality: single): ArrayOfTPointF;
begin
  result := BGRAPath.ComputeEllipse(x,y,rx,ry, quality);
end;

function TBGRADefaultBitmap.ComputeEllipseContour(AOrigin, AXAxis,
  AYAxis: TPointF; quality: single): ArrayOfTPointF;
begin
  result := BGRAPath.ComputeEllipse(AOrigin,AXAxis,AYAxis, quality);
end;

function TBGRADefaultBitmap.ComputeEllipseBorder(x, y, rx, ry, w: single; quality: single): ArrayOfTPointF;
begin
  result := ComputeWidePolygon(ComputeEllipseContour(x,y,rx,ry, quality),w);
end;

function TBGRADefaultBitmap.ComputeEllipseBorder(AOrigin, AXAxis,
  AYAxis: TPointF; w: single; quality: single): ArrayOfTPointF;
begin
  result := ComputeWidePolygon(ComputeEllipseContour(AOrigin,AXAxis,AYAxis, quality),w);
end;

function TBGRADefaultBitmap.ComputeArc65536(x, y, rx, ry: single; start65536,
  end65536: word; quality: single): ArrayOfTPointF;
begin
  result := BGRAPath.ComputeArc65536(x,y,rx,ry,start65536,end65536,quality);
end;

function TBGRADefaultBitmap.ComputeArcRad(x, y, rx, ry: single; startRad,
  endRad: single; quality: single): ArrayOfTPointF;
begin
  result := BGRAPath.ComputeArcRad(x,y,rx,ry,startRad,endRad,quality);
end;

function TBGRADefaultBitmap.ComputeRoundRect(x1, y1, x2, y2, rx, ry: single; quality: single): ArrayOfTPointF;
begin
  result := BGRAPath.ComputeRoundRect(x1,y1,x2,y2,rx,ry,quality);
end;

function TBGRADefaultBitmap.ComputeRoundRect(x1, y1, x2, y2, rx, ry: single;
  options: TRoundRectangleOptions; quality: single): ArrayOfTPointF;
begin
  Result:= BGRAPath.ComputeRoundRect(x1,y1,x2,y2,rx,ry,options,quality);
end;

function TBGRADefaultBitmap.ComputePie65536(x, y, rx, ry: single; start65536,
  end65536: word; quality: single): ArrayOfTPointF;
begin
  result := BGRAPath.ComputeArc65536(x,y,rx,ry,start65536,end65536,quality);
  if (start65536 <> end65536) then
  begin
    setlength(result,length(result)+1);
    result[high(result)] := PointF(x,y);
  end;
end;

function TBGRADefaultBitmap.ComputePieRad(x, y, rx, ry: single; startRad,
  endRad: single; quality: single): ArrayOfTPointF;
begin
  result := self.ComputePie65536(x,y,rx,ry,round(startRad*32768/Pi),round(endRad*32768/Pi),quality);
end;

{---------------------------------- Fill ---------------------------------}

procedure TBGRADefaultBitmap.Fill(c: TBGRAPixel; start, Count: integer);
begin
  if start < 0 then
  begin
    inc(Count, start);
    start := 0;
  end;
  if start >= nbPixels then
    exit;
  if start + Count > nbPixels then
    Count := nbPixels - start;

  FillInline(Data + start, c, Count);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.AlphaFill(alpha: byte; start, Count: integer);
begin
  if alpha = 0 then
    Fill(BGRAPixelTransparent, start, Count);
  if start < 0 then
  begin
    inc(Count, start);
    start := 0;
  end;
  if start >= nbPixels then
    exit;
  if start + Count > nbPixels then
    Count := nbPixels - start;

  AlphaFillInline(Data + start, alpha, Count);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.FillMask(x, y: integer; AMask: TCustomUniversalBitmap;
  const AColor: TBGRAPixel; ADrawMode: TDrawMode);
var
  scan: TBGRACustomScanner;
begin
  if (AMask = nil) or (AColor.alpha = 0) then exit;
  scan := TBGRASolidColorMaskScanner.Create(AMask, Point(-X,-Y), AColor);
  self.FillRect(X,Y, X+AMask.Width,Y+AMask.Height, scan, ADrawMode);
  scan.Free;
end;

procedure TBGRADefaultBitmap.FillMask(x, y: integer; AMask: TCustomUniversalBitmap;
  ATexture: IBGRAScanner; ADrawMode: TDrawMode; AOpacity: byte);
var
  scan: TBGRACustomScanner;
begin
  if AMask = nil then exit;
  scan := TBGRATextureMaskScanner.Create(AMask, Point(-X,-Y), ATexture, AOpacity);
  self.FillRect(X,Y, X+AMask.Width,Y+AMask.Height, scan, ADrawMode);
  scan.Free;
end;

procedure TBGRADefaultBitmap.EraseMask(x, y: integer; AMask: TBGRACustomBitmap;
  alpha: byte);
var
  x0,y0,x2, y2, yb,xb, tx, delta: integer;
  p, psrc: PBGRAPixel;
begin
  if (AMask = nil) or (alpha = 0) then exit;
  x0 := x;
  y0 := y;
  x2 := x+AMask.Width;
  y2 := y+AMask.Height;
  if not CheckClippedRectBounds(x,y,x2,y2) then exit;
  tx := x2 - x;
  Dec(x2);
  Dec(y2);

  p := Scanline[y] + x;
  if FLineOrder = riloBottomToTop then
    delta := -Width
  else
    delta := Width;

  for yb := y to y2 do
  begin
    psrc := AMask.ScanLine[yb-y0]+(x-x0);
    if alpha = 255 then
    begin
      for xb := tx-1 downto 0 do
      begin
        ErasePixelInline(p, psrc^.green);
        inc(p);
        inc(psrc);
      end;
    end else
    begin
      for xb := tx-1 downto 0 do
      begin
        ErasePixelInline(p, ApplyOpacity(psrc^.green,alpha));
        inc(p);
        inc(psrc);
      end;
    end;
    dec(p, tx);
    Inc(p, delta);
  end;

  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.FillClearTypeMask(x, y: integer; xThird: integer;
  AMask: TBGRACustomBitmap; color: TBGRAPixel; ARGBOrder: boolean);
begin
  BGRAFillClearTypeMask(self,x, y, xThird, AMask, color, nil, ARGBOrder);
end;

procedure TBGRADefaultBitmap.FillClearTypeMask(x, y: integer; xThird: integer;
  AMask: TBGRACustomBitmap; texture: IBGRAScanner; ARGBOrder: boolean);
begin
  BGRAFillClearTypeMask(self,x, y, xThird, AMask, BGRAPixelTransparent, texture, ARGBOrder);
end;

{ Replace color without taking alpha channel into account }
procedure TBGRADefaultBitmap.ReplaceColor(before, after: TColor);
var
  p: PLongWord;
  n: integer;
  colorMask,beforeBGR, afterBGR: LongWord;
  rAfter,gAfter,bAfter,rBefore,gBefore,bBefore: byte;
begin
  colorMask := LongWord(BGRA(255,255,255,0));
  RedGreenBlue(before, rBefore,gBefore,bBefore);
  RedGreenBlue(after, rAfter,gAfter,bAfter);
  beforeBGR := LongWord(BGRA(rBefore,gBefore,bBefore,0));
  afterBGR  := LongWord(BGRA(rAfter,gAfter,bAfter,0));

  p := PLongWord(Data);
  for n := NbPixels - 1 downto 0 do
  begin
    if p^ and colorMask = beforeBGR then
      p^ := (p^ and not ColorMask) or afterBGR;
    Inc(p);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.ReplaceColor(ABounds: TRect; before, after: TColor);
var p: PLongWord;
  xb,yb,xcount: integer;

  colorMask,beforeBGR, afterBGR: LongWord;
  rAfter,gAfter,bAfter,rBefore,gBefore,bBefore: byte;
begin
  colorMask := LongWord(BGRA(255,255,255,0));
  RedGreenBlue(before, rBefore,gBefore,bBefore);
  RedGreenBlue(after, rAfter,gAfter,bAfter);
  beforeBGR := LongWord(BGRA(rBefore,gBefore,bBefore,0));
  afterBGR  := LongWord(BGRA(rAfter,gAfter,bAfter,0));

  ABounds.Intersect(ClipRect);
  if ABounds.IsEmpty then exit;
  xcount := ABounds.Right-ABounds.Left;
  for yb := ABounds.Top to ABounds.Bottom-1 do
  begin
    p := PLongWord(ScanLine[yb]+ABounds.Left);
    for xb := xcount-1 downto 0 do
    begin
      if p^ and colorMask = beforeBGR then
        p^ := (p^ and not ColorMask) or afterBGR;
      Inc(p);
    end;
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.ParallelFloodFill(X, Y: integer;
  Dest: TCustomUniversalBitmap; Color: TBGRAPixel; mode: TFloodfillMode;
  Tolerance: byte; DestOfsX: integer; DestOfsY: integer);
var
  b: TUniversalBrush;
begin
  case mode of
    fmSet: Dest.SolidBrushBGRA(b, Color, dmSet);
    fmDrawWithTransparency: Dest.SolidBrushBGRA(b, Color, dmDrawWithTransparency);
    fmLinearBlend: Dest.SolidBrushBGRA(b, Color, dmLinearBlend);
    fmXor: Dest.SolidBrushBGRA(b, Color, dmXor);
    fmProgressive: Dest.SolidBrushBGRA(b, Color, dmDrawWithTransparency);
  end;
  ParallelFloodFill(X,Y, Dest, b, mode=fmProgressive, (Tolerance shl 8)+$ff, DestOfsX, DestOfsY);
end;

{ General purpose FloodFill. It can be used to fill inplace or to
  fill a destination bitmap according to the content of the current bitmap.

  The first pixel encountered is taken as a reference, further pixels
  are compared to this pixel. If the distance between next colors and
  the first color is lower than the tolerance, then the floodfill continues.

  It uses an array of bits to store visited places to avoid filling twice
  the same area. It also uses a stack of positions to remember where
  to continue after a place is completely filled.

  The first direction to be checked is horizontal, then
  it checks pixels on the line above and on the line below. }
procedure TBGRADefaultBitmap.ParallelFloodFill(X, Y: integer;
  Dest: TCustomUniversalBitmap; const Brush: TUniversalBrush; Progressive: boolean;
  ToleranceW: Word; DestOfsX: integer; DestOfsY: integer);
var
  S: TBGRAPixel;
  SExpand: TExpandedPixel;
  SX, EX, I: integer;
  Added: boolean;

  Visited: array of LongWord;
  VisitedLineSize: integer;

  Stack:      array of integer;
  StackCount: integer;
  pScan: PBGRAPixel;

  function CheckPixel(AX, AY: integer): boolean; inline;
  begin
    if Visited[AX shr 5 + AY * VisitedLineSize] and (1 shl (AX and 31)) <> 0 then
      Result := False
    else
    begin
      if (pScan+AX)^ = S then result := true else
        Result := ExpandedDiff(GammaExpansion((pScan+AX)^), SExpand) <= ToleranceW;
    end;
  end;

  procedure SetVisited(X1, AY, X2: integer);
  var
    StartMask, EndMask: LongWord;
    StartPos, EndPos:   integer;
  begin
    if X2 < X1 then
      exit;
    StartMask := $FFFFFFFF shl (X1 and 31);
    case X2 and 31 of
    31: EndMask := $FFFFFFFF;
    30: EndMask := $7FFFFFFF;
    else
      EndMask := 1 shl ((X2 and 31) + 1) - 1;
    end;
    StartPos := X1 shr 5 + AY * VisitedLineSize;
    EndPos := X2 shr 5 + AY * VisitedLineSize;
    if StartPos = EndPos then
      Visited[StartPos] := Visited[StartPos] or (StartMask and EndMask)
    else
    begin
      Visited[StartPos] := Visited[StartPos] or StartMask;
      Visited[EndPos]   := Visited[EndPos] or EndMask;
      if EndPos - StartPos > 1 then
        FillDWord(Visited[StartPos + 1], EndPos - StartPos - 1, $FFFFFFFF);
    end;
  end;

  procedure Push(AX, AY: integer); inline;
  begin
    if StackCount + 1 >= High(Stack) then
      SetLength(Stack, Length(Stack) shl 1);

    Stack[StackCount] := AX;
    Inc(StackCount);
    Stack[StackCount] := AY;
    Inc(StackCount);
  end;

  procedure Pop(var AX, AY: integer); inline;
  begin
    Dec(StackCount);
    AY := Stack[StackCount];
    Dec(StackCount);
    AX := Stack[StackCount];
  end;

begin
  if Brush.DoesNothing then exit;
  if Progressive and not (dest is TBGRACustomBitmap) then
    raise exception.Create('Progressive mode only available on TBGRACustomBitmap and derived classes');
  if PtInClipRect(X,Y) then
  begin
    S := GetPixel(X, Y);
    SExpand := s.ToExpanded;

    VisitedLineSize := (Width + 31) shr 5;
    SetLength(Visited, VisitedLineSize * Height);
    FillDWord(Visited[0], Length(Visited), 0);

    SetLength(Stack, 2);
    StackCount := 0;

    Push(X, Y);
    repeat
      Pop(X, Y);
      pScan := GetScanlineFast(Y);
      if not CheckPixel(X, Y) then
        Continue;

      SX := X;
      while (SX > FClipRect.Left) and CheckPixel(Pred(SX), Y) do
        Dec(SX);
      EX := X;
      while (EX < Pred(FClipRect.Right)) and CheckPixel(Succ(EX), Y) do
        Inc(EX);

      SetVisited(SX, Y, EX);
      if Progressive then
        TBGRACustomBitmap(dest).HorizLineDiff(SX+DestOfsX, Y+DestOfsY, EX+DestOfsX, Brush, S, ToleranceW)
      else
        dest.HorizLine(SX+DestOfsX, Y+DestOfsY, EX+DestOfsX, Brush);

      Added := False;
      if Y > FClipRect.Top then
      begin
        pScan := GetScanlineFast(Pred(Y));
        for I := SX to EX do
          if CheckPixel(I, Pred(Y)) then
          begin
            if Added then //do not add twice the same segment
              Continue;
            Push(I, Pred(Y));
            Added := True;
          end
          else
            Added := False;
      end;

      Added := False;
      if Y < Pred(FClipRect.Bottom) then
      begin
        pScan := GetScanlineFast(Succ(Y));
        for I := SX to EX do
          if CheckPixel(I, Succ(Y)) then
          begin
            if Added then //do not add twice the same segment
              Continue;
            Push(I, Succ(Y));
            Added := True;
          end
          else
            Added := False;
      end;
    until StackCount <= 0;
  end;
end;

procedure TBGRADefaultBitmap.GradientFill(x, y, x2, y2: integer;
  c1, c2: TBGRAPixel; gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
  gammaColorCorrection: boolean; Sinus: Boolean; ditherAlgo: TDitheringAlgorithm);
var
  scanner: TBGRAGradientScanner;
begin
  if (c1.alpha = 0) and (c2.alpha = 0) then
    FillRect(x, y, x2, y2, BGRAPixelTransparent, mode)
  else
  if ditherAlgo <> daNearestNeighbor then
    GradientFillDithered(x,y,x2,y2,c1,c2,gtype,o1,o2,mode,gammaColorCorrection,sinus,ditherAlgo)
  else
  begin
    scanner := TBGRAGradientScanner.Create(c1,c2,gtype,o1,o2,gammaColorCorrection,Sinus);
    FillRect(x,y,x2,y2,scanner,mode);
    scanner.Free;
  end;
end;

procedure TBGRADefaultBitmap.GradientFill(x, y, x2, y2: integer;
  gradient: TBGRACustomGradient; gtype: TGradientType; o1, o2: TPointF;
  mode: TDrawMode; Sinus: Boolean; ditherAlgo: TDitheringAlgorithm);
var
  scanner: TBGRAGradientScanner;
begin
  if ditherAlgo <> daNearestNeighbor then
    GradientFillDithered(x,y,x2,y2,gradient,gtype,o1,o2,mode,sinus,ditherAlgo)
  else
  begin
    scanner := TBGRAGradientScanner.Create(gradient,gtype,o1,o2,sinus);
    FillRect(x,y,x2,y2,scanner,mode);
    scanner.Free;
  end;
end;

procedure TBGRADefaultBitmap.GradientFillDithered(x, y, x2, y2: integer; c1,
  c2: TBGRAPixel; gtype: TGradientType; o1, o2: TPointF;
  mode: TDrawMode; gammaColorCorrection: boolean; Sinus: Boolean;
  ditherAlgo: TDitheringAlgorithm);
var
  scanner: TBGRAGradientScanner;
begin
  if (c1.alpha = 0) and (c2.alpha = 0) then
  begin
    if mode = dmSet then
      FillRect(x, y, x2, y2, BGRAPixelTransparent, dmSet);
  end
  else
  begin
    scanner := TBGRAGradientScanner.Create(c1,c2,gtype,o1,o2,gammaColorCorrection,Sinus);
    FillRect(x,y,x2,y2,scanner,mode,ditherAlgo);
    scanner.Free;
  end;
end;

procedure TBGRADefaultBitmap.GradientFillDithered(x, y, x2, y2: integer;
  gradient: TBGRACustomGradient; gtype: TGradientType; o1, o2: TPointF;
  mode: TDrawMode; Sinus: Boolean; ditherAlgo: TDitheringAlgorithm);
var
  scanner: TBGRAGradientScanner;
begin
  scanner := TBGRAGradientScanner.Create(gradient,gtype,o1,o2,sinus);
  FillRect(x,y,x2,y2,scanner,mode,ditherAlgo);
  scanner.Free;
end;

function TBGRADefaultBitmap.ScanAtInteger(X, Y: integer): TBGRAPixel;
begin
  if (FScanWidth <> 0) and (FScanHeight <> 0) then
    result := (GetScanlineFast(PositiveMod(Y+ScanOffset.Y, FScanHeight))+PositiveMod(X+ScanOffset.X, FScanWidth))^
  else
    result := BGRAPixelTransparent;
end;

function TBGRADefaultBitmap.ScanNextPixel: TBGRAPixel;
begin
  if (FScanWidth <> 0) and (FScanHeight <> 0) then
  begin
    result := PBGRAPixel(FScanPtr)^;
    inc(FScanCurX);
    inc(FScanPtr, sizeof(TBGRAPixel));
    if FScanCurX = FScanWidth then //cycle
    begin
      FScanCurX := 0;
      dec(FScanPtr, FRowSize);
    end;
  end
  else
    result := BGRAPixelTransparent;
end;

function TBGRADefaultBitmap.ScanAt(X, Y: Single): TBGRAPixel;
var
  ix, iy: Int32or64;
  iFactX,iFactY: Int32or64;
begin
  if (FScanWidth = 0) or (FScanHeight = 0) then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  LoadFromBitmapIfNeeded;
  ix := round(x*256);
  iy := round(y*256);
  if ScanInterpolationFilter = rfBox then
  begin
    ix := PositiveMod((ix+128)+(ScanOffset.X shl 8), FScanWidth shl 8) shr 8;
    iy := PositiveMod((iy+128)+(ScanOffset.Y shl 8), FScanHeight shl 8) shr 8;
    result := (GetScanlineFast(iy)+ix)^;
    exit;
  end;
  iFactX := ix and 255;
  iFactY := iy and 255;
  ix := PositiveMod(ix+(ScanOffset.X shl 8), FScanWidth shl 8) shr 8;
  iy := PositiveMod(iy+(ScanOffset.Y shl 8), FScanHeight shl 8) shr 8;
  if (iFactX = 0) and (iFactY = 0) then
  begin
    result := (GetScanlineFast(iy)+ix)^;
    exit;
  end;
  if ScanInterpolationFilter <> rfLinear then
  begin
    iFactX := FineInterpolation256( iFactX, ScanInterpolationFilter );
    iFactY := FineInterpolation256( iFactY, ScanInterpolationFilter );
  end;
  result := InternalGetPixelCycle256(ix,iy, iFactX,iFactY);
end;

function TBGRADefaultBitmap.IsScanPutPixelsDefined: boolean;
begin
  Result:= true;
end;

procedure TBGRADefaultBitmap.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
var
  i,nbCopy: Integer;
  c: TBGRAPixel;
begin
  if (FScanWidth <= 0) or (FScanHeight <= 0) then
  begin
    if mode = dmSet then
      FillDWord(pdest^, count, LongWord(BGRAPixelTransparent));
    exit;
  end;
  case mode of
    dmLinearBlend:
      for i := 0 to count-1 do
      begin
        FastBlendPixelInline(pdest, ScanNextPixel);
        inc(pdest);
      end;
    dmDrawWithTransparency:
      for i := 0 to count-1 do
      begin
        DrawPixelInlineWithAlphaCheck(pdest, ScanNextPixel);
        inc(pdest);
      end;
    dmSet:
      while count > 0 do
      begin
        nbCopy := FScanWidth-FScanCurX;
        if count < nbCopy then nbCopy := count;
        move(FScanPtr^,pdest^,nbCopy*sizeof(TBGRAPixel));
        inc(pdest,nbCopy);
        inc(FScanCurX,nbCopy);
        inc(FScanPtr,nbCopy*sizeof(TBGRAPixel));
        if FScanCurX = FScanWidth then
        begin
          FScanCurX := 0;
          dec(FScanPtr, RowSize);
        end;
        dec(count,nbCopy);
      end;
    dmSetExceptTransparent:
      for i := 0 to count-1 do
      begin
        c := ScanNextPixel;
        if c.alpha = 255 then pdest^ := c;
        inc(pdest);
      end;
    dmXor:
      for i := 0 to count-1 do
      begin
        PLongWord(pdest)^ := PLongWord(pdest)^ xor LongWord(ScanNextPixel);
        inc(pdest);
      end;
  end;
end;

{ General purpose pixel drawing function }
procedure TBGRADefaultBitmap.DrawPixels(c: TBGRAPixel; start, Count: integer);
var
  p: PBGRAPixel;
begin
  if c.alpha = 0 then
    exit;
  if c.alpha = 255 then
  begin
    Fill(c,start,Count);
    exit;
  end;

  if start < 0 then
  begin
    inc(Count, start);
    start := 0;
  end;
  if start >= nbPixels then
    exit;
  if start + Count > nbPixels then
    Count := nbPixels - start;

  p := Data + start;
  DrawPixelsInline(p,c,Count);
  InvalidateBitmap;
end;

{------------------------- End fill ------------------------------}

procedure TBGRADefaultBitmap.DoAlphaCorrection;
var
  p: PBGRAPixel;
  n: integer;
  colormask: LongWord;
  changed: boolean;
begin
  if CanvasAlphaCorrection then
  begin
    p := PBGRAPixel(FDataByte); // avoid Data to avoid reloading from bitmap and thus stack overflow
    colormask := $ffffffff - (255 shl TBGRAPixel_AlphaShift);
    changed := false;
    for n := NbPixels - 1 downto 0 do
    begin
      if (plongword(p)^ and colormask <> 0) and (p^.alpha = 0) then
      begin
        p^.alpha := FCanvasOpacity;
        changed := true;
      end;
      Inc(p);
    end;
    if changed then InvalidateBitmap;
  end;
  FAlphaCorrectionNeeded := False;
end;

{ Ensure that transparent pixels have all channels to zero }
procedure TBGRADefaultBitmap.ClearTransparentPixels;
var
  p: PBGRAPixel;
  n: integer;
begin
  p := Data;
  for n := NbPixels - 1 downto 0 do
  begin
    if (p^.alpha = 0) then
      p^ := BGRAPixelTransparent;
    Inc(p);
  end;
  InvalidateBitmap;
end;

function TBGRADefaultBitmap.CheckAntialiasRectBounds(var x, y, x2, y2: single;
  w: single): boolean;
var
  temp: Single;
begin
  if (x > x2) then
  begin
    temp := x;
    x    := x2;
    x2   := temp;
  end;
  if (y > y2) then
  begin
    temp := y;
    y    := y2;
    y2   := temp;
  end;

  result := (x2 - x > w) and (y2 - y > w);
end;

function TBGRADefaultBitmap.GetCanvasBGRA: TBGRACanvas;
begin
  if FCanvasBGRA = nil then
    FCanvasBGRA := TBGRACanvas.Create(self);
  result := FCanvasBGRA;
end;

function TBGRADefaultBitmap.GetCanvas2D: TBGRACanvas2D;
begin
  if FCanvas2D = nil then
    FCanvas2D := TBGRACanvas2D.Create(self);
  result := FCanvas2D;
end;

procedure TBGRADefaultBitmap.PutImage(X, Y: integer; ASource: TCustomUniversalBitmap;
  AMode: TDrawMode; AOpacity: byte);
begin
  inherited PutImage(X,Y, ASource, AMode, AOpacity);
  if (AMode in [dmSetExceptTransparent,dmDrawWithTransparency,dmLinearBlend]) and
     (ASource is TBGRACustomBitmap) and Assigned(TBGRACustomBitmap(ASource).XorMask) then
    PutImage(X,Y,TBGRACustomBitmap(ASource).XorMask,dmXor,AOpacity);
end;

procedure TBGRADefaultBitmap.BlendImage(x, y: integer; ASource: TBGRACustomBitmap;
  AOperation: TBlendOperation);
begin
  BlendImage(RectWithSize(x,y,ASource.Width,ASource.Height), ASource, -x,-y,AOperation);
end;

procedure TBGRADefaultBitmap.BlendImage(ADest: TRect; ASource: IBGRAScanner;
  AOffsetX, AOffsetY: integer; AOperation: TBlendOperation);
const BufSize = 8;
var
  yb, remain, i, delta_dest: integer;
  psource, pdest: PBGRAPixel;
  sourceRect: TRect;
  sourceScanline, sourcePut: boolean;
  buf: packed array[0..BufSize-1] of TBGRAPixel;
begin
  if not CheckClippedRectBounds(ADest.Left, ADest.Top, ADest.Right, ADest.Bottom) then exit;

  sourceRect := ADest;
  sourceRect.Offset(AOffsetX, AOffsetY);
  sourceScanline := ASource.ProvidesScanline(sourceRect);
  sourcePut := ASource.IsScanPutPixelsDefined;

  pdest := Scanline[ADest.Top] + ADest.Left;
  if LineOrder = riloBottomToTop then
    delta_dest := -Width
    else delta_dest := Width;

  for yb := sourceRect.Top to sourceRect.Bottom-1 do
  begin
    if sourceScanline then
    begin
      psource := ASource.GetScanlineAt(sourceRect.Left, yb);
      BlendPixels(pdest, psource, AOperation, ADest.Width);
    end else
    begin
      ASource.ScanMoveTo(sourceRect.Left, yb);
      remain := ADest.Width;
      if sourcePut then
        while remain >= BufSize do
        begin
          ASource.ScanPutPixels(@buf, BufSize, dmSet);
          BlendPixels(pdest, @buf, AOperation, BufSize);
          inc(pdest, bufSize);
          dec(remain, bufSize);
        end;
      if remain > 0 then
      begin
        for i := 0 to remain-1 do
          buf[i] := ASource.ScanNextPixel;
        BlendPixels(pdest, @buf, AOperation, remain);
        inc(pdest, remain);
      end;
      dec(pdest, ADest.Width);
    end;
    Inc(pdest, delta_dest);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.BlendImageOver(x, y: integer;
  ASource: TBGRACustomBitmap; AOperation: TBlendOperation; AOpacity: byte; ALinearBlend: boolean);
begin
  BlendImageOver(RectWithSize(x,y,ASource.Width,ASource.Height), ASource, -x,-y,
                 AOperation, AOpacity, ALinearBlend);
end;

procedure TBGRADefaultBitmap.BlendImageOver(ADest: TRect; ASource: IBGRAScanner;
  AOffsetX, AOffsetY: integer; AOperation: TBlendOperation; AOpacity: byte; ALinearBlend: boolean);
const BufSize = 8;
var
  yb, remain, i, delta_dest: integer;
  psource, pdest: PBGRAPixel;
  sourceRect: TRect;
  sourceScanline, sourcePut: boolean;
  buf: packed array[0..BufSize-1] of TBGRAPixel;
begin
  if not CheckClippedRectBounds(ADest.Left, ADest.Top, ADest.Right, ADest.Bottom) then exit;

  sourceRect := ADest;
  sourceRect.Offset(AOffsetX, AOffsetY);
  sourceScanline := ASource.ProvidesScanline(sourceRect);
  sourcePut := ASource.IsScanPutPixelsDefined;

  pdest := Scanline[ADest.Top] + ADest.Left;
  if LineOrder = riloBottomToTop then
    delta_dest := -Width
    else delta_dest := Width;

  for yb := sourceRect.Top to sourceRect.Bottom-1 do
  begin
    if sourceScanline then
    begin
      psource := ASource.GetScanlineAt(sourceRect.Left, yb);
      BlendPixelsOver(pdest, psource, AOperation, ADest.Width, AOpacity, ALinearBlend);
    end else
    begin
      ASource.ScanMoveTo(sourceRect.Left, yb);
      remain := ADest.Width;
      if sourcePut then
        while remain >= BufSize do
        begin
          ASource.ScanPutPixels(@buf, BufSize, dmSet);
          BlendPixelsOver(pdest, @buf, AOperation, BufSize, AOpacity, ALinearBlend);
          inc(pdest, bufSize);
          dec(remain, bufSize);
        end;
      if remain > 0 then
      begin
        for i := 0 to remain-1 do
          buf[i] := ASource.ScanNextPixel;
        BlendPixelsOver(pdest, @buf, AOperation, remain, AOpacity, ALinearBlend);
        inc(pdest, remain);
      end;
      dec(pdest, ADest.Width);
    end;
    Inc(pdest, delta_dest);
  end;
  InvalidateBitmap;
end;

{ Draw an image with an affine transformation (rotation, scale, translate).
  Parameters are the bitmap origin, the end of the horizontal axis and the end of the vertical axis.
  The output bounds correspond to the pixels that will be affected in the destination. }
procedure TBGRADefaultBitmap.PutImageAffine(AMatrix: TAffineMatrix;
  Source: TBGRACustomBitmap; AOutputBounds: TRect;
  AResampleFilter: TResampleFilter; AMode: TDrawMode; AOpacity: Byte; APixelCenteredCoords: boolean);
var affine: TBGRAAffineBitmapTransform;
    sourceBounds: TRect;
begin
  if (Source = nil) or (Source.Width = 0) or (Source.Height = 0) or (AOpacity = 0) then exit;
  AOutputBounds.Intersect(ClipRect);
  if AOutputBounds.IsEmpty then exit;

  if not APixelCenteredCoords then AMatrix := AffineMatrixTranslation(-0.5,-0.5)*AMatrix*AffineMatrixTranslation(0.5,0.5);
  if IsAffineRoughlyTranslation(AMatrix, rect(0,0,Source.Width,Source.Height)) then
  begin
    sourceBounds := AOutputBounds;
    sourceBounds.Offset(-round(AMatrix[1,3]), -round(AMatrix[2,3]));
    sourceBounds.Intersect( rect(0,0,Source.Width,Source.Height) );
    PutImagePart(round(AMatrix[1,3])+sourceBounds.Left,round(AMatrix[2,3])+sourceBounds.Top,Source,sourceBounds,AMode,AOpacity);
  end else
  begin
    affine := TBGRAAffineBitmapTransform.Create(Source, false, AResampleFilter);
    affine.GlobalOpacity := AOpacity;
    affine.ViewMatrix := AMatrix;
    FillRect(AOutputBounds,affine,AMode);
    affine.Free;
  end;
end;

function TBGRADefaultBitmap.GetImageAffineBounds(AMatrix: TAffineMatrix;
  ASourceBounds: TRect; AClipOutput: boolean; APixelCenteredCoords: boolean): TRect;
const pointMargin = 0.5 - 1/512;

  procedure FirstPoint(pt: TPointF);
  begin
    result.Left := round(pt.X);
    result.Top := round(pt.Y);
    result.Right := round(pt.X)+1;
    result.Bottom := round(pt.Y)+1;
  end;

  //include specified point in the bounds
  procedure IncludePoint(pt: TPointF);
  begin
    if round(pt.X) < result.Left then result.Left := round(pt.X);
    if round(pt.Y) < result.Top then result.Top := round(pt.Y);
    if round(pt.X)+1 > result.Right then result.Right := round(pt.X)+1;
    if round(pt.Y)+1 > result.Bottom then result.Bottom := round(pt.Y)+1;
  end;

begin
  result := EmptyRect;
  if ASourceBounds.IsEmpty then exit;

  if not APixelCenteredCoords then AMatrix := AffineMatrixTranslation(-0.5,-0.5)*AMatrix*AffineMatrixTranslation(0.5,0.5);
  if IsAffineRoughlyTranslation(AMatrix,ASourceBounds) then
  begin
    result := ASourceBounds;
    result.Offset(round(AMatrix[1,3]), round(AMatrix[2,3]));
  end else
  begin
    FirstPoint(AMatrix*PointF(ASourceBounds.Left-pointMargin,ASourceBounds.Top-pointMargin));
    IncludePoint(AMatrix*PointF(ASourceBounds.Right-1+pointMargin,ASourceBounds.Top-pointMargin));
    IncludePoint(AMatrix*PointF(ASourceBounds.Left-pointMargin,ASourceBounds.Bottom-1+pointMargin));
    IncludePoint(AMatrix*PointF(ASourceBounds.Right-1+pointMargin,ASourceBounds.Bottom-1+pointMargin));
  end;
  if AClipOutput then result.Intersect(ClipRect);
end;

procedure TBGRADefaultBitmap.StretchPutImage(ARect: TRect;
  Source: TBGRACustomBitmap; mode: TDrawMode; AOpacity: byte);
var noTransition: boolean;
begin
  If (Source = nil) or (AOpacity = 0) then exit;
  if (ARect.Right-ARect.Left = Source.Width) and (ARect.Bottom-ARect.Top = Source.Height) then
     PutImage(ARect.Left,ARect.Top,Source,mode,AOpacity)
  else
  begin
     noTransition:= (mode = dmXor) or ((mode in [dmDrawWithTransparency,dmFastBlend,dmSetExceptTransparent]) and
                                       (Source is TBGRADefaultBitmap) and
                                       Assigned(TBGRADefaultBitmap(Source).XorMask));
     BGRAResample.StretchPutImage(Source, ARect.Right-ARect.Left, ARect.Bottom-ARect.Top, self, ARect.left,ARect.Top, mode, AOpacity, noTransition);
    if (mode in [dmDrawWithTransparency,dmFastBlend,dmSetExceptTransparent]) and Assigned(TBGRADefaultBitmap(Source).XorMask) then
      BGRAResample.StretchPutImage(TBGRADefaultBitmap(Source).XorMask, ARect.Right-ARect.Left, ARect.Bottom-ARect.Top, self, ARect.left,ARect.Top, dmXor, AOpacity, noTransition);
  end;
end;

procedure TBGRADefaultBitmap.BlendRect(ADest: TRect; AColor: TBGRAPixel;
  AOperation: TBlendOperation; AExcludeChannels: TChannels);
const BufSize = 8;
var srcBuf: packed array[0..BufSize-1] of TBGRAPixel;
  i, yb, remain: Integer;
  p: PBGRAPixel;
begin
  if not CheckClippedRectBounds(ADest.Left, ADest.Top, ADest.Right, ADest.Bottom) then exit;
  for i := 0 to BufSize-1 do
    srcBuf[i] := AColor;
  for yb := ADest.Top to ADest.Bottom-1 do
  begin
    remain := ADest.Width;
    p := PBGRAPixel(GetPixelAddress(ADest.Left, yb));
    while remain >= BufSize do
    begin
      BlendPixels(p, @srcBuf, AOperation, BufSize, AExcludeChannels);
      inc(p, BufSize);
      dec(remain, BufSize);
    end;
    if remain > 0 then
      BlendPixels(p, @srcBuf, AOperation, remain, AExcludeChannels);
  end;
end;

procedure TBGRADefaultBitmap.BlendRectOver(ADest: TRect; AColor: TBGRAPixel;
  AOperation: TBlendOperation; AOpacity: byte; ALinearBlend: boolean;
  AExcludeChannels: TChannels);
const BufSize = 8;
var srcBuf: packed array[0..BufSize-1] of TBGRAPixel;
  i, yb, remain: Integer;
  p: PBGRAPixel;
begin
  if not CheckClippedRectBounds(ADest.Left, ADest.Top, ADest.Right, ADest.Bottom) then exit;
  for i := 0 to BufSize-1 do
    srcBuf[i] := AColor;
  for yb := ADest.Top to ADest.Bottom-1 do
  begin
    remain := ADest.Width;
    p := PBGRAPixel(GetPixelAddress(ADest.Left, yb));
    while remain >= BufSize do
    begin
      BlendPixelsOver(p, @srcBuf, AOperation, BufSize, AOpacity, ALinearBlend, AExcludeChannels);
      inc(p, BufSize);
      dec(remain, BufSize);
    end;
    if remain > 0 then
      BlendPixelsOver(p, @srcBuf, AOperation, remain, AOpacity, ALinearBlend, AExcludeChannels);
  end;
end;

{----------------------------- Filters -----------------------------------------}
{ Call the appropriate function }

function TBGRADefaultBitmap.FilterSmartZoom3(Option: TMedianOption): TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterSmartZoom3(self, Option) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterMedian(Option: TMedianOption): TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterMedian(self, option) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterSmooth: TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterBlurRadial(self, 3, rbPrecise) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterSphere: TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterSphere(self) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterTwirl(ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterTwirl(self, ACenter, ARadius, ATurn, AExponent) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterTwirl(ABounds: TRect; ACenter: TPoint;
  ARadius: Single; ATurn: Single; AExponent: Single): TBGRADefaultBitmap;
begin
  result := BGRAFilters.FilterTwirl(self, ABounds, ACenter, ARadius, ATurn, AExponent) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterCylinder: TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterCylinder(self) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterPlane: TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterPlane(self) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterSharpen(Amount: single = 1): TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterSharpen(self,round(Amount*256)) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterSharpen(ABounds: TRect; Amount: single
  ): TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterSharpen(self,ABounds,round(Amount*256)) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterContour(AGammaCorrection: boolean = false): TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterContour(self, AGammaCorrection) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterPixelate(pixelSize: integer;
  useResample: boolean; filter: TResampleFilter): TBGRADefaultBitmap;
begin
  Result:= BGRAFilters.FilterPixelate(self, pixelSize, useResample, filter) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterEmboss(angle: single;
  AStrength: integer; AOptions: TEmbossOptions): TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterEmboss(self, angle, AStrength, AOptions) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterEmboss(angle: single; ABounds: TRect;
  AStrength: integer; AOptions: TEmbossOptions): TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterEmboss(self, angle, ABounds, AStrength, AOptions) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterEmbossHighlight(FillSelection: boolean):
TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterEmbossHighlight(self, FillSelection, BGRAPixelTransparent) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterEmbossHighlight(FillSelection: boolean;
  BorderColor: TBGRAPixel): TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterEmbossHighlight(self, FillSelection, BorderColor) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterEmbossHighlight(FillSelection: boolean;
  BorderColor: TBGRAPixel; var Offset: TPoint): TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterEmbossHighlightOffset(self, FillSelection, BorderColor, Offset) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterGrayscale: TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterGrayscale(self) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterGrayscale(ABounds: TRect): TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterGrayscale(self, ABounds) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterNormalize(eachChannel: boolean = True):
TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterNormalize(self, eachChannel) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterNormalize(ABounds: TRect; eachChannel: boolean): TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterNormalize(self, ABounds, eachChannel) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterRotate(origin: TPointF;
  angle: single; correctBlur: boolean): TBGRADefaultBitmap;
begin
  Result := BGRAFilters.FilterRotate(self, origin, angle, correctBlur) as TBGRADefaultBitmap;
end;

function TBGRADefaultBitmap.FilterAffine(AMatrix: TAffineMatrix;
  correctBlur: boolean): TBGRADefaultBitmap;
begin
  Result := NewBitmap(Width,Height);
  Result.PutImageAffine(AMatrix,self,255,correctBlur);
end;

function TBGRADefaultBitmap.GetHasTransparentPixels: boolean;
var
  p: PBGRAPixel;
  n: integer;
begin
  p := Data;
  for n := NbPixels - 1 downto 0 do
  begin
    if p^.alpha <> 255 then
    begin
      Result := True;
      exit;
    end;
    Inc(p);
  end;
  Result := False;
end;

function TBGRADefaultBitmap.GetHasSemiTransparentPixels: boolean;
var
  n: integer;
  p: PBGRAPixel;
begin
  p := Data;
  for n := NbPixels - 1 downto 0 do
  begin
    if (p^.alpha > 0) and (p^.alpha < 255) then
    begin
      result := true;
      exit;
    end;
    inc(p);
  end;
  result := false;
end;

function TBGRADefaultBitmap.GetAverageColor: TColor;
var
  pix: TBGRAPixel;
begin
  pix := GetAveragePixel;
  {$hints off}
  if pix.alpha = 0 then
    result := clNone else
     result := RGBToColor(pix.red,pix.green,pix.blue);
  {$hints on}
end;

function TBGRADefaultBitmap.GetAveragePixel: TBGRAPixel;
var
  n:     integer;
  p:     PBGRAPixel;
  r, g, b, sum: double;
  alpha: double;
begin
  sum := 0;
  r   := 0;
  g   := 0;
  b   := 0;
  p   := Data;
  for n := NbPixels - 1 downto 0 do
  begin
    alpha := p^.alpha / 255;
    incF(sum, alpha);
    incF(r, p^.red * alpha);
    incF(g, p^.green * alpha);
    incF(b, p^.blue * alpha);
    Inc(p);
  end;
  if sum = 0 then
    Result := BGRAPixelTransparent
  else
    Result := BGRA(round(r / sum),round(g / sum),round(b / sum),round(sum*255/NbPixels));
end;

function TBGRADefaultBitmap.GetPenJoinStyle: TPenJoinStyle;
begin
  result := GetInternalPen.JoinStyle;
end;

procedure TBGRADefaultBitmap.SetPenJoinStyle(const AValue: TPenJoinStyle);
begin
  GetInternalPen.JoinStyle := AValue;
end;

function TBGRADefaultBitmap.GetPenMiterLimit: single;
begin
  result := GetInternalPen.MiterLimit;
end;

procedure TBGRADefaultBitmap.SetPenMiterLimit(const AValue: single);
begin
  GetInternalPen.MiterLimit := AValue;
end;

procedure TBGRADefaultBitmap.SetCanvasOpacity(AValue: byte);
begin
  LoadFromBitmapIfNeeded;
  FCanvasOpacity := AValue;
end;

{----------------------------- Resample ---------------------------------------}

function TBGRADefaultBitmap.FineResample(NewWidth, NewHeight: integer):
TBGRACustomBitmap;
begin
  Result := BGRAResample.FineResample(self, NewWidth, NewHeight, ResampleFilter);
end;

function TBGRADefaultBitmap.SimpleStretch(NewWidth, NewHeight: integer):
TBGRACustomBitmap;
begin
  Result := BGRAResample.SimpleStretch(self, NewWidth, NewHeight);
end;

function TBGRADefaultBitmap.Resample(newWidth, newHeight: integer;
  mode: TResampleMode): TBGRADefaultBitmap;
begin
  case mode of
    rmFineResample: Result  := FineResample(newWidth, newHeight) as TBGRADefaultBitmap;
    rmSimpleStretch: Result := SimpleStretch(newWidth, newHeight) as TBGRADefaultBitmap;
    else
      Result := nil;
  end;
end;

{-------------------------------- Data functions ------------------------}

{ Compute negative with gamma correction. A negative contains
  complentary colors (black becomes white etc.).

  It is NOT EXACTLY an involution, when applied twice, some color information is lost }
procedure TBGRADefaultBitmap.Negative;
begin
  TBGRAFilterScannerNegative.ComputeFilterInplace(self, rect(0,0,FWidth,FHeight), True);
end;

procedure TBGRADefaultBitmap.NegativeRect(ABounds: TRect);
begin
  ABounds.Intersect(ClipRect);
  if ABounds.IsEmpty then exit;
  TBGRAFilterScannerNegative.ComputeFilterInplace(self, ABounds, True);
end;

{ Compute negative without gamma correction.

  It is an involution, i.e it does nothing when applied twice }
procedure TBGRADefaultBitmap.LinearNegative;
begin
  TBGRAFilterScannerNegative.ComputeFilterInplace(self, rect(0,0,FWidth,FHeight), False);
end;

procedure TBGRADefaultBitmap.LinearNegativeRect(ABounds: TRect);
begin
  ABounds.Intersect(ClipRect);
  if ABounds.IsEmpty then exit;
  TBGRAFilterScannerNegative.ComputeFilterInplace(self, ABounds, False);
end;

procedure TBGRADefaultBitmap.InplaceGrayscale(AGammaCorrection: boolean = true);
begin
  TBGRAFilterScannerGrayscale.ComputeFilterInplace(self, rect(0,0,FWidth,FHeight), AGammaCorrection);
end;

procedure TBGRADefaultBitmap.InplaceGrayscale(ABounds: TRect; AGammaCorrection: boolean = true);
begin
  ABounds.Intersect(ClipRect);
  if ABounds.IsEmpty then exit;
  TBGRAFilterScannerGrayscale.ComputeFilterInplace(self, ABounds, AGammaCorrection);
end;

procedure TBGRADefaultBitmap.InplaceNormalize(AEachChannel: boolean);
begin
  InplaceNormalize(rect(0,0,Width,Height),AEachChannel);
end;

procedure TBGRADefaultBitmap.InplaceNormalize(ABounds: TRect;
  AEachChannel: boolean);
var scanner: TBGRAFilterScannerNormalize;
begin
  ABounds.Intersect(ClipRect);
  if ABounds.IsEmpty then exit;
  scanner := TBGRAFilterScannerNormalize.Create(self,Point(0,0),ABounds,AEachChannel);
  FillRect(ABounds,scanner,dmSet);
  scanner.Free;
end;

{ Swap red and blue channels. Useful when RGB order is swapped.

  It is an involution, i.e it does nothing when applied twice }
procedure TBGRADefaultBitmap.SwapRedBlue;
begin
  TBGRAFilterScannerSwapRedBlue.ComputeFilterInplace(self, rect(0,0,FWidth,FHeight), False);
end;

procedure TBGRADefaultBitmap.SwapRedBlue(ARect: TRect);
begin
  if not CheckClippedRectBounds(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom) then exit;
  TBGRAFilterScannerSwapRedBlue.ComputeFilterInplace(self, ARect, False);
end;

{ Convert a grayscale image into a black image with alpha value }
procedure TBGRADefaultBitmap.GrayscaleToAlpha;
var
  n:    integer;
  p:    PLongword;
begin
  LoadFromBitmapIfNeeded;
  p := PLongword(Data);
  n := NbPixels;
  if n = 0 then
    exit;
  repeat
    p^   := (p^ shr TBGRAPixel_RedShift and $FF) shl TBGRAPixel_AlphaShift;
    Inc(p);
    Dec(n);
  until n = 0;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.AlphaToGrayscale;
var
  n:    integer;
  temp: LongWord;
  p:    PLongword;
begin
  LoadFromBitmapIfNeeded;
  p := PLongword(Data);
  n := NbPixels;
  if n = 0 then
    exit;
  repeat
    temp := (p^ shr TBGRAPixel_AlphaShift) and $ff;
    p^   := (temp shl TBGRAPixel_RedShift) or (temp shl TBGRAPixel_GreenShift)
         or (temp shl TBGRAPixel_BlueShift) or ($ff shl TBGRAPixel_AlphaShift);
    Inc(p);
    Dec(n);
  until n = 0;
  InvalidateBitmap;
end;

function TBGRADefaultBitmap.GetMaskFromAlpha: TBGRADefaultBitmap;
var y,x: integer;
  psrc, pdest: PBGRAPixel;
begin
  result := BGRABitmapFactory.Create(Width,Height) as TBGRADefaultBitmap;
  for y := 0 to self.Height-1 do
  begin
    psrc := self.ScanLine[y];
    pdest := result.ScanLine[y];
    for x := 0 to self.Width-1 do
    begin
      pdest^ := BGRA(psrc^.alpha,psrc^.alpha,psrc^.alpha);
      inc(psrc);
      inc(pdest);
    end;
  end;
end;

function TBGRADefaultBitmap.GetGrayscaleMaskFromAlpha: TGrayscaleMask;
var
  psrc: PBGRAPixel;
  pdest: PByte;
  y, x: Integer;
begin
  result := TGrayscaleMask.Create;
  result.SetSize(Width,Height);
  for y := 0 to self.Height-1 do
  begin
    psrc := self.ScanLine[y];
    pdest := result.ScanLine[y];
    for x := 0 to self.Width-1 do
    begin
      pdest^ := psrc^.alpha;
      inc(psrc);
      inc(pdest);
    end;
  end;
end;

procedure TBGRADefaultBitmap.ConvertToLinearRGB;
var p: PBGRAPixel;
    n: integer;
begin
  p := Data;
  for n := NbPixels-1 downto 0 do
  begin
    p^.red := GammaExpansionTab[p^.red] shr 8;
    p^.green := GammaExpansionTab[p^.green] shr 8;
    p^.blue := GammaExpansionTab[p^.blue] shr 8;
    inc(p);
  end;
end;

procedure TBGRADefaultBitmap.ConvertFromLinearRGB;
var p: PBGRAPixel;
    n: integer;
begin
  p := Data;
  for n := NbPixels-1 downto 0 do
  begin
    p^.red := GammaCompressionTab[p^.red shl 8 + p^.red];
    p^.green := GammaCompressionTab[p^.green shl 8 + p^.green];
    p^.blue := GammaCompressionTab[p^.blue shl 8 + p^.blue];
    inc(p);
  end;
end;

{ Make a copy of the transparent bitmap to a TBitmap with a background color
  instead of transparency }
function TBGRADefaultBitmap.MakeBitmapCopy(BackgroundColor: TColor): TBitmap;
var
  opaqueCopy: TBGRACustomBitmap;
begin
  Result     := TBitmap.Create;
  Result.Width := Width;
  Result.Height := Height;
  opaqueCopy := NewBitmap(Width, Height);
  opaqueCopy.Fill(BackgroundColor);
  opaqueCopy.PutImage(0, 0, self, dmDrawWithTransparency);
  opaqueCopy.Draw(Result.canvas, 0, 0, True);
  opaqueCopy.Free;
end;

function TBGRADefaultBitmap.GetPtrBitmap(Top, Bottom: Integer
  ): TBGRACustomBitmap;
var temp: integer;
    ptrbmp: TBGRAPtrBitmap;
begin
  if Top > Bottom then
  begin
    temp := Top;
    Top := Bottom;
    Bottom := Temp;
  end;
  if Top < 0 then Top := 0;
  if Bottom > Height then Bottom := Height;
  if Top >= Bottom then
    result := nil
  else
  begin
    if LineOrder = riloTopToBottom then
      ptrbmp := CreatePtrBitmap(Width,Bottom-Top,ScanLine[Top]) else
      ptrbmp := CreatePtrBitmap(Width,Bottom-Top,ScanLine[Bottom-1]);
    ptrbmp.LineOrder := LineOrder;
    result := ptrbmp;
  end;
end;

{-------------------------- Allocation routines -------------------------------}

function TBGRADefaultBitmap.CreatePtrBitmap(AWidth, AHeight: integer;
  AData: PBGRAPixel): TBGRAPtrBitmap;
begin
  result := TBGRAPtrBitmap.Create(AWidth,AHeight,AData);
end;

procedure TBGRADefaultBitmap.FreeBitmap;
begin
  FreeAndNil(FBitmap);
end;

function TBGRADefaultBitmap.GetCanvasOpacity: byte;
begin
  result:= FCanvasOpacity;
end;

function TBGRADefaultBitmap.GetFontHeight: integer;
begin
  result := FFontHeight;
end;

{ TBGRAPtrBitmap }

function TBGRAPtrBitmap.GetLineOrder: TRawImageLineOrder;
begin
  result := inherited GetLineOrder;
end;

procedure TBGRAPtrBitmap.SetLineOrder(AValue: TRawImageLineOrder);
begin
  inherited SetLineOrder(AValue);
end;

procedure TBGRAPtrBitmap.ReallocData;
begin
  //nothing
end;

procedure TBGRAPtrBitmap.FreeData;
begin
  FDataByte := nil;
end;

procedure TBGRAPtrBitmap.CannotResize;
begin
  raise exception.Create('A pointer bitmap cannot be resized');
end;

procedure TBGRAPtrBitmap.NotImplemented;
begin
  raise exception.Create('Not implemented');
end;

procedure TBGRAPtrBitmap.RebuildBitmap;
begin
  NotImplemented;
end;

function TBGRAPtrBitmap.CreateDefaultFontRenderer: TBGRACustomFontRenderer;
begin
  result := nil;
  NotImplemented;
end;

function TBGRAPtrBitmap.LoadFromRawImage(ARawImage: TRawImage;
  DefaultOpacity: byte; AlwaysReplaceAlpha: boolean;
  RaiseErrorOnInvalidPixelFormat: boolean): boolean;
begin
  result := false;
  NotImplemented;
end;

constructor TBGRAPtrBitmap.Create(AWidth, AHeight: integer; AData: Pointer);
begin
  inherited Create(AWidth, AHeight);
  SetDataPtr(AData);
end;

procedure TBGRAPtrBitmap.SetDataPtr(AData: Pointer);
begin
  FDataByte := AData;
end;

procedure TBGRAPtrBitmap.DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
begin
  NotImplemented;
end;

procedure TBGRAPtrBitmap.DataDrawOpaque(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
begin
  NotImplemented;
end;

procedure TBGRAPtrBitmap.GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer
  );
begin
  NotImplemented;
end;

procedure TBGRAPtrBitmap.Assign(Source: TPersistent);
begin
  CannotResize;
end;

procedure TBGRAPtrBitmap.TakeScreenshot(ARect: TRect);
begin
  CannotResize;
end;

procedure TBGRAPtrBitmap.TakeScreenshotOfPrimaryMonitor;
begin
  CannotResize;
end;

procedure TBGRAPtrBitmap.LoadFromDevice(DC: HDC);
begin
  NotImplemented;
end;

procedure TBGRAPtrBitmap.LoadFromDevice(DC: HDC; ARect: TRect);
begin
  NotImplemented;
end;

procedure BGRAGradientFill(bmp: TBGRACustomBitmap; x, y, x2, y2: integer;
  c1, c2: TBGRAPixel; gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
  gammaColorCorrection: boolean = True; Sinus: Boolean=False);
begin
  bmp.GradientFill(x,y,x2,y2,c1,c2,gtype,o1,o2,mode,gammaColorCorrection,sinus);
end;

initialization

  with DefaultTextStyle do
  begin
    Alignment  := taLeftJustify;
    Layout     := tlTop;
    WordBreak  := True;
    SingleLine := True;
    Clipping   := True;
    ShowPrefix := False;
    Opaque     := False;
  end;

end.

