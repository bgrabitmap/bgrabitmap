{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}

type
  { Working area to be provided to a brush }
  PUniBrushContext = ^TUniBrushContext;
  TUniBrushContext = record
    Dest: Pointer;                    //target pixels
    Ofs: TPoint;                      //offset within image (used with textured/gradient brush)
    Custom: array[0..15] of byte;     //custom context information
  end;

  { Additional initialization of the brush context (optional) }
  TUniBrushInitContextProc = procedure
    (AFixedData: Pointer;             //address of brush fixed data
     AContextData: PUniBrushContext); //address of brush contextual data

  { Outputs pixels from current position defined in context }
  TUniBrushPutNextPixelsProc = procedure
    (AFixedData: Pointer;             //address of brush data
     AContextData: PUniBrushContext;  //address of brush contextual data
     AAlpha: Word;                    //global alpha modifier (0 to draw nothing,
                                      // 65535 to draw without opacity adjustment)
     ACount: integer);                //number of pixels to output

  { TUniversalBrush }

  PUniversalBrush = ^TUniversalBrush;
  TUniversalBrush = record
    Colorspace: TColorspaceAny;
    FixedData: array[0..31] of byte;
    InternalInitContext: TUniBrushInitContextProc;       //do not call directly
    InternalPutNextPixels: TUniBrushPutNextPixelsProc;   //do not call directly
    procedure MoveTo(AContext: PUniBrushContext; ADest: pointer; AOfsX,AOfsY: integer); inline;
    procedure PutNextPixels(AContext: PUniBrushContext; AAlpha: Word; ACount: integer); inline;
  end;

type
  PDefaultSolidBrushIndirectFixedData = ^TDefaultSolidBrushIndirectFixedData;
  TDefaultSolidBrushIndirectFixedData = packed record
    PixelSize: integer;
    Color: packed array[0..23] of byte;
  end;

procedure DefaultSolidBrushIndirectSkipPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; {%H-}AAlpha: Word; ACount: integer);
procedure DefaultSolidBrushIndirectSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; {%H-}AAlpha: Word; ACount: integer);

type
  TPathCallbackData = record
    BrushAddress: PUniversalBrush;
    Alpha: Word;
    PixelCenteredCoords: boolean;
  end;

  { TCustomUniversalBitmap }

  TCustomUniversalBitmap = class(TFPCustomImage)
  private
    procedure PathFillAliasedCallback(const APoints: array of TPointF;
      AData: pointer);
    procedure PathStrokeAliasedCallback(const APoints: array of TPointF;
      AClosed: boolean; AData: Pointer);

  protected
    FRefCount: integer;
    FColorspace: TColorspaceAny;
    FReferenceWhite: PXYZReferenceWhite;
    FWidth: integer;
    FHeight: integer;
    FNbPixels: integer;
    FPixelSize: integer;
    FRowSize: PtrInt;
    FDataByte: PByte;
    FLineOrder: TRawImageLineOrder;
    FClipRect: TRect;
    FConvertToFPColor, FConvertFromFPColor,
    FConvertFromBGRA, FConvertFromExpanded: TColorspaceConvertArrayProc;
    FPenStroker: TBGRACustomPenStroker;

    {** Specifies if linear antialiasing must be used when drawing
        antialiased shapes }
    FAntialiasingDrawMode: TDrawMode;

    procedure Init; virtual;
    procedure ReallocData; virtual;
    procedure FreeData; virtual;

    {TFPCustomImage}
    procedure SetInternalColor(x, y: integer; const Value: TFPColor); override;
    function GetInternalColor(x, y: integer): TFPColor; override;
    procedure SetInternalPixel(x, y: integer; Value: integer); override;
    function GetInternalPixel(x, y: integer): integer; override;

    function CheckEmpty: boolean; virtual;
    function CheckIsZero: boolean; virtual;
    function GetClipRect: TRect; virtual;
    function GetDataBytePtr: PByte; virtual;
    function GetHasSemiTransparentPixels: boolean; virtual;
    function GetHasTransparentPixels: boolean; virtual;
    function GetHeight: integer; virtual;
    function GetLineOrder: TRawImageLineOrder; virtual;
    procedure SetLineOrder(AValue: TRawImageLineOrder); virtual;
    function GetNbPixels: integer; virtual;
    function GetRefCount: integer; virtual;
    function GetScanLineByte(y: integer): PByte; virtual;
    function GetWidth: integer; virtual;
    procedure SetClipRect(const AValue: TRect); virtual;
    procedure RaiseInvalidBrushColorspace; virtual;
    procedure RaiseMissingUniDrawer; virtual;
    function CheckHorizLineBounds(var x:int32or64; y: int32or64; var x2: int32or64): boolean; inline;
    function CheckVertLineBounds(x: int32or64; var y, y2: int32or64): boolean; inline;
    class function DefaultColorspace: TColorspaceAny; virtual;
    function InternalDuplicate(ACopyProperties: boolean=false): TCustomUniversalBitmap; virtual;
    function InternalNew: TCustomUniversalBitmap; virtual;
    procedure InternalCopyPixels(ASource,ADest: PByte; ASourceStride,ADestStride: PtrInt; ACount: integer); virtual;
    procedure InternalSwapPixels(ABuf1,ABuf2: PByte; AStride1,AStride2: PtrInt; ACount: integer); virtual;
    procedure InternalSetPixels(ASource,ADest: PByte; ADestStride: PtrInt; ACount: integer); virtual;
    procedure AssignTransparentPixel(out ADest); virtual;
    function GetLinearAntialiasing: boolean; virtual;
    procedure SetLinearAntialiasing(AValue: boolean); virtual;
    procedure SetAntialiasingDrawMode(AValue: TDrawMode); virtual;

  public

    {** User defined caption. It does not appear on the image }
    Caption:   string;

    {** Method to use when filling polygons (winding or alternate).
        See [[BGRABitmap Types imported from Graphics|BGRAGraphics]] }
    FillMode:  TFillMode;

    {** Creates an image of width and height equal to zero. In this case,
        ''DataByte'' = '''nil''' }
    constructor Create; overload;
    constructor Create(AColorspace: TColorspaceAny; ALineOrder: TRawImageLineOrder); overload;
    constructor Create(AColorspace: TColorspaceAny; AWidth, AHeight: integer;
                       ALineOrder: TRawImageLineOrder); overload;

    {** Adds a reference (this reference count is not the same as
        the reference count of an interface, it changes only by
        explicit calls }
    function NewReference: TCustomUniversalBitmap;
    {** Free a reference. When the resulting reference count gets
        to zero, the image is freed. The initial reference count
        is equal to 1 }
    procedure FreeReference;
    {** Returns an object with a reference count equal to 1. Duplicate
        this bitmap if necessary }
    function GetUnique: TCustomUniversalBitmap;
    function Duplicate(ACopyProperties: boolean=false): TCustomUniversalBitmap;
    procedure CopyPropertiesTo(ABitmap: TCustomUniversalBitmap); virtual;
    {** Get a part of the image with repetition in both directions. It means
      that if the bounds are within the image, the result is just that part
      of the image, but if the bounds are bigger than the image, the image
      is tiled. }
    function GetPart(const ARect: TRect): TCustomUniversalBitmap;

    procedure InvalidateBitmap; virtual;         //call if you modify with Scanline
    procedure LoadFromBitmapIfNeeded; virtual;   //call to ensure that data is up to date

    class procedure EraseBrush(out {%H-}ABrush: TUniversalBrush; {%H-}AAlpha: Word); virtual;
    class procedure AlphaBrush(out {%H-}ABrush: TUniversalBrush; {%H-}AAlpha: Word); virtual;
    procedure SolidBrushBGRA(out ABrush: TUniversalBrush; ARed,AGreen,ABlue,AAlpha: Byte; ADrawMode: TDrawMode = dmDrawWithTransparency); overload; virtual;
    procedure SolidBrushBGRA(out ABrush: TUniversalBrush; AColor: TBGRAPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); overload; virtual;
    procedure SolidBrushExpanded(out ABrush: TUniversalBrush; ARed,AGreen,ABlue,AAlpha: Word; ADrawMode: TDrawMode = dmDrawWithTransparency); overload; virtual;
    procedure SolidBrushExpanded(out ABrush: TUniversalBrush; AColor: TExpandedPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); overload; virtual;
    procedure SolidBrushIndirect(out ABrush: TUniversalBrush; AColor: Pointer; ADrawMode: TDrawMode = dmDrawWithTransparency); virtual;
    class procedure ScannerBrush(out {%H-}ABrush: TUniversalBrush; {%H-}AScanner: IBGRAScanner; {%H-}ADrawMode: TDrawMode = dmDrawWithTransparency;
                                 {%H-}AOffsetX: integer = 0; {%H-}AOffsetY: integer = 0); virtual;

    {TFPCustomImage}
    {** Creates a new bitmap, initialize properties and bitmap data }
    constructor Create(AWidth, AHeight: integer); overload; override;
    {** Sets the dimension of an existing bitmap /!\ Data can be random }
    procedure SetSize(AWidth, AHeight: integer); override;
    destructor Destroy; override;

    {** Stores the image in the stream without compression nor header }
    procedure Serialize(AStream: TStream); virtual;
    {** Reads the image in a stream that was previously serialized }
    procedure Deserialize(AStream: TStream); virtual;
    {** Stores an empty image (of size zero) }
    class procedure SerializeEmpty(AStream: TStream); static;

    {** Stop clipping (clipping rectangle becomes the whole image) }
    procedure NoClip; virtual;
    {** Checks if the specified point is in the clipping rectangle ''ClipRect'' }
    function PtInClipRect(x, y: int32or64): boolean; inline;
    {** Check if the bounds are within the clipping rectangle and adjust the coordinates to fit
        (similar to IntersectRect but may flip the coordinates) }
    function CheckClippedRectBounds(var x,y,x2,y2: integer): boolean;

    {** Fill the whole image regardless of clipping rect }
    procedure Fill(const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;
    procedure Fill(ATexture: IBGRAScanner; AMode: TDrawMode); overload; virtual;
    procedure Fill(ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word); overload; virtual;
    procedure FillTransparent; virtual;
    procedure AlphaFill(alpha: byte); virtual;
    procedure ApplyGlobalOpacity(alpha: byte); overload; virtual;

    {** Fills completely a rectangle, without any border }
    procedure FillRect(ALeft, ATop, ARight, ABottom: integer; const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;
    procedure FillRect(const ARect: TRect; const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;
    procedure FillRect(ALeft, ATop, ARight, ABottom: integer; ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    procedure FillRect(const ARect: TRect; ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    procedure FillRect(ALeft, ATop, ARight, ABottom: integer; ATexture: IBGRAScanner; AMode: TDrawMode; AScanOffset: TPoint; AAlpha: Word = 65535); overload; virtual;
    procedure FillRect(const ARect: TRect; ATexture: IBGRAScanner; AMode: TDrawMode; AScanOffset: TPoint; AAlpha: Word = 65535); overload; virtual;
    procedure EraseRect(ALeft, ATop, ARight, ABottom: integer; alpha: byte); virtual;
    procedure EraseRect(const ARect: TRect; alpha: byte); virtual;
    procedure AlphaFillRect(ALeft, ATop, ARight, ABottom: integer; alpha: byte); virtual;
    procedure AlphaFillRect(const ARect: TRect; alpha: byte); virtual;
    procedure ApplyGlobalOpacity(ARect: TRect; alpha: byte); overload; virtual;

    {** Returns the address of a pixel. /!\ Does not check if the coordinates are valid. }
    function GetPixelAddress(x,y: integer): PByte; virtual;
    procedure SetPixelIndirect(x,y: int32or64; AColor: pointer); virtual;
    procedure GetPixelIndirect(x,y: int32or64; AColor: pointer); virtual;
    procedure GetPixelCycleIndirect(x,y: int32or64; AColor: pointer); virtual;
    procedure DrawPixel(x,y: Int32or64; const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;
    procedure DrawPixel(x,y: Int32or64; ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    {** Erase the content of the pixel by reducing the value of the
        alpha channel. ''alpha'' specifies how much to decrease.
        If the resulting alpha reaches zero, the content
        is replaced by the transparent pixel }
    procedure ErasePixel(x, y: int32or64; alpha: byte); virtual;
    {** Sets the alpha value at (''x'',''y''). If ''alpha'' = 0, the
        pixel is replaced by the transparent pixel }
    procedure AlphaPixel(x, y: int32or64; alpha: byte); virtual;
    procedure HorizLine(x, y, x2: int32or64; const ABrush: TUniversalBrush; AAlpha: Word = 65535); virtual;
    {** Draws an horizontal line at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified scanner
        and the specified ''AMode'' }
    procedure HorizLine(x, y, x2: int32or64; ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    procedure EraseHorizLine(x,y,x2: int32or64; alpha: byte); virtual;
    procedure AlphaHorizLine(x,y,x2: int32or64; alpha: byte); virtual;
    procedure VertLine(x, y, y2: int32or64; const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;
    procedure VertLine(x, y, y2: int32or64; ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    procedure EraseVertLine(x, y, y2: int32or64; alpha: byte); virtual;
    procedure AlphaVertLine(x, y, y2: int32or64; alpha: byte); virtual;

    {** Draws an aliased line from (x1,y1) to (x2,y2) using Bresenham's algorithm.
        ''DrawLastPixel'' specifies if (x2,y2) must be drawn. }
    procedure DrawLine(x1, y1, x2, y2: integer; const ABrush: TUniversalBrush; ADrawLastPixel: boolean; AAlpha: Word = 65535); overload; virtual;
    procedure DrawLine(x1, y1, x2, y2: integer; ATexture: IBGRAScanner; AMode: TDrawMode; ADrawLastPixel: boolean; AAlpha: Word = 65535); overload; virtual;
    {** Draws an antialiased line from (x1,y1) to (x2,y2) using an improved version of Bresenham's algorithm
        ''DrawLastPixel'' specifies if (x2,y2) must be drawn }
    procedure DrawLineAntialias(x1, y1, x2, y2: integer; const ABrush: TUniversalBrush; ADrawLastPixel: boolean; AAlpha: Word = 65535); overload; virtual;
    procedure DrawLineAntialias(x1, y1, x2, y2: integer; ATexture: IBGRAScanner; AMode: TDrawMode; ADrawLastPixel: boolean; AAlpha: Word = 65535); overload; virtual;
    {** Draws an antialiased line with two brushes as dashes of length ''ADashLen''.
        ''ADashPos'' can be used to specify the start dash position and to retrieve the dash position at the end
        of the line, in order to draw a polyline with consistent dashes }
    procedure DrawLineAntialias(x1, y1, x2, y2: integer; const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer; ADrawLastPixel: boolean; AAlpha: Word = 65535); overload; virtual;
    procedure DrawLineAntialias(x1, y1, x2, y2: integer; const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer; var ADashPos: integer; ADrawLastPixel: boolean; AAlpha: Word = 65535); overload; virtual;

    {** Erases the line from (x1,y1) to (x2,y2) using Bresenham's algorithm.
        ''alpha'' specifies how much to decrease. If ''alpha'' = 0, nothing
        is changed and if ''alpha'' = 255, all pixels become transparent.
        ''DrawListPixel'' specifies if (x2,y2) must be changed }
    procedure EraseLine(x1, y1, x2, y2: integer; alpha: byte; DrawLastPixel: boolean); virtual;
    {** Erases the line from (x1,y1) to (x2,y2) width antialiasing.
        ''alpha'' specifies how much to decrease. If ''alpha'' = 0, nothing
        is changed and if ''alpha'' = 255, all pixels become transparent.
        ''DrawListPixel'' specifies if (x2,y2) must be changed }
    procedure EraseLineAntialias(x1, y1, x2, y2: integer; alpha: byte; DrawLastPixel: boolean); virtual;
    procedure AlphaLine(x1, y1, x2, y2: integer; alpha: byte; DrawLastPixel: boolean); virtual;
    procedure AlphaLineAntialias(x1, y1, x2, y2: integer; alpha: byte; DrawLastPixel: boolean); virtual;

    procedure DrawPolyLine(const points: array of TPoint; const ABrush: TUniversalBrush; ADrawLastPixel: boolean; AAlpha: Word = 65535);
    procedure DrawPolyLineAntialias(const points: array of TPoint; const ABrush: TUniversalBrush; ADrawLastPixel: boolean; AAlpha: Word = 65535); overload;
    procedure DrawPolyLineAntialias(const points: array of TPoint; const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer; ADrawLastPixel: boolean; AAlpha: Word = 65535); overload;
    procedure ErasePolyLine(const points: array of TPoint; alpha: byte; ADrawLastPixel: boolean);
    procedure ErasePolyLineAntialias(const points: array of TPoint; alpha: byte; ADrawLastPixel: boolean);
    procedure AlphaPolyLine(const points: array of TPoint; alpha: byte; ADrawLastPixel: boolean);
    procedure AlphaPolyLineAntialias(const points: array of TPoint; alpha: byte; ADrawLastPixel: boolean);

    procedure DrawPolygon(const points: array of TPoint; const ABrush: TUniversalBrush; AAlpha: Word = 65535);
    procedure DrawPolygonAntialias(const points: array of TPoint; const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload;
    procedure DrawPolygonAntialias(const points: array of TPoint; const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer; AAlpha: Word = 65535); overload;
    procedure ErasePolygonOutline(const points: array of TPoint; alpha: byte);
    procedure ErasePolygonOutlineAntialias(const points: array of TPoint; alpha: byte);
    procedure AlphaPolygonOutline(const points: array of TPoint; alpha: byte);
    procedure AlphaPolygonOutlineAntialias(const points: array of TPoint; alpha: byte);

    procedure DrawPathAliased(APath: IBGRAPath; const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); overload;
    procedure DrawPathAliased(APath: IBGRAPath; const AMatrix: TAffineMatrix; const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); overload;

    {==== Rectangles (integer coordinates) ====}
    {* The integer coordinates of rectangles interpreted such that
       that the bottom/right pixels are not drawn. The width is equal
       to x2-x, and pixels are drawn from x to x2-1. If x = x2, then nothing
       is drawn. See [[BGRABitmap tutorial 13|coordinate system]].
     * These functions do not take into account current pen style/cap/join.
       They draw a continuous 1-pixel width border }

    {** Draw the border of a rectangle }
    procedure Rectangle(x, y, x2, y2: integer; const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;
    procedure Rectangle(x, y, x2, y2: integer; ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    procedure Rectangle(const ARect: TRect; const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;
    procedure Rectangle(const ARect: TRect; ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    {** Draw a filled rectangle with a border }
    procedure Rectangle(x, y, x2, y2: integer; const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;
    procedure Rectangle(const ARect: TRect; const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;

    procedure RoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; const ABorderBrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;
    procedure RoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    procedure RoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;
    procedure FillRoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; const AFillBrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;
    procedure FillRoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    procedure EraseRoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; alpha: byte); virtual;
    procedure AlphaFillRoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; alpha: byte); virtual;

    procedure EllipseInRect(ARect: TRect; const ABorderBrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;
    procedure EllipseInRect(ARect: TRect; ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    procedure EllipseInRect(ARect: TRect; const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;
    procedure FillEllipseInRect(ARect: TRect; const AFillBrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;
    procedure FillEllipseInRect(ARect: TRect; ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    procedure EraseEllipseInRect(ARect: TRect; alpha: byte); virtual;
    procedure AlphaFillEllipseInRect(ARect: TRect; alpha: byte); virtual;

    procedure FillShape(AShape: TBGRACustomFillInfo; const ABrush: TUniversalBrush; AAlpha: Word = 65535); virtual;
    procedure FillShape(AShape: TBGRACustomFillInfo; ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word = 65535); virtual;
    procedure EraseShape(AShape: TBGRACustomFillInfo; alpha: byte); virtual;
    procedure AlphaFillShape(AShape: TBGRACustomFillInfo; alpha: byte); virtual;

    procedure FillPoly(const APoints: array of TPointF; const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); overload; virtual;
    procedure FillPoly(const APoints: array of TPointF; ATexture: IBGRAScanner; AMode: TDrawMode; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); overload; virtual;
    procedure ErasePoly(const APoints: array of TPointF; alpha: byte; APixelCenteredCoordinates: boolean = true); virtual;
    procedure AlphaFillPoly(const APoints: array of TPointF; alpha: byte; APixelCenteredCoordinates: boolean = true); virtual;

    procedure FillPathAliased(APath: IBGRAPath; const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); overload;
    procedure FillPathAliased(APath: IBGRAPath; const AMatrix: TAffineMatrix; const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); overload;
    procedure ErasePathAliased(APath: IBGRAPath; alpha: byte; APixelCenteredCoordinates: boolean = true); overload;
    procedure ErasePathAliased(APath: IBGRAPath; const AMatrix: TAffineMatrix; alpha: byte; APixelCenteredCoordinates: boolean = true); overload;
    procedure AlphaFillPathAliased(APath: IBGRAPath; alpha: byte; APixelCenteredCoordinates: boolean = true); overload;
    procedure AlphaFillPathAliased(APath: IBGRAPath; const AMatrix: TAffineMatrix; alpha: byte; APixelCenteredCoordinates: boolean = true); overload;

    property Colorspace: TColorspaceAny read FColorspace;

    procedure VerticalFlip; overload; virtual;
    procedure VerticalFlip(ARect: TRect); overload; virtual;
    procedure HorizontalFlip; overload; virtual;
    procedure HorizontalFlip(ARect: TRect); overload; virtual;
    procedure RotateUDInplace; overload; virtual;
    procedure RotateUDInplace(ARect: TRect); overload; virtual;

    { Return a new bitmap rotated in a clock wise direction. }
    function RotateCW: TCustomUniversalBitmap;
    { Return a new bitmap rotated in a counter clock wise direction. }
    function RotateCCW: TCustomUniversalBitmap;
    { Return a new bitmap rotated 180° (upside down). }
    function RotateUD: TCustomUniversalBitmap;

    {** Width of the image in pixels }
    property Width: integer Read GetWidth;
    {** Height of the image in pixels }
    property Height: integer Read GetHeight;

    {** Size in bytes of a row of pixels }
    property RowSize: PtrInt read FRowSize;

    {** Clipping rectangle for all drawing functions }
    property ClipRect: TRect read GetClipRect write SetClipRect;

    property LinearAntialiasing: boolean read GetLinearAntialiasing write SetLinearAntialiasing;
    property AntialiasingDrawMode: TDrawMode read FAntialiasingDrawMode write SetAntialiasingDrawMode;

    {** Total number of pixels. It is always true that ''NbPixels'' = ''Width'' * ''Height'' }
    property NbPixels: integer Read GetNbPixels;

    {** Returns the address of the left-most pixel of any line.
        The parameter y ranges from 0 to Height-1 }
    property ScanLineByte[y: integer]: PByte Read GetScanLineByte;

    {** Indicates the order in which lines are stored in memory.
        If it is equal to ''riloTopToBottom'', the first line is the top line.
        If it is equal to ''riloBottomToTop'', the first line is the bottom line.
        See [[BGRABitmap Miscellaneous types|miscellaneous types]] }
    property LineOrder: TRawImageLineOrder Read GetLineOrder write SetLineOrder;

    {** Provides a pointer to the first pixel in memory.
        Depending on the ''LineOrder'' property, this can be the top-left pixel or the bottom-left pixel.
        There is no padding between scanlines.
        See [[BGRABitmap tutorial 4]] }
    property DataByte: PByte Read GetDataBytePtr;

    {** Number of references to this image. It is increased by the function
        ''NewReference'' and decreased by the function ''FreeReference'' }
    property RefCount: integer Read GetRefCount;

    {** Returns True if the bitmap only contains transparent pixels or has a size of zero }
    property Empty: boolean Read CheckEmpty;

    {** Returns True if the bitmap is filled with zero values or has a size of zero }
    property IsZero: boolean Read CheckIsZero;

    {** Returns True if there are transparent and so if the image would
        be stored at least with an alpha channel of 1 bit }
    property HasTransparentPixels: boolean Read GetHasTransparentPixels;

    {** Returns True if there are semitransparent pixels
        and so if the image would be stored with an alpha channel of at least 8 bit }
    property HasSemiTransparentPixels: boolean Read GetHasSemiTransparentPixels;

    {** Current reference white used for color conversion }
    property ReferenceWhite: PXYZReferenceWhite read FReferenceWhite write FReferenceWhite;


  protected {----------- pen style ----------------}
    function GetArrow: TBGRACustomArrow; virtual;
    function GetLineCap: TPenEndCap; virtual;
    function GetPenStroker: TBGRACustomPenStroker; virtual;
    procedure SetLineCap(AValue: TPenEndCap); virtual;

  public
     {** How to draw the ends of a line (applies to arrow as well) }
     property LineCap: TPenEndCap read GetLineCap write SetLineCap;
     {** You can use this class to generate strokes polygonal representations }
     property Pen: TBGRACustomPenStroker read GetPenStroker;
     {** Properties of arrow }
     property Arrow: TBGRACustomArrow read GetArrow;

    procedure DrawLineAntialias(x1, y1, x2, y2: single; const ABrush: TUniversalBrush; APenWidth: single; AAlpha: Word = 65535); overload; virtual;

  end;

  TCustomUniversalDrawer = class
    {** Draws an aliased line from (x1,y1) to (x2,y2) using Bresenham's algorithm.
        ''DrawLastPixel'' specifies if (x2,y2) must be drawn. }
    class procedure DrawLine(ADest: TCustomUniversalBitmap; x1, y1, x2, y2: integer; const ABrush: TUniversalBrush; ADrawLastPixel: boolean; AAlpha: Word = 65535); virtual; abstract;
    {** Draws an antialiased line from (x1,y1) to (x2,y2) using an improved version of Bresenham's algorithm
        ''DrawLastPixel'' specifies if (x2,y2) must be drawn }
    class procedure DrawLineAntialias(ADest: TCustomUniversalBitmap; x1, y1, x2, y2: integer; const ABrush: TUniversalBrush; ADrawLastPixel: boolean; AAlpha: Word = 65535); overload; virtual; abstract;
    {** Draws an antialiased line with two brushes as dashes of length ''ADashLen''.
        ''ADashPos'' specifies the start dash position and allows to retrieve the dash position at the end
        of the line, in order to draw a polyline with consistent dashes }
    class procedure DrawLineAntialias(ADest: TCustomUniversalBitmap; x1, y1, x2, y2: integer; const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer; var DashPos: integer; DrawLastPixel: boolean; AAlpha: Word = 65535); overload; virtual; abstract;

    class procedure DrawPolyLine(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush: TUniversalBrush; ADrawLastPixel: boolean; AAlpha: Word = 65535); virtual; abstract;
    class procedure DrawPolyLineAntialias(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush: TUniversalBrush; DrawLastPixel: boolean; AAlpha: Word = 65535); overload; virtual; abstract;
    class procedure DrawPolyLineAntialias(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer; DrawLastPixel: boolean; AAlpha: Word = 65535); overload; virtual; abstract;

    class procedure DrawPolygon(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush: TUniversalBrush; AAlpha: Word = 65535); virtual; abstract;
    class procedure DrawPolygonAntialias(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual; abstract;
    class procedure DrawPolygonAntialias(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer; AAlpha: Word = 65535); overload; virtual; abstract;

    {** Draw the border of a rectangle }
    class procedure Rectangle(ADest: TCustomUniversalBitmap; x, y, x2, y2: integer; const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual; abstract;
    {** Draw a filled rectangle with a border }
    class procedure Rectangle(ADest: TCustomUniversalBitmap; x, y, x2, y2: integer; const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual; abstract;

    class procedure RoundRect(ADest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer; DX, DY: integer; const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual; abstract;
    class procedure RoundRect(ADest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer; DX, DY: integer; const ABorderBrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual; abstract;
    class procedure FillRoundRect(ADest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer; DX, DY: integer; const AFillBrush: TUniversalBrush; AAlpha: Word = 65535); virtual; abstract;

    class procedure FillShape(ADest: TCustomUniversalBitmap; AShape: TBGRACustomFillInfo; AFillMode: TFillMode; ABrush: TUniversalBrush; AAlpha: Word = 65535); virtual; abstract;
    class procedure FillPoly(ADest: TCustomUniversalBitmap; const APoints: array of TPointF; AFillMode: TFillMode; ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); virtual; abstract;

    // using pen
    class function CreatePenStroker: TBGRACustomPenStroker; virtual; abstract;
    class function CreateArrow: TBGRACustomArrow; virtual; abstract;

    class procedure DrawLineAntialias(ADest: TCustomUniversalBitmap; x1, y1, x2, y2: single; const ABrush: TUniversalBrush; APenWidth: single; AAlpha: Word = 65535); overload; virtual; abstract;

  end;
  TUniversalDrawerAny = class of TCustomUniversalDrawer;

var
  UniDrawerClass: TUniversalDrawerAny;

{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}

{ TUniversalBrush }

procedure TUniversalBrush.MoveTo(AContext: PUniBrushContext; ADest: pointer;
  AOfsX, AOfsY: integer);
begin
  with AContext^ do
  begin
    AContext^.Dest:= ADest;
    AContext^.Ofs.X := AOfsX;
    AContext^.Ofs.Y := AOfsY;
  end;
  if Assigned(InternalInitContext) then
    InternalInitContext(@FixedData, AContext);
end;

procedure TUniversalBrush.PutNextPixels(AContext: PUniBrushContext;
  AAlpha: Word; ACount: integer);
begin
  InternalPutNextPixels(@FixedData, AContext, AAlpha, ACount);
end;

{ TCustomUniversalBitmap }

function TCustomUniversalBitmap.CheckEmpty: boolean;
var
  alphaIdx, i: Integer;
  p: PByte;
begin
  alphaIdx := Colorspace.IndexOfAlphaChannel;
  if (alphaIdx=-1) or (FDataByte=nil) then exit(false);
  p := FDataByte;
  for i := NbPixels-1 downto 0 do
  begin
    if Colorspace.GetColorTransparency(p) <> ctFullyTransparent then exit(true);
    inc(p, FPixelSize);
  end;
  exit(false);
end;

function TCustomUniversalBitmap.CheckIsZero: boolean;
var
  i, dataSize: PtrInt;
  p: PByte;
begin
  p := DataByte;
  if p = nil then exit(true);
  dataSize := FNbPixels*IntPtr(FPixelSize);
  for i := (dataSize shr 3) - 1 downto 0 do
  begin
    if PQWord(p)^ <> 0 then exit(false);
    Inc(p,8);
  end;
  for i := (dataSize and 7) - 1 downto 0 do
  begin
    if PByte(p)^ <> 0 then exit(false);
    inc(p);
  end;
  Result := True;
end;

function TCustomUniversalBitmap.GetClipRect: TRect;
begin
  result := FClipRect;
end;

function TCustomUniversalBitmap.GetDataBytePtr: PByte;
begin
  LoadFromBitmapIfNeeded;
  result := FDataByte;
end;

function TCustomUniversalBitmap.GetHasSemiTransparentPixels: boolean;
var
  alphaIdx, i: Integer;
  p: PByte;
begin
  alphaIdx := Colorspace.IndexOfAlphaChannel;
  if (alphaIdx=-1) or (FDataByte=nil) then exit(false);
  p := FDataByte;
  for i := NbPixels-1 downto 0 do
  begin
    if Colorspace.GetColorTransparency(p) = ctSemiTransparent then exit(true);
    inc(p, FPixelSize);
  end;
  exit(false);
end;

function TCustomUniversalBitmap.GetHasTransparentPixels: boolean;
var
  alphaIdx, i: Integer;
  p: PByte;
begin
  alphaIdx := Colorspace.IndexOfAlphaChannel;
  if (alphaIdx=-1) or (FDataByte=nil) then exit(false);
  p := FDataByte;
  for i := NbPixels-1 downto 0 do
  begin
    if Colorspace.GetColorTransparency(p) <> ctFullyOpaque then exit(true);
    inc(p, FPixelSize);
  end;
  exit(false);
end;

function TCustomUniversalBitmap.GetHeight: integer;
begin
  result := FHeight;
end;

function TCustomUniversalBitmap.GetLineOrder: TRawImageLineOrder;
begin
  result := FLineOrder;
end;

function TCustomUniversalBitmap.GetNbPixels: integer;
begin
  result := FNbPixels;
end;

function TCustomUniversalBitmap.GetRefCount: integer;
begin
  result := FRefCount;
end;

function TCustomUniversalBitmap.GetPixelAddress(x,y: integer): PByte;
begin
  Result := FDataByte;
  if FLineOrder = riloBottomToTop then y := FHeight - 1 - y;
  Inc(Result, FRowSize * y + IntPtr(FPixelSize)*x);
end;

function TCustomUniversalBitmap.GetScanLineByte(y: integer): PByte;
begin
  if (y < 0) or (y >= FHeight) then
    raise ERangeError.Create('Scanline: out of bounds')
  else
  begin
    LoadFromBitmapIfNeeded;
    if FLineOrder = riloBottomToTop then y := FHeight - 1 - y;
    Result := FDataByte + FRowSize*y;
  end;
end;

function TCustomUniversalBitmap.GetWidth: integer;
begin
  result := FWidth;
end;

procedure TCustomUniversalBitmap.SetClipRect(const AValue: TRect);
begin
  IntersectRect(FClipRect,AValue,Rect(0,0,FWidth,FHeight));
end;

procedure TCustomUniversalBitmap.RaiseInvalidBrushColorspace;
begin
  raise exception.Create('Brush is not in '+Colorspace.GetName+' colorspace.');
end;

procedure TCustomUniversalBitmap.RaiseMissingUniDrawer;
begin
  raise exception.Create('Universal drawer not found. Add UniversalDrawer to the uses clause.');
end;

function TCustomUniversalBitmap.CheckHorizLineBounds(var x:int32or64; y: int32or64; var x2: int32or64): boolean;
var
  temp: int32or64;
begin
  if (y < FClipRect.Top) or (y >= FClipRect.Bottom) then exit(false);
  if (x2 < x) then
  begin
    temp := x;
    x    := x2;
    x2   := temp;
  end;
  if (x >= FClipRect.Right) or (x2 < FClipRect.Left) then exit(false);
  if x < FClipRect.Left then x := FClipRect.Left;
  if x2 >= FClipRect.Right then x2 := FClipRect.Right - 1;
  result := true;
end;

function TCustomUniversalBitmap.CheckVertLineBounds(x: int32or64; var y,y2: int32or64): boolean; inline;
var
  temp: int32or64;
begin
  if (x < FClipRect.Left) or (x >= FClipRect.Right) then exit(false);
  if (y2 < y) then
  begin
    temp := y;
    y    := y2;
    y2   := temp;
  end;
  if (y >= FClipRect.Bottom) or (y2 < FClipRect.Top) then exit(false);
  if y < FClipRect.Top then y := FClipRect.Top;
  if y2 >= FClipRect.Bottom then y2 := FClipRect.Bottom - 1;
  result := true;
end;

class function TCustomUniversalBitmap.DefaultColorspace: TColorspaceAny;
begin
  result := TBGRAPixelColorspace;
end;

function TCustomUniversalBitmap.InternalDuplicate(ACopyProperties: boolean): TCustomUniversalBitmap;
begin
  LoadFromBitmapIfNeeded;
  result := InternalNew;
  result.SetSize(FWidth,FHeight);
  Move(FDataByte^, result.FDataByte^, FRowSize*FHeight);
  result.InvalidateBitmap;
  result.Caption := Caption;
  if ACopyProperties then
    CopyPropertiesTo(result);
end;

function TCustomUniversalBitmap.InternalNew: TCustomUniversalBitmap;
begin
  result := TCustomUniversalBitmap.Create(Colorspace, LineOrder);
end;

procedure TCustomUniversalBitmap.InternalCopyPixels(ASource, ADest: PByte;
  ASourceStride, ADestStride: PtrInt; ACount: integer);
begin
  while ACount>0 do
  begin
    move(ASource^, ADest^, FPixelSize);
    inc(ASource, ASourceStride);
    inc(ADest, ADestStride);
    dec(ACount);
  end;
end;

procedure TCustomUniversalBitmap.InternalSwapPixels(ABuf1, ABuf2: PByte;
  AStride1, AStride2: PtrInt; ACount: integer);
var temp: array[0..31] of byte;
begin
  while ACount>0 do
  begin
    move(ABuf1^, {%H-}temp, FPixelSize);
    move(ABuf2^, ABuf1^, FPixelSize);
    move(temp, ABuf2^, FPixelSize);
    inc(ABuf1, AStride1);
    inc(ABuf2, AStride2);
    dec(ACount);
  end;
end;

procedure TCustomUniversalBitmap.InternalSetPixels(ASource, ADest: PByte;
  ADestStride: PtrInt; ACount: integer);
begin
  while ACount>0 do
  begin
    move(ASource^, ADest^, FPixelSize);
    inc(ADest, ADestStride);
    dec(ACount);
  end;
end;

procedure TCustomUniversalBitmap.AssignTransparentPixel(out ADest);
begin
  FillByte({%H-}ADest, FPixelSize, 0);
end;

function TCustomUniversalBitmap.GetArrow: TBGRACustomArrow;
begin
  if Pen.Arrow = nil then
  begin
    if UniDrawerClass = nil then RaiseMissingUniDrawer;
    Pen.Arrow := UniDrawerClass.CreateArrow;
    Pen.Arrow.LineCap := LineCap;
    Pen.ArrowOwned := true;
  end;
  result := Pen.Arrow;
end;

function TCustomUniversalBitmap.GetLinearAntialiasing: boolean;
begin
  result := FAntialiasingDrawMode in[dmLinearBlend,dmXor];
end;

function TCustomUniversalBitmap.GetLineCap: TPenEndCap;
begin
  result := Pen.LineCap;
end;

function TCustomUniversalBitmap.GetPenStroker: TBGRACustomPenStroker;
begin
  if FPenStroker = nil then
  begin
    if UniDrawerClass = nil then RaiseMissingUniDrawer;
    FPenStroker := UniDrawerClass.CreatePenStroker;
  end;
  result := FPenStroker;
end;

procedure TCustomUniversalBitmap.PathFillAliasedCallback(
  const APoints: array of TPointF; AData: pointer);
begin
  with TPathCallbackData(AData^) do
    UniDrawerClass.FillPoly(self, APoints, FillMode, BrushAddress^, PixelCenteredCoords, Alpha);
end;

procedure TCustomUniversalBitmap.PathStrokeAliasedCallback(
  const APoints: array of TPointF; AClosed: boolean; AData: Pointer);
var pts: array of TPoint;
  i: Integer;
begin
  with TPathCallbackData(AData^) do
  begin
    setlength(pts, length(APoints));
    if PixelCenteredCoords then
    begin
      for i := 0 to high(pts) do
        pts[i]:= APoints[i].Round;
    end else
    begin
      for i := 0 to high(pts) do
        pts[i]:= APoints[i].Floor;
    end;
    if AClosed then
      UniDrawerClass.DrawPolygon(self, pts, BrushAddress^, Alpha)
    else
      UniDrawerClass.DrawPolyLine(self, pts, BrushAddress^, true, Alpha);
  end;
end;

procedure TCustomUniversalBitmap.SetAntialiasingDrawMode(AValue: TDrawMode);
begin
  if FAntialiasingDrawMode=AValue then Exit;
  FAntialiasingDrawMode:=AValue;
end;

procedure TCustomUniversalBitmap.SetLinearAntialiasing(AValue: boolean);
begin
  if AValue then AntialiasingDrawMode:= dmLinearBlend
  else AntialiasingDrawMode:= dmDrawWithTransparency;
end;

procedure TCustomUniversalBitmap.SetLineCap(AValue: TPenEndCap);
begin
  if AValue <> Pen.LineCap then
  begin
    Pen.LineCap := AValue;
    if Assigned(Pen.Arrow) then
      Pen.Arrow.LineCap := AValue;
  end;
end;

procedure TCustomUniversalBitmap.DrawLineAntialias(x1, y1, x2, y2: single;
  const ABrush: TUniversalBrush; APenWidth: single; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.DrawLineAntialias(self, x1,y1,x2,y2, ABrush,APenWidth,AAlpha);
end;

procedure TCustomUniversalBitmap.SetLineOrder(AValue: TRawImageLineOrder);
begin
  FLineOrder:= AValue;
end;

procedure TCustomUniversalBitmap.Init;
begin
  FRefCount := 1;
  if FColorspace = nil then FColorspace := DefaultColorspace;
  FPixelSize := FColorspace.GetSize;
  FConvertToFPColor:= FColorspace.GetConversion(TFPColorColorspace);
  FConvertFromFPColor:= TFPColorColorspace.GetConversion(FColorspace);
  FConvertFromBGRA := TBGRAPixelColorspace.GetConversion(FColorspace);
  FConvertFromExpanded := TExpandedPixelColorspace.GetConversion(FColorspace);
  FReferenceWhite := nil;
  FWidth := 0;
  FHeight := 0;
  FNbPixels := 0;
  FRowSize := 0;
  FDataByte := nil;
  FLineOrder := riloTopToBottom;
  FClipRect := EmptyRect;
  FillMode := fmWinding;
  FAntialiasingDrawMode:= dmDrawWithTransparency;
  FPenStroker := nil;
end;

procedure TCustomUniversalBitmap.InvalidateBitmap;
begin
  //not linked to a bitmap
end;

procedure TCustomUniversalBitmap.LoadFromBitmapIfNeeded;
begin
  //not linked to a bitmap
end;

class procedure TCustomUniversalBitmap.EraseBrush(out ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  raise exception.Create('Erase brush not implemented');
end;

class procedure TCustomUniversalBitmap.AlphaBrush(out ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  raise exception.Create('Alpha brush not implemented');
end;

procedure TCustomUniversalBitmap.SolidBrushBGRA(out ABrush: TUniversalBrush;
  ARed, AGreen, ABlue, AAlpha: Byte; ADrawMode: TDrawMode);
var c: TBGRAPixel;
  c2: array[0..31] of byte;
begin
  c.red := ARed; c.green := AGreen; c.blue := ABlue; c.alpha := AAlpha;
  FConvertFromBGRA(@c,@c2,1,sizeof(c),FPixelSize,FReferenceWhite);
  SolidBrushIndirect(ABrush, @c2, ADrawMode);
end;

procedure TCustomUniversalBitmap.SolidBrushBGRA(out ABrush: TUniversalBrush;
  AColor: TBGRAPixel; ADrawMode: TDrawMode);
var
  c2: array[0..31] of byte;
begin
  FConvertFromBGRA(@AColor,@c2,1,sizeof(AColor),FPixelSize,FReferenceWhite);
  SolidBrushIndirect(ABrush, @c2, ADrawMode);
end;

procedure TCustomUniversalBitmap.SolidBrushExpanded(out
  ABrush: TUniversalBrush; ARed, AGreen, ABlue, AAlpha: Word;
  ADrawMode: TDrawMode);
var c: TExpandedPixel;
  c2: array[0..31] of byte;
begin
  c.red := ARed; c.green := AGreen; c.blue := ABlue; c.alpha := AAlpha;
  FConvertFromExpanded(@c,@c2,1,sizeof(c),FPixelSize,FReferenceWhite);
  SolidBrushIndirect(ABrush, @c2, ADrawMode);
end;

procedure TCustomUniversalBitmap.SolidBrushExpanded(out
  ABrush: TUniversalBrush; AColor: TExpandedPixel; ADrawMode: TDrawMode);
var
  c2: array[0..31] of byte;
begin
  FConvertFromExpanded(@AColor,@c2,1,sizeof(AColor),FPixelSize,FReferenceWhite);
  SolidBrushIndirect(ABrush, @c2, ADrawMode);
end;

procedure DefaultSolidBrushIndirectSkipPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
begin
  inc(AContextData^.Dest, ACount*PtrInt(PDefaultSolidBrushIndirectFixedData(AFixedData)^.PixelSize));
end;

procedure DefaultSolidBrushIndirectSetPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PByte;
begin
  if AAlpha < 32768 then
  begin
    inc(AContextData^.Dest, ACount*PtrInt(PDefaultSolidBrushIndirectFixedData(AFixedData)^.PixelSize));
    exit;
  end;
  pDest := AContextData^.Dest;
  while ACount > 0 do
  begin
    with PDefaultSolidBrushIndirectFixedData(AFixedData)^ do
    begin
      move(Color, pDest^, PixelSize);
      inc(pDest, PixelSize);
    end;
    dec(ACount);
  end;
  AContextData^.Dest := pDest;
end;

procedure TCustomUniversalBitmap.SolidBrushIndirect(out ABrush: TUniversalBrush;
  AColor: Pointer; ADrawMode: TDrawMode);
var
  ct: TColorTransparency;
begin
  if FPixelSize+4 > sizeof(ABrush.FixedData) then
    raise exception.Create('Brush fixed data size too small');

  ct := Colorspace.GetColorTransparency(AColor);
  if (ADrawMode in[dmLinearBlend,dmDrawWithTransparency]) and
   (ct = ctSemiTransparent) then
     raise exception.Create('Semi-tranparent drawing not handled by default brush')
  else if ADrawMode = dmXor then
     raise exception.Create('Xor mode not handled by default brush');

  ABrush.Colorspace := Colorspace;
  ABrush.InternalInitContext:= nil;
  PDefaultSolidBrushIndirectFixedData(@ABrush.FixedData)^.PixelSize:= FPixelSize;

  if (ADrawMode <> dmSet) and (ct <> ctFullyOpaque) then
    ABrush.InternalPutNextPixels:= @DefaultSolidBrushIndirectSkipPixels
  else
  begin
    move(AColor^, PDefaultSolidBrushIndirectFixedData(@ABrush.FixedData)^.Color, FPixelSize);
    ABrush.InternalPutNextPixels:= @DefaultSolidBrushIndirectSetPixels
  end;
end;

class procedure TCustomUniversalBitmap.ScannerBrush(out
  ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode;
  AOffsetX: integer = 0; AOffsetY: integer = 0);
begin
  raise exception.Create('Scanner brush not implemented');
end;

procedure TCustomUniversalBitmap.ReallocData;
begin
  ReAllocMem(FDataByte, FHeight * FRowSize);
  if (FNbPixels > 0) and (FDataByte = nil) then
    raise EOutOfMemory.Create('TUniversalBitmap: Not enough memory');
  InvalidateBitmap;
end;

procedure TCustomUniversalBitmap.FreeData;
begin
  Freemem(FDataByte);
  FDataByte := nil;
end;

function TCustomUniversalBitmap.CheckClippedRectBounds(var x, y, x2, y2: integer): boolean;
var
  temp: integer;
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
  if (x >= FClipRect.Right) or (x2 <= FClipRect.Left) or (y >= FClipRect.Bottom) or (y2 <= FClipRect.Top) then
  begin
    result := false;
    exit;
  end;
  if x < FClipRect.Left then
    x := FClipRect.Left;
  if x2 > FClipRect.Right then
    x2 := FClipRect.Right;
  if y < FClipRect.Top then
    y := FClipRect.Top;
  if y2 > FClipRect.Bottom then
    y2 := FClipRect.Bottom;
  if (x2 - x <= 0) or (y2 - y <= 0) then
  begin
    result := false;
    exit;
  end else
    result := true;
end;

function TCustomUniversalBitmap.PtInClipRect(x, y: int32or64): boolean;
begin
  result := (x >= FClipRect.Left) and (y >= FClipRect.Top) and (x < FClipRect.Right) and (y < FClipRect.Bottom);
end;

procedure TCustomUniversalBitmap.SetInternalColor(x, y: integer; const Value: TFPColor);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  FConvertFromFPColor(@Value, GetPixelAddress(x,y),
                      1, sizeof(TFPColor), FPixelSize, FReferenceWhite);
  InvalidateBitmap;
end;

function TCustomUniversalBitmap.GetInternalColor(x, y: integer): TFPColor;
begin
  if not PtInClipRect(x,y) then exit(colTransparent);
  LoadFromBitmapIfNeeded;
  FConvertFromFPColor(GetPixelAddress(x,y), @result,
                      1, FPixelSize, sizeof(TFPColor), FReferenceWhite);
end;

procedure TCustomUniversalBitmap.SetInternalPixel(x, y: integer; Value: integer);
begin
  SetInternalColor(x,y, Palette.Color[Value]);
end;

function TCustomUniversalBitmap.GetInternalPixel(x, y: integer): integer;
begin
  result := Palette.IndexOf(GetInternalColor(x,y));
end;

constructor TCustomUniversalBitmap.Create;
begin
  Init;
  inherited Create(0, 0);
end;

constructor TCustomUniversalBitmap.Create(AColorspace: TColorspaceAny;
  ALineOrder: TRawImageLineOrder);
begin
  FColorspace := AColorspace;
  Init;
  FLineOrder:= ALineOrder;
  inherited Create(0,0);
end;

constructor TCustomUniversalBitmap.Create(AWidth, AHeight: integer);
begin
  Init;
  inherited Create(AWidth, AHeight);
  if FDataByte<>nil then FillByte(FDataByte^, FHeight*FRowSize, 0);
end;

constructor TCustomUniversalBitmap.Create(AColorspace: TColorspaceAny; AWidth,
  AHeight: integer; ALineOrder: TRawImageLineOrder);
begin
  FColorspace := AColorspace;
  Init;
  FLineOrder:= ALineOrder;
  inherited Create(AWidth, AHeight);
  if FDataByte<>nil then FillByte(FDataByte^, FHeight*FRowSize, 0);
end;

function TCustomUniversalBitmap.NewReference: TCustomUniversalBitmap;
begin
  if self <> nil then Inc(FRefCount);
  Result := self;
end;

procedure TCustomUniversalBitmap.FreeReference;
begin
  if self = nil then
    exit;

  if FRefCount > 0 then
  begin
    Dec(FRefCount);
    if FRefCount = 0 then
    begin
      self.Destroy;
    end;
  end;
end;

function TCustomUniversalBitmap.GetUnique: TCustomUniversalBitmap;
begin
  if FRefCount > 1 then
  begin
    Dec(FRefCount);
    Result := self.Duplicate;
  end
  else
    Result := self;
end;

function TCustomUniversalBitmap.Duplicate(ACopyProperties: boolean): TCustomUniversalBitmap;
begin
  result := InternalDuplicate(ACopyProperties);
end;

procedure TCustomUniversalBitmap.CopyPropertiesTo(
  ABitmap: TCustomUniversalBitmap);
begin
  ABitmap.FillMode := FillMode;
  ABitmap.ClipRect := ClipRect;
end;

function TCustomUniversalBitmap.GetPart(const ARect: TRect): TCustomUniversalBitmap;
var
  sx, sy: integer;
  xSrc0, ySrc, remainX, xSrc, yDest, copyCount, copyByte: integer;
  pSrc, pDest, pSrcLine: PByte;
begin
  sx := ARect.Width;
  sy := ARect.Height;
  if (sx = 0) or (sy = 0) then exit(nil);
  result := InternalNew;
  result.SetSize(sx,sy);
  if (FWidth = 0) or (FHeight = 0) then
  begin
    result.FillTransparent;
    exit;
  end;
  LoadFromBitmapIfNeeded;
  xSrc0 := PositiveMod(ARect.Left, FWidth);
  ySrc := PositiveMod(ARect.Top, FHeight);
  for yDest := 0 to sy-1 do
  begin
    xSrc := xSrc0;
    pSrcLine := GetScanLineByte(ySrc);
    pSrc := pSrcLine + xSrc*FPixelSize;
    pDest := result.GetScanLineByte(yDest);
    remainX := sx;
    while remainX > 0 do
    begin
      if xSrc+remainX > FWidth then
      begin
        copyCount := FWidth-xSrc;
        copyByte := copyCount*FPixelSize;
        move(pSrc^, pDest^, copyByte);
        inc(pDest, copyByte);
        xSrc := 0;
        pSrc := pSrcLine;
        dec(remainX, copyCount);
      end
      else
      begin
        move(pSrc^, pDest^, remainX*FPixelSize);
        break;
      end;
    end;
    inc(ySrc);
    if ySrc = FHeight then ySrc := 0;
  end;
  result.InvalidateBitmap;
end;

procedure TCustomUniversalBitmap.SetSize(AWidth, AHeight: integer);
var
  nbPixels64: Int64;
begin
  if AWidth < 0 then AWidth := 0;
  if AHeight < 0 then AHeight := 0;
  if (AWidth = Width) and (AHeight = Height) then exit;
  inherited SetSize(AWidth, AHeight);
  FWidth    := AWidth;
  FHeight   := AHeight;
  nbPixels64 := int64(AWidth) * int64(AHeight);
  if nbPixels64 > maxLongint then
  begin
    // 2 gigapixels limit
    raise EOutOfMemory.Create('Image too big');
  end;
  FNbPixels := nbPixels64;
  FRowSize := PtrInt(FWidth)*FPixelSize;
  ReallocData;
  NoClip;
end;

destructor TCustomUniversalBitmap.Destroy;
begin
  FreeData;
  FreeAndNil(FPenStroker);
  inherited Destroy;
end;

procedure TCustomUniversalBitmap.Serialize(AStream: TStream);
var lWidth,lHeight,y: integer;
begin
  lWidth := NtoLE(Width);
  lHeight := NtoLE(Height);
  AStream.Write(lWidth,sizeof(lWidth));
  AStream.Write(lHeight,sizeof(lHeight));
  for y := 0 to Height-1 do
    AStream.Write(GetPixelAddress(0,y)^, RowSize);
end;

procedure TCustomUniversalBitmap.Deserialize(AStream: TStream);
var lWidth,lHeight,y: integer;
begin
  AStream.Read({%H-}lWidth,sizeof(lWidth));
  AStream.Read({%H-}lHeight,sizeof(lHeight));
  lWidth := LEtoN(lWidth);
  lHeight := LEtoN(lHeight);
  SetSize(lWidth,lHeight);
  for y := 0 to Height-1 do
    AStream.Read(GetPixelAddress(0,y)^, RowSize);
  InvalidateBitmap;
end;

class procedure TCustomUniversalBitmap.SerializeEmpty(AStream: TStream);
var zero: integer;
begin
  zero := 0;
  AStream.Write(zero,sizeof(zero));
  AStream.Write(zero,sizeof(zero));
end;

procedure TCustomUniversalBitmap.NoClip;
begin
  FClipRect := rect(0,0,FWidth,FHeight);
end;

procedure TCustomUniversalBitmap.Fill(const ABrush: TUniversalBrush; AAlpha: Word = 65535);
var
  pDest: PByte;
  delta: PtrInt;
  yb: Integer;
  ctx: TUniBrushContext;
begin
  if NbPixels = 0 then exit;
  LoadFromBitmapIfNeeded;
  if LineOrder = riloBottomToTop then
    delta := -RowSize
  else
    delta := RowSize;
  pDest := GetPixelAddress(0,0);
  for yb := 0 to Height-1 do
  begin
    ABrush.MoveTo(@ctx, pDest,0,yb);
    ABrush.PutNextPixels(@ctx, AAlpha,Width);
    inc(pDest, delta);
  end;
  InvalidateBitmap;
end;

procedure TCustomUniversalBitmap.Fill(ATexture: IBGRAScanner; AMode: TDrawMode);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AMode);
  Fill(b);
end;

procedure TCustomUniversalBitmap.Fill(ATexture: IBGRAScanner; AMode: TDrawMode;
  AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AMode);
  Fill(b, AAlpha);
end;

procedure TCustomUniversalBitmap.FillTransparent;
var
  b: TUniversalBrush;
begin
  EraseBrush(b, 65535);
  Fill(b);
end;

procedure TCustomUniversalBitmap.AlphaFill(alpha: byte);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha + (alpha shl 8));
  Fill(b);
end;

procedure TCustomUniversalBitmap.ApplyGlobalOpacity(alpha: byte);
begin
  ApplyGlobalOpacity(ClipRect, alpha);
end;

procedure TCustomUniversalBitmap.ApplyGlobalOpacity(ARect: TRect; alpha: byte);
begin
  EraseRect(ARect, 255-alpha);
end;

procedure TCustomUniversalBitmap.FillRect(ALeft, ATop, ARight, ABottom: integer;
  const ABrush: TUniversalBrush; AAlpha: Word);
var
  yb, sx: integer;
  pScan: PByte;
  delta: PtrInt;
  ctx: TUniBrushContext;
begin
  if ABrush.Colorspace <> Colorspace then RaiseInvalidBrushColorspace;
  if not CheckClippedRectBounds({%H-}ALeft,{%H-}ATop,{%H-}ARight,{%H-}ABottom) or
    (AAlpha = 0) then exit;

  LoadFromBitmapIfNeeded;
  pScan := GetPixelAddress(ALeft, ATop);
  if LineOrder = riloBottomToTop then
    delta := -RowSize
  else
    delta := RowSize;
  sx := ARight - ALeft;

  for yb := ATop to ABottom-1 do
  begin
    ABrush.MoveTo(@ctx, pScan,ALeft,yb);
    ABrush.PutNextPixels(@ctx, AAlpha,sx);
    inc(pScan, delta);
  end;
  InvalidateBitmap;
end;

procedure TCustomUniversalBitmap.FillRect(const ARect: TRect;
  const ABrush: TUniversalBrush; AAlpha: Word);
begin
  FillRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, ABrush, AAlpha);
end;

procedure TCustomUniversalBitmap.FillRect(ALeft, ATop, ARight,
  ABottom: integer; ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AMode);
  FillRect(ALeft, ATop, ARight, ABottom, b, AAlpha);
end;

procedure TCustomUniversalBitmap.FillRect(const ARect: TRect;
  ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word);
begin
  FillRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, ATexture, AMode, AAlpha);
end;

procedure TCustomUniversalBitmap.FillRect(ALeft, ATop, ARight,
  ABottom: integer; ATexture: IBGRAScanner; AMode: TDrawMode;
  AScanOffset: TPoint; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AMode,AScanOffset.X,AScanOffset.Y);
  FillRect(ALeft,ATop,ARight,ABottom, b,AAlpha);
end;

procedure TCustomUniversalBitmap.FillRect(const ARect: TRect;
  ATexture: IBGRAScanner; AMode: TDrawMode; AScanOffset: TPoint; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AMode,AScanOffset.X,AScanOffset.Y);
  FillRect(ARect, b,AAlpha);
end;

procedure TCustomUniversalBitmap.EraseRect(ALeft, ATop, ARight,
  ABottom: integer; alpha: byte);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha + (alpha shl 8));
  FillRect(ALeft, ATop, ARight, ABottom, b);
end;

procedure TCustomUniversalBitmap.EraseRect(const ARect: TRect; alpha: byte);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha + (alpha shl 8));
  FillRect(ARect, b);
end;

procedure TCustomUniversalBitmap.AlphaFillRect(ALeft, ATop, ARight,
  ABottom: integer; alpha: byte);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha + (alpha shl 8));
  FillRect(ALeft, ATop, ARight, ABottom, b);
end;

procedure TCustomUniversalBitmap.AlphaFillRect(const ARect: TRect; alpha: byte);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha + (alpha shl 8));
  FillRect(ARect, b);
end;

procedure TCustomUniversalBitmap.SetPixelIndirect(x, y: int32or64; AColor: pointer);
var
  pScan: PByte;
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  pScan := GetPixelAddress(x,y);
  move(AColor^, pScan^, FPixelSize);
  InvalidateBitmap;
end;

procedure TCustomUniversalBitmap.GetPixelIndirect(x, y: int32or64; AColor: pointer);
var
  pScan: Pointer;
begin
  if (x < 0) or (x >= FWidth) or (y < 0) or (y >= FHeight) then //it is possible to read pixels outside of the cliprect
    AssignTransparentPixel(AColor^)
  else
  begin
    LoadFromBitmapIfNeeded;
    pScan := GetPixelAddress(x,y);
    move(pScan^, AColor^, FPixelSize);
  end;
end;

procedure TCustomUniversalBitmap.GetPixelCycleIndirect(x, y: int32or64;
  AColor: pointer);
var
  pScan: PByte;
begin
  if (FWidth=0) or (FHeight=0) then AssignTransparentPixel(AColor^)
  else
  begin
    LoadFromBitmapIfNeeded;
    pScan := GetPixelAddress(PositiveMod(x, Width), PositiveMod(y, Height));
    move(pScan^, AColor^, FPixelSize);
  end;
end;

procedure TCustomUniversalBitmap.DrawPixel(x, y: Int32or64;
  const ABrush: TUniversalBrush; AAlpha: Word);
var
  pScan: Pointer;
  ctx: TUniBrushContext;
begin
  if ABrush.Colorspace <> Colorspace then RaiseInvalidBrushColorspace;
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  pScan := GetPixelAddress(x,y);
  ABrush.MoveTo(@ctx, pScan,x,y);
  ABrush.PutNextPixels(@ctx, AAlpha,1);
  InvalidateBitmap;
end;

procedure TCustomUniversalBitmap.DrawPixel(x, y: Int32or64;
  ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AMode);
  DrawPixel(x,y, b, AAlpha);
end;

procedure TCustomUniversalBitmap.ErasePixel(x, y: int32or64; alpha: byte);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha+(alpha shl 8));
  DrawPixel(x,y, b);
end;

procedure TCustomUniversalBitmap.AlphaPixel(x, y: int32or64; alpha: byte);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha+(alpha shl 8));
  DrawPixel(x,y, b);
end;

procedure TCustomUniversalBitmap.HorizLine(x, y, x2: int32or64;
  const ABrush: TUniversalBrush; AAlpha: Word);
var
  pScan: Pointer;
  ctx: TUniBrushContext;
begin
  if ABrush.Colorspace <> Colorspace then RaiseInvalidBrushColorspace;
  if not CheckHorizLineBounds(x,y,x2) then exit;
  LoadFromBitmapIfNeeded;
  pScan := GetPixelAddress(x,y);
  ABrush.MoveTo(@ctx, pScan,x,y);
  ABrush.PutNextPixels(@ctx, AAlpha,x2-x+1);
  InvalidateBitmap;
end;

procedure TCustomUniversalBitmap.HorizLine(x, y, x2: int32or64;
  ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AMode);
  HorizLine(x,y,x2, b,AAlpha);
end;

procedure TCustomUniversalBitmap.EraseHorizLine(x, y, x2: int32or64; alpha: byte);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha+(alpha shl 8));
  HorizLine(x,y,x2, b);
end;

procedure TCustomUniversalBitmap.AlphaHorizLine(x, y, x2: int32or64; alpha: byte);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha+(alpha shl 8));
  HorizLine(x,y,x2, b);
end;

procedure TCustomUniversalBitmap.VertLine(x, y, y2: int32or64;
  const ABrush: TUniversalBrush; AAlpha: Word);
var
  pScan: PByte;
  delta: PtrInt;
  yb: Int32or64;
  ctx: TUniBrushContext;
begin
  if ABrush.Colorspace <> Colorspace then RaiseInvalidBrushColorspace;
  if not CheckVertLineBounds(x,y,y2) then exit;
  LoadFromBitmapIfNeeded;
  pScan := GetPixelAddress(x,y);
  if LineOrder = riloTopToBottom then delta := RowSize else delta := -rowSize;
  for yb := y to y2 do
  begin
    ABrush.MoveTo(@ctx, pScan,x,y);
    ABrush.PutNextPixels(@ctx, AAlpha,1);
    inc(pScan, delta);
  end;
  InvalidateBitmap;
end;

procedure TCustomUniversalBitmap.VertLine(x, y, y2: int32or64;
  ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AMode);
  VertLine(x,y,y2, b,AAlpha);
end;

procedure TCustomUniversalBitmap.EraseVertLine(x, y, y2: int32or64; alpha: byte);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha+(alpha shl 8));
  VertLine(x,y,y2, b);
end;

procedure TCustomUniversalBitmap.AlphaVertLine(x, y, y2: int32or64; alpha: byte);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha+(alpha shl 8));
  VertLine(x,y,y2, b);
end;

procedure TCustomUniversalBitmap.DrawLine(x1, y1, x2, y2: integer;
  const ABrush: TUniversalBrush; ADrawLastPixel: boolean; AAlpha: Word = 65535);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.DrawLine(self, x1,y1,x2,y2, ABrush, ADrawLastPixel, AAlpha);
end;

procedure TCustomUniversalBitmap.DrawLine(x1, y1, x2, y2: integer;
  ATexture: IBGRAScanner; AMode: TDrawMode; ADrawLastPixel: boolean; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AMode);
  DrawLine(x1,y1,x2,y2, b,ADrawLastPixel, AAlpha);
end;

procedure TCustomUniversalBitmap.DrawLineAntialias(x1, y1, x2, y2: integer;
  const ABrush: TUniversalBrush; ADrawLastPixel: boolean; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.DrawLineAntialias(self, x1,y1,x2,y2, ABrush, ADrawLastPixel, AAlpha);
end;

procedure TCustomUniversalBitmap.DrawLineAntialias(x1, y1, x2, y2: integer;
  ATexture: IBGRAScanner; AMode: TDrawMode; ADrawLastPixel: boolean;
  AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AMode);
  DrawLineAntialias(x1,y1,x2,y2, b,ADrawLastPixel,AAlpha);
end;

procedure TCustomUniversalBitmap.DrawLineAntialias(x1, y1, x2, y2: integer;
  const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer;
  ADrawLastPixel: boolean; AAlpha: Word);
var
  dashPos: integer;
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  dashPos := 0;
  UniDrawerClass.DrawLineAntialias(self, x1,y1,x2,y2, ABrush1,ABrush2, ADashLen, dashPos, ADrawLastPixel, AAlpha);
end;

procedure TCustomUniversalBitmap.DrawLineAntialias(x1, y1, x2, y2: integer;
  const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer;
  var ADashPos: integer; ADrawLastPixel: boolean; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.DrawLineAntialias(self, x1,y1,x2,y2, ABrush1,ABrush2, ADashLen, ADashPos, ADrawLastPixel, AAlpha);
end;

procedure TCustomUniversalBitmap.EraseLine(x1, y1, x2, y2: integer;
  alpha: byte; DrawLastPixel: boolean);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha+(alpha shl 8));
  DrawLine(x1,y1,x2,y2,b,DrawLastPixel);
end;

procedure TCustomUniversalBitmap.EraseLineAntialias(x1, y1, x2, y2: integer;
  alpha: byte; DrawLastPixel: boolean);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha+(alpha shl 8));
  DrawLineAntialias(x1,y1,x2,y2,b,DrawLastPixel);
end;

procedure TCustomUniversalBitmap.AlphaLine(x1, y1, x2, y2: integer;
  alpha: byte; DrawLastPixel: boolean);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha+(alpha shl 8));
  DrawLine(x1,y1,x2,y2,b,DrawLastPixel);
end;

procedure TCustomUniversalBitmap.AlphaLineAntialias(x1, y1, x2, y2: integer;
  alpha: byte; DrawLastPixel: boolean);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha+(alpha shl 8));
  DrawLineAntialias(x1,y1,x2,y2,b,DrawLastPixel);
end;

procedure TCustomUniversalBitmap.DrawPolyLine(const points: array of TPoint;
  const ABrush: TUniversalBrush; ADrawLastPixel: boolean; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.DrawPolyLine(self, points, ABrush, ADrawLastPixel, AAlpha);
end;

procedure TCustomUniversalBitmap.DrawPolyLineAntialias(
  const points: array of TPoint; const ABrush: TUniversalBrush;
  ADrawLastPixel: boolean; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.DrawPolyLineAntialias(self, points, ABrush, ADrawLastPixel, AAlpha);
end;

procedure TCustomUniversalBitmap.DrawPolyLineAntialias(
  const points: array of TPoint; const ABrush1, ABrush2: TUniversalBrush;
  ADashLen: integer; ADrawLastPixel: boolean; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.DrawPolyLineAntialias(self, points, ABrush1, ABrush2, ADashLen, ADrawLastPixel, AAlpha);
end;

procedure TCustomUniversalBitmap.ErasePolyLine(const points: array of TPoint;
  alpha: byte; ADrawLastPixel: boolean);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha + (alpha shl 8));
  DrawPolyLine(points, b, ADrawLastPixel);
end;

procedure TCustomUniversalBitmap.ErasePolyLineAntialias(
  const points: array of TPoint; alpha: byte; ADrawLastPixel: boolean);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha + (alpha shl 8));
  DrawPolyLineAntialias(points, b, ADrawLastPixel);
end;

procedure TCustomUniversalBitmap.AlphaPolyLine(const points: array of TPoint;
  alpha: byte; ADrawLastPixel: boolean);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha + (alpha shl 8));
  DrawPolyLine(points, b, ADrawLastPixel);
end;

procedure TCustomUniversalBitmap.AlphaPolyLineAntialias(
  const points: array of TPoint; alpha: byte; ADrawLastPixel: boolean);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha + (alpha shl 8));
  DrawPolyLineAntialias(points, b, ADrawLastPixel);
end;

procedure TCustomUniversalBitmap.DrawPolygon(const points: array of TPoint;
  const ABrush: TUniversalBrush; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.DrawPolygon(self, points, ABrush, AAlpha);
end;

procedure TCustomUniversalBitmap.DrawPolygonAntialias(
  const points: array of TPoint;
  const ABrush: TUniversalBrush; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.DrawPolygonAntialias(self, points, ABrush, AAlpha);
end;

procedure TCustomUniversalBitmap.DrawPolygonAntialias(
  const points: array of TPoint; const ABrush1,
  ABrush2: TUniversalBrush; ADashLen: integer; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.DrawPolygonAntialias(self, points, ABrush1,ABrush2, ADashLen, AAlpha);
end;

procedure TCustomUniversalBitmap.ErasePolygonOutline(const points: array of TPoint;
  alpha: byte);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha + (alpha shl 8));
  DrawPolygon(points, b);
end;

procedure TCustomUniversalBitmap.ErasePolygonOutlineAntialias(
  const points: array of TPoint; alpha: byte);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha + (alpha shl 8));
  DrawPolygonAntialias(points, b);
end;

procedure TCustomUniversalBitmap.AlphaPolygonOutline(const points: array of TPoint;
  alpha: byte);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha + (alpha shl 8));
  DrawPolygon(points, b);
end;

procedure TCustomUniversalBitmap.AlphaPolygonOutlineAntialias(
  const points: array of TPoint; alpha: byte);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha + (alpha shl 8));
  DrawPolygonAntialias(points, b);
end;

procedure TCustomUniversalBitmap.DrawPathAliased(APath: IBGRAPath;
  const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean; AAlpha: Word);
var
  data: TPathCallbackData;
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  data.BrushAddress := @ABrush;
  data.Alpha:= AAlpha;
  data.PixelCenteredCoords := APixelCenteredCoordinates;
  APath.stroke(@PathStrokeAliasedCallback, @data);
end;

procedure TCustomUniversalBitmap.DrawPathAliased(APath: IBGRAPath;
  const AMatrix: TAffineMatrix; const ABrush: TUniversalBrush;
  APixelCenteredCoordinates: boolean; AAlpha: Word);
var
  data: TPathCallbackData;
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  data.BrushAddress := @ABrush;
  data.Alpha:= AAlpha;
  data.PixelCenteredCoords := APixelCenteredCoordinates;
  APath.stroke(@PathStrokeAliasedCallback, AMatrix, @data);
end;

procedure TCustomUniversalBitmap.Rectangle(x, y, x2, y2: integer;
  const ABrush: TUniversalBrush; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.Rectangle(self, x,y,x2,y2, ABrush, AAlpha);
end;

procedure TCustomUniversalBitmap.Rectangle(x, y, x2, y2: integer;
  ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AMode);
  Rectangle(x,y,x2,y2, b,AAlpha);
end;

procedure TCustomUniversalBitmap.Rectangle(const ARect: TRect;
  const ABrush: TUniversalBrush; AAlpha: Word);
begin
  Rectangle(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom, ABrush,AAlpha);
end;

procedure TCustomUniversalBitmap.Rectangle(const ARect: TRect;
  ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word);
begin
  Rectangle(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom, ATexture,AMode,AAlpha);
end;

procedure TCustomUniversalBitmap.Rectangle(x, y, x2, y2: integer; const ABorderBrush,
  AFillBrush: TUniversalBrush; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.Rectangle(self, x,y,x2,y2, ABorderBrush,AFillBrush, AAlpha);
end;

procedure TCustomUniversalBitmap.Rectangle(const ARect: TRect;
  const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word);
begin
  Rectangle(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom, ABorderBrush,AFillBrush,AAlpha);
end;

procedure TCustomUniversalBitmap.RoundRect(X1, Y1, X2, Y2: integer; DX,
  DY: integer; const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.RoundRect(self, X1,Y1,X2,Y2,DX,DY, ABorderBrush,AFillBrush, AAlpha);
end;

procedure TCustomUniversalBitmap.RoundRect(X1, Y1, X2, Y2: integer; DX,
  DY: integer; const ABorderBrush: TUniversalBrush; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.RoundRect(self, X1,Y1,X2,Y2,DX,DY, ABorderBrush, AAlpha);
end;

procedure TCustomUniversalBitmap.RoundRect(X1, Y1, X2, Y2: integer; DX,
  DY: integer; ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AMode);
  RoundRect(X1,Y1,X2,Y2,DX,DY, b,AAlpha);
end;

procedure TCustomUniversalBitmap.FillRoundRect(X1, Y1, X2, Y2: integer; DX,
  DY: integer; const AFillBrush: TUniversalBrush; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.RoundRect(self, X1,Y1,X2,Y2,DX,DY, AFillBrush, AFillBrush, AAlpha);
end;

procedure TCustomUniversalBitmap.FillRoundRect(X1, Y1, X2, Y2: integer; DX,
  DY: integer; ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AMode);
  FillRoundRect(X1,Y1,X2,Y2,DX,DY, b,AAlpha);
end;

procedure TCustomUniversalBitmap.EraseRoundRect(X1, Y1, X2, Y2: integer; DX,
  DY: integer; alpha: byte);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha + (alpha shl 8));
  FillRoundRect(X1,Y1,X2,Y2,DX,DY,b);
end;

procedure TCustomUniversalBitmap.AlphaFillRoundRect(X1, Y1, X2, Y2: integer;
  DX, DY: integer; alpha: byte);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha + (alpha shl 8));
  FillRoundRect(X1,Y1,X2,Y2,DX,DY,b);
end;

procedure TCustomUniversalBitmap.EllipseInRect(ARect: TRect;
  const ABorderBrush: TUniversalBrush; AAlpha: Word);
begin
  RoundRect(ARect.left,ARect.top,ARect.right,ARect.bottom,
            abs(ARect.right-ARect.left),abs(ARect.bottom-ARect.top),
            ABorderBrush,AAlpha);
end;

procedure TCustomUniversalBitmap.EllipseInRect(ARect: TRect;
  ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AMode);
  EllipseInRect(ARect, b,AAlpha);
end;

procedure TCustomUniversalBitmap.EllipseInRect(ARect: TRect; const ABorderBrush,
  AFillBrush: TUniversalBrush; AAlpha: Word);
begin
  RoundRect(ARect.left,ARect.top,ARect.right,ARect.bottom,
            abs(ARect.right-ARect.left),abs(ARect.bottom-ARect.top),
            ABorderBrush,AFillBrush,AAlpha);
end;

procedure TCustomUniversalBitmap.FillEllipseInRect(ARect: TRect;
  const AFillBrush: TUniversalBrush; AAlpha: Word);
begin
  FillRoundRect(ARect.left,ARect.top,ARect.right,ARect.bottom,
            abs(ARect.right-ARect.left),abs(ARect.bottom-ARect.top),
            AFillBrush,AAlpha);
end;

procedure TCustomUniversalBitmap.FillEllipseInRect(ARect: TRect;
  ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AMode);
  FillEllipseInRect(ARect, b,AAlpha);
end;

procedure TCustomUniversalBitmap.EraseEllipseInRect(ARect: TRect; alpha: byte);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha + (alpha shl 8));
  FillEllipseInRect(ARect,b);
end;

procedure TCustomUniversalBitmap.AlphaFillEllipseInRect(ARect: TRect;
  alpha: byte);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha + (alpha shl 8));
  FillEllipseInRect(ARect,b);
end;

procedure TCustomUniversalBitmap.FillShape(AShape: TBGRACustomFillInfo;
  const ABrush: TUniversalBrush; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.FillShape(self, AShape, FillMode, ABrush, AAlpha);
end;

procedure TCustomUniversalBitmap.FillShape(AShape: TBGRACustomFillInfo;
  ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AMode);
  FillShape(AShape, b,AAlpha);
end;

procedure TCustomUniversalBitmap.EraseShape(AShape: TBGRACustomFillInfo;
  alpha: byte);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha + (alpha shl 8));
  FillShape(AShape,b);
end;

procedure TCustomUniversalBitmap.AlphaFillShape(AShape: TBGRACustomFillInfo;
  alpha: byte);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha + (alpha shl 8));
  FillShape(AShape,b);
end;

procedure TCustomUniversalBitmap.FillPoly(const APoints: array of TPointF;
  const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean;
  AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.FillPoly(self, APoints, FillMode, ABrush, APixelCenteredCoordinates, AAlpha);
end;

procedure TCustomUniversalBitmap.FillPoly(const APoints: array of TPointF;
  ATexture: IBGRAScanner; AMode: TDrawMode; APixelCenteredCoordinates: boolean;
  AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AMode);
  FillPoly(APoints, b,APixelCenteredCoordinates,AAlpha);
end;

procedure TCustomUniversalBitmap.ErasePoly(const APoints: array of TPointF;
  alpha: byte; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha + (alpha shl 8));
  FillPoly(APoints,b,APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.AlphaFillPoly(const APoints: array of TPointF;
  alpha: byte; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha + (alpha shl 8));
  FillPoly(APoints,b,APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.FillPathAliased(APath: IBGRAPath;
  const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean;
  AAlpha: Word);
var
  data: TPathCallbackData;
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  data.BrushAddress := @ABrush;
  data.Alpha:= AAlpha;
  data.PixelCenteredCoords := APixelCenteredCoordinates;
  APath.fill(@PathFillAliasedCallback, @data);
end;

procedure TCustomUniversalBitmap.FillPathAliased(APath: IBGRAPath;
  const AMatrix: TAffineMatrix; const ABrush: TUniversalBrush;
  APixelCenteredCoordinates: boolean; AAlpha: Word);
var
  data: TPathCallbackData;
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  data.BrushAddress := @ABrush;
  data.Alpha:= AAlpha;
  data.PixelCenteredCoords := APixelCenteredCoordinates;
  APath.fill(@PathFillAliasedCallback, AMatrix, @data);
end;

procedure TCustomUniversalBitmap.ErasePathAliased(APath: IBGRAPath;
  alpha: byte; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha + (alpha shl 8));
  FillPathAliased(APath,b,APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.ErasePathAliased(APath: IBGRAPath;
  const AMatrix: TAffineMatrix; alpha: byte; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  if alpha = 0 then exit;
  EraseBrush(b, alpha + (alpha shl 8));
  FillPathAliased(APath,AMatrix,b,APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.AlphaFillPathAliased(APath: IBGRAPath;
  alpha: byte; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha + (alpha shl 8));
  FillPathAliased(APath,b,APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.AlphaFillPathAliased(APath: IBGRAPath;
  const AMatrix: TAffineMatrix; alpha: byte; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  AlphaBrush(b, alpha + (alpha shl 8));
  FillPathAliased(APath,AMatrix,b,APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.VerticalFlip;
begin
  VerticalFlip(rect(0,0,Width,Height));
end;

procedure TCustomUniversalBitmap.VerticalFlip(ARect: TRect);
var
  yb,h2: integer;
  line: Pointer;
  linesize, delta: PtrInt;
  PStart,PEnd: Pointer;
begin
  if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then exit;
  if not IntersectRect(ARect, ARect, rect(0,0,Width,Height)) then exit;
  LoadFromBitmapIfNeeded;
  linesize := (ARect.Right-ARect.Left) * FPixelSize;
  line     := nil;
  getmem(line, linesize);
  PStart := GetPixelAddress(ARect.Left, ARect.Top);
  PEnd   := GetPixelAddress(ARect.Left,ARect.Bottom-1);
  h2 := (ARect.Bottom-ARect.Top) div 2;
  if LineOrder = riloTopToBottom then delta := +RowSize else delta := -RowSize;
  for yb := h2-1 downto 0 do
  begin
    move(PStart^, line^, linesize);
    move(PEnd^, PStart^, linesize);
    move(line^, PEnd^, linesize);
    Inc(PStart, delta);
    Dec(PEnd, delta);
  end;
  freemem(line);
  InvalidateBitmap;
end;

procedure TCustomUniversalBitmap.HorizontalFlip;
begin
  HorizontalFlip(rect(0,0,Width,Height));
end;

procedure TCustomUniversalBitmap.HorizontalFlip(ARect: TRect);
var
  yb, w: integer;
  PStart: PByte;
  PEnd:   PByte;
begin
  if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then exit;
  if not IntersectRect(ARect, ARect, rect(0,0,Width,Height)) then exit;
  w := ARect.Right-ARect.Left;
  LoadFromBitmapIfNeeded;
  for yb := ARect.Top to ARect.Bottom-1 do
  begin
    PStart := GetPixelAddress(ARect.Left,yb);
    PEnd   := PStart + (w-1)*FPixelSize;
    InternalSwapPixels(PStart, PEnd, FPixelSize, -FPixelSize, w shr 1);
  end;
  InvalidateBitmap;
end;

procedure TCustomUniversalBitmap.RotateUDInplace;
begin
  RotateUDInplace(rect(0,0,Width,Height));
end;

procedure TCustomUniversalBitmap.RotateUDInplace(ARect: TRect);
var
  yb,h,h2:  integer;
  line:   PByte;
  linesize, delta: IntPtr;
  PStart: PByte;
  PEnd:   PByte;
  w: integer;
begin
  if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then exit;
  if not IntersectRect(ARect, ARect, rect(0,0,Width,Height)) then exit;
  LoadFromBitmapIfNeeded;
  w := ARect.Right-ARect.Left;
  linesize := w * FPixelSize;
  line     := nil;
  getmem(line, linesize);
  PStart := GetPixelAddress(ARect.Left, ARect.Top);
  PEnd   := GetPixelAddress(ARect.Right-1, ARect.Bottom-1);
  h := ARect.Bottom-ARect.Top;
  h2 := h div 2;
  if LineOrder = riloTopToBottom then delta := +RowSize else delta := -RowSize;
  for yb := h2-1 downto 0 do
  begin
    InternalSwapPixels(PStart, PEnd, FPixelSize, -FPixelSize, w);
    Inc(PStart, delta);
    Dec(PEnd, delta);
  end;
  if odd(h) then
    InternalSwapPixels(PStart, PEnd, FPixelSize, -FPixelSize, w shr 1);
  freemem(line);
  InvalidateBitmap;
end;

function TCustomUniversalBitmap.RotateCW: TCustomUniversalBitmap;
var
  psrc, pdest: PByte;
  yb: integer;
  delta: PtrInt;
begin
  LoadFromBitmapIfNeeded;
  result := InternalNew;
  result.SetSize(Height, Width);
  if Result.LineOrder = riloTopToBottom then
    delta := Result.RowSize
  else
    delta := -Result.RowSize;
  for yb := 0 to Height - 1 do
  begin
    psrc  := ScanLineByte[yb];
    pdest := Result.GetPixelAddress(Height-1-yb, 0);
    InternalCopyPixels(psrc, pdest, FPixelSize, delta, Width);
  end;
end;

function TCustomUniversalBitmap.RotateCCW: TCustomUniversalBitmap;
var
  psrc, pdest: PByte;
  yb: integer;
  delta: PtrInt;
begin
  LoadFromBitmapIfNeeded;
  result := InternalNew;
  result.SetSize(Height, Width);
  if Result.LineOrder = riloTopToBottom then
    delta := Result.RowSize
  else
    delta := -Result.RowSize;
  for yb := 0 to Height - 1 do
  begin
    psrc  := ScanLineByte[yb];
    pdest := Result.GetPixelAddress(yb, Width - 1);
    InternalCopyPixels(psrc, pdest, FPixelSize,-delta, Width);
  end;
end;

function TCustomUniversalBitmap.RotateUD: TCustomUniversalBitmap;
var
  yb: Integer;
  psrc, pdest: PByte;
begin
  LoadFromBitmapIfNeeded;
  result := InternalNew;
  result.SetSize(Width, Height);
  for yb := 0 to Height-1 do
  begin
    psrc  := ScanLineByte[yb];
    pdest := result.GetPixelAddress(Width-1,Height-1-yb);
    InternalCopyPixels(psrc, pdest, FPixelSize, -FPixelSize, Width);
  end;
end;

{$ENDIF}
