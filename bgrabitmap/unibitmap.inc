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

const
  UniversalBrushFlag_DoNothing = 1;   //the brush does nothing at all and can be skipped
  UniversalBrushFlag_Reserved1 = 2;
  UniversalBrushFlag_Reserved2 = 4;
  UniversalBrushFlag_Reserved3 = 8;
  UniversalBrushFlag_Reserved4 = 16;
  UniversalBrushFlag_Reserved5 = 32;
  UniversalBrushFlag_Reserved6 = 64;
  UniversalBrushFlag_Reserved7 = 128;
  //you can define your custom flags from 256 on

type
  { TUniversalBrush }

  PUniversalBrush = ^TUniversalBrush;
  TUniversalBrush = record
  private
    FColorspace: TColorspaceAny;
    function GetDoesNothing: boolean; inline;
    procedure SetColorspace(AValue: TColorspaceAny);
    procedure SetDoesNothing(AValue: boolean); inline;
  public
    FixedData: array[0..31] of byte;
    InternalInitContext: TUniBrushInitContextProc;       //do not call directly
    InternalPutNextPixels: TUniBrushPutNextPixelsProc;   //do not call directly
    Flags: Cardinal;
    procedure MoveTo(AContext: PUniBrushContext; ADest: pointer; AOfsX,AOfsY: integer); inline;
    procedure PutNextPixels(AContext: PUniBrushContext; AAlpha: Word; ACount: integer); inline;
    property Colorspace: TColorspaceAny read FColorspace write SetColorspace;
    property DoesNothing: boolean read GetDoesNothing write SetDoesNothing;
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
    Width: single;
    PixelCenteredCoords: boolean;
  end;

  { TCustomUniversalBitmap }

  TCustomUniversalBitmap = class(TFPCustomImage, IBGRAScanner)
  private
    procedure PathStrokeAliasedCallback(const APoints: array of TPointF;
      AClosed: boolean; AData: Pointer);
    procedure PathStrokeAntialiasCallback(const APoints: array of TPointF;
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

    //Scan
    FScanWidth, FScanHeight: integer;    //possibility to reduce the zone being scanned
    FScanPtr : PByte;                    //current scan address
    FScanCurX,FScanCurY: integer;        //current scan coordinates

    FClipRect: TRect;
    FConvertToFPColor, FConvertFromFPColor,
    FConvertToBGRA, FConvertFromBGRA,
    FConvertToExpanded, FConvertFromExpanded: TBridgedConversion;
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

    procedure Assign(Source: TPersistent); override;
    function NewBitmap: TCustomUniversalBitmap;

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

    {** Clear all channels of transparent pixels }
    procedure ClearTransparentPixels; virtual;

    class procedure EraseBrush(out {%H-}ABrush: TUniversalBrush; {%H-}AAlpha: Word); virtual;
    class procedure AlphaBrush(out {%H-}ABrush: TUniversalBrush; {%H-}AAlpha: Word); virtual;
    procedure SolidBrushBGRA(out ABrush: TUniversalBrush; ARed,AGreen,ABlue,AAlpha: Byte; ADrawMode: TDrawMode = dmDrawWithTransparency); overload; virtual;
    procedure SolidBrushBGRA(out ABrush: TUniversalBrush; AColor: TBGRAPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); overload; virtual;
    procedure SolidBrushExpanded(out ABrush: TUniversalBrush; ARed,AGreen,ABlue,AAlpha: Word; ADrawMode: TDrawMode = dmDrawWithTransparency); overload; virtual;
    procedure SolidBrushExpanded(out ABrush: TUniversalBrush; AColor: TExpandedPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); overload; virtual;
    procedure SolidBrushIndirect(out ABrush: TUniversalBrush; AColor: Pointer; ADrawMode: TDrawMode = dmDrawWithTransparency); virtual;
    class procedure ScannerBrush(out {%H-}ABrush: TUniversalBrush; {%H-}AScanner: IBGRAScanner; {%H-}ADrawMode: TDrawMode = dmDrawWithTransparency;
                                 {%H-}AOffsetX: integer = 0; {%H-}AOffsetY: integer = 0); virtual;
    class procedure MaskBrush(out {%H-}ABrush: TUniversalBrush; {%H-}AScanner: IBGRAScanner;
                              {%H-}AOffsetX: integer = 0; {%H-}AOffsetY: integer = 0); virtual;

    {TFPCustomImage}
    {** Creates a new bitmap, initialize properties and bitmap data }
    constructor Create(AWidth, AHeight: integer); overload; override;
    {** Sets the dimension of an existing bitmap /!\ Data can be random }
    procedure SetSize(AWidth, AHeight: integer); override;
    destructor Destroy; override;

    {==== Load and save files ====}

    {** Stores the image in the stream without compression nor header }
    procedure Serialize(AStream: TStream); virtual;
    {** Reads the image in a stream that was previously serialized }
    procedure Deserialize(AStream: TStream); virtual;
    {** Stores an empty image (of size zero) }
    class procedure SerializeEmpty(AStream: TStream); static;

    //there are UTF8 functions that are different from standard function as those
    //depend on TFPCustomImage that does not clearly handle UTF8

    {** Load image from a file. ''filename'' is an ANSI string }
    procedure LoadFromFile(const AFilename: string); overload; virtual;
    procedure LoadFromFile(const AFilename: string; AOptions: TBGRALoadingOptions); overload; virtual;
    {** Load image from a file with the specified image reader. ''filename'' is an ANSI string }
    procedure LoadFromFile(const AFilename:String; AHandler:TFPCustomImageReader); overload; virtual;
    procedure LoadFromFile(const AFilename:String; AHandler:TFPCustomImageReader; AOptions: TBGRALoadingOptions); overload; virtual;
    {** Load image from a file. ''filename'' is an UTF8 string }
    procedure LoadFromFileUTF8(const AFilenameUTF8: string; AOptions: TBGRALoadingOptions = []); overload; virtual;
    {** Load image from a file with the specified image reader. ''filename'' is an UTF8 string }
    procedure LoadFromFileUTF8(const AFilenameUTF8: string; AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions = []); overload; virtual;
    {** Load image from a stream. Format is detected automatically }
    procedure LoadFromStream(AStream: TStream);overload; virtual;
    procedure LoadFromStream(AStream: TStream; AOptions: TBGRALoadingOptions);overload; virtual;
    {** Load image from a stream. The specified image reader is used }
    procedure LoadFromStream(AStream: TStream; AHandler: TFPCustomImageReader);overload; virtual;
    procedure LoadFromStream(AStream: TStream; AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions);overload; virtual;
    {** Load image from an embedded Lazarus resource. Format is detected automatically }
    procedure LoadFromResource(AFilename: string); overload; virtual;
    procedure LoadFromResource(AFilename: string; AOptions: TBGRALoadingOptions); overload; virtual;
    {** Load image from an embedded Lazarus resource. The specified image reader is used }
    procedure LoadFromResource(AFilename: string; AHandler: TFPCustomImageReader); overload; virtual;
    procedure LoadFromResource(AFilename: string; AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions); overload; virtual;

    {** Save image to a file. The format is guessed from the file extension. ''filename'' is an ANSI string }
    procedure SaveToFile(const AFilename: string);overload; virtual;
    {** Save image to a file with the specified image writer. ''filename'' is an ANSI string }
    procedure SaveToFile(const AFilename: string; AHandler:TFPCustomImageWriter);overload; virtual;
    {** Save image to a file. The format is guessed from the file extension. ''filename'' is an ANSI string }
    procedure SaveToFileUTF8(const AFilenameUTF8: string);overload; virtual;
    {** Save image to a file with the specified image writer. ''filename'' is an UTF8 string }
    procedure SaveToFileUTF8(const AFilenameUTF8: string; AHandler:TFPCustomImageWriter);overload; virtual;

    {** Save image to a stream with the specified image writer }{inherited
    procedure SaveToStream (Str:TStream; Handler:TFPCustomImageWriter);
   }{** Save image to a stream in the specified image format }
    procedure SaveToStreamAs(AStream: TStream; AFormat: TBGRAImageFormat); virtual;
    {** Save image to a stream in PNG format }
    procedure SaveToStreamAsPng(AStream: TStream); virtual;

    {==== Clipping ====}

    {** Stop clipping (clipping rectangle becomes the whole image) }
    procedure NoClip; virtual;
    {** Reduce the clipping region further by intersection and returns the previous clipping rectangle }
    function IntersectClip(const ARect: TRect): TRect;
    {** Checks if the specified point is in the clipping rectangle ''ClipRect'' }
    function PtInClipRect(x, y: int32or64): boolean; inline;
    {** Check if the bounds are within the clipping rectangle and adjust the coordinates to fit
        (similar to IntersectRect but may flip the coordinates) }
    function CheckClippedRectBounds(var x,y,x2,y2: integer): boolean;

    {==== Basic drawing functions ====}

    {** Fill the whole image regardless of clipping rect }
    procedure Fill(const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual;
    procedure Fill(ATexture: IBGRAScanner; AMode: TDrawMode); overload; virtual;
    procedure Fill(ATexture: IBGRAScanner; AMode: TDrawMode; AAlpha: Word); overload; virtual;
    procedure FillTransparent; virtual;
    procedure AlphaFill(alpha: byte); virtual;

    {** Masking }
    procedure ApplyMask(mask: TCustomUniversalBitmap; AAlpha: Word = 65535); overload;
    procedure ApplyMask(mask: TCustomUniversalBitmap; ARect: TRect; AAlpha: Word = 65535); overload;
    procedure ApplyMask(mask: TCustomUniversalBitmap; ARect: TRect; AMaskRectTopLeft: TPoint; AAlpha: Word = 65535); overload; virtual;
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
    procedure FillPathAliased(APath: IBGRAPath; ATexture: IBGRAScanner; AMode: TDrawMode; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); overload;
    procedure FillPathAliased(APath: IBGRAPath; const AMatrix: TAffineMatrix; ATexture: IBGRAScanner; AMode: TDrawMode; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); overload;
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


    {==== Pen style ====}
  protected
    function GetArrow: TBGRACustomArrow; virtual;
    function GetLineCap: TPenEndCap; virtual;
    function GetInternalPen: TBGRACustomPenStroker; virtual;
    function GetPenStroker: TBGRACustomPenStroker; virtual;
    procedure SetLineCap(AValue: TPenEndCap); virtual;

  public
     {** You can use this class set pen style and generate strokes polygonal representations }
     property Pen: TBGRACustomPenStroker read GetPenStroker;
     {** How to draw the ends of a line (applies to arrow as well) }
     property LineCap: TPenEndCap read GetLineCap write SetLineCap;
     {** Properties of arrow ends }
     property Arrow: TBGRACustomArrow read GetArrow;

     {==== Drawing lines and paths (floating point coordinates) ====}
     {* These functions use the current pen style/cap/join. The parameter ''APenWidth''
        specifies the width of the line and the base unit for dashes.
        See [[BGRABitmap tutorial 13|coordinate system]].
      * The coordinates are pixel-centered by default, so that when filling a rectangle,
        if the supplied values are integers, the border will be half transparent.
        If you want the border to be completely filled, you can subtract/add
        0.5 to the coordinates to include the remaining thin border.
        See [[BGRABitmap tutorial 13|coordinate system]]. }

     {** Draws a line from (x1,y1) to (x2,y2) using current pen style/cap/join }
    procedure DrawLineAntialias(x1, y1, x2, y2: single; const ABrush: TUniversalBrush; APenWidth: single); overload; virtual;
    {** Draws a line from (x1,y1) to (x2,y2) using current pen style/cap/join.
        ''texture'' specifies the source color to use when filling the line }
    procedure DrawLineAntialias(x1, y1, x2, y2: single; ATexture: IBGRAScanner; APenWidth: single); overload; virtual;
    {** Draws a line from (x1,y1) to (x2,y2) using current pen style/cap/join.
        ''Closed'' specifies if the end of the line is roundly closed. If it is not closed,
        a space is left so that the next line can fit }
    procedure DrawLineAntialias(x1, y1, x2, y2: single; const ABrush: TUniversalBrush; APenWidth: single; AClosedCap: boolean); overload; virtual;
    {** Same as above with ''texture'' specifying the source color to use when filling the line }
    procedure DrawLineAntialias(x1, y1, x2, y2: single; ATexture: IBGRAScanner; APenWidth: single; AClosedCap: boolean); overload; virtual;
    {** Erases a line from (x1,y1) to (x2,y2) using current pen style/cap/join }
    procedure EraseLineAntialias(x1, y1, x2, y2: single; AAlpha: Byte; APenWidth: single); overload; virtual;
    {** Erases a line from (x1,y1) to (x2,y2) using current pen style/cap/join.
        ''Closed'' specifies if the end of the line is roundly closed. If it is not closed,
        a space is left so that the next line can fit }
    procedure EraseLineAntialias(x1, y1, x2, y2: single; AAlpha: Byte; APenWidth: single; AClosedCap: boolean); overload; virtual;

    {** Draws a polyline using current pen style/cap/join }
    procedure DrawPolyLineAntialias(const APoints: array of TPointF; const ABrush: TUniversalBrush; APenWidth: single); overload; virtual;
    {** Draws a polyline using current pen style/cap/join.
        ''texture'' specifies the source color to use when filling the line }
    procedure DrawPolyLineAntialias(const APoints: array of TPointF; ATexture: IBGRAScanner; APenWidth: single); overload; virtual;
    {** Erases a polyline using current pen style/cap/join }
    procedure ErasePolyLineAntialias(const APoints: array of TPointF; AAlpha: byte; APenWidth: single); overload; virtual;

    {** Draws a polyline using current pen style/cap/join.
        ''Closed'' specifies if the end of the line is roundly closed. If it is not closed,
        a space is left so that the next line can fit }
    procedure DrawPolyLineAntialias(const APoints: array of TPointF; const ABrush: TUniversalBrush; APenWidth: single; AClosedCap: boolean); overload; virtual;
    procedure DrawPolyLineAntialias(const APoints: array of TPointF; ATexture: IBGRAScanner; APenWidth: single; AClosedCap: boolean); overload; virtual;
    procedure ErasePolyLineAntialias(const APoints: array of TPointF; AAlpha: byte; APenWidth: single; AClosedCap: boolean); overload; virtual;

    {** Draws a polyline using current pen style/cap/join.
        The last point considered as a join with the first point if it has
        the same coordinate }
    procedure DrawPolyLineAntialiasAutocycle(const APoints: array of TPointF; const ABrush: TUniversalBrush; APenWidth: single); overload; virtual;
    {** Draws a polygon using current pen style/cap/join. Use a texture to fill the line.
        The polygon is always closed. You don't need to set the last point
        to be the same as the first point }
    procedure DrawPolyLineAntialiasAutocycle(const APoints: array of TPointF; ATexture: IBGRAScanner; APenWidth: single); overload; virtual;
    procedure ErasePolyLineAntialiasAutocycle(const APoints: array of TPointF; AAlpha: byte; APenWidth: single); overload; virtual;

    {** Draws a polygon using current pen style/cap/join.
        The polygon is always closed. You don't need to set the last point
        to be the same as the first point }
    procedure DrawPolygonAntialias(const APoints: array of TPointF; const ABrush: TUniversalBrush; APenWidth: single); overload; virtual;
    procedure DrawPolygonAntialias(const APoints: array of TPointF; ATexture: IBGRAScanner; APenWidth: single); overload; virtual;
    procedure ErasePolygonOutlineAntialias(const APoints: array of TPointF; AAlpha: byte; APenWidth: single); overload; virtual;

    {** Draws an ellipse without antialising. ''rx'' is the horizontal radius and
        ''ry'' the vertical radius }
    procedure Ellipse(x, y, rx, ry: single; const ABrush: TUniversalBrush; AWidth: single; AAlpha: Word = 65535); overload; virtual;
    procedure Ellipse(x, y, rx, ry: single; ATexture: IBGRAScanner; AWidth: single; AMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    procedure Ellipse(const AOrigin, AXAxis, AYAxis: TPointF; const ABrush: TUniversalBrush; AWidth: single; AAlpha: Word = 65535); overload; virtual;
    procedure Ellipse(const AOrigin, AXAxis, AYAxis: TPointF; ATexture: IBGRAScanner; AWidth: single; AMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    {** Draws an ellipse with antialising. ''rx'' is the horizontal radius and
        ''ry'' the vertical radius }
    procedure EllipseAntialias(x, y, rx, ry: single; const ABrush: TUniversalBrush; AWidth: single); overload; virtual;
    procedure EllipseAntialias(x, y, rx, ry: single; ATexture: IBGRAScanner; AWidth: single); overload; virtual;
    procedure EllipseAntialias(const AOrigin, AXAxis, AYAxis: TPointF; const ABrush: TUniversalBrush; AWidth: single); overload; virtual;
    procedure EllipseAntialias(const AOrigin, AXAxis, AYAxis: TPointF; ATexture: IBGRAScanner; AWidth: single); overload; virtual;

    procedure DrawPath(APath: IBGRAPath; const ABrush: TUniversalBrush; AWidth: single; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure DrawPath(APath: IBGRAPath; ATexture: IBGRAScanner; AWidth: single; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure DrawPath(APath: IBGRAPath; const AMatrix: TAffineMatrix; const ABrush: TUniversalBrush; AWidth: single; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure DrawPath(APath: IBGRAPath; const AMatrix: TAffineMatrix; ATexture: IBGRAScanner; AWidth: single; APixelCenteredCoordinates: boolean = true); overload; virtual;

    {==== Antialias fill ====}
    procedure FillShapeAntialias(AShape: TBGRACustomFillInfo; const ABrush: TUniversalBrush); overload; virtual;
    procedure FillShapeAntialias(AShape: TBGRACustomFillInfo; ATexture: IBGRAScanner); overload; virtual;
    procedure EraseShapeAntialias(AShape: TBGRACustomFillInfo; AAlpha: Byte); overload; virtual;
    procedure FillPolyAntialias(const APoints: array of TPointF; const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillPolyAntialias(const APoints: array of TPointF; ATexture: IBGRAScanner; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure ErasePolyAntialias(const APoints: array of TPointF; AAlpha: Byte; APixelCenteredCoordinates: boolean = true); overload; virtual;
    {** Fills an ellipse which axes are parallel to X and Y axes }
    procedure FillEllipseAntialias(x, y, rx, ry: single; const ABrush: TUniversalBrush); overload; virtual;
    procedure FillEllipseAntialias(x, y, rx, ry: single; ATexture: IBGRAScanner); overload; virtual;
    {** Erases the content of an ellipse which axes are parallel to X and Y axes }
    procedure EraseEllipseAntialias(x, y, rx, ry: single; AAlpha: Byte); overload; virtual;
    {** Fills an ellipse with any axes }
    procedure FillEllipseAntialias(const AOrigin, AXAxis, AYAxis: TPointF; const ABrush: TUniversalBrush); overload; virtual;
    procedure FillEllipseAntialias(const AOrigin, AXAxis, AYAxis: TPointF; ATexture: IBGRAScanner); overload; virtual;
    {** Erases the content of an ellipse with any axes }
    procedure EraseEllipseAntialias(const AOrigin, AXAxis, AYAxis: TPointF; AAlpha: Byte); overload; virtual;
    {** Fills a rectangle with antialiasing. Note that the pixel (x2,y2) is
        included contrary to integer coordinates. For example (-0.5,-0.5,0.5,0.5)
        with pixel-centered coords fills one pixel }
    procedure FillRectAntialias(x, y, x2, y2: single; const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillRectAntialias(x, y, x2, y2: single; ATexture: IBGRAScanner; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure EraseRectAntialias(x, y, x2, y2: single; AAlpha: Byte; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillRectAntialias(const ARectF: TRectF; const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillRectAntialias(const ARectF: TRectF; ATexture: IBGRAScanner; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure EraseRectAntialias(const ARectF: TRectF; AAlpha: Byte; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillPath(APath: IBGRAPath; const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillPath(APath: IBGRAPath; ATexture: IBGRAScanner; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure ErasePath(APath: IBGRAPath; AAlpha: Byte; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillPath(APath: IBGRAPath; const AMatrix: TAffineMatrix; const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillPath(APath: IBGRAPath; const AMatrix: TAffineMatrix; ATexture: IBGRAScanner; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure ErasePath(APath: IBGRAPath; const AMatrix: TAffineMatrix; AAlpha: byte; APixelCenteredCoordinates: boolean = true); overload; virtual;

  protected {==== 'IBGRAScanner'' interface ====}
    function ProvidesScanline({%H-}ARect: TRect): boolean; virtual;
    function GetScanlineAt({%H-}X, {%H-}Y: integer): PBGRAPixel; virtual;
    function GetTextureGL: IUnknown; virtual;
  public
    {** Offset to apply when the image is scanned }
    ScanOffset: TPoint;

    function ScanAtInteger(X,Y: integer): TBGRAPixel; virtual;
    function ScanAtIntegerExpanded(X, Y: integer): TExpandedPixel; virtual;
    procedure ScanMoveTo(X,Y: Integer); virtual;
    function ScanNextPixel: TBGRAPixel; virtual;
    function ScanNextExpandedPixel: TExpandedPixel; virtual;
    function ScanAt(X,Y: Single): TBGRAPixel; virtual;
    function ScanAtExpanded(X, Y: Single): TExpandedPixel; virtual;
    function IsScanPutPixelsDefined: boolean; virtual;
    procedure ScanPutPixels({%H-}pdest: PBGRAPixel; {%H-}count: integer; {%H-}mode: TDrawMode); virtual;
    procedure ScanSkipPixels(ACount: integer); virtual;
    function GetImageBounds: TRect; overload; virtual;
    function GetImageBounds(Channel: TChannel; ANothingValue: Byte = 0): TRect; overload;
    function GetImageBounds(Channels: TChannels; ANothingValue: Byte = 0): TRect; overload;
    function GetImageBoundsWithin(const ARect: TRect; Channel: TChannel = cAlpha; ANothingValue: Byte = 0): TRect; overload; virtual;
    function GetImageBoundsWithin(const ARect: TRect; Channels: TChannels; ANothingValue: Byte = 0): TRect; overload; virtual;
    function GetScanCustomColorspace: TColorspaceAny; virtual;
    procedure ScanNextCustomChunk(var ACount: integer; out APixels: Pointer); virtual;
    procedure ScanNextMaskChunk(var {%H-}ACount: integer; out {%H-}AMask: PByteMask; out {%H-}AStride: integer); virtual;
    function ScanAtIntegerMask(X,Y: integer): TByteMask; virtual;
    function ScanAtMask({%H-}X,{%H-}Y: Single): TByteMask; virtual;

  protected
    //interface
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};

  public
    procedure PutImage(X, Y: integer; ASource: TCustomUniversalBitmap; AMode: TDrawMode; AOpacity: byte = 255); virtual;

  public
    //filters
    function FilterBlurRadial(radius: single; blurType: TRadialBlurType): TCustomUniversalBitmap; overload;
    function FilterBlurRadial(const ABounds: TRect; radius: single; blurType: TRadialBlurType): TCustomUniversalBitmap; overload;
    function FilterBlurRadial(radiusX, radiusY: single; blurType: TRadialBlurType): TCustomUniversalBitmap; overload;
    function FilterBlurRadial(const ABounds: TRect; radiusX, radiusY: single; blurType: TRadialBlurType): TCustomUniversalBitmap; overload;
    function FilterBlurMotion(distance: single; angle: single; oriented: boolean): TCustomUniversalBitmap; overload;
    function FilterBlurMotion(const ABounds: TRect; distance: single; angle: single; oriented: boolean): TCustomUniversalBitmap; overload;
    function FilterCustomBlur(mask: TCustomUniversalBitmap): TCustomUniversalBitmap; overload;
    function FilterCustomBlur(const ABounds: TRect; mask: TCustomUniversalBitmap): TCustomUniversalBitmap; overload;

  end;

  { TCustomUniversalDrawer }

  TCustomUniversalDrawer = class

    {==== Load and save files ====}

    //there are UTF8 functions that are different from standard function as those
    //depend on TFPCustomImage that does not clearly handle UTF8

    {** Load image from a file. ''filename'' is an ANSI string }
    class procedure LoadFromFile(ADest: TCustomUniversalBitmap; const AFilename: string); overload; virtual; abstract;
    class procedure LoadFromFile(ADest: TCustomUniversalBitmap; const AFilename: string; AOptions: TBGRALoadingOptions); overload; virtual; abstract;
    {** Load image from a file with the specified image reader. ''filename'' is an ANSI string }
    class procedure LoadFromFile(ADest: TCustomUniversalBitmap; const AFilename:String; AHandler:TFPCustomImageReader); overload; virtual; abstract;
    class procedure LoadFromFile(ADest: TCustomUniversalBitmap; const AFilename:String; AHandler:TFPCustomImageReader; AOptions: TBGRALoadingOptions); overload; virtual; abstract;
    {** Load image from a file. ''filename'' is an UTF8 string }
    class procedure LoadFromFileUTF8(ADest: TCustomUniversalBitmap; const AFilenameUTF8: string; AOptions: TBGRALoadingOptions = []); overload; virtual; abstract;
    {** Load image from a file with the specified image reader. ''filename'' is an UTF8 string }
    class procedure LoadFromFileUTF8(ADest: TCustomUniversalBitmap; const AFilenameUTF8: string; AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions = []); overload; virtual; abstract;
    {** Load image from a stream. Format is detected automatically }
    class procedure LoadFromStream(ADest: TCustomUniversalBitmap; AStream: TStream);overload; virtual; abstract;
    class procedure LoadFromStream(ADest: TCustomUniversalBitmap; AStream: TStream; AOptions: TBGRALoadingOptions);overload; virtual; abstract;
    {** Load image from a stream. The specified image reader is used }
    class procedure LoadFromStream(ADest: TCustomUniversalBitmap; AStream: TStream; AHandler: TFPCustomImageReader);overload; virtual; abstract;
    class procedure LoadFromStream(ADest: TCustomUniversalBitmap; AStream: TStream; AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions); overload; virtual; abstract;
    {** Load image from an embedded Lazarus resource. Format is detected automatically }
    class procedure LoadFromResource(ADest: TCustomUniversalBitmap; AFilename: string); overload; virtual; abstract;
    class procedure LoadFromResource(ADest: TCustomUniversalBitmap; AFilename: string; AOptions: TBGRALoadingOptions); overload; virtual; abstract;
    {** Load image from an embedded Lazarus resource. The specified image reader is used }
    class procedure LoadFromResource(ADest: TCustomUniversalBitmap; AFilename: string; AHandler: TFPCustomImageReader); overload; virtual; abstract;
    class procedure LoadFromResource(ADest: TCustomUniversalBitmap; AFilename: string; AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions); overload; virtual; abstract;

    {** Save image to a file. The format is guessed from the file extension. ''filename'' is an ANSI string }
    class procedure SaveToFile(ASource: TCustomUniversalBitmap; const AFilename: string); overload; virtual; abstract;
    {** Save image to a file with the specified image writer. ''filename'' is an ANSI string }
    class procedure SaveToFile(ASource: TCustomUniversalBitmap; const AFilename: string; AHandler:TFPCustomImageWriter); overload; virtual; abstract;
    {** Save image to a file. The format is guessed from the file extension. ''filename'' is an ANSI string }
    class procedure SaveToFileUTF8(ASource: TCustomUniversalBitmap; const AFilenameUTF8: string); overload; virtual; abstract;
    {** Save image to a file with the specified image writer. ''filename'' is an UTF8 string }
    class procedure SaveToFileUTF8(ASource: TCustomUniversalBitmap; const AFilenameUTF8: string; AHandler:TFPCustomImageWriter); overload; virtual; abstract;

    {** Save image to a stream in the specified image format }
    class procedure SaveToStreamAs(ASource: TCustomUniversalBitmap; AStream: TStream; AFormat: TBGRAImageFormat); virtual; abstract;
    {** Save image to a stream in PNG format }
    class procedure SaveToStreamAsPng(ASource: TCustomUniversalBitmap; AStream: TStream); virtual; abstract;

    {==== Pixelwise drawing ====}

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

    class procedure DrawPolyLine(ADest: TCustomUniversalBitmap; const APoints: array of TPoint; const ABrush: TUniversalBrush; ADrawLastPixel: boolean; AAlpha: Word = 65535); virtual; abstract;
    class procedure DrawPolyLineAntialias(ADest: TCustomUniversalBitmap; const APoints: array of TPoint; const ABrush: TUniversalBrush; DrawLastPixel: boolean; AAlpha: Word = 65535); overload; virtual; abstract;
    class procedure DrawPolyLineAntialias(ADest: TCustomUniversalBitmap; const APoints: array of TPoint; const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer; DrawLastPixel: boolean; AAlpha: Word = 65535); overload; virtual; abstract;

    class procedure DrawPolygon(ADest: TCustomUniversalBitmap; const APoints: array of TPoint; const ABrush: TUniversalBrush; AAlpha: Word = 65535); virtual; abstract;
    class procedure DrawPolygonAntialias(ADest: TCustomUniversalBitmap; const APoints: array of TPoint; const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual; abstract;
    class procedure DrawPolygonAntialias(ADest: TCustomUniversalBitmap; const APoints: array of TPoint; const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer; AAlpha: Word = 65535); overload; virtual; abstract;

    {** Draw the border of a rectangle }
    class procedure Rectangle(ADest: TCustomUniversalBitmap; x, y, x2, y2: integer; const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual; abstract;
    {** Draw a filled rectangle with a border }
    class procedure Rectangle(ADest: TCustomUniversalBitmap; x, y, x2, y2: integer; const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual; abstract;

    class procedure RoundRect(ADest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer; DX, DY: integer; const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual; abstract;
    class procedure RoundRect(ADest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer; DX, DY: integer; const ABorderBrush: TUniversalBrush; AAlpha: Word = 65535); overload; virtual; abstract;
    class procedure FillRoundRect(ADest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer; DX, DY: integer; const AFillBrush: TUniversalBrush; AAlpha: Word = 65535); virtual; abstract;

    class procedure FillShape(ADest: TCustomUniversalBitmap; AShape: TBGRACustomFillInfo; AFillMode: TFillMode; ABrush: TUniversalBrush; AAlpha: Word = 65535); virtual; abstract;
    class procedure FillPoly(ADest: TCustomUniversalBitmap; const APoints: array of TPointF; AFillMode: TFillMode; ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); virtual; abstract;

    {==== Using pen ====}
    class function CreatePenStroker: TBGRACustomPenStroker; virtual; abstract;
    class function CreateArrow: TBGRACustomArrow; virtual; abstract;

    class procedure Ellipse(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; x, y, rx, ry: single;
        const ABrush: TUniversalBrush; AWidth: single; AAlpha: Word=65535); overload; virtual; abstract;
    class procedure Ellipse(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; const AOrigin, AXAxis, AYAxis: TPointF;
        const ABrush: TUniversalBrush; AWidth: single; AAlpha: Word=65535); overload; virtual; abstract;
    class procedure EllipseAntialias(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; x, y, rx, ry: single;
        const ABrush: TUniversalBrush; AWidth: single); overload; virtual; abstract;
    class procedure EllipseAntialias(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; const AOrigin, AXAxis, AYAxis: TPointF;
        const ABrush: TUniversalBrush; AWidth: single); overload; virtual; abstract;

    {==== Filling ====}
    class procedure FillRectAntialias(ADest: TCustomUniversalBitmap;
                    x, y, x2, y2: single; const ABrush: TUniversalBrush;
                    APixelCenteredCoordinates: boolean = true); virtual; abstract;
    class procedure FillShapeAntialias(ADest: TCustomUniversalBitmap;
                    AShape: TBGRACustomFillInfo; AFillMode: TFillMode;
                    ABrush: TUniversalBrush); virtual; abstract;
    class procedure FillPolyAntialias(ADest: TCustomUniversalBitmap;
                    const APoints: array of TPointF; AFillMode: TFillMode;
                    ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean); virtual; abstract;
    class procedure FillEllipseAntialias(ADest: TCustomUniversalBitmap;
                    x, y, rx, ry: single; const ABrush: TUniversalBrush); overload; virtual; abstract;
    class procedure FillEllipseAntialias(ADest: TCustomUniversalBitmap;
                    const AOrigin, AXAxis, AYAxis: TPointF; const ABrush: TUniversalBrush); overload; virtual; abstract;

    //filters
    class procedure FilterBlurRadial(ASource: TCustomUniversalBitmap; const ABounds: TRect;
                              radiusX, radiusY: single; blurType: TRadialBlurType;
                              ADest: TCustomUniversalBitmap); virtual; abstract;
    class procedure FilterBlurMotion(ASource: TCustomUniversalBitmap; const ABounds: TRect;
                              distance: single; angle: single; oriented: boolean;
                              ADest: TCustomUniversalBitmap); virtual; abstract;
    class procedure FilterCustomBlur(ASource: TCustomUniversalBitmap; const ABounds: TRect;
                              mask: TCustomUniversalBitmap;
                              ADest: TCustomUniversalBitmap); virtual; abstract;

  end;
  TUniversalDrawerAny = class of TCustomUniversalDrawer;

var
  UniDrawerClass: TUniversalDrawerAny;

{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}

{ TUniversalBrush }

function TUniversalBrush.GetDoesNothing: boolean;
begin
  result := (Flags and UniversalBrushFlag_DoNothing) <> 0;
end;

procedure TUniversalBrush.SetColorspace(AValue: TColorspaceAny);
begin
  FColorspace:=AValue;
  InternalInitContext:= nil;
  InternalPutNextPixels := nil;
  Flags := 0;
end;

procedure TUniversalBrush.SetDoesNothing(AValue: boolean);
begin
  if AValue then Flags := Flags or UniversalBrushFlag_DoNothing
  else Flags := Flags and not UniversalBrushFlag_DoNothing;
end;

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

procedure TCustomUniversalBitmap.ClearTransparentPixels;
var
  alphaIdx, i: Integer;
  p: PByte;
begin
  alphaIdx := Colorspace.IndexOfAlphaChannel;
  if (alphaIdx=-1) or (FDataByte=nil) then exit;
  LoadFromBitmapIfNeeded;
  p := FDataByte;
  for i := NbPixels-1 downto 0 do
  begin
    if Colorspace.GetColorTransparency(p) = ctFullyTransparent then
      AssignTransparentPixel(p^);
    inc(p, FPixelSize);
  end;
  InvalidateBitmap;
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
var
  p: TBGRACustomPenStroker;
begin
  p := GetInternalPen;
  if p.Arrow = nil then
  begin
    if UniDrawerClass = nil then RaiseMissingUniDrawer;
    p.Arrow := UniDrawerClass.CreateArrow;
    p.Arrow.LineCap := LineCap;
    p.ArrowOwned := true;
  end;
  result := p.Arrow;
end;

function TCustomUniversalBitmap.GetLinearAntialiasing: boolean;
begin
  result := FAntialiasingDrawMode in[dmLinearBlend,dmXor];
end;

function TCustomUniversalBitmap.GetLineCap: TPenEndCap;
begin
  result := GetInternalPen.LineCap;
end;

function TCustomUniversalBitmap.GetInternalPen: TBGRACustomPenStroker;
begin
  if FPenStroker = nil then
  begin
    if UniDrawerClass = nil then RaiseMissingUniDrawer;
    FPenStroker := UniDrawerClass.CreatePenStroker;
  end;
  result := FPenStroker;
end;

function TCustomUniversalBitmap.GetPenStroker: TBGRACustomPenStroker;
begin
  result := GetInternalPen;
  if result.Arrow = nil then GetArrow;
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

procedure TCustomUniversalBitmap.PathStrokeAntialiasCallback(
  const APoints: array of TPointF; AClosed: boolean; AData: Pointer);
var pts: array of TPointF;
begin
  with TPathCallbackData(AData^) do
  begin
    if AClosed then
      pts := GetInternalPen.ComputePolygon(APoints, Width)
    else
      pts := GetInternalPen.ComputePolyline(APoints, Width);
    FillPolyAntialias(pts, BrushAddress^, PixelCenteredCoords);
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
  if AValue <> GetInternalPen.LineCap then
  begin
    GetInternalPen.LineCap := AValue;
    if Assigned(GetInternalPen.Arrow) then
      GetInternalPen.Arrow.LineCap := AValue;
  end;
end;

procedure TCustomUniversalBitmap.FillPolyAntialias(
  const APoints: array of TPointF; const ABrush: TUniversalBrush;
  APixelCenteredCoordinates: boolean);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.FillPolyAntialias(self, APoints, FillMode,
                  ABrush, APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.FillPolyAntialias(
  const APoints: array of TPointF; ATexture: IBGRAScanner;
  APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  FillPolyAntialias(APoints, b, APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.ErasePolyAntialias(
  const APoints: array of TPointF; AAlpha: Byte;
  APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  EraseBrush(b, AAlpha + (AAlpha shl 8));
  FillPolyAntialias(APoints, b, APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.FillEllipseAntialias(x, y, rx, ry: single;
  const ABrush: TUniversalBrush);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.FillEllipseAntialias(self, x,y,rx,ry, ABrush);
end;

procedure TCustomUniversalBitmap.FillEllipseAntialias(x, y, rx, ry: single;
  ATexture: IBGRAScanner);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  FillEllipseAntialias(x, y, rx, ry, b);
end;

procedure TCustomUniversalBitmap.EraseEllipseAntialias(x, y, rx, ry: single;
  AAlpha: Byte);
var
  b: TUniversalBrush;
begin
  EraseBrush(b, AAlpha + (AAlpha shl 8));
  FillEllipseAntialias(x, y, rx, ry, b);
end;

procedure TCustomUniversalBitmap.FillEllipseAntialias(const AOrigin, AXAxis,
  AYAxis: TPointF; const ABrush: TUniversalBrush);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.FillEllipseAntialias(self, AOrigin, AXAxis, AYAxis, ABrush);
end;

procedure TCustomUniversalBitmap.FillEllipseAntialias(const AOrigin, AXAxis,
  AYAxis: TPointF; ATexture: IBGRAScanner);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  FillEllipseAntialias(AOrigin, AXAxis, AYAxis, b);
end;

procedure TCustomUniversalBitmap.EraseEllipseAntialias(const AOrigin, AXAxis,
  AYAxis: TPointF; AAlpha: Byte);
var
  b: TUniversalBrush;
begin
  EraseBrush(b, AAlpha + (AAlpha shl 8));
  FillEllipseAntialias(AOrigin, AXAxis, AYAxis, b);
end;

procedure TCustomUniversalBitmap.FillRectAntialias(x, y, x2, y2: single;
  const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.FillRectAntialias(self, x,y,x2,y2, ABrush,APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.FillRectAntialias(x, y, x2, y2: single;
  ATexture: IBGRAScanner; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AntialiasingDrawMode);
  FillRectAntialias(x,y,x2,y2, b, APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.EraseRectAntialias(x, y, x2, y2: single;
  AAlpha: Byte; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  EraseBrush(b, AAlpha);
  FillRectAntialias(x,y,x2,y2, b, APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.FillRectAntialias(const ARectF: TRectF;
  const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean);
begin
  FillRectAntialias(ARectF.Left, ARectF.Top, ARectF.Right, ARectF.Bottom,
     ABrush, APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.FillRectAntialias(const ARectF: TRectF;
  ATexture: IBGRAScanner; APixelCenteredCoordinates: boolean);
begin
  FillRectAntialias(ARectF.Left, ARectF.Top, ARectF.Right, ARectF.Bottom,
     ATexture, APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.EraseRectAntialias(const ARectF: TRectF;
  AAlpha: Byte; APixelCenteredCoordinates: boolean);
begin
  EraseRectAntialias(ARectF.Left, ARectF.Top, ARectF.Right, ARectF.Bottom,
     AAlpha, APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.FillPath(APath: IBGRAPath;
  const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean);
begin
  FillPolyAntialias(APath.getPoints,ABrush,APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.FillPath(APath: IBGRAPath;
  ATexture: IBGRAScanner; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  FillPath(APath, b, APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.FillPath(APath: IBGRAPath;
  const AMatrix: TAffineMatrix; const ABrush: TUniversalBrush;
  APixelCenteredCoordinates: boolean);
begin
  FillPolyAntialias(APath.getPoints(AMatrix), ABrush,APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.FillPath(APath: IBGRAPath;
  const AMatrix: TAffineMatrix; ATexture: IBGRAScanner;
  APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  FillPath(APath,AMatrix, b, APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.ErasePath(APath: IBGRAPath; AAlpha: Byte;
  APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  EraseBrush(b, AAlpha + (AAlpha shl 8));
  FillPath(APath, b, APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.ErasePath(APath: IBGRAPath;
  const AMatrix: TAffineMatrix; AAlpha: byte; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  EraseBrush(b, AAlpha + (AAlpha shl 8));
  FillPath(APath,AMatrix, b, APixelCenteredCoordinates);
end;

function TCustomUniversalBitmap.ScanAtInteger(X, Y: integer): TBGRAPixel;
begin
  if (FScanWidth <> 0) and (FScanHeight <> 0) then
    FConvertToBGRA.Convert(GetPixelAddress(PositiveMod(X+ScanOffset.X, FScanWidth),
                             PositiveMod(Y+ScanOffset.Y, FScanHeight)),
                           @result, 1, FPixelSize, sizeof(TBGRAPixel), nil)
  else
    result := BGRAPixelTransparent;
end;

function TCustomUniversalBitmap.ScanAtIntegerExpanded(X, Y: integer): TExpandedPixel;
begin
  if (FScanWidth <> 0) and (FScanHeight <> 0) then
    FConvertToExpanded.Convert(GetPixelAddress(PositiveMod(X+ScanOffset.X, FScanWidth),
                                 PositiveMod(Y+ScanOffset.Y, FScanHeight)),
                               @result, 1, FPixelSize, sizeof(TExpandedPixel), nil)
  else
    result := ExpandedPixelTransparent;
end;

procedure TCustomUniversalBitmap.ScanMoveTo(X, Y: Integer);
begin
  if (FScanWidth = 0) or (FScanHeight = 0) then exit;
  LoadFromBitmapIfNeeded;
  FScanCurX := PositiveMod(X+ScanOffset.X, FScanWidth);
  FScanCurY := PositiveMod(Y+ScanOffset.Y, FScanHeight);
  FScanPtr := GetPixelAddress(FScanCurX,FScanCurY);
end;

function TCustomUniversalBitmap.ScanNextPixel: TBGRAPixel;
begin
  if (FScanWidth <> 0) and (FScanHeight <> 0) then
  begin
    FConvertToBGRA.Convert(FScanPtr, @result, 1, FPixelSize, sizeof(TBGRAPixel), nil);
    inc(FScanCurX);
    inc(FScanPtr, FPixelSize);
    if FScanCurX = FScanWidth then //cycle
    begin
      FScanCurX := 0;
      dec(FScanPtr, FRowSize);
    end;
  end
  else
    result := BGRAPixelTransparent;
end;

function TCustomUniversalBitmap.ScanNextExpandedPixel: TExpandedPixel;
begin
  if (FScanWidth <> 0) and (FScanHeight <> 0) then
  begin
    FConvertToExpanded.Convert(FScanPtr, @result, 1, FPixelSize, sizeof(TExpandedPixel), nil);
    inc(FScanCurX);
    inc(FScanPtr, FPixelSize);
    if FScanCurX = FScanWidth then //cycle
    begin
      FScanCurX := 0;
      dec(FScanPtr, FRowSize);
    end;
  end
  else
    result := BGRAPixelTransparent;
end;

function TCustomUniversalBitmap.ScanAt(X, Y: Single): TBGRAPixel;
begin
  result := ScanAtInteger(round(X),round(Y));
end;

function TCustomUniversalBitmap.ScanAtExpanded(X, Y: Single): TExpandedPixel;
begin
  result := ScanAtIntegerExpanded(round(X),round(Y));
end;

function TCustomUniversalBitmap.IsScanPutPixelsDefined: boolean;
begin
  result := False;
end;

procedure TCustomUniversalBitmap.ScanPutPixels(pdest: PBGRAPixel;
  count: integer; mode: TDrawMode);
begin
  //do nothing
end;

procedure TCustomUniversalBitmap.ScanSkipPixels(ACount: integer);
var
  fit: Integer;
begin
  if (FScanWidth <= 0) or (FScanHeight <= 0) then exit;
  if ACount >= FScanWidth then ACount := PositiveMod(ACount, FScanWidth);
  fit := FScanWidth-FScanCurX;
  if ACount >= fit then
  begin
    dec(ACount, fit);
    dec(FScanPtr, FScanCurX*PtrInt(FPixelSize));
    FScanCurX := 0;
  end;
  inc(FScanCurX, ACount);
  inc(FScanPtr, ACount*PtrInt(FPixelSize));
end;

function TCustomUniversalBitmap.GetImageBounds: TRect;
begin
  result := GetImageBounds(cAlpha);
end;

function TCustomUniversalBitmap.GetImageBounds(Channel: TChannel;
  ANothingValue: Byte): TRect;
begin
  result := GetImageBoundsWithin(rect(0,0,Width,Height), Channel, ANothingValue);
end;

function TCustomUniversalBitmap.GetImageBounds(Channels: TChannels;
  ANothingValue: Byte): TRect;
begin
  result := GetImageBoundsWithin(rect(0,0,Width,Height), Channels, ANothingValue);
end;

function TCustomUniversalBitmap.ProvidesScanline(ARect: TRect): boolean;
begin
  result := false;
end;

function TCustomUniversalBitmap.GetScanlineAt(X, Y: integer): PBGRAPixel;
begin
  result := nil;
end;

function TCustomUniversalBitmap.GetTextureGL: IUnknown;
begin
  result := nil;
end;

function TCustomUniversalBitmap.GetImageBoundsWithin(const ARect: TRect;
  Channel: TChannel; ANothingValue: Byte): TRect;
var
  idxChannel: Integer;
  actualRect: TRect;
  maxx, maxy, minx, miny, yb, xb, xb2: LongInt;
  p: PByte;
  minValueF, nothingValueF: Single;
begin
  case Channel of
  cAlpha: idxChannel := Colorspace.IndexOfAlphaChannel;
  cRed: idxChannel := Colorspace.IndexOfChannel('Red');
  cGreen: idxChannel := Colorspace.IndexOfChannel('Green');
  cBlue: idxChannel := Colorspace.IndexOfChannel('Blue');
  else raise exception.Create('Unexpected channel');
  end;
  if (idxChannel = -1) and (Channel in [cRed,cGreen,cBlue]) then
    idxChannel := Colorspace.IndexOfChannel('Gray');
  if idxChannel = -1 then raise exception.Create('Channel not found');
  minValueF := Colorspace.GetMinValue(idxChannel);
  nothingValueF := (ANothingValue - minValueF)/(Colorspace.GetMaxValue(idxChannel)-minValueF);
  actualRect := TRect.Intersect(ARect, rect(0,0,Width,Height));
  maxx := actualRect.Left-1;
  maxy := actualRect.Top-1;
  minx := actualRect.Right;
  miny := actualRect.Bottom;
  for yb := actualRect.Top to actualRect.Bottom-1 do
  begin
    p := GetPixelAddress(actualRect.Left,yb);
    for xb := actualRect.Left to actualRect.Right - 1 do
    begin
      if Colorspace.GetChannel(p, idxChannel) <> nothingValueF then
      begin
        if xb < minx then
          minx := xb;
        if yb < miny then
          miny := yb;
        if xb > maxx then
          maxx := xb;
        if yb > maxy then
          maxy := yb;

        inc(p, (actualRect.Right-1-xb)*FPixelSize);
        for xb2 := actualRect.Right-1 downto xb+1 do
        begin
          if Colorspace.GetChannel(p, idxChannel) <> nothingValueF then
          begin
            if xb2 > maxx then
              maxx := xb2;
            break;
          end;
          dec(p, FPixelSize);
        end;
        break;
      end;
      Inc(p, FPixelSize);
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

function TCustomUniversalBitmap.GetImageBoundsWithin(const ARect: TRect;
  Channels: TChannels; ANothingValue: Byte): TRect;
var
  c: TChannel;
  resultForChannel: TRect;
begin
  result := EmptyRect;
  for c := low(TChannel) to high(TChannel) do
  begin
    if c in Channels then
    begin
      resultForChannel := GetImageBoundsWithin(ARect, c, ANothingValue);
      if IsRectEmpty(result) then result := resultForChannel
      else UnionRect(result, result, resultForChannel);
    end;
  end;
end;

function TCustomUniversalBitmap.GetScanCustomColorspace: TColorspaceAny;
begin
  result := Colorspace;
end;

procedure TCustomUniversalBitmap.ScanNextCustomChunk(var ACount: integer; out
  APixels: Pointer);
var
  quantity: Integer;
begin
  if (FScanWidth = 0) or (FScanHeight = 0) then raise exception.Create('Zero size scanner');
  APixels := FScanPtr;
  quantity := FScanWidth-FScanCurX;
  if ACount <= quantity then
    quantity := ACount
    else ACount := quantity;
  Inc(FScanPtr, quantity*PtrInt(FPixelSize));
  inc(FScanCurX, quantity);
  if FScanCurX = FWidth then
  begin
    FScanCurX := 0;
    Dec(FScanPtr, RowSize);
  end;
end;

procedure TCustomUniversalBitmap.ScanNextMaskChunk(var ACount: integer; out AMask: PByteMask; out AStride: integer);
begin
  raise exception.Create('This bitmap does not provide a mask.');
end;

function TCustomUniversalBitmap.ScanAtIntegerMask(X,Y: integer): TByteMask;
begin
  result := ScanAtMask(X,Y);
end;

function TCustomUniversalBitmap.ScanAtMask(X,Y: Single): TByteMask;
begin
  result.gray := 0;
  raise exception.Create('This bitmap does not provide a mask.');
end;

{ Interface gateway }
function TCustomUniversalBitmap.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := longint(E_NOINTERFACE);
end;

{ There is no automatic reference counting, but it is compulsory to define these functions }
function TCustomUniversalBitmap._AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function TCustomUniversalBitmap._Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

procedure TCustomUniversalBitmap.PutImage(X, Y: integer;
  ASource: TCustomUniversalBitmap; AMode: TDrawMode; AOpacity: byte);
var
  oldOfs: TPoint;
begin
  if (ASource = nil) or (AOpacity = 0) then exit;
  oldOfs := ASource.ScanOffset;
  ASource.ScanOffset := Point(-X,-Y);
  FillRect(RectWithSize(X,Y,ASource.Width,ASource.Height), ASource, AMode, AOpacity + (AOpacity shl 8));
  ASource.ScanOffset := oldOfs;
end;

function TCustomUniversalBitmap.FilterBlurRadial(radius: single;
  blurType: TRadialBlurType): TCustomUniversalBitmap;
begin
  Result := FilterBlurRadial(rect(0,0,Width,Height), radius,radius, blurType);
end;

function TCustomUniversalBitmap.FilterBlurRadial(const ABounds: TRect;
  radius: single; blurType: TRadialBlurType): TCustomUniversalBitmap;
begin
  Result := FilterBlurRadial(ABounds, radius,radius, blurType);
end;

function TCustomUniversalBitmap.FilterBlurRadial(radiusX, radiusY: single;
  blurType: TRadialBlurType): TCustomUniversalBitmap;
begin
  Result := FilterBlurRadial(rect(0,0,Width,Height), radiusX,radiusY, blurType);
end;

function TCustomUniversalBitmap.FilterBlurRadial(const ABounds: TRect; radiusX,
  radiusY: single; blurType: TRadialBlurType): TCustomUniversalBitmap;
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  result := NewBitmap; result.SetSize(Width,Height); fillbyte(result.DataByte^, result.Height*result.RowSize, 0);
  UniDrawerClass.FilterBlurRadial(self, ABounds, radiusX,radiusY, blurType, result);
end;

function TCustomUniversalBitmap.FilterBlurMotion(distance: single;
  angle: single; oriented: boolean): TCustomUniversalBitmap;
begin
  result := FilterBlurMotion(rect(0,0,Width,Height), distance, angle, oriented);
end;

function TCustomUniversalBitmap.FilterBlurMotion(const ABounds: TRect;
  distance: single; angle: single; oriented: boolean): TCustomUniversalBitmap;
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  result := NewBitmap; result.SetSize(Width,Height); fillbyte(result.DataByte^, result.Height*result.RowSize, 0);
  UniDrawerClass.FilterBlurMotion(self, ABounds, distance, angle, oriented, result);
end;

function TCustomUniversalBitmap.FilterCustomBlur(mask: TCustomUniversalBitmap): TCustomUniversalBitmap;
begin
  result := FilterCustomBlur(rect(0,0,Width,Height), mask);
end;

function TCustomUniversalBitmap.FilterCustomBlur(const ABounds: TRect;
  mask: TCustomUniversalBitmap): TCustomUniversalBitmap;
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  result := NewBitmap; result.SetSize(Width,Height); fillbyte(result.DataByte^, result.Height*result.RowSize, 0);
  UniDrawerClass.FilterCustomBlur(self, ABounds, mask, result);
end;

procedure TCustomUniversalBitmap.DrawLineAntialias(x1, y1, x2, y2: single;
  const ABrush: TUniversalBrush; APenWidth: single);
begin
  FillPolyAntialias(GetInternalPen.ComputePolyline([PointF(x1,y1),PointF(x2,y2)],APenWidth), ABrush);
end;

procedure TCustomUniversalBitmap.DrawLineAntialias(x1, y1, x2, y2: single;
  ATexture: IBGRAScanner; APenWidth: single);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  DrawLineAntialias(x1,y1,x2,y2, b,APenWidth);
end;

procedure TCustomUniversalBitmap.DrawLineAntialias(x1, y1, x2, y2: single;
  const ABrush: TUniversalBrush; APenWidth: single; AClosedCap: boolean);
begin
  FillPolyAntialias(GetInternalPen.ComputePolyline([PointF(x1,y1),PointF(x2,y2)],APenWidth,AClosedCap), ABrush);
end;

procedure TCustomUniversalBitmap.DrawLineAntialias(x1, y1, x2, y2: single;
  ATexture: IBGRAScanner; APenWidth: single; AClosedCap: boolean);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  DrawLineAntialias(x1,y1,x2,y2, b,APenWidth, AClosedCap);
end;

procedure TCustomUniversalBitmap.EraseLineAntialias(x1, y1, x2, y2: single;
  AAlpha: Byte; APenWidth: single);
var
  b: TUniversalBrush;
begin
  EraseBrush(b, AAlpha + (AAlpha shl 8));
  DrawLineAntialias(x1,y1,x2,y2, b, APenWidth);
end;

procedure TCustomUniversalBitmap.EraseLineAntialias(x1, y1, x2, y2: single;
  AAlpha: Byte; APenWidth: single; AClosedCap: boolean);
var
  b: TUniversalBrush;
begin
  EraseBrush(b, AAlpha + (AAlpha shl 8));
  DrawLineAntialias(x1,y1,x2,y2, b, APenWidth, AClosedCap);
end;

procedure TCustomUniversalBitmap.DrawPolyLineAntialias(
  const APoints: array of TPointF; const ABrush: TUniversalBrush;
  APenWidth: single);
begin
  FillPolyAntialias(GetInternalPen.ComputePolyline(APoints,APenWidth),ABrush);
end;

procedure TCustomUniversalBitmap.DrawPolyLineAntialias(
  const APoints: array of TPointF; ATexture: IBGRAScanner; APenWidth: single);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  DrawPolyLineAntialias(APoints, b, APenWidth);
end;

procedure TCustomUniversalBitmap.ErasePolyLineAntialias(
  const APoints: array of TPointF; AAlpha: byte; APenWidth: single);
var
  b: TUniversalBrush;
begin
  EraseBrush(b, AAlpha + (AAlpha shl 8));
  DrawPolyLineAntialias(APoints, b, APenWidth);
end;

procedure TCustomUniversalBitmap.DrawPolyLineAntialias(
  const APoints: array of TPointF; const ABrush: TUniversalBrush;
  APenWidth: single; AClosedCap: boolean);
begin
  FillPolyAntialias(GetInternalPen.ComputePolyline(APoints,APenWidth,AClosedCap),ABrush);
end;

procedure TCustomUniversalBitmap.DrawPolyLineAntialias(
  const APoints: array of TPointF; ATexture: IBGRAScanner; APenWidth: single;
  AClosedCap: boolean);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  DrawPolyLineAntialias(APoints, b, APenWidth, AClosedCap);
end;

procedure TCustomUniversalBitmap.ErasePolyLineAntialias(
  const APoints: array of TPointF; AAlpha: byte; APenWidth: single;
  AClosedCap: boolean);
var
  b: TUniversalBrush;
begin
  EraseBrush(b, AAlpha + (AAlpha shl 8));
  DrawPolyLineAntialias(APoints, b, APenWidth, AClosedCap);
end;

procedure TCustomUniversalBitmap.DrawPolyLineAntialiasAutocycle(
  const APoints: array of TPointF; const ABrush: TUniversalBrush;
  APenWidth: single);
begin
   FillPolyAntialias(GetInternalPen.ComputePolylineAutoCycle(APoints,APenWidth),ABrush);
end;

procedure TCustomUniversalBitmap.DrawPolyLineAntialiasAutocycle(
  const APoints: array of TPointF; ATexture: IBGRAScanner; APenWidth: single);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  DrawPolyLineAntialiasAutocycle(APoints, b, APenWidth);
end;

procedure TCustomUniversalBitmap.ErasePolyLineAntialiasAutocycle(
  const APoints: array of TPointF; AAlpha: byte; APenWidth: single);
var
  b: TUniversalBrush;
begin
  EraseBrush(b, AAlpha + (AAlpha shl 8));
  DrawPolyLineAntialiasAutocycle(APoints, b, APenWidth);
end;

procedure TCustomUniversalBitmap.DrawPolygonAntialias(
  const APoints: array of TPointF; const ABrush: TUniversalBrush;
  APenWidth: single);
begin
   FillPolyAntialias(GetInternalPen.ComputePolygon(APoints,APenWidth),ABrush);
end;

procedure TCustomUniversalBitmap.DrawPolygonAntialias(
  const APoints: array of TPointF; ATexture: IBGRAScanner; APenWidth: single);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  DrawPolygonAntialias(APoints, b, APenWidth);
end;

procedure TCustomUniversalBitmap.ErasePolygonOutlineAntialias(
  const APoints: array of TPointF; AAlpha: byte; APenWidth: single);
var
  b: TUniversalBrush;
begin
  EraseBrush(b, AAlpha + (AAlpha shl 8));
  DrawPolygonAntialias(APoints, b, APenWidth);
end;

procedure TCustomUniversalBitmap.Ellipse(x, y, rx, ry: single;
  const ABrush: TUniversalBrush; AWidth: single; AAlpha: Word);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.Ellipse(self, GetInternalPen, x,y,rx,ry, ABrush, AWidth, AAlpha);
end;

procedure TCustomUniversalBitmap.Ellipse(x, y, rx, ry: single;
  ATexture: IBGRAScanner; AWidth: single; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AMode);
  Ellipse(x,y,rx,ry, b, AWidth, AAlpha);
end;

procedure TCustomUniversalBitmap.Ellipse(const AOrigin, AXAxis, AYAxis: TPointF;
  const ABrush: TUniversalBrush; AWidth: single; AAlpha: Word);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.Ellipse(self, GetInternalPen, AOrigin, AXAxis, AYAxis, ABrush, AWidth, AAlpha);
end;

procedure TCustomUniversalBitmap.Ellipse(const AOrigin, AXAxis, AYAxis: TPointF;
  ATexture: IBGRAScanner; AWidth: single; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AMode);
  Ellipse(AOrigin, AXAxis, AYAxis, b,AWidth,AAlpha);
end;

procedure TCustomUniversalBitmap.EllipseAntialias(x, y, rx, ry: single;
  const ABrush: TUniversalBrush; AWidth: single);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.EllipseAntialias(self, GetInternalPen, x,y,rx,ry, ABrush, AWidth);
end;

procedure TCustomUniversalBitmap.EllipseAntialias(x, y, rx, ry: single;
  ATexture: IBGRAScanner; AWidth: single);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  EllipseAntialias(x,y,rx,ry, b, AWidth);
end;

procedure TCustomUniversalBitmap.EllipseAntialias(const AOrigin, AXAxis,
  AYAxis: TPointF; const ABrush: TUniversalBrush; AWidth: single);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.EllipseAntialias(self, GetInternalPen, AOrigin, AXAxis, AYAxis, ABrush, AWidth);
end;

procedure TCustomUniversalBitmap.EllipseAntialias(const AOrigin, AXAxis,
  AYAxis: TPointF; ATexture: IBGRAScanner; AWidth: single);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  EllipseAntialias(AOrigin, AXAxis, AYAxis, b,AWidth);
end;

procedure TCustomUniversalBitmap.DrawPath(APath: IBGRAPath;
  const ABrush: TUniversalBrush; AWidth: single;
  APixelCenteredCoordinates: boolean);
var
  data: TPathCallbackData;
begin
  if ABrush.DoesNothing then exit;
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  data.BrushAddress := @ABrush;
  data.Alpha:= 65535;
  data.Width:= AWidth;
  data.PixelCenteredCoords := APixelCenteredCoordinates;
  APath.stroke(@PathStrokeAntialiasCallback, @data);
end;

procedure TCustomUniversalBitmap.DrawPath(APath: IBGRAPath;
  ATexture: IBGRAScanner; AWidth: single;
  APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  DrawPath(APath, b,AWidth,APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.DrawPath(APath: IBGRAPath;
  const AMatrix: TAffineMatrix; const ABrush: TUniversalBrush; AWidth: single;
  APixelCenteredCoordinates: boolean);
var
  data: TPathCallbackData;
begin
  if ABrush.DoesNothing then exit;
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  data.BrushAddress := @ABrush;
  data.Alpha:= 65535;
  data.Width:= AWidth;
  data.PixelCenteredCoords := APixelCenteredCoordinates;
  APath.stroke(@PathStrokeAntialiasCallback, AMatrix, @data);
end;

procedure TCustomUniversalBitmap.DrawPath(APath: IBGRAPath;
  const AMatrix: TAffineMatrix; ATexture: IBGRAScanner; AWidth: single;
  APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  DrawPath(APath,AMatrix, b,AWidth,APixelCenteredCoordinates);
end;

procedure TCustomUniversalBitmap.FillShapeAntialias(
  AShape: TBGRACustomFillInfo; const ABrush: TUniversalBrush);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  UniDrawerClass.FillShapeAntialias(self, AShape, FillMode,
                  ABrush);
end;

procedure TCustomUniversalBitmap.FillShapeAntialias(
  AShape: TBGRACustomFillInfo; ATexture: IBGRAScanner);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture,AntialiasingDrawMode);
  FillShapeAntialias(AShape, b);
end;

procedure TCustomUniversalBitmap.EraseShapeAntialias(
  AShape: TBGRACustomFillInfo; AAlpha: Byte);
var
  b: TUniversalBrush;
begin
  EraseBrush(b, AAlpha + (AAlpha shl 8));
  FillShapeAntialias(AShape, b);
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
  FConvertToFPColor:= FColorspace.GetBridgedConversion(TFPColorColorspace);
  FConvertFromFPColor:= TFPColorColorspace.GetBridgedConversion(FColorspace);
  FConvertToBGRA := FColorspace.GetBridgedConversion(TBGRAPixelColorspace);
  FConvertFromBGRA := TBGRAPixelColorspace.GetBridgedConversion(FColorspace);
  FConvertToExpanded := FColorspace.GetBridgedConversion(TExpandedPixelColorspace);
  FConvertFromExpanded := TExpandedPixelColorspace.GetBridgedConversion(FColorspace);
  FReferenceWhite := nil;
  FWidth := 0;
  FHeight := 0;

  FScanWidth := 0;
  FScanHeight:= 0;
  ScanOffset := Point(0,0);
  FScanPtr := nil;
  FScanCurX:= 0;
  FScanCurY:= 0;

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
  FConvertFromBGRA.Convert(@c,@c2,1,sizeof(c),FPixelSize,FReferenceWhite);
  SolidBrushIndirect(ABrush, @c2, ADrawMode);
end;

procedure TCustomUniversalBitmap.SolidBrushBGRA(out ABrush: TUniversalBrush;
  AColor: TBGRAPixel; ADrawMode: TDrawMode);
var
  c2: array[0..31] of byte;
begin
  FConvertFromBGRA.Convert(@AColor,@c2,1,sizeof(AColor),FPixelSize,FReferenceWhite);
  SolidBrushIndirect(ABrush, @c2, ADrawMode);
end;

procedure TCustomUniversalBitmap.SolidBrushExpanded(out
  ABrush: TUniversalBrush; ARed, AGreen, ABlue, AAlpha: Word;
  ADrawMode: TDrawMode);
var c: TExpandedPixel;
  c2: array[0..31] of byte;
begin
  c.red := ARed; c.green := AGreen; c.blue := ABlue; c.alpha := AAlpha;
  FConvertFromExpanded.Convert(@c,@c2,1,sizeof(c),FPixelSize,FReferenceWhite);
  SolidBrushIndirect(ABrush, @c2, ADrawMode);
end;

procedure TCustomUniversalBitmap.SolidBrushExpanded(out
  ABrush: TUniversalBrush; AColor: TExpandedPixel; ADrawMode: TDrawMode);
var
  c2: array[0..31] of byte;
begin
  FConvertFromExpanded.Convert(@AColor,@c2,1,sizeof(AColor),FPixelSize,FReferenceWhite);
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
  PDefaultSolidBrushIndirectFixedData(@ABrush.FixedData)^.PixelSize:= FPixelSize;

  if (ADrawMode <> dmSet) and (ct <> ctFullyOpaque) then
  begin
    ABrush.InternalPutNextPixels:= @DefaultSolidBrushIndirectSkipPixels;
    ABrush.DoesNothing:= true;
  end
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

class procedure TCustomUniversalBitmap.MaskBrush(out ABrush: TUniversalBrush;
  AScanner: IBGRAScanner; AOffsetX: integer; AOffsetY: integer);
begin
  raise exception.Create('Mask brush not implemented');
end;

procedure TCustomUniversalBitmap.ReallocData;
begin
  ReAllocMem(FDataByte, FHeight * FRowSize);
  if (FNbPixels > 0) and (FDataByte = nil) then
    raise EOutOfMemory.Create('TUniversalBitmap: Not enough memory');
  InvalidateBitmap;
  FScanPtr:= nil;
end;

procedure TCustomUniversalBitmap.FreeData;
begin
  Freemem(FDataByte);
  FDataByte := nil;
  FScanPtr:= nil;
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
  FConvertFromFPColor.Convert(@Value, GetPixelAddress(x,y),
                      1, sizeof(TFPColor), FPixelSize, FReferenceWhite);
  InvalidateBitmap;
end;

function TCustomUniversalBitmap.GetInternalColor(x, y: integer): TFPColor;
begin
  if not PtInClipRect(x,y) then exit(colTransparent);
  LoadFromBitmapIfNeeded;
  FConvertToFPColor.Convert(GetPixelAddress(x,y), @result,
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

procedure TCustomUniversalBitmap.Assign(Source: TPersistent);
var pdest: PByte;
  x,y: NativeInt;
  col: TFPColor;
begin
  if Source is TCustomUniversalBitmap then
  begin
    SetSize(TCustomUniversalBitmap(Source).Width, TCustomUniversalBitmap(Source).Height);
    PutImage(0, 0, TCustomUniversalBitmap(Source), dmSet);
  end else
  if Source is TFPCustomImage then
  begin
    SetSize(TFPCustomImage(Source).Width, TFPCustomImage(Source).Height);
    for y := 0 to TFPCustomImage(Source).Height-1 do
    begin
      pdest := GetPixelAddress(0,y);
      for x := 0 to TFPCustomImage(Source).Width-1 do
      begin
        col := TFPCustomImage(Source).Colors[x,y];
        FConvertFromFPColor.Convert(@col, pdest, 1, sizeof(TFPColor), FPixelSize, nil);
        inc(pdest);
      end;
    end;
  end else
    inherited Assign(Source);
end;

function TCustomUniversalBitmap.NewBitmap: TCustomUniversalBitmap;
begin
  result := InternalNew;
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
  ABitmap.ScanOffset := ScanOffset;
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
  FScanWidth := AWidth;
  FScanHeight:= AHeight;
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

procedure TCustomUniversalBitmap.LoadFromFile(const AFilename: string);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.LoadFromFile(self, AFilename);
end;

procedure TCustomUniversalBitmap.LoadFromFile(const AFilename: string;
  AOptions: TBGRALoadingOptions);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.LoadFromFile(self, AFilename, AOptions);
end;

procedure TCustomUniversalBitmap.LoadFromFile(const AFilename: String;
  AHandler: TFPCustomImageReader);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.LoadFromFile(self, AFilename, AHandler);
end;

procedure TCustomUniversalBitmap.LoadFromFile(const AFilename: String;
  AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.LoadFromFile(self, AFilename, AHandler, AOptions);
end;

procedure TCustomUniversalBitmap.LoadFromFileUTF8(const AFilenameUTF8: string;
  AOptions: TBGRALoadingOptions);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.LoadFromFileUTF8(self, AFilenameUTF8, AOptions);
end;

procedure TCustomUniversalBitmap.LoadFromFileUTF8(const AFilenameUTF8: string;
  AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.LoadFromFileUTF8(self, AFilenameUTF8, AHandler, AOptions);
end;

procedure TCustomUniversalBitmap.LoadFromStream(AStream: TStream);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.LoadFromStream(self, AStream);
end;

procedure TCustomUniversalBitmap.LoadFromStream(AStream: TStream;
  AOptions: TBGRALoadingOptions);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.LoadFromStream(self, AStream, AOptions);
end;

procedure TCustomUniversalBitmap.LoadFromStream(AStream: TStream;
  AHandler: TFPCustomImageReader);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.LoadFromStream(self, AStream, AHandler);
end;

procedure TCustomUniversalBitmap.LoadFromStream(AStream: TStream;
  AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.LoadFromStream(self, AStream, AHandler, AOptions);
end;

procedure TCustomUniversalBitmap.LoadFromResource(AFilename: string);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.LoadFromResource(self, AFilename);
end;

procedure TCustomUniversalBitmap.LoadFromResource(AFilename: string;
  AOptions: TBGRALoadingOptions);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.LoadFromResource(self, AFilename, AOptions);
end;

procedure TCustomUniversalBitmap.LoadFromResource(AFilename: string;
  AHandler: TFPCustomImageReader);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.LoadFromResource(self, AFilename, AHandler);
end;

procedure TCustomUniversalBitmap.LoadFromResource(AFilename: string;
  AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.LoadFromResource(self, AFilename, AHandler, AOptions);
end;

procedure TCustomUniversalBitmap.SaveToFile(const AFilename: string);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.SaveToFile(self, AFilename);
end;

procedure TCustomUniversalBitmap.SaveToFile(const AFilename: string;
  AHandler: TFPCustomImageWriter);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.SaveToFile(self, AFilename, AHandler);
end;

procedure TCustomUniversalBitmap.SaveToFileUTF8(const AFilenameUTF8: string);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.SaveToFileUTF8(self, AFilenameUTF8);
end;

procedure TCustomUniversalBitmap.SaveToFileUTF8(const AFilenameUTF8: string;
  AHandler: TFPCustomImageWriter);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.SaveToFile(self, AFilenameUTF8, AHandler);
end;

procedure TCustomUniversalBitmap.SaveToStreamAs(AStream: TStream;
  AFormat: TBGRAImageFormat);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.SaveToStreamAs(self, AStream, AFormat);
end;

procedure TCustomUniversalBitmap.SaveToStreamAsPng(AStream: TStream);
begin
  if UniDrawerClass = nil then RaiseMissingUniDrawer;
  UniDrawerClass.SaveToStreamAsPng(self, AStream);
end;

procedure TCustomUniversalBitmap.NoClip;
begin
  FClipRect := rect(0,0,FWidth,FHeight);
end;

function TCustomUniversalBitmap.IntersectClip(const ARect: TRect): TRect;
var
  remain: TRect;
begin
  result := ClipRect;
  remain := EmptyRect;
  IntersectRect(remain, ARect, result);
  ClipRect := remain;
end;

procedure TCustomUniversalBitmap.Fill(const ABrush: TUniversalBrush; AAlpha: Word = 65535);
var
  pDest: PByte;
  delta: PtrInt;
  yb: Integer;
  ctx: TUniBrushContext;
begin
  if ABrush.DoesNothing or (NbPixels = 0) then exit;
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

procedure TCustomUniversalBitmap.ApplyMask(mask: TCustomUniversalBitmap; AAlpha: Word);
begin
  ApplyMask(mask, Rect(0,0,Width,Height), Point(0,0), AAlpha);
end;

procedure TCustomUniversalBitmap.ApplyMask(mask: TCustomUniversalBitmap; ARect: TRect; AAlpha: Word);
begin
  ApplyMask(mask, ARect, ARect.TopLeft, AAlpha);
end;

{ Apply a mask to the bitmap. It means that alpha channel is
  changed according to grayscale values of the mask.

  See : http://wiki.lazarus.freepascal.org/BGRABitmap_tutorial_5 }
procedure TCustomUniversalBitmap.ApplyMask(mask: TCustomUniversalBitmap;
  ARect: TRect; AMaskRectTopLeft: TPoint; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  MaskBrush(b, mask, AMaskRectTopLeft.X-ARect.Left, AMaskRectTopLeft.Y-ARect.Top);
  FillRect(ARect, b, AAlpha);
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
    (AAlpha = 0) or ABrush.DoesNothing then exit;

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
  if not CheckVertLineBounds(x,y,y2) or ABrush.DoesNothing then exit;
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
  if ABrush.DoesNothing then exit;
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
  if ABrush.DoesNothing then exit;
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
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  FillPoly(APath.getPoints, ABrush, APixelCenteredCoordinates, AAlpha);
end;

procedure TCustomUniversalBitmap.FillPathAliased(APath: IBGRAPath;
  const AMatrix: TAffineMatrix; const ABrush: TUniversalBrush;
  APixelCenteredCoordinates: boolean; AAlpha: Word);
begin
  if UniDrawerClass=nil then RaiseMissingUniDrawer;
  FillPoly(APath.getPoints(AMatrix), ABrush, APixelCenteredCoordinates, AAlpha);
end;

procedure TCustomUniversalBitmap.FillPathAliased(APath: IBGRAPath;
  ATexture: IBGRAScanner; AMode: TDrawMode; APixelCenteredCoordinates: boolean;
  AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AMode);
  FillPathAliased(APath, b,APixelCenteredCoordinates,AAlpha);
end;

procedure TCustomUniversalBitmap.FillPathAliased(APath: IBGRAPath;
  const AMatrix: TAffineMatrix; ATexture: IBGRAScanner; AMode: TDrawMode;
  APixelCenteredCoordinates: boolean; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  ScannerBrush(b, ATexture, AMode);
  FillPathAliased(APath,AMatrix, b,APixelCenteredCoordinates,AAlpha);
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
