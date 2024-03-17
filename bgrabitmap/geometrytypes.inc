// SPDX-License-Identifier: LGPL-3.0-linking-exception
{=== Geometry types ===}{}

{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}
type
  TPoint = BGRAClasses.TPoint;

  {** A constant representing an empty point (with minimum possible integer values for X and Y)
  that can be used as a separator in a list of TPoint }
  const EmptyPoint : TPoint = (X: -2147483648; Y: -2147483648);

{** Checks if the given point is equivalent to EmptyPoint }
function IsEmptyPoint(const APoint: TPoint): boolean;

  {* Value indicating that there is nothing in the single-precision floating point value.
     It is also used as a separator in lists }
  const EmptySingle = single(-3.402823e38);

type
  {* Pointer to a TPointF structure }
  PPointF = ^BGRAClasses.TPointF;
  TPointF = BGRAClasses.TPointF;

  {** Value indicating that there is an empty TPointF structure.
      It is also used as a separator in lists of points }
  const EmptyPointF: TPointF = (x: -3.402823e38; y: -3.402823e38);

  {----------------- Operators for TPointF --------------------}
  {** Creates a new structure with values _x_ and _y_ }
  function PointF(x, y: single): TPointF; overload;
  {** Creates a new structure converting integer values to single-precision floating-point coordinates }
  function PointF(pt: TPoint): TPointF; overload;
  {** Checks if the structure is empty (equal to EmptyPointF) }
  function isEmptyPointF(const pt: TPointF): boolean;
  {** Checks if both _x_ and _y_ are equal }
  operator = (const pt1, pt2: TPointF): boolean; inline;
  {** Adds _x_ and _y_ components separately. It is like adding vectors }
  operator + (const pt1, pt2: TPointF): TPointF; inline;
  {** Subtract _x_ and _y_ components separately. It is like subtracting vectors }
  operator - (const pt1, pt2: TPointF): TPointF; inline;
  {** Returns a point with opposite values for _x_ and _y_ components }
  operator - (const pt2: TPointF): TPointF; inline;
  {$if FPC_FULLVERSION < 30203}
  {** Scalar product (deprecated): multiplies _x_ and _y_ components and returns the sum }
  operator * (const pt1, pt2: TPointF): single; inline;
  {** Scalar product: multiplies _x_ and _y_ components and returns the sum }
  operator ** (const pt1, pt2: TPointF): single; inline;
  {$endif}
  {** Multiplies both _x_ and _y_ by _factor_. It scales the vector represented by (_x_, _y_) }
  operator * (const pt1: TPointF; factor: single): TPointF; inline;
  {** Multiplies both _x_ and _y_ by _factor_. It scales the vector represented by (_x_, _y_) }
  operator * (factor: single; const pt1: TPointF): TPointF; inline;
  {** Returns the length of the vector (_dx_, _dy_) }
  function VectLen(const dx,dy: single): single; overload; inline;
  {** Returns the length of the vector represented by (_x_, _y_) }
  function VectLen(const v: TPointF): single; overload;
  {** Computes the determinant of the two vectors (equal to the determinant of the square matrix
      with those vectors as columns) }
  function VectDet(const v1,v2: TPointF): double; inline;

type
  { TPointFHelper }

  {* Helper for TPointF structure providing additional functionality for point manipulation }
  TPointFHelper = record helper for TPointF
    {** Offsets the point by another point (TPointF) }
    procedure Offset(const apt: TPointF); overload;
    {** Offsets the point by another point (TPoint) }
    procedure Offset(const apt: TPoint); overload;
    {** Offsets the point by specified distances in the X (_dx_) and Y (_dy_) directions (integer values) }
    procedure Offset(dx, dy: longint); overload;
    {** Offsets the point by specified distances in the X (_dx_) and Y (_dy_) directions (single values) }
    procedure Offset(dx, dy: single); overload;
    {** Scales the point as a vector from the origin) by a specified factor }
    procedure Scale(AScale: single);
    {** Normalizes the point (modifies it to a unit length) }
    procedure Normalize;

    {** Rounds the coordinates of the point to the nearest integer towards positive infinity }
    function Ceiling: TPoint;
    {** Truncates the coordinates of the point, discarding fractional part }
    function Truncate: TPoint;
    {** Rounds the coordinates of the point to the nearest integer towards negative infinity }
    function Floor: TPoint;
    {** Rounds the coordinates of the point to the nearest integer }
    function Round: TPoint;
    {** Calculates the length (magnitude) of the point from the origin }
    function Length: Single;
    {** Determines if the point is empty (has special coordinates EmptyPointF) }
    function IsEmpty: boolean;
  end;

  {* Contains an array of points with single-precision floating point coordinates }
  ArrayOfTPointF = array of TPointF;

  {** Creates an array of TPointF }
  function PointsF(const pts: array of TPointF): ArrayOfTPointF;
  {** Concatenates arrays of TPointF }
  function ConcatPointsF(const APolylines: array of ArrayOfTPointF; AInsertEmptyPointInBetween: boolean = false): ArrayOfTPointF;
  {** Compute the length of the polyline contained in the array.
      _AClosed_ specifies if the last point is to be joined to the first one }
  function PolylineLen(const pts: array of TPointF; AClosed: boolean = false): single;

type
  {* An affine matrix contains three 2D vectors: the image of x, the image of y and the translation }
  TAffineMatrix = array[1..2,1..3] of single;

  TRectF = BGRAClasses.TRectF;

  {** A constant representing an empty rectangle with all coordinates set to 0 }
  const EmptyRectF : TRectF = (left:0; top:0; right:0; bottom: 0);

  {** Creates a TRectF structure with specified left, top, right, and bottom coordinates }
  function RectF(Left, Top, Right, Bottom: Single): TRectF;

  {** Creates a TRectF structure from two TPointF points representing the top-left and bottom-right corners }
  function RectF(const ATopLeft,ABottomRight: TPointF): TRectF;

  {** Creates a TRectF structure from a TRect structure (integer coordinates converted to single precision) }
  function RectF(const ARect: TRect): TRectF;

  {** Creates a TRectF structure with a specified position (left, top) and size (width, height) }
  function RectWithSizeF(left,top,width,height: Single): TRectF;

  {** Checks if the given TRectF structure is empty (of zero size) }
  function IsEmptyRectF(const ARect:TRectF): boolean;

  {** A value for an empty rectangle with integer coordinates }
  const EmptyRect : TRect = (left:0; top:0; right:0; bottom: 0);

  {** Checks if a point is in a rectangle with integer coordinates.
      This follows usual convention that _r.Right_ and _r.Bottom_ are not considered
      to be included in the rectangle. }
  function PtInRect(const pt: TPoint; r: TRect): boolean; overload;

  {** Creates a rectangle with integer coordinates with the specified _width_ and _height_ }
  function RectWithSize(left,top,width,height: integer): TRect;

{$if FPC_FULLVERSION<030001}
  {$define BGRA_DEFINE_TRECTHELPER}
type
  {* Helper for TRect structure providing additional functionality to manipulate rectangles }
  TRectHelper = record helper for TRect
  private
    {** Gets the height of the rectangle }
    function GetHeight: integer;
    {** Determines if the rectangle is empty (of zero size) }
    function GetIsEmpty: boolean;
    {** Gets the width of the rectangle }
    function GetWidth: integer;
    {** Sets the height of the rectangle }
    procedure SetHeight(AValue: integer);
    {** Sets the width of the rectangle }
    procedure SetWidth(AValue: integer);
  public
    {** Creates a rectangle with specified origin, width, and height }
    constructor Create(Origin: TPoint; AWidth, AHeight: Longint); overload;
    {** Creates a rectangle with specified left, top, right, and bottom coordinates }
    constructor Create(ALeft, ATop, ARight, ABottom: Longint); overload;
    {** Modifies the rectangle to be the intersection of itself and another rectangle }
    procedure Intersect(R: TRect);
    {** Returns the intersection of two given rectangles }
    class function Intersect(R1: TRect; R2: TRect): TRect; static;
    {** Checks if the rectangle intersects with another rectangle }
    function IntersectsWith(R: TRect): Boolean;
    {** Returns the smallest rectangle that contains both of the given rectangles }
    class function Union(R1, R2: TRect): TRect; static;
    {** Modifies the rectangle to be the union of itself and another rectangle }
    procedure Union(R: TRect);
    {** Offsets the rectangle by the given distances in the X and Y directions }
    procedure Offset(DX, DY: Longint);
    {** Expands or contracts the rectangle by the specified amounts in both directions }
    procedure Inflate(DX, DY: Longint);
    {** Checks if the rectangle contains a specified point }
    function Contains(const APoint: TPoint): boolean; overload;
    {** Checks if the rectangle completely contains another rectangle }
    function Contains(const ARect: TRect): boolean; overload;
    {** Property to get or set the width of the rectangle }
    property Width: integer read GetWidth write SetWidth;
    {** Property to get or set the height of the rectangle }
    property Height: integer read GetHeight write SetHeight;
    {** Property to check if the rectangle is empty }
    property IsEmpty: boolean read GetIsEmpty;
  end;

{ Check if two rectangles are equal (all coordinates are equal) }
operator=(const ARect1,ARect2: TRect): boolean;
{$endif}

type
  TSize = BGRAClasses.TSize;

{$if (FPC_FULLVERSION<030001) or defined(BGRABITMAP_USE_MSEGUI)}
  {$define BGRA_DEFINE_TSIZEHELPER}

  {* Helper to access a size }
  TSizeHelper = record helper for TSize
  private
    {** Gets the height of the size }
    function GetHeight: integer;
    {** Gets the width of the size }
    function GetWidth: integer;
  public
    {** Property to get the width }
    property Width: integer read GetWidth;
    {** Property to get the height }
    property Height: integer read GetHeight;
  end;
{$ENDIF}

type
  {* Pointer to a TRectF structure }
  PRectF = ^TRectF;

  {* Helper for TRectF structure providing additional functionality for rectangle manipulation }
  TRectFHelper = record helper for TRectF
    {** Returns the intersection of two given rectangles }
    class function Intersect(const R1: TRectF; const R2: TRectF): TRectF; overload; static;
    {** Returns the union of two given rectangles }
    class function Union(const R1: TRectF; const R2: TRectF): TRectF; overload; static;
    {** Returns the union of two given rectangles, with an option to ignore empty rectangles }
    class function Union(const R1: TRectF; const R2: TRectF; ADiscardEmpty: boolean): TRectF; overload; static;
    {** Returns the union of itself and another rectangle }
    function Union(const r: TRectF): TRectF; overload;
    {** Returns the union of itself and another rectangle, with an option to ignore empty rectangles }
    function Union(const r: TRectF; ADiscardEmpty: boolean): TRectF; overload;
    {** Expands the rectangle to include a specified point (empty points are ignored) }
    procedure Include(const APoint: TPointF);
    {** Checks if the rectangle contains a specified point, with an option to include or exclude the bottom-right boundary }
    function Contains(const APoint: TPointF; AIncludeBottomRight: boolean = false): boolean;
    {** Checks if the rectangle intersects with another rectangle }
    function IntersectsWith(const r: TRectF): boolean;
    {** Determines if the rectangle is empty (of zero size) }
    function IsEmpty: boolean;
  end;

type
  {* @abstract(Object representing an affine box.)

     Box is defined by three corner points (top-left, top-tight, bottom-left). }
  TAffineBox = object
  private
    {** Retrieves the affine box as an array of points (polygon representation) }
    function GetAsPolygon: ArrayOfTPointF;
    {** Gets the bottom-right corner of the affine box }
    function GetBottomRight: TPointF;
    {** Calculates the center point of the affine box }
    function GetCenter: TPointF;
    {** Calculates the height of the affine box }
    function GetHeight: single;
    {** Determines if the affine box is empty (one of the point being empty, not just of zero size) }
    function GetIsEmpty: boolean;
    {** Gets the bounding rectangle of the affine box as TRect }
    function GetRectBounds: TRect;
    {** Gets the bounding rectangle of the affine box as TRectF }
    function GetRectBoundsF: TRectF;
    {** Calculates the surface area of the affine box }
    function GetSurface: single;
    {** Calculates the width of the affine box }
    function GetWidth: single;
  public
    {** Top-left corner of the affine box }
    TopLeft,
    {** Top-right corner of the affine box }
    TopRight,
    {** Bottom-left corner of the affine box }
    BottomLeft: TPointF;

    {** Creates an empty affine box }
    class function EmptyBox: TAffineBox; static;
    {** Creates an affine box defined by three corner points }
    class function AffineBox(ATopLeft, ATopRight, ABottomLeft: TPointF): TAffineBox; overload; static;
    {** Creates an affine box from a TRectF structure }
    class function AffineBox(ARectF: TRectF): TAffineBox; overload; static;
    {** Offsets the affine box by specified X and Y amounts }
    procedure Offset(AOfsX, AOfsY: single); overload;
    {** Offsets the affine box by a specified point }
    procedure Offset(AOfs: TPointF); overload;
    {** Inflates the affine box along its axes by specified horizontal and vertical amounts }
    procedure Inflate(AHoriz, AVert: single);
    {** Checks if a point is contained within the affine box }
    function Contains(APoint: TPointF): boolean;

    {** Bounding rectangle as TRect }
    property RectBounds: TRect read GetRectBounds;
    {** Bounding rectangle as TRectF }
    property RectBoundsF: TRectF read GetRectBoundsF;
    {** Bottom-right corner of the affine box }
    property BottomRight: TPointF read GetBottomRight;
    {** Check if the affine box is empty (containing empty points, not just of zero size) }
    property IsEmpty: boolean read GetIsEmpty;
    {** Affine box as an array of points (polygon) }
    property AsPolygon: ArrayOfTPointF read GetAsPolygon;
    {** Width of the affine box along its axes }
    property Width: single read GetWidth;
    {** Height of the affine box along its axes }
    property Height: single read GetHeight;
    {** Surface area of the affine box }
    property Surface: single read GetSurface;
    {** Center point of the affine box }
    property Center: TPointF read GetCenter;
  end;

type
  {* Enumerates the modes of face culling in rendering. Face culling is a technique used to
     determine which faces of a polygon are visible and should be rendered }
  TFaceCulling = (
    {** No face culling is applied. All faces of the polygon are rendered }
    fcNone,
    {** Keeps only the faces that are clockwise (CW) from the viewer's perspective.
        Typically used to remove back faces when the convention is that CW is for front faces }
    fcKeepCW,
    {** Keeps only the faces that are counter-clockwise (CCW) from the viewer's perspective.
        Typically used to remove back faces when the convention is that CCW is for front faces }
    fcKeepCCW);

type
  {* A pen style can be dashed, dotted, etc. It is defined as a list of floating point number.
     The first number is the length of the first dash,
     the second number is the length of the first gap,
     the third number is the length of the second dash...
     It must have an even number of values. This is used as a complement
     to [[BGRABitmap Types imported from Graphics|TPenStyle]] }
  TBGRAPenStyle = array Of Single;

  {** Creates a pen style with the specified length for the dashes and the spaces }
  function BGRAPenStyle(dash1, space1: single; dash2: single=0; space2: single = 0; dash3: single=0; space3: single = 0; dash4 : single = 0; space4 : single = 0): TBGRAPenStyle;

type
  {* Different types of spline. A spline is a series of points that are used
     as control points to draw a curve. The first point and last point may
     or may not be the starting and ending point }
  TSplineStyle = (
    {** The curve is drawn inside the polygonal envelope without reaching the starting and ending points }
    ssInside,
    {** The curve is drawn inside the polygonal envelope and the starting and ending points are reached }
    ssInsideWithEnds,
    {** The curve crosses the polygonal envelope without reaching the starting and ending points }
    ssCrossing,
    {** The curve crosses the polygonal envelope and the starting and ending points are reached }
    ssCrossingWithEnds,
    {** The curve is outside the polygonal envelope (starting and ending points are reached) }
    ssOutside,
    {** The curve expands outside the polygonal envelope (starting and ending points are reached) }
    ssRoundOutside,
    {** The curve is outside the polygonal envelope and there is a tangeant at vertices (starting and ending points are reached) }
    ssVertexToSide,
    {** The curve is rounded using Bezier curves when the angle is less than or equal to 45° }
    ssEasyBezier);

type
  {* Pointer to an arc definition }
  PArcDef = ^TArcDef;
  {* Definition of an arc of an ellipse }
  TArcDef = record
    {** Center of the ellipse }
    center: TPointF;
    {** Horizontal and vertical of the ellipse before rotation }
    radius: TPointF;
    {** Rotation of the ellipse }
    xAngleRadCW: single;
    {** Start and end angle, in radian and clockwise. See angle convention in BGRAPath }
    startAngleRadCW, endAngleRadCW: single;
    {** Specifies if the arc goes anticlockwise }
    anticlockwise: boolean
  end;

  {** Creates a structure for an arc definition }
  function ArcDef(cx, cy, rx,ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single; anticlockwise: boolean) : TArcDef;

type
  {* Possible options for drawing an arc of an ellipse (used in BGRACanvas) }
  TArcOption = (
    {** Close the path by joining the ending and starting point together }
    aoClosePath,
    {** Draw a pie shape by joining the ending and starting point to the center of the ellipse }
    aoPie,
    {** Fills the shape }
    aoFillPath);
    {** Set of options for drawing an arc }
    TArcOptions = set of TArcOption;

  {* Enumerates different styles of arrows that can be used in graphic rendering.
     Each style represents a unique visual appearance for arrowheads or tails }
  TBGRAArrowStyle = (
    {** No arrow style. This option indicates that no arrowhead or tail is to be rendered }
    asNone,
    {** A normal arrow style, representing a standard arrowhead with an angle }
    asNormal,
    {** A cut arrow style, where the edge of the arrow are cut instead of using the pen end cap }
    asCut,
    {** A triangle arrow style, forming a simple triangular arrowhead }
    asTriangle,
    {** A hollow triangle arrow style, similar to _asTriangle_ but with an unfilled center }
    asHollowTriangle,
    {** A flipped arrow style, where the arrowhead points in the opposite direction to the normal style }
    asFlipped,
    {** A flipped cut arrow style, combining the aspects of _asFlipped_ and _asCut_ }
    asFlippedCut,
    {** A small tail, typically used to depict the starting point of an arrow }
    asTail,
    {** A tail repeat arrow style, depicting a series of repeated patterns typically used for the starting point of an arrow }
    asTailRepeat);

  {* Generic class configuring and rendering an arrow }
  TBGRACustomArrow = class
  protected
    {** Retrieves the X-offset for the end of the arrow }
    function GetEndOffsetX: single; virtual; abstract;
    {** Retrieves the repeat count for the end symbol }
    function GetEndRepeatCount: integer; virtual; abstract;
    {** Retrieves the size factor for the end of the arrow }
    function GetEndSizeFactor: TPointF; virtual; abstract;
    {** Determines if the end of the arrow is defined }
    function GetIsEndDefined: boolean; virtual; abstract;

    {** Determines if the start of the arrow is defined }
    function GetIsStartDefined: boolean; virtual; abstract;
    {** Retrieves the X-offset for the start of the arrow }
    function GetStartOffsetX: single; virtual; abstract;
    {** Retrieves the repeat count for the start symbol}
    function GetStartRepeatCount: integer; virtual; abstract;
    {** Retrieves the size factor for the start of the arrow }
    function GetStartSizeFactor: TPointF; virtual; abstract;

    {** Sets the X-offset for the end of the arrow }
    procedure SetEndOffsetX(AValue: single); virtual; abstract;
    {** Sets the repeat count for the end symbol }
    procedure SetEndRepeatCount(AValue: integer); virtual; abstract;
    {** Sets the size factor for the end of the arrow }
    procedure SetEndSizeFactor(AValue: TPointF); virtual; abstract;

    {** Sets the X-offset for the start of the arrow }
    procedure SetStartOffsetX(AValue: single); virtual; abstract;
    {** Sets the repeat count for the start symbol }
    procedure SetStartRepeatCount(AValue: integer); virtual; abstract;
    {** Sets the size factor for the start of the arrow }
    procedure SetStartSizeFactor(AValue: TPointF); virtual; abstract;

    {** Retrieves the line cap style for the classic arrow }
    function GetLineCap: TPenEndCap; virtual; abstract;
    {** Sets the line cap style for the classic arrow }
    procedure SetLineCap(AValue: TPenEndCap); virtual; abstract;
  public
    {** Computes the start of the arrow at a given position and direction }
    function ComputeStartAt(const APosition, ADirection: TPointF; const AWidth, ACurrentPos: single): ArrayOfTPointF; virtual; abstract;
    {** Computes the end of the arrow at a given position and direction }
    function ComputeEndAt(const APosition, ADirection: TPointF; const AWidth, ACurrentPos: single): ArrayOfTPointF; virtual; abstract;

    {** Sets the start of the arrow to no style }
    procedure StartAsNone; virtual; abstract;
    {** Sets the start of the arrow to a classic style }
    procedure StartAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1); virtual; abstract;
    {** Sets the start of the arrow to a triangle style }
    procedure StartAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5); virtual; abstract;
    {** Sets the start of the arrow to a tail style }
    procedure StartAsTail; virtual; abstract;

    {** Sets the end of the arrow to no style }
    procedure EndAsNone; virtual; abstract;
    {** Sets the end of the arrow to a classic style }
    procedure EndAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1); virtual; abstract;
    {** Sets the end of the arrow to a triangle style }
    procedure EndAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5); virtual; abstract;
    {** Sets the end of the arrow to a tail style }
    procedure EndAsTail; virtual; abstract;

    {** Check if the start of the arrow is defined }
    property IsStartDefined: boolean read GetIsStartDefined;
    {** Check if the end of the arrow is defined }
    property IsEndDefined: boolean read GetIsEndDefined;

    {** X-offset for the start of the arrow }
    property StartOffsetX: single read GetStartOffsetX write SetStartOffsetX;
    {** X-offset for the end of the arrow }
    property EndOffsetX: single read GetEndOffsetX write SetEndOffsetX;
    {** Line cap style }
    property LineCap: TPenEndCap read GetLineCap write SetLineCap;
    {** Size factor for the start of the arrow }
    property StartSize: TPointF read GetStartSizeFactor write SetStartSizeFactor;
    {** Size factor for the end of the arrow }
    property EndSize: TPointF read GetEndSizeFactor write SetEndSizeFactor;
    {** Repeat count for the start symbol }
    property StartRepeatCount: integer read GetStartRepeatCount write SetStartRepeatCount;
    {** Repeat count for the end symbol }
    property EndRepeatCount: integer read GetEndRepeatCount write SetEndRepeatCount;
  end;

  {* Generic class representing a pen stroker, used for drawing and styling lines with various attributes }
  TBGRACustomPenStroker = class
  protected
    {** Retrieves the arrow style used at both ends of strokes }
    function GetArrow: TBGRACustomArrow; virtual; abstract;
    {** Determines if the arrow is owned (managed) by the pen stroker }
    function GetArrowOwned: boolean; virtual; abstract;
    {** Gets the custom pen style }
    function GetCustomPenStyle: TBGRAPenStyle; virtual; abstract;
    {** Retrieves the style of line joins (e.g., bevel, miter) }
    function GetJoinStyle: TPenJoinStyle; virtual; abstract;
    {** Retrieves the line cap style (e.g., butt, round) }
    function GetLineCap: TPenEndCap; virtual; abstract;
    {** Gets the miter limit for line joins }
    function GetMiterLimit: single; virtual; abstract;
    {** Retrieves the standard pen style }
    function GetPenStyle: TPenStyle; virtual; abstract;
    {** Gets the stroke transformation matrix (applied only to stroke and not to path points) }
    function GetStrokeMatrix: TAffineMatrix; virtual; abstract;
    {** Sets the arrow style used at both ends of strokes }
    procedure SetArrow(AValue: TBGRACustomArrow); virtual; abstract;
    {** Sets whether the arrow is owned by the pen stroker }
    procedure SetArrowOwned(AValue: boolean); virtual; abstract;
    {** Sets the custom pen style }
    procedure SetCustomPenStyle(AValue: TBGRAPenStyle); virtual; abstract;
    {** Sets the style of line joins }
    procedure SetJoinStyle(AValue: TPenJoinStyle); virtual; abstract;
    {** Sets the line cap style }
    procedure SetLineCap(AValue: TPenEndCap); virtual; abstract;
    {** Sets the miter limit for line joins }
    procedure SetMiterLimit(AValue: single); virtual; abstract;
    {** Sets the standard pen style }
    procedure SetPenStyle(AValue: TPenStyle); virtual; abstract;
    {** Sets the stroke transformation matrix (applied only to stroke and not to path points) }
    procedure SetStrokeMatrix(const AValue: TAffineMatrix); virtual; abstract;

  public
    {** Computes a polyline with the given points and width, optionally closing the cap }
    function ComputePolyline(const APoints: array of TPointF; AWidth: single; AClosedCap: boolean = true): ArrayOfTPointF; overload; virtual; abstract;
    {** Computes a polyline with the given points, width, and pen color, optionally closing the cap }
    function ComputePolyline(const APoints: array of TPointF; AWidth: single; APenColor: TBGRAPixel; AClosedCap: boolean = true): ArrayOfTPointF; overload; virtual; abstract;
    {** Computes a polyline with automatic cycling of points and given width }
    function ComputePolylineAutoCycle(const APoints: array of TPointF; AWidth: single): ArrayOfTPointF; virtual; abstract;
    {** Computes a polygon with the given points and width }
    function ComputePolygon(const APoints: array of TPointF; AWidth: single): ArrayOfTPointF; virtual; abstract;

    {** Standard pen style }
    property Style: TPenStyle read GetPenStyle write SetPenStyle;
    {** Custom pen style (with given dash and gap lengths) }
    property CustomPenStyle: TBGRAPenStyle read GetCustomPenStyle write SetCustomPenStyle;
    {** Arrow style at both ends of strokes }
    property Arrow: TBGRACustomArrow read GetArrow write SetArrow;
    {** Is the arrow is owned by the pen stroker }
    property ArrowOwned: boolean read GetArrowOwned write SetArrowOwned;
    {** Stroke transformation matrix (applied only to stroke and not to path points) }
    property StrokeMatrix: TAffineMatrix read GetStrokeMatrix write SetStrokeMatrix;
    {** Line cap style }
    property LineCap: TPenEndCap read GetLineCap write SetLineCap;
    {** Join style of lines }
    property JoinStyle: TPenJoinStyle read GetJoinStyle write SetJoinStyle;
    {** Miter limit for line joins }
    property MiterLimit: single read GetMiterLimit write SetMiterLimit;
  end;

type
  {* Pointer to a TPoint3D structure }
  PPoint3D = ^TPoint3D;

  {* Point in 3D with single-precision floating point coordinates }
  TPoint3D = record
    x,y,z: single;
    {** Offsets the point by the given 3D vector }
    procedure Offset(const point3D: TPoint3D);
    {** Scale the point (as a vector from the origin) }
    procedure Scale(AScale: single);
  end;

  {----------------- Operators for TPoint3D ---------------}
  {** Creates a new structure with values (_x_, _y_, _z_) }
  function Point3D(x,y,z: single): TPoint3D;
  {** Checks if all components _x_, _y_ and _z_ are equal }
  operator = (const v1,v2: TPoint3D): boolean; inline;
  {** Adds components separately. It is like adding vectors }
  operator + (const v1,v2: TPoint3D): TPoint3D; inline;
  {** Subtract components separately. It is like subtracting vectors }
  operator - (const v1,v2: TPoint3D): TPoint3D; inline;
  {** Returns a point with opposite values for all components }
  operator - (const v: TPoint3D): TPoint3D; inline;
  {** Scalar product: multiplies components and returns the sum }
  operator * (const v1,v2: TPoint3D): single; inline;
  {** Multiplies components by _factor_. It scales the vector represented by (_x_, _y_, _z_) }
  operator * (const v1: TPoint3D; const factor: single): TPoint3D; inline;
  {** Multiplies components by _factor_. It scales the vector represented by (_x_, _y_, _z_) }
  operator * (const factor: single; const v1: TPoint3D): TPoint3D; inline;
  {** Computes the vectorial product _w_. It is perpendicular to both _u_ and _v_ }
  procedure VectProduct3D(u,v: TPoint3D; out w: TPoint3D);
  {** Normalize the vector, i.e. scale it so that its length be 1 }
  procedure Normalize3D(var v: TPoint3D); inline;
  {** Computes the length of the vector from the origin to the point }
  function VectLen3D(const v: TPoint3D): single;

type
  {* Defition of a line in the euclidian plane }
  TLineDef = record
    {** Some point in the line }
    origin: TPointF;
    {** Vector indicating the direction }
    dir: TPointF;
  end;

  {----------- Line and polygon functions -----------}
  {** Computes the intersection of two lines. If they are parallel, returns
      the middle of the segment between the two origins }
  function IntersectLine(line1, line2: TLineDef): TPointF; overload;
  {** Computes the intersection of two lines. If they are parallel, returns
      the middle of the segment between the two origins. The value _parallel_
      is set to indicate if the lines were parallel }
  function IntersectLine(line1, line2: TLineDef; out parallel: boolean): TPointF; overload;
  {** Checks if the polygon formed by the given points is convex. _IgnoreAlign_
      specifies that if the points are aligned, it should still be considered as convex }
  function IsConvex(const pts: array of TPointF; IgnoreAlign: boolean = true): boolean;
  {** Checks if the points follow a clockwise curve }
  function IsClockwise(const pts: array of TPointF): boolean;
  {** Checks if the curve is clockwise on the whole but accepting some counter-clockwise points }
  function IsMostlyClockwise(const pts: array of TPointF): boolean;
  {** Checks if the quad formed by the 4 given points intersects itself }
  function DoesQuadIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;
  {** Checks if two segment intersect }
  function DoesSegmentIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;

type
  TBGRACustomPathCursor = class;
  {* Callback function when rendering a stroke }
  TBGRAPathDrawProc = procedure(const APoints: array of TPointF; AClosed: boolean; AData: Pointer) of object;
  {* Callback function when filling a shape }
  TBGRAPathFillProc = procedure(const APoints: array of TPointF; AData: pointer) of object;

  {* @abstract(Interface for path functions.)

     A path is the ability to define a contour with _moveTo_, _lineTo_...

     Even if it is an interface, it must not implement reference counting. }
  IBGRAPath = interface
    {** Closes the current path with a line to the starting point }
    procedure closePath;
    {** Moves to a location, disconnected from previous points }
    procedure moveTo(constref pt: TPointF);
    {** Adds a line from the current point }
    procedure lineTo(constref pt: TPointF);
    {** Adds a polyline from the current point }
    procedure polylineTo(const pts: array of TPointF);
    {** Adds a quadratic Bézier curve from the current point }
    procedure quadraticCurveTo(constref cp,pt: TPointF);
    {** Adds a cubic Bézier curve from the current point }
    procedure bezierCurveTo(constref cp1,cp2,pt: TPointF);
    {** Adds an arc. If there is a current point, it is connected to the beginning of the arc }
    procedure arc(constref arcDef: TArcDef);
    {** Adds an opened spline. If there is a current point, it is connected to the beginning of the spline }
    procedure openedSpline(const pts: array of TPointF; style: TSplineStyle);
    {** Adds an closed spline. If there is a current point, it is connected to the beginning of the spline }
    procedure closedSpline(const pts: array of TPointF; style: TSplineStyle);
    {** Copy the content of this path to the specified destination }
    procedure copyTo(dest: IBGRAPath);
    {** Returns the content of the path as an array of points }
    function getPoints: ArrayOfTPointF; overload;
    {** Returns the content of the path as an array of points with the transformation specified by _AMatrix_ }
    function getPoints(AMatrix: TAffineMatrix): ArrayOfTPointF; overload;
    {** Calls a given draw procedure for each sub path with computed coordinates for rendering }
    procedure stroke(ADrawProc: TBGRAPathDrawProc; AData: pointer); overload;
    {** Calls a given draw procedure for each sub path with computed coordinates using given _AMatrix_ transformation }
    procedure stroke(ADrawProc: TBGRAPathDrawProc; const AMatrix: TAffineMatrix; AData: pointer); overload;
    {** Calls a given fill procedure for each sub path with computed coordinates for rendering }
    procedure fill(AFillProc: TBGRAPathFillProc; AData: pointer); overload;
    {** Calls a given fill procedure for each sub path with computed coordinates using given _AMatrix_ transformation }
    procedure fill(AFillProc: TBGRAPathFillProc; const AMatrix: TAffineMatrix; AData: pointer); overload;
    {** Returns a cursor to go through the path. The cursor must be freed by calling _Free_. }
    function getCursor: TBGRACustomPathCursor;
  end;

  {* Generic class representing a path, providing methods for creation and exploration }
  TBGRACustomPath = class(IBGRAPath)
    {** Constructor to create a custom path. Must be overridden in subclasses }
    constructor Create; virtual; abstract;

    {** Begins a new path }
    procedure beginPath; virtual; abstract;
    {** Closes the current path }
    procedure closePath; virtual; abstract;
    {** Moves the current point to a specified location, starting a new sub-path }
    procedure moveTo(constref pt: TPointF); virtual; abstract;
    {** Adds a line from the current point to a specified point }
    procedure lineTo(constref pt: TPointF); virtual; abstract;
    {** Adds a series of lines to the path based on an array of points }
    procedure polylineTo(const pts: array of TPointF); virtual; abstract;
    {** Adds a quadratic Bézier curve to the path }
    procedure quadraticCurveTo(constref cp, pt: TPointF); virtual; abstract;
    {** Adds a cubic Bézier curve to the path }
    procedure bezierCurveTo(constref cp1, cp2, pt: TPointF); virtual; abstract;
    {** Adds an arc to the path based on an arc definition }
    procedure arc(constref arcDef: TArcDef); virtual; abstract;
    {** Adds an open spline to the path based on a series of points and a spline style }
    procedure openedSpline(const pts: array of TPointF; style: TSplineStyle); virtual; abstract;
    {** Adds a closed spline to the path based on a series of points and a spline style }
    procedure closedSpline(const pts: array of TPointF; style: TSplineStyle); virtual; abstract;
    {** Copies the path to another path object }
    procedure copyTo(dest: IBGRAPath); virtual; abstract;

  protected
    {** Retrieves the points of the path }
    function getPoints: ArrayOfTPointF; overload; virtual; abstract;
    {** Retrieves the points of the path, transformed by a specified matrix }
    function getPoints(AMatrix: TAffineMatrix): ArrayOfTPointF; overload; virtual; abstract;
    {** Strokes the path with a specified drawing procedure }
    procedure stroke(ADrawProc: TBGRAPathDrawProc; AData: pointer); overload; virtual; abstract;
    {** Strokes the path with a specified drawing procedure and transformation matrix }
    procedure stroke(ADrawProc: TBGRAPathDrawProc; const AMatrix: TAffineMatrix; AData: pointer); overload; virtual; abstract;
    {** Fills the path with a specified filling procedure }
    procedure fill(AFillProc: TBGRAPathFillProc; AData: pointer); overload; virtual; abstract;
    {** Fills the path with a specified filling procedure and transformation matrix }
    procedure fill(AFillProc: TBGRAPathFillProc; const AMatrix: TAffineMatrix; AData: pointer); overload; virtual; abstract;
    {** Retrieves the length of the path }
    function getLength: single; virtual; abstract;
    {** Retrieves a cursor for navigating the path }
    function getCursor: TBGRACustomPathCursor; virtual; abstract;
  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
  end;

  {* Type for specifying any derived class of TBGRACustomPath }
  TBGRAPathAny = class of TBGRACustomPath;

  {* Class that contains a cursor to browse an existing path }
  TBGRACustomPathCursor = class
  protected
    {** Retrieves the current position from the start }
    function GetArcPos: single; virtual; abstract;
    {** Returns the current coordinate in the path }
    function GetCurrentCoord: TPointF; virtual; abstract;
    {** Returns the tangent vector to the current position }
    function GetCurrentTangent: TPointF; virtual; abstract;
    {** Retrieves if the cursor loops when there is a closed shape }
    function GetLoopClosedShapes: boolean; virtual; abstract;
    {** Retrieves if the cursor loops at the end of the whole path }
    function GetLoopPath: boolean; virtual; abstract;
    {** Retreive the full arc length of the path }
    function GetPathLength: single; virtual; abstract;
    {** Compute the bounds of the path }
    function GetBounds: TRectF; virtual; abstract;
    {** Starting coordinate of the path }
    function GetStartCoordinate: TPointF; virtual; abstract;
    {** Sets the current position from the start }
    procedure SetArcPos(AValue: single); virtual; abstract;
    {** Sets if the cursor loops when there is a closed shape }
    procedure SetLoopClosedShapes(AValue: boolean); virtual; abstract;
    {** Sets if the cursor loops at the end of the whole path }
    procedure SetLoopPath(AValue: boolean); virtual; abstract;
  public
    {** Go forward in the path, increasing the value of _Position_. If _ADistance_ is negative, then
        it goes backward instead. _ACanJump_ specifies if the cursor can jump from one shape to another
        without a line or an arc. Otherwise, the cursor is stuck, and the return value is less than
        the value _ADistance_ provided.  If all the way has been travelled, the
        return value is equal to _ADistance_ }
    function MoveForward(ADistance: single; ACanJump: boolean = true): single; virtual; abstract;
    {** Go backward, decreasing the value of _Position_. If _ADistance_ is negative, then it goes
        forward instead. _ACanJump_ specifies if the cursor can jump from one shape to another
        without a line or an arc. Otherwise, the cursor is stuck, and the return value is less than
        the value _ADistance_ provided. If all the way has been travelled, the
        return value is equal to _ADistance_ }
    function MoveBackward(ADistance: single; ACanJump: boolean = true): single; virtual; abstract;
    {** Returns the current coordinate in the path }
    property CurrentCoordinate: TPointF read GetCurrentCoord;
    {** Returns the tangent vector. It is a vector of length one that is parallel to the curve
        at the current point. A normal vector is easily deduced as PointF(y,-x) }
    property CurrentTangent: TPointF read GetCurrentTangent;
    {** Current position in the path, as a distance along the arc from the starting point of the path }
    property Position: single read GetArcPos write SetArcPos;
    {** Full arc length of the path }
    property PathLength: single read GetPathLength;
    {** Starting coordinate of the path }
    property StartCoordinate: TPointF read GetStartCoordinate;
    {** Specifies if the cursor loops when there is a closed shape }
    property LoopClosedShapes: boolean read GetLoopClosedShapes write SetLoopClosedShapes;
    {** Specifies if the cursor loops at the end of the path. Note that if it needs to jump to go
        to the beginning, it will be only possible if the parameter _ACanJump_ is set to True
        when moving along the path }
    property LoopPath: boolean read GetLoopPath write SetLoopPath;
  end;

var
  {* Factory provided to create a TBGRAPath object }
  BGRAPathFactory: TBGRAPathAny;

{$DEFINE INCLUDE_INTERFACE}
{$I bezier.inc}

type
  {* Possible options for a round rectangle }
  TRoundRectangleOption = (
    {** specify that a corner is a square (not rounded) }
    rrTopLeftSquare,rrTopRightSquare,rrBottomRightSquare,rrBottomLeftSquare,
    {** specify that a corner is a bevel (cut) }
    rrTopLeftBevel,rrTopRightBevel,rrBottomRightBevel,rrBottomLeftBevel,
    {** default option, does nothing particular }
    rrDefault);
    {** A set of options for a round rectangle }
    TRoundRectangleOptions = set of TRoundRectangleOption;
  {* Order of polygons when rendered using TBGRAMultiShapeFiller
     (in unit BGRAPolygon) }
  TPolygonOrder = (
    {** No order, colors are mixed together }
    poNone,
    {** First polygon is on top }
    poFirstOnTop,
    {** Last polygon is on top }
    poLastOnTop);

  PIntersectionInfo = ^TIntersectionInfo;
  {* Contains an intersection between an horizontal line and any shape. It
     is used when filling shapes }
  TIntersectionInfo = class
    {** Horizontal position of the intersection }
    interX: single;
    {** Winding count }
    winding: integer;
    {** Number identifying the current segment where the intersection was found }
    numSegment: integer;
    {** Sets the position and other information about the intersection }
    procedure SetValues(AInterX: Single; AWinding, ANumSegment: integer);
  end;
  {** An array of intersections between an horizontal line and any shape }
  ArrayOfTIntersectionInfo = array of TIntersectionInfo;

  {* Abstract class defining any shape that can be filled }
  TBGRACustomFillInfo = class
    public
      {** Returns true if one segment number can represent a curve and
          thus cannot be considered exactly straight }
      function SegmentsCurved: boolean; virtual; abstract;

      {** Returns integer bounds for the shape }
      function GetBounds: TRect; virtual; abstract;

      {** Check if the point is inside the shape }
      function IsPointInside(x,y: single; windingMode: boolean): boolean; virtual; abstract;

      {** Create an array that will contain computed intersections.
          To augment that array, use CreateIntersectionInfo for new items }
      function CreateIntersectionArray: ArrayOfTIntersectionInfo; virtual; abstract;
      {** Create a structure to define one single intersection }
      function CreateIntersectionInfo: TIntersectionInfo; virtual; abstract;
      {** Free an array of intersections }
      procedure FreeIntersectionArray(var inter: ArrayOfTIntersectionInfo); virtual; abstract;

      {** Fill an array _inter_ with actual intersections with the shape at the y coordinate _cury_.
          _nbInter_ receives the number of computed intersections. _windingMode_ specifies if
          the winding method must be used to determine what is inside of the shape }
      procedure ComputeAndSort(cury: single; var inter: ArrayOfTIntersectionInfo; out nbInter: integer; windingMode: boolean); virtual; abstract;

      {** Returns the index of the current slice (horizontal stripe) }
      function GetSliceIndex: integer; virtual; abstract;
  end;

type
  {* Shape of a gradient }
  TGradientType = (
    {** The color changes along a certain vector and does not change along its perpendicular direction }
    gtLinear,
    {** The color changes like in _gtLinear_ however it is symmetrical to a specified direction }
    gtReflected,
    {** The color changes along a diamond shape }
    gtDiamond,
    {** The color changes in a radial way from a given center }
    gtRadial,
    {** The color changes according to the angle relative to a given center }
    gtAngular);

  {** List of string to represent gradient types }
  const GradientTypeStr : array[TGradientType] of string
  = ('Linear','Reflected','Diamond','Radial','Angular');

  {** Returns the gradient type represented by the given string }
  function StrToGradientType(str: string): TGradientType;

type
  {* Function type to get the the color at a given integer position }
  TBGRAGradientGetColorAtFunc = function(position: integer): TBGRAPixel of object;
  {* Function type to get the the color at a given single-precision floating type position }
  TBGRAGradientGetColorAtFloatFunc = function(position: single): TBGRAPixel of object;
  {* Function type to get the the expanded color at a given integer position }
  TBGRAGradientGetExpandedColorAtFunc = function(position: integer): TExpandedPixel of object;
  {* Function type to get the the expanded color at a given single-precision floating type position }
  TBGRAGradientGetExpandedColorAtFloatFunc = function(position: single): TExpandedPixel of object;

  {* Defines a gradient of color, not specifying its shape but only the
     series of colors }
  TBGRACustomGradient = class
  public
    {** Returns the color at a given _position_. The reference range is
        from 0 to 65535, however values beyond are possible as well }
    function GetColorAt(position: integer): TBGRAPixel; virtual; abstract;
    {** Returns the expanded color at a given _position_. The reference range is
        from 0 to 65535, however values beyond are possible as well }
    function GetExpandedColorAt(position: integer): TExpandedPixel; virtual;
    {** Returns the color at a given _position_. The reference range is
        from 0 to 1, however values beyond are possible as well }
    function GetColorAtF(position: single): TBGRAPixel; virtual;
    {** Returns the expanded color at a given _position_. The reference range is
        from 0 to 1, however values beyond are possible as well }
    function GetExpandedColorAtF(position: single): TExpandedPixel; virtual;
    {** Returns the average color of the gradient }
    function GetAverageColor: TBGRAPixel; virtual; abstract;
    {** Returns the average expanded color of the gradient }
    function GetAverageExpandedColor: TExpandedPixel; virtual;
    {** Returns whether the gradient has only one color }
    function GetMonochrome: boolean; virtual; abstract;
    {** If the gradient contains only one color and thus is not really a gradient }
    property Monochrome: boolean read GetMonochrome;
  end;

{$ENDIF}

////////////////////////////////////////////////////////////////////////////////

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}

{$IFDEF BGRA_DEFINE_TRECTHELPER}
{ TRectHelper }

function TRectHelper.GetHeight: integer;
begin
  result := Bottom-Top;
end;

function TRectHelper.GetIsEmpty: boolean;
begin
  result := (Width = 0) and (Height = 0)
end;

function TRectHelper.GetWidth: integer;
begin
  result := Right-Left;
end;

procedure TRectHelper.SetHeight(AValue: integer);
begin
  Bottom := Top+AValue;
end;

procedure TRectHelper.SetWidth(AValue: integer);
begin
  Right := Left+AValue;
end;

constructor TRectHelper.Create(Origin: TPoint; AWidth, AHeight: Longint);
begin
  self.Left := Origin.X;
  self.Top := Origin.Y;
  self.Right := Origin.X+AWidth;
  self.Bottom := Origin.Y+AHeight;
end;

constructor TRectHelper.Create(ALeft, ATop, ARight, ABottom: Longint);
begin
  self.Left := ALeft;
  self.Top := ATop;
  self.Right := ARight;
  self.Bottom := ABottom;
end;

procedure TRectHelper.Intersect(R: TRect);
begin
  self := TRect.Intersect(self, R);
end;

class function TRectHelper.Intersect(R1: TRect; R2: TRect): TRect;
begin
  if R1.Left >= R2.Left then result.Left := R1.Left else result.Left := R2.Left;
  if R1.Top >= R2.Top then result.Top := R1.Top else result.Top := R2.Top;
  if R1.Right <= R2.Right then result.Right := R1.Right else result.Right := R2.Right;
  if R1.Bottom <= R2.Bottom then result.Bottom := R1.Bottom else result.Bottom := R2.Bottom;
  if result.IsEmpty then fillchar(result, sizeof(result), 0);
end;

function TRectHelper.IntersectsWith(R: TRect): Boolean;
begin
  Result := (Left < R.Right) and (R.Left < Right) and (Top < R.Bottom) and (R.Top < Bottom);
end;

class function TRectHelper.Union(R1, R2: TRect): TRect;
begin
  if R1.Left <= R2.Left then result.Left := R1.Left else result.Left := R2.Left;
  if R1.Top <= R2.Top then result.Top := R1.Top else result.Top := R2.Top;
  if R1.Right >= R2.Right then result.Right := R1.Right else result.Right := R2.Right;
  if R1.Bottom >= R2.Bottom then result.Bottom := R1.Bottom else result.Bottom := R2.Bottom;
  if result.IsEmpty then fillchar(result, sizeof(result), 0);
end;

procedure TRectHelper.Union(R: TRect);
begin
  self := TRect.Union(self, R);
end;

procedure TRectHelper.Offset(DX, DY: Longint);
begin
  Inc(Left, DX);
  Inc(Top, DY);
  Inc(Right, DX);
  Inc(Bottom, DY);
end;

procedure TRectHelper.Inflate(DX, DY: Longint);
begin
  Dec(Left, DX);
  Dec(Top, DY);
  Inc(Right, DX);
  Inc(Bottom, DY);
end;

function TRectHelper.Contains(const APoint: TPoint): boolean;
begin
  result := (APoint.X >= Left) and (APoint.X < Right) and
    (APoint.Y >= Top) and (APoint.Y < Bottom);
end;

function TRectHelper.Contains(const ARect: TRect): boolean;
begin
  Result := (Left <= ARect.Left) and (ARect.Right <= Right) and (Top <= ARect.Top) and (ARect.Bottom <= Bottom);
end;

operator =(const ARect1, ARect2: TRect): boolean;
begin
  result:= (ARect1.Left = ARect2.Left) and (ARect1.Top = ARect2.Top) and
           (ARect1.Right = ARect2.Right) and (ARect1.Bottom = ARect2.Bottom);
end;
{$ENDIF}

{$ifdef BGRA_DEFINE_TSIZEHELPER}
{ TSizeHelper }

function TSizeHelper.GetHeight: integer;
begin
  result := cy;
end;

function TSizeHelper.GetWidth: integer;
begin
  result := cx;
end;
{$ENDIF}

function IsEmptyPoint(const APoint: TPoint): boolean;
begin
  result := (APoint.x = -2147483648) or (APoint.y = -2147483648);
end;

procedure TPointFHelper.Offset(const apt: TPointF);
begin
  if isEmptyPointF(self) then exit;
  IncF(self.x, apt.x);
  IncF(self.y, apt.y);
end;

procedure TPointFHelper.Offset(const apt: TPoint);
begin
  if isEmptyPointF(self) then exit;
  IncF(self.x, apt.x);
  IncF(self.y, apt.y);
end;

procedure TPointFHelper.Offset(dx, dy: longint);
begin
  if isEmptyPointF(self) then exit;
  IncF(self.x, dx);
  IncF(self.y, dy);
end;

procedure TPointFHelper.Offset(dx, dy: single);
begin
  if isEmptyPointF(self) then exit;
  IncF(self.x, dx);
  IncF(self.y, dy);
end;

procedure TPointFHelper.Scale(AScale: single);
begin
  if not isEmptyPointF(self) then
  begin
    self.x := self.x * AScale;
    self.y := self.y * AScale;
  end;
end;

procedure TPointFHelper.Normalize;
var
  len: Single;
begin
  len := Length;
  if len > 0 then self := self*(1/len);
end;

function TPointFHelper.Ceiling: TPoint;
begin
  if isEmptyPointF(self) then
    result := EmptyPoint
  else
  begin
    result.x:=ceil(x);
    result.y:=ceil(y);
  end;
end;

function TPointFHelper.Truncate: TPoint;
begin
  if isEmptyPointF(self) then
    result := EmptyPoint
  else
  begin
    result.x:=trunc(x);
    result.y:=trunc(y);
  end;
end;

function TPointFHelper.Floor: TPoint;
begin
  if isEmptyPointF(self) then
    result := EmptyPoint
  else
  begin
    result.x:=Math.floor(x);
    result.y:=Math.floor(y);
  end;
end;

function TPointFHelper.Round: TPoint;
begin
  if isEmptyPointF(self) then
    result := EmptyPoint
  else
  begin
    result.x:=System.round(x);
    result.y:=System.round(y);
  end;
end;

function TPointFHelper.Length: Single;
begin
  result:= VectLen(self);
end;

function TPointFHelper.IsEmpty: boolean;
begin
  result := isEmptyPointF(self);
end;

class function TRectFHelper.Intersect(const R1: TRectF; const R2: TRectF): TRectF;
begin
  result.left:=max(R1.left,R2.left);
  result.top:=max(R1.top,R2.top);
  result.right:=min(R1.right,R2.right);
  result.bottom:=min(R1.bottom,R2.bottom);
  if (result.left >= result.right) or (result.top >= result.bottom) then
    result := EmptyRectF;
end;

class function TRectFHelper.Union(const R1: TRectF; const R2: TRectF): TRectF;
begin
  result.left:=min(R1.left,R2.left);
  result.top:=min(R1.top,R2.top);
  result.right:=max(R1.right,R2.right);
  result.bottom:=max(R1.bottom,R2.bottom);
end;

class function TRectFHelper.Union(const R1: TRectF; const R2: TRectF; ADiscardEmpty: boolean): TRectF;
begin
  if ADiscardEmpty and IsEmptyRectF(R1) then result:= R2 else
  if ADiscardEmpty and IsEmptyRectF(R2) then result:= R1 else
    result := Union(R1,R2);
end;

function TRectFHelper.Union(const r: TRectF): TRectF;
begin
  result := TRectF.Union(self, r);
end;

function TRectFHelper.Union(const r: TRectF; ADiscardEmpty: boolean): TRectF;
begin
  result := TRectF.Union(self, r, ADiscardEmpty);
end;

procedure TRectFHelper.Include(const APoint: TPointF);
begin
  if APoint.x <> EmptySingle then
  begin
    if APoint.x < Left then Left := APoint.x else
    if APoint.x > Right then Right := APoint.x;
  end;
  if APoint.y <> EmptySingle then
  begin
    if APoint.y < Top then Top := APoint.y else
    if APoint.y > Bottom then Bottom := APoint.y;
  end;
end;

function TRectFHelper.Contains(const APoint: TPointF;
  AIncludeBottomRight: boolean): boolean;
begin
  if isEmptyPointF(APoint) then result := false else
  if (APoint.x < Left) or (APoint.y < Top) then result := false else
  if AIncludeBottomRight and ((APoint.x > Right) or (APoint.y > Bottom)) then result := false else
  if not AIncludeBottomRight and ((APoint.x >= Right) or (APoint.y >= Bottom)) then result := false
  else result := true;
end;

function TRectFHelper.IntersectsWith(const r: TRectF): boolean;
begin
  result:= not TRectF.Intersect(self, r).IsEmpty;
end;

function TRectFHelper.IsEmpty: boolean;
begin
  result:= IsEmptyRectF(self);
end;

{ TAffineBox }

function TAffineBox.GetAsPolygon: ArrayOfTPointF;
begin
  result := PointsF([TopLeft,TopRight,BottomRight,BottomLeft]);
end;

function TAffineBox.GetBottomRight: TPointF;
begin
  if IsEmpty then
    result := EmptyPointF
  else
    result := TopRight + (BottomLeft-TopLeft);
end;

function TAffineBox.GetCenter: TPointF;
begin
  result := (TopLeft + BottomRight)*0.5;
end;

function TAffineBox.GetHeight: single;
begin
  if isEmptyPointF(TopLeft) or isEmptyPointF(BottomLeft) then
    result := 0
  else
    result := VectLen(BottomLeft-TopLeft);
end;

function TAffineBox.GetIsEmpty: boolean;
begin
  result := isEmptyPointF(TopRight) or isEmptyPointF(BottomLeft) or isEmptyPointF(TopLeft);
end;

function TAffineBox.GetRectBounds: TRect;
begin
  with GetRectBoundsF do
    result := Rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
end;

function TAffineBox.GetRectBoundsF: TRectF;
var
  x1,y1,x2,y2: single;
begin
  x1 := TopLeft.x; x2 := x1;
  y1 := TopLeft.y; y2 := y1;
  if TopRight.x > x2 then x2 := TopRight.x;
  if TopRight.x < x1 then x1 := TopRight.x;
  if TopRight.y > y2 then y2 := TopRight.y;
  if TopRight.y < y1 then y1 := TopRight.y;
  if BottomLeft.x > x2 then x2 := BottomLeft.x;
  if BottomLeft.x < x1 then x1 := BottomLeft.x;
  if BottomLeft.y > y2 then y2 := BottomLeft.y;
  if BottomLeft.y < y1 then y1 := BottomLeft.y;
  if BottomRight.x > x2 then x2 := BottomRight.x;
  if BottomRight.x < x1 then x1 := BottomRight.x;
  if BottomRight.y > y2 then y2 := BottomRight.y;
  if BottomRight.y < y1 then y1 := BottomRight.y;
  result := RectF(x1,y1,x2,y2);
end;

function TAffineBox.GetSurface: single;
var
  u, v: TPointF;
  lenU, lenH: Single;
begin
  u := TopRight-TopLeft;
  lenU := VectLen(u);
  if lenU = 0 then exit(0);
  u.Scale(1/lenU);
  v := BottomLeft-TopLeft;
  lenH := PointF(-u.y,u.x)**v;
  result := abs(lenU*lenH);
end;

function TAffineBox.GetWidth: single;
begin
  if isEmptyPointF(TopLeft) or isEmptyPointF(TopRight) then
    result := 0
  else
    result := VectLen(TopRight-TopLeft);
end;

class function TAffineBox.EmptyBox: TAffineBox;
begin
  result.TopLeft := EmptyPointF;
  result.TopRight := EmptyPointF;
  result.BottomLeft := EmptyPointF;
end;

class function TAffineBox.AffineBox(ATopLeft, ATopRight, ABottomLeft: TPointF): TAffineBox;
begin
  if isEmptyPointF(ATopLeft) or isEmptyPointF(ATopRight) or isEmptyPointF(ABottomLeft) then
     raise exception.Create('Unexpected empty point');
  result.TopLeft := ATopLeft;
  result.TopRight := ATopRight;
  result.BottomLeft := ABottomLeft;
end;

class function TAffineBox.AffineBox(ARectF: TRectF): TAffineBox;
begin
  if isEmptyPointF(ARectF.TopLeft) or isEmptyPointF(ARectF.BottomRight) then
     raise exception.Create('Unexpected empty point');
  result.TopLeft := ARectF.TopLeft;
  result.TopRight := PointF(ARectF.Right, ARectF.Top);
  result.BottomLeft := PointF(ARectF.Left, ARectF.Bottom);
end;

procedure TAffineBox.Offset(AOfsX, AOfsY: single);
begin
  TopLeft.Offset(AOfsX,AOfsY);
  TopRight.Offset(AOfsX,AOfsY);
  BottomLeft.Offset(AOfsX,AOfsY);
end;

procedure TAffineBox.Offset(AOfs: TPointF);
begin
  Offset(AOfs.X,AOfs.Y);
end;

procedure TAffineBox.Inflate(AHoriz, AVert: single);
var
  u, v, ofs_horiz, ofs_vert: TPointF;
  lenU, lenV: Single;
begin
  u := TopRight-TopLeft;
  v := BottomLeft-TopLeft;
  lenU := VectLen(u);
  if lenU > 0 then u := u*(1/lenU);
  lenV := VectLen(v);
  if lenV > 0 then v := v*(1/lenV);
  ofs_horiz := u*AHoriz;
  ofs_vert := v*AVert;
  TopLeft := TopLeft - ofs_horiz - ofs_vert;
  TopRight := TopRight + ofs_horiz - ofs_vert;
  BottomLeft := BottomLeft - ofs_horiz + ofs_vert;
end;

function TAffineBox.Contains(APoint: TPointF): boolean;
var
  u,v,perpU,perpV: TPointF;
  posV1, posV2, posU1, posU2: single;
begin
  if IsEmpty then exit(false);

  u := TopRight-TopLeft;
  perpU := PointF(-u.y,u.x);
  v := BottomLeft-TopLeft;
  perpV := PointF(v.y,-v.x);

  //reverse normal if not in the same direction as other side
  if perpU**v < 0 then
  begin
    perpU := -perpU;
    perpV := -perpV;
  end;

  //determine position along normals
  posU1 := (APoint-TopLeft)**perpU;
  posU2 := (APoint-BottomLeft)**perpU;
  posV1 := (APoint-TopLeft)**perpV;
  posV2 := (APoint-TopRight)**perpV;

  result := (posU1 >= 0) and (posU2 < 0) and (posV1 >= 0) and (posV2 < 0);
end;

function StrToGradientType(str: string): TGradientType;
var gt: TGradientType;
begin
  result := gtLinear;
  str := LowerCase(str);
  for gt := low(TGradientType) to high(TGradientType) do
    if str = LowerCase(GradientTypeStr[gt]) then
    begin
      result := gt;
      exit;
    end;
end;

{ TBGRACustomGradient }

function TBGRACustomGradient.GetExpandedColorAt(position: integer
  ): TExpandedPixel;
begin
  result := GammaExpansion(GetColorAt(position));
end;

function TBGRACustomGradient.GetColorAtF(position: single): TBGRAPixel;
begin
  if position = EmptySingle then exit(BGRAPixelTransparent);
  position := position * 65536;
  if position < low(integer) then
    result := GetColorAt(low(Integer))
  else if position > high(integer) then
    result := GetColorAt(high(Integer))
  else
    result := GetColorAt(round(position));
end;

function TBGRACustomGradient.GetExpandedColorAtF(position: single): TExpandedPixel;
begin
  if position = EmptySingle then exit(BGRAPixelTransparent);
  position := position * 65536;
  if position < low(integer) then
    result := GetExpandedColorAt(low(Integer))
  else if position > high(integer) then
    result := GetExpandedColorAt(high(Integer))
  else
    result := GetExpandedColorAt(round(position));
end;

function TBGRACustomGradient.GetAverageExpandedColor: TExpandedPixel;
begin
  result := GammaExpansion(GetAverageColor);
end;

{ TIntersectionInfo }

procedure TIntersectionInfo.SetValues(AInterX: Single; AWinding,
  ANumSegment: integer);
begin
  interX := AInterX;
  winding := AWinding;
  numSegment := ANumSegment;
end;

{********************** TRect functions **************************}

function PtInRect(const pt: TPoint; r: TRect): boolean;
var
  temp: integer;
begin
  if r.right < r.left then
  begin
    temp    := r.left;
    r.left  := r.right;
    r.Right := temp;
  end;
  if r.bottom < r.top then
  begin
    temp     := r.top;
    r.top    := r.bottom;
    r.bottom := temp;
  end;
  Result := (pt.X >= r.left) and (pt.Y >= r.top) and (pt.X < r.right) and
    (pt.y < r.bottom);
end;

function RectWithSize(left, top, width, height: integer): TRect;
begin
  result.left := left;
  result.top := top;
  result.right := left+width;
  result.bottom := top+height;
end;

{ Make a pen style. Need an even number of values. See TBGRAPenStyle }
function BGRAPenStyle(dash1, space1: single; dash2: single; space2: single;
  dash3: single; space3: single; dash4: single; space4: single): TBGRAPenStyle;
var
  i: Integer;
begin
  if dash4 <> 0 then
  begin
    setlength(result,8);
    result[6] := dash4;
    result[7] := space4;
    result[4] := dash3;
    result[5] := space3;
    result[2] := dash2;
    result[3] := space2;
  end else
  if dash3 <> 0 then
  begin
    setlength(result,6);
    result[4] := dash3;
    result[5] := space3;
    result[2] := dash2;
    result[3] := space2;
  end else
  if dash2 <> 0 then
  begin
    setlength(result,4);
    result[2] := dash2;
    result[3] := space2;
  end else
  begin
    setlength(result,2);
  end;
  result[0] := dash1;
  result[1] := space1;
  for i := 0 to high(result) do
    if result[i]=0 then
      raise exception.Create('Zero is not a valid value');
end;

{ TBGRACustomPath }

function TBGRACustomPath.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := longint(E_NOINTERFACE);
end;

{ There is no automatic reference counting, but it is compulsory to define these functions }
function TBGRACustomPath._AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function TBGRACustomPath._Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function ArcDef(cx, cy, rx, ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single;
  anticlockwise: boolean): TArcDef;
begin
  result.center := PointF(cx,cy);
  result.radius := PointF(rx,ry);
  result.xAngleRadCW:= xAngleRadCW;
  result.startAngleRadCW := startAngleRadCW;
  result.endAngleRadCW:= endAngleRadCW;
  result.anticlockwise:= anticlockwise;
end;

{----------------- Operators for TPoint3D ---------------}
operator = (const v1, v2: TPoint3D): boolean; inline;
begin
  result := (v1.x=v2.x) and (v1.y=v2.y) and (v1.z=v2.z);
end;

operator * (const v1,v2: TPoint3D): single; inline;
begin
  result := v1.x*v2.x + v1.y*v2.y + v1.z*v2.z;
end;

operator * (const v1: TPoint3D; const factor: single): TPoint3D; inline;
begin
  result.x := v1.x*factor;
  result.y := v1.y*factor;
  result.z := v1.z*factor;
end;

operator - (const v1,v2: TPoint3D): TPoint3D; inline;
begin
  result.x := v1.x-v2.x;
  result.y := v1.y-v2.y;
  result.z := v1.z-v2.z;
end;

operator -(const v: TPoint3D): TPoint3D; inline;
begin
  result.x := -v.x;
  result.y := -v.y;
  result.z := -v.z;
end;

operator + (const v1,v2: TPoint3D): TPoint3D; inline;
begin
  result.x := v1.x+v2.x;
  result.y := v1.y+v2.y;
  result.z := v1.z+v2.z;
end;

operator*(const factor: single; const v1: TPoint3D): TPoint3D;
begin
  result.x := v1.x*factor;
  result.y := v1.y*factor;
  result.z := v1.z*factor;
end;

{ TPoint3D }

procedure TPoint3D.Offset(const point3D: TPoint3D);
begin
  IncF(self.x, point3d.x);
  IncF(self.y, point3d.y);
  IncF(self.z, point3d.z);
end;

procedure TPoint3D.Scale(AScale: single);
begin
  self.x := self.x * AScale;
  self.y := self.y * AScale;
  self.z := self.z * AScale;
end;

function Point3D(x, y, z: single): TPoint3D;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

procedure Normalize3D(var v: TPoint3D); inline;
var len: double;
begin
  len := v*v;
  if len = 0 then exit;
  len := sqrt(len);
  v.x := v.x / len;
  v.y := v.y / len;
  v.z := v.z / len;
end;

function VectLen3D(const v: TPoint3D): single;
begin
  result := sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
end;

procedure VectProduct3D(u,v: TPoint3D; out w: TPoint3D);
begin
  w.x := u.y*v.z-u.z*v.y;
  w.y := u.z*v.x-u.x*v.z;
  w.z := u.x*v.Y-u.y*v.x;
end;

{----------------- Operators for TPointF --------------------}
operator =(const pt1, pt2: TPointF): boolean;
begin
  result := (pt1.x = pt2.x) and (pt1.y = pt2.y);
end;

operator -(const pt1, pt2: TPointF): TPointF;
begin
  result.x := pt1.x-pt2.x;
  result.y := pt1.y-pt2.y;
end;

operator -(const pt2: TPointF): TPointF;
begin
  result.x := -pt2.x;
  result.y := -pt2.y;
end;

operator +(const pt1, pt2: TPointF): TPointF;
begin
  result.x := pt1.x+pt2.x;
  result.y := pt1.y+pt2.y;
end;

{$if FPC_FULLVERSION < 30203}
// keep alias for backward compatibility
operator *(const pt1, pt2: TPointF): single;
begin
  result := pt1.x*pt2.x + pt1.y*pt2.y;
end;

// new operator for scalar product
operator **(const pt1, pt2: TPointF): single;
begin
  result := pt1.x*pt2.x + pt1.y*pt2.y;
end;
{$endif}

operator *(const pt1: TPointF; factor: single): TPointF;
begin
  result.x := pt1.x*factor;
  result.y := pt1.y*factor;
end;

operator *(factor: single; const pt1: TPointF): TPointF;
begin
  result.x := pt1.x*factor;
  result.y := pt1.y*factor;
end;

function RectF(Left, Top, Right, Bottom: Single): TRectF;
begin
  result.Left:= Left;
  result.Top:= Top;
  result.Right:= Right;
  result.Bottom:= Bottom;
end;

function RectF(const ATopLeft, ABottomRight: TPointF): TRectF;
begin
  result.TopLeft:= ATopLeft;
  result.BottomRight:= ABottomRight;
end;

function RectF(const ARect: TRect): TRectF;
begin
  result.Left := ARect.Left;
  result.Top := ARect.Top;
  result.Right := ARect.Right;
  result.Bottom := ARect.Bottom;
end;

function RectWithSizeF(left, top, width, height: Single): TRectF;
begin
  result.Left:= Left;
  result.Top:= Top;
  result.Right:= left+width;
  result.Bottom:= top+height;
end;

function IsEmptyRectF(const ARect: TRectF): boolean;
begin
  result:= (ARect.Width = 0) and (ARect.Height = 0);
end;

function PointF(x, y: single): TPointF;
begin
  Result.x := x;
  Result.y := y;
end;

function PointF(pt: TPoint): TPointF;
begin
  if IsEmptyPoint(pt) then
    result:= EmptyPointF
  else
  begin
    Result.x := pt.x;
    Result.y := pt.y;
  end;
end;

function PointsF(const pts: array of TPointF): ArrayOfTPointF;
var
  i: Integer;
begin
  setlength(result, length(pts));
  for i := 0 to high(pts) do result[i] := pts[i];
end;

function ConcatPointsF(const APolylines: array of ArrayOfTPointF;
  AInsertEmptyPointInBetween: boolean): ArrayOfTPointF;
var
  i,pos,count:integer;
  j: Integer;
begin
  count := 0;
  for i := 0 to high(APolylines) do
    inc(count,length(APolylines[i]));
  if AInsertEmptyPointInBetween then inc(count, length(APolylines)-1);
  setlength(result,count);
  pos := 0;
  for i := 0 to high(APolylines) do
  begin
    if AInsertEmptyPointInBetween and (i > 0) then
    begin
      result[pos] := EmptyPointF;
      inc(pos);
    end;
    for j := 0 to high(APolylines[i]) do
    begin
      result[pos] := APolylines[i][j];
      inc(pos);
    end;
  end;
end;

function VectLen(const v: TPointF): single;
begin
  if isEmptyPointF(v) then
    result := EmptySingle
  else
    result := sqrt(sqr(v.x) + sqr(v.y));
end;

function VectDet(const v1, v2: TPointF): double;
begin
  result := v1.x*v2.y - v1.y*v2.x;
end;

function VectLen(const dx, dy: single): single;
begin
  result := sqrt(dx*dx+dy*dy);
end;

function PolylineLen(const pts: array of TPointF; AClosed: boolean): single;
var
  i: Int32or64;
begin
  result := 0;
  for i := 0 to high(pts)-1 do
    IncF(result, VectLen(pts[i+1] - pts[i]) );
  if AClosed then
    incF(result, VectLen(pts[0] - pts[high(pts)]) );
end;

{ Check if a PointF structure is empty or should be treated as a list separator }
function isEmptyPointF(const pt: TPointF): boolean;
begin
  Result := (pt.x = EmptySingle) and (pt.y = EmptySingle);
end;

{----------- Line and polygon functions -----------}
{$PUSH}{$OPTIMIZATION OFF}
function IntersectLine(line1, line2: TLineDef): TPointF;
var parallel: boolean;
begin
  result := IntersectLine(line1,line2,parallel);
end;
{$POP}

function IntersectLine(line1, line2: TLineDef; out parallel: boolean): TPointF;
  procedure SetParallel;
  begin
    parallel := true;
    //return the center of the segment between line origins
    result.x := (line1.origin.x+line2.origin.x)/2;
    result.y := (line1.origin.y+line2.origin.y)/2;
  end;
var pos, step: single;
  n: TPointF;
begin
  parallel := false;
  n := PointF(-line2.dir.y, line2.dir.x);
  step := line1.dir**n;
  if step = 0 then begin SetParallel; exit; end;
  pos := (line2.origin - line1.origin)**n;
  result := line1.origin + line1.dir * (pos/step);
end;

{ Check if a polygon is convex, i.e. it always turns in the same direction }
function IsConvex(const pts: array of TPointF; IgnoreAlign: boolean = true): boolean;
var
  positive,negative,zero: boolean;
  product: single;
  i: Integer;
begin
  positive := false;
  negative := false;
  zero := false;
  for i := 0 to high(pts) do
  begin
    product := (pts[(i+1) mod length(pts)].x-pts[i].x)*(pts[(i+2) mod length(pts)].y-pts[i].y) -
               (pts[(i+1) mod length(pts)].y-pts[i].y)*(pts[(i+2) mod length(pts)].x-pts[i].x);
    if product > 0 then
    begin
      if negative then
      begin
        result := false;
        exit;
      end;
      positive := true;
    end else
    if product < 0 then
    begin
      if positive then
      begin
        result := false;
        exit;
      end;
      negative := true;
    end else
      zero := true;
  end;
  if not IgnoreAlign and zero then
    result := false
  else
    result := true;
end;

{ Check if two segments intersect }
function DoesSegmentIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;
var
  seg1: TLineDef;
  seg1len: single;
  seg2: TLineDef;
  seg2len: single;
  inter: TPointF;
  pos1,pos2: single;
  para: boolean;

begin
  { Determine line definitions }
  seg1.origin := pt1;
  seg1.dir := pt2-pt1;
  seg1len := VectLen(seg1.dir);
  if seg1len = 0 then
  begin
    result := false;
    exit;
  end;
  seg1.dir.Scale(1/seg1len);

  seg2.origin := pt3;
  seg2.dir := pt4-pt3;
  seg2len := VectLen(seg2.dir);
  if seg2len = 0 then
  begin
    result := false;
    exit;
  end;
  seg2.dir.Scale(1/seg2len);

  //obviously parallel
  if seg1.dir = seg2.dir then
    result := false
  else
  begin
    //try to compute intersection
    inter := IntersectLine(seg1,seg2,para);
    if para then
      result := false
    else
    begin
      //check if intersections are inside the segments
      pos1 := (inter-seg1.origin)**seg1.dir;
      pos2 := (inter-seg2.origin)**seg2.dir;
      if (pos1 >= 0) and (pos1 <= seg1len) and
         (pos2 >= 0) and (pos2 <= seg2len) then
        result := true
      else
        result := false;
    end;
  end;
end;

function IsClockwise(const pts: array of TPointF): boolean;
var
  i: Integer;
begin
  for i := 0 to high(pts) do
  begin
    if (pts[(i+1) mod length(pts)].x-pts[i].x)*(pts[(i+2) mod length(pts)].y-pts[i].y) -
       (pts[(i+1) mod length(pts)].y-pts[i].y)*(pts[(i+2) mod length(pts)].x-pts[i].x) < 0 then
    begin
       result := false;
       exit;
    end;
  end;
  result := true;
end;

function IsMostlyClockwise(const pts: array of TPointF): boolean;
var
  i: Integer;
  sum: double;
begin
  sum := 0;
  for i := 0 to high(pts) do
    sum += (pts[(i+1) mod length(pts)].x-pts[i].x)*(pts[(i+1) mod length(pts)].y+pts[i].y);
  result := sum <= 0;
end;

{ Check if a quaduadrilateral intersects itself }
function DoesQuadIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;
begin
  result := DoesSegmentIntersect(pt1,pt2,pt3,pt4) or DoesSegmentIntersect(pt2,pt3,pt4,pt1);
end;

{$DEFINE INCLUDE_IMPLEMENTATION}
{$I bezier.inc}

{$ENDIF}
