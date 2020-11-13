{=== Geometry types ===}

{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}
const
  {* Value indicating that there is nothing in the single-precision floating point value.
     It is also used as a separator in lists }
  EmptySingle = single(-3.402823e38);

type
  TPoint = BGRAClasses.TPoint;
  TSize = BGRAClasses.TSize;
  
  {* Pointer to a ''TPointF'' structure }
  PPointF = ^BGRAClasses.TPointF;
  {* Contains a point with single-precision floating point coordinates }
  TPointF = BGRAClasses.TPointF;
  {* Contains an array of points with single-precision floating point coordinates }
  ArrayOfTPointF = array of TPointF;

  {* An affine matrix contains three 2D vectors: the image of x, the image of y and the translation }
  TAffineMatrix = array[1..2,1..3] of single;
  TRectF = BGRAClasses.TRectF;

{$if FPC_FULLVERSION<030001}
  {$define BGRA_DEFINE_TRECTHELPER}
  { TRectHelper }

  TRectHelper = record helper for TRect
  private
    function GetHeight: integer;
    function GetIsEmpty: boolean;
    function GetWidth: integer;
    procedure SetHeight(AValue: integer);
    procedure SetWidth(AValue: integer);
  public
    constructor Create(Origin: TPoint; AWidth, AHeight: Longint); overload;
    constructor Create(ALeft, ATop, ARight, ABottom: Longint); overload;
    procedure Intersect(R: TRect);
    class function Intersect(R1: TRect; R2: TRect): TRect; static;
    function IntersectsWith(R: TRect): Boolean;
    class function Union(R1, R2: TRect): TRect; static;
    procedure Union(R: TRect);
    procedure Offset(DX, DY: Longint);
    procedure Inflate(DX, DY: Longint);
    function Contains(const APoint: TPoint): boolean; overload;
    function Contains(const ARect: TRect): boolean; overload;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property IsEmpty: boolean read GetIsEmpty;
  end;

operator=(const ARect1,ARect2: TRect): boolean;
{$endif}

{$if (FPC_FULLVERSION<030001) or defined(BGRABITMAP_USE_MSEGUI)}
type
  {$define BGRA_DEFINE_TSIZEHELPER}
  { TSizeHelper }

  TSizeHelper = record helper for TSize
  private
    function GetHeight: integer;
    function GetWidth: integer;
  public
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
  end;
{$ENDIF}

const
  EmptyPoint : TPoint = (X: -2147483648; Y: -2147483648);

function IsEmptyPoint(const APoint: TPoint): boolean;

type

  { TPointFHelper }

  TPointFHelper = record helper for TPointF
    procedure Offset(const apt : TPointF); overload;
    procedure Offset(const apt : TPoint); overload;
    procedure Offset(dx,dy : longint); overload;
    procedure Offset(dx,dy : single); overload;
    procedure Scale(AScale: single);
    procedure Normalize;

    function Ceiling: TPoint;
    function Truncate: TPoint;
    function Floor: TPoint;
    function Round: TPoint;
    function Length: Single;
    function IsEmpty: boolean;
  end;

type
  PRectF = ^TRectF;

  { TRectFHelper }

  TRectFHelper = record helper for TRectF
    class function Intersect(const R1: TRectF; const R2: TRectF): TRectF; overload; static;
    class function Union(const R1: TRectF; const R2: TRectF): TRectF; overload; static;
    class function Union(const R1: TRectF; const R2: TRectF; ADiscardEmpty: boolean): TRectF; overload; static;
    function Union(const r: TRectF):TRectF; overload;
    function Union(const r: TRectF; ADiscardEmpty: boolean):TRectF; overload;
    procedure Include(const APoint: TPointF);
    function Contains(const APoint: TPointF; AIncludeBottomRight: boolean = false): boolean;
    function IntersectsWith(const r: TRectF): boolean;
    function IsEmpty: boolean;
  end;

const
  {* A value for an empty rectangle }
  EmptyRectF : TRectF = (left:0; top:0; right:0; bottom: 0);

  function RectF(Left, Top, Right, Bottom: Single): TRectF;
  function RectF(const ATopLeft,ABottomRight: TPointF): TRectF;
  function RectF(const ARect: TRect): TRectF;
  function RectWithSizeF(left,top,width,height: Single): TRectF;
  function IsEmptyRectF(const ARect:TRectF): boolean;

type
  { TAffineBox }

  TAffineBox = object
  private
    function GetAsPolygon: ArrayOfTPointF;
    function GetBottomRight: TPointF;
    function GetCenter: TPointF;
    function GetHeight: single;
    function GetIsEmpty: boolean;
    function GetRectBounds: TRect;
    function GetRectBoundsF: TRectF;
    function GetSurface: single;
    function GetWidth: single;
  public
    TopLeft, TopRight,
    BottomLeft: TPointF;
    class function EmptyBox: TAffineBox; static;
    class function AffineBox(ATopLeft, ATopRight, ABottomLeft: TPointF): TAffineBox; overload; static;
    class function AffineBox(ARectF: TRectF): TAffineBox; overload; static;
    procedure Offset(AOfsX, AOfsY: single); overload;
    procedure Offset(AOfs: TPointF); overload;
    procedure Inflate(AHoriz, AVert: single); //inflates along axes
    function Contains(APoint: TPointF): boolean;
    property RectBounds: TRect read GetRectBounds;
    property RectBoundsF: TRectF read GetRectBoundsF;
    property BottomRight: TPointF read GetBottomRight;
    property IsEmpty: boolean read GetIsEmpty;
    property AsPolygon: ArrayOfTPointF read GetAsPolygon;
    property Width: single read GetWidth;
    property Height: single read GetHeight;
    property Surface: single read GetSurface;
    property Center: TPointF read GetCenter;
  end;

  const
    {** Value indicating that there is an empty ''TPointF'' structure.
        It is also used as a separator in lists of points }
    EmptyPointF: TPointF = (x: -3.402823e38; y: -3.402823e38);

  {----------------- Operators for TPointF --------------------}
  {** Creates a new structure with values ''x'' and ''y'' }
  function PointF(x, y: single): TPointF; overload;
  function PointF(pt: TPoint): TPointF; overload;
  {** Checks if the structure is empty (equal to ''EmptyPointF'') }
  function isEmptyPointF(const pt: TPointF): boolean;
  {** Checks if both ''x'' and ''y'' are equal }
  operator = (const pt1, pt2: TPointF): boolean; inline;
  {** Adds ''x'' and ''y'' components separately. It is like adding vectors }
  operator + (const pt1, pt2: TPointF): TPointF; inline;
  {** Subtract ''x'' and ''y'' components separately. It is like subtracting vectors }
  operator - (const pt1, pt2: TPointF): TPointF; inline;
  {** Returns a point with opposite values for ''x'' and ''y'' components }
  operator - (const pt2: TPointF): TPointF; inline;
  {** Scalar product: multiplies ''x'' and ''y'' components and returns the sum }
  operator * (const pt1, pt2: TPointF): single; inline;
  {** Multiplies both ''x'' and ''y'' by ''factor''. It scales the vector represented by (''x'',''y'') }
  operator * (const pt1: TPointF; factor: single): TPointF; inline;
  {** Multiplies both ''x'' and ''y'' by ''factor''. It scales the vector represented by (''x'',''y'') }
  operator * (factor: single; const pt1: TPointF): TPointF; inline;
  {** Returns the length of the vector (''dx'',''dy'') }
  function VectLen(dx,dy: single): single; overload;
  {** Returns the length of the vector represented by (''x'',''y'') }
  function VectLen(v: TPointF): single; overload;
  function VectDet(v1,v2: TPointF): double; inline;

type
  TFaceCulling = (fcNone, fcKeepCW, fcKeepCCW);

  {** Creates an array of ''TPointF'' }
  function PointsF(const pts: array of TPointF): ArrayOfTPointF;
  {** Concatenates arrays of ''TPointF'' }
  function ConcatPointsF(const APolylines: array of ArrayOfTPointF; AInsertEmptyPointInBetween: boolean = false): ArrayOfTPointF;
  {** Compute the length of the polyline contained in the array.
      ''AClosed'' specifies if the last point is to be joined to the first one }
  function PolylineLen(const pts: array of TPointF; AClosed: boolean = false): single;

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
    {** Start and end angle, in radian and clockwise. See angle convention in ''BGRAPath'' }
    startAngleRadCW, endAngleRadCW: single;
    {** Specifies if the arc goes anticlockwise }
    anticlockwise: boolean
  end;

  {** Creates a structure for an arc definition }
  function ArcDef(cx, cy, rx,ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single; anticlockwise: boolean) : TArcDef;

type
  {* Possible options for drawing an arc of an ellipse (used in ''BGRACanvas'') }
  TArcOption = (
    {** Close the path by joining the ending and starting point together }
    aoClosePath,
    {** Draw a pie shape by joining the ending and starting point to the center of the ellipse }
    aoPie,
    {** Fills the shape }
    aoFillPath);
    {** Set of options for drawing an arc }
    TArcOptions = set of TArcOption;

  TBGRAArrowStyle = (asNone, asNormal, asCut, asTriangle, asHollowTriangle, asFlipped, asFlippedCut, asTail, asTailRepeat);

  { TBGRACustomArrow }

  TBGRACustomArrow = class
  protected
    function GetEndOffsetX: single; virtual; abstract;
    function GetEndRepeatCount: integer; virtual; abstract;
    function GetEndSizeFactor: TPointF; virtual; abstract;
    function GetIsEndDefined: boolean; virtual; abstract;
    function GetIsStartDefined: boolean; virtual; abstract;
    function GetStartOffsetX: single; virtual; abstract;
    function GetStartRepeatCount: integer; virtual; abstract;
    function GetStartSizeFactor: TPointF; virtual; abstract;
    procedure SetEndOffsetX(AValue: single); virtual; abstract;
    procedure SetEndRepeatCount(AValue: integer); virtual; abstract;
    procedure SetEndSizeFactor(AValue: TPointF); virtual; abstract;
    procedure SetStartOffsetX(AValue: single); virtual; abstract;
    procedure SetStartRepeatCount(AValue: integer); virtual; abstract;
    procedure SetStartSizeFactor(AValue: TPointF); virtual; abstract;
    function GetLineCap: TPenEndCap; virtual; abstract;
    procedure SetLineCap(AValue: TPenEndCap); virtual; abstract;
  public
    function ComputeStartAt(const APosition: TPointF; const ADirection: TPointF; const AWidth: single; const ACurrentPos: single): ArrayOfTPointF; virtual; abstract;
    function ComputeEndAt(const APosition: TPointF; const ADirection: TPointF; const AWidth: single; const ACurrentPos: single): ArrayOfTPointF; virtual; abstract;
    procedure StartAsNone; virtual; abstract;
    procedure StartAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1); virtual; abstract;
    procedure StartAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5); virtual; abstract;
    procedure StartAsTail; virtual; abstract;
    procedure EndAsNone; virtual; abstract;
    procedure EndAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1); virtual; abstract;
    procedure EndAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5); virtual; abstract;
    procedure EndAsTail; virtual; abstract;
    property IsStartDefined: boolean read GetIsStartDefined;
    property IsEndDefined: boolean read GetIsEndDefined;
    property StartOffsetX: single read GetStartOffsetX write SetStartOffsetX;
    property EndOffsetX: single read GetEndOffsetX write SetEndOffsetX;
    property LineCap: TPenEndCap read GetLineCap write SetLineCap;
    property StartSize: TPointF read GetStartSizeFactor write SetStartSizeFactor;
    property EndSize: TPointF read GetEndSizeFactor write SetEndSizeFactor;
    property StartRepeatCount: integer read GetStartRepeatCount write SetStartRepeatCount;
    property EndRepeatCount: integer read GetEndRepeatCount write SetEndRepeatCount;
  end;

  { TBGRACustomPenStroker }

  TBGRACustomPenStroker = class
  protected
      function GetArrow: TBGRACustomArrow; virtual; abstract;
      function GetArrowOwned: boolean; virtual; abstract;
      function GetCustomPenStyle: TBGRAPenStyle; virtual; abstract;
      function GetJoinStyle: TPenJoinStyle; virtual; abstract;
      function GetLineCap: TPenEndCap; virtual; abstract;
      function GetMiterLimit: single; virtual; abstract;
      function GetPenStyle: TPenStyle; virtual; abstract;
      function GetStrokeMatrix: TAffineMatrix; virtual; abstract;
      procedure SetArrow(AValue: TBGRACustomArrow); virtual; abstract;
      procedure SetArrowOwned(AValue: boolean); virtual; abstract;
      procedure SetCustomPenStyle(AValue: TBGRAPenStyle); virtual; abstract;
      procedure SetJoinStyle(AValue: TPenJoinStyle); virtual; abstract;
      procedure SetLineCap(AValue: TPenEndCap); virtual; abstract;
      procedure SetMiterLimit(AValue: single); virtual; abstract;
      procedure SetPenStyle(AValue: TPenStyle); virtual; abstract;
      procedure SetStrokeMatrix(const AValue: TAffineMatrix); virtual; abstract;
  public
      function ComputePolyline(const APoints: array of TPointF; AWidth: single; AClosedCap: boolean = true): ArrayOfTPointF; overload; virtual; abstract;
      function ComputePolyline(const APoints: array of TPointF; AWidth: single; APenColor: TBGRAPixel; AClosedCap: boolean = true): ArrayOfTPointF; overload; virtual; abstract;
      function ComputePolylineAutoCycle(const APoints: array of TPointF; AWidth: single): ArrayOfTPointF; virtual; abstract;
      function ComputePolygon(const APoints: array of TPointF; AWidth: single): ArrayOfTPointF; virtual; abstract;
      property Style: TPenStyle read GetPenStyle write SetPenStyle;
      property CustomPenStyle: TBGRAPenStyle read GetCustomPenStyle write SetCustomPenStyle;
      property Arrow: TBGRACustomArrow read GetArrow write SetArrow;
      property ArrowOwned: boolean read GetArrowOwned write SetArrowOwned;
      property StrokeMatrix: TAffineMatrix read GetStrokeMatrix write SetStrokeMatrix;
      property LineCap: TPenEndCap read GetLineCap write SetLineCap;
      property JoinStyle: TPenJoinStyle read GetJoinStyle write SetJoinStyle;
      property MiterLimit: single read GetMiterLimit write SetMiterLimit;
  end;

type
  {* Point in 3D with single-precision floating point coordinates }

  PPoint3D = ^TPoint3D;

  { TPoint3D }

  TPoint3D = record
    x,y,z: single;
    procedure Offset(const point3D: TPoint3D);
    procedure Scale(AScale: single);
  end;

  {----------------- Operators for TPoint3D ---------------}
  {** Creates a new structure with values (''x'',''y'',''z'') }
  function Point3D(x,y,z: single): TPoint3D;
  {** Checks if all components ''x'', ''y'' and ''z'' are equal }
  operator = (const v1,v2: TPoint3D): boolean; inline;
  {** Adds components separately. It is like adding vectors }
  operator + (const v1,v2: TPoint3D): TPoint3D; inline;
  {** Subtract components separately. It is like subtracting vectors }
  operator - (const v1,v2: TPoint3D): TPoint3D; inline;
  {** Returns a point with opposite values for all components }
  operator - (const v: TPoint3D): TPoint3D; inline;
  {** Scalar product: multiplies components and returns the sum }
  operator * (const v1,v2: TPoint3D): single; inline;
  {** Multiplies components by ''factor''. It scales the vector represented by (''x'',''y'',''z'') }
  operator * (const v1: TPoint3D; const factor: single): TPoint3D; inline;
  {** Multiplies components by ''factor''. It scales the vector represented by (''x'',''y'',''z'') }
  operator * (const factor: single; const v1: TPoint3D): TPoint3D; inline;
  {** Computes the vectorial product ''w''. It is perpendicular to both ''u'' and ''v'' }
  procedure VectProduct3D(u,v: TPoint3D; out w: TPoint3D);
  {** Normalize the vector, i.e. scale it so that its length be 1 }
  procedure Normalize3D(var v: TPoint3D); inline;
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
      the middle of the segment between the two origins. The value ''parallel''
      is set to indicate if the lines were parallel }
  function IntersectLine(line1, line2: TLineDef; out parallel: boolean): TPointF; overload;
  {** Checks if the polygon formed by the given points is convex. ''IgnoreAlign''
      specifies that if the points are aligned, it should still be considered as convex }
  function IsConvex(const pts: array of TPointF; IgnoreAlign: boolean = true): boolean;
  function IsClockwise(const pts: array of TPointF): boolean;
  {** Checks if the quad formed by the 4 given points intersects itself }
  function DoesQuadIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;
  {** Checks if two segment intersect }
  function DoesSegmentIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;

type
  TBGRACustomPathCursor = class;
  TBGRAPathDrawProc = procedure(const APoints: array of TPointF; AClosed: boolean; AData: Pointer) of object;
  TBGRAPathFillProc = procedure(const APoints: array of TPointF; AData: pointer) of object;

  {* A path is the ability to define a contour with ''moveTo'', ''lineTo''...
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
    {** Returns the content of the path as an array of points with the transformation specified by ''AMatrix'' }
    function getPoints(AMatrix: TAffineMatrix): ArrayOfTPointF; overload;
    {** Calls a given draw procedure for each sub path with computed coordinates for rendering }
    procedure stroke(ADrawProc: TBGRAPathDrawProc; AData: pointer); overload;
    procedure stroke(ADrawProc: TBGRAPathDrawProc; const AMatrix: TAffineMatrix; AData: pointer); overload;
    {** Calls a given fill procedure for each sub path with computed coordinates for rendering }
    procedure fill(AFillProc: TBGRAPathFillProc; AData: pointer); overload;
    procedure fill(AFillProc: TBGRAPathFillProc; const AMatrix: TAffineMatrix; AData: pointer); overload;
    {** Returns a cursor to go through the path. The cursor must be freed by calling ''Free''. }
    function getCursor: TBGRACustomPathCursor;
  end;

  { TBGRACustomPath }

  TBGRACustomPath = class(IBGRAPath)
    constructor Create; virtual; abstract;
    procedure beginPath; virtual; abstract;
    procedure closePath; virtual; abstract;
    procedure moveTo(constref pt: TPointF); virtual; abstract;
    procedure lineTo(constref pt: TPointF); virtual; abstract;
    procedure polylineTo(const pts: array of TPointF); virtual; abstract;
    procedure quadraticCurveTo(constref cp,pt: TPointF); virtual; abstract;
    procedure bezierCurveTo(constref cp1,cp2,pt: TPointF); virtual; abstract;
    procedure arc(constref arcDef: TArcDef); virtual; abstract;
    procedure openedSpline(const pts: array of TPointF; style: TSplineStyle); virtual; abstract;
    procedure closedSpline(const pts: array of TPointF; style: TSplineStyle); virtual; abstract;
    procedure copyTo(dest: IBGRAPath); virtual; abstract;
  protected
    function getPoints: ArrayOfTPointF; overload; virtual; abstract;
    function getPoints(AMatrix: TAffineMatrix): ArrayOfTPointF; overload; virtual; abstract;
    procedure stroke(ADrawProc: TBGRAPathDrawProc; AData: pointer); overload; virtual; abstract;
    procedure stroke(ADrawProc: TBGRAPathDrawProc; const AMatrix: TAffineMatrix; AData: pointer); overload; virtual; abstract;
    procedure fill(AFillProc: TBGRAPathFillProc; AData: pointer); overload; virtual; abstract;
    procedure fill(AFillProc: TBGRAPathFillProc; const AMatrix: TAffineMatrix; AData: pointer); overload; virtual; abstract;
    function getLength: single; virtual; abstract;
    function getCursor: TBGRACustomPathCursor; virtual; abstract;
  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
  end;

  TBGRAPathAny = class of TBGRACustomPath;

  { TBGRACustomPathCursor }
  {* Class that contains a cursor to browse an existing path }
  TBGRACustomPathCursor = class
  protected
    function GetArcPos: single; virtual; abstract;
    function GetCurrentCoord: TPointF; virtual; abstract;
    function GetCurrentTangent: TPointF; virtual; abstract;
    function GetLoopClosedShapes: boolean; virtual; abstract;
    function GetLoopPath: boolean; virtual; abstract;
    function GetPathLength: single; virtual; abstract;
    function GetBounds: TRectF; virtual; abstract;
    function GetStartCoordinate: TPointF; virtual; abstract;
    procedure SetArcPos(AValue: single); virtual; abstract;
    procedure SetLoopClosedShapes(AValue: boolean); virtual; abstract;
    procedure SetLoopPath(AValue: boolean); virtual; abstract;
  public
    {** Go forward in the path, increasing the value of ''Position''. If ''ADistance'' is negative, then
        it goes backward instead. ''ACanJump'' specifies if the cursor can jump from one shape to another
        without a line or an arc. Otherwise, the cursor is stuck, and the return value is less than
        the value ''ADistance'' provided.  If all the way has been travelled, the
        return value is equal to ''ADistance'' }
    function MoveForward(ADistance: single; ACanJump: boolean = true): single; virtual; abstract;
    {** Go backward, decreasing the value of ''Position''. If ''ADistance'' is negative, then it goes
        forward instead. ''ACanJump'' specifies if the cursor can jump from one shape to another
        without a line or an arc. Otherwise, the cursor is stuck, and the return value is less than
        the value ''ADistance'' provided. If all the way has been travelled, the
        return value is equal to ''ADistance'' }
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
        to the beginning, it will be only possible if the parameter ''ACanJump'' is set to True
        when moving along the path }
    property LoopPath: boolean read GetLoopPath write SetLoopPath;
  end;

var
  BGRAPathFactory: TBGRAPathAny;

const
  {* A value for an empty rectangle }
  EmptyRect : TRect = (left:0; top:0; right:0; bottom: 0);
{* Checks if a point is in a rectangle. This follows usual convention: ''r.Right'' and
  ''r.Bottom'' are not considered to be included in the rectangle. }
function PtInRect(const pt: TPoint; r: TRect): boolean; overload;
{* Creates a rectangle with the specified ''width'' and ''height'' }
function RectWithSize(left,top,width,height: integer): TRect;

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
  {* Order of polygons when rendered using ''TBGRAMultiShapeFiller''
     (in unit ''BGRAPolygon'') }
  TPolygonOrder = (
    {** No order, colors are mixed together }
    poNone,
    {** First polygon is on top }
    poFirstOnTop,
    {** Last polygon is on top }
    poLastOnTop);

  PIntersectionInfo = ^TIntersectionInfo;
  { TIntersectionInfo }
  {* Contains an intersection between an horizontal line and any shape. It
     is used when filling shapes }
  TIntersectionInfo = class
    interX: single;
    winding: integer;
    numSegment: integer;
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
          To augment that array, use ''CreateIntersectionInfo'' for new items }
      function CreateIntersectionArray: ArrayOfTIntersectionInfo; virtual; abstract;
      {** Create a structure to define one single intersection }
      function CreateIntersectionInfo: TIntersectionInfo; virtual; abstract;
      {** Free an array of intersections }
      procedure FreeIntersectionArray(var inter: ArrayOfTIntersectionInfo); virtual; abstract;

      {** Fill an array ''inter'' with actual intersections with the shape at the y coordinate ''cury''.
          ''nbInter'' receives the number of computed intersections. ''windingMode'' specifies if
          the winding method must be used to determine what is inside of the shape }
      procedure ComputeAndSort(cury: single; var inter: ArrayOfTIntersectionInfo; out nbInter: integer; windingMode: boolean); virtual; abstract;

      function GetSliceIndex: integer; virtual; abstract;
  end;

type
  {* Shape of a gradient }
  TGradientType = (
    {** The color changes along a certain vector and does not change along its perpendicular direction }
    gtLinear,
    {** The color changes like in ''gtLinear'' however it is symmetrical to a specified direction }
    gtReflected,
    {** The color changes along a diamond shape }
    gtDiamond,
    {** The color changes in a radial way from a given center }
    gtRadial,
    {** The color changes according to the angle relative to a given center }
    gtAngular);
const
  {** List of string to represent gradient types }
  GradientTypeStr : array[TGradientType] of string
  = ('Linear','Reflected','Diamond','Radial','Angular');
  {** Returns the gradient type represented by the given string }
  function StrToGradientType(str: string): TGradientType;

type
  TBGRAGradientGetColorAtFunc = function(position: integer): TBGRAPixel of object;
  TBGRAGradientGetColorAtFloatFunc = function(position: single): TBGRAPixel of object;
  TBGRAGradientGetExpandedColorAtFunc = function(position: integer): TExpandedPixel of object;
  TBGRAGradientGetExpandedColorAtFloatFunc = function(position: single): TExpandedPixel of object;

  { TBGRACustomGradient }
  {* Defines a gradient of color, not specifying its shape but only the
     series of colors }
  TBGRACustomGradient = class
  public
    {** Returns the color at a given ''position''. The reference range is
        from 0 to 65535, however values beyond are possible as well }
    function GetColorAt(position: integer): TBGRAPixel; virtual; abstract;
    function GetExpandedColorAt(position: integer): TExpandedPixel; virtual;
    {** Returns the color at a given ''position''. The reference range is
        from 0 to 1, however values beyond are possible as well }
    function GetColorAtF(position: single): TBGRAPixel; virtual;
    function GetExpandedColorAtF(position: single): TExpandedPixel; virtual;
    {** Returns the average color of the gradient }
    function GetAverageColor: TBGRAPixel; virtual; abstract;
    function GetAverageExpandedColor: TExpandedPixel; virtual;
    function GetMonochrome: boolean; virtual; abstract;
    {** This property is True if the gradient contains only one color,
        and thus is not really a gradient }
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
  lenH := PointF(-u.y,u.x)*v;
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
  result.TopLeft := ATopLeft;
  result.TopRight := ATopRight;
  result.BottomLeft := ABottomLeft;
end;

class function TAffineBox.AffineBox(ARectF: TRectF): TAffineBox;
begin
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
  if perpU*v < 0 then
  begin
    perpU := -perpU;
    perpV := -perpV;
  end;

  //determine position along normals
  posU1 := (APoint-TopLeft)*perpU;
  posU2 := (APoint-BottomLeft)*perpU;
  posV1 := (APoint-TopLeft)*perpV;
  posV2 := (APoint-TopRight)*perpV;

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

operator *(const pt1, pt2: TPointF): single;
begin
  result := pt1.x*pt2.x + pt1.y*pt2.y;
end;

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

function VectLen(v: TPointF): single;
begin
  if isEmptyPointF(v) then
    result := EmptySingle
  else
    result := sqrt(v*v);
end;

function VectDet(v1, v2: TPointF): double;
begin
  result := v1.x*v2.y - v1.y*v2.x;
end;

function VectLen(dx, dy: single): single;
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
  step := line1.dir*n;
  if step = 0 then begin SetParallel; exit; end;
  pos := (line2.origin - line1.origin)*n;
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
      pos1 := (inter-seg1.origin)*seg1.dir;
      pos2 := (inter-seg2.origin)*seg2.dir;
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

{ Check if a quaduadrilateral intersects itself }
function DoesQuadIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;
begin
  result := DoesSegmentIntersect(pt1,pt2,pt3,pt4) or DoesSegmentIntersect(pt2,pt3,pt4,pt1);
end;

{$DEFINE INCLUDE_IMPLEMENTATION}
{$I bezier.inc}

{$ENDIF}
