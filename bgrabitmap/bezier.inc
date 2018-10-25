{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}

type
  { TCubicBezierCurve }
  {* Definition of a Bézier curve of order 3. It has two control points ''c1'' and ''c2''. Those are not reached by the curve }
  TCubicBezierCurve = object
  private
    function SimpleComputePoints(AAcceptedDeviation: single = 0.1; AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
  public
    {** Starting point (reached) }
    p1: TPointF;
    {** First control point (not reached by the curve) }
    c1: TPointF;
    {** Second control point (not reached by the curve) }
    c2: TPointF;
    {** Ending point (reached) }
    p2: TPointF;
    {** Computes the point at time ''t'', varying from 0 to 1 }
    function ComputePointAt(t: single): TPointF;
    {** Split the curve in two such that ''ALeft.p2'' = ''ARight.p1'' }
    procedure Split(out ALeft, ARight: TCubicBezierCurve);
    {** Compute an approximation of the length of the curve. ''AAcceptedDeviation'' indicates the
       maximum orthogonal distance that is ignored and approximated by a straight line. }
    function ComputeLength(AAcceptedDeviation: single = 0.1): single;
    {** Computes a polygonal approximation of the curve. ''AAcceptedDeviation'' indicates the
       maximum orthogonal distance that is ignored and approximated by a straight line.
       ''AIncludeFirstPoint'' indicates if the first point must be included in the array }
    function ToPoints(AAcceptedDeviation: single = 0.1; AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
    procedure CopyToPath(ADest: IBGRAPath);
    function GetBounds: TRectF;
  end;

  {** Creates a structure for a cubic Bézier curve }
  function BezierCurve(origin, control1, control2, destination: TPointF) : TCubicBezierCurve; overload;

type
  { TQuadraticBezierCurve }
  {* Definition of a Bézier curve of order 2. It has one control point }
  TQuadraticBezierCurve = object
  private
    function SimpleComputePoints(AAcceptedDeviation: single = 0.1; AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
    function ComputeExtremumPositionOutsideSegment: single;
  public
    {** Starting point (reached) }
    p1: TPointF;
    {** Control point (not reached by the curve) }
    c: TPointF;
    {** Ending point (reached) }
    p2: TPointF;
    {** Computes the point at time ''t'', varying from 0 to 1 }
    function ComputePointAt(t: single): TPointF;
    {** Split the curve in two such that ''ALeft.p2'' = ''ARight.p1'' }
    procedure Split(out ALeft, ARight: TQuadraticBezierCurve);
    {** Compute the '''exact''' length of the curve }
    function ComputeLength: single;
    {** Computes a polygonal approximation of the curve. ''AAcceptedDeviation'' indicates the
       maximum orthogonal distance that is ignored and approximated by a straight line.
       ''AIncludeFirstPoint'' indicates if the first point must be included in the array }
    function ToPoints(AAcceptedDeviation: single = 0.1; AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
    procedure CopyToPath(ADest: IBGRAPath);
    function GetBounds: TRectF;
  end;

  {** Creates a structure for a quadratic Bézier curve }
  function BezierCurve(origin, control, destination: TPointF) : TQuadraticBezierCurve; overload;
  {** Creates a structure for a quadratic Bézier curve without curvature }
  function BezierCurve(origin, destination: TPointF) : TQuadraticBezierCurve; overload;

type
  { A quasi-standard rational quadratic Bezier curve is defined by three points and a number:
      p1 = starting point
      c = control point
      p2 = ending point
      weight = weight for the control point

    The curve is defined with the function (t in [0;1]):
      f: t -> ((1-t)^2*p1 + 2*t*(1-t)*weight*c + t^2*p2) / (1-t)^2 + 2*t*(1-t)*weight + t^2)

    The curve is an arc of:
       - ellipse when  weight in ]-1;1[
       - parabola when weight = 1 (classical quadratic Bezier curve)
       - hyperbola when weight > 1

    A negative weight give the complementary curve for its positive counterpart.
    So when weight <= -1 the curve is discontinuous:
      - infinite branches of parabola when weight = -1
      - infinite branches of hyperbola and symetric hyperbola when weight < -1

    To transform a rational quadratic Bezier curve with an affin transformation, you
    only have to transform the three points and leave the weight as it is. }

  ArrayOfSingle = array of single;

  { TRationalQuadraticBezierCurve }
  {* Definition of a quasi-standard rational Bézier curve of order 2. It has one weighted control point }
  TRationalQuadraticBezierCurve = object
    //** Starting, control and ending points
    p1, c, p2 : TPointF;
    //** Weight of control point
    weight : single;
  private
    function GetIsInfinite: boolean;
    function InternalComputePoints(AInfiniteBounds: TRectF; AAcceptedDeviation: single = 0.1; AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
    function GetBoundingPositions(AIncludeFirstAndLast: boolean; ASorted: boolean): ArrayOfSingle;
  public
    function ComputePointAt(t: single): TPointF;
    function ComputeLength(AAcceptedDeviation: single = 0.1): single;
    function ToPoints(AAcceptedDeviation: single = 0.1; AIncludeFirstPoint: boolean = true): ArrayOfTPointF; overload;
    function ToPoints(AInfiniteBounds: TRectF; AAcceptedDeviation: single = 0.1; AIncludeFirstPoint: boolean = true): ArrayOfTPointF; overload;
    function GetBounds: TRectF;
    procedure Split(out ALeft, ARight: TRationalQuadraticBezierCurve);
    property IsInfinite: boolean read GetIsInfinite;
  end;

  function BezierCurve(origin, control, destination: TPointF; Aweight:single) : TRationalQuadraticBezierCurve; overload;

type
  TEasyBezierCurveMode= (cmAuto, cmCurve, cmAngle);
  TEasyBezierPointTransformFunc = function(APoint: PPointF; AData: Pointer): TPointF of object;

  { TEasyBezierCurve }

  TEasyBezierCurve = object
  private
    function GetCurveMode(AIndex: integer): TEasyBezierCurveMode;
    function GetCurveStartPoint: TPointF;
    function GetPoint(AIndex: integer): TPointF;
    function GetPointCount: integer;
    procedure SetClosed(AValue: boolean);
    procedure SetCurveMode(AIndex: integer; AValue: TEasyBezierCurveMode);
    procedure SetMinimumDotProduct(AValue: single);
    procedure SetPoint(AIndex: integer; AValue: TPointF);
  protected
    FCurves: array of record
      isCurvedToNext,isCurvedToPrevious: boolean;
      Center,ControlPoint,NextCenter: TPointF;
    end;
    FInvalidated: boolean;
    FPoints: array of record
               Coord: TPointF;
               CurveMode: TEasyBezierCurveMode;
             end;
    FMinimumDotProduct: single;
    FClosed: boolean;
    function MaybeCurve(start1, end1, start2, end2: integer): boolean;
    procedure ComputeQuadraticCurves;
    function PointTransformNone(APoint: PPointF; {%H-}AData: Pointer): TPointF;
    function PointTransformOffset(APoint: PPointF; AData: Pointer): TPointF;
  public
    procedure Init;
    procedure Clear;
    procedure SetPoints(APoints: array of TPointF; ACurveMode: TEasyBezierCurveMode); overload;
    procedure SetPoints(APoints: array of TPointF; ACurveMode: array of TEasyBezierCurveMode); overload;
    procedure CopyToPath(ADest: IBGRAPath); overload;
    procedure CopyToPath(ADest: IBGRAPath; AOffset: TPointF); overload;
    procedure CopyToPath(ADest: IBGRAPath; ATransformFunc: TEasyBezierPointTransformFunc; ATransformData: Pointer); overload;
    property Point[AIndex: integer]: TPointF read GetPoint write SetPoint;
    property CurveMode[AIndex: integer]: TEasyBezierCurveMode read GetCurveMode write SetCurveMode;
    property PointCount: integer read GetPointCount;
    property MinimumDotProduct: single read FMinimumDotProduct write SetMinimumDotProduct;
    property Closed: boolean read FClosed write SetClosed;
    property CurveStartPoint: TPointF read GetCurveStartPoint;
    function ToPoints: ArrayOfTPointF;
    function ComputeLength: single;
  end;

const
  EasyBezierDefaultMinimumDotProduct = 0.707;

  function EasyBezierCurve(APoints: array of TPointF; AClosed: boolean; ACurveMode: TEasyBezierCurveMode;
    AMinimumDotProduct: single = EasyBezierDefaultMinimumDotProduct): TEasyBezierCurve; overload;

  function EasyBezierCurve(APoints: array of TPointF; AClosed: boolean; ACurveMode: array of TEasyBezierCurveMode;
    AMinimumDotProduct: single = EasyBezierDefaultMinimumDotProduct): TEasyBezierCurve; overload;

{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}
//-------------- Bézier curves definitions ----------------
// See : http://en.wikipedia.org/wiki/B%C3%A9zier_curve

// Define a Bézier curve with two control points.
function BezierCurve(origin, control1, control2, destination: TPointF): TCubicBezierCurve;
begin
  result.p1 := origin;
  result.c1 := control1;
  result.c2 := control2;
  result.p2 := destination;
end;

// Define a Bézier curve with one control point.
function BezierCurve(origin, control, destination: TPointF
  ): TQuadraticBezierCurve;
begin
  result.p1 := origin;
  result.c := control;
  result.p2 := destination;
end;

//straight line
function BezierCurve(origin, destination: TPointF): TQuadraticBezierCurve;
begin
  result.p1 := origin;
  result.c := (origin+destination)*0.5;
  result.p2 := destination;
end;

// rational Bezier curve
function BezierCurve(origin, control, destination: TPointF; Aweight:single) : TRationalQuadraticBezierCurve;
begin
  result.p1 := origin;
  result.c := control;
  result.p2 := destination;
  result.weight := Aweight;
end;

function ComputeBezierCurvePrecision(pt1, pt2, pt3, pt4: TPointF; AAcceptedDeviation: single = 0.1): integer;
var
  len: single;
begin
  len    := sqr(pt1.x - pt2.x) + sqr(pt1.y - pt2.y);
  len    := max(len, sqr(pt3.x - pt2.x) + sqr(pt3.y - pt2.y));
  len    := max(len, sqr(pt3.x - pt4.x) + sqr(pt3.y - pt4.y));
  Result := round(sqrt(sqrt(len)/ AAcceptedDeviation) * 1);
  if Result<=0 then Result:=1;
end;

{ TCubicBezierCurve }

function TCubicBezierCurve.SimpleComputePoints(AAcceptedDeviation: single;
  AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
var
  t,step: single;
  i,nb: Integer;
  a,b,c: TpointF;
begin
  nb := ComputeBezierCurvePrecision(p1,c1,c2,p2, AAcceptedDeviation/2);
  if nb <= 1 then nb := 2;
  a:=p2-p1+3*(c1-c2);
  b:=3*(p1+c2)-6*c1;
  c:=3*(c1-p1);
  if AIncludeFirstPoint then
  begin
    setlength(result,nb);
    result[0] := p1;
    result[nb-1] := p2;
    step := 1/(nb-1);
    t := 0;
    for i := 1 to nb-2 do
    begin
      t += step;
      result[i] := p1+t*(c+t*(b+t*a))
    end;
  end else
  begin
    setlength(result,nb-1);
    result[nb-2] := p2;
    step := 1/(nb-1);
    t := 0;
    for i := 0 to nb-3 do
    begin
      t += step;
      result[i] := p1+t*(c+t*(b+t*a))
    end;
  end;
end;

function TCubicBezierCurve.ComputePointAt(t: single): TPointF;
var
  f1,f2,f3,f4: single;
begin
  f1 := (1-t);
  f2 := f1*f1;
  f1 *= f2;
  f2 *= t*3;
  f4 := t*t;
  f3 := f4*(1-t)*3;
  f4 *= t;

  result.x := f1*p1.x + f2*c1.x +
              f3*c2.x + f4*p2.x;
  result.y := f1*p1.y + f2*c1.y +
              f3*c2.y + f4*p2.y;
end;

procedure TCubicBezierCurve.Split(out ALeft, ARight: TCubicBezierCurve);
var midc: TPointF;
begin
  ALeft.p1 := p1;
  ALeft.c1 := 0.5*(p1+c1);
  ARight.p2 := p2;
  ARight.c2 := 0.5*(p2+c2);
  midc := 0.5*(c1+c2);
  ALeft.c2 := 0.5*(ALeft.c1+midc);
  ARight.c1 := 0.5*(ARight.c2+midc);
  ALeft.p2 := 0.5*(ALeft.c2+ARight.c1);
  ARight.p1 := ALeft.p2;
end;

function TCubicBezierCurve.ComputeLength(AAcceptedDeviation: single): single;
var
  t,step: single;
  i,nb: Integer;
  curCoord,nextCoord: TPointF;
begin
  nb := ComputeBezierCurvePrecision(p1,c1,c2,p2, AAcceptedDeviation);
  if nb <= 1 then nb := 2;
  result := 0;
  curCoord := p1;
  step := 1/(nb-1);
  t := 0;
  for i := 1 to nb-2 do
  begin
    t += step;
    nextCoord := ComputePointAt(t);
    result += VectLen(nextCoord-curCoord);
    curCoord := nextCoord;
  end;
  result += VectLen(p2-curCoord);
end;

function TCubicBezierCurve.ToPoints(AAcceptedDeviation: single;
  AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
begin
  result := SimpleComputePoints(AAcceptedDeviation, AIncludeFirstPoint);
end;

procedure TCubicBezierCurve.CopyToPath(ADest: IBGRAPath);
begin
  ADest.lineTo(p1);
  ADest.bezierCurveTo(c1,c2,p2);
end;

{//The following function computes by splitting the curve. It is slower than the simple function.
function TCubicBezierCurve.ToPoints(AAcceptedDeviation: single;
  ARelativeDeviation: boolean): ArrayOfTPointF;
  function ToPointsRec(const ACurve: TCubicBezierCurve): ArrayOfTPointF;
  var simpleLen2: single;
    v: TPointF;
    left,right: TCubicBezierCurve;
    subLeft,subRight: ArrayOfTPointF;
    maxDev,dev1,dev2: single;
    subLeftLen: integer;

    procedure ComputeExtremum;
    begin
      raise Exception.Create('Not implemented');
      result := nil;
    end;

  begin
    v := ACurve.p2-ACurve.p1;
    simpleLen2 := v*v;
    if simpleLen2 = 0 then
    begin
      if (ACurve.c1.x = ACurve.p1.x) and (ACurve.c1.y = ACurve.p1.y) and
         (ACurve.c2.x = ACurve.p2.x) and (ACurve.c2.y = ACurve.p2.y) then
      begin
        result := nil;
        exit;
      end;
      ACurve.Split(left,right);
    end else
    begin
      ACurve.Split(left,right);
      if not ARelativeDeviation then simpleLen2:= sqrt(simpleLen2);
      maxDev := AAcceptedDeviation*simpleLen2;
      if abs(PointF(v.y,-v.x) * (left.p2-ACurve.p1)) <= maxDev then
      begin
        dev1 := PointF(v.y,-v.x) * (ACurve.c1-ACurve.p1);
        dev2 := PointF(v.y,-v.x) * (ACurve.c2-ACurve.p2);
        if not ((Sign(dev1)<>Sign(dev2)) and ((abs(dev1) > maxDev) or (abs(dev2) > maxDev))) then
        begin
          result := nil;
          if ((ACurve.c1-ACurve.p1)*v < -maxDev) or
             ((ACurve.c1-ACurve.p2)*v > maxDev) or
             ((ACurve.c2-ACurve.p1)*v < -maxDev) or
             ((ACurve.c2-ACurve.p2)*v > maxDev) then
            ComputeExtremum;
          exit;
        end;
      end;
    end;
    subRight := ToPointsRec(right);
    subLeft := ToPointsRec(left);
    subLeftLen := length(subLeft);

    //avoid leaving a gap in memory
    result := subLeft;
    subLeft := nil;
    setlength(result, subLeftLen+1+length(subRight));
    result[subLeftLen] := left.p2;
    move(subRight[0], result[subLeftLen+1], length(subRight)*sizeof(TPointF));
  end;

var
  subLen: integer;

begin
  if (c1.x = p1.x) and (c1.y = p1.y) and
     (c1.x = c2.x) and (c1.y = c2.y) and
     (c1.x = p2.x) and (c1.y = p2.y) then
  begin
    setlength(result,1);
    result[0] := c1;
    exit;
  end else
  begin
    result := ToPointsRec(self);
    subLen := length(result);
    setlength(result, length(result)+2);
    move(result[0], result[1], subLen*sizeof(TPointF));
    result[0] := p1;
    result[high(result)] := p2;
  end;
end;}

function TCubicBezierCurve.GetBounds: TRectF;
const precision = 1e-5;

  procedure Include(pt: TPointF);
  begin
    if pt.x < result.Left then result.Left := pt.x
    else if pt.x > result.Right then result.Right := pt.x;
    if pt.y < result.Top then result.Top := pt.y
    else if pt.y > result.Bottom then result.Bottom := pt.y;
  end;

  procedure IncludeT(t: single);
  begin
    if (t > 0) and (t < 1) then
      Include(ComputePointAt(t));
  end;

  procedure IncludeABC(a,b,c: single);
  var b2ac, sqrtb2ac: single;
  begin
    if abs(a) < precision then
    begin
      if abs(b) < precision then exit;
      IncludeT(-c/b);
    end else
    begin
      b2ac := sqr(b) - 4 * a * c;
      if b2ac >= 0 then
      begin
        sqrtb2ac := sqrt(b2ac);
        IncludeT((-b + sqrtb2ac) / (2 * a));
        IncludeT((-b - sqrtb2ac) / (2 * a));
      end;
    end;
  end;

var
  va, vb, vc: TPointF;

begin
  result.TopLeft := p1;
  result.BottomRight := p1;
  Include(p2);

  vb := 6 * p1 - 12 * c1 + 6 * c2;
  va := -3 * p1 + 9 * c1 - 9 * c2 + 3 * p2;
  vc := 3 * c1 - 3 * p1;

  IncludeABC(va.x,vb.x,vc.x);
  IncludeABC(va.y,vb.y,vc.y);
end;

{ TQuadraticBezierCurve }

function TQuadraticBezierCurve.SimpleComputePoints(AAcceptedDeviation: single;
  AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
var
  t,step: single;
  i,nb: Integer;
  pA,pB : TpointF;
begin
  nb := ComputeBezierCurvePrecision(p1,c,c,p2, AAcceptedDeviation);
  if nb <= 1 then nb := 2;
  pA := p2+p1-2*c; pB := 2*(c-p1);
  if AIncludeFirstPoint then
  begin
    setlength(result,nb);
    result[0] := p1;
    result[nb-1] := p2;
    step := 1/(nb-1);
    t := 0;
    for i := 1 to nb-2 do
    begin
      t += step;
      result[i] := p1+t*(pB+t*pA);
    end;
  end else
  begin
    setlength(result,nb-1);
    result[nb-2] := p2;
    step := 1/(nb-1);
    t := 0;
    for i := 0 to nb-3 do
    begin
      t += step;
      result[i] := p1+t*(pB+t*pA);
    end;
  end;
end;

function TQuadraticBezierCurve.ComputeExtremumPositionOutsideSegment: single;
var a,b: single;
  v: TPointF;
begin
  v := self.p2-self.p1;
  a := (self.p1-2*self.c+self.p2)*v;
  if a = 0 then //no solution
  begin
    result := -1;
    exit;
  end;
  b := (self.c-self.p1)*v;
  result := -b/a;
end;

function TQuadraticBezierCurve.ComputePointAt(t: single): TPointF;
var
  rev_t,f2,t2: single;
begin
  rev_t := (1-t);
  f2 := rev_t*t*2;
  rev_t *= rev_t;
  t2 := t*t;
  result.x := rev_t*p1.x + f2*c.x + t2*p2.x;
  result.y := rev_t*p1.y + f2*c.y + t2*p2.y;
end;

procedure TQuadraticBezierCurve.Split(out ALeft, ARight: TQuadraticBezierCurve);
begin
  ALeft.p1 := p1;
  ALeft.c := 0.5*(p1+c);
  ARight.p2 := p2;
  ARight.c := 0.5*(p2+c);
  ALeft.p2 := 0.5*(ALeft.c+ARight.c);
  ARight.p1 := ALeft.p2;
end;

function TQuadraticBezierCurve.ComputeLength: single;
var a,b: TPointF;
  A_,AB_,B_,Sabc,A_2,A_32,B_2,BA,
  divisor: single;
  extremumPos: single;
  extremum: TPointF;
begin
  a := p1 - 2*c + p2;
  b := 2*(c - p1);
  A_ := 4*(a*a);
  B_ := b*b;
  if (A_ = 0) or (B_ = 0) then
  begin
    result := VectLen(p2-p1);
    exit;
  end;
  AB_ := 4*(a*b);

  A_2 := sqrt(A_);
  B_2 := 2*sqrt(B_);
  BA := AB_/A_2;
  divisor := BA+B_2;
  if divisor <= 0 then
  begin
    extremumPos:= ComputeExtremumPositionOutsideSegment;
    if (extremumPos <= 0) or (extremumPos >= 1) then
      result := VectLen(p2-p1)
    else
    begin
      extremum := ComputePointAt(extremumPos);
      result := VectLen(extremum-p1)+VectLen(p2-extremum);
    end;
    exit;
  end;

  Sabc := 2*sqrt(A_+AB_+B_);
  A_32 := 2*A_*A_2;
  result := ( A_32*Sabc +
              A_2*AB_*(Sabc-B_2) +
              (4*B_*A_-AB_*AB_)*ln( (2*A_2+BA+Sabc)/divisor )
            )/(4*A_32);
end;

function TQuadraticBezierCurve.ToPoints(AAcceptedDeviation: single;
  AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
begin
  result := SimpleComputePoints(AAcceptedDeviation, AIncludeFirstPoint);
end;

procedure TQuadraticBezierCurve.CopyToPath(ADest: IBGRAPath);
begin
  ADest.lineTo(p1);
  ADest.quadraticCurveTo(c,p2);
end;

function TQuadraticBezierCurve.GetBounds: TRectF;
const precision = 1e-5;

  procedure Include(pt: TPointF);
  begin
    if pt.x < result.Left then result.Left := pt.x
    else if pt.x > result.Right then result.Right := pt.x;
    if pt.y < result.Top then result.Top := pt.y
    else if pt.y > result.Bottom then result.Bottom := pt.y;
  end;

  procedure IncludeT(t: single);
  begin
    if (t > 0) and (t < 1) then
      Include(ComputePointAt(t));
  end;

  procedure IncludeABC(a,b,c: single);
  var denom: single;
  begin
    denom := a-2*b+c;
    if abs(denom) < precision then exit;
    IncludeT((a-b)/denom);
  end;

begin
  result.TopLeft := p1;
  result.BottomRight := p1;
  Include(p2);

  IncludeABC(p1.x,c.x,p2.x);
  IncludeABC(p1.y,c.y,p2.y);
end;

{//The following function computes by splitting the curve. It is slower than the simple function
function TQuadraticBezierCurve.ToPoints(AAcceptedDeviation: single; ARelativeDeviation: boolean): ArrayOfTPointF;

  function ToPointsRec(const ACurve: TQuadraticBezierCurve): ArrayOfTPointF;
  var simpleLen2: single;
    v: TPointF;
    left,right: TQuadraticBezierCurve;
    subLeft,subRight: ArrayOfTPointF;
    subLeftLen: Integer;

    procedure ComputeExtremum;
    var
      t: single;
    begin
      t := ACurve.ComputeExtremumPositionOutsideSegment;
      if (t <= 0) or (t >= 1) then
        result := nil
      else
      begin
        setlength(result,1);
        result[0] := ACurve.ComputePointAt(t);
      end;
    end;

  begin
    v := ACurve.p2-ACurve.p1;
    simpleLen2 := v*v;
    if simpleLen2 = 0 then
    begin
      if (ACurve.c.x = ACurve.p1.x) and (ACurve.c.y = ACurve.p1.y) then
      begin
        result := nil;
        exit;
      end;
      ACurve.Split(left,right);
    end else
    begin
      ACurve.Split(left,right);
      if not ARelativeDeviation then simpleLen2:= sqrt(simpleLen2);
      if abs(PointF(v.y,-v.x) * (left.p2-ACurve.p1))
          <= AAcceptedDeviation*simpleLen2 then
      begin
        result := nil;
        if ((ACurve.c-ACurve.p1)*v < -AAcceptedDeviation*simpleLen2) or
           ((ACurve.c-ACurve.p2)*v > AAcceptedDeviation*simpleLen2) then
          ComputeExtremum;
        exit;
      end;
    end;
    subRight := ToPointsRec(right);
    subLeft := ToPointsRec(left);
    subLeftLen := length(subLeft);

    //avoid leaving a gap in memory
    result := subLeft;
    subLeft := nil;
    setlength(result, subLeftLen+1+length(subRight));
    result[subLeftLen] := left.p2;
    move(subRight[0], result[subLeftLen+1], length(subRight)*sizeof(TPointF));
  end;

var
  subLen: integer;

begin
  if (c.x = p1.x) and (c.y = p1.y) and
     (c.x = p2.x) and (c.y = p2.y) then
  begin
    setlength(result,1);
    result[0] := c;
    exit;
  end else
  begin
    result := ToPointsRec(self);
    subLen := length(result);
    setlength(result, length(result)+2);
    move(result[0], result[1], subLen*sizeof(TPointF));
    result[0] := p1;
    result[high(result)] := p2;
  end;
end;}

{ TRationalQuadraticBezierCurve }

function TRationalQuadraticBezierCurve.GetIsInfinite: boolean;
begin
  result:= (weight <= -1);
end;

function TRationalQuadraticBezierCurve.InternalComputePoints(AInfiniteBounds: TRectF; AAcceptedDeviation: single;
  AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
var
  pA,pB : TpointF;
  a1,b1: single;

  function InternalComputeAt(t: single): TPointF;
  var
    den: single;
  begin
    den := (1+t*(b1+t*a1));
    if den <> 0 then
       result := (p1+t*(pB+t*pA))*(1/den)
    else
       result := EmptyPointF
  end;

  procedure ComputeFactors;
  var
    c2 : TpointF;
    c1: single;
  begin
    c1 := 2*weight; c2 := c1*c;
    pA := p2+p1-c2; pB := -2*p1+c2;
    a1 := 2-c1;     b1 := -a1;
  end;

  function ComputeContinuous(t1,t2: single; AIncludeFirstPoint: boolean): ArrayOfTPointF;
  var
    pointCount: integer;

    procedure AddPoint(APoint: TPointF);
    begin
      if isEmptyPointF(APoint) then exit;
      if pointCount >= length(result) then
        setlength(result, pointCount*2+4);
      result[pointCount] := APoint;
      inc(pointCount);
    end;

    procedure ComputeRec(left: single; constref leftPoint: TPointF; right: single; constref rightPoint: TPointF);
    var
      middlePoint, u: TPointF;
      middle, lenU, deviation: Single;
    begin
      if rightPoint<>leftPoint then
      begin
        middle := (left+right)*0.5;
        middlePoint := InternalComputeAt(middle);
        u := rightPoint-leftPoint;
        lenU := VectLen(u);
        if lenU>0 then u *= (1/lenU);
        deviation := abs((middlePoint-leftPoint)*PointF(u.y,-u.x));
        if deviation > AAcceptedDeviation then
        begin
          ComputeRec(left, leftPoint, middle, middlePoint);
          AddPoint(middlePoint);
          ComputeRec(middle, middlePoint, right, rightPoint);
        end else
        if deviation > AAcceptedDeviation*0.6 then
          AddPoint(middlePoint);
      end;
    end;

  var
    startPoint, endPoint: TPointF;
  begin
    pointCount := 0;
    result:= nil;
    startPoint := InternalComputeAt(t1);
    endPoint := InternalComputeAt(t2);
    if AIncludeFirstPoint then AddPoint(startPoint);
    if endPoint <> startPoint then
    begin
      ComputeRec(t1,startPoint,t2,endPoint);
      AddPoint(endPoint);
    end;
    setlength(result,PointCount);
  end;

var
  tSplitA, tSplitB, tSplit1, tSplit2, delta: single;
  leftPart,middlePart,rightPart: array of TPointF;
  tList: ArrayOfSingle;
  parts: array of ArrayOfTPointF;
  i: Integer;

  function PointWithinInifiniteBounds(APoint: TPointF): boolean;
  begin
    result := not isEmptyPointF(APoint) and
              (APoint.x > AInfiniteBounds.Left) and (APoint.x < AInfiniteBounds.Right) and
              (APoint.y > AInfiniteBounds.Top) and (APoint.y < AInfiniteBounds.Bottom);
  end;

begin
  if weight = 0 then exit(PointsF([p1,p2]));
  ComputeFactors;

  if weight > -1 then
  begin
    tList := GetBoundingPositions(true,true);
    setlength(parts, length(tList)-1);
    for i := 0 to high(parts) do
      parts[i] := ComputeContinuous(tList[i],tList[i+1], AIncludeFirstPoint and (i=0));
    result := ConcatPointsF(parts);
  end
  else
  if weight = -1 then
  begin
    tSplit1 := 0.5;
    tSplitA := 0;
    while PointWithinInifiniteBounds(InternalComputeAt(tSplitA)) do tSplitA := (tSplitA+tSplit1)*0.5;
    tSplitB := 1;
    while PointWithinInifiniteBounds(InternalComputeAt(tSplitB)) do tSplitB := (tSplitB+tSplit1)*0.5;

    tList := GetBoundingPositions(true,true);
    setlength(parts, length(tList)-1);
    for i := 0 to high(parts) do
    begin
      if (tList[i] > tSplitA) and (tList[i+1] <= tSplitB) then parts[i] := nil
      else
      if (tList[i] <= tSplitA) and (tList[i+1] >= tSplitA) then
      begin
        parts[i] := ComputeContinuous(tList[i],tSplitA, AIncludeFirstPoint or (i>0));
        setlength(parts[i], length(parts[i])+1);
        parts[i][high(parts[i])] := EmptyPointF;

        if tList[i+1] > tSplitB then
          parts[i] := ConcatPointsF([parts[i], ComputeContinuous(tSplitB,tList[i+1], true)])
        else
          tList[i+1] := tSplitB;
      end
      else
      if (tList[i] < tSplitB) and (tList[i+1] >= tSplitB) then
        parts[i] := ComputeContinuous(tSplitB,tList[i+1], AIncludeFirstPoint or (i>0))
      else
        parts[i] := ComputeContinuous(tList[i],tList[i+1], AIncludeFirstPoint or (i>0));
    end;
    result := ConcatPointsF(parts);
  end else
  begin
    delta:= 1 - 2/(1-weight);
    tSplit1 := (1 - sqrt(delta))/2;
    tSplit2 := 1-tSplit1;

    tSplitA := 0;
    while PointWithinInifiniteBounds(InternalComputeAt(tSplitA)) do tSplitA := (tSplitA+tSplit1)*0.5;
    leftPart := ComputeContinuous(0, tSplitA, AIncludeFirstPoint);

    tSplitA := (tSplit1+tSplit2)*0.5;
    tSplitB := tSplitA;
    while PointWithinInifiniteBounds(InternalComputeAt(tSplitA)) do tSplitA := (tSplitA+tSplit1)*0.5;
    while PointWithinInifiniteBounds(InternalComputeAt(tSplitB)) do tSplitB := (tSplitB+tSplit2)*0.5;
    middlePart := ComputeContinuous(tSplitA, tSplitB, true);

    tSplitB := 1;
    while PointWithinInifiniteBounds(InternalComputeAt(tSplitB)) do tSplitB := (tSplitB+tSplit2)*0.5;
    rightPart:= ComputeContinuous(tSplitB, 1, true);
    result := ConcatPointsF([leftPart, PointsF([EmptyPointF]), middlePart, PointsF([EmptyPointF]), rightPart]);
  end;
end;

function TRationalQuadraticBezierCurve.GetBoundingPositions(
  AIncludeFirstAndLast: boolean; ASorted: boolean): ArrayOfSingle;
const precision = 1e-6;
var a,delta,sqrtDelta,den,invDen: single;
    A_,B_,p2_,c_: TPointF;
    posCount : integer;

  procedure Include(t: single);
  var
    i: Integer;
  begin
    if (t < 0) or (t > 1) then exit;
    for i := 0 to PosCount-1 do
      if result[i] = t then exit;
    result[posCount] := t;
    inc(posCount);
  end;

  procedure SortList;
  var i,j,k: integer;
    temp: single;
  begin
    for i := 1 to high(result) do
    begin
      j := i;
      while (j > 0) and (result[j-1] > result[i]) do dec(j);
      if j <> i then
      begin
        temp := result[i];
        for k := i downto j+1 do
          result[k] := result[k-1];
        result[j] := temp;
      end;
    end;
  end;

begin
  setlength(result, 6);
  posCount := 0;

  if AIncludeFirstAndLast then
  begin
    Include(0);
    Include(1);
  end;

  p2_ := p2-p1; c_ := c-p1; //translation with -p1
  B_ := 2*weight*c_; A_ := p2_-B_;
  a := 2*(1-weight);

  //on Ox
  den := a*p2_.x;
  if abs(den) >= precision then
  begin
    delta := sqr(A_.x)+den*B_.x;
    if delta >= 0 then
    begin
      invDen := 1/den;
      sqrtDelta := sqrt(delta);
      Include( (A_.x-sqrtDelta)*invDen );
      Include( (A_.x+sqrtDelta)*invDen );
    end;
  end else //den=0
  if abs(A_.x) >= precision  then
    Include( -B_.x/A_.x*0.5 );

  //on Oy
  den := a*p2_.y;
  if abs(den) >= precision then
  begin
    delta := sqr(A_.y)+den*B_.y;
    if delta >= 0 then
    begin
      invDen := 1/den;
      sqrtDelta := sqrt(delta);
      Include( (A_.y-sqrtDelta)*invDen );
      Include( (A_.y+sqrtDelta)*invDen );
    end;
  end else //den=0
  if abs(A_.y) >= precision  then
    Include( -B_.y/A_.y*0.5 );

  setlength(result, posCount);
  if ASorted then SortList;
end;

function TRationalQuadraticBezierCurve.ComputePointAt(t: single): TPointF;
var
  rev_t,f2,t2,den: single;
begin
  rev_t := (1-t);
  t2 := t*t;
  f2 := weight*rev_t*t*2;
  rev_t *= rev_t;
  den := rev_t+f2+t2;
  if den <> 0 then
  begin
    result.x := (rev_t*p1.x + f2*c.x + t2*p2.x)/den;
    result.y := (rev_t*p1.y + f2*c.y + t2*p2.y)/den;
  end
  else
    result := EmptyPointF
end;

function TRationalQuadraticBezierCurve.ToPoints(AInfiniteBounds: TRectF; AAcceptedDeviation: single;
  AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
begin
  if weight=1 then
     result := BezierCurve(p1,c,p2).ToPoints(AAcceptedDeviation, AIncludeFirstPoint)
  else
     result := InternalComputePoints(AInfiniteBounds, AAcceptedDeviation, AIncludeFirstPoint)
end;

function TRationalQuadraticBezierCurve.GetBounds: TRectF;
var a: single;
    A_,B_,p2_,c_: TPointF;
    t: single;
    tList: array of Single;
    i: Integer;

  procedure Include(pt: TPointF);
  begin
    if pt.x < result.Left then result.Left := pt.x
    else if pt.x > result.Right then result.Right := pt.x;
    if pt.y < result.Top then result.Top := pt.y
    else if pt.y > result.Bottom then result.Bottom := pt.y;
  end;

begin
  if weight=1 then exit(BezierCurve(p1,c,p2).GetBounds);
  if IsInfinite then exit(EmptyRectF);
  tList:= GetBoundingPositions(false,false);

  result.TopLeft := p1;
  result.BottomRight := p1;
  Include(p2);

  p2_ := p2-p1; c_ := c-p1; //translation with -p1
  B_ := 2*weight*c_; A_ := p2_-B_;
  a := 2*(1-weight);

  for i := 0 to high(tList) do
  begin
    t := tList[i];
    Include( p1+t*(B_+t*A_)*(1/(1+t*(-a+t*a))) );
  end;
end;

function TRationalQuadraticBezierCurve.ComputeLength(AAcceptedDeviation: single): single;
var  i: Integer;
     curCoord,nextCoord: TPointF;
     pts: ArrayOfTPointF;
begin
  if weight = 1 then exit(BezierCurve(p1,c,p2).ComputeLength);
  if weight <= -1 then exit(EmptySingle); // no bounds in this case
  pts := InternalComputePoints(EmptyRectF, AAcceptedDeviation, true);
  curCoord := p1; result:=0;
  for i := 1 to high(pts) do
  begin
    nextCoord := pts[i];
    if (nextCoord <> EmptyPointF) and (curCoord <> EmptyPointF) then
       result += VectLen(nextCoord-curCoord);
    curCoord := nextCoord;
  end;
  finalize(pts)
end;

function TRationalQuadraticBezierCurve.ToPoints(AAcceptedDeviation: single;
  AIncludeFirstPoint: boolean): ArrayOfTPointF;
begin
  result := ToPoints(RectF(-64,-64, 16384, 16384), AAcceptedDeviation, AIncludeFirstPoint);
end;

procedure TRationalQuadraticBezierCurve.Split(out ALeft, ARight: TRationalQuadraticBezierCurve);
const precision=1E-6;
var M, D, E, H, c1, c2: TPointF;
    alpha, sg, w: single;

  function Intersec(): TPointF; //dichotomie
  var t, t1, t2: single;
      U, V: TPointF;
  begin
    t1 := 0; t2 := 0.5; U := E-c1;
    if VectDet(U,p1-c1)>0 then sg := 1 else sg := -1;
    while (t2-t1) > precision do //19 iterations
    begin
      t := (t1+t2)/2;
      V := ComputePointAt(t)-c1;
      if VectDet(U,V)*sg>0 then t1 := t else t2 := t;
    end;
    result := ComputePointAt((t1+t2)/2)
  end;

begin
  if IsInfinite then raise exception.Create('Cannot split an infinite curve');

  M := ComputePointAt(0.5);
  ALeft.p1 := p1;
  ALeft.p2 := M;
  ARight.p1 := M;
  ARight.p2 := p2;
  ALeft.weight := 1;
  ARight.weight := 1;
  D := 0.5*(p1+p2);
  if (weight = 1) or (D = c) then
  begin
    ALeft.c := 0.5*(p1+c);
    ARight.c := 0.5*(p2+c);
    exit;
  end;
  if weight > 0 then
    alpha := VectLen(D-M)/VectLen(D-c)
  else
    alpha := -VectLen(D-M)/VectLen(D-c);
  c1 := p1 + alpha*(c-p1);
  c2 := p2 + alpha*(c-p2);
  ALeft.c := c1;
  ARight.c := c2;
  E := 0.5*(p1+M);
  H := Intersec(); //between [c1;E] and the curve
  w := VectLen(E-c1)/VectLen(H-c1)-1; // new weight
  ALeft.weight := w;
  ARight.weight := w;
end;

{ TEasyBezierCurve }

function EasyBezierCurve(APoints: array of TPointF; AClosed: boolean;
  ACurveMode: TEasyBezierCurveMode; AMinimumDotProduct: single): TEasyBezierCurve;
begin
  result.Init;
  result.SetPoints(APoints, ACurveMode);
  result.Closed:= AClosed;
  result.MinimumDotProduct:= AMinimumDotProduct;
end;

function EasyBezierCurve(APoints: array of TPointF; AClosed: boolean;
  ACurveMode: array of TEasyBezierCurveMode;
  AMinimumDotProduct: single = EasyBezierDefaultMinimumDotProduct): TEasyBezierCurve;
begin
  result.Init;
  result.SetPoints(APoints, ACurveMode);
  result.Closed:= AClosed;
  result.MinimumDotProduct:= AMinimumDotProduct;
end;

procedure TEasyBezierCurve.CopyToPath(ADest: IBGRAPath; ATransformFunc: TEasyBezierPointTransformFunc; ATransformData: Pointer);
var i: integer;
  nextMove: boolean;
  pt,startCoord: TPointF;
begin
  if PointCount = 0 then exit;
  if (FCurves = nil) or FInvalidated then ComputeQuadraticCurves;
  nextMove := true;

  for i := 0 to PointCount-1 do
  begin
    pt := Point[i];
    if isEmptyPointF(pt) then
    begin
      if not nextMove and FClosed then ADest.closePath;
      nextMove := true;
    end else
    begin
      with FCurves[i] do
      begin
        if nextMove then
        begin
          if not isCurvedToPrevious then
            startCoord := pt
          else
            startCoord := Center;
          ADest.moveTo(ATransformFunc(@startCoord,ATransformData));
          nextMove := false;
        end else
          if not isCurvedToPrevious then
            ADest.lineTo(ATransformFunc(@pt,ATransformData));

        if isCurvedToNext then
        begin
          if not isCurvedToPrevious then ADest.lineTo(ATransformFunc(@Center,ATransformData));
          ADest.quadraticCurveTo(ATransformFunc(@ControlPoint,ATransformData),ATransformFunc(@NextCenter,ATransformData));
        end;
      end;
    end;
  end;
  if not nextmove and FClosed then ADest.closePath;
end;

function TEasyBezierCurve.ToPoints: ArrayOfTPointF;
var p: TBGRACustomPath;
begin
  if not Assigned(BGRAPathFactory) then raise exception.Create('BGRAPath unit needed');
  p := BGRAPathFactory.Create;
  CopyToPath(p);
  result := p.getPoints;
  p.Free;
end;

function TEasyBezierCurve.ComputeLength: single;
var p: TBGRACustomPath;
begin
  if not Assigned(BGRAPathFactory) then raise exception.Create('BGRAPath unit needed');
  p := BGRAPathFactory.Create;
  CopyToPath(p);
  result := p.getLength;
  p.Free;
end;

procedure TEasyBezierCurve.CopyToPath(ADest: IBGRAPath);
begin
  CopyToPath(ADest, @PointTransformNone, nil);
end;

procedure TEasyBezierCurve.CopyToPath(ADest: IBGRAPath; AOffset: TPointF);
begin
  CopyToPath(ADest, @PointTransformOffset, @AOffset);
end;

procedure TEasyBezierCurve.ComputeQuadraticCurves;
var
  i,FirstPointIndex,NextPt,NextPt2: integer;
begin
  setlength(FCurves, PointCount);
  FirstPointIndex := 0;
  for i := 0 to PointCount-1 do
    FCurves[i].isCurvedToPrevious := false;
  for i := 0 to PointCount-1 do
  begin
    FCurves[i].isCurvedToNext := false;
    FCurves[i].Center := EmptyPointF;
    FCurves[i].ControlPoint := EmptyPointF;
    FCurves[i].NextCenter := EmptyPointF;

    if IsEmptyPointF(Point[i]) then
    begin
      FirstPointIndex := i+1;
    end else
    begin
      NextPt := i+1;
      if (NextPt = PointCount) or isEmptyPointF(Point[NextPt]) then NextPt := FirstPointIndex;
      NextPt2 := NextPt+1;
      if (NextPt2 = PointCount) or isEmptyPointF(Point[NextPt2]) then NextPt2 := FirstPointIndex;

      FCurves[i].Center := (Point[i]+Point[NextPt])*0.5;
      FCurves[i].NextCenter := (Point[NextPt]+Point[NextPt2])*0.5;
      FCurves[i].ControlPoint := Point[NextPt];

      if (i < PointCount-2) or FClosed then
      begin
        case CurveMode[nextPt] of
          cmAuto: FCurves[i].isCurvedToNext:= MaybeCurve(i,NextPt,NextPt,NextPt2);
          cmCurve: FCurves[i].isCurvedToNext:= true;
          else FCurves[i].isCurvedToNext:= false;
        end;
        FCurves[NextPt].isCurvedToPrevious := FCurves[i].isCurvedToNext;
      end;
    end;
  end;
  FInvalidated:= false;
end;

function TEasyBezierCurve.PointTransformNone(APoint: PPointF; AData: Pointer): TPointF;
begin
  result := APoint^;
end;

function TEasyBezierCurve.PointTransformOffset(APoint: PPointF; AData: Pointer): TPointF;
begin
  result := APoint^ + PPointF(AData)^;
end;

procedure TEasyBezierCurve.Init;
begin
  FClosed := false;
  FMinimumDotProduct:= EasyBezierDefaultMinimumDotProduct;
  FPoints := nil;
  FInvalidated := true;
end;

procedure TEasyBezierCurve.Clear;
begin
  FPoints := nil;
end;

procedure TEasyBezierCurve.SetPoints(APoints: array of TPointF;
  ACurveMode: TEasyBezierCurveMode);
var
  i: Integer;
begin
  setlength(FPoints, length(APoints));
  for i := 0 to high(APoints) do
  begin
    FPoints[i].Coord := APoints[i];
    FPoints[i].CurveMode:= ACurveMode;
  end;
  FInvalidated:= true;
end;

procedure TEasyBezierCurve.SetPoints(APoints: array of TPointF;
  ACurveMode: array of TEasyBezierCurveMode);
var
  i,j: Integer;
begin
  setlength(FPoints, length(APoints));
  j := 0;
  for i := 0 to high(APoints) do
  begin
    FPoints[i].Coord := APoints[i];
    if length(ACurveMode) = 0 then
      FPoints[i].CurveMode:= cmAuto
    else
    begin
      FPoints[i].CurveMode:= ACurveMode[j];
      inc(j);
      if j = length(ACurveMode) then j := 0;
    end;
  end;
  FInvalidated:= true;
end;

function TEasyBezierCurve.GetCurveMode(AIndex: integer): TEasyBezierCurveMode;
begin
  if (AIndex < 0) or (AIndex >= PointCount) then raise exception.Create('Index out of bounds');
  result:= FPoints[AIndex].CurveMode;
end;

function TEasyBezierCurve.GetCurveStartPoint: TPointF;
begin
  if (PointCount=0) or isEmptyPointF(Point[0]) then exit(EmptyPointF);
  if FInvalidated or (FCurves = nil) then ComputeQuadraticCurves;
  if not FCurves[0].isCurvedToPrevious then
    result := Point[0]
  else
    result := FCurves[0].Center;
end;

function TEasyBezierCurve.GetPoint(AIndex: integer): TPointF;
begin
  if (AIndex < 0) or (AIndex >= PointCount) then raise exception.Create('Index out of bounds');
  result:= FPoints[AIndex].Coord;
end;

function TEasyBezierCurve.GetPointCount: integer;
begin
  result:= length(FPoints);
end;

procedure TEasyBezierCurve.SetClosed(AValue: boolean);
begin
  if FClosed=AValue then Exit;
  FClosed:=AValue;
  FInvalidated:= true;
end;

procedure TEasyBezierCurve.SetCurveMode(AIndex: integer;
  AValue: TEasyBezierCurveMode);
begin
  if (AIndex < 0) or (AIndex >= PointCount) then raise exception.Create('Index out of bounds');
  if FPoints[AIndex].CurveMode = AValue then exit;
  FPoints[AIndex].CurveMode := AValue;
  FInvalidated:= true;
end;

procedure TEasyBezierCurve.SetMinimumDotProduct(AValue: single);
begin
  if FMinimumDotProduct=AValue then Exit;
  FMinimumDotProduct:=AValue;
  FInvalidated:= true;
end;

procedure TEasyBezierCurve.SetPoint(AIndex: integer; AValue: TPointF);
begin
  if (AIndex < 0) or (AIndex >= PointCount) then raise exception.Create('Index out of bounds');
  if FPoints[AIndex].Coord = AValue then exit;
  FPoints[AIndex].Coord := AValue;
  FInvalidated:= true;
end;

function TEasyBezierCurve.MaybeCurve(start1,end1,start2,end2: integer): boolean;
var
  u,v: TPointF;
  lu,lv: single;
begin
  if (start1=-1) or (end1=-1) or (start2=-1) or (end2=-1) then
  begin
    result := false;
    exit;
  end;
  u := pointF(Point[end1].x - Point[start1].x, Point[end1].y - Point[start1].y);
  lu := sqrt(u*u);
  if lu <> 0 then u *= 1/lu;
  v := pointF(Point[end2].x - Point[start2].x, Point[end2].y - Point[start2].y);
  lv := sqrt(v*v);
  if lv <> 0 then v *= 1/lv;

  result := u*v > FMinimumDotProduct;
end;

{$ENDIF}
