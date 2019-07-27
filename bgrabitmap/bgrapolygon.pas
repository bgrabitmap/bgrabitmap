unit BGRAPolygon;

{$mode objfpc}{$H+}

{ This unit contains polygon drawing functions and spline functions.

  Shapes are drawn using a TBGRACustomFillInfo object, which calculates the
  intersection of an horizontal line and the polygon.

  Various shapes are handled :
  - TFillPolyInfo : polygon scanned in any order
  - TSimpleFillPolyInfo : polygon with few points
  - TOnePassFillPolyInfo : polygon scanned from top to bottom
  - TFillEllipseInfo : ellipse
  - TFillBorderEllipseInfo : ellipse border
  - TFillRoundRectangleInfo : round rectangle (or other corners)
  - TFillBorderRoundRectInfo : round rectangle border

  Various fill modes :
  - Alternate : each time there is an intersection, it enters or go out of the polygon
  - Winding : filled when the sum of ascending and descending intersection is non zero
  - Color : fill with a color defined as a TBGRAPixel argument
  - Erase : erase with an alpha in the TBGRAPixel argument
  - Texture : draws a texture with the IBGRAScanner argument

  Various border handling :
  - aliased : one horizontal line intersection is calculated per pixel in the vertical loop
  - antialiased : more lines are calculated and a density is computed by adding them together
  - multi-polygon antialiasing and superposition (TBGRAMultiShapeFiller) : same as above but
    by combining multiple polygons at the same time, and optionally subtracting top polygons
  }

interface

uses
  Classes, SysUtils, BGRAGraphics, BGRABitmapTypes, BGRAFillInfo, BGRAPath;

procedure FillShapeAliased(bmp: TBGRACustomBitmap; shapeInfo: TBGRACustomFillInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode; AliasingIncludeBottomRight: Boolean= false);
procedure FillShapeAliased(bmp: TCustomUniversalBitmap; shapeInfo: TBGRACustomFillInfo;
  brush: TUniversalBrush; Alpha: Word; NonZeroWinding: boolean; AliasingIncludeBottomRight: Boolean= false);
procedure FillPolyAliased(bmp: TBGRACustomBitmap; points: array of TPointF;
  c: TBGRAPixel; EraseMode: boolean; NonZeroWinding: boolean; drawmode: TDrawMode; APixelCenteredCoordinates: boolean = true);
procedure FillPolyAliasedWithTexture(bmp: TBGRACustomBitmap; const points: array of TPointF;
  scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode; APixelCenteredCoordinates: boolean = true);
procedure FillPolyAliased(bmp: TCustomUniversalBitmap; const points: array of TPointF;
  brush: TUniversalBrush; Alpha: Word; NonZeroWinding: boolean; APixelCenteredCoordinates: boolean = true);

procedure FillShapeAntialias(bmp: TBGRACustomBitmap; shapeInfo: TBGRACustomFillInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean; LinearBlend: boolean = false);
procedure FillShapeAntialias(bmp: TBGRACustomBitmap; shapeInfo: TBGRACustomFillInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode);
procedure FillShapeAntialias(bmp: TCustomUniversalBitmap; shapeInfo: TBGRACustomFillInfo;
  brush: TUniversalBrush; NonZeroWinding: boolean);
procedure FillShapeAntialiasWithTexture(bmp: TBGRACustomBitmap; shapeInfo: TBGRACustomFillInfo;
  scan: IBGRAScanner; NonZeroWinding: boolean; LinearBlend: boolean = false);
procedure FillPolyAntialias(bmp: TBGRACustomBitmap; points: array of TPointF;
  c: TBGRAPixel; EraseMode: boolean; NonZeroWinding: boolean; LinearBlend: boolean = false; APixelCenteredCoordinates: boolean = true);
procedure FillPolyAntialiasWithTexture(bmp: TBGRACustomBitmap; const points: array of TPointF;
  scan: IBGRAScanner; NonZeroWinding: boolean; LinearBlend: boolean = false; APixelCenteredCoordinates: boolean = true);
procedure FillPolyAntialias(bmp: TCustomUniversalBitmap; const points: array of TPointF;
  brush: TUniversalBrush; NonZeroWinding: boolean; APixelCenteredCoordinates: boolean = true);

type

  { TBGRAMultishapeFiller }

  TBGRAMultishapeFiller = class
  protected
    nbShapes: integer;
    shapes: array of record
        info: TBGRACustomFillInfo;
        internalInfo: boolean;
        texture: IBGRAScanner;
        internalTexture: TObject;
        color: TExpandedPixel;
        bounds: TRect;
        fillMode: TFillMode;
        fillModeOverride: boolean;
      end;
    function AddShape(AInfo: TBGRACustomFillInfo; AInternalInfo: boolean; ATexture: IBGRAScanner; AInternalTexture: TObject; AColor: TBGRAPixel): integer; overload;
    function CheckRectangleBorderBounds(var x1, y1, x2, y2: single; w: single): boolean;
    procedure InternalAddStroke(const APoints: array of TPointF; AClosed: boolean; AData: Pointer);
  public
    FillMode : TFillMode;
    PolygonOrder: TPolygonOrder;
    Antialiasing: Boolean;
    AliasingIncludeBottomRight: Boolean;
    constructor Create;
    destructor Destroy; override;
    function AddShape(AShape: TBGRACustomFillInfo; AColor: TBGRAPixel): integer; overload;
    function AddShape(AShape: TBGRACustomFillInfo; ATexture: IBGRAScanner): integer; overload;
    function AddPolygon(const points: array of TPointF; AColor: TBGRAPixel): integer; overload;
    function AddPolygon(const points: array of TPointF; ATexture: IBGRAScanner): integer; overload;
    procedure AddPathStroke(APath: TBGRAPath; AColor: TBGRAPixel; AWidth: single; AStroker: TBGRACustomPenStroker); overload;
    procedure AddPathStroke(APath: TBGRAPath; ATexture: IBGRAScanner; AWidth: single; AStroker: TBGRACustomPenStroker); overload;
    procedure AddPathStroke(APath: TBGRAPath; AMatrix: TAffineMatrix; AColor: TBGRAPixel; AWidth: single; AStroker: TBGRACustomPenStroker); overload;
    procedure AddPathStroke(APath: TBGRAPath; AMatrix: TAffineMatrix; ATexture: IBGRAScanner; AWidth: single; AStroker: TBGRACustomPenStroker); overload;
    function AddPathFill(APath: TBGRAPath; AColor: TBGRAPixel): integer; overload;
    function AddPathFill(APath: TBGRAPath; ATexture: IBGRAScanner): integer; overload;
    function AddPathFill(APath: TBGRAPath; AMatrix: TAffineMatrix; AColor: TBGRAPixel): integer; overload;
    function AddPathFill(APath: TBGRAPath; AMatrix: TAffineMatrix; ATexture: IBGRAScanner): integer; overload;
    function AddPolylineStroke(const points: array of TPointF; AColor: TBGRAPixel; AWidth: single; AStroker: TBGRACustomPenStroker): integer; overload;
    function AddPolylineStroke(const points: array of TPointF; ATexture: IBGRAScanner; AWidth: single; AStroker: TBGRACustomPenStroker): integer; overload;
    function AddPolygonStroke(const points: array of TPointF; AColor: TBGRAPixel; AWidth: single; AStroker: TBGRACustomPenStroker): integer; overload;
    function AddPolygonStroke(const points: array of TPointF; ATexture: IBGRAScanner; AWidth: single; AStroker: TBGRACustomPenStroker): integer; overload;
    function AddTriangleLinearColor(pt1, pt2, pt3: TPointF; c1, c2, c3: TBGRAPixel): integer;
    function AddTriangleLinearMapping(pt1, pt2, pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF): integer;
    procedure AddQuadLinearColor(pt1, pt2, pt3, pt4: TPointF; c1, c2, c3, c4: TBGRAPixel);
    procedure AddQuadLinearMapping(pt1, pt2, pt3, pt4: TPointF; texture: IBGRAScanner; tex1, tex2, {%H-}tex3, tex4: TPointF;
       ACulling: TFaceCulling = fcNone);
    procedure AddQuadPerspectiveMapping(pt1, pt2, pt3, pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF);
    function AddEllipse(x, y, rx, ry: single; AColor: TBGRAPixel): integer; overload;
    function AddEllipse(x, y, rx, ry: single; ATexture: IBGRAScanner): integer; overload;
    function AddEllipseBorder(x, y, rx, ry, w: single; AColor: TBGRAPixel): integer; overload;
    function AddEllipseBorder(x, y, rx, ry, w: single; ATexture: IBGRAScanner): integer; overload;
    function AddRoundRectangle(x1, y1, x2, y2, rx, ry: single; AColor: TBGRAPixel; options: TRoundRectangleOptions= []): integer; overload;
    function AddRoundRectangle(x1, y1, x2, y2, rx, ry: single; ATexture: IBGRAScanner; options: TRoundRectangleOptions= []): integer; overload;
    function AddRoundRectangleBorder(x1, y1, x2, y2, rx, ry, w: single; AColor: TBGRAPixel; options: TRoundRectangleOptions= []): integer; overload;
    function AddRoundRectangleBorder(x1, y1, x2, y2, rx, ry, w: single; ATexture: IBGRAScanner; options: TRoundRectangleOptions= []): integer; overload;
    function AddRectangle(x1, y1, x2, y2: single; AColor: TBGRAPixel): integer; overload;
    function AddRectangle(x1, y1, x2, y2: single; ATexture: IBGRAScanner): integer; overload;
    function AddRectangleBorder(x1, y1, x2, y2, w: single; AColor: TBGRAPixel): integer; overload;
    function AddRectangleBorder(x1, y1, x2, y2, w: single; ATexture: IBGRAScanner): integer; overload;
    procedure OverrideFillMode(AShapeIndex: integer; AFillMode: TFillMode);
    procedure Draw(dest: TBGRACustomBitmap; ADrawMode: TDrawMode = dmDrawWithTransparency);
    property ShapeCount: integer read nbShapes;
  end;

procedure FillEllipseAntialias(bmp: TCustomUniversalBitmap; x, y, rx, ry: single; ABrush: TUniversalBrush);
procedure FillEllipseAntialias(bmp: TBGRACustomBitmap; x, y, rx, ry: single;
  c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean = false);
procedure FillEllipseAntialiasWithTexture(bmp: TBGRACustomBitmap; x, y, rx, ry: single;
  scan: IBGRAScanner; LinearBlend: boolean = false);

procedure BorderEllipseAntialias(bmp: TCustomUniversalBitmap; x, y, rx, ry, w: single; ABrush: TUniversalBrush);
procedure BorderEllipseAntialias(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean = false);
procedure BorderEllipseAntialiasWithTexture(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  scan: IBGRAScanner; LinearBlend: boolean = false);

procedure BorderEllipse(bmp: TCustomUniversalBitmap; x, y, rx, ry, w: single; ABrush: TUniversalBrush; AAlpha: word = 65535);
procedure BorderEllipse(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  c: TBGRAPixel; EraseMode: boolean; drawmode: TDrawMode);
procedure BorderEllipseWithTexture(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  scan: IBGRAScanner; drawmode: TDrawMode);

procedure FillRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry: single;
  options: TRoundRectangleOptions; c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean = false; APixelCenteredCoordinates: boolean = true);
procedure FillRoundRectangleAntialiasWithTexture(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry: single;
  options: TRoundRectangleOptions; scan: IBGRAScanner; LinearBlend: boolean = false; APixelCenteredCoordinates: boolean = true);

procedure BorderRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry, w: single;
  options: TRoundRectangleOptions; c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean = false; APixelCenteredCoordinates: boolean = true);
procedure BorderRoundRectangleAntialiasWithTexture(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry, w: single;
  options: TRoundRectangleOptions; scan: IBGRAScanner; LinearBlend: boolean = false; APixelCenteredCoordinates: boolean = true);

procedure BorderAndFillRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry, w: single;
  options: TRoundRectangleOptions; bordercolor,fillcolor: TBGRAPixel; bordertexture,filltexture: IBGRAScanner; EraseMode: boolean; APixelCenteredCoordinates: boolean = true);

implementation

uses Math, BGRABlend, BGRAGradientScanner, BGRATransform;

procedure AnyBrush(out ABrush: TUniversalBrush; ABmp: TBGRACustomBitmap; ACol: TBGRAPixel;
   AEraseMode: boolean; AScan: IBGRAScanner; ADrawmode: TDrawMode);
begin
  if AScan <> nil then
    ABmp.ScannerBrush(ABrush, AScan, ADrawmode)
  else
  begin
    if AEraseMode then ABmp.EraseBrush(ABrush, ACol.alpha + (ACol.alpha shl 8))
    else ABmp.SolidBrush(ABrush, ACol, ADrawmode);;
  end;
end;

procedure AnyBrush(out ABrush: TUniversalBrush; ABmp: TBGRACustomBitmap; ACol: TBGRAPixel;
   AEraseMode: boolean; AScan: IBGRAScanner; ALinearBlend: boolean);
begin
  if ALinearBlend then AnyBrush(ABrush,ABmp,ACol,AEraseMode,AScan,dmLinearBlend)
  else AnyBrush(ABrush,ABmp,ACol,AEraseMode,AScan,dmDrawWithTransparency);
end;

procedure FillShapeAliased(bmp: TBGRACustomBitmap; shapeInfo: TBGRACustomFillInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode; AliasingIncludeBottomRight: Boolean= false);
var
  bFill: TUniversalBrush;
begin
  AnyBrush(bFill, bmp, c,EraseMode,scan,drawmode);
  FillShapeAliased(bmp, shapeInfo, bFill,65535,NonZeroWinding,AliasingIncludeBottomRight);
end;

procedure FillShapeAliased(bmp: TCustomUniversalBitmap;
  shapeInfo: TBGRACustomFillInfo; brush: TUniversalBrush; Alpha: Word;
  NonZeroWinding: boolean; AliasingIncludeBottomRight: Boolean);
var
  inter:    array of TIntersectionInfo;
  nbInter:  integer;

  miny, maxy, minx, maxx: integer;
  yb, i: integer;
  x1, x2: single;
  ix1, ix2: integer;
  pdest: PByte;
  AliasingOfs: TPointF;
  ctx: TUniBrushContext;

begin
  if brush.DoesNothing or (Alpha=0) then exit;
  If not BGRAShapeComputeMinMax(shapeInfo,minx,miny,maxx,maxy,bmp.ClipRect) then exit;
  inter := shapeInfo.CreateIntersectionArray;

  if AliasingIncludeBottomRight then
    AliasingOfs := PointF(0.0001, 0.0001) else
    AliasingOfs := PointF(0,0);
  bmp.LoadFromBitmapIfNeeded;

  //vertical scan
  for yb := miny to maxy do
  begin
    //find intersections
    shapeInfo.ComputeAndSort( yb+0.5-AliasingOfs.Y, inter, nbInter, NonZeroWinding);

    for i := 0 to nbinter div 2 - 1 do
    begin
      x1 := inter[i + i].interX+AliasingOfs.X;
      x2 := inter[i + i+ 1].interX+AliasingOfs.X;

      if x1 <> x2 then
      begin
        ComputeAliasedRowBounds(x1,x2, minx,maxx, ix1,ix2);
        if ix1 <= ix2 then
        begin
          //render scanline
          pdest := bmp.GetPixelAddress(ix1,yb);
          brush.MoveTo(@ctx, pdest, ix1,yb);
          brush.PutNextPixels(@ctx, Alpha, ix2-ix1+1);
        end;
      end;
    end;
  end;

  shapeInfo.FreeIntersectionArray(inter);
  bmp.InvalidateBitmap;
end;

procedure FillPolyAliased(bmp: TBGRACustomBitmap; points: array of TPointF;
  c: TBGRAPixel; EraseMode: boolean; NonZeroWinding: boolean; drawmode: TDrawMode; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  AnyBrush(b, bmp, c,EraseMode,nil,drawmode);
  FillPolyAliased(bmp, points, b, 65535, NonZeroWinding, APixelCenteredCoordinates);
end;

procedure FillPolyAliasedWithTexture(bmp: TBGRACustomBitmap;
  const points: array of TPointF; scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  AnyBrush(b, bmp, BGRAPixelTransparent,false,scan,drawmode);
  FillPolyAliased(bmp, points, b, 65535, NonZeroWinding, APixelCenteredCoordinates);
end;

procedure FillPolyAliased(bmp: TCustomUniversalBitmap;
  const points: array of TPointF; brush: TUniversalBrush; Alpha: Word;
  NonZeroWinding: boolean; APixelCenteredCoordinates: boolean);
var
  info: TCustomFillPolyInfo;
begin
  if brush.DoesNothing or (length(points) < 3) then exit;
  if length(points)<=10 then
    info := TSimpleFillPolyInfo.Create(points, APixelCenteredCoordinates)
  else
    info := TOnePassFillPolyInfo.Create(points, APixelCenteredCoordinates);
  FillShapeAliased(bmp, info, brush, Alpha, NonZeroWinding);
  info.Free;
end;

//////////////////////////////////////////////////////////////////////////////

procedure FillShapeAntialias(bmp: TBGRACustomBitmap; shapeInfo: TBGRACustomFillInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean; LinearBlend: boolean = false);
var
  drawMode: TDrawMode;
begin
  if LinearBlend then drawMode := dmLinearBlend else drawMode := dmDrawWithTransparency;
  FillShapeAntialias(bmp, shapeInfo, c, EraseMode, scan, NonZeroWinding, drawmode);
end;

procedure FillShapeAntialias(bmp: TBGRACustomBitmap;
  shapeInfo: TBGRACustomFillInfo; c: TBGRAPixel; EraseMode: boolean;
  scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode);
var
  bFill: TUniversalBrush;
begin
  AnyBrush(bFill, bmp, c,EraseMode,scan,drawmode);
  FillShapeAntialias(bmp, shapeInfo, bFill,NonZeroWinding);
end;

procedure FillShapeAntialias(bmp: TCustomUniversalBitmap;
  shapeInfo: TBGRACustomFillInfo; brush: TUniversalBrush;
  NonZeroWinding: boolean);
const oneOver512 = 1/512;
var
  inter:   array of TIntersectionInfo;
  nbInter: integer;

  firstScan, lastScan: record
    inter:   array of TIntersectionInfo;
    nbInter: integer;
    sliceIndex: integer;
  end;

  miny, maxy, minx, maxx,
  densMinX, densMaxX: integer;
  joinDensity, nextJoinDensity: boolean;

  density: PDensity;

  xb, yb, yc, i: integer;
  tempDensity: UInt32or64;

  x1, x2, x1b,x2b: single;
  ix1, ix2, drawCount: integer;
  pdens:    PDensity;

  curvedSeg,optimised: boolean;
  temp: Single;
  pDest: PByte;
  ctx: TUniBrushContext;

  function GetYScan(num: integer): single; inline;
  begin
    result := yb + (num * 2 + 1) / (AntialiasPrecision * 2);
  end;

  procedure SubTriangleDensity(x1,density1, x2, density2: single);
  var ix1,ix2,n: integer;
      slope: single;
    function densityAt(x: single): single; inline;
    begin
      result := (x-x1)*slope+density1;
    end;
  var
      curdens: single;
      pdens: pdensity;
      newvalue: Int32or64;
  begin
    if (x1 <> x2) and (x1 < maxx + 1) and (x2 >= minx) then
    begin
      slope := (density2-density1)/(x2-x1);
      if x1 < minx then
      begin
        density1 := densityAt(minx);
        x1 := minx;
      end;
      if x2 >= maxx + 1 then
      begin
        density2 := densityAt(maxx+1);
        x2 := maxx + 1;
      end;
      ix1  := floor(x1);
      ix2  := floor(x2);

      if ix1 = ix2 then
      begin
        newValue := (density + (ix1 - minx))^ - round((x2 - x1)*(density1+density2)/2);
        if newValue < 0 then newValue := 0;
        if newValue > 256 then newValue := 256;
        (density + (ix1 - minx))^ := newValue
      end
      else
      begin
        newValue := (density + (ix1 - minx))^ - round((1 - (x1 - ix1))*(density1+densityAt(ix1+1))/2) ;
        if newValue < 0 then newValue := 0;
        if newValue > 256 then newValue := 256;
        (density + (ix1 - minx))^ := newValue;
        if (ix2 <= maxx) then
        begin
          newValue := (density + (ix2 - minx))^ - round((x2 - ix2)*(density2+densityAt(ix2))/2);
          if newValue < 0 then newValue := 0;
          if newValue > 256 then newValue := 256;
          (density + (ix2 - minx))^ := newValue;
        end;
      end;
      if ix2 > ix1 + 1 then
      begin
        curdens := densityAt(ix1+1.5);
        pdens := density + (ix1+1 - minx);
        for n := ix2-1-(ix1+1) downto 0 do
        begin
          newValue := pdens^ - round(curdens);
          if newValue < 0 then newValue := 0;
          if newValue > 256 then newValue := 256;
          pdens^ := newValue;
          curdens += slope;
          inc(pdens);
        end;
      end;
    end;
  end;

begin
  If brush.DoesNothing or not BGRAShapeComputeMinMax(shapeInfo,minx,miny,maxx,maxy,bmp.ClipRect) then exit;
  bmp.LoadFromBitmapIfNeeded;

  inter := shapeInfo.CreateIntersectionArray;
  getmem(density, (maxx - minx + 2)*sizeof(TDensity)); //more for safety

  curvedSeg := shapeInfo.SegmentsCurved;
  if not curvedSeg then
  begin
    firstScan.inter := shapeInfo.CreateIntersectionArray;
    lastScan.inter := shapeInfo.CreateIntersectionArray;
  end;

  //vertical scan
  for yb := miny to maxy do
  begin
    //mean density
    fillchar(density^,(maxx-minx+1)*sizeof(TDensity),0);

    densMinX := maxx+1;
    densMaxX := minx-1;

    if not curvedSeg then
    begin
      with firstScan do
      begin
        shapeInfo.ComputeAndSort(yb+1/256,inter,nbInter,NonZeroWinding);
        sliceIndex:= shapeInfo.GetSliceIndex;
      end;
      with lastScan do
      begin
        shapeInfo.ComputeAndSort(yb+255/256,inter,nbInter,NonZeroWinding);
        sliceIndex:= shapeInfo.GetSliceIndex;
      end;
      if (firstScan.sliceIndex = lastScan.sliceIndex) and (firstScan.nbInter = lastScan.nbInter) then
      begin
        optimised := true;
        for i := 0 to firstScan.nbInter-1 do
          if firstScan.inter[i].numSegment <> lastScan.inter[i].numSegment then
          begin
            optimised := false;
            break;
          end;
      end else
        optimised := false;

      if optimised then
      begin
        nextJoinDensity := false;
        for i := 0 to firstScan.nbinter div 2 - 1 do
        begin
          joinDensity := nextJoinDensity;
          x1 := firstScan.inter[i+i].interX;
          x1b := lastScan.inter[i+i].interX;
          x2 := firstScan.inter[i+i+1].interX;
          x2b := lastScan.inter[i+i+1].interX;
          nextJoinDensity := not ((i+i+2 >= firstScan.nbInter) or
              ((firstScan.inter[i+i+2].interX >= x2+1) and
               (lastScan.inter[i+i+2].interX >= x2b+1)));
          if (abs(x1-x1b)<oneOver512) and (abs(x2-x2b)<oneOver512) and
              not joinDensity and not nextJoinDensity then
          begin
            x1 := (x1+x1b)*0.5;
            x2 := (x2+x2b)*0.5;

            if x1 < minx then x1 := minx;
            ix1 := floor(x1);

            if x2 >= maxx+1 then
            begin
              x2 := maxx+1;
              ix2 := maxx;
            end else
              ix2 := floor(x2);
            if ix2 > maxx then ix2 := maxx;

            if ix1>ix2 then continue;
            pDest := bmp.GetPixelAddress(ix1,yb);
            brush.MoveTo(@ctx,pDest,ix1,yb);
            if ix1=ix2 then
            begin
              tempDensity:= round((x2-x1)*65535);
              brush.PutNextPixels(@ctx,tempDensity,1);
            end else
            begin
              tempDensity:= round((ix1+1-x1)*65535);
              brush.PutNextPixels(@ctx,tempDensity,1);
              inc(ix1);

              tempDensity:= round((x2-ix2)*65535);
              if tempDensity < 65535 then
              begin
                dec(ix2);
                if ix2 >= ix1 then brush.PutNextPixels(@ctx,65535,ix2-ix1+1);
                brush.PutNextPixels(@ctx,tempDensity,1);
              end else
                brush.PutNextPixels(@ctx,65535,ix2-ix1+1);
            end;
            continue;
          end else
          begin
            if (x1 > x1b) then
            begin
              temp := x1;
              x1 := x1b;
              x1b := temp;
            end;
            if (x2 < x2b) then
            begin
              temp := x2;
              x2 := x2b;
              x2b := temp;
            end;

  	    {$DEFINE INCLUDE_FILLDENSITY}
  	    {$DEFINE PARAM_SINGLESEGMENT}
            {$i density256.inc}
            SubTriangleDensity(x1,256,x1b,0);
            SubTriangleDensity(x2b,0,x2,256);
          end;
        end;
      end else
      begin
        for yc := 0 to AntialiasPrecision - 1 do
        begin
          //find intersections
          shapeInfo.ComputeAndSort(GetYScan(yc),inter,nbInter,NonZeroWinding);

	  {$DEFINE INCLUDE_FILLDENSITY}
          {$i density256.inc}
        end;
      end;
    end else
    begin
      optimised := false;
      //precision scan
      for yc := 0 to AntialiasPrecision - 1 do
      begin
        //find intersections
        shapeInfo.ComputeAndSort(GetYScan(yc),inter,nbInter,NonZeroWinding);

	{$DEFINE INCLUDE_FILLDENSITY}
        {$i density256.inc}
      end;
    end;

    if optimised then
      {$DEFINE INCLUDE_RENDERDENSITY}
      {$i density256.inc}
    else
      {$DEFINE INCLUDE_RENDERDENSITY}
      {$define PARAM_ANTIALIASINGFACTOR}
      {$i density256.inc}
  end;

  shapeInfo.FreeIntersectionArray(inter);

  if not curvedSeg then
  begin
    with firstScan do
    begin
      for i := 0 to high(inter) do
        inter[i].free;
    end;
    with lastScan do
    begin
      for i := 0 to high(inter) do
        inter[i].free;
    end;
  end;
  freemem(density);

  bmp.InvalidateBitmap;
end;

procedure FillShapeAntialiasWithTexture(bmp: TBGRACustomBitmap;
  shapeInfo: TBGRACustomFillInfo; scan: IBGRAScanner; NonZeroWinding: boolean; LinearBlend: boolean);
begin
  FillShapeAntialias(bmp,shapeInfo,BGRAPixelTransparent,False,scan,NonZeroWinding,LinearBlend);
end;

procedure FillPolyAntialias(bmp: TBGRACustomBitmap; points: array of TPointF;
  c: TBGRAPixel; EraseMode: boolean; NonZeroWinding: boolean; LinearBlend: boolean; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  AnyBrush(b, bmp,c,EraseMode,nil,LinearBlend);
  FillPolyAntialias(bmp, points, b, NonZeroWinding, APixelCenteredCoordinates);
end;

procedure FillPolyAntialiasWithTexture(bmp: TBGRACustomBitmap;
  const points: array of TPointF; scan: IBGRAScanner; NonZeroWinding: boolean; LinearBlend: boolean; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  AnyBrush(b, bmp,BGRAPixelTransparent,false,scan,LinearBlend);
  FillPolyAntialias(bmp, points, b, NonZeroWinding, APixelCenteredCoordinates);
end;

procedure FillPolyAntialias(bmp: TCustomUniversalBitmap;
  const points: array of TPointF; brush: TUniversalBrush;
  NonZeroWinding: boolean; APixelCenteredCoordinates: boolean);
var
  info: TCustomFillPolyInfo;
begin
  if brush.DoesNothing or (length(points) < 3) then exit;
  info := TOnePassFillPolyInfo.Create(points, APixelCenteredCoordinates);
  FillShapeAntialias(bmp, info, brush, NonZeroWinding);
  info.Free;
end;

////////////////////////////////////////////////////////////////////////

{ TBGRAMultishapeFiller }

type
  TPathStrokeData = record
    Stroker: TBGRACustomPenStroker;
    Texture: IBGRAScanner;
    Color: TBGRAPixel;
    Width: Single;
  end;

function TBGRAMultishapeFiller.AddShape(AInfo: TBGRACustomFillInfo; AInternalInfo: boolean; ATexture: IBGRAScanner; AInternalTexture: TObject; AColor: TBGRAPixel): integer;
begin
  if length(shapes) = nbShapes then
    setlength(shapes, (length(shapes)+1)*2);
  result := nbShapes;
  inc(nbShapes);

  with shapes[result] do
  begin
    info := AInfo;
    internalInfo:= AInternalInfo;
    texture := ATexture;
    internalTexture:= AInternalTexture;
    color := GammaExpansion(AColor);
    fillModeOverride:= false;
  end;
end;

function TBGRAMultishapeFiller.CheckRectangleBorderBounds(var x1, y1, x2,
  y2: single; w: single): boolean;
var temp: single;
begin
  if x1 > x2 then
  begin
    temp := x1;
    x1 := x2;
    x2 := temp;
  end;
  if y1 > y2 then
  begin
    temp := y1;
    y1 := y2;
    y2 := temp;
  end;
  result := (x2-x1 > w) and (y2-y1 > w);
end;

procedure TBGRAMultishapeFiller.InternalAddStroke(
  const APoints: array of TPointF; AClosed: boolean; AData: Pointer);
var pts: ArrayOfTPointF;
  idxShape: Integer;
begin
  with TPathStrokeData(AData^) do
  begin
    if AClosed then
      pts := Stroker.ComputePolygon(APoints, Width)
    else
      pts := Stroker.ComputePolylineAutoCycle(APoints, Width);
    if Texture <> nil then
      idxShape := AddPolygon(pts, Texture)
    else
      idxShape := AddPolygon(pts, Color);
    OverrideFillMode(idxShape, fmWinding);
  end;
end;

constructor TBGRAMultishapeFiller.Create;
begin
  nbShapes := 0;
  shapes := nil;
  PolygonOrder := poNone;
  Antialiasing := True;
  AliasingIncludeBottomRight := False;
end;

destructor TBGRAMultishapeFiller.Destroy;
var
  i: Integer;
begin
  for i := 0 to nbShapes-1 do
  begin
    if shapes[i].internalInfo then shapes[i].info.free;
    shapes[i].texture := nil;
    if shapes[i].internalTexture <> nil then shapes[i].internalTexture.Free;
  end;
  shapes := nil;
  inherited Destroy;
end;

function TBGRAMultishapeFiller.AddShape(AShape: TBGRACustomFillInfo;
  AColor: TBGRAPixel): integer;
begin
  result := AddShape(AShape,False,nil,nil,AColor);
end;

function TBGRAMultishapeFiller.AddShape(AShape: TBGRACustomFillInfo;
  ATexture: IBGRAScanner): integer;
begin
  result := AddShape(AShape,False,ATexture,nil,BGRAPixelTransparent);
end;

function TBGRAMultishapeFiller.AddPolygon(const points: array of TPointF;
  AColor: TBGRAPixel): integer;
begin
  if length(points) <= 2 then exit(-1);
  result := AddShape(TOnePassFillPolyInfo.Create(points),True,nil,nil,AColor);
end;

function TBGRAMultishapeFiller.AddPolygon(const points: array of TPointF;
  ATexture: IBGRAScanner): integer;
begin
  if length(points) <= 2 then exit(-1);
  result := AddShape(TOnePassFillPolyInfo.Create(points),True,ATexture,nil,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddPathStroke(APath: TBGRAPath;
  AColor: TBGRAPixel; AWidth: single; AStroker: TBGRACustomPenStroker);
begin
  AddPathStroke(APath,AffineMatrixIdentity,AColor,AWidth,AStroker);
end;

procedure TBGRAMultishapeFiller.AddPathStroke(APath: TBGRAPath;
  ATexture: IBGRAScanner; AWidth: single; AStroker: TBGRACustomPenStroker);
begin
  AddPathStroke(APath,AffineMatrixIdentity,ATexture,AWidth,AStroker);
end;

procedure TBGRAMultishapeFiller.AddPathStroke(APath: TBGRAPath;
  AMatrix: TAffineMatrix; AColor: TBGRAPixel; AWidth: single;
  AStroker: TBGRACustomPenStroker);
var data: TPathStrokeData;
begin
  data.Stroker := AStroker;
  data.Color := AColor;
  data.Texture := nil;
  data.Width := AWidth;
  APath.stroke(@InternalAddStroke, AMatrix, 0.1, @data);
end;

procedure TBGRAMultishapeFiller.AddPathStroke(APath: TBGRAPath;
  AMatrix: TAffineMatrix; ATexture: IBGRAScanner; AWidth: single;
  AStroker: TBGRACustomPenStroker);
var data: TPathStrokeData;
begin
  data.Stroker := AStroker;
  data.Color := BGRAPixelTransparent;
  data.Texture := ATexture;
  data.Width := AWidth;
  APath.stroke(@InternalAddStroke, AMatrix, 0.1, @data);
end;

function TBGRAMultishapeFiller.AddPathFill(APath: TBGRAPath; AColor: TBGRAPixel): integer;
begin
  result := AddPolygon(APath.ToPoints, AColor);
end;

function TBGRAMultishapeFiller.AddPathFill(APath: TBGRAPath;
  ATexture: IBGRAScanner): integer;
begin
  result := AddPolygon(APath.ToPoints, ATexture);
end;

function TBGRAMultishapeFiller.AddPathFill(APath: TBGRAPath;
  AMatrix: TAffineMatrix; AColor: TBGRAPixel): integer;
begin
  result := AddPolygon(APath.ToPoints(AMatrix), AColor);
end;

function TBGRAMultishapeFiller.AddPathFill(APath: TBGRAPath;
  AMatrix: TAffineMatrix; ATexture: IBGRAScanner): integer;
begin
  result := AddPolygon(APath.ToPoints(AMatrix), ATexture);
end;

function TBGRAMultishapeFiller.AddPolylineStroke(
  const points: array of TPointF; AColor: TBGRAPixel; AWidth: single;
  AStroker: TBGRACustomPenStroker): integer;
begin
  result := AddPolygon(AStroker.ComputePolyline(points,AWidth,AColor), AColor);
end;

function TBGRAMultishapeFiller.AddPolylineStroke(
  const points: array of TPointF; ATexture: IBGRAScanner; AWidth: single;
  AStroker: TBGRACustomPenStroker): integer;
begin
  result := AddPolygon(AStroker.ComputePolyline(points,AWidth), ATexture);
end;

function TBGRAMultishapeFiller.AddPolygonStroke(const points: array of TPointF;
  AColor: TBGRAPixel; AWidth: single; AStroker: TBGRACustomPenStroker): integer;
begin
  result := AddPolygon(AStroker.ComputePolygon(points,AWidth), AColor);
end;

function TBGRAMultishapeFiller.AddPolygonStroke(const points: array of TPointF;
  ATexture: IBGRAScanner; AWidth: single; AStroker: TBGRACustomPenStroker
  ): integer;
begin
  result := AddPolygon(AStroker.ComputePolygon(points,AWidth), ATexture);
end;

function TBGRAMultishapeFiller.AddTriangleLinearColor(pt1, pt2, pt3: TPointF;
  c1, c2, c3: TBGRAPixel): integer;
var grad: TBGRAGradientTriangleScanner;
begin
  if (c1 = c2) and (c2 = c3) then
    result := AddPolygon([pt1,pt2,pt3],c1)
  else
  begin
    grad := TBGRAGradientTriangleScanner.Create(pt1,pt2,pt3, c1,c2,c3);
    result := AddShape(TOnePassFillPolyInfo.Create([pt1,pt2,pt3]),True,grad,grad,BGRAPixelTransparent);
  end;
end;

function TBGRAMultishapeFiller.AddTriangleLinearMapping(pt1, pt2, pt3: TPointF;
  texture: IBGRAScanner; tex1, tex2, tex3: TPointF): integer;
var
  mapping: TBGRATriangleLinearMapping;
begin
  mapping := TBGRATriangleLinearMapping.Create(texture, pt1,pt2,pt3, tex1, tex2, tex3);
  result := AddShape(TOnePassFillPolyInfo.Create([pt1,pt2,pt3]),True,mapping,mapping,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddQuadLinearColor(pt1, pt2, pt3, pt4: TPointF;
  c1, c2, c3, c4: TBGRAPixel);
var
  center: TPointF;
  centerColor: TBGRAPixel;
begin
  if (c1 = c2) and (c2 = c3) and (c3 = c4) then
    AddPolygon([pt1,pt2,pt3,pt4],c1)
  else
  begin
    center := (pt1+pt2+pt3+pt4)*(1/4);
    centerColor := GammaCompression( MergeBGRA(MergeBGRA(GammaExpansion(c1),GammaExpansion(c2)),
                      MergeBGRA(GammaExpansion(c3),GammaExpansion(c4))) );
    AddTriangleLinearColor(pt1,pt2,center, c1,c2,centerColor);
    AddTriangleLinearColor(pt2,pt3,center, c2,c3,centerColor);
    AddTriangleLinearColor(pt3,pt4,center, c3,c4,centerColor);
    AddTriangleLinearColor(pt4,pt1,center, c4,c1,centerColor);
  end;
end;

procedure TBGRAMultishapeFiller.AddQuadLinearMapping(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF;
  ACulling: TFaceCulling);
var
  mapping: TBGRAQuadLinearScanner;
begin
  mapping := TBGRAQuadLinearScanner.Create(texture,
    [tex1,tex2,tex3,tex4],
    [pt1,pt2,pt3,pt4]);
  mapping.Culling := ACulling;
  mapping.Padding := true;
  AddShape(TOnePassFillPolyInfo.Create([pt1,pt2,pt3,pt4]),True,mapping,mapping,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddQuadPerspectiveMapping(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF);
var persp: TBGRAPerspectiveScannerTransform;
begin
  persp := TBGRAPerspectiveScannerTransform.Create(texture,[tex1,tex2,tex3,tex4],[pt1,pt2,pt3,pt4]);
  AddShape(TOnePassFillPolyInfo.Create([pt1,pt2,pt3,pt4]),True,persp,persp,BGRAPixelTransparent);
end;

function TBGRAMultishapeFiller.AddEllipse(x, y, rx, ry: single;
  AColor: TBGRAPixel): integer;
begin
  result := AddShape(TFillEllipseInfo.Create(x,y,rx,ry),True,nil,nil,AColor);
end;

function TBGRAMultishapeFiller.AddEllipse(x, y, rx, ry: single;
  ATexture: IBGRAScanner): integer;
begin
  result := AddShape(TFillEllipseInfo.Create(x,y,rx,ry),True,ATexture,nil,BGRAPixelTransparent);
end;

function TBGRAMultishapeFiller.AddEllipseBorder(x, y, rx, ry, w: single;
  AColor: TBGRAPixel): integer;
begin
  result := AddShape(TFillBorderEllipseInfo.Create(x,y,rx,ry,w),True,nil,nil,AColor);
end;

function TBGRAMultishapeFiller.AddEllipseBorder(x, y, rx, ry, w: single;
  ATexture: IBGRAScanner): integer;
begin
  result := AddShape(TFillBorderEllipseInfo.Create(x,y,rx,ry,w),True,ATexture,nil,BGRAPixelTransparent);
end;

function TBGRAMultishapeFiller.AddRoundRectangle(x1, y1, x2, y2, rx,
  ry: single; AColor: TBGRAPixel; options: TRoundRectangleOptions): integer;
begin
  result := AddShape(TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry,options),True,nil,nil,AColor);
end;

function TBGRAMultishapeFiller.AddRoundRectangle(x1, y1, x2, y2, rx,
  ry: single; ATexture: IBGRAScanner; options: TRoundRectangleOptions): integer;
begin
  result := AddShape(TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry,options),True,
     ATexture,nil,BGRAPixelTransparent);
end;

function TBGRAMultishapeFiller.AddRoundRectangleBorder(x1, y1, x2, y2, rx, ry,
  w: single; AColor: TBGRAPixel; options: TRoundRectangleOptions): integer;
begin
  result := AddShape(TFillBorderRoundRectInfo.Create(x1, y1, x2, y2, rx, ry,w,options),True,
    nil,nil,AColor);
end;

function TBGRAMultishapeFiller.AddRoundRectangleBorder(x1, y1, x2, y2, rx, ry,
  w: single; ATexture: IBGRAScanner; options: TRoundRectangleOptions): integer;
begin
  result := AddShape(TFillBorderRoundRectInfo.Create(x1, y1, x2, y2, rx, ry,w,options),True,
    ATexture,nil,BGRAPixelTransparent);
end;

function TBGRAMultishapeFiller.AddRectangle(x1, y1, x2, y2: single;
  AColor: TBGRAPixel): integer;
begin
  result := AddPolygon([PointF(x1,y1),PointF(x2,y1),PointF(x2,y2),PointF(x1,y2)],AColor);
end;

function TBGRAMultishapeFiller.AddRectangle(x1, y1, x2, y2: single;
  ATexture: IBGRAScanner): integer;
begin
  result := AddPolygon([PointF(x1,y1),PointF(x2,y1),PointF(x2,y2),PointF(x1,y2)],ATexture);
end;

function TBGRAMultishapeFiller.AddRectangleBorder(x1, y1, x2, y2, w: single;
  AColor: TBGRAPixel): integer;
var hw : single;
begin
  hw := w/2;
  if not CheckRectangleBorderBounds(x1,y1,x2,y2,w) then
    result := AddRectangle(x1-hw,y1-hw,x2+hw,y2+hw,AColor) else
    result := AddPolygon([PointF(x1-hw,y1-hw),PointF(x2+hw,y1-hw),PointF(x2+hw,y2+hw),PointF(x1-hw,y2+hw),EmptyPointF,
                PointF(x1+hw,y2-hw),PointF(x2-hw,y2-hw),PointF(x2-hw,y1+hw),PointF(x1+hw,y1+hw)],AColor);
end;

function TBGRAMultishapeFiller.AddRectangleBorder(x1, y1, x2, y2, w: single;
  ATexture: IBGRAScanner): integer;
var hw : single;
begin
  hw := w/2;
  if not CheckRectangleBorderBounds(x1,y1,x2,y2,w) then
    result := AddRectangle(x1-hw,y1-hw,x2+hw,y2+hw,ATexture) else
    result := AddPolygon([PointF(x1-hw,y1-hw),PointF(x2+hw,y1-hw),PointF(x2+hw,y2+hw),PointF(x1-hw,y2+hw),EmptyPointF,
                PointF(x1+hw,y2-hw),PointF(x2-hw,y2-hw),PointF(x2-hw,y1+hw),PointF(x1+hw,y1+hw)],ATexture);
end;

procedure TBGRAMultishapeFiller.OverrideFillMode(AShapeIndex: integer;
  AFillMode: TFillMode);
begin
  if AShapeIndex < 0 then exit;
  if AShapeIndex >= nbShapes then raise exception.Create('Index out of bounds');
  shapes[AShapeIndex].fillMode := AFillMode;
  shapes[AShapeIndex].fillModeOverride := true;
end;

procedure TBGRAMultishapeFiller.Draw(dest: TBGRACustomBitmap; ADrawMode: TDrawMode = dmDrawWithTransparency);
var
  shapeRow: array of record
    density: PDensity;
    densMinx,densMaxx: integer;
    nbInter: integer;
    inter: array of TIntersectionInfo;
  end;
  shapeRowsList: array of integer;
  NbShapeRows: integer;
  miny, maxy, minx, maxx,
  rowminx, rowmaxx: integer;

  procedure SubstractScanlines(src,dest: integer);
  var i: integer;

    procedure SubstractSegment(srcseg: integer);
    var x1,x2, x3,x4: single;
      j: integer;

      procedure AddSegment(xa,xb: single);
      var nb: PInteger;
      begin
        nb := @shapeRow[dest].nbinter;
        if length(shapeRow[dest].inter) < nb^+2 then
          setlength(shapeRow[dest].inter, nb^*2+2);
        with shapeRow[dest] do
        begin
          if inter[nb^] = nil then inter[nb^] := shapes[dest].info.CreateIntersectionInfo;
          inter[nb^].interX := xa;
          if inter[nb^+1] = nil then inter[nb^+1] := shapes[dest].info.CreateIntersectionInfo;
          inter[nb^+1].interX := xb;
        end;
        inc(nb^,2);
      end;

    begin
      x1 := shapeRow[src].inter[(srcseg-1)*2].interX;
      x2 := shapeRow[src].inter[srcseg*2-1].interX;
      for j := shapeRow[dest].nbInter div 2 downto 1 do
      begin
        x3 := shapeRow[dest].inter[(j-1)*2].interX;
        x4 := shapeRow[dest].inter[j*2-1].interX;
        if (x2 <= x3) or (x1 >= x4) then continue; //not overlapping
        if (x1 <= x3) and (x2 >= x4) then
          shapeRow[dest].inter[j*2-1].interX := x3 //empty
        else
        if (x1 <= x3) and (x2 < x4) then
          shapeRow[dest].inter[(j-1)*2].interX := x2 //remove left part
        else
        if (x1 > x3) and (x2 >= x4) then
          shapeRow[dest].inter[j*2-1].interX := x1 else //remove right part
        begin
          //[x1,x2] is inside [x3,x4]
          shapeRow[dest].inter[j*2-1].interX := x1; //left part
          AddSegment(x2,x4);
        end;
      end;
    end;

  begin
    for i := 1 to shapeRow[src].nbInter div 2 do
      SubstractSegment(i);
  end;

var
    AliasingOfs: TPointF;
    useAA: boolean;

  procedure AddOneLineDensity(cury: single);
  var
    i,k: integer;
    ix1,ix2: integer;
    x1,x2: single;
  begin
    for k := 0 to NbShapeRows-1 do
      with shapeRow[shapeRowsList[k]], shapes[shapeRowsList[k]] do
      begin
        //find intersections
        info.ComputeAndSort(cury, inter, nbInter, fillMode=fmWinding);
        nbInter := nbInter and not 1; //even
      end;

      case PolygonOrder of
        poLastOnTop: begin
          for k := 1 to NbShapeRows-1 do
            if shapeRow[shapeRowsList[k]].nbInter > 0 then
              for i := 0 to k-1 do
                SubstractScanlines(shapeRowsList[k],shapeRowsList[i]);
        end;
        poFirstOnTop: begin
          for k := 0 to NbShapeRows-2 do
            if shapeRow[shapeRowsList[k]].nbInter > 0 then
              for i := k+1 to NbShapeRows-1 do
                SubstractScanlines(shapeRowsList[k],shapeRowsList[i]);
        end;
      end;

      for k := 0 to NbShapeRows-1 do
      with shapeRow[shapeRowsList[k]] do
      begin
        //fill density
        if not useAA then
        begin
          for i := 0 to nbinter div 2 - 1 do
          begin
            x1 := inter[i + i].interX;
            x2 := inter[i + i + 1].interX;
            ComputeAliasedRowBounds(x1+AliasingOfs.X,x2+AliasingOfs.X,minx,maxx,ix1,ix2);

            if ix1 < densMinx then densMinx := ix1;
            if ix2 > densMaxx then densMaxx := ix2;

            if ix2 >= ix1 then
              FillWord(density[ix1-minx],ix2-ix1+1,256);
          end;
        end else
          {$DEFINE INCLUDE_FILLDENSITY}
          {$i density256.inc}
      end;

      for k := 0 to NbShapeRows-1 do
      with shapeRow[shapeRowsList[k]] do
      begin
        if densMinX < rowminx then rowminx := densMinX;
        if densMaxX > rowmaxx then rowmaxx := densMaxX;
      end;
  end;

type
    TCardinalSum = record
          sumR,sumG,sumB,sumA: cardinal;
        end;

var
  MultiEmpty: boolean;
  bounds: TRect;

  xb, yb, yc, k: integer;
  pdest:    PBGRAPixel;

  curSum,nextSum: ^TCardinalSum;
  sums: array of TCardinalSum;
  curAlpha: byte;

  pdens: PDensity;
  w: UInt32or64;
  ec: TExpandedPixel;
  count: integer;
  ScanNextFunc: function: TBGRAPixel of object;

begin
  if nbShapes = 0 then exit;
  for k := 0 to nbShapes-1 do
    if not shapes[k].fillModeOverride then shapes[k].fillMode:= fillMode;

  useAA := Antialiasing and (ADrawMode in [dmDrawWithTransparency,dmLinearBlend]);
  if nbShapes = 1 then
  begin
    if useAA then
      FillShapeAntialias(dest,shapes[0].info,GammaCompression(shapes[0].color),False,shapes[0].texture,shapes[0].fillMode = fmWinding, ADrawMode=dmLinearBlend) else
      FillShapeAliased(dest,shapes[0].info,GammaCompression(shapes[0].color),False,shapes[0].texture,shapes[0].fillMode = fmWinding, ADrawMode,
        AliasingIncludeBottomRight);
    exit;
  end;
  bounds := Rect(0,0,0,0);
  MultiEmpty := True;
  for k := 0 to nbShapes-1 do
  begin
    If BGRAShapeComputeMinMax(shapes[k].info,minx,miny,maxx,maxy,dest) then
    begin
      shapes[k].bounds := rect(minx,miny,maxx+1,maxy+1);
      if MultiEmpty then
      begin
        MultiEmpty := False;
        bounds := shapes[k].bounds;
      end else
      begin
        if minx < bounds.left then bounds.left := minx;
        if miny < bounds.top then bounds.top := miny;
        if maxx >= bounds.right then bounds.right := maxx+1;
        if maxy >= bounds.bottom then bounds.bottom := maxy+1;
      end;
    end else
      shapes[k].bounds := rect(0,0,0,0);
  end;
  if MultiEmpty then exit;
  minx := bounds.left;
  miny := bounds.top;
  maxx := bounds.right-1;
  maxy := bounds.bottom-1;

  setlength(shapeRow, nbShapes);
  for k := 0 to nbShapes-1 do
  begin
    shapeRow[k].inter := shapes[k].info.CreateIntersectionArray;
    getmem(shapeRow[k].density, (maxx - minx + 2)*sizeof(TDensity)); //more for safety
  end;

  if AliasingIncludeBottomRight then
    AliasingOfs := PointF(0.0001,0.0001) else
    AliasingOfs := PointF(0,0);

  setlength(sums,maxx-minx+1);
  setlength(shapeRowsList, nbShapes);

  //vertical scan
  for yb := miny to maxy do
  begin
    rowminx := maxx+1;
    rowmaxx := minx-1;

    //init shape rows
    NbShapeRows := 0;
    for k := 0 to nbShapes-1 do
    if (yb >= shapes[k].bounds.top) and (yb < shapes[k].bounds.Bottom) then
    begin
      shapeRowsList[NbShapeRows] := k;
      inc(NbShapeRows);

      fillchar(shapeRow[k].density^,(maxx-minx+1)*sizeof(TDensity),0);
      shapeRow[k].densMinx := maxx+1;
      shapeRow[k].densMaxx := minx-1;
    end;

    If useAA then
    begin
      //precision scan
      for yc := 0 to AntialiasPrecision - 1 do
        AddOneLineDensity( yb + (yc * 2 + 1) / (AntialiasPrecision * 2) );
    end else
    begin
      AddOneLineDensity( yb + 0.5 - AliasingOfs.Y );
    end;

    if rowminx < minx then rowminx := minx;
    if rowmaxx > maxx then rowmaxx := maxx;

    if rowminx <= rowmaxx then
    begin
      FillChar(sums[rowminx-minx],(rowmaxx-rowminx+1)*sizeof(sums[0]),0);

      if useAA then
        {$define PARAM_ANTIALIASINGFACTOR}
        {$i multishapeline.inc}
      else
        {$i multishapeline.inc};

      pdest := dest.ScanLine[yb] + rowminx;
      xb := rowminx;
      nextSum := @sums[xb-minx];
      case ADrawMode of
        dmDrawWithTransparency:
          while xb <= rowmaxx do
          begin
            curSum := nextSum;
            inc(nextSum);
            with curSum^ do
            begin
              if sumA <> 0 then
              begin
                ec.red := (sumR+sumA shr 1) div sumA;
                ec.green := (sumG+sumA shr 1) div sumA;
                ec.blue := (sumB+sumA shr 1) div sumA;
                if sumA > 255 then curAlpha := 255 else curAlpha := sumA;
                ec.alpha := curAlpha shl 8 + curAlpha;
                count := 1;
                while (xb < rowmaxx) and (nextSum^.sumA = sumA) and (nextSum^.sumB = sumB)
                  and (nextSum^.sumG = sumG) and (nextSum^.sumR = sumR) do
                begin
                  inc(xb);
                  inc(nextSum);
                  inc(count);
                end;
                if count = 1 then
                  DrawExpandedPixelInlineNoAlphaCheck(pdest,ec,curAlpha) else
                   DrawExpandedPixelsInline(pdest, ec, count );
                inc(pdest,count-1);
              end;
            end;
            inc(xb);
            inc(pdest);
          end;

        dmLinearBlend:
          while xb <= rowmaxx do
          begin
            curSum := nextSum;
            inc(nextSum);
            with curSum^ do
            begin
              if sumA <> 0 then
              begin
                ec.red := (sumR+sumA shr 1) div sumA;
                ec.green := (sumG+sumA shr 1) div sumA;
                ec.blue := (sumB+sumA shr 1) div sumA;
                if sumA > 255 then curAlpha := 255 else curAlpha := sumA;
                ec.alpha := curAlpha shl 8 + curAlpha;
                count := 1;
                while (xb < rowmaxx) and (nextSum^.sumA = sumA) and (nextSum^.sumB = sumB)
                  and (nextSum^.sumG = sumG) and (nextSum^.sumR = sumR) do
                begin
                  inc(xb);
                  inc(nextSum);
                  inc(count);
                end;
                if count = 1 then
                  DrawPixelInlineNoAlphaCheck(pdest,GammaCompression(ec)) else
                begin
                   DrawPixelsInline(pdest, GammaCompression(ec), count );
                   inc(pdest,count-1);
                end;
              end;
            end;
            inc(xb);
            inc(pdest);
          end;

        dmXor:
          while xb <= rowmaxx do
          begin
            curSum := nextSum;
            inc(nextSum);
            with curSum^ do
            begin
              if sumA <> 0 then
              begin
                ec.red := (sumR+sumA shr 1) div sumA;
                ec.green := (sumG+sumA shr 1) div sumA;
                ec.blue := (sumB+sumA shr 1) div sumA;
                if sumA > 255 then curAlpha := 255 else curAlpha := sumA;
                ec.alpha := curAlpha shl 8 + curAlpha;
                count := 1;
                while (xb < rowmaxx) and (nextSum^.sumA = sumA) and (nextSum^.sumB = sumB)
                  and (nextSum^.sumG = sumG) and (nextSum^.sumR = sumR) do
                begin
                  inc(xb);
                  inc(nextSum);
                  inc(count);
                end;
                XorInline(pdest,GammaCompression(ec),count);
                inc(pdest,count-1);
              end;
            end;
            inc(xb);
            inc(pdest);
          end;

        dmSet:
          while xb <= rowmaxx do
          begin
            curSum := nextSum;
            inc(nextSum);
            with curSum^ do
            begin
              if sumA <> 0 then
              begin
                ec.red := (sumR+sumA shr 1) div sumA;
                ec.green := (sumG+sumA shr 1) div sumA;
                ec.blue := (sumB+sumA shr 1) div sumA;
                if sumA > 255 then curAlpha := 255 else curAlpha := sumA;
                ec.alpha := curAlpha shl 8 + curAlpha;
                count := 1;
                while (xb < rowmaxx) and (nextSum^.sumA = sumA) and (nextSum^.sumB = sumB)
                  and (nextSum^.sumG = sumG) and (nextSum^.sumR = sumR) do
                begin
                  inc(xb);
                  inc(nextSum);
                  inc(count);
                end;
                FillInline(pdest,GammaCompression(ec),count);
                inc(pdest,count-1);
              end;
            end;
            inc(xb);
            inc(pdest);
          end;

        dmSetExceptTransparent:
          while xb <= rowmaxx do
          begin
            curSum := nextSum;
            inc(nextSum);
            with curSum^ do
            begin
              if sumA >= 255 then
              begin
                ec.red := (sumR+sumA shr 1) div sumA;
                ec.green := (sumG+sumA shr 1) div sumA;
                ec.blue := (sumB+sumA shr 1) div sumA;
                if sumA > 255 then curAlpha := 255 else curAlpha := sumA;
                ec.alpha := curAlpha shl 8 + curAlpha;
                count := 1;
                while (xb < rowmaxx) and (nextSum^.sumA = sumA) and (nextSum^.sumB = sumB)
                  and (nextSum^.sumG = sumG) and (nextSum^.sumR = sumR) do
                begin
                  inc(xb);
                  inc(nextSum);
                  inc(count);
                end;
                FillInline(pdest,GammaCompression(ec),count);
                inc(pdest,count-1);
              end;
            end;
            inc(xb);
            inc(pdest);
          end;

      end;
    end;

  end;

  for k := 0 to nbShapes-1 do
  begin
    freemem(shapeRow[k].density);
    shapes[k].info.FreeIntersectionArray(shapeRow[k].inter);
  end;

  dest.InvalidateBitmap;
end;

//////////////////////////////////////////////////////////////////////////////

procedure FillEllipseAntialias(bmp: TCustomUniversalBitmap; x, y, rx,
  ry: single; ABrush: TUniversalBrush);
var
  info: TFillEllipseInfo;
begin
  if ABrush.DoesNothing or (rx = 0) or (ry = 0) or (x = EmptySingle) or (y = EmptySingle) then exit;
  info := TFillEllipseInfo.Create(x, y, rx, ry);
  FillShapeAntialias(bmp, info, ABrush, False);
  info.Free;
end;

procedure FillEllipseAntialias(bmp: TBGRACustomBitmap; x, y, rx, ry: single;
  c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean);
var
  b: TUniversalBrush;
begin
  AnyBrush(b, bmp,c,EraseMode,nil,LinearBlend);
  FillEllipseAntialias(bmp, x,y,rx,ry, b);
end;

procedure FillEllipseAntialiasWithTexture(bmp: TBGRACustomBitmap; x, y, rx,
  ry: single; scan: IBGRAScanner; LinearBlend: boolean);
var
  b: TUniversalBrush;
begin
  AnyBrush(b, bmp, BGRAPixelTransparent,false,scan,LinearBlend);
  FillEllipseAntialias(bmp, x,y,rx,ry, b);
end;

procedure BorderEllipseAntialias(bmp: TCustomUniversalBitmap; x, y, rx, ry,
  w: single; ABrush: TUniversalBrush);
var
  info: TFillBorderEllipseInfo;
begin
  if ABrush.DoesNothing or ((rx = 0) and (ry = 0)) or (w=0) or (x = EmptySingle) or (y = EmptySingle) then exit;
  info := TFillBorderEllipseInfo.Create(x, y, rx, ry, w);
  FillShapeAntialias(bmp, info, ABrush, False);
  info.Free;
end;

procedure BorderEllipseAntialias(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean);
var
  b: TUniversalBrush;
begin
  AnyBrush(b, bmp,c,EraseMode,nil,LinearBlend);
  BorderEllipseAntialias(bmp, x,y,rx,ry,w,b);
end;

procedure BorderEllipseAntialiasWithTexture(bmp: TBGRACustomBitmap; x, y, rx,
  ry, w: single; scan: IBGRAScanner; LinearBlend: boolean);
var
  b: TUniversalBrush;
begin
  AnyBrush(b, bmp,BGRAPixelTransparent,false,scan,LinearBlend);
  BorderEllipseAntialias(bmp, x,y,rx,ry,w,b);
end;
   
procedure BorderEllipse(bmp: TCustomUniversalBitmap; x, y, rx, ry, w: single;
  ABrush: TUniversalBrush; AAlpha: word);
var
  info: TFillBorderEllipseInfo;
begin
  if ABrush.DoesNothing or ((rx = 0) and (ry = 0)) or (w=0) or (x = EmptySingle) or (y = EmptySingle) then exit;
  info := TFillBorderEllipseInfo.Create(x, y, rx, ry, w);
  FillShapeAliased(bmp, info, ABrush, AAlpha, False);
  info.Free;
end;

procedure BorderEllipse(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  c: TBGRAPixel; EraseMode: boolean; drawmode: TDrawMode);
var
  bFill: TUniversalBrush;
begin
  AnyBrush(bFill, bmp, c,EraseMode,nil,drawmode);
  BorderEllipse(bmp, x,y,rx,ry,w,bFill);
end;

procedure BorderEllipseWithTexture(bmp: TBGRACustomBitmap; x, y, rx, ry,
  w: single; scan: IBGRAScanner; drawmode: TDrawMode);
var
  bFill: TUniversalBrush;
begin
  bmp.ScannerBrush(bFill, scan,drawMode);
  BorderEllipse(bmp, x,y,rx,ry,w,bFill);
end;

procedure FillRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2, y2,
  rx, ry: single; options: TRoundRectangleOptions; c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean; APixelCenteredCoordinates: boolean);
var
  info: TFillRoundRectangleInfo;
begin
  if (x1 = x2) or (y1 = y2) then exit;
  info := TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry, options, APixelCenteredCoordinates);
  FillShapeAntialias(bmp, info, c, EraseMode,nil, False, LinearBlend);
  info.Free;
end;

procedure FillRoundRectangleAntialiasWithTexture(bmp: TBGRACustomBitmap; x1,
  y1, x2, y2, rx, ry: single; options: TRoundRectangleOptions;
  scan: IBGRAScanner; LinearBlend: boolean; APixelCenteredCoordinates: boolean);
var
  info: TFillRoundRectangleInfo;
begin
  if (x1 = x2) or (y1 = y2) then exit;
  info := TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry, options, APixelCenteredCoordinates);
  FillShapeAntialiasWithTexture(bmp, info, scan, False, LinearBlend);
  info.Free;
end;

procedure BorderRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2,
  y2, rx, ry, w: single; options: TRoundRectangleOptions; c: TBGRAPixel;
  EraseMode: boolean; LinearBlend: boolean; APixelCenteredCoordinates: boolean);
var
  info: TFillShapeInfo;
  oldLinear: boolean;
begin
  if w=0 then exit;
  if ((rx=0) or (ry=0)) and not EraseMode then
  begin
    oldLinear := bmp.LinearAntialiasing;
    bmp.LinearAntialiasing := LinearBlend;
    bmp.RectangleAntialias(x1,y1,x2,y2,c,w);
    bmp.LinearAntialiasing := oldLinear;
    exit;
  end;
  info := TFillBorderRoundRectInfo.Create(x1, y1, x2,y2, rx, ry, w, options, APixelCenteredCoordinates);
  FillShapeAntialias(bmp, info, c, EraseMode, nil, False, LinearBlend);
  info.Free;
end;

procedure BorderRoundRectangleAntialiasWithTexture(bmp: TBGRACustomBitmap; x1,
  y1, x2, y2, rx, ry, w: single; options: TRoundRectangleOptions;
  scan: IBGRAScanner; LinearBlend: boolean; APixelCenteredCoordinates: boolean);
var
  info: TFillBorderRoundRectInfo;
  oldLinear: Boolean;
begin
  if w=0 then exit;
  if (rx=0) or (ry=0) then
  begin
    oldLinear := bmp.LinearAntialiasing;
    bmp.LinearAntialiasing := LinearBlend;
    bmp.RectangleAntialias(x1,y1,x2,y2,scan,w);
    bmp.LinearAntialiasing := oldLinear;
    exit;
  end;
  info := TFillBorderRoundRectInfo.Create(x1, y1, x2,y2, rx, ry, w, options, APixelCenteredCoordinates);
  FillShapeAntialiasWithTexture(bmp, info, scan, False, LinearBlend);
  info.Free;
end;

procedure BorderAndFillRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1,
  x2, y2, rx, ry, w: single; options: TRoundRectangleOptions; bordercolor,
  fillcolor: TBGRAPixel; bordertexture,filltexture: IBGRAScanner; EraseMode: boolean; APixelCenteredCoordinates: boolean);
var
  info: TFillBorderRoundRectInfo;
  multi: TBGRAMultishapeFiller;
begin
  if (rx = 0) or (ry = 0) then exit;
  info := TFillBorderRoundRectInfo.Create(x1, y1, x2,y2, rx, ry, w, options, APixelCenteredCoordinates);
  if not EraseMode then
  begin
    multi := TBGRAMultishapeFiller.Create;
    if filltexture <> nil then
      multi.AddShape(info.innerBorder, filltexture) else
      multi.AddShape(info.innerBorder, fillcolor);
    if w<>0 then
    begin
      if bordertexture <> nil then
        multi.AddShape(info, bordertexture) else
        multi.AddShape(info, bordercolor);
    end;
    multi.Draw(bmp);
    multi.Free;
  end else
  begin
    FillShapeAntialias(bmp, info.innerBorder, fillcolor, EraseMode, nil, False, False);
    FillShapeAntialias(bmp, info, bordercolor, EraseMode, nil, False, False);
  end;
  info.Free;
end;

end.
