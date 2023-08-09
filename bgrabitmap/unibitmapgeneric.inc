// SPDX-License-Identifier: LGPL-3.0-linking-exception
{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}

type
  { TGenericUniversalBitmap }

  generic TGenericUniversalBitmap<TPixel,TColorspace> = class(TCustomUniversalBitmap)
  private
    type PPixel = ^TPixel;
    function GetDataPtr: PPixel; inline;
    function GetScanLine(y: integer): PPixel; inline;
  protected
    class function DefaultColorspace: TColorspaceAny; override;
    procedure Init; override; //ensure colorspace
    function GetScanlineFast(y: integer): PPixel; inline; //typed function
    procedure InternalCopyPixels(ASource,ADest: PByte; ASourceStride,ADestStride: PtrInt; ACount: integer); override;
    procedure InternalSwapPixels(ABuf1,ABuf2: PByte; AStride1,AStride2: PtrInt; ACount: integer); override;
    procedure InternalSetPixels(ASource,ADest: PByte; ADestStride: PtrInt; ACount: integer); override;
  public
    //typed functions

    {** Creates an image of dimensions ''AWidth'' and ''AHeight'' and fills it with ''Color'' }
    constructor Create(AWidth, AHeight: integer; const AColor: TPixel); virtual;
    function NewBitmap(AWidth, AHeight: integer; const AColor: TPixel): TCustomUniversalBitmap; overload; virtual;

    function Equals(comp: TCustomUniversalBitmap): boolean; overload;
    function Equals(const comp: TPixel): boolean; overload;
    function GetDifferenceBounds(ABitmap: TCustomUniversalBitmap): TRect;

    procedure SolidBrushIndirect(out ABrush: TUniversalBrush; AColor: Pointer; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    class procedure IdleBrush(out ABrush: TUniversalBrush); virtual;
    class procedure SolidBrush(out ABrush: TUniversalBrush; const AColor: TPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); virtual;
    function CreateBrushTexture(ABrushStyle: TBrushStyle; APatternColor, ABackgroundColor: TPixel;
            AWidth: integer = 8; AHeight: integer = 8; APenWidth: single = 1): TCustomUniversalBitmap; virtual;

    procedure SetPixel(x,y: int32or64; const AColor: TPixel); overload; virtual;
    function GetPixel(x,y: int32or64): TPixel; inline;
    function GetPixelCycle(x,y: int32or64): TPixel; inline;
    {** Returns the address of the left-most pixel of any line.
        The parameter y ranges from 0 to Height-1 }
    property ScanLine[y: integer]: PPixel Read GetScanLine;
    property Data: PPixel read GetDataPtr;

    procedure Fill(const AColor: TPixel); overload; virtual;
    procedure Fill(const AColor: TPixel; AMode : TDrawMode; AAlpha : Word = 65535); overload; virtual;
    procedure ReplaceColor(const ABefore, AAfter: TPixel); overload; virtual;
    procedure ReplaceColor(ARect: TRect; const ABefore, AAfter: TPixel); overload; virtual;
    procedure ReplaceTransparent(const AAfter: TBGRAPixel); overload; virtual;
    procedure ReplaceTransparent(ARect: TRect; const AAfter: TPixel); overload; virtual;
    procedure FillMask(x,y: integer; AMask: TCustomUniversalBitmap; const AColor: TPixel); overload; virtual;
    procedure FillMask(x,y: integer; AMask: TCustomUniversalBitmap; const AColor: TPixel; ADrawMode: TDrawMode); overload; virtual;
    procedure FillRect(ALeft, ATop, ARight, ABottom: integer; const AColor: TPixel; ADrawMode: TDrawMode = dmSet; AAlpha: Word = 65535); overload; virtual;
    procedure FillRect(const ARect: TRect; const AColor: TPixel; ADrawMode: TDrawMode = dmSet; AAlpha: Word = 65535); overload; virtual;
    procedure DrawCheckers(ARect: TRect; const AColorEven,AColorOdd: TPixel; AGridWidth: integer = 8; AGridHeight: integer = 8; ADrawMode: TDrawMode = dmSet); virtual;
    procedure DrawPixel(x,y: Int32or64; const AColor: TPixel); overload; virtual;
    procedure DrawPixel(x,y: Int32or64; const AColor: TPixel; ADrawMode: TDrawMode); overload; virtual;
    procedure DrawPixel(x,y: Int32or64; const AColor: TPixel; ADrawMode: TDrawMode; AAlpha: Word); overload; virtual;
    procedure DrawPixelF(x,y: single; const AColor: TPixel; ADrawMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    {** Replaces the content of the pixels at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color }
    procedure SetHorizLine(x, y, x2: int32or64; const AColor: TPixel); virtual;
    procedure HorizLine(x, y, x2: int32or64; const AColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload; virtual;
    {** Replaces a vertical line at column ''x'' and at row ''y'' to ''y2'' }
    procedure SetVertLine(x, y, y2: int32or64; const AColor: TPixel); virtual;
    procedure VertLine(x, y, y2: int32or64; const AColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload; virtual;
    procedure DrawLine(x1, y1, x2, y2: integer; const AColor: TPixel; ADrawLastPixel: boolean; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload; virtual;
    procedure DrawLineAntialias(x1, y1, x2, y2: integer; const AColor: TPixel; ADrawLastPixel: boolean; AAlpha: Word = 65535); overload; virtual;
    procedure DrawLineAntialias(x1, y1, x2, y2: integer; const AColor1, AColor2: TPixel; ADashLen: integer; ADrawLastPixel: boolean; AAlpha: Word = 65535); overload; virtual;
    procedure DrawLineAntialias(x1, y1, x2, y2: integer; const AColor1, AColor2: TPixel; ADashLen: integer; var ADashPos: integer; ADrawLastPixel: boolean; AAlpha: Word = 65535); overload; virtual;
    procedure DrawPolyLine(const points: array of TPoint; const AColor: TPixel; ADrawLastPixel: boolean; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload;
    procedure DrawPolyLineAntialias(const points: array of TPoint; const AColor: TPixel; ADrawLastPixel: boolean; AAlpha: Word = 65535); overload;
    procedure DrawPolyLineAntialias(const points: array of TPoint; const AColor1, AColor2: TPixel; ADashLen: integer; ADrawLastPixel: boolean; AAlpha: Word = 65535); overload;
    procedure DrawPolygon(const points: array of TPoint; const AColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload;
    procedure DrawPolygonAntialias(const points: array of TPoint; const AColor: TPixel; AAlpha: Word = 65535); overload;
    procedure DrawPolygonAntialias(const points: array of TPoint; const AColor1, AColor2: TPixel; ADashLen: integer; AAlpha: Word = 65535); overload;
    procedure DrawPathAliased(APath: IBGRAPath; const AColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); overload;
    procedure DrawPathAliased(APath: IBGRAPath; const AMatrix: TAffineMatrix; const AColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); overload;
    procedure Rectangle(x, y, x2, y2: integer; const AColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload; virtual;
    procedure Rectangle(x, y, x2, y2: integer; const ABorderColor, AFillColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload; virtual;
    procedure Rectangle(const ARect: TRect; const AColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload; virtual;
    procedure Rectangle(const ARect: TRect; const ABorderColor, AFillColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload; virtual;
    procedure RoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; const ABorderColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload; virtual;
    procedure RoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; const ABorderColor, AFillColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload; virtual;
    procedure FillRoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; const AFillColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload; virtual;
    procedure EllipseInRect(ARect: TRect; const ABorderColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload; virtual;
    procedure EllipseInRect(ARect: TRect; const ABorderColor, AFillColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload; virtual;
    procedure FillEllipseInRect(ARect: TRect; const AFillColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload; virtual;
    procedure FillShape(AShape: TBGRACustomFillInfo; const AFillColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; AAlpha: Word = 65535); overload; virtual;
    procedure FillPoly(const APoints: array of TPointF; const AFillColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); overload; virtual;
    procedure FillPathAliased(APath: IBGRAPath; const AFillColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); overload;
    procedure FillPathAliased(APath: IBGRAPath; const AMatrix: TAffineMatrix; const AFillColor: TPixel; AMode : TDrawMode = dmDrawWithTransparency; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); overload;

    //optimized
    procedure SetPixelIndirect(x,y: int32or64; AColor: pointer); override;
    procedure GetPixelIndirect(x,y: int32or64; AColor: pointer); override;
    procedure GetPixelCycleIndirect(x,y: int32or64; AColor: pointer); override;
    function GetPixelAddress(x, y: integer): PByte; override;

    {==== Drawing lines and paths (floating point coordinates) ====}
    {* These functions use the current pen style/cap/join. The parameter ''APenWidth''
       specifies the width of the line and the base unit for dashes.
       See [[BGRABitmap tutorial 13|coordinate system]].
     * The coordinates are pixel-centered by default, so that when filling a rectangle,
       if the supplied values are integers, the border will be half transparent.
       If you want the border to be completely filled, you can subtract/add
       0.5 to the coordinates to include the remaining thin border.
       See [[BGRABitmap tutorial 13|coordinate system]]. }

    procedure DrawLineAntialias(x1, y1, x2, y2: single; const AColor: TPixel; APenWidth: single); overload; virtual;
    procedure DrawLineAntialias(x1, y1, x2, y2: single; const AColor: TPixel; APenWidth: single; AClosedCap: boolean); overload; virtual;

    {** Draws a polyline using current pen style/cap/join }
    procedure DrawPolyLineAntialias(const APoints: array of TPointF; const AColor: TPixel; APenWidth: single); overload; virtual;
    {** Draws a polyline using current pen style/cap/join.
        ''Closed'' specifies if the end of the line is roundly closed. If it is not closed,
        a space is left so that the next line can fit }
    procedure DrawPolyLineAntialias(const APoints: array of TPointF; const AColor: TPixel; APenWidth: single; AClosedCap: boolean); overload; virtual;
    {** Draws a polyline using current pen style/cap/join.
        The last point considered as a join with the first point if it has
        the same coordinate }
    procedure DrawPolyLineAntialiasAutocycle(const APoints: array of TPointF; const AColor: TPixel; APenWidth: single); overload; virtual;
    {** Draws a polygon using current pen style/cap/join.
        The polygon is always closed. You don't need to set the last point
        to be the same as the first point }
    procedure DrawPolygonAntialias(const APoints: array of TPointF; const AColor: TPixel; APenWidth: single); overload; virtual;

    procedure RectangleAntialias(x, y, x2, y2: single; const AColor: TPixel; AWidth: single); overload; virtual;

    {** Draws an ellipse without antialising. ''rx'' is the horizontal radius and
        ''ry'' the vertical radius }
    procedure Ellipse(x, y, rx, ry: single; const AColor: TPixel; AWidth: single; AMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    procedure Ellipse(const AOrigin, AXAxis, AYAxis: TPointF; const AColor: TPixel; AWidth: single; AMode: TDrawMode; AAlpha: Word = 65535); overload; virtual;
    {** Draws an ellipse with antialising. ''rx'' is the horizontal radius and
        ''ry'' the vertical radius }
    procedure EllipseAntialias(x, y, rx, ry: single; const AColor: TPixel; AWidth: single); overload; virtual;
    procedure EllipseAntialias(const AOrigin, AXAxis, AYAxis: TPointF; const AColor: TPixel; AWidth: single); overload; virtual;

    procedure DrawPath(APath: IBGRAPath; const AColor: TPixel; AWidth: single; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure DrawPath(APath: IBGRAPath; const AMatrix: TAffineMatrix; const AColor: TPixel; AWidth: single; APixelCenteredCoordinates: boolean = true); overload; virtual;

    //-------------------------------- antialias fill ---------------------------------------------
    procedure FillPolyAntialias(const APoints: array of TPointF; const AColor: TPixel; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillEllipseAntialias(x, y, rx, ry: single; const AColor: TPixel); overload; virtual;
    procedure FillEllipseAntialias(const AOrigin, AXAxis, AYAxis: TPointF; const AColor: TPixel); overload; virtual;
    procedure FillPath(APath: IBGRAPath; const AColor: TPixel; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillPath(APath: IBGRAPath; const AMatrix: TAffineMatrix; const AColor: TPixel; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillRectAntialias(x, y, x2, y2: single; const AColor: TPixel; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillRectAntialias(const ARectF: TRectF; const AColor: TPixel; APixelCenteredCoordinates: boolean = true); overload; virtual;

    {** Fills a rounded rectangle with antialiasing. The corners have an
        elliptical radius of ''rx'' and ''ry''. ''options'' specifies how to
        draw the corners. See [[BGRABitmap Geometry types|geometry types]] }
    procedure FillRoundRectAntialias(x, y, x2, y2, rx, ry: single; const AColor: TPixel; AOptions: TRoundRectangleOptions = []; APixelCenteredCoordinates: boolean = true); overload; virtual;

  end;

{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}

{ TGenericUniversalBitmap }

function TGenericUniversalBitmap.GetDataPtr: PPixel;
begin
  result := PPixel(GetDataBytePtr);
end;

function TGenericUniversalBitmap.GetScanLine(y: integer): PPixel;
begin
  result := PPixel(GetScanLineByte(y));
end;

class function TGenericUniversalBitmap.DefaultColorspace: TColorspaceAny;
begin
  Result:= TColorspace;
end;

procedure TGenericUniversalBitmap.Init;
begin
  inherited Init;
  if FColorspace <> DefaultColorspace then raise exception.Create('Unexpected colorspace');
end;

{ Get scanline without checking bounds nor updated from bitmap }
function TGenericUniversalBitmap.GetScanlineFast(y: integer): PPixel;
begin
  if FLineOrder = riloBottomToTop then y := FHeight - 1 - y;
  result := PPixel(FDataByte+FRowSize*y);
end;

procedure TGenericUniversalBitmap.InternalCopyPixels(ASource, ADest: PByte;
  ASourceStride, ADestStride: PtrInt; ACount: integer);
begin
  while ACount>0 do
  begin
    PPixel(ADest)^ := PPixel(ASource)^;
    inc(ASource, ASourceStride);
    inc(ADest, ADestStride);
    dec(ACount);
  end;
end;

procedure TGenericUniversalBitmap.InternalSwapPixels(ABuf1, ABuf2: PByte;
  AStride1, AStride2: PtrInt; ACount: integer);
var temp: TPixel;
begin
  while ACount>0 do
  begin
    temp := PPixel(ABuf1)^;
    PPixel(ABuf1)^ := PPixel(ABuf2)^;
    PPixel(ABuf2)^ := temp;
    inc(ABuf1, AStride1);
    inc(ABuf2, AStride2);
    dec(ACount);
  end;
end;

procedure TGenericUniversalBitmap.InternalSetPixels(ASource, ADest: PByte;
  ADestStride: PtrInt; ACount: integer);
begin
  while ACount>0 do
  begin
    PPixel(ADest)^ := PPixel(ASource)^;
    inc(ADest, ADestStride);
    dec(ACount);
  end;
end;

constructor TGenericUniversalBitmap.Create(AWidth, AHeight: integer;
  const AColor: TPixel);
var
  p: PByte;
  i: Integer;
begin
  inherited Create(0, 0);
  SetSize(AWidth,AHeight);
  p := DataByte;
  for i := NbPixels-1 downto 0 do
  begin
    PPixel(p)^ := AColor;
    inc(p, sizeof(TPixel));
  end;
end;

function TGenericUniversalBitmap.NewBitmap(AWidth, AHeight: integer;
  const AColor: TPixel): TCustomUniversalBitmap;
var
  b: TUniversalBrush;
begin
  result := InternalNew;
  result.SetSize(AWidth,AHeight);
  SolidBrush(b, AColor, dmSet);
  result.Fill(b);
end;

function TGenericUniversalBitmap.Equals(comp: TCustomUniversalBitmap): boolean;
var
  p,pComp: PByte;
  y, x: Integer;
  delta, compDelta: PtrInt;
begin
  if (comp.Colorspace<>Colorspace) or
     (comp.Width <> Width) or (comp.Height <> Height) then exit(false);
  p := GetPixelAddress(0,0);
  if LineOrder = riloTopToBottom then delta := 0 else delta := -RowSize*2;
  pComp := comp.GetPixelAddress(0,0);
  if comp.LineOrder = riloTopToBottom then compDelta := 0 else compDelta := -comp.RowSize*2;
  for y := 0 to Height-1 do
  begin
    for x := Width-1 downto 0 do
    begin
      if PPixel(p)^ <> PPixel(pComp)^ then exit(false);
      inc(p, sizeof(TPixel));
      inc(pComp, sizeof(TPixel));
    end;
    inc(p, delta);
    inc(pComp, compDelta);
  end;
  result := true;
end;

function TGenericUniversalBitmap.Equals(const comp: TPixel): boolean;
var
  p: PByte;
  i: Integer;
begin
  p := DataByte;
  for i := NbPixels-1 downto 0 do
  begin
    if PPixel(p)^ <> comp then exit(false);
    inc(p, sizeof(TPixel));
  end;
  result := true;
end;

function TGenericUniversalBitmap.GetDifferenceBounds(
  ABitmap: TCustomUniversalBitmap): TRect;
var
  minx, miny, maxx, maxy: integer;
  xb, yb: integer;
  p, p2:  PPixel;
begin
  if (ABitmap.Width <> Width) or (ABitmap.Height <> Height)
    or (ABitmap.Colorspace <> Colorspace) then
  begin
    result := rect(0,0,Width,Height);
    if ABitmap.Width > result.Right then result.Right := ABitmap.Width;
    if ABitmap.Height > result.bottom then result.bottom := ABitmap.Height;
    exit;
  end;
  maxx := -1;
  maxy := -1;
  minx := self.Width;
  miny := self.Height;
  for yb := 0 to self.Height - 1 do
  begin
    p := self.ScanLine[yb];
    p2 := PPixel(ABitmap.ScanLineByte[yb]);
    for xb := 0 to self.Width - 1 do
    begin
      if p^ <> p2^ then
      begin
        if xb < minx then
          minx := xb;
        if yb < miny then
          miny := yb;
        if xb > maxx then
          maxx := xb;
        if yb > maxy then
          maxy := yb;
      end;
      Inc(p);
      Inc(p2);
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

procedure TGenericUniversalBitmap.SolidBrushIndirect(out
  ABrush: TUniversalBrush; AColor: Pointer; ADrawMode: TDrawMode);
begin
  SolidBrush(ABrush, PPixel(AColor)^, ADrawMode);
end;

class procedure TGenericUniversalBitmap.IdleBrush(out ABrush: TUniversalBrush);
begin
  ABrush.Colorspace := TColorspace;
  ABrush.InternalInitContext:= nil;
  PDefaultSolidBrushIndirectFixedData(@ABrush.FixedData)^.PixelSize:= sizeof(TPixel);
  ABrush.InternalPutNextPixels:= @DefaultSolidBrushIndirectSkipPixels;
  ABrush.DoesNothing:= true;
end;

class procedure TGenericUniversalBitmap.SolidBrush(out ABrush: TUniversalBrush;
  const AColor: TPixel; ADrawMode: TDrawMode);
var
  ct: TColorTransparency;
begin
  ct := TColorspace.GetColorTransparency(@AColor);
  if (ADrawMode in[dmLinearBlend,dmDrawWithTransparency]) and
   (ct = ctSemiTransparent) then
     raise exception.Create('Semi-transparent drawing not handled by default brush')
  else if ADrawMode = dmXor then
     raise exception.Create('Xor mode not handled by default brush');

  ABrush.Colorspace := TColorspace;
  ABrush.InternalInitContext:= nil;
  PDefaultSolidBrushIndirectFixedData(@ABrush.FixedData)^.PixelSize:= sizeof(TPixel);

  if (ADrawMode <> dmSet) and (ct <> ctFullyOpaque) then
  begin
    ABrush.InternalPutNextPixels:= @DefaultSolidBrushIndirectSkipPixels;
    ABrush.DoesNothing:= true;
  end
  else
  begin
    PPixel(@PDefaultSolidBrushIndirectFixedData(@ABrush.FixedData)^.Color)^ := AColor;
    ABrush.InternalPutNextPixels:= @DefaultSolidBrushIndirectSetPixels
  end;
end;

function TGenericUniversalBitmap.CreateBrushTexture(ABrushStyle: TBrushStyle;
  APatternColor, ABackgroundColor: TPixel; AWidth: integer; AHeight: integer;
  APenWidth: single): TCustomUniversalBitmap;
var
  b: TUniversalBrush;
begin
  result := InternalNew;
  result.SetSize(AWidth,AHeight);
  if ABrushStyle=bsClear then
    result.FillTransparent
  else
  begin
    SolidBrush(b, ABackgroundColor, dmSet);
    result.Fill(b);
    SolidBrush(b, APatternColor,dmDrawWithTransparency);
    if ABrushStyle in[bsDiagCross,bsBDiagonal] then
    begin
      result.DrawLineAntialias(-1,AHeight,AWidth,-1, b,APenWidth);
      result.DrawLineAntialias(-1-APenWidth,0+APenWidth,0+APenWidth,-1-APenWidth, b,APenWidth);
      result.DrawLineAntialias(AWidth-1-APenWidth,AHeight+APenWidth,AWidth+APenWidth,AHeight-1-APenWidth, b,APenWidth);
    end;
    if ABrushStyle in[bsDiagCross,bsFDiagonal] then
    begin
      result.DrawLineAntialias(-1,-1,AWidth,AHeight, b,APenWidth);
      result.DrawLineAntialias(AWidth-1-APenWidth,-1-APenWidth,AWidth+APenWidth,0+APenWidth, b,APenWidth);
      result.DrawLineAntialias(-1-APenWidth,AHeight-1-APenWidth,0+APenWidth,AHeight+APenWidth, b,APenWidth);
    end;
    if ABrushStyle in[bsHorizontal,bsCross] then
      result.DrawLineAntialias(-1,AHeight div 2,AWidth,AHeight div 2, b,APenWidth);
    if ABrushStyle in[bsVertical,bsCross] then
      result.DrawLineAntialias(AWidth div 2,-1,AWidth div 2,AHeight, b,APenWidth);
  end;
end;

procedure TGenericUniversalBitmap.SetPixelIndirect(x, y: int32or64;
  AColor: pointer);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  (GetScanlineFast(y)+x)^ := PPixel(AColor)^;
  InvalidateBitmap;
end;

procedure TGenericUniversalBitmap.GetPixelIndirect(x, y: int32or64;
  AColor: pointer);
begin
  if (x < 0) or (x >= FWidth) or (y < 0) or (y >= FHeight) then //it is possible to read pixels outside of the cliprect
    AssignTransparentPixel(AColor^) else
  begin
    LoadFromBitmapIfNeeded;
    PPixel(AColor)^ := (GetScanlineFast(y)+x)^;
  end;
end;

procedure TGenericUniversalBitmap.GetPixelCycleIndirect(x, y: int32or64;
  AColor: pointer);
begin
  if (Width = 0) or (Height = 0) then AssignTransparentPixel(AColor^) else
  begin
    LoadFromBitmapIfNeeded;
    PPixel(AColor)^ := (GetScanlineFast(PositiveMod(y, Height)) + PositiveMod(x, Width))^;
  end;
end;

procedure TGenericUniversalBitmap.SetPixel(x, y: int32or64; const AColor: TPixel);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  (GetScanlineFast(y)+x)^ := AColor;
  InvalidateBitmap;
end;

function TGenericUniversalBitmap.GetPixel(x, y: int32or64): TPixel;
begin
  GetPixelIndirect(x,y, @result);
end;

function TGenericUniversalBitmap.GetPixelCycle(x, y: int32or64): TPixel;
begin
  GetPixelCycleIndirect(x,y, @result);
end;

procedure TGenericUniversalBitmap.Fill(const AColor: TPixel);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, dmSet);
  Fill(b);
end;

procedure TGenericUniversalBitmap.Fill(const AColor: TPixel; AMode : TDrawMode; AAlpha : Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, AMode);
  Fill(b, AAlpha);
end;

procedure TGenericUniversalBitmap.ReplaceColor(const ABefore, AAfter: TPixel);
var
  p: PPixel;
  n: integer;
begin
  p := Data;
  for n := NbPixels - 1 downto 0 do
  begin
    if p^ = ABefore then p^ := AAfter;
    Inc(p);
  end;
  InvalidateBitmap;
end;

procedure TGenericUniversalBitmap.ReplaceColor(ARect: TRect;
  const ABefore,AAfter: TPixel);
var
  p: PPixel;
  n,w,yb: integer;
begin
  if not CheckClippedRectBounds(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom) then exit;
  LoadFromBitmapIfNeeded;
  w := ARect.Width-1;
  for yb := ARect.Top to ARect.Bottom-1 do
  begin
    p := GetScanlineFast(yb)+ARect.Left;
    for n := w downto 0 do
    begin
      if p^ = ABefore then p^ := AAfter;
      Inc(p);
    end;
  end;
  InvalidateBitmap;
end;

procedure TGenericUniversalBitmap.ReplaceTransparent(const AAfter: TBGRAPixel);
var before: TPixel;
begin
  AssignTransparentPixel(before);
  ReplaceColor(before, AAfter);
end;

procedure TGenericUniversalBitmap.ReplaceTransparent(ARect: TRect;
  const AAfter: TPixel);
var before: TPixel;
begin
  AssignTransparentPixel(before);
  ReplaceColor(ARect, before, AAfter);
end;

procedure TGenericUniversalBitmap.FillMask(x, y: integer;
  AMask: TCustomUniversalBitmap; const AColor: TPixel);
begin
  FillMask(x,y, AMask, AColor, dmDrawWithTransparency);
end;

procedure TGenericUniversalBitmap.FillMask(x, y: integer;
  AMask: TCustomUniversalBitmap; const AColor: TPixel; ADrawMode: TDrawMode);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, ADrawMode);
  FillMask(x,y, AMask, b);
end;

procedure TGenericUniversalBitmap.FillRect(ALeft, ATop, ARight,
  ABottom: integer; const AColor: TPixel; ADrawMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, ADrawMode);
  FillRect(ALeft,ATop,ARight,ABottom, b,AAlpha);
end;

procedure TGenericUniversalBitmap.FillRect(const ARect: TRect;
  const AColor: TPixel; ADrawMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, ADrawMode);
  FillRect(ARect, b,AAlpha);
end;

procedure TGenericUniversalBitmap.DrawCheckers(ARect: TRect; const AColorEven,
  AColorOdd: TPixel; AGridWidth: integer; AGridHeight: integer; ADrawMode: TDrawMode);
var
  bEven, bOdd: TUniversalBrush;
begin
  SolidBrush(bEven, AColorEven, ADrawMode);
  SolidBrush(bOdd, AColorOdd, ADrawMode);
  DrawCheckers(ARect, bEven, bOdd, AGridWidth, AGridHeight);
end;

procedure TGenericUniversalBitmap.DrawPixel(x, y: Int32or64;
  const AColor: TPixel);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, dmDrawWithTransparency);
  DrawPixel(x,y, b);
end;

procedure TGenericUniversalBitmap.DrawPixel(x, y: Int32or64;
  const AColor: TPixel; ADrawMode: TDrawMode);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, ADrawMode);
  DrawPixel(x,y, b);
end;

procedure TGenericUniversalBitmap.DrawPixel(x, y: Int32or64;
  const AColor: TPixel; ADrawMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, ADrawMode);
  DrawPixel(x,y, b,AAlpha);
end;

procedure TGenericUniversalBitmap.DrawPixelF(x, y: single;
  const AColor: TPixel; ADrawMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, ADrawMode);
  DrawPixelF(x,y, b,AAlpha);
end;

procedure TGenericUniversalBitmap.SetHorizLine(x, y, x2: int32or64;
  const AColor: TPixel);
var
  pScan: PByte;
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  LoadFromBitmapIfNeeded;
  pScan := GetPixelAddress(x,y);
  InternalSetPixels(@AColor, pScan, sizeof(TPixel), x2-x+1);
  InvalidateBitmap;
end;

procedure TGenericUniversalBitmap.HorizLine(x, y, x2: int32or64;
  const AColor: TPixel; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, AMode);
  HorizLine(x,y,x2, b,AAlpha);
end;

procedure TGenericUniversalBitmap.SetVertLine(x, y, y2: int32or64;
  const AColor: TPixel);
var
  pScan: PByte;
  delta: PtrInt;
  yb: Int32or64;
begin
  if not CheckVertLineBounds(x,y,y2) then exit;
  LoadFromBitmapIfNeeded;
  pScan := GetPixelAddress(x,y);
  if LineOrder = riloTopToBottom then delta := RowSize else delta := -rowSize;
  for yb := y to y2 do
  begin
    PPixel(pScan)^ := AColor;
    inc(pScan, delta);
  end;
  InvalidateBitmap;
end;

procedure TGenericUniversalBitmap.VertLine(x, y, y2: int32or64;
  const AColor: TPixel; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, AMode);
  VertLine(x,y,y2, b,AAlpha);
end;

procedure TGenericUniversalBitmap.DrawLine(x1, y1, x2, y2: integer;
  const AColor: TPixel; ADrawLastPixel: boolean; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, AMode);
  DrawLine(x1,y1,x2,y2, b,ADrawLastPixel,AAlpha);
end;

procedure TGenericUniversalBitmap.DrawLineAntialias(x1, y1, x2, y2: integer;
  const AColor: TPixel; ADrawLastPixel: boolean; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, AntialiasingDrawMode);
  DrawLineAntialias(x1,y1,x2,y2, b,ADrawLastPixel,AAlpha);
end;

procedure TGenericUniversalBitmap.DrawLineAntialias(x1, y1, x2, y2: integer;
  const AColor1, AColor2: TPixel; ADashLen: integer;
  ADrawLastPixel: boolean; AAlpha: Word);
var
  b1, b2: TUniversalBrush;
begin
  SolidBrush(b1, AColor1, AntialiasingDrawMode);
  SolidBrush(b2, AColor2, AntialiasingDrawMode);
  DrawLineAntialias(x1,y1,x2,y2, b1,b2, ADashLen, ADrawLastPixel, AAlpha);
end;

procedure TGenericUniversalBitmap.DrawLineAntialias(x1, y1, x2, y2: integer;
  const AColor1, AColor2: TPixel; ADashLen: integer;
  var ADashPos: integer; ADrawLastPixel: boolean; AAlpha: Word);
var
  b1, b2: TUniversalBrush;
begin
  SolidBrush(b1, AColor1, AntialiasingDrawMode);
  SolidBrush(b2, AColor2, AntialiasingDrawMode);
  DrawLineAntialias(x1,y1,x2,y2, b1,b2, ADashLen,ADashPos, ADrawLastPixel, AAlpha);
end;

procedure TGenericUniversalBitmap.DrawPolyLine(const points: array of TPoint;
  const AColor: TPixel; ADrawLastPixel: boolean; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, AMode);
  DrawPolyLine(points, b, ADrawLastPixel, AAlpha);
end;

procedure TGenericUniversalBitmap.DrawPolyLineAntialias(
  const points: array of TPoint; const AColor: TPixel;
  ADrawLastPixel: boolean; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, AntialiasingDrawMode);
  DrawPolyLineAntialias(points, b,ADrawLastPixel,AAlpha);
end;

procedure TGenericUniversalBitmap.DrawPolyLineAntialias(
  const points: array of TPoint; const AColor1, AColor2: TPixel;
  ADashLen: integer; ADrawLastPixel: boolean; AAlpha: Word);
var
  b1, b2: TUniversalBrush;
begin
  SolidBrush(b1, AColor1, AntialiasingDrawMode);
  SolidBrush(b2, AColor2, AntialiasingDrawMode);
  DrawPolyLineAntialias(points, b1,b2,ADashLen, ADrawLastPixel,AAlpha);
end;

procedure TGenericUniversalBitmap.DrawPolygon(const points: array of TPoint;
  const AColor: TPixel; AMode : TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, AMode);
  DrawPolygon(points, b,AAlpha);
end;

procedure TGenericUniversalBitmap.DrawPolygonAntialias(
  const points: array of TPoint; const AColor: TPixel; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, AntialiasingDrawMode);
  DrawPolygonAntialias(points, b,AAlpha);
end;

procedure TGenericUniversalBitmap.DrawPolygonAntialias(
  const points: array of TPoint; const AColor1, AColor2: TPixel;
  ADashLen: integer; AAlpha: Word);
var
  b1, b2: TUniversalBrush;
begin
  SolidBrush(b1, AColor1, AntialiasingDrawMode);
  SolidBrush(b2, AColor2, AntialiasingDrawMode);
  DrawPolygonAntialias(points, b1,b2,ADashLen, AAlpha);
end;

procedure TGenericUniversalBitmap.DrawPathAliased(APath: IBGRAPath;
  const AColor: TPixel; AMode: TDrawMode; APixelCenteredCoordinates: boolean;
  AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor,AMode);
  DrawPathAliased(APath, b,APixelCenteredCoordinates,AAlpha);
end;

procedure TGenericUniversalBitmap.DrawPathAliased(APath: IBGRAPath;
  const AMatrix: TAffineMatrix; const AColor: TPixel; AMode: TDrawMode;
  APixelCenteredCoordinates: boolean; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor,AMode);
  DrawPathAliased(APath,AMatrix, b,APixelCenteredCoordinates,AAlpha);
end;

procedure TGenericUniversalBitmap.Rectangle(x, y, x2, y2: integer;
  const AColor: TPixel; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, AColor, AMode);
  Rectangle(x,y,x2,y2, b,AAlpha);
end;

procedure TGenericUniversalBitmap.Rectangle(x, y, x2, y2: integer;
  const ABorderColor, AFillColor: TPixel; AMode: TDrawMode; AAlpha: Word);
var
  bBorder,bFill: TUniversalBrush;
begin
  SolidBrush(bBorder, ABorderColor, AMode);
  SolidBrush(bFill, AFillColor, AMode);
  Rectangle(x,y,x2,y2, bBorder,bFill,AAlpha);
end;

procedure TGenericUniversalBitmap.Rectangle(const ARect: TRect;
  const AColor: TPixel; AMode: TDrawMode; AAlpha: Word);
begin
  Rectangle(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom, AColor,AMode,AAlpha);
end;

procedure TGenericUniversalBitmap.Rectangle(const ARect: TRect;
  const ABorderColor, AFillColor: TPixel; AMode: TDrawMode; AAlpha: Word);
begin
  Rectangle(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom, ABorderColor,AFillColor,AMode,AAlpha);
end;

procedure TGenericUniversalBitmap.RoundRect(X1, Y1, X2, Y2: integer; DX,
  DY: integer; const ABorderColor: TPixel; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, ABorderColor,AMode);
  RoundRect(X1,Y1,X2,Y2,DX,DY, b,AAlpha);
end;

procedure TGenericUniversalBitmap.RoundRect(X1, Y1, X2, Y2: integer; DX,
  DY: integer; const ABorderColor, AFillColor: TPixel; AMode: TDrawMode;
  AAlpha: Word);
var
  bBorder, bFill: TUniversalBrush;
begin
  SolidBrush(bBorder, ABorderColor,AMode);
  SolidBrush(bFill, AFillColor,AMode);
  RoundRect(X1,Y1,X2,Y2,DX,DY, bBorder,bFill,AAlpha);
end;

procedure TGenericUniversalBitmap.FillRoundRect(X1, Y1, X2, Y2: integer; DX,
  DY: integer; const AFillColor: TPixel; AMode: TDrawMode; AAlpha: Word);
var
  bFill: TUniversalBrush;
begin
  SolidBrush(bFill, AFillColor,AMode);
  FillRoundRect(X1,Y1,X2,Y2,DX,DY, bFill,AAlpha);
end;

procedure TGenericUniversalBitmap.EllipseInRect(ARect: TRect;
  const ABorderColor: TPixel; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b, ABorderColor,AMode);
  EllipseInRect(ARect, b,AAlpha);
end;

procedure TGenericUniversalBitmap.EllipseInRect(ARect: TRect;
  const ABorderColor, AFillColor: TPixel; AMode: TDrawMode; AAlpha: Word);
var
  bBorder, bFill: TUniversalBrush;
begin
  SolidBrush(bBorder, ABorderColor,AMode);
  SolidBrush(bFill, AFillColor,AMode);
  EllipseInRect(ARect, bBorder,bFill,AAlpha);
end;

procedure TGenericUniversalBitmap.FillEllipseInRect(ARect: TRect;
  const AFillColor: TPixel; AMode: TDrawMode; AAlpha: Word);
var
  bFill: TUniversalBrush;
begin
  SolidBrush(bFill, AFillColor,AMode);
  FillEllipseInRect(ARect, bFill,AAlpha);
end;

procedure TGenericUniversalBitmap.FillShape(AShape: TBGRACustomFillInfo;
  const AFillColor: TPixel; AMode: TDrawMode; AAlpha: Word);
var
  bFill: TUniversalBrush;
begin
  SolidBrush(bFill, AFillColor,AMode);
  FillShape(AShape, bFill,AAlpha);
end;

procedure TGenericUniversalBitmap.FillPoly(const APoints: array of TPointF;
  const AFillColor: TPixel; AMode: TDrawMode;
  APixelCenteredCoordinates: boolean; AAlpha: Word);
var
  bFill: TUniversalBrush;
begin
  SolidBrush(bFill, AFillColor,AMode);
  FillPoly(APoints,bFill,APixelCenteredCoordinates,AAlpha);
end;

procedure TGenericUniversalBitmap.FillPathAliased(APath: IBGRAPath;
  const AFillColor: TPixel; AMode: TDrawMode;
  APixelCenteredCoordinates: boolean; AAlpha: Word);
var
  bFill: TUniversalBrush;
begin
  SolidBrush(bFill, AFillColor,AMode);
  FillPathAliased(APath,bFill,APixelCenteredCoordinates,AAlpha);
end;

procedure TGenericUniversalBitmap.FillPathAliased(APath: IBGRAPath;
  const AMatrix: TAffineMatrix; const AFillColor: TPixel; AMode: TDrawMode;
  APixelCenteredCoordinates: boolean; AAlpha: Word);
var
  bFill: TUniversalBrush;
begin
  SolidBrush(bFill, AFillColor,AMode);
  FillPathAliased(APath,AMatrix, bFill,APixelCenteredCoordinates,AAlpha);
end;

function TGenericUniversalBitmap.GetPixelAddress(x, y: integer): PByte;
begin
  if FLineOrder = riloBottomToTop then y := FHeight - 1 - y;
  result := FDataByte + FRowSize * y + IntPtr(x)*sizeof(TPixel);
end;

procedure TGenericUniversalBitmap.FillPolyAntialias(
  const APoints: array of TPointF; const AColor: TPixel;
  APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  FillPolyAntialias(APoints, b, APixelCenteredCoordinates);
end;

procedure TGenericUniversalBitmap.FillEllipseAntialias(x, y, rx, ry: single;
  const AColor: TPixel);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  FillEllipseAntialias(x,y,rx,ry, b);
end;

procedure TGenericUniversalBitmap.FillEllipseAntialias(const AOrigin, AXAxis,
  AYAxis: TPointF; const AColor: TPixel);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  {$PUSH}{$R-}
  FillEllipseAntialias(AOrigin, AXAxis, AYAxis, b);
  {$POP}
end;

procedure TGenericUniversalBitmap.FillPath(APath: IBGRAPath;
  const AColor: TPixel; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  FillPath(APath, b, APixelCenteredCoordinates);
end;

procedure TGenericUniversalBitmap.FillPath(APath: IBGRAPath;
  const AMatrix: TAffineMatrix; const AColor: TPixel;
  APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  FillPath(APath,AMatrix, b, APixelCenteredCoordinates);
end;

procedure TGenericUniversalBitmap.FillRectAntialias(x, y, x2, y2: single;
  const AColor: TPixel; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  FillRectAntialias(x,y,x2,y2,b,APixelCenteredCoordinates);
end;

procedure TGenericUniversalBitmap.FillRectAntialias(const ARectF: TRectF;
  const AColor: TPixel; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  FillRectAntialias(ARectF,b,APixelCenteredCoordinates);
end;

procedure TGenericUniversalBitmap.FillRoundRectAntialias(x, y, x2, y2, rx,
  ry: single; const AColor: TPixel; AOptions: TRoundRectangleOptions;
  APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  FillRoundRectAntialias(x,y,x2,y2, rx,ry, b, AOptions, APixelCenteredCoordinates);
end;

procedure TGenericUniversalBitmap.DrawLineAntialias(x1, y1, x2, y2: single;
  const AColor: TPixel; APenWidth: single);
var
  b: TUniversalBrush;
  c: TBGRAPixel;
  p: TBGRACustomPenStroker;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  c := AColor;
  p := GetInternalPen;
  FillPolyAntialias(p.ComputePolyline([PointF(x1,y1),PointF(x2,y2)],APenWidth,c), b);
end;

procedure TGenericUniversalBitmap.DrawLineAntialias(x1, y1, x2, y2: single;
  const AColor: TPixel; APenWidth: single; AClosedCap: boolean);
var
  b: TUniversalBrush;
  c: TBGRAPixel;
  p: TBGRACustomPenStroker;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  c := AColor;
  p := GetInternalPen;
  FillPolyAntialias(p.ComputePolyline([PointF(x1,y1),PointF(x2,y2)],APenWidth,c,AClosedCap), b);
end;

procedure TGenericUniversalBitmap.DrawPolyLineAntialias(
  const APoints: array of TPointF; const AColor: TPixel; APenWidth: single);
var
  b: TUniversalBrush;
  c: TBGRAPixel;
  p: TBGRACustomPenStroker;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  c := AColor;
  p := GetInternalPen;
  FillPolyAntialias(p.ComputePolyline(APoints,APenWidth,c),b);
end;

procedure TGenericUniversalBitmap.DrawPolyLineAntialias(
  const APoints: array of TPointF; const AColor: TPixel; APenWidth: single;
  AClosedCap: boolean);
var
  b: TUniversalBrush;
  c: TBGRAPixel;
  p: TBGRACustomPenStroker;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  c := AColor;
  p := GetInternalPen;
  FillPolyAntialias(p.ComputePolyline(APoints,APenWidth,c,AClosedCap),b);
end;

procedure TGenericUniversalBitmap.DrawPolyLineAntialiasAutocycle(
  const APoints: array of TPointF; const AColor: TPixel; APenWidth: single);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  DrawPolyLineAntialiasAutocycle(APoints, b, APenWidth);
end;

procedure TGenericUniversalBitmap.DrawPolygonAntialias(
  const APoints: array of TPointF; const AColor: TPixel; APenWidth: single);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  DrawPolygonAntialias(APoints, b, APenWidth);
end;

procedure TGenericUniversalBitmap.RectangleAntialias(x, y, x2, y2: single;
  const AColor: TPixel; AWidth: single);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  RectangleAntialias(x,y,x2,y2, b, AWidth);
end;

procedure TGenericUniversalBitmap.Ellipse(x, y, rx, ry: single;
  const AColor: TPixel; AWidth: single; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AMode);
  Ellipse(x,y,rx,ry, b, AWidth,AAlpha);
end;

procedure TGenericUniversalBitmap.Ellipse(const AOrigin, AXAxis, AYAxis: TPointF;
  const AColor: TPixel; AWidth: single; AMode: TDrawMode; AAlpha: Word);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AMode);
  {$PUSH}{$R-}
  Ellipse(AOrigin, AXAxis, AYAxis, b, AWidth,AAlpha);
  {$POP}
end;

procedure TGenericUniversalBitmap.EllipseAntialias(x, y, rx, ry: single;
  const AColor: TPixel; AWidth: single);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  EllipseAntialias(x,y,rx,ry, b, AWidth);
end;

procedure TGenericUniversalBitmap.EllipseAntialias(const AOrigin, AXAxis,
  AYAxis: TPointF; const AColor: TPixel; AWidth: single);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  {$PUSH}{$R-}
  EllipseAntialias(AOrigin, AXAxis, AYAxis, b, AWidth);
  {$POP}
end;

procedure TGenericUniversalBitmap.DrawPath(APath: IBGRAPath;
  const AColor: TPixel; AWidth: single; APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  DrawPath(APath, b, AWidth, APixelCenteredCoordinates);
end;

procedure TGenericUniversalBitmap.DrawPath(APath: IBGRAPath;
  const AMatrix: TAffineMatrix; const AColor: TPixel; AWidth: single;
  APixelCenteredCoordinates: boolean);
var
  b: TUniversalBrush;
begin
  SolidBrush(b,AColor,AntialiasingDrawMode);
  DrawPath(APath,AMatrix, b, AWidth, APixelCenteredCoordinates);
end;

{$ENDIF}
