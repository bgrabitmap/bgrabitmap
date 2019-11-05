unit BGRAFilterBlur;

{$mode objfpc}{$H+}

interface

uses
  Classes, BGRABitmapTypes, BGRAFilterType;

type
  { TCustomBlurTask }

  TCustomBlurTask = class(TFilterTask)
  private
    FBounds: TRect;
    FMask: TCustomUniversalBitmap;
    FMaskOwned: boolean;
  public
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect; AMask: TCustomUniversalBitmap; AMaskIsThreadSafe: boolean = false);
    destructor Destroy; override;
  protected
    procedure DoExecute; override;
  end;

  { TRadialBlurTask }

  TRadialBlurTask = class(TFilterTask)
  private
    FBounds: TRect;
    FRadiusX,FRadiusY: single;
    FBlurType: TRadialBlurType;
  public
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect; radius: single;
                       blurType: TRadialBlurType); overload;
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect; radiusX,radiusY: single;
                       blurType: TRadialBlurType); overload;
  protected
    procedure DoExecute; override;
  end;

  { TMotionBlurTask }

  TMotionBlurTask = class(TFilterTask)
  private
    FBounds: TRect;
    FDistance,FAngle: single;
    FOriented: boolean;
  public
    constructor Create(ABmp: TBGRACustomBitmap; ABounds: TRect; ADistance, AAngle: single; AOriented: boolean);
  protected
    procedure DoExecute; override;
  end;

procedure FilterBlurCustom(bmp: TCustomUniversalBitmap; ABounds: TRect;
   blurMask: TCustomUniversalBitmap; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
procedure FilterBlurMotion(bmp: TCustomUniversalBitmap; ABounds: TRect; distance: single;
  angle: single; oriented: boolean; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
procedure FilterBlurRadial(bmp: TCustomUniversalBitmap; ABounds: TRect; radiusX,radiusY: single;
  blurType: TRadialBlurType; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);

implementation

uses Types, Math, SysUtils, BGRAGrayscaleMask,
  BGRAGradientScanner;

type
  { TBoxBlurTask }

  TBoxBlurTask = class(TFilterTask)
  private
    FBounds: TRect;
    FRadiusX,FRadiusY: single;
  public
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect; radius: single); overload;
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect; radiusX,radiusY: single); overload;
  protected
    procedure DoExecute; override;
  end;

{ TCustomBlurTask }

constructor TCustomBlurTask.Create(bmp: TBGRACustomBitmap; ABounds: TRect;
  AMask: TCustomUniversalBitmap; AMaskIsThreadSafe: boolean);
begin
  SetSource(bmp);
  FBounds := ABounds;
  if AMaskIsThreadSafe then
  begin
    FMask := AMask;
    FMaskOwned := false;
  end else
  begin
    FMask := AMask.Duplicate;
    FMaskOwned := true;
  end;
end;

destructor TCustomBlurTask.Destroy;
begin
  If FMaskOwned then FreeAndNil(FMask);
  inherited Destroy;
end;

procedure TCustomBlurTask.DoExecute;
begin
  FilterBlurCustom(FSource,FBounds,FMask,Destination,@GetShouldStop);
end;

{ TMotionBlurTask }

constructor TMotionBlurTask.Create(ABmp: TBGRACustomBitmap; ABounds: TRect;
  ADistance, AAngle: single; AOriented: boolean);
begin
  SetSource(ABmp);
  FBounds := ABounds;
  FDistance := ADistance;
  FAngle := AAngle;
  FOriented:= AOriented;
end;

procedure TMotionBlurTask.DoExecute;
begin
  FilterBlurMotion(FSource,FBounds,FDistance,FAngle,FOriented,Destination,@GetShouldStop);
end;

{ TRadialBlurTask }

constructor TRadialBlurTask.Create(bmp: TBGRACustomBitmap; ABounds: TRect;
  radius: single; blurType: TRadialBlurType);
begin
  SetSource(bmp);
  FBounds := ABounds;
  FRadiusX := radius;
  FRadiusY := radius;
  FBlurType:= blurType;
end;

constructor TRadialBlurTask.Create(bmp: TBGRACustomBitmap; ABounds: TRect;
  radiusX, radiusY: single; blurType: TRadialBlurType);
begin
  SetSource(bmp);
  FBounds := ABounds;
  FRadiusX := radiusX;
  FRadiusY := radiusY;
  FBlurType:= blurType;
end;

procedure TRadialBlurTask.DoExecute;
begin
  FilterBlurRadial(FSource,FBounds,FRadiusX,FRadiusY,FBlurType,Destination,@GetShouldStop);
end;

procedure FilterBlurBoxRGBA(ASource: TCustomUniversalBitmap; ABounds: TRect; ARadiusX,ARadiusY: single;
         ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc=nil);
var oldClip: TRect;
const
  factMainX = 16;
  factMainY = 16;
type
  TAccumulator = Cardinal;
{$i blurbox.inc}

procedure FilterBlurBoxRGBA64(ASource: TCustomUniversalBitmap; ABounds: TRect; ARadiusX,ARadiusY: single;
         ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc=nil);
var oldClip: TRect;
const
  factMainX = 16;
  factMainY = 16;
type
  TAccumulator = UInt64;
{$i blurbox.inc}

procedure FilterBlurBoxByte(ASource: TCustomUniversalBitmap; ABounds: TRect; ARadiusX,ARadiusY: single;
         ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc=nil);
var oldClip: TRect;
const
  factMainX = 16;
  factMainY = 16;
type
  TAccumulator = Cardinal;
{$DEFINE PARAM_BYTEMASK}
{$i blurbox.inc}

procedure FilterBlurBoxByte64(ASource: TCustomUniversalBitmap; ABounds: TRect; ARadiusX,ARadiusY: single;
         ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc=nil);
var oldClip: TRect;
const
  factMainX = 16;
  factMainY = 16;
type
  TAccumulator = UInt64;
{$DEFINE PARAM_BYTEMASK}
{$i blurbox.inc}

procedure FilterBlurBox(ASource: TCustomUniversalBitmap; ABounds: TRect; ARadiusX,ARadiusY: single;
         ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc=nil);
const
    factMainX = 16;
    factMainY = 16;
var
  totalSum: UInt64;
  factExtraX,factExtraY: NativeUInt;
  {$IFNDEF CPU64}need64: Boolean;{$ENDIF}
begin
  if ADestination.Colorspace <> ASource.Colorspace then
    raise exception.Create('Colorspace mismatch');

  totalSum := (2*ceil(ARadiusX)+1)*(2*ceil(ARadiusY)+1);
  factExtraX := trunc(frac(ARadiusX+0.5/factMainX)*factMainX);
  factExtraY := trunc(frac(ARadiusY+0.5/factMainY)*factMainY);
  if factExtraX > 0 then totalSum *= factMainX;
  if factExtraY > 0 then totalSum *= factMainY;

  if ASource.Colorspace = TBGRAPixelColorspace then
  begin
    {$IFNDEF CPU64}
    need64 := totalSum > high(Cardinal) div (256*256);
    if not need64 then
     FilterBlurBoxRGBA(ASource, ABounds, ARadiusX,ARadiusY, ADestination, ACheckShouldStop) else
    {$ENDIF}
    FilterBlurBoxRGBA64(ASource, ABounds, ARadiusX,ARadiusY, ADestination, ACheckShouldStop)
  end
  else if ASource.Colorspace = TByteMaskColorspace then
  begin
    {$IFNDEF CPU64}
    need64 := totalSum > high(Cardinal) div 256;
    if not need64 then
     FilterBlurBoxByte(ASource, ABounds, ARadiusX,ARadiusY, ADestination, ACheckShouldStop) else
    {$ENDIF}
    FilterBlurBoxByte64(ASource, ABounds, ARadiusX,ARadiusY, ADestination, ACheckShouldStop);
  end
  else raise exception.Create('Unexpected colorspace: '+ASource.Colorspace.GetName);
end;

{ This is a clever solution for fast computing of the blur
  effect : it stores an array of vertical sums forming a square
  around the pixel which moves with it. For each new pixel,
  the vertical sums are kept except for the last column of
  the square }
procedure FilterBlurFastRGBA(bmp: TCustomUniversalBitmap; ABounds: TRect;
  radiusX,radiusY: single; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
 {$IFDEF CPU64}{$DEFINE FASTBLUR_DOUBLE}{$ENDIF}
  const BitMargin = 16;
  type
    PRowSum = ^TRowSum;
    TRegularRowValue = NativeUInt;
    TRowSum = record
      sumR,sumG,sumB,rgbDiv,sumA,aDiv: TRegularRowValue;
    end;
    TExtendedRowValue = {$IFDEF FASTBLUR_DOUBLE}double{$ELSE}uint64{$ENDIF};
    TExtendedRowSum = record
      sumR,sumG,sumB,rgbDiv,sumA,aDiv: TExtendedRowValue;
    end;

  procedure AccumulatePixel(psrc: Pointer; w: NativeUInt; var sums: TRowSum; verticalWeightShift: NativeInt); inline;
  var
    c: DWord;
  begin
    with sums do
    begin
      c := PDWord(psrc)^;
      Inc(aDiv, w);
      w *= ((c shr TBGRAPixel_AlphaShift) and $ff);
      inc(sumA, w);
      w := w shr verticalWeightShift;
      inc(rgbDiv, w);
      {$hints off}
      inc(sumR, ((c shr TBGRAPixel_RedShift) and $ff)*w );
      inc(sumG, ((c shr TBGRAPixel_GreenShift) and $ff)*w );
      inc(sumB, ((c shr TBGRAPixel_BlueShift) and $ff)*w );
      {$hints on}
    end;
  end;

  procedure AccumulateExtended(var ex: TExtendedRowSum; psum: PRowSum; w: NativeUInt); inline;
  begin
    with psum^ do
    begin
      ex.sumA += TExtendedRowValue(sumA)*w;
      ex.aDiv += TExtendedRowValue(aDiv)*w;
      ex.sumR += TExtendedRowValue(sumR)*w;
      ex.sumG += TExtendedRowValue(sumG)*w;
      ex.sumB += TExtendedRowValue(sumB)*w;
      ex.rgbDiv += TExtendedRowValue(rgbDiv)*w;
    end;
  end;

  procedure AccumulateShr(var total: TRowSum; psum: PRowSum; w: NativeUInt; horizontalWeightShift: NativeInt); inline;
  var
    addDiv2: NativeInt;
  begin
    with psum^ do
    begin
      addDiv2 := 1 shl (horizontalWeightShift-1);
      inc(total.sumA, (sumA*w+addDiv2) shr horizontalWeightShift );
      inc(total.aDiv, (aDiv*w+addDiv2) shr horizontalWeightShift );
      inc(total.sumR, (sumR*w+addDiv2) shr horizontalWeightShift );
      inc(total.sumG, (sumG*w+addDiv2) shr horizontalWeightShift );
      inc(total.sumB, (sumB*w+addDiv2) shr horizontalWeightShift );
      inc(total.rgbDiv, (rgbDiv*w+addDiv2) shr horizontalWeightShift );
    end;
  end;

  procedure AccumulateNormal(var total: TRowSum; psum: PRowSum; w: NativeUInt); inline;
  begin
    with psum^ do
    begin
      inc(total.sumA, sumA*w );
      inc(total.aDiv, aDiv*w );
      inc(total.sumR, sumR*w );
      inc(total.sumG, sumG*w );
      inc(total.sumB, sumB*w );
      inc(total.rgbDiv, rgbDiv*w );
    end;
  end;

  procedure ComputeExtendedAverage(const sum: TExtendedRowSum; pdest: pointer); inline;
  {$IFDEF FASTBLUR_DOUBLE}
  var v: uint32or64;
  {$ELSE}
  var rgbDivShr1: TExtendedRowValue;
  {$ENDIF}
  begin
    if (sum.aDiv <= 0) or (sum.rgbDiv <= 0) then
    begin
      PBGRAPixel(pdest)^ := BGRAPixelTransparent;
      exit;
    end;
    {$IFDEF FASTBLUR_DOUBLE}
    v := round(sum.sumA/sum.aDiv);
    if v > 255 then PBGRAPixel(pdest)^.alpha := 255 else PBGRAPixel(pdest)^.alpha := v;
    v := round(sum.sumR/sum.rgbDiv);
    if v > 255 then PBGRAPixel(pdest)^.red := 255 else PBGRAPixel(pdest)^.red := v;
    v := round(sum.sumG/sum.rgbDiv);
    if v > 255 then PBGRAPixel(pdest)^.green := 255 else PBGRAPixel(pdest)^.green := v;
    v := round(sum.sumB/sum.rgbDiv);
    if v > 255 then PBGRAPixel(pdest)^.blue := 255 else PBGRAPixel(pdest)^.blue := v;
    {$ELSE}
    rgbDivShr1:= sum.rgbDiv shr 1;
    PDWord(pdest)^ := (((sum.sumA+sum.aDiv shr 1) div sum.aDiv) shl TBGRAPixel_AlphaShift)
    or (((sum.sumR+rgbDivShr1) div sum.rgbDiv) shl TBGRAPixel_RedShift)
    or (((sum.sumG+rgbDivShr1) div sum.rgbDiv) shl TBGRAPixel_GreenShift)
    or (((sum.sumB+rgbDivShr1) div sum.rgbDiv) shl TBGRAPixel_BlueShift);
    {$ENDIF}
  end;

  procedure ComputeClampedAverage(const sum: TRowSum; pdest: pointer); inline;
  var v: UInt32or64;
  begin
    if (sum.aDiv = 0) or (sum.rgbDiv = 0) then
    begin
      PBGRAPixel(pdest)^ := BGRAPixelTransparent;
      exit;
    end;
    v := (sum.sumA+sum.aDiv shr 1) div sum.aDiv;
    if v > 255 then PBGRAPixel(pdest)^.alpha := 255 else PBGRAPixel(pdest)^.alpha := v;
    v := (sum.sumR+sum.rgbDiv shr 1) div sum.rgbDiv;
    if v > 255 then PBGRAPixel(pdest)^.red := 255 else PBGRAPixel(pdest)^.red := v;
    v := (sum.sumG+sum.rgbDiv shr 1) div sum.rgbDiv;
    if v > 255 then PBGRAPixel(pdest)^.green := 255 else PBGRAPixel(pdest)^.green := v;
    v := (sum.sumB+sum.rgbDiv shr 1) div sum.rgbDiv;
    if v > 255 then PBGRAPixel(pdest)^.blue := 255 else PBGRAPixel(pdest)^.blue := v;
  end;

  procedure ComputeAverage(const sum: TRowSum; pdest: pointer); inline;
  var rgbDivShr1: NativeUInt;
  begin
    if (sum.aDiv = 0) or (sum.rgbDiv = 0) then
    begin
      PBGRAPixel(pdest)^ := BGRAPixelTransparent;
      exit;
    end;
    rgbDivShr1:= sum.rgbDiv shr 1;
    PDWord(pdest)^ := (((sum.sumA+sum.aDiv shr 1) div sum.aDiv) shl TBGRAPixel_AlphaShift)
    or (((sum.sumR+rgbDivShr1) div sum.rgbDiv) shl TBGRAPixel_RedShift)
    or (((sum.sumG+rgbDivShr1) div sum.rgbDiv) shl TBGRAPixel_GreenShift)
    or (((sum.sumB+rgbDivShr1) div sum.rgbDiv) shl TBGRAPixel_BlueShift);
  end;

  {$I blurfast.inc}

procedure FilterBlurFastByte(bmp: TCustomUniversalBitmap; ABounds: TRect;
radiusX,radiusY: single; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);

  const BitMargin = 8;
  type
    PRowSum = ^TRowSum;
    TRegularRowValue = NativeUInt;
    TRowSum = record
      sumA,aDiv: TRegularRowValue;
    end;
    TExtendedRowValue = {$IFDEF FASTBLUR_DOUBLE}double{$ELSE}uint64{$ENDIF};
    TExtendedRowSum = record
      sumA,aDiv: TExtendedRowValue;
    end;

  procedure AccumulatePixel(psrc: Pointer; w: NativeUInt; var sums: TRowSum; verticalWeightShift: NativeInt); inline;
  begin
    with sums do
    begin
      Inc(aDiv, w);
      inc(sumA, w*PByte(psrc)^ shr verticalWeightShift);
    end;
  end;

  procedure AccumulateExtended(var ex: TExtendedRowSum; psum: PRowSum; w: NativeUInt); inline;
  begin
    with psum^ do
    begin
      ex.sumA += TExtendedRowValue(sumA)*w;
      ex.aDiv += TExtendedRowValue(aDiv)*w;
    end;
  end;

  procedure AccumulateShr(var total: TRowSum; psum: PRowSum; w: NativeUInt; horizontalWeightShift: NativeInt); inline;
  var
    addDiv2: NativeInt;
  begin
    with psum^ do
    begin
      addDiv2 := 1 shl (horizontalWeightShift-1);
      inc(total.sumA, (sumA*w+addDiv2) shr horizontalWeightShift );
      inc(total.aDiv, (aDiv*w+addDiv2) shr horizontalWeightShift );
    end;
  end;

  procedure AccumulateNormal(var total: TRowSum; psum: PRowSum; w: NativeUInt); inline;
  begin
    with psum^ do
    begin
      inc(total.sumA, sumA*w );
      inc(total.aDiv, aDiv*w );
    end;
  end;

  procedure ComputeExtendedAverage(const sum: TExtendedRowSum; pdest: pointer); inline;
  {$IFDEF FASTBLUR_DOUBLE}
  var v: uint32or64;
  {$ENDIF}
  begin
    if sum.aDiv <= 0 then
    begin
      PByte(pdest)^ := 0;
      exit;
    end;
    {$IFDEF FASTBLUR_DOUBLE}
    v := round(sum.sumA/sum.aDiv);
    if v > 255 then PByte(pdest)^ := 255 else PByte(pdest)^ := v;
    {$ELSE}
    PByte(pdest)^ := (sum.sumA+sum.aDiv shr 1) div sum.aDiv;
    {$ENDIF}
  end;

  procedure ComputeClampedAverage(const sum: TRowSum; pdest: pointer); inline;
  var v: UInt32or64;
  begin
    if sum.aDiv = 0 then
    begin
      PByte(pdest)^ := 0;
      exit;
    end;
    v := (sum.sumA+sum.aDiv shr 1) div sum.aDiv;
    if v > 255 then PByte(pdest)^ := 255 else PByte(pdest)^ := v;
  end;

  procedure ComputeAverage(const sum: TRowSum; pdest: pointer); inline;
  begin
    if sum.aDiv = 0 then
    begin
      PByte(pdest)^ := 0;
      exit;
    end;
    PByte(pdest)^ := (sum.sumA+sum.aDiv shr 1) div sum.aDiv;
  end;

  {$I blurfast.inc}

procedure FilterBlurFast(bmp: TCustomUniversalBitmap; ABounds: TRect;
  radiusX,radiusY: single; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
begin
  if ADestination.Colorspace <> bmp.Colorspace then
    raise exception.Create('Colorspace mismatch');
  if bmp.Colorspace = TBGRAPixelColorspace then
    FilterBlurFastRGBA(bmp, ABounds, radiusX,radiusY, ADestination, ACheckShouldStop)
  else if bmp.Colorspace = TByteMaskColorspace then
    FilterBlurFastByte(bmp, ABounds, radiusX,radiusY, ADestination, ACheckShouldStop);
end;

{ Normal radial blur compute a blur mask with a GradientFill and
  then posterize to optimize general purpose blur }
procedure FilterBlurRadialNormal(bmp: TCustomUniversalBitmap;
  ABounds: TRect; radiusX,radiusY: single; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  blurShape: TGrayscaleMask;
  n: Int32or64;
  p: PByte;
  shift, addRound: cardinal;
  grad: TBGRAGradientScanner;
  minRadius,maxRadius: single;
  oldClip: TRect;
begin
  if (radiusX <= 0) and (radiusY <= 0) then
  begin
    oldClip := ADestination.IntersectClip(ABounds);
    ADestination.PutImage(0,0,bmp,dmSet);
    ADestination.ClipRect := oldClip;
    exit;
  end;
  blurShape := TGrayscaleMask.Create(2 * ceil(radiusX) + 1, 2 * ceil(radiusY) + 1);
  grad := TBGRAGradientScanner.Create(BGRAWhite, BGRABlack, gtRadial,
                pointF(ceil(radiusX), ceil(radiusY)),
                pointF(ceil(radiusX)-radiusX-0.5, ceil(radiusY)),
                pointF(ceil(radiusX), ceil(radiusY)-radiusY-0.5),false);
  blurShape.Fill(grad, dmSet);
  grad.Free;
  minRadius := min(radiusX,radiusY);
  maxRadius := max(radiusX,radiusY);
  shift := max(0,min(ceil((maxRadius-8)/2), floor(minRadius)));
  if shift > 0 then
  begin
    if shift > 5 then shift := 5;
    addRound := 1 shl (shift-1);
    p := blurShape.Data;
    for n := 0 to blurShape.NbPixels-1 do
    begin
      p^ := (p^+addRound) shr shift;
      inc(p);
    end;
  end;
  FilterBlurCustom(bmp, ABounds, blurShape, ADestination, ACheckShouldStop);
  blurShape.Free;
end;

{ Blur disk creates a disk mask with a FillEllipse }
procedure FilterBlurDisk(bmp: TCustomUniversalBitmap; ABounds: TRect; radiusX,radiusY: single; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  blurShape: TGrayscaleMask;
  oldClip: TRect;
begin
  if (radiusX <= 0) and (radiusY <= 0) then
  begin
    oldClip := ADestination.IntersectClip(ABounds);
    ADestination.PutImage(0,0,bmp,dmSet);
    ADestination.ClipRect := oldClip;
    exit;
  end;
  blurShape := TGrayscaleMask.Create(2 * ceil(radiusX) + 1, 2 * ceil(radiusY) + 1, BGRABlack);
  blurShape.FillEllipseAntialias(ceil(radiusX), ceil(radiusY), radiusX + 0.5, radiusY + 0.5, BGRAWhite);
  FilterBlurCustom(bmp, ABounds, blurShape, ADestination, ACheckShouldStop);
  blurShape.Free;
end;

{ Corona blur use a circle as mask }
procedure FilterBlurCorona(bmp: TCustomUniversalBitmap; ABounds: TRect; radiusX,radiusY: single; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  blurShape: TGrayscaleMask;
  oldClip: TRect;
begin
  if (radiusX <= 0) and (radiusY <= 0) then
  begin
    oldClip := ADestination.IntersectClip(ABounds);
    ADestination.PutImage(0,0,bmp,dmSet);
    ADestination.ClipRect := oldClip;
    exit;
  end;
  blurShape := TGrayscaleMask.Create(2 * ceil(radiusX) + 1, 2 * ceil(radiusY) + 1, BGRABlack);
  blurShape.EllipseAntialias(ceil(radiusX), ceil(radiusY), radiusX, radiusY, BGRAWhite, 1);
  FilterBlurCustom(bmp, ABounds, blurShape, ADestination, ACheckShouldStop);
  blurShape.Free;
end;

procedure FilterBlurRadial(bmp: TCustomUniversalBitmap; ABounds: TRect; radiusX,radiusY: single;
  blurType: TRadialBlurType; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  oldClip: TRect;
begin
  if (radiusX <= 0) and (radiusY <= 0) then
  begin
    oldClip := ADestination.IntersectClip(ABounds);
    ADestination.PutImage(0,0,bmp,dmSet);
    ADestination.ClipRect := oldClip;
    exit;
  end;
  if radiusX < 0 then radiusX := 0;
  if radiusY < 0 then radiusY := 0;
  case blurType of
    rbCorona:  FilterBlurCorona(bmp, ABounds, radiusX,radiusY, ADestination, ACheckShouldStop);
    rbDisk:    FilterBlurDisk(bmp, ABounds, radiusX,radiusY, ADestination, ACheckShouldStop);
    rbNormal:  FilterBlurRadialNormal(bmp, ABounds, radiusX,radiusY, ADestination, ACheckShouldStop);
    rbFast:    FilterBlurFast(bmp, ABounds, radiusX,radiusY, ADestination, ACheckShouldStop);
    rbPrecise: FilterBlurRadialNormal(bmp, ABounds, radiusX / 10 + 0.5, radiusY/10 + 0.5, ADestination, ACheckShouldStop);
    rbBox:     FilterBlurBox(bmp, ABounds, radiusX,radiusY, ADestination, ACheckShouldStop);
  end;
end;

function CreateRadialBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ARadius: single;
  ABlurType: TRadialBlurType): TFilterTask; overload;
begin
  if ABlurType = rbBox then
    result := TBoxBlurTask.Create(ABmp,ABounds,ARadius)
  else
    result := TRadialBlurTask.Create(ABmp,ABounds,ARadius,ABlurType);
end;

function CreateRadialBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect;
  ARadiusX, ARadiusY: single; ABlurType: TRadialBlurType): TFilterTask; overload;
begin
  if ABlurType = rbBox then
    result := TBoxBlurTask.Create(ABmp,ABounds,ARadiusX,ARadiusY)
  else
    result := TRadialBlurTask.Create(ABmp,ABounds,ARadiusX,ARadiusY,ABlurType);
end;

{ This filter draws an antialiased line to make the mask, and
  if the motion blur is oriented, does a GradientFill to orient it }
procedure FilterBlurMotion(bmp: TCustomUniversalBitmap; ABounds: TRect; distance: single;
  angle: single; oriented: boolean; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  blurShape: TBGRACustomBitmap;
  intRadius: integer;
  dx, dy, r: single;
  oldClip: TRect;
begin
  if distance < 1e-6 then
  begin
    oldClip := ADestination.IntersectClip(ABounds);
    ADestination.PutImage(0,0,bmp,dmSet);
    ADestination.ClipRect := oldClip;
    exit;
  end;
  dx := cos(angle * Pi / 180);
  dy := sin(angle * Pi / 180);
  if not oriented and (abs(dx)<1e-6) then
    FilterBlurBox(bmp, ABounds,0,distance/2, ADestination, ACheckShouldStop)
  else if not oriented and (abs(dy)<1e-6) then
    FilterBlurBox(bmp, ABounds,distance/2,0, ADestination, ACheckShouldStop)
  else
  begin
    r  := distance / 2;
    intRadius := ceil(r);
    blurShape := BGRABitmapFactory.Create(2 * intRadius + 1, 2 * intRadius + 1, BGRABlack);
    blurShape.DrawLineAntialias(intRadius - dx * r, intRadius - dy *
      r, intRadius + dx * r, intRadius + dy * r, BGRAWhite, 1, True);
    if oriented then
      blurShape.GradientFill(0, 0, blurShape.Width, blurShape.Height,
        BGRAPixelTransparent, BGRABlack, gtRadial, pointF(intRadius -
        dx * r, intRadius - dy * r),
        pointF(intRadius + dx * (r + 0.5), intRadius + dy * (r + 0.5)),
        dmFastBlend, False);
    FilterBlurCustom(bmp, ABounds, blurShape, ADestination, ACheckShouldStop);
    blurShape.Free;
  end;
end;

function CreateMotionBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect;
  ADistance, AAngle: single; AOriented: boolean): TFilterTask;
begin
  result := TMotionBlurTask.Create(ABmp,ABounds,ADistance,AAngle,AOriented);
end;

{ General purpose blur : compute pixel sum according to the mask and then
  compute only difference while scanning from the left to the right }
procedure FilterBlurSmallMask(bmp: TCustomUniversalBitmap;
  blurMask: TCustomUniversalBitmap; ABounds: TRect; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc); forward;
procedure FilterBlurSmallMaskWithShift(bmp: TCustomUniversalBitmap;
  blurMask: TCustomUniversalBitmap; maskShift: integer; ABounds: TRect; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc); forward;
procedure FilterBlurBigMask(bmp: TCustomUniversalBitmap;
  blurMask: TCustomUniversalBitmap; ABounds: TRect; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc); forward;
procedure FilterBlurMask64(bmp: TCustomUniversalBitmap;
  blurMask: TCustomUniversalBitmap; ABounds: TRect; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc); forward;

//make sure value is in the range 0..255
function clampByte(value: NativeInt): NativeUInt; inline;
begin
  if value <= 0 then result := 0 else
  if value >= 255 then result := 255 else
    result := value;
end;

function CreateBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect;
  AMask: TBGRACustomBitmap; AMaskIsThreadSafe: boolean): TFilterTask;
begin
  result := TCustomBlurTask.Create(ABmp,ABounds,AMask,AMaskIsThreadSafe);
end;

procedure FilterBlurCustom(bmp: TCustomUniversalBitmap;
  ABounds: TRect; blurMask: TCustomUniversalBitmap; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  maskSum: int64;
  p: PByteMask;
  y, count, qty, maskShift, pStride: integer;
begin
  maskSum := 0;
  for y:= 0 to blurMask.Height-1 do
  begin
    blurMask.ScanMoveTo(0,Y);
    count := blurMask.Width;
    while count > 0 do
    begin
      qty := count;
      blurMask.ScanNextMaskChunk(qty, p, pStride);
      dec(count, qty);
      while qty > 0 do
      begin
        inc(maskSum,p^.gray);
        inc(p, pStride);
        dec(qty);
      end;
    end;
  end;
  if bmp.Colorspace = TBGRAPixelColorspace then
  begin
    maskShift := 0;
    while maskSum > 32768 do
    begin
      inc(maskShift);
      maskSum := maskSum shr 1;
    end;
    //check if sum can be stored in a 32-bit signed integer
    if maskShift = 0 then
      FilterBlurSmallMask(bmp,blurMask,ABounds,ADestination,ACheckShouldStop) else
    {$IFDEF CPU32}
    if maskShift < 8 then
      FilterBlurSmallMaskWithShift(bmp,blurMask,maskShift,ABounds,ADestination,ACheckShouldStop) else
      FilterBlurBigMask(bmp,blurMask,ABounds,ADestination,ACheckShouldStop);
    {$ELSE}
      FilterBlurMask64(bmp,blurMask,ABounds,ADestination,ACheckShouldStop);
    {$ENDIF}
  end else
  if bmp.Colorspace = TByteMaskColorspace then
  begin
    if maskSum > 32768*255 then
      FilterBlurMask64(bmp, blurMask, ABounds, Adestination, ACheckShouldStop)
    else
      FilterBlurSmallMask(bmp, blurMask, ABounds, Adestination, ACheckShouldStop)
  end else
    raise exception.Create('Unexpected source colorspace');
end;

type
  TBlurClearSumProc = procedure(AData: Pointer);
  TBlurAccumulateProc = procedure(AData: Pointer; pPix: pointer; maskAlpha: NativeInt);
  TBlurComputeAverageProc = procedure(AData: Pointer; pPix: pointer);

procedure FilterBlurGeneric(bmp: TCustomUniversalBitmap; blurMask: TCustomUniversalBitmap;
  ABounds: TRect; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc;
  AClearSum: TBlurClearSumProc; AAccumulate: TBlurAccumulateProc;
  AComputeAverage: TBlurComputeAverageProc; AData: Pointer);

  {$I blurnormal.inc}

//32-bit blur with shift
type
  TFilterBlurSmallMaskWithShift_Sum = record
      sumR, sumG, sumB,
      sumA, Adiv, RGBdiv : NativeInt;
      maskShift: integer;
    end;

procedure FilterBlurSmallMaskWithShift_ClearSum(AData: pointer);
begin
  with TFilterBlurSmallMaskWithShift_Sum(AData^) do
  begin
    sumR   := 0;
    sumG   := 0;
    sumB   := 0;
    sumA   := 0;
    Adiv   := 0;
    RGBdiv := 0;
  end;
end;

procedure FilterBlurSmallMaskWithShift_ComputeAverage(AData: pointer; pPix: pointer);
var temp,rgbDivShr1: NativeInt;
begin
  with TFilterBlurSmallMaskWithShift_Sum(AData^) do
  if (Adiv <= 0) or (RGBdiv <= 0) then
    PBGRAPixel(pPix)^ := BGRAPixelTransparent else
  begin
    temp := sumA + Adiv shr 1;
    if temp < Adiv then
      PBGRAPixel(pPix)^ := BGRAPixelTransparent
    else
    begin
      rgbDivShr1 := RGBdiv shr 1;
      PBGRAPixel(pPix)^.alpha := temp div Adiv;
      PBGRAPixel(pPix)^.red   := clampByte((sumR + rgbDivShr1) div RGBdiv);
      PBGRAPixel(pPix)^.green := clampByte((sumG + rgbDivShr1) div RGBdiv);
      PBGRAPixel(pPix)^.blue  := clampByte((sumB + rgbDivShr1) div RGBdiv);
    end;
  end;
end;

procedure FilterBlurSmallMaskWithShift_AccumulateSum(AData: pointer; pPix: pointer; maskAlpha: NativeInt);
var
  pixMaskAlpha: NativeInt;
  tempPixel: TBGRAPixel;
begin
  with TFilterBlurSmallMaskWithShift_Sum(AData^) do
  begin
    tempPixel := PBGRAPixel(pPix)^;
    pixMaskAlpha := maskAlpha * tempPixel.alpha;
    sumA    += pixMaskAlpha;
    Adiv    += maskAlpha;
    pixMaskAlpha := (cardinal(pixMaskAlpha)+$80000000) shr maskShift - ($80000000 shr maskShift);
    RGBdiv  += pixMaskAlpha;
    sumR    += NativeInt(tempPixel.red) * pixMaskAlpha;
    sumG    += NativeInt(tempPixel.green) * pixMaskAlpha;
    sumB    += NativeInt(tempPixel.blue) * pixMaskAlpha;
  end;
end;

procedure FilterBlurSmallMaskWithShift(bmp: TCustomUniversalBitmap;
  blurMask: TCustomUniversalBitmap; maskShift: integer; ABounds: TRect; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var Sum: TFilterBlurSmallMaskWithShift_Sum;
begin
  if ADestination.Colorspace <> bmp.Colorspace then
    raise exception.Create('Colorspace mismatch');
  if bmp.Colorspace <> TBGRAPixelColorspace then
    raise exception.Create('Unexpected colorspace: '+bmp.Colorspace.GetName);
  Sum.maskShift:= maskShift;
  FilterBlurGeneric(bmp, blurMask, ABounds, ADestination, ACheckShouldStop,
                    @FilterBlurSmallMaskWithShift_ClearSum,
                    @FilterBlurSmallMaskWithShift_AccumulateSum,
                    @FilterBlurSmallMaskWithShift_ComputeAverage, @Sum);
end;

//32-bit blur
type
  TFilterBlurSmallMask_Sum= record
    sumR, sumG, sumB, sumA, Adiv : integer;
  end;

procedure FilterBlurSmallMask_ClearSum(AData: pointer);
begin
  with TFilterBlurSmallMask_Sum(AData^) do
  begin
    sumR   := 0;
    sumG   := 0;
    sumB   := 0;
    sumA   := 0;
    Adiv   := 0;
  end;
end;

procedure FilterBlurSmallMask_ComputeAverageRGBA(AData: pointer; pPix: pointer);
var temp,sumAShr1: integer;
begin
  with TFilterBlurSmallMask_Sum(AData^) do
  if Adiv <= 0 then PBGRAPixel(pPix)^ := BGRAPixelTransparent else
  begin
    temp := sumA + Adiv shr 1;
    if temp < Adiv then
      PBGRAPixel(pPix)^ := BGRAPixelTransparent
    else
    begin
      sumAShr1 := sumA shr 1;
      PBGRAPixel(pPix)^.alpha := temp div Adiv;
      PBGRAPixel(pPix)^.red   := clampByte((sumR + sumAShr1) div sumA);
      PBGRAPixel(pPix)^.green := clampByte((sumG + sumAShr1) div sumA);
      PBGRAPixel(pPix)^.blue  := clampByte((sumB + sumAShr1) div sumA);
    end;
  end;
end;

procedure FilterBlurSmallMask_ComputeAverageByte(AData: pointer; pPix: pointer);
begin
  with TFilterBlurSmallMask_Sum(AData^) do
  begin
    if Adiv <= 0 then PByte(pPix)^ := 0 else
      PByte(pPix)^ := (sumA + Adiv shr 1) div Adiv;
  end;
end;

procedure FilterBlurSmallMask_AccumulateSumRGBA(AData: pointer; pPix: pointer; maskAlpha: NativeInt);
var
  pixMaskAlpha: integer;
  tempPixel: TBGRAPixel;
begin
  with TFilterBlurSmallMask_Sum(AData^) do
  begin
    tempPixel := PBGRAPixel(pPix)^;
    pixMaskAlpha := integer(maskAlpha) * tempPixel.alpha;
    sumA    += pixMaskAlpha;
    Adiv    += maskAlpha;
    sumR    += integer(tempPixel.red) * pixMaskAlpha;
    sumG    += integer(tempPixel.green) * pixMaskAlpha;
    sumB    += integer(tempPixel.blue) * pixMaskAlpha;
  end;
end;

procedure FilterBlurSmallMask_AccumulateSumByte(AData: pointer; pPix: pointer; maskAlpha: NativeInt);
begin
  with TFilterBlurSmallMask_Sum(AData^) do
  begin
    sumA    += maskAlpha * PByte(pPix)^;
    Adiv    += maskAlpha;
  end;
end;

procedure FilterBlurSmallMask(bmp: TCustomUniversalBitmap;
  blurMask: TCustomUniversalBitmap; ABounds: TRect; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var Sum: TFilterBlurSmallMask_Sum;
  accumulate: TBlurAccumulateProc;
  computeAverage: TBlurComputeAverageProc;
begin
  if ADestination.Colorspace <> bmp.Colorspace then
    raise exception.Create('Colorspace mismatch');

  if bmp.Colorspace = TBGRAPixelColorspace then
  begin
    accumulate := @FilterBlurSmallMask_AccumulateSumRGBA;
    computeAverage := @FilterBlurSmallMask_ComputeAverageRGBA
  end
  else if bmp.Colorspace = TByteMaskColorspace then
  begin
    accumulate := @FilterBlurSmallMask_AccumulateSumByte;
    computeAverage := @FilterBlurSmallMask_ComputeAverageByte
  end else
    raise exception.Create('Unexpected colorspace: '+bmp.Colorspace.GetName);

  FilterBlurGeneric(bmp, blurMask, ABounds, ADestination, ACheckShouldStop,
                    @FilterBlurSmallMask_ClearSum,accumulate,computeAverage, @Sum);
end;

//64-bit blur
type
  TFilterBlurMask64_Sum= record
    sumR, sumG, sumB, sumA, Adiv : int64;
  end;

procedure FilterBlurMask64_ClearSum(AData: pointer);
begin
  with TFilterBlurMask64_Sum(AData^) do
  begin
    sumR   := 0;
    sumG   := 0;
    sumB   := 0;
    sumA   := 0;
    Adiv   := 0;
  end;
end;

procedure FilterBlurMask64_ComputeAverageRGBA(AData: pointer; pPix: pointer);
var
  temp, sumAShr1: Int64;
begin
  with TFilterBlurMask64_Sum(AData^) do
  begin
    if Adiv <= 0 then PBGRAPixel(pPix)^ := BGRAPixelTransparent else
    begin
      temp := sumA + Adiv shr 1;
      if temp < Adiv then
        PBGRAPixel(pPix)^ := BGRAPixelTransparent
      else
      begin
        sumAShr1 := sumA shr 1;
        PBGRAPixel(pPix)^.alpha := temp div Adiv;
        PBGRAPixel(pPix)^.red   := clampByte((sumR + sumAShr1) div sumA);
        PBGRAPixel(pPix)^.green := clampByte((sumG + sumAShr1) div sumA);
        PBGRAPixel(pPix)^.blue  := clampByte((sumB + sumAShr1) div sumA);
      end;
    end;
  end;
end;

procedure FilterBlurMask64_ComputeAverageConvertRGBAToByteMask(AData: pointer; pPix: pointer);
var c: TBGRAPixel;
begin
  FilterBlurMask64_ComputeAverageRGBA(AData, @c);
  PByteMask(pPix)^ := BGRAToMask(c);
end;

procedure FilterBlurMask64_ComputeAverageByte(AData: pointer; pPix: pointer);
begin
  with TFilterBlurMask64_Sum(AData^) do
  begin
    if Adiv <= 0 then PByte(pPix)^ := 0 else
      PByte(pPix)^ := (sumA + Adiv shr 1) div Adiv;
  end;
end;

procedure FilterBlurMask64_ComputeAverageConvertByteMaskToRGBA(AData: pointer; pPix: pointer);
begin
  with TFilterBlurMask64_Sum(AData^) do
  begin
    if Adiv <= 0 then PBGRAPixel(pPix)^ := BGRAPixelTransparent else
      PBGRAPixel(pPix)^ := MaskToBGRA(TByteMask.New((sumA + Adiv shr 1) div Adiv));
  end;
end;

procedure FilterBlurMask64_AccumulateSumRGBA(AData: pointer; pPix: pointer; maskAlpha: NativeInt);
var
  pixMaskAlpha: NativeInt;
  tempPixel: TBGRAPixel;
begin
  with TFilterBlurMask64_Sum(AData^) do
  begin
    tempPixel := PBGRAPixel(pPix)^;
    pixMaskAlpha := maskAlpha * tempPixel.alpha;
    sumA    += pixMaskAlpha;
    Adiv    += maskAlpha;
    sumR    += tempPixel.red * pixMaskAlpha;
    sumG    += tempPixel.green * pixMaskAlpha;
    sumB    += tempPixel.blue * pixMaskAlpha;
  end;
end;

procedure FilterBlurMask64_AccumulateSumByte(AData: pointer; pPix: pointer; maskAlpha: NativeInt);
begin
  with TFilterBlurMask64_Sum(AData^) do
  begin
    sumA    += maskAlpha * PByte(pPix)^;
    Adiv    += maskAlpha;
  end;
end;

procedure FilterBlurMask64(bmp: TCustomUniversalBitmap;
  blurMask: TCustomUniversalBitmap; ABounds: TRect; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var Sum: TFilterBlurMask64_Sum;
  accumulate: TBlurAccumulateProc;
  computeAverage: TBlurComputeAverageProc;
begin
  if ADestination.Colorspace <> bmp.Colorspace then
    raise exception.Create('Colorspace mismatch');

  if bmp.Colorspace = TBGRAPixelColorspace then
  begin
    accumulate := @FilterBlurMask64_AccumulateSumRGBA;
    computeAverage := @FilterBlurMask64_ComputeAverageRGBA
  end
  else if bmp.Colorspace = TByteMaskColorspace then
  begin
    accumulate := @FilterBlurMask64_AccumulateSumByte;
    computeAverage := @FilterBlurMask64_ComputeAverageByte
  end else
    raise exception.Create('Unexpected colorspace: '+bmp.Colorspace.GetName);

  FilterBlurGeneric(bmp, blurMask, ABounds, ADestination, ACheckShouldStop,
                    @FilterBlurMask64_ClearSum,accumulate,computeAverage, @Sum);
end;

//floating point blur
type
  TFilterBlurBigMask_Sum= record
    sumR, sumG, sumB, sumA, Adiv : single;
  end;

procedure FilterBlurBigMask_ClearSum(AData: pointer);
begin
  with TFilterBlurBigMask_Sum(AData^) do
  begin
    sumR   := 0;
    sumG   := 0;
    sumB   := 0;
    sumA   := 0;
    Adiv   := 0;
  end;
end;

procedure FilterBlurBigMask_ComputeAverage(AData: pointer; pPix: pointer);
begin
  with TFilterBlurBigMask_Sum(AData^) do
  if Adiv <= 0 then PBGRAPixel(pPix)^ := BGRAPixelTransparent else
  begin
    PBGRAPixel(pPix)^.alpha := round(sumA/Adiv);
    if PBGRAPixel(pPix)^.alpha = 0 then
      PBGRAPixel(pPix)^ := BGRAPixelTransparent
    else
    begin
      PBGRAPixel(pPix)^.red   := clampByte(round(sumR/sumA));
      PBGRAPixel(pPix)^.green := clampByte(round(sumG/sumA));
      PBGRAPixel(pPix)^.blue  := clampByte(round(sumB/sumA));
    end;
  end;
end;

procedure FilterBlurBigMask_AccumulateSum(AData: pointer; pPix: pointer; maskAlpha: NativeInt);
var
  pixMaskAlpha: NativeInt;
  tempPixel: TBGRAPixel;
begin
  with TFilterBlurBigMask_Sum(AData^) do
  begin
    tempPixel := PBGRAPixel(pPix)^;
    pixMaskAlpha := maskAlpha * tempPixel.alpha;
    sumA    += pixMaskAlpha;
    Adiv    += maskAlpha;
    sumR    += tempPixel.red * pixMaskAlpha;
    sumG    += tempPixel.green * pixMaskAlpha;
    sumB    += tempPixel.blue * pixMaskAlpha;
  end;
end;

procedure FilterBlurBigMask(bmp: TCustomUniversalBitmap;
  blurMask: TCustomUniversalBitmap; ABounds: TRect; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var Sum: TFilterBlurBigMask_Sum;
begin
  if ADestination.Colorspace <> bmp.Colorspace then
    raise exception.Create('Colorspace mismatch');
  if bmp.Colorspace <> TBGRAPixelColorspace then
    raise exception.Create('Unexpected colorspace: '+bmp.Colorspace.GetName);
  FilterBlurGeneric(bmp, blurMask, ABounds, ADestination, ACheckShouldStop,
                    @FilterBlurBigMask_ClearSum,
                    @FilterBlurBigMask_AccumulateSum,
                    @FilterBlurBigMask_ComputeAverage, @Sum);
end;

constructor TBoxBlurTask.Create(bmp: TBGRACustomBitmap; ABounds: TRect;
  radius: single);
begin
  SetSource(bmp);
  FBounds := ABounds;
  FRadiusX := radius;
  FRadiusY := radius;
end;

constructor TBoxBlurTask.Create(bmp: TBGRACustomBitmap; ABounds: TRect;
  radiusX, radiusY: single);
begin
  SetSource(bmp);
  FBounds := ABounds;
  FRadiusX := max(radiusX,0);
  FRadiusY := max(radiusY,0);
end;

procedure TBoxBlurTask.DoExecute;
begin
  FilterBlurBox(FSource,FBounds,FRadiusX,FRadiusY,Destination,@GetShouldStop);
end;

end.

