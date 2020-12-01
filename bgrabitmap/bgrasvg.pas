// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRASVG;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, DOM, BGRAUnits, BGRASVGShapes,
  BGRACanvas2D, BGRASVGType, FPimage;

{ An SVG file has a width and height which describe the viewport size. It is not however really
  the size of the content. The latter is defined as a rectangle called viewbox. There are thus
  various ways of drawing an SVG file. If the viewbox is not specified, then it is a rectangle
  with origin (0,0) and with the same size as the viewport. In this case, a pixel in the viewbox
  corresponds to a pixel in the viewport. Otherwise, viewbox is scaled to fit the viewport.

  ------ Different DPIs -----------------

  The default DPI of the SVG file is the DPI used to convert between pixel units and other units
  like centimeters or inches. In general, this should not be used to scale the image, because it
  can make the SVG measures inconsistent. It should rather be set to the DPI which was used
  internally and which should be 96 according to CSS specifications. Some programs however
  use a different DPI so it can be useful to change it according to the source program.

  The destination DPI that can be specified when drawing can be used to scale the SVG. For example
  you can prepare your SVG to be at the correct size for 96 DPI display and then draw them
  on a display for which the apparent DPI is different (this can be set systemwide on Windows and
  Linux).

  ------ Various ways of scaling --------

  There are various functions to draw and also to get the presentation matrix. The latter is
  a matrix, not including the translation to x and y, that contains all transforms to apply
  to the viewbox coordinates. The drawing functions are Draw and StretchDraw and to get the
  presentation matrix the functions are GetPresentationMatrix and GetStretchPresentationMatrix
  respectively.

  Drawing in user coordinates, i.e. without taking account of the viewbox or viewport size. One
  can achieve this by calling the Draw function with an x and y coordinates and a destination
  unit/DPI. You would do that only if you want to make use of the viewbox offset or
  if you already have applied all necessary transforms. The corresponding presentation matrix
  is the identity.

  Drawing with a specific alignment in user coordinates. This can be done by calling Draw
  with horizontal and vertical alignment, x and y coordinates to align to, a destination unit/DPI
  and Scaled set to False.

  Drawing with a specific alignment, scaled to view port units. This will draw the SVG at its
  expected size but apply the alignment you want. This is what you would want to do if you
  want to customize the alignment. One can achieve this as above but by specifying the Scaled
  parameter to True.

  Drawing inside a specified rectangle, not preserving the aspect ratio. In this case, you
  want a certain size and the original size does not matter much. The viewbox of the SVG is
  stretched to fit the rectangle. One can achieve this by calling StretchDraw with the
  specified rectangle and the UseSvgAspectRatio to False.

  Drawing inside a rectangle, using the parameter of the SVG about the aspect ratio. In this case,
  the SVG may either be stretched as before, scaled to fit the rectangle or scaled to cover it.
  Also the SVG may be set with a certain horizontal and vertical alignment within the rectangle.
  One can draw this way by calling StretchDraw with the UseSvgAspectRatio set to True.
  You would typically use this method to show the SVG as it was intended to look like but in
  a custom container by suppling WidthAsPixel and HeightAsPixel for the rectangle size.

  Drawing inside a rectangle, preserving the aspect ratio. In this case, you call StretchDraw with
  a horizontal and vertical alignment, the rectangle and the Slice parameter. When Slice is set
  to False, the viewbox will fit the rectangle. If Slice is to True then the viewbox may overflow
  the rectangle so that it covers the whole surface. Using this function, one can have control
  over the way the SVG is scaled. Slice is useful for background.

  Drawing inside a rectangle of the size defined by the SVG. That will draw the SVG as it is
  supposed to look like. One can do that by calling StretchDraw with a unit parameter. You would
  typically use pixel units and this will draw to the size WidthAsPixel by HeightAsPixel.

  }

type
  TCSSUnit = BGRAUnits.TCSSUnit;

const
  cuCustom = BGRAUnits.cuCustom;
  cuPixel = BGRAUnits.cuPixel;
  cuCentimeter = BGRAUnits.cuCentimeter;
  cuMillimeter = BGRAUnits.cuMillimeter;
  cuInch = BGRAUnits.cuInch;
  cuPica = BGRAUnits.cuPica;
  cuPoint = BGRAUnits.cuPoint;
  cuFontEmHeight = BGRAUnits.cuFontEmHeight;
  cuFontXHeight = BGRAUnits.cuFontXHeight;
  cuPercent = BGRAUnits.cuPercent;

type

  { TSVGUnits }

  TSVGUnits = class(TCSSUnitConverter)
  private
    FOnRecompute: TSVGRecomputeEvent;
    procedure Recompute;
    procedure SetOnRecompute(AValue: TSVGRecomputeEvent);
  protected
    FSvg: TDOMElement;
    FContainerHeight: TFloatWithCSSUnit;
    FContainerWidth: TFloatWithCSSUnit;
    FDefaultDpi: PSingle;

    //fetched or computed
    FViewBox: TSVGViewBox;
    FPreserveAspectRatio: TSVGPreserveAspectRatio;
    FViewPortSize, FProportionalViewSize, FStretchedViewSize: TSVGSize;

    procedure SetContainerHeight(AValue: TFloatWithCSSUnit);
    procedure SetContainerWidth(AValue: TFloatWithCSSUnit);
    function GetDpiX: single; override;
    function GetDpiY: single; override;
    function GetCustomOrigin: TPointF;
    procedure SetCustomOrigin(AValue: TPointF);
    procedure SetViewBox(AValue: TSVGViewBox);
  public
    procedure SetDefaultDpiAndOrigin;
    constructor Create(ASvg: TDOMElement; ADefaultDpi: PSingle);
    function GetStretchRectF(AViewPort: TRectF; const par: TSVGPreserveAspectRatio): TRectF;
    property ViewBox: TSVGViewBox read FViewBox write SetViewBox;
    property ViewPortSize: TSVGSize read FViewPortSize;
    property ProportionalViewSize: TSVGSize read FProportionalViewSize;
    property PreserveAspectRatio: TSVGPreserveAspectRatio read FPreserveAspectRatio;
    property StretchedViewSize: TSVGSize read FStretchedViewSize;
    property CustomOrigin: TPointF read GetCustomOrigin write SetCustomOrigin;
    property ContainerWidth: TFloatWithCSSUnit read FContainerWidth write SetContainerWidth;
    property ContainerHeight: TFloatWithCSSUnit read FContainerHeight write SetContainerHeight;
    property OnRecompute: TSVGRecomputeEvent read FOnRecompute write SetOnRecompute;
  end;

  { TBGRASVG }

  TBGRASVG = class(TSVGCustomElement)
  private
    function GetComputedHeight: TFloatWithCSSUnit;
    function GetComputedWidth: TFloatWithCSSUnit;
    function GetContainerHeight: TFloatWithCSSUnit;
    function GetContainerHeightAsPixel: single;
    function GetContainerWidth: TFloatWithCSSUnit;
    function GetContainerWidthAsPixel: single;
    function GetFontSize: TFloatWithCSSUnit;
    function GetHeight: TFloatWithCSSUnit;
    function GetHeightAsCm: single;
    function GetHeightAsInch: single;
    function GetHeightAsPixel: single;
    function GetLayer(AIndex: integer): TSVGGroup;
    function GetLayerCount: integer;
    function GetPreserveAspectRatio: TSVGPreserveAspectRatio;
    function GetUnits: TSVGUnits;
    function GetUTF8String: utf8string;
    function GetViewBox: TSVGViewBox; overload;
    function GetViewBox(AUnit: TCSSUnit): TSVGViewBox; overload;
    procedure GetViewBoxIndirect(AUnit: TCSSUnit; out AViewBox: TSVGViewBox);
    function GetViewMin(AUnit: TCSSUnit): TPointF;
    function GetViewSize(AUnit: TCSSUnit): TPointF;
    function GetVisualHeight: TFloatWithCSSUnit;
    function GetVisualHeightAsPixel: single;
    function GetVisualWidth: TFloatWithCSSUnit;
    function GetVisualWidthAsPixel: single;
    function GetWidth: TFloatWithCSSUnit;
    function GetWidthAsCm: single;
    function GetWidthAsInch: single;
    function GetWidthAsPixel: single;
    function GetZoomable: boolean;
    procedure SetContainerHeight(AValue: TFloatWithCSSUnit);
    procedure SetContainerHeightAsPixel(AValue: single);
    procedure SetContainerWidth(AValue: TFloatWithCSSUnit);
    procedure SetContainerWidthAsPixel(AValue: single);
    procedure SetDefaultDpi(AValue: single);
    procedure SetFontSize(AValue: TFloatWithCSSUnit);
    procedure SetHeight(AValue: TFloatWithCSSUnit);
    procedure SetHeightAsCm(AValue: single);
    procedure SetHeightAsInch(AValue: single);
    procedure SetHeightAsPixel(AValue: single);
    procedure SetPreserveAspectRatio(AValue: TSVGPreserveAspectRatio);
    procedure SetUTF8String(AValue: utf8string);
    procedure SetViewBox(AValue: TSVGViewBox);
    procedure SetWidth(AValue: TFloatWithCSSUnit);
    procedure SetWidthAsCm(AValue: single);
    procedure SetWidthAsInch(AValue: single);
    procedure SetWidthAsPixel(AValue: single);
    procedure SetZoomable(AValue: boolean);
  protected
    FXml: TXMLDocument;
    FDefaultDpi: single;
    FContent: TSVGContent;
    FDataLink: TSVGDataLink;
    procedure Init(ACreateEmpty: boolean);
    function GetViewBoxAlignment(AHorizAlign: TAlignment; AVertAlign: TTextLayout; AUnit: TCSSUnit): TPointF;
    function GetViewBoxScale: TPointF;
    procedure UnitsRecompute(Sender: TObject);
    procedure SetAttribute(AName: string; AValue: string); override;
  public
    constructor Create; overload;
    constructor Create(AWidth,AHeight: single; AUnit: TCSSUnit); overload;
    constructor Create(AFilenameUTF8: string); overload;
    constructor Create(AStream: TStream); overload;
    constructor CreateFromString(AUTF8String: string);
    destructor Destroy; override;
    function Duplicate: TBGRASVG;
    procedure CropToViewBox(AScale: single = 1);
    procedure LoadFromFile(AFilenameUTF8: string);
    procedure LoadFromStream(AStream: TStream; AURI: UnicodeString = 'stream:');
    procedure LoadFromResource(AFilename: string);
    procedure SaveToFile(AFilenameUTF8: string);
    procedure SaveToStream(AStream: TStream);
    procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; AUnit: TCSSUnit = cuPixel); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; destDpi: single); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; destDpi: TPointF); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y: single; AUnit: TCSSUnit = cuPixel; AScale: boolean = true); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y: single; destDpi: single; AScale: boolean = true); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y: single; destDpi: TPointF; AScale: boolean = true); overload;
    procedure StretchDraw(ACanvas2d: TBGRACanvas2D; x,y,w,h: single; useSvgAspectRatio: boolean = false); overload;
    procedure StretchDraw(ACanvas2d: TBGRACanvas2D; r: TRectF; useSvgAspectRatio: boolean = false); overload;
    procedure StretchDraw(ACanvas2d: TBGRACanvas2D; x, y: single; AUnit: TCSSUnit); overload;
    procedure StretchDraw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment;
      AVertAlign: TTextLayout; x,y,w,h: single; ASlice: boolean = false); overload;
    procedure StretchDraw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment;
      AVertAlign: TTextLayout; r: TRectF; ASlice: boolean = false); overload;
    function GetStretchRectF(AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y,w,h: single; ASlice: boolean = false): TRectF; overload;
    function GetStretchRectF(x,y,w,h: single): TRectF; overload;
    function GetPresentationMatrix(AHorizAlign: TAlignment; AVertAlign: TTextLayout;
      AUnit: TCSSUnit; AScale: boolean): TAffineMatrix;
    function GetStretchPresentationMatrix(w,h: single; useSvgAspectRatio: boolean = false): TAffineMatrix; overload;
    function GetStretchPresentationMatrix(AHorizAlign: TAlignment; AVertAlign: TTextLayout; w,h: single; ASlice: boolean = false): TAffineMatrix; overload;
    function GetStretchPresentationMatrix(AUnit: TCSSUnit): TAffineMatrix; overload;
    function FindElementById(AID: string): TSVGElement; overload;
    function FindElementById(AID: string; AClass: TSVGFactory): TSVGElement; overload;
    procedure ConvertToUnit(AUnit: TCSSUnit); override; //except Width, Height, ContainerWidth, ContainerHeight
    property AsUTF8String: utf8string read GetUTF8String write SetUTF8String;
    property Units: TSVGUnits read GetUnits;
    property FontSize: TFloatWithCSSUnit read GetFontSize write SetFontSize;
    property Width: TFloatWithCSSUnit read GetWidth write SetWidth;
    property Height: TFloatWithCSSUnit read GetHeight write SetHeight;
    property ComputedWidth: TFloatWithCSSUnit read GetComputedWidth;
    property ComputedHeight: TFloatWithCSSUnit read GetComputedHeight;
    property WidthAsPixel: single read GetWidthAsPixel write SetWidthAsPixel;
    property HeightAsPixel: single read GetHeightAsPixel write SetHeightAsPixel;
    property WidthAsCm: single read GetWidthAsCm write SetWidthAsCm;
    property HeightAsCm: single read GetHeightAsCm write SetHeightAsCm;
    property WidthAsInch: single read GetWidthAsInch write SetWidthAsInch;
    property HeightAsInch: single read GetHeightAsInch write SetHeightAsInch;
    property ContainerWidth: TFloatWithCSSUnit read GetContainerWidth write SetContainerWidth;
    property ContainerWidthAsPixel: single read GetContainerWidthAsPixel write SetContainerWidthAsPixel;
    property ContainerHeight: TFloatWithCSSUnit read GetContainerHeight write SetContainerHeight;
    property ContainerHeightAsPixel: single read GetContainerHeightAsPixel write SetContainerHeightAsPixel;
    property Zoomable: boolean read GetZoomable write SetZoomable;
    property ViewBox: TSVGViewBox read GetViewBox write SetViewBox;
    property ViewBoxInUnit[AUnit: TCSSUnit]: TSVGViewBox read GetViewBox;
    property ViewMinInUnit[AUnit: TCSSUnit]: TPointF read GetViewMin;
    property ViewSizeInUnit[AUnit: TCSSUnit]: TPointF read GetViewSize;
    property VisualWidth: TFloatWithCSSUnit read GetVisualWidth;
    property VisualHeight: TFloatWithCSSUnit read GetVisualHeight;
    property VisualWidthAsPixel: single read GetVisualWidthAsPixel;
    property VisualHeightAsPixel: single read GetVisualHeightAsPixel;
    property Attribute[AName: string]: string read GetAttribute write SetAttribute;
    property AttributeDef[AName: string; ADefault: string]: string read GetAttribute;
    property DefaultDpi: single read FDefaultDpi write SetDefaultDpi; //this is not saved in the SVG file
    property Content: TSVGContent read FContent;
    property DataLink: TSVGDataLink read FDataLink;//(for test or internal info)
    property preserveAspectRatio: TSVGPreserveAspectRatio read GetPreserveAspectRatio write SetPreserveAspectRatio;
    property Layer[AIndex: integer]: TSVGGroup read GetLayer;
    property LayerCount: integer read GetLayerCount;
  end;

  { TFPReaderSVG }

  TFPReaderSVG = class(TBGRAImageReader)
    private
      FRenderDpi: single;
      FWidth,FHeight: integer;
      FScale: single;
    protected
      function InternalCheck(Stream: TStream): boolean; override;
      procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    public
      constructor Create; override;
      function GetQuickInfo(AStream: TStream): TQuickImageInfo; override;
      function GetBitmapDraft(AStream: TStream; AMaxWidth, AMaxHeight: integer; out AOriginalWidth,AOriginalHeight: integer): TBGRACustomBitmap; override;
      property RenderDpi: single read FRenderDpi write FRenderDpi;
      property Width: integer read FWidth;
      property Height: integer read FHeight;
      property Scale: single read FScale write FScale;
  end;

procedure RegisterSvgFormat;

implementation

uses XMLRead, XMLWrite, BGRAUTF8, math, xmltextreader, URIParser, BGRATransform;

const SvgNamespace = 'http://www.w3.org/2000/svg';

{ TFPReaderSVG }

function TFPReaderSVG.InternalCheck(Stream: TStream): boolean;
var
  magic: array[1..6] of char;
  prevPos: int64;
  count: LongInt;
begin
  prevPos := Stream.Position;
  count := Stream.Read({%H-}magic, sizeof(magic));
  Stream.Position:= prevPos;
  result:= (count = sizeof(magic)) and ((magic = '<?xml ') or (copy(magic,1,5)='<svg '));
end;

procedure TFPReaderSVG.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  svg: TBGRASVG;
  vsize: TPointF;
  bgra: TBGRACustomBitmap;
  c2d: TBGRACanvas2D;
  y, x: Integer;
  p: PBGRAPixel;
begin
  svg := TBGRASVG.Create(Stream);
  bgra := nil;
  try
    svg.DefaultDpi:= RenderDpi;
    if Img is TBGRACustomBitmap then
      bgra := TBGRACustomBitmap(Img)
    else
      bgra := BGRABitmapFactory.Create;
    if svg.preserveAspectRatio.Preserve then
      vsize := svg.GetViewSize(cuPixel)
      else vsize := PointF(svg.WidthAsPixel, svg.HeightAsPixel);
    bgra.SetSize(ceil(vsize.x*scale),ceil(vsize.y*scale));
    bgra.FillTransparent;
    c2d := TBGRACanvas2D.Create(bgra);
    svg.StretchDraw(c2d, 0,0,bgra.Width,bgra.Height, true);
    c2d.Free;
    if bgra<>Img then
    begin
      Img.SetSize(bgra.Width,bgra.Height);
      for y := 0 to bgra.Height-1 do
      begin
        p := bgra.ScanLine[y];
        for x := 0 to bgra.Width-1 do
        begin
          Img.Colors[x,y] := BGRAToFPColor(p^);
          inc(p);
        end;
      end;
    end;
    FWidth:= bgra.Width;
    FHeight:= bgra.Height;
  finally
    if bgra<>Img then bgra.Free;
    svg.Free;
  end;
end;

constructor TFPReaderSVG.Create;
begin
  inherited Create;
  FRenderDpi:= 96;
  FScale := 1;
end;

function TFPReaderSVG.GetQuickInfo(AStream: TStream): TQuickImageInfo;
var
  svg: TBGRASVG;
  vsize: TPointF;
begin
  svg := TBGRASVG.Create(AStream);
  svg.DefaultDpi:= RenderDpi;
  vsize := svg.GetViewSize(cuPixel);
  svg.Free;
  result.Width:= ceil(vsize.x);
  result.Height:= ceil(vsize.y);
  result.AlphaDepth:= 8;
  result.ColorDepth:= 24;
end;

function TFPReaderSVG.GetBitmapDraft(AStream: TStream; AMaxWidth,
  AMaxHeight: integer; out AOriginalWidth, AOriginalHeight: integer): TBGRACustomBitmap;
var
  svg: TBGRASVG;
  vsize: TPointF;
  c2d: TBGRACanvas2D;
  ratio: Single;
begin
  svg := TBGRASVG.Create(AStream);
  result := nil;
  try
    svg.DefaultDpi:= RenderDpi;
    if svg.preserveAspectRatio.Preserve then
      vsize := svg.GetViewSize(cuPixel)
      else vsize := PointF(svg.WidthAsPixel, svg.HeightAsPixel);
    AOriginalWidth:= ceil(vsize.x);
    AOriginalHeight:= ceil(vsize.y);
    if (vsize.x = 0) or (vsize.y = 0) then exit;
    ratio := min(AMaxWidth/vsize.x, AMaxHeight/vsize.y);
    result := BGRABitmapFactory.Create(ceil(vsize.x*ratio),ceil(vsize.y*ratio));
    if ratio <> 0 then
    begin
      c2d := TBGRACanvas2D.Create(result);
      svg.StretchDraw(c2d, 0,0,result.width,result.height);
      c2d.Free;
    end;
  finally
    svg.Free;
  end;
end;

var AlreadyRegistered: boolean;

procedure RegisterSvgFormat;
begin
  if AlreadyRegistered then exit;
  ImageHandlers.RegisterImageReader ('Scalable Vector Graphic', 'svg', TFPReaderSVG);
  AlreadyRegistered:= True;
end;

function TSVGUnits.GetCustomOrigin: TPointF;
begin
  result := FViewBox.min;
end;

procedure TSVGUnits.SetCustomOrigin(AValue: TPointF);
var newViewBox: TSVGViewBox;
begin
  newViewBox := ViewBox;
  newViewBox.min := AValue;
  ViewBox := newViewBox;
end;

procedure TSVGUnits.Recompute;
begin
  FViewBox:= TSVGViewBox.Parse( FSvg.GetAttribute('viewBox') );
  FPreserveAspectRatio := TSVGPreserveAspectRatio.Parse( FSvg.GetAttribute('preserveAspectRatio') );
  FViewPortSize.width := parseValue(FSvg.GetAttribute('width'), FloatWithCSSUnit(FViewBox.size.x, cuPixel));
  FViewPortSize.height := parseValue(FSvg.GetAttribute('height'), FloatWithCSSUnit(FViewBox.size.y, cuPixel));

  //view port defined as percentage of container
  if FViewPortSize.width.CSSUnit = cuPercent then
  begin
    FViewPortSize.width.value := FViewPortSize.width.value/100*FContainerWidth.value;
    FViewPortSize.width.CSSUnit := FContainerWidth.CSSUnit;
  end;
  if FViewPortSize.height.CSSUnit = cuPercent then
  begin
    FViewPortSize.height.value := FViewPortSize.height.value/100*FContainerHeight.value;
    FViewPortSize.height.CSSUnit := FContainerHeight.CSSUnit;
  end;

  //ensure same unit for width and height
  if FViewPortSize.width.CSSUnit <> FViewPortSize.height.CSSUnit then
    FViewPortSize.height := ConvertHeight(FViewPortSize.height, FViewPortSize.width.CSSUnit, 0);

  //if not viewbox is specified, it is equal to the viewport
  if (FViewBox.size.x <= 0) and (FViewBox.size.y <= 0) then
  begin
    FViewBox.min := PointF(0, 0);
    FViewBox.size.x := ConvertWidth(FViewPortSize.width, cuPixel, 0).value;
    FViewBox.size.y := ConvertHeight(FViewPortSize.height, cuPixel, 0).value;
  end;

  //compute stretching for default SVG aspect ratio (meet)
  FProportionalViewSize := FViewPortSize;
  with GetStretchRectF(RectF(0,0,FViewPortSize.width.value,FViewPortSize.height.value),
       TSVGPreserveAspectRatio.DefaultValue) do
  begin
    FProportionalViewSize.width.value := Width;
    FProportionalViewSize.height.value := Height;
  end;

  //compute stretching according to specified SVG aspect ratio (can be slice or none)
  FStretchedViewSize := FViewPortSize;
  with GetStretchRectF(RectF(0,0,FViewPortSize.width.value,FViewPortSize.height.value),
       PreserveAspectRatio) do
  begin
    FStretchedViewSize.width.value := Width;
    FStretchedViewSize.height.value := Height;
  end;

  ViewBoxWidth := FloatWithCSSUnit(FViewBox.size.x, cuPixel);
  ViewBoxHeight := FloatWithCSSUnit(FViewBox.size.y, cuPixel);

  if Assigned(FOnRecompute) then FOnRecompute(self);
end;

procedure TSVGUnits.SetOnRecompute(AValue: TSVGRecomputeEvent);
begin
  if FOnRecompute=AValue then Exit;
  FOnRecompute:=AValue;
end;

procedure TSVGUnits.SetContainerHeight(AValue: TFloatWithCSSUnit);
begin
  if CompareMem(@FContainerHeight,@AValue,sizeof(TFloatWithCSSUnit)) then Exit;
  FContainerHeight:=AValue;
  Recompute;
end;

procedure TSVGUnits.SetContainerWidth(AValue: TFloatWithCSSUnit);
begin
  if CompareMem(@FContainerWidth,@AValue,sizeof(TFloatWithCSSUnit)) then Exit;
  FContainerWidth:=AValue;
  Recompute;
end;

procedure TSVGUnits.SetDefaultDpiAndOrigin;
begin
  FSvg.RemoveAttribute('viewBox');
  Recompute;
end;

procedure TSVGUnits.SetViewBox(AValue: TSVGViewBox);
begin
  FSvg.SetAttribute('viewBox', formatValue(AValue.min.x)+' '+
    formatValue(AValue.min.y)+' '+
    formatValue(AValue.size.x)+' '+
    formatValue(AValue.size.y));
  Recompute;
end;

function TSVGUnits.GetDpiX: single;
begin
  result := FDefaultDpi^;
end;

function TSVGUnits.GetDpiY: single;
begin
  result := FDefaultDpi^;
end;

constructor TSVGUnits.Create(ASvg: TDOMElement; ADefaultDpi: PSingle);
begin
  FSvg := ASvg;
  FDefaultDpi := ADefaultDpi;
  FContainerWidth := FloatWithCSSUnit(640,cuPixel);
  FContainerHeight := FloatWithCSSUnit(480,cuPixel);
  Recompute;
end;

function TSVGUnits.GetStretchRectF(AViewPort: TRectF; const par: TSVGPreserveAspectRatio): TRectF;
var w0,h0,w,h: single;
begin
  result := AViewPort;
  w0 := AViewPort.Width;
  h0 := AViewPort.Height;

  if par.Preserve and
     (FViewBox.size.x > 0) and (FViewBox.size.y > 0) and
     (w0 > 0) and (h0 > 0) then
  begin
    w := w0;
    h := h0;

    //viewBox wider than viewSize
    if (FViewBox.size.x/FViewBox.size.y > w/h) xor par.Slice then
    begin
      h := w * FViewBox.size.y / FViewBox.size.x;
      result.Bottom := result.Top+h;
    end else
    begin
      w := h * FViewBox.size.x / FViewBox.size.y;
      result.Right := result.Left+w;
    end;
    case par.HorizAlign of
      taCenter: result.Offset((w0-w)/2, 0);
      taRightJustify: result.Offset(w0-w, 0);
    end;
    case par.VertAlign of
      tlCenter: result.Offset(0, (h0-h)/2);
      tlBottom: result.Offset(0, h0-h);
    end;
  end;
end;

{ TBGRASVG }

function TBGRASVG.GetComputedHeight: TFloatWithCSSUnit;
begin
  result := Units.ViewPortSize.height;
end;

function TBGRASVG.GetComputedWidth: TFloatWithCSSUnit;
begin
  result := Units.ViewPortSize.width;
end;

function TBGRASVG.GetVisualHeight: TFloatWithCSSUnit;
begin
  result := Units.StretchedViewSize.height;
end;

function TBGRASVG.GetVisualHeightAsPixel: single;
begin
  result := Units.ConvertHeight(VisualHeight, cuPixel).value;
end;

function TBGRASVG.GetVisualWidth: TFloatWithCSSUnit;
begin
  result := Units.StretchedViewSize.width;
end;

function TBGRASVG.GetContainerHeight: TFloatWithCSSUnit;
begin
  result := Units.ContainerHeight;
end;

function TBGRASVG.GetContainerHeightAsPixel: single;
begin
  result := Units.ConvertHeight(Units.ContainerHeight, cuPixel).value;
end;

function TBGRASVG.GetContainerWidth: TFloatWithCSSUnit;
begin
  result := Units.ContainerWidth;
end;

function TBGRASVG.GetContainerWidthAsPixel: single;
begin
  result := Units.ConvertWidth(Units.ContainerWidth, cuPixel).value;
end;

function TBGRASVG.GetFontSize: TFloatWithCSSUnit;
begin
  result:= GetVerticalAttributeOrStyleWithUnit('font-size',Units.CurrentFontEmHeight,false);
end;

function TBGRASVG.GetHeight: TFloatWithCSSUnit;
begin
  result := TCSSUnitConverter.parseValue(Attribute['height'],FloatWithCSSUnit(Units.ViewBox.size.y,cuCustom));
  if result.CSSUnit = cuCustom then result.CSSUnit:= cuPixel;
end;

function TBGRASVG.GetHeightAsCm: single;
begin
  result := FUnits.ConvertHeight(ComputedHeight,cuCentimeter).value;
end;

function TBGRASVG.GetHeightAsInch: single;
begin
  result := FUnits.ConvertHeight(ComputedHeight,cuInch).value;
end;

function TBGRASVG.GetHeightAsPixel: single;
begin
  result := FUnits.ConvertHeight(ComputedHeight,cuPixel).value;
end;

function TBGRASVG.GetLayer(AIndex: integer): TSVGGroup;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Content.ElementCount-1 do
  begin
    if (Content.ElementObject[i] is TSVGGroup) and
       TSVGGroup(Content.Element[i]).IsLayer then
    begin
      if AIndex = 0 then exit(TSVGGroup(Content.Element[i]));
      dec(AIndex);
    end;
  end;
end;

function TBGRASVG.GetLayerCount: integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to Content.ElementCount-1 do
  begin
    if (Content.ElementObject[i] is TSVGGroup) and
       TSVGGroup(Content.Element[i]).IsLayer then
      inc(result);
  end;
end;

function TBGRASVG.GetPreserveAspectRatio: TSVGPreserveAspectRatio;
begin
  result := Units.PreserveAspectRatio;
end;

function TBGRASVG.GetStretchPresentationMatrix(AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; w, h: single; ASlice: boolean): TAffineMatrix;
var
  rF: TRectF;
begin
  with GetViewBoxAlignment(taLeftJustify, tlTop, cuPixel) do
    result := AffineMatrixTranslation(x, y);
  rF := GetStretchRectF(AHorizAlign, AVertAlign, 0, 0, w, h, ASlice);
  with Units.ViewBox do
  begin
    if size.x > 0 then result := AffineMatrixScale(rF.Width/size.x, 1) * result;
    if size.y > 0 then result := AffineMatrixScale(1, rF.Height/size.y) * result;
  end;
  result := AffineMatrixTranslation(rF.Left, rF.Top) * result;
end;

function TBGRASVG.GetStretchPresentationMatrix(AUnit: TCSSUnit): TAffineMatrix;
var
  w, h: TFloatWithCSSUnit;
begin
  w := ComputedWidth;
  h := ComputedHeight;
  result := GetStretchPresentationMatrix(Units.ConvertWidth(w, AUnit).value,
    Units.ConvertHeight(h, AUnit).value, true);
end;

function TBGRASVG.GetUnits: TSVGUnits;
begin
  result := TSVGUnits(FUnits);
end;

function TBGRASVG.GetUTF8String: utf8string;
var str: TMemoryStream;
begin
  str := TMemoryStream.Create;
  SaveToStream(str);
  setlength(result, str.Size);
  str.Position := 0;
  str.Read(result[1], length(result));
  str.Free;
end;

function TBGRASVG.GetViewBox: TSVGViewBox;
begin
  result := Units.ViewBox;
end;

function TBGRASVG.GetViewBox(AUnit: TCSSUnit): TSVGViewBox;
begin
  GetViewBoxIndirect(AUnit,result);
end;

procedure TBGRASVG.GetViewBoxIndirect(AUnit: TCSSUnit; out AViewBox: TSVGViewBox);
begin
  with Units.ViewBox do
  begin
    AViewBox.min := FUnits.ConvertCoord(min,cuCustom,AUnit);
    AViewBox.size := FUnits.ConvertCoord(size,cuCustom,AUnit);
  end;
end;

function TBGRASVG.GetViewMin(AUnit: TCSSUnit): TPointF;
var
  vb: TSVGViewBox;
begin
  GetViewBoxIndirect(AUnit,vb);
  result:= vb.min;
end;

function TBGRASVG.GetViewSize(AUnit: TCSSUnit): TPointF;
var
  vb: TSVGViewBox;
begin
  GetViewBoxIndirect(AUnit,vb);
  result:= vb.size;
end;

function TBGRASVG.GetVisualWidthAsPixel: single;
begin
  result := Units.ConvertWidth(VisualWidth, cuPixel).value;
end;

function TBGRASVG.GetWidth: TFloatWithCSSUnit;
begin
  result := TCSSUnitConverter.parseValue(Attribute['width'],FloatWithCSSUnit(Units.ViewBox.size.x,cuCustom));
  if result.CSSUnit = cuCustom then result.CSSUnit := cuPixel;
end;

function TBGRASVG.GetWidthAsCm: single;
begin
  result := FUnits.ConvertWidth(ComputedWidth,cuCentimeter).value;
end;

function TBGRASVG.GetWidthAsInch: single;
begin
  result := FUnits.ConvertWidth(ComputedWidth,cuInch).value;
end;

function TBGRASVG.GetWidthAsPixel: single;
begin
  result := FUnits.ConvertWidth(ComputedWidth,cuPixel).value;
end;

function TBGRASVG.GetZoomable: boolean;
begin
  result := AttributeDef['zoomAndPan','magnify']<>'disable';
end;

procedure TBGRASVG.SetContainerHeight(AValue: TFloatWithCSSUnit);
begin
  if AValue.CSSUnit = cuPercent then raise exception.Create('Container width cannot be expressed as percentage');
  Units.ContainerHeight := AValue;
end;

procedure TBGRASVG.SetContainerHeightAsPixel(AValue: single);
begin
  ContainerHeight := FloatWithCSSUnit(AValue, cuPixel);
end;

procedure TBGRASVG.SetContainerWidth(AValue: TFloatWithCSSUnit);
begin
  if AValue.CSSUnit = cuPercent then raise exception.Create('Container width cannot be expressed as percentage');
  Units.ContainerWidth := AValue;
end;

procedure TBGRASVG.SetContainerWidthAsPixel(AValue: single);
begin
  ContainerWidth := FloatWithCSSUnit(AValue, cuPixel);
end;

procedure TBGRASVG.SetAttribute(AName: string; AValue: string);
begin
  AName := trim(AName);
  if compareText(AName,'viewBox')= 0 then AName := 'viewBox' else
  if compareText(AName,'width')=0 then AName := 'width' else
  if compareText(AName,'height')=0 then AName := 'height';
  inherited SetAttribute(AName,AValue);
  if (AName = 'viewBox') or (AName = 'width') or (AName = 'height') then
    Units.Recompute;
end;

procedure TBGRASVG.SetDefaultDpi(AValue: single);
begin
  if FDefaultDpi=AValue then Exit;
  FDefaultDpi:=AValue;
  Units.Recompute;
end;

procedure TBGRASVG.SetFontSize(AValue: TFloatWithCSSUnit);
begin
  SetVerticalAttributeWithUnit('font-size', AValue);
end;

procedure TBGRASVG.SetHeight(AValue: TFloatWithCSSUnit);
begin
  if AValue.CSSUnit = cuPixel then AValue.CSSUnit := cuCustom;
  Attribute['height'] := TCSSUnitConverter.formatValue(AValue);
  Units.Recompute;
end;

procedure TBGRASVG.SetHeightAsCm(AValue: single);
begin
  Height := FloatWithCSSUnit(AValue,cuCentimeter);
end;

procedure TBGRASVG.SetHeightAsInch(AValue: single);
begin
  Height := FloatWithCSSUnit(AValue,cuInch);
end;

procedure TBGRASVG.SetHeightAsPixel(AValue: single);
begin
  Height := FloatWithCSSUnit(AValue,cuPixel);
end;

procedure TBGRASVG.SetPreserveAspectRatio(AValue: TSVGPreserveAspectRatio);
begin
  Attribute['preserveAspectRatio'] := AValue.ToString;
  Units.Recompute;
end;

procedure TBGRASVG.SetUTF8String(AValue: utf8string);
var str: TMemoryStream;
begin
  str:= TMemoryStream.Create;
  str.Write(AValue[1],length(AValue));
  str.Position:= 0;
  LoadFromStream(str);
  str.Free;
end;

{$PUSH}{$OPTIMIZATION OFF} //avoids Internal error 2012090607
procedure TBGRASVG.SetViewBox(AValue: TSVGViewBox);
begin
  Units.ViewBox := AValue;
end;
{$POP}

procedure TBGRASVG.SetWidth(AValue: TFloatWithCSSUnit);
begin
  if AValue.CSSUnit = cuPixel then AValue.CSSUnit := cuCustom;
  Attribute['width'] := TCSSUnitConverter.formatValue(AValue);
  Units.Recompute;
end;

procedure TBGRASVG.SetWidthAsCm(AValue: single);
begin
  Width := FloatWithCSSUnit(AValue,cuCentimeter);
end;

procedure TBGRASVG.SetWidthAsInch(AValue: single);
begin
  Width := FloatWithCSSUnit(AValue,cuInch);
end;

procedure TBGRASVG.SetWidthAsPixel(AValue: single);
begin
  Width := FloatWithCSSUnit(AValue,cuPixel);
end;

procedure TBGRASVG.SetZoomable(AValue: boolean);
begin
  if AValue then
    Attribute['zoomAndPan'] := 'magnify'
  else
    Attribute['zoomAndPan'] := 'disable';
end;

procedure TBGRASVG.Init(ACreateEmpty: boolean);
begin
  FDefaultDpi := 96; //web browser default
  if ACreateEmpty then
  begin
    FXml := TXMLDocument.Create;
    FDomElem := FXml.CreateElement('svg');
    FUnits := TSVGUnits.Create(FDomElem,@FDefaultDpi);
    Units.OnRecompute:= @UnitsRecompute;
    FDataLink := TSVGDataLink.Create(nil);
    FContent := TSVGContent.Create(FDomElem,FUnits,FDataLink);
    FXml.AppendChild(FDomElem);
  end;
end;

function TBGRASVG.GetViewBoxAlignment(AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; AUnit: TCSSUnit): TPointF;
var vb: TSVGViewBox;
begin
  GetViewBoxIndirect(AUnit, vb);
  with vb do
  begin
    case AHorizAlign of
      taCenter: result.x := -(min.x+size.x*0.5);
      taRightJustify: result.x := -(min.x+size.x);
    else
      {taLeftJustify:} result.x := -min.x;
    end;
    case AVertAlign of
      tlCenter: result.y := -(min.y+size.y*0.5);
      tlBottom: result.y := -(min.y+size.y);
    else
      {tlTop:} result.y := -min.y;
    end;
  end;
end;

function TBGRASVG.GetViewBoxScale: TPointF;
var
  svs: TSVGSize;
  vb: TSVGViewBox;
begin
  svs := Units.StretchedViewSize;
  vb := ViewBox;
  if vb.size.x <> 0 then
    result.x := Units.ConvertWidth(svs.width, cuPixel).value / vb.size.x
    else result.x := 1;
  if vb.size.y <> 0 then
    result.y := Units.ConvertHeight(svs.Height, cuPixel).value / vb.size.y
    else result.y := 1;
end;

procedure TBGRASVG.UnitsRecompute(Sender: TObject);
begin
  FContent.Recompute;
end;

constructor TBGRASVG.Create;
begin
  Init(True);
end;

constructor TBGRASVG.Create(AWidth, AHeight: single; AUnit: TCSSUnit);
begin
  Init(True);
  Width := FloatWithCSSUnit(AWidth,AUnit);
  Height := FloatWithCSSUnit(AHeight,AUnit);
end;

constructor TBGRASVG.Create(AFilenameUTF8: string);
begin
  Init(False);
  LoadFromFile(AFilenameUTF8);
end;

constructor TBGRASVG.Create(AStream: TStream);
begin
  Init(False);
  LoadFromStream(AStream);
end;

constructor TBGRASVG.CreateFromString(AUTF8String: string);
begin
  Init(False);
  AsUTF8String:= AUTF8String;
end;

destructor TBGRASVG.Destroy;
begin
  FreeAndNil(FContent);
  FreeAndNil(FDataLink);
  FreeAndNil(FUnits);
  FDomElem:= nil;
  FreeAndNil(FXml);
  inherited Destroy;
end;

function TBGRASVG.Duplicate: TBGRASVG;
var
  stream: TMemoryStream;
  svg: TBGRASVG;
begin
  stream := TMemoryStream.Create;
  svg := nil;
  try
    SaveToStream(stream);
    stream.Position:= 0;
    svg := TBGRASVG.Create;
    svg.DefaultDpi:= DefaultDpi;
    svg.LoadFromStream(stream);
    result := svg;
    svg := nil;
  finally
    svg.Free;
    stream.Free
  end;
end;

procedure TBGRASVG.CropToViewBox(AScale: single);
var w,h: single;
begin
  w:= VisualWidthAsPixel * AScale;
  h:= VisualHeightAsPixel * AScale;
  ViewBox := Units.ViewBox;   // make sure viewbox is explicit
  WidthAsPixel:= w;
  HeightAsPixel:= h;
end;

procedure TBGRASVG.LoadFromFile(AFilenameUTF8: string);
var stream: TStream;
begin
  stream := TFileStreamUTF8.Create(AFilenameUTF8,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(stream, UTF8ToUTF16(FilenameToURI(AFilenameUTF8)));
  finally
    stream.Free;
  end;
end;

procedure TBGRASVG.LoadFromStream(AStream: TStream; AURI: UnicodeString);
var xml: TXMLDocument;
  root: TDOMNode;
  byteOrderMark: packed array[1..3] of byte;
  startPos: int64;
  parser: TDOMParser;
  source: TXMLInputSource;
begin
  //skip utf8 byte order mark
  startPos:= AStream.Position;
  if AStream.Read({%H-}byteOrderMark,sizeof(byteOrderMark)) = 3 then
  begin
    if (byteOrderMark[1] = $ef) and (byteOrderMark[2] = $bb) and (byteOrderMark[3] = $bf) then
      inc(startPos, 3);
  end;
  AStream.Position:= startPos;
  source := TXMLInputSource.Create(AStream);
  source.BaseURI:= AURI;
  parser := TDOMParser.Create;
  parser.Options.PreserveWhitespace:= true;
  try
    parser.Parse(source, xml);
  finally
    parser.Free;
    source.Free;
  end;
  root := xml.FirstChild;
  while (root <> nil) and not (root is TDOMElement) do root := root.NextSibling;
  if root = nil then
  begin
    xml.Free;
    raise exception.Create('Root node not found');
  end;
  FreeAndNil(FContent);
  FreeAndNil(FDataLink);
  FreeAndNil(FUnits);
  FreeAndNil(FXml);
  FXml := xml;
  FDomElem := root as TDOMElement;
  FUnits := TSVGUnits.Create(FDomElem,@FDefaultDpi);
  Units.OnRecompute:= @UnitsRecompute;
  FDataLink := TSVGDataLink.Create(nil);
  FContent := TSVGContent.Create(FDomElem,FUnits,FDataLink);
end;

procedure TBGRASVG.LoadFromResource(AFilename: string);
var
  stream: TStream;
begin
  stream := BGRAResource.GetResourceStream(AFilename);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TBGRASVG.SaveToFile(AFilenameUTF8: string);
var stream: TFileStreamUTF8;
begin
  stream := TFileStreamUTF8.Create(AFilenameUTF8,fmCreate);
  try
    SaveToStream(stream);
  finally
    stream.free;
  end;
end;

procedure TBGRASVG.SaveToStream(AStream: TStream);
begin
  if Attribute['xmlns'] = '' then Attribute['xmlns'] := SvgNamespace;
  if (NamespaceURI['xlink'] = '') and NeedNamespace('xlink') then
    NamespaceURI['xlink'] := 'http://www.w3.org/1999/xlink';
  WriteXMLFile(FXml, AStream);
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; x, y: single; AUnit: TCSSUnit; AScale: boolean);
var prevMatrix: TAffineMatrix;
begin
  prevMatrix := ACanvas2d.matrix;
  ACanvas2d.translate(x,y);
  if AScale then
    with GetViewBoxScale do ACanvas2d.scale(x, y);
  with GetViewBoxAlignment(AHorizAlign,AVertAlign,cuPixel) do ACanvas2d.translate(x,y);
  Draw(ACanvas2d, 0,0, AUnit);
  ACanvas2d.matrix := prevMatrix;
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; x, y: single; destDpi: single; AScale: boolean);
begin
  Draw(ACanvas2d, AHorizAlign,AVertAlign, x,y, PointF(destDpi,destDpi), AScale);
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; x, y: single; destDpi: TPointF; AScale: boolean);
begin
  ACanvas2d.save;
  ACanvas2d.translate(x,y);
  ACanvas2d.scale(destDpi.x/Units.DpiX,destDpi.y/Units.DpiY);
  if AScale then
    with GetViewBoxScale do ACanvas2d.scale(x, y);
  with GetViewBoxAlignment(AHorizAlign,AVertAlign, cuPixel) do ACanvas2d.translate(x,y);
  Draw(ACanvas2d, 0,0, cuPixel);
  ACanvas2d.restore;
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; x, y: single; AUnit: TCSSUnit);
var prevLinearBlend: boolean;
  prevFontSize: TFloatWithCSSUnit;
begin
  prevLinearBlend:= ACanvas2d.linearBlend;
  acanvas2d.linearBlend := true;
  ACanvas2d.save;
  ACanvas2d.translate(x,y);
  ACanvas2d.strokeMatrix := ACanvas2d.matrix;
  prevFontSize := EnterFontSize(true);
  Content.Draw(ACanvas2d,AUnit);
  ExitFontSize(prevFontSize);
  ACanvas2d.restore;
  ACanvas2d.linearBlend := prevLinearBlend;
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; x, y: single; destDpi: single);
begin
  Draw(ACanvas2d, x,y, PointF(destDpi,destDpi));
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; x, y: single; destDpi: TPointF);
begin
  ACanvas2d.save;
  ACanvas2d.translate(x,y);
  ACanvas2d.scale(destDpi.x/Units.DpiX,destDpi.y/Units.DpiY);
  Draw(ACanvas2d, 0,0, cuPixel);
  ACanvas2d.restore;
end;

procedure TBGRASVG.StretchDraw(ACanvas2d: TBGRACanvas2D; x, y, w, h: single; useSvgAspectRatio: boolean);
var vb: TSVGViewBox;
begin
  if useSvgAspectRatio then
  begin
    with preserveAspectRatio do
      StretchDraw(ACanvas2d, HorizAlign, VertAlign, x,y,w,h, Slice);
    exit;
  end;
  ACanvas2d.save;
  ACanvas2d.translate(x,y);
  ACanvas2d.strokeResetTransform;
  GetViewBoxIndirect(cuPixel,vb);
  with vb do
  begin
    ACanvas2d.translate(-min.x,-min.y);
    if size.x <> 0 then
      ACanvas2d.scale(w/size.x,1);
    if size.y <> 0 then
      ACanvas2d.scale(1,h/size.y);
  end;
  Draw(ACanvas2d, 0,0, cuPixel);
  ACanvas2d.restore;
end;

procedure TBGRASVG.StretchDraw(ACanvas2d: TBGRACanvas2D; r: TRectF; useSvgAspectRatio: boolean);
begin
  StretchDraw(ACanvas2d, r.Left, r.Top, r.Width, r.Height, useSvgAspectRatio);
end;

procedure TBGRASVG.StretchDraw(ACanvas2d: TBGRACanvas2D; x, y: single; AUnit: TCSSUnit);
var
  w, h: TFloatWithCSSUnit;
begin
  w := ComputedWidth;
  h := ComputedHeight;
  StretchDraw(ACanvas2d, x, y, Units.ConvertWidth(w, AUnit).value,
    Units.ConvertHeight(h, AUnit).value, true);
end;

procedure TBGRASVG.StretchDraw(ACanvas2d: TBGRACanvas2D;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; x, y, w, h: single;
  ASlice: boolean);
var r: TRectF;
begin
  r := GetStretchRectF(AHorizAlign,AVertAlign, x, y, w, h, ASlice);
  StretchDraw(ACanvas2d, r);
end;

procedure TBGRASVG.StretchDraw(ACanvas2d: TBGRACanvas2D;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; r: TRectF; ASlice: boolean);
begin
  StretchDraw(ACanvas2d, AHorizAlign, AVertAlign, r.Left, r.Top, r.Width, r.Height, ASlice);
end;

function TBGRASVG.GetStretchRectF(x,y,w,h: single): TRectF;
begin
  result := Units.GetStretchRectF(RectWithSizeF(x,y,w,h), preserveAspectRatio);
end;

function TBGRASVG.GetPresentationMatrix(AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; AUnit: TCSSUnit; AScale: boolean): TAffineMatrix;
begin
  with GetViewBoxAlignment(AHorizAlign, AVertAlign, AUnit) do
    result := AffineMatrixTranslation(x, y);
  if AScale then
    with GetViewBoxScale do
      result := AffineMatrixScale(x, y) * result;
end;

function TBGRASVG.GetStretchPresentationMatrix(w, h: single;
  useSvgAspectRatio: boolean): TAffineMatrix;
var
  rF: TRectF;
begin
  if not useSvgAspectRatio then
  begin
    with GetViewBoxAlignment(taLeftJustify, tlTop, cuPixel) do
      result := AffineMatrixTranslation(x, y);
    with Units.ViewBox do
    begin
      if size.x > 0 then result := AffineMatrixScale(w/size.x, 1) * result;
      if size.y > 0 then result := AffineMatrixScale(1, h/size.y) * result;
    end;
  end else
  begin
    with GetViewBoxAlignment(taLeftJustify, tlTop, cuPixel) do
      result := AffineMatrixTranslation(x, y);
    rF := GetStretchRectF(0,0,w,h);
    with Units.ViewBox do
    begin
      if size.x > 0 then result := AffineMatrixScale(rF.Width/size.x, 1) * result;
      if size.y > 0 then result := AffineMatrixScale(1, rF.Height/size.y) * result;
    end;
    result := AffineMatrixTranslation(rF.Left, rF.Top) * result;
  end;
end;

function TBGRASVG.GetStretchRectF(AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; x, y, w, h: single; ASlice: boolean): TRectF;
var
  aspect: TSVGPreserveAspectRatio;
begin
  aspect.HorizAlign:= AHorizAlign;
  aspect.VertAlign:= AVertAlign;
  aspect.Preserve:= true;
  aspect.Slice:= ASlice;
  result := Units.GetStretchRectF(RectWithSizeF(x, y, w, h), aspect);
end;

function TBGRASVG.FindElementById(AID: string): TSVGElement;
begin
  result := DataLink.FindElementById(AId, TSVGElement);
end;

function TBGRASVG.FindElementById(AID: string; AClass: TSVGFactory): TSVGElement;
begin
  result := DataLink.FindElementById(AId, AClass);
end;

procedure TBGRASVG.ConvertToUnit(AUnit: TCSSUnit);
var
  prevFontSize: TFloatWithCSSUnit;
begin
  prevFontSize := Units.CurrentFontEmHeight;
  Units.CurrentFontEmHeight := Units.RootFontEmHeight;
  if HasAttribute('font-size') then
    SetVerticalAttributeWithUnit('font-size', Units.ConvertHeight(GetVerticalAttributeWithUnit('font-size'), AUnit));
  Units.CurrentFontEmHeight := prevFontSize;

  prevFontSize := EnterFontSize(true);
  inherited ConvertToUnit(AUnit);
  Content.ConvertToUnit(AUnit);
  ExitFontSize(prevFontSize);
end;

initialization

  DefaultBGRAImageReader[ifSvg] := TFPReaderSVG;

end.

