unit BGRASVG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, laz2_DOM, BGRAUnits, BGRASVGShapes,
  BGRACanvas2D, BGRASVGType;

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
  TSVGViewBox = record
    min, size: TPointF;
  end;
  TSVGSize = record
    width, height: TFloatWithCSSUnit;
  end;

  { TSVGUnits }

  TSVGUnits = class(TCSSUnitConverter)
  private
    FOnRecompute: TSVGRecomputeEvent;
    FViewOffset: TPointF;
    function GetCustomDpi: TPointF;
    procedure Recompute;
    procedure SetOnRecompute(AValue: TSVGRecomputeEvent);
  protected
    FSvg: TDOMElement;
    FViewBox: TSVGViewBox;
    FOriginalViewSize, FProportionalViewSize: TSVGSize;

    FDefaultUnitHeight, FDefaultUnitWidth: TFloatWithCSSUnit;
    FDefaultDpi: PSingle;
    FUseDefaultDPI: boolean;
    FDpiScaleX,FDpiScaleY: single;
    FContainerHeight: TFloatWithCSSUnit;
    FContainerWidth: TFloatWithCSSUnit;
    procedure SetContainerHeight(AValue: TFloatWithCSSUnit);
    procedure SetContainerWidth(AValue: TFloatWithCSSUnit);
    function GetDefaultUnitHeight: TFloatWithCSSUnit; override;
    function GetDefaultUnitWidth: TFloatWithCSSUnit; override;
    function GetDpiX: single; override;
    function GetDpiY: single; override;
    function GetCustomDpiX: single;
    function GetCustomDpiY: single;
    function GetCustomOrigin: TPointF;
    procedure SetCustomOrigin(AValue: TPointF);
    procedure SetViewBox(AValue: TSVGViewBox);
    procedure SetCustomDpi(ADpi: TPointF);
    function GetDpiScaleX: single; override;
    function GetDpiScaleY: single; override;
    function GetDPIScaled: boolean; override;
  public
    procedure SetDefaultDpiAndOrigin;
    constructor Create(ASvg: TDOMElement; ADefaultDpi: PSingle);
    function GetStretchRectF(AViewSize: TRectF; par: TSVGPreserveAspectRatio): TRectF;
    property ViewBox: TSVGViewBox read FViewBox write SetViewBox;
    property OriginalViewSize: TSVGSize read FOriginalViewSize;
    property ProportionalViewSize: TSVGSize read FProportionalViewSize;
    property ViewOffset: TPointF read FViewOffset;
    property CustomOrigin: TPointF read GetCustomOrigin write SetCustomOrigin;
    property CustomDpiX: single read GetCustomDpiX;
    property CustomDpiY: single read GetCustomDpiY;
    property CustomDpi: TPointF read GetCustomDpi write SetCustomDpi;
    property ContainerWidth: TFloatWithCSSUnit read FContainerWidth write SetContainerWidth;
    property ContainerHeight: TFloatWithCSSUnit read FContainerHeight write SetContainerHeight;
    property OnRecompute: TSVGRecomputeEvent read FOnRecompute write SetOnRecompute;
  end;

  { TBGRASVG }

  TBGRASVG = class
  private
    function GetAttribute(AName: string): string;
    function GetAttribute(AName: string; ADefault: string): string;
    function GetCustomDpi: TPointF;
    function GetHeight: TFloatWithCSSUnit;
    function GetHeightAsCm: single;
    function GetHeightAsInch: single;
    function GetPreserveAspectRatio: TSVGPreserveAspectRatio;
    function GetUTF8String: utf8string;
    function GetViewBox: TSVGViewBox;
    function GetViewBox(AUnit: TCSSUnit): TSVGViewBox;
    procedure GetViewBoxIndirect(AUnit: TCSSUnit; out AViewBox: TSVGViewBox);
    function GetWidth: TFloatWithCSSUnit;
    function GetWidthAsCm: single;
    function GetWidthAsInch: single;
    function GetZoomable: boolean;
    procedure SetAttribute(AName: string; AValue: string);
    procedure SetCustomDpi(AValue: TPointF);
    procedure SetDefaultDpi(AValue: single);
    procedure SetHeight(AValue: TFloatWithCSSUnit);
    procedure SetHeightAsCm(AValue: single);
    procedure SetHeightAsInch(AValue: single);
    procedure SetPreserveAspectRatio(AValue: TSVGPreserveAspectRatio);
    procedure SetUTF8String(AValue: utf8string);
    procedure SetViewBox(AValue: TSVGViewBox);
    procedure SetWidth(AValue: TFloatWithCSSUnit);
    procedure SetWidthAsCm(AValue: single);
    procedure SetWidthAsInch(AValue: single);
    procedure SetZoomable(AValue: boolean);
  protected
    FXml: TXMLDocument;
    FRoot: TDOMElement;
    FUnits: TSVGUnits;
    FDefaultDpi: single;
    FContent: TSVGContent;
    FDataLink: TSVGDataLink;
    procedure Init(ACreateEmpty: boolean);
    function GetViewBoxAlignment(AHorizAlign: TAlignment; AVertAlign: TTextLayout): TPointF;
    procedure UnitsRecompute(Sender: TObject);
  public
    constructor Create; overload;
    constructor Create(AWidth,AHeight: single; AUnit: TCSSUnit); overload;
    constructor Create(AWidth,AHeight: single; AUnit: TCSSUnit; ACustomDPI: single); overload;
    constructor Create(AFilenameUTF8: string); overload;
    constructor Create(AStream: TStream); overload;
    constructor CreateFromString(AUTF8String: string);
    destructor Destroy; override;
    procedure LoadFromFile(AFilenameUTF8: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(AFilenameUTF8: string);
    procedure SaveToStream(AStream: TStream);
    procedure Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y: single; AUnit: TCSSUnit = cuPixel); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y: single; destDpi: single); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y: single; destDpi: TPointF); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; AUnit: TCSSUnit = cuPixel); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; destDpi: single); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; destDpi: TPointF); overload;
    procedure StretchDraw(ACanvas2d: TBGRACanvas2D; x,y,w,h: single; useSvgAspectRatio: boolean = false); overload;
    procedure StretchDraw(ACanvas2d: TBGRACanvas2D; r: TRectF; useSvgAspectRatio: boolean = false); overload;
    procedure StretchDraw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y,w,h: single); overload;
    function GetStretchRectF(AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y,w,h: single): TRectF;
    property AsUTF8String: utf8string read GetUTF8String write SetUTF8String;
    property Units: TSVGUnits read FUnits;
    property Width: TFloatWithCSSUnit read GetWidth write SetWidth;
    property Height: TFloatWithCSSUnit read GetHeight write SetHeight;
    property WidthAsCm: single read GetWidthAsCm write SetWidthAsCm;
    property HeightAsCm: single read GetHeightAsCm write SetHeightAsCm;
    property WidthAsInch: single read GetWidthAsInch write SetWidthAsInch;
    property HeightAsInch: single read GetHeightAsInch write SetHeightAsInch;
    property Zoomable: boolean read GetZoomable write SetZoomable;
    property ViewBox: TSVGViewBox read GetViewBox write SetViewBox;
    property ViewBoxInUnit[AUnit: TCSSUnit]: TSVGViewBox read GetViewBox;
    property Attribute[AName: string]: string read GetAttribute write SetAttribute;
    property AttributeDef[AName: string; ADefault: string]: string read GetAttribute;
    property DefaultDpi: single read FDefaultDpi write SetDefaultDpi; //this is not saved in the SVG file
    property CustomDpi: TPointF read GetCustomDpi write SetCustomDpi;
    property Content: TSVGContent read FContent;
    property DataLink: TSVGDataLink read FDataLink;//(for test or internal info)
    property preserveAspectRatio: TSVGPreserveAspectRatio read GetPreserveAspectRatio write SetPreserveAspectRatio;
  end;

implementation

uses laz2_XMLRead, laz2_XMLWrite, BGRAUTF8;

const SvgNamespace = 'http://www.w3.org/2000/svg';

function TSVGUnits.GetCustomDpiX: single;
var pixSize: single;
begin
  with GetDefaultUnitWidth do
    pixSize := Convert(value,CSSUnit,cuInch,FDefaultDpi^);
  if pixSize = 0 then
    result := 0
  else
    result := 1/pixSize;
end;

function TSVGUnits.GetCustomDpiY: single;
var pixSize: single;
begin
  with GetDefaultUnitHeight do
    pixSize := Convert(value,CSSUnit,cuInch,FDefaultDpi^);
  if pixSize = 0 then
    result := 0
  else
    result := 1/pixSize;
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

function TSVGUnits.GetCustomDpi: TPointF;
begin
  result := PointF(CustomDpiX,CustomDpiY);
end;

procedure TSVGUnits.Recompute;
var viewBoxStr: string;

  function parseNextFloat: single;
  var
    idxSpace,{%H-}errPos: integer;
  begin
    idxSpace:= pos(' ',viewBoxStr);
    if idxSpace <> 0 then
      val(copy(viewBoxStr,1,idxSpace-1),result,errPos)
    else
      result := 0;
    delete(viewBoxStr,1,idxSpace);
    while (viewBoxStr <> '') and (viewBoxStr[1] = ' ') do delete(viewBoxStr,1,1);
  end;

begin
  viewBoxStr := trim(FSvg.GetAttribute('viewBox'))+' ';
  FViewBox.min.x := parseNextFloat;
  FViewBox.min.y := parseNextFloat;
  FViewBox.size.x := parseNextFloat;
  FViewBox.size.y := parseNextFloat;

  FOriginalViewSize.width := parseValue(FSvg.GetAttribute('width'), FloatWithCSSUnit(FViewBox.size.x, cuPixel));
  if FOriginalViewSize.width.CSSUnit = cuCustom then FOriginalViewSize.width.CSSUnit := cuPixel;
  if FOriginalViewSize.width.CSSUnit = cuPercent then
  begin
    FOriginalViewSize.width.value := FOriginalViewSize.width.value/100*FContainerWidth.value;
    FOriginalViewSize.width.CSSUnit := FContainerWidth.CSSUnit;
  end;
  FOriginalViewSize.height := parseValue(FSvg.GetAttribute('height'), FloatWithCSSUnit(FViewBox.size.y, cuPixel));
  if FOriginalViewSize.height.CSSUnit = cuCustom then FOriginalViewSize.height.CSSUnit := cuPixel;
  if FOriginalViewSize.height.CSSUnit = cuPercent then
  begin
    FOriginalViewSize.height.value := FOriginalViewSize.height.value/100*FContainerHeight.value;
    FOriginalViewSize.height.CSSUnit := FContainerHeight.CSSUnit;
  end;
  if FOriginalViewSize.height.CSSUnit <> FOriginalViewSize.width.CSSUnit then
    FOriginalViewSize.height := ConvertHeight(FOriginalViewSize.height, FOriginalViewSize.width.CSSUnit);

  FProportionalViewSize := FOriginalViewSize;
  with GetStretchRectF(RectF(0,0,FOriginalViewSize.width.value,FOriginalViewSize.height.value), TSVGPreserveAspectRatio.DefaultValue) do
  begin
    FProportionalViewSize.width.value := Right-Left;
    FProportionalViewSize.height.value := Bottom-Top;
  end;

  if (FViewBox.size.x <= 0) and (FViewBox.size.y <= 0) then
    begin
      FDefaultUnitWidth.value:= 1/FDefaultDpi^;
      FDefaultUnitWidth.CSSUnit := cuInch;
      FDefaultUnitHeight.value:= 1/FDefaultDpi^;
      FDefaultUnitHeight.CSSUnit := cuInch;
      FUseDefaultDPI := true;
      FDpiScaleX := 1;
      FDpiScaleY := 1;
      FViewBox.min := PointF(0,0);
      FViewBox.size.x := ConvertWidth(FProportionalViewSize.width,cuCustom).value;
      FViewBox.size.y := ConvertHeight(FProportionalViewSize.height,cuCustom).value;
    end else
    begin
      FDefaultUnitWidth.value := FProportionalViewSize.width.value/FViewBox.size.x;
      FDefaultUnitWidth.CSSUnit := FProportionalViewSize.width.CSSUnit;
      if FDefaultUnitWidth.CSSUnit = cuCustom then
        begin
          FDefaultUnitWidth.value /= FDefaultDpi^;
          FDefaultUnitWidth.CSSUnit := cuInch;
        end;
      FDefaultUnitHeight.value := FProportionalViewSize.height.value/FViewBox.size.y;
      FDefaultUnitHeight.CSSUnit := FProportionalViewSize.height.CSSUnit;
      if FDefaultUnitHeight.CSSUnit = cuCustom then
        begin
          FDefaultUnitHeight.value /= FDefaultDpi^;
          FDefaultUnitHeight.CSSUnit := cuInch;
        end;
      FUseDefaultDPI := false;
      FDpiScaleX := CustomDpiX/DpiX;
      FDpiScaleY := CustomDpiY/DpiY;
    end;

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

procedure TSVGUnits.SetCustomDpi(ADpi: TPointF);
var vb: TSVGViewBox;
  vs: TSVGSize;
begin
  vb := ViewBox;
  vs := FProportionalViewSize;
  if (vs.width.value > 0) and (vs.height.value > 0) then
    begin
      vb.size.x := ConvertWidth(vs.width,cuInch).value*ADpi.X;
      vb.size.y := ConvertHeight(vs.height,cuInch).value*ADpi.Y;
    end
  else
    raise exception.Create('The size of the view port is not properly defined. Use Width and Height properties of TBGRASVG object.');
  viewBox := vb;
end;

function TSVGUnits.GetDpiScaleX: single;
begin
  Result:=FDpiScaleX;
end;

function TSVGUnits.GetDpiScaleY: single;
begin
  Result:=FDpiScaleY;
end;

function TSVGUnits.GetDPIScaled: boolean;
begin
  Result:= not FUseDefaultDPI;
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

function TSVGUnits.GetDefaultUnitHeight: TFloatWithCSSUnit;
begin
  result := FDefaultUnitHeight;
end;

function TSVGUnits.GetDefaultUnitWidth: TFloatWithCSSUnit;
begin
  result := FDefaultUnitWidth;
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

function TSVGUnits.GetStretchRectF(AViewSize: TRectF; par: TSVGPreserveAspectRatio): TRectF;
var w0,h0,w,h: single;
begin
  result := AViewSize;
  w0 := AViewSize.Right-AViewSize.Left;
  h0 := AViewSize.Bottom-AViewSize.Top;
  w := w0;
  h := h0;

  if par.Preserve and
     (FViewBox.size.x > 0) and (FViewBox.size.y > 0) and
     (w > 0) and (h > 0) then
  begin
    //viewBox wider than viewSize
    if (FViewBox.size.x/FViewBox.size.y > w/h) xor par.Slice then
      h := w * FViewBox.size.y / FViewBox.size.x
    else
      w := h * FViewBox.size.x / FViewBox.size.y;
    case par.HorizAlign of
      taCenter: result.Left += (w0-w)/2;
      taRightJustify: result.Left += w0-w;
    end;
    case par.VertAlign of
      tlCenter: result.Top += (h0-h)/2;
      tlBottom: result.Top += h0-h;
    end;
  end;
  result.Right := result.Left+w;
  result.Bottom := result.Top+h;
end;

{ TBGRASVG }

function TBGRASVG.GetAttribute(AName: string): string;
begin
  result := Trim(FRoot.GetAttribute(AName));
end;

function TBGRASVG.GetAttribute(AName: string; ADefault: string): string;
begin
  result := GetAttribute(AName);
  if result = '' then result := ADefault;
end;

function TBGRASVG.GetCustomDpi: TPointF;
begin
  result := FUnits.CustomDpi;
end;

function TBGRASVG.GetHeight: TFloatWithCSSUnit;
begin
  result := TCSSUnitConverter.parseValue(Attribute['height'],FloatWithCSSUnit(ViewBox.size.y,cuCustom));
end;

function TBGRASVG.GetHeightAsCm: single;
begin
  result := FUnits.ConvertHeight(Height,cuCentimeter).value;
end;

function TBGRASVG.GetHeightAsInch: single;
begin
  result := FUnits.ConvertHeight(Height,cuInch).value;
end;

function TBGRASVG.GetPreserveAspectRatio: TSVGPreserveAspectRatio;
begin
  result := TSVGPreserveAspectRatio.Parse(Attribute['preserveAspectRatio','xMidYMid']);
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
  result := FUnits.ViewBox;
end;

function TBGRASVG.GetViewBox(AUnit: TCSSUnit): TSVGViewBox;
begin
  GetViewBoxIndirect(AUnit,result);
end;

procedure TBGRASVG.GetViewBoxIndirect(AUnit: TCSSUnit; out AViewBox: TSVGViewBox);
begin
  with FUnits.ViewBox do
  begin
    AViewBox.min := FUnits.ConvertCoord(min,cuCustom,AUnit);
    AViewBox.size := FUnits.ConvertCoord(size,cuCustom,AUnit);
  end;
end;

function TBGRASVG.GetWidth: TFloatWithCSSUnit;
begin
  result := TCSSUnitConverter.parseValue(Attribute['width'],FloatWithCSSUnit(ViewBox.size.x,cuCustom));
end;

function TBGRASVG.GetWidthAsCm: single;
begin
  result := FUnits.ConvertWidth(Width,cuCentimeter).value;
end;

function TBGRASVG.GetWidthAsInch: single;
begin
  result := FUnits.ConvertWidth(Width,cuInch).value;
end;

function TBGRASVG.GetZoomable: boolean;
begin
  result := AttributeDef['zoomAndPan','magnify']<>'disable';
end;

procedure TBGRASVG.SetAttribute(AName: string; AValue: string);
begin
  AName := trim(AName);
  if compareText(AName,'viewBox')= 0 then AName := 'viewBox' else
  if compareText(AName,'width')=0 then AName := 'width' else
  if compareText(AName,'height')=0 then AName := 'height';
  FRoot.SetAttribute(AName,AValue);
  if (AName = 'viewBox') or (AName = 'width') or (AName = 'height') then
    FUnits.Recompute;
end;

procedure TBGRASVG.SetCustomDpi(AValue: TPointF);
begin
  FUnits.CustomDpi := AValue;
  if AValue.x <> AValue.y then
    preserveAspectRatio := TSVGPreserveAspectRatio.Parse('none');
end;

procedure TBGRASVG.SetDefaultDpi(AValue: single);
begin
  if FDefaultDpi=AValue then Exit;
  FDefaultDpi:=AValue;
  Units.Recompute;
end;

procedure TBGRASVG.SetHeight(AValue: TFloatWithCSSUnit);
begin
  Attribute['height'] := TCSSUnitConverter.formatValue(AValue);
end;

procedure TBGRASVG.SetHeightAsCm(AValue: single);
begin
  Height := FloatWithCSSUnit(AValue,cuCentimeter);
end;

procedure TBGRASVG.SetHeightAsInch(AValue: single);
begin
  Height := FloatWithCSSUnit(AValue,cuInch);
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
  FUnits.ViewBox := AValue;
end;
{$POP}

procedure TBGRASVG.SetWidth(AValue: TFloatWithCSSUnit);
begin
  Attribute['width'] := TCSSUnitConverter.formatValue(AValue);
end;

procedure TBGRASVG.SetWidthAsCm(AValue: single);
begin
  Width := FloatWithCSSUnit(AValue,cuCentimeter);
end;

procedure TBGRASVG.SetWidthAsInch(AValue: single);
begin
  Width := FloatWithCSSUnit(AValue,cuInch);
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
    FRoot := FXml.CreateElement('svg');
    FUnits := TSVGUnits.Create(FRoot,@FDefaultDpi);
    FUnits.OnRecompute:= @UnitsRecompute;
    FDataLink := TSVGDataLink.Create;
    FContent := TSVGContent.Create(FXml,FRoot,FUnits,FDataLink,nil);
    FXml.AppendChild(FRoot);
  end;
end;

function TBGRASVG.GetViewBoxAlignment(AHorizAlign: TAlignment;
  AVertAlign: TTextLayout): TPointF;
var vb: TSVGViewBox;
begin
  GetViewBoxIndirect(cuPixel, vb);
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
  if AUnit in[cuInch,cuPoint,cuPica] then
    CustomDpi := PointF(288,288)
  else if AUnit in[cuCentimeter,cuMillimeter] then
    CustomDpi := PointF(254,254);
end;

constructor TBGRASVG.Create(AWidth,AHeight: single; AUnit: TCSSUnit; ACustomDPI: single);
begin
  Init(True);
  Width := FloatWithCSSUnit(AWidth,AUnit);
  Height := FloatWithCSSUnit(AHeight,AUnit);
  CustomDpi := PointF(ACustomDPI,ACustomDPI);
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
  FreeAndNil(FDataLink);
  FreeAndNil(FContent);
  FreeAndNil(FUnits);
  FRoot:= nil;
  FreeAndNil(FXml);
  inherited Destroy;
end;

procedure TBGRASVG.LoadFromFile(AFilenameUTF8: string);
var stream: TStream;
begin
  stream := TFileStreamUTF8.Create(AFilenameUTF8,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TBGRASVG.LoadFromStream(AStream: TStream);
var xml: TXMLDocument;
  root: TDOMNode;
  byteOrderMark: packed array[1..3] of byte;
  startPos: int64;
begin
  //skip utf8 byte order mark
  startPos:= AStream.Position;
  if AStream.Read({%H-}byteOrderMark,sizeof(byteOrderMark)) = 3 then
  begin
    if (byteOrderMark[1] = $ef) and (byteOrderMark[2] = $bb) and (byteOrderMark[3] = $bf) then
      startPos += 3;
  end;
  AStream.Position:= startPos;
  ReadXMLFile(xml,AStream);
  root := xml.FirstChild;
  while (root <> nil) and not (root is TDOMElement) do root := root.NextSibling;
  if root = nil then
  begin
    xml.Free;
    raise exception.Create('Root node not found');
  end;
  FreeAndNil(FDataLink);
  FreeAndNil(FContent);
  FreeAndNil(FUnits);
  FreeAndNil(FXml);
  FXml := xml;
  FRoot := root as TDOMElement;
  FUnits := TSVGUnits.Create(FRoot,@FDefaultDpi);
  FUnits.OnRecompute:= @UnitsRecompute;
  FDataLink := TSVGDataLink.Create;
  FContent := TSVGContent.Create(FXml,FRoot,FUnits,FDataLink,nil);
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
  WriteXMLFile(FXml, AStream);
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; x, y: single; AUnit: TCSSUnit);
var prevMatrix: TAffineMatrix;
begin
  prevMatrix := ACanvas2d.matrix;
  ACanvas2d.translate(x,y);
  with GetViewBoxAlignment(AHorizAlign,AVertAlign) do ACanvas2d.translate(x,y);
  Draw(ACanvas2d, 0,0, AUnit);
  ACanvas2d.matrix := prevMatrix;
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; x, y: single; destDpi: single);
begin
  Draw(ACanvas2d, AHorizAlign,AVertAlign, x,y, PointF(destDpi,destDpi));
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; x, y: single; destDpi: TPointF);
begin
  ACanvas2d.save;
  ACanvas2d.translate(x,y);
  ACanvas2d.scale(destDpi.x/Units.DpiX,destDpi.y/Units.DpiY);
  with GetViewBoxAlignment(AHorizAlign,AVertAlign) do ACanvas2d.translate(x,y);
  Draw(ACanvas2d, 0,0, cuPixel);
  ACanvas2d.restore;
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; x, y: single; AUnit: TCSSUnit);
var prevLinearBlend: boolean;
begin
  prevLinearBlend:= ACanvas2d.linearBlend;
  acanvas2d.linearBlend := true;
  ACanvas2d.save;
  ACanvas2d.translate(x,y);
  ACanvas2d.strokeMatrix := ACanvas2d.matrix;
  Content.Draw(ACanvas2d,AUnit);
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
      StretchDraw(ACanvas2d, HorizAlign, VertAlign, x,y,w,h);
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
  Draw(ACanvas2d, 0,0);
  ACanvas2d.restore;
end;

procedure TBGRASVG.StretchDraw(ACanvas2d: TBGRACanvas2D; r: TRectF; useSvgAspectRatio: boolean);
begin
  StretchDraw(ACanvas2d, r.Left,r.Top,r.Right-r.Left,r.Bottom-r.Top, useSvgAspectRatio);
end;

procedure TBGRASVG.StretchDraw(ACanvas2d: TBGRACanvas2D;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; x, y, w, h: single);
var r: TRectF;
begin
  r := GetStretchRectF(AHorizAlign,AVertAlign, x, y, w, h);
  StretchDraw(ACanvas2d, r.Left,r.Top,r.Right-r.Left,r.Bottom-r.Top);
end;

function TBGRASVG.GetStretchRectF(AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; x, y, w, h: single): TRectF;
var ratio,stretchRatio,zoom: single;
  sx,sy,sw,sh: single;
  size: TSVGSize;
begin
  //determine global ratio according to viewSize
  size := Units.OriginalViewSize;
  size.width := Units.ConvertWidth(size.Width,cuPixel);
  size.height := Units.ConvertHeight(size.height,cuPixel);
  if (h = 0) or (w = 0) or (size.width.value = 0) or (size.height.value = 0) then
  begin
    result := RectF(x,y,w,h);
    exit;
  end;
  ratio := size.width.value/size.height.value;
  stretchRatio := w/h;
  if ratio > stretchRatio then
    zoom := w / size.width.value
  else
    zoom := h / size.height.value;

  sx := x;
  sy := y;
  sw := size.width.value*zoom;
  sh := size.height.value*zoom;

  case AHorizAlign of
    taCenter: sx += (w - sw)/2;
    taRightJustify: sx += w - sw;
  end;
  case AVertAlign of
    tlCenter: sy += (h - sh)/2;
    tlBottom: sy += h - sh;
  end;

  result := Units.GetStretchRectF(RectF(sx,sy,sx+sw,sy+sh), preserveAspectRatio);
end;

end.

