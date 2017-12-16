unit BGRASVGType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRATransform, BGRABitmapTypes, BGRAUnits,
  laz2_DOM, BGRACanvas2D, fgl, BGRAGraphics;

type
  ArrayOfFloat = array of single;
  
  TSVGElement = class;
  TSVGElementList = specialize TFPGList<TSVGElement>;
  TSVGFactory = class of TSVGElement;
  
  TSVGFillMode = (
     sfmNonZero = Ord(fmWinding),
     sfmEvenOdd = Ord(fmAlternate)
   );
  
  { TSVGDataLink }

  TSVGDataLink = class
     Elements: TSVGElementList;
     Gradients: TSVGElementList;

     constructor Create;
     destructor Destroy; override;

     function Linking(el: TSVGElement): Integer;
  end;    

  { TSVGElement }

  TSVGElement = class
    private
      FDataParent: TSVGElement;
      FDataChildList: TSVGElementList;
      FGroupList: TSVGElementList;
      function GetAttributeOrStyle(AName,ADefault: string): string; overload;
      function GetAttributeOrStyle(AName: string): string; overload;
      function GetFill: string;
      function GetFillColor: TBGRAPixel;
      function GetFillOpacity: single;
      function GetFillRule: string;
      function GetHorizAttributeOrStyleWithUnit(AName: string;
        ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
      function GetIsFillNone: boolean;
      function GetIsStrokeNone: boolean;
      function GetMatrix(AUnit: TCSSUnit): TAffineMatrix;
      function GetOpacity: single;
      function GetOrthoAttributeOrStyleWithUnit(AName: string;
        ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
      function GetStroke: string;
      function GetStrokeColor: TBGRAPixel;
      function GetStrokeLineCap: string;
      function GetStrokeLineJoin: string;
      function GetStrokeMiterLimit: single;
      function GetStrokeOpacity: single;
      function GetStrokeWidth: TFloatWithCSSUnit;
      function GetStrokeDashArray: string;
      function GetStrokeDashArrayF: ArrayOfFloat;
      function GetStrokeDashOffset: TFloatWithCSSUnit;
      function GetStyle(const AName,ADefault: string): string; overload;
      function GetStyle(const AName: string): string; overload;
      function GetTransform: string;
      function GetUnits: TCSSUnitConverter;
      function GetAttribute(AName,ADefault: string): string; overload;
      function GetAttribute(AName: string): string; overload;
      function GetVerticalAttributeOrStyleWithUnit(AName: string;
        ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
      procedure SetAttribute(AName: string; AValue: string);
      function GetAttributeWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit; overload;
      function GetAttributeWithUnit(AName: string): TFloatWithCSSUnit; overload;
      function GetAttributeOrStyleWithUnit(AName: string;
        ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit; overload;
      function GetAttributeOrStyleWithUnit(AName: string): TFloatWithCSSUnit; overload;
      function GetOrthoAttributeWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit; overload;
      function GetOrthoAttributeWithUnit(AName: string): TFloatWithCSSUnit; overload;
      function GetHorizAttributeWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit; overload;
      function GetHorizAttributeWithUnit(AName: string): TFloatWithCSSUnit; overload;
      function GetVerticalAttributeWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit; overload;
      function GetVerticalAttributeWithUnit(AName: string): TFloatWithCSSUnit; overload;
      function GetID: string;
      procedure SetAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
      procedure SetFill(AValue: string);
      procedure SetFillColor(AValue: TBGRAPixel);
      procedure SetFillOpacity(AValue: single);
      procedure SetFillRule(AValue: string);
      procedure SetHorizAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
      procedure SetMatrix(AUnit: TCSSUnit; const AValue: TAffineMatrix);
      procedure SetOpacity(AValue: single);
      procedure SetStroke(AValue: string);
      procedure SetStrokeColor(AValue: TBGRAPixel);
      procedure SetStrokeLineCap(AValue: string);
      procedure SetStrokeLineJoin(AValue: string);
      procedure SetStrokeMiterLimit(AValue: single);
      procedure SetStrokeOpacity(AValue: single);
      procedure SetStrokeWidth(AValue: TFloatWithCSSUnit);
      procedure SetStrokeDashArray(AValue: string);
      procedure SetStrokeDashArrayF(AValue: ArrayOfFloat);
      procedure SetStrokeDashOffset(AValue: TFloatWithCSSUnit);
      procedure SetStyle(AName: string; AValue: string);
      procedure SetTransform(AValue: string);
      procedure SetVerticalAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
      procedure SetOrthoAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
      procedure SetID(AValue: string);
    protected
      FDataLink: TSVGDataLink;
      FDomElem: TDOMElement;
      FUnits: TCSSUnitConverter;
      function GetDOMElement: TDOMElement; virtual;
      procedure Init(ADocument: TXMLDocument; ATag: string; AUnits: TCSSUnitConverter); overload;
      procedure Init({%H-}ADocument: TXMLDocument; AElement: TDOMElement; AUnits: TCSSUnitConverter); overload;
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); virtual;
      procedure LocateStyleDeclaration(AText: string; AProperty: string; out AStartPos,AColonPos,AValueLength: integer);
      procedure ApplyStrokeStyle(ACanvas2D: TBGRACanvas2D; AUnit: TCSSUnit);
      procedure Initialize; virtual;
    public
      constructor Create({%H-}ADocument: TXMLDocument; AElement: TDOMElement; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); virtual;
      constructor Create({%H-}ADocument: TXMLDocument; {%H-}AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); virtual;
      destructor Destroy; override;
      procedure Draw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit);
      procedure fillNone;
      procedure strokeNone;
      procedure transformNone;
      procedure RemoveStyle(const AName: string);
      function HasAttribute(AName: string): boolean;
      function fillMode: TSVGFillMode;
      function DataChildList: TSVGElementList;
      function GroupList: TSVGElementList;
      property DataLink: TSVGDataLink read FDataLink write FDataLink;
      property AttributeDef[AName,ADefault: string]: string read GetAttribute;
      property Attribute[AName: string]: string read GetAttribute write SetAttribute;
      property AttributeOrStyleDef[AName,ADefault: string]: string read GetAttributeOrStyle;
      property AttributeOrStyle[AName: string]: string read GetAttributeOrStyle;
      property StyleDef[AName,ADefault: string]: string read GetStyle;
      property Style[AName: string]: string read GetStyle write SetStyle;
      property AttributeWithUnitDef[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetAttributeWithUnit;
      property AttributeWithUnit[AName: string]: TFloatWithCSSUnit read GetAttributeWithUnit write SetAttributeWithUnit;
      property OrthoAttributeWithUnitDef[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetOrthoAttributeWithUnit;
      property OrthoAttributeWithUnit[AName: string]: TFloatWithCSSUnit read GetOrthoAttributeWithUnit write SetOrthoAttributeWithUnit;
      property HorizAttributeWithUnitDef[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetHorizAttributeWithUnit;
      property HorizAttributeWithUnit[AName: string]: TFloatWithCSSUnit read GetHorizAttributeWithUnit write SetHorizAttributeWithUnit;
      property VerticalAttributeWithUnitDef[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetVerticalAttributeWithUnit;
      property VerticalAttributeWithUnit[AName: string]: TFloatWithCSSUnit read GetVerticalAttributeWithUnit write SetVerticalAttributeWithUnit;
      property OrthoAttributeOrStyleWithUnit[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetOrthoAttributeOrStyleWithUnit;
      property HorizAttributeOrStyleWithUnit[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetHorizAttributeOrStyleWithUnit;
      property VerticalAttributeOrStyleWithUnit[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetVerticalAttributeOrStyleWithUnit;
      property DOMElement: TDOMElement read GetDOMElement;
      property Units: TCSSUnitConverter read GetUnits;
      property transform: string read GetTransform write SetTransform;
      property matrix[AUnit: TCSSUnit]: TAffineMatrix read GetMatrix write SetMatrix;
      property isFillNone: boolean read GetIsFillNone;
      property isStrokeNone: boolean read GetIsStrokeNone;
      property stroke: string read GetStroke write SetStroke;
      property strokeWidth: TFloatWithCSSUnit read GetStrokeWidth write SetStrokeWidth;
      property strokeColor: TBGRAPixel read GetStrokeColor write SetStrokeColor;
      property strokeOpacity: single read GetStrokeOpacity write SetStrokeOpacity;
      property strokeMiterLimit: single read GetStrokeMiterLimit write SetStrokeMiterLimit;
      property strokeLineJoin: string read GetStrokeLineJoin write SetStrokeLineJoin;
      property strokeLineCap: string read GetStrokeLineCap write SetStrokeLineCap;
      property strokeDashArray: string read GetStrokeDashArray write SetStrokeDashArray;
      property strokeDashArrayF: ArrayOfFloat read GetStrokeDashArrayF write SetStrokeDashArrayF;
      property strokeDashOffset: TFloatWithCSSUnit read GetStrokeDashOffset write SetStrokeDashOffset;
      property fill: string read GetFill write SetFill;
      property fillColor: TBGRAPixel read GetFillColor write SetFillColor;
      property fillOpacity: single read GetFillOpacity write SetFillOpacity;
      property fillRule: string read GetFillRule write SetFillRule;
      property opacity: single read GetOpacity write SetOpacity;
      property ID: string read GetID write SetID;
      property DataParent: TSVGElement read FDataParent write FDataParent;
  end;

  { TSVGParser }

  TSVGParser = class
  private
    function GetDone: boolean;
  protected
    FPos: integer;
    FNumberError: boolean;
    FText: string;
  public
    constructor Create(AText: string);
    function ParseFloat: single;
    function ParseId: string;
    function ParseSymbol: char;
    procedure SkipSymbol(ASymbol: char);
    procedure SkipUpToSymbol(ASymbol:char);
    procedure ClearError;
    property Position: integer read FPos write FPos;
    property NumberError: boolean read FNumberError;
    property Text: string read FText;
    property Done: boolean read GetDone;
  end;

implementation

uses BGRASVGShapes;

{ TSVGParser }

function TSVGParser.GetDone: boolean;
begin
  result := FPos>length(FText)
end;

constructor TSVGParser.Create(AText: string);
begin
  FNumberError:= false;
  FPos := 1;
  FText := AText;
end;

function TSVGParser.ParseFloat: single;
var numberStart: integer;
    errPos: integer;
begin
  while (FPos <= length(FText)) and (FText[FPos] in[#0..#32,',']) do inc(FPos);
  numberStart:= FPos;
  if (FPos <= length(FText)) and (FText[FPos] in['+','-']) then inc(FPos);
  while (FPos <= length(FText)) and (FText[FPos] in['0'..'9','.']) do inc(FPos);
  if (FPos <= length(FText)) and (FText[FPos] in['e','E']) then inc(FPos);
  if (FPos <= length(FText)) and (FText[FPos] in['+','-']) then inc(FPos);
  while (FPos <= length(FText)) and (FText[FPos] in['0'..'9','.']) do inc(FPos);
  if FPos = numberStart then
  begin
    FNumberError := true;
    result := 0;
  end
  else
  begin
    val(copy(FText,numberStart,FPos-numberStart),result,errPos);
    if errPos <> 0 then FNumberError := true;
  end;
end;

function TSVGParser.ParseId: string;
var idStart: integer;
begin
  while (FPos <= length(FText)) and (FText[FPos] in[#0..#32,',']) do inc(FPos);
  idStart:= FPos;
  if (FPos <= length(FText)) and (FText[FPos] in['A'..'Z','a'..'z']) then inc(FPos);
  while (FPos <= length(FText)) and (FText[FPos] in['0'..'9','A'..'Z','a'..'z','_']) do inc(FPos);
  result := copy(FText,idStart,FPos-idStart);
end;

function TSVGParser.ParseSymbol: char;
begin
  while (FPos <= length(FText)) and (FText[FPos] in[#0..#32,',']) do inc(FPos);
  if (FPos <= length(FText)) and not (FText[FPos] in['A'..'Z','a'..'z','0'..'9']) then
  begin
    result := FText[FPos];
    inc(FPos);
  end else
    result := #0;
end;

procedure TSVGParser.SkipSymbol(ASymbol: char);
begin
  while (FPos <= length(FText)) and (FText[FPos] in[#0..#32,',']) do inc(FPos);
  if (FPos <= length(FText)) and (FText[FPos] = ASymbol) then inc(FPos);
end;

procedure TSVGParser.SkipUpToSymbol(ASymbol: char);
begin
  while (FPos <= length(FText)) and (FText[FPos]<>ASymbol) do inc(FPos);
  if (FPos <= length(FText)) and (FText[FPos]=ASymbol) then inc(FPos);
end;

procedure TSVGParser.ClearError;
begin
  FNumberError:= false;
end;

{ TSVGDataLink }

constructor TSVGDataLink.Create;
begin
   Elements:= TSVGElementList.Create;
   Gradients:= TSVGElementList.Create;
end;

destructor TSVGDataLink.Destroy;
begin
   FreeAndNil(Gradients);
   FreeAndNil(Elements);
   inherited Destroy;
end;

function TSVGDataLink.Linking(el: TSVGElement): Integer;
begin
  Elements.Add(el);
  Result:= Elements.Count;
  if (el is TSVGGradient) or
     (el is TSVGStopGradient) then
    Gradients.Add(el);
end;        

{ TSVGElement }

function TSVGElement.GetAttribute(AName,ADefault: string): string;
var
  i: integer;
begin
  result := FDomElem.GetAttribute(AName);
  
  //Find on <g> block
  if (result = '') and (not (Self is TSVGGroup)) then
    for i:= FGroupList.Count-1 downto 0 do
    begin
      result:= FGroupList[i].GetAttribute(AName,ADefault);
      if result <> '' then
        Break;
    end;  

  if result = '' then
    result:= ADefault;
end;

function TSVGElement.GetAttribute(AName: string): string;
begin
  result:= GetAttribute(AName,'');
end;  

function TSVGElement.GetVerticalAttributeOrStyleWithUnit(AName: string;
  ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := GetAttributeOrStyleWithUnit(AName,ADefault);
  if result.CSSUnit <> cuCustom then
    if units.DpiScaleY = 0 then
      result.value := 0
    else
      result.value /= Units.DpiScaleY;
end;

function TSVGElement.GetAttributeWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := TCSSUnitConverter.parseValue(Attribute[AName],ADefault);
end;

function TSVGElement.GetAttributeWithUnit(AName: string): TFloatWithCSSUnit;
begin
  result := GetAttributeWithUnit(AName,FloatWithCSSUnit(0,cuCustom));
end; 

function TSVGElement.GetAttributeOrStyleWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
var
  valueText: string;
begin
  valueText := Style[AName];
  if valueText = '' then
    valueText := Attribute[AName];
  result := TCSSUnitConverter.parseValue(valueText,ADefault);
end;

function TSVGElement.GetAttributeOrStyleWithUnit(AName: string): TFloatWithCSSUnit;
begin
  result := GetAttributeOrStyleWithUnit(AName,FloatWithCSSUnit(0,cuCustom));
end; 

function TSVGElement.GetOrthoAttributeWithUnit(AName: string;
  ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := GetHorizAttributeWithUnit(AName,ADefault);
  //value will be inconsistent if scaling is inconsistent
end;

function TSVGElement.GetOrthoAttributeWithUnit(AName: string): TFloatWithCSSUnit;
begin
  result := GetOrthoAttributeWithUnit(AName,FloatWithCSSUnit(0,cuCustom));
end;

function TSVGElement.GetHorizAttributeWithUnit(AName: string;
  ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := GetAttributeWithUnit(AName,ADefault);
  if result.CSSUnit <> cuCustom then
    if units.DpiScaleX = 0 then
      result.value := 0
    else
      result.value /= Units.DpiScaleX;
end;

function TSVGElement.GetHorizAttributeWithUnit(AName: string): TFloatWithCSSUnit;
begin
  result := GetHorizAttributeWithUnit(AName,FloatWithCSSUnit(0,cuCustom));
end; 

function TSVGElement.GetAttributeOrStyle(AName,ADefault: string): string;
begin
  result := GetStyle(AName,ADefault);
  if result = '' then
    result := GetAttribute(AName,ADefault);
end;

function TSVGElement.GetAttributeOrStyle(AName: string): string;
begin
  result:= GetAttributeOrStyle(AName,'');
end;   

function TSVGElement.GetFill: string;
begin
  result := AttributeOrStyleDef['fill','black'];
end;

function TSVGElement.GetFillColor: TBGRAPixel;
begin
  result := StrToBGRA(fill,BGRABlack);
  result.alpha := round(result.alpha*fillOpacity*opacity);
  if result.alpha = 0 then result := BGRAPixelTransparent;
end;

function TSVGElement.GetFillOpacity: single;
var errPos: integer;
begin
  val(AttributeOrStyleDef['fill-opacity','1'], result, errPos);
  if errPos <> 0 then result := 1 else
    if result < 0 then result := 0 else
      if result > 1 then result := 1;
end;

function TSVGElement.GetFillRule: string;
begin
  result := AttributeOrStyleDef['fill-rule','nonzero'];
end; 

function TSVGElement.GetHorizAttributeOrStyleWithUnit(AName: string;
  ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := GetAttributeOrStyleWithUnit(AName,ADefault);
  if result.CSSUnit <> cuCustom then
    if units.DpiScaleX = 0 then
      result.value := 0
    else
      result.value /= Units.DpiScaleX;
end;

function TSVGElement.GetIsFillNone: boolean;
begin
  result := compareText(trim(fill),'none')=0;
end;

function TSVGElement.GetIsStrokeNone: boolean;
var strokeStr: string;
begin
  strokeStr := stroke;
  result := (trim(strokeStr)='') or (compareText(trim(strokeStr),'none')=0);
end;

function TSVGElement.GetMatrix(AUnit: TCSSUnit): TAffineMatrix;
var parser: TSVGParser;
    s,kind: string;
    m : TAffineMatrix;
    angle,tx,ty: single;
begin
  result := AffineMatrixIdentity;
  s := transform;
  if s='' then exit;
  parser := TSVGParser.Create(s);
  while not parser.Done do
  begin
    kind := parser.ParseId;
    if kind = '' then break;
    if parser.ParseSymbol <> '(' then break;
    if compareText(kind,'matrix')=0 then
    begin
      m[1,1] := parser.ParseFloat;
      parser.SkipSymbol(',');
      m[2,1] := parser.ParseFloat;
      parser.SkipSymbol(',');
      m[1,2] := parser.ParseFloat;
      parser.SkipSymbol(',');
      m[2,2] := parser.ParseFloat;
      parser.SkipSymbol(',');
      m[1,3] := parser.ParseFloat;
      parser.SkipSymbol(',');
      m[2,3] := parser.ParseFloat;
      result *= m;
    end else
    if compareText(kind,'translate')=0 then
    begin
      tx := parser.ParseFloat;
      parser.SkipSymbol(',');
      ty := parser.ParseFloat;
      result *= AffineMatrixTranslation(tx,ty);
    end else
    if compareText(kind,'scale')=0 then
    begin
      tx := parser.ParseFloat;
      parser.SkipSymbol(',');
      parser.ClearError;
      ty := parser.ParseFloat;
      if parser.NumberError then ty := tx;
      result *= AffineMatrixScale(tx,ty);
    end else
    if compareText(kind,'rotate')=0 then
    begin
      angle := parser.ParseFloat;
      parser.SkipSymbol(',');
      tx := parser.ParseFloat;
      parser.SkipSymbol(',');
      ty := parser.ParseFloat;
      result *= AffineMatrixTranslation(tx,ty)*AffineMatrixRotationDeg(angle)*
                AffineMatrixTranslation(-tx,-ty);
    end else
    if compareText(kind,'skewx')=0 then
    begin
      angle := parser.ParseFloat;
      result *= AffineMatrixSkewXDeg(angle);
    end else
    if compareText(kind,'skewy')=0 then
    begin
      angle := parser.ParseFloat;
      result *= AffineMatrixSkewYDeg(angle);
    end;
    parser.SkipUpToSymbol(')');
  end;
  parser.free;
  result[1,3] := Units.ConvertWidth(result[1,3],cuCustom,AUnit);
  result[2,3] := Units.ConvertHeight(result[2,3],cuCustom,AUnit);
end;

function TSVGElement.GetOpacity: single;
var errPos: integer;
begin
  val(AttributeOrStyleDef['opacity','1'], result, errPos);
  if errPos <> 0 then result := 1 else
    if result < 0 then result := 0 else
      if result > 1 then result := 1;
end;

function TSVGElement.GetOrthoAttributeOrStyleWithUnit(AName: string;
  ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := GetHorizAttributeOrStyleWithUnit(AName,ADefault);
  //value will be inconsistent if scaling is inconsistent
end;

function TSVGElement.GetStroke: string;
begin
  result := AttributeOrStyleDef['stroke','none'];
end;

function TSVGElement.GetStrokeColor: TBGRAPixel;
begin
  result := StrToBGRA(stroke);
  result.alpha := round(result.alpha*strokeOpacity*opacity);
  if result.alpha = 0 then result := BGRAPixelTransparent;
end;

function TSVGElement.GetStrokeLineCap: string;
begin
  result := AttributeOrStyleDef['stroke-linecap','butt'];
end;

function TSVGElement.GetStrokeLineJoin: string;
begin
  result := AttributeOrStyleDef['stroke-linejoin','miter'];
end;

function TSVGElement.GetStrokeMiterLimit: single;
var errPos: integer;
begin
  val(AttributeOrStyleDef['stroke-miterlimit','4'], result, errPos);
  if errPos <> 0 then result := 4 else
    if result < 1 then result := 1;
end;

function TSVGElement.GetStrokeOpacity: single;
var errPos: integer;
begin
  val(AttributeOrStyleDef['stroke-opacity','1'], result, errPos);
  if errPos <> 0 then result := 1 else
    if result < 0 then result := 0 else
      if result > 1 then result := 1;
end;

function TSVGElement.GetStrokeWidth: TFloatWithCSSUnit;
begin
  result := OrthoAttributeOrStyleWithUnit['stroke-width',FloatWithCSSUnit(1,cuCustom)];
end;

function TSVGElement.GetStrokeDashArray: string;
begin
  result := AttributeDef['stroke-dasharray','none']; 
end;

function TSVGElement.GetStrokeDashArrayF: ArrayOfFloat;
var 
  parser: TSVGParser;
  nvalue,i: integer;
  s_array: String;
begin
  s_array:= strokeDashArray;
  if s_array = 'none' then
  begin
    setlength(Result,0);
    exit;
  end;
  parser:=TSVGParser.Create(s_array);
  nvalue := 0;
  repeat
    parser.ParseFloat;
    if not parser.NumberError then
      inc(nvalue);
  until parser.NumberError or parser.Done;
  parser.ClearError;
  setlength(Result,nvalue);
  parser.Position := 1;
  for i := 0 to high(result) do
    result[i] := parser.ParseFloat;
  parser.Free;
end;

function TSVGElement.GetStrokeDashOffset: TFloatWithCSSUnit;
begin
  result := OrthoAttributeWithUnit['stroke-dashoffset'];
end;    

function TSVGElement.GetStyle(const AName,ADefault: string): string;
var
    startPos, colonPos, valueLength: integer;
    ruleset: string;
begin
  ruleset := Attribute['style',ADefault];
  LocateStyleDeclaration(ruleset, AName, startPos,colonPos, valueLength);
  if valueLength <> -1 then
  begin
    result := trim(copy(ruleset, colonPos+1, valueLength));
  end else
    result := '';
end;

function TSVGElement.GetStyle(const AName: string): string;
begin
  result:= GetStyle(AName,'');
end; 

function TSVGElement.GetTransform: string;
begin
  result := Attribute['transform'];
end;

function TSVGElement.GetUnits: TCSSUnitConverter;
begin
  result := FUnits;
end;

function TSVGElement.GetVerticalAttributeWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := GetAttributeWithUnit(AName,ADefault);
  if result.CSSUnit <> cuCustom then
    if units.DpiScaleY = 0 then
      result.value := 0
    else
      result.value /= Units.DpiScaleY;
end;

function TSVGElement.GetVerticalAttributeWithUnit(AName: string): TFloatWithCSSUnit;
begin
  result := GetVerticalAttributeWithUnit(AName,FloatWithCSSUnit(0,cuCustom));
end;  

function TSVGElement.GetDOMElement: TDOMElement;
begin
  result := FDomElem;
end;

function TSVGElement.GetID: string;
begin
  result := Attribute['id'];
end; 

procedure TSVGElement.SetAttribute(AName: string; AValue: string);
begin
  FDomElem.SetAttribute(AName,AValue);
end;

procedure TSVGElement.SetAttributeWithUnit(AName: string;
  AValue: TFloatWithCSSUnit);
begin
  Attribute[AName] := TCSSUnitConverter.formatValue(AValue);
end;

procedure TSVGElement.SetFill(AValue: string);
begin
  Attribute['fill'] := AValue;
  RemoveStyle('fill');
end;

procedure TSVGElement.SetFillColor(AValue: TBGRAPixel);
begin
  fillOpacity:= AValue.alpha/255;
  AValue.alpha:= 255;
  fill := BGRAToStr(AValue, CSSColors);
end;

procedure TSVGElement.SetFillOpacity(AValue: single);
begin
  Attribute['fill-opacity'] := Units.formatValue(AValue);
  RemoveStyle('fill-opacity');
end;

procedure TSVGElement.SetFillRule(AValue: string);
begin
  Attribute['fill-rule'] := AValue;
  RemoveStyle('fill-rule');
end;  

procedure TSVGElement.SetHorizAttributeWithUnit(AName: string;
  AValue: TFloatWithCSSUnit);
begin
  if Units.DpiScaled then
    SetAttribute(AName, TCSSUnitConverter.formatValue(Units.ConvertWidth(AValue,cuCustom)))
  else
  if AValue.CSSUnit <> cuCustom then
    SetAttributeWithUnit(AName, FloatWithCSSUnit(AValue.value*Units.DpiScaleX,AValue.CSSUnit))
  else
    SetAttributeWithUnit(AName, AValue);
end;

procedure TSVGElement.SetMatrix(AUnit: TCSSUnit; const AValue: TAffineMatrix);
var m: TAffineMatrix;
    s: string;
    translateStr: string;
begin
  translateStr := 'translate('+Units.formatValue(Units.ConvertWidth(AValue[1,3],AUnit,cuCustom))+' '+
      Units.formatValue(Units.ConvertHeight(AValue[2,3],AUnit,cuCustom))+')';
  if IsAffineMatrixTranslation(AValue) then
  begin
    if IsAffineMatrixIdentity(AValue) then
    begin
      transformNone;
      exit;
    end;
    transform := translateStr;
  end else
  begin
    m := AValue;
    if (m[1,3] <> 0) or (m[2,3] <> 0) then
    begin
      s := translateStr;
      m[1,3] := 0;
      m[2,3] := 0;
    end else
      s := '';
    if IsAffineMatrixScale(AValue) then
    begin
      transform := trim(s+' scale('+Units.formatValue(m[1,1])+' '+Units.formatValue(m[2,2])+')');
      exit;
    end;
    transform := trim(s+' matrix('+Units.formatValue(m[1,1])+' '+Units.formatValue(m[2,1])+' '+
                     Units.formatValue(m[1,2])+' '+Units.formatValue(m[2,2])+' ' +
                     Units.formatValue(m[1,3])+' '+Units.formatValue(m[2,3]));
  end;
end;

procedure TSVGElement.SetOpacity(AValue: single);
begin
  Attribute['opacity'] := Units.formatValue(AValue);
  RemoveStyle('opacity');
end;

procedure TSVGElement.SetStroke(AValue: string);
begin
  Attribute['stroke'] := AValue;
  RemoveStyle('stroke');
end;

procedure TSVGElement.SetStrokeColor(AValue: TBGRAPixel);
begin
  strokeOpacity:= AValue.alpha/255;
  AValue.alpha:= 255;
  stroke := BGRAToStr(AValue, CSSColors);
end;

procedure TSVGElement.SetStrokeLineCap(AValue: string);
begin
  Attribute['stroke-linecap'] := AValue;
  RemoveStyle('stroke-linecap');
end;

procedure TSVGElement.SetStrokeLineJoin(AValue: string);
begin
  Attribute['stroke-linejoin'] := AValue;
  RemoveStyle('stroke-linejoin');
end;

procedure TSVGElement.SetStrokeMiterLimit(AValue: single);
begin
  if AValue < 1 then AValue := 1;
  Attribute['stroke-miterlimit'] := Units.formatValue(AValue);
  RemoveStyle('stroke-miterlimit');
end;

procedure TSVGElement.SetStrokeOpacity(AValue: single);
begin
  Attribute['stroke-opacity'] := Units.formatValue(AValue);
  RemoveStyle('stroke-opacity');
end;

procedure TSVGElement.SetStrokeWidth(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['stroke-width'] := AValue;
  RemoveStyle('stroke-width');
end;

procedure TSVGElement.SetStrokeDashArray(AValue: string);
begin
  Attribute['stroke-dasharray'] := AValue;
end;

procedure TSVGElement.SetStrokeDashArrayF(AValue: ArrayOfFloat);
var 
  s: string;
  i: integer;
begin
  s:= '';
  for i := 0 to high(AValue) do
  begin
    if s <> '' then s += ' ';
    s += TCSSUnitConverter.formatValue(AValue[i])+' ';
  end;
  strokeDashArray := s;
end;

procedure TSVGElement.SetStrokeDashOffset(AValue: TFloatWithCSSUnit);
begin
  OrthoAttributeWithUnit['stroke-dashoffset'] := AValue;
end;      

procedure TSVGElement.SetStyle(AName: string; AValue: string);
var
    startPos, colonPos, valueLength: integer;
    ruleset: string;
begin
  if pos(';',AValue)<>0 then
    raise exception.Create('Invalid character in value');
  if pos(':',AName)<>0 then
    raise exception.Create('Invalid character in name');
  ruleset := Attribute['style'];
  LocateStyleDeclaration(ruleset, AName, startPos,colonPos, valueLength);
  if valueLength <> -1 then
  begin
    delete(ruleset, colonPos+1, valueLength);
    insert(' '+Trim(AValue), ruleset, colonPos+1);
  end else
  begin
    while (length(ruleset) > 0) and (ruleset[length(ruleset)] in[' ',#9,#10,#12,#13]) do
      delete(ruleset, length(ruleset), 1);
    if length(ruleset)>0 then
    begin
      if ruleset[length(ruleset)] <> ';' then ruleset += '; ';
    end;
    ruleset += AName+': '+AValue;
  end;
  Attribute['style'] := ruleset;
end;

procedure TSVGElement.SetTransform(AValue: string);
begin
  Attribute['transform'] := AValue;
end;

procedure TSVGElement.SetVerticalAttributeWithUnit(AName: string;
  AValue: TFloatWithCSSUnit);
begin
  if Units.DpiScaled then
    SetAttribute(AName, TCSSUnitConverter.formatValue(Units.ConvertHeight(AValue,cuCustom)))
  else
  if AValue.CSSUnit <> cuCustom then
    SetAttributeWithUnit(AName, FloatWithCSSUnit(AValue.value*Units.DpiScaleY,AValue.CSSUnit))
  else
    SetAttributeWithUnit(AName, AValue);
end;

procedure TSVGElement.SetOrthoAttributeWithUnit(AName: string;
  AValue: TFloatWithCSSUnit);
begin
  if (AValue.CSSUnit <> cuCustom) and (Units.DpiScaleX<>Units.DpiScaleY) then
    raise exception.Create('Impossible to set value with inconsistent scaling');
  if Units.DpiScaled then
    SetAttribute(AName, TCSSUnitConverter.formatValue(Units.ConvertWidth(AValue,cuCustom)))
  else
    SetHorizAttributeWithUnit(AName,AValue);
end;

procedure TSVGElement.SetID(AValue: string);
begin
  Attribute['id'] := AValue;
end; 

procedure TSVGElement.Init(ADocument: TXMLDocument; ATag: string;
  AUnits: TCSSUnitConverter);
begin
  FDomElem := ADocument.CreateElement(ATag);
  FUnits := AUnits;
end;

procedure TSVGElement.Init(ADocument: TXMLDocument; AElement: TDOMElement;
  AUnits: TCSSUnitConverter);
begin
  FDomElem := AElement;
  FUnits := AUnits;
end;

procedure TSVGElement.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  //nothing
end;

procedure TSVGElement.LocateStyleDeclaration(AText: string; AProperty: string; out AStartPos,
  AColonPos, AValueLength: integer);
var i: integer;
    curStart,curColon,curValueLength: integer;

    function CheckShouldReturnResult: boolean;
    begin
      if Trim(Copy(AText,curStart,curColon-curStart)) = AProperty then
      begin
        AStartPos:= curStart;
        AColonPos:= curColon;
        AValueLength:= curValueLength;
        result := true
      end
      else
        result := false
    end;

begin
  AProperty := Trim(AProperty);
  AStartPos := -1;
  AColonPos := -1;
  AValueLength:= -1;
  curStart := -1;
  curColon := -1;
  curValueLength := -1;
  for i := 1 to length(AText) do
  begin
    if curStart = -1 then
    begin
      if AText[i] in['-','_','a'..'z','A'..'Z','\'] then
      begin
        curStart := i;
        curColon := -1;
      end;
    end else
    if curColon = -1 then
    begin
      if AText[i] = ':' then
      begin
        curColon := i;
        curValueLength:= -1;
      end;
    end else
    if AText[i] = ';' then
    begin
      curValueLength := i-(curColon+1);
      if CheckShouldReturnResult then exit;
      curStart := -1;
      curColon := -1;
      curValueLength:= -1;
    end;
  end;
  if curColon <> -1 then
  begin
    curValueLength:= length(AText)-(curColon+1)+1;
    if CheckShouldReturnResult then exit;
  end;
end;

procedure TSVGElement.ApplyStrokeStyle(ACanvas2D: TBGRACanvas2D; AUnit: TCSSUnit);
var
  a: ArrayOfFloat;
begin
  ACanvas2d.strokeStyle(strokeColor);
  ACanvas2d.lineWidth := Units.ConvertWidth(strokeWidth,AUnit).value;
  ACanvas2d.lineCap := strokeLineCap;
  ACanvas2d.lineJoin := strokeLineJoin;
  ACanvas2d.miterLimit := strokeMiterLimit;
  
  a:= strokeDashArrayF;
  if Length(a) <> 0 then
  begin
    ACanvas2d.lineStyle(a);
    SetLength(a,0);
  end; 
end;

procedure TSVGElement.Initialize;
begin
  FDataParent:= nil;
  FDataChildList:= TSVGElementList.Create;
  FGroupList:= TSVGElementList.Create;
end; 

constructor TSVGElement.Create(ADocument: TXMLDocument; AElement: TDOMElement;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  FDataLink:= ADataLink;
  Initialize;
  Init(ADocument,AElement,AUnits);
end;

constructor TSVGElement.Create(ADocument: TXMLDocument;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  FDataLink:= ADataLink;
  Initialize;
  //raise exception.Create('Cannot create a generic element');
end;

destructor TSVGElement.Destroy;
begin
  FreeAndNil(FGroupList);
  FreeAndNil(FDataChildList);
  inherited Destroy;
end; 

procedure TSVGElement.Draw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var prevMatrix: TAffineMatrix;
begin
  prevMatrix := ACanvas2d.matrix;
  ACanvas2d.transform(matrix[AUnit]);
  InternalDraw(ACanvas2d,AUnit);
  ACanvas2d.matrix := prevMatrix;
end;

procedure TSVGElement.fillNone;
begin
  fill := 'none';
end;

procedure TSVGElement.strokeNone;
begin
  stroke := 'none';
end;

procedure TSVGElement.transformNone;
begin
  FDomElem.RemoveAttribute('transform');
end;

procedure TSVGElement.RemoveStyle(const AName: string);
var
    startPos, colonPos, valueLength: integer;
    ruleset: string;
begin
  ruleset := Attribute['style'];
  LocateStyleDeclaration(ruleset, AName, startPos,colonPos, valueLength);
  if valueLength <> -1 then
  begin
    delete(ruleset, startPos, colonPos+valueLength-startPos);
    while (length(ruleset)>=startPos) and (ruleset[startPos] in[' ',#9,#10,#12,#13]) do delete(ruleset,startPos,1);
    if (length(ruleset)>=startPos) and (ruleset[startPos] = ';') then delete(ruleset,startPos,1);
    Attribute['style'] := ruleset;
  end;
end;

function TSVGElement.HasAttribute(AName: string): boolean;
begin
  result := FDomElem.hasAttribute(AName);
end; 

function TSVGElement.fillMode: TSVGFillMode;
begin
  if fillRule = 'evenodd' then
    result := sfmEvenOdd
  else
    result := sfmNonZero;
end;

function TSVGElement.DataChildList: TSVGElementList;
begin
   result:= FDataChildList;
end;

function TSVGElement.GroupList: TSVGElementList;
begin
   result:= FGroupList;
end;  

end.

