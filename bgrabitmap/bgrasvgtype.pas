// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRASVGType;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  BGRAClasses, SysUtils, BGRATransform, BGRABitmapTypes, BGRAUnits,
  DOM, BGRACanvas2D, fgl, BGRAGraphics;

type
  ArrayOfFloat = array of single;
  ArrayOfString = array of string;
  
  TSVGElement = class;
  TSVGElementList = specialize TFPGList<TSVGElement>;
  TSVGElementDictionary = specialize TFPGMap<string,TSVGElement>;
  TSVGFactory = class of TSVGElement;
  TIterateElementCallback = procedure(AElement: TSVGElement; AData: pointer; var ARecursive: boolean) of object;
  
  TSVGFillMode = (
     sfmEvenOdd = Ord(fmAlternate),
     sfmNonZero = Ord(fmWinding)
   );

  TSVGPaintOrder = (
     spoFillStrokeMarkers,
     spoFillMarkersStroke,
     spoStrokeFillMarkers,
     spoStrokeMarkersFill,
     spoMarkersFillStroke,
     spoMarkersStrokeFill
  );

  TSVGLengthAdjust = (
     slaSpacing,
     slaSpacingAndGlyphs
   );

  TSVGTextPathMethod = (
     stpmAlign,
     stpmStretch
   );

  TSVGTextPathSpacing = (
     stpsAuto,
     stpsExact
   );

  TSVGTextAnchor = (
     staStart,
     staMiddle,
     staEnd
   );
   
  TSVGTextDirection = (  
     stdLtr,
     stdRtl
   );
   
  TSVGObjectUnits = (
     souUserSpaceOnUse,
     souObjectBoundingBox
   );

  TSVGSpreadMethod = (
     ssmPad,
     ssmReflect,
     ssmRepeat
  );

  TSVGColorInterpolation = (
     sciStdRGB,
     sciLinearRGB
  );

  TSVGImageRendering = (
    sirAuto,
    sirSmooth,
    sirHighQuality,
    sirCrispEdges,
    sirPixelated
  );

  TSVGRenderingIntent = (
     sriAuto,
     sriPerceptual,
     sriRelativeColorimetric,
     sriSaturation,
     sriAbsoluteColorimetric
   );

  TSVGMarkerUnits = (
     smuStrokeWidth,
     smuUserSpaceOnUse
   );

  TSVGOrientAuto = (soaNone,soaAuto,soaAutoReverse);
  TSVGOrient = record
    auto: TSVGOrientAuto;
    angle: TSVGNumber;
  end;

  TFindStyleState = (fssNotSearched,
                     fssNotFound,
                     fssFound);
  TStyleAttribute = record
     attr  : string;
     pos   : integer;
  end;
  ArrayOfTStyleAttribute = array of TStyleAttribute;
  
  { TSVGViewBox }

  TSVGViewBox = record
    min, size: TPointF;
    function ToString: string;
    class function Parse(AValue: string): TSVGViewBox; static;
    class function DefaultValue: TSVGViewBox; static;
  end;
  TSVGSize = record
    width, height: TFloatWithCSSUnit;
  end;

  { TSVGPreserveAspectRatio }

  TSVGPreserveAspectRatio = record
     Preserve, Slice: boolean;
     HorizAlign: TAlignment;
     VertAlign: TTextLayout;
     function ToString: string;
     class function Parse(AValue: string): TSVGPreserveAspectRatio; static;
     class function DefaultValue: TSVGPreserveAspectRatio; static;
  end;

  TSVGRecomputeEvent = procedure(Sender: TObject) of object;
  TSVGLinkEvent = procedure(Sender: TObject; AElement: TSVGElement; ALink: boolean) of object;
  TSVGLinkListeners = specialize TFPGList<TSVGLinkEvent>;
  
  { TSVGDataLink }

  TSVGDataLink = class
   private
     FElements: TSVGElementDictionary;
     FStyles: TSVGElementList;
     FParent: TSVGDataLink;
     FChildren: TList;
     FLinkListeners: TSVGLinkListeners;
     function GetElement(AIndex: integer): TSVGElement;
     function GetStyle(AIndex: integer): TSVGElement;
     function IsValidIndex(const AIndex: integer; list: TSVGElementList): boolean;
     function FindTo(el: TSVGElement; list: TSVGElementList): integer;
     procedure NotifyLink(AElement: TSVGElement; ALink: boolean);
     procedure SetParent(AValue: TSVGDataLink);
   public
     constructor Create(AParent: TSVGDataLink);
     destructor Destroy; override;

     function ElementCount: integer;
     function StyleCount: integer;
     function FindElement(el: TSVGElement): integer;
     function FindElementById(AID: string; AClass: TSVGFactory): TSVGElement;
     function FindElementByRef(ARef: string; AClass: TSVGFactory): TSVGElement;
     function FindElementByRef(ARef: string; ANeedUrl: boolean; AClass: TSVGFactory; out ANotFound: boolean): TSVGElement;
     function FindStyle(el: TSVGElement): integer;
     function IsLinkElement(el: TSVGElement): boolean;
     function IsLinkStyle(el: TSVGElement): boolean;
     function IsLink(el: TSVGElement): boolean;
     function Link(el: TSVGElement): integer;
     procedure Unlink(el: TSVGElement);
     procedure UnlinkAll;
     procedure RegisterLinkListener(AHandler: TSVGLinkEvent; ARegister: boolean);

     property Styles[ID: integer]: TSVGElement read GetStyle;
     property Elements[AIndex: integer]: TSVGElement read GetElement;
     property Parent: TSVGDataLink read FParent write SetParent;
   end;

  { TSVGCustomElement }

  TSVGCustomElement = class
  protected
    FDomElem: TDOMElement;
    FUnits: TCSSUnitConverter;
    function GetDOMElement: TDOMElement; virtual;

    function GetAttributeFromElement(ANode: TDOMElement; AName: string; ACanInherit: boolean): string;
    function GetAttribute(AName: string; ADefault: string; ACanInherit: boolean): string; overload;
    function GetAttribute(AName: string; ADefault: string): string; overload;
    function GetAttribute(AName: string): string; overload;
    function GetAttributeNumber(AName: string; ADefault: TSVGNumber): TSVGNumber; overload;
    function GetArrayOfAttributeNumber(AName: string): ArrayOfTSVGNumber;
    function GetArrayOfAttributeNumber(AName: string; ACanInherit: boolean): ArrayOfTSVGNumber;
    function GetAttributeWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit; overload;
    function GetAttributeWithUnit(AName: string): TFloatWithCSSUnit; overload;
    function GetArrayOfAttributeWithUnit(AName: string; ACanInherit: boolean): ArrayOfTFloatWithCSSUnit;
    function GetArrayOfAttributeWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;

    function GetHorizAttribute(AName: string; ADefault: TSVGNumber): TSVGNumber;
    function GetHorizAttribute(AName: string): TSVGNumber;
    function GetHorizAttributeWithUnit(AName: string;
      ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
    function GetHorizAttributeWithUnit(AName: string): TFloatWithCSSUnit;
    function GetArrayOfHorizAttributeWithUnit(AName: string;
      ACanInherit: boolean): ArrayOfTFloatWithCSSUnit;
    function GetArrayOfHorizAttributeWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;

    function GetVerticalAttribute(AName: string; ADefault: TSVGNumber): TSVGNumber;
    function GetVerticalAttribute(AName: string): TSVGNumber;
    function GetVerticalAttributeWithUnit(AName: string;
      ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
    function GetVerticalAttributeWithUnit(AName: string): TFloatWithCSSUnit;
    function GetArrayOfVerticalAttributeWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
    function GetArrayOfVerticalAttributeWithUnit(AName: string; ACanInherit: boolean): ArrayOfTFloatWithCSSUnit;

    function GetOrthoAttributeWithUnit(AName: string;
      ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
    function GetOrthoAttributeWithUnit(AName: string): TFloatWithCSSUnit;
    function GetArrayOfOrthoAttributeWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
    function GetArrayOfOrthoAttributeWithUnit(AName: string; ACanInherit: boolean): ArrayOfTFloatWithCSSUnit;

    function GetAttributeOrStyle(AName,ADefault: string; ACanInherit: boolean): string; overload;
    function GetAttributeOrStyle(AName,ADefault: string): string; overload;
    function GetAttributeOrStyle(AName: string): string; overload;
    function GetHorizAttributeOrStyleWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
    function GetArrayOfHorizAttributeOrStyleWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
    function GetOrthoAttributeOrStyleWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
    function GetArrayOfOrthoAttributeOrStyleWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
    function GetAttributeOrStyleWithUnit(AName: string; ADefault: TFloatWithCSSUnit; ACanInherit: boolean): TFloatWithCSSUnit; overload;
    function GetAttributeOrStyleWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit; overload;
    function GetAttributeOrStyleWithUnit(AName: string): TFloatWithCSSUnit; overload;
    function GetArrayOfAttributeOrStyleWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
    function GetVerticalAttributeOrStyleWithUnit(AName: string; ADefault: TFloatWithCSSUnit; ACanInherit: boolean): TFloatWithCSSUnit; overload;
    function GetVerticalAttributeOrStyleWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit; overload;
    function GetArrayOfVerticalAttributeOrStyleWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;

    function GetNamespaceCount: integer;
    function GetNamespacePrefix(AIndex: integer): string;
    function GetNamespaceURI(APrefix: string): string;

    class function GetPropertyFromStyleDeclarationBlock(ABlock: string;
      AProperty: string; ADefault: string): string;
    class procedure LocateStyleDeclaration(AText: string; AProperty: string;
      out AStartPos, AColonPos, AValueLength: integer);
    class procedure UpdateStyleDeclarationBlock(var ABlock: string; AProperty: string; AValue: string);
    class function RemovePropertyFromDeclarationBlock(var ABlock: string; AProperty: string): boolean;
    function GetInlineStyle(const AName,ADefault: string): string;
    function GetInlineStyleWithUnit(const AName: string): TFloatWithCSSUnit; overload;
    function GetInlineStyleWithUnit(const AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit; overload;
    function GetStyleFromStyleSheet(const {%H-}AName,ADefault: string): string; virtual;
    function GetStyle(const AName,ADefault: string): string; overload;
    function GetStyle(const AName: string): string; overload;

    procedure SetAttribute(AName: string; AValue: TSVGNumber); virtual; overload;
    procedure SetAttribute(AName: string; AValue: string); virtual; overload;
    procedure SetAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
    procedure SetArrayOfAttributeWithUnit(AName: string; const AValue: ArrayOfTFloatWithCSSUnit);

    procedure SetHorizAttribute(AName: string; AValue: TSVGNumber);
    procedure SetHorizAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
    procedure SetArrayOfHorizAttributeWithUnit(AName: string;
      AValue: ArrayOfTFloatWithCSSUnit);

    procedure SetVerticalAttribute(AName: string; AValue: TSVGNumber);
    procedure SetArrayOfAttributeNumber(AName: string; AValue: ArrayOfTSVGNumber);
    procedure SetVerticalAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
    procedure SetArrayOfVerticalAttributeWithUnit(AName: string; AValue: ArrayOfTFloatWithCSSUnit);

    procedure SetOrthoAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
    procedure SetArrayOfOrthoAttributeWithUnit(AName: string; AValue: ArrayOfTFloatWithCSSUnit);

    procedure SetNamespaceURI(APrefix: string; AValue: string);

    procedure SetInlineStyle(AName: string; AValue: string); overload;
    procedure SetInlineStyle(AName: string; AValue: TFloatWithCSSUnit); overload;
  public
    procedure RemoveStyle(const AName: string);
    function HasAttribute(AName: string): boolean;
    function HasInlineStyle(AName: string): boolean;

    procedure IterateElements(ACallback: TIterateElementCallback; AData: pointer; ARecursive: boolean); virtual;
    procedure ConvertToUnit(AUnit: TCSSUnit); virtual;
    function EnterFontSize(AIsRoot: boolean = false): TFloatWithCSSUnit; virtual;
    procedure ExitFontSize(APrevFontSize: TFloatWithCSSUnit); virtual;

    function MatrixToTransform(m: TAffineMatrix; AFromUnit: TCSSUnit): string;
    function TransformToMatrix(ATransform: string; AToUnit: TCSSUnit): TAffineMatrix;

    procedure RemoveNamespace(APrefix: string);
    function NeedNamespace(APrefix: string): boolean;
    property NamespaceURI[APrefix: string]: string read GetNamespaceURI write SetNamespaceURI;
    property NamespacePrefix[AIndex: integer]: string read GetNamespacePrefix;
    property NamespaceCount: integer read GetNamespaceCount;

    property Style[AName: string]: string read GetStyle write SetInlineStyle;
    property StyleDef[AName,ADefault: string]: string read GetStyle;
  end;

  { TSVGElement }

  TSVGElement = class(TSVGCustomElement)
  private
    FImportStyleState: TFindStyleState;
    FImportedStyles: ArrayOfTStyleAttribute;
    function GetClipPath: string;
    function GetFill: string;
    function GetFillColor: TBGRAPixel;
    function GetFillOpacity: single;
    function GetFillRule: string;
    function GetIsFillNone: boolean;
    function GetIsStrokeNone: boolean;
    function GetMatrix(AUnit: TCSSUnit): TAffineMatrix;
    function GetMixBlendMode: TBlendOperation;
    function GetOpacity: single;
    function GetPaintOrder: TSVGPaintOrder;
    function GetStroke: string;
    function GetStrokeColor: TBGRAPixel;
    function GetStrokeLineCap: string;
    function GetStrokeLineCapLCL: TPenEndCap;
    function GetStrokeLineJoin: string;
    function GetStrokeLineJoinLCL: TPenJoinStyle;
    function GetStrokeMiterLimit: single;
    function GetStrokeOpacity: single;
    function GetStrokeWidth: TFloatWithCSSUnit;
    function GetStrokeDashArray: string;
    function GetStrokeDashArrayF: ArrayOfFloat;
    function GetStrokeDashOffset: TFloatWithCSSUnit;
    function GetTransform: string;
    function GetID: string;
    function GetClassAttr: string;
    function GetVisible: boolean;
    procedure SetClipPath(AValue: string);
    procedure SetFillColor(AValue: TBGRAPixel);
    procedure SetFillOpacity(AValue: single);
    procedure SetFillRule(AValue: string);
    procedure SetMatrix(AUnit: TCSSUnit; const AValue: TAffineMatrix);
    procedure SetMixBlendMode(AValue: TBlendOperation);
    procedure SetOpacity(AValue: single);
    procedure SetPaintOrder(AValue: TSVGPaintOrder);
    procedure SetStrokeColor(AValue: TBGRAPixel);
    procedure SetStrokeLineCap(AValue: string);
    procedure SetStrokeLineCapLCL(AValue: TPenEndCap);
    procedure SetStrokeLineJoin(AValue: string);
    procedure SetStrokeLineJoinLCL(AValue: TPenJoinStyle);
    procedure SetStrokeMiterLimit(AValue: single);
    procedure SetStrokeOpacity(AValue: single);
    procedure SetStrokeWidth(AValue: TFloatWithCSSUnit);
    procedure SetStrokeDashArray(AValue: string);
    procedure SetStrokeDashArrayF(AValue: ArrayOfFloat);
    procedure SetStrokeDashOffset(AValue: TFloatWithCSSUnit);
    procedure SetTransform(AValue: string);
    procedure SetID(AValue: string);
    procedure SetClassAttr(AValue: string);
    function FindStyleElementInternal(const classStr: string;
      out attributesStr: string): integer;
    procedure ImportStyles;
    procedure SetVisible(AValue: boolean);
  protected
    FDataLink: TSVGDataLink;
    procedure Init(ADocument: TDOMDocument; ATag: string; AUnits: TCSSUnitConverter); overload;
    procedure Init(AElement: TDOMElement; AUnits: TCSSUnitConverter); overload;
    procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); virtual;
    procedure InternalCopyPathTo({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); virtual;
    function GetStyleFromStyleSheet(const AName,ADefault: string): string; override;
    procedure ApplyFillStyle(ACanvas2D: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); virtual;
    procedure ApplyStrokeStyle(ACanvas2D: TBGRACanvas2D; AUnit: TCSSUnit); virtual;
    procedure SetDatalink(AValue: TSVGDataLink); virtual;
    procedure SetFill(AValue: string); virtual;
    procedure SetStroke(AValue: string); virtual;
    procedure Initialize; virtual;
    procedure Paint(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
  public
    constructor Create(AElement: TDOMElement; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); overload; virtual;
    constructor Create(ADocument: TDOMDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); overload; virtual;
    class function GetDOMTag: string; virtual;
    destructor Destroy; override;
    procedure ListIdentifiers(AResult: TStringList); virtual;
    procedure RenameIdentifiers(AFrom, ATo: TStringList); virtual;
    procedure ConvertToUnit(AUnit: TCSSUnit); override;
    procedure Recompute; virtual;
    procedure Draw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit);
    procedure CopyPathTo({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit);
    procedure fillNone;
    procedure strokeNone;
    procedure strokeDashArrayNone;
    procedure transformNone;
    function fillMode: TSVGFillMode;
    property DataLink: TSVGDataLink read FDataLink write SetDataLink;
    property DOMElement: TDOMElement read GetDOMElement;
    property Units: TCSSUnitConverter read FUnits;
    property ID: string read GetID write SetID;
    property classAttr: string read GetClassAttr write SetClassAttr;
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
    property strokeLineJoinLCL: TPenJoinStyle read GetStrokeLineJoinLCL write SetStrokeLineJoinLCL;
    property strokeLineCap: string read GetStrokeLineCap write SetStrokeLineCap;
    property strokeLineCapLCL: TPenEndCap read GetStrokeLineCapLCL write SetStrokeLineCapLCL;
    property strokeDashArray: string read GetStrokeDashArray write SetStrokeDashArray;
    property strokeDashArrayF: ArrayOfFloat read GetStrokeDashArrayF write SetStrokeDashArrayF;
    property strokeDashOffset: TFloatWithCSSUnit read GetStrokeDashOffset write SetStrokeDashOffset;
    property fill: string read GetFill write SetFill;
    property fillColor: TBGRAPixel read GetFillColor write SetFillColor;
    property fillOpacity: single read GetFillOpacity write SetFillOpacity;
    property fillRule: string read GetFillRule write SetFillRule;
    property paintOrder: TSVGPaintOrder read GetPaintOrder write SetPaintOrder;
    property mixBlendMode: TBlendOperation read GetMixBlendMode write SetMixBlendMode;
    property opacity: single read GetOpacity write SetOpacity;
    property clipPath: string read GetClipPath write SetClipPath;
    property Visible: boolean read GetVisible write SetVisible;

    property Attribute[AName: string]: string read GetAttribute write SetAttribute;
    property AttributeDef[AName,ADefault: string]: string read GetAttribute;
    property AttributeOrStyleDef[AName,ADefault: string]: string read GetAttributeOrStyle;
    property AttributeOrStyle[AName: string]: string read GetAttributeOrStyle;
    property AttributeWithUnitDef[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetAttributeWithUnit;
    property AttributeWithUnit[AName: string]: TFloatWithCSSUnit read GetAttributeWithUnit write SetAttributeWithUnit;
    property ArrayOfAttributeWithUnitInherit[AName: string; ACanInherit: boolean]: ArrayOfTFloatWithCSSUnit read GetArrayOfAttributeWithUnit;
    property ArrayOfAttributeWithUnit[AName: string]: ArrayOfTFloatWithCSSUnit read GetArrayOfAttributeWithUnit write SetArrayOfAttributeWithUnit;

    property ArrayOfAttributeNumberInherit[AName: string; ACanInherit: boolean]: ArrayOfTSVGNumber read GetArrayOfAttributeNumber;
    property ArrayOfAttributeNumber[AName: string]: ArrayOfTSVGNumber read GetArrayOfAttributeNumber write SetArrayOfAttributeNumber;

    property HorizAttributeDef[AName: string; ADefault: TSVGNumber]: TSVGNumber read GetHorizAttribute;
    property HorizAttribute[AName: string]: TSVGNumber read GetHorizAttribute write SetHorizAttribute;
    property HorizAttributeWithUnitDef[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetHorizAttributeWithUnit;
    property HorizAttributeWithUnit[AName: string]: TFloatWithCSSUnit read GetHorizAttributeWithUnit write SetHorizAttributeWithUnit;
    property HorizAttributeOrStyleWithUnit[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetHorizAttributeOrStyleWithUnit;
    property ArrayOfHorizAttributeWithUnitInherit[AName: string; ACanInherit: boolean]: ArrayOfTFloatWithCSSUnit read GetArrayOfHorizAttributeWithUnit;
    property ArrayOfHorizAttributeWithUnit[AName: string]: ArrayOfTFloatWithCSSUnit read GetArrayOfHorizAttributeWithUnit write SetArrayOfHorizAttributeWithUnit;
    property ArrayOfHorizAttributeOrStyleWithUnit[AName: string]: ArrayOfTFloatWithCSSUnit read GetArrayOfHorizAttributeOrStyleWithUnit;

    property VerticalAttributeDef[AName: string; ADefault: TSVGNumber]: TSVGNumber read GetVerticalAttribute;
    property VerticalAttribute[AName: string]: TSVGNumber read GetVerticalAttribute write SetVerticalAttribute;
    property VerticalAttributeWithUnitDef[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetVerticalAttributeWithUnit;
    property VerticalAttributeWithUnit[AName: string]: TFloatWithCSSUnit read GetVerticalAttributeWithUnit write SetVerticalAttributeWithUnit;
    property VerticalAttributeOrStyleWithUnit[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetVerticalAttributeOrStyleWithUnit;
    property ArrayOfVerticalAttributeWithUnitInherit[AName: string; ACanInherit: boolean]: ArrayOfTFloatWithCSSUnit read GetArrayOfVerticalAttributeWithUnit;
    property ArrayOfVerticalAttributeWithUnit[AName: string]: ArrayOfTFloatWithCSSUnit read GetArrayOfVerticalAttributeWithUnit write SetArrayOfVerticalAttributeWithUnit;
    property ArrayOfVerticalAttributeOrStyleWithUnit[AName: string]: ArrayOfTFloatWithCSSUnit read GetArrayOfVerticalAttributeOrStyleWithUnit;

    property OrthoAttributeWithUnitDef[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetOrthoAttributeWithUnit;
    property OrthoAttributeWithUnit[AName: string]: TFloatWithCSSUnit read GetOrthoAttributeWithUnit write SetOrthoAttributeWithUnit;
    property OrthoAttributeOrStyleWithUnit[AName: string; ADefault: TFloatWithCSSUnit]: TFloatWithCSSUnit read GetOrthoAttributeOrStyleWithUnit;
    property ArrayOfOrthoAttributeWithUnit[AName: string]: ArrayOfTFloatWithCSSUnit read GetArrayOfOrthoAttributeWithUnit write SetArrayOfOrthoAttributeWithUnit;
    property ArrayOfOrthoAttributeOrStyleWithUnit[AName: string]: ArrayOfTFloatWithCSSUnit read GetArrayOfOrthoAttributeOrStyleWithUnit;
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
    function ParseTransform: TAffineMatrix;
    procedure SkipSymbol(ASymbol: char);
    procedure SkipUpToSymbol(ASymbol:char);
    procedure ClearError;
    property Position: integer read FPos write FPos;
    property NumberError: boolean read FNumberError;
    property Text: string read FText;
    property Done: boolean read GetDone;
  end;
  
  resourcestring
    rsInvalidIndex = 'Invalid index';

implementation

uses BGRASVGShapes;

{ TSVGCustomElement }

function TSVGCustomElement.GetNamespaceCount: integer;
var
  i: Integer;
  name: string;
begin
  result := 0;
  for i := 0 to FDomElem.Attributes.Length-1 do
  begin
    name := FDomElem.Attributes.Item[i].NodeName;
    if name.StartsWith('xmlns:') then inc(result);
  end;
end;

function TSVGCustomElement.GetNamespacePrefix(AIndex: integer): string;
var
  i: Integer;
  name: string;
begin
  if AIndex < 0 then exit('');
  result := '';
  for i := 0 to FDomElem.Attributes.Length-1 do
  begin
    name := FDomElem.Attributes.Item[i].NodeName;
    if name.StartsWith('xmlns:') then
    begin
      if AIndex > 0 then dec(AIndex)
      else exit(name.Substring(6));
    end;
  end;
end;

function TSVGCustomElement.GetNamespaceURI(APrefix: string): string;
begin
  result := GetAttribute('xmlns:' + APrefix);
end;

procedure TSVGCustomElement.SetNamespaceURI(APrefix: string; AValue: string);
begin
  if AValue = '' then FDomElem.RemoveAttribute('xmlns:' + APrefix)
  else SetAttribute('xmlns:' + APrefix, AValue);
end;

function TSVGCustomElement.GetDOMElement: TDOMElement;
begin
  result := FDomElem;
end;

function TSVGCustomElement.GetAttributeOrStyle(AName, ADefault: string;
  ACanInherit: boolean): string;
var
  curNode: TDOMElement;
  styleDecl: DOMString;
begin
  result := GetInlineStyle(AName,'');
  if result = '' then
  begin
    result := GetStyleFromStyleSheet(AName,'');
    if result = '' then
    begin
      result := GetAttributeFromElement(FDomElem, AName, false);
      if (result = 'currentColor') and (AName <> 'color') then
      begin
        AName := 'color';
        result := GetAttributeFromElement(FDomElem, AName, false);
      end;

      if result = '' then
      begin
        if ACanInherit then
        begin
          curNode := FDomElem;
          while true do
          begin
            if curNode.ParentNode is TDOMElement then
              curNode := TDOMElement(curNode.ParentNode)
            else break;

            styleDecl := curNode.GetAttribute('style');
            result := GetPropertyFromStyleDeclarationBlock(styleDecl, AName, '');
            if result <> '' then exit;
            result := GetAttributeFromElement(curNode, AName, false);
            if (result = 'currentColor') and (AName <> 'color') then
            begin
              curNode := FDomElem;
              AName := 'color';
              continue;
            end;
            if result <> '' then exit;
          end;
        end;
        result := ADefault;
      end;
    end;
  end;
end;

function TSVGCustomElement.GetAttributeOrStyle(AName, ADefault: string): string;
begin
  result := GetAttributeOrStyle(AName, ADefault, true);
end;

function TSVGCustomElement.GetAttributeOrStyle(AName: string): string;
begin
  result:= GetAttributeOrStyle(AName,'');
end;

function TSVGCustomElement.GetHorizAttributeOrStyleWithUnit(AName: string;
  ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := GetAttributeOrStyleWithUnit(AName,ADefault);
end;

function TSVGCustomElement.GetArrayOfHorizAttributeOrStyleWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
begin
  result := GetArrayOfAttributeOrStyleWithUnit(AName);
end;

function TSVGCustomElement.GetOrthoAttributeOrStyleWithUnit(AName: string;
  ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := GetAttributeOrStyleWithUnit(AName,ADefault);
end;

function TSVGCustomElement.GetArrayOfOrthoAttributeOrStyleWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
begin
  result := GetArrayOfAttributeOrStyleWithUnit(AName);
end;

class procedure TSVGCustomElement.LocateStyleDeclaration(AText: string; AProperty: string; out AStartPos,
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

class procedure TSVGCustomElement.UpdateStyleDeclarationBlock(var ABlock: string;
  AProperty: string; AValue: string);
var
  startPos, colonPos, valueLength: integer;
begin
  if pos(';',AValue)<>0 then
    raise exception.Create('Invalid character in value');
  if pos(':',AProperty)<>0 then
    raise exception.Create('Invalid character in name');
  LocateStyleDeclaration(ABlock, AProperty, startPos,colonPos, valueLength);
  if valueLength <> -1 then
  begin
    delete(ABlock, colonPos+1, valueLength);
    insert(' '+Trim(AValue), ABlock, colonPos+1);
  end else
  begin
    while (length(ABlock) > 0) and (ABlock[length(ABlock)] in[' ',#9,#10,#12,#13]) do
      delete(ABlock, length(ABlock), 1);
    if length(ABlock)>0 then
    begin
      if ABlock[length(ABlock)] <> ';' then AppendStr(ABlock, '; ');
    end;
    AppendStr(ABlock, AProperty+': '+AValue);
  end;
end;

class function TSVGCustomElement.RemovePropertyFromDeclarationBlock(
  var ABlock: string; AProperty: string): boolean;
var
  startPos, colonPos, valueLength: integer;
begin
  LocateStyleDeclaration(ABlock, AProperty, startPos,colonPos, valueLength);
  if valueLength <> -1 then
  begin
    delete(ABlock, startPos, colonPos+valueLength-startPos+1);
    while (length(ABlock)>=startPos) and (ABlock[startPos] in[' ',#9,#10,#12,#13]) do delete(ABlock,startPos,1);
    if (length(ABlock)>=startPos) and (ABlock[startPos] = ';') then delete(ABlock,startPos,1);
    result := true;
  end else
    result := false;
end;

class function TSVGCustomElement.GetPropertyFromStyleDeclarationBlock(ABlock: string;
  AProperty: string; ADefault: string): string;
var
  startPos, colonPos, valueLength: integer;
begin
  LocateStyleDeclaration(ABlock, AProperty, startPos,colonPos, valueLength);
  if valueLength <> -1 then
    result := trim(copy(ABlock, colonPos+1, valueLength))
  else
    result := ADefault;
end;

function TSVGCustomElement.GetInlineStyle(const AName, ADefault: string
  ): string;
var
  styleDecl: String;
begin
  styleDecl := GetAttribute('style','',False);
  result := GetPropertyFromStyleDeclarationBlock(styleDecl, AName, ADefault);
end;

function TSVGCustomElement.GetInlineStyleWithUnit(const AName: string): TFloatWithCSSUnit;
begin
  result := GetInlineStyleWithUnit(AName, FloatWithCSSUnit(0, cuCustom));
end;

function TSVGCustomElement.GetInlineStyleWithUnit(const AName: string;
  ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := TCSSUnitConverter.parseValue(GetInlineStyle(AName, ''), ADefault);
end;

function TSVGCustomElement.GetStyleFromStyleSheet(const AName, ADefault: string): string;
begin
  result := ADefault;
end;

function TSVGCustomElement.GetStyle(const AName, ADefault: string): string;
var
  curNode: TDOMElement;
  styleDecl: DOMString;
begin
  result:= GetInlineStyle(AName,'');
  if result <> '' then exit;

  result := GetStyleFromStyleSheet(AName,'');
  if result <> '' then exit;

  curNode := FDomElem;
  while true do
  begin
    if curNode.ParentNode is TDOMElement then
      curNode := TDOMElement(curNode.ParentNode)
    else break;

    styleDecl := curNode.GetAttribute('style');
    result := GetPropertyFromStyleDeclarationBlock(styleDecl, AName, '');
    if result <> '' then exit;
  end;

  result := ADefault;
end;

function TSVGCustomElement.GetStyle(const AName: string): string;
begin
  result:= GetStyle(AName,'');
end;

function TSVGCustomElement.GetAttributeFromElement(ANode: TDOMElement;
  AName: string; ACanInherit: boolean): string;
begin
  repeat
    if ((AName = 'xlink:href') or (AName = 'xlink:title')) and
       not ANode.hasAttribute(AName) and ANode.hasAttribute(AName.Substring(6)) then
      result := Trim(ANode.GetAttribute(AName.Substring(6)))
      else result := Trim(ANode.GetAttribute(AName));

    if result = 'inherit' then result := '';
    if (result = '') and ACanInherit and
      (ANode.ParentNode is TDOMElement) then
      ANode := ANode.ParentNode as TDOMElement
    else
      ANode := nil;
  until ANode = nil;
end;

function TSVGCustomElement.GetAttribute(AName: string; ADefault: string;
  ACanInherit: boolean): string;
begin
  result := GetAttributeFromElement(FDomElem, AName, ACanInherit);
  if result = '' then result := ADefault else
  if (result = 'currentColor') and (AName <> 'color') then
    result := GetAttribute('color', ADefault, ACanInherit);
end;

function TSVGCustomElement.GetAttribute(AName: string; ADefault: string): string;
begin
  result := GetAttribute(AName, ADefault, False);
end;

function TSVGCustomElement.GetAttribute(AName: string): string;
begin
  result:= GetAttribute(AName,'');
end;

function TSVGCustomElement.GetAttributeNumber(AName: string; ADefault: TSVGNumber): TSVGNumber;
begin
  result := TCSSUnitConverter.parseValue(GetAttribute(AName),ADefault);
end;

function TSVGCustomElement.GetAttributeWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := TCSSUnitConverter.parseValue(GetAttribute(AName),ADefault);
end;

function TSVGCustomElement.GetAttributeWithUnit(AName: string): TFloatWithCSSUnit;
begin
  result := GetAttributeWithUnit(AName,FloatWithCSSUnit(0,cuCustom));
end;

function TSVGCustomElement.GetAttributeOrStyleWithUnit(AName: string;
  ADefault: TFloatWithCSSUnit; ACanInherit: boolean): TFloatWithCSSUnit;
var
  valueText: string;
begin
  valueText := GetAttributeOrStyle(AName, '', ACanInherit);
  result := TCSSUnitConverter.parseValue(valueText,ADefault);
end;

function TSVGCustomElement.GetAttributeOrStyleWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := GetAttributeOrStyleWithUnit(AName, ADefault, True);
end;

function TSVGCustomElement.GetAttributeOrStyleWithUnit(AName: string): TFloatWithCSSUnit;
begin
  result := GetAttributeOrStyleWithUnit(AName,FloatWithCSSUnit(0,cuCustom));
end;

function TSVGCustomElement.GetVerticalAttributeOrStyleWithUnit(AName: string;
  ADefault: TFloatWithCSSUnit; ACanInherit: boolean): TFloatWithCSSUnit;
begin
  result := GetAttributeOrStyleWithUnit(AName,ADefault,ACanInherit);
end;

function TSVGCustomElement.GetVerticalAttributeOrStyleWithUnit(AName: string;
  ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := GetVerticalAttributeOrStyleWithUnit(AName,ADefault,true);
end;

function TSVGCustomElement.GetArrayOfVerticalAttributeOrStyleWithUnit(
  AName: string): ArrayOfTFloatWithCSSUnit;
begin
  result := GetArrayOfAttributeOrStyleWithUnit(AName);
end;

procedure TSVGCustomElement.SetInlineStyle(AName: string; AValue: string);
var
  declarationBlock: string;
begin
  declarationBlock := GetAttribute('style','',false);
  UpdateStyleDeclarationBlock(declarationBlock, AName, AValue);
  SetAttribute('style', declarationBlock);
end;

procedure TSVGCustomElement.SetInlineStyle(AName: string;
  AValue: TFloatWithCSSUnit);
begin
  SetInlineStyle(AName, FUnits.formatValue(AValue));
end;

function TSVGCustomElement.GetOrthoAttributeWithUnit(AName: string;
  ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := GetAttributeWithUnit(AName,ADefault);
end;

function TSVGCustomElement.GetOrthoAttributeWithUnit(AName: string): TFloatWithCSSUnit;
begin
  result := GetOrthoAttributeWithUnit(AName,FloatWithCSSUnit(0,cuCustom));
end;

function TSVGCustomElement.GetHorizAttribute(AName: string;
  ADefault: TSVGNumber): TSVGNumber;
var value: TFloatWithCSSUnit;
begin
  value.value := GetAttributeNumber(AName,ADefault);
  value.CSSUnit := cuPixel;
  result := value.value;
end;

function TSVGCustomElement.GetHorizAttribute(AName: string): TSVGNumber;
begin
  result := GetHorizAttribute(AName,0);
end;

function TSVGCustomElement.GetHorizAttributeWithUnit(AName: string;
  ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := GetAttributeWithUnit(AName,ADefault);
end;

function TSVGCustomElement.GetHorizAttributeWithUnit(AName: string): TFloatWithCSSUnit;
begin
  result := GetHorizAttributeWithUnit(AName,FloatWithCSSUnit(0,cuCustom));
end;

function TSVGCustomElement.GetArrayOfAttributeNumber(AName: string): ArrayOfTSVGNumber;
begin
  result := GetArrayOfAttributeNumber(AName,true);
end;

function TSVGCustomElement.GetArrayOfAttributeWithUnit(AName: string; ACanInherit: boolean): ArrayOfTFloatWithCSSUnit;
begin
  result := TCSSUnitConverter.parseArrayOfValuesWithUnit(GetAttribute(AName,'',ACanInherit));
end;

function TSVGCustomElement.GetArrayOfAttributeWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
begin
  result := GetArrayOfAttributeWithUnit(AName,true);
end;

function TSVGCustomElement.GetArrayOfAttributeOrStyleWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
var
  valueText: string;
begin
  valueText := GetAttributeOrStyle(AName);
  result := TCSSUnitConverter.parseArrayOfValuesWithUnit(valueText);
end;

function TSVGCustomElement.GetArrayOfOrthoAttributeWithUnit(AName: string;
  ACanInherit: boolean): ArrayOfTFloatWithCSSUnit;
begin
  result := GetArrayOfAttributeWithUnit(AName, ACanInherit);
end;

function TSVGCustomElement.GetArrayOfOrthoAttributeWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
begin
  result := GetArrayOfOrthoAttributeWithUnit(AName, true);
end;

function TSVGCustomElement.GetArrayOfHorizAttributeWithUnit(AName: string; ACanInherit: boolean): ArrayOfTFloatWithCSSUnit;
begin
  result := GetArrayOfAttributeWithUnit(AName,ACanInherit);
end;

function TSVGCustomElement.GetArrayOfHorizAttributeWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
begin
  result := GetArrayOfHorizAttributeWithUnit(AName,true);
end;

function TSVGCustomElement.GetArrayOfVerticalAttributeWithUnit(AName: string;
  ACanInherit: boolean): ArrayOfTFloatWithCSSUnit;
begin
  result := GetArrayOfAttributeWithUnit(AName,ACanInherit);
end;

function TSVGCustomElement.GetArrayOfAttributeNumber(AName: string;
  ACanInherit: boolean): ArrayOfTSVGNumber;
begin
  result := TCSSUnitConverter.parseArrayOfNumbers(GetAttribute(AName,'',ACanInherit));
end;

function TSVGCustomElement.GetVerticalAttribute(AName: string; ADefault: TSVGNumber): TSVGNumber;
begin
  result := GetAttributeNumber(AName,ADefault);
end;

function TSVGCustomElement.GetVerticalAttribute(AName: string): TSVGNumber;
begin
  result := GetVerticalAttribute(AName,0);
end;

function TSVGCustomElement.GetVerticalAttributeWithUnit(AName: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
begin
  result := GetAttributeWithUnit(AName,ADefault);
end;

function TSVGCustomElement.GetVerticalAttributeWithUnit(AName: string): TFloatWithCSSUnit;
begin
  result := GetVerticalAttributeWithUnit(AName,FloatWithCSSUnit(0,cuCustom));
end;

function TSVGCustomElement.GetArrayOfVerticalAttributeWithUnit(AName: string): ArrayOfTFloatWithCSSUnit;
begin
  result := GetArrayOfVerticalAttributeWithUnit(AName, True);
end;

procedure TSVGCustomElement.SetAttribute(AName: string; AValue: TSVGNumber);
begin
  SetAttribute(AName, TCSSUnitConverter.formatValue(AValue));
end;

procedure TSVGCustomElement.SetAttribute(AName: string; AValue: string);
begin
  if ((AName = 'xlink:href') or (AName = 'xlink:title')) and
     not FDomElem.hasAttribute(AName) and FDomElem.hasAttribute(AName.Substring(6)) then
    FDomElem.SetAttribute(AName.Substring(6), AValue)
  else
    FDomElem.SetAttribute(AName,AValue);
end;

procedure TSVGCustomElement.SetAttributeWithUnit(AName: string;
  AValue: TFloatWithCSSUnit);
begin
  SetAttribute(AName, TCSSUnitConverter.formatValue(AValue));
end;

procedure TSVGCustomElement.SetHorizAttribute(AName: string;
  AValue: TSVGNumber);
begin
  SetAttribute(AName, AValue);
end;

procedure TSVGCustomElement.SetHorizAttributeWithUnit(AName: string;
  AValue: TFloatWithCSSUnit);
begin
  SetAttributeWithUnit(AName, AValue);
end;

procedure TSVGCustomElement.SetArrayOfHorizAttributeWithUnit(AName: string;
  AValue: ArrayOfTFloatWithCSSUnit);
begin
  SetAttribute(AName, TCSSUnitConverter.formatValue(AValue));
end;

procedure TSVGCustomElement.SetVerticalAttribute(AName: string;
  AValue: TSVGNumber);
begin
  SetAttribute(AName, AValue)
end;

procedure TSVGCustomElement.SetArrayOfAttributeNumber(AName: string;
  AValue: ArrayOfTSVGNumber);
begin
  SetAttribute(AName, TCSSUnitConverter.formatValue(AValue));
end;

procedure TSVGCustomElement.SetArrayOfAttributeWithUnit(AName: string;
  const AValue: ArrayOfTFloatWithCSSUnit);
begin
  SetAttribute(AName, TCSSUnitConverter.formatValue(AValue));
end;

procedure TSVGCustomElement.SetVerticalAttributeWithUnit(AName: string;
  AValue: TFloatWithCSSUnit);
begin
  SetAttributeWithUnit(AName, AValue);
end;

procedure TSVGCustomElement.SetArrayOfVerticalAttributeWithUnit(AName: string;
  AValue: ArrayOfTFloatWithCSSUnit);
begin
  SetAttribute(AName, TCSSUnitConverter.formatValue(AValue));
end;

procedure TSVGCustomElement.SetOrthoAttributeWithUnit(AName: string;
  AValue: TFloatWithCSSUnit);
begin
  SetAttribute(AName, TCSSUnitConverter.formatValue(AValue));
end;

procedure TSVGCustomElement.SetArrayOfOrthoAttributeWithUnit(AName: string;
  AValue: ArrayOfTFloatWithCSSUnit);
begin
  SetAttribute(AName, TCSSUnitConverter.formatValue(AValue));
end;

procedure TSVGCustomElement.RemoveStyle(const AName: string);
var
  declarationBlock: string;
begin
  declarationBlock := GetAttribute('style','',false);
  if RemovePropertyFromDeclarationBlock(declarationBlock, AName) then
    SetAttribute('style', declarationBlock);
end;

function TSVGCustomElement.HasAttribute(AName: string): boolean;
begin
  result := FDomElem.hasAttribute(AName);
end;

function TSVGCustomElement.HasInlineStyle(AName: string): boolean;
begin
  result := trim(GetInlineStyle(AName, '')) <> '';  //an empty declaration is illegal
end;

procedure TSVGCustomElement.IterateElements(ACallback: TIterateElementCallback;
  AData: pointer; ARecursive: boolean);
begin
  // no content by default
end;

procedure TSVGCustomElement.ConvertToUnit(AUnit: TCSSUnit);
begin
  if HasInlineStyle('stroke-width') then
    SetInlineStyle('stroke-width', FUnits.ConvertWidth(GetInlineStyleWithUnit('stroke-width'), AUnit));
  if HasInlineStyle('stroke-dash-offset') then
    SetInlineStyle('stroke-dash-offset', FUnits.ConvertWidth(GetInlineStyleWithUnit('stroke-dash-offset'), AUnit));
  if HasInlineStyle('font-size') then
    SetInlineStyle('font-size', FUnits.ConvertHeight(GetInlineStyleWithUnit('font-size'), AUnit));
end;

function TSVGCustomElement.EnterFontSize(AIsRoot: boolean): TFloatWithCSSUnit;
var
  fs: TFloatWithCSSUnit;
begin
  result := FUnits.CurrentFontEmHeight;
  if AIsRoot then FUnits.CurrentFontEmHeight := FUnits.RootFontEmHeight;
  fs := GetAttributeOrStyleWithUnit('font-size', FUnits.CurrentFontEmHeight, false);
  if fs.CSSUnit in [cuFontEmHeight,cuFontXHeight] then
    fs := FUnits.ConvertHeight(fs, cuCustom);
  FUnits.CurrentFontEmHeight:= fs;
end;

procedure TSVGCustomElement.ExitFontSize(APrevFontSize: TFloatWithCSSUnit);
begin
  FUnits.CurrentFontEmHeight := APrevFontSize;
end;

procedure TSVGCustomElement.RemoveNamespace(APrefix: string);
begin
  NamespaceURI['APrefix'] := '';
end;

function TSVGCustomElement.NeedNamespace(APrefix: string): boolean;
var
  prefixColon: DOMString;

  function NeedNamespaceRec(ANode: TDOMElement): boolean;
  var
    i: Integer;
  begin
    for i := 0 to ANode.Attributes.Length-1 do
      if copy(ANode.Attributes[i].NodeName, 1, length(prefixColon)) = prefixColon then
        exit(true);

    for i := 0 to ANode.ChildNodes.Length-1 do
      if (ANode.ChildNodes[i] is TDOMElement) and
         NeedNamespaceRec(ANode.ChildNodes[i] as TDOMElement) then
        exit(true);

    result := false;
  end;

begin
  prefixColon := APrefix+':';
  result := NeedNamespaceRec(FDomElem);
end;

function TSVGCustomElement.MatrixToTransform(m: TAffineMatrix;
  AFromUnit: TCSSUnit): string;
var s: string;
    translateStr: string;
begin
  translateStr := 'translate('+FUnits.formatValue(FUnits.ConvertWidth(m[1,3],AFromUnit,cuCustom))+' '+
      FUnits.formatValue(FUnits.ConvertHeight(m[2,3],AFromUnit,cuCustom))+')';
  if IsAffineMatrixTranslation(m) then
  begin
    if IsAffineMatrixIdentity(m) then
      result := ''
      else result := translateStr;
  end else
  begin
    if (m[1,3] <> 0) or (m[2,3] <> 0) then
    begin
      s := translateStr;
      m[1,3] := 0;
      m[2,3] := 0;
    end else
      s := '';
    if IsAffineMatrixScale(m) then
      result := trim(s+' scale('+FUnits.formatValue(m[1,1])+' '+FUnits.formatValue(m[2,2])+')')
    else
      result := trim(s+' matrix('+FUnits.formatValue(m[1,1])+' '+FUnits.formatValue(m[2,1])+' '+
                     FUnits.formatValue(m[1,2])+' '+FUnits.formatValue(m[2,2])+' ' +
                     FUnits.formatValue(m[1,3])+' '+FUnits.formatValue(m[2,3])+')');
  end;
end;

function TSVGCustomElement.TransformToMatrix(ATransform: string;
  AToUnit: TCSSUnit): TAffineMatrix;
var parser: TSVGParser;
begin
  if Trim(ATransform) = '' then
  begin
    result := AffineMatrixIdentity;
    exit;
  end;
  parser := TSVGParser.Create(ATransform);
  result := parser.ParseTransform;
  result[1,3] := FUnits.ConvertWidth(result[1,3], cuCustom, AToUnit);
  result[2,3] := FUnits.ConvertHeight(result[2,3], cuCustom, AToUnit);
  parser.Free;
end;

{ TSVGViewBox }

function TSVGViewBox.ToString: string;
begin
  result :=
    TCSSUnitConverter.formatValue(min.x)+' '+
    TCSSUnitConverter.formatValue(min.y)+' '+
    TCSSUnitConverter.formatValue(size.x)+' '+
    TCSSUnitConverter.formatValue(size.y);
end;

class function TSVGViewBox.Parse(AValue: string): TSVGViewBox;

  function parseNextFloat: single;
  var
    idxSpace,{%H-}errPos: integer;
  begin
    idxSpace:= pos(' ',AValue);
    if idxSpace <> 0 then
      val(copy(AValue,1,idxSpace-1),result,errPos)
    else
      result := 0;
    delete(AValue,1,idxSpace);
    while (AValue <> '') and (AValue[1] = ' ') do delete(AValue,1,1);
  end;

begin
  AValue := trim(AValue)+' ';
  with result do
  begin
    min.x := parseNextFloat;
    min.y := parseNextFloat;
    size.x := parseNextFloat;
    size.y := parseNextFloat;
  end;
end;

class function TSVGViewBox.DefaultValue: TSVGViewBox;
begin
 with result do
 begin
   min.x := 0;
   min.y := 0;
   size.x := 0;
   size.y := 0;
 end;
end;     

{ TSVGPreserveAspectRatio }

function TSVGPreserveAspectRatio.ToString: string;
begin
  if not Preserve then result := 'none' else
  begin
    result := '';
    case HorizAlign of
    taCenter: AppendStr(result, 'xMid');
    taRightJustify: AppendStr(result, 'xMax');
    else AppendStr(result, 'xMin');
    end;
    case VertAlign of
    tlCenter: AppendStr(result, 'YMid');
    tlBottom: AppendStr(result, 'YMax');
    else AppendStr(result, 'YMin');
    end;
    if Slice then AppendStr(result, ' slice')
    else AppendStr(result, ' meet');
  end;
end;

class function TSVGPreserveAspectRatio.Parse(AValue: string
  ): TSVGPreserveAspectRatio;
var p: TSVGParser;
  id: string;
begin
  p := TSVGParser.Create(AValue);
  result := DefaultValue;
  repeat
    id := p.ParseId;
    if id = 'none' then
    begin
      result.Preserve := false;
      //set other parameters for intermediate value of ViewSize (before stretching non-proportionaly)
      result.Slice := false;
      result.HorizAlign := taCenter;
      result.VertAlign := tlCenter;
      exit;
    end else
    if id = 'slice' then result.Slice := true
    else if (length(id)=8) and (id[1] = 'x') and (id[5] = 'Y') then
    begin
      case copy(id,2,3) of
      'Min': result.HorizAlign := taLeftJustify;
      'Mid': result.HorizAlign := taCenter;
      'Max': result.HorizAlign := taRightJustify;
      end;
      case copy(id,6,3) of
      'Min': result.VertAlign := tlTop;
      'Mid': result.VertAlign := tlCenter;
      'Max': result.VertAlign := tlBottom;
      end;
    end;
  until id = '';
  p.Free;
end;

class function TSVGPreserveAspectRatio.DefaultValue: TSVGPreserveAspectRatio;
begin
  result.Preserve := true;
  result.Slice := false;
  result.HorizAlign := taCenter;
  result.VertAlign := tlCenter;
end;

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

function TSVGParser.ParseTransform: TAffineMatrix;
var
  kind: String;
  m : TAffineMatrix;
  angle,tx,ty: single;
begin
  result := AffineMatrixIdentity;
  while not Done do
  begin
    kind := ParseId;
    if kind = '' then break;
    if ParseSymbol <> '(' then break;
    if compareText(kind,'matrix')=0 then
    begin
      m[1,1] := ParseFloat;
      SkipSymbol(',');
      m[2,1] := ParseFloat;
      SkipSymbol(',');
      m[1,2] := ParseFloat;
      SkipSymbol(',');
      m[2,2] := ParseFloat;
      SkipSymbol(',');
      m[1,3] := ParseFloat;
      SkipSymbol(',');
      m[2,3] := ParseFloat;
      result := result * m;
    end else
    if compareText(kind,'translate')=0 then
    begin
      tx := ParseFloat;
      SkipSymbol(',');
      ty := ParseFloat;
      result := result * AffineMatrixTranslation(tx,ty);
    end else
    if compareText(kind,'scale')=0 then
    begin
      tx := ParseFloat;
      SkipSymbol(',');
      ClearError;
      ty := ParseFloat;
      if NumberError then ty := tx;
      result := result * AffineMatrixScale(tx,ty);
    end else
    if compareText(kind,'rotate')=0 then
    begin
      angle := ParseFloat;
      SkipSymbol(',');
      tx := ParseFloat;
      SkipSymbol(',');
      ty := ParseFloat;
      result := result * AffineMatrixTranslation(tx,ty)*
                AffineMatrixRotationDeg(angle)*
                AffineMatrixTranslation(-tx,-ty);
    end else
    if compareText(kind,'skewx')=0 then
    begin
      angle := ParseFloat;
      result := result * AffineMatrixSkewXDeg(angle);
    end else
    if compareText(kind,'skewy')=0 then
    begin
      angle := ParseFloat;
      result := result * AffineMatrixSkewYDeg(angle);
    end;
    SkipUpToSymbol(')');
  end;
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

constructor TSVGDataLink.Create(AParent: TSVGDataLink);
begin
  FElements:= TSVGElementDictionary.Create;
  FElements.Sorted := true;
  FStyles:= TSVGElementList.Create;
  FParent := AParent;
  if Assigned(FParent) then FParent.FChildren.Add(self);
  FLinkListeners := TSVGLinkListeners.Create;
  FChildren := TList.Create;
end;

destructor TSVGDataLink.Destroy;
var
  i: Integer;
begin
  for i := FChildren.Count-1 downto 0 do
    TSVGDatalink(FChildren[i]).Parent := nil;
  Parent := nil;
  FreeAndNil(FChildren);
  FreeAndNil(FLinkListeners);
  FreeAndNil(FElements);
  FreeAndNil(FStyles);
  inherited Destroy;
end;

function TSVGDataLink.GetElement(AIndex: integer): TSVGElement;
begin
  if (AIndex < 0) or (AIndex > FElements.Count) then
   raise exception.Create(rsInvalidIndex);
  result:= FElements.Data[AIndex];
end;

function TSVGDataLink.GetStyle(AIndex: integer): TSVGElement;
begin
  if not IsValidIndex(AIndex,FStyles) then
   raise exception.Create(rsInvalidIndex);
  result:= FStyles[AIndex];
end;

function TSVGDataLink.IsValidIndex(const AIndex: integer; list: TSVGElementList): boolean;
begin
  result:= (AIndex >= 0) and (AIndex < list.Count);
end;

function TSVGDataLink.FindTo(el: TSVGElement; list: TSVGElementList): integer;
begin
  result := list.IndexOf(el);
end;

procedure TSVGDataLink.NotifyLink(AElement: TSVGElement; ALink: boolean);
var
  i: Integer;
  temp: array of TSVGLinkEvent;
begin
  // make copy because listeners might change the list
  SetLength(temp, FLinkListeners.Count);
  for i:= 0 to high(temp) do
    temp[i] := FLinkListeners.Items[i];
  for i := 0 to high(temp) do
    temp[i](self, AElement, ALink);
  // children datalinks may use the element
  for i := FChildren.Count-1 downto 0 do
    TSVGDataLink(FChildren[i]).NotifyLink(AElement, ALink);
end;

procedure TSVGDataLink.SetParent(AValue: TSVGDataLink);
  // notify link change for all elements and parent elements
  procedure NotifyLinkRec(ADatalink: TSVGDataLink; ALink: boolean);
  var
    i: Integer;
  begin
    if ADatalink = nil then exit;
    for i := 0 to ADatalink.ElementCount-1 do
      NotifyLink(ADatalink.Elements[i], ALink);
    NotifyLinkRec(ADatalink.Parent, ALink);
  end;
begin
  if FParent=AValue then Exit;
  NotifyLinkRec(FParent, False);
  if Assigned(FParent) then FParent.FChildren.Remove(self);
  FParent:=AValue;
  if Assigned(FParent) then FParent.FChildren.Add(self);
  NotifyLinkRec(FParent, True);
end;

function TSVGDataLink.FindElement(el: TSVGElement): integer;
begin
  result:= FElements.IndexOfData(el);
end;

function TSVGDataLink.FindStyle(el: TSVGElement): integer;
begin
  result:= FindTo(el,FStyles);
end;

function TSVGDataLink.ElementCount: integer;
begin
  result:= FElements.Count;
end;

function TSVGDataLink.StyleCount: integer;
begin
  result:= FStyles.Count;
end;

function TSVGDataLink.IsLinkElement(el: TSVGElement): boolean;
begin
  result:= FindElement(el) <> -1;
end;

function TSVGDataLink.IsLinkStyle(el: TSVGElement): boolean;
begin
  result:= FindStyle(el) <> -1;
end;

function TSVGDataLink.IsLink(el: TSVGElement): boolean;
begin
  result:= IsLinkStyle(el) or IsLinkElement(el);
end;

function TSVGDataLink.Link(el: TSVGElement): integer;
begin
  if el.ID <> '' then
  begin
    if FElements.IndexOf(el.ID)<>-1 then exit(-1);
    result := FElements.Add(el.ID, el);
  end else
    result := -1;

  if el is TSVGStyle then
    FStyles.Add(el);

  NotifyLink(el, true);
end;

procedure TSVGDataLink.Unlink(el: TSVGElement);
var
  index: integer;
begin
  index:= FindElement(el);
  if index = -1 then exit;

  if el is TSVGStyle then
    FStyles.Remove(el);

  FElements.Delete(index);
  NotifyLink(el, false);
end;

procedure TSVGDataLink.UnlinkAll;
begin
  FStyles.Clear;
  FElements.Clear;
end;

procedure TSVGDataLink.RegisterLinkListener(AHandler: TSVGLinkEvent;
  ARegister: boolean);
begin
  if ARegister then
    FLinkListeners.Add(AHandler)
    else FLinkListeners.Remove(AHandler);
end;

function TSVGDataLink.FindElementById(AID: string; AClass: TSVGFactory): TSVGElement;
var
  index: Integer;
begin
 index := FElements.IndexOf(AId);
 if index = -1 then
 begin
   if Assigned(Parent) then
     result := Parent.FindElementById(AID, AClass)
     else result := nil
 end
 else
 begin
   result := FElements.Data[index];
   if not (result is AClass) then result := nil;
 end;
end;

function StringStartsWith(AText, AStart: string): boolean;
begin
  Result:= (AStart<>'') and (StrLComp(PChar(AStart),PChar(AText),length(AStart))=0);
end;

function TSVGDataLink.FindElementByRef(ARef: string; AClass: TSVGFactory): TSVGElement;
var
  notFound: boolean;
begin
  result := FindElementByRef(ARef, false, AClass, notFound);
end;

function TSVGDataLink.FindElementByRef(ARef: string; ANeedUrl: boolean; AClass: TSVGFactory;
  out ANotFound: boolean): TSVGElement;
begin
  if StringStartsWith(ARef,'url(#') then
  begin
    result := FindElementById(System.Copy(ARef,6,Length(ARef)-6), AClass);
    ANotFound := (result = nil);
  end
  else if not ANeedUrl and StringStartsWith(ARef,'#') then
  begin
    result := FindElementById(System.Copy(ARef,2,Length(ARef)-1), AClass);
    ANotFound := (result = nil);
  end
  else
  begin
    ANotFound := false;
    exit(nil);
  end;
end;

{ TSVGElement }

function TSVGElement.GetClipPath: string;
begin
  result := GetAttributeOrStyle('clip-path', '', false);
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
begin
 result := TransformToMatrix(transform, AUnit);
end;

function TSVGElement.GetMixBlendMode: TBlendOperation;
var
  opstr: String;
begin
  opstr := AttributeOrStyle['mix-blend-mode'];
  if opstr = 'lighten' then
    result := boLighten else
  if opstr = 'screen' then
    result := boScreen else
  if opstr = 'color-dodge' then
    result := boColorDodge else
  if opstr = 'color-burn' then
    result := boColorBurn else
  if opstr = 'darken' then
    result := boDarken else
  if (opstr = 'plus') or (opstr = 'add') then
    result := boLinearAdd else
  if opstr = 'multiply' then
    result := boMultiply else
  if opstr = 'overlay' then
    result := boOverlay else
  if opstr = 'soft-light' then
    result := boSvgSoftLight else
  if opstr = 'hard-light' then
    result := boHardLight else
  if opstr = 'difference' then
    result := boLinearDifference else
  if opstr = 'difference' then
    result := boLinearDifference else
  if opstr = 'exclusion' then
    result := boLinearExclusion else
  if opstr = 'hue' then
    result := boCorrectedHue else
  if opstr = 'color' then
    result := boCorrectedColor else
  if opstr = 'luminosity' then
    result := boCorrectedLightness else
  if opstr = 'saturation' then
    result := boCorrectedSaturation
  else
    result := boTransparent;
end;

function TSVGElement.GetOpacity: single;
var errPos: integer;
begin
  val(AttributeOrStyleDef['opacity','1'], result, errPos);
  if errPos <> 0 then result := 1 else
    if result < 0 then result := 0 else
      if result > 1 then result := 1;
end;

function TSVGElement.GetPaintOrder: TSVGPaintOrder;
var
  parser: TSVGParser;

  function GetNext: integer;
  var
    id: String;
  begin
    id := parser.ParseId;
    if id = 'fill' then exit(0)
    else if id = 'stroke' then exit(1)
    else if id = 'markers' then exit(2)
    else if id = '' then exit(-1)
    else result := GetNext;
  end;

var
  s: string;
begin
  s := AttributeOrStyle['paint-order', 'normal'];
  if s = 'normal' then exit(spoFillStrokeMarkers);
  parser := TSVGParser.Create(s);
  case GetNext of
  0: case GetNext of
     2: result := spoFillMarkersStroke;
     else result := spoFillStrokeMarkers;
     end;
  1: case GetNext of
     2: result := spoStrokeMarkersFill;
     else result := spoStrokeFillMarkers;
     end;
  2: case GetNext of
     1: result := spoMarkersStrokeFill;
     else result := spoMarkersFillStroke;
     end;
  else
    result := spoFillStrokeMarkers;
  end;
  parser.Free;
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

function TSVGElement.GetStrokeLineCapLCL: TPenEndCap;
var
  s: String;
begin
  s := strokeLineCap;
  if s = 'round' then result := pecRound
  else if s = 'square' then result := pecSquare
  else result := pecFlat;
end;

function TSVGElement.GetStrokeLineJoin: string;
begin
  result := AttributeOrStyleDef['stroke-linejoin','miter'];
end;

function TSVGElement.GetStrokeLineJoinLCL: TPenJoinStyle;
var
  s: String;
begin
  s := strokeLineJoin;
  if s = 'bevel' then result := pjsBevel
  else if s = 'miter' then result := pjsMiter
  else result := pjsRound;
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
  result := AttributeOrStyleDef['stroke-dasharray','none']; 
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
  result := OrthoAttributeOrStyleWithUnit['stroke-dashoffset',
              FloatWithCSSUnit(0,cuCustom)];
end;

function TSVGElement.GetStyleFromStyleSheet(const AName, ADefault: string): string;
var
  i: Integer;
begin
  if FImportStyleState = fssNotSearched then ImportStyles;
  if FImportStyleState <> fssNotFound then
    for i:= Length(FImportedStyles)-1 downto 0 do
    begin
      result:= GetPropertyFromStyleDeclarationBlock(FImportedStyles[i].attr, AName, '');
      if result <> '' then exit;
    end;
  result := inherited GetStyleFromStyleSheet(AName, ADefault);
end;

function TSVGElement.GetTransform: string;
begin
  result := Attribute['transform'];
end;

function TSVGElement.GetID: string;
begin
  result := Attribute['xml:id'];
  if result = '' then result := Attribute['id'];
end; 

function TSVGElement.GetClassAttr: string;
begin
  result := Attribute['class'];
end;

function TSVGElement.GetVisible: boolean;
begin
  result := (AttributeOrStyle['display'] <> 'none');
end;

procedure TSVGElement.SetDatalink(AValue: TSVGDataLink);
begin
  if Assigned(FDataLink) then FDataLink.Unlink(self);
  FDataLink := AValue;
  if Assigned(FDataLink) then FDataLink.Link(self);
end;

procedure TSVGElement.SetClipPath(AValue: string);
begin
  Attribute['clip-path'] := AValue;
  RemoveStyle('clip-path');
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
  fill := LowerCase(BGRAToStr(AValue, CSSColors, 0, true, true));
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

procedure TSVGElement.SetMatrix(AUnit: TCSSUnit; const AValue: TAffineMatrix);
begin
  if not IsAffineMatrixIdentity(AValue) then
    transform := MatrixToTransform(AValue, AUnit)
    else transformNone;
end;

procedure TSVGElement.SetMixBlendMode(AValue: TBlendOperation);
var
  opstr: String;
begin
  case AValue of
  boLighten: opstr := 'lighten';
  boScreen: opstr := 'screen';
  boColorDodge: opstr := 'color-dodge';
  boColorBurn: opstr := 'color-burn';
  boDarken: opstr := 'darken';
  boLinearAdd: opstr := 'add';
  boMultiply: opstr := 'multiply';
  boOverlay: opstr := 'overlay';
  boSvgSoftLight: opstr := 'soft-light';
  boHardLight: opstr := 'hard-light';
  boLinearDifference: opstr := 'difference';
  boLinearExclusion: opstr := 'exclusion';
  boCorrectedHue: opstr := 'hue';
  boCorrectedColor: opstr := 'color';
  boCorrectedLightness: opstr := 'luminosity';
  boCorrectedSaturation: opstr := 'saturation';
  else {boTransparent}
    begin
      RemoveStyle('mix-blend-mode');
      FDomElem.RemoveAttribute('mix-blend-mode');
      exit;
    end;
  end;
  Style['mix-blend-mode'] := opstr;
  FDomElem.RemoveAttribute('mix-blend-mode');
end;

procedure TSVGElement.SetOpacity(AValue: single);
begin
  Attribute['opacity'] := Units.formatValue(AValue);
  RemoveStyle('opacity');
end;

procedure TSVGElement.SetPaintOrder(AValue: TSVGPaintOrder);
var
  s: String;
begin
  case AValue of
    spoFillStrokeMarkers: s := 'normal';
    spoFillMarkersStroke: s := 'fill markers';
    spoStrokeFillMarkers: s := 'stroke';
    spoStrokeMarkersFill: s := 'stroke markers';
    spoMarkersFillStroke: s := 'markers';
    spoMarkersStrokeFill: s := 'markers stroke';
  else
    raise exception.Create('Unknown paint order');
  end;
  Attribute['paint-order'] := s;
  RemoveStyle('paint-order');
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
  stroke := Lowercase(BGRAToStr(AValue, CSSColors, 0, true, true));
end;

procedure TSVGElement.SetStrokeLineCap(AValue: string);
begin
  Attribute['stroke-linecap'] := AValue;
  RemoveStyle('stroke-linecap');
end;

procedure TSVGElement.SetStrokeLineCapLCL(AValue: TPenEndCap);
begin
  case AValue of
  pecRound: strokeLineCap:= 'round';
  pecSquare: strokeLineCap:= 'square';
  else strokeLineCap:= 'butt';
  end;
end;

procedure TSVGElement.SetStrokeLineJoin(AValue: string);
begin
  Attribute['stroke-linejoin'] := AValue;
  RemoveStyle('stroke-linejoin');
end;

procedure TSVGElement.SetStrokeLineJoinLCL(AValue: TPenJoinStyle);
begin
 case AValue of
 pjsBevel: strokeLineJoin:= 'bevel';
 pjsMiter: strokeLineJoin:= 'miter';
 else strokeLineJoin:= 'round';
 end;
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
  RemoveStyle('stroke-dasharray');
end;

procedure TSVGElement.SetStrokeDashArrayF(AValue: ArrayOfFloat);
var 
  s: string;
  i: integer;
begin
  if length(AValue) = 0 then
  begin
    strokeDashArrayNone;
    exit;
  end;
  s:= '';
  for i := 0 to high(AValue) do
  begin
    if s <> '' then AppendStr(s, ' ');
    AppendStr(s, TCSSUnitConverter.formatValue(AValue[i])+' ');
  end;
  strokeDashArray := s;
end;

procedure TSVGElement.SetStrokeDashOffset(AValue: TFloatWithCSSUnit);
begin
  OrthoAttributeWithUnit['stroke-dashoffset'] := AValue;
  RemoveStyle('stroke-dashoffset');
end;      

procedure TSVGElement.SetTransform(AValue: string);
begin
  Attribute['transform'] := AValue;
end;

procedure TSVGElement.SetID(AValue: string);
begin
  if AValue = ID then exit;
  if Assigned(DataLink) then DataLink.Unlink(self);
  if Attribute['xml:id']<>'' then
    Attribute['xml:id'] := AValue
  else
    Attribute['id'] := AValue;
  if Assigned(DataLink) then DataLink.Link(self);
end;

procedure TSVGElement.SetClassAttr(AValue: string);
begin
  Attribute['class'] := AValue;
end; 

procedure TSVGElement.Init(ADocument: TDOMDocument; ATag: string;
  AUnits: TCSSUnitConverter);
begin
  if ATag='' then
    raise exception.Create('Cannot create a generic element');

  FDomElem := ADocument.CreateElement(ATag);
  FUnits := AUnits;
  if Assigned(FDataLink) then FDataLink.Link(self);
end;

procedure TSVGElement.Init(AElement: TDOMElement;
  AUnits: TCSSUnitConverter);
begin
  FDomElem := AElement;
  FUnits := AUnits;
  if Assigned(FDataLink) then FDataLink.Link(self);
end;

procedure TSVGElement.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  //nothing
end;

procedure TSVGElement.InternalCopyPathTo({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit);
begin
 //nothing
end;

procedure TSVGElement.ApplyFillStyle(ACanvas2D: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  ACanvas2D.fillStyle(fillColor);

  ACanvas2D.fillMode := TFillMode(fillMode);
end;    

procedure TSVGElement.ApplyStrokeStyle(ACanvas2D: TBGRACanvas2D; AUnit: TCSSUnit);
var
  a: ArrayOfFloat;
  lw: single;
  i: Integer;
begin
  ACanvas2d.strokeStyle(strokeColor);
  lw := Units.ConvertWidth(strokeWidth,AUnit).value;
  ACanvas2d.lineWidth := lw;
  ACanvas2d.lineCap := strokeLineCap;
  ACanvas2d.lineJoin := strokeLineJoin;
  ACanvas2d.miterLimit := strokeMiterLimit;
  
  a:= strokeDashArrayF;
  if (Length(a) <> 0) and (lw > 0) then
  begin
    for i := 0 to high(a) do
      a[i] := a[i] / lw;
    ACanvas2d.lineStyle(a);
  end
  else
    ACanvas2d.lineStyle(psSolid);
end;

procedure TSVGElement.Initialize;
begin
  SetLength(FImportedStyles,0);
  FImportStyleState   := fssNotSearched;
end;

procedure TSVGElement.Paint(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
  procedure DoFill;
  begin
    if not isFillNone then
    begin
      ApplyFillStyle(ACanvas2D,AUnit);
      ACanvas2d.fill;
    end;
  end;
  procedure DoStroke;
  begin
    if not isStrokeNone then
    begin
      ApplyStrokeStyle(ACanvas2D,AUnit);
      ACanvas2d.stroke;
    end;
  end;
begin
  if paintOrder in [spoFillStrokeMarkers, spoFillMarkersStroke, spoMarkersFillStroke] then
  begin
    DoFill;
    DoStroke;
  end else
  begin
    DoStroke;
    DoFill;
  end;
end;

constructor TSVGElement.Create(AElement: TDOMElement;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  FDataLink:= ADataLink;
  Initialize;
  Init(AElement,AUnits);
end;

constructor TSVGElement.Create(ADocument: TDOMDocument;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  FDataLink:= ADataLink;
  Initialize;
  Init(ADocument, GetDOMTag, AUnits);
end;

class function TSVGElement.GetDOMTag: string;
begin
  result := '';
end;

destructor TSVGElement.Destroy;
begin
  SetLength(FImportedStyles,0);
  if Assigned(FDataLink) then FDataLink.Unlink(self);
  inherited Destroy;
end;

procedure TSVGElement.ListIdentifiers(AResult: TStringList);
var
  myId: String;
begin
  myId := Id;
  if (myId <> '') and (AResult.IndexOf(myId) = -1) then
    AResult.Add(myId);
end;

procedure TSVGElement.RenameIdentifiers(AFrom, ATo: TStringList);
var
  idx: Integer;
  strokeDone, fillDone, clipDone, HrefDone: boolean;
  before, after: String;
begin
  if AFrom.Count <> ATo.Count then raise exception.Create('Identifier list size mismatch');
  idx := AFrom.IndexOf(Id);
  if idx <> -1 then Id := ATo[idx];
  strokeDone := false;
  fillDone:= false;
  clipDone:= false;
  HrefDone:= false;
  for idx := 0 to AFrom.Count-1 do
  begin
    before := 'url(#' + AFrom[idx] + ')';
    after := 'url(#' + ATo[idx] + ')';
    if not strokeDone and (stroke = before) then
    begin stroke := after; strokeDone := true; end;
    if not fillDone and (fill = before) then
    begin fill := after; fillDone := true; end;
    if not clipDone and (clipPath = before) then
    begin clipPath := after; clipDone := true; end;
    if not hrefDone and (Attribute['xlink:href'] = before) then
    begin Attribute['xlink:href'] := after; hrefDone := true; end;
  end;
end;

procedure TSVGElement.ConvertToUnit(AUnit: TCSSUnit);
begin
 inherited ConvertToUnit(AUnit);
 FImportStyleState:= fssNotSearched;
 if HasAttribute('stroke-width') then
   SetAttributeWithUnit('stroke-width', Units.ConvertWidth(GetAttributeWithUnit('stroke-width'), AUnit));
 if HasAttribute('stroke-dash-offset') then
   SetAttributeWithUnit('stroke-dash-offset', Units.ConvertWidth(GetAttributeWithUnit('stroke-dash-offset'), AUnit));
end;

procedure TSVGElement.Recompute;
begin

end;

procedure TSVGElement.Draw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var prevMatrix: TAffineMatrix;
  clipUrl: string;
  clipElem: TSVGClipPath;
  clipFound: boolean;
begin
  if not Visible then exit;
  prevMatrix := ACanvas2d.matrix;
  ACanvas2d.transform(matrix[AUnit]);
  clipUrl := clipPath;
  if clipUrl <> '' then
  begin
    clipElem := TSVGClipPath(DataLink.FindElementByRef(clipUrl, true, TSVGClipPath, clipFound));
    if clipElem <> nil then
    begin
      ACanvas2d.save;
      ACanvas2d.beginPath;
      clipElem.CopyPathTo(ACanvas2d, AUnit);
      ACanvas2d.clip;
    end;
  end
  else
    clipElem := nil;
  InternalDraw(ACanvas2d,AUnit);
  if clipElem <> nil then ACanvas2d.restore;
  ACanvas2d.matrix := prevMatrix;
end;

procedure TSVGElement.CopyPathTo({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit);
var prevMatrix: TAffineMatrix;
begin
  if not Visible then exit;
  prevMatrix := ACanvas2d.matrix;
  ACanvas2d.transform(matrix[AUnit]);
  InternalCopyPathTo(ACanvas2d,AUnit);
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

procedure TSVGElement.strokeDashArrayNone;
begin
  strokeDashArray := 'none';
end;

procedure TSVGElement.transformNone;
begin
  FDomElem.RemoveAttribute('transform');
end;

function TSVGElement.fillMode: TSVGFillMode;
begin
  if fillRule = 'evenodd' then
    result := sfmEvenOdd
  else
    result := sfmNonZero;
end;

function TSVGElement.FindStyleElementInternal(const classStr: string;
  out attributesStr: string): integer;
var
  i: integer;
  data: TSVGDataLink;
begin
  attributesStr:= '';
  data := FDataLink;
  while data <> nil do
  begin
    with data do
      for i:= 0 to StyleCount-1 do
      begin
        result:= (Styles[i] as TSVGStyle).Find(classStr);
        if result <> -1 then
        begin
          attributesStr:= (Styles[i] as TSVGStyle).Ruleset[result].declarations;
          Exit;
        end;
      end;
    data := data.Parent;
  end;
  result:= -1;
end;

procedure TSVGElement.ImportStyles;

  procedure AddStyle(const s: string; const id: integer);
  var
    l: integer;
  begin
    FImportStyleState:= fssFound;
    l:= Length(FImportedStyles);
    SetLength(FImportedStyles,l+1);
    with FImportedStyles[l] do
    begin
     attr:= s;
     pos:= id;
    end;
  end;

var
  fid: integer;
  tag,styleC,s,c: string;
begin
  FImportStyleState:= fssNotFound;
  SetLength(FImportedStyles,0);
  tag:= FDomElem.TagName;
  (*
    if style element is:
    <style>
     circle.test{fill:red; fill-opacity: 0.8;}
     circle{fill:blue; fill-opacity: 0.4;}
     circle.style1{fill:yellow;}
    </style>
    and circle declare:
    <circle class = "style1" cx="160" cy="160" r="35" stroke="black" />

    FImportedStyles[0] = 'fill:blue; fill-opacity: 0.4;'
    FImportedStyles[1] = 'fill:yellow;'

    fill-opacity for "style1" = 0.4 not default 1!
  *)

  //Find as: "[tag]" example "circle"
  fid:= FindStyleElementInternal(tag,s);
  if fid <> -1 then
    AddStyle(s,fid);
  styleC:= classAttr.Trim;
  if styleC <> '' then
  for c in styleC.Trim.Split([' ']) do
  begin
    //Find as: "[tag].[class]" example "circle.style1"
    fid:= FindStyleElementInternal(tag+'.'+c,s);
    if fid <> -1 then
      AddStyle(s,fid)
    else
    begin
      //Find as: ".[class]" example ".style1"
      fid:= FindStyleElementInternal('.'+c,s);
      if fid <> -1 then
        AddStyle(s,fid);
    end;
  end;
end;

procedure TSVGElement.SetVisible(AValue: boolean);
begin
  if AValue <> Visible then
    Style['display'] := 'inline';
  FDomElem.RemoveAttribute('display');
end;

end.

