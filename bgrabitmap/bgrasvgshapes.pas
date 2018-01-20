unit BGRASVGShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAUnits, laz2_DOM, BGRAPath, BGRABitmapTypes,
  BGRACanvas2D, BGRASVGType;

type
  TSVGGradient = class;
  
  { TSVGElementWithGradient }

  TSVGElementWithGradient = class(TSVGElement)
    private
      FGradientElement: TSVGGradient;
      FGradientElementDefined: boolean;
      FCanvasGradient: IBGRACanvasGradient2D;
      function EvaluatePercentage(fu: TFloatWithCSSUnit): single; { fu is a percentage of a number [0.0..1.0] }
      function GetGradientElement: TSVGGradient;
      procedure ResetGradient;
      function FindGradientElement: boolean;
    protected
      procedure Initialize; override;
      procedure AddStopElements(canvas: IBGRACanvasGradient2D);
      procedure CreateCanvasLinearGradient(ACanvas2d: TBGRACanvas2D; ASVGGradient: TSVGGradient;
        const origin: TPointF; const w,h: single; AUnit: TCSSUnit);
      procedure CreateCanvasRadialGradient(ACanvas2d: TBGRACanvas2D; ASVGGradient: TSVGGradient;
        const origin: TPointF; const w,h: single; AUnit: TCSSUnit);
      procedure ApplyFillStyle(ACanvas2D: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      procedure InitializeGradient(ACanvas2d: TBGRACanvas2D;
                const origin: TPointF; const w,h: single; AUnit: TCSSUnit);
      property GradientElement: TSVGGradient read GetGradientElement;
  end;       
  
  { TSVGLine }

  TSVGLine = class(TSVGElement)
    private
      function GetX1: TFloatWithCSSUnit;
      function GetX2: TFloatWithCSSUnit;
      function GetY1: TFloatWithCSSUnit;
      function GetY2: TFloatWithCSSUnit;
      procedure SetX1(AValue: TFloatWithCSSUnit);
      procedure SetX2(AValue: TFloatWithCSSUnit);
      procedure SetY1(AValue: TFloatWithCSSUnit);
      procedure SetY2(AValue: TFloatWithCSSUnit);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
      property x1: TFloatWithCSSUnit read GetX1 write SetX1;
      property y1: TFloatWithCSSUnit read GetY1 write SetY1;
      property x2: TFloatWithCSSUnit read GetX2 write SetX2;
      property y2: TFloatWithCSSUnit read GetY2 write SetY2;
  end;

  { TSVGRectangle }

  TSVGRectangle = class(TSVGElementWithGradient)
    private
      function GetX: TFloatWithCSSUnit;
      function GetY: TFloatWithCSSUnit;
      function GetWidth: TFloatWithCSSUnit;
      function GetHeight: TFloatWithCSSUnit;
      function GetRX: TFloatWithCSSUnit;
      function GetRY: TFloatWithCSSUnit;
      procedure SetX(AValue: TFloatWithCSSUnit);
      procedure SetY(AValue: TFloatWithCSSUnit);
      procedure SetWidth(AValue: TFloatWithCSSUnit);
      procedure SetHeight(AValue: TFloatWithCSSUnit);
      procedure SetRX(AValue: TFloatWithCSSUnit);
      procedure SetRY(AValue: TFloatWithCSSUnit);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
      property x: TFloatWithCSSUnit read GetX write SetX;
      property y: TFloatWithCSSUnit read GetY write SetY;
      property width: TFloatWithCSSUnit read GetWidth write SetWidth;
      property height: TFloatWithCSSUnit read GetHeight write SetHeight;
      property rx: TFloatWithCSSUnit read GetRX write SetRX;
      property ry: TFloatWithCSSUnit read GetRY write SetRY;
  end;

  { TSVGCircle }

  TSVGCircle = class(TSVGElementWithGradient)
    private
      function GetCX: TFloatWithCSSUnit;
      function GetCY: TFloatWithCSSUnit;
      function GetR: TFloatWithCSSUnit;
      procedure SetCX(AValue: TFloatWithCSSUnit);
      procedure SetCY(AValue: TFloatWithCSSUnit);
      procedure SetR(AValue: TFloatWithCSSUnit);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
      property cx: TFloatWithCSSUnit read GetCX write SetCX;
      property cy: TFloatWithCSSUnit read GetCY write SetCY;
      property r: TFloatWithCSSUnit read GetR write SetR;
  end;

  { TSVGEllipse }

  TSVGEllipse = class(TSVGElementWithGradient)
    private
      function GetCX: TFloatWithCSSUnit;
      function GetCY: TFloatWithCSSUnit;
      function GetRX: TFloatWithCSSUnit;
      function GetRY: TFloatWithCSSUnit;
      procedure SetCX(AValue: TFloatWithCSSUnit);
      procedure SetCY(AValue: TFloatWithCSSUnit);
      procedure SetRX(AValue: TFloatWithCSSUnit);
      procedure SetRY(AValue: TFloatWithCSSUnit);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
      property cx: TFloatWithCSSUnit read GetCX write SetCX;
      property cy: TFloatWithCSSUnit read GetCY write SetCY;
      property rx: TFloatWithCSSUnit read GetRX write SetRX;
      property ry: TFloatWithCSSUnit read GetRY write SetRY;
  end;

  { TSVGPath }

  TSVGPath = class(TSVGElementWithGradient)
    private
      FPath: TBGRAPath;
      FBoundingBox: TRectF;
      FBoundingBoxComputed: boolean;
      function GetBoundingBoxF: TRectF;
      function GetPath: TBGRAPath;
      function GetPathLength: TFloatWithCSSUnit;
      function GetData: string;
      procedure SetPathLength(AValue: TFloatWithCSSUnit);
      procedure SetData(AValue: string);
    protected
      function GetDOMElement: TDOMElement; override;
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
      constructor Create(ADocument: TXMLDocument; AElement: TDOMElement; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
      destructor Destroy; override;
      property d: string read GetData write SetData;
      property path: TBGRAPath read GetPath;
      property pathLength: TFloatWithCSSUnit read GetPathLength write SetPathLength;
      property boundingBoxF: TRectF read GetBoundingBoxF;
  end;

  { TSVGPolypoints }

  TSVGPolypoints = class(TSVGElementWithGradient)
    private
      FBoundingBox: TRectF;
      FBoundingBoxComputed: boolean;
      function GetBoundingBoxF: TRectF;
      function GetClosed: boolean;
      function GetPoints: string;
      function GetPointsF: ArrayOfTPointF;
      procedure SetPoints(AValue: string);
      procedure SetPointsF(AValue: ArrayOfTPointF);
      procedure ComputeBoundingBox(APoints: ArrayOfTPointF);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; AClosed: boolean; ADataLink: TSVGDataLink); overload;
      destructor Destroy; override;
      property points: string read GetPoints write SetPoints;
      property pointsF: ArrayOfTPointF read GetPointsF write SetPointsF;
      property closed: boolean read GetClosed;
      property boundingBoxF: TRectF read GetBoundingBoxF;
  end;

  { TSVGText }

  TSVGText = class(TSVGElementWithGradient)
    private
      function GetFontBold: boolean;
      function GetFontFamily: string;
      function GetFontItalic: boolean;
      function GetFontSize: TFloatWithCSSUnit;
      function GetFontStyle: string;
      function GetFontWeight: string;
      function GetSimpleText: string;
      function GetTextDecoration: string;
      function GetX: TFloatWithCSSUnit;
      function GetY: TFloatWithCSSUnit;
      procedure SetFontBold(AValue: boolean);
      procedure SetFontFamily(AValue: string);
      procedure SetFontItalic(AValue: boolean);
      procedure SetFontSize(AValue: TFloatWithCSSUnit);
      procedure SetFontStyle(AValue: string);
      procedure SetFontWeight(AValue: string);
      procedure SetSimpleText(AValue: string);
      procedure SetTextDecoration(AValue: string);
      procedure SetX(AValue: TFloatWithCSSUnit);
      procedure SetY(AValue: TFloatWithCSSUnit);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
      property x: TFloatWithCSSUnit read GetX write SetX;
      property y: TFloatWithCSSUnit read GetY write SetY;
      property SimpleText: string read GetSimpleText write SetSimpleText;
      property fontSize: TFloatWithCSSUnit read GetFontSize write SetFontSize;
      property fontFamily: string read GetFontFamily write SetFontFamily;
      property fontWeight: string read GetFontWeight write SetFontWeight;
      property fontStyle: string read GetFontStyle write SetFontStyle;
      property textDecoration: string read GetTextDecoration write SetTextDecoration;
      property fontBold: boolean read GetFontBold write SetFontBold;
      property fontItalic: boolean read GetFontItalic write SetFontItalic;
  end;

  TSVGContent = class;
  
  TConvMethod = (cmNone,cmHoriz,cmVertical,cmOrtho);
  
  { TSVGGradient } 

  TSVGGradient = class(TSVGElement)
    private
      FContent: TSVGContent;
      function GetGradientMatrix(AUnit: TCSSUnit): TAffineMatrix;
      function GetGradientTransform: string;
      function GetGradientUnits: string;
      function GetHRef: string;
      function GetUseObjectBoundingBox: boolean;
      procedure SetGradientTransform(AValue: string);
      procedure SetGradientUnits(AValue: string);
      procedure SetHRef(AValue: string);
      function HRefToGradientID(const AValue: string): string;
      function FindGradientRef(const AGradientID: string): integer;
    protected
      InheritedGradients: TSVGElementList;//(for HRef)
      procedure Initialize; override;
      function GetInheritedAttribute(AValue: string;
        AConvMethod: TConvMethod; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter;
        ADataLink: TSVGDataLink); override;
      constructor Create(ADocument: TXMLDocument; AElement: TDOMElement;
        AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
      destructor Destroy; override;
      procedure Recompute; override;
      procedure ScanInheritedGradients(const forceScan: boolean = false);
      property Content: TSVGContent read FContent;
      property hRef: string read GetHRef write SetHRef;
      property gradientUnits: string read GetGradientUnits write SetGradientUnits;
      property gradientTransform: string read GetGradientTransform write SetGradientTransform;
      property useObjectBoundingBox: boolean read GetUseObjectBoundingBox;
      property gradientMatrix[AUnit: TCSSUnit]: TAffineMatrix read GetGradientMatrix;
  end;        

  { TSVGGradientLinear }

  TSVGLinearGradient = class(TSVGGradient)
    private
      function GetX1: TFloatWithCSSUnit;
      function GetX2: TFloatWithCSSUnit;
      function GetY1: TFloatWithCSSUnit;
      function GetY2: TFloatWithCSSUnit;
      procedure SetX1(AValue: TFloatWithCSSUnit);
      procedure SetX2(AValue: TFloatWithCSSUnit);
      procedure SetY1(AValue: TFloatWithCSSUnit);
      procedure SetY2(AValue: TFloatWithCSSUnit);
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter;
        ADataLink: TSVGDataLink); override;
      property x1: TFloatWithCSSUnit read GetX1 write SetX1;
      property y1: TFloatWithCSSUnit read GetY1 write SetY1;
      property x2: TFloatWithCSSUnit read GetX2 write SetX2;
      property y2: TFloatWithCSSUnit read GetY2 write SetY2;
  end;

  { TSVGRadialGradient }

  TSVGRadialGradient = class(TSVGGradient)
    private
      function GetCX: TFloatWithCSSUnit;
      function GetCY: TFloatWithCSSUnit;
      function GetR: TFloatWithCSSUnit;
      function GetFX: TFloatWithCSSUnit;
      function GetFY: TFloatWithCSSUnit;
      function GetFR: TFloatWithCSSUnit;
      procedure SetCX(AValue: TFloatWithCSSUnit);
      procedure SetCY(AValue: TFloatWithCSSUnit);
      procedure SetR(AValue: TFloatWithCSSUnit);
      procedure SetFX(AValue: TFloatWithCSSUnit);
      procedure SetFY(AValue: TFloatWithCSSUnit);
      procedure SetFR(AValue: TFloatWithCSSUnit);
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter;
        ADataLink: TSVGDataLink); override;
      property cx: TFloatWithCSSUnit read GetCX write SetCX;
      property cy: TFloatWithCSSUnit read GetCY write SetCY;
      property r: TFloatWithCSSUnit read GetR write SetR;
      property fx: TFloatWithCSSUnit read GetFX write SetFX;
      property fy: TFloatWithCSSUnit read GetFY write SetFY;
      property fr: TFloatWithCSSUnit read GetFR write SetFR;
  end;

  { TSVGStopGradient }

  TSVGStopGradient = class(TSVGElement)
    private
      function GetOffset: TFloatWithCSSUnit;
      procedure SetOffset(AValue: TFloatWithCSSUnit);
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter;
        ADataLink: TSVGDataLink); override;
      property Offset: TFloatWithCSSUnit read GetOffset write SetOffset;
  end;

  { TSVGDefine }

  TSVGDefine = class(TSVGElement)
  protected
    FContent: TSVGContent;
  public
    constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter;
      ADataLink: TSVGDataLink); override;
    constructor Create(ADocument: TXMLDocument; AElement: TDOMElement;
      AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
    destructor Destroy; override;
    procedure Recompute; override;
    property Content: TSVGContent read FContent;
  end; 

  { TSVGGroup }

  TSVGGroup = class(TSVGElement)
  protected
    FContent: TSVGContent;
    procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
  public
    constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
    constructor Create(ADocument: TXMLDocument; AElement: TDOMElement;
      AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
    destructor Destroy; override;
    procedure Recompute; override;
    property Content: TSVGContent read FContent;
  end;
  
  { TSVGStyle }

  TSVGStyleItem = record
    name,
    attribute: string;
  end;
  ArrayOfTSVGStyleItem = array of TSVGStyleItem;

  TSVGStyle = class(TSVGElement)
   private
     FStyles: ArrayOfTSVGStyleItem;
     procedure Parse(const s: String);
     function IsValidID(const sid: integer): boolean;
     function GetStyle(const sid: integer): TSVGStyleItem;
     procedure SetStyle(const sid: integer; sr: TSVGStyleItem);
     function Find(sr: TSVGStyleItem): integer; overload;
   protected
     procedure Initialize; override;
   public
     constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
     constructor Create(ADocument: TXMLDocument; AElement: TDOMElement;
       AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
     destructor Destroy; override;
     function Count: Integer;
     function Find(const AName: string): integer; overload;
     function Add(sr: TSVGStyleItem): integer;
     procedure Remove(sr: TSVGStyleItem);
     procedure Clear;
     procedure ReParse;
     property Styles[sid: integer]: TSVGStyleItem read GetStyle write SetStyle;
  end;                  

  { TSVGContent }

  TSVGContent = class
    protected
      FDataLink: TSVGDataLink;
      FDomElem: TDOMElement;
      FDoc: TXMLDocument;
      FElements: TFPList;
      FUnits: TCSSUnitConverter;
      procedure AppendElement(AElement: TSVGElement);
      procedure InsertElementBefore(AElement: TSVGElement; ASuccessor: TSVGElement);
      function GetElement(AIndex: integer): TSVGElement;
      function GetElementCount: integer;
      function GetUnits: TCSSUnitConverter;
    public
      constructor Create(ADocument: TXMLDocument; AElement: TDOMElement; AUnits: TCSSUnitConverter; 
        ADataLink: TSVGDataLink; ADataParent: TSVGElement);
      destructor Destroy; override;
      procedure Recompute;
      procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; AUnit: TCSSUnit); overload;
      procedure Draw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); overload;
      function AppendLine(x1,y1,x2,y2: single; AUnit: TCSSUnit = cuCustom): TSVGLine; overload;
      function AppendLine(p1,p2: TPointF; AUnit: TCSSUnit = cuCustom): TSVGLine; overload;
      function AppendCircle(cx,cy,r: single; AUnit: TCSSUnit = cuCustom): TSVGCircle; overload;
      function AppendCircle(c: TPointF; r: single; AUnit: TCSSUnit = cuCustom): TSVGCircle; overload;
      function AppendEllipse(cx,cy,rx,ry: single; AUnit: TCSSUnit = cuCustom): TSVGEllipse; overload;
      function AppendEllipse(c,r: TPointF; AUnit: TCSSUnit = cuCustom): TSVGEllipse; overload;
      function AppendPath(data: string; AUnit: TCSSUnit = cuCustom): TSVGPath; overload;
      function AppendPath(path: TBGRAPath; AUnit: TCSSUnit = cuCustom): TSVGPath; overload;
      function AppendPolygon(const points: array of single; AUnit: TCSSUnit = cuCustom): TSVGPolypoints; overload;
      function AppendPolygon(const points: array of TPointF; AUnit: TCSSUnit = cuCustom): TSVGPolypoints; overload;
      function AppendRect(x,y,width,height: single; AUnit: TCSSUnit = cuCustom): TSVGRectangle; overload;
      function AppendRect(origin,size: TPointF; AUnit: TCSSUnit = cuCustom): TSVGRectangle; overload;
      function AppendText(x,y: single; AText: string; AUnit: TCSSUnit = cuCustom): TSVGText; overload;
      function AppendText(origin: TPointF; AText: string; AUnit: TCSSUnit = cuCustom): TSVGText; overload;
      function AppendRoundRect(x,y,width,height,rx,ry: single; AUnit: TCSSUnit = cuCustom): TSVGRectangle; overload;
      function AppendRoundRect(origin,size,radius: TPointF; AUnit: TCSSUnit = cuCustom): TSVGRectangle; overload;
      property ElementCount: integer read GetElementCount;
      property Element[AIndex: integer]: TSVGElement read GetElement;
      property Units: TCSSUnitConverter read GetUnits;
  end;

function GetSVGFactory(ATagName: string): TSVGFactory;
function CreateSVGElementFromNode(ADocument: TXMLDocument;
  AElement: TDOMElement; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink; ADataParent: TSVGElement): TSVGElement;

implementation

uses BGRATransform, BGRAGraphics;

function GetSVGFactory(ATagName: string): TSVGFactory;
var tag: string;
begin
  tag := LowerCase(ATagName);
  if tag='line' then
    result := TSVGLine else
  if tag='rect' then
    result := TSVGRectangle else
  if tag='circle' then
    result := TSVGCircle else
  if tag='ellipse' then
    result := TSVGEllipse else
  if tag='path' then
    result := TSVGPath else
  if (tag='polygon') or (tag='polyline') then
    result := TSVGPolypoints else
  if tag='text' then
    result := TSVGText else
  if tag='lineargradient' then
    result := TSVGLinearGradient else
  if tag='radialgradient' then
    result := TSVGRadialGradient else
  if tag='stop' then
    result := TSVGStopGradient else
  if tag='defs' then
    result := TSVGDefine else 
  if tag='g' then
    result := TSVGGroup else
  if tag='style' then 
    result := TSVGStyle else
    result := TSVGElement;
end;

function CreateSVGElementFromNode(ADocument: TXMLDocument;
  AElement: TDOMElement; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink; ADataParent: TSVGElement): TSVGElement;
var
  factory: TSVGFactory;
begin
  factory := GetSVGFactory(AElement.TagName);
  result := factory.Create(ADocument,AElement,AUnits,ADataLink);
  
  ADataLink.Link(result,ADataParent);
end;

{ TSVGElementWithGradient }

procedure TSVGElementWithGradient.Initialize;
begin
  inherited Initialize;
  ResetGradient;
end;

procedure TSVGElementWithGradient.ResetGradient;
begin
  FGradientElementDefined := false;
  FGradientElement        := nil;
  FCanvasGradient         := nil;
end;

function TSVGElementWithGradient.FindGradientElement: boolean;
var
  i: integer;
  s: string;
begin
  Result:= false;
  s:= fill;
  if s <> '' then
    if Pos('url(#',s) = 1 then
    begin
      s:= System.Copy(s,6,Length(s)-6);
      with FDataLink do
        for i:= GradientCount-1 downto 0 do 
          if (Gradients[i] as TSVGGradient).ID = s then
          begin
            FGradientElement:= TSVGGradient(Gradients[i]);
            Result:= true;
            Exit;
          end;
    end;
end;

function TSVGElementWithGradient.EvaluatePercentage(fu: TFloatWithCSSUnit): single;
begin
  Result:= fu.value;
  if fu.CSSUnit <> cuPercent then
  begin
    if Result < 0 then
      Result:= 0
    else if Result > 1 then
      Result:= 1;
    Result:= Result * 100;
  end;
end;

function TSVGElementWithGradient.GetGradientElement: TSVGGradient;
begin
  if not FGradientElementDefined then
  begin
    FindGradientElement;
    FGradientElementDefined:= true;
    if FGradientElement <> nil then
      FGradientElement.ScanInheritedGradients;
  end;
  result := FGradientElement;
end;

procedure TSVGElementWithGradient.AddStopElements(canvas: IBGRACanvasGradient2D);

  function AddStopElementFrom(el: TSVGElement): integer;
  var
    i: integer;
    col: TBGRAPixel;
  begin
    result:= 0;
    with el.DataChildList do
      for i:= 0 to Count-1 do
        if Items[i] is TSVGStopGradient then
          with (Items[i] as TSVGStopGradient) do
          begin
            col:= StrToBGRA( AttributeOrStyleDef['stop-color','black'] );
            col.alpha:= Round( Units.parseValue(AttributeOrStyleDef['stop-opacity','1'],1) * col.alpha );
            canvas.addColorStop(EvaluatePercentage(offset)/100, col);
            Inc(result);
          end;
  end;

var
  i: integer;
begin
  if not Assigned(GradientElement) then exit;
  with GradientElement.InheritedGradients do
    for i:= 0 to Count-1 do
      AddStopElementFrom(Items[i]);
end;

procedure TSVGElementWithGradient.CreateCanvasLinearGradient(
  ACanvas2d: TBGRACanvas2D; ASVGGradient: TSVGGradient;
  const origin: TPointF; const w,h: single; AUnit: TCSSUnit);
var p1,p2: TPointF;
  g: TSVGLinearGradient;
  m: TAffineMatrix;
begin
  g := ASVGGradient as TSVGLinearGradient;
  if g.useObjectBoundingBox then
  begin
    p1.x:= EvaluatePercentage(g.x1)/100;
    p1.y:= EvaluatePercentage(g.y1)/100;
    p2.x:= EvaluatePercentage(g.x2)/100;
    p2.y:= EvaluatePercentage(g.y2)/100;
    m := ACanvas2d.matrix;
    ACanvas2d.translate(origin.x,origin.y);
    ACanvas2d.scale(w,h);
    ACanvas2d.transform(g.gradientMatrix[cuCustom]);
    FCanvasGradient:= ACanvas2d.createLinearGradient(p1,p2);
    ACanvas2d.matrix := m;
  end else
  begin
    p1.x:= Units.ConvertWidth(g.x1,AUnit,w).value;
    p1.y:= Units.ConvertHeight(g.y1,AUnit,h).value;
    p2.x:= Units.ConvertWidth(g.x1,AUnit,w).value;
    p2.y:= Units.ConvertHeight(g.y1,AUnit,h).value;
    m := ACanvas2d.matrix;
    ACanvas2d.transform(g.gradientMatrix[AUnit]);
    FCanvasGradient:= ACanvas2d.createLinearGradient(p1,p2);
    ACanvas2d.matrix := m;
  end;

  AddStopElements(FCanvasGradient);
end;

procedure TSVGElementWithGradient.CreateCanvasRadialGradient(
  ACanvas2d: TBGRACanvas2D; ASVGGradient: TSVGGradient; const origin: TPointF;
  const w, h: single; AUnit: TCSSUnit);
var c,f: TPointF;
  r,fr: single;
  g: TSVGRadialGradient;
  m: TAffineMatrix;

  procedure CheckFocalAndCreate(c: TPointF; r: single; f: TPointF; fr: single);
  var u: TPointF;
    d: single;
  begin
    u := f-c;
    d := VectLen(u);
    if d >= r then
    begin
      u *= (r/d)*0.99999;
      f := c+u;
    end;
    FCanvasGradient:= ACanvas2d.createRadialGradient(c,r,f,fr,true);
    AddStopElements(FCanvasGradient);
  end;

begin
  g := ASVGGradient as TSVGRadialGradient;
  if g.useObjectBoundingBox then
  begin
    c.x:= EvaluatePercentage(g.cx)/100;
    c.y:= EvaluatePercentage(g.cy)/100;
    r:= abs(EvaluatePercentage(g.r))/100;
    f.x:= EvaluatePercentage(g.fx)/100;
    f.y:= EvaluatePercentage(g.fy)/100;
    fr:= abs(EvaluatePercentage(g.fr))/100;

    m := ACanvas2d.matrix;
    ACanvas2d.translate(origin.x,origin.y);
    ACanvas2d.scale(w,h);
    ACanvas2d.transform(g.gradientMatrix[cuCustom]);
    CheckFocalAndCreate(c,r,f,fr);
    ACanvas2d.matrix := m;
  end else
  begin
    c.x:= Units.ConvertWidth(g.cx,AUnit,w).value;
    c.y:= Units.ConvertHeight(g.cy,AUnit,h).value;
    r:= abs(Units.ConvertWidth(g.r,AUnit,w).value);
    f.x:= Units.ConvertWidth(g.fx,AUnit,w).value;
    f.y:= Units.ConvertHeight(g.fy,AUnit,h).value;
    fr:= abs(Units.ConvertWidth(g.fr,AUnit,w).value);

    m := ACanvas2d.matrix;
    ACanvas2d.transform(g.gradientMatrix[AUnit]);
    CheckFocalAndCreate(c,r,f,fr);
    ACanvas2d.matrix := m;
  end;
end;

procedure TSVGElementWithGradient.InitializeGradient(ACanvas2d: TBGRACanvas2D;
  const origin: TPointF; const w,h: single; AUnit: TCSSUnit);
begin
  if GradientElement <> nil then
  begin
    if GradientElement is TSVGLinearGradient then
      CreateCanvasLinearGradient(ACanvas2d, GradientElement, origin, w,h, AUnit)
    else
    if GradientElement is TSVGRadialGradient then
      CreateCanvasRadialGradient(ACanvas2d, GradientElement, origin, w,h, AUnit);
  end;
end; 

procedure TSVGElementWithGradient.ApplyFillStyle(ACanvas2D: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  if FCanvasGradient = nil then
    inherited ApplyFillStyle(ACanvas2D,AUnit)
  else
  begin
    ACanvas2D.fillStyle(FCanvasGradient);
    ACanvas2D.fillMode:= TFillMode(fillMode);
  end;
end;  

{ TSVGText }

function TSVGText.GetFontBold: boolean;
var valueText: string;
begin
  valueText := trim(fontWeight);
  result := (valueText = 'bold') or (valueText = 'bolder') or
  (valueText = '600') or (valueText = '700') or (valueText = '800') or
   (valueText = '900');
end;

function TSVGText.GetFontFamily: string;
begin
  result := AttributeOrStyleDef['font-family','Arial'];
end;

function TSVGText.GetFontItalic: boolean;
var valueText: string;
begin
  valueText := trim(fontStyle);
  result := (valueText = 'oblique') or (valueText = 'italic');
end;

function TSVGText.GetFontSize: TFloatWithCSSUnit;
begin
  result:= VerticalAttributeOrStyleWithUnit['font-size',FloatWithCSSUnit(12,cuPoint)];
end;

function TSVGText.GetFontStyle: string;
begin
  result := AttributeOrStyleDef['font-style','normal'];
end;

function TSVGText.GetFontWeight: string;
begin
  result := AttributeOrStyleDef['font-weight','normal'];
end;

function TSVGText.GetSimpleText: string;
begin
  result := FDomElem.TextContent;
end;

function TSVGText.GetTextDecoration: string;
begin
  result := AttributeOrStyleDef['text-decoration','none'];
end;

function TSVGText.GetX: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['x'];
end;

function TSVGText.GetY: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['y'];
end;

procedure TSVGText.SetFontBold(AValue: boolean);
begin
  if AValue then fontWeight:= 'bold' else fontWeight:= 'normal';
end;

procedure TSVGText.SetFontFamily(AValue: string);
begin
  Attribute['font-family'] := AValue;
  RemoveStyle('font-family');
end;

procedure TSVGText.SetFontItalic(AValue: boolean);
begin
  if AValue then fontStyle:= 'italic' else fontStyle:= 'normal';
end;

procedure TSVGText.SetFontSize(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['font-size'] := AValue;
end;

procedure TSVGText.SetFontStyle(AValue: string);
begin
  Attribute['font-style'] := AValue;
  RemoveStyle('font-style');
end;

procedure TSVGText.SetFontWeight(AValue: string);
begin
  Attribute['font-weight'] := AValue;
  RemoveStyle('font-weight');
end;

procedure TSVGText.SetSimpleText(AValue: string);
begin
  FDomElem.TextContent := AValue;
end;

procedure TSVGText.SetTextDecoration(AValue: string);
begin
  Attribute['text-decoration'] := AValue;
  RemoveStyle('text-decoration');
end;

procedure TSVGText.SetX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['x'] := AValue;
end;

procedure TSVGText.SetY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['y'] := AValue;
end;

procedure TSVGText.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var 
  fs:TFontStyles;
  vx,vy: single;
begin
  ACanvas2d.beginPath;
  ACanvas2d.fontEmHeight := Units.ConvertHeight(fontSize,AUnit).value;
  ACanvas2d.fontName := fontFamily;
  fs := [];
  if fontBold then fs += [fsBold];
  if fontItalic then fs += [fsItalic];
  ACanvas2d.fontStyle := fs;
  vx:= Units.ConvertWidth(x,AUnit).value;
  vy:= Units.ConvertHeight(y,AUnit).value;
  ACanvas2d.text(SimpleText,vx,vy);

  if Assigned(GradientElement) then
    with ACanvas2d.measureText(SimpleText) do
      InitializeGradient(ACanvas2d, PointF(vx,vy),width,height,AUnit);
             
  if not isFillNone then
  begin
    ApplyFillStyle(ACanvas2D,AUnit);
    ACanvas2d.fill;
  end;
  if not isStrokeNone then
  begin
    ApplyStrokeStyle(ACanvas2D,AUnit);
    ACanvas2d.stroke;
  end;
end;

constructor TSVGText.Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  Init(ADocument,'text',AUnits);
end;

{ TSVGGroup }

constructor TSVGGroup.Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  FContent := TSVGContent.Create(ADocument,FDomElem,AUnits,ADataLink,Self);
end;

constructor TSVGGroup.Create(ADocument: TXMLDocument; AElement: TDOMElement;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AElement, AUnits, ADataLink);
  FContent := TSVGContent.Create(ADocument,AElement,AUnits,ADataLink,Self);
end;

destructor TSVGGroup.Destroy;
begin
  FreeAndNil(FContent);
  inherited Destroy;
end;

procedure TSVGGroup.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  FContent.Draw(ACanvas2d, AUnit);
end;

procedure TSVGGroup.Recompute;
begin
  inherited Recompute;
  FContent.Recompute;
end;

{ TSVGStyle }  

constructor TSVGStyle.Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  Init(ADocument,'style',AUnits);
end;

constructor TSVGStyle.Create(ADocument: TXMLDocument; AElement: TDOMElement;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AElement, AUnits, ADataLink);
  Parse(AElement.TextContent);
end;

procedure TSVGStyle.Initialize;
begin
  inherited Initialize;
  Clear;
end;

destructor TSVGStyle.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSVGStyle.Parse(const s: String);

  function IsValidAttribute(const sa: string): boolean;
  var
    i: integer;
  begin
    //(for case example "{ ; ;}")
    for i:= 1 to Length(sa) do
     if not (sa[i] in [' ',';']) then
      exit(true);
    result:= false;
  end;

const
  EmptyRec: TSVGStyleItem = (name: ''; attribute: '');
var
  i,l,pg: integer;
  st: String;
  rec: TSVGStyleItem;
begin
  (*
    Example of internal style block
    circle {..}         
    circle.type1 {..}   
    .pic1 {..}          
  *)
  Clear;
  l:= 0;
  pg:= 0;
  st:= '';
  rec:= EmptyRec;
  for i:= 1 to Length(s) do
  begin
    if s[i] = '{' then
    begin
      Inc(pg);
      if (pg = 1) and (Length(st) <> 0) then
      begin
       rec.name:= Trim(st);
       st:= '';
      end;
    end
    else if s[i] = '}' then
    begin
      Dec(pg);
      if (pg = 0) and (Length(st) <> 0) then
      begin
        if IsValidAttribute(st) then
        begin
          rec.attribute:= Trim(st);
          Inc(l);
          SetLength(FStyles,l);
          FStyles[l-1]:= rec;
          rec:= EmptyRec;
        end;
        st:= '';
      end;
    end
    else
      st:= st + s[i];
  end;
end;

function TSVGStyle.IsValidID(const sid: integer): boolean;
begin
  result:= (sid >= 0) and (sid < Length(FStyles));
end;

function TSVGStyle.GetStyle(const sid: integer): TSVGStyleItem;
begin
  if IsValidID(sid) then
    result:= FStyles[sid]
  else
    raise exception.Create(rsInvalidId);
end;

procedure TSVGStyle.SetStyle(const sid: integer; sr: TSVGStyleItem);
begin
  if IsValidID(sid) then
    FStyles[sid]:= sr
  else
    raise exception.Create(rsInvalidId);
end;

function TSVGStyle.Count: Integer;
begin
  result:= Length(FStyles);
end;

function TSVGStyle.Find(sr: TSVGStyleItem): integer;
var
  i: integer;
begin
  for i:= 0 to Length(FStyles)-1 do
    with FStyles[i] do
      if (name = sr.name) and
         (attribute = sr.attribute) then
      begin
        result:= i;
        Exit;
      end;
  result:= -1;
end;

function TSVGStyle.Find(const AName: string): integer;
var
  i: integer;
begin
  for i:= 0 to Length(FStyles)-1 do
    with FStyles[i] do
      if name = AName then
      begin
        result:= i;
        Exit;
      end;
  result:= -1;
end;

function TSVGStyle.Add(sr: TSVGStyleItem): integer;
var
  l: integer;
begin
  l:= Length(FStyles);
  SetLength(FStyles,l+1);
  FStyles[l]:= sr;
  result:= l;
end;

procedure TSVGStyle.Remove(sr: TSVGStyleItem);
var
  l,p: integer;
begin
  p:= Find(sr);
  l:= Length(FStyles);
  if p <> -1 then
  begin
    Finalize(FStyles[p]);
    System.Move(FStyles[p+1], FStyles[p], (l-p)*SizeOf(TSVGStyleItem));
    SetLength(FStyles,l-1);
  end;
end;

procedure TSVGStyle.Clear;
begin
  SetLength(FStyles,0);
end;

procedure TSVGStyle.ReParse;
begin
 Parse(FDomElem.TextContent);
end;           

{ TSVGRectangle }

function TSVGRectangle.GetX: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['x'];
end;

function TSVGRectangle.GetY: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['y'];
end;

function TSVGRectangle.GetWidth: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['width'];
end;

function TSVGRectangle.GetHeight: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['height'];
end;

function TSVGRectangle.GetRX: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['rx'];
end;

function TSVGRectangle.GetRY: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['ry'];
end;

procedure TSVGRectangle.SetX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['x'] := AValue;
end;

procedure TSVGRectangle.SetY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['y'] := AValue;
end;

procedure TSVGRectangle.SetWidth(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['width'] := AValue;
end;

procedure TSVGRectangle.SetHeight(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['height'] := AValue;
end;

procedure TSVGRectangle.SetRX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['rx'] := AValue;
end;

procedure TSVGRectangle.SetRY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['ry'] := AValue;
end;

constructor TSVGRectangle.Create(ADocument: TXMLDocument;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  Init(ADocument,'rect',AUnits);
end;

procedure TSVGRectangle.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var
  vx,vy,vw,vh: Single;
begin
  if not isStrokeNone or not isFillNone then
  begin
    vx:= Units.ConvertWidth(x,AUnit).value;
    vy:= Units.ConvertHeight(y,AUnit).value;
    vw:= Units.ConvertWidth(width,AUnit).value;
    vh:= Units.ConvertHeight(height,AUnit).value;
    ACanvas2d.beginPath;
    ACanvas2d.roundRect(vx,vy, vw,vh,
       Units.ConvertWidth(rx,AUnit).value,Units.ConvertHeight(ry,AUnit).value);
    if Assigned(GradientElement) then
      InitializeGradient(ACanvas2d, PointF(vx,vy),vw,vh,AUnit);
    if not isFillNone then
    begin
      ApplyFillStyle(ACanvas2D,AUnit);
      ACanvas2d.fill;
    end;
    if not isStrokeNone then
    begin
      ApplyStrokeStyle(ACanvas2D,AUnit);
      ACanvas2d.stroke;
    end;
  end;
end;

{ TSVGPolypoints }

function TSVGPolypoints.GetClosed: boolean;
begin
  result := FDomElem.TagName = 'polygon';
end;

function TSVGPolypoints.GetBoundingBoxF: TRectF;
begin
  if not FBoundingBoxComputed then
    ComputeBoundingBox(pointsF);
  result := FBoundingBox;
end;

function TSVGPolypoints.GetPoints: string;
begin
  result := Attribute['points'];
end;

function TSVGPolypoints.GetPointsF: ArrayOfTPointF;
var parser: TSVGParser;
  nbcoord,i: integer;
begin
  parser:=TSVGParser.Create(points);
  nbcoord := 0;
  repeat
    parser.ParseFloat;
    if not parser.NumberError then
      inc(nbcoord);
  until parser.NumberError or parser.Done;
  parser.ClearError;
  setlength(Result,nbcoord div 2);
  parser.Position := 1;
  for i := 0 to high(result) do
  begin
    result[i].x := parser.ParseFloat;
    result[i].y := parser.ParseFloat;
  end;
  parser.Free;
end;

procedure TSVGPolypoints.SetPoints(AValue: string);
begin
  Attribute['points'] := AValue;
end;

procedure TSVGPolypoints.SetPointsF(AValue: ArrayOfTPointF);
var s: string;
  i: integer;
begin
  s:= '';
  for i := 0 to high(AValue) do
  begin
    if s <> '' then s += ' ';
    with AValue[i] do
      s += TCSSUnitConverter.formatValue(x)+' '+TCSSUnitConverter.formatValue(y);
  end;
  points := s;
  ComputeBoundingBox(AValue);
end;

procedure TSVGPolypoints.ComputeBoundingBox(APoints: ArrayOfTPointF);
var
  i: Integer;
begin
  if length(APoints) > 1 then
  begin
    with APoints[0] do
      FBoundingBox:= RectF(x,y,x,y);
    for i:= 1 to high(APoints) do
      with APoints[i] do
      begin
        if x < FBoundingBox.Left then
         FBoundingBox.Left:= x
        else if x > FBoundingBox.Right then
         FBoundingBox.Right:= x;
        if y < FBoundingBox.Top then
         FBoundingBox.Top:= y
        else if y > FBoundingBox.Bottom then
         FBoundingBox.Bottom:= y;
      end;
    FBoundingBoxComputed := true;
  end else
  begin
    FBoundingBox := RectF(0,0,0,0);
    FBoundingBoxComputed := true;
  end;
end;

constructor TSVGPolypoints.Create(ADocument: TXMLDocument;
  AUnits: TCSSUnitConverter; AClosed: boolean; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  if AClosed then
    Init(ADocument, 'polygon', AUnits)
  else
    Init(ADocument, 'polyline', AUnits);
end;

destructor TSVGPolypoints.Destroy;
begin
  inherited Destroy;
end;

procedure TSVGPolypoints.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var
  prevMatrix: TAffineMatrix;
  pts: ArrayOfTPointF;
begin
  if isFillNone and isStrokeNone then exit;
  if AUnit <> cuCustom then
  begin
    prevMatrix := ACanvas2d.matrix;
    ACanvas2d.scale(Units.ConvertWidth(1,cuCustom,AUnit),
      Units.ConvertHeight(1,cuCustom,AUnit));
    InternalDraw(ACanvas2d, cuCustom);
    ACanvas2d.matrix:= prevMatrix;
  end else
  begin
    ACanvas2d.beginPath;
    pts := pointsF;
    ACanvas2d.polylineTo(pts);
    if closed then ACanvas2d.closePath;
    
    with boundingBoxF do
      InitializeGradient(ACanvas2d,
        PointF(Left,Top),abs(Right-Left),abs(Bottom-Top),AUnit);
    
    if not isFillNone then
    begin
      ApplyFillStyle(ACanvas2D,AUnit);
      ACanvas2d.fill;
    end;
    if not isStrokeNone then
    begin
      ApplyStrokeStyle(ACanvas2D,AUnit);
      ACanvas2d.stroke;
    end;
  end;
end;

{ TSVGPath }

function TSVGPath.GetPathLength: TFloatWithCSSUnit;
begin
  result := OrthoAttributeWithUnit['pathLength'];
end;

function TSVGPath.GetPath: TBGRAPath;
begin
  if FPath = nil then
    FPath := TBGRAPath.Create(Attribute['d']);
  result := FPath;
end;

function TSVGPath.GetBoundingBoxF: TRectF;
begin
  if not FBoundingBoxComputed then
  begin
    FBoundingBox := path.GetBounds;
    FBoundingBoxComputed := true;
  end;
  result := FBoundingBox;
end;

function TSVGPath.GetData: string;
begin
  if FPath = nil then
    result := Attribute['d']
  else
    result := FPath.SvgString;
end;

procedure TSVGPath.SetPathLength(AValue: TFloatWithCSSUnit);
begin
  OrthoAttributeWithUnit['pathLength'] := AValue;
end;

procedure TSVGPath.SetData(AValue: string);
begin
  if FPath = nil then
    Attribute['d'] := AValue
  else
    FPath.SvgString := AValue;
  FBoundingBoxComputed := false;
end;

function TSVGPath.GetDOMElement: TDOMElement;
begin
  if FPath <> nil then Attribute['d'] := FPath.SvgString;
  Result:=inherited GetDOMElement;
end;

constructor TSVGPath.Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  Init(ADocument,'path',AUnits);
  FPath := nil;
  FBoundingBoxComputed := false;
  FBoundingBox := rectF(0,0,0,0);
end;

constructor TSVGPath.Create(ADocument: TXMLDocument; AElement: TDOMElement;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AElement, AUnits, ADataLink);
  Init(ADocument, AElement, AUnits);
  FPath := nil;
  FBoundingBoxComputed := false;
  FBoundingBox := rectF(0,0,0,0);
end;

destructor TSVGPath.Destroy;
begin
  FreeAndNil(FPath);
  inherited Destroy;
end;

procedure TSVGPath.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var
  prevMatrix: TAffineMatrix;
begin
  if isFillNone and isStrokeNone then exit;
  if AUnit <> cuCustom then
  begin
    prevMatrix := ACanvas2d.matrix;
    ACanvas2d.scale(Units.ConvertWidth(1,cuCustom,AUnit),
      Units.ConvertHeight(1,cuCustom,AUnit));
    InternalDraw(ACanvas2d, cuCustom);
    ACanvas2d.matrix:= prevMatrix;
  end else
  begin
    ACanvas2d.path(path);
    if Assigned(GradientElement) then
      with boundingBoxF do
        InitializeGradient(ACanvas2d,
          PointF(Left,Top),abs(Right-Left),abs(Bottom-Top),AUnit);
    if not isFillNone then
    begin
      ApplyFillStyle(ACanvas2D,AUnit);
      ACanvas2d.fill;
    end;
    if not isStrokeNone then
    begin
      ApplyStrokeStyle(ACanvas2D,AUnit);
      ACanvas2d.stroke;
    end;
  end;
end;

{ TSVGEllipse }

function TSVGEllipse.GetCX: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['cx'];
end;

function TSVGEllipse.GetCY: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['cy'];
end;

function TSVGEllipse.GetRX: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['rx'];
end;

function TSVGEllipse.GetRY: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['ry'];
end;

procedure TSVGEllipse.SetCX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['cx'] := AValue;
end;

procedure TSVGEllipse.SetCY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['cy'] := AValue;
end;

procedure TSVGEllipse.SetRX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['rx'] := AValue;
end;

procedure TSVGEllipse.SetRY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['ry'] := AValue;
end;

constructor TSVGEllipse.Create(ADocument: TXMLDocument;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  Init(ADocument,'ellipse',AUnits);
end;

procedure TSVGEllipse.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var
  vcx,vcy,vrx,vry: Single;
begin
  if not isFillNone or not isStrokeNone then
  begin
    vcx:= Units.ConvertWidth(cx,AUnit).value;
    vcy:= Units.ConvertHeight(cy,AUnit).value;
    vrx:= Units.ConvertWidth(rx,AUnit).value;
    vry:= Units.ConvertHeight(ry,AUnit).value;
    ACanvas2d.beginPath;
    ACanvas2d.ellipse(vcx,vcy,vrx,vry);
    if Assigned(GradientElement) then
      InitializeGradient(ACanvas2d, PointF(vcx-vrx,vcy-vry),vrx*2,vry*2,AUnit);      
    if not isFillNone then
    begin
      ApplyFillStyle(ACanvas2D,AUnit);
      ACanvas2d.fill;
    end;
    if not isStrokeNone then
    begin
      ApplyStrokeStyle(ACanvas2D,AUnit);
      ACanvas2d.stroke;
    end;
  end;
end;

{ TSVGCircle }

function TSVGCircle.GetCX: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['cx'];
end;

function TSVGCircle.GetCY: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['cy'];
end;

function TSVGCircle.GetR: TFloatWithCSSUnit;
begin
  result := OrthoAttributeWithUnit['r'];
end;

procedure TSVGCircle.SetCX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['cx'] := AValue;
end;

procedure TSVGCircle.SetCY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['cy'] := AValue;
end;

procedure TSVGCircle.SetR(AValue: TFloatWithCSSUnit);
begin
  OrthoAttributeWithUnit['r'] := AValue;
end;

constructor TSVGCircle.Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink); 
  Init(ADocument,'circle',AUnits);
end;

procedure TSVGCircle.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var
  vcx,vcy,vr: Single;
begin
  if not isFillNone or not isStrokeNone then
  begin
    vcx:= Units.ConvertWidth(cx,AUnit).value;
    vcy:= Units.ConvertHeight(cy,AUnit).value;
    vr:= Units.ConvertWidth(r,AUnit).value;
    ACanvas2d.beginPath;
    ACanvas2d.circle(vcx,vcy,vr);
    if Assigned(GradientElement) then
      InitializeGradient(ACanvas2d, PointF(vcx-vr,vcy-vr),vr*2,vr*2,AUnit);
    if not isFillNone then
    begin
      ApplyFillStyle(ACanvas2D,AUnit);
      ACanvas2d.fill;
    end;
    if not isStrokeNone then
    begin
      ApplyStrokeStyle(ACanvas2D,AUnit);
      ACanvas2d.stroke;
    end;
  end;
end;

{ TSVGLine }

function TSVGLine.GetX1: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['x1'];
end;

function TSVGLine.GetX2: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['x2'];
end;

function TSVGLine.GetY1: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['y1'];
end;

function TSVGLine.GetY2: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['y2'];
end;

procedure TSVGLine.SetX1(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['x1'] := AValue;
end;

procedure TSVGLine.SetX2(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['x2'] := AValue;
end;

procedure TSVGLine.SetY1(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['y1'] := AValue;
end;

procedure TSVGLine.SetY2(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['y2'] := AValue;
end;

constructor TSVGLine.Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  Init(ADocument,'line',AUnits);
end;

procedure TSVGLine.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  if not isStrokeNone then
  begin
    ApplyStrokeStyle(ACanvas2D,AUnit);
    ACanvas2d.beginPath;
    ACanvas2d.moveTo(Units.ConvertWidth(x1,AUnit).value,Units.ConvertHeight(y1,AUnit).value);
    ACanvas2d.lineTo(Units.ConvertWidth(x2,AUnit).value,Units.ConvertHeight(y2,AUnit).value);
    ACanvas2d.stroke;
  end;
end;

{ TSVGGradient } //##

function TSVGGradient.GetHRef: string;
begin
  result := Attribute['xlink:href'];
  if result = '' then
    result := Attribute['href'];//(Note: specific for svg 2)
end;

function TSVGGradient.GetUseObjectBoundingBox: boolean;
begin
  result := (gradientUnits = 'objectBoundingBox');
end;

procedure TSVGGradient.SetGradientTransform(AValue: string);
begin
  Attribute['gradientTransform'] := AValue;
end;

function TSVGGradient.GetGradientUnits: string;
begin
  result := AttributeDef['gradientUnits','objectBoundingBox'];
end;

function TSVGGradient.GetGradientTransform: string;
begin
  result := Attribute['gradientTransform'];
end;

function TSVGGradient.GetGradientMatrix(AUnit: TCSSUnit): TAffineMatrix;
var parser: TSVGParser;
  s: string;
begin
  s := gradientTransform;
  if s = '' then
  begin
    result := AffineMatrixIdentity;
    exit;
  end;
  parser := TSVGParser.Create(s);
  result := parser.ParseTransform;
  parser.Free;
  result[1,3] := Units.ConvertWidth(result[1,3],cuCustom,AUnit);
  result[2,3] := Units.ConvertHeight(result[2,3],cuCustom,AUnit);
end;

procedure TSVGGradient.SetGradientUnits(AValue: string);
begin
  Attribute['gradientUnits'] := AValue;
end;

procedure TSVGGradient.SetHRef(AValue: string);
begin
  Attribute['xlink:href'] := AValue;
end;

constructor TSVGGradient.Create(ADocument: TXMLDocument;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  FContent := TSVGContent.Create(ADocument,FDomElem,AUnits,ADataLink,Self);
end;

function TSVGGradient.HRefToGradientID(const AValue: string): string;
var
  l: integer;
begin
  //(example input: "#gradient1")
  l:= Length(AValue);
  if l < 2 then
    result:= ''
  else
    result:= System.Copy(AValue,2,l-1);
end;

function TSVGGradient.FindGradientRef(const AGradientID: string): integer;
var
  i: integer;
begin
  with FDataLink do
    for i:= 0 to GradientCount-1 do
      if (Gradients[i] as TSVGGradient).ID = AGradientID then
      begin
        result:= i;
        exit;
      end;
  result:= -1;
end;

procedure TSVGGradient.Initialize;
begin
  inherited;
  InheritedGradients:= TSVGElementList.Create;
end;

function TSVGGradient.GetInheritedAttribute(AValue: string;
  AConvMethod: TConvMethod; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
var
  i: integer;
  el: TSVGGradient;
  invalidDef: TFloatWithCSSUnit;
begin
  invalidDef:= FloatWithCSSUnit(EmptySingle,cuPercent);
  //find valid inherited attribute (start from "self": item[0])
  for i:= 0 to InheritedGradients.Count-1 do
  begin
    el:= TSVGGradient( InheritedGradients[i] );
    with el do
    begin
      if AConvMethod = cmHoriz then
        result:= HorizAttributeWithUnitDef[AValue,invalidDef]
      else if AConvMethod = cmVertical then
        result:= VerticalAttributeWithUnitDef[AValue,invalidDef]
      else if AConvMethod = cmOrtho then
        result:= OrthoAttributeWithUnitDef[AValue,invalidDef]
      else
        result:= AttributeWithUnitDef[AValue,invalidDef];

      if (result.value <> invalidDef.value) or
         (result.CSSUnit <> invalidDef.CSSUnit) then
        exit;
    end;
  end;
  result:= ADefault;
end;

constructor TSVGGradient.Create(ADocument: TXMLDocument; AElement: TDOMElement;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AElement, AUnits, ADataLink);
  FContent := TSVGContent.Create(ADocument,AElement,AUnits,ADataLink,Self);
end;

destructor TSVGGradient.Destroy;
begin
  FreeAndNil(FContent);
  FreeAndNil(InheritedGradients);
  inherited Destroy;
end;

procedure TSVGGradient.Recompute;
begin
  inherited Recompute;
  FContent.Recompute;
end;

procedure TSVGGradient.ScanInheritedGradients(const forceScan: boolean = false);
var
  el: TSVGGradient;
  pos: integer;
  gradientID: string;
begin
  //(if list empty = not scan)
  if (InheritedGradients.Count <> 0) and (not forceScan) then
    exit;

  InheritedGradients.Clear;
  InheritedGradients.Add(Self);//(important)
  el:= Self;
  while el.hRef <> '' do
  begin
    gradientID:= HRefToGradientID(el.hRef);
    pos:= FindGradientRef(gradientID);
    if pos = -1 then
      exit
    else
    begin
      el:= TSVGGradient(FDataLink.Gradients[pos]);
      InheritedGradients.Add(el);
    end;
  end;
end;        

{ TSVGLinearGradient }

function TSVGLinearGradient.GetX1: TFloatWithCSSUnit;
begin
  result := GetInheritedAttribute('x1',cmNone,FloatWithCSSUnit(0,cuPercent));
end;

function TSVGLinearGradient.GetX2: TFloatWithCSSUnit;
begin
  result := GetInheritedAttribute('x2',cmNone,FloatWithCSSUnit(100,cuPercent));
end;

function TSVGLinearGradient.GetY1: TFloatWithCSSUnit;
begin
  result := GetInheritedAttribute('y1',cmNone,FloatWithCSSUnit(0,cuPercent));
end;

function TSVGLinearGradient.GetY2: TFloatWithCSSUnit;
begin
  result := GetInheritedAttribute('y2',cmNone,FloatWithCSSUnit(0,cuPercent));
end; 

procedure TSVGLinearGradient.SetX1(AValue: TFloatWithCSSUnit);
begin
  AttributeWithUnit['x1']:= AValue;
end;

procedure TSVGLinearGradient.SetX2(AValue: TFloatWithCSSUnit);
begin
  AttributeWithUnit['x2']:= AValue;
end;

procedure TSVGLinearGradient.SetY1(AValue: TFloatWithCSSUnit);
begin
  AttributeWithUnit['y1']:= AValue;
end;

procedure TSVGLinearGradient.SetY2(AValue: TFloatWithCSSUnit);
begin
  AttributeWithUnit['y2']:= AValue;
end;

constructor TSVGLinearGradient.Create(ADocument: TXMLDocument;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  Init(ADocument,'linearGradient',AUnits);
end;

{ TSVGRadialGradient }

function TSVGRadialGradient.GetCX: TFloatWithCSSUnit;
begin
  result := GetInheritedAttribute('cx',cmHoriz,FloatWithCSSUnit(50,cuPercent));
end;

function TSVGRadialGradient.GetCY: TFloatWithCSSUnit;
begin
  result := GetInheritedAttribute('cy',cmVertical,FloatWithCSSUnit(50,cuPercent));
end;

function TSVGRadialGradient.GetR: TFloatWithCSSUnit;
begin
  result := GetInheritedAttribute('r',cmOrtho,FloatWithCSSUnit(50,cuPercent));
end;

function TSVGRadialGradient.GetFX: TFloatWithCSSUnit;
begin
  result := GetInheritedAttribute('fx',cmHoriz,cx);
end;

function TSVGRadialGradient.GetFY: TFloatWithCSSUnit;
begin
  result := GetInheritedAttribute('fy',cmVertical,cy);
end;         

function TSVGRadialGradient.GetFR: TFloatWithCSSUnit;
begin
  result := GetInheritedAttribute('fr',cmHoriz,FloatWithCSSUnit(0,cuPercent));
end;

procedure TSVGRadialGradient.SetCX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['cx'] := AValue;
end;

procedure TSVGRadialGradient.SetCY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['cy'] := AValue;
end;

procedure TSVGRadialGradient.SetR(AValue: TFloatWithCSSUnit);
begin
  OrthoAttributeWithUnit['r'] := AValue;
end;

procedure TSVGRadialGradient.SetFX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['fx'] := AValue;
end;

procedure TSVGRadialGradient.SetFY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['fy'] := AValue;
end;

procedure TSVGRadialGradient.SetFR(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['fr'] := AValue;
end;

constructor TSVGRadialGradient.Create(ADocument: TXMLDocument;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  Init(ADocument,'radialGradient',AUnits);
end;

{ TSVGStopGradient }

function TSVGStopGradient.GetOffset: TFloatWithCSSUnit;
begin
  result := AttributeWithUnit['offset'];
end;

procedure TSVGStopGradient.SetOffset(AValue: TFloatWithCSSUnit);
begin
  AttributeWithUnit['offset'] := AValue;
end; 

constructor TSVGStopGradient.Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter;
  ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  Init(ADocument,'stop',AUnits);
end;

{ TSVGDefine }

constructor TSVGDefine.Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter;
  ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  FContent := TSVGContent.Create(ADocument,FDomElem,AUnits,ADataLink,Self);
end;

constructor TSVGDefine.Create(ADocument: TXMLDocument; AElement: TDOMElement;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AElement, AUnits, ADataLink);
  FContent := TSVGContent.Create(ADocument,AElement,AUnits,ADataLink,Self);
end;

destructor TSVGDefine.Destroy;
begin
  FreeAndNil(FContent);
  inherited Destroy;
end;

procedure TSVGDefine.Recompute;
begin
  inherited Recompute;
  FContent.Recompute;
end;

{ TSVGContent }

function TSVGContent.GetElement(AIndex: integer): TSVGElement;
begin
  result := TSVGElement(FElements.Items[AIndex]);
end;

function TSVGContent.GetElementCount: integer;
begin
  result := FElements.Count;
end;

function TSVGContent.GetUnits: TCSSUnitConverter;
begin
  result := FUnits;
end;

procedure TSVGContent.AppendElement(AElement: TSVGElement);
begin
  FDomElem.AppendChild(AElement.DOMElement);
  FElements.Add(AElement);
end;

procedure TSVGContent.InsertElementBefore(AElement: TSVGElement;
  ASuccessor: TSVGElement);
var idx: integer;
begin
  idx := FElements.IndexOf(ASuccessor);
  if idx <> -1 then
  begin
    FElements.Insert(idx,AElement);
    FDomElem.InsertBefore(AElement.DOMElement, ASuccessor.DOMElement);
  end
  else
  begin
    FElements.Add(AElement);
    FDomElem.AppendChild(ASuccessor.DOMElement);
  end;
end;

constructor TSVGContent.Create(ADocument: TXMLDocument; AElement: TDOMElement;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink; ADataParent: TSVGElement);
var cur: TDOMNode;
begin
  FDoc := ADocument;
  FDomElem := AElement;
  FDataLink := ADataLink;
  FElements := TFPList.Create;
  FUnits := AUnits;
  cur := FDomElem.FirstChild;
  while cur <> nil do
  begin
    if cur is TDOMElement then
      FElements.Add(CreateSVGElementFromNode(
        ADocument,TDOMElement(cur),FUnits,ADataLink,ADataParent));
    cur := cur.NextSibling;
  end;
end;

destructor TSVGContent.Destroy;
var i:integer;
begin
  for i := 0 to ElementCount-1 do
    Element[i].free;
  FreeAndNil(FElements);
  inherited Destroy;
end;

procedure TSVGContent.Recompute;
var
  i: Integer;
begin
  for i := 0 to ElementCount-1 do
    Element[i].Recompute;
end;

procedure TSVGContent.Draw(ACanvas2d: TBGRACanvas2D; x, y: single; AUnit: TCSSUnit);
var prevMatrix: TAffineMatrix;
begin
  if (x<>0) or (y<>0) then
  begin
    prevMatrix := ACanvas2d.matrix;
    ACanvas2d.translate(x,y);
    Draw(ACanvas2d, AUnit);
    ACanvas2d.matrix := prevMatrix;
  end else
    Draw(ACanvas2d, AUnit);
end;

procedure TSVGContent.Draw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var i: integer;
begin
  for i := 0 to ElementCount-1 do
    Element[i].Draw(ACanvas2d, AUnit);
end;

function TSVGContent.AppendLine(x1, y1, x2, y2: single; AUnit: TCSSUnit
  ): TSVGLine;
begin
  result := TSVGLine.Create(FDoc,Units,FDataLink);
  result.x1 := FloatWithCSSUnit(x1,AUnit);
  result.y1 := FloatWithCSSUnit(y1,AUnit);
  result.x2 := FloatWithCSSUnit(x2,AUnit);
  result.y2 := FloatWithCSSUnit(y2,AUnit);
  AppendElement(result);
end;

function TSVGContent.AppendLine(p1, p2: TPointF; AUnit: TCSSUnit): TSVGLine;
begin
  result := AppendLine(p1.x,p1.y,p2.x,p2.y,AUnit);
end;

function TSVGContent.AppendCircle(cx, cy, r: single; AUnit: TCSSUnit
  ): TSVGCircle;
begin
  if (AUnit <> cuCustom) and (Units.DpiScaleX <> Units.DpiScaleY) then
  begin
    result := TSVGCircle.Create(FDoc,Units,FDataLink);
    result.cx := FloatWithCSSUnit(Units.Convert(cx,AUnit,cuCustom,Units.DpiX),cuCustom);
    result.cy := FloatWithCSSUnit(Units.Convert(cy,AUnit,cuCustom,Units.DpiY),cuCustom);
    result.r := FloatWithCSSUnit(Units.Convert(r,AUnit,cuCustom,Units.DpiX),cuCustom);
    result.transform:= Units.DpiScaleTransform;
    AppendElement(result);
  end else
  begin
    result := TSVGCircle.Create(FDoc,Units,FDataLink);
    result.cx := FloatWithCSSUnit(cx,AUnit);
    result.cy := FloatWithCSSUnit(cy,AUnit);
    result.r := FloatWithCSSUnit(r,AUnit);
    AppendElement(result);
  end;
end;

function TSVGContent.AppendCircle(c: TPointF; r: single; AUnit: TCSSUnit
  ): TSVGCircle;
begin
  result := AppendCircle(c.x,c.y,r,AUnit);
end;

function TSVGContent.AppendEllipse(cx, cy, rx, ry: single; AUnit: TCSSUnit
  ): TSVGEllipse;
begin
  result := TSVGEllipse.Create(FDoc,Units,FDataLink);
  result.cx := FloatWithCSSUnit(cx,AUnit);
  result.cy := FloatWithCSSUnit(cy,AUnit);
  result.rx := FloatWithCSSUnit(rx,AUnit);
  result.ry := FloatWithCSSUnit(ry,AUnit);
  AppendElement(result);
end;

function TSVGContent.AppendEllipse(c, r: TPointF; AUnit: TCSSUnit): TSVGEllipse;
begin
  result := AppendEllipse(c.x,c.y,r.x,r.y,AUnit);
end;

function TSVGContent.AppendPath(data: string; AUnit: TCSSUnit): TSVGPath;
var tempPath: TBGRAPath;
begin
  if AUnit <> cuCustom then
  begin
    tempPath := TBGRAPath.Create(data);
    result := AppendPath(tempPath, AUnit);
    tempPath.Free;
  end else
  begin
    result := TSVGPath.Create(FDoc,Units,FDataLink);
    result.d := data;
    AppendElement(result);
  end;
end;

function TSVGContent.AppendPath(path: TBGRAPath; AUnit: TCSSUnit): TSVGPath;
begin
  if (AUnit <> cuCustom) and (Units.DpiScaleX <> Units.DpiScaleY) then
  begin
    result := TSVGPath.Create(FDoc,Units,FDataLink);
    result.path.scale(Units.Convert(1,AUnit,cuCustom,Units.DpiX));
    path.copyTo(result.path);
    result.transform := Units.DpiScaleTransform;
    AppendElement(result);
  end else
  begin
    result := TSVGPath.Create(FDoc,Units,FDataLink);
    result.path.scale(Units.ConvertWidth(1,AUnit,cuCustom));
    path.copyTo(result.path);
    AppendElement(result);
  end;
end;

function TSVGContent.AppendPolygon(const points: array of single;
  AUnit: TCSSUnit): TSVGPolypoints;
var
  pts: ArrayOfTPointF;
  i: integer;
begin
  result := TSVGPolypoints.Create(FDoc,FUnits,true,FDataLink);
  setlength(pts, length(points) div 2);
  for i := 0 to high(pts) do
    pts[i] := Units.ConvertCoord(PointF(points[i shl 1],points[(i shl 1)+1]),AUnit,cuCustom);
  result.pointsF := pts;
  AppendElement(result);
end;

function TSVGContent.AppendPolygon(const points: array of TPointF;
  AUnit: TCSSUnit): TSVGPolypoints;
var
  pts: ArrayOfTPointF;
  i: integer;
begin
  result := TSVGPolypoints.Create(FDoc,FUnits,true,FDataLink);
  setlength(pts, length(points));
  for i := 0 to high(pts) do
    pts[i] := Units.ConvertCoord(points[i],AUnit,cuCustom);
  result.pointsF := pts;
  AppendElement(result);
end;

function TSVGContent.AppendRect(x, y, width, height: single; AUnit: TCSSUnit
  ): TSVGRectangle;
begin
  result := TSVGRectangle.Create(FDoc,Units,FDataLink);
  result.x := FloatWithCSSUnit(x,AUnit);
  result.y := FloatWithCSSUnit(y,AUnit);
  result.width := FloatWithCSSUnit(width,AUnit);
  result.height := FloatWithCSSUnit(height,AUnit);
  AppendElement(result);
end;

function TSVGContent.AppendRect(origin, size: TPointF; AUnit: TCSSUnit
  ): TSVGRectangle;
begin
  result := AppendRect(origin.x,origin.y,size.x,size.y,AUnit);
end;

function TSVGContent.AppendText(x, y: single; AText: string; AUnit: TCSSUnit
  ): TSVGText;
begin
  result := TSVGText.Create(FDoc,Units,FDataLink);
  result.x := FloatWithCSSUnit(x,AUnit);
  result.y := FloatWithCSSUnit(y,AUnit);
  result.SimpleText:= AText;
  AppendElement(result);
end;

function TSVGContent.AppendText(origin: TPointF; AText: string; AUnit: TCSSUnit
  ): TSVGText;
begin
  result := AppendText(origin.x,origin.y,AText,AUnit);
end;

function TSVGContent.AppendRoundRect(x, y, width, height, rx, ry: single;
  AUnit: TCSSUnit): TSVGRectangle;
begin
  result := TSVGRectangle.Create(FDoc,Units,FDataLink);
  result.x := FloatWithCSSUnit(x,AUnit);
  result.y := FloatWithCSSUnit(y,AUnit);
  result.width := FloatWithCSSUnit(width,AUnit);
  result.height := FloatWithCSSUnit(height,AUnit);
  result.rx := FloatWithCSSUnit(rx,AUnit);
  result.ry := FloatWithCSSUnit(ry,AUnit);
  AppendElement(result);
end;

function TSVGContent.AppendRoundRect(origin, size, radius: TPointF;
  AUnit: TCSSUnit): TSVGRectangle;
begin
  result := AppendRoundRect(origin.x,origin.y,size.x,size.y,radius.x,radius.y,AUnit);
end;

end.

