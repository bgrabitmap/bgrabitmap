unit BGRASVGShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAUnits, laz2_DOM, BGRAPath, BGRABitmapTypes,
  BGRACanvas2D, BGRASVGType;

type
  TSVGContent = class;

  { TSVGElementWithContent }

  TSVGElementWithContent = class(TSVGElement)
  protected
    FContent: TSVGContent;
  public
    constructor Create(ADocument: TDOMDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
    constructor Create(AElement: TDOMElement; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
    destructor Destroy; override;
    procedure Recompute; override;
    property Content: TSVGContent read FContent;
  end;

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
      class function GetDOMTag: string; override;
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
      class function GetDOMTag: string; override;
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
      class function GetDOMTag: string; override;
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
      class function GetDOMTag: string; override;
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
      class function GetDOMTag: string; override;
      constructor Create(ADocument: TDOMDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
      constructor Create(AElement: TDOMElement; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
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
      constructor Create(ADocument: TDOMDocument; AUnits: TCSSUnitConverter; AClosed: boolean; ADataLink: TSVGDataLink); overload;
      destructor Destroy; override;
      property points: string read GetPoints write SetPoints;
      property pointsF: ArrayOfTPointF read GetPointsF write SetPointsF;
      property closed: boolean read GetClosed;
      property boundingBoxF: TRectF read GetBoundingBoxF;
  end;

  { TSVGTextElement }

  TSVGTextElement = class(TSVGElementWithGradient);

  { TSVGTextElementWithContent }

  TSVGTextElementWithContent = class(TSVGTextElement)
    protected
      FContent: TSVGContent;
    public
      constructor Create(ADocument: TDOMDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
      constructor Create(AElement: TDOMElement; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); override;
      destructor Destroy; override;
      property Content: TSVGContent read FContent;
  end;

  { TSVGTextPositioning }

  TSVGTextPositioning = class(TSVGTextElementWithContent)
    private
      function GetX: ArrayOfTFloatWithCSSUnit;
      function GetY: ArrayOfTFloatWithCSSUnit;
      function GetDx: ArrayOfTFloatWithCSSUnit;
      function GetDy: ArrayOfTFloatWithCSSUnit;
      function GetRotate: ArrayOfTSVGNumber;
      procedure SetX(AValue: ArrayOfTFloatWithCSSUnit);
      procedure SetY(AValue: ArrayOfTFloatWithCSSUnit);
      procedure SetDx(AValue: ArrayOfTFloatWithCSSUnit);
      procedure SetDy(AValue: ArrayOfTFloatWithCSSUnit);
      procedure SetRotate(AValue: ArrayOfTSVGNumber);
    public
      property x: ArrayOfTFloatWithCSSUnit read GetX write SetX;
      property y: ArrayOfTFloatWithCSSUnit read GetY write SetY;
      property dX: ArrayOfTFloatWithCSSUnit read GetDx write SetDx;
      property dY: ArrayOfTFloatWithCSSUnit read GetDy write SetDy;
      property rotate: ArrayOfTSVGNumber read GetRotate write SetRotate;
  end;

  { TSVGTRef }

  TSVGTRef = class(TSVGTextElement)
    private
      function GetXlinkHref: string;
      procedure SetXlinkHref(AValue: string);
    public
      class function GetDOMTag: string; override;
      property xlinkHref: string read GetXlinkHref write SetXlinkHref;
  end;

  ArrayOfTextParts = array of record
    Level: integer;
    BaseElement: TSVGElement;
    Text: string;
    SplitPos: integer;
    AbsoluteCoord: TPointF;
    PartStartCoord, PartEndCoord: TPointF;
    Bounds: TRectF;
  end;

  { TSVGText }

  TSVGText = class(TSVGTextPositioning)
    private
      FInGetSimpleText: boolean;
      function GetFontBold: boolean;
      function GetFontFamily: string;
      function GetFontItalic: boolean;
      function GetFontSize: TFloatWithCSSUnit;
      function GetFontStyle: string;
      function GetFontWeight: string;
      function GetSimpleText: string;
      function GetTextAnchor: TSVGTextAnchor;
      function GetTextDirection: TSVGTextDirection;
      function GetTextDecoration: string;
      function GetTextLength: TFloatWithCSSUnit;
      function GetLengthAdjust: TSVGLengthAdjust;
      procedure SetFontBold(AValue: boolean);
      procedure SetFontFamily(AValue: string);
      procedure SetFontItalic(AValue: boolean);
      procedure SetFontSize(AValue: TFloatWithCSSUnit);
      procedure SetFontStyle(AValue: string);
      procedure SetFontWeight(AValue: string);
      procedure SetSimpleText(AValue: string);
      procedure SetTextAnchor(AValue: TSVGTextAnchor);
      procedure SetTextDirection(AValue: TSVGTextDirection);
      procedure SetTextDecoration(AValue: string);
      procedure SetTextLength(AValue: TFloatWithCSSUnit);
      procedure SetLengthAdjust(AValue: TSVGLengthAdjust);
    protected
      procedure InternalDrawOrCompute(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit;
                                      ADraw: boolean; AAllTextBounds: TRectF;
                                      var APosition: TPointF;
                                      var ATextParts: ArrayOfTextParts); overload;
      procedure InternalDrawOrCompute(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit;
                                      ADraw: boolean; AAllTextBounds: TRectF;
                                      var APosition: TPointF;
                                      var ATextParts: ArrayOfTextParts; ALevel: integer;
                                      AStartPart, AEndPart: integer); overload;
      procedure InternalDrawOrComputePart(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit;
                                      AText: string; ADraw: boolean; AAllTextBounds: TRectF;
                                      var APosition: TPointF; out ABounds: TRectF);
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
      procedure CleanText(var ATextParts: ArrayOfTextParts);
      function GetTRefContent(AElement: TSVGTRef): string;
      function GetAllText: ArrayOfTextParts;
    public
      class function GetDOMTag: string; override;
      property textLength: TFloatWithCSSUnit read GetTextLength write SetTextLength;
      property lengthAdjust: TSVGLengthAdjust read GetLengthAdjust write SetLengthAdjust;
      property SimpleText: string read GetSimpleText write SetSimpleText;
      property fontSize: TFloatWithCSSUnit read GetFontSize write SetFontSize;
      property fontFamily: string read GetFontFamily write SetFontFamily;
      property fontWeight: string read GetFontWeight write SetFontWeight;
      property fontStyle: string read GetFontStyle write SetFontStyle;
      property textDecoration: string read GetTextDecoration write SetTextDecoration;
      property fontBold: boolean read GetFontBold write SetFontBold;
      property fontItalic: boolean read GetFontItalic write SetFontItalic;
      property textAnchor: TSVGTextAnchor read GetTextAnchor write SetTextAnchor;
      property textDirection: TSVGTextDirection read GetTextDirection write SetTextDirection;
  end;

  { TSVGTSpan }

  TSVGTSpan = class(TSVGText)
    public
      class function GetDOMTag: string; override;
  end;

  { TSVGTextPath }

  TSVGTextPath = class(TSVGTextElementWithContent)
    private
      function GetStartOffset: TFloatWithCSSUnit;
      function GetMethod: TSVGTextPathMethod;
      function GetSpacing: TSVGTextPathSpacing;
      function GetXlinkHref: string;
      procedure SetStartOffset(AValue: TFloatWithCSSUnit);
      procedure SetMethod(AValue: TSVGTextPathMethod);
      procedure SetSpacing(AValue: TSVGTextPathSpacing);
      procedure SetXlinkHref(AValue: string);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      property startOffset: TFloatWithCSSUnit read GetStartOffset write SetStartOffset;
      property method: TSVGTextPathMethod read GetMethod write SetMethod;
      property spacing: TSVGTextPathSpacing read GetSpacing write SetSpacing;
      property xlinkHref: string read GetXlinkHref write SetXlinkHref;
  end;

  { TSVGAltGlyph }

  TSVGAltGlyph = class(TSVGTextElementWithContent)
    private
      function GetGlyphRef: string;
      function GetFormat: string;
      function GetXlinkHref: string;
      procedure SetGlyphRef(AValue: string);
      procedure SetFormat(AValue: string);
      procedure SetXlinkHref(AValue: string);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      property glyphRef: string read GetGlyphRef write SetGlyphRef;
      property format: string read GetFormat write SetFormat;
      property xlinkHref: string read GetXlinkHref write SetXlinkHref;
  end;

  { TSVGAltGlyphDef }

  TSVGAltGlyphDef = class(TSVGTextElementWithContent)
    public
      class function GetDOMTag: string; override;
  end;

  { TSVGAltGlyphItem }

  TSVGAltGlyphItem = class(TSVGTextElementWithContent)
    public
      class function GetDOMTag: string; override;
  end;

  { TSVGGlyphRef }

  TSVGGlyphRef = class(TSVGTextElement)
    private
      function GetX: TSVGNumber;
      function GetY: TSVGNumber;
      function GetDx: TSVGNumber;
      function GetDy: TSVGNumber;
      function GetGlyphRef: string;
      function GetFormat: string;
      function GetXlinkHref: string;
      procedure SetX(AValue: TSVGNumber);
      procedure SetY(AValue: TSVGNumber);
      procedure SetDx(AValue: TSVGNumber);
      procedure SetDy(AValue: TSVGNumber);
      procedure SetGlyphRef(AValue: string);
      procedure SetFormat(AValue: string);
      procedure SetXlinkHref(AValue: string);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      property x: TSVGNumber read GetX write SetX;
      property y: TSVGNumber read GetY write SetY;
      property dX: TSVGNumber read GetDx write SetDx;
      property dY: TSVGNumber read GetDy write SetDy;
      property glyphRef: string read GetGlyphRef write SetGlyphRef;
      property format: string read GetFormat write SetFormat;
      property xlinkHref: string read GetXlinkHref write SetXlinkHref;
  end;
  
  { TSVGClipPath }

  TSVGClipPath = class(TSVGElement)
    private
      function GetExternalResourcesRequired: boolean;
      function GetClipPathUnits: TSVGObjectUnits;
      procedure SetExternalResourcesRequired(AValue: boolean);
      procedure SetClipPathUnits(AValue: TSVGObjectUnits);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      property externalResourcesRequired: boolean
       read GetExternalResourcesRequired write SetExternalResourcesRequired;
      property clipPathUnits: TSVGObjectUnits read GetClipPathUnits write SetClipPathUnits;
  end;   
  
  { TSVGColorProfile }

  TSVGColorProfile = class(TSVGElement)
    private
      function GetLocal: string;
      function GetName: string;
      function GetRenderingIntent: TSVGRenderingIntent;
      function GetXlinkHref: string;
      procedure SetLocal(AValue: string);
      procedure SetName(AValue: string);
      procedure SetRenderingIntent(AValue: TSVGRenderingIntent);
      procedure SetXlinkHref(AValue: string);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      property local: string read GetLocal write SetLocal;
      property name: string read GetName write SetName;
      property renderingIntent: TSVGRenderingIntent read GetRenderingIntent write SetRenderingIntent;
      property xlinkHref: string read GetXlinkHref write SetXlinkHref;
  end;  
  
  { TSVGImage }

  TSVGImage = class(TSVGElement)
    private
      function GetExternalResourcesRequired: boolean;
      function GetX: TFloatWithCSSUnit;
      function GetY: TFloatWithCSSUnit;
      function GetWidth: TFloatWithCSSUnit;
      function GetHeight: TFloatWithCSSUnit;
      function GetPreserveAspectRatio: TSVGPreserveAspectRatio;
      function GetXlinkHref: string;
      procedure SetExternalResourcesRequired(AValue: boolean);
      procedure SetX(AValue: TFloatWithCSSUnit);
      procedure SetY(AValue: TFloatWithCSSUnit);
      procedure SetWidth(AValue: TFloatWithCSSUnit);
      procedure SetHeight(AValue: TFloatWithCSSUnit);
      procedure SetPreserveAspectRatio(AValue: TSVGPreserveAspectRatio);
      procedure SetXlinkHref(AValue: string);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      property externalResourcesRequired: boolean
       read GetExternalResourcesRequired write SetExternalResourcesRequired;
      property x: TFloatWithCSSUnit read GetX write SetX;
      property y: TFloatWithCSSUnit read GetY write SetY;
      property width: TFloatWithCSSUnit read GetWidth write SetWidth;
      property height: TFloatWithCSSUnit read GetHeight write SetHeight;
      property preserveAspectRatio: TSVGPreserveAspectRatio
       read GetPreserveAspectRatio write SetPreserveAspectRatio;
      property xlinkHref: string read GetXlinkHref write SetXlinkHref;
  end;   
  
  { TSVGPattern }

  TSVGPattern = class(TSVGImage)
    private
      function GetPatternUnits: TSVGObjectUnits;
      function GetPatternContentUnits: TSVGObjectUnits;
      function GetPatternTransform: string;
      function GetViewBox: TSVGViewBox;
      procedure SetPatternUnits(AValue: TSVGObjectUnits);
      procedure SetPatternContentUnits(AValue: TSVGObjectUnits);
      procedure SetPatternTransform(AValue: string);
      procedure SetViewBox(AValue: TSVGViewBox);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      property patternUnits: TSVGObjectUnits read GetPatternUnits write SetPatternUnits;
      property patternContentUnits: TSVGObjectUnits
       read GetPatternContentUnits write SetPatternContentUnits;
      property patternTransform: string read GetPatternTransform write SetPatternTransform;
      property viewBox: TSVGViewBox read GetViewBox write SetViewBox;
  end;
  
  { TSVGMarker }

  TSVGMarker = class(TSVGElement)
    private
      function GetExternalResourcesRequired: boolean;
      function GetViewBox: TSVGViewBox;
      function GetPreserveAspectRatio: TSVGPreserveAspectRatio;
      function GetRefX: TFloatWithCSSUnit;
      function GetRefY: TFloatWithCSSUnit;
      function GetMarkerWidth: TFloatWithCSSUnit;
      function GetMarkerHeight: TFloatWithCSSUnit;
      function GetMarkerUnits: TSVGMarkerUnits;
      function GetOrient: TSVGOrient;
      procedure SetExternalResourcesRequired(AValue: boolean);
      procedure SetViewBox(AValue: TSVGViewBox);
      procedure SetPreserveAspectRatio(AValue: TSVGPreserveAspectRatio);
      procedure SetRefX(AValue: TFloatWithCSSUnit);
      procedure SetRefY(AValue: TFloatWithCSSUnit);
      procedure SetMarkerWidth(AValue: TFloatWithCSSUnit);
      procedure SetMarkerHeight(AValue: TFloatWithCSSUnit);
      procedure SetMarkerUnits(AValue: TSVGMarkerUnits);
      procedure SetOrient(AValue: TSVGOrient);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      property externalResourcesRequired: boolean
       read GetExternalResourcesRequired write SetExternalResourcesRequired;
      property viewBox: TSVGViewBox read GetViewBox write SetViewBox;
      property preserveAspectRatio: TSVGPreserveAspectRatio
       read GetPreserveAspectRatio write SetPreserveAspectRatio;
      property refX: TFloatWithCSSUnit read GetRefX write SetRefX;
      property refY: TFloatWithCSSUnit read GetRefY write SetRefY;
      property markerWidth: TFloatWithCSSUnit read GetMarkerWidth write SetMarkerWidth;
      property markerHeight: TFloatWithCSSUnit read GetMarkerHeight write SetMarkerHeight;
      property markerUnits: TSVGMarkerUnits read GetMarkerUnits write SetMarkerUnits;
      property orient: TSVGOrient read GetOrient write SetOrient;
  end;
  
  { TSVGMask }

  TSVGMask = class(TSVGElement)
    private
      function GetExternalResourcesRequired: boolean;
      function GetX: TFloatWithCSSUnit;
      function GetY: TFloatWithCSSUnit;
      function GetWidth: TFloatWithCSSUnit;
      function GetHeight: TFloatWithCSSUnit;
      function GetMaskUnits: TSVGObjectUnits;
      function GetMaskContentUnits: TSVGObjectUnits;
      procedure SetExternalResourcesRequired(AValue: boolean);
      procedure SetX(AValue: TFloatWithCSSUnit);
      procedure SetY(AValue: TFloatWithCSSUnit);
      procedure SetWidth(AValue: TFloatWithCSSUnit);
      procedure SetHeight(AValue: TFloatWithCSSUnit);
      procedure SetMaskUnits(AValue: TSVGObjectUnits);
      procedure SetMaskContentUnits(AValue: TSVGObjectUnits);
    protected
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); override;
    public
      class function GetDOMTag: string; override;
      property externalResourcesRequired: boolean
       read GetExternalResourcesRequired write SetExternalResourcesRequired;
      property x: TFloatWithCSSUnit read GetX write SetX;
      property y: TFloatWithCSSUnit read GetY write SetY;
      property width: TFloatWithCSSUnit read GetWidth write SetWidth;
      property height: TFloatWithCSSUnit read GetHeight write SetHeight;
      property maskUnits: TSVGObjectUnits read GetMaskUnits write SetMaskUnits;
      property maskContentUnits: TSVGObjectUnits
       read GetMaskContentUnits write SetMaskContentUnits;
  end;      
  
  TConvMethod = (cmNone,cmHoriz,cmVertical,cmOrtho);
  
  { TSVGGradient } 

  TSVGGradient = class(TSVGElementWithContent)
    private
      function GetGradientMatrix(AUnit: TCSSUnit): TAffineMatrix;
      function GetGradientTransform: string;
      function GetGradientUnits: TSVGObjectUnits;
      function GetHRef: string;
      procedure SetGradientTransform(AValue: string);
      procedure SetGradientUnits(AValue: TSVGObjectUnits);
      procedure SetHRef(AValue: string);
    protected
      InheritedGradients: TSVGElementList;//(for HRef)
      procedure Initialize; override;
      function GetInheritedAttribute(AValue: string;
        AConvMethod: TConvMethod; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
    public
      destructor Destroy; override;
      procedure ScanInheritedGradients(const forceScan: boolean = false);
      property hRef: string read GetHRef write SetHRef;
      property gradientUnits: TSVGObjectUnits read GetGradientUnits write SetGradientUnits;
      property gradientTransform: string read GetGradientTransform write SetGradientTransform;
      property gradientMatrix[AUnit: TCSSUnit]: TAffineMatrix read GetGradientMatrix;
  end;        

  { TSVGGradientLinear }

  { TSVGLinearGradient }

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
      class function GetDOMTag: string; override;
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
      class function GetDOMTag: string; override;
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
      class function GetDOMTag: string; override;
      property Offset: TFloatWithCSSUnit read GetOffset write SetOffset;
  end;

  { TSVGDefine }

  TSVGDefine = class(TSVGElementWithContent)
  end; 

  { TSVGGroup }

  TSVGGroup = class(TSVGElementWithContent)
  private
    function GetFontSize: TFloatWithCSSUnit;
    procedure SetFontSize(AValue: TFloatWithCSSUnit);
  protected
    procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    property fontSize: TFloatWithCSSUnit read GetFontSize write SetFontSize;
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
     class function GetDOMTag: string; override;
     constructor Create(AElement: TDOMElement; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink); overload; override;
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
      FDoc: TDOMDocument;
      FElements: TFPList;
      FUnits: TCSSUnitConverter;
      function GetDOMNode(AElement: TObject): TDOMNode;
      function GetElementDOMNode(AIndex: integer): TDOMNode;
      procedure AppendElement(AElement: TObject); overload;
      procedure InsertElementBefore(AElement: TSVGElement; ASuccessor: TSVGElement);
      function GetElement(AIndex: integer): TSVGElement;
      function GetIsSVGElement(AIndex: integer): boolean;
      function GetElementCount: integer;
      function GetUnits: TCSSUnitConverter;
      function TryCreateElementFromNode(ANode: TDOMNode): TObject; virtual;
    public
      constructor Create(AElement: TDOMElement; AUnits: TCSSUnitConverter;
        ADataLink: TSVGDataLink);
      destructor Destroy; override;
      procedure Clear;
      procedure Recompute;
      procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; AUnit: TCSSUnit); overload;
      procedure Draw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); overload;
      function AppendElement(ASVGType: TSVGFactory): TSVGElement; overload;
      function AppendDOMText(AText: string): TDOMText;
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
      property ElementDOMNode[AIndex: integer]: TDOMNode read GetElementDOMNode;
      property IsSVGElement[AIndex: integer]: boolean read GetIsSVGElement;
      property Units: TCSSUnitConverter read GetUnits;
  end;

function GetSVGFactory(ATagName: string): TSVGFactory;
function CreateSVGElementFromNode(AElement: TDOMElement; AUnits: TCSSUnitConverter;
  ADataLink: TSVGDataLink): TSVGElement;

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
  if tag='tspan' then
    result := TSVGTSpan else
  if tag='tref' then
    result := TSVGTRef else
  if tag='textpath' then
    result := TSVGTextPath else
  if tag='altglyph' then
    result := TSVGAltGlyph else
  if tag='altglyphdef' then
    result := TSVGAltGlyphDef else
  if tag='altglyphitem' then
    result := TSVGAltGlyphItem else
  if tag='glyphref' then
    result := TSVGGlyphRef else
  if tag='clippath' then
    result := TSVGClipPath else
  if tag='colorprofile' then
    result := TSVGColorProfile else
  if tag='image' then
    result := TSVGImage else
  if tag='pattern' then
    result := TSVGPattern else
  if tag='marker' then
    result := TSVGMarker else
  if tag='mask' then
    result := TSVGMask else
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

function CreateSVGElementFromNode(AElement: TDOMElement; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink): TSVGElement;
var
  factory: TSVGFactory;
begin
  factory := GetSVGFactory(AElement.TagName);
  result := factory.Create(AElement,AUnits,ADataLink);
  
  ADataLink.Link(result);
end;

{ TSVGElementWithContent }

constructor TSVGElementWithContent.Create(ADocument: TDOMDocument;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  FContent := TSVGContent.Create(FDomElem,AUnits,ADataLink);
end;

constructor TSVGElementWithContent.Create(AElement: TDOMElement;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(AElement, AUnits, ADataLink);
  FContent := TSVGContent.Create(AElement,AUnits,ADataLink);
end;

destructor TSVGElementWithContent.Destroy;
begin
  FreeAndNil(FContent);
  inherited Destroy;
end;

procedure TSVGElementWithContent.Recompute;
begin
  FContent.Recompute;
  inherited Recompute;
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
begin
  FGradientElement := TSVGGradient(FDataLink.FindElementByRef(fill, TSVGGradient));
  Result := Assigned(FGradientElement);
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
    with (el as TSVGGradient).Content do
      for i:= 0 to ElementCount-1 do
        if IsSVGElement[i] and (Element[i] is TSVGStopGradient) then
          with TSVGStopGradient(Element[i]) do
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
  if g.gradientUnits = souObjectBoundingBox then
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
      u.Scale( (r/d)*0.99999 );
      f := c+u;
    end;
    FCanvasGradient:= ACanvas2d.createRadialGradient(c,r,f,fr,true);
    AddStopElements(FCanvasGradient);
  end;

begin
  g := ASVGGradient as TSVGRadialGradient;
  if g.gradientUnits = souObjectBoundingBox then
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

{ TSVGTextElementWithContent }

constructor TSVGTextElementWithContent.Create(ADocument: TDOMDocument;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  FContent := TSVGContent.Create(FDomElem,AUnits,ADataLink);
end;

constructor TSVGTextElementWithContent.Create(AElement: TDOMElement;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(AElement, AUnits, ADataLink);
  FContent := TSVGContent.Create(AElement,AUnits,ADataLink);
end;

destructor TSVGTextElementWithContent.Destroy;
begin
  FreeAndNil(FContent);
  inherited Destroy;
end;

{ TSVGTextPositioning }

function TSVGTextPositioning.GetX: ArrayOfTFloatWithCSSUnit;
begin
  result := ArrayOfHorizAttributeWithUnitInherit['x',False];
end;

function TSVGTextPositioning.GetY: ArrayOfTFloatWithCSSUnit;
begin
  result := ArrayOfVerticalAttributeWithUnitInherit['y',False];
end;

function TSVGTextPositioning.GetDx: ArrayOfTFloatWithCSSUnit;
begin
  result := ArrayOfHorizAttributeWithUnitInherit['dx',False];
end;

function TSVGTextPositioning.GetDy: ArrayOfTFloatWithCSSUnit;
begin
  result := ArrayOfVerticalAttributeWithUnitInherit['dy',False];
end;

function TSVGTextPositioning.GetRotate: ArrayOfTSVGNumber;
begin
  result := ArrayOfAttributeNumberInherit['rotate',False];
end;

procedure TSVGTextPositioning.SetX(AValue: ArrayOfTFloatWithCSSUnit);
begin
  ArrayOfHorizAttributeWithUnit['x'] := AValue;
end;

procedure TSVGTextPositioning.SetY(AValue: ArrayOfTFloatWithCSSUnit);
begin
  ArrayOfVerticalAttributeWithUnit['y'] := AValue;
end;

procedure TSVGTextPositioning.SetDx(AValue: ArrayOfTFloatWithCSSUnit);
begin
  ArrayOfHorizAttributeWithUnit['dx'] := AValue;
end;

procedure TSVGTextPositioning.SetDy(AValue: ArrayOfTFloatWithCSSUnit);
begin
  ArrayOfVerticalAttributeWithUnit['dy'] := AValue;
end;

procedure TSVGTextPositioning.SetRotate(AValue: ArrayOfTSVGNumber);
begin
  ArrayOfAttributeNumber['rotate'] := AValue;
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
  result:= GetVerticalAttributeOrStyleWithUnit('font-size',Units.CurrentFontEmHeight,false);
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
var
  i: Integer;
begin
  if FInGetSimpleText then exit(''); //avoid reentrance
  FInGetSimpleText := true;
  result := '';
  for i := 0 to FContent.ElementCount-1 do
    if FContent.IsSVGElement[i] then
    begin
      if FContent.Element[i] is TSVGTRef then
        result += GetTRefContent(TSVGTRef(FContent.Element[i]))
      else
      if FContent.Element[i] is TSVGText then
        result += TSVGText(FContent.Element[i]).SimpleText;
    end else
    begin
      if FContent.ElementDOMNode[i] is TDOMText then
        result += TDOMText(FContent.ElementDOMNode[i]).Data;
    end;
  FInGetSimpleText := false;
end;

function TSVGText.GetTextAnchor: TSVGTextAnchor;
begin
  case AttributeOrStyleDef['text-anchor','start'] of
   'middle': result := staMiddle;
   'end': result := staEnd;
   else result := staStart;
  end;
end;

function TSVGText.GetTextDirection: TSVGTextDirection;
begin
  if AttributeOrStyle['direction'] = 'rtl' then
    result := stdRtl
  else
    result := stdLtr;
end;

function TSVGText.GetTextDecoration: string;
begin
  result := AttributeOrStyleDef['text-decoration','none'];
end;

function TSVGText.GetTextLength: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnitDef['textLength'];
end;

function TSVGText.GetLengthAdjust: TSVGLengthAdjust;
var
  valueText: string;
begin
  valueText := trim(Attribute['lengthAdjust','spacing']);
  if valueText = 'spacing' then
    result := slaSpacing
  else
    result := slaSpacingAndGlyphs;
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

procedure TSVGText.SetTextAnchor(AValue: TSVGTextAnchor);
begin
  case AValue of
   staMiddle: Attribute['text-anchor'] := 'middle';
   staEnd: Attribute['text-anchor'] := 'end';
  else {staStart} Attribute['text-anchor'] := 'start';
  end;
end;

procedure TSVGText.SetTextDirection(AValue: TSVGTextDirection);
begin
  if AValue = stdLtr then
    Attribute['direction'] := 'ltr'
  else
    Attribute['direction'] := 'rtl';
end;  

procedure TSVGText.SetSimpleText(AValue: string);
begin
  Content.Clear;
  Content.appendDOMText(AValue);
end;

procedure TSVGText.SetTextDecoration(AValue: string);
begin
  Attribute['text-decoration'] := AValue;
  RemoveStyle('text-decoration');
end;

procedure TSVGText.SetTextLength(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['textLength'] := AValue;
  RemoveStyle('textLength');
end;

procedure TSVGText.SetLengthAdjust(AValue: TSVGLengthAdjust);
begin
  if AValue = slaSpacing then
    Attribute['lengthAdjust'] := 'spacing'
  else
    Attribute['lengthAdjust'] := 'spacingAndGlyphs';
  RemoveStyle('lengthAdjust');
end;

procedure TSVGText.InternalDrawOrCompute(ACanvas2d: TBGRACanvas2D;
  AUnit: TCSSUnit; ADraw: boolean; AAllTextBounds: TRectF;
  var APosition: TPointF; var ATextParts: ArrayOfTextParts);
begin
  if not ADraw then ATextParts[0].AbsoluteCoord := APosition;
  InternalDrawOrCompute(ACanvas2d, AUnit, ADraw, AAllTextBounds, APosition, ATextParts, 0,0,high(ATextParts));
end;

procedure TSVGText.InternalDrawOrCompute(ACanvas2d: TBGRACanvas2D;
  AUnit: TCSSUnit; ADraw: boolean; AAllTextBounds: TRectF;
  var APosition: TPointF; var ATextParts: ArrayOfTextParts;
  ALevel: integer; AStartPart, AEndPart: integer);
var
  prevFontEmHeight, fs: TFloatWithCSSUnit;
  ax, ay, adx, ady: ArrayOfTFloatWithCSSUnit;
  i, subStartPart, subEndPart, subLevel: integer;
  subElem: TSVGText;
  partBounds: TRectF;
begin
  if AStartPart > AEndPart then exit;

  prevFontEmHeight := Units.CurrentFontEmHeight;
  fs := fontSize;
  if fs.CSSUnit in [cuFontEmHeight,cuFontXHeight] then
    fs := Units.ConvertHeight(fontSize,AUnit);
  Units.CurrentFontEmHeight:= fs;

  if not ADraw then
  begin
    ax := Units.ConvertWidth(x,AUnit);
    ay := Units.ConvertHeight(y,AUnit);
    if length(ax)>0 then APosition.x := ax[0].value;
    if length(ay)>0 then APosition.y := ay[0].value;
    if (length(ax)>0) or (length(ay)>0) then
      ATextParts[AStartPart].AbsoluteCoord := APosition;
  end else
    APosition := ATextParts[AStartPart].AbsoluteCoord;

  adx := Units.ConvertWidth(dx,AUnit);
  ady := Units.ConvertHeight(dy,AUnit);
  if length(adx)>0 then APosition.x += adx[0].value;
  if length(ady)>0 then APosition.y += ady[0].value;

  i := AStartPart;
  while i <= AEndPart do
  begin
    if ATextParts[i].Level > ALevel then
    begin
      subStartPart := i;
      subEndPart := i;
      subElem := TSVGText(ATextParts[subStartPart].BaseElement);
      subLevel := ATextParts[subStartPart].Level;
      while (subEndPart < AEndPart) and
            ( ((ATextParts[subEndPart+1].Level = subLevel) and (ATextParts[subEndPart+1].BaseElement = subElem)) or
              (ATextParts[subEndPart+1].Level > subLevel) ) do
        inc(subEndPart);
      subElem.InternalDrawOrCompute(
        ACanvas2d, AUnit, ADraw, AAllTextBounds, APosition,
        ATextParts, subLevel, subStartPart, subEndPart);
      i := subEndPart+1;
    end
    else
    begin
      if not ADraw then
        ATextParts[i].PartStartCoord := APosition
      else
        APosition := ATextParts[i].PartStartCoord;

      if ATextParts[i].Text <>'' then
        InternalDrawOrComputePart(ACanvas2d, AUnit, ATextParts[i].Text, ADraw, AAllTextBounds, APosition, partBounds)
      else
        partBounds := EmptyRectF;

      if not ADraw then
      begin
        ATextParts[i].PartEndCoord := APosition;
        ATextParts[i].Bounds := partBounds;
      end
      else
        APosition := ATextParts[i].PartEndCoord;

      inc(i);
    end;
  end;

  Units.CurrentFontEmHeight := prevFontEmHeight;
end;

procedure TSVGText.InternalDrawOrComputePart(ACanvas2d: TBGRACanvas2D;
  AUnit: TCSSUnit; AText: string; ADraw: boolean; AAllTextBounds: TRectF;
  var APosition: TPointF; out ABounds: TRectF);
var
  ts: TCanvas2dTextSize;
  fs: TFontStyles;
  dir: TSVGTextDirection;
  deco: String;
begin
  ACanvas2d.fontEmHeight := Units.ConvertHeight(Units.CurrentFontEmHeight, AUnit).value;
  ACanvas2d.fontName := fontFamily;
  fs := [];
  if fontBold then include(fs, fsBold);
  if fontItalic then include(fs, fsItalic);
  deco := ' '+textDecoration+' ';
  if pos(' line-through ',deco)<>0 then include(fs, fsStrikeOut);
  if pos(' underline ',deco)<>0 then include(fs, fsUnderline);
  ACanvas2d.fontStyle := fs;
  dir := textDirection;
  case dir of
   stdRtl: ACanvas2d.direction:= fbmRightToLeft;
   else {stdRtl} ACanvas2d.direction:= fbmLeftToRight;
  end;

  ts := ACanvas2d.measureText(AText);
  if dir = stdRtl then APosition.x -= ts.width;

  ABounds := RectF(APosition.x,APosition.y,APosition.x+ts.width,APosition.y+ts.height);
  if ADraw then
  begin
    ACanvas2d.beginPath;
    if Assigned(GradientElement) then
      InitializeGradient(ACanvas2d, AAllTextBounds.TopLeft, AAllTextBounds.Width,AAllTextBounds.Height,AUnit);
    ACanvas2d.text(AText,APosition.x,APosition.y);
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

  if dir = stdLtr then APosition.x += ts.width;
end;

procedure TSVGText.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var
  allTextBounds: TRectF;
  textParts: ArrayOfTextParts;
  anchor: TSVGTextAnchor;

  procedure DoAlignText(AStartPart,AEndPart: integer);
  var
    advance,ofs: single;
    j: Integer;
  begin
    advance := textParts[AEndPart].PartEndCoord.x - textParts[AStartPart].AbsoluteCoord.x;
    ofs := 0;

    case anchor of
    staMiddle: ofs := (-1/2)*advance;
    staEnd: ofs := -advance;
    else ofs := 0;
    end;

    for j := AStartPart to AEndPart do
    begin
      if not isEmptyPointF(textParts[j].AbsoluteCoord) then textParts[j].AbsoluteCoord.x += ofs;
      if not isEmptyPointF(textParts[j].PartStartCoord) then textParts[j].PartStartCoord.x += ofs;
      if not isEmptyPointF(textParts[j].PartEndCoord) then textParts[j].PartEndCoord.x += ofs;
      if not IsEmptyRectF(textParts[j].Bounds) then textParts[j].Bounds.Offset(ofs,0);
    end;
  end;

var
  i, absStartIndex: Integer;
  pos: TPointF;
begin
  textParts := GetAllText;
  CleanText(textParts);
  if length(textParts)>0 then
  begin
    pos := PointF(0,0);
    InternalDrawOrCompute(ACanvas2d, AUnit, False, EmptyRectF, pos, textParts);

    anchor := textAnchor;

    absStartIndex := -1;
    for i := 0 to high(textParts) do
    begin
      if not IsEmptyPointF(textParts[i].AbsoluteCoord) then
      begin
        if absStartIndex <> -1 then DoAlignText(absStartIndex,i-1);
        absStartIndex := i;
      end;
    end;
    if absStartIndex <> -1 then DoAlignText(absStartIndex,high(textParts));

    allTextBounds := EmptyRectF;
    for i := 0 to high(textParts) do
      allTextBounds := allTextBounds.Union(textParts[i].Bounds);

    pos := PointF(0,0);
    InternalDrawOrCompute(ACanvas2d, AUnit, True, allTextBounds, pos, textParts);
  end;
end;

procedure TSVGText.CleanText(var ATextParts: ArrayOfTextParts);
var wasSpace: boolean;
  wasSpaceBeforePartIdx: integer;
  i,j: integer;
  k,l, startPos, endPosP1: integer;
  fullText, cleanedText: string;
begin
  wasSpace := false;
  wasSpaceBeforePartIdx:= -1;
  fullText := '';
  for k := 0 to high(ATextParts) do
    fullText += ATextParts[k].Text;

  setlength(cleanedText, length(fullText));
  j := 0;
  k := 0;
  for i := 1 to length(fullText) do
  begin
    if not (fullText[i] in[#0..#32]) and wasSpace and (j>0) then
    begin
      inc(j);
      cleanedText[j] := ' ';
      if wasSpaceBeforePartIdx <> -1 then
        for l := wasSpaceBeforePartIdx to k-1 do
          inc(ATextParts[l].SplitPos);
      wasSpace:= false;
    end;
    while (k < length(ATextParts)) and (i = ATextParts[k].SplitPos) do
    begin
      if wasSpace and (wasSpaceBeforePartIdx = -1) then
        wasSpaceBeforePartIdx:= k;
      ATextParts[k].SplitPos := j+1;
      inc(k);
    end;
    if fullText[i] in[#0..#32] then
      wasSpace := true
    else
    begin
      inc(j);
      cleanedText[j] := fullText[i];
      wasSpace := false;
      wasSpaceBeforePartIdx := -1;
    end;
  end;
  while k < length(ATextParts) do
  begin
    ATextParts[k].SplitPos := j+1;
    inc(k);
  end;
  setlength(cleanedText, j);

  for k := 0 to high(ATextParts) do
  begin
    startPos := ATextParts[k].SplitPos;
    if k = high(ATextParts) then endPosP1 := j+1 else
      endPosP1 := ATextParts[k+1].SplitPos;
    ATextParts[k].Text:= copy(cleanedText, startPos, endPosP1 - startPos);
  end;
end;

function TSVGText.GetTRefContent(AElement: TSVGTRef): string;
var
  refText: TSVGText;
begin
  refText := TSVGText(FDataLink.FindElementByRef(AElement.xlinkHref, TSVGText));
  if Assigned(refText) then result := refText.SimpleText else result := '';
end;

function TSVGText.GetAllText: ArrayOfTextParts;
var
  i,j,idxOut,curLen: Integer;
  svgElem: TSVGElement;
  subParts: ArrayOfTextParts;
  node: TDOMNode;

  procedure AppendPart(AText: string);
  begin
    if (idxOut > 0) and (result[idxOut-1].Text = '')
      and (result[idxOut-1].BaseElement = self) then dec(idxOut);
    result[idxOut].Level := 0;
    result[idxOut].BaseElement:= self;
    result[idxOut].Text := AText;
    result[idxOut].SplitPos:= curLen+1;
    result[idxOut].AbsoluteCoord := EmptyPointF;
    result[idxOut].PartStartCoord := EmptyPointF;
    result[idxOut].Bounds := EmptyRectF;
    inc(curLen, length(AText));
    inc(idxOut);
  end;

begin
  setlength(result, Content.ElementCount+1);
  idxOut := 0;
  curLen := 0;
  AppendPart(''); //needed when there is a sub part to know the base element
  for i := 0 to Content.ElementCount-1 do
  begin
    if Content.IsSVGElement[i] then
    begin
      svgElem := Content.Element[i];
      if svgElem is TSVGTRef then
        AppendPart(GetTRefContent(TSVGTRef(svgElem)))
      else
      if svgElem is TSVGText then
      begin
        subParts := TSVGText(svgElem).GetAllText;
        if length(subParts) > 0 then
        begin
          setlength(result, length(result)+length(subParts)-1);
          for j := 0 to high(subParts) do
          begin
            result[idxOut] := subParts[j];
            inc(result[idxOut].Level);
            result[idxOut].SplitPos:= curLen+1;
            inc(curLen, length(result[idxOut].Text));
            inc(idxOut);
          end;
        end else
          AppendPart('');
      end;
    end else
    begin
      node := Content.ElementDOMNode[i];
      if node is TDOMText then
        AppendPart(TDOMText(node).Data);
    end;
  end;
  setlength(result, idxOut);
end;

class function TSVGText.GetDOMTag: string;
begin
  Result:= 'text';
end;

{ TSVGTSpan }

class function TSVGTSpan.GetDOMTag: string;
begin
  Result:= 'tspan';
end;

{ TSVGTRef }

function TSVGTRef.GetXlinkHref: string;
begin
  result := Attribute['xlink:href',''];
end;

procedure TSVGTRef.SetXlinkHref(AValue: string);
begin
  Attribute['xlink:href'] := AValue;
end;

class function TSVGTRef.GetDOMTag: string;
begin
  Result:= 'tref';
end;

{ TSVGTextPath }

function TSVGTextPath.GetStartOffset: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnitDef['startOffset'];
end;

function TSVGTextPath.GetMethod: TSVGTextPathMethod;
var
  valueText: string;
begin
  valueText := trim(Attribute['method','align']);
  if valueText = 'align' then
    result := stpmAlign
  else
    result := stpmStretch;
end;

function TSVGTextPath.GetSpacing: TSVGTextPathSpacing;
var
  valueText: string;
begin
  valueText := trim(Attribute['spacing','exact']);
  if valueText = 'exact' then
    result := stpsExact
  else
    result := stpsAuto;
end;

function TSVGTextPath.GetXlinkHref: string;
begin
  result := Attribute['xlink:href',''];
end;

procedure TSVGTextPath.SetStartOffset(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['startOffset'] := AValue;
  RemoveStyle('startOffset');
end;

procedure TSVGTextPath.SetMethod(AValue: TSVGTextPathMethod);
begin
 if AValue = stpmAlign then
   Attribute['method'] := 'align'
 else
   Attribute['method'] := 'stretch';
 RemoveStyle('method');
end;

procedure TSVGTextPath.SetSpacing(AValue: TSVGTextPathSpacing);
begin
 if AValue = stpsExact then
   Attribute['spacing'] := 'exact'
 else
   Attribute['spacing'] := 'auto';
 RemoveStyle('spacing');
end;

procedure TSVGTextPath.SetXlinkHref(AValue: string);
begin
  Attribute['xlink:href'] := AValue;
end;

procedure TSVGTextPath.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  //todo
end;

class function TSVGTextPath.GetDOMTag: string;
begin
  Result:= 'textpath';
end;

{ TSVGAltGlyph }

function TSVGAltGlyph.GetGlyphRef: string;
begin
  result := Attribute['glyphRef',''];
end;

function TSVGAltGlyph.GetFormat: string;
begin
  result := Attribute['format',''];
end;

function TSVGAltGlyph.GetXlinkHref: string;
begin
  result := Attribute['xlink:href',''];
end;

procedure TSVGAltGlyph.SetGlyphRef(AValue: string);
begin
  Attribute['glyphRef'] := AValue;
end;

procedure TSVGAltGlyph.SetFormat(AValue: string);
begin
  Attribute['format'] := AValue;
end;

procedure TSVGAltGlyph.SetXlinkHref(AValue: string);
begin
  Attribute['xlink:href'] := AValue;
end;

procedure TSVGAltGlyph.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  //todo
end;

class function TSVGAltGlyph.GetDOMTag: string;
begin
  Result:= 'altglyph';
end;

{ TSVGAltGlyphDef }

class function TSVGAltGlyphDef.GetDOMTag: string;
begin
  Result:= 'altglyphdef';
end;

{ TSVGAltGlyphItem }

class function TSVGAltGlyphItem.GetDOMTag: string;
begin
  Result:= 'altglyphitem';
end;

{ TSVGGlyphRef }

function TSVGGlyphRef.GetX: TSVGNumber;
begin
  result := HorizAttribute['x'];
end;

function TSVGGlyphRef.GetY: TSVGNumber;
begin
  result := VerticalAttribute['y'];
end;

function TSVGGlyphRef.GetDx: TSVGNumber;
begin
  result := HorizAttribute['dx'];
end;

function TSVGGlyphRef.GetDy: TSVGNumber;
begin
  result := VerticalAttribute['dy'];
end;

function TSVGGlyphRef.GetGlyphRef: string;
begin
  result := Attribute['glyphRef',''];
end;

function TSVGGlyphRef.GetFormat: string;
begin
  result := Attribute['format',''];
end;

function TSVGGlyphRef.GetXlinkHref: string;
begin
  result := Attribute['xlink:href',''];
end;

procedure TSVGGlyphRef.SetX(AValue: TSVGNumber);
begin
  HorizAttribute['x'] := AValue;
end;

procedure TSVGGlyphRef.SetY(AValue: TSVGNumber);
begin
  VerticalAttribute['y'] := AValue;
end;

procedure TSVGGlyphRef.SetDx(AValue: TSVGNumber);
begin
  HorizAttribute['dx'] := AValue;
end;

procedure TSVGGlyphRef.SetDy(AValue: TSVGNumber);
begin
  HorizAttribute['dy'] := AValue;
end;

procedure TSVGGlyphRef.SetGlyphRef(AValue: string);
begin
  Attribute['glyphRef'] := AValue;
end;

procedure TSVGGlyphRef.SetFormat(AValue: string);
begin
  Attribute['format'] := AValue;
end;

procedure TSVGGlyphRef.SetXlinkHref(AValue: string);
begin
  Attribute['xlink:href'] := AValue;
end;

procedure TSVGGlyphRef.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  //todo
end;

class function TSVGGlyphRef.GetDOMTag: string;
begin
  Result:= 'glyphref';
end;

{ TSVGClipPath }

function TSVGClipPath.GetExternalResourcesRequired: boolean;
begin
  if Attribute['externalResourcesRequired'] = 'true' then
    result := true
  else
    result := false;
end;

function TSVGClipPath.GetClipPathUnits: TSVGObjectUnits;
begin
  if Attribute['clipPathUnits'] = 'objectBoundingBox' then
    result := souObjectBoundingBox
  else
    result := souUserSpaceOnUse;
end;

procedure TSVGClipPath.SetExternalResourcesRequired(AValue: boolean);
begin
  if AValue then
    Attribute['ExternalResourcesRequired'] := 'true'
  else
    Attribute['ExternalResourcesRequired'] := 'false';
end;

procedure TSVGClipPath.SetClipPathUnits(AValue: TSVGObjectUnits);
begin
  if AValue = souUserSpaceOnUse then
    Attribute['clipPathUnits'] := 'userSpaceOnUse'
  else
    Attribute['clipPathUnits'] := 'objectBoundingBox';
end;

procedure TSVGClipPath.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  //todo
end;

class function TSVGClipPath.GetDOMTag: string;
begin
  Result:= 'clippath';
end;

{ TSVGColorProfile }

function TSVGColorProfile.GetLocal: string;
begin
  result := Attribute['local'];
end;

function TSVGColorProfile.GetName: string;
begin
  result := Attribute['name'];
end;

function TSVGColorProfile.GetRenderingIntent: TSVGRenderingIntent;
var
  s: string;
begin
  s := Attribute['rendering-intent','auto'];
  if s = 'auto' then
    result := sriAuto
  else if s = 'perceptual' then
    result :=  sriPerceptual
  else if s = 'relative-colorimetric' then
    result := sriRelativeColorimetric
  else if s = 'saturation' then
    result := sriSaturation
  else { 'absolute-colorimetric' }
    result := sriAbsoluteColorimetric;
end;

function TSVGColorProfile.GetXlinkHref: string;
begin
  result := Attribute['xlink:href',''];
end;

procedure TSVGColorProfile.SetLocal(AValue: string);
begin
  Attribute['local'] := AValue;
end;

procedure TSVGColorProfile.SetName(AValue: string);
begin
  Attribute['name'] := AValue;
end;

procedure TSVGColorProfile.SetRenderingIntent(AValue: TSVGRenderingIntent);
begin
  if AValue = sriAuto then
    Attribute['rendering-intent'] := 'auto'
  else if AValue = sriPerceptual then
    Attribute['rendering-intent'] := 'perceptual'
  else if AValue = sriRelativeColorimetric then
    Attribute['rendering-intent'] := 'relative-colorimetric'
  else if AValue = sriSaturation then
    Attribute['rendering-intent'] := 'saturation'
  else { sriAbsoluteColorimetric }
    Attribute['rendering-intent'] := 'absolute-colorimetric'
end;

procedure TSVGColorProfile.SetXlinkHref(AValue: string);
begin
  Attribute['xlink:href'] := AValue;
end;

procedure TSVGColorProfile.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  //todo
end;

class function TSVGColorProfile.GetDOMTag: string;
begin
  Result:= 'colorprofile';
end;

{ TSVGImage }

function TSVGImage.GetExternalResourcesRequired: boolean;
begin
  if Attribute['externalResourcesRequired'] = 'true' then
    result := true
  else
    result := false;
end;

function TSVGImage.GetX: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['x'];
end;

function TSVGImage.GetY: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['y'];
end;

function TSVGImage.GetWidth: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['width'];
end;

function TSVGImage.GetHeight: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['height'];
end;

function TSVGImage.GetPreserveAspectRatio: TSVGPreserveAspectRatio;
begin
  result := TSVGPreserveAspectRatio.Parse(Attribute['preserveAspectRatio','xMidYMid']);
end;

function TSVGImage.GetXlinkHref: string;
begin
  result := Attribute['xlink:href',''];
end;

procedure TSVGImage.SetExternalResourcesRequired(AValue: boolean);
begin
  if AValue then
    Attribute['ExternalResourcesRequired'] := 'true'
  else
    Attribute['ExternalResourcesRequired'] := 'false';
end;

procedure TSVGImage.SetX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['x'] := AValue;
end;

procedure TSVGImage.SetY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['y'] := AValue;
end;

procedure TSVGImage.SetWidth(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['width'] := AValue;
end;

procedure TSVGImage.SetHeight(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['height'] := AValue;
end;

procedure TSVGImage.SetPreserveAspectRatio(AValue: TSVGPreserveAspectRatio);
begin
  Attribute['preserveAspectRatio'] := AValue.ToString;
end;

procedure TSVGImage.SetXlinkHref(AValue: string);
begin
  Attribute['xlink:href'] := AValue;
end;

procedure TSVGImage.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  //todo
end;

class function TSVGImage.GetDOMTag: string;
begin
  Result:= 'image';
end;

{ TSVGPattern }

function TSVGPattern.GetPatternUnits: TSVGObjectUnits;
begin
  if Attribute['patternUnits'] = 'userSpaceOnUse' then
    result := souUserSpaceOnUse
  else
    result := souObjectBoundingBox;
end;

function TSVGPattern.GetPatternContentUnits: TSVGObjectUnits;
begin
  if Attribute['patternContentUnits'] = 'objectBoundingBox' then
    result := souObjectBoundingBox
  else
    result := souUserSpaceOnUse;
end;

function TSVGPattern.GetPatternTransform: string;
begin
  result := Attribute['patternTransform'];
end;

function TSVGPattern.GetViewBox: TSVGViewBox;
begin
  result := TSVGViewBox.Parse(Attribute['viewBox']);
end;

procedure TSVGPattern.SetPatternUnits(AValue: TSVGObjectUnits);
begin
  if AValue = souUserSpaceOnUse then
    Attribute['patternUnits'] := 'userSpaceOnUse'
  else
    Attribute['patternUnits'] := 'objectBoundingBox';
end;

procedure TSVGPattern.SetPatternContentUnits(AValue: TSVGObjectUnits);
begin
  if AValue = souUserSpaceOnUse then
    Attribute['patternContentUnits'] := 'userSpaceOnUse'
  else
    Attribute['patternContentUnits'] := 'objectBoundingBox';
end;

procedure TSVGPattern.SetPatternTransform(AValue: string);
begin
  Attribute['patternTransform'] := AValue;
end;

procedure TSVGPattern.SetViewBox(AValue: TSVGViewBox);
begin
  Attribute['viewBox'] := AValue.ToString;
end;

procedure TSVGPattern.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  //todo
end;

class function TSVGPattern.GetDOMTag: string;
begin
  Result:= 'pattern';
end;

{ TSVGMarker }

function TSVGMarker.GetExternalResourcesRequired: boolean;
begin
  if Attribute['externalResourcesRequired'] = 'true' then
    result := true
  else
    result := false;
end;

function TSVGMarker.GetViewBox: TSVGViewBox;
begin
  result := TSVGViewBox.Parse(Attribute['viewBox']);
end;

function TSVGMarker.GetPreserveAspectRatio: TSVGPreserveAspectRatio;
begin
  result := TSVGPreserveAspectRatio.Parse(Attribute['preserveAspectRatio','xMidYMid']);
end;

function TSVGMarker.GetRefX: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['refX'];
end;

function TSVGMarker.GetRefY: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['refY'];
end;

function TSVGMarker.GetMarkerWidth: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['markerWidth'];
end;

function TSVGMarker.GetMarkerHeight: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['markerHeight'];
end;  

function TSVGMarker.GetMarkerUnits: TSVGMarkerUnits;
begin
  if Attribute['markerUnits','strokeWidth'] = 'strokeWidth' then
    result := smuStrokeWidth
  else
    result := smuUserSpaceOnUse;
end;

function TSVGMarker.GetOrient: TSVGOrient;
var
  err: integer;
  s: string;
begin
  s := Attribute['orient','0'];
  result.angle := 0;
  if s = 'auto' then
    result.auto := soaAuto
  else if s = 'auto-start-reverse' then
    result.auto := soaAutoReverse
  else
  begin
    result.auto := soaNone;
    Val(s, result.angle, err);
    if err <> 0 then
      raise Exception('conversion error: '+IntToStr(err)+#13+'"'+s+'"');
  end;
end;

procedure TSVGMarker.SetExternalResourcesRequired(AValue: boolean);
begin
  if AValue then
    Attribute['ExternalResourcesRequired'] := 'true'
  else
    Attribute['ExternalResourcesRequired'] := 'false';
end;

procedure TSVGMarker.SetViewBox(AValue: TSVGViewBox);
begin
  Attribute['viewBox'] := AValue.ToString;
end;

procedure TSVGMarker.SetPreserveAspectRatio(AValue: TSVGPreserveAspectRatio);
begin
  Attribute['preserveAspectRatio'] := AValue.ToString;
end;

procedure TSVGMarker.SetRefX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['refX'] := AValue;
end;

procedure TSVGMarker.SetRefY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['refY'] := AValue;
end;

procedure TSVGMarker.SetMarkerWidth(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['markerWidth'] := AValue;
end;

procedure TSVGMarker.SetMarkerHeight(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['markerHeight'] := AValue;
end;      

procedure TSVGMarker.SetMarkerUnits(AValue: TSVGMarkerUnits);
begin
  if AValue = smuStrokeWidth then
    Attribute['markerUnits'] := 'strokeWidth'
  else
    Attribute['markerUnits'] := 'useSpaceOnUse';
end;

procedure TSVGMarker.SetOrient(AValue: TSVGOrient);
var
  s: string;
begin
  if AValue.auto = soaAuto then
    s := 'auto'
  else if AValue.auto = soaAutoReverse then
    s := 'auto-start-reverse'
  else
    s := FloatToStr(AValue.angle);
  Attribute['orient'] := s;
end;

procedure TSVGMarker.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  //todo
end;

class function TSVGMarker.GetDOMTag: string;
begin
  Result:= 'marker';
end;

{ TSVGMask }

function TSVGMask.GetExternalResourcesRequired: boolean;
begin
  if Attribute['externalResourcesRequired'] = 'true' then
    result := true
  else
    result := false;
end;

function TSVGMask.GetX: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['x'];
end;

function TSVGMask.GetY: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['y'];
end;

function TSVGMask.GetWidth: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['width'];
end;

function TSVGMask.GetHeight: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['height'];
end;

function TSVGMask.GetMaskUnits: TSVGObjectUnits;
begin
  if Attribute['maskUnits'] = 'objectBoundingBox' then
    result := souObjectBoundingBox
  else
    result := souUserSpaceOnUse;
end;

function TSVGMask.GetMaskContentUnits: TSVGObjectUnits;
begin
  if Attribute['maskContentUnits'] = 'objectBoundingBox' then
    result := souObjectBoundingBox
  else
    result := souUserSpaceOnUse;
end;

procedure TSVGMask.SetExternalResourcesRequired(AValue: boolean);
begin
  if AValue then
    Attribute['ExternalResourcesRequired'] := 'true'
  else
    Attribute['ExternalResourcesRequired'] := 'false';
end;

procedure TSVGMask.SetX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['x'] := AValue;
end;

procedure TSVGMask.SetY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['y'] := AValue;
end;

procedure TSVGMask.SetWidth(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['width'] := AValue;
end;

procedure TSVGMask.SetHeight(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['height'] := AValue;
end;

procedure TSVGMask.SetMaskUnits(AValue: TSVGObjectUnits);
begin
  if AValue = souUserSpaceOnUse then
    Attribute['maskUnits'] := 'userSpaceOnUse'
  else
    Attribute['maskUnits'] := 'objectBoundingBox';
end;

procedure TSVGMask.SetMaskContentUnits(AValue: TSVGObjectUnits);
begin
  if AValue = souUserSpaceOnUse then
    Attribute['maskContentUnits'] := 'userSpaceOnUse'
  else
    Attribute['maskContentUnits'] := 'objectBoundingBox';
end;

procedure TSVGMask.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  //todo
end;

class function TSVGMask.GetDOMTag: string;
begin
  Result:= 'mask';
end;

{ TSVGGroup }

function TSVGGroup.GetFontSize: TFloatWithCSSUnit;
begin
  result:= GetVerticalAttributeOrStyleWithUnit('font-size',Units.CurrentFontEmHeight,false);
end;

procedure TSVGGroup.SetFontSize(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['font-size'] := AValue;
end;

procedure TSVGGroup.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var
  prevFontEmHeight, fs: TFloatWithCSSUnit;
begin
  prevFontEmHeight := Units.CurrentFontEmHeight;
  fs := fontSize;
  if fs.CSSUnit in [cuFontEmHeight,cuFontXHeight] then
    fs := Units.ConvertHeight(fontSize,AUnit);
  Units.CurrentFontEmHeight:= fs;
  FContent.Draw(ACanvas2d, AUnit);
  Units.CurrentFontEmHeight:= prevFontEmHeight;
end;

{ TSVGStyle }  

constructor TSVGStyle.Create(AElement: TDOMElement;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(AElement, AUnits, ADataLink);
  Parse(AElement.TextContent);
end;

procedure TSVGStyle.Initialize;
begin
  inherited Initialize;
  Clear;
end;

class function TSVGStyle.GetDOMTag: string;
begin
  Result:= 'style';
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
    raise exception.Create(rsInvalidIndex);
end;

procedure TSVGStyle.SetStyle(const sid: integer; sr: TSVGStyleItem);
begin
  if IsValidID(sid) then
    FStyles[sid]:= sr
  else
    raise exception.Create(rsInvalidIndex);
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

class function TSVGRectangle.GetDOMTag: string;
begin
  Result:= 'rect';
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

constructor TSVGPolypoints.Create(ADocument: TDOMDocument;
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

constructor TSVGPath.Create(ADocument: TDOMDocument; AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(ADocument, AUnits, ADataLink);
  FPath := nil;
  FBoundingBoxComputed := false;
  FBoundingBox := rectF(0,0,0,0);
end;

constructor TSVGPath.Create(AElement: TDOMElement;
  AUnits: TCSSUnitConverter; ADataLink: TSVGDataLink);
begin
  inherited Create(AElement, AUnits, ADataLink);
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

class function TSVGPath.GetDOMTag: string;
begin
  Result:= 'path';
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

class function TSVGEllipse.GetDOMTag: string;
begin
  Result:= 'ellipse';
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

class function TSVGCircle.GetDOMTag: string;
begin
  Result:= 'circle';
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

class function TSVGLine.GetDOMTag: string;
begin
  Result:= 'line';
end;

{ TSVGGradient } //##

function TSVGGradient.GetHRef: string;
begin
  result := Attribute['xlink:href'];
  if result = '' then
    result := Attribute['href'];//(Note: specific for svg 2)
end;

procedure TSVGGradient.SetGradientTransform(AValue: string);
begin
  Attribute['gradientTransform'] := AValue;
end;

function TSVGGradient.GetGradientUnits: TSVGObjectUnits;
begin
  if Attribute['gradientUnits','objectBoundingBox'] = 'userSpaceOnUse' then
    result := souUserSpaceOnUse
  else
    result := souObjectBoundingBox;
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

procedure TSVGGradient.SetGradientUnits(AValue: TSVGObjectUnits);
begin
  if AValue = souUserSpaceOnUse then
    Attribute['gradientUnits'] := 'userSpaceOnUse'
  else
    Attribute['gradientUnits'] := 'objectBoundingBox';
end;

procedure TSVGGradient.SetHRef(AValue: string);
begin
  Attribute['xlink:href'] := AValue;
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

destructor TSVGGradient.Destroy;
begin
  FreeAndNil(InheritedGradients);
  inherited Destroy;
end;

procedure TSVGGradient.ScanInheritedGradients(const forceScan: boolean = false);
var
  el: TSVGGradient;
begin
  //(if list empty = not scan)
  if (InheritedGradients.Count <> 0) and (not forceScan) then
    exit;

  InheritedGradients.Clear;
  InheritedGradients.Add(Self);//(important)
  el:= Self;
  while el.hRef <> '' do
  begin
    el := TSVGGradient(FDataLink.FindElementByRef(el.hRef, TSVGGradient));
    if Assigned(el) then InheritedGradients.Add(el);
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

class function TSVGLinearGradient.GetDOMTag: string;
begin
  Result:= 'linearGradient';
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

class function TSVGRadialGradient.GetDOMTag: string;
begin
  Result:= 'radialGradient';
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

class function TSVGStopGradient.GetDOMTag: string;
begin
  Result:= 'stop';
end;

{ TSVGContent }

function TSVGContent.GetElement(AIndex: integer): TSVGElement;
begin
  result := TObject(FElements.Items[AIndex]) as TSVGElement;
end;

function TSVGContent.GetElementCount: integer;
begin
  result := FElements.Count;
end;

function TSVGContent.GetUnits: TCSSUnitConverter;
begin
  result := FUnits;
end;

function TSVGContent.TryCreateElementFromNode(ANode: TDOMNode): TObject;
begin
  if ANode is TDOMElement then
    result := CreateSVGElementFromNode(TDOMElement(ANode),FUnits,FDataLink)
  else
    result := ANode;
end;

function TSVGContent.GetIsSVGElement(AIndex: integer): boolean;
begin
  result := TObject(FElements[AIndex]) is TSVGElement;
end;

function TSVGContent.GetElementDOMNode(AIndex: integer): TDOMNode;
begin
  result := GetDOMNode(TObject(FElements[AIndex]));
end;

function TSVGContent.GetDOMNode(AElement: TObject): TDOMNode;
begin
  if AElement is TDOMNode then
    result := TDOMNode(AElement)
  else if AElement is TSVGElement then
    result := TSVGElement(AElement).DOMElement
  else
    raise exception.Create('Unexpected element type');
end;

procedure TSVGContent.AppendElement(AElement: TObject);
begin
  FDomElem.AppendChild(GetDOMNode(AElement));
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
    FDomElem.InsertBefore(GetDOMNode(AElement), GetDOMNode(ASuccessor));
  end
  else
    AppendElement(AElement);
end;

constructor TSVGContent.Create(AElement: TDOMElement; AUnits: TCSSUnitConverter;
  ADataLink: TSVGDataLink);
var cur: TDOMNode;
  elem: TObject;
begin
  FDoc := AElement.OwnerDocument;
  FDomElem := AElement;
  FDataLink := ADataLink;
  FElements := TFPList.Create;
  FUnits := AUnits;
  cur := FDomElem.FirstChild;
  while cur <> nil do
  begin
    elem := TryCreateElementFromNode(cur);
    if Assigned(elem) then FElements.Add(elem);
    cur := cur.NextSibling;
  end;
end;

destructor TSVGContent.Destroy;
var i:integer;
begin
  for i := 0 to ElementCount-1 do
    if not (TObject(FElements[i]) is TDOMNode) then TObject(FElements[i]).Free;
  FreeAndNil(FElements);
  inherited Destroy;
end;

procedure TSVGContent.Clear;
var
  i: Integer;
begin
  for i := 0 to ElementCount-1 do
    if IsSVGElement[i] then Element[i].Free;
  while Assigned(FDomElem.FirstChild) do
    FDomElem.RemoveChild(FDomElem.FirstChild);
end;

procedure TSVGContent.Recompute;
var
  i: Integer;
begin
  for i := 0 to ElementCount-1 do
    if IsSVGElement[i] then
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
    if IsSVGElement[i] then
      Element[i].Draw(ACanvas2d, AUnit);
end;

function TSVGContent.AppendElement(ASVGType: TSVGFactory): TSVGElement;
begin
  result := ASVGType.Create(FDoc,Units,FDataLink);
  AppendElement(result);
end;

function TSVGContent.AppendDOMText(AText: string): TDOMText;
begin
  result := TDOMText.Create(FDomElem.OwnerDocument);
  result.Data:= AText;
  AppendElement(result);
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
var
  a: ArrayOfTFloatWithCSSUnit;
begin
  result := TSVGText.Create(FDoc,Units,FDataLink);
  setlength(a,1);
  try
    a[0] := FloatWithCSSUnit(x,AUnit);
    result.x := a;
    a[0] := FloatWithCSSUnit(y,AUnit);
    result.y := a;
  finally
    setlength(a,0);
  end;
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

