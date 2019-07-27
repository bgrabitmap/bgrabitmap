unit BGRALayers;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  BGRAGraphics, Classes, SysUtils, Types, BGRABitmapTypes, BGRABitmap,
  BGRAMemDirectory, BGRATransform, fgl, BGRALayerOriginal;

type
  TBGRACustomLayeredBitmap = class;
  TBGRACustomLayeredBitmapClass = class of TBGRACustomLayeredBitmap;

  { TBGRALayerOriginalEntry }

  TBGRALayerOriginalEntry = record
     Guid: TGuid;
     Instance: TBGRALayerCustomOriginal;
     class operator = (const AEntry1,AEntry2: TBGRALayerOriginalEntry): boolean;
  end;

function BGRALayerOriginalEntry(AGuid: TGuid): TBGRALayerOriginalEntry;
function BGRALayerOriginalEntry(AInstance: TBGRALayerCustomOriginal): TBGRALayerOriginalEntry;

type
  TBGRALayerOriginalList = specialize TFPGList<TBGRALayerOriginalEntry>;

  TBGRALayeredBitmap = class;
  TBGRALayeredBitmapClass = class of TBGRALayeredBitmap;

  TBGRALayeredBitmapSaveToStreamProc = procedure(AStream: TStream; ALayers: TBGRACustomLayeredBitmap);
  TBGRALayeredBitmapLoadFromStreamProc = procedure(AStream: TStream; ALayers: TBGRACustomLayeredBitmap);
  TBGRALayeredBitmapCheckStreamProc = function(AStream: TStream): boolean;
  TOriginalRenderStatus = (orsNone, orsDraft, orsPartialDraft, orsProof, orsPartialProof);

  { TBGRACustomLayeredBitmap }

  TBGRACustomLayeredBitmap = class(TGraphic)
  private
    FFrozenRange: array of record
      firstLayer,lastLayer: integer;
      image: TBGRABitmap;
      linearBlend: boolean;
    end;
    FLinearBlend: boolean;
    FMemDirectory: TMemDirectory;
    FMemDirectoryOwned: boolean;
    function GetDefaultBlendingOperation: TBlendOperation;
    function GetHasMemFiles: boolean;
    function GetLinearBlend: boolean;
    procedure SetLinearBlend(AValue: boolean);

  protected
    function GetNbLayers: integer; virtual; abstract;
    function GetMemDirectory: TMemDirectory;
    function GetBlendOperation(Layer: integer): TBlendOperation; virtual; abstract;
    function GetLayerVisible(layer: integer): boolean; virtual; abstract;
    function GetLayerOpacity(layer: integer): byte; virtual; abstract;
    function GetLayerName(layer: integer): string; virtual;
    function GetLayerOffset(layer: integer): TPoint; virtual;
    function GetLayerFrozenRange(layer: integer): integer;
    function GetLayerFrozen(layer: integer): boolean; virtual;
    function GetLayerUniqueId(layer: integer): integer; virtual;
    function GetLayerOriginal({%H-}layer: integer): TBGRALayerCustomOriginal; virtual;
    function GetLayerOriginalKnown({%H-}layer: integer): boolean; virtual;
    function GetLayerOriginalMatrix({%H-}layer: integer): TAffineMatrix; virtual;
    function GetLayerOriginalGuid({%H-}layer: integer): TGuid; virtual;
    function GetLayerOriginalRenderStatus({%H-}layer: integer): TOriginalRenderStatus; virtual;
    function GetOriginalCount: integer; virtual;
    function GetOriginalByIndex({%H-}AIndex: integer): TBGRALayerCustomOriginal; virtual;
    function GetOriginalByIndexKnown({%H-}AIndex: integer): boolean; virtual;
    function GetOriginalByIndexClass({%H-}AIndex: integer): TBGRALayerOriginalAny; virtual;
    function GetTransparent: Boolean; override;
    function GetEmpty: boolean; override;

    function IndexOfOriginal(AGuid: TGuid): integer; overload; virtual;
    function IndexOfOriginal(AOriginal: TBGRALayerCustomOriginal): integer; overload; virtual;

    procedure SetWidth(Value: Integer); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetMemDirectory(AValue: TMemDirectory);
    procedure SetTransparent(Value: Boolean); override;

    procedure SetLayerFrozen(layer: integer; AValue: boolean); virtual;
    function RangeIntersect(first1,last1,first2,last2: integer): boolean;
    procedure RemoveFrozenRange(index: integer);
    function ContainsFrozenRange(first,last: integer): boolean;

  public
    procedure SaveToFile(const filenameUTF8: string); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToStreamAs(Stream: TStream; AExtension: string);
    constructor Create; override;
    destructor Destroy; override;
    function ToString: ansistring; override;
    function GetLayerBitmapDirectly(layer: integer): TBGRABitmap; virtual;
    function GetLayerBitmapCopy(layer: integer): TBGRABitmap; virtual; abstract;
    function ComputeFlatImage(ASeparateXorMask: boolean = false): TBGRABitmap; overload;
    function ComputeFlatImage(firstLayer, lastLayer: integer; ASeparateXorMask: boolean = false): TBGRABitmap; overload;
    function ComputeFlatImage(ARect: TRect; ASeparateXorMask: boolean = false): TBGRABitmap; overload;
    function ComputeFlatImage(ARect: TRect; firstLayer, lastLayer: integer; ASeparateXorMask: boolean = false): TBGRABitmap; overload;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override; overload;
    procedure Draw(Canvas: TCanvas; x,y: integer); overload;
    procedure Draw(Canvas: TCanvas; x,y: integer; firstLayer, lastLayer: integer); overload;
    procedure Draw(Dest: TBGRABitmap; x,y: integer); overload;
    procedure Draw(Dest: TBGRABitmap; x,y: integer; ASeparateXorMask: boolean); overload;
    procedure Draw(Dest: TBGRABitmap; AX,AY: integer; firstLayer, lastLayer: integer; ASeparateXorMask: boolean = false); overload;

    procedure FreezeExceptOneLayer(layer: integer); overload;
    procedure Freeze(firstLayer, lastLayer: integer); overload;
    procedure Freeze; overload;
    procedure Unfreeze; overload;
    procedure Unfreeze(layer: integer); overload;
    procedure Unfreeze(firstLayer, lastLayer: integer); overload;

    procedure NotifyLoaded; virtual;
    procedure NotifySaving; virtual;

    property NbLayers: integer read GetNbLayers;
    property BlendOperation[layer: integer]: TBlendOperation read GetBlendOperation;
    property LayerVisible[layer: integer]: boolean read GetLayerVisible;
    property LayerOpacity[layer: integer]: byte read GetLayerOpacity;
    property LayerName[layer: integer]: string read GetLayerName;
    property LayerOffset[layer: integer]: TPoint read GetLayerOffset;
    property LayerFrozen[layer: integer]: boolean read GetLayerFrozen;
    property LayerUniqueId[layer: integer]: integer read GetLayerUniqueId;
    property LayerOriginal[layer: integer]: TBGRALayerCustomOriginal read GetLayerOriginal;
    property LayerOriginalKnown[layer: integer]: boolean read GetLayerOriginalKnown;
    property LayerOriginalGuid[layer: integer]: TGuid read GetLayerOriginalGuid;
    property LayerOriginalMatrix[layer: integer]: TAffineMatrix read GetLayerOriginalMatrix;
    property LayerOriginalRenderStatus[layer: integer]: TOriginalRenderStatus read GetLayerOriginalRenderStatus;
    property LinearBlend: boolean read GetLinearBlend write SetLinearBlend; //use linear blending unless specified
    property DefaultBlendingOperation: TBlendOperation read GetDefaultBlendingOperation;
    property MemDirectory: TMemDirectory read GetMemDirectory write SetMemDirectory;
    property MemDirectoryOwned: boolean read FMemDirectoryOwned write FMemDirectoryOwned;
    property HasMemFiles: boolean read GetHasMemFiles;
  end;

  TEmbeddedOriginalChangeEvent = procedure (ASender: TObject; AOriginal: TBGRALayerCustomOriginal) of object;
  TEmbeddedOriginalEditingChangeEvent = procedure (ASender: TObject; AOriginal: TBGRALayerCustomOriginal) of object;

  TBGRALayerInfo = record
    UniqueId: integer;
    Name: string;
    x, y: integer;
    Source: TBGRABitmap;
    blendOp: TBlendOperation;
    Opacity: byte;
    Visible: boolean;
    Owner: boolean;
    Frozen: boolean;
    OriginalMatrix: TAffineMatrix;
    OriginalRenderStatus: TOriginalRenderStatus;
    OriginalGuid: TGuid;
    OriginalInvalidatedBounds: TRectF;
  end;

  { TBGRALayeredBitmap }

  TBGRALayeredBitmap = class(TBGRACustomLayeredBitmap)
  private
    FNbLayers: integer;
    FLayers: array of TBGRALayerInfo;
    FOnEditorFocusChanged: TNotifyEvent;
    FEditorFocused: boolean;
    FOriginalChange: TEmbeddedOriginalChangeEvent;
    FOriginalEditingChange: TEmbeddedOriginalEditingChangeEvent;
    FWidth,FHeight: integer;
    FOriginals: TBGRALayerOriginalList;
    FOriginalEditor: TBGRAOriginalEditor;
    FOriginalEditorOriginal: TBGRALayerCustomOriginal;
    FOriginalEditorViewMatrix: TAffineMatrix;
    procedure EditorFocusedChanged(Sender: TObject);
    function GetLayerOriginalClass(layer: integer): TBGRALayerOriginalAny;
    function GetOriginalGuid(AIndex: integer): TGUID;
    procedure SetEditorFocused(AValue: boolean);

  protected
    function GetWidth: integer; override;
    function GetHeight: integer; override;
    function GetNbLayers: integer; override;
    function GetBlendOperation(Layer: integer): TBlendOperation; override;
    function GetLayerVisible(layer: integer): boolean; override;
    function GetLayerOpacity(layer: integer): byte; override;
    function GetLayerOffset(layer: integer): TPoint; override;
    function GetLayerName(layer: integer): string; override;
    function GetLayerFrozen(layer: integer): boolean; override;
    function GetLayerUniqueId(layer: integer): integer; override;
    function GetLayerOriginal(layer: integer): TBGRALayerCustomOriginal; override;
    function GetLayerOriginalKnown(layer: integer): boolean; override;
    function GetLayerOriginalMatrix(layer: integer): TAffineMatrix; override;
    function GetLayerOriginalGuid(layer: integer): TGuid; override;
    function GetLayerOriginalRenderStatus(layer: integer): TOriginalRenderStatus; override;
    function GetOriginalCount: integer; override;
    function GetOriginalByIndex(AIndex: integer): TBGRALayerCustomOriginal; override;
    function GetOriginalByIndexKnown(AIndex: integer): boolean; override;
    function GetOriginalByIndexClass(AIndex: integer): TBGRALayerOriginalAny; override;
    procedure SetBlendOperation(Layer: integer; op: TBlendOperation);
    procedure SetLayerVisible(layer: integer; AValue: boolean);
    procedure SetLayerOpacity(layer: integer; AValue: byte);
    procedure SetLayerOffset(layer: integer; AValue: TPoint);
    procedure SetLayerName(layer: integer; AValue: string);
    procedure SetLayerFrozen(layer: integer; AValue: boolean); override;
    procedure SetLayerUniqueId(layer: integer; AValue: integer);
    procedure SetLayerOriginalMatrix(layer: integer; AValue: TAffineMatrix);
    procedure SetLayerOriginalGuid(layer: integer; const AValue: TGuid);
    procedure SetLayerOriginalRenderStatus(layer: integer; AValue: TOriginalRenderStatus);

    procedure FindOriginal(AGuid: TGuid;
                out ADir: TMemDirectory;
                out AClass: TBGRALayerOriginalAny);
    procedure StoreOriginal(AOriginal: TBGRALayerCustomOriginal);
    procedure OriginalChange(ASender: TObject; ABounds: PRectF = nil);
    procedure OriginalEditingChange(ASender: TObject);
    function GetLayerDirectory(layer: integer): TMemDirectory;

  public
    procedure LoadFromFile(const filenameUTF8: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure LoadFromResource(AFilename: string);
    procedure SetSize(AWidth, AHeight: integer); virtual;
    procedure Clear; override;
    procedure ClearOriginals;
    procedure RemoveLayer(index: integer);
    procedure InsertLayer(index: integer; fromIndex: integer);
    procedure Assign(ASource: TBGRACustomLayeredBitmap; ASharedLayerIds: boolean = false); overload;
    function MoveLayerUp(index: integer): integer;
    function MoveLayerDown(index: integer): integer;

    function AddLayer(Source: TBGRABitmap; Opacity: byte = 255): integer; overload;
    function AddLayer(Source: TBGRABitmap; Position: TPoint; BlendOp: TBlendOperation; Opacity: byte = 255; Shared: boolean = false): integer; overload;
    function AddLayer(Source: TBGRABitmap; Position: TPoint; Opacity: byte = 255): integer; overload;
    function AddLayer(Source: TBGRABitmap; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayer(AName: string; Source: TBGRABitmap; Opacity: byte = 255): integer; overload;
    function AddLayer(AName: string; Source: TBGRABitmap; Position: TPoint; BlendOp: TBlendOperation; Opacity: byte = 255; Shared: boolean = false): integer; overload;
    function AddLayer(AName: string; Source: TBGRABitmap; Position: TPoint; Opacity: byte = 255): integer; overload;
    function AddLayer(AName: string; Source: TBGRABitmap; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddSharedLayer(Source: TBGRABitmap; Opacity: byte = 255): integer; overload;
    function AddSharedLayer(Source: TBGRABitmap; Position: TPoint; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddSharedLayer(Source: TBGRABitmap; Position: TPoint; Opacity: byte = 255): integer; overload;
    function AddSharedLayer(Source: TBGRABitmap; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromFile(AFileName: string; Opacity: byte = 255): integer; overload;
    function AddLayerFromFile(AFileName: string; Position: TPoint; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromFile(AFileName: string; Position: TPoint; Opacity: byte = 255): integer; overload;
    function AddLayerFromFile(AFileName: string; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddOwnedLayer(ABitmap: TBGRABitmap; Opacity: byte = 255): integer; overload;
    function AddOwnedLayer(ABitmap: TBGRABitmap; Position: TPoint; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddOwnedLayer(ABitmap: TBGRABitmap; Position: TPoint; Opacity: byte = 255): integer; overload;
    function AddOwnedLayer(ABitmap: TBGRABitmap; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromOriginal(const AGuid: TGuid; Opacity: byte = 255): integer; overload;
    function AddLayerFromOriginal(const AGuid: TGuid; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromOriginal(const AGuid: TGuid; Matrix: TAffineMatrix; Opacity: byte = 255): integer; overload;
    function AddLayerFromOriginal(const AGuid: TGuid; Matrix: TAffineMatrix; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; Matrix: TAffineMatrix; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; Matrix: TAffineMatrix; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;

    function AddOriginal(AOriginal: TBGRALayerCustomOriginal; AOwned: boolean = true): integer;
    function AddOriginalFromStream(AStream: TStream; ALateLoad: boolean = false): integer; overload;
    function AddOriginalFromStream(AStream: TStream; const AGuid: TGuid; ALateLoad: boolean = false): integer; overload;
    function AddOriginalFromStorage(AStorage: TBGRAMemOriginalStorage; ALateLoad: boolean = false): integer; overload;
    function AddOriginalFromStorage(AStorage: TBGRAMemOriginalStorage; const AGuid: TGuid; ALateLoad: boolean = false): integer; overload;
    procedure SaveOriginalToStream(AIndex: integer; AStream: TStream); overload;
    procedure SaveOriginalToStream(const AGuid: TGuid; AStream: TStream); overload;
    function RemoveOriginal(AOriginal: TBGRALayerCustomOriginal): boolean;
    procedure DeleteOriginal(AIndex: integer);
    procedure NotifyLoaded; override;
    procedure NotifySaving; override;
    procedure RenderLayerFromOriginal(layer: integer; ADraft: boolean = false; AFullSizeLayer: boolean = false); overload;
    procedure RenderLayerFromOriginal(layer: integer; ADraft: boolean; ARenderBounds: TRect; AFullSizeLayer: boolean = false); overload;
    procedure RenderLayerFromOriginal(layer: integer; ADraft: boolean; ARenderBoundsF: TRectF; AFullSizeLayer: boolean = false); overload;
    procedure RenderLayerFromOriginalIfNecessary(layer: integer; ADraft: boolean; var ABounds: TRect);
    function RenderOriginalsIfNecessary(ADraft: boolean = false): TRect;
    function RenderOriginalIfNecessary(const AGuid: TGuid; ADraft: boolean = false): TRect;
    procedure RemoveUnusedOriginals;

    destructor Destroy; override;
    constructor Create; overload; override;
    constructor Create(AWidth, AHeight: integer); overload; virtual;
    function GetLayerBitmapDirectly(layer: integer): TBGRABitmap; override;
    function GetLayerBitmapCopy(layer: integer): TBGRABitmap; override;
    function GetLayerIndexFromId(AIdentifier: integer): integer;
    function Duplicate(ASharedLayerIds: boolean = false): TBGRALayeredBitmap;
    function ProduceLayerUniqueId: integer;

    procedure RotateCW;
    procedure RotateCCW;
    procedure RotateUD; overload;
    procedure RotateUD(ALayerIndex: integer); overload;
    procedure HorizontalFlip; overload;
    procedure HorizontalFlip(ALayerIndex: integer); overload;
    procedure VerticalFlip; overload;
    procedure VerticalFlip(ALayerIndex: integer); overload;
    procedure Resample(AWidth, AHeight: integer; AResampleMode: TResampleMode; AFineResampleFilter: TResampleFilter = rfLinear);
    procedure SetLayerBitmap(layer: integer; ABitmap: TBGRABitmap; AOwned: boolean);
    function TakeLayerBitmap(layer: integer): TBGRABitmap;
    procedure ApplyLayerOffset(ALayerIndex: integer; APadWithTranparentPixels: boolean);

    function DrawEditor(ADest: TBGRABitmap; ALayerIndex: integer; X, Y: Integer; APointSize: single): TRect; overload;
    function DrawEditor(ADest: TBGRABitmap; ALayerIndex: integer; AMatrix: TAffineMatrix; APointSize: single): TRect; overload;
    function GetEditorBounds(ALayerIndex: integer; X, Y: Integer; APointSize: single): TRect; overload;
    function GetEditorBounds(ADestRect: TRect; ALayerIndex: integer; X, Y: Integer; APointSize: single): TRect; overload;
    function GetEditorBounds(ALayerIndex: integer; AMatrix: TAffineMatrix; APointSize: single): TRect; overload;
    function GetEditorBounds(ADestRect: TRect; ALayerIndex: integer; AMatrix: TAffineMatrix; APointSize: single): TRect; overload;
    procedure MouseMove(Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor);
    procedure MouseDown(RightButton: boolean; Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor);
    procedure MouseUp(RightButton: boolean; Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor);
    procedure MouseMove(Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor; out AHandled: boolean);
    procedure MouseDown(RightButton: boolean; Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor; out AHandled: boolean);
    procedure MouseUp(RightButton: boolean; Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor; out AHandled: boolean);
    procedure KeyDown(Shift: TShiftState; Key: TSpecialKey; out AHandled: boolean);
    procedure KeyUp(Shift: TShiftState; Key: TSpecialKey; out AHandled: boolean);
    procedure KeyPress(UTF8Key: string; out AHandled: boolean);

    property Width : integer read GetWidth;
    property Height: integer read GetHeight;
    property NbLayers: integer read GetNbLayers;
    property BlendOperation[layer: integer]: TBlendOperation read GetBlendOperation write SetBlendOperation;
    property LayerVisible[layer: integer]: boolean read GetLayerVisible write SetLayerVisible;
    property LayerOpacity[layer: integer]: byte read GetLayerOpacity write SetLayerOpacity;
    property LayerName[layer: integer]: string read GetLayerName write SetLayerName;
    property LayerBitmap[layer: integer]: TBGRABitmap read GetLayerBitmapDirectly;
    property LayerOffset[layer: integer]: TPoint read GetLayerOffset write SetLayerOffset;
    property LayerUniqueId[layer: integer]: integer read GetLayerUniqueId write SetLayerUniqueId;
    property LayerOriginal[layer: integer]: TBGRALayerCustomOriginal read GetLayerOriginal;
    property LayerOriginalKnown[layer: integer]: boolean read GetLayerOriginalKnown;
    property LayerOriginalClass[layer: integer]: TBGRALayerOriginalAny read GetLayerOriginalClass;
    property LayerOriginalGuid[layer: integer]: TGuid read GetLayerOriginalGuid write SetLayerOriginalGuid;
    property LayerOriginalMatrix[layer: integer]: TAffineMatrix read GetLayerOriginalMatrix write SetLayerOriginalMatrix;
    property LayerOriginalRenderStatus[layer: integer]: TOriginalRenderStatus read GetLayerOriginalRenderStatus write SetLayerOriginalRenderStatus;

    function IndexOfOriginal(AGuid: TGuid): integer; overload; override;
    function IndexOfOriginal(AOriginal: TBGRALayerCustomOriginal): integer; overload; override;
    property OriginalCount: integer read GetOriginalCount;
    property Original[AIndex: integer]: TBGRALayerCustomOriginal read GetOriginalByIndex;
    property OriginalGuid[AIndex: integer]: TGUID read GetOriginalGuid;
    property OriginalKnown[AIndex: integer]: boolean read GetOriginalByIndexKnown;
    property OriginalClass[AIndex: integer]: TBGRALayerOriginalAny read GetOriginalByIndexClass;
    property OnOriginalChange: TEmbeddedOriginalChangeEvent read FOriginalChange write FOriginalChange;
    property OnOriginalEditingChange: TEmbeddedOriginalEditingChangeEvent read FOriginalEditingChange write FOriginalEditingChange;
    property EditorFocused: boolean read FEditorFocused write SetEditorFocused;
    property OnEditorFocusChanged: TNotifyEvent read FOnEditorFocusChanged write FOnEditorFocusChanged;
    property OriginalEditor: TBGRAOriginalEditor read FOriginalEditor;
  end;

  TAffineMatrix = BGRABitmapTypes.TAffineMatrix;

procedure RegisterLayeredBitmapWriter(AExtensionUTF8: string; AWriter: TBGRALayeredBitmapClass);
procedure RegisterLayeredBitmapReader(AExtensionUTF8: string; AReader: TBGRACustomLayeredBitmapClass);
function TryCreateLayeredBitmapWriter(AExtensionUTF8: string): TBGRALayeredBitmap;
function TryCreateLayeredBitmapReader(AExtensionUTF8: string): TBGRACustomLayeredBitmap;

var
  LayeredBitmapSaveToStreamProc : TBGRALayeredBitmapSaveToStreamProc;
  LayeredBitmapLoadFromStreamProc : TBGRALayeredBitmapLoadFromStreamProc;
  LayeredBitmapCheckStreamProc: TBGRALayeredBitmapCheckStreamProc;

type
  TOnLayeredBitmapLoadStartProc = procedure(AFilenameUTF8: string) of object;
  TOnLayeredBitmapLoadProgressProc = procedure(APercentage: integer) of object;
  TOnLayeredBitmapLoadedProc = procedure() of object;

procedure OnLayeredBitmapLoadFromStreamStart;
procedure OnLayeredBitmapLoadStart(AFilenameUTF8: string);
procedure OnLayeredBitmapLoadProgress(APercentage: integer);
procedure OnLayeredBitmapLoaded();
procedure RegisterLoadingHandler(AStart: TOnLayeredBitmapLoadStartProc; AProgress: TOnLayeredBitmapLoadProgressProc;
     ADone: TOnLayeredBitmapLoadedProc);
procedure UnregisterLoadingHandler(AStart: TOnLayeredBitmapLoadStartProc; AProgress: TOnLayeredBitmapLoadProgressProc;
     ADone: TOnLayeredBitmapLoadedProc);

implementation

uses BGRAUTF8, BGRABlend, BGRAMultiFileType, math;

const
  OriginalsDirectory = 'originals';
  LayersDirectory = 'layers';
  RenderSubDirectory = 'render';

var
  OnLayeredBitmapLoadStartProc: TOnLayeredBitmapLoadStartProc;
  OnLayeredBitmapLoadProgressProc: TOnLayeredBitmapLoadProgressProc;
  OnLayeredBitmapLoadedProc: TOnLayeredBitmapLoadedProc;

var
  NextLayerUniqueId: cardinal;
  LayeredBitmapReaders: array of record
     extension: string;
     theClass: TBGRACustomLayeredBitmapClass;
  end;
  LayeredBitmapWriters: array of record
     extension: string;
     theClass: TBGRALayeredBitmapClass;
  end;

{ TBGRALayerOriginalEntry }

class operator TBGRALayerOriginalEntry.=(const AEntry1,
  AEntry2: TBGRALayerOriginalEntry): boolean;
begin
  result := AEntry1.Guid = AEntry2.Guid;
end;

function BGRALayerOriginalEntry(AGuid: TGuid): TBGRALayerOriginalEntry;
begin
  result.Guid := AGuid;
  result.Instance := nil;
end;

function BGRALayerOriginalEntry(AInstance: TBGRALayerCustomOriginal): TBGRALayerOriginalEntry;
begin
  result.Guid := AInstance.Guid;
  result.Instance := AInstance;
end;

{ TBGRALayeredBitmap }

function TBGRALayeredBitmap.GetLayerUniqueId(layer: integer): integer;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    Result:= FLayers[layer].UniqueId;
end;

function TBGRALayeredBitmap.GetLayerOriginal(layer: integer): TBGRALayerCustomOriginal;
var
  idxOrig: Integer;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].OriginalGuid = GUID_NULL then exit(nil);
    idxOrig := IndexOfOriginal(FLayers[layer].OriginalGuid);
    if idxOrig = -1 then exit(nil);
    result := Original[idxOrig];
  end;
end;

function TBGRALayeredBitmap.GetLayerOriginalMatrix(layer: integer
  ): TAffineMatrix;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    result := FLayers[layer].OriginalMatrix;
end;

function TBGRALayeredBitmap.GetLayerOriginalGuid(layer: integer): TGuid;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    result := FLayers[layer].OriginalGuid;
end;

function TBGRALayeredBitmap.GetLayerOriginalRenderStatus(layer: integer
  ): TOriginalRenderStatus;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    result := FLayers[layer].OriginalRenderStatus;
end;

procedure TBGRALayeredBitmap.SetLayerUniqueId(layer: integer; AValue: integer);
var i: integer;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    for i := 0 to NbLayers-1 do
      if (i <> layer) and (FLayers[i].UniqueId = AValue) then
        raise Exception.Create('Another layer has the same identifier');
    FLayers[layer].UniqueId := AValue;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerOriginalMatrix(layer: integer;
  AValue: TAffineMatrix);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].OriginalMatrix = AValue then exit;
    FLayers[layer].OriginalMatrix := AValue;
    if FLayers[layer].OriginalGuid <> GUID_NULL then
    begin
      FLayers[layer].OriginalRenderStatus := orsNone;
      Unfreeze(layer);
    end;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerOriginalGuid(layer: integer;
  const AValue: TGuid);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].OriginalGuid = AValue then exit;
    FLayers[layer].OriginalGuid := AValue;

    if (AValue <> GUID_NULL) and (IndexOfOriginal(AValue) <> -1) then
    begin
      FLayers[layer].OriginalRenderStatus := orsNone;
      Unfreeze(layer);
    end;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerOriginalRenderStatus(layer: integer;
  AValue: TOriginalRenderStatus);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].OriginalRenderStatus = AValue then exit;
    FLayers[layer].OriginalRenderStatus := AValue;
    Unfreeze(layer);
  end;
end;

procedure TBGRALayeredBitmap.FindOriginal(AGuid: TGuid; out
  ADir: TMemDirectory; out AClass: TBGRALayerOriginalAny);
var
  c: String;
begin
  ADir := nil;
  AClass := nil;

  if HasMemFiles then
  begin
    ADir := MemDirectory.FindPath(OriginalsDirectory+'/'+GUIDToString(AGuid));
    if ADir <> nil then
    begin
      c := ADir.RawStringByFilename['class'];
      AClass := FindLayerOriginalClass(c);
    end;
  end;
end;

procedure TBGRALayeredBitmap.StoreOriginal(AOriginal: TBGRALayerCustomOriginal);
var
  dir, subdir: TMemDirectory;
  storage: TBGRAMemOriginalStorage;
begin
  if AOriginal.Guid = GUID_NULL then raise exception.Create('Original GUID undefined');
  dir := MemDirectory.Directory[MemDirectory.AddDirectory(OriginalsDirectory)];
  subdir := dir.Directory[dir.AddDirectory(GUIDToString(AOriginal.Guid))];
  storage := TBGRAMemOriginalStorage.Create(subdir);
  try
    AOriginal.SaveToStorage(storage);
    storage.RawString['class'] := AOriginal.StorageClassName;
  finally
    storage.Free;
  end;
end;

procedure TBGRALayeredBitmap.OriginalChange(ASender: TObject; ABounds: PRectF);
var
  i: Integer;
  orig: TBGRALayerCustomOriginal;
  transfBounds: TRectF;
begin
  orig := TBGRALayerCustomOriginal(ASender);
  if not (Assigned(ABounds) and IsEmptyRectF(ABounds^)) then
  begin
    for i := 0 to NbLayers-1 do
      if LayerOriginalGuid[i] = orig.Guid then
      begin
        if ABounds = nil then
          LayerOriginalRenderStatus[i] := orsNone
        else
        begin
          transfBounds := (LayerOriginalMatrix[i]*TAffineBox.AffineBox(ABounds^)).RectBoundsF;
          case LayerOriginalRenderStatus[i] of
          orsDraft: begin
                      LayerOriginalRenderStatus[i] := orsPartialDraft;
                      FLayers[i].OriginalInvalidatedBounds := transfBounds;
                    end;
          orsProof: begin
                      LayerOriginalRenderStatus[i] := orsPartialProof;
                      FLayers[i].OriginalInvalidatedBounds := transfBounds;
                    end;
          orsPartialDraft: FLayers[i].OriginalInvalidatedBounds :=
                             FLayers[i].OriginalInvalidatedBounds.Union(transfBounds, true);
          orsPartialProof: FLayers[i].OriginalInvalidatedBounds :=
                             FLayers[i].OriginalInvalidatedBounds.Union(transfBounds, true);
          end;
        end;
      end;
  end;
  if Assigned(FOriginalChange) then
    FOriginalChange(self, orig);
end;

procedure TBGRALayeredBitmap.OriginalEditingChange(ASender: TObject);
var
  orig: TBGRALayerCustomOriginal;
begin
  orig := TBGRALayerCustomOriginal(ASender);
  if Assigned(FOriginalEditingChange) then
    FOriginalEditingChange(self, orig);
end;

function TBGRALayeredBitmap.GetLayerDirectory(layer: integer): TMemDirectory;
var
  layersDir: TMemDirectory;
  id: LongInt;
begin
  layersDir := MemDirectory.Directory[MemDirectory.AddDirectory(LayersDirectory)];
  id := LayerUniqueId[layer];
  result := layersDir.Directory[layersDir.AddDirectory(IntToStr(id))];
end;

function TBGRALayeredBitmap.GetOriginalCount: integer;
begin
  if Assigned(FOriginals) then
    result := FOriginals.Count
  else
    result := 0;
end;

function TBGRALayeredBitmap.GetOriginalByIndex(AIndex: integer
  ): TBGRALayerCustomOriginal;
var
  dir: TMemDirectory;
  c: TBGRALayerOriginalAny;
  guid: TGuid;
  storage: TBGRAMemOriginalStorage;
begin
  if (AIndex < 0) or (AIndex >= OriginalCount) then
    raise ERangeError.Create('Index out of bounds');

  result := FOriginals[AIndex].Instance;
  guid := FOriginals[AIndex].Guid;

  // load original on the fly
  if (result = nil) and (guid <> GUID_NULL) then
  begin
    FindOriginal(guid, dir, c);
    if not Assigned(dir) then
      raise exception.Create('Original directory not found');
    if not Assigned(c) then
      raise exception.Create('Original class not found (it can be registered with the RegisterLayerOriginal function)');

    result := c.Create;
    result.Guid := guid;
    storage := TBGRAMemOriginalStorage.Create(dir);
    try
      result.LoadFromStorage(storage);
      FOriginals[AIndex] := BGRALayerOriginalEntry(result);
      result.OnChange:= @OriginalChange;
      result.OnEditingChange:= @OriginalEditingChange;
    except
      on ex: exception do
      begin
        FreeAndNil(result);
        storage.Free;
        raise exception.Create(ex.Message);
      end;
    end;
    storage.Free;
  end;
end;

function TBGRALayeredBitmap.GetLayerOriginalKnown(layer: integer): boolean;
var
  idxOrig: Integer;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].OriginalGuid = GUID_NULL then exit(true);
    idxOrig := IndexOfOriginal(FLayers[layer].OriginalGuid);
    if idxOrig = -1 then exit(false);
    result := OriginalKnown[idxOrig];
  end;
end;

function TBGRALayeredBitmap.GetOriginalByIndexKnown(AIndex: integer): boolean;
var
  dir: TMemDirectory;
  c: TBGRALayerOriginalAny;
  guid: TGuid;
begin
  if (AIndex < 0) or (AIndex >= OriginalCount) then
    raise ERangeError.Create('Index out of bounds');

  if Assigned(FOriginals[AIndex].Instance) then exit(true);
  guid := FOriginals[AIndex].Guid;
  if guid = GUID_NULL then exit(true);

  FindOriginal(guid, dir, c);
  result:= Assigned(dir) and Assigned(c);
end;

function TBGRALayeredBitmap.GetOriginalGuid(AIndex: integer): TGUID;
begin
  if (AIndex < 0) or (AIndex >= OriginalCount) then
    raise ERangeError.Create('Index out of bounds');

  result := FOriginals[AIndex].Guid;
end;

procedure TBGRALayeredBitmap.SetEditorFocused(AValue: boolean);
begin
  if Assigned(FOriginalEditor) then FOriginalEditor.Focused := AValue
  else
  begin
    if FEditorFocused=AValue then Exit;
    FEditorFocused:=AValue;
    if Assigned(FOnEditorFocusChanged) then FOnEditorFocusChanged(self);
  end;
end;

function TBGRALayeredBitmap.GetLayerOriginalClass(layer: integer): TBGRALayerOriginalAny;
var
  idxOrig: Integer;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].OriginalGuid = GUID_NULL then exit(nil);
    idxOrig := IndexOfOriginal(FLayers[layer].OriginalGuid);
    if idxOrig = -1 then exit(nil);
    result := OriginalClass[idxOrig];
  end;
end;

procedure TBGRALayeredBitmap.EditorFocusedChanged(Sender: TObject);
begin
  if Assigned(FOriginalEditor) then
  begin
    FEditorFocused := FOriginalEditor.Focused;
    if Assigned(FOnEditorFocusChanged) then FOnEditorFocusChanged(self);
  end;
end;

function TBGRALayeredBitmap.GetOriginalByIndexClass(AIndex: integer): TBGRALayerOriginalAny;
var
  dir: TMemDirectory;
  c: TBGRALayerOriginalAny;
  guid: TGuid;
begin
  if (AIndex < 0) or (AIndex >= OriginalCount) then
    raise ERangeError.Create('Index out of bounds');

  if Assigned(FOriginals[AIndex].Instance) then exit(TBGRALayerOriginalAny(FOriginals[AIndex].Instance.ClassType));
  guid := FOriginals[AIndex].Guid;
  if guid = GUID_NULL then exit(nil);

  FindOriginal(guid, dir, c);
  result:= c;
end;

function TBGRALayeredBitmap.GetWidth: integer;
begin
  Result:= FWidth;
end;

function TBGRALayeredBitmap.GetHeight: integer;
begin
  Result:= FHeight;
end;

function TBGRALayeredBitmap.GetNbLayers: integer;
begin
  Result:= FNbLayers;
end;

function TBGRALayeredBitmap.GetBlendOperation(Layer: integer): TBlendOperation;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    Result:= FLayers[layer].blendOp;
end;

function TBGRALayeredBitmap.GetLayerVisible(layer: integer): boolean;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    Result:= FLayers[layer].Visible;
end;

function TBGRALayeredBitmap.GetLayerOpacity(layer: integer): byte;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    Result:= FLayers[layer].Opacity;
end;

function TBGRALayeredBitmap.GetLayerOffset(layer: integer): TPoint;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    with FLayers[layer] do
      Result:= Point(x,y);
end;

function TBGRALayeredBitmap.GetLayerName(layer: integer): string;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if not FLayers[layer].Owner and (FLayers[layer].Source <> nil) then
      Result := FLayers[layer].Source.Caption
    else
      Result:= FLayers[layer].Name;
    if Result = '' then
      result := inherited GetLayerName(layer);
  end;
end;

function TBGRALayeredBitmap.GetLayerFrozen(layer: integer): boolean;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    Result:= FLayers[layer].Frozen;
end;

procedure TBGRALayeredBitmap.SetBlendOperation(Layer: integer;
  op: TBlendOperation);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].blendOp <> op then
    begin
      FLayers[layer].blendOp := op;
      Unfreeze(layer);
    end;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerVisible(layer: integer; AValue: boolean);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].Visible <> AValue then
    begin
      FLayers[layer].Visible := AValue;
      Unfreeze(layer);
    end;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerOpacity(layer: integer; AValue: byte);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].Opacity <> AValue then
    begin
      FLayers[layer].Opacity := AValue;
      Unfreeze(layer);
    end;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerOffset(layer: integer; AValue: TPoint);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if (FLayers[layer].x <> AValue.x) or
      (FLayers[layer].y <> AValue.y) then
    begin
      if FLayers[layer].OriginalGuid <> GUID_NULL then
        raise exception.Create('The offset of the layer is computed from an original. You can change it by changing the layer original matrix.');

      FLayers[layer].x := AValue.x;
      FLayers[layer].y := AValue.y;
      Unfreeze(layer);
    end;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerName(layer: integer; AValue: string);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if not FLayers[layer].Owner and (FLayers[layer].Source <> nil) then
      FLayers[layer].Source.Caption := AValue
    else
      FLayers[layer].Name := AValue;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerFrozen(layer: integer; AValue: boolean);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    FLayers[layer].Frozen := AValue;
end;

function TBGRALayeredBitmap.GetLayerBitmapDirectly(layer: integer): TBGRABitmap;
begin
  if (layer < 0) or (layer >= NbLayers) then
    result := nil
  else
  begin
    if FLayers[layer].OriginalRenderStatus = orsNone then
      RenderLayerFromOriginal(layer, true)
    else if FLayers[layer].OriginalRenderStatus in [orsPartialDraft,orsPartialProof] then
      RenderLayerFromOriginal(layer, true, FLayers[layer].OriginalInvalidatedBounds);
    Result:= FLayers[layer].Source;
  end;
end;

procedure TBGRALayeredBitmap.LoadFromFile(const filenameUTF8: string);
var bmp: TBGRABitmap;
    ext: string;
    temp: TBGRACustomLayeredBitmap;
    i: integer;
    stream: TFileStreamUTF8;
begin
  ext := UTF8LowerCase(ExtractFileExt(filenameUTF8));
  for i := 0 to high(LayeredBitmapReaders) do
    if '.'+LayeredBitmapReaders[i].extension = ext then
    begin
      temp := LayeredBitmapReaders[i].theClass.Create;
      try
        temp.LoadFromFile(filenameUTF8);
        Assign(temp);
      finally
        temp.Free;
      end;
      exit;
    end;

  //when using "data" extension, simply deserialize
  if (ext='.dat') or (ext='.data') then
  begin
    if Assigned(LayeredBitmapLoadFromStreamProc) then
    begin
      stream := TFileStreamUTF8.Create(filenameUTF8, fmOpenRead, fmShareDenyWrite);
      try
        LayeredBitmapLoadFromStreamProc(stream, self);
      finally
        stream.Free;
      end;
    end else
      raise exception.Create('Enable layer deserialization by calling BGRAStreamLayers.RegisterStreamLayers');
  end else
  begin
    bmp := TBGRABitmap.Create(filenameUTF8, True);
    Clear;
    SetSize(bmp.Width,bmp.Height);
    AddOwnedLayer(bmp);
  end;
end;

procedure TBGRALayeredBitmap.LoadFromStream(stream: TStream);
var bmp: TBGRABitmap;
begin
  if Assigned(LayeredBitmapLoadFromStreamProc) then
  begin
    if not Assigned(LayeredBitmapCheckStreamProc) or
      LayeredBitmapCheckStreamProc(stream) then
    begin
      LayeredBitmapLoadFromStreamProc(Stream, self);
      exit;
    end;
  end;

  bmp := TBGRABitmap.Create(stream);
  Clear;
  SetSize(bmp.Width,bmp.Height);
  AddOwnedLayer(bmp);
end;

procedure TBGRALayeredBitmap.LoadFromResource(AFilename: string);
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

procedure TBGRALayeredBitmap.SetSize(AWidth, AHeight: integer);
begin
  Unfreeze;
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TBGRALayeredBitmap.Clear;
var i: integer;
begin
  Unfreeze;
  for i := NbLayers-1 downto 0 do
    RemoveLayer(i);
  MemDirectory := nil;
  ClearOriginals;
end;

procedure TBGRALayeredBitmap.ClearOriginals;
var
  i: Integer;
begin
  if Assigned(FOriginals) then
  begin
    for i := 0 to OriginalCount-1 do
      FOriginals[i].Instance.Free;
    FreeAndNil(FOriginals);
  end;
end;

procedure TBGRALayeredBitmap.RemoveLayer(index: integer);
var i: integer;
  id: LongInt;
  layersDir: TMemDirectory;
begin
  if (index < 0) or (index >= NbLayers) then exit;
  Unfreeze;
  if Assigned(FMemDirectory) then
  begin
    id := LayerUniqueId[index];
    layersDir := FMemDirectory.Directory[FMemDirectory.AddDirectory(LayersDirectory)];
    layersDir.Delete(IntToStr(id),'');
  end;
  if FLayers[index].Owner then FLayers[index].Source.Free;
  for i := index to FNbLayers-2 do
    FLayers[i] := FLayers[i+1];
  Dec(FNbLayers);
end;

procedure TBGRALayeredBitmap.InsertLayer(index: integer; fromIndex: integer);
var info: TBGRALayerInfo;
    i: integer;
begin
  if (index < 0) or (index > NbLayers) or (index = fromIndex) then exit;
  if (fromIndex < 0) or (fromIndex >= NbLayers) then exit;
  Unfreeze;
  info := FLayers[fromIndex];
  for i := fromIndex to FNbLayers-2 do
    FLayers[i] := FLayers[i+1];
  for i := FNbLayers-1 downto index+1 do
    FLayers[i] := FLayers[i-1];
  FLayers[index] := info;
end;

procedure TBGRALayeredBitmap.Assign(ASource: TBGRACustomLayeredBitmap; ASharedLayerIds: boolean);
var i,idx,idxOrig,idxNewOrig: integer;
    usedOriginals: array of record
       used: boolean;
       sourceGuid,newGuid: TGuid;
    end;
    orig: TBGRALayerCustomOriginal;
    stream: TMemoryStream;

begin
  if ASource = nil then
    raise exception.Create('Unexpected nil reference');
  Clear;
  SetSize(ASource.Width,ASource.Height);
  LinearBlend:= ASource.LinearBlend;
  setlength(usedOriginals, ASource.GetOriginalCount);
  for idxOrig := 0 to high(usedOriginals) do
  with usedOriginals[idxOrig] do
  begin
    used:= false;
    newGuid := GUID_NULL;
  end;
  for i := 0 to ASource.NbLayers-1 do
  if (ASource.LayerOriginalGuid[i]<>GUID_NULL) and
     (ASource.LayerOriginalKnown[i] or (ASource is TBGRALayeredBitmap)) then
  begin
    idxOrig := ASource.IndexOfOriginal(ASource.LayerOriginalGuid[i]);
    if not usedOriginals[idxOrig].used then
    begin
      if ASource.LayerOriginalKnown[i] then
      begin
        orig := ASource.GetOriginalByIndex(idxOrig);
        idxNewOrig := AddOriginal(orig, false);
        usedOriginals[idxOrig].sourceGuid := orig.Guid;
      end else
      begin
        stream := TMemoryStream.Create;
        (ASource as TBGRALayeredBitmap).SaveOriginalToStream(idxOrig, stream);
        stream.Position:= 0;
        idxNewOrig := AddOriginalFromStream(stream,true);
        stream.Free;
        usedOriginals[idxOrig].sourceGuid := (ASource as TBGRALayeredBitmap).OriginalGuid[idxOrig];
      end;
      usedOriginals[idxOrig].newGuid := OriginalGuid[idxNewOrig];
      usedOriginals[idxOrig].used := true;
    end;
  end;
  for i := 0 to ASource.NbLayers-1 do
  begin
    idx := AddOwnedLayer(ASource.GetLayerBitmapCopy(i),ASource.LayerOffset[i],ASource.BlendOperation[i],ASource.LayerOpacity[i]);
    LayerName[idx] := ASource.LayerName[i];
    LayerVisible[idx] := ASource.LayerVisible[i];
    if ASharedLayerIds and (ASource is TBGRALayeredBitmap) then
      LayerUniqueId[idx] := TBGRALayeredBitmap(ASource).LayerUniqueId[i];
    for idxOrig := 0 to high(usedOriginals) do
      if usedOriginals[idxOrig].sourceGuid = ASource.LayerOriginalGuid[i] then
      begin
        LayerOriginalGuid[idx] := usedOriginals[idxOrig].newGuid;
        LayerOriginalMatrix[idx] := ASource.LayerOriginalMatrix[i];
        LayerOriginalRenderStatus[idx] := ASource.LayerOriginalRenderStatus[i];
        break;
      end;
  end;
end;

function TBGRALayeredBitmap.MoveLayerUp(index: integer): integer;
begin
  if (index >= 0) and (index <= NbLayers-2) then
  begin
    InsertLayer(index+1,index);
    result := index+1;
  end else
    result := -1;
end;

function TBGRALayeredBitmap.MoveLayerDown(index: integer): integer;
begin
  if (index > 0) and (index <= NbLayers-1) then
  begin
    InsertLayer(index-1,index);
    result := index-1;
  end else
    result := -1;
end;

function TBGRALayeredBitmap.AddLayer(Source: TBGRABitmap; Opacity: byte
  ): integer;
begin
  result := AddLayer(Source, Point(0,0), DefaultBlendingOperation, Opacity, False);
end;

function TBGRALayeredBitmap.AddLayer(Source: TBGRABitmap; Position: TPoint;
  BlendOp: TBlendOperation; Opacity: byte; Shared: boolean): integer;
begin
  result := AddLayer(Source.Caption,Source,Position,BlendOp,Opacity,Shared);
end;

function TBGRALayeredBitmap.AddLayer(Source: TBGRABitmap; Position: TPoint;
  Opacity: byte): integer;
begin
  result := AddLayer(Source,Position,DefaultBlendingOperation,Opacity);
end;

function TBGRALayeredBitmap.AddLayer(Source: TBGRABitmap;
  BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddLayer(Source,Point(0,0),BlendOp,Opacity);
end;

function TBGRALayeredBitmap.AddLayer(AName: string; Source: TBGRABitmap;
  Opacity: byte): integer;
begin
  result := AddLayer(AName,Source,Point(0,0),Opacity);
end;

function TBGRALayeredBitmap.AddLayer(AName: string; Source: TBGRABitmap;
  Position: TPoint; BlendOp: TBlendOperation; Opacity: byte; Shared: boolean): integer;
begin
  if length(FLayers) = FNbLayers then
    setlength(FLayers, length(FLayers)*2+1);
  FLayers[FNbLayers].Name := AName;
  FLayers[FNbLayers].X := Position.X;
  FLayers[FNbLayers].Y := Position.Y;
  FLayers[FNbLayers].blendOp := BlendOp;
  FLayers[FNbLayers].Opacity := Opacity;
  FLayers[FNbLayers].Visible := true;
  FLayers[FNbLayers].Frozen := false;
  FLayers[FNbLayers].UniqueId := ProduceLayerUniqueId;
  FLayers[FNbLayers].OriginalMatrix := AffineMatrixIdentity;
  FLayers[FNbLayers].OriginalRenderStatus := orsNone;
  FLayers[FNbLayers].OriginalGuid := GUID_NULL;
  if Shared then
  begin
    FLayers[FNbLayers].Source := Source;
    FLayers[FNbLayers].Owner := false;
  end else
  begin
    FLayers[FNbLayers].Source := Source.Duplicate as TBGRABitmap;
    FLayers[FNbLayers].Owner := true;
  end;
  result := FNbLayers;
  inc(FNbLayers);
  if (FNbLayers = 1) and (FWidth = 0) and (FHeight = 0) and (Source <> nil) then
    SetSize(Source.Width,Source.Height);
end;

function TBGRALayeredBitmap.AddLayer(AName: string; Source: TBGRABitmap;
  Position: TPoint; Opacity: byte): integer;
begin
  result := AddLayer(AName, Source, Position, DefaultBlendingOperation, Opacity);
end;

function TBGRALayeredBitmap.AddLayer(AName: string; Source: TBGRABitmap;
  BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddLayer(AName, Source, Point(0,0), blendOp, Opacity);
end;

function TBGRALayeredBitmap.AddSharedLayer(Source: TBGRABitmap; Opacity: byte
  ): integer;
begin
  result := AddSharedLayer(Source, Point(0,0), DefaultBlendingOperation, Opacity);
end;

function TBGRALayeredBitmap.AddSharedLayer(Source: TBGRABitmap;
  Position: TPoint; BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddLayer(Source, Position, BlendOp, Opacity, True);
end;

function TBGRALayeredBitmap.AddSharedLayer(Source: TBGRABitmap;
  Position: TPoint; Opacity: byte): integer;
begin
  result := AddSharedLayer(Source, Position, DefaultBlendingOperation, Opacity);
end;

function TBGRALayeredBitmap.AddSharedLayer(Source: TBGRABitmap;
  BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddSharedLayer(Source, Point(0,0), blendOp, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromFile(AFileName: string; Opacity: byte
  ): integer;
begin
  result := AddOwnedLayer(TBGRABitmap.Create(AFilename),Opacity);
  FLayers[result].Name := ExtractFileName(AFilename);
end;

function TBGRALayeredBitmap.AddLayerFromFile(AFileName: string;
  Position: TPoint; BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddOwnedLayer(TBGRABitmap.Create(AFilename),Position,BlendOp,Opacity);
  FLayers[result].Name := ExtractFileName(AFilename);
end;

function TBGRALayeredBitmap.AddLayerFromFile(AFileName: string;
  Position: TPoint; Opacity: byte): integer;
begin
  result := AddOwnedLayer(TBGRABitmap.Create(AFilename),Position,Opacity);
  FLayers[result].Name := ExtractFileName(AFilename);
end;

function TBGRALayeredBitmap.AddLayerFromFile(AFileName: string;
  BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddOwnedLayer(TBGRABitmap.Create(AFilename),BlendOp,Opacity);
  FLayers[result].Name := ExtractFileName(AFilename);
end;

function TBGRALayeredBitmap.AddOwnedLayer(ABitmap: TBGRABitmap; Opacity: byte
  ): integer;
begin
  result := AddSharedLayer(ABitmap,Opacity);
  FLayers[result].Owner := True;
end;

function TBGRALayeredBitmap.AddOwnedLayer(ABitmap: TBGRABitmap;
  Position: TPoint; BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddSharedLayer(ABitmap,Position,BlendOp,Opacity);
  FLayers[result].Owner := True;
end;

function TBGRALayeredBitmap.AddOwnedLayer(ABitmap: TBGRABitmap;
  Position: TPoint; Opacity: byte): integer;
begin
  result := AddSharedLayer(ABitmap,Position,Opacity);
  FLayers[result].Owner := True;
end;

function TBGRALayeredBitmap.AddOwnedLayer(ABitmap: TBGRABitmap;
  BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddSharedLayer(ABitmap,BlendOp,Opacity);
  FLayers[result].Owner := True;
end;

function TBGRALayeredBitmap.AddLayerFromOriginal(const AGuid: TGuid;
  Opacity: byte): integer;
begin
  result := AddLayerFromOriginal(AGuid, DefaultBlendingOperation, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromOriginal(const AGuid: TGuid;
  BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddLayerFromOriginal(AGuid, AffineMatrixIdentity, BlendOp, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromOriginal(const AGuid: TGuid;
  Matrix: TAffineMatrix; Opacity: byte): integer;
begin
  result := AddLayerFromOriginal(AGuid, Matrix, DefaultBlendingOperation, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromOriginal(const AGuid: TGuid;
  Matrix: TAffineMatrix; BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddOwnedLayer(TBGRABitmap.Create, BlendOp, Opacity);
  LayerOriginalGuid[result] := AGuid;
  LayerOriginalMatrix[result] := Matrix;
  if not Assigned(LayerOriginal[result]) then
    raise exception.Create('Original data or class not found');
end;

function TBGRALayeredBitmap.AddLayerFromOwnedOriginal(
  AOriginal: TBGRALayerCustomOriginal; Opacity: byte): integer;
begin
  if IndexOfOriginal(AOriginal) = -1 then AddOriginal(AOriginal);
  result := AddLayerFromOriginal(AOriginal.Guid, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromOwnedOriginal(
  AOriginal: TBGRALayerCustomOriginal; BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  if IndexOfOriginal(AOriginal) = -1 then AddOriginal(AOriginal);
  result := AddLayerFromOriginal(AOriginal.Guid, BlendOp, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromOwnedOriginal(
  AOriginal: TBGRALayerCustomOriginal; Matrix: TAffineMatrix; Opacity: byte): integer;
begin
  if IndexOfOriginal(AOriginal) = -1 then AddOriginal(AOriginal);
  result := AddLayerFromOriginal(AOriginal.Guid, Matrix, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromOwnedOriginal(
  AOriginal: TBGRALayerCustomOriginal; Matrix: TAffineMatrix;
  BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  if IndexOfOriginal(AOriginal) = -1 then AddOriginal(AOriginal);
  result := AddLayerFromOriginal(AOriginal.Guid, Matrix, BlendOp, Opacity);
end;

function TBGRALayeredBitmap.AddOriginal(AOriginal: TBGRALayerCustomOriginal; AOwned: boolean): integer;
var
  newGuid: TGuid;
begin
  if AOriginal = nil then
    raise exception.Create('Unexpected nil reference');;
  if AOriginal.Guid = GUID_NULL then
  begin
    if CreateGUID(newGuid)<> 0 then
    begin
      if AOwned then AOriginal.Free;
      raise exception.Create('Error while creating GUID');
    end;
    AOriginal.Guid := newGuid;
  end else
  begin
    if IndexOfOriginal(AOriginal) <> -1 then
    begin
      if AOwned then AOriginal.Free;
      raise exception.Create('Original already added');
    end;
    if IndexOfOriginal(AOriginal.Guid) <> -1 then
    begin
      if AOwned then AOriginal.Free;
      raise exception.Create('GUID is already in use');
    end;
  end;
  StoreOriginal(AOriginal);
  if FOriginals = nil then FOriginals := TBGRALayerOriginalList.Create;
  if AOwned then
  begin
    result := FOriginals.Add(BGRALayerOriginalEntry(AOriginal));
    AOriginal.OnChange:= @OriginalChange;
    AOriginal.OnEditingChange:= @OriginalEditingChange;
  end
  else
    result := FOriginals.Add(BGRALayerOriginalEntry(AOriginal.Guid));
end;

function TBGRALayeredBitmap.AddOriginalFromStream(AStream: TStream;
  ALateLoad: boolean): integer;
var
  newGuid: TGUID;
begin
  if CreateGUID(newGuid)<> 0 then raise exception.Create('Error while creating GUID');
  result := AddOriginalFromStream(AStream, newGuid, ALateLoad);
end;


function TBGRALayeredBitmap.AddOriginalFromStream(AStream: TStream;
  const AGuid: TGuid; ALateLoad: boolean): integer;
var
  storage: TBGRAMemOriginalStorage;
begin
  storage:= TBGRAMemOriginalStorage.Create;
  storage.LoadFromStream(AStream);
  try
    result := AddOriginalFromStorage(storage, AGuid, ALateLoad);
  finally
    storage.Free;
  end;
end;

function TBGRALayeredBitmap.AddOriginalFromStorage(AStorage: TBGRAMemOriginalStorage; ALateLoad: boolean): integer;
var
  newGuid: TGUID;
begin
  if CreateGUID(newGuid)<> 0 then raise exception.Create('Error while creating GUID');
  result := AddOriginalFromStorage(AStorage, newGuid, ALateLoad);
end;

function TBGRALayeredBitmap.AddOriginalFromStorage(
  AStorage: TBGRAMemOriginalStorage; const AGuid: TGuid; ALateLoad: boolean): integer;
var
  origClassName: String;
  origClass: TBGRALayerOriginalAny;
  orig: TBGRALayerCustomOriginal;
  dir, subdir: TMemDirectory;
begin
  result := -1;
  origClassName := AStorage.RawString['class'];
  if origClassName = '' then raise Exception.Create('Original class name not defined');
  if ALateLoad then
  begin
    if IndexOfOriginal(AGuid)<>-1 then
      raise exception.Create('Duplicate GUID');

    dir := MemDirectory.Directory[MemDirectory.AddDirectory(OriginalsDirectory)];
    subdir := dir.Directory[dir.AddDirectory(GUIDToString(AGuid))];
    AStorage.CopyTo(subdir);

    if FOriginals = nil then FOriginals := TBGRALayerOriginalList.Create;
    result := FOriginals.Add(BGRALayerOriginalEntry(AGuid));
  end else
  begin
    origClass := FindLayerOriginalClass(origClassName);
    if origClass = nil then raise exception.Create('Original class not found (it can be registered with the RegisterLayerOriginal function)');
    orig := origClass.Create;
    try
      orig.LoadFromStorage(AStorage);
      orig.Guid := AGuid;
      result := AddOriginal(orig, true);
    except on ex:exception do
      begin
        orig.Free;
        raise exception.Create('Error loading original. '+ ex.Message);
      end;
    end;
  end;
end;

procedure TBGRALayeredBitmap.SaveOriginalToStream(AIndex: integer;
  AStream: TStream);
var
  dir: TMemDirectory;
  c: TBGRALayerOriginalAny;
begin
  if (AIndex < 0) or (AIndex >= OriginalCount) then
    raise ERangeError.Create('Index out of bounds');

  if Assigned(FOriginals[AIndex].Instance) then
    FOriginals[AIndex].Instance.SaveToStream(AStream)
  else
  begin
    FindOriginal(FOriginals[AIndex].Guid, dir, c);
    if dir = nil then
      raise exception.Create('Originals directory not found');
    dir.SaveToStream(AStream);
  end;
end;

procedure TBGRALayeredBitmap.SaveOriginalToStream(const AGuid: TGuid;
  AStream: TStream);
var
  idxOrig: Integer;
begin
  idxOrig := IndexOfOriginal(AGuid);
  if idxOrig = -1 then raise exception.Create('Original not found');
  SaveOriginalToStream(idxOrig, AStream);
end;

function TBGRALayeredBitmap.RemoveOriginal(AOriginal: TBGRALayerCustomOriginal): boolean;
var
  idx: Integer;
begin
  idx := IndexOfOriginal(AOriginal);
  if idx = -1 then exit(false);
  DeleteOriginal(idx);
  result := true;
end;

procedure TBGRALayeredBitmap.DeleteOriginal(AIndex: integer);
var
  dir: TMemDirectory;
  i: Integer;
  guid: TGuid;
begin
  if (AIndex < 0) or (AIndex >= OriginalCount) then
    raise ERangeError.Create('Index out of bounds');

  guid := FOriginals[AIndex].Guid;
  for i := 0 to NbLayers-1 do
    if LayerOriginalGuid[i] = guid then
    begin
      LayerOriginalGuid[i] := GUID_NULL;
      LayerOriginalMatrix[i] := AffineMatrixIdentity;
    end;

  dir := MemDirectory.Directory[MemDirectory.AddDirectory(OriginalsDirectory)];
  dir.Delete(GUIDToString(guid),'');

  FOriginals[AIndex].Instance.Free;
  FOriginals.Delete(AIndex); //AOriginals freed
end;

procedure TBGRALayeredBitmap.NotifyLoaded;
var
  foundGuid: array of TGuid;
  nbFoundGuid: integer;

  procedure AddGuid(const AGuid: TGuid);
  begin
    foundGuid[nbFoundGuid] := AGuid;
    inc(nbFoundGuid);
  end;

  function IndexOfGuid(AGuid: TGuid): integer;
  var
    i: Integer;
  begin
    for i := 0 to nbFoundGuid-1 do
      if foundGuid[i] = AGuid then exit(i);
    result := -1;
  end;

var
  i: Integer;
  dir: TMemDirectory;
  newGuid: TGUID;

begin
  inherited NotifyLoaded;

  //if there are no files in memory, we are sure that there are no originals
  if not HasMemFiles then
  begin
    ClearOriginals;
    exit;
  end;

  //determine list of GUID of originals
  dir := MemDirectory.Directory[MemDirectory.AddDirectory(OriginalsDirectory)];
  setlength(foundGuid, dir.Count);
  nbFoundGuid:= 0;
  for i := 0 to dir.Count-1 do
    if dir.IsDirectory[i] and (dir.Entry[i].Extension = '') then
    begin
      if TryStringToGUID(dir.Entry[i].Name, newGuid) then
        AddGuid(newGuid);
    end;

  //remove originals that do not exist anymore
  for i := OriginalCount-1 downto 0 do
    if IndexOfGuid(FOriginals[i].Guid) = -1 then
      DeleteOriginal(i);

  //add originals from memory directory
  for i := 0 to nbFoundGuid-1 do
  begin
    if IndexOfOriginal(foundGuid[i]) = -1 then
    begin
      if FOriginals = nil then FOriginals := TBGRALayerOriginalList.Create;
      FOriginals.Add(BGRALayerOriginalEntry(foundGuid[i]));
    end;
  end;
end;

procedure TBGRALayeredBitmap.NotifySaving;
var
  i: Integer;
begin
  inherited NotifySaving;

  RenderOriginalsIfNecessary;

  for i := 0 to OriginalCount-1 do
    if Assigned(FOriginals[i].Instance) then
      StoreOriginal(FOriginals[i].Instance);
end;

procedure TBGRALayeredBitmap.RenderLayerFromOriginal(layer: integer;
  ADraft: boolean; AFullSizeLayer: boolean = false);
begin
  RenderLayerFromOriginal(layer, ADraft, rectF(0,0,Width,Height), AFullSizeLayer);
end;

procedure TBGRALayeredBitmap.RenderLayerFromOriginal(layer: integer;
  ADraft: boolean; ARenderBounds: TRect; AFullSizeLayer: boolean = false);
var
  orig: TBGRALayerCustomOriginal;
  rAll, rNewBounds, rInterRender: TRect;
  newSource: TBGRABitmap;
  layerDir, renderDir: TMemDirectory;

  procedure FreeSource;
  begin
    if FLayers[layer].Owner then
      FreeAndNil(FLayers[layer].Source)
    else
      FLayers[layer].Source := nil;
  end;

begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds');

  orig := LayerOriginal[layer];
  if Assigned(orig) then
  begin
    Unfreeze(layer);
    layerDir := GetLayerDirectory(layer);
    renderDir := layerDir.Directory[layerDir.AddDirectory(RenderSubDirectory)];
    orig.RenderStorage := TBGRAMemOriginalStorage.Create(renderDir);

    rAll := rect(0,0,Width,Height);
    if AFullSizeLayer then
      rNewBounds := rAll
    else
    begin
      rNewBounds := orig.GetRenderBounds(rAll,FLayers[layer].OriginalMatrix);
      IntersectRect({%H-}rNewBounds, rNewBounds, rAll);
    end;
    IntersectRect({%H-}rInterRender, ARenderBounds, rNewBounds);
    if (FLayers[layer].x = rNewBounds.Left) and
      (FLayers[layer].y = rNewBounds.Top) and
      (FLayers[layer].Source.Width = rNewBounds.Width) and
      (FLayers[layer].Source.Height = rNewBounds.Height) then
    begin
      OffsetRect(rInterRender, -rNewBounds.Left, -rNewBounds.Top);
      FLayers[layer].Source.FillRect(rInterRender, BGRAPixelTransparent, dmSet);
      FLayers[layer].Source.ClipRect := rInterRender;
      orig.Render(FLayers[layer].Source, Point(-rNewBounds.Left,-rNewBounds.Top), FLayers[layer].OriginalMatrix, ADraft);
      FLayers[layer].Source.NoClip;
    end else
    begin
      if rInterRender = rNewBounds then
      begin
        FreeSource;
        newSource := TBGRABitmap.Create(rNewBounds.Width,rNewBounds.Height);
        orig.Render(newSource, Point(-rNewBounds.Left,-rNewBounds.Top), FLayers[layer].OriginalMatrix, ADraft);
      end else
      begin
        newSource := TBGRABitmap.Create(rNewBounds.Width,rNewBounds.Height);
        newSource.PutImage(FLayers[layer].x - rNewBounds.Left, FLayers[layer].y - rNewBounds.Top, FLayers[layer].Source, dmSet);
        FreeSource;
        OffsetRect(rInterRender, -rNewBounds.Left, -rNewBounds.Top);
        if not IsRectEmpty(rInterRender) then
        begin
          newSource.FillRect(rInterRender, BGRAPixelTransparent, dmSet);
          newSource.ClipRect := rInterRender;
          orig.Render(newSource, Point(-rNewBounds.Left,-rNewBounds.Top), FLayers[layer].OriginalMatrix, ADraft);
          newSource.NoClip;
        end;
      end;
      FLayers[layer].Source := newSource;
      FLayers[layer].x := rNewBounds.Left;
      FLayers[layer].y := rNewBounds.Top;
    end;

    orig.RenderStorage.AffineMatrix['last-matrix'] := FLayers[layer].OriginalMatrix;
    orig.RenderStorage.Free;
    orig.renderStorage := nil;
    if renderDir.Count = 1 then //only matrix
      layerDir.Delete(RenderSubDirectory,'');
  end;
  if ADraft then
    FLayers[layer].OriginalRenderStatus := orsDraft
  else
    FLayers[layer].OriginalRenderStatus := orsProof;
  FLayers[layer].OriginalInvalidatedBounds := EmptyRectF;
end;

procedure TBGRALayeredBitmap.RenderLayerFromOriginal(layer: integer;
  ADraft: boolean; ARenderBoundsF: TRectF; AFullSizeLayer: boolean = false);
var
  r: TRect;
begin
  with ARenderBoundsF do
    r := Rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
  RenderLayerFromOriginal(layer, ADraft, r, AFullSizeLayer);
end;

procedure TBGRALayeredBitmap.RenderLayerFromOriginalIfNecessary(layer: integer;
  ADraft: boolean; var ABounds: TRect);
  procedure UnionLayerArea(ALayer: integer);
  var
    r: TRect;
  begin
    if (FLayers[ALayer].Source = nil) or
      (FLayers[ALayer].Source.Width = 0) or
      (FLayers[ALayer].Source.Height = 0) then exit;

    r := RectWithSize(LayerOffset[ALayer].X, LayerOffset[ALayer].Y,
                      FLayers[ALayer].Source.Width, FLayers[ALayer].Source.Height);
    if IsRectEmpty(ABounds) then ABounds := r else
      UnionRect(ABounds,ABounds,r);
  end;

var
  r: TRect;

begin
  case LayerOriginalRenderStatus[layer] of
  orsNone:
       begin
         UnionLayerArea(layer);
         RenderLayerFromOriginal(layer, ADraft);
         UnionLayerArea(layer);
       end;
  orsDraft: if not ADraft then
       begin
         UnionLayerArea(layer);
         RenderLayerFromOriginal(layer, ADraft);
         UnionLayerArea(layer);
       end;
  orsPartialDraft,orsPartialProof:
       if not ADraft and (LayerOriginalRenderStatus[layer] = orsPartialDraft) then
       begin
         UnionLayerArea(layer);
         RenderLayerFromOriginal(layer, ADraft, rect(0,0,Width,Height), true);
         UnionLayerArea(layer);
       end
       else
       begin
         with FLayers[layer].OriginalInvalidatedBounds do
           r := Rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
         RenderLayerFromOriginal(layer, ADraft, r, true);
         if not IsRectEmpty(r) then
         begin
           if IsRectEmpty(ABounds) then
             ABounds := r
           else
             UnionRect(ABounds, ABounds, r);
         end;
       end;
  end;
end;

function TBGRALayeredBitmap.RenderOriginalsIfNecessary(ADraft: boolean): TRect;
var
  i: Integer;
begin
  result:= EmptyRect;
  for i := 0 to NbLayers-1 do
    RenderLayerFromOriginalIfNecessary(i, ADraft, result);
end;

function TBGRALayeredBitmap.RenderOriginalIfNecessary(const AGuid: TGuid;
  ADraft: boolean): TRect;
var
  i: Integer;
begin
  result:= EmptyRect;
  for i := 0 to NbLayers-1 do
    if LayerOriginalGuid[i] = AGuid then
      RenderLayerFromOriginalIfNecessary(i, ADraft, result);
end;

procedure TBGRALayeredBitmap.RemoveUnusedOriginals;
var useCount: array of integer;
  i, idxOrig: Integer;
begin
  if OriginalCount = 0 then exit;
  setlength(useCount, OriginalCount);
  for i := 0 to NbLayers-1 do
  begin
    idxOrig := IndexOfOriginal(LayerOriginalGuid[i]);
    if idxOrig <> -1 then inc(useCount[idxOrig]);
  end;
  for i := high(useCount) downto 0 do
    if useCount[i] = 0 then DeleteOriginal(i);
end;

destructor TBGRALayeredBitmap.Destroy;
begin
  FOriginalEditor.Free;
  inherited Destroy;
end;

constructor TBGRALayeredBitmap.Create;
begin
  inherited Create;
  FWidth := 0;
  FHeight := 0;
  FNbLayers:= 0;
  FOriginals := nil;
end;

constructor TBGRALayeredBitmap.Create(AWidth, AHeight: integer);
begin
  inherited Create;
  if AWidth < 0 then
    FWidth := 0
  else
    FWidth := AWidth;
  if AHeight < 0 then
    FHeight := 0
  else
    FHeight := AHeight;
  FNbLayers:= 0;
end;

function TBGRALayeredBitmap.GetLayerBitmapCopy(layer: integer): TBGRABitmap;
begin
  result := GetLayerBitmapDirectly(layer).Duplicate as TBGRABitmap;
end;

function TBGRALayeredBitmap.GetLayerIndexFromId(AIdentifier: integer): integer;
var i: integer;
begin
  for i := 0 to NbLayers-1 do
    if FLayers[i].UniqueId = AIdentifier then
    begin
      result := i;
      exit;
    end;
  result := -1; //not found
end;

function TBGRALayeredBitmap.Duplicate(ASharedLayerIds: boolean): TBGRALayeredBitmap;
begin
  result := TBGRALayeredBitmap.Create;
  result.Assign(self, ASharedLayerIds);
end;

function TBGRALayeredBitmap.ProduceLayerUniqueId: integer;
begin
  result := InterLockedIncrement(NextLayerUniqueId);
end;

procedure TBGRALayeredBitmap.RotateCW;
var i: integer;
  newBmp: TBGRABitmap;
  newOfs: TPointF;
  m: TAffineMatrix;
begin
  SetSize(Height,Width); //unfreeze
  m := AffineMatrixTranslation(Width,0)*AffineMatrixRotationDeg(90);
  for i := 0 to NbLayers-1 do
  begin
    newOfs:= m*PointF(FLayers[i].x,FLayers[i].y+FLayers[i].Source.Height);
    newBmp := FLayers[i].Source.RotateCW as TBGRABitmap;
    if FLayers[i].Owner then FreeAndNil(FLayers[i].Source);
    FLayers[i].Source := newBmp;
    FLayers[i].Owner := true;
    FLayers[i].x := round(newOfs.x);
    FLayers[i].y := round(newOfs.y);
    FLayers[i].OriginalMatrix := m*FLayers[i].OriginalMatrix;
  end;
end;

procedure TBGRALayeredBitmap.RotateCCW;
var i: integer;
  newBmp: TBGRABitmap;
  newOfs: TPointF;
  m: TAffineMatrix;
begin
  SetSize(Height,Width); //unfreeze
  m := AffineMatrixTranslation(0,Height)*AffineMatrixRotationDeg(-90);
  for i := 0 to NbLayers-1 do
  begin
    newOfs:= m*PointF(FLayers[i].x+FLayers[i].Source.Width,FLayers[i].y);
    newBmp := FLayers[i].Source.RotateCCW as TBGRABitmap;
    if FLayers[i].Owner then FreeAndNil(FLayers[i].Source);
    FLayers[i].Source := newBmp;
    FLayers[i].Owner := true;
    FLayers[i].x := round(newOfs.x);
    FLayers[i].y := round(newOfs.y);
    FLayers[i].OriginalMatrix := m*FLayers[i].OriginalMatrix;
  end;
end;

procedure TBGRALayeredBitmap.RotateUD;
var i: integer;
begin
  Unfreeze;
  for i := 0 to NbLayers-1 do
    RotateUD(i);
end;

procedure TBGRALayeredBitmap.RotateUD(ALayerIndex: integer);
begin
  if (ALayerIndex < 0) or (ALayerIndex >= NbLayers) then
    raise ERangeError.Create('Index out of bounds');
  Unfreeze(ALayerIndex);
  if FLayers[ALayerIndex].Owner then
    FLayers[ALayerIndex].Source.RotateUDInplace
  else
  begin
    FLayers[ALayerIndex].Source := FLayers[ALayerIndex].Source.RotateUD as TBGRABitmap;
    FLayers[ALayerIndex].Owner := true;
  end;
  FLayers[ALayerIndex].x := Width-FLayers[ALayerIndex].x-FLayers[ALayerIndex].Source.Width;
  FLayers[ALayerIndex].y := Height-FLayers[ALayerIndex].y-FLayers[ALayerIndex].Source.Height;
  FLayers[ALayerIndex].OriginalMatrix := AffineMatrixTranslation(+Width/2,+Height/2)*AffineMatrixScale(-1,-1)*AffineMatrixTranslation(-Width/2,-Height/2)*FLayers[ALayerIndex].OriginalMatrix;
end;

procedure TBGRALayeredBitmap.HorizontalFlip;
var i: integer;
begin
  Unfreeze;
  for i := 0 to NbLayers-1 do
    HorizontalFlip(i);
end;

procedure TBGRALayeredBitmap.HorizontalFlip(ALayerIndex: integer);
begin
  if (ALayerIndex < 0) or (ALayerIndex >= NbLayers) then
    raise ERangeError.Create('Index out of bounds');
  Unfreeze(ALayerIndex);
  if FLayers[ALayerIndex].Owner then
    FLayers[ALayerIndex].Source.HorizontalFlip
  else
  begin
    FLayers[ALayerIndex].Source := FLayers[ALayerIndex].Source.Duplicate(True) as TBGRABitmap;
    FLayers[ALayerIndex].Source.HorizontalFlip;
    FLayers[ALayerIndex].Owner := true;
  end;
  FLayers[ALayerIndex].x := Width-FLayers[ALayerIndex].x-FLayers[ALayerIndex].Source.Width;
  FLayers[ALayerIndex].OriginalMatrix := AffineMatrixTranslation(+Width/2,0)*AffineMatrixScale(-1,1)*AffineMatrixTranslation(-Width/2,0)*FLayers[ALayerIndex].OriginalMatrix;
end;

procedure TBGRALayeredBitmap.VerticalFlip;
var i: integer;
begin
  Unfreeze;
  for i := 0 to NbLayers-1 do
    VerticalFlip(i);
end;

procedure TBGRALayeredBitmap.VerticalFlip(ALayerIndex: integer);
begin
  if (ALayerIndex < 0) or (ALayerIndex >= NbLayers) then
    raise ERangeError.Create('Index out of bounds');
  Unfreeze(ALayerIndex);
  if FLayers[ALayerIndex].Owner then
    FLayers[ALayerIndex].Source.VerticalFlip
  else
  begin
    FLayers[ALayerIndex].Source := FLayers[ALayerIndex].Source.Duplicate(True) as TBGRABitmap;
    FLayers[ALayerIndex].Source.VerticalFlip;
    FLayers[ALayerIndex].Owner := true;
  end;
  FLayers[ALayerIndex].y := Height-FLayers[ALayerIndex].y-FLayers[ALayerIndex].Source.Height;
  FLayers[ALayerIndex].OriginalMatrix := AffineMatrixTranslation(0,+Height/2)*AffineMatrixScale(1,-1)*AffineMatrixTranslation(0,-Height/2)*FLayers[ALayerIndex].OriginalMatrix;
end;

procedure TBGRALayeredBitmap.Resample(AWidth, AHeight: integer;
  AResampleMode: TResampleMode; AFineResampleFilter: TResampleFilter);
var i, prevWidth, prevHeight: integer;
    resampled: TBGRABitmap;
    oldFilter : TResampleFilter;
begin
  if (AWidth < 0) or (AHeight < 0) then
    raise exception.Create('Invalid size');
  prevWidth := Width;
  if prevWidth < 1 then prevWidth := AWidth;
  prevHeight := Height;
  if prevHeight < 1 then prevHeight := AHeight;
  SetSize(AWidth, AHeight); //unfreeze
  for i := 0 to NbLayers-1 do
  if (FLayers[i].OriginalGuid <> GUID_NULL) and LayerOriginalKnown[i] then
    LayerOriginalMatrix[i] := AffineMatrixScale(AWidth/prevWidth,AHeight/prevHeight)*LayerOriginalMatrix[i]
  else
  begin
    oldFilter := LayerBitmap[i].ResampleFilter;
    LayerBitmap[i].ResampleFilter := AFineResampleFilter;
    resampled := LayerBitmap[i].Resample(AWidth,AHeight, AResampleMode) as TBGRABitmap;
    LayerBitmap[i].ResampleFilter := oldFilter;
    SetLayerBitmap(i, resampled, True);
  end;
  if AResampleMode = rmFineResample then RenderOriginalsIfNecessary;
end;

procedure TBGRALayeredBitmap.SetLayerBitmap(layer: integer;
  ABitmap: TBGRABitmap; AOwned: boolean);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if ABitmap = FLayers[layer].Source then exit;
    Unfreeze(layer);
    if FLayers[layer].Owner then FLayers[layer].Source.Free;
    FLayers[layer].Source := ABitmap;
    FLayers[layer].Owner := AOwned;
    FLayers[layer].OriginalGuid := GUID_NULL;
    FLayers[layer].OriginalMatrix := AffineMatrixIdentity;
  end;
end;

function TBGRALayeredBitmap.TakeLayerBitmap(layer: integer): TBGRABitmap;
begin
  result := GetLayerBitmapDirectly(layer);
  if Assigned(result) then
  begin
    if FLayers[layer].Owner then FLayers[layer].Owner := false
    else result := result.Duplicate as TBGRABitmap;
  end;
end;

procedure TBGRALayeredBitmap.ApplyLayerOffset(ALayerIndex: integer;
  APadWithTranparentPixels: boolean);
var
  r: TRect;
  newBmp: TBGRABitmap;
begin
  if APadWithTranparentPixels then
  begin
    if (LayerOffset[ALayerIndex].X=0) and (LayerOffset[ALayerIndex].Y=0) and
       (LayerBitmap[ALayerIndex].Width=Width) and (LayerBitmap[ALayerIndex].Height=Height) then exit;
    newBmp := TBGRABitmap.Create(Width,Height);
    newBmp.PutImage(LayerOffset[ALayerIndex].X, LayerOffset[ALayerIndex].Y, LayerBitmap[ALayerIndex], dmSet);
    if FLayers[ALayerIndex].Owner then FLayers[ALayerIndex].Source.Free;
    FLayers[ALayerIndex].Source := newBmp;
    FLayers[ALayerIndex].Owner := true;
    FLayers[ALayerIndex].x := 0;
    FLayers[ALayerIndex].y := 0;
  end else
  begin
    if (LayerOffset[ALayerIndex].X>=0) and (LayerOffset[ALayerIndex].Y>=0) and
       (LayerOffset[ALayerIndex].X+LayerBitmap[ALayerIndex].Width <= Width) and
       (LayerOffset[ALayerIndex].Y+LayerBitmap[ALayerIndex].Height <= Height) then exit;
    r := RectWithSize(LayerOffset[ALayerIndex].X, LayerOffset[ALayerIndex].Y,
                      LayerBitmap[ALayerIndex].Width, LayerBitmap[ALayerIndex].Height);
    IntersectRect(r, r, rect(0,0,Width,Height));
    newBmp := TBGRABitmap.Create(r.Width,r.Height);
    newBmp.PutImage(LayerOffset[ALayerIndex].X - r.Left, LayerOffset[ALayerIndex].Y - r.Top, LayerBitmap[ALayerIndex], dmSet);
    if FLayers[ALayerIndex].Owner then FLayers[ALayerIndex].Source.Free;
    FLayers[ALayerIndex].Source := newBmp;
    FLayers[ALayerIndex].Owner := true;
    FLayers[ALayerIndex].x := r.Left;
    FLayers[ALayerIndex].y := r.Top;
  end;
end;

function TBGRALayeredBitmap.DrawEditor(ADest: TBGRABitmap;
  ALayerIndex: integer; X, Y: Integer; APointSize: single): TRect;
begin
  result := DrawEditor(ADest, ALayerIndex, AffineMatrixTranslation(X,Y), APointSize);
end;

function TBGRALayeredBitmap.DrawEditor(ADest: TBGRABitmap; ALayerIndex: integer;
  AMatrix: TAffineMatrix; APointSize: single): TRect;
var
  orig: TBGRALayerCustomOriginal;
begin
  orig := LayerOriginal[ALayerIndex];

  if orig <> FOriginalEditorOriginal then
  begin
    FreeAndNil(FOriginalEditor);
    FOriginalEditorOriginal := orig;
  end;

  if Assigned(orig) then
  begin
    if FOriginalEditor = nil then
    begin
      FOriginalEditor := orig.CreateEditor;
      FOriginalEditor.Focused := FEditorFocused;
      FOriginalEditor.OnFocusChanged:=@EditorFocusedChanged;
    end;
    FOriginalEditor.Clear;
    orig.ConfigureEditor(FOriginalEditor);
    FOriginalEditorViewMatrix := AffineMatrixTranslation(-0.5,-0.5)*AMatrix*AffineMatrixTranslation(0.5,0.5);
    FOriginalEditor.Matrix := AffineMatrixTranslation(-0.5,-0.5)*AMatrix*LayerOriginalMatrix[ALayerIndex]*AffineMatrixTranslation(0.5,0.5);
    FOriginalEditor.PointSize := APointSize;
    result := FOriginalEditor.Render(ADest, rect(0,0,ADest.Width,ADest.Height));
  end else
    result := EmptyRect;
end;

function TBGRALayeredBitmap.GetEditorBounds(ALayerIndex: integer; X,
  Y: Integer; APointSize: single): TRect;
begin
  result := GetEditorBounds(ALayerIndex, AffineMatrixTranslation(X,Y), APointSize);
end;

function TBGRALayeredBitmap.GetEditorBounds(ADestRect: TRect;
  ALayerIndex: integer; X, Y: Integer; APointSize: single): TRect;
begin
  result := GetEditorBounds(ADestRect, ALayerIndex, AffineMatrixTranslation(X,Y), APointSize);
end;

function TBGRALayeredBitmap.GetEditorBounds(ALayerIndex: integer;
  AMatrix: TAffineMatrix; APointSize: single): TRect;
begin
  result := GetEditorBounds(rect(-maxLongint,-maxLongint,maxLongint,maxLongint), ALayerIndex, AMatrix, APointSize);
end;

function TBGRALayeredBitmap.GetEditorBounds(ADestRect: TRect; ALayerIndex: integer;
  AMatrix: TAffineMatrix; APointSize: single): TRect;
var
  orig: TBGRALayerCustomOriginal;
begin
  orig := LayerOriginal[ALayerIndex];

  if orig <> FOriginalEditorOriginal then
  begin
    FreeAndNil(FOriginalEditor);
    FOriginalEditorOriginal := orig;
  end;

  if Assigned(orig) then
  begin
    if FOriginalEditor = nil then
    begin
      FOriginalEditor := orig.CreateEditor;
      if FOriginalEditor = nil then
        raise exception.Create('Unexpected nil value');
      FOriginalEditor.Focused := FEditorFocused;
      FOriginalEditor.OnFocusChanged:=@EditorFocusedChanged;
    end;
    FOriginalEditor.Clear;
    orig.ConfigureEditor(FOriginalEditor);
    FOriginalEditorViewMatrix := AffineMatrixTranslation(-0.5,-0.5)*AMatrix*AffineMatrixTranslation(0.5,0.5);
    FOriginalEditor.Matrix := AffineMatrixTranslation(-0.5,-0.5)*AMatrix*LayerOriginalMatrix[ALayerIndex]*AffineMatrixTranslation(0.5,0.5);
    FOriginalEditor.PointSize := APointSize;
    result := FOriginalEditor.GetRenderBounds(ADestRect);
  end else
    result := EmptyRect;
end;

procedure TBGRALayeredBitmap.MouseMove(Shift: TShiftState; ImageX, ImageY: Single; out
  ACursor: TOriginalEditorCursor);
var
  handled: boolean;
begin
  MouseMove(Shift, ImageX,ImageY, ACursor, handled);
end;

procedure TBGRALayeredBitmap.MouseDown(RightButton: boolean;
  Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor);
var
  handled: boolean;
begin
  MouseDown(RightButton, Shift, ImageX,ImageY, ACursor, handled);
end;

procedure TBGRALayeredBitmap.MouseUp(RightButton: boolean; Shift: TShiftState;
  ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor);
var
  handled: boolean;
begin
  MouseUp(RightButton, Shift, ImageX,ImageY, ACursor, handled);
end;

procedure TBGRALayeredBitmap.MouseMove(Shift: TShiftState; ImageX, ImageY: Single; out
  ACursor: TOriginalEditorCursor; out AHandled: boolean);
var
  viewPt: TPointF;
begin
  if Assigned(FOriginalEditor) then
  begin
    viewPt := FOriginalEditorViewMatrix*PointF(ImageX,ImageY);
    FOriginalEditor.MouseMove(Shift, viewPt.X, viewPt.Y, ACursor, AHandled);
  end
  else
  begin
    ACursor:= oecDefault;
    AHandled:= false;
  end;
end;

procedure TBGRALayeredBitmap.MouseDown(RightButton: boolean;
  Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor; out
  AHandled: boolean);
var
  viewPt: TPointF;
begin
  if Assigned(FOriginalEditor) then
  begin
    viewPt := FOriginalEditorViewMatrix*PointF(ImageX,ImageY);
    FOriginalEditor.MouseDown(RightButton, Shift, viewPt.X, viewPt.Y, ACursor, AHandled);
  end
  else
  begin
    ACursor:= oecDefault;
    AHandled:= false;
  end;
end;

procedure TBGRALayeredBitmap.MouseUp(RightButton: boolean; Shift: TShiftState;
  ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor; out AHandled: boolean);
var
  viewPt: TPointF;
begin
  if Assigned(FOriginalEditor) then
  begin
    viewPt := FOriginalEditorViewMatrix*PointF(ImageX,ImageY);
    FOriginalEditor.MouseUp(RightButton, Shift, viewPt.X,viewPt.Y, ACursor, AHandled);
  end
  else
  begin
    ACursor:= oecDefault;
    AHandled:= false;
  end;
end;

procedure TBGRALayeredBitmap.KeyDown(Shift: TShiftState; Key: TSpecialKey; out
  AHandled: boolean);
begin
  if Assigned(FOriginalEditor) then
    FOriginalEditor.KeyDown(Shift, Key, AHandled)
  else
    AHandled := false;
end;

procedure TBGRALayeredBitmap.KeyUp(Shift: TShiftState; Key: TSpecialKey; out
  AHandled: boolean);
begin
  if Assigned(FOriginalEditor) then
    FOriginalEditor.KeyUp(Shift, Key, AHandled)
  else
    AHandled := false;
end;

procedure TBGRALayeredBitmap.KeyPress(UTF8Key: string; out AHandled: boolean);
begin
  if Assigned(FOriginalEditor) then
    FOriginalEditor.KeyPress(UTF8Key, AHandled)
  else
    AHandled := false;
end;

function TBGRALayeredBitmap.IndexOfOriginal(AGuid: TGuid): integer;
var
  i: Integer;
begin
  for i := 0 to OriginalCount-1 do
    if FOriginals[i].Guid = AGuid then
    begin
      result := i;
      exit;
    end;
  result := -1
end;

function TBGRALayeredBitmap.IndexOfOriginal(AOriginal: TBGRALayerCustomOriginal): integer;
begin
  if Assigned(FOriginals) then
    result := FOriginals.IndexOf(BGRALayerOriginalEntry(AOriginal))
  else
    result := -1;
end;

{ TBGRACustomLayeredBitmap }

function TBGRACustomLayeredBitmap.GetLinearBlend: boolean;
begin
  result := FLinearBlend;
end;

function TBGRACustomLayeredBitmap.GetMemDirectory: TMemDirectory;
begin
  if FMemDirectory = nil then
  begin
    FMemDirectory:= TMemDirectory.Create;
    FMemDirectoryOwned := true;
  end;
  result := FMemDirectory;
end;

function TBGRACustomLayeredBitmap.GetDefaultBlendingOperation: TBlendOperation;
begin
  result := boTransparent;
end;

function TBGRACustomLayeredBitmap.GetHasMemFiles: boolean;
begin
  result := assigned(FMemDirectory) and (FMemDirectory.Count > 0);
end;

function TBGRACustomLayeredBitmap.GetLayerOriginalGuid(layer: integer): TGuid;
begin
  result := GUID_NULL;
end;

function TBGRACustomLayeredBitmap.GetLayerOriginalRenderStatus(layer: integer): TOriginalRenderStatus;
begin
  result := orsProof;
end;

function TBGRACustomLayeredBitmap.GetOriginalCount: integer;
begin
  result := 0;
end;

function TBGRACustomLayeredBitmap.GetOriginalByIndex(AIndex: integer): TBGRALayerCustomOriginal;
begin
  result := nil;
  raise exception.Create('Not implemented');
end;

function TBGRACustomLayeredBitmap.GetOriginalByIndexKnown(AIndex: integer): boolean;
begin
  result := true;
end;

function TBGRACustomLayeredBitmap.GetOriginalByIndexClass(AIndex: integer): TBGRALayerOriginalAny;
begin
  result := nil;
end;

function TBGRACustomLayeredBitmap.GetLayerOriginal(layer: integer): TBGRALayerCustomOriginal;
begin
  result := nil;
end;

function TBGRACustomLayeredBitmap.GetLayerOriginalKnown(layer: integer): boolean;
begin
  result := true;
end;

function TBGRACustomLayeredBitmap.GetLayerOriginalMatrix(layer: integer): TAffineMatrix;
begin
  result := AffineMatrixIdentity;
end;

procedure TBGRACustomLayeredBitmap.SetLinearBlend(AValue: boolean);
begin
  Unfreeze;
  FLinearBlend := AValue;
end;

procedure TBGRACustomLayeredBitmap.SetMemDirectory(AValue: TMemDirectory);
begin
  if AValue = FMemDirectory then exit;
  if FMemDirectoryOwned then FMemDirectory.Free;
  FMemDirectory := AValue;
  FMemDirectoryOwned := false;
end;

function TBGRACustomLayeredBitmap.GetLayerName(layer: integer): string;
begin
  result := 'Layer' + inttostr(layer+1);
end;

{$hints off}
function TBGRACustomLayeredBitmap.GetLayerOffset(layer: integer): TPoint;
begin
  //optional function
  result := Point(0,0);
end;
{$hints on}

{$hints off}
function TBGRACustomLayeredBitmap.GetLayerBitmapDirectly(layer: integer
  ): TBGRABitmap;
begin
  //optional function
  result:= nil;
end;

function TBGRACustomLayeredBitmap.GetLayerFrozenRange(layer: integer): integer;
var i: integer;
begin
  for i := 0 to high(FFrozenRange) do
    if (layer >= FFrozenRange[i].firstLayer) and (layer <= FFrozenRange[i].lastLayer) then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

function TBGRACustomLayeredBitmap.GetLayerFrozen(layer: integer): boolean;
var i: integer;
begin
  for i := 0 to high(FFrozenRange) do
    if (layer >= FFrozenRange[i].firstLayer) and (layer <= FFrozenRange[i].lastLayer) then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

function TBGRACustomLayeredBitmap.GetLayerUniqueId(layer: integer): integer;
begin
  result := layer;
end;

procedure TBGRACustomLayeredBitmap.SetLayerFrozen(layer: integer;
  AValue: boolean);
begin
  //nothing
end;

function TBGRACustomLayeredBitmap.RangeIntersect(first1, last1, first2,
  last2: integer): boolean;
begin
  result := (first1 <= last2) and (last1 >= first2);
end;

procedure TBGRACustomLayeredBitmap.RemoveFrozenRange(index: integer);
var j,i: integer;
begin
  for j := FFrozenRange[index].firstLayer to FFrozenRange[index].lastLayer do
    SetLayerFrozen(j,False);
  FFrozenRange[index].image.Free;
  for i := index to high(FFrozenRange)-1 do
    FFrozenRange[i] := FFrozenRange[i+1];
  setlength(FFrozenRange,length(FFrozenRange)-1);
end;

function TBGRACustomLayeredBitmap.ContainsFrozenRange(first, last: integer): boolean;
var i: integer;
begin
  for i := 0 to high(FFrozenRange) do
    if (FFrozenRange[i].firstLayer = first) and (FFrozenRange[i].lastLayer = last) then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

function TBGRACustomLayeredBitmap.GetEmpty: boolean;
begin
  result := (NbLayers = 0) and (Width = 0) and (Height = 0);
end;

function TBGRACustomLayeredBitmap.IndexOfOriginal(AGuid: TGuid): integer;
begin
  result := -1;
end;

function TBGRACustomLayeredBitmap.IndexOfOriginal(
  AOriginal: TBGRALayerCustomOriginal): integer;
begin
  result := -1;
end;

procedure TBGRACustomLayeredBitmap.SetWidth(Value: Integer);
begin
  //nothing
end;

procedure TBGRACustomLayeredBitmap.SetHeight(Value: Integer);
begin
  //nothing
end;

function TBGRACustomLayeredBitmap.GetTransparent: Boolean;
begin
  result := true;
end;

procedure TBGRACustomLayeredBitmap.SetTransparent(Value: Boolean);
begin
  //nothing
end;

procedure TBGRACustomLayeredBitmap.SaveToFile(const filenameUTF8: string);
var bmp: TBGRABitmap;
    ext: string;
    temp: TBGRALayeredBitmap;
    i: integer;
    stream: TFileStreamUTF8;
begin
  ext := UTF8LowerCase(ExtractFileExt(filenameUTF8));
  for i := 0 to high(LayeredBitmapWriters) do
    if '.'+LayeredBitmapWriters[i].extension = ext then
    begin
      temp := LayeredBitmapWriters[i].theClass.Create;
      try
        temp.Assign(self);
        temp.SaveToFile(filenameUTF8);
      finally
        temp.Free;
      end;
      exit;
    end;

  //when using "data" extension, simply serialize
  if (ext='.dat') or (ext='.data') then
  begin
    if Assigned(LayeredBitmapLoadFromStreamProc) then
    begin
      stream := TFileStreamUTF8.Create(filenameUTF8, fmCreate);
      try
        LayeredBitmapSaveToStreamProc(stream, self);
      finally
        stream.Free;
      end;
    end else
      raise exception.Create('Enable layer serialization by calling BGRAStreamLayers.RegisterStreamLayers');
  end else
  begin
    bmp := ComputeFlatImage;
    try
      bmp.SaveToFileUTF8(filenameUTF8);
    finally
      bmp.Free;
    end;
  end;
end;

procedure TBGRACustomLayeredBitmap.SaveToStream(Stream: TStream);
begin
  if Assigned(LayeredBitmapSaveToStreamProc) then
    LayeredBitmapSaveToStreamProc(Stream, self)
  else
    raise exception.Create('Call BGRAStreamLayers.RegisterStreamLayers first');
end;

procedure TBGRACustomLayeredBitmap.SaveToStreamAs(Stream: TStream;
  AExtension: string);
var bmp: TBGRABitmap;
    ext: string;
    format: TBGRAImageFormat;
    temp: TBGRALayeredBitmap;
    i: integer;
begin
  ext := UTF8LowerCase(AExtension);
  if ext[1] <> '.' then ext := '.'+ext;

  for i := 0 to high(LayeredBitmapWriters) do
    if '.'+LayeredBitmapWriters[i].extension = ext then
    begin
      temp := LayeredBitmapWriters[i].theClass.Create;
      try
        temp.Assign(self);
        temp.SaveToStream(Stream);
      finally
        temp.Free;
      end;
      exit;
    end;

  format := SuggestImageFormat(ext);
  bmp := ComputeFlatImage;
  try
    bmp.SaveToStreamAs(Stream, format);
  finally
    bmp.Free;
  end;
end;

constructor TBGRACustomLayeredBitmap.Create;
begin
  FFrozenRange := nil;
  FLinearBlend:= True;
  FMemDirectory := nil;
  FMemDirectoryOwned:= false;
end;

{$hints on}

function TBGRACustomLayeredBitmap.ToString: ansistring;
var
  i: integer;
begin
  Result := 'LayeredBitmap' + LineEnding + LineEnding;
  for i := 0 to NbLayers - 1 do
  begin
    Result += LineEnding + 'Layer ' + IntToStr(i) + ' : ' + LayerName[i] + LineEnding;
  end;
end;

function TBGRACustomLayeredBitmap.ComputeFlatImage(ASeparateXorMask: boolean): TBGRABitmap;
begin
  result := ComputeFlatImage(rect(0,0,Width,Height), 0, NbLayers - 1, ASeparateXorMask);
end;

function TBGRACustomLayeredBitmap.ComputeFlatImage(firstLayer,
  lastLayer: integer; ASeparateXorMask: boolean): TBGRABitmap;
begin
  result := ComputeFlatImage(rect(0,0,Width,Height), firstLayer,LastLayer,ASeparateXorMask);
end;

function TBGRACustomLayeredBitmap.ComputeFlatImage(ARect: TRect;
  ASeparateXorMask: boolean): TBGRABitmap;
begin
  result := ComputeFlatImage(ARect,0, NbLayers - 1, ASeparateXorMask);
end;

destructor TBGRACustomLayeredBitmap.Destroy;
begin
  Clear;
end;

function TBGRACustomLayeredBitmap.ComputeFlatImage(ARect: TRect; firstLayer, lastLayer: integer; ASeparateXorMask: boolean): TBGRABitmap;
var
  tempLayer: TBGRABitmap;
  i,j: integer;
  mustFreeCopy: boolean;
  op: TBlendOperation;
begin
  if (firstLayer < 0) or (lastLayer > NbLayers-1) then
    raise ERangeError.Create('Layer index out of bounds');
  If (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then
  begin
    result := TBGRABitmap.Create(0,0);
    exit;
  end;
  Result := TBGRABitmap.Create(ARect.Right-ARect.Left, ARect.Bottom-ARect.Top);
  i := firstLayer;
  while i <= lastLayer do
  begin
    if LayerFrozen[i] then
    begin
      j := GetLayerFrozenRange(i);
      if j <> -1 then
      begin
        if i = 0 then
          Result.PutImage(-ARect.Left,-ARect.Top,FFrozenRange[j].image,dmSet) else
        if not FFrozenRange[j].linearBlend then
          Result.PutImage(-ARect.Left,-ARect.Top,FFrozenRange[j].image,dmDrawWithTransparency)
        else
          Result.PutImage(-ARect.Left,-ARect.Top,FFrozenRange[j].image,dmLinearBlend);
        i := FFrozenRange[j].lastLayer+1;
        continue;
      end;
    end;
    if LayerVisible[i] and (LayerOpacity[i]<>0) then
    begin
      tempLayer := GetLayerBitmapDirectly(i);
      if tempLayer <> nil then
        mustFreeCopy := false
      else
      begin
        mustFreeCopy := true;
        tempLayer := GetLayerBitmapCopy(i);
      end;
      if tempLayer <> nil then
      with LayerOffset[i] do
      begin
        op := BlendOperation[i];
        //XOR mask
        if (op = boXor) and ASeparateXorMask then
        begin
          result.NeedXorMask;
          result.XorMask.BlendImageOver(x-ARect.Left,y-ARect.Top, tempLayer, op, LayerOpacity[i], LinearBlend);
        end else
        //first layer is simply the background
        if i = firstLayer then
          Result.PutImage(x-ARect.Left, y-ARect.Top, tempLayer, dmSet, LayerOpacity[i])
        else
        //simple blend operations
        if (op = boLinearBlend) or ((op = boTransparent) and LinearBlend) then
          Result.PutImage(x-ARect.Left,y-ARect.Top,tempLayer,dmLinearBlend, LayerOpacity[i]) else
        if op = boTransparent then
          Result.PutImage(x-ARect.Left,y-ARect.Top,tempLayer,dmDrawWithTransparency, LayerOpacity[i])
        else
          //complex blend operations are done in a third bitmap
          result.BlendImageOver(x-ARect.Left,y-ARect.Top, tempLayer, op, LayerOpacity[i], LinearBlend);
        if mustFreeCopy then tempLayer.Free;
      end;
    end;
    inc(i);
  end;
  if result.XorMask <> nil then
    AlphaFillInline(result.XorMask.Data, 0, result.XorMask.NbPixels);
end;

procedure TBGRACustomLayeredBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
var temp: TBGRABitmap;
begin
  if (Rect.Right <= Rect.Left) or (Rect.Bottom <= Rect.Top) then exit;
  if (Rect.Right-Rect.Left = Width) and (Rect.Bottom-Rect.Top = Height) then
    Draw(ACanvas, Rect.Left,Rect.Top) else
  begin
    temp := ComputeFlatImage;
    BGRAReplace(temp,temp.Resample(Rect.Right-Rect.Left,Rect.Bottom-Rect.Top));
    temp.Draw(ACanvas, Rect.Left,Rect.Top, False);
    temp.Free;
  end;
end;

procedure TBGRACustomLayeredBitmap.Draw(Canvas: TCanvas; x, y: integer);
begin
  Draw(Canvas,x,y,0,NbLayers-1);
end;

procedure TBGRACustomLayeredBitmap.Draw(Canvas: TCanvas; x, y: integer; firstLayer, lastLayer: integer);
var temp: TBGRABitmap;
begin
  temp := ComputeFlatImage(firstLayer,lastLayer);
  temp.Draw(Canvas,x,y,False);
  temp.Free;
end;

procedure TBGRACustomLayeredBitmap.Draw(Dest: TBGRABitmap; x, y: integer);
begin
  Draw(Dest,x,y,0,NbLayers-1);
end;

procedure TBGRACustomLayeredBitmap.Draw(Dest: TBGRABitmap; x, y: integer;
  ASeparateXorMask: boolean);
begin
  Draw(Dest,x,y,0,NbLayers-1,ASeparateXorMask);
end;

procedure TBGRACustomLayeredBitmap.Draw(Dest: TBGRABitmap; AX, AY: integer; firstLayer, lastLayer: integer; ASeparateXorMask: boolean);
var
  temp: TBGRABitmap;
  i,j: integer;
  tempLayer: TBGRABitmap;
  mustFreeCopy: boolean;
  OldClipRect: TRect;
  NewClipRect: TRect;
begin
  OldClipRect := Dest.ClipRect;
  NewClipRect := rect(0,0,0,0);
  if not IntersectRect(NewClipRect,rect(AX,AY,AX+Width,AY+Height),Dest.ClipRect) then exit; //nothing to be drawn

  for i := firstLayer to lastLayer do
    if LayerVisible[i] and not (BlendOperation[i] in[boTransparent,boLinearBlend]) then
    begin
      temp := ComputeFlatImage(rect(NewClipRect.Left-AX,NewClipRect.Top-AY,NewClipRect.Right-AX,NewClipRect.Bottom-AY), ASeparateXorMask);
      if self.LinearBlend then
        Dest.PutImage(NewClipRect.Left,NewClipRect.Top,temp,dmLinearBlend)
      else
        Dest.PutImage(NewClipRect.Left,NewClipRect.Top,temp,dmDrawWithTransparency);
      temp.Free;
      exit;
    end;

  Dest.ClipRect := NewClipRect;
  i := firstLayer;
  while i <= lastLayer do
  begin
    if LayerFrozen[i] then
    begin
      j := GetLayerFrozenRange(i);
      if j <> -1 then
      begin
        if not FFrozenRange[j].linearBlend then
          Dest.PutImage(AX,AY,FFrozenRange[j].image,dmDrawWithTransparency)
        else
          Dest.PutImage(AX,AY,FFrozenRange[j].image,dmLinearBlend);
        i := FFrozenRange[j].lastLayer+1;
        continue;
      end;
    end;
    if LayerVisible[i] then
    begin
      tempLayer := GetLayerBitmapDirectly(i);
      if tempLayer <> nil then
        mustFreeCopy := false
      else
      begin
        mustFreeCopy := true;
        tempLayer := GetLayerBitmapCopy(i);
      end;
      if tempLayer <> nil then
      with LayerOffset[i] do
      begin
        if (BlendOperation[i] = boTransparent) and not self.LinearBlend then //here it is specified not to use linear blending
          Dest.PutImage(AX+x,AY+y,tempLayer,dmDrawWithTransparency, LayerOpacity[i])
        else
          Dest.PutImage(AX+x,AY+y,tempLayer,dmLinearBlend, LayerOpacity[i]);
        if mustFreeCopy then tempLayer.Free;
      end;
    end;
    inc(i);
  end;
  Dest.ClipRect := OldClipRect;
end;

procedure TBGRACustomLayeredBitmap.FreezeExceptOneLayer(layer: integer);
begin
  if (layer < 0) or (layer >= NbLayers) then
  begin
    Freeze;
    exit;
  end;
  Unfreeze(layer,layer);
  if layer > 1 then
    Freeze(0,layer-1);
  if layer < NbLayers-2 then
    Freeze(layer+1,NbLayers-1);
end;

procedure TBGRACustomLayeredBitmap.Freeze(firstLayer, lastLayer: integer);

  procedure DoFreeze(first,last: integer; linear: boolean);
  var i,nbVisible: integer;
    computedImage: TBGRABitmap;
  begin
    if last <= first then exit; //at least 2 frozen layers
    nbVisible := 0;
    for i := first to last do
      if LayerVisible[i] and (LayerOpacity[i] > 0) then inc(nbVisible);
    if nbvisible < 2 then exit;  //at least 2 frozen layers

    if ContainsFrozenRange(first,last) then exit; //already frozen
    Unfreeze(first,last);

    computedImage := ComputeFlatImage(first,last); //must compute before layers are considered as frozen
    setlength(FFrozenRange, length(FFrozenRange)+1);
    with FFrozenRange[high(FFrozenRange)] do
    begin
      firstLayer := first;
      lastLayer:= last;
      image := computedImage;
      linearBlend := linear;
    end;
    for i := first to last do
      SetLayerFrozen(i,True);
  end;

var j: integer;
  start: integer;
  linear,nextLinear: boolean;
begin
  start := -1;
  linear := false; //to avoid hint
  for j := firstlayer to lastLayer do
  if (BlendOperation[j] in [boTransparent,boLinearBlend]) or (start = 0) or ((firstlayer= 0) and (j=0)) then
  begin
    nextLinear := (BlendOperation[j] = boLinearBlend) or self.LinearBlend;
    if start = -1 then
    begin
      start := j;
      linear := nextLinear;
    end else
    begin
      if linear <> nextLinear then
      begin
        DoFreeze(start,j-1,linear);
        start := j;
        linear := nextLinear;
      end;
    end;
  end else
  begin
    if start <> -1 then
    begin
      DoFreeze(start,j-1,linear);
      start := -1;
    end;
  end;
  if start <> -1 then
    DoFreeze(start,lastLayer,linear);
end;

procedure TBGRACustomLayeredBitmap.Freeze;
begin
  Freeze(0,NbLayers-1);
end;

procedure TBGRACustomLayeredBitmap.Unfreeze;
begin
  Unfreeze(0,NbLayers-1);
end;

procedure TBGRACustomLayeredBitmap.Unfreeze(layer: integer);
begin
  Unfreeze(layer,layer);
end;

procedure TBGRACustomLayeredBitmap.Unfreeze(firstLayer, lastLayer: integer);
var i: integer;
begin
  for i := high(FFrozenRange) downto 0 do
    if RangeIntersect(firstLayer,lastLayer,FFrozenRange[i].firstLayer,FFrozenRange[i].lastLayer) then
      RemoveFrozenRange(i);
end;

procedure TBGRACustomLayeredBitmap.NotifyLoaded;
begin
  //nothing
end;

procedure TBGRACustomLayeredBitmap.NotifySaving;
begin
  //nothing
end;

procedure RegisterLayeredBitmapReader(AExtensionUTF8: string; AReader: TBGRACustomLayeredBitmapClass);
begin
  setlength(LayeredBitmapReaders,length(LayeredBitmapReaders)+1);
  with LayeredBitmapReaders[high(LayeredBitmapReaders)] do
  begin
    extension:= UTF8LowerCase(AExtensionUTF8);
    theClass := AReader;
  end;
end;

function TryCreateLayeredBitmapWriter(AExtensionUTF8: string): TBGRALayeredBitmap;
var
  i: Integer;
begin
  AExtensionUTF8:= UTF8LowerCase(AExtensionUTF8);
  if (AExtensionUTF8 = '') or (AExtensionUTF8[1] <> '.') then
    AExtensionUTF8:= '.'+AExtensionUTF8;
  for i := 0 to high(LayeredBitmapWriters) do
    if '.'+LayeredBitmapWriters[i].extension = AExtensionUTF8 then
    begin
      result := LayeredBitmapWriters[i].theClass.Create;
      exit;
    end;
  result := nil;
end;

function TryCreateLayeredBitmapReader(AExtensionUTF8: string): TBGRACustomLayeredBitmap;
var
  i: Integer;
begin
  AExtensionUTF8:= UTF8LowerCase(AExtensionUTF8);
  if (AExtensionUTF8 = '') or (AExtensionUTF8[1] <> '.') then
    AExtensionUTF8:= '.'+AExtensionUTF8;
  for i := 0 to high(LayeredBitmapReaders) do
    if '.'+LayeredBitmapReaders[i].extension = AExtensionUTF8 then
    begin
      result := LayeredBitmapReaders[i].theClass.Create;
      exit;
    end;
  result := nil;
end;

procedure OnLayeredBitmapLoadFromStreamStart;
begin
  OnLayeredBitmapLoadStart('<Stream>');
end;

procedure OnLayeredBitmapLoadStart(AFilenameUTF8: string);
begin
  if Assigned(OnLayeredBitmapLoadStartProc) then
    OnLayeredBitmapLoadStartProc(AFilenameUTF8);
end;

procedure OnLayeredBitmapLoadProgress(APercentage: integer);
begin
  if Assigned(OnLayeredBitmapLoadProgressProc) then
    OnLayeredBitmapLoadProgressProc(APercentage);
end;

procedure OnLayeredBitmapLoaded;
begin
  if Assigned(OnLayeredBitmapLoadedProc) then
    OnLayeredBitmapLoadedProc();
end;

procedure RegisterLoadingHandler(AStart: TOnLayeredBitmapLoadStartProc;
  AProgress: TOnLayeredBitmapLoadProgressProc; ADone: TOnLayeredBitmapLoadedProc
  );
begin
  OnLayeredBitmapLoadProgressProc:= AProgress;
  OnLayeredBitmapLoadStartProc := AStart;
  OnLayeredBitmapLoadedProc:= ADone;
end;

procedure UnregisterLoadingHandler(AStart: TOnLayeredBitmapLoadStartProc;
  AProgress: TOnLayeredBitmapLoadProgressProc; ADone: TOnLayeredBitmapLoadedProc);
begin
  if OnLayeredBitmapLoadProgressProc = AProgress then OnLayeredBitmapLoadProgressProc := nil;
  if OnLayeredBitmapLoadStartProc = AStart then OnLayeredBitmapLoadStartProc := nil;
  if OnLayeredBitmapLoadedProc = ADone then OnLayeredBitmapLoadedProc := nil;
end;

procedure RegisterLayeredBitmapWriter(AExtensionUTF8: string; AWriter: TBGRALayeredBitmapClass);
begin
  while (length(AExtensionUTF8)>0) and (AExtensionUTF8[1]='.') do delete(AExtensionUTF8,1,1);
  setlength(LayeredBitmapWriters,length(LayeredBitmapWriters)+1);
  with LayeredBitmapWriters[high(LayeredBitmapWriters)] do
  begin
    extension:= UTF8LowerCase(AExtensionUTF8);
    theClass := AWriter;
  end;
end;

initialization

  NextLayerUniqueId := 1;

end.

