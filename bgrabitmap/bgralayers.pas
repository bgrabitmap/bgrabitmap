unit BGRALayers;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  BGRAGraphics, Classes, SysUtils, Types, BGRABitmapTypes, BGRABitmap,
  BGRAMemDirectory, BGRATransform, fgl;

type
  TBGRACustomLayeredBitmap = class;
  TBGRACustomLayeredBitmapClass = class of TBGRACustomLayeredBitmap;
  TBGRALayerCustomOriginal = class;
  TBGRALayerOriginalAny = class of TBGRALayerCustomOriginal;

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
    function GetLayerOriginalMatrix({%H-}layer: integer): TAffineMatrix; virtual;
    function GetLayerOriginalGuid({%H-}layer: integer): TGuid; virtual;
    function GetTransparent: Boolean; override;
    function GetEmpty: boolean; override;

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
    property LayerOriginalMatrix[layer: integer]: TAffineMatrix read GetLayerOriginalMatrix;
    property LayerOriginalGuid[layer: integer]: TGuid read GetLayerOriginalGuid;
    property LinearBlend: boolean read GetLinearBlend write SetLinearBlend; //use linear blending unless specified
    property DefaultBlendingOperation: TBlendOperation read GetDefaultBlendingOperation;
    property MemDirectory: TMemDirectory read GetMemDirectory write SetMemDirectory;
    property MemDirectoryOwned: boolean read FMemDirectoryOwned write FMemDirectoryOwned;
    property HasMemFiles: boolean read GetHasMemFiles;
  end;

  TOriginalRenderStatus = (orsNone, orsDraft, orsProof);

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
  end;

  { TBGRALayeredBitmap }

  TBGRALayeredBitmap = class(TBGRACustomLayeredBitmap)
  private
    FNbLayers: integer;
    FLayers: array of TBGRALayerInfo;
    FWidth,FHeight: integer;
    FOriginals: TBGRALayerOriginalList;

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
    function GetLayerOriginalMatrix(layer: integer): TAffineMatrix; override;
    function GetLayerOriginalGuid(layer: integer): TGuid; override;
    function GetLayerOriginalRenderStatus(layer: integer): TOriginalRenderStatus;
    function GetOriginalCount: integer;
    function GetOriginalByIndex(AIndex: integer): TBGRALayerCustomOriginal;
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

  public
    procedure LoadFromFile(const filenameUTF8: string); override;
    procedure LoadFromStream(stream: TStream); override;
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
    function AddLayerFromOriginal(AGuid: TGuid; Opacity: byte = 255): integer; overload;
    function AddLayerFromOriginal(AGuid: TGuid; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromOriginal(AGuid: TGuid; Matrix: TAffineMatrix; Opacity: byte = 255): integer; overload;
    function AddLayerFromOriginal(AGuid: TGuid; Matrix: TAffineMatrix; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; Matrix: TAffineMatrix; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; Matrix: TAffineMatrix; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;

    function AddOriginal(AOriginal: TBGRALayerCustomOriginal; AOwned: boolean = true): integer;
    function RemoveOriginal(AOriginal: TBGRALayerCustomOriginal): boolean;
    procedure DeleteOriginal(AIndex: integer);
    procedure NotifyLoaded; override;
    procedure NotifySaving; override;
    procedure RenderLayerFromOriginal(layer: integer; ADraft: boolean = false);
    procedure RenderOriginalsIfNecessary(ADraft: boolean = false);
    procedure RemoveUnusedOriginals;

    destructor Destroy; override;
    constructor Create; override; overload;
    constructor Create(AWidth, AHeight: integer); virtual; overload;
    function GetLayerBitmapDirectly(layer: integer): TBGRABitmap; override;
    function GetLayerBitmapCopy(layer: integer): TBGRABitmap; override;
    function GetLayerIndexFromId(AIdentifier: integer): integer;
    function Duplicate(ASharedLayerIds: boolean = false): TBGRALayeredBitmap;
    function ProduceLayerUniqueId: integer;

    procedure RotateCW;
    procedure RotateCCW;
    procedure HorizontalFlip;
    procedure VerticalFlip;
    procedure Resample(AWidth, AHeight: integer; AResampleMode: TResampleMode; AFineResampleFilter: TResampleFilter = rfLinear);
    procedure SetLayerBitmap(layer: integer; ABitmap: TBGRABitmap; AOwned: boolean);

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
    property LayerOriginalGuid[layer: integer]: TGuid read GetLayerOriginalGuid write SetLayerOriginalGuid;
    property LayerOriginalMatrix[layer: integer]: TAffineMatrix read GetLayerOriginalMatrix write SetLayerOriginalMatrix;
    property LayerOriginalRenderStatus[layer: integer]: TOriginalRenderStatus read GetLayerOriginalRenderStatus write SetLayerOriginalRenderStatus;

    function IndexOfOriginal(AGuid: TGuid): integer; overload;
    function IndexOfOriginal(AOriginal: TBGRALayerCustomOriginal): integer; overload;
    property OriginalCount: integer read GetOriginalCount;
    property Original[AIndex: integer]: TBGRALayerCustomOriginal read GetOriginalByIndex;
  end;

  TAffineMatrix = BGRABitmapTypes.TAffineMatrix;

  TBGRACustomOriginalStorage = class;

  { TBGRALayerCustomOriginal }

  TBGRALayerCustomOriginal = class
  protected
    FGuid: TGuid;
    function GetGuid: TGuid;
    procedure SetGuid(AValue: TGuid);
  public
    constructor Create; virtual;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); virtual; abstract;
    function GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix): TRect; virtual; abstract;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); virtual; abstract;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); virtual; abstract;
    procedure LoadFromFile(AFilenameUTF8: string); virtual;
    procedure LoadFromStream(AStream: TStream); virtual;
    procedure SaveToFile(AFilenameUTF8: string); virtual;
    procedure SaveToStream(AStream: TStream); virtual;
    class function StorageClassName: RawByteString; virtual; abstract;
    property Guid: TGuid read GetGuid write SetGuid;
  end;

  { TBGRACustomOriginalStorage }

  TBGRACustomOriginalStorage = class
  private
    function GetBool(AName: utf8string): boolean;
    procedure SetBool(AName: utf8string; AValue: boolean);
  protected
    FFormats: TFormatSettings;
    function GetInteger(AName: utf8string): integer;
    function GetPointF(AName: utf8string): TPointF;
    function GetRawString(AName: utf8string): RawByteString; virtual; abstract;
    function GetSingle(AName: utf8string): single;
    function GetColor(AName: UTF8String): TBGRAPixel;
    procedure SetInteger(AName: utf8string; AValue: integer);
    procedure SetPointF(AName: utf8string; AValue: TPointF);
    procedure SetRawString(AName: utf8string; AValue: RawByteString); virtual; abstract;
    procedure SetSingle(AName: utf8string; AValue: single);
    procedure SetColor(AName: UTF8String; AValue: TBGRAPixel);
  public
    constructor Create;
    procedure RemoveAttribute(AName: utf8string); virtual; abstract;
    procedure RemoveFile(AName: utf8string); virtual; abstract;
    function ReadFile(AName: UTF8String; ADest: TStream): boolean; virtual; abstract;
    procedure WriteFile(AName: UTF8String; ASource: TStream; ACompress: boolean); virtual; abstract;
    property RawString[AName: utf8string]: RawByteString read GetRawString write SetRawString;
    property Int[AName: utf8string]: integer read GetInteger write SetInteger;
    property Bool[AName: utf8string]: boolean read GetBool write SetBool;
    property Float[AName: utf8string]: single read GetSingle write SetSingle;
    property PointF[AName: utf8string]: TPointF read GetPointF write SetPointF;
    property Color[AName: UTF8String]: TBGRAPixel read GetColor write SetColor;
  end;

  { TBGRAMemOriginalStorage }

  TBGRAMemOriginalStorage = class(TBGRACustomOriginalStorage)
  protected
    FMemDir: TMemDirectory;
    function GetRawString(AName: utf8string): RawByteString; override;
    procedure SetRawString(AName: utf8string; AValue: RawByteString); override;
  public
    constructor Create(AMemDir: TMemDirectory);
    procedure RemoveAttribute(AName: utf8string); override;
    procedure RemoveFile(AName: utf8string); override;
    function ReadFile(AName: UTF8String; ADest: TStream): boolean; override;
    procedure WriteFile(AName: UTF8String; ASource: TStream; ACompress: boolean); override;
  end;

procedure RegisterLayerOriginal(AClass: TBGRALayerOriginalAny);

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

uses BGRAUTF8, BGRABlend, BGRAMultiFileType;

const
  OriginalsDirectory = 'originals';

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
  LayerOriginalClasses: array of TBGRALayerOriginalAny;

operator =(const AGuid1, AGuid2: TGuid): boolean;
begin
  result := CompareMem(@AGuid1, @AGuid2, sizeof(TGuid));
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

{ TBGRAMemOriginalStorage }

function TBGRAMemOriginalStorage.GetRawString(AName: utf8string): RawByteString;
begin
  if pos('.',AName)<>0 then exit('');
  result := FMemDir.RawStringByFilename[AName];
end;

procedure TBGRAMemOriginalStorage.SetRawString(AName: utf8string;
  AValue: RawByteString);
begin
  if pos('.',AName)<>0 then exit;
  FMemDir.RawStringByFilename[AName] := AValue;
end;

constructor TBGRAMemOriginalStorage.Create(AMemDir: TMemDirectory);
begin
  inherited Create;
  FMemDir := AMemDir;
end;

procedure TBGRAMemOriginalStorage.RemoveAttribute(AName: utf8string);
begin
  if pos('.',AName)<>0 then exit;
  FMemDir.Delete(AName,'');
end;

procedure TBGRAMemOriginalStorage.RemoveFile(AName: utf8string);
begin
  FMemDir.Delete(EntryFilename(AName));
end;

function TBGRAMemOriginalStorage.ReadFile(AName: UTF8String; ADest: TStream): boolean;
var
  entryId: Integer;
begin
  entryId := FMemDir.IndexOf(EntryFilename(AName));
  if entryId <> -1 then
  begin
    with FMemDir.Entry[entryId] do
      result := CopyTo(ADest) = FileSize
  end
  else
    result := false;
end;

procedure TBGRAMemOriginalStorage.WriteFile(AName: UTF8String; ASource: TStream; ACompress: boolean);
var
  idxEntry: Integer;
begin
  idxEntry := FMemDir.Add(EntryFilename(AName), ASource, true, false);
  if ACompress then FMemDir.IsEntryCompressed[idxEntry] := true;
end;

{ TBGRACustomOriginalStorage }

function TBGRACustomOriginalStorage.GetColor(AName: UTF8String): TBGRAPixel;
begin
  result := StrToBGRA(RawString[AName], BGRAPixelTransparent);
end;

procedure TBGRACustomOriginalStorage.SetColor(AName: UTF8String;
  AValue: TBGRAPixel);
begin
  RawString[AName] := LowerCase(BGRAToStr(AValue, CSSColors));
end;

function TBGRACustomOriginalStorage.GetBool(AName: utf8string): boolean;
begin
  result := StrToBool(RawString[AName]);
end;

procedure TBGRACustomOriginalStorage.SetBool(AName: utf8string; AValue: boolean);
begin
  RawString[AName] := BoolToStr(AValue,'true','false');
end;

function TBGRACustomOriginalStorage.GetInteger(AName: utf8string): integer;
begin
  result := StrToIntDef(RawString[AName],0);
end;

function TBGRACustomOriginalStorage.GetPointF(AName: utf8string): TPointF;
var
  s: String;
  posComma: integer;
begin
  s := RawString[AName];
  posComma := pos(',',s);
  if posComma = 0 then
    exit(EmptyPointF);

  result.x := StrToFloat(copy(s,1,posComma-1));
  result.y := StrToFloat(copy(s,posComma+1,length(s)-posComma));
end;

function TBGRACustomOriginalStorage.GetSingle(AName: utf8string): single;
begin
  result := StrToFloatDef(RawString[AName], EmptySingle, FFormats);
end;

procedure TBGRACustomOriginalStorage.SetInteger(AName: utf8string;
  AValue: integer);
begin
  RawString[AName] := IntToStr(AValue);
end;

procedure TBGRACustomOriginalStorage.SetPointF(AName: utf8string;
  AValue: TPointF);
begin
  if isEmptyPointF(AValue) then RemoveAttribute(AName)
  else RawString[AName] := FloatToStrF(AValue.x, ffGeneral,7,3, FFormats)+','+FloatToStrF(AValue.y, ffGeneral,7,3, FFormats);
end;

procedure TBGRACustomOriginalStorage.SetSingle(AName: utf8string; AValue: single);
begin
  if AValue = EmptySingle then RemoveAttribute(AName)
  else RawString[AName] := FloatToStrF(AValue, ffGeneral,7,3, FFormats);
end;

constructor TBGRACustomOriginalStorage.Create;
begin
  FFormats := DefaultFormatSettings;
  FFormats.DecimalSeparator := '.';
end;

{ TBGRALayerCustomOriginal }

function TBGRALayerCustomOriginal.GetGuid: TGuid;
begin
  result := FGuid;
end;

procedure TBGRALayerCustomOriginal.SetGuid(AValue: TGuid);
begin
  FGuid := AValue;
end;

constructor TBGRALayerCustomOriginal.Create;
begin
  FGuid := GUID_NULL;
end;

procedure TBGRALayerCustomOriginal.LoadFromFile(AFilenameUTF8: string);
var
  s: TFileStreamUTF8;
begin
  s := TFileStreamUTF8.Create(AFilenameUTF8, fmOpenRead, fmShareDenyWrite);
  try
    LoadFromStream(s);
  finally
    s.Free;
  end;
end;

procedure TBGRALayerCustomOriginal.LoadFromStream(AStream: TStream);
var storage: TBGRAMemOriginalStorage;
  memDir: TMemDirectory;
begin
  memDir := TMemDirectory.Create;
  storage := nil;
  try
    memDir.LoadFromStream(AStream);
    storage := TBGRAMemOriginalStorage.Create(memDir);
    if storage.RawString['class'] <> StorageClassName then
      raise exception.Create('Invalid class');
    LoadFromStorage(storage);
    FreeAndNil(storage);
  finally
    storage.Free;
    memDir.Free;
  end;
end;

procedure TBGRALayerCustomOriginal.SaveToFile(AFilenameUTF8: string);
var
  s: TFileStreamUTF8;
begin
  s := TFileStreamUTF8.Create(AFilenameUTF8, fmCreate);
  try
    SaveToStream(s);
  finally
    s.Free;
  end;
end;

procedure TBGRALayerCustomOriginal.SaveToStream(AStream: TStream);
var storage: TBGRAMemOriginalStorage;
  memDir: TMemDirectory;
begin
  memDir := TMemDirectory.Create;
  storage := nil;
  try
    storage := TBGRAMemOriginalStorage.Create(memDir);
    storage.RawString['class'] := StorageClassName;
    SaveToStorage(storage);
    FreeAndNil(storage);
    memDir.SaveToStream(AStream);
  finally
    storage.Free;
    memDir.Free;
  end;
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
      if (i <> layer) and (FLayers[layer].UniqueId = AValue) then
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
  i: Integer;
begin
  ADir := nil;
  AClass := nil;

  if HasMemFiles then
  begin
    ADir := MemDirectory.FindPath(OriginalsDirectory+'/'+GUIDToString(AGuid));
    if ADir <> nil then
    begin
      c := ADir.RawStringByFilename['class'];
      for i := 0 to high(LayerOriginalClasses) do
        if LayerOriginalClasses[i].StorageClassName = c then
        begin
          AClass := LayerOriginalClasses[i];
          break;
        end;
    end;
  end;
end;

procedure TBGRALayeredBitmap.StoreOriginal(AOriginal: TBGRALayerCustomOriginal);
var
  dir, subdir: TMemDirectory;
  storage: TBGRAMemOriginalStorage;
begin
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

function TBGRALayeredBitmap.GetOriginalCount: integer;
begin
  if Assigned(FOriginals) then
    result := FOriginals.Count;
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
      raise EDirectoryNotFoundException.Create('Original directory not found');
    if not Assigned(c) then
      raise exception.Create('Original class not found (it can be registered with the RegisterLayerOriginal function)');

    result := c.Create;
    result.Guid := guid;
    storage := TBGRAMemOriginalStorage.Create(dir);
    try
      result.LoadFromStorage(storage);
    finally
      storage.Free;
    end;
    FOriginals[AIndex] := BGRALayerOriginalEntry(result);
  end;
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
      RenderLayerFromOriginal(layer, true);
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
begin
  if (index < 0) or (index >= NbLayers) then exit;
  Unfreeze;
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
var i,idx: integer;
begin
  Clear;
  SetSize(ASource.Width,ASource.Height);
  LinearBlend:= ASource.LinearBlend;
  for i := 0 to ASource.NbLayers-1 do
  begin
    idx := AddOwnedLayer(ASource.GetLayerBitmapCopy(i),ASource.LayerOffset[i],ASource.BlendOperation[i],ASource.LayerOpacity[i]);
    LayerName[idx] := ASource.LayerName[i];
    LayerVisible[idx] := ASource.LayerVisible[i];
    if ASharedLayerIds and (ASource is TBGRALayeredBitmap) then
      LayerUniqueId[idx] := TBGRALayeredBitmap(ASource).LayerUniqueId[idx];
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

function TBGRALayeredBitmap.AddLayerFromOriginal(AGuid: TGuid;
  Opacity: byte): integer;
begin
  result := AddLayerFromOriginal(AGuid, DefaultBlendingOperation, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromOriginal(AGuid: TGuid;
  BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddLayerFromOriginal(AGuid, AffineMatrixIdentity, BlendOp, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromOriginal(AGuid: TGuid;
  Matrix: TAffineMatrix; Opacity: byte): integer;
begin
  result := AddLayerFromOriginal(AGuid, Matrix, DefaultBlendingOperation, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromOriginal(AGuid: TGuid;
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
  if AOriginal.Guid = GUID_NULL then
  begin
    if CreateGUID(newGuid)<> 0 then
      raise exception.Create('Error while creating GUID');
    AOriginal.Guid := newGuid;
  end else
  begin
    if IndexOfOriginal(AOriginal) <> -1 then
      raise exception.Create('Original already added');
    if IndexOfOriginal(AOriginal.Guid) <> -1 then
      raise exception.Create('GUID is already in use');
  end;
  StoreOriginal(AOriginal);
  if FOriginals = nil then FOriginals := TBGRALayerOriginalList.Create;
  if AOwned then
    result := FOriginals.Add(BGRALayerOriginalEntry(AOriginal))
  else
    result := FOriginals.Add(BGRALayerOriginalEntry(AOriginal.Guid));
end;

function TBGRALayeredBitmap.RemoveOriginal(AOriginal: TBGRALayerCustomOriginal): boolean;
var
  idx: Integer;
begin
  idx := IndexOfOriginal(AOriginal);
  if idx = -1 then exit(false);
  DeleteOriginal(idx);
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

  FOriginals[i].Instance.Free;
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
  ADraft: boolean);
var
  orig: TBGRALayerCustomOriginal;
  rAll, r, rInter: TRect;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds');

  orig := LayerOriginal[layer];
  if Assigned(orig) then
  begin
    if FLayers[layer].Owner then
      FreeAndNil(FLayers[layer].Source)
    else
      FLayers[layer].Source := nil;
    rAll := rect(0,0,Width,Height);
    r := orig.GetRenderBounds(rAll,FLayers[layer].OriginalMatrix);
    rInter := TRect.Intersect(r, rAll);
    FLayers[layer].Source := TBGRABitmap.Create(rInter.Width,rInter.Height);
    orig.Render(FLayers[layer].Source, AffineMatrixTranslation(-rInter.Left,-rInter.Top)*FLayers[layer].OriginalMatrix, ADraft);
    FLayers[layer].x := rInter.Left;
    FLayers[layer].y := rInter.Top;
  end;
  if ADraft then
    FLayers[layer].OriginalRenderStatus := orsDraft
  else
    FLayers[layer].OriginalRenderStatus := orsProof;
end;

procedure TBGRALayeredBitmap.RenderOriginalsIfNecessary(ADraft: boolean);
var
  i: Integer;
begin
  for i := 0 to NbLayers-1 do
    case LayerOriginalRenderStatus[i] of
    orsNone: RenderLayerFromOriginal(i, ADraft);
    orsDraft: if not ADraft then RenderLayerFromOriginal(i, ADraft);
    end;
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
    if idxOrig <> -1 then useCount[idxOrig] += 1;
  end;
  for i := high(useCount) downto 0 do
    if useCount[i] = 0 then DeleteOriginal(i);
end;

destructor TBGRALayeredBitmap.Destroy;
begin
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
begin
  SetSize(Height,Width); //unfreeze
  for i := 0 to NbLayers-1 do
    if LayerOriginalGuid[i] <> GUID_NULL then
      LayerOriginalMatrix[i] := AffineMatrixTranslation(Width,0)*AffineMatrixRotationDeg(90)*LayerOriginalMatrix[i]
    else
      SetLayerBitmap(i, LayerBitmap[i].RotateCW as TBGRABitmap, True);
end;

procedure TBGRALayeredBitmap.RotateCCW;
var i: integer;
begin
  SetSize(Height,Width); //unfreeze
  for i := 0 to NbLayers-1 do
    if LayerOriginalGuid[i] <> GUID_NULL then
      LayerOriginalMatrix[i] := AffineMatrixTranslation(0,Height)*AffineMatrixRotationDeg(-90)*LayerOriginalMatrix[i]
    else
      SetLayerBitmap(i, LayerBitmap[i].RotateCCW as TBGRABitmap, True);
end;

procedure TBGRALayeredBitmap.HorizontalFlip;
var i: integer;
begin
  Unfreeze;
  for i := 0 to NbLayers-1 do
  begin
    if FLayers[i].Owner then
      FLayers[i].Source.HorizontalFlip
    else
    begin
      FLayers[i].Source := FLayers[i].Source.Duplicate(True) as TBGRABitmap;
      FLayers[i].Source.HorizontalFlip;
      FLayers[i].Owner := true;
    end;
    FLayers[i].x := Width-FLayers[i].x-FLayers[i].Source.Width;
    FLayers[i].OriginalMatrix := AffineMatrixTranslation(+Width/2,0)*AffineMatrixScale(-1,1)*AffineMatrixTranslation(-Width/2,0)*FLayers[i].OriginalMatrix;
  end;
end;

procedure TBGRALayeredBitmap.VerticalFlip;
var i: integer;
begin
  Unfreeze;
  for i := 0 to NbLayers-1 do
  begin
    if FLayers[i].Owner then
      FLayers[i].Source.VerticalFlip
    else
    begin
      FLayers[i].Source := FLayers[i].Source.Duplicate(True) as TBGRABitmap;
      FLayers[i].Source.VerticalFlip;
      FLayers[i].Owner := true;
    end;
    FLayers[i].y := Height-FLayers[i].y-FLayers[i].Source.Height;
    FLayers[i].OriginalMatrix := AffineMatrixTranslation(0,+Height/2)*AffineMatrixScale(1,-1)*AffineMatrixTranslation(0,-Height/2)*FLayers[i].OriginalMatrix;
  end;
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
  if FLayers[i].OriginalGuid <> GUID_NULL then
    LayerOriginalMatrix[i] := AffineMatrixScale(AWidth/prevWidth,AHeight/prevHeight)*LayerOriginalMatrix[i]
  else
  begin
    oldFilter := LayerBitmap[i].ResampleFilter;
    LayerBitmap[i].ResampleFilter := AFineResampleFilter;
    resampled := LayerBitmap[i].Resample(AWidth,AHeight, AResampleMode) as TBGRABitmap;
    LayerBitmap[i].ResampleFilter := oldFilter;
    SetLayerBitmap(i, resampled, True);
  end;
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

function TBGRACustomLayeredBitmap.GetLayerOriginal(layer: integer): TBGRALayerCustomOriginal;
begin
  result := nil;
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
      if LayerVisible[i] and (LayerOpacity[i] > 0) then nbVisible += 1;
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

procedure RegisterLayerOriginal(AClass: TBGRALayerOriginalAny);
begin
  setlength(LayerOriginalClasses, length(LayerOriginalClasses)+1);
  LayerOriginalClasses[high(LayerOriginalClasses)] := AClass;
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

