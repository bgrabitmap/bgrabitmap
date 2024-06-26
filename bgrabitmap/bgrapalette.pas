// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Palette of colors for various purposes, loading/saving into various formats }
unit BGRAPalette;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, Avl_Tree, BGRABitmapTypes, FPimage;

const
  MaxLastAddedColors = 10;

type
  TBGRAPaletteFormat = integer;

const
  palUnknown : TBGRAPaletteFormat = 0;
  palPaintDotNet : TBGRAPaletteFormat = 1;
  palGimp : TBGRAPaletteFormat = 2;
  palAdobeSwatchExchange : TBGRAPaletteFormat = 3;
  palKOffice : TBGRAPaletteFormat = 4;
  palJascPSP : TBGRAPaletteFormat = 5;
  palCustom : TBGRAPaletteFormat = 100;

type
  { Indexed color in palette }
  TBGRAIndexedPaletteEntry = packed record
    Color: TBGRAPixel;
    Index: UInt32;
  end;
  PBGRAIndexedPaletteEntry = ^TBGRAIndexedPaletteEntry;
  { Weighted color in palette }
  TBGRAWeightedPaletteEntry = packed record
    Color: TBGRAPixel;
    Weight: UInt32;
  end;
  PBGRAWeightedPaletteEntry = ^TBGRAWeightedPaletteEntry;
  ArrayOfWeightedColor = array of TBGRAWeightedPaletteEntry;

  TBGRAPixelComparer = function (p1,p2 : PBGRAPixel): boolean;

  { Abstract class containing a palette }
  TBGRACustomPalette = class
  private
    function GetDominantColor: TBGRAPixel;
  protected
    function GetCount: integer; virtual; abstract;
    function GetColorByIndex(AIndex: integer): TBGRAPixel; virtual; abstract;
  public
    function ContainsColor(AValue: TBGRAPixel): boolean; virtual; abstract;
    function IndexOfColor(AValue: TBGRAPixel): integer; virtual; abstract;
    function GetAsArrayOfColor: ArrayOfTBGRAPixel; virtual; abstract;
    function GetAsArrayOfWeightedColor: ArrayOfWeightedColor; virtual; abstract;
    procedure AssignTo(AImage: TFPCustomImage); overload;
    procedure AssignTo(APalette: TFPPalette); overload;
    property DominantColor: TBGRAPixel read GetDominantColor;
    property Count: integer read GetCount;
    property Color[AIndex: integer]: TBGRAPixel read GetColorByIndex;
  end;

type
  { @abstract(Abstract implementation of a BGRA palette using a binary tree.)

    It ensures relatively fast and average access time even with many entries. }
  TBGRAAvgLvlPalette = class(TBGRACustomPalette)
  protected
    FTree: TAVLTree;
    FArray: array of PBGRAPixel;
    FLastAddedColors: packed array[0..MaxLastAddedColors-1] of PBGRAPixel;
    FLastAddedColorCount: integer;
    function GetCount: integer; override;
    function GetColorByIndex(AIndex: integer): TBGRAPixel; override;
    procedure FreeEntry(AEntry: PBGRAPixel); virtual; abstract;
    procedure NeedArray; virtual;
    procedure ClearArray; virtual;
    procedure AddLastColor(AColor: PBGRAPixel);
    function GetLastColor(AValue: TBGRAPixel): PBGRAPixel;
    procedure ClearLastColors;
  public
    constructor Create; overload;
    function ContainsColor(AValue: TBGRAPixel): boolean; override;
    function IndexOfColor(AValue: TBGRAPixel): integer; override;
    procedure Clear; virtual;
    destructor Destroy; override;
    function GetAsArrayOfColor: ArrayOfTBGRAPixel; override;
    function GetAsArrayOfWeightedColor: ArrayOfWeightedColor; override;
  end;

  { Palette of colors, roughly sorted by luminosity }
  TBGRAPalette = class(TBGRAAvgLvlPalette)
  protected
    function CreateEntry(AColor: TBGRAPixel): PBGRAPixel; virtual;
    procedure FreeEntry(AEntry: PBGRAPixel); override;
    procedure IncludePixel(PPixel: PBGRAPixel); virtual;
    procedure ExceptionUnknownPaletteFormat;
    procedure ExceptionInvalidPaletteFormat;
  public
    constructor Create(ABitmap: TBGRACustomBitmap); overload; virtual;
    constructor Create(APalette: TBGRACustomPalette); overload; virtual;
    constructor Create(AColors: ArrayOfTBGRAPixel); overload; virtual;
    constructor Create(AColors: ArrayOfWeightedColor); overload; virtual;
    function AddColor(AValue: TBGRAPixel): boolean; virtual;
    procedure AddColors(ABitmap: TBGRACustomBitmap); overload; virtual;
    procedure AddColors(APalette: TBGRACustomPalette); overload; virtual;
    function RemoveColor(AValue: TBGRAPixel): boolean; virtual;
    procedure LoadFromFile(AFilenameUTF8: string); virtual;
    procedure LoadFromStream(AStream: TStream; AFormat: TBGRAPaletteFormat); virtual;
    procedure LoadFromResource(AFilename: string; AFormat: TBGRAPaletteFormat);
    procedure SaveToFile(AFilenameUTF8: string); virtual;
    procedure SaveToStream(AStream: TStream; AFormat: TBGRAPaletteFormat); virtual;
    function DetectPaletteFormat(AStream: TStream): TBGRAPaletteFormat; overload; virtual;
    function DetectPaletteFormat(AFilenameUTF8: string): TBGRAPaletteFormat; overload;
    function SuggestPaletteFormat(AFilenameUTF8: string): TBGRAPaletteFormat; virtual;
  end;

  { Indexed palette of colors }
  TBGRAIndexedPalette = class(TBGRAPalette)
  private
    FCurrentIndex: UInt32;
  protected
    procedure NeedArray; override;
    function CreateEntry(AColor: TBGRAPixel): PBGRAPixel; override;
    procedure FreeEntry(AEntry: PBGRAPixel); override;
  public
    function RemoveColor({%H-}AValue: TBGRAPixel): boolean; override;
    function IndexOfColor(AValue: TBGRAPixel): integer; override;
    procedure Clear; override;
  end;

  { @abstract(Palette of weighted colors.)

    The weight is similar to a number of occurrences. It can be increased or decreased.
  }
  TBGRAWeightedPalette = class(TBGRAPalette)
  private
  protected
    function CreateEntry(AColor: TBGRAPixel): PBGRAPixel; override;
    procedure FreeEntry(AEntry: PBGRAPixel); override;
    function GetWeightByIndex(AIndex: Integer): UInt32; virtual;
    procedure IncludePixel(PPixel: PBGRAPixel); override;
  public
    constructor Create(AColors: ArrayOfWeightedColor); override;
    function GetAsArrayOfWeightedColor: ArrayOfWeightedColor; override;
    function IncColor(AValue: TBGRAPixel; out NewWeight: UInt32): boolean;
    procedure IncColors(ABitmap: TBGRACustomBitmap); overload; virtual;
    function DecColor(AValue: TBGRAPixel; out NewWeight: UInt32): boolean;
    property Weight[AIndex: Integer]: UInt32 read GetWeightByIndex;
  end;

  { Palette of colors indirectly specify by a pointer }
  TBGRAReferencePalette = class(TBGRAAvgLvlPalette)
  protected
    procedure FreeEntry({%H-}AEntry: PBGRAPixel); override;
  public
    function AddColor(AValue: PBGRAPixel): boolean;
    function RemoveColor(AValue: PBGRAPixel): boolean;
  end;

  { Abstract palette that can find an approximate matching color }
  TBGRACustomApproxPalette = class(TBGRACustomPalette)
  private
    function FindNearestColorIgnoreAlpha(AValue: TBGRAPixel): TBGRAPixel; inline;
    function FindNearestColorIndexIgnoreAlpha(AValue: TBGRAPixel): integer; inline;
  protected
    function GetWeightByIndex({%H-}AIndex: Integer): UInt32; virtual;
  public
    function FindNearestColor(AValue: TBGRAPixel; AIgnoreAlpha: boolean): TBGRAPixel; overload;
    function FindNearestColor(AValue: TBGRAPixel): TBGRAPixel; overload; virtual; abstract;
    function FindNearestColorIndex(AValue: TBGRAPixel; AIgnoreAlpha: boolean): integer; overload;
    function FindNearestColorIndex(AValue: TBGRAPixel): integer; overload; virtual; abstract;
    property Weight[AIndex: Integer]: UInt32 read GetWeightByIndex;
  end;

  { Palette containing all possible 16-bit colors. }
  TBGRA16BitPalette = class(TBGRACustomApproxPalette)
  protected
    function GetCount: integer; override;
    function GetColorByIndex(AIndex: integer): TBGRAPixel; override;
  public
    function ContainsColor(AValue: TBGRAPixel): boolean; override;
    function IndexOfColor(AValue: TBGRAPixel): integer; override;
    function GetAsArrayOfColor: ArrayOfTBGRAPixel; override;
    function GetAsArrayOfWeightedColor: ArrayOfWeightedColor; override;
    function FindNearestColor(AValue: TBGRAPixel): TBGRAPixel; override;
    function FindNearestColorIndex(AValue: TBGRAPixel): integer; override;
  end;

  { @abstract(Abstract class for color quantization.)

    Quantization is the process by which each color is reduced to an index into a palette.
    Surrounding pixels can affect the approximate color, an effect called dithering.
  }
  TBGRACustomColorQuantizer = class
  protected
    function GetDominantColor: TBGRAPixel; virtual;
    function GetPalette: TBGRACustomApproxPalette; virtual; abstract;
    function GetSourceColor(AIndex: integer): TBGRAPixel; virtual; abstract;
    function GetSourceColorCount: Integer; virtual; abstract;
    function GetReductionColorCount: integer; virtual; abstract;
    procedure SetReductionColorCount(AValue: Integer); virtual; abstract;
  public
    constructor Create(APalette: TBGRACustomPalette; ASeparateAlphaChannel: boolean); overload; virtual; abstract;
    constructor Create(AColors: array of TBGRAPixel; ASeparateAlphaChannel: boolean); overload;
    constructor Create(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption); overload; virtual; abstract;
    constructor Create(APalette: TBGRACustomPalette; ASeparateAlphaChannel: boolean; AReductionColorCount: integer); overload; virtual; abstract;
    constructor Create(AColors: array of TBGRAPixel; ASeparateAlphaChannel: boolean; AReductionColorCount: integer); overload;
    constructor Create(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption; AReductionColorCount: integer); overload; virtual; abstract;
    procedure ApplyDitheringInplace(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; ABounds: TRect); overload; virtual; abstract;
    procedure ApplyDitheringInplace(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap); overload;
    function GetDitheredBitmap(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; ABounds: TRect): TBGRACustomBitmap; overload; virtual; abstract;
    function GetDitheredBitmap(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap): TBGRACustomBitmap; overload;
    procedure SaveBitmapToFile(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; AFilenameUTF8: string); overload;
    procedure SaveBitmapToFile(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; AFilenameUTF8: string; AFormat: TBGRAImageFormat); overload;
    procedure SaveBitmapToStream(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; AStream: TStream; AFormat: TBGRAImageFormat); virtual; abstract;
    function GetDitheredBitmapIndexedData(ABitDepth: integer; AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; out AScanlineSize: PtrInt): Pointer; overload;
    function GetDitheredBitmapIndexedData(ABitDepth: integer; AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap): Pointer; overload;
    function GetDitheredBitmapIndexedData(ABitDepth: integer; AByteOrder: TRawImageByteOrder; AAlgorithm: TDitheringAlgorithm;
      ABitmap: TBGRACustomBitmap; out AScanlineSize: PtrInt): Pointer; overload; virtual; abstract;
    { Number colors provided in the source }
    property SourceColorCount: Integer read GetSourceColorCount;
    { Value of a color in the source }
    property SourceColor[AIndex: integer]: TBGRAPixel read GetSourceColor;
    { @abstract(Number of different colors in the reduced palette.)

    By default, there are 256 colors.

**Example to reduce an image to 16 colors:**
```pascal
uses BGRAColorQuantization, BGRABitmapTypes, BGRABitmap;
var
  quant : TBGRAColorQuantizer;
  sourceBmp: TBGRABitmap;
begin
  sourceBmp := TBGRABitmap.Create('picture.jpg');
  quant := TBGRAColorQuantizer.Create(sourceBmp, acIgnore);
  quant.ReductionColorCount := 16;
  quant.SaveBitmapToFile(daFloydSteinberg, sourceBmp, 'picture_in_4_bits.bmp');
  quant.Free;
  sourceBmp.Free;
end;
```}
    property ReductionColorCount: Integer read GetReductionColorCount write SetReductionColorCount;
    { Palette with reduced number of colors after applying quantization }
    property ReducedPalette: TBGRACustomApproxPalette read GetPalette;
    { Colors that is the most represented in the source }
    property DominantColor: TBGRAPixel read GetDominantColor;
  end;

  TBGRAColorQuantizerAny = class of TBGRACustomColorQuantizer;

var
  BGRAColorQuantizerFactory: TBGRAColorQuantizerAny;

function BGRARequiredBitDepth(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption): integer; overload;
function BGRARequiredBitDepth(APalette: TBGRACustomPalette): integer; overload;

type
  TPaletteReaderProc = function(APalette: TBGRAPalette; AStream: TStream): boolean;
  TPaletteWriterProc = procedure(APalette: TBGRAPalette; AStream: TStream);
  TCheckPaletteFormatProc = function(ABuf256: string): boolean;

procedure BGRARegisterPaletteFormat(AFormatIndex: TBGRAPaletteFormat; AExtension: string; ADescription: string;
  AReadProc: TPaletteReaderProc; AWriteProc: TPaletteWriterProc; ACheckFormatProc: TCheckPaletteFormatProc);
function BGRARegisteredPaletteFormatFilter(AAllSupportedDescription: string) : string;

procedure ArrayOfWeightedColor_QuickSort(AColors: ArrayOfWeightedColor; AMinIndex,
  AMaxIndex: Int32or64; AComparer: TBGRAPixelComparer = nil);

procedure ArrayOfWeightedColor_InsertionSort(AColors: ArrayOfWeightedColor; AMinIndex,
  AMaxIndex: Int32or64; AComparer: TBGRAPixelComparer = nil);

procedure ArrayOfTBGRAPixel_QuickSort(AColors: ArrayOfTBGRAPixel; AMinIndex,
  AMaxIndex: Int32or64; AComparer: TBGRAPixelComparer = nil);

procedure ArrayOfTBGRAPixel_InsertionSort(AColors: ArrayOfTBGRAPixel; AMinIndex,
  AMaxIndex: Int32or64; AComparer: TBGRAPixelComparer = nil);

implementation

uses BGRAUTF8, bufstream;

function IsDWordGreater(p1, p2: PBGRAPixel): boolean;
begin
  result := LongWord(p1^) > LongWord(p2^);
end;

const
  InsertionSortLimit = 10;

procedure ArrayOfWeightedColor_InsertionSort(AColors: ArrayOfWeightedColor; AMinIndex,
  AMaxIndex: Int32or64; AComparer: TBGRAPixelComparer = nil);
var i,j,insertPos: Int32or64;
  compared: TBGRAWeightedPaletteEntry;
begin
  if AComparer = nil then AComparer := @IsDWordGreater;
  for i := AMinIndex+1 to AMaxIndex do
  begin
    insertPos := i;
    compared := AColors[i];
    while (insertPos > AMinIndex) and AComparer(@AColors[insertPos-1].Color,@compared.Color) do
      dec(insertPos);
    if insertPos <> i then
    begin
      for j := i downto insertPos+1 do
        AColors[j] := AColors[j-1];
      AColors[insertPos] := compared;
    end;
  end;
end;

procedure ArrayOfWeightedColor_QuickSort(AColors: ArrayOfWeightedColor; AMinIndex,
  AMaxIndex: Int32or64; AComparer: TBGRAPixelComparer = nil);
var Pivot: TBGRAPixel;
  CurMin,CurMax,i : Int32or64;

  procedure Swap(a,b: Int32or64);
  var Temp: TBGRAWeightedPaletteEntry;
  begin
    if a = b then exit;
    Temp := AColors[a];
    AColors[a] := AColors[b];
    AColors[b] := Temp;
  end;
begin
  if AComparer = nil then AComparer := @IsDWordGreater;
  if AMaxIndex-AMinIndex+1 <= InsertionSortLimit then
  begin
    ArrayOfWeightedColor_InsertionSort(AColors,AMinIndex,AMaxIndex,AComparer);
    exit;
  end;
  Pivot := AColors[(AMinIndex+AMaxIndex) shr 1].Color;
  CurMin := AMinIndex;
  CurMax := AMaxIndex;
  i := CurMin;
  while i < CurMax do
  begin
    if AComparer(@AColors[i].Color, @Pivot) then
    begin
      Swap(i, CurMax);
      dec(CurMax);
    end else
    begin
      if AComparer(@Pivot, @AColors[i].Color) then
      begin
        Swap(i, CurMin);
        inc(CurMin);
      end;
      inc(i);
    end;
  end;
  if AComparer(@Pivot, @AColors[i].Color) then
  begin
    Swap(i, CurMin);
    inc(CurMin);
  end;
  if CurMin > AMinIndex then ArrayOfWeightedColor_QuickSort(AColors,AMinIndex,CurMin,AComparer);
  if CurMax < AMaxIndex then ArrayOfWeightedColor_QuickSort(AColors,CurMax,AMaxIndex,AComparer);
end;

procedure ArrayOfTBGRAPixel_InsertionSort(AColors: ArrayOfTBGRAPixel; AMinIndex,
  AMaxIndex: Int32or64; AComparer: TBGRAPixelComparer = nil);
var i,j,insertPos: Int32or64;
  compared: TBGRAPixel;
begin
  if AComparer = nil then AComparer := @IsDWordGreater;
  for i := AMinIndex+1 to AMaxIndex do
  begin
    insertPos := i;
    compared := AColors[i];
    while (insertPos > AMinIndex) and AComparer(@AColors[insertPos-1],@compared) do
      dec(insertPos);
    if insertPos <> i then
    begin
      for j := i downto insertPos+1 do
        AColors[j] := AColors[j-1];
      AColors[insertPos] := compared;
    end;
  end;
end;

procedure ArrayOfTBGRAPixel_QuickSort(AColors: ArrayOfTBGRAPixel; AMinIndex,
  AMaxIndex: Int32or64; AComparer: TBGRAPixelComparer = nil);
var Pivot: TBGRAPixel;
  CurMin,CurMax,i : Int32or64;

  procedure Swap(a,b: Int32or64);
  var Temp: TBGRAPixel;
  begin
    if a = b then exit;
    Temp := AColors[a];
    AColors[a] := AColors[b];
    AColors[b] := Temp;
  end;
begin
  if AComparer = nil then AComparer := @IsDWordGreater;
  if AMaxIndex-AMinIndex+1 <= InsertionSortLimit then
  begin
    ArrayOfTBGRAPixel_InsertionSort(AColors,AMinIndex,AMaxIndex,AComparer);
    exit;
  end;
  Pivot := AColors[(AMinIndex+AMaxIndex) shr 1];
  CurMin := AMinIndex;
  CurMax := AMaxIndex;
  i := CurMin;
  while i < CurMax do
  begin
    if AComparer(@AColors[i], @Pivot) then
    begin
      Swap(i, CurMax);
      dec(CurMax);
    end else
    begin
      if AComparer(@Pivot, @AColors[i]) then
      begin
        Swap(i, CurMin);
        inc(CurMin);
      end;
      inc(i);
    end;
  end;
  if AComparer(@Pivot, @AColors[i]) then
  begin
    Swap(i, CurMin);
    inc(CurMin);
  end;
  if CurMin > AMinIndex then ArrayOfTBGRAPixel_QuickSort(AColors,AMinIndex,CurMin,AComparer);
  if CurMax < AMaxIndex then ArrayOfTBGRAPixel_QuickSort(AColors,CurMax,AMaxIndex,AComparer);
end;

{$i paletteformats.inc}

function BGRARequiredBitDepth(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption): integer;
var
  palette: TBGRAPalette;
  p: PBGRAPixel;
  i: Int32or64;
  transparentEntry: boolean;
begin
  palette := TBGRAPalette.Create;
  p := ABitmap.Data;
  transparentEntry := false;
  if AAlpha = acIgnore then
  begin
    for i := ABitmap.NbPixels-1 downto 0 do
    begin
      palette.AddColor(BGRA(p^.red,p^.green,p^.blue));
      inc(p);
      if palette.Count > 256 then break;
    end;
  end else
  if AAlpha = acTransparentEntry then
  begin
    for i := ABitmap.NbPixels-1 downto 0 do
    begin
      if p^.alpha < 128 then
        transparentEntry:= true
      else
        palette.AddColor(BGRA(p^.red,p^.green,p^.blue));
      inc(p);
      if palette.Count > 256 then break;
    end;
  end else
  begin
    for i := ABitmap.NbPixels-1 downto 0 do
    begin
      palette.AddColor(p^);
      inc(p);
      if palette.Count > 256 then break;
    end;
  end;

  if palette.Count+byte(transparentEntry) > 256 then
  begin
    if (AAlpha = acFullChannelInPalette) and ABitmap.HasTransparentPixels then
      result := 32
    else
    if (AAlpha = acTransparentEntry) and ABitmap.HasTransparentPixels then
      result := 25
    else
      result := 24;
  end else
  begin
    result := 8;
    while (result > 0) and (1 shl (result shr 1) >= palette.Count) do result := result shr 1;
  end;
  palette.Free;
end;

function BGRARequiredBitDepth(APalette: TBGRACustomPalette): integer;
var i: integer;
  hasTransp: boolean;
begin
  if APalette.Count > 256 then
  begin
    hasTransp := false;
    for i := 0 to APalette.Count-1 do
      if APalette.Color[i].alpha <> 255 then
      begin
        hasTransp:= true;
        break;
      end;
    if hasTransp then
      result := 32
    else
      result := 24;
  end else
  begin
    result := 8;
    while (result > 0) and (1 shl (result shr 1) >= APalette.Count) do result := result shr 1;
  end;
end;

{ TBGRA16BitPalette }

function TBGRA16BitPalette.GetCount: integer;
begin
  result := 65537;
end;

function TBGRA16BitPalette.GetColorByIndex(AIndex: integer): TBGRAPixel;
begin
  if (AIndex >= 65536) or (AIndex < 0) then
    result := BGRAPixelTransparent
  else
    result := Color16BitToBGRA(AIndex);
end;

function TBGRA16BitPalette.ContainsColor(AValue: TBGRAPixel): boolean;
begin
  if AValue.alpha = 0 then
    result := true
  else
    result := (AValue.alpha = 255) and (FindNearestColor(AValue)=AValue);
end;

function TBGRA16BitPalette.IndexOfColor(AValue: TBGRAPixel): integer;
var idx: integer;
begin
  if AValue.Alpha = 0 then
    result := 65536
  else
  begin
    idx := BGRAToColor16Bit(AValue);
    if Color16BitToBGRA(idx)=AValue then
      result := idx
    else
      result := -1;
  end;
end;

function TBGRA16BitPalette.GetAsArrayOfColor: ArrayOfTBGRAPixel;
begin
  result := nil;
  raise exception.Create('Palette too big');
end;

function TBGRA16BitPalette.GetAsArrayOfWeightedColor: ArrayOfWeightedColor;
begin
  result := nil;
  raise exception.Create('Palette too big');
end;

function TBGRA16BitPalette.FindNearestColor(AValue: TBGRAPixel): TBGRAPixel;
begin
  if AValue.alpha = 0 then result := BGRAPixelTransparent
  else
    result := GetColorByIndex(BGRAToColor16Bit(AValue));
end;

function TBGRA16BitPalette.FindNearestColorIndex(AValue: TBGRAPixel): integer;
begin
  result := BGRAToColor16Bit(AValue);
end;

{ TBGRAIndexedPalette }

procedure TBGRAIndexedPalette.NeedArray;
var Node: TAVLTreeNode;
  n: UInt32;
begin
  n := Count;
  if UInt32(length(FArray)) <> n then
  begin
    setLength(FArray,n);
    for Node in FTree do
    with PBGRAIndexedPaletteEntry(Node.Data)^ do
    begin
      if Index < n then //index is unsigned so always >= 0
        FArray[Index] := @Color;
    end;
  end;
end;

function TBGRAIndexedPalette.CreateEntry(AColor: TBGRAPixel): PBGRAPixel;
begin
  result := PBGRAPixel(GetMem(sizeOf(TBGRAIndexedPaletteEntry)));
  result^ := AColor;
  PBGRAIndexedPaletteEntry(result)^.Index := FCurrentIndex;
  Inc(FCurrentIndex);
end;

procedure TBGRAIndexedPalette.FreeEntry(AEntry: PBGRAPixel);
begin
  FreeMem(AEntry);
end;

function TBGRAIndexedPalette.RemoveColor(AValue: TBGRAPixel): boolean;
begin
  Result:= false;
  raise exception.Create('It is not possible to remove a color from an indexed palette');
end;

function TBGRAIndexedPalette.IndexOfColor(AValue: TBGRAPixel): integer;
Var Node: TAVLTreeNode;
begin
  Node := FTree.Find(@AValue);
  if Assigned(Node) then
    result := PBGRAIndexedPaletteEntry(Node.Data)^.Index
  else
    result := -1;
end;

procedure TBGRAIndexedPalette.Clear;
begin
  inherited Clear;
  FCurrentIndex := 0;
end;

{ TBGRACustomColorQuantizer }

function TBGRACustomColorQuantizer.GetDominantColor: TBGRAPixel;
begin
  result := ReducedPalette.DominantColor;
end;

constructor TBGRACustomColorQuantizer.Create(AColors: array of TBGRAPixel;
  ASeparateAlphaChannel: boolean);
var palette: TBGRAPalette;
  i: Integer;
begin
  palette := TBGRAPalette.Create;
  for i := 0 to high(AColors) do
    palette.AddColor(AColors[i]);
  Create(palette, ASeparateAlphaChannel);
  palette.Free;
end;

constructor TBGRACustomColorQuantizer.Create(AColors: array of TBGRAPixel;
  ASeparateAlphaChannel: boolean; AReductionColorCount: integer);
var palette: TBGRAPalette;
  i: Integer;
begin
  palette := TBGRAPalette.Create;
  for i := 0 to high(AColors) do
    palette.AddColor(AColors[i]);
  Create(palette, ASeparateAlphaChannel, AReductionColorCount);
  palette.Free;
end;

procedure TBGRACustomColorQuantizer.ApplyDitheringInplace(
  AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap);
begin
  ApplyDitheringInplace(AAlgorithm, ABitmap, rect(0,0,ABitmap.Width,ABitmap.Height));
end;

function TBGRACustomColorQuantizer.GetDitheredBitmap(
  AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap
  ): TBGRACustomBitmap;
begin
  result := GetDitheredBitmap(AAlgorithm, ABitmap, rect(0,0,ABitmap.Width,ABitmap.Height));
end;

procedure TBGRACustomColorQuantizer.SaveBitmapToFile(
  AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap;
  AFilenameUTF8: string);
begin
  SaveBitmapToFile(AAlgorithm, ABitmap, AFilenameUTF8, SuggestImageFormat(AFilenameUTF8));
end;

procedure TBGRACustomColorQuantizer.SaveBitmapToFile(
  AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap;
  AFilenameUTF8: string; AFormat: TBGRAImageFormat);
var
  stream: TFileStreamUTF8;
begin
   stream := TFileStreamUTF8.Create(AFilenameUTF8,fmCreate);
   try
     SaveBitmapToStream(AAlgorithm, ABitmap, stream, AFormat);
   finally
     stream.Free;
   end;
end;

function TBGRACustomColorQuantizer.GetDitheredBitmapIndexedData(
  ABitDepth: integer; AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap;
  out AScanlineSize: PtrInt): Pointer;
begin
  result := GetDitheredBitmapIndexedData(ABitDepth,
  {$IFDEF ENDIAN_LITTLE}riboLSBFirst{$ELSE}riboMSBFirst{$endif},
  AAlgorithm, ABitmap, AScanlineSize);
end;

function TBGRACustomColorQuantizer.GetDitheredBitmapIndexedData(
  ABitDepth: integer; AAlgorithm: TDitheringAlgorithm;
  ABitmap: TBGRACustomBitmap): Pointer;
var dummy: PtrInt;
begin
  result := GetDitheredBitmapIndexedData(ABitDepth, AAlgorithm, ABitmap, dummy);
end;

{ TBGRACustomPalette }

function TBGRACustomPalette.GetDominantColor: TBGRAPixel;
var
  w: ArrayOfWeightedColor;
  i: Integer;
  maxWeight, totalWeight: UInt32;
begin
  result := BGRAWhite;
  maxWeight := 0;
  w := GetAsArrayOfWeightedColor;
  totalWeight:= 0;
  for i := 0 to high(w) do
    inc(totalWeight, w[i].Weight);
  for i := 0 to high(w) do
    if (w[i].Weight > maxWeight) and (BGRAToGSBA(w[i].Color).saturation > 16000) then
    begin
      maxWeight:= w[i].Weight;
      result := w[i].Color;
    end;
  if maxWeight > totalWeight div 20 then exit;
  for i := 0 to high(w) do
    if (w[i].Weight > maxWeight) and (BGRAToGSBA(w[i].Color).lightness < 56000) and (BGRAToGSBA(w[i].Color).lightness > 16000) then
    begin
      maxWeight:= w[i].Weight;
      result := w[i].Color;
    end;
  if maxWeight > 0 then exit;
  for i := 0 to high(w) do
    if (w[i].Weight > maxWeight) then
    begin
      maxWeight:= w[i].Weight;
      result := w[i].Color;
    end;
end;

procedure TBGRACustomPalette.AssignTo(AImage: TFPCustomImage);
begin
  AImage.UsePalette := true;
  AssignTo(AImage.Palette);
end;

procedure TBGRACustomPalette.AssignTo(APalette: TFPPalette);
var i: integer;
begin
  APalette.Clear;
  APalette.Capacity := Count;
  for i := 0 to Count-1 do
    APalette.Color[i] := BGRAToFPColor(Color[i]);
end;

{ TBGRACustomApproxPalette }

function TBGRACustomApproxPalette.FindNearestColorIgnoreAlpha(AValue: TBGRAPixel): TBGRAPixel;
var saveAlpha: byte;
begin
  if AValue.alpha = 0 then
    result := BGRAPixelTransparent
  else
  begin
    saveAlpha := AValue.alpha;
    AValue.alpha := 255;
    result := FindNearestColor(AValue);
    result.alpha := saveAlpha;
  end;
end;

function TBGRACustomApproxPalette.FindNearestColorIndexIgnoreAlpha(
  AValue: TBGRAPixel): integer;
begin
  if AValue.alpha = 0 then
    result := -1
  else
  begin
    AValue.alpha := 255;
    result := FindNearestColorIndex(AValue);
  end;
end;

function TBGRACustomApproxPalette.GetWeightByIndex(AIndex: Integer): UInt32;
begin
  result := 1;
end;

function TBGRACustomApproxPalette.FindNearestColor(AValue: TBGRAPixel; AIgnoreAlpha: boolean): TBGRAPixel;
begin
  if AIgnoreAlpha then
    result := FindNearestColorIgnoreAlpha(AValue)
  else
    result := FindNearestColor(AValue);
end;

function TBGRACustomApproxPalette.FindNearestColorIndex(AValue: TBGRAPixel;
  AIgnoreAlpha: boolean): integer;
begin
  if AIgnoreAlpha then
    result := FindNearestColorIndexIgnoreAlpha(AValue)
  else
    result := FindNearestColorIndex(AValue);
end;

{ TBGRAWeightedPalette }

function TBGRAWeightedPalette.GetWeightByIndex(AIndex: Integer): UInt32;
begin
  NeedArray;
  if (AIndex >= 0) and (AIndex < length(FArray)) then
    result := PBGRAWeightedPaletteEntry(FArray[AIndex])^.Weight
  else
    raise ERangeError.Create('Index out of bounds');
end;

procedure TBGRAWeightedPalette.IncludePixel(PPixel: PBGRAPixel);
var dummy: UInt32;
begin
  IncColor(PPixel^,dummy);
end;

constructor TBGRAWeightedPalette.Create(AColors: ArrayOfWeightedColor);
var
  i: Integer;
begin
  inherited Create;
  for i := 0 to high(AColors) do
    with AColors[i] do IncColor(Color,Weight);
end;

function TBGRAWeightedPalette.GetAsArrayOfWeightedColor: ArrayOfWeightedColor;
var
  i: Int32or64;
begin
  NeedArray;
  setlength(result, length(FArray));
  for i := 0 to high(result) do
    result[i] := PBGRAWeightedPaletteEntry(FArray[i])^;
end;

function TBGRAWeightedPalette.CreateEntry(AColor: TBGRAPixel): PBGRAPixel;
begin
  result := PBGRAPixel(GetMem(sizeOf(TBGRAWeightedPaletteEntry)));
  result^ := AColor;
  PBGRAWeightedPaletteEntry(result)^.Weight := 1;
end;

procedure TBGRAWeightedPalette.FreeEntry(AEntry: PBGRAPixel);
begin
  FreeMem(AEntry);
end;

function TBGRAWeightedPalette.IncColor(AValue: TBGRAPixel; out NewWeight: UInt32
  ): boolean;
Var Node: TAVLTreeNode;
  Entry: PBGRAPixel;
begin
  Entry := GetLastColor(AValue);
  if Entry <> nil then
  begin
    NewWeight := PBGRAWeightedPaletteEntry(Entry)^.Weight+1;
    PBGRAWeightedPaletteEntry(Entry)^.Weight := NewWeight;
    result := false;
    exit;
  end;
  Node := FTree.Find(@AValue);
  if Assigned(Node) then
  begin
    Entry := PBGRAPixel(Node.Data);
    NewWeight := PBGRAWeightedPaletteEntry(Entry)^.Weight+1;
    PBGRAWeightedPaletteEntry(Entry)^.Weight := NewWeight;
    AddLastColor(Entry);
    result := false;
  end
  else
  begin
    Entry := CreateEntry(AValue);
    FTree.Add(Entry);
    ClearArray;
    NewWeight := PBGRAWeightedPaletteEntry(Entry)^.Weight;
    AddLastColor(Entry);
    result := true;
  end;
end;

procedure TBGRAWeightedPalette.IncColors(ABitmap: TBGRACustomBitmap);
var p: PBGRAPixel;
  n: integer;
  w: UInt32;
begin
  n := ABitmap.NbPixels;
  p := ABitmap.Data;
  while n > 0 do
  begin
    IncColor(p^, w);
    inc(p);
    dec(n);
  end;
end;

function TBGRAWeightedPalette.DecColor(AValue: TBGRAPixel; out NewWeight: UInt32
  ): boolean;
var
  Node : TAVLTreeNode;
  Entry: PBGRAPixel;
begin
  Node := FTree.Find(@AValue);
  if Assigned(Node) then
  begin
    Entry := PBGRAPixel(Node.Data);
    NewWeight := PBGRAWeightedPaletteEntry(Entry)^.Weight;
    if NewWeight >= 2 then
    begin
      dec(NewWeight);
      PBGRAWeightedPaletteEntry(Entry)^.Weight := NewWeight;
    end
    else
    begin
      NewWeight := 0;
      FreeEntry(Entry);
      FTree.Delete(Node);
      ClearArray;
      ClearLastColors;
    end;
    result := true;
  end else
  begin
    result := false;
    NewWeight := 0;
  end;
end;

{ TBGRAReferencePalette }

procedure TBGRAReferencePalette.FreeEntry(AEntry: PBGRAPixel);
begin
  //nothing
end;

function TBGRAReferencePalette.AddColor(AValue: PBGRAPixel): boolean;
begin
  if Assigned(GetLastColor(AValue^)) then
  begin
    result := false;
    exit;
  end;
  AddLastColor(AValue);
  if Assigned(FTree.Find(AValue)) then
  begin
    result := false;
  end
  else
  begin
    result := true;
    FTree.Add(AValue);
    ClearArray;
  end;
end;

function TBGRAReferencePalette.RemoveColor(AValue: PBGRAPixel): boolean;
var
  Node : TAVLTreeNode;
begin
  Node := FTree.Find(AValue);
  if Assigned(Node) then
  begin
    FTree.Delete(Node);
    result := true;
    ClearArray;
    ClearLastColors;
  end else
    result := false;
end;

function PaletteOnCompareItems(Data1, Data2: Pointer): integer;
var gray1, gray2: UInt32or64;
  c1, c2: TBGRAPixel;
begin
  c1 := PBGRAPixel(Data1)^;
  c2 := PBGRAPixel(Data2)^;
  if c1.alpha < c2.alpha then
    result := -1
  else if c1.alpha > c2.alpha then
    result := 1
  else
  begin
    gray1 := (GammaExpansionTab[c1.red] shl 8)+(GammaExpansionTab[c1.green] shl 9)+(GammaExpansionTab[c1.blue] shl 7);
    gray2 := (GammaExpansionTab[c2.red] shl 8)+(GammaExpansionTab[c2.green] shl 9)+(GammaExpansionTab[c2.blue] shl 7);
    if gray1<gray2 then
      result := -1
    else if gray1>gray2 then
      result := 1
    else
    begin
      if c1.green > c2.green then
        result := 1
      else if c1.green < c2.green then
        result := -1
      else if c1.red > c2.red then
        result := 1
      else if c1.red < c2.red then
        result := -1
      else if c1.blue > c2.blue then
        result := 1
      else if c1.blue < c2.blue then
        result := -1
      else
        result := 0;
    end;
  end;
end;

{ TBGRAAvgLvlPalette }

constructor TBGRAAvgLvlPalette.Create;
begin
  FTree := TAVLTree.Create;
  FTree.OnCompare := @PaletteOnCompareItems;
end;

destructor TBGRAAvgLvlPalette.Destroy;
begin
  Clear;
  FreeAndNil(FTree);
  inherited Destroy;
end;

function TBGRAAvgLvlPalette.GetAsArrayOfColor: ArrayOfTBGRAPixel;
var i: Int32or64;
begin
  NeedArray;
  setlength(result, Length(FArray));
  for i := 0 to high(result) do
    result[i] := FArray[i]^;
end;

function TBGRAAvgLvlPalette.GetAsArrayOfWeightedColor: ArrayOfWeightedColor;
var i: Int32or64;
begin
  NeedArray;
  setlength(result, Length(FArray));
  for i := 0 to high(result) do
  with result[i] do
  begin
    Color := FArray[i]^;
    Weight:= 1;
  end;
end;

procedure TBGRAAvgLvlPalette.Clear;
var Node: TAVLTreeNode;
begin
  For Node in FTree do
    FreeEntry(PBGRAPixel(Node.Data));
  FTree.Clear;
  ClearArray;
  FLastAddedColorCount := 0;
end;

function TBGRAAvgLvlPalette.GetCount: integer;
begin
  result := FTree.Count;
end;

function TBGRAAvgLvlPalette.ContainsColor(AValue: TBGRAPixel): boolean;
Var Node: TAVLTreeNode;
begin
  if Assigned(GetLastColor(AValue)) then
  begin
    result := true;
    exit;
  end;
  Node := FTree.Find(@AValue);
  result := Assigned(Node);
  if result then AddLastColor(PBGRAPixel(Node.Data));
end;

function TBGRAAvgLvlPalette.IndexOfColor(AValue: TBGRAPixel): integer;
Var Node: TAVLTreeNode;
begin
  Node := FTree.Find(@AValue);
  if Assigned(Node) then
  begin
    result := 0;
    Node := Node.Left;
    while Assigned(Node) do
    begin
      inc(result);
      Node := Node.Left;
    end;
  end else
    result := -1;
end;

function TBGRAAvgLvlPalette.GetColorByIndex(AIndex: integer): TBGRAPixel;
begin
  NeedArray;
  if (AIndex >= 0) and (AIndex < length(FArray)) then
    result := FArray[AIndex]^
  else
    raise ERangeError.Create('Index out of bounds');
end;

procedure TBGRAAvgLvlPalette.NeedArray;
var Node: TAVLTreeNode;
  i,n: integer;
begin
  n := Count;
  if length(FArray) <> n then
  begin
    setLength(FArray,n);
    i := 0;
    for Node in FTree do
    begin
      if i >= n then break;
      FArray[i] := PBGRAPixel(Node.Data);
      inc(i);
    end;
  end;
end;

procedure TBGRAAvgLvlPalette.ClearArray;
begin
  FArray := nil;
end;

procedure TBGRAAvgLvlPalette.AddLastColor(AColor: PBGRAPixel);
begin
  if FLastAddedColorCount < MaxLastAddedColors then
  begin
    FLastAddedColors[FLastAddedColorCount] := AColor;
    inc(FLastAddedColorCount);
  end else
  begin
    move(FLastAddedColors[1],FLastAddedColors[0],(FLastAddedColorCount-1)*sizeof(PBGRAPixel));
    FLastAddedColors[FLastAddedColorCount-1] := AColor;
  end;
end;

function TBGRAAvgLvlPalette.GetLastColor(AValue: TBGRAPixel): PBGRAPixel;
var
  i: Int32or64;
begin
  for i := FLastAddedColorCount-1 downto 0 do
    if PLongWord(FLastAddedColors[i])^ = LongWord(AValue) then
    begin
      result := FLastAddedColors[i];
      exit;
    end;
  result := nil;
end;

procedure TBGRAAvgLvlPalette.ClearLastColors;
begin
  FLastAddedColorCount := 0;
end;

{ TBGRAPalette }

function TBGRAPalette.CreateEntry(AColor: TBGRAPixel): PBGRAPixel;
begin
  result := PBGRAPixel(GetMem(sizeOf(TBGRAPixel)));
  result^ := AColor;
end;

procedure TBGRAPalette.FreeEntry(AEntry: PBGRAPixel);
begin
  FreeMem(AEntry);
end;

procedure TBGRAPalette.IncludePixel(PPixel: PBGRAPixel);
begin
  AddColor(PPixel^);
end;

procedure TBGRAPalette.ExceptionUnknownPaletteFormat;
begin
  raise Exception.Create('Unknown palette format');
end;

procedure TBGRAPalette.ExceptionInvalidPaletteFormat;
begin
  raise Exception.Create('Invalid palette format');
end;

constructor TBGRAPalette.Create(ABitmap: TBGRACustomBitmap);
var p: PBGRAPixel;
  n: integer;
begin
  inherited Create;
  n:= ABitmap.NbPixels;
  p := ABitmap.Data;
  while n > 0 do
  begin
    IncludePixel(p);
    inc(p);
    dec(n);
  end;
end;

constructor TBGRAPalette.Create(APalette: TBGRACustomPalette);
begin
  inherited Create;
  AddColors(APalette);
end;

constructor TBGRAPalette.Create(AColors: ArrayOfTBGRAPixel);
var
  i: Integer;
begin
  inherited Create;
  for i := 0 to high(AColors) do
    AddColor(AColors[i]);
end;

constructor TBGRAPalette.Create(AColors: ArrayOfWeightedColor);
var
  i: Integer;
begin
  inherited Create;
  for i := 0 to high(AColors) do
    AddColor(AColors[i].Color);
end;

function TBGRAPalette.AddColor(AValue: TBGRAPixel): boolean;
Var Node: TAVLTreeNode;
  Entry: PBGRAPixel;
begin
  if Assigned(GetLastColor(AValue)) then
  begin
    result := false;
    exit;
  end;
  Node := FTree.Find(@AValue);
  if Assigned(Node) then
  begin
    AddLastColor(PBGRAPixel(Node.Data));
    result := false;
  end
  else
  begin
    result := true;
    Entry := CreateEntry(AValue);
    FTree.Add(Entry);
    ClearArray;
    AddLastColor(Entry);
  end;
end;

procedure TBGRAPalette.AddColors(ABitmap: TBGRACustomBitmap);
var p: PBGRAPixel;
  n: integer;
begin
  n := ABitmap.NbPixels;
  p := ABitmap.Data;
  while n > 0 do
  begin
    AddColor(p^);
    inc(p);
    dec(n);
  end;
end;

procedure TBGRAPalette.AddColors(APalette: TBGRACustomPalette);
var i: Int32or64;
begin
  for i := 0 to APalette.Count- 1 do
    AddColor(APalette.Color[i]);
end;

function TBGRAPalette.RemoveColor(AValue: TBGRAPixel): boolean;
var
  Node : TAVLTreeNode;
begin
  Node := FTree.Find(@AValue);
  if Assigned(Node) then
  begin
    FreeEntry(Node.Data);
    FTree.Delete(Node);
    result := true;
    ClearArray;
    ClearLastColors;
  end else
    result := false;
end;

procedure TBGRAPalette.LoadFromFile(AFilenameUTF8: string);
var
  stream: TFileStreamUTF8;
  format: TBGRAPaletteFormat;
begin
  format := DetectPaletteFormat(AFilenameUTF8);
  if format = palUnknown then
  begin
    ExceptionUnknownPaletteFormat;
    exit;
  end;
  stream:= TFileStreamUTF8.Create(AFilenameUTF8,fmOpenRead);
  try
    LoadFromStream(stream, format);
  finally
    stream.Free;
  end;
end;

procedure TBGRAPalette.LoadFromStream(AStream: TStream;
  AFormat: TBGRAPaletteFormat);
var buf: TReadBufStream;
  handled: boolean;
  i: Integer;
begin
  RegisterDefaultPaletteFormats;
  Clear;
  buf := TReadBufStream.Create(AStream);
  try
    handled := false;
    for i := 0 to High(PaletteFormats) do
      if PaletteFormats[i].formatIndex = AFormat then
      begin
        if not PaletteFormats[i].reader(self, AStream) then
          ExceptionInvalidPaletteFormat;
        handled := true;
        break;
      end;
    if not handled then ExceptionUnknownPaletteFormat;
  finally
    buf.Free;
  end;
end;

procedure TBGRAPalette.LoadFromResource(AFilename: string; AFormat: TBGRAPaletteFormat);
var
  stream: TStream;
begin
  stream := BGRAResource.GetResourceStream(AFilename);
  try
    LoadFromStream(stream, AFormat);
  finally
    stream.Free;
  end;
end;

procedure TBGRAPalette.SaveToFile(AFilenameUTF8: string);
var
  stream: TFileStreamUTF8;
begin
  stream:= TFileStreamUTF8.Create(AFilenameUTF8,fmCreate);
  try
    SaveToStream(stream, SuggestPaletteFormat(AFilenameUTF8));
  finally
    stream.Free;
  end;
end;

procedure TBGRAPalette.SaveToStream(AStream: TStream;
  AFormat: TBGRAPaletteFormat);
var buf: TWriteBufStream;
  handled: boolean;
  i: Integer;
begin
  RegisterDefaultPaletteFormats;
  buf := TWriteBufStream.Create(AStream);
  try
    handled := false;
    for i := 0 to High(PaletteFormats) do
      if PaletteFormats[i].formatIndex = AFormat then
      begin
        PaletteFormats[i].writer(self, AStream);
        handled := true;
        break;
      end;
    if not handled then ExceptionUnknownPaletteFormat;
  finally
    buf.Free;
  end;
end;

function TBGRAPalette.DetectPaletteFormat(AStream: TStream): TBGRAPaletteFormat;
var buf: string;
  oldPos: int64;
  i: Integer;
begin
  result := palUnknown;
  setlength(buf,256);
  fillchar(buf[1],length(buf),#0);
  oldPos := AStream.Position;
  AStream.Read(buf[1],length(buf));
  AStream.Position := oldPos;
  if length(buf)>0 then
  begin
    RegisterDefaultPaletteFormats;
    for i := 0 to high(PaletteFormats) do
      if PaletteFormats[i].checkFormat(buf) then
      begin
        result := PaletteFormats[i].formatIndex;
        exit;
      end;
  end;
end;

function TBGRAPalette.DetectPaletteFormat(AFilenameUTF8: string
  ): TBGRAPaletteFormat;
var stream: TFileStreamUTF8;
begin
  result := SuggestPaletteFormat(AFilenameUTF8);
  if not FileExists(AFilenameUTF8) then exit;
  try
    stream := TFileStreamUTF8.Create(AFilenameUTF8,fmOpenRead or fmShareDenyWrite);
  except
    exit;
  end;
  try
    result := DetectPaletteFormat(stream);
    if result = palUnknown then
      result := SuggestPaletteFormat(AFilenameUTF8);
  finally
    stream.Free;
  end;
end;

function TBGRAPalette.SuggestPaletteFormat(AFilenameUTF8: string
  ): TBGRAPaletteFormat;
var ext: string;
  i: Integer;
begin
  RegisterDefaultPaletteFormats;
  ext := ExtractFileExt(AFilenameUTF8);
  if ext <> '' then
  begin
    for i := 0 to high(PaletteFormats) do
      if CompareText(PaletteFormats[i].ext,ext) = 0 then
      begin
        result := PaletteFormats[i].formatIndex;
        exit;
      end;
  end;
  result := palUnknown;
end;

end.

