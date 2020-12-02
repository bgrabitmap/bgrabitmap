// SPDX-License-Identifier: LGPL-3.0-linking-exception
{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}

type
  TSpectralLocusPoint = record
    W,X,Y,Z: Single;
  end;
  TIlluminantSpectrumPoint = record
    W,Y: Single;
  end;

{$I spectraldata.inc}

type
  PXYZReferenceWhite = ^TXYZReferenceWhite;
  TXYZReferenceWhite = packed record
    X, Y, Z: single;
    ObserverAngle: integer;
    Illuminant: string;
    L,M,S: single;
  end;

  TCustomColorspace = class;
  TColorspaceAny = class of TCustomColorspace;

  TColorspaceConvertArrayProc = procedure(ASource: pointer; ADest: Pointer; ACount: integer;
                  ASourceStride:integer; ADestStride:integer; AReferenceWhite: PXYZReferenceWhite);

  { TBridgedConversion }

  TBridgedConversion = record
    ConvertToBridge,FinalConvert: TColorspaceConvertArrayProc;
    procedure Convert(ASource: pointer; ADest: Pointer; ACount: integer;
              ASourceStride:integer; ADestStride:integer; AReferenceWhite: PXYZReferenceWhite); inline;
  end;

  TColorTransparency = (ctFullyTransparent, ctSemiTransparent, ctFullyOpaque);
  TColorspaceFlag = (cfFixedReferenceWhite,       // ex: sRGB, AdobeRGB fixed at D65
                     cfMovableReferenceWhite,     // XYZ
                     cfReferenceWhiteIndependent, // L*a*b*, LCh
                     cfHasImaginaryColors);       // XYZ, L*a*b*, LCh
  TColorspaceFlags = set of TColorspaceFlag;

  { TCustomColorspace }

  TCustomColorspace = class
    class function GetChannelName(AIndex: integer): string; virtual; abstract;
    class function GetChannelCount: integer; virtual; abstract;
    class function IndexOfChannel(AName: string): integer;
    class function IndexOfAlphaChannel: integer; virtual; abstract;
    class function GetColorTransparency(AColor: Pointer): TColorTransparency; virtual; abstract;
    class function GetMaxValue(AIndex: integer): single; virtual; abstract;
    class function GetMinValue(AIndex: integer): single; virtual; abstract;
    class function GetChannelBitDepth(AIndex: integer): byte; virtual; abstract;
    class function GetName: string; virtual; abstract;
    class function GetSize: integer; virtual; abstract;
    class function GetChannel(AColor: Pointer; AIndex: integer): single; virtual; abstract;
    class procedure SetChannel(AColor: Pointer; AIndex: integer; AValue: single); virtual; abstract;
    class procedure Convert(const ASource; out ADest; ADestColorspace: TColorspaceAny;
                            ACount: integer = 1; AReferenceWhite: PXYZReferenceWhite = nil);
    class function GetDirectConversion(ADestColorspace: TColorspaceAny): TColorspaceConvertArrayProc;
    class function GetBridgedConversion(ADestColorspace: TColorspaceAny): TBridgedConversion;
    class function GetFlags: TColorspaceFlags; virtual; abstract;
  end;

  { ColorspaceCollection }

  ColorspaceCollection = class
  protected
    class var FColorspaces : array of TColorspaceAny;
    class var FColorspaceCount: integer;
    class var FColorspaceConversions: array of array of TColorspaceConvertArrayProc;
  public
    class function GetCount: integer; static;
    class function GetItem(AIndex: integer): TColorspaceAny; static;
    class function IndexOf(AColorspace: TColorspaceAny): integer; static;
    class procedure Add(AColorspace: TColorspaceAny); static;
    class procedure AddConversion(ASource: TColorspaceAny; ADest: TColorspaceAny; AConversion: TColorspaceConvertArrayProc); static;
    class function GetDirectConversion(ASource: TColorspaceAny; ADest: TColorspaceAny): TColorspaceConvertArrayProc; static;
    class function GetBridgedConversion(ASource: TColorspaceAny; ADest: TColorspaceAny): TBridgedConversion; static;
  end;

type {* How to handle overflow when converting from XYZ }
  TColorspaceOverflow =
    {** Colors outside of target colorspace are converted to transparent }
    (xroClipToTarget,
    {** Each color channel is saturated independently (hue may be lost) }
    xroSaturateEachChannel,
    {** Hue is preserved by reducing intensity or saturation }
    xroPreserveHue);

var
  XYZToRGBOverflowMin : TColorspaceOverflow = xroSaturateEachChannel;
  XYZToRGBOverflowMax : TColorspaceOverflow = xroSaturateEachChannel;

{$DEFINE INCLUDE_INTERFACE}
{$I generatedcolorspace.inc}

function BGRAToMask(const ABGRAPixel: TBGRAPixel): TByteMask;
function ExpandedPixelToByteMask(const AExpandedPixel: TExpandedPixel): TByteMask;
function MaskToBGRA(const AMask: TByteMask; AAlpha: byte = 255): TBGRAPixel;
function ByteMaskToExpandedPixel(const AMask: TByteMask; AAlpha: byte = 255): TExpandedPixel;
function StdRGBAToBGRAPixel(const AStdRGBA: TStdRGBA): TBGRAPixel;
function BGRAPixelToStdRGBA(const ABGRAPixel: TBGRAPixel): TStdRGBA;
function LinearRGBAToExpandedPixel(const ALinearRGBA: TLinearRGBA): TExpandedPixel;
function ExpandedPixelToLinearRGBA(const AExpandedPixel: TExpandedPixel): TLinearRGBA;
function ExpandedPixelToStdRGBA(const AExpandedPixel: TExpandedPixel): TStdRGBA;
function GammaCompressionWF(AValue: Word): single;
function StdRGBAToExpandedPixel(const AStdRGBA: TStdRGBA): TExpandedPixel;
function GammaExpansionFW(AValue: single): word;
function LinearRGBAToXYZA(const ALinearRGBA: TLinearRGBA): TXYZA; overload;
function LinearRGBAToXYZA(const ALinearRGBA: TLinearRGBA; const AReferenceWhite: TXYZReferenceWhite): TXYZA; overload;
function XYZAToLinearRGBA(const AXYZA: TXYZA): TLinearRGBA; overload;
function XYZAToLinearRGBA(const AXYZA: TXYZA; const AReferenceWhite: TXYZReferenceWhite): TLinearRGBA; overload;
function ExpandedPixelToWordXYZA(const AExpandedPixel: TExpandedPixel): TWordXYZA; overload;
function ExpandedPixelToWordXYZA(const AExpandedPixel: TExpandedPixel; const AReferenceWhite: TXYZReferenceWhite): TWordXYZA; overload;
function XYZAToWordXYZA(const AXYZA: TXYZA): TWordXYZA;
function WordXYZAToXYZA(const AWordXYZA: TWordXYZA): TXYZA;
function WordXYZAToExpandedPixel(const AXYZA: TWordXYZA): TExpandedPixel; overload;
function WordXYZAToExpandedPixel(const AXYZA: TWordXYZA; const AReferenceWhite: TXYZReferenceWhite): TExpandedPixel; overload;
function XYZAToLabA(const AXYZA: TXYZA): TLabA; overload;
function XYZAToLabA(const AXYZA: TXYZA; const AReferenceWhite: TXYZReferenceWhite): TLabA; overload;
function LabAToXYZA(const ALabA: TLabA): TXYZA; overload;
function LabAToXYZA(const ALabA: TLabA; const AReferenceWhite: TXYZReferenceWhite): TXYZA; overload;
function StdRGBAToLinearRGBA(const AStdRGBA: TStdRGBA): TLinearRGBA;
function LinearRGBAToStdRGBA(const ALinearRGBA: TLinearRGBA): TStdRGBA;
function StdRGBAToStdHSLA(const AStdRGBA: TStdRGBA): TStdHSLA;
function StdHSLAToStdRGBA(const AStdHSLA: TStdHSLA): TStdRGBA;
function StdRGBAToStdHSVA(const AStdRGBA: TStdRGBA): TStdHSVA;
function StdHSVAToStdRGBA(const AStdHSVA: TStdHSVA): TStdRGBA;
function StdHSLAToStdHSVA(const AStdHSLA: TStdHSLA): TStdHSVA;
function StdHSVAToStdHSLA(const AStdHSVA: TStdHSVA): TStdHSLA;
function StdRGBAToStdCMYK(const AStdRGBA: TStdRGBA): TStdCMYK;
function StdCMYKToStdRGBA(const AStdCMYK: TStdCMYK; AAlpha: Single = 1): TStdRGBA;
function LabAToLChA(const ALabA: TLabA): TLChA;
function LChAToLabA(const ALChA: TLChA): TLabA;
function AdobeRGBAToXYZA(const ASource: TAdobeRGBA): TXYZA; overload;
function AdobeRGBAToXYZA(const ASource: TAdobeRGBA; const AReferenceWhite: TXYZReferenceWhite): TXYZA; overload;
function XYZAToAdobeRGBA(const AXYZA: TXYZA): TAdobeRGBA; overload;
function XYZAToAdobeRGBA(const AXYZA: TXYZA; const AReferenceWhite: TXYZReferenceWhite): TAdobeRGBA; overload;
function SpectrumRangeReflectToXYZA(reflectance,wavelen1,wavelen2,alpha: single): TXYZA;
procedure XYZToLMS(const X,Y,Z: Single; out L,M,S: single);
procedure LMSToXYZ(const L,M,S: Single; out X,Y,Z: single);
procedure ChromaticAdaptXYZ(var X,Y,Z: single; const AFrom, ATo: TXYZReferenceWhite); inline;
procedure ChromaticAdaptWordXYZ(var X,Y,Z: word; const AFrom, ATo: TXYZReferenceWhite); inline;

procedure SetReferenceWhite(AObserverAngle: integer; AIlluminant: string); overload;
procedure SetReferenceWhite(AReferenceWhite: TXYZReferenceWhite); overload;
function GetReferenceWhite: TXYZReferenceWhite; overload;
function GetReferenceWhiteIndirect: PXYZReferenceWhite; overload;
function GetReferenceWhite(AObserverAngle: integer; AIlluminant: string): TXYZReferenceWhite; overload;
function GetReferenceWhiteIndirect(AObserverAngle: integer; AIlluminant: string): PXYZReferenceWhite; overload;

procedure AddReferenceWhite(const AReferenceWhite: TXYZReferenceWhite); overload;
procedure AddReferenceWhite(AObserverAngle: integer; AIlluminant: string; AX, AY, AZ: single); overload;
function GetReferenceWhiteCount: integer;
function GetReferenceWhiteByIndex(AIndex: integer): TXYZReferenceWhite;

var
  ReferenceWhite2D50, ReferenceWhite2D65, ReferenceWhite2E: TXYZReferenceWhite;

{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION} 

{ TBridgedConversion }

procedure TBridgedConversion.Convert(ASource: pointer; ADest: Pointer;
  ACount: integer; ASourceStride: integer; ADestStride: integer;
  AReferenceWhite: PXYZReferenceWhite);
const
  bufSize = 512;
  bufCount = 512 div sizeof(TExpandedPixel);
var
  buf: array[0..bufSize-1] of byte;
  psrc, pdest: PByte;
begin
  if Assigned(ConvertToBridge) then
  begin
    psrc := PByte(ASource);
    pdest := PByte(ADest);
    while ACount > 0 do
    begin
      if ACount > bufCount then
      begin
        ConvertToBridge(psrc, @buf, bufCount, ASourceStride, sizeof(TExpandedPixel), AReferenceWhite);
        FinalConvert(@buf, pdest, bufCount, sizeof(TExpandedPixel), ADestStride, AReferenceWhite);
        inc(psrc, ASourceStride*bufCount);
        inc(pdest, ADestStride*bufCount);
        dec(ACount,bufCount);
      end
      else
      begin
        ConvertToBridge(psrc, @buf, ACount, ASourceStride, sizeof(TExpandedPixel), AReferenceWhite);
        FinalConvert(@buf, pdest, ACount, sizeof(TExpandedPixel), ADestStride, AReferenceWhite);
        break;
      end;
    end;
  end else
    FinalConvert(ASource,ADest,ACount,ASourceStride,ADestStride,AReferenceWhite);
end;

{ TCustomColorspace }

class function TCustomColorspace.IndexOfChannel(AName: string): integer;
var
  i: Integer;
begin
  for i := 0 to GetChannelCount-1 do
    if GetChannelName(i) = AName then exit(i);
  exit(-1);
end;

class procedure TCustomColorspace.Convert(const ASource;
  out ADest; ADestColorspace: TColorspaceAny;
  ACount: integer; AReferenceWhite: PXYZReferenceWhite);
var
  conv: TBridgedConversion;
begin
  if self = TCustomColorspace then
    raise exception.Create('Cannot convert from abstract colorspace');
  if self = ADestColorspace then
    move(ASource, {%H-}ADest, self.GetSize * ACount)
  else
  begin
    conv := ColorspaceCollection.GetBridgedConversion(self, ADestColorspace);
    conv.Convert(@ASource,@ADest,ACount,self.GetSize,ADestColorspace.GetSize,AReferenceWhite);
  end;
end;

class function TCustomColorspace.GetDirectConversion(ADestColorspace: TColorspaceAny): TColorspaceConvertArrayProc;
begin
  result := ColorspaceCollection.GetDirectConversion(self, ADestColorspace);
end;

class function TCustomColorspace.GetBridgedConversion(
  ADestColorspace: TColorspaceAny): TBridgedConversion;
begin
  result := ColorspaceCollection.GetBridgedConversion(self, ADestColorspace);
end;

{ ColorspaceCollection }

class function ColorspaceCollection.GetCount: integer;
begin
  result := FColorspaceCount;
end;

class function ColorspaceCollection.GetItem(AIndex: integer): TColorspaceAny;
begin
  if (AIndex < 0) or (AIndex >= FColorspaceCount) then
    raise ERangeError.Create('Index out of bounds');
  result := FColorspaces[AIndex];
end;

class function ColorspaceCollection.IndexOf(AColorspace: TColorspaceAny): integer;
var
  i: Integer;
begin
  for i := 0 to FColorspaceCount-1 do
    if FColorspaces[i] = AColorspace then exit(i);
  result := -1;
end;

class procedure ColorspaceCollection.Add(AColorspace: TColorspaceAny);
var
  i: Integer;
begin
  for i := 0 to high(FColorspaces) do
    if FColorspaces[i] = AColorspace then exit;

  if FColorspaceCount >= length(FColorspaces) then
    setlength(FColorspaces, FColorspaceCount*2+8);
  FColorspaces[FColorspaceCount] := AColorspace;
  inc(FColorspaceCount);
end;

class procedure ColorspaceCollection.AddConversion(ASource: TColorspaceAny;
  ADest: TColorspaceAny; AConversion: TColorspaceConvertArrayProc);
var
  idxSource, idxDest: Integer;
begin
  idxSource := IndexOf(ASource);
  if idxSource = -1 then raise exception.Create('Colorspace not registered');
  idxDest := IndexOf(ADest);
  if idxDest = -1 then raise exception.Create('Colorspace not registered');
  if idxSource >= length(FColorspaceConversions) then
    setlength(FColorspaceConversions, FColorspaceCount+4);
  if idxDest >= length(FColorspaceConversions[idxSource]) then
  setlength(FColorspaceConversions[idxSource], FColorspaceCount+4);
  FColorspaceConversions[idxSource][idxDest] := AConversion;
end;

procedure CopyColorsAny(ASource: pointer; ADest: Pointer; ACount: integer;
         {%H-}ASourceStride:integer; ADestStride:integer; {%H-}AReferenceWhite: PXYZReferenceWhite);
begin
  move(ASource^, ADest^, ADestStride * ACount);
end;

class function ColorspaceCollection.GetDirectConversion(ASource: TColorspaceAny;
  ADest: TColorspaceAny): TColorspaceConvertArrayProc;
var
  idxSource, idxDest: Integer;
begin
  if ASource = ADest then
    exit(@CopyColorsAny);

  idxSource := IndexOf(ASource);
  if idxSource = -1 then raise exception.Create('Colorspace not registered');
  idxDest := IndexOf(ADest);
  if idxDest = -1 then raise exception.Create('Colorspace not registered');

  if (idxSource < length(FColorspaceConversions)) and
     (idxDest < length(FColorspaceConversions[idxSource])) then
    result := FColorspaceConversions[idxSource][idxDest]
  else
    result := nil;
end;

class function ColorspaceCollection.GetBridgedConversion(
  ASource: TColorspaceAny; ADest: TColorspaceAny): TBridgedConversion;
begin
  result.FinalConvert:= GetDirectConversion(ASource,ADest);
  if result.FinalConvert<>nil then
  begin // direct conversion so no bridge needed
    result.ConvertToBridge:= nil;
  end else
  begin
    result.ConvertToBridge:= GetDirectConversion(ASource,TExpandedPixelColorspace);
    if result.ConvertToBridge=nil then raise exception.Create('Cannot convert '+ASource.GetName+' to bridge');
    result.FinalConvert:= GetDirectConversion(TExpandedPixelColorspace,ADest);
    if result.FinalConvert=nil then raise exception.Create('Cannot convert '+ADest.GetName+' from bridge');
  end;
end;

var
  CurrentReferenceWhite: TXYZReferenceWhite;
  ReferenceWhiteArray: array of TXYZReferenceWhite;

function Clamp(const V, Min, Max: single): single;
begin
  Result := V;
  if Result < Min then
    Result := Min
  else if Result > Max then
    Result := Max
  else Result := V;
end;

function ClampInt(V, Min, Max: integer): integer;
begin
  Result := V;
  if Result < Min then
    Result := Min
  else if Result > Max then
    Result := Max
  else Result := V;
end;

function PositiveModSingle(x, cycle: single): single;
begin
  if (x < 0) or (x >= cycle) then
    Result := x - cycle * floor(x / cycle)
  else
    result := x;
end;

procedure PrepareReferenceWhiteArray;
begin
  //Source:http://www.easyrgb.com/index.php?X=MATH&H=15#text15
  //domestic, tungsten-filament lighting
  AddReferenceWhite(2, 'A', 1.09850, 1.00, 0.35585);
  AddReferenceWhite(10, 'A', 1.11144, 1.00, 0.35200);
  //deprecated daylight
  AddReferenceWhite(2, 'C', 0.98074, 1.00, 1.18232);
  AddReferenceWhite(10, 'C', 0.97285, 1.00, 1.16145);
  //daylight
  AddReferenceWhite(2, 'D50', 0.96422, 1.00, 0.82521);
  AddReferenceWhite(10, 'D50', 0.96720, 1.00, 0.81427);
  AddReferenceWhite(2, 'D55', 0.95682, 1.00, 0.92149);
  AddReferenceWhite(10, 'D55', 0.95799, 1.00, 0.90926);
  AddReferenceWhite(2, 'D65', 0.95047, 1.00, 1.08883);
  AddReferenceWhite(10, 'D65', 0.94811, 1.00, 1.07304);
  AddReferenceWhite(2, 'D75', 0.94972, 1.00, 1.22638);
  AddReferenceWhite(10, 'D75', 0.94416, 1.00, 1.20641);
  //equal energy
  AddReferenceWhite(2, 'E', 1,1,1);
  AddReferenceWhite(10, 'E', 1,1,1);
  //fluorescent light
  AddReferenceWhite(2, 'F2', 0.99187, 1.00, 0.67395);
  AddReferenceWhite(10, 'F2', 1.03280, 1.00, 0.69026);
  AddReferenceWhite(2, 'F7', 0.95044, 1.00, 1.08755);
  AddReferenceWhite(10, 'F7', 0.95792, 1.00, 1.07687);
  AddReferenceWhite(2, 'F11', 1.00966, 1.00, 0.64370);
  AddReferenceWhite(10, 'F11', 1.03866, 1.00, 0.65627);
end;

procedure SetReferenceWhite(AObserverAngle: integer; AIlluminant: string);
var
  rp: TXYZReferenceWhite;
  i: integer;
begin
  for i := 0 to Length(ReferenceWhiteArray) - 1 do
  begin
    rp := ReferenceWhiteArray[i];
    if (rp.ObserverAngle = AObserverAngle) and (rp.Illuminant = AIlluminant) then
    begin
      CurrentReferenceWhite := rp;
      Break;
    end;
  end;
end;

procedure SetReferenceWhite(AReferenceWhite: TXYZReferenceWhite);
begin
  CurrentReferenceWhite := AReferenceWhite;
end;

function GetReferenceWhite: TXYZReferenceWhite;
begin
  Result := CurrentReferenceWhite;
end;

function GetReferenceWhiteIndirect: PXYZReferenceWhite;
begin
  result := @CurrentReferenceWhite;
end;

function GetReferenceWhite(AObserverAngle: integer; AIlluminant: string): TXYZReferenceWhite;
var
  p: PXYZReferenceWhite;
begin
  p := GetReferenceWhiteIndirect(AObserverAngle, AIlluminant);
  if p = nil then raise exception.Create('Reference white not found');
  result := p^;
end;

function GetReferenceWhiteIndirect(AObserverAngle: integer; AIlluminant: string): PXYZReferenceWhite;
var
  rp: PXYZReferenceWhite;
  i: integer;
begin
  for i := 0 to Length(ReferenceWhiteArray) - 1 do
  begin
    rp := @ReferenceWhiteArray[i];
    if (rp^.ObserverAngle = AObserverAngle) and (rp^.Illuminant = AIlluminant) then
    begin
      result := rp;
      exit;
    end;
  end;
  result := nil;
end;

procedure AddReferenceWhite(const AReferenceWhite: TXYZReferenceWhite);
begin
  if GetReferenceWhiteIndirect(AReferenceWhite.ObserverAngle, AReferenceWhite.Illuminant)<>nil then
    raise exception.Create('Reference white already defined');
  SetLength(ReferenceWhiteArray, Length(ReferenceWhiteArray) + 1);
  ReferenceWhiteArray[Length(ReferenceWhiteArray) - 1] := AReferenceWhite;
  with ReferenceWhiteArray[Length(ReferenceWhiteArray) - 1] do
    XYZToLMS(X,Y,Z, L,M,S);
end;

procedure AddReferenceWhite(AObserverAngle: integer; AIlluminant: string; AX, AY, AZ: single);
var
  rp: TXYZReferenceWhite;
begin
  rp.Illuminant := AIlluminant;
  rp.ObserverAngle := AObserverAngle;
  rp.X := AX;
  rp.Y := AY;
  rp.Z := AZ;
  AddReferenceWhite(rp);
end;

function GetReferenceWhiteCount: integer;
begin
  result := length(ReferenceWhiteArray);
end;

function GetReferenceWhiteByIndex(AIndex: integer): TXYZReferenceWhite;
begin
  if (AIndex < 0) or (AIndex >= length(ReferenceWhiteArray)) then
    raise ERangeError.Create('Index out of bounds');
  result := ReferenceWhiteArray[AIndex];
end;

{$DEFINE INCLUDE_IMPLEMENTATION}
{$I generatedcolorspace.inc}

function BGRAToMask(const ABGRAPixel: TBGRAPixel): TByteMask;
var
  ec: TExpandedPixel;
begin
  ec    := GammaExpansion(ABGRAPixel);
  result.gray:= (ec.red * redWeightShl10 + ec.green * greenWeightShl10 +
    ec.blue * blueWeightShl10 + (1 shl 9)) shr 18;
end;

function ExpandedPixelToByteMask(const AExpandedPixel: TExpandedPixel): TByteMask;
begin
  result.gray:= (AExpandedPixel.red * redWeightShl10 + AExpandedPixel.green * greenWeightShl10 +
    AExpandedPixel.blue * blueWeightShl10 + (1 shl 9)) shr 18;
end;

function MaskToBGRA(const AMask: TByteMask; AAlpha: byte): TBGRAPixel;
begin
  result.red := GammaCompressionTab[AMask.gray+(AMask.gray shl 8)];
  result.green := result.red;
  result.blue := result.red;
  result.alpha := AAlpha;
end;

function ByteMaskToExpandedPixel(const AMask: TByteMask; AAlpha: byte = 255): TExpandedPixel;
begin
  result.red := AMask.gray+(AMask.gray shl 8);
  result.green := result.red;
  result.blue := result.red;
  result.alpha := AAlpha;
end;

function StdRGBAToBGRAPixel(const AStdRGBA: TStdRGBA): TBGRAPixel;
begin
  with AStdRGBA do
  begin
    result.red := ClampInt(round(red * 255), 0, 255);
    result.green := ClampInt(round(green * 255), 0, 255);
    result.blue := ClampInt(round(blue * 255), 0, 255);
    result.alpha := ClampInt(round(alpha * 255), 0, 255);
  end;
end;

function BGRAPixelToStdRGBA(const ABGRAPixel: TBGRAPixel): TStdRGBA;
const oneOver255 = 1/255;
begin
  with ABGRAPixel do
  begin
    result.red := red * oneOver255;
    result.green := green * oneOver255;
    result.blue := blue * oneOver255;
    result.alpha := alpha * oneOver255;
  end;
end;

function LinearRGBAToExpandedPixel(const ALinearRGBA: TLinearRGBA): TExpandedPixel;
begin
  with ALinearRGBA do
  begin
    result.red := ClampInt(round(red * 65535), 0, 65535);
    result.green := ClampInt(round(green * 65535), 0, 65535);
    result.blue := ClampInt(round(blue * 65535), 0, 65535);
    result.alpha := ClampInt(round(alpha * 65535), 0, 65535);
  end;
end;

function ExpandedPixelToLinearRGBA(const AExpandedPixel: TExpandedPixel): TLinearRGBA;
begin
  with AExpandedPixel do
  begin
    result.red := red / 65535;
    result.green := green / 65535;
    result.blue := blue / 65535;
    result.alpha := alpha / 65535;
  end;
end;

function GammaCompressionWF(AValue: Word): single;
const
  oneOver255 = 1/255;
var
  compByte: Byte;
  reExp, reExp2: Word;
begin
  if AValue=0 then exit(0) else
  if AValue=65535 then exit(1) else
  begin
    compByte := GammaCompressionTab[AValue];
    reExp := GammaExpansionTab[compByte];
    if reExp = AValue then
      result := compByte * oneOver255
    else
    if reExp < AValue then
    begin
      reExp2 := GammaExpansionTabHalf[compByte];
      if reExp2<>reExp then
        result := (compByte + (AValue-reExp)/(reExp2-reExp)*0.5)*oneOver255;
    end else
    begin
      reExp2 := GammaExpansionTabHalf[compByte-1];
      if reExp2<>reExp then
        result := (compByte - (reExp-AValue)/(reExp-reExp2)*0.5)*oneOver255;
    end;
  end;
end;

function ExpandedPixelToStdRGBA(const AExpandedPixel: TExpandedPixel): TStdRGBA;
begin
  result.red := GammaCompressionWF(AExpandedPixel.red);
  result.green := GammaCompressionWF(AExpandedPixel.green);
  result.blue := GammaCompressionWF(AExpandedPixel.blue);
  result.alpha := AExpandedPixel.alpha/65535;
end;

function GammaExpansionFW(AValue: single): word;
const
  fracShift = 10;
  intRange = 255 shl fracShift;
  fracAnd = (1 shl fracShift)-1;
  fracHalf = 1 shl (fracShift-1);
  fracQuarter = 1 shl (fracShift-2);
var
  valInt, byteVal, fracPart: integer;
  half: Word;
begin
  if AValue <= 0 then exit(0)
  else if AValue >= 1 then exit(65535);

  valInt := round(AValue*intRange);
  byteVal := valInt shr fracShift;
  fracPart := valInt and fracAnd;
  if fracPart >= fracHalf then
  begin
    result := GammaExpansionTab[byteVal+1];
    half := GammaExpansionTabHalf[byteVal];
    dec(result, ((result-half)*((1 shl fracShift)-fracPart)+fracQuarter) shr (fracShift-1));
  end
  else
  begin
    result := GammaExpansionTab[byteVal];
    if fracPart > 0 then
    begin
      half := GammaExpansionTabHalf[byteVal];
      inc(result, ((half-result)*fracPart+fracQuarter) shr (fracShift-1));
    end;
  end;
end;

function StdRGBAToExpandedPixel(const AStdRGBA: TStdRGBA): TExpandedPixel;
begin
  result.red := GammaExpansionFW(AStdRGBA.red);
  result.green := GammaExpansionFW(AStdRGBA.green);
  result.blue := GammaExpansionFW(AStdRGBA.blue);
  result.alpha:= round(AStdRGBA.alpha*65535);
end;

function LinearRGBAToXYZA(const ALinearRGBA: TLinearRGBA): TXYZA;
begin
  result := LinearRGBAToXYZA(ALinearRGBA, GetReferenceWhiteIndirect^);
end;

function LinearRGBAToXYZA(const ALinearRGBA: TLinearRGBA;
  const AReferenceWhite: TXYZReferenceWhite): TXYZA;
begin
  if AReferenceWhite.Illuminant = 'D50' then
  begin
    with ALinearRGBA do
    begin
      result.X := red * 0.4360746 + green * 0.3850649 + blue * 0.1430804;
      result.Y := red * 0.2225045 + green * 0.7168786 + blue * 0.0606169;
      result.Z := red * 0.0139322 + green * 0.0971045 + blue * 0.7141733;
    end;
    result.ChromaticAdapt(ReferenceWhite2D50, AReferenceWhite);
  end else
  begin
    with ALinearRGBA do
    begin
      result.X := red * 0.4124564 + green * 0.3575761 + blue * 0.1804375;
      result.Y := red * 0.2126729 + green * 0.7151522 + blue * 0.0721750;
      result.Z := red * 0.0193339 + green * 0.1191920 + blue * 0.9503041;
    end;
    result.ChromaticAdapt(ReferenceWhite2D65, AReferenceWhite);
  end;
  Result.alpha := ALinearRGBA.alpha;
end;

function XYZAToLinearRGBA(const AXYZA: TXYZA): TLinearRGBA;
begin
  result := XYZAToLinearRGBA(AXYZA, GetReferenceWhiteIndirect^);
end;

function XYZAToLinearRGBA(const AXYZA: TXYZA;
  const AReferenceWhite: TXYZReferenceWhite): TLinearRGBA;
var
  minVal, lightVal, maxVal: single;
  ad: TXYZA;
begin
  ad := AXYZA;
  if AReferenceWhite.Illuminant = 'D50' then
  begin
    ad.ChromaticAdapt(AReferenceWhite, ReferenceWhite2D50);
    with ad do
    begin
      result.red := X * 3.1338561 + Y * (-1.6168667) + Z * (-0.4906146);
      result.green := X * (-0.9787684) + Y * 1.9161415 + Z * 0.0334540;
      result.blue := X * 0.0719453 + Y * (-0.2289914) + Z * 1.4052427;
    end;
  end else
  begin
    ad.ChromaticAdapt(AReferenceWhite, ReferenceWhite2D65);
    with ad do
    begin
      result.red := X * 3.2404542 + Y * (-1.5371385) + Z * (-0.4985314);
      result.green := X * (-0.9692660) + Y * 1.8760108 + Z * 0.0415560;
      result.blue := X * 0.0556434 + Y * (-0.2040259) + Z * 1.0572252;
    end;
  end;
  if ( (XYZToRGBOverflowMin = xroClipToTarget) and ((result.red < 0) or
       (result.green < 0) or (result.blue < 0)) ) or
     ( (XYZToRGBOverflowMax = xroClipToTarget) and ((result.red > 1) or
       (result.green > 1) or (result.blue > 1)) ) then
  begin
    result.red := 0;
    result.green := 0;
    result.blue := 0;
    result.alpha := 0;
    exit;
  end;
  case XYZToRGBOverflowMin of
    xroPreserveHue: begin
        minVal := min(min(result.red,result.green),result.blue);
        if minVal<0 then
        begin
          lightVal := result.red*0.299+result.green*0.587+result.blue*0.114;
          if lightVal <= 0 then
          begin
            result.red := 0;
            result.green := 0;
            result.blue := 0;
          end else
          begin
            result.red := (result.red-minVal)*lightVal/(lightVal-minVal);
            result.green := (result.green-minVal)*lightVal/(lightVal-minVal);
            result.blue := (result.blue-minVal)*lightVal/(lightVal-minVal);
          end;
        end;
      end;
  end;
  case XYZToRGBOverflowMax of
    xroPreserveHue:
      begin
        maxVal := max(max(result.red,result.green),result.blue);
        if maxVal > 1 then
        begin
          result.red := result.red/maxVal;
          result.green := result.green/maxVal;
          result.blue := result.blue/maxVal;
        end;
      end;
    xroSaturateEachChannel:
      begin
        if result.red > 1 then result.red := 1;
        if result.green > 1 then result.green := 1;
        if result.blue > 1 then result.blue := 1;
      end;
  end;
  if XYZToRGBOverflowMin = xroSaturateEachChannel then
  begin
    if result.red < 0 then result.red := 0;
    if result.green < 0 then result.green := 0;
    if result.blue < 0 then result.blue := 0;
  end;
  Result.alpha := AXYZA.alpha;
end;

function ExpandedPixelToWordXYZA(const AExpandedPixel: TExpandedPixel): TWordXYZA; overload;
begin
  result := ExpandedPixelToWordXYZA(AExpandedPixel, GetReferenceWhiteIndirect^);
end;

function ExpandedPixelToWordXYZA(const AExpandedPixel: TExpandedPixel; const AReferenceWhite: TXYZReferenceWhite): TWordXYZA; overload;
begin
  if AReferenceWhite.Illuminant = 'D50' then
  begin
    with AExpandedPixel do
    begin
      result.X := ClampInt(round((red * 0.4360746 + green * 0.3850649 + blue * 0.1430804)*(50000/65535)),0,65535);
      result.Y := ClampInt(round((red * 0.2225045 + green * 0.7168786 + blue * 0.0606169)*(50000/65535)),0,65535);
      result.Z := ClampInt(round((red * 0.0139322 + green * 0.0971045 + blue * 0.7141733)*(50000/65535)),0,65535);
    end;
    result.ChromaticAdapt(ReferenceWhite2D50, AReferenceWhite);
  end else
  begin
    with AExpandedPixel do
    begin
      result.X := ClampInt(round((red * 0.4124564 + green * 0.3575761 + blue * 0.1804375)*(50000/65535)),0,65535);
      result.Y := ClampInt(round((red * 0.2126729 + green * 0.7151522 + blue * 0.0721750)*(50000/65535)),0,65535);
      result.Z := ClampInt(round((red * 0.0193339 + green * 0.1191920 + blue * 0.9503041)*(50000/65535)),0,65535);
    end;
    result.ChromaticAdapt(ReferenceWhite2D65, AReferenceWhite);
  end;
  Result.alpha := AExpandedPixel.alpha;
end;

function WordXYZAToExpandedPixel(const AXYZA: TWordXYZA): TExpandedPixel; overload;
begin
  result := WordXYZAToExpandedPixel(AXYZA, GetReferenceWhiteIndirect^);
end;

function WordXYZAToExpandedPixel(const AXYZA: TWordXYZA; const AReferenceWhite: TXYZReferenceWhite): TExpandedPixel; overload;
var
  minVal, lightVal, maxVal,
  r,g,b, valRangeDiv2: Int32or64;
  ad: TWordXYZA;
begin
  ad := AXYZA;
  if AReferenceWhite.Illuminant = 'D50' then
  begin
    ad.ChromaticAdapt(AReferenceWhite, ReferenceWhite2D50);
    with ad do
    begin
      r := round((X * 3.1338561 + Y * (-1.6168667) + Z * (-0.4906146))*(65535/50000));
      g := round((X * (-0.9787684) + Y * 1.9161415 + Z * 0.0334540)*(65535/50000));
      b := round((X * 0.0719453 + Y * (-0.2289914) + Z * 1.4052427)*(65535/50000));
    end;
  end else
  begin
    ad.ChromaticAdapt(AReferenceWhite, ReferenceWhite2D65);
    with ad do
    begin
      r := round((X * 3.2404542 + Y * (-1.5371385) + Z * (-0.4985314))*(65535/50000));
      g := round((X * (-0.9692660) + Y * 1.8760108 + Z * 0.0415560)*(65535/50000));
      b := round((X * 0.0556434 + Y * (-0.2040259) + Z * 1.0572252)*(65535/50000));
    end;
  end;
  if ( (XYZToRGBOverflowMin = xroClipToTarget) and ((r < 0) or
       (g < 0) or (b < 0)) ) or
     ( (XYZToRGBOverflowMax = xroClipToTarget) and ((r > 65535) or
       (g > 65535) or (b > 65535)) ) then
  begin
    result.red := 0;
    result.green := 0;
    result.blue := 0;
    result.alpha := 0;
    exit;
  end;
  case XYZToRGBOverflowMin of
    xroPreserveHue: begin
        minVal := min(min(r,g),b);
        if minVal<0 then
        begin
          lightVal := r*redWeightShl10 + g*greenWeightShl10
                    + b*blueWeightShl10;
          if lightVal <= 0 then
          begin
            result.red := 0;
            result.green := 0;
            result.blue := 0;
            Result.alpha := AXYZA.alpha;
            exit;
          end else
          begin
            lightVal := (lightVal+512) shr 10;
            valRangeDiv2 := (lightVal-minVal) shr 1;
            r := (int64(r-minVal)*lightVal+valRangeDiv2) div (lightVal-minVal);
            g := (int64(g-minVal)*lightVal+valRangeDiv2) div (lightVal-minVal);
            b := (int64(b-minVal)*lightVal+valRangeDiv2) div (lightVal-minVal);
          end;
        end;
      end;
  end;
  case XYZToRGBOverflowMax of
    xroPreserveHue:
      begin
        maxVal := max(max(r,g),b);
        if maxVal > 65535 then
        begin
          r := (int64(r)*65535+(maxVal shr 1)) div maxVal;
          g := (int64(g)*65535+(maxVal shr 1)) div maxVal;
          b := (int64(b)*65535+(maxVal shr 1)) div maxVal;
        end;
      end;
    xroSaturateEachChannel:
      begin
        if r > 65535 then r := 65535;
        if g > 65535 then g := 65535;
        if b > 65535 then b := 65535;
      end;
  end;
  if XYZToRGBOverflowMin = xroSaturateEachChannel then
  begin
    if r < 0 then r := 0;
    if g < 0 then g := 0;
    if b < 0 then b := 0;
  end;
  result.red := r;
  result.green := g;
  result.blue := b;
  Result.alpha := AXYZA.alpha;
end;

function XYZAToWordXYZA(const AXYZA: TXYZA): TWordXYZA;
begin
  result.X := ClampInt(round(AXYZA.X*50000),0,65535);
  result.Y := ClampInt(round(AXYZA.Y*50000),0,65535);
  result.Z := ClampInt(round(AXYZA.Z*50000),0,65535);
  result.alpha := round(AXYZA.alpha*65535);
end;

function WordXYZAToXYZA(const AWordXYZA: TWordXYZA): TXYZA;
const oneOver50000 = 1/50000;
begin
  result.X := AWordXYZA.X*oneOver50000;
  result.Y := AWordXYZA.Y*oneOver50000;
  result.Z := AWordXYZA.Z*oneOver50000;
  result.alpha:= AWordXYZA.alpha*(1/65535);
end;

function XYZAToLabA(const AXYZA: TXYZA): TLabA;
begin
  Result := XYZAToLabA(AXYZA, GetReferenceWhiteIndirect^);
end;

function XYZAToLabA(const AXYZA: TXYZA; const AReferenceWhite: TXYZReferenceWhite): TLabA;
var
  xp, yp, zp: double;
begin
  xp := AXYZA.X / AReferenceWhite.X;
  yp := AXYZA.Y / AReferenceWhite.Y;
  zp := AXYZA.Z / AReferenceWhite.Z;
  if xp > 0.008856 then
    xp := Power(xp, 1 / 3)
  else
    xp := (7.787 * xp) + 0.138;
  if yp > 0.008856 then
    yp := Power(yp, 1 / 3)
  else
    yp := (7.787 * yp) + 0.138;
  if zp > 0.008856 then
    zp := Power(zp, 1 / 3)
  else
    zp := (7.787 * zp) + 0.138;

  result.L := Clamp((116 * yp) - 16, 0, 100);
  result.a := 500 * (xp - yp);
  result.b := 200 * (yp - zp);
  Result.Alpha := AXYZA.alpha;
end;

function LabAToXYZA(const ALabA: TLabA): TXYZA;
begin
  Result := LabAToXYZA(ALabA, GetReferenceWhiteIndirect^);
end;

function LabAToXYZA(const ALabA: TLabA; const AReferenceWhite: TXYZReferenceWhite): TXYZA;
var
  xp, yp, zp: double;
begin
  yp := (ALabA.L + 16) / 116;
  xp := ALabA.a / 500 + yp;
  zp := yp - ALabA.b / 200;
  if yp > 0.2069 then
    yp := IntPower(yp, 3)
  else
    yp := (yp - 0.138) / 7.787;
  if xp > 0.2069 then
    xp := IntPower(xp, 3)
  else
    xp := (xp - 0.138) / 7.787;
  if zp > 0.2069 then
    zp := IntPower(zp, 3)
  else
    zp := (zp - 0.138) / 7.787;
  Result.X := AReferenceWhite.X * xp;
  Result.Y := AReferenceWhite.Y * yp;
  Result.Z := AReferenceWhite.Z * zp;
  Result.alpha := ALabA.Alpha;
end;

function StdRGBAToStdHSVA(const AStdRGBA: TStdRGBA): TStdHSVA;
var
  Delta, mini: single;
begin
  with AStdRGBA do
  begin
    result.value := max(max(red, green), blue);
    mini := min(min(red, green), blue);
    Delta := result.value - mini;

    if result.value = 0.0 then
      result.saturation := 0
    else
      result.saturation := Delta / result.value;

    if result.saturation = 0.0 then
      result.hue := 0
    else
    begin
      if red = result.value then
        result.hue := 60.0 * (green - blue) / Delta
      else
      if green = result.value then
        result.hue := 120.0 + 60.0 * (blue - red) / Delta
      else
      {if blue = result.value then}
        result.hue := 240.0 + 60.0 * (red - green) / Delta;

      if result.hue < 0.0 then
        IncF(result.hue, 360.0);
    end;
    result.alpha := alpha;
  end;
end;

function StdHSVAToStdRGBA(const AStdHSVA: TStdHSVA): TStdRGBA;
var
  C, X, M, rp, gp, bp, sp, vp: single;
  h360: single;
begin
  vp := AStdHSVA.value;
  sp := AStdHSVA.saturation;
  C := Vp * sp;
  h360 := PositiveModSingle(AStdHSVA.hue, 360);
  X := C * (1 - abs(PositiveModSingle(h360 / 60, 2) - 1));
  m := vp - c;
  rp := 0;
  gp := 0;
  bp := 0;
  case floor(h360) of
    -1..59:
    begin
      rp := C;
      gp := X;
      bp := 0;
    end;
    60..119:
    begin
      rp := X;
      gp := C;
      bp := 0;
    end;
    120..179:
    begin
      rp := 0;
      gp := C;
      bp := X;
    end;
    180..239:
    begin
      rp := 0;
      gp := X;
      bp := C;
    end;
    240..299:
    begin
      rp := X;
      gp := 0;
      bp := C;
    end;
    300..359:
    begin
      rp := C;
      gp := 0;
      bp := X;
    end;
  end;
  result.red := rp + m;
  result.green := gp + m;
  result.blue := bp + m;
  result.alpha := AStdHSVA.alpha;
end;

function StdHSLAToStdHSVA(const AStdHSLA: TStdHSLA): TStdHSVA;
var
  s, l, v: single;
begin
  Result.hue := AStdHSLA.hue;
  s := AStdHSLA.saturation;
  l := AStdHSLA.lightness;
  v := (2 * l + s * (1 - abs(2 * l - 1))) / 2;
  if v <> 0 then
    Result.saturation := 2 * (v - l) / v
  else
    Result.saturation := 0;
  Result.value := v;
end;

function StdHSVAToStdHSLA(const AStdHSVA: TStdHSVA): TStdHSLA;
var
  s, v, l: single;
begin
  Result.hue := AStdHSVA.hue;
  s := AStdHSVA.saturation;
  v := AStdHSVA.value;
  l := 0.5 * v * (2 - s);
  if l <> 0 then
    Result.saturation := v * s / (1 - abs(2 * l - 1))
  else
    Result.saturation := 0;
  Result.lightness := l;
end;

function StdRGBAToStdCMYK(const AStdRGBA: TStdRGBA): TStdCMYK;
begin
  with AStdRGBA do
  begin
    result.K := 1 - max(max(red, green), blue);
    if result.K >= 1 then
    begin
      result.C := 0;
      result.M := 0;
      result.Y := 0;
    end
    else
    begin
      result.C := 1 - red / (1 - result.K);
      result.M := 1 - green / (1 - result.K);
      result.Y := 1 - blue / (1 - result.K);
    end;
  end;
end;

function StdCMYKToStdRGBA(const AStdCMYK: TStdCMYK; AAlpha: Single = 1): TStdRGBA;
begin
  with AStdCMYK do
  begin
    result.red := (1 - C) * (1 - K);
    result.green := (1 - M) * (1 - K);
    result.blue := (1 - Y) * (1 - K);
    result.alpha := AAlpha;
  end;
end;

function LabAToLChA(const ALabA: TLabA): TLChA;
var
  a, b, HRad: single;
begin
  a := ALabA.a;
  b := ALabA.b;
  HRad := ArcTan2(b, a);
  if HRad >= 0 then
    result.H := (HRad / PI) * 180
  else
    result.H := 360 - (ABS(HRad) / PI) * 180;
  result.L := ALabA.L;
  result.C := SQRT(a*a + b*b);
  result.alpha := ALabA.Alpha;
end;

function LChAToLabA(const ALChA: TLChA): TLabA;
begin
  result.L := ALChA.L;
  result.a := cos(DegToRad(ALChA.h)) * ALChA.C;
  result.b := sin(DegToRad(ALChA.h)) * ALChA.C;
  result.Alpha:= ALChA.alpha;
end;

function AdobeRGBAToXYZA(const ASource: TAdobeRGBA; const AReferenceWhite: TXYZReferenceWhite): TXYZA;
var R,G,B: single;
begin
  R := GammaExpansionTab[ASource.red]/65535;
  G := GammaExpansionTab[ASource.green]/65535;
  B := GammaExpansionTab[ASource.blue]/65535;
  if AReferenceWhite.Illuminant = 'D50' then
  begin
    result.X := R*0.6097559 + G*0.2052401 + B*0.1492240;
    result.Y := R*0.3111242 + G*0.6256560 + B*0.0632197;
    result.Z := R*0.0194811 + G*0.0608902 + B*0.7448387;
    result.ChromaticAdapt(ReferenceWhite2D50, AReferenceWhite);
  end else
  begin
    result.X := R*0.5767309 + G*0.1855540 + B*0.1881852;
    result.Y := R*0.2973769 + G*0.6273491 + B*0.0752741;
    result.Z := R*0.0270343 + G*0.0706872 + B*0.9911085;
    result.ChromaticAdapt(ReferenceWhite2D65, AReferenceWhite);
  end;
  result.alpha := ASource.alpha/255;
end;

function AdobeRGBAToXYZA(const ASource: TAdobeRGBA): TXYZA;
begin
  result := AdobeRGBAToXYZA(ASource, GetReferenceWhiteIndirect^);
end;

function XYZAToAdobeRGBA(const AXYZA: TXYZA; const AReferenceWhite: TXYZReferenceWhite): TAdobeRGBA;
var R,G,B: single;
  ad: TXYZA;
begin
  ad := AXYZA;
  if AReferenceWhite.Illuminant = 'D50' then
  begin
    ad.ChromaticAdapt(AReferenceWhite, ReferenceWhite2D50);
    with ad do
    begin
      R := Clamp(1.9624274*X - 0.6105343*Y - 0.3413404*Z,0,1);
      G := Clamp(-0.9787684*X + 1.9161415*Y + 0.0334540*Z,0,1);
      B := Clamp(0.0286869*X - 0.1406752*Y + 1.3487655*Z,0,1);
    end;
  end else
  begin
    ad.ChromaticAdapt(AReferenceWhite, ReferenceWhite2D65);
    with ad do
    begin
      R := Clamp(2.0413690*X - 0.5649464*Y - 0.3446944*Z,0,1);
      G := Clamp(-0.9692660*X + 1.8760108*Y + 0.0415560*Z,0,1);
      B := Clamp(0.0134474*X - 0.1183897*Y + 1.0154096*Z,0,1);
    end;
  end;
  result.red := GammaCompressionTab[round(R*65535)];
  result.green := GammaCompressionTab[round(G*65535)];
  result.blue := GammaCompressionTab[round(B*65535)];
  result.alpha := ClampInt(round(AXYZA.alpha*255),0,255);
end;

function XYZAToAdobeRGBA(const AXYZA: TXYZA): TAdobeRGBA;
begin
  result := XYZAToAdobeRGBA(AXYZA, GetReferenceWhiteIndirect^);
end;

function StdRGBAToLinearRGBA(const AStdRGBA: TStdRGBA): TLinearRGBA;
var
  ec: TExpandedPixel;
begin
  ec := StdRGBAToExpandedPixel(AStdRGBA);
  result := ExpandedPixelToLinearRGBA(ec);
  result.alpha := AStdRGBA.alpha;
end;

function LinearRGBAToStdRGBA(const ALinearRGBA: TLinearRGBA): TStdRGBA;
var
  ec: TExpandedPixel;
begin
  ec := LinearRGBAToExpandedPixel(ALinearRGBA);
  result := ExpandedPixelToStdRGBA(ec);
  result.alpha := ALinearRGBA.alpha;
end;

function StdRGBAToStdHSLA(const AStdRGBA: TStdRGBA): TStdHSLA;
var
  d, cmax, cmin: double;
begin
  with AStdRGBA do
  begin
    cmax := Max(red, Max(green, blue));
    cmin := Min(red, Min(green, blue));
    result.lightness := (cmax + cmin) / 2;

    if cmax = cmin then
    begin
      result.hue := 0;
      result.saturation := 0;
    end
    else
    begin
      d := cmax - cmin;
      if result.lightness < 0.5 then
        result.saturation := d / (cmax + cmin)
      else
        result.saturation := d / (2 - cmax - cmin);

      if red = cmax then
        result.hue := (green - blue) / d
      else
      if green = cmax then
        result.hue := 2 + (blue - red) / d
      else
        result.hue := 4 + (red - green) / d;
      if result.hue < 0 then IncF(result.hue, 6);
      result.hue := result.hue * 60;
    end;
    result.alpha := alpha;
  end;
end;

function StdHSLAToStdRGBA(const AStdHSLA: TStdHSLA): TStdRGBA;
var
  C, X, M, rp, gp, bp, sp, lp, h360: single;
begin
  lp := AStdHSLA.lightness;
  sp := AStdHSLA.saturation;
  C := (1 - abs(2 * Lp - 1)) * Sp;
  h360 := PositiveModSingle(AStdHSLA.hue, 360);
  X := C * (1 - abs(PositiveModSingle(h360 / 60, 2) - 1));
  m := Lp - C / 2;
  rp := 0;
  gp := 0;
  bp := 0;
  case floor(h360) of
    -1..59:
    begin
      rp := C;
      gp := X;
      bp := 0;
    end;
    60..119:
    begin
      rp := X;
      gp := C;
      bp := 0;
    end;
    120..179:
    begin
      rp := 0;
      gp := C;
      bp := X;
    end;
    180..239:
    begin
      rp := 0;
      gp := X;
      bp := C;
    end;
    240..299:
    begin
      rp := X;
      gp := 0;
      bp := C;
    end;
    300..359:
    begin
      rp := C;
      gp := 0;
      bp := X;
    end;
  end;
  result.red := rp + m;
  result.green := gp + m;
  result.blue := bp + m;
  result.alpha := AStdHSLA.alpha;
end;

function SpectrumRangeReflectToXYZA(reflectance,wavelen1,wavelen2,alpha: single): TXYZA;
var isEqualEnergy: boolean;
  fromRefWhite: PXYZReferenceWhite;

  function GetIlluminantSpectrum(AIndex: integer): single;
  begin
    if isEqualEnergy then result := 1 else
      result := IlluminantSpectrumD65[AIndex].Y;
  end;

  procedure IncludeWavelength(fromWavelen, toWavelen: single);
  var i: integer;
    factor, ill: single;
  begin
    for i := 0 to high(SpectralLocus) do
      if (SpectralLocus[i].W+2.5 >= fromWavelen) and
         (SpectralLocus[i].W-2.5 < toWavelen) then
      begin
        factor := 1;
        if SpectralLocus[i].W-2.5 < fromWavelen then
          DecF(factor, (fromWavelen - (SpectralLocus[i].W-2.5))/5);
        if SpectralLocus[i].W+2.5 > toWavelen then
          DecF(factor, ((SpectralLocus[i].W+2.5) - toWavelen)/5);
        if factor > 0 then
        begin
          ill := GetIlluminantSpectrum(i);
          IncF(result.X, SpectralLocus[i].X*factor*ill);
          IncF(result.Y, SpectralLocus[i].Y*factor*ill);
          IncF(result.Z, SpectralLocus[i].Z*factor*ill);
        end;
      end;
  end;

var
  minWavelen, maxWavelen, ill: single;
  totalXYZ: TXYZA;
  i: Integer;
begin
  result.X := 0;
  result.Y := 0;
  result.Z := 0;
  result.alpha:= alpha;

  with GetReferenceWhiteIndirect^ do
    isEqualEnergy := (X = 1) and (Y = 1) and (Z = 1);
  if isEqualEnergy then fromRefWhite := @ReferenceWhite2E
  else fromRefWhite := @ReferenceWhite2D65;

  totalXYZ := BGRABlack;
  for i := 0 to high(SpectralLocus) do
  begin
    ill := GetIlluminantSpectrum(i);
    IncF(totalXYZ.X, SpectralLocus[i].X*ill);
    IncF(totalXYZ.Y, SpectralLocus[i].Y*ill);
    IncF(totalXYZ.Z, SpectralLocus[i].Z*ill);
  end;

  minWavelen := SpectralLocus[0].W;
  maxWavelen := SpectralLocus[high(SpectralLocus)].W;

  if wavelen1 <= minWavelen then wavelen1 := minWavelen-2.5;
  if wavelen2 >= maxWavelen then wavelen2 := maxWavelen+2.5;

  if wavelen2 > wavelen1 then
    IncludeWavelength(wavelen1, wavelen2)
  else
  begin
    IncludeWavelength(wavelen1, maxWavelen+2.5);
    IncludeWavelength(minWavelen-2.5, wavelen2);
  end;

  result.X := result.X * fromRefWhite^.X/totalXYZ.X * reflectance;
  result.Y := result.Y * fromRefWhite^.Y/totalXYZ.Y * reflectance;
  result.Z := result.Z * fromRefWhite^.Z/totalXYZ.Z * reflectance;
  result.ChromaticAdapt(fromRefWhite^, GetReferenceWhiteIndirect^);
end;

procedure XYZToLMS(const X,Y,Z: Single; out L,M,S: single);
begin
  L := max(0.8951*X+0.2664*Y-0.1615*Z, 0);
  M := max(-0.7502*X+1.7135*Y+0.0367*Z, 0);
  S := max(0.0389*X-0.0685*Y+1.0296*Z, 0);
end;

procedure LMSToXYZ(const L,M,S: Single; out X,Y,Z: single);
begin
  X := 0.98699*L-0.14705*M+0.16006*S;
  Y := 0.43230*L+0.51836*M+0.04933*S;
  Z := -0.00853*L+0.04004*M+0.96849*S;
end;

procedure ChromaticAdaptXYZ(var X,Y,Z: Single; const AFrom, ATo: TXYZReferenceWhite);
var
  L, M, S: single;
begin
  if (AFrom.L=ATo.L) and (AFrom.M=ATo.M) and (AFrom.S=ATo.S) then exit;
  XYZToLMS(X,Y,Z, L,M,S);
  L := L * ATo.L/AFrom.L;
  M := M * ATo.M/AFrom.M;
  S := S * ATo.S/AFrom.S;
  LMSToXYZ(L,M,S, X,Y,Z);
end;

procedure ChromaticAdaptWordXYZ(var X,Y,Z: Word; const AFrom, ATo: TXYZReferenceWhite);
const oneOver50000 = 1/50000;
var Xf,Yf,Zf: Single;
begin
  Xf := X*oneOver50000;
  Yf := Y*oneOver50000;
  Zf := Z*oneOver50000;
  ChromaticAdaptXYZ(Xf,Yf,Zf, AFrom,ATo);
  X := min(round(Xf*50000),65535);
  Y := min(round(Yf*50000),65535);
  Z := min(round(Zf*50000),65535);
end;

{$ENDIF}

{$IFDEF INCLUDE_INITIALIZATION}
{$UNDEF INCLUDE_INITIALIZATION} 

  PrepareReferenceWhiteArray;
  ReferenceWhite2D50 := GetReferenceWhite(2, 'D50');
  ReferenceWhite2D65 := GetReferenceWhite(2, 'D65');
  ReferenceWhite2E := GetReferenceWhite(2, 'E');
  SetReferenceWhite(ReferenceWhite2D50);

  {$DEFINE INCLUDE_INITIALIZATION}
  {$I generatedcolorspace.inc}

{$ENDIF}
