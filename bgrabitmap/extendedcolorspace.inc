{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}

type
  TSpectralLocusPoint = record
    W,X,Y,Z: Single;
  end;

const //horseshoe shape of visible colors
  SpectralLocus: array[0..94] of TSpectralLocusPoint =
 ((W:360; X:0.0001299; Y:0.000003917; Z:0.0006061),
  (W:365; X:0.0002321; Y:0.000006965; Z:0.001086),
  (W:370; X:0.0004149; Y:0.00001239; Z:0.001946),
  (W:375; X:0.0007416; Y:0.00002202; Z:0.003486),
  (W:380; X:0.001368; Y:0.000039; Z:0.006450001),
  (W:385; X:0.002236; Y:0.000064; Z:0.01054999),
  (W:390; X:0.004243; Y:0.00012; Z:0.02005001),
  (W:395; X:0.00765; Y:0.000217; Z:0.03621),
  (W:400; X:0.01431; Y:0.000396; Z:0.06785001),
  (W:405; X:0.02319; Y:0.00064; Z:0.1102),
  (W:410; X:0.04351; Y:0.00121; Z:0.2074),
  (W:415; X:0.07763; Y:0.00218; Z:0.3713),
  (W:420; X:0.13438; Y:0.004; Z:0.6456),
  (W:425; X:0.21477; Y:0.0073; Z:1.0390501),
  (W:430; X:0.2839; Y:0.0116; Z:1.3856),
  (W:435; X:0.3285; Y:0.01684; Z:1.62296),
  (W:440; X:0.34828; Y:0.023; Z:1.74706),
  (W:445; X:0.34806; Y:0.0298; Z:1.7826),
  (W:450; X:0.3362; Y:0.038; Z:1.77211),
  (W:455; X:0.3187; Y:0.048; Z:1.7441),
  (W:460; X:0.2908; Y:0.06; Z:1.6692),
  (W:465; X:0.2511; Y:0.0739; Z:1.5281),
  (W:470; X:0.19536; Y:0.09098; Z:1.28764),
  (W:475; X:0.1421; Y:0.1126; Z:1.0419),
  (W:480; X:0.09564; Y:0.13902; Z:0.8129501),
  (W:485; X:0.05795001; Y:0.1693; Z:0.6162),
  (W:490; X:0.03201; Y:0.20802; Z:0.46518),
  (W:495; X:0.0147; Y:0.2586; Z:0.3533),
  (W:500; X:0.0049; Y:0.323; Z:0.272),
  (W:505; X:0.0024; Y:0.4073; Z:0.2123),
  (W:510; X:0.0093; Y:0.503; Z:0.1582),
  (W:515; X:0.0291; Y:0.6082; Z:0.1117),
  (W:520; X:0.06327; Y:0.71; Z:0.07824999),
  (W:525; X:0.1096; Y:0.7932; Z:0.05725001),
  (W:530; X:0.1655; Y:0.862; Z:0.04216),
  (W:535; X:0.2257499; Y:0.9148501; Z:0.02984),
  (W:540; X:0.2904; Y:0.954; Z:0.0203),
  (W:545; X:0.3597; Y:0.9803; Z:0.0134),
  (W:550; X:0.4334499; Y:0.9949501; Z:0.008749999),
  (W:555; X:0.5120501; Y:1; Z:0.005749999),
  (W:560; X:0.5945; Y:0.995; Z:0.0039),
  (W:565; X:0.6784; Y:0.9786; Z:0.002749999),
  (W:570; X:0.7621; Y:0.952; Z:0.0021),
  (W:575; X:0.8425; Y:0.9154; Z:0.0018),
  (W:580; X:0.9163; Y:0.87; Z:0.001650001),
  (W:585; X:0.9786; Y:0.8163; Z:0.0014),
  (W:590; X:1.0263; Y:0.757; Z:0.0011),
  (W:595; X:1.0567; Y:0.6949; Z:0.001),
  (W:600; X:1.0622; Y:0.631; Z:0.0008),
  (W:605; X:1.0456; Y:0.5668; Z:0.0006),
  (W:610; X:1.0026; Y:0.503; Z:0.00034),
  (W:615; X:0.9384; Y:0.4412; Z:0.00024),
  (W:620; X:0.8544499; Y:0.381; Z:0.00019),
  (W:625; X:0.7514; Y:0.321; Z:0.0001),
  (W:630; X:0.6424; Y:0.265; Z:0.00005),
  (W:635; X:0.5419; Y:0.217; Z:0.00003),
  (W:640; X:0.4479; Y:0.175; Z:0.00002),
  (W:645; X:0.3608; Y:0.1382; Z:0.00001),
  (W:650; X:0.2835; Y:0.107; Z:0),
  (W:655; X:0.2187; Y:0.0816; Z:0),
  (W:660; X:0.1649; Y:0.061; Z:0),
  (W:665; X:0.1212; Y:0.04458; Z:0),
  (W:670; X:0.0874; Y:0.032; Z:0),
  (W:675; X:0.0636; Y:0.0232; Z:0),
  (W:680; X:0.04677; Y:0.017; Z:0),
  (W:685; X:0.0329; Y:0.01192; Z:0),
  (W:690; X:0.0227; Y:0.00821; Z:0),
  (W:695; X:0.01584; Y:0.005723; Z:0),
  (W:700; X:0.01135916; Y:0.004102; Z:0),
  (W:705; X:0.008110916; Y:0.002929; Z:0),
  (W:710; X:0.005790346; Y:0.002091; Z:0),
  (W:715; X:0.004106457; Y:0.001484; Z:0),
  (W:720; X:0.002899327; Y:0.001047; Z:0),
  (W:725; X:0.00204919; Y:0.00074; Z:0),
  (W:730; X:0.001439971; Y:0.00052; Z:0),
  (W:735; X:0.0009999493; Y:0.0003611; Z:0),
  (W:740; X:0.0006900786; Y:0.0002492; Z:0),
  (W:745; X:0.0004760213; Y:0.0001719; Z:0),
  (W:750; X:0.0003323011; Y:0.00012; Z:0),
  (W:755; X:0.0002348261; Y:0.0000848; Z:0),
  (W:760; X:0.0001661505; Y:0.00006; Z:0),
  (W:765; X:0.000117413; Y:0.0000424; Z:0),
  (W:770; X:8.307527E-05; Y:0.00003; Z:0),
  (W:775; X:5.870652E-05; Y:0.0000212; Z:0),
  (W:780; X:4.150994E-05; Y:0.00001499; Z:0),
  (W:785; X:2.935326E-05; Y:0.0000106; Z:0),
  (W:790; X:2.067383E-05; Y:7.4657E-06; Z:0),
  (W:795; X:1.455977E-05; Y:5.2578E-06; Z:0),
  (W:800; X:0.000010254; Y:3.7029E-06; Z:0),
  (W:805; X:7.221456E-06; Y:2.6078E-06; Z:0),
  (W:810; X:5.085868E-06; Y:1.8366E-06; Z:0),
  (W:815; X:3.581652E-06; Y:1.2934E-06; Z:0),
  (W:820; X:2.522525E-06; Y:9.1093E-07; Z:0),
  (W:825; X:1.776509E-06; Y:6.4153E-07; Z:0),
  (W:830; X:1.251141E-06; Y:4.5181E-07; Z:0));

type
  PXYZReferenceWhite = ^TXYZReferenceWhite;
  TXYZReferenceWhite = packed record
    X, Y, Z: single;
    ObserverAngle: integer;
    Illuminant: string
  end;

  TCustomColorspace = class;
  TColorspaceAny = class of TCustomColorspace;

  TColorspaceConvertArrayProc = procedure(ASource: pointer; ADest: Pointer; ACount: integer;
                  ASourceStride:integer; ADestStride:integer; AReferenceWhite: PXYZReferenceWhite);

  { TCustomColorspace }

  TCustomColorspace = class
    class function GetChannelName(AIndex: integer): string; virtual; abstract;
    class function GetChannelCount: integer; virtual; abstract;
    class function IndexOfChannel(AName: string): integer;
    class function GetMaxValue(AIndex: integer): single; virtual; abstract;
    class function GetMinValue(AIndex: integer): single; virtual; abstract;
    class function GetName: string; virtual; abstract;
    class function GetSize: integer; virtual; abstract;
    class function GetChannel(AColor: Pointer; AIndex: integer): single; virtual; abstract;
    class procedure SetChannel(AColor: Pointer; AIndex: integer; AValue: single); virtual; abstract;
    class procedure Convert(const ASource; out ADest; ADestColorspace: TColorspaceAny;
                            ACount: integer = 1; AReferenceWhite: PXYZReferenceWhite = nil);
    class function HasReferenceWhite: boolean; virtual; abstract;
  end;

  ColorspaceCollection = class
  protected
    class var FColorspaces : array of TColorspaceAny;
    class var FColorspaceCount: integer;
    class var FColorspaceConversions: array of array of TColorspaceConvertArrayProc;
  public
    class function GetCount: integer;
    class function GetItem(AIndex: integer): TColorspaceAny;
    class function IndexOf(AColorspace: TColorspaceAny): integer;
    class procedure Add(AColorspace: TColorspaceAny);
    class procedure AddConversion(ASource: TColorspaceAny; ADest: TColorspaceAny; AConversion: TColorspaceConvertArrayProc);
    class function GetConversion(ASource: TColorspaceAny; ADest: TColorspaceAny): TColorspaceConvertArrayProc;
  end;

{$DEFINE INCLUDE_INTERFACE}
{$I generatedcolorspace.inc}

function StdRGBAToBGRAPixel(const AStdRGBA: TStdRGBA): TBGRAPixel;
function BGRAPixelToStdRGBA(const ABGRAPixel: TBGRAPixel): TStdRGBA;
function LinearRGBAToExpandedPixel(const ALinearRGBA: TLinearRGBA): TExpandedPixel;
function ExpandedPixelToLinearRGBA(const AExpandedPixel: TExpandedPixel): TLinearRGBA;
function LinearRGBAToXYZA(const ALinearRGBA: TLinearRGBA): TXYZA;
function XYZAToLinearRGBA(const AXYZA: TXYZA): TLinearRGBA;
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
function AdobeRGBAToXYZA(const ASource: TAdobeRGBA): TXYZA;
function XYZAToAdobeRGBA(const AXYZA: TXYZA): TAdobeRGBA;

procedure SetReferenceWhite(AObserverAngle: integer; AIlluminant: string); overload;
procedure SetReferenceWhite(AReferenceWhite: TXYZReferenceWhite); overload;
function GetReferenceWhite: TXYZReferenceWhite;

procedure AddReferenceWhite(AReferenceWhite: TXYZReferenceWhite); overload;
procedure AddReferenceWhite(AObserverAngle: integer; AIlluminant: string; AX, AY, AZ: single); overload;
function GetReferenceWhiteCount: integer;
function GetReferenceWhiteByIndex(AIndex: integer): TXYZReferenceWhite;

{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION} 

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
  convProc: TColorspaceConvertArrayProc;
begin
  if self = TCustomColorspace then
    raise exception.Create('Cannot convert from abstract colorspace')
  else
  if self = ADestColorspace then
    move(ASource, {%H-}ADest, self.GetSize * ACount)
  else
  begin
    convProc := ColorspaceCollection.GetConversion(self, ADestColorspace);
    if convProc = nil then
      raise exception.Create('Conversion procedure not found');
    convProc(@ASource, @ADest, ACount, self.GetSize, ADestColorspace.GetSize, AReferenceWhite);
  end;
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

class function ColorspaceCollection.GetConversion(ASource: TColorspaceAny;
  ADest: TColorspaceAny): TColorspaceConvertArrayProc;
var
  idxSource, idxDest: Integer;
begin
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

var
  CurrentReferenceWhite: TXYZReferenceWhite;
  ReferenceWhiteArray: array of TXYZReferenceWhite;

function Clamp(const V, Min, Max: single): single;
begin
  Result := V;
  if Result < Min then
    Result := Min;
  if Result > Max then
    Result := Max;
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

procedure AddReferenceWhite(AReferenceWhite: TXYZReferenceWhite);
begin
  SetLength(ReferenceWhiteArray, Length(ReferenceWhiteArray) + 1);
  ReferenceWhiteArray[Length(ReferenceWhiteArray) - 1] := AReferenceWhite;
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

function StdRGBAToBGRAPixel(const AStdRGBA: TStdRGBA): TBGRAPixel;
begin
  with AStdRGBA do
  begin
    result.red := round(Clamp(red * 255, 0, 255));
    result.green := round(Clamp(green * 255, 0, 255));
    result.blue := round(Clamp(blue * 255, 0, 255));
    result.alpha := round(Clamp(alpha * 255, 0, 255));
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
    result.red := round(Clamp(red * 65535, 0, 65535));
    result.green := round(Clamp(green * 65535, 0, 65535));
    result.blue := round(Clamp(blue * 65535, 0, 65535));
    result.alpha := round(Clamp(alpha * 65535, 0, 65535));
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

function LinearRGBAToXYZA(const ALinearRGBA: TLinearRGBA): TXYZA;
begin
  with ALinearRGBA do
  begin
    // Observer= 2°, Illuminant= D65
    result.X := red * 0.4124 + green * 0.3576 + blue * 0.1805;
    result.Y := red * 0.2126 + green * 0.7152 + blue * 0.0722;
    result.Z := red * 0.0193 + green * 0.1192 + blue * 0.9505;
  end;
  Result.alpha := ALinearRGBA.alpha;
end;

function XYZAToLinearRGBA(const AXYZA: TXYZA): TLinearRGBA;
begin
  with AXYZA do
  begin
    result.red := Clamp(X * 3.2406 + Y * (-1.5372) + Z * (-0.49), 0, 1);
    result.green := Clamp(X * (-0.969) + Y * 1.8758 + Z * 0.0415, 0, 1);
    result.blue := Clamp(X * 0.0557 + Y * (-0.2040) + Z * 1.0570, 0, 1);
  end;
  Result.alpha := AXYZA.alpha;
end;

function XYZAToLabA(const AXYZA: TXYZA): TLabA;
begin
  Result := XYZAToLabA(AXYZA, CurrentReferenceWhite);
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
  Result := LabAToXYZA(ALabA, CurrentReferenceWhite);
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
        result.hue += 360.0;
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

function AdobeRGBAToXYZA(const ASource: TAdobeRGBA): TXYZA;
var R,G,B: single;
begin
  R := GammaExpansionTab[ASource.red]/65535;
  G := GammaExpansionTab[ASource.green]/65535;
  B := GammaExpansionTab[ASource.blue]/65535;
  result.X := R*0.57667 + G*0.18556 + B*0.18823;
  result.Y := R*0.29735 + G*0.62736 + B*0.07529;
  result.Z := R*0.02703 + G*0.07069 + B*0.99133;
  result.alpha := ASource.alpha/255;
end;

function XYZAToAdobeRGBA(const AXYZA: TXYZA): TAdobeRGBA;
var R,G,B: single;
begin
  with AXYZA do
  begin
    R := Clamp(2.04159*X - 0.56501*Y - 0.34473*Z,0,1);
    G := Clamp(-0.96924*X + 1.87597*Y + 0.04156*Z,0,1);
    B := Clamp(0.01344*X - 0.11836*Y + 1.01518*Z,0,1);
  end;
  result.red := GammaCompressionTab[round(R*65535)];
  result.green := GammaCompressionTab[round(G*65535)];
  result.blue := GammaCompressionTab[round(B*65535)];
  result.alpha := round(Clamp(AXYZA.alpha,0,1)*255);
end;

function StdRGBAToLinearRGBA(const AStdRGBA: TStdRGBA): TLinearRGBA;

  function GammaExpansionF(ACompressed: single): single;
  const oneOver65535 : single = 1/65535;
  var
    intPart: Integer;
    fracPart: Single;
  begin
    if ACompressed <= 0 then
      result := 0
    else if ACompressed >= 1 then
      result := 1
    else
    begin
      ACompressed *= 255;
      intPart := trunc(ACompressed);
      fracPart := ACompressed - intPart;
      if fracPart = 0 then
        result := GammaExpansionTab[intPart]*oneOver65535
      else
        result := (GammaExpansionTab[intPart]*(1-fracPart)+GammaExpansionTab[intPart+1]*fracPart)*oneOver65535;
    end;
  end;

begin
  result.red := GammaExpansionF(AStdRGBA.red);
  result.green := GammaExpansionF(AStdRGBA.green);
  result.blue := GammaExpansionF(AStdRGBA.blue);
  result.alpha := AStdRGBA.alpha;
end;

function LinearRGBAToStdRGBA(const ALinearRGBA: TLinearRGBA): TStdRGBA;

  function GammaCompressionF(AExpanded: single): single;
  const oneOver255 : single = 1/255;
    oneOver256: single = 1/256;
  var
    intPart: Integer;
    fracPart: Single;
  begin
    if AExpanded <= 0 then
      result := 0
    else if AExpanded >= 1 then
      result := 1
    else
    begin
      AExpanded *= 65535;
      intPart := trunc(AExpanded);
      fracPart := AExpanded - intPart;
      if fracPart = 0 then
        result := (GammaCompressionTab[intPart] + GammaCompressionTabFrac[intPart]*oneOver256)*oneOver255
      else
        result := ( (GammaCompressionTab[intPart]+GammaCompressionTabFrac[intPart]*oneOver256)     *(1-fracPart)+
                    (GammaCompressionTab[intPart+1]+GammaCompressionTabFrac[intPart+1]*oneOver256) * fracPart     )*oneOver255;
    end;
  end;

begin
  result.red := GammaCompressionF(ALinearRGBA.red);
  result.green := GammaCompressionF(ALinearRGBA.green);
  result.blue := GammaCompressionF(ALinearRGBA.blue);
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
      if result.hue < 0 then result.hue += 6;
      result.hue *= 60;
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

{$ENDIF}

{$IFDEF INCLUDE_INITIALIZATION}
{$UNDEF INCLUDE_INITIALIZATION} 

  PrepareReferenceWhiteArray;
  SetReferenceWhite(2, 'D65');

  {$DEFINE INCLUDE_INITIALIZATION}
  {$I generatedcolorspace.inc}

{$ENDIF}
