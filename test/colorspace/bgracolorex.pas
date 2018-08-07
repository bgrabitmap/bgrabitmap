unit BGRAColorEx;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, FPimage, BGRAGraphics, BGRABitmapTypes;

type

  { TColorEx }

  TColorEx = object
  private
    FColorspace: TColorspaceAny;
    FValue: array[0..31] of byte;
  private
    function GetAlpha: byte;
    function GetAlphaPercent: single;
    function GetBlack: single;
    function GetBlackPercent: single;
    function GetBlue: byte;
    function GetBluePercent: single;
    function GetCyan: single;
    function GetCyanPercent: single;
    function GetGreen: byte;
    function GetGreenPercent: single;
    function GetIsOpaque: boolean;
    function GetIsTransparent: boolean;
    function GetLightness: single;
    function GetLightnessPercent: single;
    function GetRed: byte;
    function GetRedPercent: single;
    function GetSaturation: single;
    function GetSaturationPercent: single;
    function GetYellow: single;
    function GetYellowPercent: single;
    function GetHue: single;
    function GetHuePercent: single;
    function GetMagenta: single;
    function GetMagentaPercent: single;
    procedure SetYellow(AValue: single);
    procedure SetYellowPercent(AValue: single);
    procedure SetAlpha(AValue: byte);
    procedure SetAlphaPercent(AValue: single);
    procedure SetBlack(AValue: single);
    procedure SetBlackPercent(AValue: single);
    procedure SetBlue(AValue: byte);
    procedure SetBluePercent(AValue: single);
    procedure SetCyan(AValue: single);
    procedure SetCyanPercent(AValue: single);
    procedure SetGreen(AValue: byte);
    procedure SetGreenPercent(AValue: single);
    procedure SetLightness(AValue: single);
    procedure SetLightnessPercent(AValue: single);
    procedure SetRed(AValue: byte);
    procedure SetRedPercent(AValue: single);
    procedure SetSaturation(AValue: single);
    procedure SetSaturationPercent(AValue: single);
    procedure SetHue(AValue: single);
    procedure SetHuePercent(AValue: single);
    procedure SetMagenta(AValue: single);
    procedure SetMagentaPercent(AValue: single);
  public
    class function New: TColorEx;
    class function New(const AValue: TColorEx): TColorEx;
    class function New(const AValue: string): TColorEx;
    class function New(const AValue: TColor): TColorEx;
    class function New(const AValue: TBGRAPixel): TColorEx;
    class function New(const AValue: TExpandedPixel): TColorEx;
    class function New(const AValue: TStdRGBA): TColorEx;
    class function New(const AValue: TLinearRGBA): TColorEx;
    class function New(const AValue: TXYZA): TColorEx;
    class function New(const AValue: TLabA): TColorEx;
    class function New(const AValue: TStdHSLA): TColorEx;
    class function New(const AValue: TStdHSVA): TColorEx;
    class function New(const AValue: TStdCMYK): TColorEx;
    class function New(const AValue: TLChA): TColorEx;
    class function New(const ARed, AGreen, ABlue: byte; const AAlpha: byte = 255): TColorEx;
    procedure SetValue(const AValue; AColorspace: TColorspaceAny);
    procedure GetValue(out AValue; AColorspace: TColorspaceAny);
  public
    function ToBGRAPixel: TBGRAPixel;
    function ToColor: TColor;
    function ToDecimal: integer;
    function ToGrayscale: TColorEx;
    function ToHex: string;
    function ToStdRGBA: TStdRGBA;
    function ToStdHSLA: TStdHSLA;
    function ToStdHSVA: TStdHSVA;
    function ToStdCMYK: TStdCMYK;
    function ToExpandedPixel: TExpandedPixel;
    function ToLinearRGBA: TLinearRGBA;
    function ToAdobeRGBA: TAdobeRGBA;
    function ToHSLAPixel: THSLAPixel;
    function ToGSBAPixel: TGSBAPixel;
    function ToXYZA: TXYZA;
    function ToLabA: TLabA;
    function ToLChA: TLChA;
    function ToName: string;
    function ToString: string;
    function ToInvert: TColorEx;
    procedure FromBGRAPixel(AValue: TBGRAPixel);
    procedure FromColor(AValue: TColor);
    procedure FromDecimal(AValue: integer);
    procedure FromHex(AValue: string);
    procedure FromStdRGBA(const AValue: TStdRGBA);
    procedure FromStdHSLA(const AValue: TStdHSLA);
    procedure FromStdHSVA(const AValue: TStdHSVA);
    procedure FromStdCMYK(const AValue: TStdCMYK);
    procedure FromExpandedPixel(const AValue: TExpandedPixel);
    procedure FromLinearRGBA(const AValue: TLinearRGBA);
    procedure FromAdobeRGBA(const AValue: TAdobeRGBA);
    procedure FromHSLAPixel(const AValue: THSLAPixel);
    procedure FromGSBAPixel(const AValue: TGSBAPixel);
    procedure FromXYZA(AValue: TXYZA);
    procedure FromLabA(AValue: TLabA);
    procedure FromLChA(AValue: TLChA);
    procedure FromName(AValue: string);
    procedure FromString(AValue: string);
  public
    function Fade(APercent: single): TColorEx;
    function Darken(APercent: single): TColorEx;
    function Lighten(APercent: single): TColorEx;
    function Premultiply: TColorEx;
  public
    property Colorspace: TColorspaceAny read FColorspace;
    property Red: byte read GetRed write SetRed;
    property Green: byte read GetGreen write SetGreen;
    property Blue: byte read GetBlue write SetBlue;
    property Alpha: byte read GetAlpha write SetAlpha;
    property Hue: single read GetHue write SetHue;
    property Saturation: single read GetSaturation write SetSaturation;
    property Lightness: single read GetLightness write SetLightness;
    property Cyan: single read GetCyan write SetCyan;
    property Magenta: single read GetMagenta write SetMagenta;
    property Yellow: single read GetYellow write SetYellow;
    property Black: single read GetBlack write SetBlack;
    property RedPercent: single read GetRedPercent write SetRedPercent;
    property GreenPercent: single read GetGreenPercent write SetGreenPercent;
    property BluePercent: single read GetBluePercent write SetBluePercent;
    property AlphaPercent: single read GetAlphaPercent write SetAlphaPercent;
    property HuePercent: single read GetHuePercent write SetHuePercent;
    property SaturationPercent: single read GetSaturationPercent write SetSaturationPercent;
    property LightnessPercent: single read GetLightnessPercent write SetLightnessPercent;
    property CyanPercent: single read GetCyanPercent write SetCyanPercent;
    property MagentaPercent: single read GetMagentaPercent write SetMagentaPercent;
    property YellowPercent: single read GetYellowPercent write SetYellowPercent;
    property BlackPercent: single read GetBlackPercent write SetBlackPercent;
    property Name: string read ToName write FromName;
    property AsHex: string read ToHex write FromHex;
    property AsDecimal: integer read ToDecimal write FromDecimal;
    property AsString: string read ToString write FromString;
    property AsColor: TColor read ToColor write FromColor;
    property AsBGRAPixel: TBGRAPixel read ToBGRAPixel write FromBGRAPixel;
    property AsStdRGBA: TStdRGBA read ToStdRGBA write FromStdRGBA;
    property AsStdHSLA: TStdHSLA read ToStdHSLA write FromStdHSLA;
    property AsStdHSVA: TStdHSVA read ToStdHSVA write FromStdHSVA;
    property AsStdCMYK: TStdCMYK read ToStdCMYK write FromStdCMYK;
    property AsExpandedPixel: TExpandedPixel read ToExpandedPixel write FromExpandedPixel;
    property AsLinearRGBA: TLinearRGBA read ToLinearRGBA write FromLinearRGBA;
    property AsAdobeRGBA: TAdobeRGBA read ToAdobeRGBA write FromAdobeRGBA;
    property AsHSLAPixel: THSLAPixel read ToHSLAPixel write FromHSLAPixel;
    property AsGSBAPixel: TGSBAPixel read ToGSBAPixel write FromGSBAPixel;
    property AsXYZA: TXYZA read ToXYZA write FromXYZA;
    property AsLabA: TLabA read ToLabA write FromLabA;
    property AsLChA: TLChA read ToLChA write FromLChA;
    property AsGrayscale: TColorEx read ToGrayscale;
    property AsInvert: TColorEx read ToInvert;
    property IsTransparent: boolean read GetIsTransparent;
    property IsOpaque: boolean read GetIsOpaque;
  end;

function ColorEx(const ARed, AGreen, ABlue: byte; const AAlpha: byte = 255): TColorEx;
function ColorEx(const AValue: string): TColorEx;
function ColorEx(const AValue: string; const AAlpha: single): TColorEx;
function clRandom: TColorEx;

operator := (const AValue: TColorEx): string;
operator := (const AValue: TColorEx): TColor;
operator := (const AValue: TColorEx): TBGRAPixel;
operator := (const AValue: TColorEx): TExpandedPixel;
operator := (const AValue: TColorEx): TStdRGBA;
operator := (const AValue: TColorEx): TLinearRGBA;
operator := (const AValue: TColorEx): TXYZA;
operator := (const AValue: TColorEx): TLabA;
operator := (const AValue: TColorEx): TStdHSLA;
operator := (const AValue: TColorEx): TStdHSVA;
operator := (const AValue: TColorEx): TStdCMYK;
operator := (const AValue: TColorEx): TLChA;

operator := (const AValue: string): TColorEx;
operator := (const AValue: TColor): TColorEx;
operator := (const AValue: TBGRAPixel): TColorEx;
operator := (const AValue: TExpandedPixel): TColorEx;
operator := (const AValue: TStdRGBA): TColorEx;
operator := (const AValue: TLinearRGBA): TColorEx;
operator := (const AValue: TXYZA): TColorEx;
operator := (const AValue: TLabA): TColorEx;
operator := (const AValue: TStdHSLA): TColorEx;
operator := (const AValue: TStdHSVA): TColorEx;
operator := (const AValue: TStdCMYK): TColorEx;
operator := (const AValue: TLChA): TColorEx;

implementation

function ColorEx(const ARed, AGreen, ABlue: byte; const AAlpha: byte): TColorEx;
begin
  Result := TColorEx.New(ARed, AGreen, ABlue, AAlpha);
end;

function ColorEx(const AValue: string): TColorEx;
begin
  Result := TColorEx.New(AValue);
end;

function ColorEx(const AValue: string; const AAlpha: single): TColorEx;
begin
  Result := TColorEx.New(AValue);
  Result.AlphaPercent := AAlpha;
end;

function clRandom: TColorEx;
begin
  Result := TStdHSLA.New(Random(360), 0.5, 0.5);
end;

operator := (const AValue: TColorEx): string;
begin
  Result := AValue.AsString;
end;

operator := (const AValue: TColorEx): TColor;
begin
  Result := AValue.AsColor;
end;

operator := (const AValue: TColorEx): TBGRAPixel;
begin
  Result := AValue.AsBGRAPixel;
end;

operator := (const AValue: TColorEx): TExpandedPixel;
begin
  Result := AValue.AsExpandedPixel;
end;

operator := (const AValue: TColorEx): TStdRGBA;
begin
  Result := AValue.AsStdRGBA;
end;

operator := (const AValue: TColorEx): TLinearRGBA;
begin
  Result := AValue.AsLinearRGBA;
end;

operator := (const AValue: TColorEx): TXYZA;
begin
  Result := AValue.AsXYZA;
end;

operator := (const AValue: TColorEx): TLabA;
begin
  Result := AValue.AsLabA;
end;

operator := (const AValue: TColorEx): TStdHSLA;
begin
  Result := AValue.AsStdHSLA;
end;

operator := (const AValue: TColorEx): TStdHSVA;
begin
  Result := AValue.AsStdHSVA;
end;

operator := (const AValue: TColorEx): TStdCMYK;
begin
  Result := AValue.AsStdCMYK;
end;

operator := (const AValue: TColorEx): TLChA;
begin
  Result := AValue.AsLChA;
end;

operator := (const AValue: string): TColorEx;
begin
  Result.AsString := AValue;
end;

operator := (const AValue: TColor): TColorEx;
begin
  Result.AsColor := AValue;
end;

operator := (const AValue: TBGRAPixel): TColorEx;
begin
  Result.AsBGRAPixel := AValue;
end;

operator := (const AValue: TExpandedPixel): TColorEx;
begin
  Result.AsExpandedPixel := AValue;
end;

operator := (const AValue: TStdRGBA): TColorEx;
begin
  Result.AsStdRGBA := AValue;
end;

operator := (const AValue: TLinearRGBA): TColorEx;
begin
  Result.AsLinearRGBA := AValue;
end;

operator := (const AValue: TXYZA): TColorEx;
begin
  Result.AsXYZA := AValue;
end;

operator := (const AValue: TLabA): TColorEx;
begin
  Result.AsLabA := AValue;
end;

operator := (const AValue: TStdHSLA): TColorEx;
begin
  Result.AsStdHSLA := AValue;
end;

operator := (const AValue: TStdHSVA): TColorEx;
begin
  Result.AsStdHSVA := AValue;
end;

operator := (const AValue: TStdCMYK): TColorEx;
begin
  Result.AsStdCMYK := AValue;
end;

operator := (const AValue: TLChA): TColorEx;
begin
  Result.AsLChA := AValue;
end;

{ TColorEx }

function TColorEx.GetAlpha: byte;
begin
  Result := round(AsStdRGBA.alpha * 255);
end;

function TColorEx.GetAlphaPercent: single;
begin
  Result := AsStdRGBA.alpha * 100;
end;

function TColorEx.ToBGRAPixel: TBGRAPixel;
begin
  GetValue(result, result.Colorspace);
end;

function TColorEx.GetBlack: single;
begin
  Result := AsStdCMYK.K;
end;

function TColorEx.GetBlackPercent: single;
begin
  Result := Black * 100;
end;

function TColorEx.GetBlue: byte;
begin
  Result := round(AsStdRGBA.blue * 255);
end;

function TColorEx.GetBluePercent: single;
begin
  Result := AsStdRGBA.blue * 100;
end;

function TColorEx.ToStdCMYK: TStdCMYK;
begin
  GetValue(result, result.Colorspace);
end;

function TColorEx.ToColor: TColor;
begin
  GetValue(result, TColorColorspace);
end;

function TColorEx.GetCyan: single;
begin
  Result := AsStdCMYK.C;
end;

function TColorEx.GetCyanPercent: single;
begin
  Result := Cyan * 100;
end;

function TColorEx.ToDecimal: integer;
begin
  with AsBGRAPixel do
    Result := (red shl 16) or (green shl 8) or blue;
end;

function TColorEx.ToExpandedPixel: TExpandedPixel;
begin
  GetValue(result, result.Colorspace);
end;

function TColorEx.ToGrayscale: TColorEx;
begin
  Result.AsStdRGBA := AsBGRAPixel.ToGrayscale(True);
end;

function TColorEx.GetGreen: byte;
begin
  Result := round(AsStdRGBA.green * 255);
end;

function TColorEx.GetGreenPercent: single;
begin
  Result := AsStdRGBA.green * 100;
end;

function TColorEx.GetIsOpaque: boolean;
begin
  Result := AlphaPercent >= 100;
end;

function TColorEx.GetIsTransparent: boolean;
begin
  Result := AlphaPercent <= 0;
end;

function TColorEx.ToHex: string;
begin
  with AsBGRAPixel do
  begin
    Result := '#' + IntToHex(red, 2) + IntToHex(green, 2) + IntToHex(blue, 2);
    if alpha <> 255 then
      Result += IntToHex(alpha, 2);
  end;
end;

function TColorEx.ToStdHSLA: TStdHSLA;
begin
  GetValue(result, result.Colorspace);
end;

function TColorEx.ToStdHSVA: TStdHSVA;
begin
  GetValue(result, result.Colorspace);
end;

function TColorEx.GetHue: single;
begin
  Result := AsStdHSLA.hue * 100;
end;

function TColorEx.GetHuePercent: single;
begin
  Result := Hue / 3.6;
end;

function TColorEx.ToInvert: TColorEx;
begin
  with AsStdRGBA do
  begin
    Result.AsStdRGBA := TStdRGBA.New(1 - red, 1 - green, 1 - blue, alpha);
  end;
end;

function TColorEx.ToLabA: TLabA;
begin
  GetValue(result, result.Colorspace);
end;

function TColorEx.ToLChA: TLChA;
begin
  GetValue(result, result.Colorspace);
end;

function TColorEx.GetLightness: single;
begin
  Result := AsStdHSLA.lightness;
end;

function TColorEx.GetLightnessPercent: single;
begin
  Result := Lightness * 100;
end;

function TColorEx.ToLinearRGBA: TLinearRGBA;
begin
  GetValue(result, result.Colorspace);
end;

function TColorEx.ToAdobeRGBA: TAdobeRGBA;
begin
  GetValue(result, result.Colorspace);
end;

function TColorEx.ToHSLAPixel: THSLAPixel;
begin
  GetValue(result, result.Colorspace);
end;

function TColorEx.ToGSBAPixel: TGSBAPixel;
begin
  GetValue(result, result.Colorspace);
end;

function TColorEx.GetMagenta: single;
begin
  Result := AsStdCMYK.M;
end;

function TColorEx.GetMagentaPercent: single;
begin
  Result := Magenta * 100;
end;

function TColorEx.ToName: string;
var
  idx: integer;
  c: TBGRAPixel;
begin
  Result := '';
  c := AsBGRAPixel;
  if Assigned(CSSColors) then
  begin
    idx := CSSColors.IndexOfColor(c, 1000);
    if idx <> -1 then
    begin
      Result := CSSColors.Name[idx];
      exit;
    end;
  end;
end;

function TColorEx.GetRed: byte;
begin
  Result := round(AsStdRGBA.red * 255);
end;

function TColorEx.GetRedPercent: single;
begin
  Result := AsStdRGBA.red * 100;
end;

function TColorEx.GetSaturation: single;
begin
  Result := AsStdHSLA.saturation;
end;

function TColorEx.GetSaturationPercent: single;
begin
  Result := Saturation * 100;
end;

function TColorEx.ToStdRGBA: TStdRGBA;
begin
  GetValue(result, result.Colorspace);
end;

function TColorEx.ToString: string;
begin
  Result := BGRAToStr(AsBGRAPixel);
end;

function TColorEx.ToXYZA: TXYZA;
begin
  GetValue(result, result.Colorspace);
end;

function TColorEx.GetYellow: single;
begin
  Result := AsStdCMYK.Y;
end;

function TColorEx.GetYellowPercent: single;
begin
  Result := Yellow * 100;
end;

procedure TColorEx.SetAlpha(AValue: byte);
begin
  with AsStdRGBA do
    Self.AsStdRGBA := TStdRGBA.New(red, green, blue, AValue / 255);
end;

procedure TColorEx.SetAlphaPercent(AValue: single);
begin
  with AsStdRGBA do
    Self.AsStdRGBA := TStdRGBA.New(red, green, blue, AValue / 100);
end;

procedure TColorEx.FromBGRAPixel(AValue: TBGRAPixel);
begin
  SetValue(AValue, AValue.Colorspace);
end;

procedure TColorEx.SetBlack(AValue: single);
begin
  with AsStdCMYK do
    Self.AsStdCMYK := TStdCMYK.New(C, M, Y, AValue);
end;

procedure TColorEx.SetBlackPercent(AValue: single);
begin
  with AsStdCMYK do
    Self.AsStdCMYK := TStdCMYK.New(C, M, Y, AValue / 100);
end;

procedure TColorEx.SetBlue(AValue: byte);
begin
  with AsStdRGBA do
    Self.AsStdRGBA := TStdRGBA.New(red, green, AValue / 255, alpha);
end;

procedure TColorEx.SetBluePercent(AValue: single);
begin
  with AsStdRGBA do
    Self.AsStdRGBA := TStdRGBA.New(red, green, AValue / 100, alpha);
end;

procedure TColorEx.FromStdCMYK(const AValue: TStdCMYK);
begin
  SetValue(AValue, AValue.Colorspace);
end;

procedure TColorEx.FromColor(AValue: TColor);
begin
  SetValue(AValue, AValue.Colorspace);
end;

procedure TColorEx.SetCyan(AValue: single);
begin
  with AsStdCMYK do
    Self.AsStdCMYK:= TStdCMYK.New(AValue, M, Y, K);
end;

procedure TColorEx.SetCyanPercent(AValue: single);
begin
  with AsStdCMYK do
    Self.AsStdCMYK := TStdCMYK.New(AValue / 100, M, Y, K);
end;

procedure TColorEx.FromDecimal(AValue: integer);
var
  r, g, b: byte;
begin
  r := (AValue shr 16) and $000000ff;
  g := (AValue shr 8) and $000000ff;
  b := AValue and $000000ff;
  AsBGRAPixel := TBGRAPixel.New(r, g, b);
end;

procedure TColorEx.FromExpandedPixel(const AValue: TExpandedPixel);
begin
  SetValue(AValue, AValue.Colorspace);
end;

procedure TColorEx.SetGreen(AValue: byte);
begin
  with AsStdRGBA do
    Self.AsStdRGBA := TStdRGBA.New(red, AValue / 255, blue, alpha);
end;

procedure TColorEx.SetGreenPercent(AValue: single);
begin
  with AsStdRGBA do
    Self.AsStdRGBA := TStdRGBA.New(red, AValue / 100, blue, alpha);
end;

procedure TColorEx.FromHex(AValue: string);
var
  missingValues, error: boolean;
  c: TBGRAPixel;
begin
  c := BGRAPixelTransparent;
  TryStrToBGRA(AValue, c, missingValues, error);
  if not (missingValues or error) then
    AsBGRAPixel := c;
end;

procedure TColorEx.FromStdHSLA(const AValue: TStdHSLA);
begin
  SetValue(AValue, AValue.Colorspace);
end;

procedure TColorEx.FromStdHSVA(const AValue: TStdHSVA);
begin
  SetValue(AValue, AValue.Colorspace);
end;

procedure TColorEx.FromHSLAPixel(const AValue: THSLAPixel);
begin
  SetValue(AValue, AValue.Colorspace);
end;

procedure TColorEx.FromGSBAPixel(const AValue: TGSBAPixel);
begin
  SetValue(AValue, AValue.Colorspace);
end;

procedure TColorEx.SetHue(AValue: single);
begin
  with AsStdHSLA do
    Self.AsStdHSLA := TStdHSLA.New(AValue, saturation, lightness, alpha);
end;

procedure TColorEx.SetHuePercent(AValue: single);
begin
  with AsStdHSLA do
    Self.AsStdHSLA := TStdHSLA.New(AValue / 3.6, saturation, lightness, alpha);
end;

procedure TColorEx.FromLabA(AValue: TLabA);
begin
  SetValue(AValue, AValue.Colorspace);
end;

procedure TColorEx.FromLChA(AValue: TLChA);
begin
  SetValue(AValue, AValue.Colorspace);
end;

procedure TColorEx.SetLightness(AValue: single);
begin
  with AsStdHSLA do
    Self.AsStdHSLA := TStdHSLA.New(hue, saturation, AValue, alpha);
end;

procedure TColorEx.SetLightnessPercent(AValue: single);
begin
  with AsStdHSLA do
    Self.AsStdHSLA := TStdHSLA.New(hue, saturation, AValue / 100, alpha);
end;

procedure TColorEx.FromLinearRGBA(const AValue: TLinearRGBA);
begin
  SetValue(AValue, AValue.Colorspace);
end;

procedure TColorEx.FromAdobeRGBA(const AValue: TAdobeRGBA);
begin
  SetValue(AValue, AValue.Colorspace);
end;

procedure TColorEx.SetMagenta(AValue: single);
begin
  with AsStdCMYK do
    Self.AsStdCMYK := TStdCMYK.New(C, AValue, Y, K);
end;

procedure TColorEx.SetMagentaPercent(AValue: single);
begin
  with AsStdCMYK do
    Self.AsStdCMYK := TStdCMYK.New(C, AValue / 100, Y, K);
end;

class function TColorEx.New: TColorEx;
begin
  Result := BGRAPixelTransparent;
end;

procedure TColorEx.FromName(AValue: string);
var
  missingValues, error: boolean;
  c: TBGRAPixel;
begin
  c := BGRAPixelTransparent;
  TryStrToBGRA(AValue, c, missingValues, error);
  if not (missingValues or error) then
    AsBGRAPixel := c;
end;

procedure TColorEx.SetRed(AValue: byte);
begin
  with AsStdRGBA do
    Self.AsStdRGBA := TStdRGBA.New(AValue / 255, green, blue, alpha);
end;

procedure TColorEx.SetRedPercent(AValue: single);
begin
  with AsStdRGBA do
    Self.AsStdRGBA := TStdRGBA.New(AValue / 100, green, blue, alpha);
end;

procedure TColorEx.SetSaturation(AValue: single);
begin
  with AsStdHSLA do
    Self.AsStdHSLA := TStdHSLA.New(hue, AValue, lightness, alpha);
end;

procedure TColorEx.SetSaturationPercent(AValue: single);
begin
  with AsStdHSLA do
    Self.AsStdHSLA := TStdHSLA.New(hue, AValue / 100, lightness, alpha);
end;

procedure TColorEx.FromStdRGBA(const AValue: TStdRGBA);
begin
  SetValue(AValue, AValue.Colorspace);
end;

procedure TColorEx.FromString(AValue: string);
begin
  AsBGRAPixel := StrToBGRA(AValue);
end;

procedure TColorEx.FromXYZA(AValue: TXYZA);
begin
  SetValue(AValue, AValue.Colorspace);
end;

function TColorEx.Fade(APercent: single): TColorEx;
begin
  Result := Self;
  if APercent = 1 then
    Exit;
  Result.AlphaPercent := Result.AlphaPercent * APercent;
end;

function TColorEx.Darken(APercent: single): TColorEx;
begin
  Result := Self;
  Result.LightnessPercent := Result.LightnessPercent - APercent;
end;

function TColorEx.Lighten(APercent: single): TColorEx;
begin
  Result := Self;
  Result.LightnessPercent := Result.LightnessPercent + APercent;
end;

function TColorEx.Premultiply: TColorEx;
begin
  with AsStdRGBA do
    Self.AsStdRGBA:= TStdRGBA.New(red*alpha,green*alpha,blue*alpha,alpha);
  Result:=Self;
end;

procedure TColorEx.SetYellow(AValue: single);
begin
  with AsStdCMYK do
    Self.AsStdCMYK := TStdCMYK.New(C, M, AValue, K);
end;

procedure TColorEx.SetYellowPercent(AValue: single);
begin
  with AsStdCMYK do
    Self.AsStdCMYK := TStdCMYK.New(C, M, AValue / 100, K);
end;

class function TColorEx.New(const AValue: TColorEx): TColorEx;
begin
  Result := AValue;
end;

class function TColorEx.New(const AValue: string): TColorEx;
begin
  Result := AValue;
end;

class function TColorEx.New(const AValue: TColor): TColorEx;
begin
  Result := AValue;
end;

class function TColorEx.New(const AValue: TBGRAPixel): TColorEx;
begin
  Result := AValue;
end;

class function TColorEx.New(const AValue: TExpandedPixel): TColorEx;
begin
  Result := AValue;
end;

class function TColorEx.New(const AValue: TStdRGBA): TColorEx;
begin
  Result := AValue;
end;

class function TColorEx.New(const AValue: TLinearRGBA): TColorEx;
begin
  Result := AValue;
end;

class function TColorEx.New(const AValue: TXYZA): TColorEx;
begin
  Result := AValue;
end;

class function TColorEx.New(const AValue: TLabA): TColorEx;
begin
  Result := AValue;
end;

class function TColorEx.New(const AValue: TStdHSLA): TColorEx;
begin
  Result := AValue;
end;

class function TColorEx.New(const AValue: TStdHSVA): TColorEx;
begin
  Result := AValue;
end;

class function TColorEx.New(const AValue: TStdCMYK): TColorEx;
begin
  Result := AValue;
end;

class function TColorEx.New(const AValue: TLChA): TColorEx;
begin
  Result := AValue;
end;

class function TColorEx.New(const ARed, AGreen, ABlue: byte; const AAlpha: byte): TColorEx;
begin
  Result := TStdRGBA.New(ARed / 255, AGreen / 255, ABlue / 255, AAlpha / 255);
end;

procedure TColorEx.SetValue(const AValue; AColorspace: TColorspaceAny);
begin
  FColorspace:= AColorspace;
  move(AValue, FValue, AColorspace.GetSize);
end;

procedure TColorEx.GetValue(out AValue; AColorspace: TColorspaceAny);
begin
  if Assigned(FColorspace) then
    FColorspace.Convert(FValue, AValue, AColorspace)
  else
    TBGRAPixelColorspace.Convert(BGRAPixelTransparent, AValue, AColorspace)
end;

end.
