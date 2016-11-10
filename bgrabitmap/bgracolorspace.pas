unit BGRAColorspace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes;

type
  { TCIERGB }

  TCIERGB = packed record
    //components are between 0 and 1
    R,G,B,A: single;
    function ToBGRA: TBGRAPixel;
    procedure FromBGRA(AValue: TBGRAPixel);
    function ToExpanded: TExpandedPixel;
    procedure FromExpanded(AValue: TExpandedPixel);
  end;

  { TCIEXYZ }

  TCIEXYZ = packed record
    //components are between 0 and 1
    X, Y, Z,
    A: single;
    function ToRGB: TCIERGB;
    procedure FromRGB(AValue: TCIERGB);
  end;

  { TBGRAPixelColorspaceHelper }

  TBGRAPixelColorspaceHelper = record helper(TBGRAPixelHelper) for TBGRAPixel
    function ToXYZ: TCIEXYZ;
    procedure FromXYZ(const AValue: TCIEXYZ);
  end;

  { TExpandedPixelColorspaceHelper }

  TExpandedPixelColorspaceHelper = record helper(TExpandedPixelHelper) for TExpandedPixel
    function ToXYZ: TCIEXYZ;
    procedure FromXYZ(const AValue: TCIEXYZ);
  end;

procedure RGBToXYZ(R, G, B: single; out X, Y, Z: single);
procedure XYZToRGB(X, Y, Z: single; out R, G, B: single);

implementation

function ClampF(AValue,AMin,AMax: single): single;
begin
  if AValue <= AMin then result := AMin
  else if AValue >= AMax then result := AMax
  else result := AValue;
end;

procedure RGBToXYZ(R, G, B: single; out X, Y, Z: single);
begin
  // Observer= 2°, Illuminant= D65
  X := R * 0.4124 + G * 0.3576 + B * 0.1805;
  Y := R * 0.2126 + G * 0.7152 + B * 0.0722;
  Z := R * 0.0193 + G * 0.1192 + B * 0.9505;
end;

procedure XYZToRGB(X, Y, Z: single; out R, G, B: single);
begin
  R := ClampF(X * 3.2406 + Y * (-1.5372) + Z * (-0.49), 0, 1);
  G := ClampF(X * (-0.969) + Y * 1.8758 + Z * 0.0415, 0, 1);
  B := ClampF(X * 0.0557 + Y * (-0.2040) + Z * 1.0570, 0, 1);
end;

{ TCIERGB }

function TCIERGB.ToBGRA: TBGRAPixel;
var
  redF,greenF,blueF: single;
begin
  if r > 0.00313 then
    redF := 1.055 * Power(r, 1 / 2.4) - 0.055
  else
    redF := 12.92 * r;
  if g > 0.00313 then
    greenF := 1.055 * Power(g, 1 / 2.4) - 0.055
  else
    greenF := 12.92 * g;
  if b > 0.00313 then
    blueF := 1.055 * Power(b, 1 / 2.4) - 0.055
  else
    blueF := 12.92 * b;

  result.red := round(clampF(redF,0,1)*255);
  result.green := round(clampF(greenF,0,1)*255);
  result.blue := round(clampF(blueF,0,1)*255);
  result.alpha := round(clampF(A,0,1)*255);
end;

procedure TCIERGB.FromBGRA(AValue: TBGRAPixel);
begin
  R := AValue.red/255;
  G := AValue.green/255;
  B := AValue.blue/255;
  A := AValue.alpha/255;

  if R > 0.04045 then
    R := Power((R + 0.055) / 1.055, 2.4)
  else
    R := R / 12.92;
  if G > 0.04045 then
    G := Power((G + 0.055) / 1.055, 2.4)
  else
    G := G / 12.92;
  if B > 0.04045 then
    B := Power((B + 0.055) / 1.055, 2.4)
  else
    B := B / 12.92;
end;

function TCIERGB.ToExpanded: TExpandedPixel;
begin
  result.red := round(ClampF(R,0,1)*65535);
  result.green := round(ClampF(G,0,1)*65535);
  result.blue := round(ClampF(B,0,1)*65535);
  result.alpha := round(ClampF(A,0,1)*65535);
end;

procedure TCIERGB.FromExpanded(AValue: TExpandedPixel);
begin
  R := AValue.red/65535;
  G := AValue.green/65535;
  B := AValue.blue/65535;
  A := AValue.alpha/65535;
end;

{ TCIEXYZ }

function TCIEXYZ.ToBGRA: TBGRAPixel;
begin
  result.FromXYZ(self);
end;

procedure TCIEXYZ.FromBGRA(AValue: TBGRAPixel);
begin
  self := AValue.ToXYZ;
end;

function TCIEXYZ.ToExpanded: TExpandedPixel;
begin
  result.FromXYZ(self);
end;

procedure TCIEXYZ.FromExpanded(AValue: TExpandedPixel);
begin
  self := AValue.ToXYZ;
end;

function TCIEXYZ.ToRGB: TCIERGB;
begin
  XYZToRGB(X,Y,Z, result.R,result.G,result.B);
  result.A := A;
end;

procedure TCIEXYZ.FromRGB(AValue: TCIERGB);
begin
  RGBToXYZ(AValue.R,AValue.G,AValue.B, X,Y,Z);
  A := AValue.A;
end;

{ TExpandedPixelColorspaceHelper }

function TExpandedPixelColorspaceHelper.ToXYZ: TCIEXYZ;
var RGB: TCIERGB;
begin
  RGB.FromExpanded(Self);
  result.FromRGB(RGB);
end;

procedure TExpandedPixelColorspaceHelper.FromXYZ(const AValue: TCIEXYZ);
var redF,greenF,blueF: single;
begin
  self := AValue.ToRGB.ToExpanded;
end;

{ TBGRAPixelColorspaceHelper }

function TBGRAPixelColorspaceHelper.ToXYZ: TCIEXYZ;
var RGB: TCIERGB;
begin
  RGB.FromBGRA(Self);
  result.FromRGB(RGB);
end;

procedure TBGRAPixelColorspaceHelper.FromXYZ(const AValue: TCIEXYZ);
begin
  self := AValue.ToRGB.ToBGRA;
end;

end.

