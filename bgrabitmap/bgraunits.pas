// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAUnits;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, BGRABitmapTypes;

type
  TSVGNumber = single;//double
  ArrayOfTSVGNumber = array of TSVGNumber;

  TCSSUnit = (cuCustom, cuPixel,
              cuCentimeter, cuMillimeter,
              cuInch, cuPica, cuPoint,
              cuFontEmHeight, cuFontXHeight, cuPercent);
  TFloatWithCSSUnit = record
    value: single;
    CSSUnit: TCSSUnit;
  end;

  ArrayOfTFloatWithCSSUnit = array of TFloatWithCSSUnit;


function FloatWithCSSUnit(AValue: single; AUnit: TCSSUnit): TFloatWithCSSUnit;

const
  CSSUnitShortName: array[TCSSUnit] of string =
        ('','px',
         'cm','mm',
         'in','pc','pt',
         'em','ex','%');

type
  { TCSSUnitConverter }

  TCSSUnitConverter = class
  protected
    FViewBoxHeight: TFloatWithCSSUnit;
    FViewBoxWidth: TFloatWithCSSUnit;
    FViewBoxHeightInUnit: array[TCSSUnit] of single;
    FViewBoxWidthInUnit: array[TCSSUnit] of single;
    FCurrentFontEmHeight: TFloatWithCSSUnit;
    function GetRootFontEmHeight: TFloatWithCSSUnit;
    function GetDefaultUnitHeight: TFloatWithCSSUnit; virtual;
    function GetDefaultUnitWidth: TFloatWithCSSUnit; virtual;
    function GetDpiX: single; virtual;
    function GetDpiY: single; virtual;
    function GetFontEmHeight: TFloatWithCSSUnit; virtual;
    function GetFontXHeight: TFloatWithCSSUnit; virtual;
    procedure SetViewBoxHeight(AValue: TFloatWithCSSUnit);
    procedure SetViewBoxWidth(AValue: TFloatWithCSSUnit);
    property FontEmHeight: TFloatWithCSSUnit read GetFontEmHeight;
    property FontXHeight: TFloatWithCSSUnit read GetFontXHeight;
    property DefaultUnitWidth: TFloatWithCSSUnit read GetDefaultUnitWidth;
    property DefaultUnitHeight: TFloatWithCSSUnit read GetDefaultUnitHeight;
  public
    constructor Create;
    function ConvertOrtho(xy: single; sourceUnit, destUnit: TCSSUnit): single; overload;
    function ConvertOrtho(AValue: TFloatWithCSSUnit; destUnit: TCSSUnit): TFloatWithCSSUnit; overload;
    function ConvertWidth(x: single; sourceUnit, destUnit: TCSSUnit): single; overload;
    function ConvertHeight(y: single; sourceUnit, destUnit: TCSSUnit): single; overload;
    function ConvertWidth(AValue: TFloatWithCSSUnit; destUnit: TCSSUnit): TFloatWithCSSUnit; overload;
    function ConvertHeight(AValue: TFloatWithCSSUnit; destUnit: TCSSUnit): TFloatWithCSSUnit; overload;
    function ConvertWidth(AValue: ArrayOfTFloatWithCSSUnit; destUnit: TCSSUnit): ArrayOfTFloatWithCSSUnit; overload;
    function ConvertHeight(AValue: ArrayOfTFloatWithCSSUnit; destUnit: TCSSUnit): ArrayOfTFloatWithCSSUnit; overload;
    function ConvertCoord(pt: TPointF; sourceUnit, destUnit: TCSSUnit): TPointF; overload;
    function GetConversionMatrix(AFromUnit, AToUnit: TCSSUnit): TAffineMatrix; overload;
    function Convert(xy: single; sourceUnit, destUnit: TCSSUnit; dpi: single; containerSize: single = 0): single;
    function ConvertOrtho(AValue: TFloatWithCSSUnit; destUnit: TCSSUnit; containerWidth: single; containerHeight: single): TFloatWithCSSUnit; overload;
    function ConvertWidth(x: single; sourceUnit, destUnit: TCSSUnit; containerWidth: single): single; overload;
    function ConvertHeight(y: single; sourceUnit, destUnit: TCSSUnit; containerHeight: single): single; overload;
    function ConvertWidth(AValue: TFloatWithCSSUnit; destUnit: TCSSUnit; containerWidth: single): TFloatWithCSSUnit; overload;
    function ConvertHeight(AValue: TFloatWithCSSUnit; destUnit: TCSSUnit; containerHeight: single): TFloatWithCSSUnit; overload;
    function ConvertWidth(AValue: ArrayOfTFloatWithCSSUnit; destUnit: TCSSUnit; containerWidth: single): ArrayOfTFloatWithCSSUnit; overload;
    function ConvertHeight(AValue: ArrayOfTFloatWithCSSUnit; destUnit: TCSSUnit; containerHeight: single): ArrayOfTFloatWithCSSUnit; overload;
    function ConvertCoord(pt: TPointF; sourceUnit, destUnit: TCSSUnit; containerWidth: single; containerHeight: single): TPointF; overload;
    function GetConversionMatrix(AFromUnit, AToUnit: TCSSUnit; containerWidth: single; containerHeight: single): TAffineMatrix; overload;
    class function parseValue(AValue: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit; overload; static;
    class function parseValue(AValue: string; ADefault: single): single; overload; static;
    class function parseArrayOfNumbers(AValue: string): ArrayOfTSVGNumber; overload; static;
    class function parseArrayOfValuesWithUnit(AValue: string): ArrayOfTFloatWithCSSUnit; overload; static;
    class function formatValue(AValue: TFloatWithCSSUnit; APrecision: integer = 7): string; overload; static;
    class function formatValue(AValue: single; APrecision: integer = 7): string; overload; static;
    class function formatValue(AValue: ArrayOfTSVGNumber; APrecision: integer = 7): string; overload; static;
    class function formatValue(AValue: ArrayOfTFloatWithCSSUnit; APrecision: integer = 7): string; overload; static;
    property ViewBoxWidth: TFloatWithCSSUnit read FViewBoxWidth write SetViewBoxWidth;
    property ViewBoxHeight: TFloatWithCSSUnit read FViewBoxHeight write SetViewBoxHeight;
    property DpiX: single read GetDpiX;
    property DpiY: single read GetDpiY;
    property CurrentFontEmHeight: TFloatWithCSSUnit read FCurrentFontEmHeight write FCurrentFontEmHeight;
    property RootFontEmHeight: TFloatWithCSSUnit read GetRootFontEmHeight;
  end;

implementation

uses BGRATransform;

var
  formats: TFormatSettings;

const InchFactor: array[TCSSUnit] of integer =
      (9600, 9600,
       254, 2540,
       100, 600, 7200,
       0, 0, 0);

function FloatWithCSSUnit(AValue: single; AUnit: TCSSUnit): TFloatWithCSSUnit;
begin
  result.value:= AValue;
  result.CSSUnit:= AUnit;
end;

{ TCSSUnitConverter }

procedure TCSSUnitConverter.SetViewBoxHeight(AValue: TFloatWithCSSUnit);
var
  u: TCSSUnit;
begin
  if (FViewBoxHeight.value=AValue.value) and
    (FViewBoxHeight.CSSUnit=AValue.CSSUnit) then Exit;
  FViewBoxHeight:=AValue;
  for u := low(TCSSUnit) to high(TCSSUnit) do
    FViewBoxHeightInUnit[u] := ConvertHeight(FViewBoxHeight, u, 0).value;
end;

procedure TCSSUnitConverter.SetViewBoxWidth(AValue: TFloatWithCSSUnit);
var
  u: TCSSUnit;
begin
  if (FViewBoxWidth.value=AValue.value) and
    (FViewBoxWidth.CSSUnit=AValue.CSSUnit) then Exit;
  FViewBoxWidth:=AValue;
  for u := low(TCSSUnit) to high(TCSSUnit) do
    FViewBoxWidthInUnit[u] := ConvertWidth(FViewBoxWidth, u, 0).value;
end;

function TCSSUnitConverter.GetRootFontEmHeight: TFloatWithCSSUnit;
begin
  result := FloatWithCSSUnit(12, cuPoint);
end; 

function TCSSUnitConverter.GetFontEmHeight: TFloatWithCSSUnit;
begin
  result := FCurrentFontEmHeight;
end;

function TCSSUnitConverter.GetFontXHeight: TFloatWithCSSUnit;
begin
  result := FCurrentFontEmHeight;
  result.value := result.value * 0.5; //approximation
end;

function TCSSUnitConverter.GetDefaultUnitHeight: TFloatWithCSSUnit;
begin
  result := FloatWithCSSUnit(1,cuPixel);
end;

function TCSSUnitConverter.GetDefaultUnitWidth: TFloatWithCSSUnit;
begin
  result := FloatWithCSSUnit(1,cuPixel);
end;

function TCSSUnitConverter.GetDpiX: single;
begin
  result := 96;
end;

function TCSSUnitConverter.GetDpiY: single;
begin
  result := 96;
end;

function TCSSUnitConverter.Convert(xy: single; sourceUnit, destUnit: TCSSUnit;
  dpi: single; containerSize: single): single;
var sourceFactor, destFactor: integer;
begin
  //fallback values for cuCustom as pixels
  if sourceUnit = cuCustom then sourceUnit := cuPixel;
  if destUnit = cuCustom then destUnit := cuPixel;
  if (sourceUnit = destUnit) then
    result := xy
  else
  if sourceUnit = cuPercent then
  begin
    result := xy/100*containerSize;
  end else
  if sourceUnit = cuFontEmHeight then
  begin
    with FontEmHeight do result := Convert(xy*value,CSSUnit, destUnit, dpi);
  end else
  if sourceUnit = cuFontXHeight then
  begin
    with FontXHeight do result := Convert(xy*value,CSSUnit, destUnit, dpi);
  end else
  if destUnit = cuFontEmHeight then
  begin
    with ConvertHeight(FontEmHeight, sourceUnit) do
      if value = 0 then result := 0 else result := xy/value;
  end else
  if destUnit = cuFontXHeight then
  begin
    with ConvertHeight(FontXHeight, sourceUnit) do
      if value = 0 then result := 0 else result := xy/value;
  end else
  if sourceUnit = cuPixel then
  begin
    if dpi = 0 then result := 0
    else result := xy*(InchFactor[destUnit]/(dpi*100));
  end else
  if destUnit = cuPixel then
  begin
    if dpi = 0 then result := 0
    else result := xy*((dpi*100)/InchFactor[sourceUnit]);
  end else
  begin
    sourceFactor := InchFactor[sourceUnit];
    destFactor := InchFactor[destUnit];
    if (sourceFactor = 0) or (destFactor = 0) then
      result := 0
    else
      result := xy*(destFactor/sourceFactor);
  end;
end;

function TCSSUnitConverter.ConvertOrtho(AValue: TFloatWithCSSUnit;
  destUnit: TCSSUnit; containerWidth: single; containerHeight: single): TFloatWithCSSUnit;
begin
  result.value := (ConvertWidth(AValue.value, AValue.CSSUnit, destUnit, containerWidth) +
                   ConvertHeight(AValue.value, AValue.CSSUnit, destUnit, containerHeight)) / 2;
  result.CSSUnit:= destUnit;
end;

function TCSSUnitConverter.ConvertWidth(x: single; sourceUnit,
  destUnit: TCSSUnit; containerWidth: single): single;
begin
  if sourceUnit = destUnit then
    result := x
  else if sourceUnit = cuCustom then
  with DefaultUnitWidth do
  begin
    result := x*ConvertWidth(value,CSSUnit, destUnit, containerWidth)
  end
  else if sourceUnit = cuPercent then
  begin
    result := x/100*containerWidth;
  end
  else if destUnit = cuCustom then
  with ConvertWidth(DefaultUnitWidth,sourceUnit) do
  begin
    if value = 0 then
      result := 0
    else
      result := x/value;
  end else
    result := Convert(x, sourceUnit, destUnit, DpiX, containerWidth);
end;

function TCSSUnitConverter.ConvertHeight(y: single; sourceUnit,
  destUnit: TCSSUnit; containerHeight: single): single;
begin
  if sourceUnit = cuCustom then
  with DefaultUnitHeight do
  begin
    result := y*ConvertHeight(value,CSSUnit, destUnit, containerHeight)
  end
  else if sourceUnit = cuPercent then
  begin
    result := y/100*containerHeight;
  end
  else if destUnit = cuCustom then
  with ConvertHeight(DefaultUnitHeight,sourceUnit) do
  begin
    if value = 0 then
      result := 0
    else
      result := y/value;
  end else
    result := Convert(y, sourceUnit, destUnit, DpiY, containerHeight);
end;

function TCSSUnitConverter.ConvertWidth(AValue: TFloatWithCSSUnit;
  destUnit: TCSSUnit; containerWidth: single): TFloatWithCSSUnit;
begin
  result.CSSUnit := destUnit;
  result.value:= ConvertWidth(AValue.value,AValue.CSSUnit,destUnit,containerWidth);
end;

function TCSSUnitConverter.ConvertHeight(AValue: TFloatWithCSSUnit;
  destUnit: TCSSUnit; containerHeight: single): TFloatWithCSSUnit;
begin
  result.CSSUnit := destUnit;
  result.value:= ConvertHeight(AValue.value,AValue.CSSUnit,destUnit,containerHeight);
end;

function TCSSUnitConverter.ConvertWidth(AValue: ArrayOfTFloatWithCSSUnit;
  destUnit: TCSSUnit; containerWidth: single): ArrayOfTFloatWithCSSUnit;
var
  i: integer;
begin
  for i := low(AValue) to high(AValue) do
   AValue[i]:= ConvertWidth(AValue[i],destUnit,containerWidth);
  result := AValue;
end;

function TCSSUnitConverter.ConvertHeight(AValue: ArrayOfTFloatWithCSSUnit;
  destUnit: TCSSUnit; containerHeight: single): ArrayOfTFloatWithCSSUnit;
var
  i: integer;
begin
  for i := low(AValue) to high(AValue) do
   AValue[i]:= ConvertHeight(AValue[i],destUnit,containerHeight);
  result := AValue;
end;

function TCSSUnitConverter.ConvertCoord(pt: TPointF; sourceUnit,
  destUnit: TCSSUnit; containerWidth: single; containerHeight: single): TPointF;
begin
  result.x := ConvertWidth(pt.x, sourceUnit, destUnit, containerWidth);
  result.y := ConvertHeight(pt.y, sourceUnit, destUnit, containerHeight);
end;

function TCSSUnitConverter.GetConversionMatrix(AFromUnit, AToUnit: TCSSUnit;
  containerWidth: single; containerHeight: single): TAffineMatrix;
var
  ptUnit: TPointF;
begin
  ptUnit := ConvertCoord(PointF(1, 1), AFromUnit, AToUnit, containerWidth, containerHeight);
  result := AffineMatrixScale(ptUnit.x, ptUnit.y);
end;

class function TCSSUnitConverter.parseValue(AValue: string;
  ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit;
var cssUnit: TCSSUnit;
  errPos: integer;
begin
  AValue := trim(AValue);
  result.CSSUnit:= cuCustom;
  for cssUnit := succ(cuCustom) to high(cssUnit) do
    if (length(AValue)>=length(CSSUnitShortName[cssUnit])) and
     (CompareText(copy(AValue,length(AValue)-length(CSSUnitShortName[cssUnit])+1,length(CSSUnitShortName[cssUnit])),
        CSSUnitShortName[cssUnit])=0) then
    begin
      AValue := copy(AValue,1,length(AValue)-length(CSSUnitShortName[cssUnit]));
      result.CSSUnit := cssUnit;
      break;
    end;
  val(AValue,result.value,errPos);
  if errPos <> 0 then
    result := ADefault;
end;

class function TCSSUnitConverter.parseValue(AValue: string; ADefault: single): single;
var
  errPos: integer;
begin
  AValue := trim(AValue);
  val(AValue,result,errPos);
  if errPos <> 0 then
    result := ADefault;
end;

class function TCSSUnitConverter.parseArrayOfNumbers(AValue: string): ArrayOfTSVGNumber;
var
  i, l,p: integer;

  procedure CanAddToArray;
  var
    len: integer;
  begin
    if l <> 0 then
    begin
      len := length(result);
      setlength(result,len+1);
      result[len] := parseValue( copy(AValue,p,l), 0);
    end;
  end;

begin
  AValue := trim(AValue);
  if AValue = '' then exit(nil);

  setlength(result,0);
  p:= 1;
  l:= 0;
  for i := 1 to length(AValue) do
  begin
    if AValue[i] in [#9,#10,#13,#32,#44] then
    begin
      CanAddToArray;
      p:= i+1;
      l:= 0;
    end
    else
      Inc(l);
  end;
  CanAddToArray;
end;

class function TCSSUnitConverter.parseArrayOfValuesWithUnit(AValue: string): ArrayOfTFloatWithCSSUnit;
var
  i, l,p: integer;
  def: TFloatWithCSSUnit;

  procedure CanAddToArray;
  var
    len: integer;
  begin
    if l <> 0 then
    begin
      len := length(result);
      setlength(result,len+1);
      result[len] := parseValue( copy(AValue,p,l), def);
    end;
  end;

begin
  AValue := trim(AValue);
  if AValue = '' then exit(nil);

  def := FloatWithCSSUnit(0, cuCustom);
  setlength(result,0);
  p:= 1;
  l:= 0;
  for i := 1 to length(AValue) do
  begin
    if AValue[i] in [#9,#10,#13,#32,#44] then
    begin
      CanAddToArray;
      p:= i+1;
      l:= 0;
    end
    else
      Inc(l);
  end;
  CanAddToArray;
end;

class function TCSSUnitConverter.formatValue(AValue: TFloatWithCSSUnit; APrecision: integer = 7): string;
begin
  result := FloatToStrF(AValue.value,ffGeneral,APrecision,0,formats)+CSSUnitShortName[AValue.CSSUnit];
end;

class function TCSSUnitConverter.formatValue(AValue: single; APrecision: integer
  ): string;
begin
  result := FloatToStrF(AValue,ffGeneral,APrecision,0,formats);
end;

class function TCSSUnitConverter.formatValue(AValue: ArrayOfTSVGNumber; APrecision: integer = 7): string;
var
  i, len: integer;
begin
  len:= length(AValue);
  if len = 0 then
    result:= ''
  else if len = 1 then
    result:= formatValue(AValue[0], APrecision)
  else
  begin
    result:= '';
    for i := 0 to len-1 do
    begin
      result:= result + formatValue(AValue[i], APrecision);
      if i <> (len-1) then
       result:= result + ', ';
    end;
  end;
end;

class function TCSSUnitConverter.formatValue(AValue: ArrayOfTFloatWithCSSUnit; APrecision: integer = 7): string;
var
  i, len: integer;
begin
  len:= length(AValue);
  if len = 0 then
    result:= ''
  else if len = 1 then
    result:= formatValue(AValue[0], APrecision)
  else
  begin
    result:= '';
    for i := 0 to len-1 do
    begin
      result:= result + formatValue(AValue[i], APrecision);
      if i <> (len-1) then
       result:= result + ', ';
    end;
  end;
end;

constructor TCSSUnitConverter.Create;
begin
  inherited;
  FCurrentFontEmHeight:= GetRootFontEmHeight;
  ViewBoxWidth := FloatWithCSSUnit(0, cuPixel);
  ViewBoxHeight := FloatWithCSSUnit(0, cuPixel);
end;

function TCSSUnitConverter.ConvertOrtho(xy: single; sourceUnit,
  destUnit: TCSSUnit): single;
begin
  result := (ConvertWidth(xy, sourceUnit, destUnit) +
    ConvertHeight(xy, sourceUnit, destUnit)) / 2;
end;

function TCSSUnitConverter.ConvertOrtho(AValue: TFloatWithCSSUnit;
  destUnit: TCSSUnit): TFloatWithCSSUnit;
begin
  result.value := (ConvertWidth(AValue.value, AValue.CSSUnit, destUnit) +
                   ConvertHeight(AValue.value, AValue.CSSUnit, destUnit)) / 2;
  result.CSSUnit:= destUnit;
end;

function TCSSUnitConverter.ConvertWidth(x: single; sourceUnit,
  destUnit: TCSSUnit): single;
begin
  result := ConvertWidth(x, sourceUnit, destUnit, FViewBoxWidthInUnit[destUnit]);
end;

function TCSSUnitConverter.ConvertHeight(y: single; sourceUnit,
  destUnit: TCSSUnit): single;
begin
  result := ConvertHeight(y, sourceUnit, destUnit, FViewBoxHeightInUnit[destUnit]);
end;

function TCSSUnitConverter.ConvertWidth(AValue: TFloatWithCSSUnit;
  destUnit: TCSSUnit): TFloatWithCSSUnit;
begin
  result := ConvertWidth(AValue, destUnit, FViewBoxWidthInUnit[destUnit]);
end;

function TCSSUnitConverter.ConvertHeight(AValue: TFloatWithCSSUnit;
  destUnit: TCSSUnit): TFloatWithCSSUnit;
begin
  result := ConvertHeight(AValue, destUnit, FViewBoxHeightInUnit[destUnit]);
end;

function TCSSUnitConverter.ConvertWidth(AValue: ArrayOfTFloatWithCSSUnit;
  destUnit: TCSSUnit): ArrayOfTFloatWithCSSUnit;
begin
  result := ConvertWidth(AValue, destUnit, FViewBoxWidthInUnit[destUnit]);
end;

function TCSSUnitConverter.ConvertHeight(AValue: ArrayOfTFloatWithCSSUnit;
  destUnit: TCSSUnit): ArrayOfTFloatWithCSSUnit;
begin
  result := ConvertHeight(AValue, destUnit, FViewBoxHeightInUnit[destUnit]);
end;

function TCSSUnitConverter.ConvertCoord(pt: TPointF; sourceUnit,
  destUnit: TCSSUnit): TPointF;
begin
  result := ConvertCoord(pt, sourceUnit, destUnit,
    FViewBoxWidthInUnit[destUnit], FViewBoxHeightInUnit[destUnit]);
end;

function TCSSUnitConverter.GetConversionMatrix(AFromUnit, AToUnit: TCSSUnit): TAffineMatrix;
begin
  result := GetConversionMatrix(AFromUnit, AToUnit,
    FViewBoxWidthInUnit[AToUnit], FViewBoxHeightInUnit[AToUnit]);
end;

initialization

  formats := DefaultFormatSettings;
  formats.DecimalSeparator := '.';

end.

