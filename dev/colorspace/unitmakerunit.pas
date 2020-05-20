// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
unit UnitMakerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, rttiutils;

{ Table of working colorspaces (where blending could be done)

              |  Byte            Word            Single
--------------+---------------------------------------------
sRGB          |  BGRAPixel       FPColor         StdRGBA
Adobe RGB     |  AdobeRGBA
gray          |  ByteMask        (WordMask)
linear RGB    |                  ExpandedPixel   LinearRGBA
XYZ           |                  WordXYZA        XYZA

}

type
  TColorspaceEnum = (csColor, csBGRAPixel, csFPColor, csStdRGBA, //sRGB
    csAdobeRGBA,
    csStdHSLA, csStdHSVA, csStdCMYKA,              //based on sRGB
    csByteMask, {csWordMask,}                        //linear grayscale
    csExpandedPixel, csLinearRGBA,                 //linear RGB
    csHSLAPixel, csGSBAPixel,                      //based on linear RGB
    csXYZA, csWordXYZA,                            //CIE XYZ
    csLabA, csLChA);                               //based on XYZ

  TChannelValueType = (cvtByte, cvtWord, cvtLongWord, cvtSingle, cvtDouble);

const
  ChannelValueTypeName : array[TChannelValueType] of string = ('byte', 'word', 'longword', 'single', 'double');
  ChannelValueTypePrecision : array[TChannelValueType] of integer = (1, 2, 4, 2, 6);
  ChannelValueTypeBitDepth : array[TChannelValueType] of integer = (8, 16, 32, 28, 58);
  MAXWORD = $ffff;

type
  TColorspaceInfo = record
    Name: string;
    Declaration: string;
    Colorspace: string;
    HasAlpha, NeedRefWhite: boolean;
    ValueType: TChannelValueType;
    BasicHelper: boolean;
    VariableNames, FullNames, MinMax: string;
    IsBridge, HasImaginary: boolean;
  end;

const
  ColorspaceInfo: array [TColorspaceEnum] of TColorspaceInfo =
  ((Name: 'Color';         Declaration: 'type helper';   Colorspace: 'StdRGB';      HasAlpha: false;  NeedRefWhite: false;  ValueType: cvtByte;    BasicHelper: false;
   VariableNames: 'red,green,blue';                      FullNames: 'Red,Green,Blue';                 MinMax: '0,0,0,255,255,255';                IsBridge: false; HasImaginary: false),
   (Name: 'BGRAPixel';     Declaration: 'record helper'; Colorspace: 'StdRGB';      HasAlpha: true;   NeedRefWhite: false;  ValueType: cvtByte;    BasicHelper: true;
   VariableNames: 'red,green,blue,alpha';                FullNames: 'Red,Green,Blue,Alpha';           MinMax: '0,0,0,0,255,255,255,255';          IsBridge: false; HasImaginary: false),
   (Name: 'FPColor';       Declaration: 'record helper'; Colorspace: 'StdRGB';      HasAlpha: true;   NeedRefWhite: false;  ValueType: cvtWord;    BasicHelper: true;
   VariableNames: 'red,green,blue,alpha';                FullNames: 'Red,Green,Blue,Alpha';           MinMax: '0,0,0,0,65535,65535,65535,65535';  IsBridge: false; HasImaginary: false),

   (Name: 'StdRGBA';       Declaration: 'packed record'; Colorspace: 'StdRGB';      HasAlpha: true;   NeedRefWhite: false;  ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'red,green,blue,alpha';                FullNames: 'Red,Green,Blue,Alpha';           MinMax: '0,0,0,0,1,1,1,1';                  IsBridge: false; HasImaginary: false),
   (Name: 'AdobeRGBA';     Declaration: 'packed record'; Colorspace: 'AdobeRGB';    HasAlpha: true;   NeedRefWhite: false;  ValueType: cvtByte;    BasicHelper: false;
   VariableNames: 'red,green,blue,alpha';                FullNames: 'Red,Green,Blue,Alpha';           MinMax: '0,0,0,0,255,255,255,255';          IsBridge: false; HasImaginary: false),

   (Name: 'StdHSLA';       Declaration: 'packed record'; Colorspace: 'StdHSL';      HasAlpha: true;   NeedRefWhite: false;  ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'hue,saturation,lightness,alpha';      FullNames: 'Hue,Saturation,Lightness,Alpha'; MinMax: '0,0,0,0,360,1,1,1';                IsBridge: false; HasImaginary: false),
   (Name: 'StdHSVA';       Declaration: 'packed record'; Colorspace: 'StdHSV';      HasAlpha: true;   NeedRefWhite: false;  ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'hue,saturation,value,alpha';          FullNames: 'Hue,Saturation,Value,Alpha';     MinMax: '0,0,0,0,360,1,1,1';                IsBridge: false; HasImaginary: false),
   (Name: 'StdCMYK';       Declaration: 'packed record'; Colorspace: 'StdCMYK';     HasAlpha: false;  NeedRefWhite: false;  ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'C,M,Y,K';                             FullNames: 'Cyan,Magenta,Yellow,Black';      MinMax: '0,0,0,0,1,1,1,1';                  IsBridge: false; HasImaginary: false),

   (Name: 'ByteMask';      Declaration: 'packed record'; Colorspace: 'Grayscale';   HasAlpha: false;  NeedRefWhite: false;  ValueType: cvtByte;    BasicHelper: false;
   VariableNames: 'gray';                                FullNames: 'Gray';         MinMax: '0,255';                                              IsBridge: false; HasImaginary: false),
{   (Name: 'WordMask';      Declaration: 'packed record'; Colorspace: 'Grayscale';  HasAlpha: false;  NeedRefWhite: false;  ValueType: cvtWord;    BasicHelper: false;
   VariableNames: 'gray';                                FullNames: 'Gray';         MinMax: '0,65535';                                            IsBridge: false; HasImaginary: false),}

   (Name: 'ExpandedPixel'; Declaration: 'record helper'; Colorspace: 'LinearRGB';   HasAlpha: true;   NeedRefWhite: false;  ValueType: cvtWord;    BasicHelper: true;
   VariableNames: 'red,green,blue,alpha';                FullNames: 'Red,Green,Blue,Alpha';           MinMax: '0,0,0,0,65535,65535,65535,65535';  IsBridge: true; HasImaginary: false),
   (Name: 'LinearRGBA';    Declaration: 'packed record'; Colorspace: 'LinearRGB';   HasAlpha: true;   NeedRefWhite: false;  ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'red,green,blue,alpha';                FullNames: 'Red,Green,Blue,Alpha';           MinMax: '0,0,0,0,1,1,1,1';                  IsBridge: false; HasImaginary: false),

   (Name: 'HSLAPixel';     Declaration: 'record helper'; Colorspace: 'HSL';         HasAlpha: true;   NeedRefWhite: false;  ValueType: cvtWord;    BasicHelper: true;
   VariableNames: 'hue,saturation,lightness,alpha';      FullNames: 'Hue,Saturation,Lightness,Alpha'; MinMax: '0,0,0,0,65535,65535,65535,65535';  IsBridge: false; HasImaginary: false),
   (Name: 'GSBAPixel';     Declaration: 'record helper'; Colorspace: 'GSB';         HasAlpha: true;   NeedRefWhite: false;  ValueType: cvtWord;    BasicHelper: true;
   VariableNames: 'hue,saturation,lightness,alpha';      FullNames: 'Hue,Saturation,Brightness,Alpha';MinMax: '0,0,0,0,65535,65535,65535,65535';  IsBridge: false; HasImaginary: false),

   (Name: 'XYZA';          Declaration: 'packed record'; Colorspace: 'CIE XYZ';     HasAlpha: true;   NeedRefWhite: true;   ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'X,Y,Z,alpha';                         FullNames: 'X,Y,Z,Alpha';                    MinMax: '0,0,0,0,1,1,1,1';                  IsBridge: false; HasImaginary: true),
   (Name: 'WordXYZA';      Declaration: 'packed record'; Colorspace: 'CIE XYZ';     HasAlpha: true;   NeedRefWhite: true;   ValueType: cvtWord;    BasicHelper: false;
   VariableNames: 'X,Y,Z,alpha';                         FullNames: 'X,Y,Z,Alpha';                    MinMax: '0,0,0,0,50000,50000,50000,65535';  IsBridge: false; HasImaginary: true),

   (Name: 'LabA';          Declaration: 'packed record'; Colorspace: 'CIE Lab';     HasAlpha: true;   NeedRefWhite: false;  ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'L,a,b,alpha';                         FullNames: 'Lightness,a,b,Alpha';            MinMax: '0,-166,-132,0,100,142,147,1';      IsBridge: false; HasImaginary: true),
   (Name: 'LChA';          Declaration: 'packed record'; Colorspace: 'CIE LCh';     HasAlpha: true;   NeedRefWhite: false;  ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'L,C,h,alpha';                         FullNames: 'Lightness,Chroma,Hue,Alpha';     MinMax: '0,0,0,0,100,192,360,1';            IsBridge: false; HasImaginary: true) );

type
  TColorPair = record
    First, Last: TColorspaceEnum;
    ToFirstFunc, ToLastFunc: string;
    HandlesExtraAlpha: boolean;
    Weight: integer;
  end;

type
  TColorspaceArray = array of TColorspaceEnum;

  TPath = array of record
            PairIndex: integer;
            Reverse: boolean;
          end;
  TPathArray = array of TPath;

var
  PairsList: array of TColorPair;
  PathMatrix: packed array[TColorspaceEnum, TColorspaceEnum] of Word;
  ConvMatrix: packed array[TColorspaceEnum, TColorspaceEnum] of boolean;
  ConvBridgeMatrix: packed array[TColorspaceEnum, TColorspaceEnum] of TColorspaceEnum;

function FindPath(AFrom, ATo: TColorspaceEnum): TColorspaceArray;
function NewColorPair(AFirst, ALast: TColorspaceEnum;
                      AToFirstFunc, AToLastFunc: string;
                      AHandlesExtraAlpha: boolean; AWeight: integer): TColorPair;
procedure AddColorPair(AFirst, ALast: TColorspaceEnum;
                       AToFirstFunc : string = ''; AToLastFunc: string = '';
                       AHandlesExtraAlpha: boolean = true;
                       AWeight: integer = 1);
function GetConversionFunction(AFrom, ATo: TColorspaceEnum): string;
procedure AddAlphaPairs;
procedure GenerateCode;

implementation

uses math;

function IsHelperOnly(cs: TColorspaceEnum): boolean;
begin
  result := ColorspaceInfo[cs].Declaration.EndsWith(' helper');
end;

procedure AddColorPair(AFirst, ALast: TColorspaceEnum;
                       AToFirstFunc : string = ''; AToLastFunc: string = '';
                       AHandlesExtraAlpha: boolean = true;
                       AWeight: integer = 1);
begin
  SetLength(PairsList, Length(PairsList) + 1);
  if AToFirstFunc = '' then AToFirstFunc:= ColorspaceInfo[ALast].Name + 'To' + ColorspaceInfo[AFirst].Name;
  if AToLastFunc = '' then AToLastFunc:= ColorspaceInfo[AFirst].Name + 'To' + ColorspaceInfo[ALast].Name;
  PairsList[Length(PairsList) - 1] := NewColorPair(AFirst, ALast, AToFirstFunc, AToLastFunc, AHandlesExtraAlpha, AWeight);
end;

function GetConversionFunction(AFrom, ATo: TColorspaceEnum; out AHandlesExtraAlpha: boolean): string;
var
  i: Integer;
begin
  for i := 0 to high(PairsList) do
    if (PairsList[i].First = AFrom) and (PairsList[i].Last = ATo) and (PairsList[i].ToLastFunc <> '') then
    begin
      AHandlesExtraAlpha := PairsList[i].HandlesExtraAlpha;
      exit(PairsList[i].ToLastFunc)
    end
    else if (PairsList[i].First = ATo) and (PairsList[i].Last = AFrom) and (PairsList[i].ToFirstFunc <> '') then
    begin
      AHandlesExtraAlpha := PairsList[i].HandlesExtraAlpha;
      exit(PairsList[i].ToFirstFunc);
    end;

  result := ColorspaceInfo[AFrom].Name + 'To' + ColorspaceInfo[ATo].Name;
  AHandlesExtraAlpha := ConvMatrix[AFrom,ATo];
end;

function GetConversionFunction(AFrom, ATo: TColorspaceEnum): string;
var AHandlesExtraAlpha: boolean;
begin
  result := GetConversionFunction(AFrom,ATo,AHandlesExtraAlpha);
end;

function NeedXYZReferenceWhite(c1,c2: TColorspaceEnum): boolean;
begin
  result := (ColorspaceInfo[c1].NeedRefWhite or ColorspaceInfo[c2].NeedRefWhite) and not
            ([c1,c2] = [csXYZA,csWordXYZA]);
end;

function GetConversionFunctionRec(c1, c2: TColorspaceEnum; AValueParam: string; AReferenceWhiteParam: string = ''): string;
var
  c, cBridge: TColorspaceEnum;

  procedure AppendConv(ATo: TColorspaceEnum);
  begin
    if (AReferenceWhiteParam <> '') and NeedXYZReferenceWhite(c,ATo) then
      result := GetConversionFunction(c,ATo)+'('+result+','+AReferenceWhiteParam+')'
    else
      result := GetConversionFunction(c,ATo)+'('+result+')';
    c := ATo;
  end;

begin
  result := AValueParam;
  c := c1;
  while c <> c2 do
  begin
    if ConvMatrix[c,c2] then AppendConv(c2) else
    begin
      cBridge := ConvBridgeMatrix[c,c2];
      if cBridge = low(TColorspaceEnum) then
        raise exception.Create('Conversion bridge not found');
      AppendConv(cBridge);
    end;
  end;
end;

procedure AddAlphaPairs;
var
  i, j: TColorspaceEnum;
begin
  for i := Low(TColorspaceEnum) to High(TColorspaceEnum) do
    for j := Low(TColorspaceEnum) to High(TColorspaceEnum) do
      if (ColorspaceInfo[j].Name = ColorspaceInfo[i].Name + 'A') and
         (ColorspaceInfo[j].HasAlpha and not ColorspaceInfo[i].HasAlpha) then
      begin
        AddColorPair(i, j);
        AddColorPair(j, i);
      end;
end;

function FindPathRec(AFrom, ATo: TColorspaceEnum; AEnd: TPath;
  ARemainLen: Word; AWantedPrecision: integer): TPathArray;
var
  i, j: Integer;
  cs: TColorspaceEnum;
  subEnd: TPath;
  subResult: TPathArray;
begin
  result := nil;
  for cs := low(TColorspaceEnum) to high(TColorspaceEnum) do
    if (PathMatrix[AFrom,cs] = ARemainLen-1) and
      (ChannelValueTypePrecision[ColorspaceInfo[cs].ValueType] >= AWantedPrecision) then
    begin
      for i := 0 to high(PairsList) do
      if ((PairsList[i].First = cs) and (PairsList[i].Last = ATo)) or
        ((PairsList[i].Last = cs) and (PairsList[i].First = ATo))  then
      begin
        subEnd := nil;
        setLength(subEnd, length(AEnd)+1);
        for j := 0 to high(AEnd) do
          subEnd[j+1] := AEnd[j];
        subEnd[0].PairIndex := i;
        subEnd[0].Reverse:= PairsList[i].Last = cs;

        if ARemainLen <= 1 then
        begin
          setlength(result, length(result)+1);
          result[high(result)] := subEnd;
        end else
        begin
          subResult := FindPathRec(AFrom,cs, subEnd, ARemainLen-1, AWantedPrecision);
          for j := 0 to high(subResult) do
          begin
            setlength(result, length(result)+1);
            result[high(result)] := subResult[j];
          end;
        end;
        break;
      end;
    end;
end;

function FindPath(AFrom, ATo: TColorspaceEnum): TColorspaceArray;
var
  pathLen: Word;
  i: Integer;
  subResult: TPathArray;
  bestIndex, bestWeight, weight, j, wantedPrecision: integer;
  path: TPath;
begin
  result := nil;
  pathLen := PathMatrix[AFrom,ATo];
  wantedPrecision := min(ChannelValueTypePrecision[ColorspaceInfo[AFrom].ValueType],
                         ChannelValueTypePrecision[ColorspaceInfo[ATo].ValueType]);
  if pathLen = MAXWORD then
    raise exception.Create('No path found');

  subResult := FindPathRec(AFrom,ATo, nil, pathLen, wantedPrecision);
  bestIndex := -1;
  bestWeight := maxLongint;
  for i := 0 to high(subResult) do
  begin
    weight := 0;
    path := subResult[i];
    for j := 0 to high(path) do
      inc(weight, PairsList[path[j].PairIndex].Weight);
    if weight < bestWeight then
    begin
      bestWeight := weight;
      bestIndex := i;
    end;
  end;

  if bestIndex = -1 then raise exception.Create('No best path found between '+ColorspaceInfo[AFrom].Name+' to '+ColorspaceInfo[ATo].Name);
  path := subResult[bestIndex];
  setlength(result, length(path)+1);
  for j := 0 to high(path) do
  begin
    if path[j].Reverse then
      result[j] := PairsList[path[j].PairIndex].Last
    else
      result[j] := PairsList[path[j].PairIndex].First;
  end;
  if path[high(path)].Reverse then
    result[high(result)] := PairsList[path[high(path)].PairIndex].First
  else
    result[high(result)] := PairsList[path[high(path)].PairIndex].Last;
end;

procedure MakePathMatrix;

  function FindNewPath(cs: TColorspaceEnum; FromLen: integer): boolean;
  var
    CSFrom: TColorspaceEnum;
    i: Integer;
  begin
    result := false;
    for CSFrom := low(TColorspaceEnum) to high(TColorspaceEnum) do
    if PathMatrix[cs,CSFrom] = FromLen then
    begin
      for i := 0 to high(PairsList) do
        if PairsList[i].First = CSFrom then
        begin
          if PathMatrix[cs, PairsList[i].Last] = MAXWORD then
          begin
            PathMatrix[cs, PairsList[i].Last] := FromLen+1;
            result := true;
          end;
        end else
        if PairsList[i].Last = CSFrom then
        begin
          if PathMatrix[cs, PairsList[i].First] = MAXWORD then
          begin
            PathMatrix[cs, PairsList[i].First] := FromLen+1;
            result := true;
          end;
        end;
    end;
    inc(FromLen);
  end;

var
  cs: TColorspaceEnum;
  FromLen: integer;
begin
  FillWord(PathMatrix, sizeof(PathMatrix) div sizeof(word), MAXWORD);
  for cs := low(TColorspaceEnum) to high(TColorspaceEnum) do
  begin
    PathMatrix[cs,cs] := 0;
    FromLen := 0;
    while FindNewPath(cs, FromLen) do inc(FromLen);
  end;
end;

function NewColorPair(AFirst, ALast: TColorspaceEnum; AToFirstFunc, AToLastFunc: string; AHandlesExtraAlpha: boolean; AWeight: integer): TColorPair;
begin
  with Result do
  begin
    First := AFirst;
    Last := ALast;
    ToFirstFunc:= AToFirstFunc;
    ToLastFunc:= AToLastFunc;
    HandlesExtraAlpha:= AHandlesExtraAlpha;
    Weight := AWeight;
  end;
end;

procedure GenerateCode;
var
  s: string;
  intsl, impsl: TStringList;
  InfSpaceAdd: string;
  ColorTypeDefined: array[TColorspaceEnum] of boolean;

  procedure Add(ls: string);
  begin
    if ls = '' then intsl.add('') else
      intsl.Add(InfSpaceAdd + ls);
  end;

  procedure AddImp(ls: string);
  begin
    impsl.Add(ls);
  end;

  procedure AddProcedureImp(h, ls: string);
  begin
    AddImp(h);
    ls := Trim(ls);
    if ls.EndsWith(';') then delete(ls, length(ls), 1);
    AddImp('begin ' + ls + ' end;');
    AddImp('');
  end;

  procedure AddProcedureImp(h: string; ls: array of string);
  var
    i: Integer;
  begin
    AddImp(h);
    AddImp('begin');
    for i := 0 to high(ls) do
      AddImp('  ' + ls[i]);
    AddImp('end;');
    AddImp('');
  end;

  function GetProcedure(AFullname, AParams: string; AOverload: boolean): string;
  begin
    Result := 'procedure ' + AFullname;
    if AParams <> '' then
      Result += '(' + AParams + ')';
    Result += ';';
    if AOverload then
      Result += ' overload;';
  end;

  function GetFunction(AFullname, AParams, AResultType: string; AOverload: boolean; AStatic: boolean = False): string;
  begin
    Result := 'function ' + AFullname;
    if AParams <> '' then
      Result += '(' + AParams + ')';
    Result += ': ' + AResultType + ';';
    if AOverload then
      Result += 'overload;';
    if AStatic then
      Result += 'static;';
  end;

  function Split(str: string): TStringArray;
  var
    sl: TStringList;
    i: integer;
  begin
    sl := TStringList.Create;
    sl.StrictDelimiter := True;
    sl.CommaText := str;
    SetLength(Result, sl.Count);
    for i := 0 to Length(Result) - 1 do
    begin
      Result[i] := sl.Strings[i];
    end;
    sl.Free;
  end;

  function GetVariablesNames(cp: TColorspaceEnum): TStringArray;
  begin
    Result := Split(ColorspaceInfo[cp].VariableNames);
  end;

  procedure MakeConverters;

    procedure AddAlphaConverter(c1, c2: TColorspaceEnum; ad: string);
    var
      s, ls, h: string;
      i: integer;
      vn: TStringArray;
    begin
      if ad <> '' then
        vn := GetVariablesNames(c1)
      else
        vn := GetVariablesNames(c2);
      h := GetFunction(ColorspaceInfo[c1].Name + 'To' + ColorspaceInfo[c2].Name, 'const A' + ColorspaceInfo[c1].Name + ': T' + ColorspaceInfo[c1].Name + ad, 'T' + ColorspaceInfo[c2].Name, False);
      s := '';
      for i := 0 to Length(vn) - 1 do
      begin
        s += 'A' + ColorspaceInfo[c1].Name + '.' + vn[i];
        if i <> Length(vn) - 1 then
          s += ',';
      end;
      if ad <> '' then
        s += ', AAlpha';
      ls := 'Result := T' + ColorspaceInfo[c2].Name + '.New(' + s + ');';
      AddProcedureImp(h, ls);
    end;

    function GetResult(fn, p: string): string;
    begin
      Result := fn + '(' + p + ')';
    end;

    function AddConverter(c1, c2: TColorspaceEnum): string;
    var
      bp: TColorspaceArray;
      cs1, cs2: TColorspaceEnum;
      i: integer;
      ls, lf, s: string;
      fn, avn: string;
      bb: boolean;
      vmax: string;
      vn, vsam: TStringArray;
      needRefPoint: boolean;
      h, functionName: string;
    begin
      result := '';
      bp := FindPath(c1, c2);
      if Length(bp) = 0 then
      begin
        WriteLn('Path shouldn''t be empty');
        Exit;
      end;
      if not ColorspaceInfo[c1].IsBridge and
         not ColorspaceInfo[c2].IsBridge then
      begin
        for i := 1 to high(bp)-1 do
          if ColorspaceInfo[bp[i]].IsBridge then
          begin
            ConvBridgeMatrix[c1,c2] := bp[i];
            exit(''); //go via bridge
          end;
      end;

      functionName := ColorspaceInfo[c1].Name + 'To' + ColorspaceInfo[c2].Name;
      result := functionName;

      s := 'Result := ';
      ls := '';
      lf := '';
      for i := Length(bp) - 1 downto 1 do
      begin
        cs1 := bp[i - 1];
        cs2 := bp[i];
        s += GetConversionFunction(cs1,cs2) + '(';
        lf += ')';
      end;

      needRefPoint := NeedXYZReferenceWhite(c1,c2);

      vsam := Split(ColorspaceInfo[c2].MinMax);
      vmax := vsam[Length(vsam) - 1];

      ls := s + 'A' + ColorspaceInfo[c1].Name + lf + ';';
      if not ColorspaceInfo[c1].HasAlpha and ColorspaceInfo[c2].HasAlpha then
      begin
        vn := GetVariablesNames(c2);
        avn := vn[Length(vn) - 1];
        if not avn.StartsWith('[') then avn := '.'+avn;
        ls := ls + LineEnding + '  ' + 'Result' + avn + ' := AAlpha;';
        h := GetFunction(functionName,
                         'const A' + ColorspaceInfo[c1].Name + ': T' + ColorspaceInfo[c1].Name + ';const AAlpha' + ': ' + ChannelValueTypeName[ColorspaceInfo[c2].ValueType] + '=' + vmax,
                         'T' + ColorspaceInfo[c2].Name, needRefPoint);
      end
      else
      begin
        h := GetFunction(functionName,
                         'const A' + ColorspaceInfo[c1].Name + ': T' + ColorspaceInfo[c1].Name,
                         'T' + ColorspaceInfo[c2].Name, needRefPoint);
      end;
      AddProcedureImp(h, ls);
      if needRefPoint then
      begin
        ls := 'A' + ColorspaceInfo[c1].Name;
        for i := 0 to Length(bp) - 2 do
        begin
          cs1 := bp[i];
          cs2 := bp[i + 1];
          fn := GetConversionFunction(cs1,cs2);
          bb := NeedXYZReferenceWhite(cs1, cs2);
          if bb then
            ls += ',AReferenceWhite';
          ls := fn + '(' + ls + ')';
        end;
        ls := 'Result := ' + ls + ';';
        if not ColorspaceInfo[c1].HasAlpha and ColorspaceInfo[c2].HasAlpha then
        begin
          h := GetFunction(functionName,
                           'const A' + ColorspaceInfo[c1].Name + ': T' + ColorspaceInfo[c1].Name + '; ' + 'const AReferenceWhite: TXYZReferenceWhite' + ';const AAlpha' + ': ' + ChannelValueTypeName[ColorspaceInfo[c2].ValueType] + '=' + vmax,
                           'T' + ColorspaceInfo[c2].Name, needRefPoint);
          vn := GetVariablesNames(c2);
          avn := vn[Length(vn) - 1];
          if not avn.StartsWith('[') then avn := '.' + avn;
          ls := ls + #13#10 + '  ' + 'Result' + avn + ' := AAlpha;';
        end
        else
          h := GetFunction(functionName,
                           'const A' + ColorspaceInfo[c1].Name + ': T' + ColorspaceInfo[c1].Name + '; ' + 'const AReferenceWhite: TXYZReferenceWhite',
                           'T' + ColorspaceInfo[c2].Name, needRefPoint);
        AddProcedureImp(h, ls);
      end;
    end;

  var
    i, j: TColorspaceEnum;
    convertFunc: string;
    pl: integer;

  begin
    AddImp('{Converters}');
    AddImp('');

    for i := Low(TColorspaceEnum) to High(TColorspaceEnum) do
    begin
      for j := Low(TColorspaceEnum) to High(TColorspaceEnum) do
      begin
        if (ColorspaceInfo[j].Name = ColorspaceInfo[i].Name + 'A'){ or
        ((not (i in AlphaSupportedColorspaces)) and  (j in AlphaSupportedColorspaces)) } then
        begin
          AddAlphaConverter(i, j, '; const AAlpha: single = 1');
          AddAlphaConverter(j, i, '');
        end;
      end;
    end;
    for i := Low(TColorspaceEnum) to High(TColorspaceEnum) do
    begin
      for j := Low(TColorspaceEnum) to High(TColorspaceEnum) do
      begin
        if (i <> j) and (ColorspaceInfo[j].Name <> ColorspaceInfo[i].Name + 'A') and (ColorspaceInfo[i].Name <> ColorspaceInfo[j].Name + 'A') then
        begin
          convertFunc := '';
          for pl := 0 to Length(PairsList) - 1 do
          begin
            if (PairsList[pl].First = i) and (PairsList[pl].Last = j) then
            begin
              convertFunc := PairsList[pl].ToLastFunc;
              break;
            end;
            if (PairsList[pl].Last = i) and (PairsList[pl].First = j) then
            begin
              convertFunc := PairsList[pl].ToFirstFunc;
              break;
            end;
          end;
          if convertFunc = '' then
            convertFunc := AddConverter(i, j);

          if convertFunc = '' then continue;

          ConvMatrix[i,j] := true;

          AddImp('procedure Convert' + ColorspaceInfo[i].Name+'ArrayTo'+ColorspaceInfo[j].Name+'Array' +
                           '(ASource: pointer; ADest: Pointer; ACount: integer; '+
                           'ASourceStride:integer=sizeOf(T'+ColorspaceInfo[i].Name+'); '+
                           'ADestStride:integer=sizeOf(T'+ColorspaceInfo[j].Name+'); '+
                           '{%H-}AReferenceWhite: PXYZReferenceWhite=nil);');
          AddImp('begin');
          if NeedXYZReferenceWhite(i,j) then
            AddImp('  if AReferenceWhite = nil then AReferenceWhite := @CurrentReferenceWhite;');
          AddImp('  while ACount > 0 do begin');
          if NeedXYZReferenceWhite(i,j) then
            AddImp('    T'+ColorspaceInfo[j].Name+'(ADest^) := '+convertFunc+'(T'+ColorspaceInfo[i].Name+'(ASource^), AReferenceWhite^);')
          else
            AddImp('    T'+ColorspaceInfo[j].Name+'(ADest^) := '+convertFunc+'(T'+ColorspaceInfo[i].Name+'(ASource^));');
          AddImp('    inc(PByte(ASource), ASourceStride); inc(PByte(ADest), ADestStride); dec(ACount); end;');
          AddImp('end;');
          AddImp('');
        end;
      end;
    end;
  end;

  procedure MakeHelper(Colorspace: TColorspaceEnum; AHelperOnly, AColorspaceOnly: boolean);
  var
    i: integer;
    HelperName, ColorspaceName, ColorTypeName, n, h, nt: string;
    VariablesNames: TStringArray;
    MinValues,MaxValues: TStringArray;
    cs: TColorspaceEnum;
    b: boolean;

    function GetConvertProcedureImp(cpto: TColorspaceEnum; AReferenceWhiteParam: string): string;
    begin
      Result := 'Result := ' + GetConversionFunctionRec(ColorSpace, cpto, 'Self', AReferenceWhiteParam) + ';';
    end;

    function GetFromConvertProcedureImp(cpfrom: TColorspaceEnum; AReferenceWhiteParam: string): string;
    begin
      Result := 'Self := ' + GetConversionFunctionRec(cpfrom, ColorSpace, 'AValue', AReferenceWhiteParam) + ';';
    end;

    procedure AddNew(s: string; ov: boolean);
    var ls: array of string;
      i: integer;
      params: TStringList;
    begin
      params := TStringList.Create;
      params.Delimiter:= ',';
      params.StrictDelimiter:= true;
      params.DelimitedText := s;

      h := 'class ' + GetFunction('New', 'const ' + s + ':' + ChannelValueTypeName[ColorspaceInfo[Colorspace].ValueType], ColorTypeName, ov, True);
      Add('  ' + h);
      h := 'class ' + GetFunction(HelperName + '.New', 'const ' + s + ':' + ChannelValueTypeName[ColorspaceInfo[Colorspace].ValueType], ColorTypeName, ov);
      case Colorspace of
      csColor: AddProcedureImp(h, 'Result := BGRAGraphics.RGBToColor(' + s + ');');
      else
        begin
          setlength(ls, length(VariablesNames));
          for i := 0 to high(VariablesNames) do
            if VariablesNames[i].StartsWith('[') then
            begin
              if i >= params.Count then
                WriteStr(ls[i],'Result',VariablesNames[i],' := ',MaxValues[i],';')
              else
                WriteStr(ls[i],'Result',VariablesNames[i],' := ',params[i],';');
            end else
            begin
              if i >= params.Count then
                WriteStr(ls[i],'Result.',VariablesNames[i],' := ',MaxValues[i],';')
              else
                WriteStr(ls[i],'Result.',VariablesNames[i],' := ',params[i],';');
            end;
          AddProcedureImp(h, ls);
        end
      end;
      params.Free;
    end;

  var
    ov, ba: boolean;
    vsam, vsfm, body, vn2: TStringArray;
    cn: integer;
    typeDeclaration, flagStr: string;
    handlesExtraAlpha: boolean;
  begin
    ColorspaceName := ColorspaceInfo[Colorspace].Name;

    ColorTypeName := 'T' + ColorspaceName;
    VariablesNames := GetVariablesNames(Colorspace);
    vsfm := Split(ColorspaceInfo[Colorspace].FullNames);
    vsam := Split(ColorspaceInfo[Colorspace].MinMax);
    cn := Length(vsam) div 2;
    setlength(MaxValues, cn);
    setlength(MinValues, cn);
    for i := 0 to cn-1 do
    begin
      MinValues[i] := vsam[i];
      MaxValues[i] := vsam[i+cn];
    end;

    if AColorspaceOnly then
    begin
      Add('{ '+ColorTypeName+'Colorspace }');
      Add('');
      Add(ColorTypeName+'Colorspace = class(TCustomColorspace)');
      Add('  class function GetChannelName(AIndex: integer): string; override;');
      Add('  class function GetChannelCount: integer; override;');
      Add('  class function IndexOfAlphaChannel: integer; override;');
      if not ColorspaceInfo[Colorspace].HasAlpha then
        Add('  class function GetColorTransparency({%H-}AColor: Pointer): TColorTransparency; override;')
      else
        Add('  class function GetColorTransparency(AColor: Pointer): TColorTransparency; override;');
      Add('  class function GetMaxValue(AIndex: integer): single; override;');
      Add('  class function GetMinValue(AIndex: integer): single; override;');
      Add('  class function GetChannelBitDepth({%H-}AIndex: integer): byte; override;');
      Add('  class function GetName: string; override;');
      Add('  class function GetSize: integer; override;');
      Add('  class function GetChannel(AColor: Pointer; AIndex: integer): single; override;');
      Add('  class procedure SetChannel(AColor: Pointer; AIndex: integer; AValue: single); override;');
      Add('  class function GetFlags: TColorspaceFlags; override;');
      Add('end;');
      Add('');
      AddImp('{ '+ColorTypeName+'Colorspace }');
      AddImp('');

      setlength(body, length(vsfm)+3);
      body[0] := 'case AIndex of';
      for i := 0 to high(vsfm) do
        body[i+1] := inttostr(i)+': result := ''' + vsfm[i] + ''';';
      body[high(body)-1] := 'else raise ERangeError.Create(''Index out of bounds'');';
      body[high(body)] := 'end;';
      AddProcedureImp('class function '+ColorTypeName+'Colorspace.GetChannelName(AIndex: integer): string;', body);

      AddProcedureImp('class function '+ColorTypeName+'Colorspace.GetChannelCount: integer;',
                      'result := ' + inttostr(length(vsfm)));

      if ColorspaceInfo[Colorspace].HasAlpha then
        AddProcedureImp('class function '+ColorTypeName+'Colorspace.IndexOfAlphaChannel: integer;',
                        'result := ' + inttostr(length(vsfm)-1))
      else
        AddProcedureImp('class function '+ColorTypeName+'Colorspace.IndexOfAlphaChannel: integer;',
                        'result := -1');

      if not ColorspaceInfo[Colorspace].HasAlpha then
        AddProcedureImp('class function '+ColorTypeName+'Colorspace.GetColorTransparency(AColor: Pointer): TColorTransparency;',
                        'result := ctFullyOpaque')
      else
        AddProcedureImp('class function '+ColorTypeName+'Colorspace.GetColorTransparency(AColor: Pointer): TColorTransparency;',
                        ['if '+ColorTypeName+'(AColor^).'+VariablesNames[cn-1]+' >= '+MaxValues[cn-1]+' then exit(ctFullyOpaque) else',
                        'if '+ColorTypeName+'(AColor^).'+VariablesNames[cn-1]+' <= '+MinValues[cn-1]+' then exit(ctFullyTransparent) else',
                        'exit(ctSemiTransparent)']);

      setlength(body, cn + 3);
      body[0] := 'case AIndex of';
      for i := 0 to cn - 1 do
        body[i+1] := inttostr(i)+': result := ' + MaxValues[i] + ';';
      body[high(body)-1] := 'else raise ERangeError.Create(''Index out of bounds'');';
      body[high(body)] := 'end;';
      AddProcedureImp('class function '+ColorTypeName+'Colorspace.GetMaxValue(AIndex: integer): single;', body);

      for i := 0 to cn - 1 do
        body[i+1] := inttostr(i)+': result := ' + MinValues[i] + ';';
      AddProcedureImp('class function '+ColorTypeName+'Colorspace.GetMinValue(AIndex: integer): single;', body);

      AddProcedureImp('class function '+ColorTypeName+'Colorspace.GetChannelBitDepth(AIndex: integer): byte;',
                      'result := ' + IntToStr(ChannelValueTypeBitDepth[ColorspaceInfo[Colorspace].ValueType]) + ';');

      AddProcedureImp('class function '+ColorTypeName+'Colorspace.GetName: string;',
                      'result := ''' + ColorspaceName + ''';');

      AddProcedureImp('class function '+ColorTypeName+'Colorspace.GetSize: integer;',
                      'result := sizeof(' + ColorTypeName + ');');

      setlength(body, length(VariablesNames)+3);
      body[0] := 'case AIndex of';
      for i := 0 to high(VariablesNames) do
        body[i+1] := inttostr(i)+': result := ' + ColorTypeName + '(AColor^).' + VariablesNames[i] + ';';
      body[high(body)-1] := 'else raise ERangeError.Create(''Index out of bounds'');';
      body[high(body)] := 'end;';
      AddProcedureImp('class function '+ColorTypeName+'Colorspace.GetChannel(AColor: Pointer; AIndex: integer): single;', body);

      setlength(body, length(VariablesNames)+3);
      body[0] := 'case AIndex of';
      for i := 0 to high(VariablesNames) do
      begin
        if not (ColorspaceInfo[Colorspace].ValueType in[cvtSingle,cvtDouble]) then
          body[i+1] := inttostr(i)+': ' + ColorTypeName + '(AColor^).' + VariablesNames[i] + ' := Round(Clamp(AValue,' + MinValues[i] + ',' +MaxValues[i] + '));'
        else
          body[i+1] := inttostr(i)+': ' + ColorTypeName + '(AColor^).' + VariablesNames[i] + ' := AValue;';
      end;
      body[high(body)-1] := 'else raise ERangeError.Create(''Index out of bounds'');';
      body[high(body)] := 'end;';
      AddProcedureImp('class procedure '+ColorTypeName+'Colorspace.SetChannel(AColor: Pointer; AIndex: integer; AValue: single);', body);

      if ColorspaceInfo[Colorspace].NeedRefWhite then flagStr := 'cfMovableReferenceWhite' else
      if Colorspace >= csXYZA then flagStr := 'cfReferenceWhiteIndependent' else
        flagStr := 'cfFixedReferenceWhite';
      if ColorspaceInfo[Colorspace].HasImaginary then flagStr += ',cfHasImaginaryColors';
      AddProcedureImp('class function '+ColorTypeName+'Colorspace.GetFlags: TColorspaceFlags;',
                      'result := [' + flagStr + '];');

      AddImp('');
      exit;
    end;

    if IsHelperOnly(Colorspace) or AHelperOnly then
    begin
      if not AHelperOnly then exit;
      HelperName := ColorTypeName + 'Helper';
      if IsHelperOnly(Colorspace) then
      begin
        if ColorspaceInfo[Colorspace].BasicHelper then
          typeDeclaration := ColorspaceInfo[Colorspace].Declaration+ '(' + ColorTypeName + 'BasicHelper) for ' + ColorTypeName
        else
          typeDeclaration := ColorspaceInfo[Colorspace].Declaration+ ' for ' + ColorTypeName;
      end
      else
      begin
        typeDeclaration := 'record helper for ' + ColorTypeName
      end;
    end else
    begin
      HelperName := ColorTypeName;
      typeDeclaration := ColorspaceInfo[Colorspace].Declaration;
      ColorTypeDefined[Colorspace] := true;
    end;

    Add('{ ' + HelperName + ' }');
    Add('');
    if not IsHelperOnly(Colorspace) and not AHelperOnly then
      Add('P'+ColorspaceName+' = ^'+ColorTypeName+';');
    Add(HelperName + ' = ' + typeDeclaration);

    AddImp('{ ' + HelperName + ' }');
    AddImp('');

    if not IsHelperOnly(Colorspace) and not AHelperOnly then
    begin
      Add('  ' + ColorspaceInfo[Colorspace].VariableNames + ': ' + ChannelValueTypeName[ColorspaceInfo[Colorspace].ValueType] + ';');
    end;

    if not AHelperOnly or IsHelperOnly(Colorspace) then
    begin
      ov := ColorspaceInfo[Colorspace].HasAlpha;

      s := '';
      for i := 0 to Length(vsfm) - 1 do
      begin
        s += 'A'+vsfm[i];
        if i <> Length(vsfm) - 1 then
          s += ',';
      end;
      AddNew(s, ov);

      if ColorspaceInfo[Colorspace].HasAlpha then
      begin
        s := '';
        for i := 0 to Length(vsfm) - 2 do
        begin
          s += 'A'+vsfm[i];
          if i <> Length(vsfm) - 2 then
            s += ',';
        end;
        AddNew(s, ov);
      end;
    end;

    if AHelperOnly then
    begin
      h := 'class function '+HelperName+'.Colorspace: TColorspaceAny; static;';
      AddProcedureImp(h, 'result := T'+ColorspaceName+'Colorspace;');
      Add('  ' + StringReplace(h, HelperName+'.', '', []));

      if Colorspace = csColor then
      begin
        Add('private');
        h := GetFunction(HelperName+'.GetRed', '', 'byte', false);
        AddProcedureImp(h, 'result := {$IFDEF TCOLOR_BLUE_IN_LOW_BYTE}(self shr 16) and $ff{$ELSE}self and $ff{$ENDIF};');
        Add('  ' + StringReplace(h, HelperName+'.', '', []));
        h := GetFunction(HelperName+'.GetGreen', '', 'byte', false);
        AddProcedureImp(h, 'result := (self shr 8) and $ff;');
        Add('  ' + StringReplace(h, HelperName+'.', '', []));
        h := GetFunction(HelperName+'.GetBlue', '', 'byte', false);
        AddProcedureImp(h, 'result := {$IFDEF TCOLOR_BLUE_IN_LOW_BYTE}self and $ff{$ELSE}(self shr 16) and $ff{$ENDIF};');
        Add('  ' + StringReplace(h, HelperName+'.', '', []));

        h := GetProcedure(HelperName+'.SetRed', 'AValue: byte', false);
        AddProcedureImp(h, 'self := {$IFDEF TCOLOR_BLUE_IN_LOW_BYTE}LongWord(self and $00ffff) or (AValue shl 16){$ELSE}LongWord(self and $ffff00) or AValue{$ENDIF};');
        Add('  ' + StringReplace(h, HelperName+'.', '', []));
        h := GetProcedure(HelperName+'.SetGreen', 'AValue: byte', false);
        AddProcedureImp(h, 'self := LongWord(self and $ff00ff) or (AValue shl 8);');
        Add('  ' + StringReplace(h, HelperName+'.', '', []));
        h := GetProcedure(HelperName+'.SetBlue', 'AValue: byte', false);
        AddProcedureImp(h, 'self := {$IFDEF TCOLOR_BLUE_IN_LOW_BYTE}LongWord(self and $ffff00) or AValue{$ELSE}LongWord(self and $00ffff) or (AValue shl 16){$ENDIF};');
        Add('  ' + StringReplace(h, HelperName+'.', '', []));
        add('public');
      end;

      if Colorspace = csXYZA then
      begin
        h := GetProcedure(HelperName+'.ChromaticAdapt', 'const AFrom, ATo: TXYZReferenceWhite', false);
        AddProcedureImp(h, 'ChromaticAdaptXYZ(self.X,self.Y,self.Z, AFrom,ATo);');
        Add('  ' + StringReplace(h, HelperName+'.', '', []));
      end else
      if Colorspace = csWordXYZA then
      begin
        h := GetProcedure(HelperName+'.ChromaticAdapt', 'const AFrom, ATo: TXYZReferenceWhite', false);
        AddProcedureImp(h, 'ChromaticAdaptWordXYZ(self.X,self.Y,self.Z, AFrom,ATo);');
        Add('  ' + StringReplace(h, HelperName+'.', '', []));
      end;

      for cs := Low(TColorspaceEnum) to High(TColorspaceEnum) do
      begin
        if (cs = Colorspace) or not ColorTypeDefined[cs] then Continue;
        if ColorspaceInfo[Colorspace].BasicHelper and (ColorspaceInfo[cs].BasicHelper or (cs = csColor)) then continue;

        n := ColorspaceInfo[cs].Name;
        b := NeedXYZReferenceWhite(cs,Colorspace);
        ba := not ColorspaceInfo[Colorspace].HasAlpha and ColorspaceInfo[cs].HasAlpha;

        h := GetFunction('To' + n, '', 'T' + n, b or ba);
        Add('  ' + h);
        h := GetFunction(HelperName + '.To' + n, '', 'T' + n, b or ba);
        AddProcedureImp(h, GetConvertProcedureImp(cs, ''));

        if ba then
        begin
          h := GetFunction('To' + n, 'AAlpha: ' + ChannelValueTypeName[ColorspaceInfo[cs].ValueType], 'T' + n, b or ba);
          Add('  ' + h);
          h := GetFunction(HelperName + '.To' + n, 'AAlpha: ' + ChannelValueTypeName[ColorspaceInfo[cs].ValueType], 'T' + n, b or ba);
          GetConversionFunction(Colorspace, cs, handlesExtraAlpha);
          if handlesExtraAlpha then
            AddProcedureImp(h, 'result := '+GetConversionFunctionRec(ColorSpace, cs, 'Self, AAlpha', '')+';')
          else
          begin
            vn2 := Split(ColorspaceInfo[cs].VariableNames);
            AddProcedureImp(h, [GetConvertProcedureImp(cs, ''), 'result.'+vn2[high(vn2)]+' := AAlpha;']);
          end;
        end;

        if b then
        begin
          h := GetFunction('To' + n, 'const AReferenceWhite: TXYZReferenceWhite', 'T' + n, b or ba);
          Add('  ' + h);
          h := GetFunction(HelperName + '.To' + n, 'const AReferenceWhite: TXYZReferenceWhite', 'T' + n, b or ba);
          AddProcedureImp(h, GetConvertProcedureImp(cs, 'AReferenceWhite'));
        end;
      end;

      for cs := Low(TColorspaceEnum) to High(TColorspaceEnum) do
      begin
        if (cs = Colorspace) or not ColorTypeDefined[cs] then Continue;
        if ColorspaceInfo[Colorspace].BasicHelper and (ColorspaceInfo[cs].BasicHelper or (cs = csColor)) then continue;

        n := ColorspaceInfo[cs].Name;
        nt := 'T' + n;
        b := NeedXYZReferenceWhite(cs,Colorspace);
        h := GetProcedure('From' + n, 'AValue: ' + nt, b);
        Add('  ' + h);
        h := GetProcedure(HelperName + '.From' + n, 'AValue: ' + nt, b);
        AddProcedureImp(h, GetFromConvertProcedureImp(cs, ''));
        if b then
        begin
          h := GetProcedure('From' + n, 'AValue: ' + nt + '; ' + 'const AReferenceWhite: TXYZReferenceWhite', b);
          Add('  ' + h);
          h := GetProcedure(HelperName + '.From' + n, 'AValue: ' + nt + '; ' + 'const AReferenceWhite: TXYZReferenceWhite', b);
          AddProcedureImp(h, GetFromConvertProcedureImp(cs, 'AReferenceWhite'));
        end;
      end;

      if Colorspace = csColor then
      begin
        Add('  property red: byte read GetRed write SetRed;');
        Add('  property green: byte read GetGreen write SetGreen;');
        Add('  property blue: byte read GetBlue write SetBlue;');
      end;
    end;

    Add('end;');
    Add('');
  end;

  procedure MakeHelpers;
  var
    cs: TColorspaceEnum;
  begin
    for cs := Low(TColorspaceEnum) to High(TColorspaceEnum) do
      ColorTypeDefined[cs] := IsHelperOnly(cs);
    InfSpaceAdd := '  ';
    for cs := Low(TColorspaceEnum) to High(TColorspaceEnum) do
      if not IsHelperOnly(cs) then
        MakeHelper(cs, false, false);
    for cs := Low(TColorspaceEnum) to High(TColorspaceEnum) do
      MakeHelper(cs, false, true);
    for cs := Low(TColorspaceEnum) to High(TColorspaceEnum) do
      MakeHelper(cs, true, false);
    InfSpaceAdd := '';
  end;

  procedure MakeOperators;

    procedure AddOperator(c1, c2: TColorspaceEnum);
    var
      h, ls: string;
    begin
      h := 'operator := (const AValue: T' + ColorspaceInfo[c1].Name + '): T' + ColorspaceInfo[c2].Name + ';';
      ls := 'Result := ' + GetConversionFunctionRec(c1,c2,'AValue') + ';';
      Add(h);
      AddProcedureImp(h, ls);
    end;

  var
    i, j: TColorspaceEnum;
  begin
    AddImp('{Operators}');
    AddImp('');
    for i := Low(TColorspaceEnum) to High(TColorspaceEnum) do
      for j := Low(TColorspaceEnum) to High(TColorspaceEnum) do
        if (i <> j) and not ([i,j] <= [csHSLAPixel,csGSBAPixel]) and
        not ((ColorspaceInfo[i].BasicHelper or (i = csColor)) and (ColorspaceInfo[j].BasicHelper or (j = csColor))) then
          AddOperator(i, j);
  end;

  procedure RegisterColorspaces;
  var
    i,j: TColorspaceEnum;
  begin
    for i := Low(TColorspaceEnum) to High(TColorspaceEnum) do
      AddImp('  ColorspaceCollection.Add(T' + ColorspaceInfo[i].Name +'Colorspace);');

    for i := Low(TColorspaceEnum) to High(TColorspaceEnum) do
      for j := Low(TColorspaceEnum) to High(TColorspaceEnum) do
        if (i <> j) and (ConvMatrix[i,j]) then
          AddImp('  ColorspaceCollection.AddConversion(T' + ColorspaceInfo[i].Name +'Colorspace, T' + ColorspaceInfo[j].Name +'Colorspace,'
                      +' @Convert' + ColorspaceInfo[i].Name +'ArrayTo' + ColorspaceInfo[j].Name +'Array);');
  end;

begin
  writeln('Generating colorspaces...');
  SetLength(PairsList, 0);

  //direct conversions (using single predefined function)
  //TExpandedPixel is the first bridge between colorspaces

  AddColorPair(csBGRAPixel, csColor, 'ColorToBGRA', 'BGRAToColor');
  AddColorPair(csBGRAPixel, csExpandedPixel, 'GammaCompression', 'GammaExpansion');

  AddColorPair(csFPColor, csBGRAPixel, 'BGRAToFPColor', 'FPColorToBGRA');
  AddColorPair(csFPColor, csExpandedPixel, 'ExpandedToFPColor', 'FPColorToExpanded', true, 2);

  {AddColorPair(csHSLAPixel, csBGRAPixel, 'BGRAToHSLA', 'HSLAToBGRA', true, 2);
  AddColorPair(csGSBAPixel, csBGRAPixel, 'BGRAToGSBA', 'GSBAToBGRA', true, 2);}
  AddColorPair(csHSLAPixel, csGSBAPixel, 'GSBAToHSLA', 'HSLAToGSBA');
  AddColorPair(csHSLAPixel, csExpandedPixel, 'ExpandedToHSLA', 'HSLAToExpanded');
  AddColorPair(csGSBAPixel, csExpandedPixel, 'ExpandedToGSBA', 'GSBAToExpanded');

  AddColorPair(csStdRGBA, csBGRAPixel);
  AddColorPair(csStdHSLA, csStdRGBA);
  AddColorPair(csStdHSVA, csStdRGBA);
  AddColorPair(csStdHSLA, csStdHSVA);
  AddColorPair(csStdCMYKA, csStdRGBA);
  AddColorPair(csStdRGBA, csExpandedPixel, '','',true, 2);

 { AddColorPair(csWordMask, csExpandedPixel, 'ExpandedToWordMask', 'WordMaskToExpanded');
  AddColorPair(csByteMask, csWordMask, 'MaskWordToByte', 'MaskByteToWord');}
  AddColorPair(csByteMask, csBGRAPixel, 'BGRAToMask', 'MaskToBGRA', true, 3);
  AddColorPair(csByteMask, csExpandedPixel, 'ExpandedPixelToByteMask', 'ByteMaskToExpandedPixel', true, 2);

  //the other bridge is TXYZA
  //TLinearRGBA is between TExpandedPixel and TXYZA
  //there two paths to linear RGBA
  AddColorPair(csExpandedPixel, csLinearRGBA);
  //AddColorPair(csStdRGBA,       csLinearRGBA, '','',true, 2);

  AddColorPair(csExpandedPixel, csWordXYZA);
  AddColorPair(csXYZA, csWordXYZA);

  AddColorPair(csXYZA, csLinearRGBA);
  AddColorPair(csLabA, csXYZA, '','',true, 2);
  AddColorPair(csLabA, csLChA);
  AddColorPair(csAdobeRGBA, csXYZA);

  //Add pairs for color spaces with and without alpha support
  AddAlphaPairs;

  //Make all possible paths
  MakePathMatrix;

  //Write unit
  intsl := TStringList.Create;
  impsl := TStringList.Create;
  Add('{ This file is generated by dev/colorspace/UnitMaker program }');
  Add('');
  Add('{$IFDEF INCLUDE_INTERFACE}');
  Add('{$UNDEF INCLUDE_INTERFACE}');
  Add('type');
  Add('');
  AddImp('{$IFDEF INCLUDE_IMPLEMENTATION}');
  AddImp('{$UNDEF INCLUDE_IMPLEMENTATION}');
  AddImp('');
  MakeConverters;
  MakeHelpers;
  MakeOperators;
  Add('{$ENDIF}');
  AddImp('{$ENDIF}');

  AddImp('{$IFDEF INCLUDE_INITIALIZATION}');
  AddImp('{$UNDEF INCLUDE_INITIALIZATION}');
  RegisterColorspaces;
  AddImp('{$ENDIF}');
  //Save
  intsl.AddStrings(impsl);
  intsl.SaveToFile('generatedcolorspace.inc');
  intsl.Free;
  impsl.Free;
  WriteLn('Done generating colorspaces.');
end;

end.
