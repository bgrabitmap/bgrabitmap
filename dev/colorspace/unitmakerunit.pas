unit UnitMakerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, rttiutils, BGRABitmapTypes;

type
  TColorspaceEnum = (csColor, csBGRAPixel, csStdRGBA, //sRGB
    csAdobeRGBA,
    csStdHSLA, csStdHSVA, csStdCMYKA,            //based on sRGB
    csExpandedPixel, csLinearRGBA,               //linear RGB
    csHSLAPixel, csGSBAPixel, csXYZA,            //based on linear RGB
    csLabA, csLChA);                             //based on XYZ

  TChannelValueType = (cvtByte, cvtWord, cvtLongWord, cvtSingle, cvtDouble);

const
  ChannelValueTypeName : array[TChannelValueType] of string = ('byte', 'word', 'longword', 'single', 'double');
  MAXWORD = $ffff;

type
  TColorspaceInfo = record
    Name: string;
    Declaration: string;
    Colorspace: string;
    HasAlpha, HasWhiteRef: boolean;
    ValueType: TChannelValueType;
    BasicHelper: boolean;
    VariableNames, FullNames, MinMax: string;
  end;

const
  ColorspaceInfo: array [TColorspaceEnum] of TColorspaceInfo =
  ((Name: 'Color';         Declaration: 'type helper';   Colorspace: 'StdRGB';      HasAlpha: false;  HasWhiteRef: false;  ValueType: cvtByte;    BasicHelper: false;
   VariableNames: 'red,green,blue';                      FullNames: 'Red,Green,Blue';                 MinMax: '0,0,0,255,255,255'),
   (Name: 'BGRAPixel';     Declaration: 'record helper'; Colorspace: 'StdRGB';      HasAlpha: true;   HasWhiteRef: false;  ValueType: cvtByte;    BasicHelper: true;
   VariableNames: 'red,green,blue,alpha';                FullNames: 'Red,Green,Blue,Alpha';           MinMax: '0,0,0,0,255,255,255,255'),
   (Name: 'StdRGBA';       Declaration: 'packed record'; Colorspace: 'StdRGB';      HasAlpha: true;   HasWhiteRef: false;  ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'red,green,blue,alpha';                FullNames: 'Red,Green,Blue,Alpha';           MinMax: '0,0,0,0,1,1,1,1'),
   (Name: 'AdobeRGBA';     Declaration: 'packed record'; Colorspace: 'AdobeRGB';    HasAlpha: true;   HasWhiteRef: false;  ValueType: cvtByte;    BasicHelper: false;
   VariableNames: 'red,green,blue,alpha';                FullNames: 'Red,Green,Blue,Alpha';           MinMax: '0,0,0,0,255,255,255,255'),

   (Name: 'StdHSLA';       Declaration: 'packed record'; Colorspace: 'StdHSL';      HasAlpha: true;   HasWhiteRef: false;  ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'hue,saturation,lightness,alpha';      FullNames: 'Hue,Saturation,Lightness,Alpha'; MinMax: '0,0,0,0,360,1,1,1'),
   (Name: 'StdHSVA';       Declaration: 'packed record'; Colorspace: 'StdHSV';      HasAlpha: true;   HasWhiteRef: false;  ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'hue,saturation,value,alpha';          FullNames: 'Hue,Saturation,Value,Alpha';     MinMax: '0,0,0,0,360,1,1,1'),
   (Name: 'StdCMYK';       Declaration: 'packed record'; Colorspace: 'StdCMYK';     HasAlpha: false;  HasWhiteRef: false;  ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'C,M,Y,K';                             FullNames: 'Cyan,Magenta,Yellow,Black';      MinMax: '0,0,0,0,1,1,1,1'),

   (Name: 'ExpandedPixel'; Declaration: 'record helper'; Colorspace: 'LinearRGB';   HasAlpha: true;   HasWhiteRef: false;  ValueType: cvtWord;    BasicHelper: true;
   VariableNames: 'red,green,blue,alpha';                FullNames: 'Red,Green,Blue,Alpha';           MinMax: '0,0,0,0,65535,65535,65535,65535'),
   (Name: 'LinearRGBA';    Declaration: 'packed record'; Colorspace: 'LinearRGB';   HasAlpha: true;   HasWhiteRef: false;  ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'red,green,blue,alpha';                FullNames: 'Red,Green,Blue,Alpha';           MinMax: '0,0,0,0,1,1,1,1'),

   (Name: 'HSLAPixel';     Declaration: 'record helper'; Colorspace: 'HSL';         HasAlpha: true;   HasWhiteRef: false;  ValueType: cvtWord;    BasicHelper: true;
   VariableNames: 'hue,saturation,lightness,alpha';      FullNames: 'Hue,Saturation,Lightness,Alpha'; MinMax: '0,0,0,0,65535,65535,65535,65535'),
   (Name: 'GSBAPixel';     Declaration: 'record helper'; Colorspace: 'GSB';         HasAlpha: true;   HasWhiteRef: false;  ValueType: cvtWord;    BasicHelper: true;
   VariableNames: 'hue,saturation,lightness,alpha';      FullNames: 'Hue,Saturation,Brightness,Alpha';MinMax: '0,0,0,0,65535,65535,65535,65535'),
   (Name: 'XYZA';          Declaration: 'packed record'; Colorspace: 'CIE XYZ';     HasAlpha: true;   HasWhiteRef: false;  ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'X,Y,Z,alpha';                         FullNames: 'X,Y,Z,Alpha';                    MinMax: '0,0,0,0,1,1,1,1'),

   (Name: 'LabA';          Declaration: 'packed record'; Colorspace: 'CIE Lab';     HasAlpha: true;   HasWhiteRef: true;   ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'L,a,b,alpha';                         FullNames: 'Lightness,a,b,Alpha';            MinMax: '0,-128,-128,0,100,127,127,1'),
   (Name: 'LChA';          Declaration: 'packed record'; Colorspace: 'CIE LCh';     HasAlpha: true;   HasWhiteRef: true;   ValueType: cvtSingle;  BasicHelper: false;
   VariableNames: 'L,C,h,alpha';                         FullNames: 'Lightness,Chroma,Hue,Alpha';     MinMax: '0,0,0,0,100,180,360,1') );

type
  TColorPair = record
    First, Last: TColorspaceEnum;
    ToFirstFunc, ToLastFunc: string;
    HandlesExtraAlpha: boolean;
  end;

type
  TColorspaceArray = array of TColorspaceEnum;

var
  PairsList: array of TColorPair;
  PathMatrix: packed array[TColorspaceEnum, TColorspaceEnum] of Word;

function FindPath(AFrom, ATo: TColorspaceEnum): TColorspaceArray;
function NewColorPair(AFirst, ALast: TColorspaceEnum; AToFirstFunc, AToLastFunc: string; AHandlesExtraAlpha: boolean): TColorPair;
procedure AddColorPair(AFirst, ALast: TColorspaceEnum; AToFirstFunc : string = ''; AToLastFunc: string = ''; AHandlesExtraAlpha: boolean = true);
function GetConversionFunction(AFrom, ATo: TColorspaceEnum): string;
procedure AddAlphaPairs;
procedure GenerateCode;

implementation

function IsHelperOnly(cs: TColorspaceEnum): boolean;
begin
  result := ColorspaceInfo[cs].Declaration.EndsWith(' helper');
end;

procedure AddColorPair(AFirst, ALast: TColorspaceEnum; AToFirstFunc, AToLastFunc: string; AHandlesExtraAlpha: boolean);
begin
  SetLength(PairsList, Length(PairsList) + 1);
  if AToFirstFunc = '' then AToFirstFunc:= ColorspaceInfo[ALast].Name + 'To' + ColorspaceInfo[AFirst].Name;
  if AToLastFunc = '' then AToLastFunc:= ColorspaceInfo[AFirst].Name + 'To' + ColorspaceInfo[ALast].Name;
  PairsList[Length(PairsList) - 1] := NewColorPair(AFirst, ALast, AToFirstFunc, AToLastFunc, AHandlesExtraAlpha);
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
  AHandlesExtraAlpha := true;
end;

function GetConversionFunction(AFrom, ATo: TColorspaceEnum): string;
var AHandlesExtraAlpha: boolean;
begin
  result := GetConversionFunction(AFrom,ATo,AHandlesExtraAlpha);
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

function FindPath(AFrom, ATo: TColorspaceEnum): TColorspaceArray;
var
  curLen: Word;
  cs: TColorspaceEnum;
  i: Integer;
  found: boolean;
begin
  result := nil;
  curLen := PathMatrix[AFrom,ATo];
  if curLen = MAXWORD then
    raise exception.Create('No path found');

  setlength(result, curLen+1);
  result[curLen] := ATo;
  while curLen > 0 do
  begin
    found := false;
    for cs := low(TColorspaceEnum) to high(TColorspaceEnum) do
      if PathMatrix[AFrom,cs] = curLen-1 then
      begin
        for i := 0 to high(PairsList) do
        if ((PairsList[i].First = cs) and (PairsList[i].Last = result[curLen])) or
          ((PairsList[i].Last = cs) and (PairsList[i].First = result[curLen]))  then
        begin
          dec(curLen);
          result[curLen] := cs;
          found := true;
          break;
        end;
      end;
    if not found then
      raise exception.Create('Cannot find link');
  end;
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

function NewColorPair(AFirst, ALast: TColorspaceEnum; AToFirstFunc, AToLastFunc: string; AHandlesExtraAlpha: boolean): TColorPair;
begin
  with Result do
  begin
    First := AFirst;
    Last := ALast;
    ToFirstFunc:= AToFirstFunc;
    ToLastFunc:= AToLastFunc;
    HandlesExtraAlpha:= AHandlesExtraAlpha;
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

  function GetProcedure(pn, ls: string; ov: boolean): string;
  begin
    Result := 'procedure ' + pn;
    if ls <> '' then
      Result += '(' + ls + ')';
    Result += ';';
    if ov then
      Result += ' overload;';
  end;

  function GetFunction(pn, ls, res: string; ov: boolean; st: boolean = False): string;
  begin
    Result := 'function ' + pn;
    if ls <> '' then
      Result += '(' + ls + ')';
    Result += ': ' + res + ';';
    if ov then
      Result += 'overload;';
    if st then
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

    function NeedXYZReferenceWhite(cs1, cs2: TColorspaceEnum): boolean;
    begin
      result := ColorspaceInfo[cs1].HasWhiteRef xor ColorspaceInfo[cs2].HasWhiteRef;
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
        ls := ls + #13#10 + '  ' + 'Result' + avn + ' := AAlpha;';
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

    function GetConvertProcedureImp(cpto: TColorspaceEnum; ad: string): string;
    begin
      Result := 'Result := ' + GetConversionFunction(ColorSpace, cpto) + '(Self' + ad + ');';
    end;

    function GetFromConvertProcedureImp(cpfrom: TColorspaceEnum; ad: string): string;
    begin
      Result := 'Self := ' + GetConversionFunction(cpfrom, ColorSpace) + '(AValue' + ad + ');';
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

    function NeedXYZReferenceWhite(cs: TColorspaceEnum): boolean;
    begin
      result := ColorspaceInfo[cs].HasWhiteRef xor ColorspaceInfo[Colorspace].HasWhiteRef;
    end;

  var
    ov, ba: boolean;
    vsam, vsfm, body, vn2: TStringArray;
    cn: integer;
    typeDeclaration, fn: string;
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
      Add('  class function GetMaxValue(AIndex: integer): single; override;');
      Add('  class function GetMinValue(AIndex: integer): single; override;');
      Add('  class function GetName: string; override;');
      Add('  class function GetSize: integer; override;');
      Add('  class function GetChannel(AColor: Pointer; AIndex: integer): single; override;');
      Add('  class procedure SetChannel(AColor: Pointer; AIndex: integer; AValue: single); override;');
      Add('  class function HasReferenceWhite: boolean; override;');
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

      AddProcedureImp('class function '+ColorTypeName+'Colorspace.HasReferenceWhite: boolean;',
                      'result := ' + BoolToStr(ColorspaceInfo[Colorspace].HasWhiteRef, true) + ';');

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

    Add('{' + HelperName + '}');
    Add('');
    Add(HelperName + ' = ' + typeDeclaration);

    AddImp('{' + HelperName + '}');
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
        AddProcedureImp(h, 'result := self and $ff;');
        Add('  ' + StringReplace(h, HelperName+'.', '', []));
        h := GetFunction(HelperName+'.GetGreen', '', 'byte', false);
        AddProcedureImp(h, 'result := (self shr 8) and $ff;');
        Add('  ' + StringReplace(h, HelperName+'.', '', []));
        h := GetFunction(HelperName+'.GetBlue', '', 'byte', false);
        AddProcedureImp(h, 'result := (self shr 16) and $ff;');
        Add('  ' + StringReplace(h, HelperName+'.', '', []));

        h := GetProcedure(HelperName+'.SetRed', 'AValue: byte', false);
        AddProcedureImp(h, 'self := (self and $ffff00) or AValue;');
        Add('  ' + StringReplace(h, HelperName+'.', '', []));
        h := GetProcedure(HelperName+'.SetGreen', 'AValue: byte', false);
        AddProcedureImp(h, 'self := (self and $ff00ff) or (AValue shl 8);');
        Add('  ' + StringReplace(h, HelperName+'.', '', []));
        h := GetProcedure(HelperName+'.SetBlue', 'AValue: byte', false);
        AddProcedureImp(h, 'self := (self and $00ffff) or (AValue shl 16);');
        Add('  ' + StringReplace(h, HelperName+'.', '', []));
        add('public');
      end;

      for cs := Low(TColorspaceEnum) to High(TColorspaceEnum) do
      begin
        if (cs = Colorspace) or not ColorTypeDefined[cs] then Continue;
        if ColorspaceInfo[Colorspace].BasicHelper and (ColorspaceInfo[cs].BasicHelper or (cs = csColor)) then continue;

        n := ColorspaceInfo[cs].Name;
        b := NeedXYZReferenceWhite(cs);
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
          fn := GetConversionFunction(Colorspace, cs, handlesExtraAlpha);
          if handlesExtraAlpha then
            AddProcedureImp(h, 'result := '+fn+'(self, AAlpha);')
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
          AddProcedureImp(h, GetConvertProcedureImp(cs, ',AReferenceWhite'));
        end;
      end;

      for cs := Low(TColorspaceEnum) to High(TColorspaceEnum) do
      begin
        if (cs = Colorspace) or not ColorTypeDefined[cs] then Continue;
        if ColorspaceInfo[Colorspace].BasicHelper and (ColorspaceInfo[cs].BasicHelper or (cs = csColor)) then continue;

        n := ColorspaceInfo[cs].Name;
        nt := 'T' + n;
        b := NeedXYZReferenceWhite(cs);
        h := GetProcedure('From' + n, 'AValue: ' + nt, b);
        Add('  ' + h);
        h := GetProcedure(HelperName + '.From' + n, 'AValue: ' + nt, b);
        AddProcedureImp(h, GetFromConvertProcedureImp(cs, ''));
        if b then
        begin
          h := GetProcedure('From' + n, 'AValue: ' + nt + '; ' + 'const AReferenceWhite: TXYZReferenceWhite', b);
          Add('  ' + h);
          h := GetProcedure(HelperName + '.From' + n, 'AValue: ' + nt + '; ' + 'const AReferenceWhite: TXYZReferenceWhite', b);
          AddProcedureImp(h, GetFromConvertProcedureImp(cs, ',AReferenceWhite'));
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
      ls := 'Result := ' + GetConversionFunction(c1,c2) + '(AValue);';
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
        if i <> j then
          AddImp('  ColorspaceCollection.AddConversion(T' + ColorspaceInfo[i].Name +'Colorspace, T' + ColorspaceInfo[j].Name +'Colorspace,'
                      +' @Convert' + ColorspaceInfo[i].Name +'ArrayTo' + ColorspaceInfo[j].Name +'Array);');
  end;

begin
  SetLength(PairsList, 0);
  //Direct conversions (using single predefined function)
  AddColorPair(csBGRAPixel, csColor, 'ColorToBGRA', 'BGRAToColor');
  AddColorPair(csStdRGBA, csBGRAPixel);
  AddColorPair(csStdHSLA, csStdRGBA);
  AddColorPair(csStdHSVA, csStdRGBA);
  AddColorPair(csStdHSLA, csStdHSVA);
  AddColorPair(csStdCMYKA, csStdRGBA);

  AddColorPair(csExpandedPixel, csBGRAPixel, 'GammaExpansion', 'GammaCompression');
  AddColorPair(csLinearRGBA, csStdRGBA);
  AddColorPair(csLinearRGBA, csExpandedPixel);
  AddColorPair(csHSLAPixel, csBGRAPixel, 'BGRAToHSLA', 'HSLAToBGRA');
  AddColorPair(csGSBAPixel, csBGRAPixel, 'BGRAToGSBA', 'GSBAToBGRA');
  AddColorPair(csHSLAPixel, csExpandedPixel, 'ExpandedToHSLA', 'HSLAToExpanded');
  AddColorPair(csGSBAPixel, csExpandedPixel, 'ExpandedToGSBA', 'GSBAToExpanded');
  AddColorPair(csHSLAPixel, csGSBAPixel, 'GSBAToHSLA', 'HSLAToGSBA');
  AddColorPair(csXYZA, csLinearRGBA);
  AddColorPair(csLabA, csXYZA);
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
  WriteLn('Finish.');
end;

end.
