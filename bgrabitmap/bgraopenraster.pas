// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAOpenRaster;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRALayers, zipper, DOM, BGRABitmap, BGRALayerOriginal,
  BGRASVGShapes, FPImage, BGRASVG;

const
  OpenRasterMimeType = 'image/openraster'; //do not change, it's part of the file format
  OpenRasterSVGDefaultDPI = 90;

type

  { TBGRAOpenRasterDocument }

  TBGRAOpenRasterDocument = class(TBGRALayeredBitmap)
  private
    FFiles: array of record
      Filename: string;
      Stream: TMemoryStream;
    end;
    FStackXML: TXMLDocument;
    FZipInputStream: TStream;
    procedure SetMimeType(AValue: string);
  protected
    Procedure ZipOnCreateStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
    Procedure ZipOnDoneStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
    Procedure ZipOnOpenInputStream(Sender : TObject; var AStream : TStream);
    Procedure ZipOnCloseInputStream(Sender : TObject; var AStream : TStream);
    procedure ClearFiles;
    function GetMemoryStream(AFilename: string): TMemoryStream;
    procedure SetMemoryStream(AFilename: string; AStream: TMemoryStream);
    function AddLayerFromMemoryStream(ALayerFilename: string): integer;
    function CopyRasterLayerToMemoryStream(ALayerIndex: integer; ALayerFilename: string): boolean;
    procedure CopySVGToMemoryStream(ASVG: TBGRASVG; ASVGMatrix: TAffineMatrix; AOutFilename: string; out AOffset: TPoint);
    function CopyBitmapToMemoryStream(ABitmap: TBGRABitmap; AFilename: string): boolean;
    procedure SetMemoryStreamAsString(AFilename: string; AContent: string);
    function GetMemoryStreamAsString(AFilename: string): string;
    procedure UnzipFromStream(AStream: TStream; AFileList: TStrings = nil);
    procedure UnzipFromFile(AFilenameUTF8: string);
    procedure ZipToFile(AFilenameUTF8: string);
    procedure ZipToStream(AStream: TStream);
    procedure CopyThumbnailToMemoryStream(AMaxWidth, AMaxHeight: integer);
    procedure AnalyzeZip; virtual;
    procedure PrepareZipToSave; virtual;
    function GetMimeType: string; override;
    procedure InternalLoadFromStream(AStream: TStream);
    procedure InternalSaveToStream(AStream: TStream);

  public
    constructor Create; overload; override;
    constructor Create(AWidth, AHeight: integer); overload; override;
    procedure Clear; override;
    function CheckMimeType(AStream: TStream): boolean;
    procedure LoadFlatImageFromStream(AStream: TStream;
              out ANbLayers: integer;
              out ABitmap: TBGRABitmap);
    procedure LoadFromStream(AStream: TStream); override;
    procedure LoadFromFile(const filenameUTF8: string); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure SaveToFile(const filenameUTF8: string); override;
    property MimeType : string read GetMimeType write SetMimeType;
    property StackXML : TXMLDocument read FStackXML;
  end;

  { TFPReaderOpenRaster }

  TFPReaderOpenRaster = class(TFPCustomImageReader)
    private
      FWidth,FHeight,FNbLayers: integer;
    protected
      function InternalCheck(Stream: TStream): boolean; override;
      procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    public
      property Width: integer read FWidth;
      property Height: integer read FHeight;
      property NbLayers: integer read FNbLayers;
  end;

  { TFPWriterOpenRaster }

  TFPWriterOpenRaster = class(TFPCustomImageWriter)
    protected
      procedure InternalWrite (Str:TStream; Img:TFPCustomImage); override;
  end;

procedure RegisterOpenRasterFormat;

implementation

uses XMLRead, XMLWrite, BGRABitmapTypes, zstream, BGRAUTF8,
  UnzipperExt, BGRASVGOriginal, BGRATransform, BGRASVGType, math;

const
  MergedImageFilename = 'mergedimage.png';
  LayerStackFilename = 'stack.xml';

function IsZipStream(stream: TStream): boolean;
var
  header:  packed array[0..1] of char;
  SavePos: int64;
begin
  Result := False;
  try
    if stream.Position + 2 < Stream.Size then
    begin
      header  := #0#0;
      SavePos := stream.Position;
      stream.Read(header, 2);
      stream.Position := SavePos;
      if (header[0] = 'P') and (header[1] = 'K') then
        Result := True;
    end;
  except
    on ex: Exception do ;
  end;
end;

{ TFPWriterOpenRaster }

procedure TFPWriterOpenRaster.InternalWrite(Str: TStream; Img: TFPCustomImage);
var doc: TBGRAOpenRasterDocument;
  tempBmp: TBGRABitmap;
  x,y: integer;

begin
  doc := TBGRAOpenRasterDocument.Create;
  if Img is TBGRABitmap then doc.AddLayer(Img as TBGRABitmap) else
  begin
    tempBmp := TBGRABitmap.Create(img.Width,img.Height);
    for y := 0 to Img.Height-1 do
      for x := 0 to img.Width-1 do
        tempBmp.SetPixel(x,y, FPColorToBGRA(img.Colors[x,y]));
    doc.AddOwnedLayer(tempBmp);
  end;
  doc.SaveToStream(Str);
  doc.Free;
end;

{ TFPReaderOpenRaster }

function TFPReaderOpenRaster.InternalCheck(Stream: TStream): boolean;
var magic: packed array[0..3] of byte;
  OldPos,BytesRead: Int64;
  doc : TBGRAOpenRasterDocument;
begin
  Result:=false;
  if Stream=nil then exit;
  oldPos := stream.Position;
  {$PUSH}{$HINTS OFF}
  BytesRead := Stream.Read({%H-}magic,sizeof(magic));
  {$POP}
  stream.Position:= OldPos;
  if BytesRead<>sizeof(magic) then exit;
  if (magic[0] = $50) and (magic[1] = $4b) and (magic[2] = $03) and (magic[3] = $04) then
  begin
    doc := TBGRAOpenRasterDocument.Create;
    result := doc.CheckMimeType(Stream);
    doc.Free;
  end;
end;

procedure TFPReaderOpenRaster.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  layeredImage: TBGRAOpenRasterDocument;
  flat: TBGRABitmap;
  x,y: integer;
begin
  FWidth := 0;
  FHeight:= 0;
  FNbLayers:= 0;
  layeredImage := TBGRAOpenRasterDocument.Create;
  try
    layeredImage.LoadFlatImageFromStream(Stream, FNbLayers, flat);
    if Assigned(flat) then
    begin
      FWidth := flat.Width;
      FHeight := flat.Height;
    end else
    begin
      layeredImage.LoadFromStream(Stream);
      flat := layeredImage.ComputeFlatImage;
      FWidth:= layeredImage.Width;
      FHeight:= layeredImage.Height;
      FNbLayers:= layeredImage.NbLayers;
    end;
    try
      if Img is TBGRACustomBitmap then
        TBGRACustomBitmap(img).Assign(flat)
      else
      begin
        Img.SetSize(flat.Width,flat.Height);
        for y := 0 to flat.Height-1 do
          for x := 0 to flat.Width-1 do
            Img.Colors[x,y] := BGRAToFPColor(flat.GetPixel(x,y));
      end;
    finally
      flat.free;
    end;
    FreeAndNil(layeredImage);
  except
    on ex: Exception do
    begin
      layeredImage.Free;
      raise Exception.Create('Error while loading OpenRaster file. ' + ex.Message);
    end;
  end;
end;

{ TBGRAOpenRasterDocument }

procedure TBGRAOpenRasterDocument.AnalyzeZip;

  function CountLayersRec(stackNode: TDOMNode): integer;
  var i: integer;
    layerNode: TDOMNode;
  begin
    result := 0;
    for i := stackNode.ChildNodes.Length-1 downto 0 do
    begin
      layerNode:= stackNode.ChildNodes[i];
      if (layerNode.NodeName = 'layer') and Assigned(layerNode.Attributes) then
        inc(result) else
      if (layerNode.NodeName = 'stack') then
        inc(result, CountLayersRec(layerNode));
    end;
  end;

var
  totalLayerCount, doneLayerCount: integer;

  procedure AddLayersRec(stackNode: TDOMNode);
  var i,j : integer;
    layerNode, attr: TDOMNode;
    idx,x,y: integer;
    float: double;
    errPos: integer;
    opstr : string;
    gammastr: string;
  begin
    for i := stackNode.ChildNodes.Length-1 downto 0 do
    begin
      OnLayeredBitmapLoadProgress(doneLayerCount*100 div totalLayerCount);
      layerNode:= stackNode.ChildNodes[i];
      if layerNode.NodeName = 'stack' then
        AddLayersRec(layerNode) else
      if (layerNode.NodeName = 'layer') and Assigned(layerNode.Attributes) then
      begin
        attr := layerNode.Attributes.GetNamedItem('src');
        idx := AddLayerFromMemoryStream(UTF8Encode(attr.NodeValue));
        if idx <> -1 then
        begin
          x := 0;
          y := 0;
          gammastr := '';
          for j := 0 to layerNode.Attributes.Length-1 do
          begin
            attr := layerNode.Attributes[j];
            if lowercase(attr.NodeName) = 'opacity' then
            begin
              val(attr.NodeValue, float, errPos);
              if errPos = 0 then
              begin
                if float < 0 then float := 0;
                if float > 1 then float := 1;
                LayerOpacity[idx] := round(float*255);
              end;
            end else
            if lowercase(attr.NodeName) = 'gamma-correction' then
              gammastr := string(attr.NodeValue) else
            if lowercase(attr.NodeName) = 'visibility' then
              LayerVisible[idx] := (attr.NodeValue = 'visible') or (attr.NodeValue = 'yes') or (attr.NodeValue = '1') else
            if (lowercase(attr.NodeName) = 'x') or (lowercase(attr.NodeName) = 'y') then
            begin
              val(attr.NodeValue, float, errPos);
              if errPos = 0 then
              begin
                if float < -(MaxInt shr 1) then float := -(MaxInt shr 1);
                if float > (MaxInt shr 1) then float := (MaxInt shr 1);
                if (lowercase(attr.NodeName) = 'x') then x := round(float);
                if (lowercase(attr.NodeName) = 'y') then y := round(float);
              end;
            end else
            if lowercase(attr.NodeName) = 'name' then
              LayerName[idx] := UTF8Encode(attr.NodeValue) else
            if lowercase(attr.NodeName) = 'composite-op' then
            begin
              opstr := StringReplace(lowercase(string(attr.NodeValue)),'_','-',[rfReplaceAll]);
              if (pos(':',opstr) = 0) and (opstr <> 'xor') then opstr := 'svg:'+opstr;
              //parse composite op
              if (opstr = 'svg:src-over') or (opstr = 'krita:dissolve') then
                BlendOperation[idx] := boTransparent else
              if opstr = 'svg:lighten' then
                BlendOperation[idx] := boLighten else
              if opstr = 'svg:screen' then
                BlendOperation[idx] := boScreen else
              if opstr = 'svg:color-dodge' then
                BlendOperation[idx] := boColorDodge else
              if (opstr = 'svg:color-burn') or (opstr = 'krita:gamma_dark'){approx} then
                BlendOperation[idx] := boColorBurn else
              if opstr = 'svg:darken' then
                BlendOperation[idx] := boDarken else
              if (opstr = 'svg:plus') or (opstr = 'svg:add') or (opstr = 'krita:linear_dodge') then
                BlendOperation[idx] := boLinearAdd else
              if (opstr = 'svg:multiply') or (opstr = 'krita:bumpmap') then
                BlendOperation[idx] := boMultiply else
              if opstr = 'svg:overlay' then
                BlendOperation[idx] := boOverlay else
              if opstr = 'svg:soft-light' then
                BlendOperation[idx] := boSvgSoftLight else
              if opstr = 'svg:hard-light' then
                BlendOperation[idx] := boHardLight else
              if opstr = 'svg:difference' then
                BlendOperation[idx] := boLinearDifference else
              if (opstr = 'krita:inverse-subtract') or (opstr = 'krita:linear-burn') then
                BlendOperation[idx] := boLinearSubtractInverse else
              if opstr = 'krita:subtract' then
                BlendOperation[idx] := boLinearSubtract else
              if (opstr = 'svg:difference') or
                (opstr = 'krita:equivalence') then
                BlendOperation[idx] := boLinearDifference else
              if (opstr = 'svg:exclusion') or
                (opstr = 'krita:exclusion') then
                BlendOperation[idx] := boLinearExclusion else
              if opstr = 'krita:divide' then
                BlendOperation[idx] := boDivide else
              if opstr = 'bgra:soft-light' then
                BlendOperation[idx] := boSoftLight else
              if opstr = 'bgra:nice-glow' then
                BlendOperation[idx] := boNiceGlow else
              if opstr = 'bgra:glow' then
                BlendOperation[idx] := boGlow else
              if opstr = 'bgra:reflect' then
                BlendOperation[idx] := boReflect else
              if opstr = 'bgra:negation' then
                BlendOperation[idx] := boLinearNegation else
              if (opstr = 'bgra:xor') or (opstr = 'xor') then
                BlendOperation[idx] := boXor else
              if opstr = 'bgra:mask' then
                BlendOperation[idx] := boMask else
              if opstr = 'bgra:linear-multiply-saturation' then
                BlendOperation[idx] := boLinearMultiplySaturation else
              if opstr = 'svg:hue' then
                BlendOperation[idx] := boCorrectedHue else
              if opstr = 'svg:color' then
                BlendOperation[idx] := boCorrectedColor else
              if opstr = 'svg:luminosity' then
                BlendOperation[idx] := boCorrectedLightness else
              if opstr = 'svg:saturation' then
                BlendOperation[idx] := boCorrectedSaturation else
              if opstr = 'krita:hue-hsl' then
                BlendOperation[idx] := boLinearHue else
              if opstr = 'krita:color-hsl' then
                BlendOperation[idx] := boLinearColor else
              if opstr = 'krita:lightness' then
                BlendOperation[idx] := boLinearLightness else
              if opstr = 'krita:saturation-hsl' then
                BlendOperation[idx] := boLinearSaturation else
              begin
                //messagedlg('Unknown blend operation : ' + attr.NodeValue,mtInformation,[mbOk],0);
                BlendOperation[idx] := boTransparent;
              end;
            end;
          end;
          if LayerOriginalGuid[idx] <> GUID_NULL then
          begin
            LayerOriginalMatrix[idx] := AffineMatrixTranslation(x,y)*LayerOriginalMatrix[idx];
            RenderLayerFromOriginal(idx);
          end else LayerOffset[idx] := point(x,y);
          if (gammastr = 'yes') or (gammastr = 'on') then
          begin
            case BlendOperation[idx] of
              boLinearAdd: BlendOperation[idx] := boAdditive;
              boOverlay: BlendOperation[idx] := boDarkOverlay;
              boLinearDifference: BlendOperation[idx] := boDifference;
              boLinearExclusion: BlendOperation[idx] := boExclusion;
              boLinearSubtract: BlendOperation[idx] := boSubtract;
              boLinearSubtractInverse: BlendOperation[idx] := boSubtractInverse;
              boLinearNegation: BlendOperation[idx] := boNegation;
            end;
          end else
          if (gammastr = 'no') or (gammastr = 'off') then
            if BlendOperation[idx] = boTransparent then
              BlendOperation[idx] := boLinearBlend; //explicit linear blending
        end;
        inc(doneLayerCount);
      end;
    end;
  end;

var StackStream: TMemoryStream;
  imageNode, stackNode, attr: TDOMNode;
  i,w,h: integer;

begin
  inherited Clear;

  if MimeType <> OpenRasterMimeType then
    raise Exception.Create('Invalid mime type');

  StackStream := GetMemoryStream(LayerStackFilename);
  if StackStream = nil then
    raise Exception.Create('Layer stack not found');

  ReadXMLFile(FStackXML, StackStream);

  imageNode := StackXML.FindNode('image');
  if imagenode = nil then
    raise Exception.Create('Image node not found');

  w := 0;
  h := 0;
  LinearBlend := true;

  if Assigned(imageNode.Attributes) then
    for i:=0 to imageNode.Attributes.Length-1 do
    begin
      attr := imagenode.Attributes[i];
      if lowercase(attr.NodeName) = 'w' then
        w := strToInt(string(attr.NodeValue)) else
      if lowercase(attr.NodeName) = 'h' then
        h := strToInt(string(attr.NodeValue)) else
      if lowercase(attr.NodeName) = 'gamma-correction' then
        linearBlend := (attr.NodeValue = 'no') or (attr.NodeValue = '0');
    end;

  SetSize(w,h);

  stackNode := imageNode.FindNode('stack');
  if stackNode = nil then
    raise Exception.Create('Stack node not found');

  totalLayerCount := CountLayersRec(stackNode);
  doneLayerCount := 0;
  AddLayersRec(stackNode);
end;

procedure TBGRAOpenRasterDocument.PrepareZipToSave;

var i: integer;
    imageNode,stackNode,layerNode: TDOMElement;
    layerFilename,strval: string;
    stackStream: TMemoryStream;
    ofs, wantedOfs: TPoint;
    fileAdded: Boolean;
    svg: TBGRASVG;
    m: TAffineMatrix;
begin
  ClearFiles;
  MimeType := OpenRasterMimeType;
  FStackXML := TXMLDocument.Create;
  imageNode := TDOMElement(StackXML.CreateElement('image'));
  StackXML.AppendChild(imageNode);
  imageNode.SetAttribute('w',widestring(inttostr(Width)));
  imageNode.SetAttribute('h',widestring(inttostr(Height)));
  if LinearBlend then
    imageNode.SetAttribute('gamma-correction','no')
  else
    imageNode.SetAttribute('gamma-correction','yes');

  stackNode := TDOMElement(StackXML.CreateElement('stack'));
  imageNode.AppendChild(stackNode);
  SetMemoryStreamAsString('stack.xml',''); //to put it before image data

  CopyThumbnailToMemoryStream(256,256);

  for i := NbLayers-1 downto 0 do
  begin
    OnLayeredBitmapSaveProgress(round((NbLayers-1-i) * 100 / NbLayers));
    if (LayerOriginalGuid[i] <> GUID_NULL) and LayerOriginalKnown[i] and
       LayerOriginalClass[i].CanConvertToSVG then
    begin
      layerFilename := 'data/layer'+inttostr(i)+'.svg';
      if LayerOriginal[i].IsInfiniteSurface then
      begin
        svg := LayerOriginal[i].ConvertToSVG(LayerOriginalMatrix[i], wantedOfs) as TBGRASVG;
        m := AffineMatrixTranslation(wantedOfs.X, wantedOfs.Y);
        svg.WidthAsPixel := self.Width;
        svg.HeightAsPixel := self.Height;
      end else
      begin
        svg := LayerOriginal[i].ConvertToSVG(AffineMatrixIdentity, wantedOfs) as TBGRASVG;
        m := LayerOriginalMatrix[i]
          * AffineMatrixTranslation(wantedOfs.X, wantedOfs.Y);
      end;
      try
        CopySVGToMemoryStream(svg, m, layerFilename, ofs);
        fileAdded := true;
      finally
        svg.Free;
      end;
    end else
    begin
      layerFilename := 'data/layer'+inttostr(i)+'.png';
      ofs := LayerOffset[i];
      fileAdded := CopyRasterLayerToMemoryStream(i, layerFilename);
    end;

    if fileAdded then
    begin
      layerNode := StackXML.CreateElement('layer');
      stackNode.AppendChild(layerNode);
      layerNode.SetAttribute('name', UTF8Decode(LayerName[i]));
      str(LayerOpacity[i]/255:0:3,strval);
      layerNode.SetAttribute('opacity',widestring(strval));
      layerNode.SetAttribute('src',widestring(layerFilename));
      if LayerVisible[i] then
        layerNode.SetAttribute('visibility','visible')
      else
        layerNode.SetAttribute('visibility','hidden');
      layerNode.SetAttribute('x',widestring(inttostr(ofs.x)));
      layerNode.SetAttribute('y',widestring(inttostr(ofs.y)));
      strval := '';
      case BlendOperation[i] of
        boLighten: strval := 'svg:lighten';
        boScreen: strval := 'svg:screen';
        boAdditive, boLinearAdd: strval := 'svg:add';
        boColorDodge: strval := 'svg:color-dodge';
        boColorBurn : strval := 'svg:color-burn';
        boDarken: strval := 'svg:darken';
        boMultiply: strval := 'svg:multiply';
        boOverlay, boDarkOverlay: strval := 'svg:overlay';
        boSoftLight: strval := 'bgra:soft-light';
        boHardLight: strval := 'svg:hard-light';
        boDifference,boLinearDifference: strval := 'svg:difference';
        boLinearSubtractInverse, boSubtractInverse: strval := 'krita:inverse_subtract';
        boLinearSubtract, boSubtract: strval := 'krita:subtract';
        boExclusion, boLinearExclusion: strval := 'svg:exclusion';
        boDivide: strval := 'krita:divide';
        boNiceGlow: strval := 'bgra:nice-glow';
        boGlow: strval := 'bgra:glow';
        boReflect: strval := 'bgra:reflect';
        boLinearNegation,boNegation: strval := 'bgra:negation';
        boXor: strval := 'bgra:xor';
        boSvgSoftLight: strval := 'svg:soft-light';
        boMask: strval := 'bgra:mask';
        boLinearMultiplySaturation: strval := 'bgra:linear-multiply-saturation';
        boCorrectedHue: strval := 'svg:hue';
        boCorrectedColor: strval := 'svg:color';
        boCorrectedLightness: strval := 'svg:luminosity';
        boCorrectedSaturation: strval := 'svg:saturation';
        boLinearHue: strval := 'krita:hue_hsl';
        boLinearColor: strval := 'krita:color_hsl';
        boLinearLightness: strval := 'krita:lightness';
        boLinearSaturation: strval := 'krita:saturation_hsl';
        else strval := 'svg:src-over';
      end;
      layerNode.SetAttribute('composite-op',widestring(strval));
      if BlendOperation[i] <> boTransparent then //in 'transparent' case, linear blending depends on general setting
      begin
        if BlendOperation[i] in[boAdditive,boDarkOverlay,boDifference,boSubtractInverse,
             boSubtract,boExclusion,boNegation] then
          strval := 'yes' else strval := 'no';
        layerNode.SetAttribute('gamma-correction',widestring(strval));
      end;
    end;
  end;
  OnLayeredBitmapSaveProgress(100);
  StackStream := TMemoryStream.Create;
  WriteXMLFile(StackXML, StackStream);
  SetMemoryStream('stack.xml',StackStream);
end;

procedure TBGRAOpenRasterDocument.LoadFromFile(const filenameUTF8: string);
var AStream: TFileStreamUTF8;
begin
  AStream := TFileStreamUTF8.Create(filenameUTF8,fmOpenRead or fmShareDenyWrite);
  OnLayeredBitmapLoadStart(filenameUTF8);
  try
    InternalLoadFromStream(AStream);
  finally
    OnLayeredBitmapLoaded;
    AStream.Free;
  end;
end;

procedure TBGRAOpenRasterDocument.SaveToStream(AStream: TStream);
begin
  OnLayeredBitmapSaveToStreamStart;
  try
    InternalSaveToStream(AStream);
  finally
    OnLayeredBitmapSaved;
  end;
end;

procedure TBGRAOpenRasterDocument.SaveToFile(const filenameUTF8: string);
begin
  OnLayeredBitmapSaveStart(filenameUTF8);
  try
    PrepareZipToSave;
    ZipToFile(filenameUTF8);
  finally
    OnLayeredBitmapSaved;
    ClearFiles;
  end;
end;

procedure TBGRAOpenRasterDocument.InternalSaveToStream(AStream: TStream);
begin
  try
    PrepareZipToSave;
    ZipToStream(AStream);
  finally
    ClearFiles;
  end;
end;

function TBGRAOpenRasterDocument.GetMimeType: string;
begin
  if length(FFiles)=0 then
    result := OpenRasterMimeType
   else
    result := GetMemoryStreamAsString('mimetype');
end;

procedure TBGRAOpenRasterDocument.InternalLoadFromStream(AStream: TStream);
begin
  try
    UnzipFromStream(AStream);
    AnalyzeZip;
  finally
    ClearFiles;
  end;
end;

constructor TBGRAOpenRasterDocument.Create;
begin
  inherited Create;
  RegisterOpenRasterFormat;
end;

constructor TBGRAOpenRasterDocument.Create(AWidth, AHeight: integer);
begin
  inherited Create(AWidth, AHeight);
  RegisterOpenRasterFormat;
end;

function TBGRAOpenRasterDocument.AddLayerFromMemoryStream(ALayerFilename: string): integer;
var stream: TMemoryStream;
  bmp: TBGRABitmap;
  orig: TBGRALayerSVGOriginal;
  svg: TBGRASVG;
  g: TSVGGroup;
  i, svgElemCount: Integer;
  origViewBox: TSVGViewBox;
  elemToMove: TList;
  m: TAffineMatrix;
begin
  stream := GetMemoryStream(ALayerFilename);
  if stream = nil then raise Exception.Create('Layer not found');

  if SuggestImageFormat(ALayerFilename) = ifSvg then
  begin
    svg := TBGRASVG.Create;
    svg.DefaultDpi:= OpenRasterSVGDefaultDPI;
    try
      svg.LoadFromStream(stream);
    except
      on ex:exception do
      begin
        svg.Free;
        raise exception.Create('SVG layer format error');
      end;
    end;
    g := nil;
    svgElemCount := 0;
    for i := 0 to svg.Content.ElementCount-1 do
      if svg.Content.IsSVGElement[i] then
      begin
        inc(svgElemCount);
        if svg.Content.ElementObject[i] is TSVGGroup then
          g := TSVGGroup(svg.Content.ElementObject[i]);
      end;

    if (svgElemCount = 1) and Assigned(g) and
       g.DOMElement.hasAttribute('bgra:originalViewBox') then
    begin
      svg.ContainerWidthAsPixel:= Width;
      svg.ContainerHeightAsPixel:= Height;
      origViewBox := TSVGViewBox.Parse(g.DOMElement.GetAttribute('bgra:originalViewBox'));
      m := svg.GetStretchPresentationMatrix(cuPixel) * g.matrix[cuPixel] *
        AffineMatrixTranslation(origViewBox.min.x, origViewBox.min.y);
      g.DOMElement.RemoveAttribute('bgra:originalViewBox');
      for i := svg.Content.ElementCount-1 downto 0 do
        if svg.Content.ElementObject[i] <> g then
          svg.Content.RemoveElement(svg.Content.ElementObject[i]);
      elemToMove := TList.Create;
      for i := 0 to g.Content.ElementCount-1 do
        elemToMove.Add(g.Content.ElementObject[i]);
      for i := 0 to elemToMove.Count-1 do
        svg.Content.BringElement(TObject(elemToMove[i]), g.Content);
      elemToMove.Free;
      svg.Content.RemoveElement(g);
      svg.ViewBox := origViewBox;
      svg.WidthAsPixel:= origViewBox.size.x;
      svg.HeightAsPixel:= origViewBox.size.y;
    end else
      m := AffineMatrixIdentity;
    orig := TBGRALayerSVGOriginal.Create;
    orig.SetSVG(svg, Width, Height);
    result := AddLayerFromOwnedOriginal(orig);
    LayerOriginalMatrix[result] := m;
  end else
  begin
    bmp := TBGRABitmap.Create;
    try
      bmp.LoadFromStream(stream);
    except
      on ex: exception do
      begin
        bmp.Free;
        raise exception.Create('Raster layer format error');
      end;
    end;
    result := AddOwnedLayer(bmp);
  end;
  LayerName[result] := ExtractFileName(ALayerFilename);
end;

function TBGRAOpenRasterDocument.CopyRasterLayerToMemoryStream(ALayerIndex: integer;
  ALayerFilename: string): boolean;
var
  bmp: TBGRABitmap;
  mustFreeBmp: boolean;
begin
  result := false;
  bmp := LayerBitmap[ALayerIndex];
  if bmp <> nil then mustFreeBmp := false
  else
  begin
    bmp := GetLayerBitmapCopy(ALayerIndex);
    if bmp = nil then exit;
    mustFreeBmp:= true;
  end;

  result := CopyBitmapToMemoryStream(bmp,ALayerFilename);
  if mustFreeBmp then bmp.Free;
end;

procedure TBGRAOpenRasterDocument.CopySVGToMemoryStream(
  ASVG: TBGRASVG; ASVGMatrix: TAffineMatrix; AOutFilename: string; out AOffset: TPoint);

  function IsIntegerTranslation(m: TAffineMatrix; out ofs: TPoint): boolean;
  begin
    ofs := Point(round(m[1,3]), round(m[2,3]));
    result := IsAffineMatrixTranslation(m) and
             (abs(round(m[1,3]) - ofs.x) < 1e-4) and
             (abs(round(m[2,3]) - ofs.y) < 1e-4);
  end;

  procedure StoreSVG(ASVG: TBGRASVG);
  var
    memStream: TMemoryStream;
    w, h: Single;
  begin
    memStream := TMemoryStream.Create;
    try
      w := ASVG.WidthAsPixel;
      h := ASVG.HeightAsPixel;
      //ensure we are not using units affected by DPI
      ASVG.ConvertToUnit(cuCustom);
      ASVG.WidthAsPixel := w;
      ASVG.HeightAsPixel := h;
      ASVG.SaveToStream(memStream);
      SetMemoryStream(AOutFilename,memstream);
    except
      on ex: Exception do
      begin
        memStream.Free;
        raise exception.Create(ex.Message);
      end;
    end;
  end;

  procedure StoreTransformedSVG(out AOffset: TPoint);
  var
    box, transfBox: TAffineBox;
    newSvg: TBGRASVG;
    newBounds: TRectF;
    rootElems: TList;
    i: Integer;
    g: TSVGGroup;
    newViewBox, origViewBox: TSVGViewBox;
    presentMatrix: TAffineMatrix;
  begin
    newSvg := ASVG.Duplicate;
    presentMatrix := ASVGMatrix * newSvg.GetStretchPresentationMatrix(cuPixel);
    rootElems := TList.Create;
    try
      origViewBox := newSvg.ViewBox;
      with origViewBox do
        box := TAffineBox.AffineBox(RectWithSizeF(min.x, min.y, size.x, size.y));
      transfBox := presentMatrix * box;
      newBounds := RectF(transfBox.RectBounds);
      AOffset := Point(round(newBounds.Left), round(newBounds.Top));
      newBounds.Offset(-AOffset.X, -AOffset.Y);
      presentMatrix := AffineMatrixTranslation(-AOffset.X, -AOffset.Y) * presentMatrix;
      for i := 0 to newSvg.Content.ElementCount-1 do
        rootElems.Add(newSvg.Content.ElementObject[i]);
      g := newSvg.Content.AppendGroup;
      for i := 0 to rootElems.Count-1 do
        g.Content.BringElement(TObject(rootElems[i]), newSvg.Content);
      g.matrix[cuPixel] := presentMatrix;
      g.DOMElement.SetAttribute('xmlns:bgra', 'https://wiki.freepascal.org/LazPaint_SVG_format');
      g.DOMElement.SetAttribute('bgra:originalViewBox', origViewBox.ToString);
      newSvg.WidthAsPixel:= newBounds.Width;
      newSvg.HeightAsPixel:= newBounds.Height;
      newViewBox.min := newBounds.TopLeft;
      newViewBox.size := PointF(newBounds.Width, newBounds.Height);
      newSvg.ViewBox := newViewBox;
      StoreSVG(newSvg);
    finally
      rootElems.Free;
      newSvg.Free;
    end;
  end;

begin
  if IsIntegerTranslation(ASVGMatrix, AOffset) then
    StoreSVG(ASVG)
    else StoreTransformedSVG(AOffset);
end;

function TBGRAOpenRasterDocument.CopyBitmapToMemoryStream(ABitmap: TBGRABitmap;
  AFilename: string): boolean;
var
  memStream: TMemoryStream;
begin
  result := false;
  memstream := TMemoryStream.Create;
  try
    ABitmap.SaveToStreamAsPng(memStream);
    SetMemoryStream(AFilename,memstream);
    result := true;
  except
    on ex: Exception do
    begin
      memStream.Free;
    end;
  end;
end;

procedure TBGRAOpenRasterDocument.SetMemoryStreamAsString(AFilename: string;
  AContent: string);
var strstream: TStringStream;
  memstream: TMemoryStream;
begin
  strstream:= TStringStream.Create(AContent);
  memstream := TMemoryStream.Create;
  strstream.Position := 0;
  memstream.CopyFrom(strstream, strstream.Size);
  strstream.Free;
  SetMemoryStream(AFilename, memstream);
end;

function TBGRAOpenRasterDocument.GetMemoryStreamAsString(AFilename: string): string;
var stream: TMemoryStream;
  str: TStringStream;
begin
  stream := GetMemoryStream(AFilename);
  str := TStringStream.Create('');
  str.CopyFrom(stream,stream.Size);
  result := str.DataString;
  str.Free;
end;

procedure TBGRAOpenRasterDocument.UnzipFromStream(AStream: TStream;
          AFileList: TStrings = nil);
var unzip: TUnZipper;
begin
  ClearFiles;
  unzip := TUnZipper.Create;
  try
    unzip.OnCreateStream := @ZipOnCreateStream;
    unzip.OnDoneStream := @ZipOnDoneStream;
    unzip.OnOpenInputStream := @ZipOnOpenInputStream;
    unzip.OnCloseInputStream := @ZipOnCloseInputStream;
    FZipInputStream := AStream;
    if Assigned(AFileList) then
    begin
      if AFileList.Count > 0 then
        unzip.UnZipFiles(AFileList);
    end else
      unzip.UnZipAllFiles;
  finally
    FZipInputStream := nil;
    unzip.Free;
  end;
end;

procedure TBGRAOpenRasterDocument.UnzipFromFile(AFilenameUTF8: string);
var unzip: TUnZipper;
begin
  ClearFiles;
  unzip := TUnZipper.Create;
  try
    unzip.FileName := Utf8ToAnsi(AFilenameUTF8);
    unzip.OnCreateStream := @ZipOnCreateStream;
    unzip.OnDoneStream := @ZipOnDoneStream;
    unzip.UnZipAllFiles;
  finally
    unzip.Free;
  end;
end;

procedure TBGRAOpenRasterDocument.ZipToFile(AFilenameUTF8: string);
var
  stream: TFileStreamUTF8;
begin
  stream := TFileStreamUTF8.Create(AFilenameUTF8, fmCreate);
  try
    ZipToStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TBGRAOpenRasterDocument.ZipToStream(AStream: TStream);
var zip: TZipper;
  i: integer;
  tempFile: String;
begin
  zip := TZipper.Create;
  tempFile := ChangeFileExt(GetTempFileName, '');
  if ExtractFileExt(tempFile) = '.tmp' then
    tempFile := ChangeFileExt(tempFile, '');
  zip.FileName:= tempFile;
  try
    for i := 0 to high(FFiles) do
    begin
      FFiles[i].Stream.Position:= 0;
      zip.Entries.AddFileEntry(FFiles[i].Stream,FFiles[i].Filename).CompressionLevel := clnone;
    end;
    zip.SaveToStream(AStream);
  finally
    zip.Free;
  end;
end;

procedure TBGRAOpenRasterDocument.CopyThumbnailToMemoryStream(AMaxWidth,AMaxHeight: integer);
var thumbnail: TBGRABitmap;
  w,h: integer;
begin
  if (Width = 0) or (Height = 0) then exit;
  thumbnail := ComputeFlatImage;
  CopyBitmapToMemoryStream(thumbnail,MergedImageFilename);
  if (thumbnail.Width > AMaxWidth) or
   (thumbnail.Height > AMaxHeight) then
  begin
    if thumbnail.Width > AMaxWidth then
    begin
      w := AMaxWidth;
      h := round(thumbnail.Height* (w/thumbnail.Width));
    end else
    begin
      w := thumbnail.Width;
      h := thumbnail.Height;
    end;
    if h > AMaxHeight then
    begin
      h := AMaxHeight;
      w := round(thumbnail.Width* (h/thumbnail.Height));
    end;
    BGRAReplace(thumbnail, thumbnail.Resample(w,h));
  end;
  CopyBitmapToMemoryStream(thumbnail,'Thumbnails/thumbnail.png');
  thumbnail.Free;
end;

procedure TBGRAOpenRasterDocument.Clear;
begin
  ClearFiles;
  inherited Clear;
end;

function TBGRAOpenRasterDocument.CheckMimeType(AStream: TStream): boolean;
var unzip: TUnzipperStreamUtf8;
  mimeTypeFound: string;
  oldPos: int64;
begin
  result := false;
  unzip := TUnzipperStreamUtf8.Create;
  oldPos := AStream.Position;
  try
    unzip.InputStream := AStream;
    mimeTypeFound := unzip.UnzipFileToString('mimetype');
    if mimeTypeFound = OpenRasterMimeType then result := true;
  except
  end;
  unzip.Free;
  astream.Position:= OldPos;
end;

procedure TBGRAOpenRasterDocument.LoadFlatImageFromStream(AStream: TStream; out
  ANbLayers: integer; out ABitmap: TBGRABitmap);
var fileList: TStringList;
  imgStream, stackStream: TMemoryStream;
  imageNode, stackNode: TDOMNode;
  i: integer;
begin
  fileList := TStringList.Create;
  fileList.Add(MergedImageFilename);
  fileList.Add(LayerStackFilename);
  imgStream := nil;
  try
    UnzipFromStream(AStream, fileList);
    imgStream := GetMemoryStream(MergedImageFilename);
    if imgStream = nil then
      ABitmap := nil
    else
      ABitmap := TBGRABitmap.Create(imgStream);
    ANbLayers := 1;

    stackStream := GetMemoryStream(LayerStackFilename);
    ReadXMLFile(FStackXML, StackStream);
    imageNode := StackXML.FindNode('image');
    if Assigned(imagenode) then
    begin
      stackNode := imageNode.FindNode('stack');
      if Assigned(stackNode) then
      begin
        ANbLayers:= 0;
        for i := stackNode.ChildNodes.Length-1 downto 0 do
        begin
          if stackNode.ChildNodes[i].NodeName = 'layer' then
            inc(ANbLayers);
        end;
      end;
    end;

  finally
    fileList.Free;
    ClearFiles;
  end;
end;

procedure TBGRAOpenRasterDocument.LoadFromStream(AStream: TStream);
begin
  OnLayeredBitmapLoadFromStreamStart;
  try
    InternalLoadFromStream(AStream);
  finally
    OnLayeredBitmapLoaded;
  end;
end;

procedure TBGRAOpenRasterDocument.SetMimeType(AValue: string);
begin
  SetMemoryStreamAsString('mimetype',AValue);
end;

procedure TBGRAOpenRasterDocument.ZipOnCreateStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
var MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  SetMemoryStream(AItem.ArchiveFileName, MemStream);
  AStream := MemStream;
end;

{$hints off}
procedure TBGRAOpenRasterDocument.ZipOnDoneStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  //do nothing, files stay in memory
end;
{$hints on}

procedure TBGRAOpenRasterDocument.ZipOnOpenInputStream(Sender: TObject;
  var AStream: TStream);
begin
  AStream := FZipInputStream;
end;

procedure TBGRAOpenRasterDocument.ZipOnCloseInputStream(Sender: TObject;
  var AStream: TStream);
begin
  AStream := nil; //avoid freeing
end;

procedure TBGRAOpenRasterDocument.ClearFiles;
var i: integer;
begin
  for i := 0 to high(FFiles) do
    ffiles[i].Stream.Free;
  FFiles := nil;
  FreeAndNil(FStackXML);
end;

function TBGRAOpenRasterDocument.GetMemoryStream(AFilename: string): TMemoryStream;
var i: integer;
begin
  for i := 0 to high(FFiles) do
    if ffiles[i].Filename = AFilename then
    begin
      result := FFiles[i].Stream;
      result.Position:= 0;
      exit;
    end;
  result := nil;
end;

procedure TBGRAOpenRasterDocument.SetMemoryStream(AFilename: string;
  AStream: TMemoryStream);
var i: integer;
begin
  for i := 0 to high(FFiles) do
    if ffiles[i].Filename = AFilename then
    begin
      FreeAndNil(FFiles[i].Stream);
      FFiles[i].Stream := AStream;
      exit;
    end;
  setlength(FFiles, length(FFiles)+1);
  FFiles[high(FFiles)].Filename := AFilename;
  FFiles[high(FFiles)].Stream := AStream;
end;

var AlreadyRegistered: boolean;

procedure RegisterOpenRasterFormat;
begin
  if AlreadyRegistered then exit;
  ImageHandlers.RegisterImageReader ('OpenRaster', 'ora', TFPReaderOpenRaster);
  RegisterLayeredBitmapReader('ora', TBGRAOpenRasterDocument);
  RegisterLayeredBitmapWriter('ora', TBGRAOpenRasterDocument);
  //TPicture.RegisterFileFormat('ora', 'OpenRaster', TBGRAOpenRasterDocument);
  DefaultBGRAImageReader[ifOpenRaster] := TFPReaderOpenRaster;
  DefaultBGRAImageWriter[ifOpenRaster] := TFPWriterOpenRaster;
  AlreadyRegistered:= True;
end;

end.

