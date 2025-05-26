// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Phoxo layered image format, with .oXo extension. }
unit BGRAPhoxo;

{$mode objfpc}{$H+}

interface

uses
  BGRABitmapTypes, FPImage, BGRALayers, BGRABitmap, BGRAClasses, SysUtils, BMPcomn;

const
  PhoxoHeaderMagic : packed array[1..4] of char = 'oXo ';
  PhoxoBlock_CanvasSize = 1;
  PhoxoBlock_Layer = 2;
  PhoxoBlock_TextLayer = 3;
  PhoxoBlock_DPI = 4;
  PhoxoBlock_LayerCaption = 5;
  PhoxoBlock_LazPaintBlendMode = 128;
  PhoxoBlock_EndOfFile = 255;

type
  { PhoXo file header }
  TPhoxoHeader = packed record
    magic: packed array[1..4] of char;
    version: LongWord;
  end;

  { PhoXo block header }
  TPhoxoBlockHeader = packed record
    blockType : LongWord;
    blockSize : LongWord;
  end;

  { PhoXo layer header }
  TPhoxoLayerHeader = packed record
    layerVisible: LongWord;
    layerLimited: LongWord;
    opacityPercent: LongWord;
    bmpHeader: TBitMapInfoHeader;
    redMask,greenMask,blueMask: LongWord;
  end;

  { Layered image in Phoxo format }
  TBGRAPhoxoDocument = class(TBGRALayeredBitmap)
  private
    FDPIX,FDPIY: integer;
  protected
    function GetMimeType: string; override;
    procedure AddLayerFromPhoxoData(const ABlockHeader: TPhoxoBlockHeader; ABlockData: PByte);
    procedure InternalLoadFromStream(AStream: TStream);
    procedure InternalSaveToStream(AStream: TStream);
  public
    constructor Create; overload; override;
    constructor Create(AWidth, AHeight: integer); overload; override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure LoadFromFile(const filenameUTF8: string); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure SaveToFile(const filenameUTF8: string); override;
    class function CheckFormat(Stream: TStream; ARestorePosition: boolean): boolean; static;
    class function ReadBlock(Stream: TStream; out AHeader: TPhoxoBlockHeader; out ABlockData: PByte): boolean; static;
    property DPIX: integer read FDPIX;
    property DPIY: integer read FDPIY;
  end;

  { Reader for Phoxo image (flattened) }

  { TBGRAReaderOXO }

  TBGRAReaderOXO = class(TFPCustomImageReader)
  private
    FWidth,FHeight,FNbLayers: integer;
    FDPIX,FDPIY: integer;
  protected
    procedure ReadResolutionValues(Img: TFPCustomImage);
    function InternalCheck(Stream: TStream): boolean; override;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
  public
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property NbLayers: integer read FNbLayers;
    property DPIX: integer read FDPIX;
    property DPIY: integer read FDPIY;
  end;

  { Writer for Phoxo image (flattened) }
  TBGRAWriterOXO = class(TFPCustomImageWriter)
    protected
      procedure InternalWrite (Str:TStream; Img:TFPCustomImage); override;
  end;

procedure RegisterPhoxoFormat;

implementation

uses BGRAUTF8;

var AlreadyRegistered: boolean;

function ComputeRowStride(AWidth,ABitsPerPixel: Longword): Longword;
begin
  result := ((AWidth * ABitsPerPixel + 31) div 32)*4;
end;

procedure SwapLayerHeaderIfNeeded(var ALayerHeader: TPhoxoLayerHeader);
begin
  with ALayerHeader do
  begin
    layerVisible := LEtoN(layerVisible);
    layerLimited := LEtoN(layerLimited);
    opacityPercent := LEtoN(opacityPercent);
    {$IFNDEF ENDIAN_LITTLE}SwapBMPInfoHeader(bmpHeader);{$ENDIF}
  end;
end;

procedure RegisterPhoxoFormat;
begin
  if AlreadyRegistered then exit;

  BGRARegisterImageHandlers(ifPhoxo, TBGRAReaderOXO, TBGRAWriterOXO, True, 'PhoXo', 'oXo');

  RegisterLayeredBitmapReader('oXo', TBGRAPhoxoDocument);
  RegisterLayeredBitmapWriter('oXo', TBGRAPhoxoDocument);

  AlreadyRegistered:= True;
end;

{ TBGRAWriterOXO }

procedure TBGRAWriterOXO.InternalWrite(Str: TStream; Img: TFPCustomImage);
var doc: TBGRAPhoxoDocument;
  tempBmp: TBGRABitmap;
  x,y: integer;
begin
  doc := TBGRAPhoxoDocument.Create;
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

{ TBGRAReaderOXO }

procedure TBGRAReaderOXO.ReadResolutionValues(Img: TFPCustomImage);
begin
  {$IF FPC_FULLVERSION<=30203}
  if (Img is TCustomUniversalBitmap) then
  with TCustomUniversalBitmap(Img) do
  begin
    ResolutionUnit:=ruPixelsPerInch;
    ResolutionX :=FDPIX;
    ResolutionY :=FDPIY;
  end;
  {$ELSE}
  Img.ResolutionUnit:=ruPixelsPerInch;
  Img.ResolutionX :=FDPIX;
  Img.ResolutionY :=FDPIY;
  {$ENDIF}
end;

function TBGRAReaderOXO.InternalCheck(Stream: TStream): boolean;
begin
  result := TBGRAPhoxoDocument.CheckFormat(Stream,True);
end;

procedure TBGRAReaderOXO.InternalRead(Stream: TStream; Img: TFPCustomImage);
var layeredImage: TBGRAPhoxoDocument;
  flat: TBGRABitmap;
  x,y: integer;
begin
  FWidth := 0;
  FHeight:= 0;
  FNbLayers:= 0;
  FDPIX := 0;
  FDPIY := 0;
  layeredImage := TBGRAPhoxoDocument.Create;
  try
    layeredImage.LoadFromStream(Stream);
    flat := layeredImage.ComputeFlatImage;
    try
      FWidth:= layeredImage.Width;
      FHeight:= layeredImage.Height;
      FNbLayers:= layeredImage.NbLayers;
      FDPIX := layeredImage.DPIX;
      FDPIY := layeredImage.DPIY;
      if Img is TBGRACustomBitmap then
        TBGRACustomBitmap(img).Assign(flat)
      else
      begin
        Img.SetSize(flat.Width,flat.Height);
        for y := 0 to flat.Height-1 do
          for x := 0 to flat.Width-1 do
            Img.Colors[x,y] := BGRAToFPColor(flat.GetPixel(x,y));
      end;

      ReadResolutionValues(img);

    finally
      flat.free;
    end;
  finally
    layeredImage.Free;
  end;
end;

{ TBGRAPhoxoDocument }

function TBGRAPhoxoDocument.GetMimeType: string;
begin
  Result:= 'image/phoxo';
end;

procedure TBGRAPhoxoDocument.AddLayerFromPhoxoData(
  const ABlockHeader: TPhoxoBlockHeader; ABlockData: PByte);
var
  layerHeader: TPhoxoLayerHeader;
  rawImageSize: LongWord;
  rowStride: LongWord;
  remaining: LongWord;
  bmp: TBGRABitmap;
  layerIndex,y,x: integer;
  pSrc: PByte;
  pDest: PBGRAPixel;
begin
  remaining := ABlockHeader.blockSize;
  if remaining < sizeof(TPhoxoLayerHeader) then raise EFormatError.Create('Block too small');
  move(ABlockData^, {%H-}layerHeader, sizeof(layerHeader));
  inc(ABlockData, sizeof(layerHeader));
  dec(remaining, sizeof(layerHeader));
  SwapLayerHeaderIfNeeded(layerHeader);

  if layerHeader.bmpHeader.Compression <> BI_RGB then raise EFormatError.Create('Compression not supported');
  if (layerHeader.bmpHeader.Width < 0) or (layerHeader.bmpHeader.Height < 0) then
    raise EFormatError.Create('Invalid image size');
  if int64(layerHeader.bmpHeader.Width)*layerHeader.bmpHeader.Height > maxLongint div 4 then
    raise EOutOfMemory.Create('Image too big');
  rowStride := ComputeRowStride(layerHeader.bmpHeader.Width,layerHeader.bmpHeader.BitCount);
  rawImageSize := rowStride * layerHeader.bmpHeader.Height;

  if rawImageSize > remaining then
     raise EFormatError.Create('Invalid image size');

  bmp := TBGRABitmap.Create(layerHeader.bmpHeader.Width, layerHeader.bmpHeader.Height);
  layerIndex := AddOwnedLayer(bmp, (layerHeader.opacityPercent*255 + 50) div 100);
  LayerVisible[layerIndex] := (layerHeader.layerVisible = 1);

  case layerHeader.bmpHeader.BitCount of
    8: begin
      for y := bmp.Height-1 downto 0 do
        begin
          pSrc := ABlockData + (bmp.Height-1 - y)*rowStride;
          pDest := bmp.ScanLine[y];
          for x := bmp.Width-1 downto 0 do
          begin
            pDest^ := BGRA(pSrc^,pSrc^,pSrc^);
            inc(pDest);
            inc(pSrc,3);
          end;
        end;
    end;
    24: begin
      for y := bmp.Height-1 downto 0 do
        begin
          pSrc := ABlockData + (bmp.Height-1 - y)*rowStride;
          pDest := bmp.ScanLine[y];
          for x := bmp.Width-1 downto 0 do
          begin
            pDest^ := BGRA((pSrc+2)^,(pSrc+1)^,pSrc^);
            inc(pDest);
            inc(pSrc,3);
          end;
        end;
    end;
    32: begin
      move(ABlockData^, bmp.Data^, sizeof(TBGRAPixel)*bmp.NbPixels);
      if bmp.LineOrder = riloTopToBottom then bmp.VerticalFlip;
      if TBGRAPixel_RGBAOrder then bmp.SwapRedBlue;
    end;
  else
    raise EFormatError.Create('Unsupported bit depth');
  end;

  inc(ABlockData, rawImageSize);
  dec(remaining, rawImageSize);
  if remaining >= 8 then
  begin
    LayerOffset[layerIndex] := Point(LEtoN(PLongInt(ABlockData)^),LEtoN((PLongInt(ABlockData)+1)^));
    inc(ABlockData, 8);
    dec(remaining, 8);
  end;
end;

constructor TBGRAPhoxoDocument.Create;
begin
  inherited Create;
  RegisterPhoxoFormat;
end;

constructor TBGRAPhoxoDocument.Create(AWidth, AHeight: integer);
begin
  inherited Create(AWidth, AHeight);
  RegisterPhoxoFormat;
end;

procedure TBGRAPhoxoDocument.LoadFromStream(AStream: TStream);
begin
  OnLayeredBitmapLoadFromStreamStart;
  try
    InternalLoadFromStream(AStream);
  finally
    OnLayeredBitmapLoaded;
  end;
end;

procedure TBGRAPhoxoDocument.InternalLoadFromStream(AStream: TStream);
var blockHeader: TPhoxoBlockHeader;
    blockData: PByte;
    wCaption: UnicodeString;
    i: Integer;
begin
  if not CheckFormat(AStream,False) then
    raise EFormatError.Create('File header is invalid');
  Clear;
  FDPIX := 0;
  FDPIY := 0;
  blockData := nil;
  repeat
    if not ReadBlock(AStream, blockHeader,blockData) then
    begin
      if NbLayers = 0 then
        raise EFormatError.Create('Error reading block from file')
      else
        break;
    end;
    try
      case blockHeader.blockType of
        PhoxoBlock_CanvasSize:
          begin
            if blockHeader.blockSize < 8 then raise EFormatError.Create('Block too small');
            SetSize(LEtoN(PLongWord(blockData)^),LEtoN((PLongWord(blockData)+1)^));
          end;
        PhoxoBlock_DPI:
          begin
            if blockHeader.blockSize >= 8 then
            begin
              FDPIX := LEtoN(PLongWord(blockData)^);
              FDPIY := LEtoN((PLongWord(blockData)+1)^);
            end;
          end;
        PhoxoBlock_Layer, PhoxoBlock_TextLayer:
          AddLayerFromPhoxoData(blockHeader,blockData);
        PhoxoBlock_LayerCaption:
          begin
            if (blockHeader.blockSize >= 2) and (NbLayers > 0) then
            begin
              setlength(wCaption, blockHeader.blockSize div 2);
              for i := 1 to length(wCaption) do
                Word(wCaption[i]) := LEtoN((PWord(blockData)+i-1)^);
              if wCaption[1] = #1 then Delete(wCaption,1,1);
              LayerName[NbLayers-1] := UTF8Encode(wCaption);

            end;
          end;
        PhoxoBlock_LazPaintBlendMode:
          begin
            if (blockHeader.blockSize >= 2) and (NbLayers > 0) then
            begin
              setlength(wCaption, blockHeader.blockSize div 2);
              for i := 1 to length(wCaption) do
                Word(wCaption[i]) := LEtoN((PWord(blockData)+i-1)^);
              BlendOperation[NbLayers-1] := StrToBlendOperation(UTF8Encode(wCaption));
            end;
          end;
      end;
    finally
      FreeMem(blockData);
    end;
  until blockHeader.blockType = PhoxoBlock_EndOfFile;
end;

procedure TBGRAPhoxoDocument.LoadFromFile(const filenameUTF8: string);
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

procedure TBGRAPhoxoDocument.SaveToStream(AStream: TStream);
begin
  OnLayeredBitmapSaveToStreamStart;
  try
    InternalSaveToStream(AStream);
  finally
    OnLayeredBitmapSaved;
  end;
end;

procedure TBGRAPhoxoDocument.SaveToFile(const filenameUTF8: string);
var AStream: TFileStreamUTF8;
begin
  AStream := TFileStreamUTF8.Create(filenameUTF8,fmCreate or fmShareDenyWrite);
  OnLayeredBitmapSaveStart(filenameUTF8);
  try
    InternalSaveToStream(AStream);
  finally
    OnLayeredBitmapSaved;
    AStream.Free;
  end;
end;

procedure TBGRAPhoxoDocument.InternalSaveToStream(AStream: TStream);

  procedure WriteFileHeader;
  var fileHeader: TPhoxoHeader;
  begin
    fileHeader.magic := PhoxoHeaderMagic;
    fileHeader.version := 1;
    fileHeader.version := NtoLE(fileHeader.version);
    AStream.WriteBuffer(fileHeader, sizeof(fileHeader));
  end;

  procedure WriteBlockHeader(blockType: LongWord; blockSize: LongWord);
  var blockHeader: TPhoxoBlockHeader;
  begin
    blockHeader.blockType := NtoLE(blockType);
    blockHeader.blockSize := NtoLE(blockSize);
    AStream.WriteBuffer(blockHeader, sizeof(blockHeader));
  end;

  procedure WriteLongInt(value: longint);
  begin
    value := NtoLE(value);
    AStream.WriteBuffer(value, sizeof(value));
  end;

  procedure WriteLayer(index: integer);
  var wCaption: UnicodeString;
      pCaption: PWord;

      layerHeader: TPhoxoLayerHeader;
      rowStride: LongWord;

      temp,pdest: PByte;
      i,x,y: integer;
      psrc: PBGRAPixel;
  begin
    if LayerVisible[index] then
      layerHeader.layerVisible := 1
    else
      layerHeader.layerVisible := 0;
    layerHeader.layerLimited:= 0;
    layerHeader.opacityPercent := (LayerOpacity[index]*100 + 127) div 255;
    with layerHeader.bmpHeader do
    begin
      Size := $28;
      Width := self.LayerBitmap[index].Width;
      Height := self.LayerBitmap[index].Height;
      Planes := 1;
      BitCount := 32; //24-bit does not seem to be supported
      Compression := BI_RGB;
      SizeImage := 0;
      XPelsPerMeter := 0;
      YPelsPerMeter := 0;
      ClrUsed := 0;
      ClrImportant := 0;
    end;
    layerHeader.redMask := 0;
    layerHeader.greenMask := 0;
    layerHeader.blueMask := 0;

    rowStride := ComputeRowStride(layerHeader.bmpHeader.Width, layerHeader.bmpHeader.BitCount);

    WriteBlockHeader(PhoxoBlock_Layer, sizeof(layerHeader) + rowStride*layerHeader.bmpHeader.Height + sizeof(TPoint));
    SwapLayerHeaderIfNeeded(layerHeader);
    AStream.WriteBuffer(layerHeader,sizeof(layerHeader));
    SwapLayerHeaderIfNeeded(layerHeader);

    case layerHeader.bmpHeader.BitCount of
      32: begin
            if TBGRAPixel_RGBAOrder then self.LayerBitmap[index].SwapRedBlue;
            for y := self.LayerBitmap[index].Height-1 downto 0 do
              AStream.WriteBuffer(self.LayerBitmap[index].ScanLine[y]^, rowStride);
            if TBGRAPixel_RGBAOrder then self.LayerBitmap[index].SwapRedBlue;
          end;
      24: begin
            GetMem(temp, rowStride);
            fillchar(temp^, rowStride, 0);
            try
              for y := self.LayerBitmap[index].Height-1 downto 0 do
              begin
                psrc := self.LayerBitmap[index].ScanLine[y];
                pdest := temp;
                for x := self.LayerBitmap[index].Width-1 downto 0 do
                begin
                  pdest^ := psrc^.blue; inc(pdest);
                  pdest^ := psrc^.green; inc(pdest);
                  pdest^ := psrc^.red; inc(pdest);
                  inc(psrc);
                end;
                AStream.WriteBuffer(temp^, rowstride);
              end;
            finally
              FreeMem(temp);
            end;
          end
      else
        raise exception.Create('Internal error');
      end;

    WriteLongInt(LayerOffset[index].X);
    WriteLongInt(LayerOffset[index].Y);

    if LayerName[index]<>'' then
    begin
      wCaption := UTF8ToUTF16(LayerName[index]);
      WriteBlockHeader(PhoxoBlock_LayerCaption, length(wCaption)*2);
      getmem(pCaption, length(wCaption)*2);
      try
        for i := 1 to length(wCaption) do
          (pCaption+i-1)^ := NtoLE(Word(wCaption[i]));
        AStream.WriteBuffer(pCaption^, length(wCaption)*2);
      finally
        freemem(pCaption);
      end;
    end;

    if BlendOperation[index] <> boTransparent then
    begin
      wCaption := UTF8ToUTF16(BlendOperationStr[BlendOperation[index]]);
      WriteBlockHeader(PhoxoBlock_LazPaintBlendMode, length(wCaption)*2);
      getmem(pCaption, length(wCaption)*2);
      try
        for i := 1 to length(wCaption) do
          (pCaption+i-1)^ := NtoLE(Word(wCaption[i]));
        AStream.WriteBuffer(pCaption^, length(wCaption)*2);
      finally
        freemem(pCaption);
      end;
    end;
  end;

var
  i: Integer;
begin
  WriteFileHeader;

  WriteBlockHeader(PhoxoBlock_CanvasSize, 8);
  WriteLongInt(Width);
  WriteLongInt(Height);

  if (DPIX <> 0) and (DPIY <> 0) then
  begin
    WriteBlockHeader(PhoxoBlock_DPI, 8);
    WriteLongInt(DPIX);
    WriteLongInt(DPIY);
  end;

  for i := 0 to NbLayers-1 do
  begin
    OnLayeredBitmapSaveProgress(round(i*100/NbLayers));
    WriteLayer(i);
  end;
  OnLayeredBitmapSaveProgress(100);

  WriteBlockHeader(PhoxoBlock_EndOfFile,0);
end;

class function TBGRAPhoxoDocument.CheckFormat(Stream: TStream; ARestorePosition: boolean): boolean;
var header: TPhoxoHeader;
  oldPos: int64;
begin
  oldPos := Stream.Position;
  if Stream.Read({%H-}header,sizeof(header))<>sizeof(header) then
    result := false else
  begin
    header.version:= LEtoN(header.version);
    if (header.magic <> PhoxoHeaderMagic) or (header.version <> 1) then
      result := false
    else
      result := true;
  end;
  if ARestorePosition then Stream.Position:= oldPos;
end;

class function TBGRAPhoxoDocument.ReadBlock(Stream: TStream; out
  AHeader: TPhoxoBlockHeader; out ABlockData: PByte): boolean;
begin
  ABlockData := nil;
  if Stream.Read({%H-}AHeader,sizeof(AHeader)) <> sizeof(AHeader) then
  begin
    AHeader.blockType := 0;
    AHeader.blockSize := 0;
    result := false;
    exit;
  end;
  AHeader.blockType := LEtoN(AHeader.blockType);
  AHeader.blockSize := LEtoN(AHeader.blockSize);
  if Stream.Position + AHeader.blockSize > Stream.Size then
  begin
    AHeader.blockSize := 0;
    result := false;
    exit;
  end;
  GetMem(ABlockData, AHeader.blockSize);
  if Stream.Read(ABlockData^, AHeader.blockSize) <> AHeader.blockSize then
  begin
    FreeMem(ABlockData);
    AHeader.blockSize := 0;
    result := false;
    exit;
  end;
  result := true;
end;

end.

