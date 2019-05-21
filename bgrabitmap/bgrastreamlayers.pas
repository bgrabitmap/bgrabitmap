unit BGRAStreamLayers;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, BGRALayers, BGRABitmap, BGRALzpCommon, BGRAMemDirectory;

function CheckStreamForLayers(AStream: TStream): boolean;
function LoadLayersFromStream(AStream: TStream; out ASelectedLayerIndex: integer; ALoadLayerUniqueIds: boolean = false;
         ADestination: TBGRALayeredBitmap = nil): TBGRALayeredBitmap;
procedure SaveLayersToStream(AStream: TStream; ALayers: TBGRACustomLayeredBitmap; ASelectedLayerIndex: integer; ACompression: TLzpCompression = lzpZStream);
procedure SaveLayerBitmapToStream(AStream: TStream; ABitmap: TBGRABitmap; ACaption: string; ACompression: TLzpCompression = lzpZStream);
function LoadLayerBitmapFromStream(AStream: TStream; ACompression: TLzpCompression = lzpZStream) : TBGRABitmap;
procedure RegisterStreamLayers;

implementation

uses BGRABitmapTypes, BGRACompressableBitmap, zstream, BGRAReadLzp, BGRAWriteLzp,
     BGRAUTF8, Math;

type
  PLayerHeader = ^TLayerHeader;

  { TLayerHeader }

  TLayerHeader = packed record
    LayerOption, BlendOp,
    LayerOfsX, LayerOfsY,
    LayerUniqueId, LayerOpacity: Longint;
    LayerBitmapSize: int64;
    OriginalGuid: TGuid;
    OriginalMatrix: TAffineMatrix;
    procedure FixEndian;
  end;

{ TLayerHeader }

procedure TLayerHeader.FixEndian;
begin
  LayerOption := NtoLE(LayerOption);
  BlendOp := NtoLE(BlendOp);
  LayerOfsX := NtoLE(LayerOfsX);
  LayerOfsY := NtoLE(LayerOfsY);
  LayerUniqueId := NtoLE(LayerUniqueId);
  LayerOpacity := NtoLE(LayerOpacity);
  LayerBitmapSize := NtoLE(LayerBitmapSize);
  OriginalGuid.D1 := NtoBE(OriginalGuid.D1);
  OriginalGuid.D2 := NtoBE(OriginalGuid.D2);
  OriginalGuid.D3 := NtoBE(OriginalGuid.D3);
  DWord(OriginalMatrix[1,1]) := NtoLE(DWord(OriginalMatrix[1,1]));
  DWord(OriginalMatrix[2,1]) := NtoLE(DWord(OriginalMatrix[2,1]));
  DWord(OriginalMatrix[1,2]) := NtoLE(DWord(OriginalMatrix[1,2]));
  DWord(OriginalMatrix[2,2]) := NtoLE(DWord(OriginalMatrix[2,2]));
  DWord(OriginalMatrix[1,3]) := NtoLE(DWord(OriginalMatrix[1,3]));
  DWord(OriginalMatrix[2,3]) := NtoLE(DWord(OriginalMatrix[2,3]));
end;

procedure SaveLayeredBitmapToStream(AStream: TStream; ALayers: TBGRACustomLayeredBitmap);
begin
  SaveLayersToStream(AStream,ALayers,-1);
end;

procedure LoadLayeredBitmapFromStream(AStream: TStream; ALayers: TBGRACustomLayeredBitmap);
var selectedIndex: integer;
begin
  if not CheckStreamForLayers(AStream) then
  begin
    if Assigned(ALayers) then ALayers.Clear;
  end
  else
    LoadLayersFromStream(AStream,selectedIndex,false,ALayers as TBGRALayeredBitmap);
end;

const
  StreamHeader = 'TBGRALayeredBitmap'#26#0;
  StreamMaxLayerCount = 4096;
  StreamMaxHeaderSize = 256;

function CheckStreamForLayers(AStream: TStream): boolean;
var
  OldPosition: Int64;
  HeaderFound: string;
begin
  result := false;
  OldPosition:= AStream.Position;
  try
    SetLength(HeaderFound, length(StreamHeader));
    SetLength(HeaderFound, AStream.Read(HeaderFound[1], length(HeaderFound)));
    if HeaderFound = StreamHeader then
      result := true;
  except
    on ex: exception do
    begin
      //nothing
    end;
  end;
  AStream.Position:= OldPosition;
end;

procedure RenameLayersToUniqueId(ALayers: TBGRACustomLayeredBitmap);
var
  layerDir: TMemDirectory;
  i: Integer;
begin
  layerDir := ALayers.MemDirectory.FindPath('layers');
  if Assigned(layerDir) then
  begin
    for i := 0 to ALayers.NbLayers-1 do
      layerDir.Rename('layer'+inttostr(i+1), '', inttostr(ALayers.LayerUniqueId[i]));
  end;
end;

procedure RenameLayersToIndex(ALayers: TBGRACustomLayeredBitmap);
var
  layerDir: TMemDirectory;
  i: Integer;
begin
  layerDir := ALayers.MemDirectory.FindPath('layers');
  if Assigned(layerDir) then
  begin
    for i := 0 to ALayers.NbLayers-1 do
      layerDir.Rename(inttostr(ALayers.LayerUniqueId[i]), '', 'layer'+inttostr(i+1));
  end;
end;

function LoadLayersFromStream(AStream: TStream; out ASelectedLayerIndex: integer; ALoadLayerUniqueIds: boolean = false;
         ADestination: TBGRALayeredBitmap = nil): TBGRALayeredBitmap;
var
  OldPosition: Int64;
  HeaderFound: string;
  NbLayers, canvasWidth, canvasHeight: LongInt;
  HeaderSize, LayerHeaderSize: LongInt;
  LayerStackStartPosition, LayerHeaderPosition,
  LayerBitmapPosition, LayerEndPosition, MemDirPos: Int64;
  StackOption: LongInt;
  Layer: TBGRABitmap;
  i,LayerIndex: integer;
  LayerName: string;
  Compression: TLzpCompression;
  LayerBlendOp: TBlendOperation;
  LayerIdFound: boolean;
  h: TLayerHeader;
begin
  if Assigned(ADestination) then
  begin
    result := ADestination;
    result.Clear;
  end else
    result := TBGRALayeredBitmap.Create;
  OldPosition:= AStream.Position;
  SetLength(HeaderFound, length(StreamHeader));
  try
    //format identifier
    SetLength(HeaderFound, AStream.Read(HeaderFound[1], length(HeaderFound)));
    if HeaderFound <> StreamHeader then
      raise exception.Create('Invalid header');

    //header size
    HeaderSize:= LEReadLongint(AStream);
    if (HeaderSize < 12) or (HeaderSize > StreamMaxHeaderSize) then
      raise exception.Create('Invalid header size');
    LayerStackStartPosition := AStream.Position + HeaderSize;

    NbLayers:= LEReadLongint(AStream);
    if (NbLayers < 0) or (NbLayers > StreamMaxLayerCount) then
      raise exception.Create('Invalid layer count');

    ASelectedLayerIndex:= LEReadLongint(AStream);
    if (ASelectedLayerIndex < -1) or (ASelectedLayerIndex >= NbLayers) then
      raise exception.Create('Selected layer out of bounds');

    StackOption := LEReadLongint(AStream);
    result.LinearBlend := (StackOption and 1) = 1;
    if (StackOption and 2) = 2 then Compression := lzpRLE else Compression:= lzpZStream;

    if headerSize >= 20 then
    begin
      canvasWidth := LEReadLongint(AStream);
      canvasHeight := LEReadLongint(AStream);
      result.SetSize(canvasWidth,canvasHeight);
    end;

    if headerSize >= 28 then
    begin
      MemDirPos := LEReadInt64(AStream);
    end else MemDirPos := 0;
    //end of header

    if MemDirPos <> 0 then
    begin
      AStream.Position:= MemDirPos+OldPosition;
      result.MemDirectory.LoadFromStream(AStream);
    end else
      result.MemDirectory.Clear;

    AStream.Position:= LayerStackStartPosition;
    for i := 0 to NbLayers-1 do
    begin
      LayerHeaderSize:= LEReadLongint(AStream);

      LayerHeaderPosition := AStream.Position;
      LayerBitmapPosition := LayerHeaderPosition + LayerHeaderSize;
      LayerEndPosition := -1;

      fillchar({%H-}h, sizeof(h), 0);
      h.LayerOption := 1; //visible
      h.BlendOp:= integer(result.DefaultBlendingOperation);
      h.LayerOpacity := 65535; //opaque
      h.LayerUniqueId:= maxLongint;
      h.FixEndian;

      AStream.ReadBuffer(h, min(LayerHeaderSize, sizeof(h)));
      h.FixEndian;

      if h.BlendOp > ord(high(TBlendOperation)) then
        LayerBlendOp := result.DefaultBlendingOperation
      else
        LayerBlendOp:= TBlendOperation(h.BlendOp);

      LayerIdFound := h.LayerUniqueId <> maxLongint;

      if h.LayerBitmapSize > 0 then
        LayerEndPosition:= LayerBitmapPosition+h.LayerBitmapSize;

      AStream.Position:= LayerBitmapPosition;
      Layer := LoadLayerBitmapFromStream(AStream, Compression);
      LayerName := Layer.Caption;
      LayerIndex := result.AddOwnedLayer(Layer);
      Layer := nil;

      result.LayerName[LayerIndex] := LayerName;
      result.LayerVisible[LayerIndex] := (h.LayerOption and 1) = 1;
      result.BlendOperation[LayerIndex]:= LayerBlendOp;
      result.LayerOffset[LayerIndex] := Point(h.LayerOfsX,h.LayerOfsY);
      if ALoadLayerUniqueIds and LayerIdFound then
        result.LayerUniqueId[LayerIndex] := h.LayerUniqueId;
      result.LayerOpacity[LayerIndex] := h.LayerOpacity shr 8;
      result.LayerOriginalGuid[LayerIndex] := h.OriginalGuid;
      result.LayerOriginalMatrix[LayerIndex] := h.OriginalMatrix;
      result.LayerOriginalRenderStatus[layerIndex] := orsProof;

      if LayerEndPosition <> -1 then AStream.Position := LayerEndPosition;
    end;

    RenameLayersToUniqueId(result);
    result.NotifyLoaded;
  except
    on ex: Exception do
    begin
      AStream.Position := OldPosition;
      if not Assigned(ADestination) then result.Free;
      raise ex;
    end;
  end;
end;

procedure SaveLayersToStream(AStream: TStream; ALayers: TBGRACustomLayeredBitmap; ASelectedLayerIndex: integer; ACompression: TLzpCompression);
var
  StackOption: longint;
  i: integer;
  DirectoryOffsetPos, EndPos: int64;
  LayerHeaderPosition: int64;
  LayerBitmapPosition,BitmapSize, startPos: int64;
  bitmap: TBGRABitmap;
  h: TLayerHeader;
begin
  if (ASelectedLayerIndex < -1) or (ASelectedLayerIndex >= ALayers.NbLayers) then
    raise exception.Create('Selected layer out of bounds');

  ALayers.NotifySaving;

  startPos := AStream.Position;
  AStream.Write(StreamHeader[1], length(StreamHeader));
  LEWriteLongint(AStream, 28); //header size
  LEWriteLongint(AStream, ALayers.NbLayers);
  LEWriteLongint(AStream, ASelectedLayerIndex);
  StackOption := 0;
  if ALayers.LinearBlend then StackOption := StackOption or 1;
  if ACompression = lzpRLE then StackOption:= StackOption or 2;
  LEWriteLongint(AStream, StackOption);
  LEWriteLongint(AStream, ALayers.Width);
  LEWriteLongint(AStream, ALayers.Height);
  DirectoryOffsetPos := AStream.Position;
  LEWriteInt64(AStream, 0);
  //end of header

  for i := 0 to ALayers.NbLayers-1 do
  begin
    LEWriteLongint(AStream, sizeof(h));
    LayerHeaderPosition := AStream.Position;

    bitmap := ALayers.GetLayerBitmapDirectly(i); //do it before to ensure update from original

    h.LayerOption:= 0;
    if ALayers.LayerVisible[i] then h.LayerOption:= h.LayerOption or 1;
    h.BlendOp:= Longint(ALayers.BlendOperation[i]);
    h.LayerOfsX:= ALayers.LayerOffset[i].x;
    h.LayerOfsY:= ALayers.LayerOffset[i].y;
    h.LayerUniqueId:= ALayers.LayerUniqueId[i];
    h.LayerOpacity:= integer(ALayers.LayerOpacity[i])*$101;
    h.LayerBitmapSize := 0;
    h.OriginalGuid := ALayers.LayerOriginalGuid[i];
    h.OriginalMatrix := ALayers.LayerOriginalMatrix[i];
    h.FixEndian;
    AStream.WriteBuffer(h, sizeof(h));
    //end of layer header

    LayerBitmapPosition:=AStream.Position;
    if bitmap <> nil then
      SaveLayerBitmapToStream(AStream, bitmap, ALayers.LayerName[i], ACompression)
    else
    begin
      bitmap := ALayers.GetLayerBitmapCopy(i);
      SaveLayerBitmapToStream(AStream, bitmap, ALayers.LayerName[i], ACompression);
      bitmap.free;
    end;

    BitmapSize := AStream.Position - LayerBitmapPosition;

    //store back the bitmap size
    AStream.Position:= LayerHeaderPosition + (PByte(@PLayerHeader(nil)^.LayerBitmapSize)-PByte(nil));
    LEWriteInt64(AStream, BitmapSize);

    AStream.Position:= LayerBitmapPosition+BitmapSize;
  end;

  EndPos:= AStream.Position;
  if ALayers.HasMemFiles then
  begin
    AStream.Position := DirectoryOffsetPos;
    LEWriteInt64(AStream,EndPos-startPos);
    AStream.Position:= EndPos;
    RenameLayersToIndex(ALayers);
    ALayers.MemDirectory.SaveToStream(AStream);
    RenameLayersToUniqueId(ALayers);
  end;
end;

procedure SaveLayerBitmapToStream(AStream: TStream; ABitmap: TBGRABitmap; ACaption: string; ACompression: TLzpCompression);
var Compressed: TBGRACompressableBitmap;
begin
  if ACompression = lzpZStream then
  begin
    Compressed := TBGRACompressableBitmap.Create(ABitmap);
    Compressed.Caption := ACaption;
    Compressed.CompressionLevel:= cldefault;
    Compressed.WriteToStream(AStream);
    Compressed.Free;
  end else
    TBGRAWriterLazPaint.WriteRLEImage(AStream, ABitmap, ACaption);
end;

function LoadLayerBitmapFromStream(AStream: TStream; ACompression: TLzpCompression): TBGRABitmap;
var Compressed: TBGRACompressableBitmap;
  captionFound: string;
begin
  if ACompression = lzpZStream then
  begin
    Compressed := TBGRACompressableBitmap.Create;
    Compressed.ReadFromStream(AStream);
    result := Compressed.GetBitmap;
    Compressed.Free;
  end else
  begin
    result := TBGRABitmap.Create;
    TBGRAReaderLazPaint.LoadRLEImage(AStream, result, captionFound);
    result.Caption := captionFound;
  end;
end;

procedure RegisterStreamLayers;
begin
  LayeredBitmapSaveToStreamProc := @SaveLayeredBitmapToStream;
  LayeredBitmapLoadFromStreamProc := @LoadLayeredBitmapFromStream;
  LayeredBitmapCheckStreamProc := @CheckStreamForLayers;
end;

initialization

  RegisterStreamLayers;

end.

