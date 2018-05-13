unit BGRALazPaint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRALayers, BGRABitmapTypes, BGRAReadLzp, BGRAWriteLzp,
  BGRALzpCommon, FPimage;

type
  TLzpCompression = BGRALzpCommon.TLzpCompression;

  { TBGRALazPaintImage }

  TBGRALazPaintImage = class(TBGRALayeredBitmap)
  private
    FSelectedLayerIndex: integer;
  public
    constructor Create; override; overload;
    constructor Create(AWidth, AHeight: integer); override; overload;
    procedure LoadFromStream(AStream: TStream); override;
    procedure LoadFromFile(const filenameUTF8: string); override;
    procedure SaveToFile(const filenameUTF8: string); override;
    procedure SaveToStream(AStream: TStream); override;
    property SelectedLayerIndex: integer read FSelectedLayerIndex write FSelectedLayerIndex;
  end;

  { TBGRAWriterLazPaintWithLayers }

  TBGRAWriterLazPaintWithLayers = class(TBGRAWriterLazPaint)
    protected
      FLayers: TBGRALayeredBitmap;
      FSelectedLayerIndex: integer;
      FCompression: TLzpCompression;
      function GetNbLayers: integer; override;
      function InternalWriteLayers(Str: TStream; {%H-}Img: TFPCustomImage): boolean; override;
    public
      constructor Create(ALayers: TBGRALayeredBitmap); overload;
      property SelectedLayerIndex: integer read FSelectedLayerIndex write FSelectedLayerIndex;
      property Compression: TLzpCompression read FCompression write FCompression;
  end;

  { TBGRAReaderLazPaintWithLayers }

  TBGRAReaderLazPaintWithLayers = class(TBGRAReaderLazPaint)
    protected
      FLayers: TBGRALayeredBitmap;
      FLayersLoaded: boolean;
      FSelectedLayerIndex: integer;
      procedure InternalReadLayers(str: TStream; {%H-}Img: TFPCustomImage); override;
    public
      constructor Create(ALayers: TBGRALayeredBitmap); overload;
      property LayersLoaded: boolean read FLayersLoaded;
      property SelectedLayerIndex: integer read FSelectedLayerIndex;
  end;

procedure RegisterLazPaintFormat;

implementation

uses BGRAStreamLayers, BGRABitmap, BGRAUTF8;

{ TBGRALazPaintImage }

constructor TBGRALazPaintImage.Create;
begin
  inherited Create;
  RegisterLazPaintFormat;
  FSelectedLayerIndex:= 0;
end;

constructor TBGRALazPaintImage.Create(AWidth, AHeight: integer);
begin
  inherited Create(AWidth, AHeight);
  RegisterLazPaintFormat;
  FSelectedLayerIndex:= 0;
end;

procedure TBGRALazPaintImage.LoadFromStream(AStream: TStream);
var
  {%H-}header: TLazPaintImageHeader;
  bmp: TBGRACustomBitmap;
  reader: TBGRAReaderLazPaintWithLayers;
begin
  AStream.ReadBuffer({%H-}header, sizeof(header));
  LazPaintImageHeader_SwapEndianIfNeeded(header);
  AStream.Position:= AStream.Position-sizeof(header);

  //use shortcut if possible
  if (header.magic = LAZPAINT_MAGIC_HEADER) and (header.zero1 = 0)
   and (header.layersOffset >= sizeof(header)) then
  begin
    AStream.Position:= AStream.Position+header.layersOffset;
    LoadLayersFromStream(AStream, FSelectedLayerIndex, false, self);
  end else
  begin
    reader := TBGRAReaderLazPaintWithLayers.Create(self);
    try
      bmp := BGRABitmapFactory.Create;
      bmp.LoadFromStream(AStream, reader);
      if reader.LayersLoaded then
      begin
        bmp.Free;
      end else
      begin
        Clear;
        SetSize(bmp.Width,bmp.Height);
        AddOwnedLayer(bmp as TBGRABitmap);
        LayerName[0] := reader.Caption;
      end;
      SelectedLayerIndex:= reader.SelectedLayerIndex;
    finally
      reader.Free;
    end;
  end;
end;

procedure TBGRALazPaintImage.LoadFromFile(const filenameUTF8: string);
var AStream: TFileStreamUTF8;
begin
  AStream := TFileStreamUTF8.Create(filenameUTF8,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TBGRALazPaintImage.SaveToFile(const filenameUTF8: string);
var AStream: TFileStreamUTF8;
begin
  AStream := TFileStreamUTF8.Create(filenameUTF8,fmCreate or fmShareDenyWrite);
  try
    SaveToStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TBGRALazPaintImage.SaveToStream(AStream: TStream);
var
  writer: TBGRAWriterLazPaint;
  flat: TBGRACustomBitmap;
begin
  if NbLayers = 0 then
    raise exception.Create('File cannot be empty');

  writer := nil;
  flat := nil;
  try
    if (NbLayers > 1) or (LayerOpacity[0] <> 255) or not LayerVisible[0] or (BlendOperation[0]<>boTransparent) then
    begin
      writer := TBGRAWriterLazPaintWithLayers.Create(self);
      writer.Caption := 'Preview';
      TBGRAWriterLazPaintWithLayers(writer).SelectedLayerIndex := self.SelectedLayerIndex;
    end else
    begin
      writer := TBGRAWriterLazPaint.Create;
      writer.Caption := LayerName[0];
    end;

    writer.IncludeThumbnail:= true;
    flat := ComputeFlatImage;
    flat.SaveToStream(AStream, writer);
  finally
    writer.Free;
    flat.Free;
  end;
end;

{ TBGRAReaderLazPaintWithLayers }

procedure TBGRAReaderLazPaintWithLayers.InternalReadLayers(str: TStream;
  Img: TFPCustomImage);
begin
  if Assigned(FLayers) then
  begin
    if CheckStreamForLayers(str) then
    begin
      LoadLayersFromStream(str, FSelectedLayerIndex, false, FLayers);
      FLayersLoaded := true;
    end;
  end;
end;

constructor TBGRAReaderLazPaintWithLayers.Create(ALayers: TBGRALayeredBitmap);
begin
  FLayersLoaded := false;
  FLayers := ALayers;
  FSelectedLayerIndex:= -1;
end;

{ TBGRAWriterLazPaintWithLayers }

function TBGRAWriterLazPaintWithLayers.GetNbLayers: integer;
begin
  if Assigned(FLayers) then
    Result:= FLayers.NbLayers
  else
    Result := 1;
end;

function TBGRAWriterLazPaintWithLayers.InternalWriteLayers(Str: TStream;
  Img: TFPCustomImage): boolean;
begin
  If Assigned(FLayers) then
  begin
    SaveLayersToStream(str, FLayers, FSelectedLayerIndex, FCompression);
    Result:=true;
  end
  else result := False;
end;

constructor TBGRAWriterLazPaintWithLayers.Create(ALayers: TBGRALayeredBitmap);
begin
  inherited Create;
  FLayers := ALayers;
  FSelectedLayerIndex:= 0;
  FCompression:= lzpRLE;
  IncludeThumbnail:= true;
end;

var AlreadyRegistered: boolean;

procedure RegisterLazPaintFormat;
begin
  if AlreadyRegistered then exit;
  RegisterLayeredBitmapReader('lzp', TBGRALazPaintImage);
  RegisterLayeredBitmapWriter('lzp', TBGRALazPaintImage);
  AlreadyRegistered:= True;
end;

end.

