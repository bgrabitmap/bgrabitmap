// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAThumbnail;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

uses
  BGRAClasses, SysUtils, BGRABitmap, BGRABitmapTypes, FPimage;

function GetBitmapThumbnail(ABitmap: TBGRACustomBitmap; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil; AVerticalShrink: single = 1; AHorizShrink: single = 1): TBGRABitmap; overload;
function GetBitmapThumbnail(ABitmap: TBGRACustomBitmap; AFormat: TBGRAImageFormat; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil; AVerticalShrink: single = 1; AHorizShrink: single = 1): TBGRABitmap; overload;
function GetFileThumbnail(AFilenameUTF8: string; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetStreamThumbnail(AStream: TStream; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ASuggestedExtensionUTF8: string = ''; ADest: TBGRABitmap= nil): TBGRABitmap; overload;
function GetStreamThumbnail(AStream: TStream; AReader: TFPCustomImageReader; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap; overload;

function GetOpenRasterThumbnail(AStream: TStream; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetLazPaintThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetPhoxoThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetJpegThumbnail(AStream: TStream; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetPsdThumbnail(AStream: TStream; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetPngThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetPaintDotNetThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetBmpThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetIcoThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetCurThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;

function GetPcxThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetTargaThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetTiffThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetGifThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetXwdThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetXPixMapThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetBmpMioMapThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;

procedure DrawThumbnailCheckers(bmp: TBGRABitmap; ARect: TRect; AIconCheckers: boolean = false);
procedure DrawThumbnailCheckers(bmp: TBGRABitmap; ARect: TRect; AIconCheckers: boolean; AScale: single);

var
  ImageCheckersColor1,ImageCheckersColor2  : TBGRAPixel;
  IconCheckersColor1,IconCheckersColor2  : TBGRAPixel;
  CheckersScale: single = 1;

implementation

uses base64, BGRAUTF8,
     DOM, XMLRead, BGRAReadJPEG, BGRAReadPng, BGRAReadGif, BGRAReadBMP,
     BGRAReadPSD, BGRAReadIco, UnzipperExt, BGRAReadLzp;

procedure DrawThumbnailCheckers(bmp: TBGRABitmap; ARect: TRect;
  AIconCheckers: boolean);
begin
  DrawThumbnailCheckers(bmp, ARect,  AIconCheckers, CheckersScale);
end;

procedure DrawThumbnailCheckers(bmp: TBGRABitmap; ARect: TRect; AIconCheckers: boolean; AScale: single);
begin
  if AIconCheckers then
    bmp.DrawCheckers(ARect, IconCheckersColor1, IconCheckersColor2, round(8*AScale), round(8*AScale))
  else
    bmp.DrawCheckers(ARect, ImageCheckersColor1, ImageCheckersColor2, round(8*AScale), round(8*AScale));
end;

function InternalGetBitmapThumbnail(ABitmap: TBGRACustomBitmap; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean;
  ADest: TBGRABitmap; AVerticalShrink: single = 1; AHorizShrink: single = 1; AShowHotSpot: boolean = false; ADarkCheckers: boolean = false): TBGRABitmap;
var
  factorX, factorY, factor: single;
  xIcon,yIcon,wIcon,hIcon: Integer;
  hotspot: TPoint;
begin
  result := nil;
  try
    if (ABitmap <> nil) and (ABitmap.Width <> 0) and (ABitmap.Height <> 0) then
    begin
      If Assigned(ADest) then
      begin
        result := ADest;
        result.SetSize(AWidth,AHeight);
        result.Fill(ABackColor);
      end else
        result := TBGRABitmap.Create(AWidth,AHeight,ABackColor);
      factorX := result.Width/(ABitmap.Width*AHorizShrink);
      factorY := result.Height/(ABitmap.Height*AVerticalShrink);
      if factorX < factorY then factor := factorX else factor := factorY;
      wIcon := round(ABitmap.Width*AHorizShrink*factor);
      if wIcon = 0 then wIcon := 1;
      hIcon := round(ABitmap.Height*AVerticalShrink*factor);
      if hIcon = 0 then hIcon := 1;
      xIcon:= (result.Width-wIcon) div 2;
      yIcon:= (result.Height-hIcon) div 2;
      if ACheckers then DrawThumbnailCheckers(result,Rect(xIcon,yIcon,xIcon+wIcon,yIcon+hIcon),ADarkCheckers,CheckersScale);
      if AShowHotSpot and (wIcon > 0) and (hIcon > 0) then
      begin
        hotspot := Point(xIcon+ABitmap.HotSpot.X*wIcon div ABitmap.Width,yIcon+ABitmap.HotSpot.Y*hIcon div ABitmap.Height);
        result.HorizLine(xIcon,hotspot.y-1,xIcon+wIcon-1,CSSLime,dmDrawWithTransparency);
        result.HorizLine(xIcon,hotspot.y,xIcon+wIcon-1,CSSLime,dmDrawWithTransparency);
        result.HorizLine(xIcon,hotspot.y+1,xIcon+wIcon-1,CSSLime,dmDrawWithTransparency);
        result.VertLine(hotspot.x-1,yIcon,yIcon+hIcon-1,CSSLime,dmDrawWithTransparency);
        result.VertLine(hotspot.x,yIcon,yIcon+hIcon-1,CSSLime,dmDrawWithTransparency);
        result.VertLine(hotspot.x+1,yIcon,yIcon+hIcon-1,CSSLime,dmDrawWithTransparency);
      end;
      if (ABackColor.alpha <> 0) or ACheckers then
        result.StretchPutImage(Rect(xIcon,yIcon,xIcon+wIcon,yIcon+hIcon),ABitmap,dmDrawWithTransparency) else
        result.StretchPutImage(Rect(xIcon,yIcon,xIcon+wIcon,yIcon+hIcon),ABitmap,dmSet);
      if AShowHotSpot and (wIcon > 0) and (hIcon > 0) then
      begin
        result.HorizLine(xIcon,yIcon+ABitmap.HotSpot.Y*hIcon div ABitmap.Height,xIcon+wIcon-1,BGRA(255,0,255,96),dmDrawWithTransparency);
        result.VertLine(xIcon+ABitmap.HotSpot.X*wIcon div ABitmap.Width,yIcon,yIcon+hIcon-1,BGRA(255,0,255,96),dmDrawWithTransparency);
      end;
    end;
  except
  end;
end;

function GetBitmapThumbnail(ABitmap: TBGRACustomBitmap; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap = nil;
  AVerticalShrink: single = 1; AHorizShrink: single = 1): TBGRABitmap;
begin
  result := InternalGetBitmapThumbnail(ABitmap,AWidth,AHeight,ABackColor,ACheckers,ADest,AVerticalShrink,AHorizShrink,
                                       false,false);
end;

function GetBitmapThumbnail(ABitmap: TBGRACustomBitmap; AFormat: TBGRAImageFormat; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap = nil; AVerticalShrink: single = 1; AHorizShrink: single = 1): TBGRABitmap;
begin
  result := InternalGetBitmapThumbnail(ABitmap,AWidth,AHeight,ABackColor,ACheckers,ADest,AVerticalShrink,AHorizShrink,
                                       AFormat = ifCur, AFormat in[ifCur,ifIco]);

end;

function GetFileThumbnail(AFilenameUTF8: string; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var stream: TFileStreamUTF8;
begin
  result := nil;
  try
    stream := TFileStreamUTF8.Create(AFilenameUTF8,fmOpenRead or fmShareDenyWrite);
  except
    exit;
  end;
  try
    result := GetStreamThumbnail(stream, AWidth,AHeight,ABackColor,ACheckers,ExtractFileExt(AFilenameUTF8),ADest);
  finally
    stream.free;
  end;
end;

function GetStreamThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ASuggestedExtensionUTF8: string;
  ADest: TBGRABitmap): TBGRABitmap;
var
  ff: TBGRAImageFormat;
  reader: TFPCustomImageReader;
begin
  ff := DetectFileFormat(AStream,ASuggestedExtensionUTF8);
  case ff of
    ifJpeg: result := GetJpegThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifIco: result := GetIcoThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifCur: result := GetCurThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifPaintDotNet: result := GetPaintDotNetThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifLazPaint: result := GetLazPaintThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifOpenRaster: result := GetOpenRasterThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifPhoxo: result := GetPhoxoThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifPsd: result := GetPsdThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    else
    begin
      if (ff = ifUnknown) or (DefaultBGRAImageReader[ff] = nil) then
        result := nil
      else
      begin
        result := nil;
        reader := nil;
        try
          reader := CreateBGRAImageReader(ff);
          result := GetStreamThumbnail(AStream, reader, AWidth, AHeight, ABackColor, ACheckers, ADest);
        finally
          reader.Free;
        end;
      end;
    end;
  end;
end;

function GetStreamThumbnail(AStream: TStream; AReader: TFPCustomImageReader;
  AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean;
  ADest: TBGRABitmap): TBGRABitmap;
var
  bmp: TBGRACustomBitmap;
  AOriginalWidth, AOriginalHeight: integer;
begin
  if AReader is TBGRAImageReader then
  begin
    bmp := nil;
    try
      bmp := TBGRAImageReader(AReader).GetBitmapDraft(AStream, AWidth,AHeight, AOriginalWidth,AOriginalHeight);
      if Assigned(bmp) and (bmp.Height <> 0) and (bmp.Width <> 0) then
        result := GetBitmapThumbnail(bmp, AWidth, AHeight, ABackColor, ACheckers, ADest,
                    AOriginalHeight/bmp.Height, AOriginalWidth/bmp.Width);
    except
      result := nil;
    end;
    bmp.free;
    exit;
  end;

  bmp := TBGRABitmap.Create;
  try
    bmp.LoadFromStream(AStream, AReader);
  except
    FreeAndNil(bmp);
  end;
  if bmp = nil then
    result := nil
  else
  begin
    result := GetBitmapThumbnail(bmp, AWidth, AHeight, ABackColor, ACheckers, ADest);
    bmp.Free;
  end;
end;

function GetOpenRasterThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  unzip: TUnzipperStreamUtf8;
  png: TMemoryStream;
begin
  result := nil;
  unzip := TUnzipperStreamUtf8.Create;
  try
    unzip.InputStream := AStream;
    png := TMemoryStream.Create;
    try
      if unzip.UnzipFileToStream('Thumbnails\thumbnail.png', png, False) then
      begin
        png.Position:= 0;
        result := GetPngThumbnail(png,AWidth,AHeight,ABackColor,ACheckers,ADest);
      end else
      begin
        png.Clear;
        if unzip.UnzipFileToStream('mergedimage.png', png, False) then
        begin
          png.Position:= 0;
          result := GetPngThumbnail(png,AWidth,AHeight,ABackColor,ACheckers,ADest);
        end;
      end;
    finally
      png.Free;
    end;
  except
  end;
  unzip.Free;
end;

function GetLazPaintThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TBGRAReaderLazPaint;
begin
  reader:= TBGRAReaderLazPaint.Create;
  reader.WantThumbnail := true;
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers,ADest);
  reader.Free;
end;

function GetPhoxoThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TFPCustomImageReader;
begin
  if DefaultBGRAImageReader[ifPhoxo] = nil then
    result := nil
  else
  begin
    reader := CreateBGRAImageReader(ifPhoxo);
    result := GetStreamThumbnail(AStream, reader, AWidth,AHeight,ABackColor,ACheckers,ADest);
    reader.Free;
  end;
end;

function GetJpegThumbnail(AStream: TStream; AWidth, AHeight: integer
  ; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  jpeg: TBGRAReaderJpeg;
begin
  jpeg := TBGRAReaderJpeg.Create;
  jpeg.Performance := jpBestSpeed;
  jpeg.MinWidth := AWidth;
  jpeg.MinHeight := AHeight;
  result := GetStreamThumbnail(AStream, jpeg, AWidth,AHeight,ABackColor,ACheckers,ADest);
  jpeg.Free;
end;

function GetPsdThumbnail(AStream: TStream; AWidth, AHeight: integer
  ; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  psd: TBGRAReaderPSD;
  bmp: TBGRABitmap;
begin
  psd:= TBGRAReaderPSD.Create;
  psd.MinifyHeight:= AHeight;
  bmp := TBGRABitmap.Create;
  try
    bmp.LoadFromStream(AStream, psd);
  except
    FreeAndNil(bmp);
  end;
  if bmp = nil then
    result := nil
  else
  begin
    result := GetBitmapThumbnail(bmp, AWidth, AHeight, ABackColor, ACheckers, ADest, psd.Height/bmp.Height);
    bmp.Free;
  end;
  psd.Free;
end;

function GetPngThumbnail(AStream: TStream; AWidth, AHeight: integer;
    ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
var
  pngFormat: TBGRAReaderPNG;
begin
  pngFormat:= TBGRAReaderPNG.Create;
  result:= GetStreamThumbnail(AStream, pngFormat, AWidth,AHeight, ABackColor, ACheckers, ADest);
  pngFormat.Free;
end;

function GetPaintDotNetThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  {%H-}magic: packed array[0..6] of byte;
  xmlHeader: TMemoryStream;
  xmlHeaderSize: longint;
  doc: TXMLDocument;
  custom,thumb,pngNode: TDOMNode;
  png64: TStringStream;
  decode64: TBase64DecodingStream;
begin
  result := nil;
  if AStream.Read({%H-}magic,sizeof(magic)) <> sizeof(magic) then exit;
  if chr(magic[0])+chr(magic[1])+chr(magic[2])+chr(magic[3]) <> 'PDN3' then exit;
  xmlHeaderSize := magic[4] + (magic[5] shl 8) + (magic[6] shl 16);
  if xmlHeaderSize >= 10*1024*1024 then exit;
  xmlHeader:= TMemoryStream.Create;
  try
    if xmlHeader.CopyFrom(AStream,xmlHeaderSize) <> xmlHeaderSize then
    begin
      xmlHeader.Free;
      exit;
    end;
  except
    xmlHeader.Free;
    exit;
  end;
  xmlHeader.Position := 0;
  try
    XMLRead.ReadXMLFile(doc, xmlHeader);
  except
    xmlHeader.Free;
    exit;
  end;
  xmlHeader.Free;
  try
    custom := doc.DocumentElement.FindNode('custom');
    if Assigned(custom) then
    begin
      thumb := custom.FindNode('thumb');
      if Assigned(thumb) then
      begin
        pngNode := thumb.Attributes.GetNamedItem('png');
        if Assigned(pngNode) then
        begin
          png64 := TStringStream.Create(string(pngNode.NodeValue));
          try
            png64.Position := 0;
            decode64 := TBase64DecodingStream.Create(png64);
            try
              result := GetPngThumbnail(decode64,AWidth,AHeight,ABackColor,ACheckers, ADest);
            finally
              decode64.Free;
            end;
          finally
            png64.free;
          end;
        end;
      end;
    end;
  except
  end;
  doc.Free;
end;

function GetBmpThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  bmpFormat: TBGRAReaderBMP;
begin
  bmpFormat:= TBGRAReaderBMP.Create;
  result:= GetStreamThumbnail(AStream, bmpFormat, AWidth,AHeight, ABackColor, ACheckers, ADest);
  bmpFormat.Free;
end;

function GetIcoThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TBGRAReaderIco;
  icoBmp: TBGRABitmap;
begin
  result := nil;
  reader := TBGRAReaderIco.Create;
  reader.WantedWidth:= AWidth;
  reader.WantedHeight:= AHeight;
  icoBmp := TBGRABitmap.Create;
  try
    icoBmp.LoadFromStream(AStream, reader);
    result := GetBitmapThumbnail(icoBmp, ifIco, AWidth, AHeight, ABackColor, ACheckers, ADest);
  except
  end;
  icoBmp.Free;
  reader.Free;
end;

function GetCurThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TBGRAReaderCur;
  icoBmp: TBGRABitmap;
begin
  result := nil;
  reader := TBGRAReaderCur.Create;
  reader.WantedWidth:= AWidth;
  reader.WantedHeight:= AHeight;
  icoBmp := TBGRABitmap.Create;
  try
    icoBmp.LoadFromStream(AStream, reader);
    result := GetBitmapThumbnail(icoBmp, ifCur, AWidth, AHeight, ABackColor, ACheckers, ADest);
  except
  end;
  icoBmp.Free;
  reader.Free;
end;

function GetPcxThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TFPCustomImageReader;
begin
  reader:= CreateBGRAImageReader(ifPcx);
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers,ADest);
  reader.Free;
end;

function GetTargaThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TFPCustomImageReader;
begin
  reader:= CreateBGRAImageReader(ifTarga);
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers, ADest);
  reader.Free;
end;

function GetTiffThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TFPCustomImageReader;
begin
  reader:= CreateBGRAImageReader(ifTiff);
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers,ADest);
  reader.Free;
end;

function GetGifThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TFPCustomImageReader;
begin
  reader:= CreateBGRAImageReader(ifGif);
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers, ADest);
  reader.Free;
end;

function GetXwdThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TFPCustomImageReader;
begin
  reader:= CreateBGRAImageReader(ifXwd);
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers, ADest);
  reader.Free;
end;

function GetXPixMapThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TFPCustomImageReader;
begin
  reader:= CreateBGRAImageReader(ifXPixMap);
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers,ADest);
  reader.Free;
end;

function GetBmpMioMapThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TFPCustomImageReader;
begin
  reader:= CreateBGRAImageReader(ifBmpMioMap);
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers,ADest);
  reader.Free;
end;

initialization

  IconCheckersColor1 := BGRA(140,180,180);
  IconCheckersColor2 := BGRA(80,140,140);

  ImageCheckersColor1 := BGRA(255,255,255);
  ImageCheckersColor2 := BGRA(220,220,220);

end.
