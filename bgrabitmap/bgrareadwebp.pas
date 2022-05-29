// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAReadWebP;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPImage;

type
  TWebPHeader = record
    RIFFCode: array[1..4] of char;
    FileSize: LongWord;
    WebPCode: array[1..4] of char;
  end;

  { TBGRAReaderWebP }

  TBGRAReaderWebP = class(TFPCustomImageReader)
  protected
    function ReadHeader(Str: TStream): TWebPHeader;
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
  end;

implementation

uses libwebp, BGRABitmapTypes;

var
  MyLibWebPLoaded: boolean;

procedure NeedLibWebP;
begin
  if not MyLibWebPLoaded then
  begin
    if not LibWebPLoad then
      raise exception.Create('Cannot find libwebp library ('+LibWebPFilename+')');
    MyLibWebPLoaded:= true;
  end;
end;

{ TBGRAReaderWebP }

function TBGRAReaderWebP.ReadHeader(Str: TStream): TWebPHeader;
begin
  fillchar({%H-}result, sizeof({%H-}result), 0);
  Str.Read(result, sizeof(result));
  result.FileSize:= LEtoN(result.FileSize);
end;

procedure TBGRAReaderWebP.InternalRead(Str: TStream; Img: TFPCustomImage);
const
  CopySize = 65536;
var
  header: TWebPHeader;
  oldPos: Int64;
  mem, p: PByte;
  totalSize, remain: LongWord;
  toRead, w, h, x, y: integer;
  loadInto: TBGRACustomBitmap;
  pbgra: PBGRAPixel;
  ok: Boolean;
begin
  NeedLibWebP;

  oldPos := Str.Position;
  header := ReadHeader(Str);
  if (header.RIFFCode <> 'RIFF') or (header.WebPCode <> 'WEBP') then
    raise exception.Create('Invalid header');
  Str.Position:= OldPos;
  totalSize := header.FileSize+8;
  getmem(mem, totalSize);
  loadInto := nil;
  try
    p := mem;
    remain := totalSize;
    while remain > 0 do
    begin
      if remain > CopySize then toRead := CopySize else
        toRead := remain;
      Str.ReadBuffer(p^, toRead);
      inc(p, toRead);
      dec(remain, toRead);
    end;

    if WebPGetInfo(mem, totalSize, @w, @h) = 0 then
      raise exception.Create('Invalid WebP header');

    Img.SetSize(w, h);
    if Img is TBGRACustomBitmap then
      loadInto := TBGRACustomBitmap(Img)
    else
      loadInto := BGRABitmapFactory.Create(w,h);
    {$PUSH}{$WARNINGS OFF}
    if TBGRAPixel_RGBAOrder then
      ok := WebPDecodeRGBAInto(mem, totalSize, loadInto.DataByte, loadInto.RowSize*h, loadInto.RowSize)<>nil
    else
      ok := WebPDecodeBGRAInto(mem, totalSize, loadInto.DataByte, loadInto.RowSize*h, loadInto.RowSize)<>nil;
    {$POP}
    loadInto.InvalidateBitmap;
    if not ok then raise exception.Create('Error decoding WebP');
    if loadInto.LineOrder = riloBottomToTop then loadInto.VerticalFlip;
    if Img <> loadInto then
    begin
      for y := 0 to h-1 do
      begin
        pbgra := loadInto.ScanLine[y];
        for x := 0 to w-1 do
        begin
          Img.Colors[x,y] := pbgra^.ToFPColor;
          inc(pbgra);
        end;
      end;
    end;
  finally
    if Assigned(loadInto) and (loadInto <> Img) then loadInto.Free;
    freemem(mem);
  end;
end;

function TBGRAReaderWebP.InternalCheck(Str: TStream): boolean;
var
  oldPos: Int64;
  header: TWebPHeader;
begin
  oldPos := Str.Position;
  try
    header := ReadHeader(Str);
    result := (header.RIFFCode = 'RIFF') and (header.WebPCode = 'WEBP') and
       (header.FileSize <= $FFFFFFF6) and (header.FileSize <= Str.Size-Str.Position+4);
  finally
    Str.Position:= OldPos;
  end;
end;

initialization

  DefaultBGRAImageReader[ifWebP] := TBGRAReaderWebP;

finalization

  if MyLibWebPLoaded then
  begin
    LibWebPUnload;
    MyLibWebPLoaded:= false;
  end;

end.

