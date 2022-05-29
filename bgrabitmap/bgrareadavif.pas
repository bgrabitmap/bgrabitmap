// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAReadAvif;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPImage;

type

  { TBGRAReaderAvif }

  TBGRAReaderAvif = class(TFPCustomImageReader)
  protected
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
  end;

implementation

uses avifbgra,libavif, BGRABitmapTypes, BGRABitmap;

var
  MyLibAvifLoaded: boolean;

procedure NeedLibAvif;
begin
  if not MyLibAvifLoaded then
  begin
    if not LibAvifLoad then
      raise exception.Create('Cannot find libavif library ('+LibAvifFilename+')');
    MyLibAvifLoaded:= true;
  end;
end;

{ TBGRAReaderAvif }

procedure TBGRAReaderAvif.InternalRead(Str: TStream; Img: TFPCustomImage);
var
  w, h, x, y: integer;
  loadInto: TBGRACustomBitmap;
  pbgra: PBGRAPixel;
begin
  NeedLibAvif;
  if not InternalCheck(Str) then
    raise exception.Create('Invalid avif header');
  try
    if Img is TBGRACustomBitmap then
      loadInto := TBGRACustomBitmap(Img)
    else
      loadInto := BGRABitmapFactory.Create(Img.Width,Img.Height);
    avifLoadFromStream(Str,TBGRABitmap(loadInto));
    //if loadInto.LineOrder = riloBottomToTop then loadInto.VerticalFlip;
    if Img <> loadInto then
    begin
      h:=loadInto.Height;
      w:=loadInto.Width;
      Img.SetSize(w, h);
      Img.SetSize(w, h);
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
  end;
end;

function TBGRAReaderAvif.InternalCheck(Str: TStream): boolean;
var
  oldPos: Int64;
  header: array[0..11] of char;
begin
  oldPos := Str.Position;
  try
    fillchar({%H-}header, sizeof({%H-}header), 0);
    Str.Read(header, sizeof(header));
    result:=AvifValidateHeaderSignature(@header[0]);
  finally
    Str.Position:= OldPos;
  end;
end;

initialization

  DefaultBGRAImageReader[ifAvif] := TBGRAReaderAvif;

finalization

  if MyLibAvifLoaded then
  begin
    LibAvifUnload;
    MyLibAvifLoaded:= false;
  end;

end.

