// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Provides readers for icons and cursors }
unit BGRAReadIco;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

{$IFDEF BGRABITMAP_USE_LCL}
  {$DEFINE USE_LCL_ICON_READER}
{$ENDIF}

interface

uses
  BGRAClasses, SysUtils, FPimage{$IFDEF USE_LCL_ICON_READER}, Graphics{$ENDIF};

type
  {$IFDEF USE_LCL_ICON_READER}TCustomIconClass = class of TCustomIcon;{$ENDIF}
  TByteSet = set of byte;

  { Image reader for ICO and CUR format }

  { TBGRAReaderIcoOrCur }

  TBGRAReaderIcoOrCur = class(TFPCustomImageReader)
  protected
    procedure InternalRead({%H-}Str: TStream; {%H-}Img: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
    function ExpectedMagic: TByteSet; virtual; abstract;
    class function InternalSize(Str: TStream): TPoint; override;
    {$IFDEF USE_LCL_ICON_READER}class function LazClass: TCustomIconClass; virtual; abstract;{$ENDIF}
  public
    WantedWidth, WantedHeight : integer;
  end;

  { Image reader for ICO format }
  TBGRAReaderIco = class(TBGRAReaderIcoOrCur)
  protected
    function ExpectedMagic: TByteSet; override;
    {$IFDEF USE_LCL_ICON_READER}class function LazClass: TCustomIconClass; override;{$ENDIF}
  end;

  { Image reader for CUR format }
  TBGRAReaderCur = class(TBGRAReaderIcoOrCur)
  protected
    function ExpectedMagic: TByteSet; override;
    {$IFDEF USE_LCL_ICON_READER}class function LazClass: TCustomIconClass; override;{$ENDIF}
  end;

implementation

uses BGRABitmapTypes{$IFNDEF USE_LCL_ICON_READER}, BGRAIconCursor{$ENDIF};

{ TBGRAReaderCur }

function TBGRAReaderCur.ExpectedMagic: TByteSet;
begin
  result := [2];
end;

{$IFDEF USE_LCL_ICON_READER}class function TBGRAReaderCur.LazClass: TCustomIconClass;
begin
  result := TCursorImage;
end;{$ENDIF}

{ TBGRAReaderIco }

function TBGRAReaderIco.ExpectedMagic: TByteSet;
begin
  result := [1,2];
end;

{$IFDEF USE_LCL_ICON_READER}class function TBGRAReaderIco.LazClass: TCustomIconClass;
begin
  result := TIcon;
end;{$ENDIF}

{$IFDEF USE_LCL_ICON_READER}function FindBestIndex(AIcon: TCustomIcon;
  AWantedWidth, AWantedHeight: integer): integer;
var
  i, bestIdx: integer;
  width, height, bestWidth, bestHeight: word;
  format, maxFormat: TPixelFormat;
begin
  bestIdx := -1;
  bestHeight := 0;
  bestWidth := 0;
  maxFormat := pfDevice;
  for i := 0 to AIcon.Count-1 do
  begin
    AIcon.GetDescription(i, format, height, width);
    if (bestIdx = -1) or
      (abs(height-AWantedHeight) + abs(width-AWantedWidth)
      < abs(bestHeight-AWantedHeight) + abs(bestWidth-AWantedWidth)) or
      ((height = bestHeight) and (width = bestWidth) and (format > maxFormat)) then
    begin
      bestIdx := i;
      bestHeight := height;
      bestWidth := width;
      maxFormat := format;
    end;
  end;
  if (bestWidth = 0) or (bestHeight = 0) then
    bestIdx := -1;
  result := bestIdx;
end;{$ENDIF}

{ TBGRAReaderIcoOrCur }

procedure TBGRAReaderIcoOrCur.InternalRead(Str: TStream; Img: TFPCustomImage);
{$IFDEF USE_LCL_ICON_READER}
var ico: TCustomIcon; bestIdx: integer;
    compWidth,compHeight: integer;
begin
  if WantedWidth > 0 then compWidth:= WantedWidth else compWidth:= 65536;
  if WantedHeight > 0 then compHeight:= WantedHeight else compHeight:= 65536;
  ico := LazClass.Create;
  try
    ico.LoadFromStream(Str);
    bestIdx := FindBestIndex(ico, compWidth, compHeight);
    if bestIdx = -1 then
      raise exception.Create('No adequate icon found') else
    begin
      ico.Current := bestIdx;
      Img.Assign(ico);
    end;
  finally
    ico.free;
  end;
end;
{$ELSE}
var icoCur: TBGRAIconCursor;
    compWidth,compHeight: integer;
    bmp: TBGRACustomBitmap;
begin
  if WantedWidth > 0 then compWidth:= WantedWidth else compWidth:= 65536;
  if WantedHeight > 0 then compHeight:= WantedHeight else compHeight:= 65536;
  icoCur := TBGRAIconCursor.Create(Str);
  try
    bmp := icoCur.GetBestFitBitmap(compWidth,compHeight);
    try
      Img.Assign(bmp);
    finally
      bmp.Free;
    end;
  finally
    icoCur.Free;
  end;
end;
{$ENDIF}

function TBGRAReaderIcoOrCur.InternalCheck(Str: TStream): boolean;
var {%H-}magic: packed array[0..5] of byte;
    oldPos: int64;
begin
  oldPos := str.Position;
  result := (str.Read({%H-}magic,sizeof(magic)) = sizeof(magic));
  str.Position:= oldPos;
  if result then
    result := (magic[0] = $00) and (magic[1] = $00) and (magic[2] in ExpectedMagic) and (magic[3] = $00) and
             (magic[4] + (magic[5] shl 8) > 0);
end;

class function TBGRAReaderIcoOrCur.InternalSize(Str: TStream): TPoint;
{$IFDEF USE_LCL_ICON_READER}
var ico: TCustomIcon; bestIdx: integer;
begin
  result := Point(0, 0);
  ico := LazClass.Create;
  try
    ico.LoadFromStream(Str);
    bestIdx := FindBestIndex(ico, 65536, 65536);
    if bestIdx <> -1 then
    begin
      ico.Current := bestIdx;
      result := Point(ico.Width, ico.Height);
    end;
  finally
    ico.free;
  end;
end;
{$ELSE}
var
  icoCur: TBGRAIconCursor;
  bestIdx: Integer;
begin
  result := Point(0, 0);
  icoCur := TBGRAIconCursor.Create(Str);
  try
    bestIdx := icoCur.GetBestFitIndex(65536,65536);
    if bestIdx <> -1 then
      result := Point(icoCur.Width[bestIdx], icoCur.Height[bestIdx]);
  finally
    icoCur.Free;
  end;
end;{$ENDIF}

initialization
  BGRARegisterImageReader(ifIco, TBGRAReaderIco, True, 'Icon Format', 'ico');
  BGRARegisterImageReader(ifCur, TBGRAReaderCur, True, 'Cursor Format', 'cur');

end.

