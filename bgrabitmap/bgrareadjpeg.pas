// SPDX-License-Identifier: LGPL-3.0-linking-exception
{*****************************************************************************}
{
  2023-06  - Massimo Magnano
           - added Resolution support
}
{*****************************************************************************}
unit BGRAReadJpeg;

{$mode objfpc}{$H+}

//issue #40327 TFPReaderJPEG.Finfo inaccesible to descendant classes
//if your version of fcl-image is older than issue #40327 solution comment the next line
//{$define HAVE_COMPRESSINFO}

interface

uses
  BGRAClasses, BGRABitmapTypes, Classes, Types, SysUtils, FPReadJPEG, FPImage,
  JPEGLib, JdAPImin, JDataSrc, JdAPIstd, JmoreCfg;

type
  TJPEGScale = FPReadJPEG.TJPEGScale;
  TJPEGReadPerformance = FPReadJPEG.TJPEGReadPerformance;

const
  jsFullSize = FPReadJPEG.jsFullSize;
  jsHalf = FPReadJPEG.jsHalf;
  jsQuarter = FPReadJPEG.jsQuarter;
  jsEighth = FPReadJPEG.jsEighth;

  jpBestQuality = FPReadJPEG.jpBestQuality;
  jpBestSpeed = FPReadJPEG.jpBestSpeed;

type
  { TBGRAReaderJpeg }

  TBGRAReaderJpeg = class(TFPReaderJPEG)
    constructor Create; override;
  protected
    {$ifndef HAVE_COMPRESSINFO}
    CompressInfo: jpeg_decompress_struct;
    FError: jpeg_error_mgr;
    {$endif}

    procedure ReadResolutionValues(Img: TFPCustomImage); virtual;
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
  end;

function density_unitToResolutionUnit(Adensity_unit: UINT8): TResolutionUnit;
function ResolutionUnitTodensity_unit(AResolutionUnit: TResolutionUnit): UINT8;

implementation

function density_unitToResolutionUnit(Adensity_unit: UINT8): TResolutionUnit;
begin
  Case Adensity_unit of
  1: Result :=ruPixelsPerInch;
  2: Result :=ruPixelsPerCentimeter;
  else Result :=ruNone;
  end;
end;

function ResolutionUnitTodensity_unit(AResolutionUnit: TResolutionUnit): UINT8;
begin
  Case AResolutionUnit of
  ruPixelsPerInch: Result :=1;
  ruPixelsPerCentimeter: Result :=2;
  else Result :=0;
  end;
end;

{$ifndef HAVE_COMPRESSINFO}
var
  jpeg_std_error: jpeg_error_mgr;

{$endif}

{ TBGRAReaderJpeg }

constructor TBGRAReaderJpeg.Create;
begin
  inherited Create;
  Performance := jpBestQuality;
end;

procedure TBGRAReaderJpeg.ReadResolutionValues(Img: TFPCustomImage);
begin
  if (Img is TCustomUniversalBitmap) then
  with TCustomUniversalBitmap(Img) do
  begin
    ResolutionUnit:=density_unitToResolutionUnit(CompressInfo.density_unit);
    ResolutionX :=CompressInfo.X_density;
    ResolutionY :=CompressInfo.Y_density;
  end;
end;

procedure TBGRAReaderJpeg.InternalRead(Str: TStream; Img: TFPCustomImage);
begin
  inherited InternalRead(Str, Img);

  {$ifdef HAVE_COMPRESSINFO}
  ReadResolutionValues;
  {$else}
  //I'm forced to re-read Compress Info because it's not declared as Protected see issue #40327
  FillChar(CompressInfo,SizeOf(CompressInfo),0);

  if (Str.Size>0) then
  begin
    FError:=jpeg_std_error;
    CompressInfo.err := @FError;
    jpeg_CreateDecompress(@CompressInfo, JPEG_LIB_VERSION, SizeOf(CompressInfo));
    try
       Str.Position:=0;
       jpeg_stdio_src(@CompressInfo, @Str);
       jpeg_read_header(@CompressInfo, false);
       ReadResolutionValues(Img);
    finally
       jpeg_Destroy_Decompress(@CompressInfo);
    end;
  end;
  {$endif}
end;

function TBGRAReaderJpeg.InternalCheck(Str: TStream): boolean;
var {%H-}magic: packed array[0..3] of byte;
  OldPos,BytesRead:int64;
begin
  Result:=false;
  if Str=nil then exit;
  OldPos:= str.Position;
  BytesRead := str.Read({%H-}magic,sizeof(magic));
  str.Position:=OldPos;
  if BytesRead<>sizeof(magic) then exit;
  if (magic[0] = $ff) and (magic[1] = $d8) and (magic[2] = $ff) and (magic[3] >= $c0) then result := true;
end;

{$ifndef HAVE_COMPRESSINFO}
procedure JPEGError(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
  raise Exception.CreateFmt('JPEG error',[CurInfo^.err^.msg_code]);
end;

procedure EmitMessage(CurInfo: j_common_ptr; msg_level: Integer);
begin
  if CurInfo=nil then exit;
  if msg_level=0 then ;
end;

procedure OutputMessage(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
end;

procedure FormatMessage(CurInfo: j_common_ptr; var buffer: string);
begin
  if CurInfo=nil then exit;
  {$ifdef FPC_Debug_Image}
     writeln('FormatMessage ',buffer);
  {$endif}
end;

procedure ResetErrorMgr(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
  CurInfo^.err^.num_warnings := 0;
  CurInfo^.err^.msg_code := 0;
end;
{$endif}

initialization
{$ifndef HAVE_COMPRESSINFO}
  with jpeg_std_error do begin
    error_exit:=@JPEGError;
    emit_message:=@EmitMessage;
    output_message:=@OutputMessage;
    format_message:=@FormatMessage;
    reset_error_mgr:=@ResetErrorMgr;
  end;
{$endif}

  DefaultBGRAImageReader[ifJpeg] := TBGRAReaderJpeg;

end.

