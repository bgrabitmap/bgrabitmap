// SPDX-License-Identifier: LGPL-3.0-linking-exception
{*****************************************************************************}
{
  2023-06  - Massimo Magnano
           - added Resolution support
}
{*****************************************************************************}

{ Provides reader for JPEG image format }
unit BGRAReadJpeg;

{$mode objfpc}{$H+}

interface

uses
  {$IF FPC_FULLVERSION<30203}
   JPEGLib, JdAPImin, JDataSrc, JdAPIstd, JmoreCfg,
  {$ENDIF}
  BGRABitmapTypes, Classes, SysUtils, FPReadJPEG, FPImage;

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
  { Reader for JPEG image format }
  TBGRAReaderJpeg = class(TFPReaderJPEG)
    constructor Create; override;
  protected
    {$IF FPC_FULLVERSION<30203}
    CompressInfo: jpeg_decompress_struct;
    FError: jpeg_error_mgr;

    procedure ReadResolutionValues(Img: TFPCustomImage); virtual;
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    {$ENDIF}

    function InternalCheck(Str: TStream): boolean; override;
  end;

implementation

{$IF FPC_FULLVERSION<30203}
function density_unitToResolutionUnit(Adensity_unit: UINT8): TResolutionUnit;
begin
  Case Adensity_unit of
  1: Result :=ruPixelsPerInch;
  2: Result :=ruPixelsPerCentimeter;
  else Result :=ruNone;
  end;
end;

var
  jpeg_std_error: jpeg_error_mgr;
{$ENDIF}

{ TBGRAReaderJpeg }

constructor TBGRAReaderJpeg.Create;
begin
  inherited Create;
  Performance := jpBestQuality;
end;

{$IF FPC_FULLVERSION<30203}
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
end;
{$ENDIF}

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

{$IF FPC_FULLVERSION<30203}
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
{$ENDIF}

initialization
{$IF FPC_FULLVERSION<30203}
  with jpeg_std_error do begin
    error_exit:=@JPEGError;
    emit_message:=@EmitMessage;
    output_message:=@OutputMessage;
    format_message:=@FormatMessage;
    reset_error_mgr:=@ResetErrorMgr;
  end;
{$ENDIF}

  DefaultBGRAImageReader[ifJpeg] := TBGRAReaderJpeg;

end.

