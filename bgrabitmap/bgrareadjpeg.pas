// SPDX-License-Identifier: LGPL-3.0-linking-exception
{*****************************************************************************}
{
  2023-06  - Massimo Magnano added Resolution support
  2025-04  - Massimo Magnano added GetJpegInfo class function
}
{*****************************************************************************}

{ Provides reader for JPEG image format }
unit BGRAReadJpeg;

{$mode objfpc}{$H+}

interface

uses
  Types, JPEGLib,
  BGRABitmapTypes, Classes, SysUtils, FPReadJPEG, FPImage;

const
  jsFullSize = FPReadJPEG.jsFullSize;
  jsHalf = FPReadJPEG.jsHalf;
  jsQuarter = FPReadJPEG.jsQuarter;
  jsEighth = FPReadJPEG.jsEighth;

  jpBestQuality = FPReadJPEG.jpBestQuality;
  jpBestSpeed = FPReadJPEG.jpBestSpeed;

type
  TJPEGScale = FPReadJPEG.TJPEGScale;
  TJPEGReadPerformance = FPReadJPEG.TJPEGReadPerformance;

  TJPEGInfo = record
    Width, Height: Integer;
    {$IF FPC_FULLVERSION>=30301}
    Orientation: TExifOrientation;
    {$ENDIF}
    ProgressiveEncoding,
    GrayScale: Boolean;
    ResolutionUnit: TResolutionUnit;
    ResolutionX,
    ResolutionY: Single;
  end;

  { Reader for JPEG image format }

  { TBGRAReaderJpeg }

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

  public
    class function GetJpegInfo(AFileName: String; var AInfo: TJpegInfo): Boolean; overload;
    class function GetJpegInfo(Str: TStream; var AInfo: TJpegInfo): Boolean; overload;

  published
    //property CompressInfo : jpeg_decompress_struct; rw
    {$IF FPC_FULLVERSION>=30301}
    property Orientation;         //: TExifOrientation; r
    {$ENDIF}
    property ProgressiveEncoding; //: boolean;  r
    property GrayScale;           //: boolean;  r
    property Smoothing;           //: boolean;  rw
    property Performance;         //: TJPEGReadPerformance; rw
    property Scale;               //: TJPEGScale; rw
    property MinWidth;            //: integer rw
    property MinHeight;           //: integer rw
  end;

implementation

uses  JdAPImin, JDataSrc, JdAPIstd, JmoreCfg;


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
var
  {%H-}magic: packed array[0..3] of byte;
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

class function TBGRAReaderJpeg.GetJpegInfo(AFileName: String; var AInfo: TJpegInfo): Boolean;
var
   AStr: TFileStream;

begin
   try
      Result:= False;
      AStr:= TFileStream.Create(AFileName, fmOpenRead);
      Result:= GetJpegInfo(AStr, AInfo);

   finally
      AStr.Free;
   end;
end;

class function TBGRAReaderJpeg.GetJpegInfo(Str: TStream; var AInfo: TJpegInfo): Boolean;
var
  {%H-}magic: packed array[0..3] of byte;
  OldPos,BytesRead: int64;
  JInfo: jpeg_decompress_struct;
  JError: jpeg_error_mgr;

begin
  //InternalCheck
  Result:=false;
  if Str=nil then exit;
  OldPos:= str.Position;
  BytesRead := str.Read({%H-}magic,sizeof(magic));
  if BytesRead<>sizeof(magic) then exit;
  if (magic[0] = $ff) and (magic[1] = $d8) and (magic[2] = $ff) and (magic[3] >= $c0) then result := true;
  Str.Position:= OldPos;

  FillChar(AInfo, Sizeof(AInfo), 0);
  if Result then
  with AInfo do
  begin
    //Same as InternalSize but with our Additional Info
    FillChar(JInfo,SizeOf(JInfo),0);
    if Str.Position < Str.Size then begin
      JError:=jpeg_std_error;
      JInfo.err := @JError;
      jpeg_CreateDecompress(@JInfo, JPEG_LIB_VERSION, SizeOf(JInfo));
      try
         jpeg_stdio_src(@JInfo, @Str);

         jpeg_read_header(@JInfo, TRUE);

         Width := JInfo.image_width;
         Height := JInfo.image_height;

         {$IF FPC_FULLVERSION>=30301}
         if JInfo.saw_EXIF_marker and (JInfo.orientation >= Ord(Low(TExifOrientation))) and (JInfo.orientation <= Ord(High(TExifOrientation))) then
           Orientation := TExifOrientation(JInfo.orientation)
         else
           Orientation := Low(TExifOrientation);
         {$ENDIF}

         Grayscale := JInfo.jpeg_color_space = JCS_GRAYSCALE;
         ProgressiveEncoding := jpeg_has_multiple_scans(@JInfo);

         ResolutionUnit:=density_unitToResolutionUnit(JInfo.density_unit);
         ResolutionX :=JInfo.X_density;
         ResolutionY :=JInfo.Y_density;

      finally
        jpeg_Destroy_Decompress(@JInfo);
      end;
    end;

    Str.Position:= OldPos;
  end;
end;

procedure JPEGError(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
  raise Exception.CreateFmt('JPEG error',[CurInfo^.err^.msg_code]);
end;

procedure EmitMessage(CurInfo: j_common_ptr; msg_level: Integer);
begin
  if CurInfo=nil then exit;
  if msg_level=0 then ;   //MaxM: really senseless, consider eliminating this procedure
end;

procedure ResetErrorMgr(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
  CurInfo^.err^.num_warnings := 0;
  CurInfo^.err^.msg_code := 0;
end;

initialization
  with jpeg_std_error do begin
    error_exit:=@JPEGError;
    emit_message:=@EmitMessage;
    output_message:= nil;
    format_message:= nil;
    reset_error_mgr:=@ResetErrorMgr;
  end;

  BGRARegisterImageReader(ifJpeg, TBGRAReaderJpeg, True, 'JPEG Graphics', 'jpg;jpeg');

end.

