// SPDX-License-Identifier: LGPL-3.0-linking-exception
{*****************************************************************************}
{
  2023-06  - Massimo Magnano
           - added Resolution support
}
{*****************************************************************************}
{ Imports the writer for the JPEG image format }
unit BGRAWriteJpeg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage, FPReadJPEG, FPWriteJPEG

  {$IF FPC_FULLVERSION<30203}, JPEGLib, JcAPIstd, JcAPImin, JDataDst, JcParam, JError{$ENDIF};

type
  TFPJPEGCompressionQuality = 1..100;   // 100 = best quality, 25 = pretty awful

  {* Extends the TFPWriterJPEG to save resolution }
  TBGRAWriterJPEG = class(TFPWriterJPEG)
  protected
    {$IF FPC_FULLVERSION<30203}
    ACompressInfo: jpeg_compress_struct;
    FError: jpeg_error_mgr;
    FProgressMgr: TFPJPEGProgressManager;

    procedure WriteResolutionValues(Img: TFPCustomImage); virtual;
    procedure InternalWrite(Str: TStream; Img: TFPCustomImage); override;
    {$ENDIF}

  published
    //property CompressInfo : jpeg_compress_struct; rw
    property CompressionQuality;  //: TFPJPEGCompressionQuality; rw
    property ProgressiveEncoding; //: boolean; rw
    property GrayScale;           //: boolean; rw
  end;

implementation

uses BGRABitmapTypes;

{$IF FPC_FULLVERSION<30203}
function ResolutionUnitTodensity_unit(AResolutionUnit: TResolutionUnit): UINT8;
begin
  Case AResolutionUnit of
  ruPixelsPerInch: Result :=1;
  ruPixelsPerCentimeter: Result :=2;
  else Result :=0;
  end;
end;

procedure JPEGError(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
  {$ifdef FPC_Debug_Image}
  writeln('JPEGError ',CurInfo^.err^.msg_code,' ');
  {$endif}
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

var
  jpeg_std_error: jpeg_error_mgr;

procedure ProgressCallback(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
  // ToDo
end;

{ TBGRAWriterJPEG }

procedure TBGRAWriterJPEG.WriteResolutionValues(Img: TFPCustomImage);
begin
  if (Img is TCustomUniversalBitmap) then
  with TCustomUniversalBitmap(Img) do
  begin
    ACompressInfo.density_unit :=ResolutionUnitTodensity_unit(ResolutionUnit);
    ACompressInfo.X_density :=Round(ResolutionX);
    ACompressInfo.Y_density :=Round(ResolutionY);
  end;
end;

procedure TBGRAWriterJPEG.InternalWrite(Str: TStream; Img: TFPCustomImage);
var
  MemStream: TMemoryStream;
  Continue: Boolean;

  procedure InitWriting;
  begin
    FError := jpeg_std_error;
    ACompressInfo := Default(jpeg_compress_struct);
    jpeg_create_compress(@ACompressInfo);
    ACompressInfo.err := jerror.jpeg_std_error(FError);
    ACompressInfo.progress := @FProgressMgr.pub;
    FProgressMgr.pub.progress_monitor := @ProgressCallback;
    FProgressMgr.instance := Self;
  end;

  procedure SetDestination;
  begin
    if Str is TMemoryStream then
      MemStream:=TMemoryStream(Str)
    else
      MemStream := TMemoryStream.Create;
    jpeg_stdio_dest(@ACompressInfo, @MemStream);
  end;

  procedure WriteHeader;
  begin
    ACompressInfo.image_width := Img.Width;
    ACompressInfo.image_height := Img.Height;
    if Grayscale then
    begin
      ACompressInfo.input_components := 1;
      ACompressInfo.in_color_space := JCS_GRAYSCALE;
    end
    else
    begin
      ACompressInfo.input_components := 3; // RGB has 3 components
      ACompressInfo.in_color_space := JCS_RGB;
    end;

    jpeg_set_defaults(@ACompressInfo);

    jpeg_set_quality(@ACompressInfo, CompressionQuality, True);

    if ProgressiveEncoding then
      jpeg_simple_progression(@ACompressInfo);

    WriteResolutionValues(Img);
  end;

  procedure WritePixels;
  var
    LinesWritten: Cardinal;
    SampArray: JSAMPARRAY;
    SampRow: JSAMPROW;
    Color: TFPColor;
    x: Integer;
    y: Integer;
  begin
    Progress(psStarting, 0, False, Rect(0,0,0,0), '', Continue);
    if not Continue then exit;
    jpeg_start_compress(@ACompressInfo, True);

    // write one line per call
    GetMem(SampArray,SizeOf(JSAMPROW));
    GetMem(SampRow,ACompressInfo.image_width*ACompressInfo.input_components);
    SampArray^[0]:=SampRow;
    try
      y:=0;
      while (ACompressInfo.next_scanline < ACompressInfo.image_height) do begin
        if Grayscale then
        for x:=0 to ACompressInfo.image_width-1 do
          SampRow^[x]:=CalculateGray(Img.Colors[x,y]) shr 8
        else
        for x:=0 to ACompressInfo.image_width-1 do begin
          Color:=Img.Colors[x,y];
          SampRow^[x*3+0]:=Color.Red shr 8;
          SampRow^[x*3+1]:=Color.Green shr 8;
          SampRow^[x*3+2]:=Color.Blue shr 8;
        end;
        LinesWritten := jpeg_write_scanlines(@ACompressInfo, SampArray, 1);
        if LinesWritten<1 then break;
        inc(y);
      end;
    finally
      FreeMem(SampRow);
      FreeMem(SampArray);
    end;

    jpeg_finish_compress(@ACompressInfo);
    Progress(psEnding, 100, False, Rect(0,0,0,0), '', Continue);
  end;

  procedure EndWriting;
  begin
    jpeg_destroy_compress(@ACompressInfo);
  end;


begin
  Continue := true;
  MemStream:=nil;
  try
    InitWriting;
    SetDestination;
    WriteHeader;
    WritePixels;
    if MemStream<>Str then begin
      MemStream.Position:=0;
      Str.CopyFrom(MemStream,MemStream.Size);
    end;
  finally
    EndWriting;
    if MemStream<>Str then
      MemStream.Free;
  end;
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

  BGRARegisterImageWriter(ifJpeg, TBGRAWriterJPEG, True, 'JPEG Graphics', 'jpg;jpeg');

end.
