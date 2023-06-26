// SPDX-License-Identifier: LGPL-3.0-linking-exception
{*****************************************************************************}
{
  2023-06  - Massimo Magnano
           - added Resolution support
}
{*****************************************************************************}
unit BGRAWriteJpeg;

{$mode objfpc}{$H+}

//issue #40327 TFPWriterJPEG.Finfo inaccesible to descendant classes
//if your version of fcl-image is older than issue #40327 solution comment the next line
//{$define HAVE_COMPRESSINFO}

interface

uses
  Classes, SysUtils, FPImage, JPEGLib, FPReadJPEG, FPWriteJPEG, BGRAReadJPeg,
  JcAPIstd, JcAPImin, JDataDst, JcParam, JError;

type
  { TBGRAWriterJPEG }

  TFPJPEGCompressionQuality = 1..100;   // 100 = best quality, 25 = pretty awful

  TBGRAWriterJPEG = class(TFPWriterJPEG)
  protected
    {$ifndef HAVE_COMPRESSINFO}
    CompressInfo: jpeg_compress_struct;
    FError: jpeg_error_mgr;
    FProgressMgr: TFPJPEGProgressManager;

    {$else}
    //Waiting for solution to issue #40329 on fcl-image
    {$endif}
    procedure WriteResolutionValues(Img: TFPCustomImage); virtual;
    procedure InternalWrite(Str: TStream; Img: TFPCustomImage); override;
  end;

implementation

uses BGRABitmapTypes;

{$ifndef HAVE_COMPRESSINFO}
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
{$endif}

{ TBGRAWriterJPEG }

procedure TBGRAWriterJPEG.WriteResolutionValues(Img: TFPCustomImage);
begin
  if (Img is TCustomUniversalBitmap) then
  with TCustomUniversalBitmap(Img) do
  begin
    CompressInfo.density_unit :=ResolutionUnitTodensity_unit(ResolutionUnit);
    CompressInfo.X_density :=Round(ResolutionX);
    CompressInfo.Y_density :=Round(ResolutionY);
  end;
end;

procedure TBGRAWriterJPEG.InternalWrite(Str: TStream; Img: TFPCustomImage);
{$ifndef HAVE_COMPRESSINFO}
var
  MemStream: TMemoryStream;
  Continue: Boolean;

  procedure InitWriting;
  begin
    FError := jpeg_std_error;
    CompressInfo := Default(jpeg_compress_struct);
    jpeg_create_compress(@CompressInfo);
    CompressInfo.err := jerror.jpeg_std_error(FError);
    CompressInfo.progress := @FProgressMgr.pub;
    FProgressMgr.pub.progress_monitor := @ProgressCallback;
    FProgressMgr.instance := Self;
  end;

  procedure SetDestination;
  begin
    if Str is TMemoryStream then
      MemStream:=TMemoryStream(Str)
    else
      MemStream := TMemoryStream.Create;
    jpeg_stdio_dest(@CompressInfo, @MemStream);
  end;

  procedure WriteHeader;
  begin
    CompressInfo.image_width := Img.Width;
    CompressInfo.image_height := Img.Height;
    if Grayscale then
    begin
      CompressInfo.input_components := 1;
      CompressInfo.in_color_space := JCS_GRAYSCALE;
    end
    else
    begin
      CompressInfo.input_components := 3; // RGB has 3 components
      CompressInfo.in_color_space := JCS_RGB;
    end;

    jpeg_set_defaults(@CompressInfo);

    jpeg_set_quality(@CompressInfo, CompressionQuality, True);

    if ProgressiveEncoding then
      jpeg_simple_progression(@CompressInfo);

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
    jpeg_start_compress(@CompressInfo, True);

    // write one line per call
    GetMem(SampArray,SizeOf(JSAMPROW));
    GetMem(SampRow,CompressInfo.image_width*CompressInfo.input_components);
    SampArray^[0]:=SampRow;
    try
      y:=0;
      while (CompressInfo.next_scanline < CompressInfo.image_height) do begin
        if Grayscale then
        for x:=0 to CompressInfo.image_width-1 do
          SampRow^[x]:=CalculateGray(Img.Colors[x,y]) shr 8
        else
        for x:=0 to CompressInfo.image_width-1 do begin
          Color:=Img.Colors[x,y];
          SampRow^[x*3+0]:=Color.Red shr 8;
          SampRow^[x*3+1]:=Color.Green shr 8;
          SampRow^[x*3+2]:=Color.Blue shr 8;
        end;
        LinesWritten := jpeg_write_scanlines(@CompressInfo, SampArray, 1);
        if LinesWritten<1 then break;
        inc(y);
      end;
    finally
      FreeMem(SampRow);
      FreeMem(SampArray);
    end;

    jpeg_finish_compress(@CompressInfo);
    Progress(psEnding, 100, False, Rect(0,0,0,0), '', Continue);
  end;

  procedure EndWriting;
  begin
    jpeg_destroy_compress(@CompressInfo);
  end;
  {$endif}

begin
  {$ifdef HAVE_COMPRESSINFO}
  inherited InternalWrite(Str, Img);
  {$else}
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
  {$endif}
end;


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

  if ImageHandlers.ImageWriter['JPEG graphics']=nil
  then ImageHandlers.RegisterImageWriter ('JPEG graphics', 'jpg;jpeg', TBGRAWriterJPEG);
  DefaultBGRAImageWriter[ifJpeg] := TBGRAWriterJPEG;

end.
